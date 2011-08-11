(* This is an adaption of Ocamlnet's rpc_client.ml *)

open Hydro_types
open Hydro_util
open Hydro_dbg
open Hydro_prelim

module SessionMap = Map.Make(Int32)

type client_response_condition = [ `Success | client_condition ]

type client_response =
    < condition : client_response_condition;
      out_args : value array;
      result : value;
         (* Raises [Client_condition] if [condition] is not [`Success] *)
      peer : Unix.sockaddr option;
      addr : Unix.sockaddr option;
      mode : op_mode;
      client : ep option
    >


and call_state =
    Waiting    (* call has not yet been sent *)
  | Pending    (* call has been sent, still no answer *)
  | Done       (* got answer for the call *)


and call_msg =
    [ `Request of request_msg
    | `Batch_request of batch_request_msg list
    ]


and ep_state =
  | Init
  | Down
  | Connecting of Hydro_connector.multiplexed_descriptor Uq_engines.engine
  | Validating  (* client waits for Validate_connection (unused for servers) *)
  | Closing1    (* close message not yet sent (clients&servers) *)
  | Closing2    (* close message is sent out (clients&servers) *)
  | Closing3    (* stream is only half-open *)
  | Up

  (* Transition for clients:
     - Init -> Connecting -> Validating -> Up -> Closing1 -> Closing2 -> Closing3 -> Down
       Closing1 is skipped if shutdown is initiated by the remote side.
     Transition for servers:
     - Init -> Up -> Closing1 -> Closing2 -> Closing3 -> Down
       The validation msg is sent out while being in Init.
       Closing1 is skipped if shutdown is initiated by the remote side.
   *)


and call =
      { msg : call_msg;
	hf : hfunction;
	
	(* Params: *)
        destination : Unix.sockaddr option;
	msg_timeout : float;
	mutable msg_timeout_group : Unixqueue.group option;
	(* When a msg timer is running, this is [Some g], and by clearing g
           the timer is stopped prematurely
         *)
	
        pass_response : client_response -> unit;

        mutable state : call_state;
	mutable peer : Unix.sockaddr option;

        (* mutable when_sent : unit -> bool; *)
      }
   (* In clients, [call] is a structure for managing pending calls. *)


and srv_response =
    { session : session;
      mutable rsent : bool;    (* whether sent *)
    }
  (* In servers, [srv_response] manages responses of RPC invocations *)



and ep =
    { ci : < >;    (* client identifier *)
      sys : system;

      mutable trans : Hydro_connector.multiplexed_descriptor option;
      esys : Unixqueue.event_system;

      mutable ep_state : ep_state;
      mutable client_enabled : bool;
      mutable server_enabled : bool;
      (* Endpoints can be clients, servers, or clients & servers at once! *)

      
      (* State only used for clients: *)

      mutable wait_queue : call Queue.t;    (* Calls not yet sent *)
      mutable wait_map : call SessionMap.t; (* Calls not yet sent *)
      mutable pending : call SessionMap.t;  (* Calls sent, but still unreplied *)
      mutable abort_queue : (unit->unit) Queue.t; (* To be notified on abort *)

      mutable next_xid : int32;

      mutable client_params : client_params option;  (* as configured *)

      mutable idle_timeout_group : Unixqueue.group option;
      mutable shutdown_when_idle : bool;

      mutable pool_callback : (unit -> unit) option;

      (* State only used for servers: *)

      mutable srv_response_queue : srv_response Queue.t;
                                      (* Responses not yet sent *)
      mutable adapters : object_dispatcher list;
      mutable server_params : server_params option;  (* as configured *)
      mutable server_endpoint : endpoint option;
      mutable onabort : (ep -> unit);

      (* State used by clients & servers: *)

      mutable max_proto_minor : int;  (* for servers constant! *)
      mutable max_enc_minor : int;    (* for servers constant! *)
      mutable shutdown_timeout_group : Unixqueue.group option;
      mutable graceful : bool;
    }

let xidopt_of_call call =
  match call.msg with
    | `Request req ->
	let xid = req#request_id in
	if xid = 0l then
	  None
	else
	  Some xid
    | _ ->
	None

let client_params ep =
  match ep.client_params with
    | Some cp -> cp
    | None -> assert false

let server_params ep =
  match ep.server_params with
    | Some cp -> cp
    | None -> assert false


let server_endpoint ep =
  match ep.server_endpoint with
    | None -> assert false
    | Some sep -> sep


let max_proto_minor ep trans =
  (* Get max protocol version of the server *)
  match trans#ctrl#transport_protocol_type with
    | `Stream ->
	ep.max_proto_minor
    | `Datagram ->
	if ep.client_params = None then
	  Hydro_marshal.max_proto_minor (* we are the server *)
	else
	  ( match (client_params ep)#max_proto_minor with
	      | None -> 0  (* conservative default *)
	      | Some pm -> pm
	  )


let max_enc_minor ep trans =
  (* Get max encoding version of the server *)
  match trans#ctrl#transport_protocol_type with
    | `Stream ->
	ep.max_enc_minor
    | `Datagram ->
	if ep.client_params = None then
	  Hydro_marshal.max_enc_minor (* we are the server *)
	else
	  ( match (client_params ep)#max_enc_minor with
	      | None -> 0  (* conservative default *)
	      | Some em -> em
	  )
	    
let max_enc_minor_nt ep =
  match ep.trans with
    | Some trans -> max_enc_minor ep trans
    | None -> 0
	(* so we can call this function if we don't have a connection 
           right now
	 *)


let addr_of_ep ep =
  match ep.trans with
    | None -> None
    | Some md -> 
	( match md # ctrl # getsockname with
	    | `Implied -> None
	    | `Sockaddr s -> Some s
	)


  (*****)

let check_for_input =  (* "forward declaration" *)
  ref (fun _ -> ())

let check_for_output = (* "forward declaration" *)
  ref (fun _ -> ())

  (*****)

let stop_call_timers ep call =
  match call.msg_timeout_group with
    | None -> ()
    | Some g ->
	Unixqueue.clear ep.esys g;
	call.msg_timeout_group <- None


let pass_call_response ep call response =
  (* Tell the callers about the response we got from the server *)
  if call.state <> Done then (  (* Don't call back twice *)
    (* Stop timers, if any : *)
    stop_call_timers ep call;
    (* Change the state of the call to 'Done': *)
    call.state <- Done;
    (* pass [response] to the call back function: *)
    try
      if dlog_enabled() then
	dlog "Hydro_endpoint: Calling back";
      call.pass_response response;
      if dlog_enabled() then
	dlog "Hydro_endpoint: Returned from callback";
    with
      | Unbound_exception x ->
	  if dlog_enabled() then
            dlog "Hydro_endpoint: Unbound_exception";
          raise x
      | any ->
          begin  (* pass the exception to the exception handler: *)
	    if dlog_enabled() then
	      dlogf
		"Hydro_endpoint: Exception from callback: %s"
		(Hydro_util.exn_to_string any);
            (client_params ep)#exception_handler#handle any
          end
  )

let pass_call_condition ep call cc connect_flag =
  (* Tell the callers about an error condition we got from the server,
     or produced ourselves
   *)
  (* Caution! This function does not remove [call] from the set of pending
   * calls.
   *)
  let cc =
    (* Check for connect errors: *)
    match cc with
      | (`Message_timeout | `Transport_timeout) when connect_flag ->
	  `Connect_timeout
      | `Error x when connect_flag -> 
	  `Connect_error x
      | `Message_lost _ ->
	  `Message_lost (not connect_flag && call.state <> Waiting)
      | _ -> cc in
  if dlog_enabled() then
    dlog ("Hydro_endpoint: Passing condition " ^ cc_to_string cc);
  let addr = addr_of_ep ep in
  let response =
    ( object
	method condition = (cc : client_condition :> client_response_condition)
	method out_args = [| |]
	method result = raise (Client_condition cc)
	method peer = call.peer
	method addr = addr
	method mode = call.hf#mode
	method client = Some ep
      end
    ) in
  pass_call_response ep call response

let pass_call_condition_to_all ep cc connect_flag =
  (* Caution! This function does not erase the set of pending calls.  *)
  if dlog_enabled() then
    dlog "Hydro_endpoint: Passing condition to all";

  let pending = ep.pending in
  ep.pending <- SessionMap.empty;

  let waiting = Queue.create() in
  Queue.transfer ep.wait_queue waiting;
  ep.wait_map <- SessionMap.empty;

  SessionMap.iter 
    (fun _ call -> pass_call_condition ep call cc connect_flag) 
    pending;
  Queue.iter
    (fun call -> pass_call_condition ep call cc connect_flag) 
    waiting

  (*****)

let is_up ep =
  match ep.ep_state with
    | Init
    | Up
    | Validating
    | Connecting _ ->
	true
    | _ ->
	false

let graceful ep =
  ep.graceful

let tell_pool ep =
  match ep.pool_callback with
    | None -> ()
    | Some f -> f()


let cancel_idle_timeout ep =
  match ep.idle_timeout_group with
    | None -> ()
    | Some g ->
	Unixqueue.clear ep.esys g;
	ep.idle_timeout_group <- None

	  
let stop_shutdown_timer ep =
  match ep.shutdown_timeout_group with
    | None -> ()
    | Some g -> 
	Unixqueue.clear ep.esys g;
	ep.shutdown_timeout_group <- None


let abort_with cc_opt ep =
  (* Immediately stop any processing because of a serious error *)
  if ep.ep_state <> Down then (  (* Avoid nested aborts *)
    if dlog_enabled() then
      dlog "Hydro_endpoint: Aborting";
    if cc_opt <> None then
      ep.graceful <- false;
    stop_shutdown_timer ep;
    cancel_idle_timeout ep;
    let was_up = is_up ep in
    let connect_flag =
      match ep.ep_state with
	| Connecting _ | Validating -> true
	| _ -> false in
    ( match ep.ep_state with
	| Down -> ()

	| Connecting e ->
	    e#abort()
	      
	| Validating | Closing1 | Closing2 | Closing3 | Up ->
	    ( match ep.trans with
		| None -> () (* strange *)
		| Some c ->
		    c # shutdown();
		    ep.trans <- None
	    )
	      
	| Init ->
	    ()
    );
    ep.ep_state <- Down;
    (* If we are a client, tell all callers about the error: *)
    let cc =
      match cc_opt with
	| Some cc -> cc
	| None -> `Message_lost false in  (* arg updated later *)
    if was_up then 
      tell_pool ep;
    ep.onabort ep;   (* Server *)
    Queue.clear ep.srv_response_queue;
    pass_call_condition_to_all ep cc connect_flag;
    Queue.iter (fun f -> f()) ep.abort_queue;
    Queue.clear ep.abort_queue
  )


let queued_requests ep =
  match ep.ep_state with
    | Down -> 
	0
    | _ ->
	let n_pending =
	  SessionMap.fold (fun _ _ n -> n+1) ep.pending 0 in
	Queue.length ep.wait_queue + n_pending

let is_idle ep =
  ep.ep_state = Up &&
    ep.wait_map = SessionMap.empty &&
    ep.pending = SessionMap.empty
  (* Note: ep.wait_queue is unreliable for checking on idleness. The
     queue may contain cancelled calls.
   *)


let endpoint_id ep =
  Oo.id ep.ci


let start_shutdown_timer ep =
  if ep.client_enabled && (client_params ep)#msg_timeout >= 0.0 then (
    (* If there is a msg_timeout this is also the upper bound for the 
       shutdown process. Note that a trans_timeout also applies because
       it is still set on the transporter.
     *)
    let g = Unixqueue.new_group ep.esys in
    Unixqueue.once ep.esys g (client_params ep)#msg_timeout
      (fun () ->
	 (* Transport_timeout seems to be most appropriate *)
	 abort_with (Some `Transport_timeout) ep
      );
    ep.shutdown_timeout_group <- Some g
  )





let shutdown ?(ondown=(fun()->())) ep =
  let g = Unixqueue.new_group ep.esys in
  let invoke_by_esys f () =
    (* invoke through event queue to have better exception handling *)
    Unixqueue.once ep.esys g 0.0 f in

  if ep.ep_state <> Down then
    Queue.push (invoke_by_esys ondown) ep.abort_queue;
  match ep.ep_state with
    | Up ->
	( match ep.trans with
	    | None -> assert false
	    | Some trans ->
		if trans#ctrl#transport_protocol_type = `Stream then (
		  if dlog_enabled() then
		    dlog "Hydro_endpoint: Will send shutdown msg";
		  cancel_idle_timeout ep;
		  start_shutdown_timer ep;
		  ep.ep_state <- Closing1;
		  tell_pool ep;
		  !check_for_output ep
		) else 
		  (* Close message isn't supported for datagrams: *)
		  abort_with None ep
	)

    | Init
    | Validating
    | Connecting _ ->
	abort_with None ep

    | Closing1
    | Closing2
    | Closing3 ->
	(* Shutdown already in progress *)
	()

    | Down ->
	(* mostly ignore *)
	invoke_by_esys ondown ()
	

let shutdown_when_idle ep =
  match ep.ep_state with
    | Init
    | Up
    | Validating
    | Connecting _ ->
	ep.shutdown_when_idle <- true
    | _ ->
	()

let abort ep =
  abort_with None ep

  (*****)

let when_idle ep =
  if ep.client_enabled then (
    cancel_idle_timeout ep;
    if ep.shutdown_when_idle then
      shutdown ep
    else (
      let tmo = (client_params ep)#idle_timeout in
      if tmo > 0.0 then (
	let g = Unixqueue.new_group ep.esys in
	Unixqueue.once ep.esys g tmo
	  (fun () -> 
	     ep.idle_timeout_group <- None;
	     shutdown ep
	  );
	ep.idle_timeout_group <- Some g;
      )
    )
  )

  (*****)

let remove_call ep call =
  ( match xidopt_of_call call with
      | Some xid ->
	  ep.pending <- SessionMap.remove xid ep.pending;
	  ep.wait_map <- SessionMap.remove xid ep.wait_map;
      | None ->
	  (* No reply expected... *)
	  ()
  )
  (* Note that we do not remove the call from waiting_calls for
     performance reasons. We simply skip it there if we find it.
     pass_exception will set call.state to Done.
   *)

let cancel_call ep call cc =
  if dlog_enabled() then
    dlog "Hydro_endpoint: Cancelling call";
  remove_call ep call;
  let connect_flag =
    match ep.ep_state with
      | Connecting _ -> true
      | _ -> false in
  pass_call_condition ep call cc connect_flag;
  if is_idle ep then when_idle ep;
  !check_for_output ep

let set_call_timeout ep call =
  if call.msg_timeout > 0.0 && call.msg_timeout_group = None then (
    (* Note: Case call_timeout = 0.0 is handled elsewhere *)
    let g = Unixqueue.new_group ep.esys in
    Unixqueue.once ep.esys g call.msg_timeout
      (fun () ->
         call.msg_timeout_group <- None;
	 if dlog_enabled() then
           dlog "Hydro_endpoint: Timeout handler";
         cancel_call ep call `Message_timeout;
         (* Maybe we have to cancel reading: *)
         !check_for_input ep
      );
    call.msg_timeout_group <- Some g
  )

let rec new_xid ep =
  let xid = ep.next_xid in
  ep.next_xid <- Int32.succ xid;
  if (xid = 0l ||     (* 0 is reserved for oneway calls *)
      SessionMap.mem xid ep.wait_map || 
      SessionMap.mem xid ep.pending)
  then
    new_xid ep
  else
    xid


let find_function intf op =
  try List.find (fun f -> f#name = op) intf#elements
  with Not_found -> raise(Marshal_error ("Operation not found: " ^ op))


let add_call ep msg hf call_params pass_response =
  cancel_idle_timeout ep;
  let msg_timeout =
    match call_params#msg_timeout with
      | Some tmo -> tmo
      | None -> (client_params ep)#msg_timeout in
  let call =
    { msg = msg;
      hf = hf;
      destination = call_params#destination;
      msg_timeout = msg_timeout;
      msg_timeout_group = None;
      pass_response = pass_response;
      state = Waiting;
      peer = None;
    } in
  if msg_timeout > 0.0 then
    set_call_timeout ep call;
  Queue.add call ep.wait_queue;
  ( match xidopt_of_call call with
      | None -> ()
      | Some xid ->
	  ep.wait_map <- SessionMap.add xid call ep.wait_map
  );
  !check_for_output ep


let create_request facet ep id hf op call_params in_args xid =
  (* This is for clients only *)
  let ctx =
    match call_params#context with
      | None -> [| |]
      | Some ctx -> Array.of_list ctx in
  let msg = 
    ( object
	method request_id = xid
	method id = id
	method facet = facet
	method operation = op
	method mode = hf#mode;
	method context = ctx
	method params =
	  (* We may only call this method when we have trans! *)
	  let em =
	    min Hydro_marshal.max_enc_minor (max_enc_minor_nt ep) in
	  let in_type =
	    TStruct hf#in_args in
	  let cflag =
	    hf # in_classes in
	  let b = 
	    Netbuffer.create 500 in
	  Hydro_marshal.marshal ep.sys in_type cflag (VStruct in_args) b em;
	  { encap_buf = b;
	    encap_pos = 0;
	    encap_len = Netbuffer.length b;
	    encap_enc_minor = em
	  }
      end
    ) in
  `Request msg

let require_client_is_up ep =
  match ep.ep_state with
    | Down | Closing1 | Closing2 | Closing3 ->
	raise(Client_condition `Client_is_down)
    | _ ->
	()

let twoway_call ?facet ep id (intf:hintf) op (call_params:call_params) args pass_response =
  require_client_is_up ep;
  let hf = find_function intf op in
  let xid = new_xid ep in
  let msg = create_request facet ep id hf op call_params args xid in
  add_call ep msg hf call_params pass_response

  (*****)

let decode_reply ep call reply peer_opt =
  (* This is for clients only *)
  let addr = addr_of_ep ep in
  match reply#result with
    | `Success eb ->
	let hf = call.hf in
	let t =
	  TStruct
	    [| "out_args", TStruct hf#out_args;
	       "resval", hf#result;
	    |] in
	let cflag = hf#out_classes in
	let v = Hydro_unmarshal.unmarshal ep.sys t cflag eb in
	( match v with
	    | VStruct [| VStruct out_vals;
			 resval;
		      |] ->
		( object 
		    method condition = `Success
		    method out_args = out_vals
		    method result = resval
		    method addr = addr
		    method peer = peer_opt
		    method mode = call.hf#mode
		    method client = Some ep
		  end
		)
	    | _ ->
		assert false
	)

    | `User_exception eb ->
	let slices = Hydro_unmarshal.unmarshal_exn ep.sys eb in
	let cc = `User_exception slices in
	( object
	    method condition = cc
	    method out_args = [| |]
	    method result = raise(Client_condition cc)
	    method addr = addr
	    method peer = peer_opt
	    method mode = call.hf#mode
	    method client = Some ep
	  end
	)
	
    | ( `Object_does_not_exist _
      | `Facet_does_not_exist _
      | `Operation_does_not_exist _
      | `Unknown_local_exception _
      | `Unknown_user_exception _
      | `Unknown_exception _ as cc
      ) ->
	( object 
	    method condition = (cc :> client_response_condition)
	    method out_args = [| |]
	    method result = raise(Client_condition cc)
	    method addr = addr
	    method peer = peer_opt
	    method mode = call.hf#mode
	    method client = Some ep
	  end
	)

  (*****)

let push_session ep (s:session) =
  if !Hydro_dbg.debug_log <> None then (
    match s#response with
      | Some r ->
	  if dlog_enabled() then
	    dlogf "Hydro_endpoint: replying req_id=%ld result=%s"
	      r#request_id (Hydro_util.result_to_string r#result)
      | None ->
	  if dlog_enabled() then
	    dlogf "Hydro_endpoint: pushing session without response!"
  );
  if ep.ep_state = Up then (
    let srv_resp =
      { session = s; rsent = false } in
    Queue.add srv_resp ep.srv_response_queue;
    !check_for_output ep
  )


let decode_params ep params_eb hf =
  let t =
    TStruct
      [| "in_args", TStruct hf#in_args;
      |] in
  let cflag = hf#in_classes in
  let v = Hydro_unmarshal.unmarshal ep.sys t cflag params_eb in
  ( match v with
      | VStruct [| VStruct in_vals |] ->
	  in_vals
      | _ ->
	  assert false
  )


let create_reply req_id (r:result) =
  ( object
      method request_id = req_id
      method result = r
    end : reply_msg
  )


let create_result_encap ep hf res_val out_vals =
  let em =
    min Hydro_marshal.max_enc_minor (max_enc_minor_nt ep) in
  let out_type =
    TStruct [| "out_args", TStruct hf#out_args;
	       "resval", hf#result;
	    |] in
  let out_val =
    VStruct [| VStruct out_vals;
	       res_val;
	    |] in
  let cflag =
    hf # out_classes in
  let b = 
    Netbuffer.create 500 in
  Hydro_marshal.marshal ep.sys out_type cflag out_val b em;
  { encap_buf = b;
    encap_pos = 0;
    encap_len = Netbuffer.length b;
    encap_enc_minor = em
  }


let create_uexn_encap ep hx sv =
  let em =
    min Hydro_marshal.max_enc_minor (max_enc_minor_nt ep) in
  let b = 
    Netbuffer.create 500 in
  Hydro_marshal.marshal_exn ep.sys hx sv b em;
  { encap_buf = b;
    encap_pos = 0;
    encap_len = Netbuffer.length b;
    encap_enc_minor = em
  }


let server_ops ep =
  ( object
      method endpoint = server_endpoint ep
      method shutdown_connection() = shutdown ep
      method abort_connection () = abort ep
      method event_system = ep.esys
      method system = ep.sys
      method server_params = server_params ep
      method server_id = endpoint_id ep
    end : server_ops
  )


let create_session ep req_id ctx hf =
  let response = ref None in
  ( object(self)
      method is_responded = !response <> None
      method response = !response

      method emit_result res_val out_vals =
	let push_flag = (!response = None) in
	let eb = create_result_encap ep hf res_val out_vals in
	response := Some(create_reply req_id (`Success eb));
	if push_flag then push_session ep self
	  (* It may happen that the server is down in the meantime.
             push_session ignores the session in this case. It is
             important, however, that [response] is set, so that the
             attempt of responding is recorded.
           *)

      method emit_user_exception sv =
	let push_flag = (!response = None) in
	let ty_id = sv#hydro_effective_id in
	let hx_opt = 
	  try Some(CiHashtbl.find ep.sys#exceptions ty_id)
	  with Not_found -> None in
	( match hx_opt with
	    | Some hx ->
		let eb = create_uexn_encap ep hx sv in
		response := Some(create_reply req_id (`User_exception eb))
	    | None ->
		let e = ("Exception with ICE type ID: " ^ ty_id) in
		response := Some(create_reply req_id (`Unknown_user_exception e))
	);
	if push_flag then push_session ep self

      method emit_unknown_local_exception e =
	let push_flag = (!response = None) in
	response := Some(create_reply req_id (`Unknown_local_exception e));
	if push_flag then push_session ep self

      method emit_unknown_user_exception e =
	let push_flag = (!response = None) in
	response := Some(create_reply req_id (`Unknown_user_exception e));
	if push_flag then push_session ep self

      method emit_unknown_exception e =
	let push_flag = (!response = None) in
	response := Some(create_reply req_id (`Unknown_exception e));
	if push_flag then push_session ep self

      method request_id = req_id

      method context = ctx

      method server_ops = server_ops ep
    end : session
  )


let direct_reply ep req_id res =
  let msg = create_reply req_id res in
  let s =
    ( object
	method is_responded = true
	method response = Some msg
	method emit_result _ _ = ()
	method emit_user_exception _ = ()
	method emit_unknown_local_exception _ = ()
	method emit_unknown_user_exception _ = ()
	method emit_unknown_exception _ = ()
	method request_id = req_id
	method context = []
	method server_ops = server_ops ep
      end : session
    ) in
  push_session ep s


let rec search_op intf lc_opname =
  (* CHECK: represent [elements] better/hashtbl*)
  try
    List.find 
      (fun hf -> 
	 String.lowercase (hf # name) = lc_opname)
      intf#elements
  with
    | Not_found ->
	search_op_list intf#super lc_opname


and search_op_list intf_list lc_opname =
  match intf_list with
    | [] -> raise Not_found
    | intf :: intf_list' ->
	try search_op intf lc_opname
	with Not_found -> search_op_list intf_list' lc_opname


      
let process_incoming_request ep req =
  (* This is for servers only *)
  (* Look into all adapters... *)
  let req_id = req#request_id in
  let facet = req#facet in
  let opname = req#operation in
  let id = req#id in
  let mode = req#mode in
  let ctx = Array.to_list req#context in

  if dlog_enabled() then
    dlogf "Hydro_endpoint: Got request req_id=%ld opname=%s id.name=%s id.cat=%s"
      req_id opname id#name id#category;

  let rec search_object oa_list =
    match oa_list with
      | [] -> raise Not_found
      | oa :: oa_list' ->
	  ( try oa#invoke_object id with Not_found -> search_object oa_list' )
  in

  let obj_opt =
    try Some(search_object ep.adapters) 
    with Not_found -> None in

  match obj_opt with
    | None ->
	(* We have to send an error response *)
	direct_reply 
	  ep req_id (`Object_does_not_exist(id,facet,opname))
    | Some obj ->
	let fct_opt =
	  try Some(obj # invoke_facet facet)
	  with Not_found -> None in
	( match fct_opt with
	    | None ->
		(* We have to send an error response *)
		direct_reply 
		  ep req_id (`Facet_does_not_exist(id,facet,opname))
	    | Some fct ->
		let type_id = fct # hydro_effective_id in
		let intf = 
		  try CiHashtbl.find ep.sys#interfaces type_id
		  with Not_found -> assert false  (* CHECK//shouldn't happen here *)in
		let lc_opname = String.lowercase opname in
		let opr_opt =
		  try Some(fct # hydro_invoke_operation lc_opname)
		  with Not_found -> None in
		( match opr_opt with
		    | None ->
			(* We have to send an error response *)
			direct_reply 
			  ep req_id (`Operation_does_not_exist(id,facet,opname))
		    | Some opr ->
			let hf = 
			  try search_op intf lc_opname
			  with Not_found -> assert false (* CHECK *)
			in
			(* Check whether mode is right: *)
			if hf#mode = `Normal && mode <> `Normal then
			  direct_reply 
			    ep req_id 
			    (`Unknown_exception
			       ("Operation is not declared as idempotent but invoked as idempotent or nonmutating: " ^ opname))
			else
			  if hf#mode <> `Normal && mode = `Normal then
			    direct_reply 
			      ep req_id 
			      (`Unknown_exception
				 ("Operation is declared as idempotent but neither invoked as idempotent nor nonmutating: " ^ opname))
			  else (
			    let session = create_session ep req_id ctx hf in
			    let in_vals = 
			      decode_params ep req#params hf in
			    opr in_vals session
			  )
		)
	)		


let process_incoming_message ep hdr nb peer_opt =
  (* First decode the message: *)
  let followup_action =
    try
      let eb =
	{ encap_buf = nb;
	  encap_pos = 0;
	  encap_len = Netbuffer.length nb;
	  encap_enc_minor = hdr#enc_minor
	} in
      let msg = Hydro_unmarshal.unmarshal_msg ep.sys hdr eb in
      (* checks version in hdr *)
      
      ( match msg with
	  | `Reply reply -> 
	      if dlog_enabled() then
		dlog "Hydro_endpoint: Got reply";
	      if not ep.client_enabled then 
		raise(Protocol_violation `BadMessageType);
	      let xid = reply#request_id in
	      let call_opt =
		try Some(SessionMap.find xid ep.pending)
		with Not_found -> None in
	      ( match call_opt with
		  | Some call ->
		      assert(call.state = Pending);
		      let response = 
			decode_reply ep call reply peer_opt in
		      remove_call ep call;
		      (fun () -> pass_call_response ep call response)
		    
		  | None ->
		      (* This could be a timed-out reply. Simply ignore! *)
		      (fun () -> ())
	      )
		
	  | `Validate_connection ->
	      if dlog_enabled() then
		dlog "Hydro_endpoint: Got Validate_connection";
	      if not ep.client_enabled then 
		raise(Protocol_violation `BadMessageType);
	      ( match ep.ep_state with
		  | Validating ->
		      ep.ep_state <- Up;
		      ep.max_proto_minor <- hdr#proto_minor;
		      ep.max_enc_minor <- hdr#enc_minor;
		  | Up -> 
		      ()
		  | _ ->
		      assert false
	      );
	      (fun () -> ())

	  | `Close_connection ->
	      if dlog_enabled() then
		dlog "Hydro_endpoint: Got Close_connection";
	      let was_up = is_up ep in
	      ( match ep.ep_state with
		  | Validating
		  | Up 
		  | Closing1 ->
		      let f =
		       match ep.trans with
			  | None -> assert false
			  | Some trans ->
			      if trans#ctrl#transport_protocol_type = `Stream 
			      then (
				ep.ep_state <- Closing2;
				(fun () -> !check_for_output ep)
			      )
			      else (fun () -> ())
				(* Close message is ignored for datagrams *) in
		      if was_up then tell_pool ep;
		      f
		  | Closing2 | Closing3 -> 
		    (fun () -> ())
		  | _ ->
		      assert false
	      )

	  | `Request req ->
	      if dlog_enabled() then
		dlog "Hydro_endpoint: Got request";
	      if not ep.server_enabled then 
		raise(Protocol_violation `BadMessageType);
	      (fun () -> process_incoming_request ep req)
		(* Note: exceptions in [process_incoming_request] are not
                   caught, and fall through the Hydro runtime
                 *)

	  | _ ->
	      raise(Protocol_violation `BadMessageType)
      )
    with
      | err ->
	  (* An exception that is not bound to a specific call: *)
	  if dlog_enabled() then
	    dlog ("Hydro_endpoint: Exception decoding message: " ^
                    Hydro_util.exn_to_string err);
	  if ep.client_enabled then
            (client_params ep)#exception_handler#handle err;   (* CHECK *)
	  (fun () -> ())
  in
  followup_action()

  (*****)

let rec handle_incoming_message ep r =
  (* Called when a complete message has been read by the transporter *)
  match r with
    | `Error err ->
	if dlog_enabled() then
	  dlogf "Hydro_endpoint: message reading exn: %s" 
	    (Hydro_util.exn_to_string err);
	abort_with (Some (`Error err)) ep
	  (* CHECK Servers: unclear whether we want to let the exception fall
             through, so the caller of Unixqueue.run gets it
           *)

    | `Ok(hdr, nb, peer_addr) ->
	if dlog_enabled() then
	  dlog "Hydro_endpoint: Message arrived";
	let peer_opt =
	  match peer_addr with
	    | `Implied -> None 
	    | `Sockaddr a -> Some a in
	let prev_state = ep.ep_state in
	process_incoming_message ep hdr nb peer_opt;  (* may fail!!! *)
	let is_validated = (prev_state = Validating) && (ep.ep_state = Up) in
	if is_idle ep then
	  when_idle ep;
	next_incoming_message ep;
	if is_validated then !check_for_output ep;
	()

    | `End_of_file ->
	if dlog_enabled() then
	  dlog "Hydro_endpoint: End of file";
	abort_with None ep

and next_incoming_message ep =
  match ep.trans with
    | None ->
	(* Not yet connected *)
	()

    | Some trans ->
	trans # ctrl # cancel_rd_polling();
	(* TODO: A better criterion for controlling reading *)
	let do_read =
	  match ep.ep_state with
	    | Init -> assert false
	    | Down -> false
	    | Connecting _ -> false
	    | Validating -> true
	    | Up ->
		let srv_cond =
		  ep.server_enabled && Queue.is_empty ep.srv_response_queue in
		let client_cond =
		  ep.client_enabled &&
		    (ep.pending <> SessionMap.empty || trans#ctrl#writing) in
		  (* That means: we read when we expect a reply, or
                     when we are writing currently
                   *)
		srv_cond || client_cond
	    | Closing1 -> true
	    | Closing2 -> true
	    | Closing3 -> true in
	if not trans#ctrl#reading && do_read then (
	  if dlog_enabled() then
	    dlog "Hydro_endpoint: (re)starting reading";
	  trans # ctrl # start_reading
	    ~when_done:(fun r -> handle_incoming_message ep r)
	    ()
	)
;;


check_for_input := next_incoming_message ;;


let rec handle_outgoing_message ep r =
  (* Called after a complete message has been sent by the transporter *)
  match r with
    | `Error err ->
	if dlog_enabled() then
	  dlogf "Hydro_endpoint: message writing exn: %s" 
	    (Hydro_util.exn_to_string err);
	abort_with (Some (`Error err)) ep
	  (* CHECK Servers: unclear whether we want to let the exception fall
             through, so the caller of Unixqueue.run gets it
           *)


    | `Ok () ->
	if dlog_enabled() then
	  dlog "Hydro_endpoint: message writing finished";
	!check_for_input ep;
	next_outgoing_message ep

and next_outgoing_message ep =
  match ep.trans with
    | None -> ()
    | Some trans ->
	if not trans#ctrl#writing then (
	  match ep.ep_state with
	    | Init         -> assert false
	    | Down         -> ()
	    | Connecting _ -> assert false   (* where is [trans] from? *)
	    | Validating   -> ()   (* not yet! *)
	    | Up           -> next_outgoing_srv_response ep trans
	    | Closing1     -> send_close_msg ep trans
	    | Closing2     -> send_eof ep trans
	    | Closing3     -> ()     (* no more output! *)
	)

and next_outgoing_srv_response ep trans =
  (* We first check for pending server responses, and only if we don't have
     any, we continue with pending (client) calls.
   *)
  let srv_resp =
    try Some(Queue.take ep.srv_response_queue) with Queue.Empty -> None in
  match srv_resp with
    | Some resp ->
	assert(not resp.rsent);
	(* Send the message: *)
	let pm =
	  Hydro_marshal.max_proto_minor in
	let em =
	  Hydro_marshal.max_enc_minor in
	let msg =
	  match resp.session # response with
	    | None -> assert false
	    | Some msg -> msg in
	let mb = 
	  Hydro_marshal.marshal_msg 
	    ep.sys `Compression_unsupported  (`Reply msg) pm em in
	let dest = trans#ctrl#getpeername in
	if dlog_enabled() then
	  dlog "Hydro_endpoint: start_writing";
        trans # ctrl # start_writing
          ~when_done:(fun r -> handle_outgoing_message ep r)
	  mb
          dest;
	!check_for_input ep

    | None ->
	next_outgoing_call ep trans


and next_outgoing_call ep trans =
  let call_opt =
    try Some(Queue.take ep.wait_queue) with Queue.Empty -> None in

  match call_opt with
    | Some call when call.state = Waiting ->
	(* Remove it also from the wait_map, if there is a xid. Add it
           to the pending calls.
	 *)
	( match xidopt_of_call call with
	    | None -> ()
	    | Some xid ->
		ep.wait_map <- SessionMap.remove xid ep.wait_map;
		ep.pending  <- SessionMap.add xid call ep.pending;
		call.state  <- Pending;
		set_call_timeout ep call;
	);
	(* Send the message: *)
	let pm =
	  min Hydro_marshal.max_proto_minor (max_proto_minor ep trans) in
	let em =
	  min Hydro_marshal.max_enc_minor (max_enc_minor ep trans) in
	let mb = 
	  Hydro_marshal.marshal_msg 
	    ep.sys `Compression_unsupported  (call.msg :> msg) pm em in
	let dest =
	  match call.destination with
	    | Some d -> `Sockaddr d
	    | None -> trans#ctrl#getpeername in
	if dlog_enabled() then
	  dlog "Hydro_endpoint: start_writing";
        trans # ctrl # start_writing
          ~when_done:(fun r ->
                        handle_outgoing_message ep r)
	  mb
          dest;
	!check_for_input ep

    | Some _ ->
	(* The call was cancelled before issued! *)
	if dlog_enabled() then
	  dlog "Hydro_endpoint: found cancelled call"
	    
    | None -> 
	()

and send_close_msg ep trans =
  let pm =
    min Hydro_marshal.max_proto_minor (max_proto_minor ep trans) in
  let em =
    min Hydro_marshal.max_enc_minor (max_enc_minor ep trans) in
  let mb = 
    Hydro_marshal.marshal_msg ep.sys `Compression_unsupported `Close_connection pm em in
  let dest = trans#ctrl#getpeername in
  if dlog_enabled() then
    dlog "Hydro_endpoint: start_writing close msg";
  trans # ctrl # start_writing
    ~when_done:(fun r -> handle_close_msg ep trans r)
    mb
    dest


and handle_close_msg ep trans r =
  (* Called after a close message has been sent by the transporter *)
  match r with
    | `Error err ->
	abort_with (Some (`Error err)) ep

    | `Ok () ->
	if dlog_enabled() then
	  dlog "Hydro_endpoint: close msg writing finished";
	ep.ep_state <- Closing2;
	!check_for_output ep


and send_eof ep trans =
  if dlog_enabled() then
    dlog "Hydro_endpoint: start_writing EOF";
  trans#ctrl#start_writing_eof
    ~when_done:(fun r -> handle_out_eof ep trans r)
    ()


and handle_out_eof ep trans r =
  (* Called after an EOF has been sent by the transporter *)
  match r with
    | `Error err ->
	abort_with (Some (`Error err)) ep

    | `Ok () ->
	if dlog_enabled() then
	  dlog "Hydro_endpoint: EOF sent";
	ep.ep_state <- Closing3;
	!check_for_input ep

	  (* No more next message! *)
;;


check_for_output := next_outgoing_message ;;


  (*****)

let default_client_params =
  Hydro_params.client_params()

let new_t sys esys =
  { ci = (object end);
    sys = sys;
    trans = None;
    esys = esys;
    ep_state = Init;
    client_enabled = false;
    server_enabled = false;
    wait_queue = Queue.create();
    wait_map = SessionMap.empty;
    pending = SessionMap.empty;
    abort_queue = Queue.create();
    next_xid = 1l;
    client_params = None;
    max_proto_minor = 0;  (* be conservative *)
    max_enc_minor = 0;    (* be conservative *)
    shutdown_when_idle = false;
    idle_timeout_group = None;
    shutdown_timeout_group = None;
    pool_callback = None;
    graceful = true;
    srv_response_queue = Queue.create();
    adapters = [];
    server_params = None;
    server_endpoint = None;
    onabort = (fun _ -> ())
  }


let create_client sys conn esys =
  let cl = new_t sys esys in
  cl.client_enabled <- true;
  cl.client_params <- Some default_client_params;

  let ept = Hydro_connector.client_endpoint_type conn in
  let tp = Hydro_connector.get_transporter ept in

  (* Start connecting a bit later: *)
  let cg = Unixqueue.new_group esys in
  Unixqueue.once esys cg 0.0
    (fun () ->
       if cl.ep_state = Init then (
	 (* i.e. not shut down in the meantime *)

	 let ce = tp # client_connect_engine conn esys in
	 cl.ep_state <- Connecting ce;
	 
	 (* Now somebody might have configured the client for a trans_timeout: *)
	 if (client_params cl)#trans_timeout >= 0.0 then (
	   Unixqueue.once esys cg (client_params cl)#trans_timeout
	     (fun () -> 
		abort_with
		  (Some `Connect_timeout)
		  cl
	     )
	 );
	 
	 Uq_engines.when_state
	   ~is_done:(fun c ->
		       if dlog_enabled() then
			 dlog "Hydro_endpoint: connected";
		       Unixqueue.clear esys cg;
		       match cl.ep_state with
			 | Connecting _ ->
			     cl.trans <- Some c;
			     if (client_params cl)#trans_timeout >= 0.0 then (
			       if dlog_enabled() then
				 dlog "Hydro_endpoint: setting trans_timeout";
			       c # ctrl # set_timeout 
				 ~notify:(fun () -> 
					    abort_with (Some `Transport_timeout) cl
					 )
				 (client_params cl)#trans_timeout;
			     );
			     ( match c#ctrl#transport_protocol_type with
				 | `Stream ->
				     cl.ep_state <- Validating;
				     !check_for_input cl
				 | `Datagram ->
				     cl.ep_state <- Up;
				     !check_for_input cl;
				     !check_for_output cl
			     )
			 | Down -> 
			     ()  (* strange *)
			 | _ ->
			     assert false
		    )
	   ~is_error:(fun err ->
			Unixqueue.clear esys cg;
			abort_with (Some (`Connect_error err)) cl;
		     )
	   ~is_aborted:(fun () ->
			  Unixqueue.clear esys cg
		       )
	   ce;
       )
    );

  cl


let configure_client cl params =
  if cl.pool_callback <> None then
    failwith "Hydro_endpoint.configure: Cannot reconfigure pool clients";
  cl.client_params <- Some params


let pool_connect cl f =
  if cl.pool_callback <> None then
    failwith "Hydro_endpoint.pool_connect: already pool member";
  cl.pool_callback <- Some f  


  (*****)

let create_server ?(onabort = fun _ -> ()) sys ept fd sp esys =
  let srv = new_t sys esys in
  srv.server_enabled <- true;

  let tp = Hydro_connector.get_transporter ept in

  let esp = tp # server_endpoint fd sp in
  srv.server_params <- Some sp;
  srv.server_endpoint <- Some esp;
  srv.onabort <- onabort;

  (* Start validating a bit later: *)
  let cg = Unixqueue.new_group esys in
  Unixqueue.once esys cg 0.0
    (fun () ->
       let conn_eng = tp # server_multiplex_connection fd esys in
       Uq_engines.when_state
	 ~is_done:(fun conn ->
		     if dlog_enabled() then
		       dlog "Hydro_endpoint: established";
		     srv.trans <- Some conn;

		     if srv.ep_state = Init then (
		       (* i.e. not shut down in the meantime *)

		       if (server_params srv)#trans_timeout >= 0.0 then (
			 conn # ctrl # set_timeout
			   ~notify:(fun () -> 
				      abort_with
					(Some `Transport_timeout) srv
				   )
			   (server_params srv)#trans_timeout
		       );

		       (* We send out the `Validate_connection message, and
                          then transition to Up. During that process, we
                          don't look for incoming messages!
			*)
		       let pm =
			 Hydro_marshal.max_proto_minor in
		       let em =
			 Hydro_marshal.max_enc_minor in
		       srv.max_proto_minor <- pm;
		       srv.max_enc_minor <- em;
		       let mb = 
			 Hydro_marshal.marshal_msg srv.sys
			   `Compression_unsupported
			   `Validate_connection pm em in
		       let dest = conn#ctrl#getpeername in
		       if dlog_enabled() then
			 dlog "Hydro_endpoint: start_writing validate_connection msg";
		       conn # ctrl # start_writing
			 ~when_done:(fun r -> 
				       match r with
					 | `Error err ->
					     abort_with (Some (`Error err)) srv
					       
					 | `Ok () ->
					     if dlog_enabled() then
					       dlog "Hydro_endpoint: validate_connection msg writing finished";
					     srv.ep_state <- Up;
					     !check_for_input srv;
				    )
			 mb
			 dest
		     )
		  )
	 ~is_error:(fun err ->
		      Unixqueue.clear esys cg;
		      abort_with (Some (`Error err)) srv;
		   )
	 ~is_aborted:(fun () ->
			Unixqueue.clear esys cg
		     )
	 conn_eng;
    );


  srv


let bind_adapter ep od =
  ep.adapters <- od :: ep.adapters

let unbind_adapter ep od =
  ep.adapters <- List.filter (fun od' -> od <> od') ep.adapters

let adapters ep =
  ep.adapters


  (*****)

module GeneralizedEndpoint = struct
  type +'kind t = ep
  type kind = [ `ClientRole | `ServerRole ]
end


module Client = struct
  (* The interface for clients *)

  type t = ep

  let create = create_client
  let configure = configure_client

  type response = client_response

  let twoway_call = twoway_call
  let queued_requests = queued_requests
  let is_up = is_up
  let is_idle = is_idle
  let client_id = endpoint_id
  let shutdown = shutdown
  let shutdown_when_idle = shutdown_when_idle
  let abort = abort
  let graceful = graceful
  let pool_connect = pool_connect

end


module Server = struct
  type t = ep

  let create = create_server
  let endpoint = server_endpoint
  let bind_adapter = bind_adapter
  let unbind_adapter = unbind_adapter
  let adapters = adapters
  let shutdown ep = shutdown ep
  let abort = abort
  let server_id = endpoint_id
end

  (*****)

module Master = struct
  type t =
      { mutable is_up : bool;
	mutable epname : endpoint option;
	mutable adapters : object_dispatcher list;
	mutable servers : (int,Server.t) Hashtbl.t;
	mutable mdescr : descriptor option;
	mutable acc_eng : descriptor Uq_engines.engine option;
	mutable cr_eng : descriptor Uq_engines.engine option;
      }


  let create sys mc params esys =
    let ept = Hydro_connector.master_endpoint_type mc in
    let tp = Hydro_connector.get_transporter ept in
    let msrv =
      { is_up = true;
	epname = None;
	adapters = [];
	servers = Hashtbl.create 10;
	mdescr = None;
	acc_eng = None;
	cr_eng = None
      } in


    let onabort srv =
      Hashtbl.remove msrv.servers (Server.server_id srv)
    in

    let start_server descr =
      let srv = 
	Server.create ~onabort sys ept descr params esys in
      List.iter
	(fun oa -> Server.bind_adapter srv oa)
	msrv.adapters;
      Hashtbl.replace msrv.servers (Server.server_id srv) srv
    in

    let rec accept_loop master_descr =
      let acc_eng = tp # server_accept_engine master_descr esys in
      Uq_engines.when_state
	~is_done:(fun conn_descr ->
		    msrv.acc_eng <- None;
		    start_server conn_descr;
		    accept_loop master_descr
		 )
	~is_error:(fun err ->
		     msrv.acc_eng <- None;
		     let g = Unixqueue.new_group esys in
		     Unixqueue.once esys g 0.0
		       (fun () -> raise err);
		     accept_loop master_descr
		  )
	~is_aborted:(fun () -> msrv.acc_eng <- None)
	acc_eng;
      msrv.acc_eng <- Some acc_eng
    in

    let cg = Unixqueue.new_group esys in
    Unixqueue.once esys cg 0.0
      (fun () ->
	 if msrv.is_up then (
	   let cr_eng = tp # server_create_engine mc esys in
	   Uq_engines.when_state
	     ~is_done:(fun master_descr ->
			 let ep = tp # server_endpoint master_descr params in
			 msrv.epname <- Some ep;
			 msrv.cr_eng <- None;
			 
			 if master_descr#is_master then (
			   (* Accept connections in a loop: *)
			   msrv.mdescr <- Some master_descr;
			   accept_loop master_descr
			 )
			 else
			   (* This is already the socket we are supposed to
                              use for serving!
                            *)
			   start_server master_descr
		      )
	     ~is_error:(fun err -> msrv.cr_eng <- None; raise err)
	     ~is_aborted:(fun () -> msrv.cr_eng <- None)
	     cr_eng;
	   msrv.cr_eng <- Some cr_eng
	 )
      );
    msrv

  let shutdown msrv =
    if msrv.is_up then (
      msrv.is_up <- false;
      ( match msrv.cr_eng with
	  | Some e -> e#abort()
	  | None -> ()
      );
      ( match msrv.acc_eng with
	  | Some e -> e#abort()
	  | None -> ()
      );
      ( match msrv.mdescr with
	  | Some d -> d#shutdown()
	  | None -> ()
      );
      msrv.mdescr <- None;
      Hashtbl.iter
	(fun _ srv -> Server.shutdown srv)
	msrv.servers;
      Hashtbl.clear msrv.servers
    )

  let bind_adapter msrv od =
    msrv.adapters <- od :: msrv.adapters

  let unbind_adapter msrv od =
    msrv.adapters <- List.filter (fun od' -> od <> od') msrv.adapters

  let adapters msrv =
    msrv.adapters

end
