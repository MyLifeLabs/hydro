(* $Id$ *)

open Hydro_types
open Hydro_util
open Hydro_dbg

type extended_proxy_addr =
    < id : identity;
      facet : string option;
      mode : proxy_mode;
      secure : bool;
      parameters : extended_proxy_parameters
    >

and extended_proxy_parameters =
    [ proxy_parameters
    | `Connectors of 
	  (Hydro_connector.client_connector * client_params) list
    ]

type shared_or_private =
    [ `Shared
    | `Private of < >
	(* The argument is the (proxy) object for which the clients
           are created.
         *)
    ]

type multiplicity =
    [ `Failover | `Concurrent of int ]


class type proxy_conf_t =
object
  method shared_connections : bool
  method multiplicity : multiplicity
  method max_reconnections : int
  method deactivation_period : float
  method resolution_period : float
  method context : (string * string) list
end

class type proxy_env_t =
object
  method event_system : Unixqueue.event_system
  method system : system
  method proxy_resolver : proxy_resolver_t
  method client_pool : pool_t
  method default_proxy_conf : proxy_conf_t 
end

and proxy_resolver_t =
object
  method resolve : 
           extended_proxy_addr -> 
           ((Hydro_connector.client_connector * 
	       client_params) list Lazy.t -> unit) ->
           proxy_env_t ->
             unit
end

and proxy_t =
object
  method hydro_env : proxy_env_t
  method hydro_id : identity
  method hydro_facet : string option
  method hydro_twoway_call : 
           hintf -> string -> value array -> call_params ->
           (Hydro_endpoint.Client.response -> unit) ->
             unit
  method hydro_set_proxy_conf : proxy_conf_t -> unit
  method hydro_proxy_conf : proxy_conf_t
  method hydro_set_monitor : (proxy_t -> managed_client_t -> bool) -> unit
  method hydro_reset : unit -> unit
  method hydro_shutdown : unit -> unit
end

and managed_client_t =
object
  method host_port : (Unix.inet_addr * int) option
  method client : Hydro_endpoint.Client.t
  method client_incarnation : int
  method available : float (* reference timestamp *) -> bool
  method deactivate : float (* duration *) -> unit
  method unused_since : float option
  method error_count : int
  method record_error : float -> unit
  method clear_error : unit -> unit
  method trigger_shutdown : ?ondown:(unit->unit) -> unit -> unit
  method abort : unit -> unit
end

and pool_t =
object
  method set_esys : Unixqueue.event_system -> unit
  method request_clients : system ->
                           shared_or_private ->
                           int ->     (* multiplicity *)
                           ( Hydro_connector.client_connector *
			     client_params
			   ) list ->
                                managed_client_t list
  method deactivate_host : Unix.inet_addr -> float -> unit
  method deactivate_port : network_port -> float -> unit
  method shutdown : unit -> unit
  method trigger_shutdown : unit -> unit
  method abort : unit -> unit
  method reset : unit -> unit
  method is_available : Unix.inet_addr -> network_port option -> 
                        shared_or_private -> float -> bool
end


type domain_resolver =
    Unixqueue.unix_event_system ->
    string ->
    (Unix.inet_addr option -> unit) ->
      unit


let default_dns_resolver esys name reply =
  let addr =
    try
      Some (Unix.inet_addr_of_string name)
    with
        Failure _ ->
          try
            let h = Unix.gethostbyname name in
            Some h.Unix.h_addr_list.(0)
          with Not_found ->
            None in
  reply addr


let proxy_supports_endpoint_type ept pm =
  let tp = Hydro_connector.get_transporter ept in
  List.mem pm (tp # proxy_modes)


let proxy_resolver ?(domain_resolver = default_dns_resolver) bcp =
  ( object
      method resolve (eps : extended_proxy_addr) callback env =
	match eps#parameters with
	  | `Endpoints el ->
	      let result_before_dns =
		List.filter
		  (fun ep ->
		     let ept = endpoint_type ep in
		     let pm = eps # mode in
		     proxy_supports_endpoint_type ept pm &&
		       (* Throw out endpoints with wrong major versions *)
		       ( match ep with
			   | `UDP udp ->
			       udp#proto_major = 1 && udp#enc_major = 1
			   | _ -> true
		       )
		  )
		  (Array.to_list el) in

	      let missing = ref (List.length result_before_dns) in
	      let result_after_dns =
		List.map (fun _ -> ref None) result_before_dns in

	      let do_callback() =
		let resolvable_results =
		  List.map
		    (fun res_ref ->
		       match !res_ref with
			 | Some res -> res
			 | None -> assert false
		    )
		    (List.filter
		       (fun res_ref -> !res_ref <> None) 
		       result_after_dns
		    ) in
		let result =
		  List.map
		    (fun (ep,conn) ->
		       let cl_params = 
			 Hydro_params.update_client_params_by_endpoint ep bcp in
		       (conn, cl_params)
		    ) 
		    resolvable_results in
		callback (lazy result)
	      in

	      List.iter2
		(fun ep out_ref ->
		   let host_port_opt = host_port_of_endpoint ep in
		   match host_port_opt with
		     | Some (host,_) ->
			 (* Have something to resolve *)
			 domain_resolver 
			   env#event_system
			   host
			   (fun res_opt ->
			      decr missing;
			      ( match res_opt with
				  | Some addr ->
				      out_ref := 
					Some(ep,
					     connector_of_resolved_endpoint
					       ep addr)
				  | None ->
				      ()
			      );
			      if !missing = 0 then
				do_callback()
			   )
		     | None ->
			 (* This is ok for exotic endpoint types *)
			 out_ref := Some(ep, `Endpoint(ep, None));
			 decr missing;
			 if !missing = 0 then
			   do_callback()
		)
		result_before_dns
		result_after_dns

	  | `Well_known | `Adapter _ ->
	      callback 
		(lazy
		   (failwith "Hydro_proxy.proxy_resolver: Indirect proxies not supported"))
		
	  | `Connectors cl ->
	      callback (lazy cl)
		
    end : proxy_resolver_t
  )

let shuffle rnd (base_resolver : proxy_resolver_t) =
  ( object
      method resolve eps callback env =
	base_resolver#resolve
	  eps
	  (fun l_lz ->
	     let r_lz =
	       lazy(
		 let l = Lazy.force l_lz in
		 let a = Array.of_list l in
		 for r = 1 to 3 do
		   for k = 0 to Array.length a - 1 do
		     let j = Random.State.int rnd (Array.length a) in
		     let x = a.(j) in
		     a.(j) <- a.(k);
		     a.(k) <- x;
		   done
		 done;
		 Array.to_list a
	       ) in
	     callback r_lz
	  )
	  env
    end : proxy_resolver_t
  )


let qualifies_for_pool conn =
  match conn with
    | `Endpoint _ -> true


let conn_cmp (conn1:Hydro_connector.client_connector)
             (conn2:Hydro_connector.client_connector) =
  (* Where conn1, conn2 satisfy [qualifies_for_pool] *)
  match conn1 with
    | `Endpoint (e1,_) ->
	( match conn2 with
	    | `Endpoint (e2,_) ->
		endpoint_cmp e1 e2
		  (* CHECK: is it safe to ignore the resolved IP address in 
                     the comparison?
                   *)
	)



module CP = struct
  type t = Hydro_connector.client_connector * client_params
  let compare : t -> t -> int =
    fun (conn1,cp1) (conn2,cp2) ->
      match conn_cmp conn1 conn2 with
	| 0 -> Hydro_params.client_params_cmp cp1 cp2
	| n -> n
end

module CPMap = Map.Make(CP)


module SSP = struct
  type t = system * shared_or_private
  let compare (sys1,sop1) (sys2,sop2) =
    match Oo.id sys1 - Oo.id sys2 with
      | 0 ->
	  ( match sop1, sop2 with
	      | `Shared, `Shared -> 0
	      | `Shared, `Private _ -> (-1)
	      | `Private _, `Shared -> 1
	      | `Private p1, `Private p2 -> Oo.id p1 - Oo.id p2
	  )
      | n -> n
end

module SSPMap = Map.Make(SSP)


let rec list_prefix n l =  (* Return the n elements at the beginning of l *)
  if n = 0 then
    []
  else 
    match l with
      | [] -> []
      | x :: l' -> x :: list_prefix (n-1) l'


let indexed_list l =          (* Attach index numbers to elements *)
  let rec loop n l =
    match l with
      | [] -> []
      | x :: l' -> (n,x) :: loop (n+1) l' in
  loop 0 l


exception ExitEventHandling

class pool() : pool_t =
object(pool)
  val mutable esys = None
    (* The current event system *)

  val mutable pool_up = true
    (* Whether pool is usable *)

  val mutable mclients = 
    (SSPMap.empty :
       managed_client_t list ref CPMap.t ref SSPMap.t
    )

  val mutable deactivated_hosts = []
  val mutable deactivated_ports = []

  val mutable t_last_gc = neg_infinity

  method set_esys new_esys =
    match esys with
      | None ->
	  esys <- Some new_esys
      | Some old_esys ->
	  if old_esys <> new_esys then
	    failwith "Hydro_proxy.pool#set_esys: Cannot switch to new event system"

  method private get_esys =
    match esys with
      | None ->
	  failwith "Hydro_proxy.pool: No event system has been set"
      | Some e ->
	  e


  method request_clients sys sp multi cp_l =
    if multi < 1 then
      invalid_arg "request_clients";
    if not pool_up then
      failwith "Hydro_proxy.pool: unavailable";
    pool # garbage_collect();
    let r = ref [] in
    let cpmap =
      try
	SSPMap.find (sys,sp) mclients
      with
	| Not_found ->
	    let cpmap = ref CPMap.empty in
	    mclients <- SSPMap.add (sys,sp) cpmap mclients;
	    cpmap in
    List.iter
      (fun (c,p) ->
	 let mcl =
	   try
	     CPMap.find (c,p) !cpmap
	   with
	     | Not_found ->
		 let mcl = ref [] in
		 cpmap := CPMap.add (c,p) mcl !cpmap;
		 mcl in
	 let mcl_len = List.length !mcl in
	 if mcl_len < multi then (
	   (* Not enough managed clients in the pool. Create new ones! *)
	   for k = 1 to multi-mcl_len do
	     let mc = pool#new_managed_client sys c p sp in
	     mcl := mc :: !mcl
	   done
	 );
	 let mcl_taken = list_prefix multi !mcl in
	 let mcl_enumerated = indexed_list mcl_taken in
	 r := mcl_enumerated :: !r
      )
      cp_l;
    let r =
      List.flatten (List.rev !r) in
    (* Sort r by index *)
    let r' =
      List.stable_sort
	(fun (n1,_) (n2,_) -> n1 - n2)
	r in
    (* Remove index *)
    List.map snd r'


  method is_available host port_opt _ tref =
    let host_available =
      try
	let t1 = List.assoc host deactivated_hosts in
	tref > t1
      with
	| Not_found -> true in
    host_available &&
      ( match port_opt with
	  | None -> true
	  | Some port ->
	      try
		let t1 = List.assoc port deactivated_ports in
		tref > t1
	      with
		| Not_found -> true
      ) 


  method private new_managed_client sys c p sp =
    let port_opt = network_port_of_connector c in
    let host_opt =
      match port_opt with
	| Some(`TCP(a,_)) | Some(`UDP(a,_)) -> Some a
	| None -> None in
    let t0 = Unix.gettimeofday() in
    ( object(managed_client) 
	val mutable mc_state = `Unused_since t0
	val mutable mc_incarnation = 0
	val mutable mc_error_info = None

	method host_port =
	  match port_opt with
	    | None -> None
	    | Some(`TCP(a,p)) | Some(`UDP(a,p)) -> Some (a,p)

	method client =
	  if not pool_up then
	    failwith "Hydro_proxy.pool: unavailable";
	  match mc_state with
	    | `Used cl ->
		assert(Hydro_endpoint.Client.is_up cl);
		cl
	    | `Unused_since _
	    | `Deactivated_until _ ->
		(* We do not refuse to create a new client if it is
                   deactivated! Deactivation is just a suggestion!
                 *)
		let esys = pool#get_esys in
		let cl = Hydro_endpoint.Client.create sys c esys in
		Hydro_endpoint.Client.configure cl p;
		Hydro_endpoint.Client.pool_connect cl managed_client#drop;
		mc_state <- `Used cl;
		mc_incarnation <- mc_incarnation + 1;
		cl

	method private drop() =
	  let t = Unix.gettimeofday() in
	  mc_state <- `Unused_since t
		
	method client_incarnation =
	  mc_incarnation

	method available tref =
	  pool_up &&
	  ( match mc_state with
	      | `Deactivated_until t ->
		  tref > t
	      | _ -> 
		  true
	  ) && 
	    ( match host_opt with
		| None -> true
		| Some host ->
		    pool # is_available host port_opt sp tref
	    )

	method deactivate duration = 
	  if duration <> 0.0 then (
	    let t1 = 
	      if duration < 0.0 then
		infinity
	      else
		Unix.gettimeofday() +. duration in
	    let t1' =
	      match mc_state with
		| `Deactivated_until t1' -> t1'
		| _ -> t1 in
	    let tmax = max t1 t1' in
	    mc_state <- `Deactivated_until tmax;
	    mc_error_info <- None;
	    if dlog_enabled() then
	      dlogf "Hydro_proxy: deactivating managed client %d until %f"
		(Oo.id (managed_client :> < > ) ) tmax
	  )

	method unused_since =
	  match mc_state with
	    | `Used _ | `Deactivated_until _ -> None
	    | `Unused_since t -> Some t

	method record_error p =
	  let now = Unix.gettimeofday() in
	  match mc_error_info with
	    | None ->
		mc_error_info <- Some(1, now +. p)
	    | Some(n,t) ->
		if now > t then
		  mc_error_info <- Some(1, now +. p)
		else
		  mc_error_info <- Some(n+1, now +. p)

	method clear_error() =
	  mc_error_info <- None

	method error_count =
	  match mc_error_info with
	    | None ->
		0
	    | Some(n,t) ->
		let now = Unix.gettimeofday() in
	  	if now > t then
		  0
		else 
		  n

	method trigger_shutdown ?ondown () =
	  if dlog_enabled() then
	    dlogf "Hydro_proxy: shutting managed client %d down"
	      (Oo.id (managed_client :> < > ) );
	  match mc_state with
	    | `Used cl ->
		Hydro_endpoint.Client.shutdown ?ondown cl
		  (* This will in turn call [drop] if the client was up *)
	    | _ -> ()

	method abort() =
	  if dlog_enabled() then
	    dlogf "Hydro_proxy: aborting client %d"
	      (Oo.id (managed_client :> < > ) );
	  match mc_state with
	    | `Used cl ->
		Hydro_endpoint.Client.abort cl
		  (* This will in turn call [drop] if the client was up *)
	    | _ -> ()

      end : managed_client_t
    )
    
  method deactivate_host host duration =
    if duration <> 0.0 then (
      let t1 = 
	if duration < 0.0 then
	  infinity
	else
	  Unix.gettimeofday() +. duration in
      let t1' =
	try List.assoc host deactivated_hosts with Not_found -> t1 in
      let l =
	List.filter (fun (h,_) -> h <> host) deactivated_hosts in
      let tmax = max t1 t1' in
      deactivated_hosts <- (host, tmax) :: l;
      if dlog_enabled() then
	dlogf "Hydro_proxy: deactivating host %s until %f" 
	  (Unix.string_of_inet_addr host) tmax
    )

  method deactivate_port port duration =
    if duration <> 0.0 then (
      let t1 = 
	if duration < 0.0 then
	  infinity
	else
	  Unix.gettimeofday() +. duration in
      let t1' =
	try List.assoc port deactivated_ports with Not_found -> t1 in
      let l =
	List.filter (fun (p,_) -> p <> port) deactivated_ports in
      let tmax = max t1 t1' in
      deactivated_ports <- (port, tmax) :: l;
      if dlog_enabled() then
	dlogf "Hydro_proxy: deactivating port %s until %f" 
	  (match port with
	     | `TCP(h,n) -> 
		 Printf.sprintf "%s:%d/tcp" (Unix.string_of_inet_addr h) n
	     | `UDP(h,n) -> 
		 Printf.sprintf "%s:%d/udp" (Unix.string_of_inet_addr h) n
	  )
	  tmax
    )

  method private garbage_collect() =
    let now = Unix.gettimeofday() in
    if t_last_gc +. 10.0 < now then (   (* Do this only every 10 secs *)
      (* Clean up mclients: *)
      let sspmap_keys_to_del = ref [] in
      SSPMap.iter
	(fun ssp cpmap ->
	   let cpmap_keys_to_del = ref [] in
	   CPMap.iter
	     (fun cp l ->
		let l' =
		  List.filter
		    (fun mc -> 
		       match mc#unused_since with
			 | None -> true
			 | Some t -> t < t_last_gc
		    )
		    !l in
		l := l';
		if l' = [] then
		  cpmap_keys_to_del := cp :: !cpmap_keys_to_del
	     )
	     !cpmap;
	   List.iter
	     (fun cp ->
		cpmap := CPMap.remove cp !cpmap)
	     !cpmap_keys_to_del;
	   if !cpmap = CPMap.empty then
	     sspmap_keys_to_del := ssp :: !sspmap_keys_to_del
	)
	mclients;
      List.iter
	(fun ssp ->
	   mclients <- SSPMap.remove ssp mclients)
	!sspmap_keys_to_del;
      (* Clean up deactivated_*: *)
      deactivated_hosts <-
	List.filter (fun (_, t) -> t > now) deactivated_hosts;
      deactivated_ports <-
	List.filter (fun (_, t) -> t > now) deactivated_ports;
      (* done *)
      t_last_gc <- now;
    )


  method trigger_shutdown() =
    if dlog_enabled() then
      dlogf "Hydro_proxy: shutting pool down";
    pool # for_all (fun mc -> mc#trigger_shutdown ?ondown:None ())


  method shutdown() =
    let old_pool_up = pool_up in
    pool_up <- false;
    let l = ref [] in
    pool # for_all (fun mc -> l := mc :: !l);
    let n = ref (List.length !l) in
    let active = ref true in
    List.iter
      (fun mc ->
	 mc # trigger_shutdown
	   ?ondown:(Some (fun () ->
			    decr n;
			    if !n = 0 && !active then raise ExitEventHandling;
			 ))
	   ()
      )
      !l;
    ( try
	Unixqueue.run pool#get_esys;
      with
	| ExitEventHandling -> ()
	| err -> active := false; raise err
    );
    active := false;
    pool_up <- old_pool_up


  method reset() =
    if dlog_enabled() then 
      dlogf "Hydro_proxy: resetting pool";
    if not pool_up then
      failwith "Hydro_pool.reset: cannot reset while shutdown is in progress";
    pool # shutdown();
    pool_up <- true;
    mclients <- SSPMap.empty;
    deactivated_hosts <- [];
    deactivated_ports <- []

  method abort() =
    if dlog_enabled() then
      dlogf "Hydro_proxy: aborting pool";
    pool # for_all (fun mc -> mc#abort())


  method private for_all f =
    SSPMap.iter
      (fun _ cpmap ->
	 CPMap.iter
	   (fun (_, _) l ->
	      List.iter f !l)
	   !cpmap
      )
      mclients

end


let pool = new pool


module Ring : sig
  type t

  val create : managed_client_t list -> t
    (* Create a ring from the list of managed clients. The first client
       is the initial ring head. The number of completed rounds is 0.
       The list must not be empty!
     *)

  val copy : t -> t
    (* Create a copy of a ring. The number of completed rounds is reset to 0
       in the copy, however. The current ring head will be the initial
       ring head in the copy.
     *)

  val head : t -> managed_client_t
    (* Return the head *)

  val index : t -> int
    (* The index of the head *)

  val round : t -> int
    (* Return the number of completed rounds *)

  val switch : t -> unit
    (* Make the successor of the current head the new head. If the new
       head is the initial ring head, the number of completed rounds is
       increased by one.
     *)

  val length : t -> int
    (* Number of ring members *)

end = struct
  type t =
      { ring : managed_client_t Queue.t;
	mutable index : int;
	mutable round : int;
      }

  let create l =
    if l = [] then
      invalid_arg "Hydro_proxy.Mclient_ring.create";
    let q = Queue.create() in
    List.iter (fun mc -> Queue.add mc q) l;
    { ring = q;
      index = 0;
      round = 0;
    }

  let copy r =
    { ring = Queue.copy r.ring;
      index = 0;
      round = 0;
    }

  let head r =
    Queue.peek r.ring

  let index r =
    r.index

  let round r =
    r.round

  let length r =
    Queue.length r.ring

  let switch r =
    let mc = Queue.take r.ring in
    Queue.add mc r.ring;
    r.index <- r.index + 1;
    if r.index = Queue.length r.ring then (
      r.index <- 0;
      r.round <- r.round + 1
    )
end


let find_available_client ring t max_rounds_opt mon =
  let start =
    match max_rounds_opt with
      | Some max_rounds -> Ring.round ring < max_rounds
      | None -> true in
  if start then (
    let mc = ref (Ring.head ring) in
    let mc_avail = ref (!mc # available t && mon !mc) in
    let n = ref 0 in
    let l = Ring.length ring in
    while 
      not !mc_avail &&                   (* mc does not qualify *)
      !n < l                             (* at most do one round now *)
    do
      Ring.switch ring;                  (* try the next ring member *)
      mc := Ring.head ring;
      mc_avail := !mc # available t && mon !mc;
      incr n;
      (* Check whether maximum number of rounds exceeded: *)
      match max_rounds_opt with
	| Some max_rounds -> 
	    if Ring.round ring >= max_rounds then ( n:= l; mc_avail := false )
	| None -> ()
    done;
    if !mc_avail then
      Some !mc
    else
      None
  )
  else None


let error_response err =
  ( object
      method condition = `Error err
      method out_args = [| |]
      method result = raise(Client_condition(`Error err))
      method addr = None
      method peer = None
      method mode = `Normal (* CHECK: may be wrong *)
      method client = None
    end : Hydro_endpoint.Client.response
  )

let internal_error =
  function
    | Invalid_argument _ -> true
    | Failure _ -> true
    | Assert_failure _ -> true
    | _ -> false


class proxy ~env ~addr () : proxy_t =
object(self)
  val mutable resolution = None
    (* If Some(res, ring, switch_flag, t):
       - res is a list of connector*client_params
       - ring is the Ring.t corresponding to res
       - switch_flag says whether to switch the ring after every call
       - t is the timestamp when the resolution was done
     *)
  val mutable proxy_up = true
    (* Whether the proxy is still up *)

  val mutable client_callbacks = Queue.create()
    (* Waiting client requests - they are appended here while a resolution
       is in progress
     *)

  val mutable proxy_conf = env#default_proxy_conf

  val mutable monitor = (fun _ _ -> true)

  initializer (
    env#client_pool#set_esys env#event_system
  )

  method hydro_env = env
  method hydro_id = addr#id
  method hydro_facet = addr#facet
  method hydro_proxy_conf = proxy_conf

  method hydro_set_proxy_conf pc =
    proxy_conf <- pc;
    resolution <- None    (* enforce that new config is used *)


  method hydro_set_monitor mon =
    monitor <- mon


  method private hydro_client callback =
    if dlog_enabled() then
      dlogf "Hydro_proxy: hydro_client 1";
    if not proxy_up then
      callback(lazy(raise(Proxy_error `ProxyIsDown)))
    else (
      (* Check whether resolution is too old *)
      ( match resolution with
	  | Some (res, ring, switch_flag, t) ->
	      let now = Unix.gettimeofday() in
	      let rp = proxy_conf#resolution_period in
	      if rp >= 0.0 && t +. rp < now then
		resolution <- None
	  | None -> ()
      );
      (* Now test resolution, or re-resolve *)
      match resolution with
	| Some (res, ring, switch_flag, t) ->
	    if dlog_enabled() then
	      dlogf "Hydro_proxy: hydro_client 2: have resolution of length %d"
		(List.length res);
	    callback(lazy(ring, switch_flag))
	| None ->
	    (* Start a new resolution only if none is in progress *)
	    if dlog_enabled() then
	      dlogf "Hydro_proxy: hydro_client 3: have to resolve";
	    let start = Queue.is_empty client_callbacks in
	    Queue.push callback client_callbacks;
	    let pool = env#client_pool in
	    let sys = env#system in
	    let sp = 
	      if proxy_conf#shared_connections then 
		`Shared
	      else
		`Private (self : #proxy_t :> < > ) in
	    let multi, switch_flag =
	      match proxy_conf#multiplicity with
		| `Failover -> (1, false)
		| `Concurrent n -> (n, true) in
	    if start then (
	      env#proxy_resolver#resolve
		addr
		(fun l_lz ->
		   let t = Unix.gettimeofday() in
		   let r_lz =
		     try
		       let res = Lazy.force l_lz in (* may raise exn! *)
		       let success =
			 if res = [] then (
			   if dlog_enabled() then
			     dlogf "Hydro_proxy: hydro_client 4: no resolution";
			   false
			 )
			 else (
			   if dlog_enabled() then
			     dlogf "Hydro_proxy: hydro_client 5: got resolution of length %d" (List.length res);
			   true
			 ) in
		       if success then (
			 let mclients = 
			   pool # request_clients sys sp multi res in
			 let ring = Ring.create mclients in
			 resolution <- Some (res, ring, switch_flag, t);
			 lazy (ring, switch_flag)
		       )
		       else
			 lazy (raise (Proxy_error `NoCallableEndpointFound))
		     with
		       | err ->
			   if dlog_enabled() then
			     dlogf 
			       "Hydro_proxy: hydro_client 6: resolution fails: %s"
			       (Hydro_util.exn_to_string err);
			   ( match err with
			       | Client_condition(`Error(Proxy_error _))
			       | Proxy_error _ ->
				   lazy(raise(Proxy_error `NoLocatorIsReachable))
			       | _ ->
				   lazy (raise err)
			   )
		   in

		   let q = Queue.create() in
		   Queue.transfer client_callbacks q;
		   let g = Unixqueue.new_group env#event_system in
		   if dlog_enabled() then
		     dlogf "Hydro_proxy: hydro_client 7: notifying";
		   Queue.iter
		     (fun cb ->
			(* Deliver it through [once] because this ensures
                             a reasonable behaviour in case of exceptions
                         *)
			Unixqueue.once env#event_system g 0.0
			  (fun () -> cb r_lz)
		     )
		     q
		)
		env
	    )
    )

  method hydro_reset() =
    if dlog_enabled() then
      dlogf "Hydro_proxy: hydro_reset";
    resolution <- None;  (* may have changed! *)

  method hydro_shutdown() =
    if dlog_enabled() then
      dlogf "Hydro_proxy: hydro_shutdown";
    if proxy_up then
      proxy_up <- false


  method hydro_twoway_call intf name args params cb =
    if dlog_enabled() then
      dlogf "Hydro_proxy: hydro_twoway_call name=%s" name;

    let params =
      (* Set the context, but only if the user hasn't set it already *)
      if params#context = None then
	Hydro_params.update_call_params
	  ~context:proxy_conf#context
	  params
      else
	params in

    self # hydro_client
      (fun r_lz ->
	 let r_opt = 
	   try
	     Some(Lazy.force r_lz)
	   with
	     | Proxy_error _ as err ->
		 cb (error_response err);
		 None
	 in
	 match r_opt with
	   | None ->
	       if dlog_enabled() then
		 dlogf "Hydro_proxy: no clients for calling name=%s" name;
	       let err = Proxy_error `NoCallableEndpointFound in
	       cb (error_response err)
	   | Some(ring,switch_flag) ->
	       if dlog_enabled() then
		 dlogf "Hydro_proxy: got ring of clients for calling name=%s" name;
	       (* Create a copy of the ring of clients, and use the copy
                  for managing the connections for this call.
                *)
	       let call_ring = Ring.copy ring in

	       (* If switch_flag is set, we advance [ring] to the next
                  available client. 
                *)
	       if switch_flag then (
		 if dlog_enabled() then
		   dlog "Hydro_proxy: Switching master ring";
		 let now = Unix.gettimeofday() in
		 Ring.switch ring;
		 let mon _ = true in
		 ignore(find_available_client ring now None mon)
	       );

	       (* Maximum number of rounds in [call_ring] *)
	       let mr = max 1 (proxy_conf#max_reconnections + 1) in

	       self # hydro_twoway_ring_call 
		 intf name args params cb call_ring mr
      )

		 
  method private hydro_twoway_ring_call intf name args params cb call_ring mr =
    let on_connect_error mc is_last_round =
      if dlog_enabled() then
	dlogf "Hydro_proxy: on_connect_error name=%s" name;
      mc#record_error (proxy_conf#deactivation_period);
      if mc#error_count >= mr then
	mc # deactivate (proxy_conf # deactivation_period);
      Ring.switch call_ring;
      self#hydro_twoway_ring_call intf name args params cb call_ring mr
    in

    let on_hard_error mc resp is_last_round =
      if dlog_enabled() then
	dlogf "Hydro_proxy: on_hard_error name=%s" name;
      mc#record_error (proxy_conf#deactivation_period);
      if mc#error_count >= mr then
	mc # deactivate (proxy_conf # deactivation_period);
      if resp#mode = `Idempotent then (
	(* Idempotent RPCs can always be repeated *)
	Ring.switch call_ring;
	self#hydro_twoway_ring_call intf name args params cb call_ring mr
      )
      else
	(* Give up: *)
	cb resp
    in

    let on_internal_error mc resp is_last_round =
      if dlog_enabled() then
	dlogf "Hydro_proxy: on_internal_error name=%s" name;
      mc#record_error (proxy_conf#deactivation_period);
      if mc#error_count >= mr then
	mc # deactivate (proxy_conf # deactivation_period);
      cb resp
    in

    if dlog_enabled() then
      dlogf "Hydro_proxy: hydro_twoway_ring_call name=%s ring.index=%d ring.round=%d" name (Ring.index call_ring) (Ring.round call_ring);
    
    let now = Unix.gettimeofday() in
    (* Check whether the ring head is an available client. If not, switch
       to the next available client. Return [None] if no client can be
       found, or the maximum number of rounds is reached
     *)
    let mon = monitor (self :> proxy_t) in
    match find_available_client call_ring now (Some mr) mon with
      | Some mc ->
	  let cl = mc#client in
	  if dlog_enabled() then
	    dlogf "Hydro_proxy: Using managed client %d incarnation %d"
	      (Oo.id mc) mc#client_incarnation;
	  let is_last_round = 
	    Ring.round call_ring + 1 >= mr in
	  Hydro_endpoint.Client.twoway_call
	    ?facet:addr#facet
	    cl
	    addr#id
	    intf
	    name
	    params
	    args
	    (fun resp -> 
	       match resp#condition with
		 | `Success
		 | `User_exception _
		 | `Object_does_not_exist _
		 | `Facet_does_not_exist _
		 | `Operation_does_not_exist _
		 | `Unknown_local_exception _
		 | `Unknown_user_exception _
		 | `Unknown_exception _ ->
		     (* These conditions indicate that the connection
                        could be established
		      *)
		     if dlog_enabled() then
		       dlogf "Hydro_proxy: got response for name=%s" name;
		     mc#clear_error();
		     cb resp

		 | `Connect_timeout
		 | `Message_lost false ->
		     on_connect_error mc is_last_round
		 | `Connect_error e  when not (internal_error e) ->
		     on_connect_error mc is_last_round
		       (* These conditions indicate that the connection
                          could not be established
			*)
		       
		 | `Error e when not (internal_error e) ->
		     on_hard_error mc resp is_last_round
		 | `Message_lost true
		 | `Message_timeout
		 | `Transport_timeout
		 | `Client_is_down  ->
		     (* All other conditions are in-between (e.g. broken
                        connection)
		      *)
		     on_hard_error mc resp is_last_round

		 | _ ->
		     (* Internal error: tell the user *)
		     on_internal_error mc resp is_last_round
	    )
      | None ->
	  if dlog_enabled() then
	    dlog "Hydro_proxy: no endpoint found in ring";
	  let err = Proxy_error `NoEndpointIsReachable in
	  cb (error_response err);
end


let proxy = new proxy

let proxy_conf
      ?(shared_connections=true)
      ?(multiplicity=`Failover) 
      ?(max_reconnections = 2)
      ?(deactivation_period = 60.0)
      ?(resolution_period = 300.0)
      ?(context = [])
      () =
  ( object
      method shared_connections = shared_connections
      method multiplicity = multiplicity
      method max_reconnections = max_reconnections
      method deactivation_period = deactivation_period
      method resolution_period = resolution_period
      method context = context
    end : proxy_conf_t
  )


let modify_proxy_conf_1
      (pc : proxy_conf_t)
      ?(shared_connections = pc#shared_connections)
      ?(multiplicity = pc#multiplicity)
      ?(max_reconnections = pc#max_reconnections)
      ?(deactivation_period = pc#deactivation_period)
      ?(resolution_period = pc#resolution_period)
      ?(context = pc#context)
      () =
  ( object
      method shared_connections = shared_connections
      method multiplicity = multiplicity
      method max_reconnections = max_reconnections
      method deactivation_period = deactivation_period
      method resolution_period = resolution_period
      method context = context
    end : proxy_conf_t
  )


let modify_proxy_conf
      ?shared_connections
      ?multiplicity
      ?max_reconnections
      ?deactivation_period
      ?resolution_period
      ?context
      (pc : proxy_conf_t) =
  modify_proxy_conf_1
    pc
    ?shared_connections
    ?multiplicity
    ?max_reconnections
    ?deactivation_period
    ?resolution_period
    ?context
    ()


class proxy_delegation (p:proxy_t) =
object
  method hydro_env = p#hydro_env
  method hydro_id = p#hydro_id
  method hydro_facet = p#hydro_facet
  method hydro_twoway_call = p#hydro_twoway_call
  method hydro_shutdown = p#hydro_shutdown
  method hydro_reset = p#hydro_reset
  method hydro_proxy_conf = p#hydro_proxy_conf
  method hydro_set_proxy_conf = p#hydro_set_proxy_conf
  method hydro_set_monitor = p#hydro_set_monitor
end


class proxy_env_delegation (e:proxy_env_t) =
object
  method event_system = e#event_system
  method system = e#system
  method proxy_resolver = e#proxy_resolver
  method client_pool = e#client_pool
  method default_proxy_conf = e#default_proxy_conf
end


class proxy_resolver_delegation (r:proxy_resolver_t) =
object
  method resolve = r#resolve
end


class pool_delegation (p:pool_t) =
object
  method set_esys = p#set_esys
  method request_clients = p#request_clients
  method deactivate_host = p#deactivate_host
  method deactivate_port = p#deactivate_port
  method shutdown = p#shutdown
  method trigger_shutdown = p#trigger_shutdown
  method abort = p#abort
  method reset = p#reset
  method is_available = p#is_available
end
