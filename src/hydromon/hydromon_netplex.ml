open Hydromon_proto
open Hydromon_util
open Hydro_types
open Printf

type config =
    { state_file : string;
      shm_name : string;
      shm_size : int;
      ping_period : float;
      ping_timeout : float;
    }

type monobj =
    { key : key;
      mutable end_ts : int64;
      mutable shm_idx : int;
      mutable dead_pings : int
    }


type ba =
    (char,Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array1.t

type state =
    { config : config;
      monobjs : (key, monobj) Hashtbl.t;
      shm : ba;
      shm_used : bool array;    (* whether shm positions are used *)
    }


let find_shm_idx st =
  let rec find i =
    if i < Array.length st.shm_used then (
      if st.shm_used.(i) then
	find (i+1)
      else
	Some i
    )
    else
      None in
  find 0


let open_shm config =
  let fd = 
    Netsys.shm_open
      config.shm_name [Netsys.SHM_O_RDWR; Netsys.SHM_O_CREAT] 0o666 in
  let st = Unix.fstat fd in
  if st.Unix.st_size = 0 then
    Unix.ftruncate fd config.shm_size;
  let shm =
    Bigarray.Array1.map_file 
      fd Bigarray.char Bigarray.c_layout true config.shm_size in
  let shm_used =
    Array.make config.shm_size false in
  Unix.close fd;
  (shm, shm_used)


let save_state st =
  let file =
    open_out st.config.state_file in
  try
    Hashtbl.iter
      (fun key obj ->
	 fprintf file "%s %d X%s X%s X%s %b %Ld %d %d\n"
	   (Unix.string_of_inet_addr key.host)
	   key.port
	   (Netencoding.Url.encode key.id_name)
	   (Netencoding.Url.encode key.id_cat)
	   (Netencoding.Url.encode key.operation)
	   key.idempotent
	   obj.end_ts
	   obj.shm_idx
	   obj.dead_pings
      )
      st.monobjs;
    close_out file
  with
    | error ->
	close_out_noerr file;
	raise error

let un_x s =
  if s <> "" && s.[0] = 'X' then
    String.sub s 1 (String.length s - 1)
  else
    s


let load_state st =
  (* st.monobjs: must have been created
     st.shm: must exist
     st.shm_used: must be initialized with [false]
   *)
  if Sys.file_exists st.config.state_file then (
    let file =
      open_in st.config.state_file in
    try
      while true do
	let line = input_line file in
	let fields = Pcre.split line in
	match fields with
	  | [ host; port; id_name; id_cat; operation; idempotent; end_ts;
	      shm_idx; dead_pings
	    ] ->
	      let key =
		{ host = Unix.inet_addr_of_string host;
		  port = int_of_string port;
		  id_name = Netencoding.Url.decode (un_x id_name);
		  id_cat = Netencoding.Url.decode (un_x id_cat);
		  operation = Netencoding.Url.decode (un_x operation);
		  idempotent = bool_of_string idempotent
		} in
	      let idx0 = int_of_string shm_idx in
	      (* Workaround for some bug... *)
	      let idx =
		if st.shm_used.( idx0 ) then (
		  match find_shm_idx st with
		    | None ->
			failwith "Out of shm cells"
		    | Some i ->
			i
		)
		else
		  idx0 in
	      let obj =
		{ key = key;
		  end_ts = Int64.of_string end_ts;
		  shm_idx = idx;
		  dead_pings = int_of_string dead_pings
		} in
	      Hashtbl.replace st.monobjs key obj;
	      st.shm.{ obj.shm_idx } <- Char.chr (min 127 obj.dead_pings);
	      st.shm_used.( obj.shm_idx ) <- true

	  | _ ->
	      failwith "Cannot parse file"
      done;
      assert false
    with
      | End_of_file ->
	  close_in file
      | Failure msg ->
	  close_in file;
	  failwith (st.config.state_file ^ ": " ^ msg)
      | e ->
	  close_in file;
	  failwith (st.config.state_file ^ ": Exception " ^ 
		      Printexc.to_string e)
  )


let log_watching st key monobj =
  let container = Netplex_cenv.self_cont() in
  let logf level =
    Printf.ksprintf (fun s -> container # log level s) in
  let now = Int64.of_float (Unix.time()) in
  logf `Info "Watching %s remaining=%Ld deadpings=%d"
    (key_string key)
    (Int64.sub monobj.end_ts now)
    monobj.dead_pings



let log_initial_state st =
  let container = Netplex_cenv.self_cont() in
  let logf level =
    Printf.ksprintf (fun s -> container # log level s) in
  logf `Info "Initialized Hydromon";
  let now = Int64.of_float (Unix.time()) in
  Hashtbl.iter
    (fun key monobj ->
       logf `Info "Watching %s remaining=%Ld deadpings=%d"
	 (key_string key)
	 (Int64.sub monobj.end_ts now)
	 monobj.dead_pings
    )
    st.monobjs


let log_removal key =
  let container = Netplex_cenv.self_cont() in
  let logf level =
    Printf.ksprintf (fun s -> container # log level s) in
  logf `Info "No longer watching %s" (key_string key)


let log_state_change info key =
  let container = Netplex_cenv.self_cont() in
  let logf level =
    Printf.ksprintf (fun s -> container # log level s) in
  logf `Info "Change %s for %s" info (key_string key)


let remove_obj st key = 
  try
    let obj = Hashtbl.find st.monobjs key in (* or Not_found *)
    st.shm_used.( obj.shm_idx ) <- false;
    Hashtbl.remove st.monobjs key;
    log_removal key;
    save_state st
  with
    | Not_found -> ()


let sys_lz = lazy (
  Hydro_lm.create_system()
)


let check_objects esys st key =
  (* Checks object [key] continuously until the end time is reached *)
  let rec next_check() =
    let do_check =
      try
	let obj = Hashtbl.find st.monobjs key in (* or Not_found *)
	let now = Unix.gettimeofday() in
	now < Int64.to_float obj.end_ts
      with Not_found -> false in
      
    if do_check then (
      (* We intentionally create new pools and proxies for every ping.
         This enforces that a new connection is used every time.
       *)
      let obj = Hashtbl.find st.monobjs key in
      let sys = Lazy.force sys_lz in
      let cl_params = 
	Hydro_params.client_params 
	  ~msg_timeout:st.config.ping_timeout () in
      (* MAYBE: exception_handler *)
      let resolver = Hydro_proxy.proxy_resolver cl_params in
      let pool = Hydro_proxy.pool() in
      let proxy_conf = Hydro_proxy.proxy_conf () in
      let proxy_env =
	( object
	    method event_system = esys
	    method system = sys
	    method proxy_resolver = resolver
	    method client_pool = pool
	    method default_proxy_conf = proxy_conf
	  end
	) in
      let addr = addr_of_key key in
      let proxy = Hydro_proxy.proxy ~env:proxy_env ~addr () in
      let hfun = 
	( object
	    method name = key.operation
	    method mode = (if key.idempotent then `Idempotent else `Normal)
	    method in_args = [| |]
	    method in_classes = false
	    method out_args = [| |]
	    method result = TVoid
	    method out_classes = false
	  end
	) in
      let hintf = 
	( object
	    method name = "dummy"
	    method super = []
	    method elements = [ hfun ]
	  end
	) in
      let call_params = Hydro_params.call_params () in
      let t0 = Unix.gettimeofday() in
      proxy # hydro_twoway_call
	hintf
	key.operation
	[| |]
	call_params
	(fun resp ->
	   let successful =
	     resp#condition = `Success in
	   pool # trigger_shutdown();
	   let g = Unixqueue.new_group esys in
	   let old_dp = obj.dead_pings in
	   if successful then (
	     st.shm.{ obj.shm_idx } <- '\000';
	     obj.dead_pings <- 0;
	     if old_dp > 0 then
	       log_state_change "OK" key;
	   )
	   else (
	     obj.dead_pings <- obj.dead_pings + 1;
	     st.shm.{ obj.shm_idx } <- Char.chr (min 127 obj.dead_pings);
	     if old_dp = 0 then
	       log_state_change "FAIL" key;
	   );
	   let t1 = Unix.gettimeofday() in
	   let twait = max 0.1 (st.config.ping_period -. (t1 -. t0)) in
           Unixqueue.once esys g twait next_check
  	)
    )
    else
      remove_obj st key
  in
  next_check()


let mk_monitoredObject config monobj =
  let addr = addr_of_key monobj.key in
  let pr = Hydro_lm.pr_of_address addr in
  { m_monobject = Some pr;
    m_operation = monobj.key.operation;
    m_idem = monobj.key.idempotent;
    m_endTimestamp = monobj.end_ts;
    m_shmName = config.shm_name;
    m_shmIndex = Int32.of_int monobj.shm_idx;
    m_deadPings = Int32.of_int monobj.dead_pings;
  }


let error msg =
  raise(User_exception (x_Hydro_Error msg :> user_exception))

class hydromon_obj st =
object (self)
  inherit skel_Hydro_Mon

  method requestMonitoring proxy_opt operation idempotent end_ts =
    parachute
      (fun session ->
	 match proxy_opt with
	   | None ->
	       error "No proxy passed"
	   | Some proxy ->
	       let addr = Hydro_lm.Unsafe.unwrap_proxy proxy in
	       let key =
		 try key_of_addr addr operation idempotent 
		 with Failure msg -> error msg in
	       let monobj_opt =
		 try Some(Hashtbl.find st.monobjs key)
		 with Not_found -> None in
	       ( match monobj_opt with
		   | Some monobj ->
		       (* Update end_ts *)
		       monobj.end_ts <- max end_ts monobj.end_ts;
		       log_watching st key monobj;
		       save_state st;
		       let m = mk_monitoredObject st.config monobj in
		       ( object method result = m end )
		   | None ->
		       let idx =
			 match find_shm_idx st with
			   | Some i -> i
			   | None ->
			       raise(User_exception 
				       (x_Hydro_TooManyMonitoredObjects ()
					  :> user_exception)) in
		       let monobj =
			 { key = key;
			   end_ts = end_ts;
			   shm_idx = idx;
			   dead_pings = 0
			 } in
		       st.shm_used.( idx ) <- true;
		       Hashtbl.add st.monobjs key monobj;
		       log_watching st key monobj;
		       save_state st;
		       let container = Netplex_cenv.self_cont() in
		       let esys = container#event_system in
		       let g = Unixqueue.new_group esys in
		       Unixqueue.once esys g 0.0
			 (fun () -> check_objects esys st key);
		       let m = mk_monitoredObject st.config monobj in
		       ( object method result = m end )
	       )
	   
      )

  method list () =
    parachute
      (fun session ->
	 let l =
	   Array.of_list
	     (Hashtbl.fold
		(fun _ monobj acc -> 
		   mk_monitoredObject st.config monobj :: acc)
		st.monobjs
		[]
	     ) in
	 ( object method result = l end )
      )

end



let hydromon_processor config =
  let hooks = new Netplex_kit.empty_processor_hooks() in

  let shm, shm_used = open_shm config in
  let st =
    { config = config;
      monobjs = Hashtbl.create 100;
      shm = shm;
      shm_used = shm_used
    } in

  load_state st;

  let sys = Hydro_lm.create_system() in
  fill_system sys;

  let srv_list = ref [] in
  let adapter = Hydro_oa.object_adapter() in
  let params = Hydro_params.server_params ~trans_timeout:60.0 () in

  let id = 
    ( object method name = "hydromon" method category = "" end ) in
  let hydromon_obj =
    new hydromon_obj st in

  adapter # add id (hydromon_obj :> Hydro_lm.interface_base);

  ( object(self)
      inherit Netplex_kit.processor_base hooks

      method post_start_hook container =
	log_initial_state st;
	Hashtbl.iter
	  (fun key _ ->
	     check_objects container#event_system st key
	  )
	  st.monobjs

      method process ~when_done container fd proto_name =
        let esys = container # event_system in
        let descr = 
          Hydro_connector.descriptor fd false `Stream in

        let onabort srv =
          srv_list :=
            List.filter
              (fun srv' -> srv' != srv)
              !srv_list;
          let g = Unixqueue.new_group esys in
          Unixqueue.once esys g 0.0 when_done
        in
        let srv =
          Hydro_endpoint.Server.create
            ~onabort
            sys
            `TCP
            descr
            params
            esys in
        srv_list := srv :: !srv_list;
        Hydro_endpoint.Server.bind_adapter
	  srv (adapter :> Hydro_types.object_dispatcher)


      method shutdown () =
        List.iter
          (fun srv ->
             Hydro_endpoint.Server.shutdown srv)
          !srv_list;
        srv_list := []

      method supported_ptypes = [ `Multi_processing ]
    end
  )



let hydromon_factory ~name () =
  ( object
      method name = name

      method create_processor ctrlcfg cf addr =
        let state_file =
          try
            cf # string_param(cf # resolve_parameter addr "state_file")
          with
            | Not_found ->
		failwith
		  ("Missing parameter " ^ cf#print addr ^ ".state_file") in
	let shm_name =
          try
            cf # string_param(cf # resolve_parameter addr "shm_name")
          with
            | Not_found ->
		failwith
		  ("Missing parameter " ^ cf#print addr ^ ".shm_name") in
	let shm_size =
          try
            cf # int_param(cf # resolve_parameter addr "shm_size")
          with
            | Not_found ->
		failwith
		  ("Missing parameter " ^ cf#print addr ^ ".shm_size") in
	let ping_period =
          try
            cf # float_param(cf # resolve_parameter addr "ping_period")
          with
            | Not_found ->
		failwith
		  ("Missing parameter " ^ cf#print addr ^ ".ping_period") in
	let ping_timeout =
          try
            cf # float_param(cf # resolve_parameter addr "ping_timeout")
          with
            | Not_found ->
		failwith
		  ("Missing parameter " ^ cf#print addr ^ ".ping_timeout") in
	let config =
	  { state_file = state_file;
	    shm_name = shm_name;
	    shm_size = shm_size;
	    ping_period = ping_period;
	    ping_timeout = ping_timeout
	  } in
	
	hydromon_processor config
    end
  )
