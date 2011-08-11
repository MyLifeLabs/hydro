(* $Id$ *)

open Netplex_types

(* Semaphore implementation: This impl. is quite problematic, but the best
   we can do without explicit Netplex support. Once Netplex implements 
   semaphores itself, we should better use that

   Problem: When containers crash, the semaphore isn't decreased
 *)

class semaphore =
object(self)
  val mutable counter_filename = lazy(assert false)
  val mutable initialized = false

  method init (socksrv : socket_service) (ctrl : controller) =
    (* Tell the semaphore what the controller is. Can be called several times.
       (From pre_start_hook)
     *)
    if not initialized then (
      let dir =
	ctrl # controller_config # socket_directory in
      let file =
	Filename.concat dir (socksrv#name ^ ".sem") in
      let f =
	open_out file in
      output_string f "0\n";
      close_out f;
      counter_filename <- lazy file;
      initialized <- true
    )

  method modify (f : int -> int) : int =
    (* Return the current value of the counter. Set the counter to the new
       value by applying [f]
     *)
    let file = Lazy.force counter_filename in
    let fd = Unix.openfile file [ Unix.O_RDWR ] 0 in
    Netsys.restart
      (fun () ->
	 Unix.lockf fd Unix.F_LOCK 0
      ) ();
    let f_in = Unix.in_channel_of_descr fd in
    let n = int_of_string(input_line f_in) in
    ignore(Unix.lseek fd 0 Unix.SEEK_SET);
    let f_out = Unix.out_channel_of_descr fd in
    output_string f_out (string_of_int (f n));
    flush f_out;
    Unix.close fd;   (* also releases lock *)
    n
      
  method incr = self#modify succ
  method decr = self#modify pred

end




let hydro_factory 
      ~configure
      ?(adapters = 0)
      ?(hooks = fun _ _ -> new Netplex_kit.empty_processor_hooks())
      ?(supported_ptypes = [ `Multi_processing ])
      ?(setup = fun _ _ _ _ -> ())
      ?register_at
      ?(server_params = Hydro_params.server_params())
      ~name
      ~sys
      () =

  ( object(self)
      method name = name
      method create_processor ctrl_cfg cf addr =
	let aa =
	  Array.init adapters (fun _ -> Hydro_oa.object_adapter()) in
	let od_lst =
	  (Array.to_list aa :> Hydro_types.object_dispatcher list) in

	let trans_timeout =
	  try
	    Some(cf # float_param(cf # resolve_parameter addr "timeout"))
	  with
	    | Not_found -> None
	    | _ -> failwith("Cannot parse " ^ cf#print addr ^ ".timeout")
	in

	let reg =
	  try
	    let reg_str =
	      cf # string_param(cf # resolve_parameter addr "register_at") in
	    let addr = Hydro_string.proxy_addr_of_string reg_str in
	    Some(Hydro_builtin.unchecked_pr_Ice_Locator
		   (Hydro_lm.pr_of_address addr))
	  with
	    | Not_found -> register_at
	    | _ -> failwith("Cannot parse " ^ cf#print addr ^ ".register_at")
	in

	let reg_hostname =
	  try
	    let str =
	      cf # string_param
		(cf # resolve_parameter addr "register_hostname") in
	    ( match str with
		| "<hostname>" ->
		    `String (Unix.gethostname())
		| "<ping>" ->
		    `Ping
		| "<sockaddr_or_ping>" ->
		    `Sockaddr_or_ping 
		| _ ->
		    `String str
	    )
	  with
	    | Not_found -> `Sockaddr_or_ping 
	    | _ -> failwith("Cannot parse " ^ cf#print addr ^
			      ".register_hostname") in

	let addr_adapters = cf#resolve_section addr "adapter" in
	let i = ref 0 in
	List.iter
	  (fun addr_adp ->
	     if !i >= Array.length aa then
	       failwith("Too many adapters in " ^ cf#print addr);
	     let adp_id_opt =
	       try
		 Some(cf # string_param
			(cf # resolve_parameter addr_adp "adapter_id"))
	       with Not_found -> None in
	     let rg_id_opt =
	       try
		 Some(cf # string_param
			(cf # resolve_parameter addr_adp "replica_group_id"))
	       with Not_found -> None in
	     let oa = aa.(!i) in
	     oa # set_adapter_id adp_id_opt;
	     oa # set_replica_group_id rg_id_opt;
	     incr i
	  )
	  addr_adapters;

	let custom_cfg = configure cf addr aa in

	let srv_list = ref [] in

	let sem = new semaphore in

	let params = 
	  Hydro_params.update_server_params 
	    ?trans_timeout server_params in
	
	( object(self)
	    inherit Netplex_kit.processor_base (hooks aa custom_cfg) as super

	    method pre_start_hook socksrv ctrl contid =
	      if reg <> None then
		sem # init socksrv ctrl;
	      super # pre_start_hook socksrv ctrl contid 

	    method post_start_hook cont =
	      ( match reg with
		  | None -> ()
		  | Some loc ->
		      let n = sem # incr in
		      if n = 0 then (
			(* First started container, so register: *)
			let socksrv = cont # socket_service in
			(* Restriction: only first file descr is registered *)
			let fd = (snd (List.hd socksrv#sockets)).(0) in
			let addr = Unix.getsockname fd in
			let ep = 
			  Hydro_connector.tcp_endpoint_of_file_descr
			    addr params#trans_timeout false in
			let host, dynamic_ip =
			  match reg_hostname with
			    | `String s -> (s, false)
			    | `Ping -> ("dummy", true)
			    | `Sockaddr_or_ping ->
				let is_ip_any =
				  match addr with
				    | Unix.ADDR_INET(ip,_)->
					( ip = Unix.inet_addr_any ||
					    ip = Unix.inet6_addr_any )
				    | _ -> false in
				if is_ip_any then
				  ("dummy", true)
				else
				  (ep#host, false) in
			let ep' =
			  Hydro_util.replace_host host (`TCP ep) in
			Hydro_locator.register_adapters
			  ~dynamic_ip loc od_lst ep'
		      )
	      );
	      super # post_start_hook cont

	    method pre_finish_hook cont =
	      (* TODO: Move this to service shutdown hook, once it is added
                 to Netplex
	       *)
	      ( match reg with
		  | None -> ()
		  | Some loc ->
		      let n = sem # decr in
		      if n = 1 then (
			(* Last container is being finished *)
			Hydro_locator.unregister_adapters loc od_lst
		      )
	      );
	      super # pre_finish_hook cont

	    method shutdown () =
	      List.iter
		(fun srv ->
		   Hydro_endpoint.Server.shutdown srv)
		!srv_list;
	      srv_list := [];
	      super # shutdown()
		

	    method process ~when_done container fd proto =
	      let esys = container # event_system in
	      let descr = 
		Hydro_connector.descriptor fd false `Stream in

	      let onabort srv =
		(* FIXME: using the onabort hook might be wrong if we allow
                   SSL, because it is called too early - when the logical
                   server circuit is aborted, and not when the last bits of
                   the connection are torn down. For TCP this is the same,
                   but for SSL the connection shutdown takes a bit longer
                   than the server lives
                 *)
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
	      List.iter (Hydro_endpoint.Server.bind_adapter srv) od_lst;
	      setup container srv aa custom_cfg

	    method supported_ptypes = 
	      supported_ptypes

	  end
	)
    end
  )
;;
