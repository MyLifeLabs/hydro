open Hydro_types
open Hydro_builtin

(*
let scan_indirect_proxy s =
  try
    let k = String.index s '@' in
    `Adapter(String.sub s 0 k, String.sub s (k+1) (String.length s - k - 1))
  with
    | Not_found ->
	`WellKnownObject s
 *)

let proxy_resolver ?domain_resolver cp pr_loc =
  let base_resolver =
    Hydro_proxy.proxy_resolver ?domain_resolver cp in
  ( object(self)
      method resolve eps emit env =
	match eps#parameters with
	  | `Endpoints _ | `Connectors _ ->
	      (* Use base_resolver: *)
	      base_resolver # resolve eps emit env
	  | `Well_known ->
	      let env' =
		Hydro_builtin_util.derive_proxy_env base_resolver env in
	      let po =
		pc_Ice_Locator env' pr_loc in
	      let id =
		eps#id in
	      (po # findObjectById id)
		# acall (self#process_reply emit env)
	  | `Adapter adapter_id ->
	      let env' =
		Hydro_builtin_util.derive_proxy_env base_resolver env in
	      let po =
		pc_Ice_Locator env' pr_loc in
	      (po # findAdapterById adapter_id)
		# acall (self#process_reply emit env)

      method private process_reply emit env r =
	try
	  let found_pr =
	    match r#result with
	      | Some pr -> pr
	      | None -> (* TODO *)
		  failwith "Inactive object/adapter" in
	  let eps' =
	    (Hydro_lm.Unsafe.unwrap_proxy found_pr :>
	       Hydro_proxy.extended_proxy_addr ) in
	  base_resolver # resolve eps' emit env
	with
	  | Hydro_builtin.User_exception ue
	      when ue#hydro_ops#is_Ice_AdapterNotFoundException ->
	        emit (lazy [])
	  | Hydro_builtin.User_exception ue
	      when ue#hydro_ops#is_Ice_ObjectNotFoundException ->
	        emit (lazy [])
	  | err ->
	        emit (lazy (raise err))

    end : Hydro_proxy.proxy_resolver_t
  )


let test_indirect_resolver (res : Hydro_proxy.proxy_resolver_t) name =
  let dummy_id =
    ( object method name = "dummy" method category = "" end ) in
  let pa =
    ( object
	method id = dummy_id
	method facet = None
	method mode = `Twoway
	method secure = false
	method parameters = `Adapter name
      end
    ) in
  let result = ref None in
  let emit_result r =
    result := Some r in

  let pool = Hydro_proxy.pool() in
  let esys = Unixqueue.create_unix_event_system() in
  let sys = Hydro_lm.create_system() in
  let conf = Hydro_proxy.proxy_conf () in
  let env =
    ( object
	method event_system = esys
	method system = sys
	method proxy_resolver = res
	method client_pool = pool
	method default_proxy_conf = conf
      end
    ) in
  res # resolve pa emit_result env;
  Unixqueue.run esys;
  pool # shutdown();
  match !result with
    | None ->
	failwith "No response"
    | Some ll ->
	let l = Lazy.force ll in
	List.map (fun (`Endpoint(ep,port),_) -> ep,port) l


let get_Ice_Locator_of_port host port =
  let addr =
    ( object
	method id =
	  ( object
	      method name = "Locator"
	      method category = "IceGrid"
	    end
	  )
	method facet = None
	method mode = `Twoway
	method secure = false
	method parameters =
	  `Endpoints [| `TCP ( object
				 method host = host
				 method port = port
				 method timeout = 5000l (*ms*)   (* TODO *)
				 method compress = false
			       end : tcp_endpoint
			     )
		     |]
      end : proxy_addr
    ) in
  unchecked_pr_Ice_Locator (Hydro_lm.pr_of_address addr)

let get_Ice_Locator_of_string s =
  unchecked_pr_Ice_Locator (Hydro_lm.pr_of_string s)


exception Error of string


let set_adapter ~dynamic_ip pr_loc adapterId replicaGroupId_opt ep_opt =
  let pool = Hydro_proxy.pool() in
  try
    let esys = Unixqueue.create_unix_event_system() in
    let sys = Hydro_lm.create_system() in
    Hydro_builtin.fill_system sys;
    let conf = Hydro_proxy.proxy_conf () in
    let cp = Hydro_params.client_params ~msg_timeout:10.0 () in
    let res =  proxy_resolver cp pr_loc in
    let env =
      ( object
	  method event_system = esys
	  method system = sys
	  method proxy_resolver = res
	  method client_pool = pool
	  method default_proxy_conf = conf
	end
      ) in
    let po = pc_Ice_Locator env pr_loc in
    let reg_reply = ( po # getRegistry() ) # scall in
    let reg_opt = reg_reply # result in
    match reg_opt with
      | None ->
	  raise(Error "Locator doesn't return LocatorRegistry")
      | Some pr_reg ->
	  let po_reg = pc_Ice_LocatorRegistry env pr_reg in
	  let pr_dummy_opt =
	    match ep_opt with
	      | Some ep ->
		  let ep' =
		    match reg_reply#hydro_response#addr with
		      | Some sockaddr when dynamic_ip ->
			  ( match sockaddr with
			      | Unix.ADDR_INET(ip,_) ->
				  let host = Unix.string_of_inet_addr ip in
				  Hydro_util.replace_host host ep
			      | _ ->
				  ep
			  )
		      | _ -> ep in

		  (* Create a dummy proxy reference: *)
		  let id =
		    ( object
			method name = "dummy"
			method category = ""
		      end
		    ) in
		  let pa =
		    ( object
			method id = id
			method facet = None
			method mode = `Twoway
			method secure = false
			method parameters = `Endpoints [| ep' |]
		      end
		    ) in
		  Some(Hydro_lm.pr_of_address pa)
	      | None ->
		  None in
	  let f =
	    match replicaGroupId_opt with
	      | None ->
		  po_reg # setAdapterDirectProxy adapterId pr_dummy_opt
	      | Some replicaGroupId ->
		  po_reg # setReplicatedAdapterDirectProxy
		    adapterId replicaGroupId pr_dummy_opt in
	  ( try
	      f # scall # result;
	      pool # shutdown();
	    with
	      | User_exception ue ->
		  raise(Error(ue#hydro_ops#exn_id))
	      | error ->
		  raise(Error(Hydro_util.exn_to_string error))
	  )

  with
    | error ->
	pool # abort();
	raise error

let set_adapters ~dynamic_ip pr_loc l ep_opt =
  List.iter
    (function oa ->
       match oa#adapter_id with
	 | None -> ()
	 | Some adp_id ->
	     set_adapter ~dynamic_ip pr_loc adp_id oa#replica_group_id ep_opt
    )
    l
    (* TODO: This could be optimized. Instead of connecting to the locator
       again for every adapter, we could do everything in one go
     *)


let register_adapter ?(dynamic_ip=false)
                     pr_loc adapterId replicaGroupId_opt ep =
  set_adapter ~dynamic_ip pr_loc adapterId replicaGroupId_opt (Some ep)


let register_adapters ?(dynamic_ip=false) pr_loc l ep =
  set_adapters ~dynamic_ip pr_loc l (Some ep)


let unregister_adapter pr_loc adapterId replicaGroupId_opt =
  set_adapter ~dynamic_ip:false pr_loc adapterId replicaGroupId_opt None


let unregister_adapters pr_loc l =
  set_adapters ~dynamic_ip:false pr_loc l None
