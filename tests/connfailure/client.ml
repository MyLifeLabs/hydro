open Printf

let main() =
  let yourname_opt = ref None in
  let host = ref "localhost" in
  let port1 = ref 5430 in
  let altports = ref [] in
  let debug = ref false in
  let async = ref 1 in
  let repeat = ref 1 in
  let msg_timeout = ref (-1.0) in
  let trans_timeout = ref (-1) in

  Arg.parse
    [ "-host", Arg.String (fun s -> host := s),
      "<hostname>  contact the hello server at this host";

      "-port", Arg.Int (fun n -> port1 := n),
      "<port>  contact the hello server at this port";

      "-alt-port", Arg.Int (fun n -> altports := !altports @ [n]),
      "<port>  use this port as alternate server (multiple possible)";

      "-async", Arg.Set_int async,
      "<n>  Send n requests asynchrounously";

      "-repeat", Arg.Set_int repeat,
      "<n>  Repeat sending messages n times";

      "-msg-tmo", Arg.Set_float msg_timeout,
      "<n>  Set the message timeout to n seconds";

      "-trans-tmo", Arg.Set_int trans_timeout,
      "<n>  Set the transport timeout to n seconds";

      "-debug", Arg.Set debug,
      "  enable debugging";
    ]
    (fun s ->
       yourname_opt := Some s)
    (sprintf "%s: [options] <yourname>" Sys.argv.(0));

  let yourname =
    match !yourname_opt with
      | Some n -> n
      | None -> failwith ("No name given") in

  if !debug then
    Hydro_dbg.set_debug_log prerr_endline;

  (* The first thing we have to create is a proxy environment. This
     requires a few other objects we now create. Once created you
     can use the proxy environment for all your ICE calls.
   *)
  let esys = Unixqueue.create_unix_event_system() in
  let sys = Hydro_lm.create_system() in
  Hello.fill_system sys;    (* Generated! *)
  let params = Hydro_params.client_params() in
  let res = Hydro_proxy.proxy_resolver params in
  let pool = Hydro_proxy.pool() in
  let proxy_conf = Hydro_proxy.proxy_conf() in
  let proxy_env =
    ( object
	method event_system = esys
	method system = sys
	method proxy_resolver = res
	method client_pool = pool
	method default_proxy_conf = proxy_conf
      end
	: Hydro_proxy.proxy_env_t
    ) in

  (* Now we need the name of the server we want to call, the proxy reference *)

  let id =
    ( object
	method name = "MySingleGreeter"
	method category = ""
      end
	: Hydro_types.identity
    ) in
  let eps =
    Array.init (List.length !altports + 1)
      (fun k ->
	 if k = 0 then
	   `TCP ( object
		    method host = !host
		    method port = !port1
		    method timeout = Int32.of_int !trans_timeout
		    method compress = false
		  end
		    : Hydro_types.tcp_endpoint
		)
	 else
	   let p = List.nth !altports (k-1) in
	   `TCP ( object
		    method host = !host
		    method port = p
		    method timeout = Int32.of_int !trans_timeout
		    method compress = false
		  end
		    : Hydro_types.tcp_endpoint
		)
      ) in
  let pa =
    ( object
	method id = id
	method facet = None
	method mode = `Twoway
	method secure = false
	method parameters = `Endpoints eps
      end
	: Hydro_types.proxy_addr
      ) in
  let pr = Hydro_lm.pr_of_address pa in
  let pr' = Hello.unchecked_pr_Hello_Greeter pr in

  (* Now we need the proxy object. The preparations until here are long.
     However, you can now use po for all calls to the server
   *)
  let po = Hello.pc_Hello_Greeter proxy_env pr' in

  (* Finally we are prepared for the call *)
  for j = 1 to !repeat do
    for k = 1 to !async do
      let cs = po # greetingMessage yourname in
      let params = cs#params in
      let cs' =
	cs#with_params (Hydro_params.update_call_params
			  ~msg_timeout:!msg_timeout params) in
      printf "Request %d/%d\n%!" j k;
      cs' # acall
	(fun response ->
	   (* Look at the response *)
	   try
	     let s = response # result in
	     printf "Response %d/%d: %s\n%!" j k s;

	     if k = !async then pool # shutdown();
	   with
	     | err ->
		 (* Something went wrong. Errors are normally accumulated, and
                  thrown when the [result] method is invoked
		  *)
		 printf "Exception %d/%d: %s\n%!"
		   j k
		   (Hydro_util.exn_to_string err)
	)
    done;
    Unixqueue.run esys
  done

let () =
  main()


