open Printf

let main() =
  let yourname_opt = ref None in
  let proxy_string = ref "MySingleGreeter:tcp -h localhost -p 5430" in
  let locator_string = ref None in
  let debug = ref false in

  Arg.parse
    [ "-proxy-string", Arg.String (fun s -> proxy_string := s),
      "<s>  set the address of the server as 'stringified proxy'";

      "-locator-string", Arg.String (fun s -> locator_string := Some s),
      "<s>  set the address of the location service as 'stringified proxy'";

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
  let res =
    match !locator_string with
      | None ->
	  Hydro_proxy.proxy_resolver params
      | Some ls ->
	  let loc = Hydro_locator.get_Ice_Locator_of_string ls in
	  Hydro_locator.proxy_resolver params loc in
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

  let pr = Hydro_lm.pr_of_string !proxy_string in
  let pr' = Hello.unchecked_pr_Hello_Greeter pr in

  (* Now we need the proxy object. The preparations until here are long.
     However, you can now use po for all calls to the server
   *)
  let po = Hello.pc_Hello_Greeter proxy_env pr' in

  (* Finally we are prepared for the call *)
  let response = (po # greetingMessage yourname) # scall in

  (* Look at the response *)
  try
    let s = response # result in
    printf "Response: %s\n%!" s;

    (* Perform a shutdown of all connections *)
    pool # shutdown();
  with
    | err ->
	(* Something went wrong. Errors are normally accumulated, and
           thrown when the [result] method is invoked
         *)
	printf "Exception: %s\n%!"
	  (Hydro_util.exn_to_string err)

let () =
  main()


