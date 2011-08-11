open Printf

class myGreeter =
object
  inherit Hello.skel_Hello_Greeter
    (* The generated skeleton class defines all operations so that they
       throw exceptions [Unimplemented_operation]. We have to override
       these definitions here, and do that for [greetingMessage].
       Furthermore, the skeleton class defines a method dispatcher, so
       our operation really gets called.
     *)

  method greetingMessage yourname =
    Hello.parachute
      (fun session ->
	 (* Compute the result... *)
	 let rstring = "Welcome " ^ yourname ^ "!" in
	 (* ... and put it into a result object. If there were any output
            parameters (keyword [out] in the ICE file), these would have
            additional methods.
          *)
	 ( object
	     method result = rstring
	   end
	 )
      )
      (* The parachute calls the inner function, and takes care of passing
         the result value back to the caller. Furthermore, it catches any
         exceptions, and also passes them to the caller. It simply ensures
         that there is a reaction for the call, and that exceptions don't
         crash the server. Using them is strongly recommended unless you
         need truly asynchronous behavior.
       *)
end
  (* This is our implementation that serves the [Greeter] interface. *)


let main() =
  let host = ref "localhost" in
  let port = ref 5430 in
  let debug = ref false in

  Arg.parse
    [ "-host", Arg.String (fun s -> host := s),
      "<hostname>  contact the hello server at this host";

      "-port", Arg.Int (fun n -> port := n),
      "<port>  contact the hello server at this port";

      "-debug", Arg.Set debug,
      "  enable debugging";
    ]
    (fun s -> raise(Arg.Bad ("Don't know what to do with: " ^ s)))
    (sprintf "%s: [options]" Sys.argv.(0));

  if !debug then
    Hydro_dbg.set_debug_log prerr_endline;

  let esys = Unixqueue.create_unix_event_system() in
  let sys = Hydro_lm.create_system() in
  Hello.fill_system sys;    (* Generated! *)
  let params = Hydro_params.server_params() in

  let ip =
    try
      Unix.inet_addr_of_string !host
    with
      | _ ->
	  (Unix.gethostbyname !host).Unix.h_addr_list.(0) in
  let sockaddr =
    Unix.ADDR_INET(ip,!port) in
  let mc = `Named_endpoint (sockaddr, `TCP) in

  let master = Hydro_endpoint.Master.create sys mc params esys in
  (* This is a master server for a TCP master socket. It automatically
     creates subservers for every incoming connection.
   *)

  let servant = new myGreeter in
  (* A servant is the implementation object behind the announced ICE
     object.
   *)

  let id = ( object 
	       method name = "MySingleGreeter"
	       method category = ""
	     end 
	   ) in
  (* The id of the ICE object *)

  let oa = Hydro_oa.object_adapter() in
  oa # add id (servant :> Hydro_lm.interface_base);
  (* An object adapter maps IDs to servants *)

  Hydro_endpoint.Master.bind_adapter
    master
    (oa :> Hydro_types.object_dispatcher);
  (* This tells the master server that it shall consult this object adapter
     for incoming remote calls
   *)

  Unixqueue.run esys
    (* This starts the event loop, and thus starts serving *)

let () =
  main()

