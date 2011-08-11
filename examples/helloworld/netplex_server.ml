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
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  let debug = ref false in

  let opts =
    [ "-debug", Arg.Set debug,
      "  enable debugging";
    ] @ opt_list in

  Arg.parse
    opts
    (fun s -> raise(Arg.Bad ("Don't know what to do with: " ^ s)))
    (sprintf "%s: [options]" Sys.argv.(0));

  if !debug then
    Hydro_dbg.set_debug_log prerr_endline;

  let sys = Hydro_lm.create_system() in
  Hello.fill_system sys;    (* Generated! *)

  let id = ( object
	       method name = "MySingleGreeter"
	       method category = ""
	     end
	   ) in
  (* The id of the ICE object *)

  let greeter_factory =
    Hydro_netplex.hydro_factory
      ~name:"greeter"
      ~sys
      ~adapters:1  (* We want one object adapter *)
      ~configure:(fun _ _ _ -> ())
      ~hooks:(fun aa () ->
		let oa = aa.(0) in   (* The first created object adapter *)
		( object
		    inherit Netplex_kit.empty_processor_hooks()
		    method post_start_hook container =
		      let servant = new myGreeter in
		      (* A servant is the implementation object behind the
                         announced ICE object. We create it in the
                         post_start_hook so you could access per-process
                         resources, e.g. database connections etc.
		       *)
		      oa # add id (servant :> Hydro_lm.interface_base)
			(* Make this servant visible in the adapter *)
		  end
		)
	     )
      () in

  let parallelizer = Netplex_mp.mp() in
  (* Multi-processing. Note that multi-threading is currently not
     supported by Hydro!
   *)

  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ greeter_factory ]           (* make this service type available *)
    cmdline_cfg


let () =
  main()

