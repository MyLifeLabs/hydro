(* This is a client for the "hello" server in the Ice distribution *)

#use "topfind";;
#require "unix,equeue,netstring";;
#load "hydro.cma";;
open Hydro_types;;

Hydro_dbg.set_debug_log prerr_endline;;

let esys = Unixqueue.create_unix_event_system();;

let intf = Hashtbl.create 1;;

let demo_hello =
  ( object
      method name = "Demo.Hello"
      method super = []
      method elements =
	[ ( object
	      method name = "sayHello"
	      method mode = `Idempotent
	      method in_args = [| |]
	      method in_classes = false
	      method out_args = [| |]
	      method result = `Void
	      method out_classes = false
	    end
	  )
	]
    end
  );;

Hashtbl.add intf demo_hello#name demo_hello

let system = Hydro_lm.create_system()

let identity =
  ( object
      method name = "hello"
      method category = ""
    end
  );;

let endpoint =
  `TCP ( object
	   method host = "localhost"
	   method port = 10000
	   method timeout = 10000l
	   method compress = false
	 end
       )

let proxy_spec =
  ( object
      method id = identity
      method facet = None
      method mode = `Twoway
      method secure = false
      method parameters = `Endpoints [| endpoint |]
    end
  )


let pool = Hydro_proxy.pool()

let env =
  (object
     method event_system = esys
     method system = system
     method proxy_resolver =
       Hydro_proxy.proxy_resolver
	 (Hydro_params.client_params())
     method client_pool = pool
     method default_proxy_conf = Hydro_proxy.proxy_conf()
   end
  )


let proxy = Hydro_proxy.proxy ~env ~addr:proxy_spec ()


let doit() =
  proxy # hydro_twoway_call
    demo_hello
    "sayHello"
    [| |]
    (Hydro_params.call_params())
    (fun resp ->
       prerr_endline "Response:";
       ( match resp#condition with
	   | `Success ->
	       prerr_endline "Success!"
	   | #client_condition as cc ->
	       prerr_endline (Hydro_util.cc_to_string cc)
       );
(*
       ( match resp#client with
	   | None -> ()
	   | Some cl -> Hydro_client.shutdown cl
       )
 *)
    );
  Unixqueue.run esys
