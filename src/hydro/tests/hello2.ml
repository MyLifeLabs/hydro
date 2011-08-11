#use "topfind";;
#require "unix,equeue,netstring";;
#load "hydro.cma";;
open Hydro_types;;

Hydro_client.debug := true;;

let esys = Unixqueue.create_unix_event_system();;

let types = Hashtbl.create 1;;
let exceptions = Hashtbl.create 1;;
let interfaces = Hashtbl.create 1;;
let classes = Hashtbl.create 1;;

let system =
  ( object
      method types = types
      method exceptions = exceptions
      method interfaces = interfaces
      method classes = classes
    end
  );;

let demo_cseq = `Sequence(`Class "::Demo::C");;

let ice_object : hclass =
  (object
     method name = "::Ice::Object"
     method super = None
     method elements = [| "facets", `Dictionary(`Void,`Void) |]
   end
  );;

let demo_C : hclass =
  (object
     method name = "::Demo::C"
     method super = Some ice_object
     method elements = [| "s", `String |]
   end
  );;


let f_getvalues : hfunction =
  (object
     method name = "getvalues"
     method idempotent = false
     method in_args = [| "number", `Int |]
     method out_args = [| |]
     method result = demo_cseq
     method in_classes = false
     method out_classes = true
   end
  );;


let demo_hello : hintf =
  (object
     method name = "::Demo::Hello"
     method super = []
     method elements = [ f_getvalues ]
   end
  ) ;;



Hashtbl.add types "::Demo::CSeq" demo_cseq;;
Hashtbl.add classes demo_C#name demo_C;;
Hashtbl.add interfaces demo_hello#name demo_hello;;

let conn =
  `Mplex_socket(`Connect_inet("localhost", 10000, `Stream),
                Hydro_client_connector.default_mplex_config);;

let identity =
  ( object
      method name = "hello"
      method category = ""  (* ??? *)
    end
  );;

let call name args =
  let client =
    Hydro_client.create system conn esys in
  let resp1 =
    ref None in
  Hydro_client.twoway_call
    client
    identity
    demo_hello
    name
    (Hydro_client.call_params())
    args
    (fun resp ->
       resp1 := Some resp;
       prerr_endline "Response:";
       ( match resp#condition with
           | `Success ->
               prerr_endline "Success!"
           | #client_condition as cc ->
               prerr_endline (Hydro_util.cc_to_string cc)
       );
       Hydro_client.shutdown client
    );
  Unixqueue.run esys;
  match !resp1 with
    | None -> assert false
    | Some r -> r
;;


let getvalues n =
  let resp =
    call "getvalues" [| `Int n |] in
  match resp#result with
    | `Sequence seq ->
	Array.map
	  (function
	     | `Class { contents = `Value cv } ->
		 let slices = cv#hydro_slices in
		 ( match slices with
		     | [
			 `Decoded("::Ice::Object", _);
			 `Decoded("::Demo::C",
				  [| `String s |]);
		       ] ->
			 s

		     | _ -> assert false
		 )
	     | _ -> assert false
	  )
	  seq

    | _ -> assert false
;;
