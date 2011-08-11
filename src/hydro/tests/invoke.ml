(* This is a client for the "invoke" server in the Ice distribution *)

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

let demo_printfailure : hexn =
  (object
     method name = "::Demo::PrintFailure"
     method super = None
     method elements = [| "reason", `String |]
   end
  );;

let demo_stringseq = `Sequence `String ;;
let demo_stringdict = `Dictionary(`String, `String);;
let demo_color = `Enum [| "red"; "green"; "blue" |] ;;
let demo_structure = `Struct [| "name", `String;
				"value", demo_color |] ;;
let demo_structureseq = `Sequence demo_structure ;;

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
     method elements = [| "s", demo_structure |]
   end
  );;

let f_printstring : hfunction =
  (object
     method name = "printString"
     method idempotent = false
     method in_args = [| "message", `String |]
     method out_args = [| |]
     method result = `Void
     method in_classes = false
     method out_classes = false
   end
  );;

let f_printstringsequence : hfunction =
  (object
     method name = "printStringSequence"
     method idempotent = false
     method in_args = [| "seq", demo_stringseq |]
     method out_args = [| |]
     method result = `Void
     method in_classes = false
     method out_classes = false
   end
  );;

let f_printdictionary : hfunction =
  (object
     method name = "printDictionary"
     method idempotent = false
     method in_args = [| "dict", demo_stringdict |]
     method out_args = [| |]
     method result = `Void
     method in_classes = false
     method out_classes = false
   end
  );;

let f_printenum : hfunction =
  (object
     method name = "printEnum"
     method idempotent = false
     method in_args = [| "c", demo_color |]
     method out_args = [| |]
     method result = `Void
     method in_classes = false
     method out_classes = false
   end
  );;

let f_printstruct : hfunction =
  (object
     method name = "printStruct"
     method idempotent = false
     method in_args = [| "st", demo_structure |]
     method out_args = [| |]
     method result = `Void
     method in_classes = false
     method out_classes = false
   end
  );;

let f_printstructsequence : hfunction =
  (object
     method name = "printStructSequence"
     method idempotent = false
     method in_args = [| "seq", demo_structureseq |]
     method out_args = [| |]
     method result = `Void
     method in_classes = false
     method out_classes = false
   end
  );;

let f_printclass : hfunction =
  (object
     method name = "printClass"
     method idempotent = false
     method in_args = [| "cls", `Class "::Demo::C" |]
     method out_args = [| |]
     method result = `Void
     method in_classes = true
     method out_classes = false
   end
  );;

let f_getvalues : hfunction =
  (object
     method name = "getValues"
     method idempotent = false
     method in_args = [| |]
     method out_args = [| "str", `String |]
     method result = `Class "::Demo::C"
     method in_classes = false
     method out_classes = true
   end
  );;

let f_throwprintfailure : hfunction =
  (object
     method name = "throwPrintFailure"
     method idempotent = false
     method in_args = [| |]
     method out_args = [| |]
     method result = `Void
     method in_classes = false
     method out_classes = false
   end
  );;

let f_shutdown : hfunction =
  (object
     method name = "shutdown"
     method idempotent = false
     method in_args = [| |]
     method out_args = [| |]
     method result = `Void
     method in_classes = false
     method out_classes = false
   end
  );;


let demo_printer : hintf =
  (object
     method name = "::Demo::Printer"
     method super = []
     method elements = [ f_printstring;
			 f_printstringsequence;
			 f_printdictionary;
			 f_printenum;
			 f_printstruct;
			 f_printstructsequence;
			 f_printclass;
			 f_getvalues;
			 f_throwprintfailure;
			 f_shutdown
		       ]
   end
  );;


Hashtbl.add exceptions demo_printfailure#name demo_printfailure;;
Hashtbl.add types "::Demo::StringSeq" demo_stringseq;;
Hashtbl.add types "::Demo::StringDict" demo_stringdict;;
Hashtbl.add types "::Demo::Color" demo_color;;
Hashtbl.add types "::Demo::Structure" demo_structure;;
Hashtbl.add types "::Demo::StructureSeq" demo_structureseq;;
Hashtbl.add classes demo_C#name demo_C;;
Hashtbl.add interfaces demo_printer#name demo_printer;;

let conn =
  `Mplex_socket(`Connect_inet("localhost", 10000, `Stream),
                Hydro_client_connector.default_mplex_config);;

let identity =
  ( object
      method name = "printer"
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
    demo_printer
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

let call0 name args =
  ignore(call name args);;


let printstring s = 
  call0 "printString" [| `String s |];;
let printstringsequence seq = 
  call0 "printStringSequence" 
    [| `Sequence (Array.map (fun s -> `String s) seq) |];;
let printdictionary dict =
  call0 "printDictionary"
    [| `Dictionary 
	 (Array.map
	    (fun (s,t) -> (`String s, `String t)) 
	    (Array.of_list dict)) |];;
let map_color color =
  match color with
    | `red -> `Enum 0
    | `green -> `Enum 1
    | `blue -> `Enum 2;;
let printenum e =
  call0 "printEnum" [| map_color e |];;
let printstruct (name,value) =
  call0 "printStruct"
    [| `Struct [| `String name; map_color value |] |];;
let printstructsequence seq =
  call0 "printStructSequence"
    [| `Sequence
	 (Array.map
	    (fun (name,value) ->
	       `Struct [| `String name; map_color value |]
	    )
	    seq) |];;
let printclass (name,value) =
  call0 "printClass"
    [| `Class ( ref (`Value 
		       (object
			  method hydro_effective_id = "::Demo::C"
			  method hydro_slices =
			    [ `Decoded("::Ice::Object",
				       [| `Dictionary [| |] |]);
			      `Decoded("::Demo::C",
				      [| `Struct
					   [| `String name; map_color value |]
				      |]);
			    ]
			end))) |];;
let getvalues () =
  let resp =
    call "getValues" [| |] in
  let c =
    match resp#result with
      | `Class { contents = `Value cv } ->
	  let slices = cv#hydro_slices in
	  ( match slices with
	      | [ `Decoded("::Ice::Object", _);
		  `Decoded("::Demo::C", 
			   [| `Struct
				[| `String name; `Enum k |]
			   |]);
		] ->
		  (name, k)

	      | _ -> assert false
	  )

      | _ -> assert false in
  let str =
    match resp#out_args with
      | [| `String s |] -> s
      | _ -> assert false in
  (c,str);;
let throwprintfailure() =
  let resp =
    call "throwPrintFailure" [| |] in
  match resp#condition with
    | `User_exception sv ->
	( match sv#hydro_slices with
	    | [ `Decoded("::Demo::PrintFailure",
			 [| `String reason |]
			)
	      ] ->
		reason
	    | _ -> assert false
	)
    | _ ->
	assert false
;;
let shutdown() =
  call0 "shutdown" [| |];;
