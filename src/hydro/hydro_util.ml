(* $Id$ *)

open Hydro_types

let t_proxy : htype =
  TStruct [| "name",     TString;
	     "category", TString;
	     "facet",    (TSequence TString);
	     "mode",     TByte;
	     "secure",   TBool
	  |]

let t_proxy_alt : htype =
  TStruct [| "facet",    (TSequence TString);
	     "mode",     TByte;
	     "secure",   TBool
	  |]


let t_tcp_endpoint : htype =
  TStruct [| "host",     TString;
	     "port",     TInt;
	     "timeout",  TInt32;
	     "compress", TBool
	  |]


let t_udp_endpoint : htype =
  TStruct [| "host",        TString;
	     "port",        TInt;
	     "proto_major", TInt;
	     "proto_minor", TInt;
	     "enc_major",   TInt;
	     "enc_minor",   TInt;
	     "compress",    TBool
	  |]


let t_msg_request : htype =
  TStruct [| "requestId",   TInt32;
	     "id.name",     TString;
	     "id.category", TString;
	     "facet",       (TSequence TString);
	     "operation",   TString;
	     "mode",        TByte;
	     "context",     (TDictionary(TString, TString));
	  |]


let t_msg_batch_request : htype =
  TStruct [| "id.name",     TString;
	     "id.category", TString;
	     "facet",       (TSequence TString);
	     "operation",   TString;
	     "mode",        TByte;
	     "context",     (TDictionary(TString, TString));
	  |]

let t_msg_reply : htype =
  TStruct [| "requestId",   TInt32;
	     "replyStatus", TByte;
	  |]

let t_msg_reply_error : htype =
  TStruct [| "id.name",     TString;
	     "id.category", TString;
	     "facet",       (TSequence TString);
	     "operation",   TString;
	  |]

let endpoint_type =
  function
    | `TCP _ -> `TCP
    | `UDP _ -> `UDP
    | `SSL _ -> `SSL
    | `Unknown(n,_) -> `Unknown n


let string_of_endpoint_type =
  function
    | `TCP -> "TCP"
    | `UDP -> "UDP"
    | `SSL -> "SSL"
    | `Unknown n -> string_of_int n


let or_cmp n f =
  if n=0 then Lazy.force f else n

let bool_cmp b1 b2 =
  match b1, b2 with
    | false, false -> 0
    | true, true -> 0
    | false, true -> (-1)
    | true, false -> 1

let float_cmp (f1:float) (f2:float) : int =
  Pervasives.compare f1 f2
  
let int_opt_cmp (io1:int option) (io2:int option) : int =
  match io1, io2 with
    | Some n1, Some n2 -> n1 - n2
    | Some _, None -> 1
    | None, Some _ -> (-1)
    | None, None -> 0

let endpoint_cmp (ep1:endpoint) (ep2:endpoint) =
  match ep1 with
    | `TCP tcp1 ->
	( match ep2 with
	    | `TCP tcp2 ->
		or_cmp
		  (String.compare tcp1#host tcp2#host)
		  (lazy 
		     (or_cmp
			(tcp1#port - tcp2#port)
			(lazy 
			   (or_cmp
			      (Int32.compare tcp1#timeout tcp2#timeout)
			      (lazy (bool_cmp tcp1#compress tcp2#compress))))))
	    | _ ->
		(-1)
	)
    | `UDP udp1 ->
	( match ep2 with
	    | `TCP _ ->
		1
	    | `UDP udp2 ->
		or_cmp
		  (String.compare udp1#host udp2#host)
		  (lazy
		     (or_cmp
			(udp1#port - udp2#port)
			(lazy
			   (or_cmp
			      (udp1#proto_major - udp2#proto_major)
			      (lazy
				 (or_cmp
				    (udp1#proto_minor - udp2#proto_minor)
				    (lazy
				       (or_cmp
					  (udp1#enc_major - udp2#enc_major)
					  (lazy
					     (or_cmp
						(udp1#enc_minor - 
						   udp2#enc_minor)
						(lazy
						   (bool_cmp
						      udp1#compress
						      udp2#compress))))))))))))
	    | _ ->
		(-1)
	)
    | `SSL ssl1 ->
	( match ep2 with
	    | `TCP _ | `UDP _ ->
		1
	    | `SSL ssl2 ->
		or_cmp
		  (String.compare ssl1#host ssl2#host)
		  (lazy 
		     (or_cmp
			(ssl1#port - ssl2#port)
			(lazy 
			   (or_cmp
			      (Int32.compare ssl1#timeout ssl2#timeout)
			      (lazy (bool_cmp ssl1#compress ssl2#compress))))))
	    | _ ->
		(-1)
	)
    | `Unknown(n1,s1) ->
	( match ep2 with
	    | `TCP _ | `UDP _ | `SSL _ ->
		1
	    | `Unknown(n2,s2) ->
		or_cmp
		  (n1-n2)
		  (lazy (String.compare s1 s2))
	)

let network_port_of_connector c : network_port option =
  (* as good as possible... we don't do dns lookups, however.
   *)
  match c with
    | `Endpoint(_,port_opt) ->
	port_opt

let host_port_of_endpoint ep =
  match ep with
    | `TCP s | `SSL s ->
	Some(s#host, s#port)
    | `UDP s ->
	Some(s#host, s#port)
    | _ ->
	None


let connector_of_resolved_endpoint ep addr =
  match ep with
    | `TCP s | `SSL s ->
	`Endpoint(ep, Some(`TCP(addr, s#port)))
    | `UDP s ->
	`Endpoint(ep, Some(`UDP(addr, s#port)))
    | _ ->
	`Endpoint(ep, None)

let replace_host host ep =
  match ep with
    | `TCP tcp ->
	`TCP
	  ( object
	      method host = host
	      method port = tcp#port
	      method timeout = tcp#timeout
	      method compress = tcp#compress
	    end
	  )
    | `SSL ssl ->
	`SSL
	  ( object
	      method host = host
	      method port = ssl#port
	      method timeout = ssl#timeout
	      method compress = ssl#compress
	    end
	  )
    | `UDP udp ->
	`UDP
	  ( object
	      method host = host
	      method port = udp#port
	      method proto_major = udp#proto_major
	      method proto_minor = udp#proto_minor
	      method enc_major = udp#enc_major
	      method enc_minor = udp#enc_minor
	      method compress = udp#compress
	    end
	  )
    | other -> other

(* for debugging: *)

let pv_to_string : protocol_violation -> string =
  function
    | `MessageFormatViolation s ->
	"MessageFormatViolation(" ^ s ^ ")"
    | `CompressionNotSupported ->
	"CompressionNotSupported"
    | `BadMessageType ->
	"BadMessageType"

let lim_to_string : limitation -> string =
  function
    | `UnsupportedEncodingVersion ->
	"UnsupportedEncodingVersion"
    | `UnsupportedProtocolVersion ->
	"UnsupportedProtocolVersion"
    | `UnsupportedEndpointType n ->
	"UnsupportedEndpointType(" ^ string_of_int n ^ ")"

let rec exn_to_string : exn -> string =
  function
    | Protocol_violation pv ->
	"Protocol_violation(" ^ pv_to_string pv ^ ")"
    | Marshal_error s ->
	"Marshal_error(" ^ s ^ ")"
    | Unmarshal_error s ->
	"Unmarshal_error(" ^ s ^ ")"
    | Limitation lim ->
	"Limitation(" ^ lim_to_string lim ^ ")"
    | Client_condition cc ->
	"Client_condition(" ^ cc_to_string cc ^ ")"
    | Proxy_error pe ->
	"Proxy_error(" ^ pe_to_string pe ^ ")"
    | Domain_not_found name ->
	"Domain_not_found(" ^ name ^ ")"
    (* Because of buggy Printexc.to_string: *)
    | Invalid_argument msg ->
	"Invalid_argument(" ^ msg ^ ")"
    | Failure msg ->
	"Failure(" ^ msg ^ ")"
    | other ->
	Printexc.to_string other

and pe_to_string =
  function
    | `NoCallableEndpointFound -> "NoCallableEndpointFound"
    | `NoEndpointIsReachable -> "NoEndpointIsReachable"
    | `NoLocatorIsReachable -> "NoLocatorIsReachable"
    | `ProxyIsDown -> "ProxyIsDown"

and cc_to_string =
  function
    | `Message_lost flag -> "Message_lost(" ^ string_of_bool flag ^ ")"
    | `Message_timeout -> "Message_timeout"
    | `Transport_timeout -> "Transport_timeout"
    | `Connect_timeout -> "Connect_timeout"
    | `Connect_error x -> "Connect_error(" ^ exn_to_string x ^ ")"
    | `Client_is_down -> "Client_is_down"
    | `User_exception _ -> "User_exception"
    | `Object_does_not_exist _ -> "Object_does_not_exist"
    | `Facet_does_not_exist _ -> "Facet_does_not_exist"
    | `Operation_does_not_exist _ -> "Operation_does_not_exist"
    | `Unknown_local_exception s -> "Unknown_local_exception(" ^ s ^ ")"
    | `Unknown_user_exception s -> "Unknown_user_exception(" ^ s ^ ")"
    | `Unknown_exception s -> "Unknown_exception(" ^ s ^ ")"
    | `Error x -> "Error(" ^ exn_to_string x ^ ")"

let result_to_string =
  function
    | `Success eb ->
	"Success(" ^ string_of_int eb.encap_len ^ ")"
    | `User_exception eb ->
	"User_exception(" ^ string_of_int eb.encap_len ^ ")"
    | `Object_does_not_exist _ ->
	"Object_does_not_exist"
    | `Facet_does_not_exist _ ->
	"Facet_does_not_exist"
    | `Operation_does_not_exist _ ->
	"Operation_does_not_exist"
    | `Unknown_local_exception s ->
	"Unknown_local_exception(" ^ s ^ ")"
    | `Unknown_user_exception s ->
	"Unknown_user_exception(" ^ s ^ ")"
    | `Unknown_exception s ->
	"Unknown_exception(" ^ s ^ ")"


let rec htype_to_string =
  function
    | TVoid -> "void"
    | TBool -> "bool"
    | TByte -> "byte"
    | TShort -> "short"
    | TInt -> "int"
    | TInt32 -> "int32"
    | TLong -> "long"
    | TFloat -> "float"
    | TDouble -> "double"
    | TString -> "string"
    | TByteseq -> "byteseq"
    | TEnum _ -> "enum"
    | TStruct s ->
	"{" ^
	  (String.concat "; " 
	     (List.map
		(fun (n,t) -> n ^ "=" ^ htype_to_string t)
		(Array.to_list s))) ^
	  "}"
    | TSequence s ->
	htype_to_string s ^ " sequence";
    | TDictionary (s,t) ->
	"(" ^ htype_to_string s ^ "," ^  htype_to_string t ^ ") dictionary"
    | TProxy p ->
	p ^ "*"
    | TClass c ->
	c ^ "@"
    | TDirectMapping(t,_,_) ->
	htype_to_string t
