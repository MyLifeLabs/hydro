(* $Id$ *)

open Hydro_lexer
open Printf

let norm_tokens enable_slash enable_at enable_colon toks =
  let rec norm toks =
    match toks with
      | Chars s1 :: Chars s2 :: toks' ->
	  norm (Chars (s1 ^ s2) :: toks')
      | Sep :: toks' ->
	  norm toks'
      | other :: toks' ->
	  other :: norm toks'
      | [] ->
	  []
  in

  let map toks =
    List.map
      (function
	 | Colon when not enable_colon -> Chars ":"
	 | At when not enable_at -> Chars "@"
	 | Slash when not enable_slash -> Chars "/"
	 | other -> other
      )
      toks in

  norm (map toks)



let identity_of_toks toks =
  match toks with
    | Chars category :: Slash :: Chars name :: toks' ->
	( object
	    method category = category
	    method name = name
	  end
	), toks'
    | Chars name :: toks' ->
	( object
	    method category = ""
	    method name = name
	  end
	), toks'
    | _ ->
	failwith "syntax error"


let identity_of_string s =
  try
    let toks1 = Hydro_lexer.split_words (Lexing.from_string s) in
    let toks2 = norm_tokens true false false toks1 in
    let id, toks3 = identity_of_toks toks2 in
    if toks3 <> [] then
      failwith "syntax error";
    id
  with
    | Failure msg ->
	failwith ("identity_of_string: " ^ msg)


let options_of_toks toks opts_with_arg opts_wo_arg =
  let ht = Hashtbl.create 5 in
  let rec loop toks =
    match toks with
      | Chars s :: toks1 ->
	  if List.mem s opts_with_arg then (
	    match toks1 with
	      | Chars s' :: toks2 ->
		  Hashtbl.add ht s s';
		  loop toks2
	      | _ ->
		  failwith ("syntax error after " ^ s)
	  )
	  else
	    if List.mem s opts_wo_arg then (
	      Hashtbl.add ht s "";
	      loop toks1
	    )
	    else
	      toks
      | _ ->
	  toks
  in
  let toks' = loop toks in
  ht, toks'



let endpoint_of_toks toks =
  match toks with
    | Colon :: Chars("tcp"|"ssl" as typ) :: toks' ->
	let opts, toks'' = options_of_toks toks' ["-h";"-p";"-t"] ["-z"] in
	let host =
	  try Hashtbl.find opts "-h" with Not_found -> "*" in
	let host =
	  if host = "*" then "0.0.0.0" else host in
	let port =
	  try int_of_string(Hashtbl.find opts "-p") 
	  with 
	    | Not_found -> 0
	    | _ -> failwith "illegal port expression" in
	let timeout =
	  try Int32.of_string(Hashtbl.find opts "-t") 
	  with 
	    | Not_found -> (-1l)
	    | _ -> failwith "illegal timeout expression" in
	let compress =
	  Hashtbl.mem opts "-z" in
	let ep_obj =
	  ( object
	      method host = host
	      method port = port
	      method timeout = timeout
	      method compress = compress
	    end
	  ) in
	let ep =
	  match typ with
	    | "tcp" ->
		`TCP ep_obj
	    | "ssl" ->
		`SSL ep_obj
	    | _ ->
		assert false in
	(ep, toks'')
    | Colon :: Chars "udp" :: toks' ->
	assert false
	  (* TODO: Currently unsupported *)
    | Colon :: Chars "opaque" :: toks' ->
	assert false
	  (* TODO: Currently unsupported *)
    | _ ->
	failwith "syntax error"


let rec endpoints_of_toks toks =
  match toks with
    | [] ->
	[]
    | Colon :: _ ->
	let (ep, toks') = endpoint_of_toks toks in
	ep :: endpoints_of_toks toks'
    | _ ->
	failwith "syntax error"


let endpoints_of_string s =
  try
    let toks1 = Hydro_lexer.split_words (Lexing.from_string s) in
    let toks2 = norm_tokens false false true toks1 in
    let ep_list = endpoints_of_toks (Colon :: toks2) in
    (Array.of_list ep_list :> Hydro_types.endpoint array)
  with
    | Failure msg ->
	failwith ("endpoints_of_string: " ^ msg)


let proxy_addr_of_toks toks =
  let id, toks1 = 
    identity_of_toks toks in
  let opts, toks2 = 
    options_of_toks toks1 ["-f"] ["-t";"-o";"-O";"-d";"-D";"-s"] in
  let have_t = Hashtbl.mem opts "-t" in
  let have_o = Hashtbl.mem opts "-o" in
  let have_O = Hashtbl.mem opts "-O" in
  let have_d = Hashtbl.mem opts "-d" in
  let have_D = Hashtbl.mem opts "-D" in
  let mode =
    match (have_t, have_o, have_O, have_d, have_D) with
      | (false, false, false, false, false) ->
	  `Twoway
      | (true, false, false, false, false) ->
	  `Twoway
      | (false, true, false, false, false) ->
	  `Oneway
      | (false, false, true, false, false) ->
	  `Batch_oneway
      | (false, false, false, true, false) ->
	  `Datagram
      | (false, false, false, false, true) ->
	  `Batch_datagram
      | _ ->
	  failwith "Only one option of -t, -o, -O, -d, or -D is allowed" in
  let facet =
    try Some(Hashtbl.find opts "-f") with Not_found -> None in
  let secure = Hashtbl.mem opts "-s" in
  let adapter_opt, toks3 =
    match toks2 with
      | At :: Chars adapter_id :: toks' ->
	  (Some adapter_id), toks'
      | _ ->
	  None, toks2 in
  let ep_list = endpoints_of_toks toks3 in
  let params =
    match adapter_opt with
      | None ->
	  if ep_list = [] then
	    `Well_known
	  else
	    `Endpoints (Array.of_list ep_list)
      | Some adapter ->
	  if ep_list <> [] then
	    failwith "An adapter is incompatible with direct endpoints";
	  `Adapter adapter in
  ( object 
      method id = id
      method facet = facet
      method mode = mode
      method secure = secure
      method parameters = params
    end : Hydro_types.proxy_addr
  )


let proxy_addr_of_string s =
  try
    let toks1 = Hydro_lexer.split_words (Lexing.from_string s) in
    let toks2 = norm_tokens true true true toks1 in
    let addr = proxy_addr_of_toks toks2 in
    addr
  with
    | Failure msg ->
	failwith ("proxy_addr_of_string: " ^ msg)


let esc_re = 
  Pcre.regexp "[/\\x5c\\x00-\\x1f\\x7f-\\xff]"

let escape enable_slash s =
  Pcre.substitute
    ~rex:esc_re
    ~subst:(fun m ->
	      match m with
		| "/" -> if enable_slash then "\\/" else "/"
		| "\\" -> "\\\\"
		| "'" -> "\\'"
		| "\"" -> "\\\""
		| "\008" -> "\\b"
		| "\012" -> "\\f"
		| "\010" -> "\\n"
		| "\013" -> "\\r"
		| "\009" -> "\\t"
		| _ -> Printf.sprintf "\\%03o" (Char.code m.[0])
	   )
    s


let string_of_identity (id : Hydro_types.identity) =
    if id#category = "" then
      escape true id#name
    else
      escape true id#category ^ "/" ^ escape true id#name


let string_of_endpoints (epa : Hydro_types.endpoint array) =
  let b = Buffer.create 100 in
  for k = 0 to Array.length epa - 1 do
    if k > 0 then Buffer.add_string b " : ";
    let ep = epa.(k) in
    match ep with
      | `TCP ep_obj | `SSL ep_obj ->
	  let proto =
	    match ep with
	      | `TCP _ -> "tcp"
	      | `SSL _ -> "ssl"
	      | _ -> assert false in
	  bprintf b "%s -h %s -p %d" proto ep_obj#host ep_obj#port;
	  let tmo = ep_obj#timeout in
	  if tmo >= 0l then
	    bprintf b " -t %ld" tmo;
	  if ep_obj#compress then
	    bprintf b " -z"

      | `UDP ep_obj ->
	  assert false  (* TODO *)

      | `Unknown _ ->
	  assert false  (* TODO *)

  done;
  Buffer.contents b


let unsafe_re = Pcre.regexp "[ :@]"

let needs_quotes s =
  Pcre.pmatch ~rex:unsafe_re s

let add_maybe_quoted b s =
  if needs_quotes s then
    bprintf b "\"%s\"" s
  else
    Buffer.add_string b s


let string_of_proxy_addr pa =
  let b = Buffer.create 100 in
  let id_string = string_of_identity pa#id in
  add_maybe_quoted b id_string;
  ( match pa#facet with
      | None -> ()
      | Some s ->
	  Buffer.add_string b " -f ";
	  add_maybe_quoted b s;
  );
  let mode_opt =
    match pa#mode with
      | `Twoway -> "-t"
      | `Oneway -> "-o"
      | `Batch_oneway -> "-O"
      | `Datagram -> "-d"
      | `Batch_datagram -> "-D" in
  bprintf b " %s" mode_opt;
  if pa#secure then Buffer.add_string b " -s";
  ( match pa#parameters with
      | `Well_known -> ()
      | `Adapter s ->
	  Buffer.add_string b " @";
	  add_maybe_quoted b s
      | `Endpoints ep ->
	  Buffer.add_string b " : ";
	  Buffer.add_string b (string_of_endpoints ep)
  );
  Buffer.contents b

