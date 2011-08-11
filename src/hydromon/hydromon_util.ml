open Hydromon_proto
open Printf

type key = 
    { host : Unix.inet_addr;
      port : int;
      id_name : string;
      id_cat : string;
      operation : string;
      idempotent : bool;
    }


let key_string key =
  sprintf "%s:%d identity=%s operation=%s idem=%b"
    (Unix.string_of_inet_addr key.host)
    key.port
    (Hydro_string.string_of_identity
       (object method name = key.id_name method category = key.id_cat end))
    key.operation
    key.idempotent


let addr_of_key key =
  let id =
    ( object
	method name = key.id_name
	method category = key.id_cat
      end
    ) in
  let ep  =
    ( object
	method host = Unix.string_of_inet_addr key.host
	method port = key.port
	method timeout = (-1l)
	method compress = false
      end
    ) in
  let addr =
    ( object
	method id = id
	method facet = None
	method mode = `Twoway
	method secure = false
	method parameters = `Endpoints [| `TCP ep |]
      end
    ) in
  addr


let pr_of_key key =
  Hydro_lm.pr_of_address (addr_of_key key)


let key_of_addr addr operation idempotent =
  if addr#facet <> None then
    failwith "Only the default facet is supported";
  if addr#mode <> `Twoway then
    failwith "Only twoway communication is supported";
  if addr#secure then
    failwith "Secure communication is not supported";
  let epa =
    match addr#parameters with
      | `Endpoints epa -> epa
      | _ -> failwith "Indirect proxies are not supported" in
  let tcp_ep =
    try
      List.find
	(function `TCP _ -> true | _ -> false)
	(Array.to_list epa) 
    with Not_found -> failwith "No TCP address found" in
  let (host,port) =
    match tcp_ep with
      | `TCP ep -> ep#host, ep#port
      | _ -> assert false in
  let host_ip =
    try Unix.inet_addr_of_string host 
    with _ -> failwith "Host must be given as IP address" in
  let key =
    { host = host_ip;
      port = port;
      id_name = addr#id#name;
      id_cat = addr#id#category;
      operation = operation;
      idempotent = idempotent
    } in
  key


let key_of_pr pr operation idempotent =
  key_of_addr (Hydro_lm.Unsafe.unwrap_proxy pr) operation idempotent 

