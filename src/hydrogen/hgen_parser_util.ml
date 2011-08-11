open Hgen_types

(* Meta defs *)

let meta_re = Pcre.regexp ":"

let parse_meta_def md : single_meta_def =
  let def = Pcre.split ~rex:meta_re ~max:(-1) md in
  match def with
    | [ "hydro"; "default"; "mutable" ] ->
	`Global_default_mutable
    | [ "hydro"; "default"; "immutable" ] ->
	`Global_default_immutable
    | [ "hydro"; "mutable" ] ->
	`Local_mutable
    | [ "hydro"; "immutable" ] ->
	`Local_immutable
    | [ "hydro"; "default"; "int" ] ->
	`Global_default_int
    | [ "hydro"; "default"; "int32" ] ->
	`Global_default_int32
    | [ "hydro"; "int" ] ->
	`Local_int
    | [ "hydro"; "int32" ] ->
	`Local_int32
    | [ "hydro"; "structprefix"; p ] ->
	`Local_structprefix p
    | [ "hydro"; "reserve"; "structprefix"; p ] ->
	`Global_reserve_structprefix p
    | [ "hydro"; "equals"; s ] ->
	`Local_equals s
    | [ "hydro"; "name"; s ] ->
	`Local_name s
    | [ "hydro"; "defmapping"; name; ocaml_type; map_to; map_from ] ->
	`Global_defmapping(name,ocaml_type,map_to,map_from)
    | [ "hydro"; "mapping"; name ] ->
	`Local_mapping name
    | [ "hydro"; "tuple" ] ->
	`Local_tuple
    | "hydro" :: _ ->
	failwith ("Unknown hydro meta definition: " ^ md)
    | [ "nonmutating" ] ->
	`Nonmutating
    | _ ->
	`Other md

let print_meta_def =
  function
    | `Global_default_mutable           -> "hydro:default:mutable"
    | `Global_default_immutable         -> "hydro:default:immutable"
    | `Local_mutable                    -> "hydro:mutable"
    | `Local_immutable                  -> "hydro:immutable"
    | `Global_default_int               -> "hydro:default:int"
    | `Global_default_int32             -> "hydro:default:int32"
    | `Local_int                        -> "hydro:int"
    | `Local_int32                      -> "hydro:int32"
    | `Local_structprefix p             -> "hydro:structprefix:" ^ p
    | `Global_reserve_structprefix p    -> "hydro:reserver:structprefix:" ^ p
    | `Local_equals s                   -> "hydro:equals:" ^ s
    | `Local_name s                     -> "hydro:name:" ^ s
    | `Global_defmapping(s1,s2,s3,s4)   -> "hydro:defmapping:" ^ s1 ^ ":" ^
	                                    s2 ^ ":" ^ s3 ^ ":" ^ s4
    | `Local_mapping s                  -> "hydro:mapping:" ^ s
    | `Local_tuple                      -> "hydro:tuple"
    | `Nonmutating                      -> "nonmutating"
    | `Other s                          -> s
