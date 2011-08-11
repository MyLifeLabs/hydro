(* $Id$ *)

open Hgen_types
open Hgen_parser

(* --- Lexing --- *)

let location bolloc lexbuf =
  { file = bolloc.file;
    line = bolloc.line;
    bol = bolloc.bol;
    offset = Lexing.lexeme_start lexbuf
  }

let bolloc_multi_nl bolloc lexbuf n m =
  { file = bolloc.file;
    line = bolloc.line + n;
    bol = Lexing.lexeme_end lexbuf - m;
    offset = Lexing.lexeme_end lexbuf
  }

let bolloc_nl bolloc lexbuf =
  (* next line, when lexbuf just consumed the LF *)
  bolloc_multi_nl bolloc lexbuf 1 0
    
let count_lf s =
  let n = ref 0 in
  let m = ref 0 in
  for k = 0 to String.length s - 1 do
    if s.[k] = '\n' then 
      ( incr n; m := 0 )
    else
      incr m
  done;
  !n, !m
    
let at_bol bolloc lexbuf =
  bolloc.bol = Lexing.lexeme_start lexbuf

let loc_of_token =
  function
    | ( K_BOOL loc | K_ENUM loc | K_IMPLEMENTS loc | K_MODULE loc |
        K_STRUCT loc | K_BYTE loc | K_EXCEPTION loc | 
        K_THROWS loc | K_CLASS loc | K_EXTENDS loc | K_INTERFACE loc |
        K_OUT loc | K_TRUE loc | K_CONST loc | K_FALSE loc | K_LOCAL loc |
        K_SEQUENCE loc | K_VOID loc | K_DICTIONARY loc |
        K_SHORT loc | K_DOUBLE loc | K_IDEMPOTENT loc | K_LONG loc |
        K_STRING loc | K_INT loc | K_FLOAT loc |
        IDENT(_,loc) | STRING_LITERAL(_,loc,_) | INT_LITERAL(_,loc) |
        FLOAT_LITERAL(_,loc) | 
        LBRACE loc | RBRACE loc | LANGLE loc | RANGLE loc | LPAREN loc |
        RPAREN loc | LBRACK loc | RBRACK loc | LDBRACK loc | RDBRACK loc |
        SEMI loc | COMMA loc | ASTERISK loc | DCOLON loc | EQUAL loc | EOF loc
      ) ->
	loc



let print_meta_def = Hgen_parser_util.print_meta_def


module TS_util = struct
  open Hgen_types.TS
  open Printf

  let colon_name =
    function
      | `Absolute path ->
	  "::" ^ (String.concat "::" path)
      | `Relative path ->
	  (String.concat "::" path)

  let mapped_name ent =
    match ent with
      | `Module hm -> hm#mapped_name
      | `Object ho -> ho#mapped_name
      | `Exn he -> he#mapped_name
      | `Type ht -> ht#mapped_name
      | `Const hc -> assert false (* CHECK *)

  let rec typeterm_string tt =
    match tt with
      | `Void -> "void"
      | `Bool -> "bool"
      | `Byte -> "byte"
      | `Short -> "short"
      | `Int -> "int"
      | `Int32 -> "int32"
      | `Long -> "long"
      | `Float -> "float"
      | `Double -> "double"
      | `String -> "string"
      | `Byteseq -> "sequence<byte>"
      | `Enum l -> "[" ^ String.concat "|" (Array.to_list l) ^ "]"
      | `Struct(d,eq_opt) ->
	  let d = Array.to_list d in
	  "{ "  ^ 
	    String.concat "; " 
	      (List.map
		 (fun (n,mn,tt',is_mutable) -> n ^ "/" ^ mn ^ " : " ^ (if is_mutable then " mutable " else "") ^ (typeterm_string tt'))
		 d
	      ) ^ " }"
      | `Struct_tuple d ->
	  let d = Array.to_list d in
	  "( "  ^ 
	    String.concat " * " 
	      (List.map
		 (fun (n,tt') -> n ^ " : " ^ (typeterm_string tt'))
		 d
	      ) ^ " )"
      | `Sequence tt' -> "sequence<" ^ typeterm_string tt' ^ ">"
      | `Dictionary(tt1, tt2) ->
	  "dictionary<" ^ typeterm_string tt1 ^ "," ^ 
	    typeterm_string tt2 ^ ">"
      | `Proxy n -> "proxy<" ^ colon_name n ^ ">"
      | `Object n -> "object<" ^ colon_name n ^ ">"
      | `Named ht -> colon_name ht#name
      | `User_mapping(tt',_,_,_) -> typeterm_string tt'

  let print_operation hf =
    printf "    op %s\n" hf#name;
    printf "      mapped_name=%s\n" hf#mapped_name;
    if hf#mode = `Idempotent then
      printf "      idempotent\n";
    Array.iter
      (fun (n,mn,tt) ->
	 printf "      in %s/%s : %s\n" n mn (typeterm_string tt)
      )
      hf#in_args;
    if hf#in_classes then
      printf "      in_classes\n";
    Array.iter
      (fun (n,mn,tt) ->
	 printf "      out %s/%s : %s\n" n mn (typeterm_string tt)
      )
      hf#out_args;
    printf "      res : %s\n" (typeterm_string hf#result);
    if hf#out_classes then
      printf "      out_classes\n";
    List.iter
      (fun he ->
	 printf "      throws %s\n" (colon_name he#name);
      )
      hf#throws;
    List.iter (fun m -> printf "      meta=%s\n" (print_meta_def m)) hf#meta

  let print_symboltable (symtab : entity CiHashtbl.t) =
    CiHashtbl.iter
      (fun strn def ->
	 printf "%s:\n" strn;
	 match def with
	   | `Module hm ->
	       printf "  Module\n";
	       printf "    mapped_name=%s\n" hm#mapped_name;
	       List.iter (fun m -> printf "    meta=%s\n" (print_meta_def m)) hm#meta

	   | `Object ho ->
	       ( match ho#objtype with
		   | `Interface -> printf "  Interface\n";
		   | `Class ->     printf "  Class\n";
	       );
	       printf "    mapped_name=%s\n" ho#mapped_name;
	       ( match ho#super with
		   | None -> ()
		   | Some ho' ->
		       printf "    super_class=%s\n" (colon_name ho'#name)
	       );
	       List.iter
		 (fun ho' ->
		    printf "    super_intf=%s\n" (colon_name ho'#name)
		 )
		 ho#super_intf;
	       Array.iter
		 (fun (n, mn, tt) ->
		    printf "    data %s/%s : %s\n" n mn (typeterm_string tt)
		 )
		 ho#data_elements;
	       List.iter
		 (fun hf ->
		    print_operation hf
		 )
		 ho#op_elements;
	       if ho#local then printf "    local\n";
	       List.iter (fun m -> printf "    meta=%s\n" (print_meta_def m)) ho#meta
       
	   | `Exn he ->
	       printf "  Exception\n";
	       printf "    mapped_name=%s\n" he#mapped_name;
	       ( match he#super with
		   | None -> ()
		   | Some he' ->
		       printf "    super_exn=%s\n" (colon_name he'#name)
	       );
	       Array.iter
		 (fun (n, mn, tt) ->
		    printf "    data %s/%s : %s\n" n mn (typeterm_string tt)
		 )
		 he#data_elements;
	       if he#local then printf "    local\n";
	       List.iter (fun m -> printf "    meta=%s\n" (print_meta_def m)) he#meta

	   | `Type ht ->
	       printf "  Type\n";
	       printf "    mapped_name=%s\n" ht#mapped_name;
	       printf "    = %s\n" (typeterm_string ht#term);
	       if ht#local then printf "    local\n";
	       List.iter (fun m -> printf "    meta=%s\n" (print_meta_def m)) ht#meta

	   | `Const (_,_,hc) ->
	       printf "  Const\n";
	       ( match hc with
		   | `Int x ->
		       printf "    = %d\n" x
		   | `Int32 x ->
		       printf "    = %ld\n" x
		   | `Int64 x ->
		       printf "    = %Ld\n" x
		   | `Float x ->
		       printf "    = %f\n" x
		   | `String x ->
		       printf "    = \"%s\"\n" (String.escaped x)
		   | `Bool x ->
		       printf "    = %b\n" x
	       )
      )
      symtab

end
