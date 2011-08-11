(* $Id$ *)

type loc = 
  { file : string;
    line : int;     (* Line number, starting with 1 *)
    offset : int;   (* Offset from beginning of file, starting with 0 *)
    bol : int;      (* The offset of the beginning of the current line *)
  }

exception Lexical_error of loc * string
exception Syntax_error of loc
exception Other_error of loc * string
exception Noloc_error of string

module CiString = struct  (* case-insensitive *)
  type t = string
  let equal s1 s2 =
    String.lowercase s1 = String.lowercase s2
  let hash s =
    Hashtbl.hash (String.lowercase s)
  let compare s1 s2 =
    String.compare (String.lowercase s1) (String.lowercase s2)
end

module CiHashtbl = Hashtbl.Make(CiString)
  
module StrSet = Set.Make(String)

module CiSet = Set.Make(CiString)

module CiMap = Map.Make(CiString)


type single_meta_def =
    [ `Global_default_mutable
    | `Global_default_immutable
    | `Local_mutable
    | `Local_immutable
    | `Global_default_int
    | `Global_default_int32
    | `Local_int
    | `Local_int32
    | `Local_structprefix of string
    | `Global_reserve_structprefix of string
    | `Local_equals of string
    | `Local_name of string
    | `Global_defmapping of string * string * string * string
    | `Local_mapping of string
    | `Local_tuple
    | `Nonmutating
    | `Other of string
    ]


module AST = struct
  type def =
    [ `Module of module_def
    | `Class of class_def
    | `Intf of intf_def
    | `Exn of exn_def
    | `Struct of struct_def
    | `Seq of seq_def
    | `Dict of dict_def
    | `Enum of enum_def
    | `Const of const_def
    | `GMeta of meta_def
    ]

  and module_def =
    < name : string;
      term : def list;
      meta : meta_def;
      loc : loc;
    >

  and class_def =
    < name : string;
      term : class_term option;
      meta : meta_def;
      local : bool;
      loc : loc;
    >

  and class_term =
    < extends : name option;
      implements : name list;
      data_members : data_member list;
      operations : operation list;
    >

  and intf_def =
    < name : string;
      term : intf_term option;
      meta : meta_def;
      local : bool;
      loc : loc;
    >

  and intf_term =
    < extends : name list;
      operations : operation list;
    >

  and exn_def =
    < name : string;
      term : exn_term option;
      meta : meta_def;
      local : bool;
      loc : loc;
    >

  and exn_term =
    < extends : name option;
      data_members : data_member list;
    >

  and struct_def =
    < name : string;
      term : struct_term option;
      meta : meta_def;
      local : bool;
      loc : loc;
    >

  and struct_term =
    < data_members : data_member list;
    >

  and seq_def =
    < name : string;
      arg_typ : typ;
      arg_meta : meta_def;
      meta : meta_def;
      local : bool;
      loc : loc;
    >

  and dict_def =
    < name : string;
      arg_typ1 : typ;
      arg_meta1 : meta_def;
      arg_typ2 : typ;
      arg_meta2 : meta_def;
      meta : meta_def;
      local : bool;
      loc : loc;
    >

  and enum_def =
    < name : string;
      term : string list;
      meta : meta_def;
      local : bool;
      loc : loc;
    >

  and const_def =
    < name : string;
      arg_typ : typ;
      arg_meta : meta_def;
      arg_value : value;
      meta : meta_def;
      loc : loc;
    >

  and data_member =
    < name : string;
      typ : typ;
      meta : meta_def;
      loc : loc;
    >

  and operation = 
    < name : string;
      typ : typ;   (* result type *)
      params : param list;
      throws : name list;
      meta : meta_def;
      idempotent : bool;
      loc : loc;
    >

  and param =
    < name : string;
      typ : typ;
      out : bool;
      meta : meta_def;
      loc : loc;
    >

  and typ =
    [ `Byte
    | `Bool
    | `Short
    | `Int
    | `Long
    | `Float
    | `Double
    | `String
    | `Void
    | `Name of name
    | `Proxy of name
    ]


  and value =
    [ `Int of int64
    | `Float of float
    | `String of string
    | `Bool of bool
    | `Name of name
    ]

  and meta_def =
      single_meta_def list

  and name =
    [ `Absolute of string list
    | `Relative of string list
    ]

end


module TS = struct
  (* Type system *)

  type name =
      [ `Absolute of string list ]

  type meta_def =
      single_meta_def list

  type ty =
      [ `Void
      | `Bool
      | `Byte
      | `Short
      | `Int     (* Prefer int as O'Caml type *)
      | `Int32   (* Prefer int32 as O'Caml type *)
      | `Long
      | `Float
      | `Double
      | `String
      | `Byteseq
      | `Enum of string array
      | `Struct of (string * string * ty * bool) array * string option
	  (* List: (original_name, mapped_name, type, mutability).
             The final string option is set when a type equation is given.
	   *)
      | `Struct_tuple of (string * ty) array
	  (* Structs represented as (immutable) tuples *)
      | `Sequence of ty
      | `Dictionary of ty * ty
      | `Proxy of name   (* name of the interface in [system] *)
      | `Object of name  (* name of the class in [system] *)
      | `Named of htype
      | `User_mapping of ty * string * string * string
	  (* (ice_type, ocaml_type, map_to, map_from) *)
      ]

  and hnamed =
      < name : name;
        mapped_name : string;    (* w/o prefix like "t_" etc. *)
        defflag : bool;          (* Can only be [false] for structs *)
	directmapping : bool ref;    (* whether DirectMapping is enabled *)
	local : bool;
	meta : meta_def;
      >
      (* Supertype of htype, hexn, hobject *)

  and htype =
      < name : name;
        mapped_name : string;    (* w/o prefix like "t_" etc. *)
        defflag : bool;            (* Can only be [false] for structs *)
        term : ty;
	directmapping : bool ref;    (* whether DirectMapping is enabled *)
	local : bool;
	meta : meta_def;
      >

  and hexn =
      < name : name;                (* Absolute name *)
        mapped_name : string;
        defflag : bool;            (* Whether this is the def not the decl *)
	directmapping : bool ref;  (* always false *)
        super : hexn option;       (* The next exception in the hierarchy *)
        data_elements : (string * string * ty) array;
	local : bool;
	meta : meta_def;
      >

  and hobject =
      < name : name;             (* Absolute name *)
        mapped_name : string;
        imported_from : string option;   (* O'Caml module name *)
        defflag : bool;
	directmapping : bool ref;  (* always false *)
	objtype : [ `Interface | `Class ];
        super : hobject option;
	   (* These are super classes. Only classes may have super classes *)
	super_intf : hobject list;
	   (* When a class has a super interface, this is the 
              "implements" relation. When an interface has super interfaces
              this is interface inheritance

              It is allowed to have a class as super interface. In this
              case only the interface aspect of the class is used.
            *)
        data_elements : (string * string * ty) array;
           (* data_elements are empty for `Interface. These are only the
              locally defined data members; effectively one has to add the
              members of the super class(es)
              - (original_name, mapped_name, type)
            *)
        op_elements : hfunction list;
	   (* The locally defined operations. Effectively, one has to add
              the ops from super class(es) and from super interfaces
            *)
	local : bool;
	meta : meta_def;
      >

  and hfunction =
      < name : string;
        mapped_name : string;
        mode : op_mode;
        in_args : (string * string * ty) array;
        in_classes : bool;  (* Whether in_args type has classes *)
        out_args : (string * string * ty) array;
        result : ty;
          (* Unclear whether further properties are needed here *)
        out_classes : bool; (* Whether out_args or result type has classes *)
	throws : hexn list;
	meta : meta_def;
      >

  and hmodule =
      < name : name;
        mapped_name : string;
	meta : meta_def;
      >

  and hconst =
    [ `Int of int
    | `Int32 of int32
    | `Int64 of int64
    | `Float of float
    | `String of string
    | `Bool of bool
    ]

  and op_mode =
    [ `Normal | `Idempotent | `Nonmutating ]


  type entity =
      [ `Module of hmodule
      | `Object of hobject   (* Interface/class *)
      | `Exn of hexn
      | `Type of htype
      | `Const of name * string (* mapped name *) * hconst
      ]
end


module IL = struct
  (* Intermediate language *)

  type type_term =
      [ `Unit
      | `Int
      | `Int32
      | `Int64
      | `String
      | `Bool
      | `Char
      | `Float
      | `Array of type_term
      | `Alist of type_term * type_term
      | `Record of (string * type_term * bool) array * string option
	  (* bool: whether mutable. string option: type equation *)
      | `Tuple of type_term list
      | `Variant of string array
      | `Fun of type_term * type_term
      | `Option of type_term
      | `Ref of type_term
      | `Object of string list * (string * type_term) list
	  (* inherit clauses, methods *)
      | `Named of string
      | `Named_arg1 of type_term * string  (* 't name *)
      | `Subst of type_term list * string
	  (* string may contain placeholders $1, $2, etc. They are replaced
             by the corresponding type_term (first in list is $1, etc.)
             Only used for hydro:defmapping feature!
           *)
      | `Value  (* = Hydro_types.value *)
      | `Opaque of type_term  (* Only effective as top-level marker *)
      ]

  type ctype_term =
      [ `Object of ctype_term list * (string * type_term) list
	  (* inherit clauses, methods *)
      | `Fun of type_term * ctype_term
	  (* Only allowed for class signatures *)
      | `Named of string
      ]

  type pat_term =
      (* Corresponds to Hydro_types.value *)
      [ `Bool of int
	  (* The int is the number of an enumerated local variable *)
      | `Byte of int
      | `Short of int
      | `Int of int
      | `Int32 of int
      | `Long of int
      | `Float of int
      | `Double of int
      | `String of int
      | `Sequence of int
      | `Byteseq of int
      | `Dictionary of int
      | `Enum of int
      | `Struct of int
      | `Null
      | `Class of int
      | `Proxy of int
      | `Constructor of string * int
	  (* constructor can be any variant or exception *)
      ]

  type expr_term =
      [ `Match1 of expr_term * pat_term * expr_term 
	  (* match e with p -> e' | _ -> fail *)
      | `Match1_Null of expr_term * pat_term * expr_term * expr_term
	  (* match e with p -> e' | VNull -> e'' | _ -> fail *)
      | `Match1_DM of expr_term * pat_term * expr_term * string
	  (*  match e with p -> e' | VDirectMapping(T x) -> x | _ -> fail *)
      | `Match_variants of expr_term * (string * expr_term) list
	  (* match e with `v1 -> e1 | `v2 -> e2 | .... *)
      | `Match_strings of expr_term * (string * expr_term) list * expr_term
	  (* match e with "s1" -> e1 | "s2" -> e2 | ... | _ -> eother *)
      | `Match_option of expr_term * int * expr_term * expr_term
	  (* match e1 with Some v1 -> e2 | None -> e3 *)
      | `Match_list of expr_term * int * int * expr_term
	  (* match e1 with v1 :: v2 -> e2 | [] -> fail *)
      | `Match_slice of expr_term * string * int list * expr_term
	  (* match e1 with `Decoded(s, [| v1; v2; ... |]) -> e2 | _ -> fail *)
      | `Match_tuple of expr_term * int list * expr_term
	  (* match e1 with (v1,v2,...) -> e2 *)
      | `Call of expr_term * expr_term list
	  (* f arg1 ... *)
      | `CallF of string * expr_term list
	  (* f arg1 ... *)
      | `CallC of string * expr_term list
	  (* Constructor arg1 ... - never optimized like a function *)
      | `CallM of expr_term * string
	  (* f # method *)
      | `Array_get of expr_term * expr_term
	  (* e.( f ) *)
      | `Array_lit of expr_term array
	  (* [| e1; e2; ... |] *)
      | `List_lit of expr_term list
	  (* [ e1; e2; ... ] *)
      | `List_cons of expr_term * expr_term
	  (* e1 :: e2 *)
      | `Record_get of expr_term * string
	  (* e.member *)
      | `Record_lit of (string * expr_term) array
	  (* { n1 = e1; n2 = e2; ... } *)
      | `Record_lit_ord of (string * expr_term) array
	  (* same, but it is ensured that the experssions are evaluated in 
             order
	   *)
      | `Int_lit of int
	  (* integer literal *)
      | `Int32_lit of int32
	  (* integer literal *)
      | `Int64_lit of int64
	  (* integer literal *)
      | `String_lit of string
	  (* string literal *)
      | `Float_lit of float
	  (* fp literal *)
      | `Bool_lit of bool
	  (* boolean literal *)
      | `Pair of expr_term * expr_term
	  (* (e1,e2) *)
      | `Tuple of expr_term list
	  (* (e1,e2, ...). For empty list: () *)
      | `Tuple_ord of expr_term list
	  (* same, but it is ensured that the experssions are evaluated in 
             order
	   *)
      | `Evar of int
	  (* Enumerated local variable *)
      | `Var of string
	  (* any O'Caml variable *)
      | `Dtvar of string
	  (* any named defterm (only valid in defterms!) *)
      | `Variant of string
	  (* `name *)
      | `Fun0 of expr_term
	  (* (fun () -> e) *)
      | `Fun of int * expr_term
	  (* (fun v -> e) *)
      | `Fun2 of int * int * expr_term
	  (* (fun (v1,v2) -> e) *)
      | `FunN of int list * expr_term
	  (* (fun (v1,v2,...) -> e). For empty list: (fun () -> e) *)
      | `MFun of int list * expr_term
	  (* (fun v1 v2 ... -> e) *)
      | `Object of string * class_term list * (string * expr_term) list * string
	  (* ( object(s) inherit ... method m1=e1 method m2=e2 ... end : ty) *)
      | `Ifthenelse of expr_term * expr_term * expr_term
	  (* if e1 then e2 else e3 *)
      | `Statements of expr_term array
	  (* e1; e2; ... *)
      | `Typeann of expr_term * string
	  (* (expr : t) *)
      | `Coerce of string * string * string
	  (* (self : #s :> t) *)
      | `Coerce_expr of expr_term * string * string
	  (* (self : #s :> t) *)
      | `Catch_not_found of expr_term
	  (* try e with Not_found -> fail *)
      | `Catch_inflated of expr_term * string
	  (* try e with <Name> x -> x | _ -> raise Hydro_lm.Invalid_coercion *)
      | `Catch_userexn of expr_term * int * expr_term * int * expr_term
	  (* try e1 with User_exception v1 -> e1 | v2 -> e2 *)
      | `Raise of string
	  (* Raise an exception *)
      | `Raise_expr of expr_term
	  (* Raise an exception *)
      | `Fail
	  (* "lm_error" style failure *)
      | `Noreturn of expr_term
	  (* let _ = expr in assert false *)
      ]

  and  class_term =
      [ `Var of string
	  (* any O'Caml variable *)
      | `Fun of int * class_term
	  (* (fun v -> ct) *)
      | `FunN of int list * class_term
	  (* (fun (v1,v2,...) -> ct) *)
      | `Call of class_term * expr_term list
	  (* ct arg1 arg2 ... *)
      | `Object of string * class_term list * (string * expr_term) list * string
	  (* ( object(s) inherit ... method m1=e1 method m2=e2 ... end : ty) *)
      | `DataObject of class_term list * (string * expr_term) list * string
	  (* ( object inherit ... val v1=ref e1 val v2=ref e2 ... method m1=v1 method m2=v2 ... end : ty) *)
      ]

  type dt_type =
      [ `Type | `Exn | `Intf | `Class | `Ctor | `Omit ]

  type pad =
      (* All lists in reverse order *)
      { mutable types : (string * type_term) list;
	mutable ctypes : (string * ctype_term) list; 
	mutable lets : (string * expr_term) list; (* For constants *)
	mutable letrec_sigs : (string * type_term) list;
	mutable letrecs : (string * expr_term) list;  (* Before User_exception *)
	mutable letrecs2 : (string * expr_term) list; (* After User_exception *)
	mutable classes : (string * class_term) list;
	mutable class_sigs : (string * ctype_term) list;
	mutable letrecs3 : (string * expr_term) list;   (* After classes *)
	mutable defterms : (string * string * dt_type * expr_term) list;
	   (* mapped_name, absolute ID, type, def expr *)

	mutable exns : (string * type_term * string option) list;
      }

end
