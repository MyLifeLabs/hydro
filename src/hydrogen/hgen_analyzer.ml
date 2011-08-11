(* Symbol table stuff etc. *)

open Hgen_types

let parse filename f =
  let lexbuf = Lexing.from_channel f in
  let lastloc = ref None in
  let scan = Hgen_lexer_driver.scan filename in
  let scan' lb =
    let tok = scan lb in
    lastloc := Some(Hgen_util.loc_of_token tok);
    tok
  in
  let defs =
    try
      Hgen_parser.start scan' lexbuf
    with
      | Parsing.Parse_error ->
	  ( match !lastloc with
	      | None -> assert false
	      | Some loc -> raise(Syntax_error loc)
	  ) in
  close_in f;
  defs


open TS
open Hgen_util.TS_util

(* TODO:
   - Introduced identifiers aren't checked (4.16.5 in the Ice manual)
 *)


type scope =
    { idents : CiSet.t;
      int_type : [ `Int | `Int32 ];
      recfield_type : [ `Mutable | `Immutable ]
    }


let exists_in_array f a =
  let rec loop k =
    if k < Array.length a then
      f a.(k) || loop (k+1)
    else
      false
  in
  loop 0


let array_for_all f a =
  let flag = ref true in
  let k = ref 0 in
  while !flag && !k < Array.length a do
    flag := f a.(!k);
    incr k
  done;
  !flag


let set_directmapping_flags symboltable =
  let dm_flags = Hashtbl.create 50 in

  let rec set_dm_flag ht =
    let flag = neither_classes_nor_proxies ht#term in
    ht # directmapping := flag;
    Hashtbl.add dm_flags ht flag

  and neither_classes_nor_proxies (tt : TS.ty) =
    match tt with
      | `Void | `Bool | `Byte | `Short | `Int | `Int32 | `Long
      | `Float | `Double | `String | `Byteseq | `Enum _ ->
	  true
      | `Struct(s,_) ->
	  array_for_all
	    (fun (_,_,tt',_) -> neither_classes_nor_proxies tt')
	    s
      | `Struct_tuple s ->
	  array_for_all
	    (fun (_,tt') ->  neither_classes_nor_proxies tt')
	    s
      | `Sequence tt' ->
	  neither_classes_nor_proxies tt'
      | `Dictionary(tt1,tt2) ->
	  neither_classes_nor_proxies tt1 &&  neither_classes_nor_proxies tt2
      | `User_mapping (tt',_,_,_) ->
	  neither_classes_nor_proxies tt'
      | `Named ht' ->
	  if not (Hashtbl.mem dm_flags ht') then
	    set_dm_flag ht';
	  !(ht'#directmapping)
      | `Proxy _ | `Object _ ->
	  (* Note that all recursive types must go through proxy or class,
             so we don't have this problem when we say [false] here
	   *)
	  false
  in

  CiHashtbl.iter
    (fun n ent ->
       match ent with
	 | `Module _ -> ()
	 | `Object _ -> ()
	 | `Exn _ -> ()
	 | `Const _ -> ()
	 | `Type ht ->
	     if not (Hashtbl.mem dm_flags ht) then
	       set_dm_flag ht
    )
    symboltable


let analyze defs =

  let symboltable = (CiHashtbl.create 100 : entity CiHashtbl.t) in
  let mapped_record_prefixes = ref StrSet.empty in
  let record_counter = ref 1 in
  let mappings = Hashtbl.create 5 in

  let uniquecheck nmstr scope loc =
    if CiSet.mem nmstr scope.idents then
      raise(Other_error(loc,"Identifier already in scope: " ^ nmstr)) in

  let nontoplevelcheck parent_opt loc =
    if parent_opt = None then
      raise(Other_error(loc,"This construct is not allowed at top-level")) in

  let novoid tt loc =
    if tt = `Void then
      raise(Other_error(loc, "Void is not allowed here")) in

  let user_name meta =
    try
      let m =
	List.find (function `Local_name _ -> true | _ -> false) meta in
      match m with
	| `Local_name l -> Some l
	| _ -> assert false
    with
      | Not_found -> None in


  let map_name parent_opt meta nmstr =
    (* Create the O'Caml name for the thing [nmstr] inside [parent_opt] *)
    let p =
      match parent_opt with
	| Some parent -> parent#mapped_name ^ "_"
	| None -> "" in
    let l =
      match user_name meta with
	| Some u -> u
	| None -> nmstr in
    p ^ l in

  let is_uppercase c =
    c >= 'A' && c <= 'Z' in

  let prefix1 s =
    (* e.g. thisIsALongWord => tialw. Always lowercase *)
    let u = ref [] in
    for k = 0 to String.length s - 1 do
      let c = s.[k] in
      if k=0 || is_uppercase c then
	u := (String.lowercase (String.make 1 c)) :: !u
    done;
    String.concat "" (List.rev !u) in

  let prefix2 s =
    (* e.g. thisIsALongWord => thisalowo. Always lowercase *)
    let u = ref [] in
    let last = ref false in
    for k = 0 to String.length s - 1 do
      let c = s.[k] in
      if k=0 || is_uppercase c then (
	u := (String.lowercase (String.make 1 c)) :: !u;
	last := true
      ) else
	if !last then (
	  u := (String.lowercase (String.make 1 c)) :: !u;
	  last := false
	)
    done;
    String.concat "" (List.rev !u) in

  let prefixes s =
    let l = ref [] in
    for k = 1 to String.length s do
      l := (String.sub s 0 k) :: !l
    done;
    List.rev !l in

  let map_struct meta structname =
    (* Returns the prefix for the components of the struct [structname] *)
    let p =
      try
	let d =
	  List.find
	    (function `Local_structprefix _ -> true | _ -> false)
	    meta in
	match d with
	  | `Local_structprefix p -> p
	  | _ -> assert false
      with
	| Not_found ->
	    let p1 = prefix1 structname in
	    let p2 = prefix2 structname in
	    let candidates = prefixes p1 @ prefixes p2 in
	    try
	      List.find
		(fun p -> not (StrSet.mem p !mapped_record_prefixes))
		candidates
	    with
	      | Not_found ->
		  (* Fallback method *)
		  let n = !record_counter in
		  incr record_counter;
		  "r" ^ string_of_int n in
    mapped_record_prefixes := StrSet.add p !mapped_record_prefixes;
    p
  in

  let apply_user_mapping meta ty loc =
    try
      let mapping =
	List.find (function `Local_mapping _ -> true | _ -> false) meta in
      match mapping with
	| `Local_mapping name ->
	    ( try
		let (ocaml_type, map_to, map_from) =
		  Hashtbl.find mappings name in
		let arity =  (* hack, hack,... *)
		  match ty with
		    | `Sequence _ -> 1
		    | `Dictionary _ -> 2
		    | `Struct(s,_) -> Array.length s
		    | `Struct_tuple s -> Array.length s
		    | _ -> 0 in
		Hgen_print_il.check_mapping_applicable arity ocaml_type name loc;
		(`User_mapping(ty, ocaml_type, map_to, map_from))
	      with
		| Not_found ->
		    raise(Other_error(loc, ("Unknown mapping: " ^ name)))
	    )
	| _ ->
	    assert false
    with
      | Not_found -> ty
  in

  let have_user_mapping meta ty loc =
    let ty' = apply_user_mapping meta ty loc in
    match ty' with
      | `User_mapping _ -> true
      | _ -> false
  in

  let rec lookup (`Absolute container) (name : AST.name) =
    (* Resolves [name]. If it is a relative name, it is resolved relative to
       the module [container] or one of its parent modules.
       Returns the found entity or raises Not_found
     *)
    match name with
      | `Absolute aname ->
	  let aname' = colon_name (`Absolute aname) in
	  CiHashtbl.find symboltable aname'
      | `Relative [ "Object" ] ->
	  lookup (`Absolute container) (`Absolute ["Ice"; "Object"])
      | `Relative [ "LocalObject" ] ->
	  lookup (`Absolute container) (`Absolute ["Ice"; "LocalObject"])
      | `Relative rname ->
	  let rec search rev_cont_prefix =
	    let cont_prefix = List.rev rev_cont_prefix in
	    let n = colon_name (`Absolute (cont_prefix @ rname)) in
	    ( try
		CiHashtbl.find symboltable n
	      with
		| Not_found ->
		    ( match rev_cont_prefix with
			| [] -> raise Not_found
			| _ :: rev_cont_prefix' ->  search rev_cont_prefix'
		    )
	    ) in
	  search (List.rev container)
  in

  let int_type scope meta =
    if List.mem `Local_int meta then
      `Int
    else
      if List.mem `Local_int32 meta then
	`Int32
      else
	( scope.int_type :> TS.ty )
  in

  let ty_of_ast (`Absolute container) scope typ local meta loc =
    (* local=false: enforces that used types are non-local, too *)
    match typ with
      | `Byte -> `Byte
      | `Bool -> `Bool
      | `Short -> `Short
      | `Int -> int_type scope meta
      | `Long -> `Long
      | `Float -> `Float
      | `Double -> `Double
      | `String -> `String
      | `Void -> `Void
      | `Name n ->
	  ( try
	      let hobj = lookup (`Absolute container) n in
	      match hobj with
		| `Type ht ->
		    if ht#local && not local then
		      raise(Other_error(loc, "Cannot use local type here: " ^
					  colon_name ht#name));
		    `Named ht
		| `Object ho ->
		    if ho#local && not local then
		      raise(Other_error(loc, "Cannot use local type here: " ^
					  colon_name ho#name));
		    `Object (ho#name)
		| _ ->
		    raise(Other_error(loc,
				      "This is not a type name: " ^
					colon_name n))
	    with
	      | Not_found ->
		  raise(Other_error(loc, "Type not found: " ^  colon_name n))
	  )
      | `Proxy n ->
	  ( try
	      let hobj = lookup (`Absolute container) n in
	      match hobj with
		| `Object ho ->
		    if ho#local && not local then
		      raise(Other_error(loc, "Cannot use local type here: " ^
					  colon_name ho#name));
		    `Proxy (ho#name)
		| _ ->
		    raise(Other_error(loc,
				      "This is not an interface name: " ^
					colon_name n))
	    with
	      | Not_found ->
		  raise(Other_error(loc, "Interface not found: " ^  colon_name n))
	  )
  in

  let rec type_contains_classes (tt:ty) =
    match tt with
      | `Object _ ->
	  true
      | `Named ht ->
	  type_contains_classes ht#term
      | `Struct(elems, _) ->
	  exists_in_array (fun (_,_,tt',_) -> type_contains_classes tt') elems
      | `Struct_tuple elems ->
	  exists_in_array (fun (_,tt') -> type_contains_classes tt') elems
      | `Sequence tt' ->
	  type_contains_classes tt'
      | `Dictionary(tt1,tt2) ->
	  type_contains_classes tt1 || type_contains_classes tt2
      | `User_mapping(tt',_,_,_) ->
	  type_contains_classes tt'
      | `Void | `Bool | `Byte | `Short | `Int | `Int32 | `Long
      | `Float | `Double | `String | `Byteseq | `Enum _ | `Proxy _ ->
	  false
  in

  let hfunction_of_ast (`Absolute container) scope local op =
    let set = ref CiSet.empty in
    List.iter
      (fun p ->
	 uniquecheck p#name scope op#loc;
	 if CiSet.mem p#name !set then
	   raise(Other_error(p#loc, "Parameter name is defined twice"));
	 set := CiSet.add p#name !set
      )
      op#params;
    let in_params =
      List.filter (fun p -> not p#out) op#params in
    let out_params =
      List.filter (fun p -> p#out) op#params in
    let in_args =
      List.map
	(fun p ->
	   let tt =
	     ty_of_ast (`Absolute container) scope p#typ local p#meta p#loc in
	   novoid tt p#loc;
	   let mn =
	     match user_name p#meta with
	       | Some u -> u
	       | None -> String.uncapitalize p#name in
	   (p#name, mn, tt))
	in_params in
    let out_args =
      List.map
	(fun p ->
	   let tt =
	     ty_of_ast (`Absolute container) scope p#typ local p#meta p#loc in
	   novoid tt p#loc;
	   let mn =
	     match user_name p#meta with
	       | Some u -> u
	       | None -> String.uncapitalize p#name in
	   (p#name, mn, tt))
	out_params in
    let res_tt =
      ty_of_ast (`Absolute container) scope op#typ local op#meta op#loc in

    let in_classes =
      List.exists (fun (_,_,tt) -> type_contains_classes tt) in_args in
    let out_classes =
      type_contains_classes res_tt ||
	List.exists (fun (_,_,tt) -> type_contains_classes tt) out_args in

    let throws =
      List.map
	(fun n ->
	   try
	     match lookup (`Absolute container) n with
	       | `Exn he ->
		   if not local && he#local then
		     raise(Other_error(op#loc, "Exception must not be local: " ^
					 colon_name n));
		   he
	       | _ ->
		   raise(Other_error(op#loc, "This is not an exception: " ^
				       colon_name n))
	   with
	     | Not_found ->
		 raise(Other_error(op#loc, "Exception not found: " ^
				     colon_name n))
	)
	op#throws in

    let mn =
      match user_name op#meta with
	| Some u -> u
	| None -> String.uncapitalize op#name in

    let hf =
      (object
	 method name = op#name
	 method mapped_name = mn
	 method mode =
	   if op#idempotent then
	     `Idempotent
	   else if List.mem `Nonmutating op#meta then
	     `Nonmutating
	   else
	     `Normal
	 method in_args = Array.of_list in_args
	 method in_classes = in_classes
	 method out_args = Array.of_list out_args
	 method result = res_tt
	 method out_classes = out_classes
	 method throws = throws
	 method meta = op#meta
       end : hfunction
      ) in
    hf
  in

  let rec data_members ho =
    (* Return all data members of the (class) object ho, including those of
       super classes
     *)
    (Array.to_list ho#data_elements) @
    ( match ho#super with
	| None -> []
	| Some ho -> data_members ho
    )
  in

  let operations ho =
    (* Return all operations of ho, inclusing those of super classes and
       interfaces
     *)
    let h = Hashtbl.create 10 in
    let rec collect ho =
      if Hashtbl.mem h ho then
	[]
      else (
	Hashtbl.add h ho ();
	ho#op_elements @
          ( match ho#super with
	      | None -> []
	      | Some ho -> collect ho
	  ) @
	  (List.flatten (List.map collect ho#super_intf))
      ) in
    collect ho
  in

  let tagged_members ho =
    List.map (fun (n, _, tt) -> (n, `Data tt)) (data_members ho) @
      List.map (fun hf -> (hf#name, `Op hf)) (operations ho)
  in

  let process_class (`Absolute modname) scope cd term =
    let super_n =
      match term#extends with
	| None ->
	    if cd#local then
	      `Absolute [ "Ice"; "LocalObject" ]
	    else
	      `Absolute [ "Ice"; "Object" ]
	| Some n ->
	    n in
    let super =
      try
	let hobj = lookup (`Absolute modname) super_n in
	match hobj with
	  | `Object ho ->
	      if ho#objtype <> `Class then
		raise(Other_error(cd#loc, "Class does not extend a class: " ^ colon_name super_n));
	      if not ho#defflag then
		raise(Other_error(cd#loc, "Super class is not yet defined: " ^ colon_name super_n));
	      if not cd#local && ho#local then
		raise(Other_error(cd#loc, "Super class is local: " ^ colon_name super_n));
	      ho
	  | _ -> raise Not_found
      with
	| Not_found ->
	    raise(Other_error(cd#loc, "Super class not found: " ^
				colon_name super_n)) in
    let super_members = tagged_members super in
    let intfs =
      List.map
	(fun n ->
	   ( try
	       let hobj = lookup (`Absolute modname) n in
		match hobj with
		  | `Object ho ->
		      (* objtype=`Class is ok here *)
		      if not ho#defflag then
			raise(Other_error(cd#loc, "Interface is not yet defined: " ^ colon_name n));
		      if not cd#local && ho#local then
			raise(Other_error(cd#loc, "Interface is local: " ^ colon_name n));
		      ho
		  | _ -> raise Not_found
	     with
	       | Not_found ->
		    raise(Other_error(cd#loc, "Interface not found: " ^
					colon_name n))
	   )
	)
	term#implements in
    (* Create class-local symbol table. all_class_symbols includes the
       super classes.
     *)
    let all_class_symbols = CiHashtbl.create 50 in
    let class_symbols = CiHashtbl.create 50 in
    (* First put super classes into symtable: *)
    List.iter
      (fun (n, mem) ->
	 CiHashtbl.replace all_class_symbols n mem
      )
      super_members;
    (* Now add locally defined symbols: *)
    List.iter
      (fun dm ->
	 uniquecheck dm#name scope dm#loc;
	 if CiHashtbl.mem all_class_symbols dm#name then
	   raise(Other_error(dm#loc, "This class member is already defined"));
	 let tt =
	   ty_of_ast (`Absolute modname) scope dm#typ cd#local dm#meta dm#loc in
	 novoid tt dm#loc;
	 CiHashtbl.replace all_class_symbols dm#name (`Data tt);
	 CiHashtbl.replace class_symbols dm#name (`Data tt);
      )
      term#data_members;
    List.iter
      (fun op ->
	 uniquecheck op#name scope op#loc;
	 let scope' =
	   { scope with idents = CiSet.add op#name scope.idents } in
	 if CiHashtbl.mem all_class_symbols op#name then
	   raise(Other_error(op#loc, "This class member is already defined"));
	 let hf = hfunction_of_ast (`Absolute modname) scope' cd#local op in
	 CiHashtbl.replace all_class_symbols op#name (`Op hf);
	 CiHashtbl.replace class_symbols op#name (`Op hf);
      )
      term#operations;
    (* Now check operations from the "implements" clause: *)
    let optable = CiHashtbl.create 50 in  (* Maps op names to interfaces *)
    List.iter
      (fun hi ->
	 List.iter
	   (fun hf ->
	      ( try
		  let hi' = CiHashtbl.find optable hf#name in
		  if hi <> hi' then
		    raise(Other_error(cd#loc, "'implements': Operation is inherited twice from different interfaces: " ^ hf#name));
		with
		  | Not_found ->
		      CiHashtbl.add optable hf#name hi
	      );
	      ( try
		  let mem = CiHashtbl.find all_class_symbols hf#name in
		  match mem with
		    | `Data _ ->
			raise(Other_error(cd#loc, "Member is already defined as data member, but expected to be an operation: " ^ hf#name))

		    | `Op hf' ->
			raise(Other_error(cd#loc, "Member already occurs in interfaces mentioned in 'implements'"))
		with
		  | Not_found ->
		      CiHashtbl.replace all_class_symbols hf#name (`Op hf)
	      )
	   )
	   (operations hi)
      )
      intfs;
    (* Get the data members in the right order: *)
    let data_members =
      List.map
	(fun dm ->
	   try
	     let mn =
	       match user_name dm#meta with
		 | Some u -> u
		 | None -> String.uncapitalize dm#name in
	     match CiHashtbl.find class_symbols dm#name with
	       | `Data tt -> (dm#name, mn, tt)
	       | _ -> assert false
	   with Not_found -> assert false
	)
	term#data_members in
    (* Get the operations in any order: *)
    let operations = ref [] in
    CiHashtbl.iter
      (fun n mem ->
	 match mem with
	   | `Data _ -> ()
	   | `Op hf -> operations := hf :: !operations
      )
      class_symbols;
    (* Return: *)
    (Some super, intfs, Array.of_list data_members, !operations)
  in

  let process_intf (`Absolute modname) scope id term =
    let def_super =
      if id#local then
	`Absolute [ "Ice"; "LocalObject" ]
      else
	`Absolute [ "Ice"; "Object" ] in
    let super_list =
      List.map
	(fun n ->
	   ( try
	       let hobj = lookup (`Absolute modname) n in
	       match hobj with
		 | `Object ho ->
		     (* We accept that we can extend classes. In that case
                        the class is simply taken as interface
                      *)
		     if not ho#defflag then
		       raise(Other_error(id#loc, "Super interface is not yet defined: " ^ colon_name n));
		     ho
		 | _ -> raise Not_found
	     with
	       | Not_found ->
		   raise(Other_error(id#loc, "Super interface not found: " ^
				       colon_name n))
	   )
	)
	(def_super :: term#extends) in
    let optable = CiHashtbl.create 50 in  (* Maps op names to interfaces *)
    List.iter
      (fun hi ->
	 List.iter
	   (fun hf ->
	      try
		let hi' = CiHashtbl.find optable hf#name in
		if hi <> hi' then
		  raise(Other_error(id#loc, "Operation is inherited twice from different interfaces: " ^ hf#name));
	      with
		| Not_found ->
		    CiHashtbl.add optable hf#name hi
	   )
	   (operations hi)
      )
      super_list;
    let local_optable = CiHashtbl.create 50 in  (* Maps op names to unit *)
    let elements =
      List.map
	(fun op ->
	   uniquecheck op#name scope op#loc;
	   if CiHashtbl.mem optable op#name then
	     raise(Other_error(op#loc, "Operation defined twice"));
	   if CiHashtbl.mem local_optable op#name then
	     raise(Other_error(op#loc, "Operation defined twice"));
	   CiHashtbl.add local_optable op#name ();
	   let scope' =
	     { scope with idents = CiSet.add op#name scope.idents } in
	   let hf = hfunction_of_ast (`Absolute modname) scope' id#local op in
	   hf
	)
	term#operations in
    (super_list, elements)
  in

  let process_exn (`Absolute modname) scope ed term =
    let super =
      match term#extends with
	| None -> None
	| Some n ->
	    ( try
		let hobj = lookup (`Absolute modname) n in
		match hobj with
		  | `Exn he ->
		      if not he#defflag then
			raise(Other_error(ed#loc, "Super exception is not yet defined: " ^ colon_name n));
		      Some he
		  | _ -> raise Not_found
	      with
		| Not_found ->
		    raise(Other_error(ed#loc, "Super exception not found: " ^
					colon_name n))
	    ) in
    let super_members =
      match super with
	| None -> []
	| Some sc -> data_members sc in
    (* Create exception-local symbol table. all_exn_symbols includes the
       super exceptions.
     *)
    let all_exn_symbols = CiHashtbl.create 50 in
    let exn_symbols = CiHashtbl.create 50 in
    (* First put super exceptions into symtable: *)
    List.iter
      (fun (n, _, tt) ->
	 CiHashtbl.replace all_exn_symbols n tt
      )
      super_members;
    (* Now add locally defined symbols: *)
    List.iter
      (fun dm ->
	 uniquecheck dm#name scope dm#loc;
	 if CiHashtbl.mem all_exn_symbols dm#name then
	   raise(Other_error(dm#loc, "This exception member is already defined"));
	 let tt =
	   ty_of_ast (`Absolute modname) scope dm#typ ed#local dm#meta dm#loc in
	 novoid tt dm#loc;
	 CiHashtbl.replace all_exn_symbols dm#name tt;
	 CiHashtbl.replace exn_symbols dm#name tt;
      )
      term#data_members;
    (* Get the data members in the right order: *)
    let data_members =
      List.map
	(fun dm ->
	   let mn =
	     match user_name dm#meta with
	       | Some u -> u
	       | None -> String.uncapitalize dm#name in
	   try
	     (dm#name, mn, CiHashtbl.find exn_symbols dm#name)
	   with Not_found -> assert false
	)
	term#data_members in
    (super, Array.of_list data_members)
  in

  let process_struct (`Absolute modname) structname scope sd term =
    (* Create struct-local symbol table *)
    let struct_symbols = CiHashtbl.create 50 in
    (* Now add locally defined symbols: *)
    List.iter
      (fun dm ->
	 uniquecheck dm#name scope dm#loc;
	 if CiHashtbl.mem struct_symbols dm#name then
	   raise(Other_error(dm#loc, "This struct member is already defined"));
	 let tt =
	   ty_of_ast (`Absolute modname) scope dm#typ sd#local dm#meta dm#loc in
	 novoid tt dm#loc;
	 CiHashtbl.replace struct_symbols dm#name tt;
      )
      term#data_members;
    (* Get the data members in the right order: *)
    let p = map_struct sd#meta structname in
    let data_members =
      List.map
	(fun dm ->
	   try
	     let is_mutable =
	       if List.mem `Local_mutable dm#meta then
		 true
	       else
		 if List.mem `Local_immutable dm#meta then
		   false
		 else
		   scope.recfield_type = `Mutable in
	     (dm#name,
	      ( match user_name dm#meta with
		  | None -> p ^ "_" ^ dm#name
		  | Some u -> u
	      ),
	      CiHashtbl.find struct_symbols dm#name,
	      is_mutable
	     )
	   with Not_found -> assert false
	)
	term#data_members in
    Array.of_list data_members
  in

  let checknewtype absname loc =
    (* Check that the type definition is new: *)
    try
      let hobj = CiHashtbl.find symboltable absname in
      match hobj with
	| `Type ht ->
	    raise(Other_error(loc, "Type defined twice"));
	| _ ->
	    raise(Other_error(loc, "Identifier already used for non-type entity"))
    with
      | Not_found -> ()
  in

  let checkenum names scope loc =
    (* Check that the enumeration is ok *)
    let h = CiHashtbl.create 10 in
    List.iter
      (fun n ->
	 if CiHashtbl.mem h n then
	   raise(Other_error(loc, "Value enumerated twice: " ^ n));
	 uniquecheck n scope loc;
	 CiHashtbl.add h n ()
      )
      names
  in

  let rec const_value (`Absolute modname) scope v t meta loc =
    let as_int i64 =
      match int_type scope meta with
	| `Int ->
	    `Int(Int64.to_int i64)
	| `Int32 ->
	    `Int32(Int64.to_int32 i64)
	| _ ->
	    assert false
    in

    match (v,t) with
      | (`Int x, `Byte) -> as_int x
      | (`Int x, `Short) -> as_int x
      | (`Int x, `Int) -> as_int x
      | (`Int x, `Long) -> `Int64 x
      | (`Float x, `Float) -> `Float x
      | (`Float x, `Double) -> `Float x
      | (`String x, `String) -> `String x
      | (`Bool x, `Bool) -> `Bool x
      | (`Name n, _) ->
	  ( try
	      let c = lookup (`Absolute modname) n in
	      match c with
		| `Const (_,_,v') -> v'
		| _ ->
		    raise(Other_error(loc, "Not a constant: " ^
					colon_name n))
	    with
	      | Not_found ->
		    raise(Other_error(loc, "Constant not found: " ^
					colon_name n))
	  )
      | (_, _) ->
	  raise(Other_error(loc, "Constant value does not fit to type"))
  in


  let rec process_def (`Absolute modname) scope parent_opt (def : AST.def) =
    (* modname: The names of the open modules, in inner-to-outer order
       scope: The set of scope names
     *)
    match def with
      | `Module md ->
	  let nmstr = md#name in
	  uniquecheck nmstr scope md#loc;
	  let name = modname @ [nmstr] in
	  let absname = colon_name (`Absolute name) in
	  let hm =
	    let mapped = map_name parent_opt md#meta nmstr in
	    ( object
		method name = `Absolute name
		method mapped_name = mapped
		method meta = md#meta
	      end : hmodule
	    ) in
	  ( try
	      let hobj = CiHashtbl.find symboltable absname in
	      match hobj with
		| `Module _ -> ()
		| _ ->
		    raise(Other_error(md#loc,"Identifier already defined"))
	    with
	      | Not_found -> ()
	  );
	  CiHashtbl.replace symboltable absname (`Module hm);
	  let scope' =
	    { scope with idents = CiSet.add nmstr scope.idents } in
	  process_defs (`Absolute name) scope' (Some hm) md#term

      | `Class cd ->
	  nontoplevelcheck parent_opt cd#loc;
	  let nmstr = cd#name in
	  uniquecheck nmstr scope cd#loc;
	  let name = modname @ [nmstr] in
	  let absname = colon_name (`Absolute name) in
	  ( (* Check whether existing declaration is compatible: *)
	    try
	      let hobj = CiHashtbl.find symboltable absname in
	      match hobj with
		| `Object ho ->
		    if ho#objtype <> `Class then
		      raise(Other_error(cd#loc, "In a previous declaration this is not a class"));
		    if ho#defflag then
		      raise(Other_error(cd#loc, "Class defined twice"));
		    if ho#local <> cd#local (* || ho#meta <> cd#meta *) then
		      raise(Other_error(cd#loc, "Class does not match previous declaration"));
		| _ ->
		    raise(Other_error(cd#loc, "Identifier already used for non-class entity"))
	    with
	      | Not_found -> ()
	  );
	  (* Preliminary class (to support recursive definitions): *)
	  let mapped = map_name parent_opt cd#meta nmstr in
	  let dm = ref false in
	  let hc_pre =
	    ( object
		method name = `Absolute name
		method mapped_name = mapped
		method imported_from = None
		method defflag = false
		method directmapping = dm
		method objtype = `Class
		method super= None
		method super_intf = []
		method data_elements = [| |]
		method op_elements = []
		method local = cd#local
		method meta = cd#meta
	      end : hobject
	    ) in
	  CiHashtbl.replace symboltable absname (`Object hc_pre);
	  (* Store class: *)
	  let scope' =
	    { scope with idents = CiSet.add nmstr scope.idents } in
	  let hc =
	    let (super, super_intf, data_elements, op_elements) =
	      match cd#term with
		| None -> (None, [], [| |], [])
		| Some t -> process_class (`Absolute modname) scope' cd t
	    in
	    ( object
		method name = `Absolute name
		method mapped_name = mapped
		method imported_from = None
		method defflag = cd#term <> None
		method directmapping = dm
		method objtype = `Class
		method super = super
		method super_intf = super_intf
		method data_elements = data_elements
		method op_elements = op_elements
		method local = cd#local
		method meta = cd#meta
	      end : hobject
	    ) in
	  CiHashtbl.replace symboltable absname (`Object hc)

      | `Intf id ->
	  nontoplevelcheck parent_opt id#loc;
	  let nmstr = id#name in
	  uniquecheck nmstr scope id#loc;
	  let name = modname @ [nmstr] in
	  let absname = colon_name (`Absolute name) in
	  ( (* Check whether existing declaration is compatible: *)
	    try
	      let hobj = CiHashtbl.find symboltable absname in
	      match hobj with
		| `Object hi ->
		    if hi#objtype <> `Interface then
		      raise(Other_error(id#loc, "In a previous declaration this is not an interface"));
		    if hi#defflag then
		      raise(Other_error(id#loc, "Interface defined twice"));
		    if hi#local <> id#local (* || hi#meta <> id#meta *) then
		      raise(Other_error(id#loc, "Interface does not match previous declaration"));
		| _ ->
		    raise(Other_error(id#loc, "Identifier already used for non-interface entity"))
	    with
	      | Not_found -> ()
	  );
	  (* Preliminary interface (to support recursive definitions): *)
	  let mapped = map_name parent_opt id#meta nmstr in
	  let dm = ref false in
	  let hi_pre =
	    ( object
		method name = `Absolute name
		method mapped_name = mapped
		method imported_from = None
		method defflag = false
		method directmapping = dm
		method objtype = `Interface
		method super = None
		method super_intf = []
		method data_elements = [| |]
		method op_elements = []
		method local = id#local
		method meta = id#meta
	      end : hobject
	    ) in
	  CiHashtbl.replace symboltable absname (`Object hi_pre);
	  (* Store interface: *)
	  let scope' =
	    { scope with idents = CiSet.add nmstr scope.idents } in
	  let hi =
	    let (super, elements) =
	      match id#term with
		| None -> ([], [])
		| Some t -> process_intf (`Absolute modname) scope' id t
	    in
	    ( object
		method name = `Absolute name
		method mapped_name = mapped
		method imported_from = None
		method defflag = id#term <> None
		method directmapping = dm
		method objtype = `Interface
		method super = None
		method super_intf = super
		method data_elements = [| |]
		method op_elements = elements
		method local = id#local
		method meta = id#meta
	      end : hobject
	    ) in
	  CiHashtbl.replace symboltable absname (`Object hi)

      | `Exn ed ->
	  nontoplevelcheck parent_opt ed#loc;
	  let nmstr = ed#name in
	  uniquecheck nmstr scope ed#loc;
	  let name = modname @ [nmstr] in
	  let absname = colon_name (`Absolute name) in
	  ( (* Check whether existing declaration is compatible: *)
	    try
	      let hobj = CiHashtbl.find symboltable absname in
	      match hobj with
		| `Exn he ->
		    if he#defflag then
		      raise(Other_error(ed#loc, "Exception defined twice"));
		    if he#local <> ed#local (* || he#meta <> ed#meta *) then
		      raise(Other_error(ed#loc, "Exception does not match previous declaration"));
		| _ ->
		    raise(Other_error(ed#loc, "Identifier already used for non-exception entity"))
	    with
	      | Not_found -> ()
	  );
	  (* Store exception: *)
	  let scope' =
	    { scope with idents = CiSet.add nmstr scope.idents } in
	  let he =
	    let (super, elements) =
	      match ed#term with
		| None -> (None, [| |])
		| Some t -> process_exn (`Absolute modname) scope' ed t
	    in
	    let mapped = map_name parent_opt ed#meta nmstr in
	    let dm = ref false in
	    ( object
		method name = `Absolute name
		method mapped_name = mapped
		method defflag = ed#term <> None
		method directmapping = dm
		method super = super
		method data_elements = elements
		method local = ed#local
		method meta = ed#meta
	      end : hexn
	    ) in
	  CiHashtbl.replace symboltable absname (`Exn he)

      | `Struct sd ->
	  nontoplevelcheck parent_opt sd#loc;
	  let nmstr = sd#name in
	  uniquecheck nmstr scope sd#loc;
	  let name = modname @ [nmstr] in
	  let absname = colon_name (`Absolute name) in
	  ( (* Check that the definition is new: *)
	    try
	      let hobj = CiHashtbl.find symboltable absname in
	      match hobj with
		| `Type ht ->
		    ( match ht#term with
			| `Struct _ -> ()
			| _ -> raise(Other_error(sd#loc, "Identifier already used for non-struct entity"))
		    );
		    if ht#defflag then
		      raise(Other_error(sd#loc, "Struct defined twice"));
		    if ht#local <> sd#local (* || ht#meta <> sd#meta *) then
		      raise(Other_error(sd#loc, "Struct does not match previous declaration"));
		| _ ->
		    raise(Other_error(sd#loc, "Identifier already used for non-type entity"))
	    with
	      | Not_found -> ()
	  );
	  (* Store type: *)
	  let scope' =
	    { scope with idents = CiSet.add nmstr scope.idents } in
	  let mapped = map_name parent_opt sd#meta nmstr in
	  let eq_opt =
	    try
	      let eq =
		List.find (function `Local_equals _ -> true|_->false) sd#meta in
	      match eq with
		| `Local_equals t -> Some t
		| _ -> assert false
	    with Not_found -> None in
	  let ht =
	    let elements =
	      match sd#term with
		| None -> [| |]
		| Some t ->
		    process_struct (`Absolute modname) nmstr scope' sd t
	    in
	    let use_tuples =
	      List.mem `Local_tuple sd#meta ||
		have_user_mapping sd#meta (`Struct (elements, eq_opt)) sd#loc in
	    let tt =
	      if use_tuples then
		`Struct_tuple(Array.map (fun (n,_,tt',_) -> (n,tt')) elements)
	      else
		`Struct (elements, eq_opt) in
	    let tt = apply_user_mapping sd#meta tt sd#loc in
	    let dm = ref false in  (* this flag is set later *)
	    ( object
		method name = `Absolute name
		method mapped_name = mapped
		method defflag = sd#term <> None
		method term = tt
		method directmapping = dm
		method local = sd#local
		method meta = sd#meta
	      end : htype
	    ) in
	  CiHashtbl.replace symboltable absname (`Type ht)

      | `Seq sd ->
	  nontoplevelcheck parent_opt sd#loc;
	  let nmstr = sd#name in
	  uniquecheck nmstr scope sd#loc;
	  let name = modname @ [nmstr] in
	  let absname = colon_name (`Absolute name) in
	  checknewtype absname sd#loc;
	  (* Store type: *)
	  let arg_tt =
	    ty_of_ast
	      (`Absolute modname) scope sd#arg_typ sd#local sd#arg_meta sd#loc in
	  novoid arg_tt sd#loc;
	  let tt =
	    if arg_tt = `Byte then
	      `Byteseq
	    else
	      `Sequence arg_tt in
	  let tt = apply_user_mapping sd#meta tt sd#loc in
	  let mapped = map_name parent_opt sd#meta nmstr in
	  let dm = ref false in  (* this flag is set later *)
	  let ht =
	    ( object
		method name = `Absolute name
		method mapped_name = mapped
		method defflag = true
		method term = tt
		method directmapping = dm
		method local = sd#local
		method meta = sd#meta
	      end : htype
	    ) in
	  CiHashtbl.replace symboltable absname (`Type ht)

      | `Dict sd ->
	  nontoplevelcheck parent_opt sd#loc;
	  let nmstr = sd#name in
	  uniquecheck nmstr scope sd#loc;
	  let name = modname @ [nmstr] in
	  let absname = colon_name (`Absolute name) in
	  checknewtype absname sd#loc;

	  (* Store type: *)
	  let arg1_tt =
	    ty_of_ast (`Absolute modname) scope sd#arg_typ1 sd#local
	      sd#arg_meta1 sd#loc in
	  let arg2_tt =
	    ty_of_ast (`Absolute modname) scope sd#arg_typ2 sd#local
	      sd#arg_meta2 sd#loc in
	  novoid arg1_tt sd#loc;
	  novoid arg2_tt sd#loc;
	  let tt =
	    `Dictionary(arg1_tt, arg2_tt) in
	  let tt = apply_user_mapping sd#meta tt sd#loc in
	  let mapped = map_name parent_opt sd#meta nmstr in
	  let dm = ref false in  (* this flag is set later *)
	  let ht =
	    ( object
		method name = `Absolute name
		method mapped_name = mapped
		method defflag = true
		method term = tt
		method directmapping = dm
		method local = sd#local
		method meta = sd#meta
	      end : htype
	    ) in
	  CiHashtbl.replace symboltable absname (`Type ht)

      | `Enum sd ->
	  nontoplevelcheck parent_opt sd#loc;
	  let nmstr = sd#name in
	  uniquecheck nmstr scope sd#loc;
	  let name = modname @ [nmstr] in
	  let absname = colon_name (`Absolute name) in
	  checknewtype absname sd#loc;
	  let scope' =
	    { scope with idents = CiSet.add nmstr scope.idents } in
	  checkenum sd#term scope' sd#loc;

	  (* CHECK: Create a constant for every enumerated value? *)

	  (* Store type: *)
	  let tt =
	    `Enum (Array.of_list sd#term) in
	  let tt = apply_user_mapping sd#meta tt sd#loc in
	  let mapped = map_name parent_opt sd#meta nmstr in
	  let dm = ref true in
	  let ht =
	    ( object
		method name = `Absolute name
		method mapped_name = mapped
		method defflag = true
		method term = tt
		method directmapping = dm
		method local = sd#local
		method meta = sd#meta
	      end : htype
	    ) in
	  CiHashtbl.replace symboltable absname (`Type ht)

      | `Const cd ->
	  nontoplevelcheck parent_opt cd#loc;
	  let nmstr = cd#name in
	  uniquecheck nmstr scope cd#loc;
	  let name = modname @ [nmstr] in
	  let absname = colon_name (`Absolute name) in
	  ( (* Check that the definition is new: *)
	    try
	      let hobj = CiHashtbl.find symboltable absname in
	      match hobj with
		| `Const (_, _, hc) ->
		    raise(Other_error(cd#loc, "Constant defined twice"));
		| _ ->
		    raise(Other_error(cd#loc, "Identifier already used for non-const entity"))
	    with
	      | Not_found -> ()
	  );
	  let v =
	    const_value
	      (`Absolute modname) scope cd#arg_value cd#arg_typ cd#meta
	      cd#loc in
	  let mname =
	    map_name parent_opt cd#meta nmstr in
	  CiHashtbl.replace
	    symboltable absname (`Const (`Absolute name, mname, v))

      | `GMeta md ->
	  ()

  and process_defs (`Absolute modname) scope parent_opt (defs : AST.def list) =
    let scope' = ref scope in
    List.iter
      (function
	 | `GMeta md ->
	     List.iter
	       (function
		  | `Global_default_mutable ->
		      scope' := { (!scope') with recfield_type = `Mutable }
		  | `Global_default_immutable ->
		      scope' := { (!scope') with recfield_type = `Immutable }
		  | `Global_default_int ->
		      scope' := { (!scope') with int_type = `Int }
		  | `Global_default_int32 ->
		      scope' := { (!scope') with int_type = `Int32 }
		  | `Global_reserve_structprefix p ->
		      mapped_record_prefixes :=
			StrSet.add p !mapped_record_prefixes
		  | `Global_defmapping(name,ocamltype,map_to,map_from) ->
		      if Hashtbl.mem mappings name then
			raise(Noloc_error("Mapping already exists: " ^ name));
		      Hashtbl.replace mappings name (ocamltype,map_to,map_from)
		  | `Nonmutating | `Other _ ->
		      ()
		  | d ->
		      raise(Noloc_error("Not allowed in global context: " ^
					  Hgen_parser_util.print_meta_def d))
	       )
	       md
	 | _ -> ()
      )
      defs;
    List.iter
      (process_def (`Absolute modname) !scope' parent_opt)
      defs
  in

  let init_scope =
    { idents =
	CiSet.add "Object" (CiSet.add "LocalObject" CiSet.empty);
      int_type = `Int32;
      recfield_type = `Immutable
    } in

  let dm = ref false in
  CiHashtbl.add symboltable
    "::Ice::Object"
    (`Object
       (object
	  method name = `Absolute [ "Ice"; "Object" ]
	  method mapped_name = "Ice_Object"
	  method imported_from = Some "Hydro_lm_IceObject"
	  method defflag = true
	  method directmapping = dm
	  method objtype = `Class
	  method super = None
	  method super_intf = []
	  method data_elements =
	    [| |] (* This is intentionally wrong! *)
	  method op_elements = []
	  method local = false
	  method meta = []
	end : hobject
       )
    );
  CiHashtbl.add symboltable
    "::Ice::LocalObject"
    (`Object
       (object
	  method name = `Absolute [ "Ice"; "LocalObject" ]
	  method mapped_name = "Ice_LocalObject"
	  method imported_from = Some "Hydro_lm_IceLocalObject"
	  method defflag = true
	  method directmapping = dm
	  method objtype = `Class
	  method super = None
	  method super_intf = []
	  method data_elements = [| |]
	  method op_elements = []
	  method local = true
	  method meta = []
	end : hobject
       )
    );

  Hashtbl.add
    mappings
    "list"
    ("$1 list", "Array.to_list", "Array.of_list");
  Hashtbl.add
    mappings
    "option"
    ("$1 option", "Hydro_lm.seq_to_option", "Hydro_lm.option_to_seq");
  Hashtbl.add
    mappings
    "hashtbl"
    ("($1,$2)Hashtbl.t",  "Hydro_lm.dict_to_hashtbl", "Hydro_lm.hashtbl_to_dict");
  Hashtbl.add
    mappings
    "strmap"
    ("($1,$2)Hydro_lm.strmap", "Hydro_lm.dict_to_strmap", "Hydro_lm.strmap_to_dict");
  Hashtbl.add
    mappings
    "complex"
    ("($1,$2)Hydro_lm.complex", "Hydro_lm.pair_to_complex", "Hydro_lm.complex_to_pair");
  Hashtbl.add
    mappings
    "identity"
    ("Hydro_types.identity", "Hydro_lm.pair_to_identity", "Hydro_lm.identity_to_pair");

  process_defs (`Absolute []) init_scope None defs;

  CiHashtbl.iter
    (fun n obj ->
       let defflag =
	 (* We don't enforce this for local declarations *)
	 match obj with
	   | `Module _ -> true
	   | `Object x -> x#defflag || x#local
	   | `Exn x -> x#defflag || x#local
	   | `Type x -> x#defflag || x#local
	   | `Const _ -> true in
       if not defflag then
	 raise(Noloc_error ("No definition given for " ^ n))
    )
    symboltable;

  set_directmapping_flags symboltable;

  symboltable
;;
