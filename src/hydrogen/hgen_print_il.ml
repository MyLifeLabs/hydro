(* $Id$ *)

open Hgen_types
open Hgen_types.IL
open Format

let error_cnt = ref 0

let lm_error() =
  let n = !error_cnt in
  incr error_cnt;
  "Hydro_lm.error " ^ string_of_int n

let pvar_cnt = ref 0

let new_pvar() =
  let p = !pvar_cnt in
  incr pvar_cnt;
  p

let check_mapping_applicable arity pat name loc =
  (* See also `Subst case in print_type_term *)
  try
    let b = Buffer.create 100 in
    Buffer.add_substitute 
      b
      (fun s ->
	 let n =
	   try int_of_string s
	   with _ ->
	     failwith("Not a number: " ^ s) in
	 if n < 1 || n > arity then failwith("Mapping not applicable");
	 ""
      )
      pat;
    ()
  with
    | Failure msg ->
	raise(Other_error(loc, "While applying mapping " ^ name ^ ": " ^ msg))



let rec print_type_term f ty =
  match ty with
    | `Unit -> 
	fprintf f "unit"
    | `Int -> 
	fprintf f "int"
    | `Int32 -> 
	fprintf f "int32"
    | `Int64 -> 
	fprintf f "int64"
    | `String -> 
	fprintf f "string"
    | `Bool -> 
	fprintf f "bool"
    | `Char -> 
	fprintf f "char"
    | `Float -> 
	fprintf f "float"
    | `Array ty' ->
	fprintf f "@[<hov 2>";
	print_type_term f ty';
	fprintf f "@ array@]"
    | `Alist(ty1,ty2) ->
	fprintf f "@[<hov 2>( @[<hov 2>";
	print_type_term f ty1;
	fprintf f " *@ ";
	print_type_term f ty2;
	fprintf f "@])@ list@]"
    | `Tuple tup ->
	if tup = [] then
	  fprintf f "unit"
	else (
	  fprintf f "( @[<hov 2>";
	  let first = ref true in
	  List.iter
	    (fun ty ->
	       if not !first then
		 fprintf f " *@ ";
	       print_type_term f ty;
	       first := false
	    )
	    tup;
	  fprintf f "@])"
	)
    | `Record(r,eq_opt) ->
	( match eq_opt with
	    | None -> ()
	    | Some eq ->
		fprintf f "%s =@ " eq
	);
	fprintf f "{ @[<hov 2>";
	Array.iter
	  (fun (n, ty', is_mutable) ->
	     if is_mutable then fprintf f "mutable ";
	     fprintf f "%s : " n;
	     print_type_term f ty';
	     fprintf f ";@ ";
	  )
	  r;
	fprintf f "@] }"
    | `Variant tags ->
	fprintf f "[ @[<hov 2>";
	Array.iter
	  (fun n ->
	     fprintf f "| `%s@ " n;
	  )
	  tags;
	fprintf f "@] ]"
    | `Fun(ty1,ty2) ->
	fprintf f "@[<hov 2>( @[<hov 2>";
	print_type_term f ty1;
	fprintf f "@] ->@ ";
	print_type_term f ty2;
	fprintf f " )@]"
    | `Option ty' ->
	print_type_term f (`Named_arg1(ty', "option"))
    | `Ref ty' ->
	print_type_term f (`Named_arg1(ty', "ref"))
    | `Object (inheritance,obj) ->
	assert(inheritance = []);   (* Not possible here *)
	fprintf f "@[<hv>@[<hv 2>< ";
	let first = ref true in
	List.iter
	  (fun (n, ty) ->
	     if not !first then fprintf f "@ ";
	     fprintf f "@[<hv 2>%s :@ " n;
	     print_type_term f ty;
	     fprintf f "@];";
	     first := false;
	  )
	  obj;
	fprintf f "@]@ @]>"
    | `Named s ->
	fprintf f "%s" s
    | `Named_arg1 (arg1, s) ->
	fprintf f "@[<hov 2>( ";
	print_type_term f arg1;
	fprintf f " )@ %s@]" s
    | `Value ->
	fprintf f "Hydro_types.value"
    | `Opaque ty ->
	print_type_term f ty
    | `Subst(l, pat) ->
	(* See also check_mapping_applicable above! *)
	(* This is a bit hackish... *)
	let b = Buffer.create 100 in
	Buffer.add_substitute 
	  b
	  (fun s ->
	     let n =
	       try int_of_string s
	       with _ ->
		 failwith("Not a number: " ^ s) in
	     let ty' =
	       try List.nth l (n-1)
	       with _ ->
		 failwith("Mapping not applicable")  in
	     let b' = Buffer.create 100 in
	     let f' = Format.formatter_of_buffer b' in
	     print_type_term f' ty';
	     Format.pp_print_flush f' ();
	     Buffer.contents b'
	  )
	  pat;
	fprintf f "(%s)" (Buffer.contents b)


let rec print_ctype f ct =
  (* Prints everything after class type <name> = *)
  match ct with
    | `Fun(ty, ct')->
	(* `Fun makes only sense after "class <name> : " *)
	fprintf f "@[<hov 2>@[<hov 2>";
	print_type_term f ty;
	fprintf f "@] ->@ ";
	print_ctype f ct';
	fprintf f "@]"
    | `Object(inherits, methods) ->
	fprintf f "@[<v>@[<v 2>object";
	List.iter
	  (fun basetype ->
	     fprintf f "@ @[<hv 2>inherit ";
	     print_ctype f basetype;
	     fprintf f "@]"
	  )
	  inherits;
	List.iter
	  (fun (methname,ty) ->
	     fprintf f "@ @[<hv 2>method %s : " methname;
	     print_type_term f ty;
	     fprintf f "@]"
	  )
	  methods;
	fprintf f "@]@ end@]"
    | `Named s ->
	fprintf f "%s" s
	


let rec print_pat_term f pat =
  match pat with
    | `Bool v ->
	fprintf f "VBool e%d" v
    | `Byte v ->
	fprintf f "VByte e%d" v
    | `Short v ->
	fprintf f "VShort e%d" v
    | `Int v ->
	fprintf f "VInt e%d" v
    | `Int32 v ->
	fprintf f "VInt32 e%d" v
    | `Long v ->
	fprintf f "VLong e%d" v
    | `Float v ->
	fprintf f "VFloat e%d" v
    | `Double v ->
	fprintf f "VDouble e%d" v
    | `String v ->
	fprintf f "VString e%d" v
    | `Sequence v ->
	fprintf f "VSequence e%d" v
    | `Byteseq v ->
	fprintf f "VByteseq e%d" v
    | `Dictionary v ->
	fprintf f "VDictionary e%d" v
    | `Enum v ->
	fprintf f "VEnum e%d" v
    | `Struct v ->
	fprintf f "VStruct e%d" v
    | `Null ->
	fprintf f "VNull"
    | `Class v ->
	fprintf f "VClass e%d" v
    | `Proxy v ->
	fprintf f "VProxy e%d" v
    | `Constructor(n,v) ->
	fprintf f "%s e%d" n v
	  
let rec print_expr_term f expr =
  match expr with
    | `Match1(e1,p,e2) ->
	fprintf f "@[<v>( @[<v 2>match@[<hov 2>@ ";
	print_expr_term f e1;
	fprintf f "@ @]with@ @[<hov 2>| ";
	print_pat_term f p;
	fprintf f " ->@ ";
	print_expr_term f e2;
	fprintf f "@]@ @[<hov 2>| _ ->@ %s@]@]@ )@]" (lm_error())
    | `Match1_DM(e1,p,e2,xn) ->
	fprintf f "@[<v>( @[<v 2>match@[<hov 2>@ ";
	print_expr_term f e1;
	fprintf f "@ @]with@ @[<hov 2>| ";
	print_pat_term f p;
	fprintf f " ->@ ";
	print_expr_term f e2;
	fprintf f "@]@ ";
	fprintf f "@[<hov 2>| VDirectMapping(%s m) ->@ m@]@ " xn;
	fprintf f "@[<hov 2>| _ ->@ %s@]@]@ )@]" (lm_error())
    | `Match1_Null(e1,p,e2,e3) ->
	fprintf f "@[<v>( @[<v 2>match@[<hov 2>@ ";
	print_expr_term f e1;
	fprintf f "@]@ with@ @[<hov 2>| ";
	print_pat_term f p;
	fprintf f " ->@ ";
	print_expr_term f e2;
	fprintf f "@]@ @[<hov 2>| VNull ->@ ";
	print_expr_term f e3;
	fprintf f "@]@ @[<hov 2>| _ ->@ %s@]@]@ )@]" (lm_error())
    | `Match_variants(e1, cases) ->
	fprintf f "@[<v>( @[<v 2>match@[<hov 2>@ ";
	print_expr_term f e1;
	fprintf f "@]@ with";
	List.iter
	  (fun (tag,e) ->
	     fprintf f "@ @[<hov 2>| `%s ->@ " tag;
	     print_expr_term f e;
	     fprintf f "@]";
	  )
	  cases;
	fprintf f "@]@ )@]"
    | `Match_strings(e1, cases, other) ->
	fprintf f "@[<v>( @[<v 2>match@[<hov 2>@ ";
	print_expr_term f e1;
	fprintf f "@]@ with";
	List.iter
	  (fun (slit,e) ->
	     fprintf f "@ @[<hov 2>| \"%s\" ->@ " (String.escaped slit);
	     print_expr_term f e;
	     fprintf f "@]";
	  )
	  cases;
	fprintf f "@]@ @[<hov 2>| _ ->@ ";
	print_expr_term f other;
	fprintf f "@]@]@ )@]"
    | `Match_option(e1,v,e2,e3) ->
	fprintf f "@[<v>( @[<v 2>match@[<hov 2>@ ";
	print_expr_term f e1;
	fprintf f "@]@ with@ @[<hov 2>| Some e%d ->@ " v;
	print_expr_term f e2;
	fprintf f "@]@ @[<hov 2>| None ->@ ";
	print_expr_term f e3;
	fprintf f "@]@]@ )@]"
    | `Match_list(e1,v1,v2,e2) ->
	fprintf f "@[<v>( @[<v 2>match@[<hov 2>@ ";
	print_expr_term f e1;
	fprintf f "@]@ with@ @[<hov 2>| e%d :: e%d ->@ " v1 v2;
	print_expr_term f e2;
	fprintf f "@]@ @[<hov 2>| [] ->@ %s@]@]@ )@]" (lm_error())
    | `Match_slice(e1,s1,vl,e2) ->
	fprintf f "@[<v>( @[<v 2>match@[<hov 2>@ ";
	print_expr_term f e1;
	fprintf f "@]@ with@ @[<hov 2>| ";
	fprintf f "`Decoded(\"%s\", [| %s |] ) ->@ "
	  (String.escaped s1)
	  (String.concat "; " (List.map (fun v -> "e" ^ string_of_int v) vl));
	print_expr_term f e2;
	fprintf f "@]@ @[<hov 2>| _ ->@ %s@]@]@ )@]" (lm_error())
    | `Match_tuple(e1,vl,e2) ->
	fprintf f "@[<v>( @[<v 2>match@[<hov 2>@ ";
	print_expr_term f e1;
	fprintf f "@]@ with@ @[<hov 2>| ";
	fprintf f "( %s ) ->@ "
	  (String.concat ", " (List.map (fun v -> "e" ^ string_of_int v) vl));
	print_expr_term f e2;
	fprintf f "@]@ @]@ )@]"
    | `Call(e,args) ->
	fprintf f "@[<hv>@[<hv 2>( ";
	print_expr_term f e;
	List.iter
	  (fun e' ->
	     fprintf f "@ ";
	     print_expr_term f e'
	  )
	  args;
	fprintf f "@]@ )@]"
    | `CallF(s,args) ->
	print_expr_term f (`Call(`Var s, args))
    | `CallC(s,args) ->
	print_expr_term f (`Call(`Var s, args))
    | `CallM(e,name) ->
	fprintf f "@[<hv>@[<hv 2>( ";
	print_expr_term f e;
	fprintf f " # %s @]@ )@]" name
    | `Array_get(e1,e2) ->
	fprintf f "( @[<hov 2>";
	print_expr_term f e1;
	fprintf f ".( @[<hov 2>";
	print_expr_term f e2;
	fprintf f "@] )@] )"
    | `Array_lit elements ->
	fprintf f "@[<hov>@[<hov 3>[| ";
	Array.iter
	  (fun e ->
	     print_expr_term f e;
	     fprintf f ";@ ";
	  )
	  elements;
	fprintf f "@]@ |]@]";
    | `List_lit elements ->
	fprintf f "@[<hov>@[<hov 3>[ ";
	List.iter
	  (fun e ->
	     print_expr_term f e;
	     fprintf f ";@ ";
	  )
	  elements;
	fprintf f "@]@ ]@]";
    | `List_cons(e1,e2) ->
	fprintf f "( @[<hov 2>";
	print_expr_term f e1;
	fprintf f " :: @[<hov 2>";
	print_expr_term f e2;
	fprintf f "@] @] )"
    | `Record_get(e1,s) ->
	fprintf f "( @[<hov 2>";
	print_expr_term f e1;
	fprintf f ".%s@] )" s
    | `Record_lit elements ->
	fprintf f "@[<v>@[<v 2>{ ";
	Array.iter
	  (fun (n,e) ->
	     fprintf f "@[<hov 2>%s =@ " n;
	     print_expr_term f e;
	     fprintf f "@];@ ";
	  )
	  elements;
	fprintf f "@]@ }@]"
    | `Record_lit_ord elements ->
	let pvars = Array.map (fun _ -> new_pvar()) elements in
	fprintf f "@[<v>@[<v 2>( ";
	Array.iteri
	  (fun k p ->
	     let (_,e) = elements.(k) in
	     fprintf f "@[<hov 2>let p%d =@ " p;
	     print_expr_term f e;
	     fprintf f "@] in@ ";
	  )
	  pvars;
	fprintf f "@[<v 2>{ ";
	Array.iteri
	  (fun k p ->
	     let (n,_) = elements.(k) in
	     fprintf f "%s = p%d;" n p;
	     if k+1 < Array.length pvars then
	       fprintf f "@ "
	  )
	  pvars;
	fprintf f "@]@ }@]@ )@]"
    | `Int_lit n ->
	fprintf f "(%d)" n
    | `Int32_lit n ->
	fprintf f "(%ldl)" n
    | `Int64_lit n ->
	fprintf f "(%LdL)" n
    | `Float_lit n ->
	fprintf f "(Int64.float_of_bits (%LdL) (* %s *) )" (Int64.bits_of_float n) (string_of_float n)
    | `String_lit s ->
	fprintf f "\"%s\"" (String.escaped s)
    | `Bool_lit b ->
	fprintf f "%b" b
    | `Pair(e1,e2) ->
	print_expr_term f (`Tuple [e1;e2])
    | `Tuple el ->
	if el = [] then
	  fprintf f "()"
	else (
	  fprintf f "@[<hv>@[<hv 2>( ";
	  let first = ref true in
	  List.iter
	    (fun e ->
	       if not !first then fprintf f ",@ ";
	       print_expr_term f e;
	       first := false
	    )
	    el;
	  fprintf f "@]@ )@]";
	)
    | `Tuple_ord el ->
	if el = [] then
	  fprintf f "()"
	else (
	  let ela = Array.of_list el in
	  let pvars = Array.map (fun _ -> new_pvar()) ela in
	  fprintf f "@[<v>@[<v 2>( ";
	  Array.iteri
	    (fun k p ->
	       let e = ela.(k) in
	       fprintf f "@[<hov 2>let p%d =@ " p;
	       print_expr_term f e;
	       fprintf f "@] in@ ";
	    )
	    pvars;
	  fprintf f "@[<hv>@[<hv 2>( ";
	  let first = ref true in
	  Array.iter
	    (fun p ->
	       if not !first then fprintf f ",@ ";
	       fprintf f "p%d" p;
	       first := false
	    )
	    pvars;
	  fprintf f "@]@ )@]";
	  fprintf f "@]@ )@]";
	)
    | `Evar n ->
	fprintf f "e%d" n
    | `Var s ->
	fprintf f "%s" s
    | `Dtvar s ->
	fprintf f "(Lazy.force !%s)" s
    | `Variant s ->
	fprintf f "`%s" s
    | `Fun0 e ->
	fprintf f "@[<v 2>(fun () -> @ ";
	print_expr_term f e;
	fprintf f ")@]"
    | `Fun(v,e) ->
	print_expr_term f (`FunN([v],e))
    | `Fun2(v1,v2,e) ->
	print_expr_term f (`FunN([v1;v2],e))
    | `FunN(vl,e) ->
	fprintf f "@[<v>@[<v 2>(fun (%s) -> @ "
	  (String.concat "," (List.map (fun v -> "e" ^ string_of_int v) vl));
	print_expr_term f e;
	fprintf f "@]@ )@]"
    | `MFun(vl,e) ->
	assert(vl <> []);
	fprintf f "@[<v>@[<v 2>(fun %s -> @ "
	  (String.concat " " (List.map (fun v -> "e" ^ string_of_int v) vl));
	print_expr_term f e;
	fprintf f "@]@ )@]"
    | `Object(selfstr, inherits, methods, tystr) ->
	fprintf f "@[<v>( @[<v 2>object(%s)" selfstr;
	List.iter
	  (fun ce ->
	     fprintf f "@ @[<hv 2>inherit @ ";
	     print_class_term f ce;
	     fprintf f "@]"
	  )
	  inherits;
	List.iter
	  (fun (n,e) ->
	     fprintf f "@ @[<hv 2>method %s =@ " n;
	     print_expr_term f e;
	     fprintf f "@]"
	  )
	  methods;
	fprintf f "@]@   end : %s)@]" tystr
    | `Ifthenelse(e1,e2,e3) ->
	fprintf f "(if @[<hov 2>";
	print_expr_term f e1;
	fprintf f "@]@ then @[<hov 2>";
	print_expr_term f e2;
	fprintf f "@]@ else @[<hov 2>";
	print_expr_term f e3;
	fprintf f "@] )"
    | `Statements stms ->
	fprintf f "( @[<hov 2>";
	let is_first = ref true in
	Array.iter
	  (fun stm -> 
	     if not !is_first then (
	       fprintf f ";";
	       fprintf f "@ ";
	     );
	     fprintf f "@[<hov 2>";
	     print_expr_term f stm;
	     fprintf f "@]";
	     is_first := false;
	  )
	  stms;
	fprintf f "@] )"
    | `Typeann(e,ty_str) ->
	fprintf f "( @[<hov 2>";
	print_expr_term f e;
	fprintf f "@ : %s@] )" ty_str
    | `Coerce(s1,s2,s3) ->
	if s2 = "" then
	  fprintf f "@[<hv 2>(%s@ :> %s)@]" s1 s3
	else
	  fprintf f "@[<hv 2>(%s@ : #%s@ :> %s)@]" s1 s2 s3
    | `Coerce_expr(e,s2,s3) ->
	fprintf f "@[<hv 2>(";
	print_expr_term f e;
	if s2 = "" then
	  fprintf f "@ :> %s)@]" s3
	else
	  fprintf f "@ : #%s@ :> %s)@]" s2 s3
    | `Catch_not_found e ->
	fprintf f "@[<hv 2>(try ";
	print_expr_term f e;
	fprintf f "@ with Not_found -> %s)@]" (lm_error())
    | `Catch_inflated(e,xname) ->
	fprintf f "@[<v>( @[<v>@[<hov 2>try@ ";
	print_expr_term f e;
	fprintf f "@]@ with@ @[<hov 2>| %s x ->@ x" xname;
	fprintf f "@]@ @[<hov 2>| _ ->@ raise Hydro_lm.Invalid_coercion@]@]@ )@]"
    | `Catch_userexn(e,v1,e1,v2,e2) ->
	fprintf f "@[<v>( @[<v>@[<hov 2>try@ ";
	print_expr_term f e;
	fprintf f "@]@ with@ @[<hov 2>| User_exception e%d ->@ " v1;
	print_expr_term f e1;
	fprintf f "@]@ @[<hov 2>| e%d -> @ " v2;
	print_expr_term f e2;
	fprintf f "@]@] )@]"
    | `Raise s ->
	fprintf f "(raise %s)" s
    | `Raise_expr e ->
	fprintf f "( @[<hov 2>raise ";
	print_expr_term f e;
	fprintf f "@] )"
    | `Fail ->
	fprintf f "(%s)" (lm_error())
    | `Noreturn e ->
	fprintf f "@[<hv>( @[<hv>@[<hv 2>let _ =@ ";
	print_expr_term f e;
	fprintf f " in@]@ assert false@]@ )@]"
	  

and print_class_term f cexpr =
  match cexpr with
    | `Var s ->
	fprintf f "%s" s
    | `Fun(v,ce) ->
	fprintf f "@[<v>@[<v 2>(fun (%s) -> @ "
	  ("e" ^ string_of_int v);
	print_class_term f ce;
	fprintf f "@]@ )@]"
    | `FunN(vl,e) ->
	fprintf f "@[<v>@[<v 2>(fun (%s) -> @ "
	  (String.concat "," (List.map (fun v -> "e" ^ string_of_int v) vl));
	print_class_term f e;
	fprintf f "@]@ )@]"
    | `Call(ce, el) ->
	fprintf f "@[<hv>@[<hv 2>( ";
	print_class_term f ce;
	List.iter
	  (fun e' ->
	     fprintf f "@ ";
	     print_expr_term f e'
	  )
	  el;
	fprintf f "@]@ )@]"
    | `Object(_,_,_,_) as obj ->
	print_expr_term f obj
    | `DataObject(inherits, methods, tystr) ->
	fprintf f "@[<v>( @[<v 2>object";
	List.iter
	  (fun ce ->
	     fprintf f "@ @[<hv 2>inherit @ ";
	     print_class_term f ce;
	     fprintf f "@]"
	  )
	  inherits;
	List.iter
	  (fun (n,e) ->
	     fprintf f "@ @[<hv 2>val %s =@ " n;
	     print_expr_term f (`CallF("ref", [e]));
	     fprintf f "@]"
	  )
	  methods;
	List.iter
	  (fun (n,e) ->
	     fprintf f "@ @[<hv 2>method %s = %s@]" n n;
	  )
	  methods;
	fprintf f "@]@   end : %s)@]" tystr



let print_types in_sig f types =
  let first = ref true in
  List.iter
    (fun (n, ty) ->
       if !first then 
	 fprintf f "@[<hov 2>type %s" n
       else
	 fprintf f "@[<hov 2>and %s" n;

       ( match ty with
	   | `Opaque ty when in_sig ->
	       ()
	   | _ ->
	       fprintf f " =@\n";
	       print_type_term f ty;
       );

       fprintf f "@]@\n";

       first := false
    )
    types


let print_exns impl_flag f exns =
  List.iter
    (fun (n, ty, eq_opt) ->
       fprintf f "@[<hov 2>exception %s" n;
       ( match eq_opt with
	   | Some eq when impl_flag -> 
	       fprintf f "@ = %s" eq
	   | _ ->       
	       fprintf f " of@ ";
	       print_type_term f ty;
       );
       fprintf f "@]@\n";
    )
    exns


let print_ctypes f ctypes =
  let first = ref true in
  List.iter
    (fun (n,ct) ->
       fprintf f "@[<hov 2>class type %s" n;
       fprintf f " =@\n";
       print_ctype f ct;
       fprintf f "@]@\n";
       first := false
    )
    ctypes



let print_class_sigs f ctypes =
  let first = ref true in
  List.iter
    (fun (n,ct) ->
       fprintf f "@[<hov 2>class %s" n;
       fprintf f " :@\n";
       print_ctype f ct;
       fprintf f "@]@\n";
       first := false
    )
    ctypes


let print_classes f classes =
  let first = ref true in
  List.iter
    (fun (n,ce) ->
       fprintf f "@[<hov 2>class %s" n;
       fprintf f " =@\n";
       print_class_term f ce;
       fprintf f "@]@\n";
       first := false
    )
    classes


let print_letrec_sigs f types =
  List.iter
    (fun (n, ty) ->
       fprintf f "@[<hov 2>val %s :@ " n;
       print_type_term f ty;
       fprintf f "@]@\n";
    )
    types

let print_letrecs f letrecs =
  let first = ref true in
  List.iter
    (fun (n, e) ->
       if !first then 
	 fprintf f "@[<hov 2>let rec %s =@\n" n
       else
	 fprintf f "@[<hov 2>and %s =@\n" n;

       print_expr_term f e;

       fprintf f "@]@\n";

       first := false
    )
    letrecs

let print_lets f lets =
  let first = ref true in
  List.iter
    (fun (n, e) ->
       if !first then 
	 fprintf f "@[<hov 2>let %s =@\n" n
       else
	 fprintf f "@[<hov 2>and %s =@\n" n;

       print_expr_term f e;

       fprintf f "@]@\n";

       first := false
    )
    lets

let print_defterms f defterms =
  List.iter
    (fun (n, _, _, e) ->
       fprintf f "@[<hov 2>let %s =@\n" n;
       fprintf f "ref(lazy(assert false))@]@\n";
    )
    defterms;
  List.iter
    (fun (n, _, _, e) ->
       fprintf f "@[<hov 2>let () =@\n";
       fprintf f "@[<hov 2>%s :=@ " n;
       fprintf f "@[<hov 2>( lazy ";
       print_expr_term f e;
       fprintf f ")@]@]@]@\n"
    )
    defterms;
  fprintf f "@[<v 2>let fill_system sys =@\n";
  fprintf f "Hydro_lm_IceObject.fill_system sys;@\n";
  fprintf f "Hydro_lm_IceLocalObject.fill_system sys;@\n";
  List.iter
    (fun (n, colon_name, dt, _) ->
       if dt <> `Omit then (
	 fprintf f "@[<v 2>let def =@ ";
	 fprintf f "(Lazy.force !%s) in@ @]" n;
	 fprintf f "@[<hov 2>Hydro_prelim.CiHashtbl.replace@ %s@ \"%s\" def;@]@ "
	   (match dt with
	      | `Type  -> "sys#types"
	      | `Exn   -> "sys#exceptions"
	      | `Intf  -> "sys#interfaces"
	      | `Class -> "sys#classes"
	      | `Ctor  -> "sys#ctors"
	      | `Omit  -> assert false
	   )
	   colon_name
       )
    )
    defterms;
  fprintf f "()@]@\n"


let print_split_letrecs f letrecs =
  let s = Hgen_simplif.split_letrecs letrecs in
  List.iter
    (fun defs ->
       print_letrecs f defs
    )
    s


let print_pad_impl f pad =
  fprintf f "open Hydro_types@\n";
  print_types false f (List.rev pad.types);
  print_ctypes f (List.rev pad.ctypes);
  print_exns true f (List.rev pad.exns);
  print_lets f (List.rev pad.lets);
  print_split_letrecs f (List.rev pad.letrecs);
  fprintf f "let catch_exn f r = try f r#result with Hydro_types.Client_condition(`User_exception sv) -> raise(User_exception(decode_exception sv))@\n";
  print_split_letrecs f (List.rev pad.letrecs2);
  print_classes f (List.rev pad.classes);
  print_split_letrecs f (List.rev pad.letrecs3);
  print_defterms f (List.rev pad.defterms)
  
let print_pad_sig f pad =
  fprintf f "open Hydro_types@\n";
  print_types true f (List.rev pad.types);
  print_ctypes f (List.rev pad.ctypes);
  print_exns false f (List.rev pad.exns);
  print_letrec_sigs f (List.rev pad.letrec_sigs);
  print_class_sigs f (List.rev pad.class_sigs);
  fprintf f "val fill_system : Hydro_types.system -> unit@\n"

