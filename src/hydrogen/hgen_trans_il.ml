open Hgen_types
open Hgen_types.IL

(* Transform to intermediate language *)

(* Prefixes:

   t_<name>            type
   to_<name>           marshalling value -> t_<name>
   of_<name>           marshalling t_<name> -> value

   T_<name>            exception of t_<name> (for DirectMapping)
   ddt_<name>          direct decoder buffer -> cursor -> endpos -> t_<name>
   ddT_<name>          direct decoder buffer -> cursor -> endpos -> exn (T_<name>)
   det_<name>          direct encoder buffer -> t_<name> -> unit
   deT_<name>          direct encoder buffer -> exn (T_<name>) -> unit

   pr_<name>           [`Names|...] proxy_reference
   po_<name>           proxy object (class type)
   pc_<name>           proxy class (of type po_<name>)
   topr_<name>         marshalling value -> pr_<name> option
   ofpr_<name>         marshalling pr_<name> option -> value
   unchecked_pr_<name> unchecked cast proxy_reference -> pr_<name>

   or_<name>           object reference (masked object_base)
   od_<name>           object data
   oi_<name>           object interface (operations)
   o_<name>            object (subtype of object_base)
   O_<name>            exception for inflating
   as_<name>           cast to other object type
   dec_<name>          slice decoder for objects
   enc_<name>          slice encoder for objects
   mk_od_<name>        class: create od_<name> container
   delegate_od_<name>  class: create od_<name> class from od_<name> object
   delegate_oi_<name>  class: create oi_<name> class from oi_<name> object
   sliced_od_<name>    class: create sliced_value from od_<name>
   skel_<name>         class: skeleton for oi_<name>, all ops fail
   mk_<name>           class: create o_<name> from od_<name>
   restore_<name>      class: create o_<name> from sliced_value
   toor_<name>         marshalling value -> or_<name> option
   ofor_<name>         marshalling or_<name> option -> value
   wrap_<name>         masking o_<name> -> or_<name>
   unwrap_<name>       unmasking or_<name> -> o_<name>
   dispatch_<name>     invoke operations by name and value array
   declexns_<name>__<method>  List of declared exceptions

   r_<name>__<method>   Mapped response object (class type, for client)
   rr_<name>__<method>   Mapped response object (class type, for server)
                       rr = response record. r_<foo> is subtype of rr_<foo>
   to_r_<name>__<method>  marshalling response -> r_<name>_<method>

   x_<name>            Object constructor for exception <name>
   dx_<name>           Exception decoder for exception <name>
   ex_<name>           Exception encoder for exception <name>
   decode_exception    General decoder sliced_value -> user_exception
   encode_exception    General encoder user_exception -> sliced_value

 *)

let enable_directmapping = ref false

let evar_cnt = ref 0

let new_evar() =
  let e = !evar_cnt in
  incr evar_cnt;
  e


let rec translate_tt symboltable (tt : TS.ty) =
  match tt with
    | `Void -> `Unit
    | `Bool -> `Bool
    | `Byte -> `Int
    | `Short -> `Int
    | `Int -> `Int
    | `Int32 -> `Int32
    | `Long -> `Int64
    | `Float -> `Float
    | `Double -> `Float
    | `String -> `String
    | `Byteseq -> `String
    | `Enum tags -> `Variant tags
    | `Struct(stru,eq_opt) ->
	`Record 
	  (Array.map
	     (fun (_, mapped_n, tt', is_mutable) -> 
		(mapped_n, translate_tt symboltable tt', is_mutable))
	     stru,
	   eq_opt
	  )
    | `Struct_tuple stru ->
	`Tuple
	  (Array.to_list
	     (Array.map (fun (_, tt') -> translate_tt symboltable tt') stru))
    | `Sequence tt' -> `Array (translate_tt symboltable tt')
    | `Dictionary (tt1,tt2) -> 
	`Alist(translate_tt symboltable tt1, translate_tt symboltable  tt2)
    | `Proxy name ->
	let n = Hgen_util.TS_util.colon_name name in
	( try
	    let ent = CiHashtbl.find symboltable n in
	    `Option (`Named ("pr_" ^ Hgen_util.TS_util.mapped_name ent))
	  with Not_found -> assert false
	)
    | `Object name ->
	let n = Hgen_util.TS_util.colon_name name in
	( try
	    let ent = CiHashtbl.find symboltable n in
	    `Option (`Named ("or_" ^ Hgen_util.TS_util.mapped_name ent))
	  with Not_found -> assert false
	)
    | `Named ht' ->
	let n = Hgen_util.TS_util.colon_name ht'#name in
	( try
	    let ent = CiHashtbl.find symboltable n in
	    `Named ("t_" ^ Hgen_util.TS_util.mapped_name ent)
	  with Not_found -> assert false
	)
    | `User_mapping(`Sequence t1,oname,_,_) ->
	let t1' = translate_tt symboltable t1 in
	`Subst([t1'], oname)
    | `User_mapping(`Dictionary(t1,t2),oname,_,_) ->
	let t1' = translate_tt symboltable t1 in
	let t2' = translate_tt symboltable t2 in
	`Subst([t1';t2'], oname)
    | `User_mapping((`Enum _|`Void|`Bool|`Byte|`Short|`Int|`Int32|
			 `Long|`Float|`Double|`String|`Byteseq|
			     `Proxy _|`Object _|`Named _) as t1, oname,_,_) ->
	let t1' = translate_tt symboltable t1 in
	`Subst([t1'], oname)
    | `User_mapping(`Struct _,oname,_,_) ->
	assert false (* This isn't supported right now *)
    | `User_mapping(`Struct_tuple stru,oname,_,_) ->
	let tl =
	  List.map
	    (fun (_,tt') ->  translate_tt symboltable tt')
	    (Array.to_list stru) in
	`Subst(tl,oname)
    | `User_mapping(`User_mapping _,oname,_,_) ->
	assert false


let e_array_to_list e =
  `CallF("Array.to_list", [e])

let e_map_array_pairs (v1, v2, e1, e2) =
  `CallF("Array.map", 
	 [ `Fun2(v1,v2,e1);
	   e2
	 ])

let match1_maybe_dm ht (e1,p,e2,xn) =
  if !enable_directmapping && !(ht#directmapping) then
    `Match1_DM(e1,p,e2,xn)
  else
    `Match1(e1,p,e2)
  

let um_id (expr : expr_term) = expr

let rec generate_marshalling_to 
            symboltable 
            (ht_opt : TS.hnamed option) 
	    (um_trans : expr_term -> expr_term)
            (tt : TS.ty)  
         : (expr_term * type_term) =
  let get_ht() = 
    match ht_opt with
      | None -> assert false
      | Some ht -> ht in
  match tt with
    | `Void ->
	let v1 = new_evar() in
	( um_trans (`Fun(v1, `Var "()")),
	  `Fun(`Value, `Unit)
	)
    | `Bool -> 
	let v1 = new_evar() in
	let v2 = new_evar() in
	( (`Fun(v1, `Match1(`Evar v1, `Bool v2, um_trans(`Evar v2)))),
	  `Fun(`Value, `Bool)
	)
    | `Byte -> 
	let v1 = new_evar() in
	let v2 = new_evar() in
	( (`Fun(v1, `Match1(`Evar v1, `Byte v2, um_trans(`Evar v2)))),
	  `Fun(`Value, `Int)
	)
    | `Short ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	( (`Fun(v1, `Match1(`Evar v1, `Short v2, um_trans(`Evar v2)))),
	  `Fun(`Value, `Int)
	)
    | `Int ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	( (`Fun(v1, `Match1(`Evar v1, `Int v2, um_trans(`Evar v2)))),
	  `Fun(`Value, `Int)
	)
    | `Int32 ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	( (`Fun(v1, `Match1(`Evar v1, `Int32 v2, um_trans(`Evar v2)))),
	  `Fun(`Value, `Int32)
	)
    | `Long ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	( (`Fun(v1, `Match1(`Evar v1, `Long v2, um_trans(`Evar v2)))),
	  `Fun(`Value, `Int64)
	)
    | `Float ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	( (`Fun(v1, `Match1(`Evar v1, `Float v2, um_trans(`Evar v2)))),
	  `Fun(`Value, `Float)
	)
    | `Double ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	( (`Fun(v1, `Match1(`Evar v1, `Double v2, um_trans(`Evar v2)))),
	  `Fun(`Value, `Float)
	)
    | `String ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	( (`Fun(v1, `Match1(`Evar v1, `String v2, um_trans(`Evar v2)))),
	  `Fun(`Value, `String)
	)
    | `Byteseq ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	( (`Fun(v1, `Match1(`Evar v1, `Byteseq v2, um_trans(`Evar v2)))),
	  `Fun(`Value, `String)
	)
    | `Enum tags ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1, `Match1(`Evar v1,
			     `Enum v2,
			     um_trans
			       (`Array_get(`Var ("e_" ^ ht#mapped_name), 
					`Evar v2)))) : expr_term),
	  `Fun(`Value, `Named("t_" ^ ht#mapped_name))
	)
    | `Struct(stru,eq_opt) ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1, 
		 match1_maybe_dm
		   ht
		   (`Evar v1,
		    `Struct v2,
		    um_trans
		      (`Record_lit
			 (Array.mapi
			    (fun k (_,mn,tt',_) ->
			       (mn, 
				`Call(fst(generate_marshalling_to_ref
					    symboltable tt'),
				      [ `Array_get(`Evar v2,
						   `Int_lit k) ]
				     )
			       )
			    )
			    stru)),
		    "T_" ^ ht#mapped_name
		   )) : expr_term),
	  `Fun(`Value, `Named("t_" ^ ht#mapped_name))
	)
    | `Struct_tuple stru ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1, 
		 match1_maybe_dm
		   ht
		   (`Evar v1,
		    `Struct v2,
		    um_trans
		      (`Tuple
			 (Array.to_list
			    (Array.mapi
			       (fun k (_,tt') ->
				  (`Call(fst(generate_marshalling_to_ref
					       symboltable tt'),
					 [ `Array_get(`Evar v2,
						      `Int_lit k) ]
					)
				  )
			       )
			       stru))),
		     "T_" ^ ht#mapped_name
		   )) : expr_term),
	  `Fun(`Value, `Named("t_" ^ ht#mapped_name))
	)
    | `Sequence tt' -> 
	let v1 = new_evar() in
	let v2 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1, 
		 match1_maybe_dm
		   ht
		   (`Evar v1,
		    `Sequence v2,
		    um_trans
		      (`CallF("Array.map",
			      [fst(generate_marshalling_to_ref
				     symboltable tt');
			       `Evar v2
			      ])),
		    "T_" ^ ht#mapped_name
		   )) : expr_term),
	  `Fun(`Value, `Named("t_" ^ ht#mapped_name))
	)
    | `Dictionary (tt1,tt2) -> 
	let v1 = new_evar() in
	let v2 = new_evar() in
	let v3 = new_evar() in
	let v4 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1,
		 match1_maybe_dm
		   ht
		   (`Evar v1, 
		    `Dictionary v2,
		    um_trans
		      (e_array_to_list
			 (e_map_array_pairs
			    (v3, 
			     v4, 
			     `Pair(`Call(fst(generate_marshalling_to_ref
					       symboltable tt1),
					 [`Evar v3]),
				   `Call(fst(generate_marshalling_to_ref
					       symboltable tt2),
					 [`Evar v4])),
			     `Evar v2))),
		    "T_" ^ ht#mapped_name
		   )) : expr_term),
	  `Fun(`Value, `Named("t_" ^ ht#mapped_name))
	)
    | `Proxy name ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1,
		 `Match1_Null(`Evar v1,
			      `Proxy v2,
			      um_trans
				(`Call(`Var "Some",
				       [`Typeann(
					  `Call(
					    `Var "Hydro_lm.Unsafe.wrap_proxy",
					    [`Evar v2]
					  ),
					  "pr_" ^
					    Hgen_util.TS_util.colon_name name)
				       ])),
			      `Var "None")) : expr_term),
	  `Fun(`Value, `Option(`Named("pr_" ^ ht#mapped_name)))
	)
    | `Object name ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1,
		 `Match1_Null(`Evar v1,
			      `Class v2,
			      um_trans
				(`Call(`Var "Some",
				       [ `Call(
					   `Var "Hydro_lm.object_of_class_repr",
					   [`Evar v2]
					 )
				       ])),
			      `Var "None")) : expr_term),
	  `Fun(`Value, `Option(`Named("or_" ^ ht#mapped_name)))
	)
	
    | `Named ht' ->
	let sign = 
	  snd(generate_marshalling_to symboltable 
		(Some (ht' :> TS.hnamed)) um_id ht'#term) in
	( `Var ("to_" ^ ht'#mapped_name),  (* CHECK: Ok to ignore um_trans *)
	  sign
	)

    | `User_mapping(t1,_,f_to,_) ->
	let um_trans expr =
	  `CallF(f_to, [expr]) in
	let (e,t) =
	  generate_marshalling_to symboltable ht_opt um_trans t1 in
	( e,
	  `Fun(`Value, translate_tt symboltable tt)
	)


and generate_marshalling_to_ref symboltable (tt : TS.ty)  
         : (expr_term * type_term) =
  (* This is for referring to already defined types *)
  match tt with
    | `Void
    | `Bool
    | `Byte
    | `Short
    | `Int
    | `Int32
    | `Long
    | `Float
    | `Double
    | `String
    | `Byteseq 
    | `Named _ ->
	(* ht is not needed for these: *)
	generate_marshalling_to symboltable None um_id tt

    | `Enum _
    | `Struct(_,_)
    | `Struct_tuple _
    | `Sequence _
    | `Dictionary _ 
    | `User_mapping _ ->
	(* these type terms are not allowed as reference *)
	assert false

    | `Proxy name ->
	(* Look up intf: *)
	let n = Hgen_util.TS_util.colon_name name in
	( try
	    let ent = CiHashtbl.find symboltable n in
	    let mn = Hgen_util.TS_util.mapped_name ent in
	    ( `Var("topr_" ^ mn),  `Option(`Named ("pr_" ^ mn )))
	  with Not_found -> assert false
	)
    | `Object name ->
	(* Look up class: *)
	let n = Hgen_util.TS_util.colon_name name in
	( try
	    let ent = CiHashtbl.find symboltable n in
	    let mn = Hgen_util.TS_util.mapped_name ent in
	    ( `Var("toor_" ^ mn),  `Option(`Named ("or_" ^ mn )))
	  with Not_found -> assert false
	)


let rec generate_marshalling_of
           symboltable (ht_opt : TS.hnamed option) (tt : TS.ty)  
         : (expr_term * type_term) =
  let get_ht() = 
    match ht_opt with
      | None -> assert false
      | Some ht -> ht in
  match tt with
    | `Void ->
	let v1 = new_evar() in
	( ( `Fun(v1, `Var "VNothing") : expr_term),
	  `Fun(`Unit, `Value)
	)
    | `Bool -> 
	let v1 = new_evar() in
	( ( `Fun(v1, `CallC("VBool", [`Evar v1])) : expr_term),
	  `Fun(`Bool, `Value)
	)
    | `Byte -> 
	let v1 = new_evar() in
	( ( `Fun(v1, `CallC("VByte", [`Evar v1])) : expr_term),
	  `Fun(`Int, `Value)
	)
    | `Short ->
	let v1 = new_evar() in
	( ( `Fun(v1, `CallC("VShort", [`Evar v1])) : expr_term),
	  `Fun(`Int, `Value)
	)
    | `Int ->
	let v1 = new_evar() in
	( ( `Fun(v1, `CallC("VInt", [`Evar v1])) : expr_term),
	  `Fun(`Int, `Value)
	)
    | `Int32 ->
	let v1 = new_evar() in
	( ( `Fun(v1, `CallC("VInt32", [`Evar v1])) : expr_term),
	  `Fun(`Int32, `Value)
	)
    | `Long ->
	let v1 = new_evar() in
	( ( `Fun(v1, `CallC("VLong", [`Evar v1])) : expr_term),
	  `Fun(`Int64, `Value)
	)
    | `Float ->
	let v1 = new_evar() in
	( ( `Fun(v1, `CallC("VFloat", [`Evar v1])) : expr_term),
	  `Fun(`Float, `Value)
	)
    | `Double ->
	let v1 = new_evar() in
	( ( `Fun(v1, `CallC("VDouble", [`Evar v1])) : expr_term),
	  `Fun(`Float, `Value)
	)
    | `String ->
	let v1 = new_evar() in
	( ( `Fun(v1, `CallC("VString", [`Evar v1])) : expr_term),
	  `Fun(`String, `Value)
	)
    | `Byteseq ->
	let v1 = new_evar() in
	( ( `Fun(v1, `CallC("VByteseq", [`Evar v1])) : expr_term),
	  `Fun(`String, `Value)
	)
    | `Enum tags ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1, 
		 `Call(`Fun(v2, `CallC("VEnum", [`Evar v2])),
		       [ `Match_variants(`Evar v1,
					 Array.to_list
					   (Array.mapi
					      (fun k tag ->
						 (tag, `Int_lit k)
					      )
					      tags))
		       ]
		      )
		) : expr_term ),
	  `Fun(`Named("t_" ^ ht#mapped_name), `Value)
	)
    | `Struct(stru,_) ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1, 
		 `Call(`Fun(v2, `CallC("VStruct", [`Evar v2])),
		       [ `Array_lit
			   ( Array.mapi
			       (fun k (_,mn,tt',_) ->
				  `Call(fst(generate_marshalling_of_ref symboltable tt'),
					[ `Record_get(`Evar v1, mn) ])
			       )
			       stru
			   )
		       ]
		      )
		) : expr_term ),
	  `Fun(`Named("t_" ^ ht#mapped_name), `Value)
	)
    | `Struct_tuple stru ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	let ht = get_ht() in
	let fields = Array.map (fun (_,tt') -> (new_evar()), tt') stru in
	( ( `Fun(v1, 
		 `Call(`Fun(v2, `CallC("VStruct", [`Evar v2])),
		       [ `Match_tuple(`Evar v1,
				      (List.map fst (Array.to_list fields)),
				      `Array_lit
					(Array.map
					   (fun (v,tt') ->
					      `Call(fst(generate_marshalling_of_ref symboltable tt'),
						    [ `Evar v ])
					   )
					   fields)
				     )
		       ])
		) : expr_term ),
	  `Fun(`Named("t_" ^ ht#mapped_name), `Value)
	)
    | `Sequence tt' -> 
	let v1 = new_evar() in
	let v2 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1, 
		 `Call(`Fun(v2, `CallC("VSequence", [`Evar v2])),
		       [ `CallF("Array.map",
				[ fst(generate_marshalling_of_ref symboltable tt');
				  `Evar v1
				])
		       ]
		      )
		) : expr_term),
	  `Fun(`Named("t_" ^ ht#mapped_name), `Value)
	)
    | `Dictionary (tt1,tt2) -> 
	let v1 = new_evar() in
	let v2 = new_evar() in
	let v3 = new_evar() in
	let v4 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1, 
		 `Call(`Fun(v2, `CallC("VDictionary", [`Evar v2])),
		       [ `CallF("Array.map",
				[ `Fun2(v3, v4,
					`Pair(
					  `Call(fst(generate_marshalling_of_ref symboltable tt1),
						[ `Evar v3 ]),
					  `Call(fst(generate_marshalling_of_ref symboltable tt2),
						[ `Evar v4 ])
					)
				       );
				  (`CallF("Array.of_list",[`Evar v1]))
				])
		       ]
		      )
		) : expr_term),
	  `Fun(`Named("t_" ^ ht#mapped_name), `Value)
	)
    | `Proxy name ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	let v3 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1,
		 `Match_option(`Evar v1,
			       v2,
			       (* Some v2: *)
			       `Call(
				 `Fun(v3, 
				      `CallC("VProxy", [`Evar v3])),
				 [ `CallF("Hydro_lm.Unsafe.unwrap_proxy",
					  [ `Evar v2 ])
				 ]),
			       (* None: *)
			       `Var "VNull"
			      )
		) : expr_term),
	  `Fun(`Option(`Named("pr_" ^ ht#mapped_name)), `Value)
	)
    | `Object name ->
	let v1 = new_evar() in
	let v2 = new_evar() in
	let ht = get_ht() in
	( ( `Fun(v1,
		 `Match_option(`Evar v1,
			       v2,
			       (* Some v2: *)
			       `CallF("Hydro_lm.value_of_object",
				      [ `Evar v2 ]
				     ),
			       (* None: *)
			       `Var "VNull"
			      )
		) : expr_term),
	  `Fun(`Option(`Named("or_" ^ ht#mapped_name)), `Value)
	)
    | `Named ht' ->
	let sign = snd(generate_marshalling_of symboltable (Some (ht' :> TS.hnamed)) ht'#term) in
	( `Var ("of_" ^ ht'#mapped_name),
	  sign
	)
    | `User_mapping(t1,_,_,f_of) ->
	let v1 = new_evar() in
	let t1_of, _ = 
	  generate_marshalling_of symboltable ht_opt t1 in
	( `Fun(v1,
	       `Call(t1_of, [ `CallF(f_of, [ `Evar v1 ]) ] ) ),
	  `Fun(translate_tt symboltable tt, `Value)
	)

and generate_marshalling_of_ref symboltable (tt : TS.ty)  
         : (expr_term * type_term) =
  (* This is for referring to already defined types *)
  match tt with
    | `Void
    | `Bool
    | `Byte
    | `Short
    | `Int
    | `Int32
    | `Long
    | `Float
    | `Double
    | `String
    | `Byteseq 
    | `Named _ ->
	(* ht is not needed for these: *)
	generate_marshalling_of symboltable None tt

    | `Enum _
    | `Struct(_,_)
    | `Struct_tuple _
    | `Sequence _
    | `Dictionary _ 
    | `User_mapping _ ->
	(* these type terms are not allowed as reference *)
	assert false

    | `Proxy name ->
	(* Look up intf: *)
	let n = Hgen_util.TS_util.colon_name name in
	( try
	    let ent = CiHashtbl.find symboltable n in
	    let mn = Hgen_util.TS_util.mapped_name ent in
	    ( `Var("ofpr_" ^ mn),  `Option(`Named ("pr_" ^ mn )))
	  with Not_found -> assert false
	)
    | `Object name ->
	(* Look up class: *)
	let n = Hgen_util.TS_util.colon_name name in
	( try
	    let ent = CiHashtbl.find symboltable n in
	    let mn = Hgen_util.TS_util.mapped_name ent in
	    ( `Var("ofor_" ^ mn),  `Option(`Named ("or_" ^ mn )))
	  with Not_found -> assert false
	)


let rec ddt_of_type mn_opt (tt : TS.ty) : expr_term =
  let v1 = new_evar() in
  let v2 = new_evar() in
  let v3 = new_evar() in
  let v123 = [`Evar v1; `Evar v2; `Evar v3] in
  let expr =
    match tt with
      | `Void ->
	  `Tuple []
      | `Bool ->
	  `CallF("Hydro_unmarshal.unmarshal_bool", v123)
      | `Byte ->
	  `CallF("Hydro_unmarshal.unmarshal_byte", v123)
      | `Short ->
	  `CallF("Hydro_unmarshal.unmarshal_short", v123)
      | `Int ->
	  `CallF("Hydro_unmarshal.unmarshal_int", v123)
      | `Int32 ->
	  `CallF("Hydro_unmarshal.unmarshal_int32", v123)
      | `Long ->
	  `CallF("Hydro_unmarshal.unmarshal_int64", v123)
      | `Float ->
	  `CallF("Hydro_unmarshal.unmarshal_float", v123)
      | `Double ->
	  `CallF("Hydro_unmarshal.unmarshal_double", v123)
      | `String ->
	  `CallF("Hydro_unmarshal.unmarshal_string", v123)
      | `Byteseq ->
	  `CallF("Hydro_unmarshal.unmarshal_string", v123)
      | `Named ht1 ->
	  `CallF("ddt_" ^ ht1#mapped_name, v123)
      | `Sequence tt1 ->
	  `CallF("Hydro_unmarshal.unmarshal_sequence",
		 (ddt_of_type None tt1 :: v123)
		)
      | `Dictionary(tt1,tt2) ->
	  `CallF("Hydro_unmarshal.unmarshal_dictionary",
		 [ ddt_of_type None tt1; ddt_of_type None tt2 ] @ v123
		)
      | `Enum e ->
	  let mn =
	    match mn_opt with
	      | None -> assert false
	      | Some mn -> mn in
	  `Array_get(`Var ("e_" ^ mn),
		     `CallF("Hydro_unmarshal.unmarshal_enum",
			    [ `Int_lit (Array.length e) ] @ v123))
      | `Struct(stru,_) ->
	  `Record_lit_ord
	    (Array.map
	       (fun (_,mn,tt',_) ->
		  ( mn, `Call(ddt_of_type None tt', v123) )
	       )
	       stru)
      | `Struct_tuple stru ->
	  `Tuple_ord
	    (Array.to_list
	       (Array.map
		  (fun (_,tt') ->`Call(ddt_of_type None tt', v123))
		  stru))
      | `User_mapping(t1,_,f_to,_) ->
	  `CallF(f_to,
		 [ `Call(ddt_of_type None t1, v123) ])
      | `Proxy _ 
      | `Object _ ->
	  assert false
  in
  `MFun( [v1;v2;v3], expr )


let ddT_of_name mn : expr_term =
  let v1 = new_evar() in
  let v2 = new_evar() in
  let v3 = new_evar() in
  let v123 = [`Evar v1; `Evar v2; `Evar v3] in
  let expr = `CallC("T_" ^ mn,
		    [ `CallF("ddt_" ^ mn, v123) ]) in
  `MFun( [v1;v2;v3], expr )


let rec det_of_type mn_opt (tt : TS.ty) : expr_term =
  let v1 = new_evar() in
  let v2 = new_evar() in
  let v12 = [`Evar v1; `Evar v2] in
  let expr =
    match tt with
      | `Void ->
	  `Tuple []
      | `Bool ->
	  `CallF("Hydro_marshal.print_bool", v12)
      | `Byte ->
	  `CallF("Hydro_marshal.print_byte", v12)
      | `Short ->
	  `CallF("Hydro_marshal.print_short", v12)
      | `Int ->
	  `CallF("Hydro_marshal.print_int", v12)
      | `Int32 ->
	  `CallF("Hydro_marshal.print_int32", v12)
      | `Long ->
	  `CallF("Hydro_marshal.print_int64", v12)
      | `Float ->
	  `CallF("Hydro_marshal.print_float", v12)
      | `Double ->
	  `CallF("Hydro_marshal.print_double", v12)
      | `String ->
	  `CallF("Hydro_marshal.print_string", v12)
      | `Byteseq ->
	  `CallF("Hydro_marshal.print_string", v12)
      | `Named ht1 ->
	  `CallF("det_" ^ ht1#mapped_name, v12)
      | `Sequence tt1 ->
	  `CallF("Hydro_marshal.marshal_sequence",
		 (det_of_type None tt1 :: v12)
		)
	    (* N.B. It is always a sequence of a named type (i.e. tt1=`Named).
               So it is ok to pass down None as mn_opt
	     *)
      | `Dictionary(tt1,tt2) ->
	  `CallF("Hydro_marshal.marshal_dictionary",
		 [ det_of_type None tt1; det_of_type None tt2 ] @ v12
		)
	    (* see remark for `Sequence *)
      | `Enum tags ->
	  `CallF("Hydro_marshal.marshal_enum",
		 [ `Int_lit(Array.length tags);
		   `Evar v1;
		   `Match_variants
		     (`Evar v2,
		      Array.to_list
			(Array.mapi
			   (fun k tag ->
			      (tag, `Int_lit k)
			   )
			   tags
			))
		 ])
      | `Struct(stru,_) ->
	  `Statements
	    (Array.map
	       (fun (_,mn,tt',_) ->
		  `Call(det_of_type None tt',
			[ `Evar v1;
			  `Record_get(`Evar v2, mn)
			]
		       )
	       )
	       stru)
      | `Struct_tuple stru ->
	  let tvars =
	    Array.map (fun _ -> new_evar()) stru in
	  `Match_tuple
	    (`Evar v2,
	     Array.to_list tvars,
	     `Statements
	       (Array.mapi
		  (fun j (_,tt') ->
		     `Call(det_of_type None tt', [`Evar v1; `Evar tvars.(j)])
		  )
		  stru)
	    )
      | `User_mapping(t1,_,_,f_from) ->
	  `Call(det_of_type None t1, [`Evar v1; `CallF(f_from, [`Evar v2])])
      | `Proxy _ 
      | `Object _ ->
	  assert false
  in
  `MFun( [v1;v2], expr )

let deT_of_name mn : expr_term =
  let v1 = new_evar() in
  let v2 = new_evar() in
  let v3 = new_evar() in
  let v13 = [`Evar v1; `Evar v3] in
  `MFun( [v1;v2],
	 `Match1(`Evar v2,
		 `Constructor("T_" ^ mn, v3),
		 `CallF("det_" ^ mn, v13)
		)
       )

let rec dt_of_type 
        (symboltable : TS.entity CiHashtbl.t) (tt : TS.ty) : expr_term =
  (* Return TS.ty as expression that generates the corresponding 
     Hydro_types.htype
   *)
  match tt with
    | `Void    -> `Var "TVoid"
    | `Bool    -> `Var "TBool"
    | `Byte    -> `Var "TByte"
    | `Short   -> `Var "TShort"
    | `Int     -> `Var "TInt"
    | `Int32   -> `Var "TInt32"
    | `Long    -> `Var "TLong"
    | `Float   -> `Var "TFloat"
    | `Double  -> `Var "TDouble"
    | `String  -> `Var "TString"
    | `Byteseq -> `Var "TByteseq"
    | `Named ht -> `Dtvar("dt_" ^ ht#mapped_name)
    | `Enum e   -> `CallC("TEnum",
			 [`Array_lit (Array.map (fun s -> `String_lit s) e)])
    | `Struct(s,_) -> `CallC("TStruct",
			  [`Array_lit 
			     (Array.map
				(fun (on, mn, tt', _) ->
				   `Pair(`String_lit on,
					 dt_of_type symboltable tt')
				)
			       s)])
    | `Struct_tuple s -> `CallC("TStruct",
			  [`Array_lit 
			     (Array.map
				(fun (on, tt') ->
				   `Pair(`String_lit on,
					 dt_of_type symboltable tt')
				)
			       s)])
    | `Sequence tt' -> `CallC("TSequence",
			      [dt_of_type symboltable tt'])
    | `Dictionary(tt1,tt2) -> `CallC("TDictionary",
				     [ `Pair
					 ( dt_of_type symboltable tt1,
					   dt_of_type symboltable tt2 ) ])
    | `Proxy name -> `CallC("TProxy",
			    [`String_lit (Hgen_util.TS_util.colon_name name)])
    | `Object name -> `CallC("TClass",
			     [`String_lit (Hgen_util.TS_util.colon_name name)])
    | `User_mapping(tt',_,_,_) -> dt_of_type symboltable tt'


let rec tt_qualifies_for_dm (tt : TS.ty) =
  (* This says where we put `DirectMapping into the generated type system.
     These are the entry points for the alternate dm decoders/encoders.
     There must be corresponding ddt/ddT functions.
   *)
  match tt with
    | `Struct _ | `Struct_tuple _ | `Sequence _ | `Dictionary _ ->
	true
    | `User_mapping(tt',_,_,_) ->
	tt_qualifies_for_dm tt'
    | _ ->
	false
  

let type_to_il (symboltable : TS.entity CiHashtbl.t) (pad : pad) ht =
  (* Generate the O'Caml type definition: *)
  let iltt = translate_tt symboltable ht#term in
  pad.types <- ("t_" ^ ht#mapped_name, iltt) :: pad.types;

  if not ht#local then (
    (* Generate the marshalling code: *)
    let (m_to_fun, m_to_sig) = 
      generate_marshalling_to 
	symboltable (Some (ht :> TS.hnamed)) um_id ht#term in
    let (m_of_fun, m_of_sig) = 
      generate_marshalling_of
	symboltable (Some (ht :> TS.hnamed)) ht#term in
    pad.letrecs     <- ( "to_" ^ ht#mapped_name, m_to_fun) :: pad.letrecs;
    pad.letrec_sigs <- ( "to_" ^ ht#mapped_name, m_to_sig) :: pad.letrec_sigs;
    pad.letrecs     <- ( "of_" ^ ht#mapped_name, m_of_fun) :: pad.letrecs;
    pad.letrec_sigs <- ( "of_" ^ ht#mapped_name, m_of_sig) :: pad.letrec_sigs;

    (* Helpers for certain types: *)
    ( match ht#term with
	| `Enum tags ->
	    let enum_array =
	      `Array_lit (Array.map (fun tag -> `Variant tag) tags) in
	    pad.letrecs <- ( "e_" ^ ht#mapped_name, enum_array) :: pad.letrecs;
	| _ -> ()
    );

    if !enable_directmapping && !(ht#directmapping) then (
      pad.exns <-
	("T_" ^ ht#mapped_name, (`Named ("t_" ^ ht#mapped_name)) ,None) ::
	pad.exns;

      pad.letrecs <- ( "ddt_" ^ ht#mapped_name,
		       ddt_of_type (Some ht#mapped_name) ht#term ) ::
	pad.letrecs;
      
      pad.letrecs <- ( "ddT_" ^ ht#mapped_name, 
		       ddT_of_name ht#mapped_name ) ::
	pad.letrecs;

      pad.letrecs <- ( "det_" ^ ht#mapped_name,
		       det_of_type (Some ht#mapped_name) ht#term ) ::
	pad.letrecs;

      pad.letrecs <- ( "deT_" ^ ht#mapped_name, 
		       deT_of_name ht#mapped_name ) ::
	pad.letrecs;

      (* Definition term: *)
      let dt_expr0 = dt_of_type symboltable ht#term in
      let dt_expr1 =
	(* Don't use DirectMapping for enums here. *)
	if tt_qualifies_for_dm ht#term then
	  `CallC("TDirectMapping",
		 [ `Tuple [ dt_expr0; 
			    `Var("deT_" ^ ht#mapped_name);
			    `Var("ddT_" ^ ht#mapped_name) 
			  ] ])
	else
	  dt_expr0 in
      pad.defterms <- ("dt_" ^ ht#mapped_name,
		       (Hgen_util.TS_util.colon_name ht#name),
		       `Type,
		       dt_expr1) :: pad.defterms;
    )
    else (
      (* Definition term: *)
      pad.defterms <- ("dt_" ^ ht#mapped_name,
		       (Hgen_util.TS_util.colon_name ht#name),
		       `Type,
		       dt_of_type symboltable ht#term) :: pad.defterms;
    )

  )
;;


let rec all_exn_elements he =
  Array.to_list he#data_elements ::
    ( match he#super with
	| None -> []
	| Some he' -> all_exn_elements he'
    )


let rec exn_hierarchy he =
  he :: (match he#super with None -> [] | Some he' -> exn_hierarchy he')


let rec is_sub_exn he1 he2 =
  (* Whether he1 can be coerced to he2 *)
  he1 = he2 ||
  ( match he1 # super with
      | None -> false
      | Some he1' -> is_sub_exn he1' he2
  )


let exns_to_il symboltable pad local_flag exns =
  let prefix = 
    if local_flag then "local_" else "" in
  pad.types <- (prefix ^ "exception_name",
		if exns = [] then
		  `Opaque `Unit
		else
		  `Variant (Array.of_list (List.map (fun he -> he#mapped_name) exns))
	       ) :: pad.types;
  pad.types <- (prefix ^ "exception_ops",
		`Object ( [],
			  [ "exn_name", `Named (prefix ^ "exception_name");
			    "exn_id", `String;
			  ] @
			    (List.flatten
			       (List.map
				  (fun he ->
				     [ "is_" ^ he#mapped_name, `Bool;
				       "as_" ^ he#mapped_name, 
				         (`Named ("t_" ^ he#mapped_name))
				     ]
				  )
				  exns))
			)
	       ) :: pad.types;
  
  let exn_types =
    List.map
      (fun he ->
	 ("t_" ^ he#mapped_name,
	  `Object ( [],
		    ( "hydro_ops", `Named (prefix ^ "exception_ops") ) ::
		      (List.map
			 (fun (_,mn,ty) ->
			    (mn, translate_tt symboltable ty)
			 )
			 (List.flatten (all_exn_elements he) )
		      )
		  )
	 )
      )
      exns in
  let user_exn_type =
    ( (prefix ^ "user_exception"),
      `Object ( [], [ "hydro_ops", `Named (prefix ^ "exception_ops") ] )
    ) in
  pad.types <- user_exn_type :: exn_types @ pad.types;

  (* Generate x_* functions that create the exception objects: *)
  let x_funs =
    List.map
      (fun he ->
	 let slices = all_exn_elements he in
	 let slices_with_evars =
	   List.map 
	     (fun slice -> 
		List.map (fun (n,mn,ty) -> (n, mn, new_evar(), ty)) slice
	     )
	     slices in
	 let flat_slices = List.flatten slices_with_evars in
	 let ops_obj =
	   `Object
	     ( "ops_self",
	       [], 
	       [ "exn_name", `Variant he#mapped_name;
		 "exn_id", `String_lit (Hgen_util.TS_util.colon_name he#name);
	       ] @
		 (List.flatten
		    (List.map
		       (fun he' ->
			  if is_sub_exn he he' then
			    [ "is_" ^ he'#mapped_name, 
			        `Var "true";
			      "as_" ^ he'#mapped_name, 
			        `Coerce("self",
					(* "t_" ^ he#mapped_name, *) "",
					"t_" ^ he'#mapped_name)
			    ]
			  else
			    [ "is_" ^ he'#mapped_name, 
			        `Var "false";
			      "as_" ^ he'#mapped_name, 
			        `Raise "Hydro_lm.Invalid_coercion"
			    ]
		       )
		       exns
		    )
		 ),
	       (prefix ^ "exception_ops")
	     ) in

	 let x_obj =
	   `Object
	     ( "self",
	       [],
	       ("hydro_ops", ops_obj) ::
		 (List.map
		    (fun (n, mn, v, _) -> (mn, `Evar v))
		    flat_slices),
	       "t_" ^ he#mapped_name
	     ) in
	 let rec mk_fun_abstr swe =
	   match swe with
	     | [] -> x_obj
	     | slice :: swe' ->
		 `FunN(List.map(fun (_,_,v,_) -> v) slice,
		       mk_fun_abstr swe')
	 in
	 let f = mk_fun_abstr slices_with_evars in
	 let letrec =
	   ("x_" ^ he#mapped_name, f) in

	 let rec mk_fun_type sl =
	   match sl with
	     | [] -> `Named("t_" ^ he#mapped_name)
	     | slice :: sl' ->
		 `Fun(`Tuple(List.map
			       (fun (_,_,tt) -> translate_tt symboltable tt)
			       slice),
		      mk_fun_type sl')
	 in
	 let ty = mk_fun_type slices in
	 let letrec_sig = 
	   ("x_" ^ he#mapped_name, ty) in

	 (letrec,letrec_sig)
      )
      exns in
  pad.letrecs <- (List.map fst x_funs) @ pad.letrecs;
  pad.letrec_sigs <- (List.map snd x_funs) @ pad.letrec_sigs;

  (* Generate dx_* functions to decode exceptions: *)
  (* (This is skipped for local exceptions!) *)
  if not local_flag then (
    let dx_funs =
      List.map
	(fun he ->
	   let v1 = new_evar() in
	   let v2 = new_evar() in
	   let v3 = new_evar() in
	   
	   let elements =
	     List.map
	       (fun (n, _, ty) -> (n, new_evar(), ty))
	       (Array.to_list he#data_elements) in
	   
	   let id = Hgen_util.TS_util.colon_name he#name in
	   
	   let f =
	     `Fun(v1,
		  `Match_list(`Evar v1,
			      v2,   (* head *)
			      v3,   (* tail *)
			      (* non-empty case: *)
			      `Match_slice(`Evar v2,
					   id,
					   (List.map (fun (_,v,_)->v) elements),
					   `Pair
					     (`Tuple
						(List.map
						   (fun (_,v,ty) ->
						      `Call(fst(generate_marshalling_to symboltable None um_id ty),
							    [`Evar v])
						   )
						   elements
						),
					      `Evar v3)
					  )
			     )
		 ) in
	   
	   ("dx_" ^ he#mapped_name, f)
	)
	exns in
    pad.letrecs <- dx_funs @ pad.letrecs;

    (* Generate the decode_exception function: *)
    (* (Also skipped for local exceptions) *)
    let decode =
      let v1 = new_evar() in
      `Fun(v1,
	   `Match_strings(`CallM(`Evar v1, "hydro_effective_id"),
			  (List.map
			     (fun he ->
				let id = Hgen_util.TS_util.colon_name he#name in
				let hier = exn_hierarchy he in
				let hier1 =
				  List.map
				    (fun he' -> (he', new_evar(), new_evar()))
				    hier in
				let expr1 =
				  `Call(`Var("x_" ^ he#mapped_name),
					(List.map 
					   (fun (_,v,_) -> `Evar v)
					   hier1)
				       ) in
				let rec mk_calls h arg =
				  match h with
				    | [] -> expr1
				    | (he',v1,v2) :: h' ->
					`Call(`Fun2(v1,
						    v2,
						    mk_calls h' (`Evar v2)),
					      [ `Call
						  (`Var ("dx_" ^ he'#mapped_name),
						   [ arg ])
					      ]
					     )
				in
				let s1 = `CallM(`Evar v1, "hydro_slices") in
				let expr = mk_calls (List.rev hier1) s1 in
				let expr = `Coerce_expr(expr, "", "user_exception") in
				(id, expr)
			     )
			     exns
			  ),
			  `Fail
			 )
	  )
    in
    let decode_sig =
      `Fun(`Named "Hydro_types.sliced_value",
	   `Named "user_exception") in

    pad.letrecs <- ("decode_exception", decode) :: pad.letrecs;
    pad.letrec_sigs <- ("decode_exception", decode_sig) :: pad.letrec_sigs;

    (* Generate ex_* functions to encode exceptions: *)
    (* (Also skipped for local exceptions) *)
    List.iter
      (fun he ->
	 let ex =
	   let id = Hgen_util.TS_util.colon_name he#name in
	   let e_uexn = new_evar() in
	   `Fun(e_uexn,
		`List_cons(
		  (* our slice: *)
		  `Call(`Variant "Decoded",
			[ `Pair(
			    `String_lit id,
			    `Array_lit(
			      Array.map
				(fun (n,mn,ty) ->
				   let f,_ = 
				     generate_marshalling_of_ref
				       symboltable ty in
				   `Call(f,
					 [ `CallM(`Evar e_uexn,
						  mn)
					 ])
				)
				he#data_elements
			    )
			  )
			]
		       ),
		  (* base slices: *)
		  ( match he#super with
		      | None ->
			  `List_lit []
		      | Some he_super ->
			  `CallF("ex_" ^ he_super#mapped_name,
				 [ `Coerce_expr(`Evar e_uexn,
						"",
						"t_" ^ he_super#mapped_name)
				 ])
		  )
		)
	       ) in
	 pad.letrecs <- ("ex_" ^ he#mapped_name, ex) :: pad.letrecs;
      )
      exns;

    (* Generate encode_exception: *)
    (* (Also skipped for local exceptions) *)

    let encode_t =
      `Fun(`Named "user_exception", `Named "Hydro_types.sliced_value") in

    let encode =
      let e_uexn = new_evar() in
      let e_typeid = new_evar() in
      let e_slices = new_evar() in
      let e_rslices = new_evar() in
      `Fun(e_uexn,
	   `Call(`Fun2(e_typeid,
		       e_slices,
		       `Call(`Fun(e_rslices,
				  `Object("self",
					  [],
					  [ "hydro_slices", 
					    `Evar e_rslices;

					    "hydro_effective_id", 
					    `Evar e_typeid;
					  ],
					  "Hydro_types.sliced_value")
				 ),
			     [ `CallF("List.rev",
				      [ `Evar e_slices ])
			     ])),
		 [ if exns = [] then
		     `Fail
		   else
		     `Match_variants(
		       `CallM(`Typeann(`Evar e_uexn, 
				       "user_exception"),
			      "hydro_ops#exn_name"),
		       ( List.map
			   (fun he ->
			      let id = Hgen_util.TS_util.colon_name he#name in
			      let enc_call =
				`CallF("ex_" ^ he#mapped_name,
				       [ `CallM(`Evar e_uexn,
						("hydro_ops#as_" ^ he#mapped_name))
				       ]) in
			      (he#mapped_name, 
			       `Pair(`String_lit id,
				     enc_call)
			      )
			   )
			   exns
		       )
		     )
		 ]
		)
	  )
    in
    pad.letrecs <- ("encode_exception", encode) :: pad.letrecs;
    pad.letrec_sigs <- ("encode_exception", encode_t) :: pad.letrec_sigs;
  );
    
  (* Add User_exception and Local_user_exception: *)
  if local_flag then
    pad.exns <- ("Local_user_exception",
		 `Named "local_user_exception", None) :: pad.exns
  else
    pad.exns <- ("User_exception",
		 `Named "user_exception", None) :: pad.exns;

  (* Generate the definition term: *)
  (* (Only done when not local_flag so it is only generated once) *)
  if not local_flag then (
    pad.defterms <- 
      List.map
        (fun (he:TS.hexn) ->
	   let id = Hgen_util.TS_util.colon_name he#name in
	   ("dt_" ^ he#mapped_name,
	    id,
	    `Exn,
	    `Object("self",
		    [],
		    [ "name", `String_lit id;
		      "super", ( match he#super with
				   | None -> `Var "None"
				   | Some he_s ->
				       `CallC("Some",
					      [ `Dtvar("dt_" ^ he_s#mapped_name)
					      ])
			       );
		      "elements", `Array_lit(Array.map
					       (fun (n,_,ty) ->
						  `Pair(`String_lit n,
							dt_of_type
							  symboltable ty
						       )
					       )
					       he#data_elements)
			
		    ],
		    "Hydro_types.hexn"
		   )
	   )
	)
        exns @ pad.defterms;
  )
;;


let proxy_fun_type_of_method symboltable ho hf : type_term =
  let rec mk_fun_type i =
    if i < Array.length hf#in_args then
      let (n,_,ty) = hf#in_args.(i) in
      `Fun(translate_tt symboltable ty,
	   mk_fun_type (i+1)
	  )
    else
      `Named_arg1(`Named("r_" ^ ho#mapped_name ^ "__" ^ hf#mapped_name),
		  "Hydro_lm.call_suspension_t")
  in
  if hf#in_args = [| |] then   (* Invent dummy Unit type *)
    `Fun(`Unit, mk_fun_type 0)
  else
    mk_fun_type 0
;;


let proxy_impl_of_method symboltable ho hf e_proxy e_intf : expr_term =
  let x_in_args =
    Array.map
      (fun (n,_,ty) -> (n,ty,new_evar()))
      hf#in_args in
  let rec mk_impl i =
    if i < Array.length x_in_args then
      let (n,ty,e) = x_in_args.(i) in
      `Fun(e, mk_impl (i+1))
    else
      `Call(`Var "Hydro_lm.call_suspension",
	    [ `Call(`CallM(`Evar e_proxy,
			   "hydro_twoway_call"),
		    [ `Evar e_intf;
		      `String_lit hf#name;
		      `Array_lit
			(Array.map
			   (fun (n,tt,e) ->
			      let (f, _) = 
				generate_marshalling_of_ref 
				  symboltable tt in
			      `Call(f, [ `Evar e ])
			   )
			   x_in_args
			)
		    ]);
	      `Var ("to_r_" ^ ho#mapped_name ^ "__" ^ hf#mapped_name);
	      `CallM(`Evar e_proxy,
		     "hydro_env#event_system")
	    ]
	   )
  in
  if hf#in_args = [| |] then   (* Invent dummy Unit type *)
    `FunN([], mk_impl 0)
  else
    mk_impl 0
;;


let class_fun_type_of_method symboltable ho hf : type_term =
  let rec mk_fun_type i =
    if i < Array.length hf#in_args then
      let (n,_,ty) = hf#in_args.(i) in
      `Fun(translate_tt symboltable ty,
	   mk_fun_type (i+1)
	  )
    else
      let emit_fun = 
	`Fun(`Named("rr_" ^ ho#mapped_name ^ "__" ^ hf#mapped_name),
	     `Unit) in
      let emit_exn_fun = 
	`Fun(`Named "user_exception", `Unit) in
      `Fun(emit_fun,
	   `Fun(emit_exn_fun,
		`Fun(`Named "Hydro_types.session",
		     `Unit)))
  in
  if hf#in_args = [| |] then   (* Invent dummy Unit type *)
    `Fun(`Unit, mk_fun_type 0)
  else
    mk_fun_type 0
;;


let class_skel_of_method hf : expr_term =
  let e_emit = new_evar() in
  let e_emit_exn = new_evar() in
  let e_session = new_evar() in
  let x_in_args =
    Array.map
      (fun (n,_,ty) -> (n,ty,new_evar()))
      hf#in_args in
  let rec mk_impl i =
    if i < Array.length x_in_args then
      let (n,ty,e) = x_in_args.(i) in
      `Fun(e, mk_impl (i+1))
    else
      `Fun(e_emit,
	   `Fun(e_emit_exn,
		`Fun(e_session,
		     `Raise_expr(
		       `CallC("Hydro_types.Unimplemented_operation",
			      [ `String_lit hf#name ])))))
  in
  if hf#in_args = [| |] then   (* Invent dummy Unit type *)
    `FunN([], mk_impl 0)
  else
    mk_impl 0
;;


let bool_term b =
  if b then `Var "true" else `Var "false"
;;


let dt_of_hf symboltable hf =
  `Object("iself",
	  [],
	  [ "name", `String_lit hf#name;
	    "mode", (match hf#mode with
		       | `Normal -> `Var "`Normal"
		       | `Idempotent -> `Var "`Idempotent"
		       | `Nonmutating -> `Var "`Nonmutating"
		    );
	    "in_args", (`Array_lit
			  (Array.map
			     (fun (n,_,ty) ->
				`Pair(`String_lit n,
				      dt_of_type symboltable ty)
			     )
			     hf#in_args));
	    "in_classes", bool_term hf#in_classes;
	    "out_args", (`Array_lit
			   (Array.map
			      (fun (n,_,ty) ->
				 `Pair(`String_lit n,
				       dt_of_type symboltable ty)
			      )
			      hf#out_args));
	    "result", dt_of_type symboltable hf#result;
	    "out_classes", bool_term hf#out_classes
	  ],
	  "Hydro_types.hfunction"
	 )
;;


let intf_to_il1 symboltable pad implements ho =
  (* Generate a proxy definition for [ho]. The base interfaces are already
     done.
   *)
  let id = Hgen_util.TS_util.colon_name ho#name in

  let d_super_ho_list =
    ( match ho#super with
	| None -> []
	| Some hos -> [hos]
    ) @ ho#super_intf in
    (* Direct super interfaces *)

  let super_ho_list = CiMap.find id implements in
    (* Included indirectly inherited interfaces. Also includes ho itself! *)

  (* "pr_<proxy>" *)
  let pr_variants =
    Array.of_list
      (List.map (fun super_ho -> super_ho#mapped_name) super_ho_list) in
  pad.types <- 
    ("pr_" ^ ho#mapped_name,
     `Named_arg1(`Variant pr_variants, "Hydro_lm.proxy_reference")) :: 
    pad.types;

  (* "ofpr_<proxy>" and "topr_<proxy>" *)
  pad.letrecs <- ("ofpr_" ^ ho#mapped_name,
		  `Var "Hydro_lm.Unsafe.of_proxy_reference") :: pad.letrecs;
  pad.letrecs <- ("topr_" ^ ho#mapped_name,
		  `Var "Hydro_lm.Unsafe.to_proxy_reference") :: pad.letrecs;
  pad.letrec_sigs <- ("ofpr_" ^ ho#mapped_name,
		      `Fun(`Option(`Named("pr_" ^ ho#mapped_name)),
			   `Value)
		     ) :: pad.letrec_sigs;
  pad.letrec_sigs <- ("topr_" ^ ho#mapped_name,
		      `Fun(`Value,
			   `Option(`Named("pr_" ^ ho#mapped_name)))
		     ) :: pad.letrec_sigs;

  (* "r_<proxy>_<method>" *)
  List.iter
    (fun hf ->
       let r_name = "r_" ^ ho#mapped_name ^ "__" ^ hf#mapped_name in
       pad.ctypes <- (r_name,
		      `Object([],
			      [ "hydro_response", 
				    `Named "Hydro_lm.client_response";
				"result",
				     (translate_tt symboltable hf#result);
			      ] @
				Array.to_list
				  (Array.map
				     (fun (name,mn,ty) ->
					("out_" ^ mn,
					 translate_tt symboltable ty
					)
				     )
				     hf#out_args)
			     )
		     ) :: pad.ctypes;
       let e_result = new_evar() in
       let to_result, _ = generate_marshalling_to_ref symboltable hf#result in
       pad.letrecs2 <- ("to_" ^ r_name,
			`Fun(e_result,
			     `Object("self",
				     [], 
				     ( [ "hydro_response", 
					   `Typeann(`Evar e_result,
						    "Hydro_lm.client_response"
						   );
					 "result", 
					   `CallF("catch_exn",
						  [ to_result;
						    `Evar e_result
						  ])
				       ] @
					 Array.to_list
					 (Array.mapi
					    (fun i (name,mn,ty) ->
					       let to_out, _ =
						 generate_marshalling_to_ref
						   symboltable ty in
					       ("out_" ^ mn,
						`Call
						  (to_out,
						   [`Array_get
						      (`CallM(`Evar e_result,
							      "out_args"),
						       `Int_lit i)
						   ])
					       )
					    )
					    hf#out_args
					 )
				     ),
				     r_name
				    )
			    )
		       ) :: pad.letrecs2;
       pad.letrec_sigs <- ("to_" ^ r_name,
			   `Fun(`Named "Hydro_lm.client_response",
				`Named r_name)
			  ) :: pad.letrec_sigs
    )
    ho#op_elements;

  (* "po_<proxy>", "poi_<proxy>" *)
  pad.ctypes <- ("poi_" ^ ho#mapped_name,
		 `Object( List.map
			    (fun super_ho ->
			       `Named ("poi_" ^ super_ho#mapped_name))
			    d_super_ho_list,
			  List.map
			    (fun hf ->
			       (hf#mapped_name,
				proxy_fun_type_of_method symboltable ho hf))
			    ho#op_elements
			)
		) :: pad.ctypes;
  pad.ctypes <- ("po_" ^ ho#mapped_name,
		  `Object( [ `Named "Hydro_proxy.proxy_t";
			     `Named("poi_" ^ ho#mapped_name)
			   ],
			   [ "hydro_proxy_reference",
			     `Named ("pr_" ^ ho#mapped_name)
			   ])
		 ) :: pad.ctypes;

  (* "pc_<proxy>", "pci_<proxy>" *)
  let e_proxy =  new_evar() in
  let e_intf = new_evar() in
  let pci_def =
    `Fun(e_proxy,
	 `Call(`Fun(e_intf,
		    `Object("self",
			    List.map
			      (fun super_ho ->
				 `Call (`Var ("pci_" ^ super_ho#mapped_name),
					[ `Evar e_proxy ]
				       )
			      )
			      d_super_ho_list,
			    List.map
			      (fun hf ->
				 (hf#mapped_name,
				  proxy_impl_of_method
				    symboltable ho hf e_proxy e_intf))
			      ho#op_elements,
			    ("poi_" ^ ho#mapped_name)
			   )
		   ),
	       [ `Catch_not_found(`CallF("Hydro_prelim.CiHashtbl.find",
					 [ `CallM (`Evar e_proxy,
						   "hydro_env#system#interfaces");
					   `String_lit id
					 ]))
	       ]
	      ) ) in
  let e_proxy_env = new_evar() in
  let e_proxy_ref = new_evar() in
  let e_proxy2 =  new_evar() in
  let pc_def =
    `Fun(e_proxy_env,
	 `Fun(e_proxy_ref,
	      `Call(`Fun(e_proxy2,
			 `Object("self",
				 [ `Call(`Var ("pci_" ^ ho#mapped_name),
					 [ `Evar e_proxy2 ]);
				   `Call(`Var "Hydro_proxy.proxy_delegation",
					 [ `Evar e_proxy2 ]) ],
				 [ "hydro_proxy_reference",
				   `Evar e_proxy_ref
				 ],
				 ("po_" ^ ho#mapped_name)
				)
			),
		    [ `CallF("Hydro_proxy.proxy",
			     [ `Evar e_proxy_env;
			       `Coerce_expr(`CallF("Hydro_lm.Unsafe.unwrap_proxy",
						   [ `Evar e_proxy_ref ]
						  ),
					    "",
					    "Hydro_proxy.extended_proxy_addr");
			       `Tuple[]
			     ])
		    ]
		   )
	     )
	) in
(*
  pad.class_sigs <- ("pc_" ^ ho#mapped_name,
		     `Fun(`Named "Hydro_proxy.proxy_env_t",
			  `Fun(`Named ("pr_" ^ ho#mapped_name),
			       `Named ("po_" ^ ho#mapped_name)))) :: 
    pad.class_sigs;
  pad.class_sigs <- ("pci_" ^ ho#mapped_name,
		     `Fun(`Named "Hydro_proxy.proxy_t",
			  `Named ("poi_" ^ ho#mapped_name))) :: 
    pad.class_sigs;
 *)
  pad.classes <- ("pci_" ^ ho#mapped_name, pci_def) :: pad.classes;
  pad.classes <- ("pc_" ^ ho#mapped_name, pc_def) :: pad.classes;
  pad.letrecs3 <- ("pc_" ^ ho#mapped_name,
		   `CallC("new",
			  [ `Var ("pc_" ^ ho#mapped_name) ])) :: pad.letrecs3;
  pad.letrec_sigs <- ("pc_" ^ ho#mapped_name,
		      `Fun(`Named "Hydro_proxy.proxy_env_t",
			  `Fun(`Named ("pr_" ^ ho#mapped_name),
			       `Named ("po_" ^ ho#mapped_name)))) ::
    pad.letrec_sigs;

  (* "unchecked_pr_<proxy>" *)
  let e_pr = new_evar() in
  pad.letrecs3 <- ("unchecked_pr_" ^ ho#mapped_name,
		   `Fun(e_pr,
			`CallF("Hydro_lm.Unsafe.wrap_proxy",
			       [ `CallF("Hydro_lm.Unsafe.unwrap_proxy",
					[ `Evar e_pr ])
			       ]) 
		       )) :: pad.letrecs3;
  pad.letrec_sigs <- ("unchecked_pr_" ^ ho#mapped_name,
		      `Fun(`Named "'t Hydro_lm.proxy_reference",
			   `Named ("pr_" ^ ho#mapped_name))) :: pad.letrec_sigs;

  (* TODO: "checked_pr_<proxy>" *)

  (* "dt_<proxy>" *)
  pad.defterms <- ("dt_" ^ho#mapped_name,
		   id,
		   `Intf,
		   (`Object("self",
			    [],
			    ["name", `String_lit id;
			     "super", 
			     `List_lit
			       (List.map
				  (fun ho' ->
				     `Dtvar("dt_" ^ ho'#mapped_name)
				  )
				  ho#super_intf);
			     "elements",
			     `List_lit
			       (List.map
				  (fun hf ->
				     dt_of_hf symboltable hf
				  )
				  ho#op_elements)
			    ],
			    "Hydro_types.hintf"
			   )
		   )
		  ) :: pad.defterms;
;;


let intf_to_il symboltable pad implements ho =
  match ho#imported_from with
    | None -> 
	intf_to_il1 symboltable pad implements ho
    | Some modname ->
	let id = Hgen_util.TS_util.colon_name ho#name in

	(* "pr_<proxy>" *)
	pad.types <- ("pr_" ^ ho#mapped_name,
		      `Named(modname ^ ".pr_" ^ ho#mapped_name)) :: pad.types;
	(* "ofpr_<proxy>" and "topr_<proxy>" *)
	pad.letrecs <- ("ofpr_" ^ ho#mapped_name,
			`Var (modname ^ ".ofpr_" ^ ho#mapped_name)) :: pad.letrecs;
	pad.letrecs <- ("topr_" ^ ho#mapped_name,
			`Var (modname ^ ".topr_" ^ ho#mapped_name)) :: pad.letrecs;
	pad.letrec_sigs <- ("ofpr_" ^ ho#mapped_name,
			    `Fun(`Option(`Named("pr_" ^ ho#mapped_name)),
				 `Value)) :: pad.letrec_sigs;
	pad.letrec_sigs <- ("topr_" ^ ho#mapped_name,
			    `Fun(`Value,
				 `Option(`Named("pr_" ^ ho#mapped_name)))
			   ) :: pad.letrec_sigs;
	(* no "r_<proxy>_<method>"! *)
	(* "po_<proxy>", "poi_<proxy>" *)
	pad.ctypes <- ("poi_" ^ ho#mapped_name,
		       `Named(modname ^ ".poi_" ^ ho#mapped_name)):: pad.ctypes;
	pad.ctypes <- ("po_" ^ ho#mapped_name,
		       `Named(modname ^ ".po_" ^ ho#mapped_name)):: pad.ctypes;

	(* "pc_<proxy>", "pci_<proxy>" *)
	pad.classes <- ("pci_" ^ ho#mapped_name,
			`Var(modname ^ ".pci_" ^ ho#mapped_name)):: pad.classes;
	(*
	pad.class_sigs <- ("pci_" ^ ho#mapped_name,
			   `Fun(`Named "Hydro_proxy.proxy_t",
				`Named(modname ^ ".pci_" ^ ho#mapped_name))):: pad.class_sigs;
	 *)
	
	pad.letrecs3 <- ("pc_" ^ ho#mapped_name,
			 `Var(modname ^ ".pc_" ^ ho#mapped_name)) :: 
	  pad.letrecs3;
	pad.letrec_sigs <- ("pc_" ^ ho#mapped_name,
			    `Fun(`Named "Hydro_proxy.proxy_env_t",
				 `Fun(`Named ("pr_" ^ ho#mapped_name),
				      `Named ("po_" ^ ho#mapped_name)))) ::
	  pad.letrec_sigs;

	(* "unchecked_pr_<proxy>" *)
	pad.letrecs3 <- ("unchecked_pr_" ^ ho#mapped_name,
			 `Var(modname ^ ".unchecked_pr_" ^ ho#mapped_name)) ::
	  pad.letrecs3;
	pad.letrec_sigs <- ("unchecked_pr_" ^ ho#mapped_name,
			    `Fun(`Named "'t Hydro_lm.proxy_reference",
				 `Named ("pr_" ^ ho#mapped_name))) :: pad.letrec_sigs;

	(* "dt_<proxy>" *)
	pad.defterms <- ("dt_" ^ho#mapped_name,
			 id,
			 `Omit,
			 `CallF("Lazy.force",
				[ `Var(modname ^ ".defterm_" ^ ho#mapped_name)
				])
			) :: pad.defterms;
;;


let rec decoded_cls_type symboltable ho : type_term =
  (* The argument type of mk_od_<name> *)
  let ho_elems =
    Array.to_list
      (Array.map
	 (fun (_,_,tt) ->
	    translate_tt symboltable tt
	 )
	 ho#data_elements
      ) in
  match ho#super with
    | None ->
	`Tuple ho_elems
    | Some ho_super ->
	`Tuple(ho_elems @ [ decoded_cls_type symboltable ho_super ])
;;


let rec get_super_classes ho =  (* including ho *)
  match ho#super with
    | None -> [ho]
    | Some ho_super -> ho :: get_super_classes ho_super
;;


let cls_to_il1 symboltable pad implements ho =
  let id = Hgen_util.TS_util.colon_name ho#name in

  let get_ho_super() =
    (* This should work for all classes, because only Ice::(Local)Object
       does not have a super class, and this is not handled here
     *)
    match ho#super with
      | None -> 
	  assert false
      | Some ho_super -> ho_super in

  let d_super_ho_list =
    (* Direct super interfaces *)
    ( match ho#super with
	| None -> []
	| Some hos -> [hos]
    ) @ ho#super_intf in

  let super_ho_list = CiMap.find id implements in
    (* Included indirectly inherited interfaces *)

  (* "or_<name>": This is only reasonable for non-local real classes *)
  if ho#objtype = `Class && not ho#local then (
    pad.types <- 
      ("or_" ^ ho#mapped_name,
       `Opaque(`Named "Hydro_lm.object_base")) :: pad.types;

    (* "ofor_<name>" and "toor_<name>" *)
    let (f_of, _) = 
      generate_marshalling_of
	symboltable (Some (ho :> TS.hnamed)) (`Object ho#name) in
    let (f_to, _) = 
      generate_marshalling_to
	symboltable (Some (ho :> TS.hnamed)) um_id (`Object ho#name) in
    pad.letrecs <- ("ofor_" ^ ho#mapped_name,
		    f_of) :: pad.letrecs;
    pad.letrecs <- ("toor_" ^ ho#mapped_name,
		    f_to) :: pad.letrecs;
    pad.letrec_sigs <- ("ofor_" ^ ho#mapped_name,
			`Fun(`Option(`Named("or_" ^ ho#mapped_name)),
			     `Value)
		       ) :: pad.letrec_sigs;
    pad.letrec_sigs <- ("toor_" ^ ho#mapped_name,
			`Fun(`Value,
			     `Option(`Named("or_" ^ ho#mapped_name)))
		       ) :: pad.letrec_sigs;

    (* wrap/unwrap *)
    pad.letrec_sigs <- ("unwrap_" ^ ho#mapped_name,
			`Fun(`Named("or_" ^ ho#mapped_name),
			     `Named("o_" ^ ho#mapped_name))
		       ) :: pad.letrec_sigs;
    let e_x = new_evar() in
    pad.letrecs <- ("unwrap_" ^ ho#mapped_name,
		    `Fun(e_x,
			 `Call(`Var("as_" ^ ho#mapped_name), [ `Evar e_x ]))
		   ) :: pad.letrecs;
    pad.letrec_sigs <- ("wrap_" ^ ho#mapped_name,
			`Fun(`Named("o_" ^ ho#mapped_name),
			     `Named("or_" ^ ho#mapped_name))
		       ) :: pad.letrec_sigs;
    let e_ov = new_evar() in
    pad.letrecs <- ("wrap_" ^ ho#mapped_name,
		    `Fun(e_ov,
			 `Coerce_expr(`Evar e_ov,
				      "",
				      "Hydro_lm.object_base")
			)
		   ) :: pad.letrecs;
  );

  (* "od_<name>": This is only reasonable for real classes *)
  if ho#objtype = `Class then (
    let ho_super = get_ho_super() in
    pad.ctypes <- ("od_" ^ ho#mapped_name,
		   `Object( [ (* inherits: *)
			      `Named("od_" ^ ho_super#mapped_name) 
			    ],
			    Array.to_list          (* methods: *)
			      (Array.map
				 (fun (n, mn, tt) ->
				    ( mn,
				      `Ref(translate_tt symboltable tt)
				    )
				 )
				 ho#data_elements
			      )
			)
		  ) :: pad.ctypes
  );

  (* "rr_<method>" and "oi_<name>": for both classes and interfaces *)
  List.iter
    (fun hf ->   (* almost identical to "r_<method>"! *)
       let rr_name = "rr_" ^ ho#mapped_name ^ "__" ^ hf#mapped_name in
       pad.ctypes <- (rr_name,
		      `Object([],                          (* inherits *)
			      [ "result",                  (* methods *)
				     (translate_tt symboltable hf#result);
			      ] @
				Array.to_list
				  (Array.map
				     (fun (name,mn,ty) ->
					("out_" ^ mn,
					 translate_tt symboltable ty
					)
				     )
				     hf#out_args)
			     )
		     ) :: pad.ctypes;
    )
    ho#op_elements;
  pad.ctypes <- ("oi_" ^ ho#mapped_name,
		 `Object( List.map                  (* inherits *)
			    (fun s -> `Named("oi_" ^ s#mapped_name))
			    d_super_ho_list,
		          List.map                  (* methods *)
			    (fun hf ->
			       (hf#mapped_name,
				class_fun_type_of_method symboltable ho hf))
			    ho#op_elements
			)
		) :: pad.ctypes;

  (* "o_<name>": Defined for real classes only *)
  if ho#objtype = `Class then (
    pad.ctypes <- ("o_" ^ ho#mapped_name,
		   `Object( ( if ho#objtype = `Class then  (* inherits: *)
				[ `Named("od_" ^ ho#mapped_name) ]
			      else
				[]
			    ) @ 
			      [ `Named ("oi_" ^ ho#mapped_name) ]
			    @
				( if ho#local then
				    []
				  else
				    [ `Named "Hydro_lm.object_base" ]
				),
			    []                             (* methods *)
			  )
		  ) :: pad.ctypes;
  );

  (* "O_<name>": Defined for real classes only (non-local) *)
  if ho#objtype = `Class && not ho#local then
    pad.exns <- ("O_" ^ ho#mapped_name,
		 `Named("o_" ^ ho#mapped_name), None) :: pad.exns;

  (* "as_<name>": Defined for real classes and interfaces (non-local) *)
  if ho#objtype = `Class && not ho#local then (
    pad.letrec_sigs <- ("as_" ^ ho#mapped_name,
			`Fun(`Named("#Hydro_lm.object_base"),
			     `Named("o_" ^ ho#mapped_name))
		       ) :: pad.letrec_sigs;
    let e_ov = new_evar() in
    pad.letrecs <- ("as_" ^ ho#mapped_name,
		    `Fun(e_ov,
			 `Catch_inflated
			   (`Noreturn
			      (`Call(`CallM(`Coerce_expr(`Evar e_ov,
							 "Hydro_lm.object_base",
							 "Hydro_lm.object_base"),
 					    "hydro_inflate"),
				     [ `String_lit id ])),
			    "O_" ^ ho#mapped_name)
			)
		   ) :: pad.letrecs;
  );

  (* "delegate_od_<name>": This is only reasonable for real classes *)
  if ho#objtype = `Class then (
    let ho_super = get_ho_super() in
    pad.class_sigs <- ("delegate_od_" ^ ho#mapped_name,
		       `Fun(`Named("#od_" ^ ho#mapped_name),
			    `Named("od_" ^ ho#mapped_name))
		      ) :: pad.class_sigs;
    let e_od = new_evar() in
    pad.classes <- ("delegate_od_" ^ ho#mapped_name,
		    `Fun(e_od,
			 `Object  (* no data object - do not copy the obj! *)
			   ( "self",
			     [ `Call                    (* inherits *)
				 (`Var("delegate_od_" ^ 
					 ho_super#mapped_name),
				  [ `Evar e_od ]
				 ) 
			     ],
			     ( Array.to_list            (* methods *)
				 (Array.map
				    (fun (n, mn, tt) ->
				       ( mn,
					 `CallM(`Evar e_od,
						mn)
				       )
				    )
				    ho#data_elements
				 )
			     ),
			     ("od_" ^ ho#mapped_name)
			   )
			)
		   ) :: pad.classes
  );

  (* "delegate_oi_<name>": for both classes and interfaces *)
  (
    let ho_super_name =
      match ho#super with
	| None -> 
	    (* use Ice::Object instead *)
	    if ho#local then
	      "delegate_oi_Ice_LocalObject"
	    else
	      "delegate_oi_Ice_Object"
	| Some ho_super -> 
	    "delegate_oi_" ^ ho_super#mapped_name in
    pad.class_sigs <- ("delegate_oi_" ^ ho#mapped_name,
		       `Fun(`Named("#oi_" ^ ho#mapped_name),
			    `Named("oi_" ^ ho#mapped_name))
		      ) :: pad.class_sigs;
    let e_oi = new_evar() in
    pad.classes <- ("delegate_oi_" ^ ho#mapped_name,
		    `Fun(e_oi,
			 `Object
			   ( "self",
			     [ `Call                      (* inherits *)
				 (`Var ho_super_name,
				  [ `Evar e_oi ]
				 ) 
			     ],
			     ( List.map
				 (fun hf ->
				    let mn = hf#mapped_name in
				    ( mn,
				      `CallM(`Evar e_oi,
					     mn)
				    )
				 )
				 ho#op_elements
			     ),
			     ("oi_" ^ ho#mapped_name)
			   )
			)
		   ) :: pad.classes
  );

  (* "dec_<name>": Only for non-local real classes *)
  if ho#objtype = `Class && not ho#local then (
    let ho_super = get_ho_super() in
    let dt = decoded_cls_type symboltable ho in
    pad.letrec_sigs <- ("dec_" ^ ho#mapped_name,
			`Fun(`Named_arg1(`Named "Hydro_types.slice", "list"),
			     `Tuple[dt;
				    `Named_arg1(`Named "Hydro_types.slice", 
						"list")])
		       ) :: pad.letrec_sigs;
    let e_slices1 = new_evar() in
    let e_slices2 = new_evar() in
    let e_slices2_hd = new_evar() in
    let e_slices2_tl = new_evar() in
    let e_parent_vals = new_evar() in

    let elems =
      Array.to_list
	(Array.map
	   (fun (n,_,tt) -> (n,tt,new_evar()))
	   ho#data_elements
	) in
    let e_elems = List.map (fun (_,_,e) -> e) elems in

    let dec_elems =
      List.map
	(fun (_,tt,e) ->
	   `Call(fst(generate_marshalling_to_ref symboltable tt),
		 [ `Evar e ])
	)
	elems in

    pad.letrecs <- ("dec_" ^ ho#mapped_name,
		    `Fun(e_slices1,
			 (`Call
			    (`Fun2(e_parent_vals,
				   e_slices2,
				   `Match_list
				     (`Evar e_slices2,
				      e_slices2_hd,
				      e_slices2_tl,
				      `Match_slice
					(`Evar e_slices2_hd,
					 id,
					 e_elems,
					 `Pair
					   (`Tuple(dec_elems @ 
						     [`Evar e_parent_vals ]) ,
					    `Evar e_slices2_tl
					   )
					)
				     )
				  ),
			     [ `CallF("dec_" ^ ho_super#mapped_name,
				      [ `Evar e_slices1 ]
				     )
			     ]
			    )
			 )
			)
		   ) :: pad.letrecs;
  );

  (* "mk_od_<name>": Only for real classes *)
  if ho#objtype = `Class then (
    let ho_super = get_ho_super() in
    let dt = decoded_cls_type symboltable ho in
    pad.class_sigs <- ("mk_od_" ^ ho#mapped_name,
		       `Fun(dt,
			    `Named("od_" ^ ho#mapped_name))
		      ) :: pad.class_sigs;

    let elems =
      Array.to_list
	(Array.map
	   (fun (_,mn,_) ->
	      (mn, new_evar())
	   )
	   ho#data_elements
	) in
    
    let e_parent = new_evar() in

    pad.classes <- ("mk_od_" ^ ho#mapped_name,
		    `FunN(List.map snd elems @ [e_parent],
			  `DataObject
			    ([ (* Inherit: *)
			       `Call(`Var("mk_od_" ^ ho_super#mapped_name),
				     [ `Evar e_parent ])
			     ],
			     ( List.map (* methods: *)
				 (fun (mn,e) ->
				    ( mn,
				      `Evar e
				    ))
				 elems
			     ),
			     ("od_" ^ ho#mapped_name)
			    )
			 )
		   ) :: pad.classes;
  );

  (* "enc_<name>": Only for non-local real classes *)
  if ho#objtype = `Class && not ho#local then (
    let ho_super = get_ho_super() in
    pad.letrec_sigs <- ("enc_" ^ ho#mapped_name,
			`Fun(`Named("od_" ^ ho#mapped_name),
			     `Named_arg1(`Named "Hydro_types.slice",
					 "list")
			    )
		       ) :: pad.letrec_sigs;
    let e_od = new_evar() in
    pad.letrecs <- ("enc_" ^ ho#mapped_name,
		    `Fun(e_od,
			 `List_cons
			   (`CallC
			      ("`Decoded",
			       [ `Tuple
				   [ `String_lit id;
				     `Array_lit
				       (Array.map
					  (fun (n, mn, tt) ->
					     let f =
					       fst
						 (generate_marshalling_of_ref
						    symboltable
						    tt) in
					     `Call
					       (f,
						[ `CallF
						    ("!",
						     [ `CallM
							 (`Evar e_od,
							  mn)
						     ])
						]
					       )
					  )
					  ho#data_elements
				       )
				   ]
			       ]
			      ),
			    `CallF("enc_" ^ ho_super#mapped_name,
				   [ `Coerce_expr(`Evar e_od,
						  ("od_" ^ ho#mapped_name),
						  ("od_" ^ ho_super#mapped_name))
				   ]
				  )
			   )
			)
		   ) :: pad.letrecs;
  );

  (* "sliced_od_<name>": Only for non-local real classes *)
  if ho#objtype = `Class && not ho#local then (
    pad.class_sigs <- ("sliced_od_" ^ ho#mapped_name,
		       `Fun(`Named ("#od_" ^ ho#mapped_name),
			    `Named ("Hydro_lm.sliced_base"))
		      ) :: pad.class_sigs;
    let e_od = new_evar() in
    pad.classes <- ("sliced_od_" ^ ho#mapped_name,
		    `Fun(e_od,
			 `Object("self",
				 [],                    (* inherits: none *)
				 [ "hydro_slices",      (* methods *)
				   ( `CallF("List.rev",
					    [ `CallF("enc_" ^ ho#mapped_name,
						     [ `Coerce_expr
							 ( `Evar e_od,
							   "",
							   ("od_" ^ 
							      ho#mapped_name)
							 ) 
						     ])
					    ])
				   );

				   "hydro_effective_id",
				   ( `String_lit id )
				 ],
				 "Hydro_lm.sliced_base"
				)
			)
		   ) :: pad.classes;
  );

  (* "declexns_<name>_<name>": always *)
  (
    List.iter
      (fun hf ->
	 pad.lets <-
	   ("declexns_" ^ ho#mapped_name ^ "__" ^ hf#mapped_name,
	    `List_lit(List.map
			(fun he -> `Variant he#mapped_name)
			hf#throws
		     )
	   ) :: pad.lets
      )
      ho#op_elements
  );

  (* "dispatch_<name>": always *)
  (
    (* We need the names of all operations:
       - that are defined for ho
       - or one of the super interfaces
       - but _not_ for one of the super classes
       - but _not_ for the interfaces implemented by one of the super classes
       - but _not_ Ice::(Local)Object
       This is computed by taking the difference: all_intf - all_super_classes
     *)
    let real_super_classes = 
      match ho#super with
	| None -> 
	    []
	| Some ho_super ->
	    let ho_super_id = Hgen_util.TS_util.colon_name ho_super#name in
	    List.map
	      (fun ho -> ho#name)
	      (CiMap.find ho_super_id implements) in
    let remaining_ho_list =
      List.filter
	(fun ho -> 
	   ho # name <> `Absolute [ "Ice"; "Object" ] &&
	   ho # name <> `Absolute [ "Ice"; "LocalObject" ] &&
	   not (List.mem ho#name real_super_classes))
	super_ho_list in
    let operations =
      List.flatten
	(List.map
	   (fun ho -> ho#op_elements)
	   remaining_ho_list) in

    pad.letrec_sigs <- ("dispatch_" ^ ho#mapped_name,
			`Fun(`Named("oi_" ^ ho#mapped_name),
			     `Fun(`String,
				  `Fun(`Array `Value,
				       `Fun(`Named "Hydro_types.session",
					    `Unit))))
		       ) :: pad.letrec_sigs;

    let e_oi = new_evar() in
    let e_name = new_evar() in
    pad.letrecs <- 
      ("dispatch_" ^ ho#mapped_name,
       `Fun(
	 e_oi,
	 `Fun(
	   e_name,
	   `Match_strings(
	     `Evar e_name,
	     (List.map
		(fun hf ->
		   (String.lowercase hf#name,
		    let e_in_args = new_evar() in
		    let e_session = new_evar() in
		    let e_emit = new_evar() in
		    let e_emit_exn = new_evar() in
		    let in_args = hf#in_args in
		    let in_arg_types = Array.map (fun (_,_,ty) -> ty) in_args in
		    `Fun(
		      e_in_args,
		      `Fun(
			e_session,
			`Call(
			  `Fun(
			    e_emit_exn,
			    `Call(
			      `Fun(
				e_emit,
				`Call(
				  `CallM(`Evar e_oi, hf#mapped_name),
				  ( Array.to_list
				      ( if in_arg_types = [| |] then
					  [| `Tuple [] |]
					else
					  Array.mapi
					    (fun i arg_ty ->
					       let of_fn, _ =
						 generate_marshalling_to_ref
						   symboltable arg_ty in
					       `Call(of_fn,
						     [ `Array_get(`Evar e_in_args,
								  `Int_lit i)
						     ])
					    )
					    in_arg_types
				      ) @
				      [ `Evar e_emit;
					`Evar e_emit_exn;
					`Evar e_session
				      ]
				  )
				)
			      ),
			      [ (* emit: *)
				let e_rr = new_evar() in
				let e_userexn = new_evar() in
				let e_anyexn = new_evar() in
				`Fun(
				  e_rr,
				  `Catch_userexn(
				    `Call(`CallM(`Evar e_session,
						 "emit_result"),
					  let result_of_fn, _ =
					    generate_marshalling_of_ref
					      symboltable hf#result in
					  [ `Call(result_of_fn,
						  [ `CallM(`Evar e_rr,
							   "result") ]);
					    `Array_lit
					      (Array.map
						 (fun (_,mn,ty) ->
						    let arg_to_fn, _ =
						      generate_marshalling_of_ref
							symboltable ty in
						    `Call(arg_to_fn,
							  [ `CallM(`Evar e_rr,
								   ("out_" ^ 
								      mn)) ])
						 )
						 hf#out_args)
					  ]),
				    e_userexn,
				    (`Call(`Evar e_emit_exn, 
					   [ `Evar e_userexn ])),
				    e_anyexn,
				    (`Call(`CallM(`Evar e_session,
						  "emit_unknown_exception"),
					   [ `CallF("Hydro_util.exn_to_string",
						    [ `Evar e_anyexn ]) ] ) )
				  )
				)
			      ])),
			  [ (* emit_exn: *)
			    let e_ue = new_evar() in
			    `Fun(e_ue, 
				 `Ifthenelse(
				   `CallF("List.mem",
					  [ `CallM(`Evar e_ue,
						   "hydro_ops#exn_name");
					    `Var("declexns_" ^ 
						   ho#mapped_name ^ "__" ^ 
						   hf#mapped_name)
					  ]),
				   `Call(`CallM(`Evar e_session,
						"emit_user_exception"),
					 [ `CallF("encode_exception",
						  [ `Evar e_ue ])
					 ]),
				   `Call(`CallM(`Evar e_session,
						"emit_unknown_user_exception"),
					 [ `CallM(`Evar e_ue,
						  "hydro_ops#exn_id") ])
				 ))
			  ]
		      )))))
		operations;
	     ),
	     (* Case: `Match_string catch rest *)
	     ( let super_dispatch_name =
		 match ho#super with
		   | None -> 
		       (* Substitute Ice::(Local)Object *)
		       if ho#local then "Ice_LocalObject" else "Ice_Object"
		   | Some ho_super ->
		       (* call the dispatch fn of the super class *)
		       ho_super#mapped_name
	       in
	       `CallF("dispatch_" ^ super_dispatch_name,
		      [ `Coerce_expr(`Evar e_oi,
				     "",
				     ("oi_" ^ super_dispatch_name));
			      `Evar e_name
		      ])
	     )
	   )
	 ))
      ) :: pad.letrecs;
  );

  (* "skel_<name>" *)
  (
    pad.class_sigs <- ("skel_" ^ ho#mapped_name,
		       `Named ("oi_" ^ ho#mapped_name)
		      ) :: pad.class_sigs;
    let inherit_from =
      List.filter
	(fun ho_super ->
	   ho_super # name <> `Absolute [ "Ice"; "Object" ]
	)
	d_super_ho_list in
    let typeid = Hgen_util.TS_util.colon_name ho#name in
    let all_typeids =
      Array.map
	(fun ho' -> Hgen_util.TS_util.colon_name ho'#name)
	(Array.of_list super_ho_list) in

    pad.classes <- ("skel_" ^ ho#mapped_name,
		    `Object
		      ("self",
		       ( List.map                               (* inherits *)
			   (fun ho_super ->
			      `Var ("skel_" ^ ho_super#mapped_name))
			   inherit_from
		       ) @
			 (* ops_Ice_Object must come last: *)
			 ( if ho#local then [] else
			     [ `Call(`Var "Hydro_lm_IceObject.ops_Ice_Object",
				     [ `String_lit typeid;
				       `Array_lit
					 (Array.map 
					    (fun s -> `String_lit s)
					    all_typeids)
				     ])
			     ]
			 ),
		       ( List.map
			   (fun hf ->
			      let e = class_skel_of_method hf in
			      (hf#mapped_name, e)
			   )
			   ho#op_elements
		       ) @
			 [ "hydro_invoke_operation",
			   (`CallF("dispatch_" ^ ho#mapped_name,
				   [ `Coerce_expr(`Var "self",
						  "",
						  "oi_" ^ ho#mapped_name)
				   ]))
			 ],
		       ("oi_" ^ ho#mapped_name)
		      )
		   ) :: pad.classes

  );

  (* "mk_<name>": Only for real classes *)
  if ho#objtype = `Class then (
    pad.class_sigs <- ("mk_" ^ ho#mapped_name,
		       `Fun(`Named ("#od_" ^ ho#mapped_name),
			    `Named ("o_" ^ ho#mapped_name))
		      ) :: pad.class_sigs;
    let e_od = new_evar() in
    let e_id = new_evar() in
    let super_classes = get_super_classes ho in

    pad.classes <- ("mk_" ^ ho#mapped_name,
		    `Fun
		      (e_od,
		       `Object
			 ("self",
			  ( [                       (* Inherits: *)
			      `Call(`Var("delegate_od_" ^ ho#mapped_name),
				    [ `Evar e_od ]
				   );
			      (* + skel_<name> *)
			    ] @
			      ( if ho#local then [] else
				  [ `Call(`Var("sliced_od_" ^ ho#mapped_name),
					  [ `Evar e_od ])
				  ]) @
			      ( [ `Var("skel_" ^ ho#mapped_name) ] )
			  ),
			  ( if ho#local then        (* methods *)
			      []
			    else
			      [ "hydro_inflate",
				(`Fun(e_id,
				      `Match_strings
					(`Evar e_id,
					 List.map
					   (fun ho' ->
					      let id' =
						Hgen_util.TS_util.colon_name 
						  ho'#name in
					      (id',
					       `Raise_expr
						 (`CallC
						    ("O_" ^ ho'#mapped_name,
						     [ `Coerce
							 ("self",
							  "o_" ^ ho#mapped_name,
							  "o_" ^ ho'#mapped_name
							 )
						     ]
						    )
						 )
					      )
					   )
					   super_classes,
					 `Fail
					)
				     )
				);
			      ]
			  ),
			  ("o_" ^ ho#mapped_name)
			 )
		      )
		   ) :: pad.classes;
  );

  (* "restore_<name>": Only for non-local real classes *)
  if ho#objtype = `Class && not ho#local then (
    pad.class_sigs <- ("restore_" ^ ho#mapped_name,
		       `Fun(`Named "Hydro_types.sliced_value",
			    `Named("o_" ^ ho#mapped_name))
		      ) :: pad.class_sigs;
    let e_sv = new_evar() in
    let e_od = new_evar() in
    pad.classes <- ("restore_" ^ ho#mapped_name,
		    `Fun(e_sv,
			 `Call(`Fun(e_od,
				    `Call(`Var("mk_" ^ ho#mapped_name),
					  [ `Evar e_od ])
				   ),
			       [ `CallC
				   ("new",
				    [ `Var("mk_od_" ^ ho#mapped_name);
				      `CallF
					("fst",
					 [ `CallF
					     ("dec_" ^ ho#mapped_name,
					      [ `CallM(`Typeann
							 (`Evar e_sv,
							  "Hydro_types.sliced_value"
							 ),
						       "hydro_slices")
					      ])
					 ])
				    ])
			       ])
			)
		   ) :: pad.classes
  );

  (* "classdt_<class>": Only for non-local real classes *)
  if ho#objtype = `Class && not ho#local then (
    let ho_super = get_ho_super() in
    pad.defterms <- ("classdt_" ^ ho#mapped_name,
		     id,
		     `Class,
		     (`Object("self",
			      [],
			      ["name", `String_lit id;
			       "super", `CallC("Some",
					       [ `Dtvar("classdt_" ^ 
							  ho_super#mapped_name)
					       ]
					      );
			       "elements",
			       ( `Array_lit
				   (Array.map
				      (fun (n,_,tt) ->
					 `Pair(`String_lit n, 
					       dt_of_type symboltable tt)
				      )
				      ho#data_elements)
			       )
			      ],
			      "Hydro_types.hclass"
			     )
		     )
		    ) :: pad.defterms;
    let e_sv = new_evar() in
    pad.defterms <- ("ctordt_" ^ ho#mapped_name,
		     id,
		     `Ctor,
		     (`Fun(e_sv,
			   `Coerce_expr(`CallC("new",
					       [ `Var("restore_" ^ 
							ho#mapped_name);
						 `Evar e_sv
					       ]),
					"",
					"Hydro_lm.object_base")
			  )
		     )
		    ) :: pad.defterms;
  );
;;


let cls_to_il2 modname symboltable pad ho =
  let id = Hgen_util.TS_util.colon_name ho#name in

  (* "or_<name>": This is only reasonable for non-local real classes *)
  if ho#objtype = `Class && not ho#local then (
    pad.types <- 
      ("or_" ^ ho#mapped_name,
       `Named(modname ^ ".or_" ^ ho#mapped_name)
      ) :: pad.types;

    (* "ofor_<name>" and "toor_<name>" *)
    pad.letrecs <- ("ofor_" ^ ho#mapped_name,
		    `Var(modname ^ ".ofor_" ^ ho#mapped_name)
		   ) :: pad.letrecs;
    pad.letrecs <- ("toor_" ^ ho#mapped_name,
		    `Var(modname ^ ".toor_" ^ ho#mapped_name)
		    ) :: pad.letrecs;
  );

  (* "od_<name>": This is only reasonable for real classes *)
  if ho#objtype = `Class then (
    pad.ctypes <- ("od_" ^ ho#mapped_name,
		   `Named(modname ^ ".od_" ^ ho#mapped_name)
		  ) :: pad.ctypes
  );

  (* "oi_<name>": Defined for both classes and interfaces *)
  pad.ctypes <- ("oi_" ^ ho#mapped_name,
		 `Named(modname ^ ".oi_" ^ ho#mapped_name)
		) :: pad.ctypes;

  (* "o_<name>": Defined for both classes and interfaces *)
  pad.ctypes <- ("o_" ^ ho#mapped_name,
		 `Named(modname ^ ".o_" ^ ho#mapped_name)
		) :: pad.ctypes;

  (* "O_<name>": Defined for both classes and interfaces (non-local) *)
  if not ho#local then
    pad.exns <- ("O_" ^ ho#mapped_name,
		 `Named("o_" ^ ho#mapped_name),
		 Some(modname ^ ".O_" ^ ho#mapped_name)
		) :: pad.exns;

  (* "dispatch_<name>": Defined for both classes and interfaces *)
  pad.letrecs <- ("dispatch_" ^ ho#mapped_name,
		  `Var(modname ^ ".dispatch_" ^ ho#mapped_name)
		 ) :: pad.letrecs;

  (* "as_<name>": not required *)

  (* "delegate_od_<name>": This is only reasonable for real classes *)
  if ho#objtype = `Class then (
(*
    pad.class_sigs <- ("delegate_od_" ^ ho#mapped_name,
		       `Fun(`Named("#od_" ^ ho#mapped_name),
			    `Named("od_" ^ ho#mapped_name))
		      ) :: pad.class_sigs;
 *)
    pad.classes <- ("delegate_od_" ^ ho#mapped_name,
		    `Var(modname ^ ".delegate_od_" ^ ho#mapped_name)
		   ) :: pad.classes
  );

  (* "delegate_oi_<name>": This is only reasonable for real classes *)
  if ho#objtype = `Class then (
    pad.classes <- ("delegate_oi_" ^ ho#mapped_name,
		    `Var(modname ^ ".delegate_oi_" ^ ho#mapped_name)
		   ) :: pad.classes
  );

  (* dec_<name>: Only for non-local real classes, not exported *)
  if ho#objtype = `Class && not ho#local then (
    pad.letrecs <- ("dec_" ^ ho#mapped_name,
		    `Var(modname ^ ".dec_" ^ ho#mapped_name)
		   ) :: pad.letrecs
  );

  (* mk_od_<name>: Only for real classes, not exported *)
  if ho#objtype = `Class then (
    pad.classes <- ("mk_od_" ^ ho#mapped_name,
		    `Var(modname ^ ".mk_od_" ^ ho#mapped_name)
		   ) :: pad.classes
  );

  (* enc_<name>: Only for non-local real classes, not exported *)
  if ho#objtype = `Class && not ho#local then (
    pad.letrecs <- ("enc_" ^ ho#mapped_name,
		    `Var(modname ^ ".enc_" ^ ho#mapped_name)
		   ) :: pad.letrecs
  );

  (* "sliced_od_<name>", "mk_<name>", "restore_<name>": not required *)

  (* "classdt_<class>": Only for non-local real classes *)
  if ho#objtype = `Class && not ho#local then (
    pad.defterms <- ("classdt_" ^ho#mapped_name,
		     id,
		     `Omit,
		     `CallF("Lazy.force",
			    [ `Var(modname ^ ".class_defterm_" ^ ho#mapped_name)
			    ])
		    ) :: pad.defterms;
  );

;;


let cls_to_il symboltable pad implements ho =
  match ho#imported_from with
    | None -> 
	cls_to_il1 symboltable pad implements ho
    | Some modname ->
	cls_to_il2 modname symboltable pad ho
;;


let objs_to_il symboltable pad objs local_flag =
  (* We need to iterate over the objects from top to bottom in the 
     inheritance graph
   *)
  let objs_done = ref CiSet.empty in
     (* A ho#name is member when traversed *)
  let rec traverse1 f ho =
    (* Call [f ho] for every object, in top-to-bottom order *)
    let n = Hgen_util.TS_util.colon_name ho#name in
    if not (CiSet.mem n !objs_done) then (
      objs_done := CiSet.add n !objs_done;
      ( match ho#super with
	  | None -> ()
	  | Some ho_super ->
	      traverse1 f ho_super
      );
      List.iter
	(fun ho_intf ->
	   traverse1 f ho_intf;
	)
	ho#super_intf;
      f ho;
    )
  and traverse f =
    objs_done := CiSet.empty;
    List.iter (traverse1 f) objs
  in

  (* Generate proxy definitions: This is not done for local interfaces *)

  if not local_flag then (
    (* "proxy_name" *)
    let all_mapped_names =
      Array.of_list(List.map (fun ho -> ho#mapped_name) objs) in
    let pname_type = `Variant all_mapped_names in
    pad.types <- ("proxy_name", pname_type) :: pad.types;
  );    

  (* Fill !implements once: *)
  let implements = ref CiMap.empty in
  (* Maps ho#name to the list of implemented interfaces *)
  
  traverse
    (fun ho ->
       let n = Hgen_util.TS_util.colon_name ho#name in
       let impl_super =
	 match ho#super with
	   | None -> []
	   | Some ho_super ->
	       let n_super = Hgen_util.TS_util.colon_name ho_super#name in
	       CiMap.find n_super !implements in
       let impl_list =
	 List.map 
	   (fun ho_intf ->
	      let n_intf = Hgen_util.TS_util.colon_name ho_intf#name in
	      CiMap.find n_intf !implements
	   )
	   ho#super_intf in
       implements := 
	 CiMap.add n (ho::(impl_super @ List.flatten impl_list)) !implements;
    );
   
  if not local_flag then (
    (* Now per proxy: *)
    traverse (intf_to_il symboltable pad !implements)
  );

  (* Generate class definitons *)
  traverse (cls_to_il symboltable pad !implements);

  (* Generate parachute: *)
  if not local_flag then (
    pad.letrec_sigs <- ("parachute",
			`Fun(`Fun(`Named "Hydro_types.session", `Named "'r"),
			     `Fun(`Fun(`Named "'r", `Unit),
				  `Fun(`Fun(`Named "user_exception", `Unit),
				       `Fun(`Named "Hydro_types.session",
					    `Unit))))) :: pad.letrec_sigs;
    
  let e_f = new_evar() in
  let e_emit = new_evar() in
  let e_emit_exn = new_evar() in
  let e_session = new_evar() in
  let e_uexn = new_evar() in
  let e_error = new_evar() in
  pad.letrecs <- ("parachute",
		  `Fun(e_f,
		       `Fun(e_emit,
			    `Fun(e_emit_exn,
				 `Fun(e_session,
				      `Catch_userexn(
					`Call(`Evar e_emit,
					      [`Call(`Evar e_f,
						     [`Evar e_session]) ]),
					e_uexn,
					`Call(`Evar e_emit_exn,
					      [ `Evar e_uexn ]),
					e_error,
					`Call(`CallM(`Evar e_session,
						     "emit_unknown_exception"),
					      [ `CallF("Hydro_util.exn_to_string",
						       [ `Evar e_error ]) ])
				      )
				     ))))
		 ) :: pad.letrecs;
  )
;;


let const_to_il symboltable pad name mapped_name hc =
  pad.lets <- ("const_" ^ mapped_name,
	       ( match hc with
		   | `Int n -> `Int_lit n
		   | `Int32 n -> `Int32_lit n
		   | `Int64 n -> `Int64_lit n
		   | `Float f -> `Float_lit f
		   | `String s -> `String_lit s
		   | `Bool b -> `Bool_lit b
	       )
	      ) :: pad.lets;
  pad.letrec_sigs <- ("const_" ^ mapped_name,
		      ( match hc with
			  | `Int n -> `Int
			  | `Int32 n -> `Int32
			  | `Int64 n -> `Int64
			  | `Float f -> `Float
			  | `String s -> `String
			  | `Bool b -> `Bool
		      )
		     ) :: pad.letrec_sigs;
;;


let to_il symboltable =
  (* Translate the symboltable to an IL pad. We iterate over the table
     and for every entity we add something to the pad
   *)

  let pad =
    { types = [];
      ctypes = [];
      letrecs = [];
      letrecs2 = [];
      letrecs3 = [];
      lets = [];
      letrec_sigs = [];
      defterms = [];
      classes = [];
      class_sigs = [];
      exns = [];
    }
  in

  let exns = ref [] in
  let objs = ref [] in
  CiHashtbl.iter
    (fun n ent ->
       match ent with
	 | `Module _ -> ()
	 | `Object ho -> objs := ho :: !objs
	 | `Exn he -> exns := he :: !exns
	 | `Type ht -> type_to_il symboltable pad ht
	 | `Const (name,mname,hc) -> const_to_il symboltable pad name mname hc
    )
    symboltable;

  let local_exns, remote_exns =
    List.partition (fun he -> he#local) !exns in

  exns_to_il symboltable pad true  local_exns;
  exns_to_il symboltable pad false remote_exns;

  let local_objs, remote_objs =
    List.partition (fun ho -> ho#local) !objs in

  objs_to_il symboltable pad remote_objs false;
  objs_to_il symboltable pad local_objs true;

  Hgen_simplif.simplify_pad pad
;;
