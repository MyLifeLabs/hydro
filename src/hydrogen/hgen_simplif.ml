(* $Id$ *)

open Hgen_types
open Hgen_types.IL
open Printf

(* Simplifications:

   * (fun x -> t) e     --> t [x\e]
   * (fun x -> f x)     --> f
 *)

let rec descent_expr (f:expr_term -> expr_term) (e:expr_term) 
          : expr_term  =
  match e with
    | `Match1(e1, p, e2) ->
	`Match1(f e1, p, f e2)
    | `Match1_Null(e1, p, e2, e3) ->
	`Match1_Null(f e1, p, f e2, f e3)
    | `Match1_DM(e1, p, e2, s) ->
	`Match1_DM(f e1, p, f e2, s)
    | `Match_variants(e1, l) ->
	`Match_variants(f e1, List.map (fun (v,e) -> (v,f e)) l)
    | `Match_strings(e1, l, e2) ->
	`Match_strings(f e1, List.map (fun (s,e) -> (s,f e)) l, f e2)
    | `Match_option(e1, k, e2, e3) ->
	`Match_option(f e1, k, f e2, f e3)
    | `Match_list(e1, k1, k2, e2) ->
	`Match_list(f e1, k1, k2, f e2)
    | `Match_slice(e1, s, l, e2) ->
	`Match_slice(f e1, s, l, f e2)
    | `Match_tuple(e1, l, e2) ->
	`Match_tuple(f e1, l, f e2)
    | `Call(e1, l) ->
	`Call(f e1, List.map f l)
    | `CallF(s, l) ->
	`CallF(s, List.map f l)
    | `CallC(s, l) ->
	`CallC(s, List.map f l)
    | `CallM(e, s) ->
	`CallM(f e, s)
    | `Array_get(e1,e2) ->
	`Array_get(f e1, f e2)
    | `Array_lit a ->
	`Array_lit(Array.map f a)
    | `List_lit l ->
	`List_lit(List.map f l)
    | `List_cons(e1,e2) ->
	`List_cons(f e1, f e2)
    | `Record_get(e1, n) ->
	`Record_get(f e1, n)
    | `Record_lit a ->
	`Record_lit(Array.map (fun (n,e) -> (n,f e)) a)
    | `Record_lit_ord a ->
	`Record_lit_ord(Array.map (fun (n,e) -> (n,f e)) a)
    | `Pair(e1,e2) ->
	`Pair(f e1,f e2)
    | `Tuple l ->
	`Tuple(List.map f l)
    | `Tuple_ord l ->
	`Tuple_ord (List.map f l)
    | `Fun0 e1 ->
	`Fun0 (f e1)
    | `Fun(k, e1) ->
	`Fun(k, f e1)
    | `Fun2(k1,k2,e1) ->
	`Fun2(k1,k2,f e1)
    | `FunN(l, e1) ->
	`FunN(l, f e1)
    | `MFun(l, e1) ->
	`MFun(l, f e1)
    | `Object(s1,ct,l,s2) ->
	`Object(s1,ct,List.map(fun (u,e1) -> (u,f e1)) l,s2)
    | `Ifthenelse(e1,e2,e3) ->
	`Ifthenelse(f e1,f e2,f e3)
    | `Statements stms ->
	`Statements(Array.map (fun stm -> f stm) stms)
    | `Typeann(e1, s) ->
	`Typeann(f e1, s)
    | `Coerce_expr(e1,s1,s2) ->
	`Coerce_expr(f e1,s1,s2)
    | `Catch_not_found e1 ->
	`Catch_not_found (f e1)
    | `Catch_inflated(e1,s) ->
	`Catch_inflated(f e1,s)
    | `Catch_userexn(e1,k1,e2,k2,e3) ->
	`Catch_userexn(f e1,k1,f e2,k2,f e3)
    | `Raise_expr e1 ->
	`Raise_expr (f e1)
    | `Noreturn e1 ->
	`Noreturn (f e1)

    | `Int_lit _ 
    | `Int32_lit _ 
    | `Int64_lit _
    | `String_lit _
    | `Float_lit _
    | `Bool_lit _ 
    | `Evar _
    | `Var _
    | `Dtvar _
    | `Variant _
    | `Coerce _
    | `Raise _
    | `Fail ->
	e


let rec subst_evar (v:int) (e2:expr_term) (e1:expr_term) =
  (* e1[v\e2] *)
  match e1 with
    | `Evar u when u=v ->
	e2
    | _ ->
	descent_expr (subst_evar v e2) e1


let rec simplify_expr (e:expr_term) =
  match e with
    | `Call(`Fun(v, e1), e2 :: eother) ->
	let e1' = subst_evar v e2 e1 in
	if eother = [] then
	  simplify_expr e1'
	else
	  simplify_expr (`Call(e1', eother))
	    
    | `Call(`MFun(v :: vl, e1), e2 :: el) ->
	let e1' = subst_evar v e2 e1 in
	simplify_expr 
	  ( match vl, el with
	      | [], [] -> e1'
	      | [], _  -> `Call(e1', el)
	      | _, []  -> `MFun(vl, e1')
	      | _, _   -> `Call(`MFun(vl, e1'), el)
	  )
    | `Fun(_, `Call(`Variant _, _ )) ->   (* skip this simplification *)
	descent_expr simplify_expr e
    | `MFun(_, `Call(`Variant _, _ )) ->  (* skip this simplification *)
	descent_expr simplify_expr e
    | `Fun(v, `Call(e1, vl )) ->
	let rvl = List.rev vl in
	( match rvl with
	    | [`Evar u] when v = u ->
		simplify_expr e1
	    | (`Evar u) :: rvl' when v = u ->
		simplify_expr (`Call(e1, List.rev rvl'))
	    | _ -> 
		descent_expr simplify_expr e
	)

    | `Fun(v, `CallF(n, vl )) ->
	let rvl = List.rev vl in
	( match rvl with
	    | [`Evar u] when v = u ->
		simplify_expr (`Var n)
	    | (`Evar u) :: rvl' when v = u ->
		simplify_expr (`CallF(n, List.rev rvl'))
	    | _ -> 
		descent_expr simplify_expr e
	)

    | `MFun(vl1, `Call(e1, vl2 )) ->
	let rvl1 = List.rev vl1 in
	let rvl2 = List.rev vl2 in
	( match rvl1, rvl2 with
	    | [v1], [`Evar v2] when v1=v2 ->
		simplify_expr e1
	    | (v1 :: rvl1'), (`Evar v2 :: rvl2') 
		when v1 = v2 && rvl1' <> [] && rvl2' <> [] ->
		simplify_expr
		  (`MFun(List.rev rvl1', `Call(e1, List.rev rvl2')))
	    | _ -> 
		descent_expr simplify_expr e
	)

    | `MFun(vl1, `CallF(n, vl2 )) ->
	let rvl1 = List.rev vl1 in
	let rvl2 = List.rev vl2 in
	( match rvl1, rvl2 with
	    | [v1], [`Evar v2] when v1=v2 ->
		simplify_expr (`Var n)
	    | (v1 :: rvl1'), (`Evar v2 :: rvl2')
		when v1 = v2 && rvl1' <> [] && rvl2' <> [] ->
		simplify_expr
		  (`MFun(List.rev rvl1', `CallF(n, List.rev rvl2')))
	    | _ -> 
		descent_expr simplify_expr e
	)
    | _ -> 
	descent_expr simplify_expr e


let simplify_top_expr (e:expr_term) =
  (* At the top-level of a "let rec" there must be a function. So 
     skip that
   *)
  match e with
    | `Fun(_, _)
    | `MFun(_, _) ->
	descent_expr simplify_expr e
    | _ ->
	simplify_expr e


let rec simplify_class (c:class_term) : class_term =
  match c with
    | `Var _ ->
	c
    | `Fun(k, c1) ->
	`Fun(k, simplify_class c1)
    | `FunN(l, c1) ->
	`FunN(l, simplify_class c1)
    | `Call(c1, l) ->
	`Call(simplify_class c1, List.map simplify_expr l)
    | `Object(s1, cl, el, s2) ->
	`Object(s1,
		List.map simplify_class cl,
		List.map (fun (n,e) -> (n,simplify_expr e)) el,
		s2)
    | `DataObject(cl, el, s) ->
	`DataObject(List.map simplify_class cl,
		    List.map (fun (n,e) -> (n,simplify_expr e)) el,
		    s)


let simplify_pad pad =
  { pad with
      lets = List.map (fun (n,e) -> (n, simplify_expr e)) pad.lets;
      letrecs = List.map (fun (n,e) -> (n, simplify_top_expr e)) pad.letrecs;
      letrecs2 = List.map (fun (n,e) -> (n, simplify_top_expr e)) pad.letrecs2;
      letrecs3 = List.map (fun (n,e) -> (n, simplify_top_expr e)) pad.letrecs3;
      classes = List.map (fun (n,c) -> (n, simplify_class c)) pad.classes
  }


(**********************************************************************)
(* split a letrec into several letrecs *)

(* This transformation does not change the meaning of the letrecs:

   letrec f1 = ... and f2 = ... and f3 = ... and f4 = ...

   is transformed into

   letrec f1 = ... and f3 = ...
   letrec f2 = ... and f4 = ...

   i.e. into a sequence of several letrecs.

   It appears that there is a performance problem in the ocamlopt code
   generator when there are big letrecs.
 *)


module StrSet = Set.Make(String)

let rec occurrences sref e =
  match e with
    | `Var n ->
	sref := StrSet.add n !sref;
	e
    | `CallF(n,_) ->
	sref := StrSet.add n !sref;
	descent_expr (occurrences sref) e
    | _ ->
	descent_expr (occurrences sref) e


let get_graph letrecs =
  let g = Hashtbl.create 50 in
  (* maps a letrec to the set of called letrecs *)

  List.iter
    (fun (n, e) ->
       let sref = ref StrSet.empty in
       ignore(occurrences sref e);
       Hashtbl.add g n !sref
    )
    letrecs;

  (* Filter the values of the graph so only names remain that are also
     keys: *)
  List.iter
    (fun (n, _) ->
       let s = Hashtbl.find g n in
       let s' = StrSet.filter (fun n' -> Hashtbl.mem g n') s in
       Hashtbl.replace g n s'
    )
    letrecs;

(*
  prerr_endline "CALL GRAPH:";
  Hashtbl.iter
    (fun n s ->
       eprintf "%s uses %s\n"
	 n (String.concat "," (StrSet.fold (fun n' acc -> n' :: acc) s []))
    )
    g;
 *)

  g


let rec until_element n l =
  (* Get the longest prefix of l so that n is not in it, i.e.
     if l = [l1; l2; l3; ...; lk; n; ...] return [l1; l2; l3;... lk].
     If n is not in l then return l unmodified
   *)
  match l with
    | x :: l' ->
	if x = n then
	  []
	else
	  x :: until_element n l'
    | [] ->
	[]


type partition =
    (* represented as union-find data structure *)
    { name : string;
      mutable parent : partition option;
      mutable rank : int
    }


let split_letrecs letrecs =
  (* Splits a single letrec into a list of consecutive letrecs.
     i.e. we decompose the call graph into a spanning tree of circular
     sub graphs
   *)
  let terms = Hashtbl.create 50 in
  (* Maps function names to function terms *)
  List.iter
    (fun (n, e) ->
       Hashtbl.add terms n e
    )
    letrecs;

  let g = get_graph letrecs in

  let partitions = Hashtbl.create 50 in
  (* Maps name to partition *)

  let new_partition n =
    let p =
      { name = n;
	parent = None;
	rank = 0
      } in
    Hashtbl.add partitions n p;
    p
  in

  let get_partition n =
    try
      Hashtbl.find partitions n
    with Not_found -> 
      new_partition n
  in

  let rec find_representative p =
    (* Return the representative for a partition. This is the "FIND" in
       UNION-FIND.
     *)
    match p.parent with
      | Some pp -> 
	  let pp' = find_representative pp in
	  p.parent <- Some pp';
	  pp
      | None -> p
  in

  let unite_partitions p1 p2 =
    (* Unite two partitions. This is the "UNION" in UNION-FIND. *)
    let r1 = find_representative p1 in
    let r2 = find_representative p2 in
    if r1.rank > r2.rank then
      r2.parent <- Some r1
    else if r1.rank < r2.rank then
      r1.parent <- Some r2
    else if r1 != r2 then (
      r2.parent <- Some r1;
      r1.rank <- r1.rank + 1
    )
  in

  let rec split visited path n =
    (* Walk the call graph, and create/merge partitions while doing so.
       We start walking at [n]. In [path] we remember the list of callers.
       [visited] is the same as [path], but represented as set. When
       we reach a function [n] that is already member of [visited/path]
       we have found a recursion.

       As a side effect, we create a partition for every visited function,
       and we unite all partitions that are in the same recursion circle.
     *)
    if StrSet.mem n visited then (
      (* Found a recursion. *)
      let circle = until_element n path in
      let circle_head = get_partition n in
      let circle_parts = List.map get_partition circle in
      List.iter
	(fun p ->
	   unite_partitions circle_head p
	)
	circle_parts
    )
    else (
      ignore(new_partition n);
      let l = try Hashtbl.find g n with Not_found -> assert false in
      let visited' = StrSet.add n visited in
      let path' = n :: path in
      StrSet.iter
	(fun occ ->
	   split visited' path' occ
	)
	l
    )
  in

  let names = List.map fst letrecs in  (* all function names as list *)
  List.iter
    (fun n ->
       if not(Hashtbl.mem partitions n) then
	 split StrSet.empty [] n
    )
    names;

  (* At this point, the [partitions] represent: Functions that mutually call
     each other are in the same partition. We don't know yet, however,
     the relationships between the partitions, i.e. which partition
     calls which other partition. Visually we have several "letrecs"
     but don't know yet their order.
   *)

  (* Each partition has a representative, i.e. the function that happens
     to be at the root of the UNION-FIND tree.
   *)

  let part_members = Hashtbl.create 50 in
  (* Maps a representative name to the set of members *)

  let part_uses = Hashtbl.create 50 in
  (* Maps a representative name to the set of names denoting the used (called)
     partitions
   *)

  Hashtbl.iter
    (fun n p ->
       let p_repr = find_representative p in
       let p_repr_name = p_repr.name in
       let m = (try Hashtbl.find part_members p_repr_name
		with Not_found -> StrSet.empty) in
       Hashtbl.replace part_members p_repr_name (StrSet.add n m);
       let l = try Hashtbl.find g n with Not_found -> assert false in
       let u = ref (try Hashtbl.find part_uses p_repr_name 
		    with Not_found -> StrSet.empty) in
       StrSet.iter
	 (fun n' ->
	    let q_repr = find_representative (get_partition n') in
	    let q_repr_name = q_repr.name in
	    if q_repr_name <> p_repr_name then  (* skip direct recursion *)
	      u := StrSet.add q_repr_name !u
	 )
	 l;
       Hashtbl.replace part_uses p_repr_name !u
    )
    partitions;

(*
  prerr_endline "PARTITIONS:";
  Hashtbl.iter
    (fun n mems ->
       eprintf "%s has members %s\n"
	 n (String.concat "," (StrSet.fold (fun n' acc -> n' :: acc) mems []))
    )
    part_members;
 *)

(*
  prerr_endline "CALL GRAPH ON PARTITION LEVEL:";
  Hashtbl.iter
    (fun n preds ->
       eprintf "%s calls %s\n"
	 n (String.concat "," (StrSet.fold (fun n' acc -> n' :: acc) preds []))
    )
    part_uses;
 *)

  let output_visited = Hashtbl.create 50 in

  let rec output n =
    if Hashtbl.mem output_visited n then
      []
    else (
      let u = try Hashtbl.find part_uses n with Not_found -> StrSet.empty in
      Hashtbl.add output_visited n ();
      StrSet.fold
	(fun n' acc ->
	   acc @ output n'
	)
	u
	[]
	@ [n]
    )
  in
  let r = 
    Hashtbl.fold
      (fun n _ acc ->
	 let out = output n in
	 acc @ out
      )
      part_members
      [] in
  List.map
    (fun n ->
       let mems = 
	 try Hashtbl.find part_members n with Not_found -> StrSet.empty in
       StrSet.fold 
	 (fun n' acc -> (n', Hashtbl.find terms n') :: acc)
	 mems
	 []
    )
    r
