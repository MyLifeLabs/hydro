open Hydro_types
open Hydro_lm
open Hydro_prelim

type proxy_name = [ `Ice_Object ]
and t_Ice_Object = pr_Ice_Object option
and pr_Ice_Object = [ `Ice_Object ] Hydro_lm.proxy_reference

let ofpr_Ice_Object = Hydro_lm.Unsafe.of_proxy_reference
let topr_Ice_Object = Hydro_lm.Unsafe.to_proxy_reference

class type ['t] r_simple_response =
object
  method hydro_response : Hydro_lm.client_response
  method result : 't
end

class type r_Ice_Object_ice_ping =
  [unit] r_simple_response

class type r_Ice_Object_ice_id =
  [string] r_simple_response

class type r_Ice_Object_ice_ids =
  [string array] r_simple_response

class type r_Ice_Object_ice_isA =
  [bool] r_simple_response

let to_r_Ice_Object_ice_ping (result : Hydro_lm.client_response) =
object
  method hydro_response = result
  method result =
    match result#result with
      | VNothing -> ()
      | _ -> assert false
end

let to_r_Ice_Object_ice_id (result : Hydro_lm.client_response) =
object
  method hydro_response = result
  method result =
    match result#result with
      | VString s -> s
      | _ -> assert false
end

let to_r_Ice_Object_ice_ids (result : Hydro_lm.client_response) =
object
  method hydro_response = result
  method result =
    match result#result with
      | VSequence seq ->
	  Array.map (function
		       | VString s -> s
		       | _ -> assert false) seq
      | _ -> assert false
end

let to_r_Ice_Object_ice_isA (result : Hydro_lm.client_response) =
object
  method hydro_response = result
  method result =
    match result#result with
      | VBool b -> b
      | _ -> assert false
end

class type poi_Ice_Object =
object
  method ice_ping : unit -> r_Ice_Object_ice_ping Hydro_lm.call_suspension_t
  method ice_id : unit -> r_Ice_Object_ice_id Hydro_lm.call_suspension_t
  method ice_ids : unit -> r_Ice_Object_ice_ids Hydro_lm.call_suspension_t
  method ice_isA : string -> r_Ice_Object_ice_isA Hydro_lm.call_suspension_t
end


class type po_Ice_Object =
object
  inherit Hydro_proxy.proxy_t
  inherit poi_Ice_Object
  method hydro_proxy_reference : pr_Ice_Object
end


class pci_Ice_Object (proxy : Hydro_proxy.proxy_t) =
  let intf =
    try CiHashtbl.find proxy#hydro_env#system#interfaces "::Ice::Object"
    with Not_found -> assert false in
object(self)
  method ice_ping() =
    new Hydro_lm.call_suspension
      (proxy # hydro_twoway_call
           intf
           "ice_ping"
           [| |]
      )
      to_r_Ice_Object_ice_ping
      proxy#hydro_env#event_system
  method ice_id() =
    new Hydro_lm.call_suspension
      (proxy # hydro_twoway_call
           intf
           "ice_id"
           [| |]
      )
      to_r_Ice_Object_ice_id
      proxy#hydro_env#event_system
  method ice_ids() =
    new Hydro_lm.call_suspension
      (proxy # hydro_twoway_call
           intf
           "ice_ids"
           [| |]
      )
      to_r_Ice_Object_ice_ids
      proxy#hydro_env#event_system
  method ice_isA typeid =
    new Hydro_lm.call_suspension
      (proxy # hydro_twoway_call
           intf
           "ice_isA"
           [| VString typeid |]
      )
      to_r_Ice_Object_ice_isA
      proxy#hydro_env#event_system
end


class pc_Ice_Object proxy_env pr =
  let proxy =
    Hydro_proxy.proxy
      ~env:proxy_env
      ~addr:(Hydro_lm.Unsafe.unwrap_proxy (pr : pr_Ice_Object)
	       : Hydro_types.proxy_addr :> Hydro_proxy.extended_proxy_addr)
      () in
object(self)
  inherit pci_Ice_Object proxy
  inherit Hydro_proxy.proxy_delegation proxy
  method hydro_proxy_reference = pr
end


let pc_Ice_Object = new pc_Ice_Object


let unchecked_pr_Ice_Object pr =
  Hydro_lm.Unsafe.wrap_proxy (Hydro_lm.Unsafe.unwrap_proxy pr)


class type od_Ice_Object =
object
  method facets : unit ref
end

type rr_Ice_Object_ice_ping = < result : unit >
type rr_Ice_Object_ice_id = < result : string >
type rr_Ice_Object_ice_ids = < result : string array >
type rr_Ice_Object_ice_isA = < result : bool >

type uncallable

class type oi_Ice_Object =
object
  method ice_ping : unit ->
                    (rr_Ice_Object_ice_ping -> unit) ->
                    (uncallable -> unit) ->
                    Hydro_types.session ->
                      unit
  method ice_id : unit ->
                  (rr_Ice_Object_ice_id -> unit) ->
                  (uncallable -> unit) ->
                  Hydro_types.session ->
                      unit
  method ice_ids : unit ->
                   (rr_Ice_Object_ice_ids -> unit) ->
                   (uncallable -> unit) ->
                   Hydro_types.session ->
                      unit
  method ice_isA : string ->
                   (rr_Ice_Object_ice_isA -> unit) ->
                   (uncallable -> unit) ->
                   Hydro_types.session ->
                      unit
  method hydro_invoke_operation :
    string -> Hydro_types.value array -> Hydro_types.session -> unit
  method hydro_effective_id :
    string
end

class type o_Ice_Object =
object
  inherit od_Ice_Object
  inherit oi_Ice_Object
  inherit Hydro_lm.object_base
end

type or_Ice_Object = object_base

exception O_Ice_Object of o_Ice_Object


let as_Ice_Object ov =
  try
    let _ =
      (ov : #object_base :> object_value) # hydro_inflate "::Ice::Object" in
    assert false
  with
    | O_Ice_Object ov' -> ov'
    | _ -> raise Hydro_lm.Invalid_coercion

let wrap_Ice_Object (o : o_Ice_Object) = (o :> object_base)
let unwrap_Ice_Object o = as_Ice_Object o

let ofor_Ice_Object = Hydro_lm.value_of_object

let toor_Ice_Object (v : value) =
  match v with
    | VClass vr ->
	Hydro_lm.object_of_class_repr vr
    | _ ->
	raise(Unmarshal_error("Hydro_lm_IceObject.toor_Ice_Object"))


class delegate_od_Ice_Object od : od_Ice_Object =
object
  method facets = od#facets
end


class delegate_oi_Ice_Object oi : oi_Ice_Object =
object
  method ice_ping = oi#ice_ping
  method ice_id = oi#ice_id
  method ice_ids = oi#ice_ids
  method ice_isA = oi#ice_isA
  method hydro_invoke_operation = oi#hydro_invoke_operation
  method hydro_effective_id = oi#hydro_effective_id
end


let dec_Ice_Object slices =
  (* let parent_vals, slices = dec_parent slices in *)
  match slices with
    | (`Decoded("::Ice::Object", [| facets |] )) :: slices' ->
	( () (* ,parent_vals *), slices')
    | _ ->
	raise(Unmarshal_error("Class value is not an ::Ice::Object"))


class mk_od_Ice_Object (facets : unit) =
object
  val facets = ref facets
  method facets = facets
end


(* A subclass C of Ice::Object would do:

  let dec_C slices =
    let parent_vals, slices' = dec_Ice_Object slices in
    match slices' with
      | (`Decoded("::C", [| foo; ... |] )) :: slices'' ->
	  ( ( of_foo foo, ... ,parent_vals), slices'')
      | _ ->
	  raise(Unmarshal_error("Class value is not a ::C"))

  class mk_od_C (foo, ..., parent_vals) =
  object
    inherit mk_od_Ice_Object parent_vals
    method foo = ref foo
  end
 *)

let enc_Ice_Object (od : #od_Ice_Object) =
  let slice = `Decoded("::Ice::Object",
		       [| (* of_XXX ! (od # facets) *)
			  VSequence [| |]
		       |]) in
  [ slice ]


class sliced_od_Ice_Object od : sliced_base =
object
  method hydro_slices = List.rev (enc_Ice_Object od)
  method hydro_effective_id = "::Ice::Object"
end

(* A subclass C of Ice::Object would do:

   let enc_C od =
     let slice = `Decoded("::C",
                          [| of_foo_type (! ( od#foo ) ) |] ) in
     slice :: enc_Ice_Object od

   class slice_od_C od =
   object
     method hydro_slices = List.rev (enc_C od)
     method hydro_effective_id = "::C"
   end
 *)


let no_emit_exn _ = assert false

let dispatch_Ice_Object obj opname =
  match opname with
    | "ice_ping" ->
	(fun in_args session ->
	   let emit _ = session # emit_result VNothing [| |] in
	   obj # ice_ping () emit no_emit_exn session
	)
    | "ice_id" ->
	(fun in_args session ->
	   let emit rr = session # emit_result (VString rr#result) [| |] in
	   obj # ice_id () emit no_emit_exn session
	)
    | "ice_ids" ->
	(fun in_args session ->
	   let to_val a =
	     VSequence (Array.map (fun s -> VString s) a) in
	   let emit rr = session # emit_result (to_val rr#result) [| |] in
	   obj # ice_ids () emit no_emit_exn session
	)
    | "ice_isa" ->
	(fun in_args session ->
	   let typeid =
	     match in_args with
	       | [| VString s |] -> s
	       | _ -> failwith "ice_isA" in
	   let emit rr = session # emit_result (VBool rr#result) [| |] in
	   obj # ice_isA typeid emit no_emit_exn session
	)
    | _ -> raise Not_found


class ops_Ice_Object typeid typeids : oi_Ice_Object =
object(self)
  method ice_ping () emit emit_exn session =
    emit (object method result = () end)
  method ice_id () emit emit_exn session =
    emit (object method result = typeid end)
  method ice_ids () emit emit_exn session =
    emit (object method result = typeids end)
  method ice_isA argid emit emit_exn session =
    (* TODO: case insentivity *)
    let b = Array.fold_left (fun b id -> b || argid=id) false typeids in
    emit (object method result = b end)
  method hydro_invoke_operation opname =
    dispatch_Ice_Object self opname
  method hydro_effective_id =
    typeid
end


class skel_Ice_Object =
  ops_Ice_Object "::Ice::Object" [| "::Ice::Object" |]


class mk_Ice_Object od : o_Ice_Object =
object(self)
  inherit delegate_od_Ice_Object od
  inherit sliced_od_Ice_Object od
  inherit skel_Ice_Object

  method hydro_inflate id =
    match id with
      | "::Ice::Object" ->
	  raise(O_Ice_Object(self : #o_Ice_Object :> o_Ice_Object))
      | _ ->
	  raise Invalid_coercion
end


class restore_Ice_Object (sv :sliced_value) =
  let od = new mk_od_Ice_Object (fst (dec_Ice_Object sv#hydro_slices)) in
  mk_Ice_Object od


let ctor_Ice_Object sv =
  (new restore_Ice_Object sv :> object_base)



let defterm_Ice_Object =
  lazy
    ( object
	method name = "::Ice::Object"
	method super = []
	method elements =
	  [ ( object
		method name = "ice_ping"
		method mode = `Idempotent
		method in_args = [| |]
		method in_classes = false
		method out_args = [| |]
		method result = TVoid
		method out_classes = false
              end : Hydro_types.hfunction
	    );
	    ( object
		method name = "ice_id"
		method mode = `Idempotent
		method in_args = [| |]
		method in_classes = false
		method out_args = [| |]
		method result = TString
		method out_classes = false
              end : Hydro_types.hfunction
	    );
	    ( object
		method name = "ice_ids"
		method mode = `Idempotent
		method in_args = [| |]
		method in_classes = false
		method out_args = [| |]
		method result = TSequence TString
		method out_classes = false
              end : Hydro_types.hfunction
	    );
	    ( object
		method name = "ice_isA"
		method mode = `Idempotent
		method in_args = [| "typeID", TString |]
		method in_classes = false
		method out_args = [| |]
		method result = TBool
		method out_classes = false
              end : Hydro_types.hfunction
	    );
	  ]
      end : Hydro_types.hintf
    )


let class_defterm_Ice_Object =
  lazy
    ( object
	method name = "::Ice::Object"
	method super = None
	method elements = [| "facets", TSequence TVoid |]
      end : Hydro_types.hclass
    )


let fill_system sys =
  let intf = Lazy.force defterm_Ice_Object in
  CiHashtbl.add
    sys#interfaces
    intf#name
    intf;
  let cls = Lazy.force class_defterm_Ice_Object in
  CiHashtbl.add
    sys#classes
    cls#name
    cls;
  CiHashtbl.add
    sys#ctors
    cls#name
    ctor_Ice_Object
;;

