(* $Id$ *)

(** The default [::Ice::Object] *)

(** This modules is used by the generated language mapping layer to
    refer to [::Ice::Object], the root of the class hierarchy.
 *)

(** {1 Proxy view} *)

type proxy_name = [ `Ice_Object ]
and pr_Ice_Object = [ `Ice_Object ] Hydro_lm.proxy_reference

val ofpr_Ice_Object : pr_Ice_Object option -> Hydro_types.value
val topr_Ice_Object : Hydro_types.value -> pr_Ice_Object option

(* To be extended. We only have [ice_ping] for now. *)

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

val to_r_Ice_Object_ice_ping :
    Hydro_lm.client_response -> r_Ice_Object_ice_ping

val to_r_Ice_Object_ice_id :
    Hydro_lm.client_response -> r_Ice_Object_ice_id

val to_r_Ice_Object_ice_ids :
    Hydro_lm.client_response -> r_Ice_Object_ice_ids

val to_r_Ice_Object_ice_isA :
    Hydro_lm.client_response -> r_Ice_Object_ice_isA


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


class pci_Ice_Object :
  Hydro_proxy.proxy_t -> poi_Ice_Object

val pc_Ice_Object :
  Hydro_proxy.proxy_env_t -> pr_Ice_Object -> po_Ice_Object

val unchecked_pr_Ice_Object :
  't Hydro_lm.proxy_reference -> pr_Ice_Object

(*
val checked_pr_Ice_Object :
  't Hydro_lm.proxy_reference -> pr_Ice_Object
 *)

type or_Ice_Object

class type od_Ice_Object =
object
  method facets : unit ref (* dummy method - this data member is unused *)
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

exception O_Ice_Object of o_Ice_Object

class ops_Ice_Object : string -> string array -> oi_Ice_Object
  (** [ops_Ice_Object typeid typeids]:
      Implements the default operations for the assumption that the object
      is compatible with all the types that are passed as argument
      [typeids]. In [typeid] the type ID of the obejct is given.
   *)

class skel_Ice_Object : oi_Ice_Object

val as_Ice_Object : #Hydro_lm.object_base -> o_Ice_Object
val wrap_Ice_Object : o_Ice_Object -> or_Ice_Object
val unwrap_Ice_Object : or_Ice_Object -> o_Ice_Object

val ofor_Ice_Object : or_Ice_Object -> Hydro_types.value
val toor_Ice_Object : Hydro_types.value -> or_Ice_Object

val dispatch_Ice_Object : 
  oi_Ice_Object -> string -> Hydro_types.value array -> Hydro_types.session ->
  unit

class delegate_od_Ice_Object : #od_Ice_Object -> od_Ice_Object

class delegate_oi_Ice_Object : #oi_Ice_Object -> oi_Ice_Object

val dec_Ice_Object : Hydro_types.slice list -> 
                         unit * Hydro_types.slice list

val enc_Ice_Object : #od_Ice_Object -> Hydro_types.slice list

class mk_od_Ice_Object : unit -> od_Ice_Object

class sliced_od_Ice_Object : #od_Ice_Object -> Hydro_lm.sliced_base

class mk_Ice_Object : #od_Ice_Object -> o_Ice_Object

class restore_Ice_Object : Hydro_types.sliced_value -> o_Ice_Object

val defterm_Ice_Object : Hydro_types.hintf Lazy.t

val class_defterm_Ice_Object : Hydro_types.hclass Lazy.t

val fill_system : Hydro_types.system -> unit

