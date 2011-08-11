(* $Id$ *)

(** Object adapters *)

(** Object adapters are registries of objects that are known to the
    server
 *)

(* TODO: facets *)

(* TODO: UUID generator *)

open Hydro_types


exception Identity_exists of identity
exception Object_exists of Hydro_lm.interface_base 


class type object_adapter_t =
object
  method add : identity -> Hydro_lm.interface_base -> unit
    (** [add id obj]: Adds the object [obj] under the identity [id] to the
        adapter. It is an error if the identity is already member of the
        adapter, or if the object is already member. In the first case
        [Identity_exists] will be raised, and in the latter case
        [Object_exists].

        The object implementation is registered as the default facet.
     *)

  method mem_id : identity -> bool
    (** Tests whether the identity is added to the adapter *)

  method mem_obj : Hydro_lm.interface_base -> bool
    (** Tests whether the object is added to the adapter *)

  method remove_id : identity -> unit
    (** Removes the identity from the adapter. It is no error if the
        identity is not member of the adapter.
     *)

  method remove_obj : Hydro_lm.interface_base -> unit
    (** Removes the object from the adapter. It is no error if the
        object is not member of the adapter.
     *)

  method invoke_object : identity -> facet_dispatcher
    (** Looks the object for the given identity up, and returns the
        corresponding facet dispatcher. If the object is not found,
        [Not_found] will be raised. This method is called by the
        Hydro runtime when an operation of an object is invoked.
     *)

  method get_identity : Hydro_lm.interface_base -> identity
    (** Looks the identity for the given object up, or raises
        [Not_found]
     *)

  method adapter_id : string option
  method set_adapter_id : string option -> unit
    (** Get/set the adapter ID. Having such an ID is optional, and only
        required when a locator service (IceGrid) is used.
     *)

  method replica_group_id : string option
  method set_replica_group_id : string option -> unit
    (** Get/set the replica group ID. Having such an ID is optional,
        and only required when the adapter is to become a member of a
        replica group in a locator service (IceGrid).
     *)

end
  (** The class type of object adapters. Note that this is a subtype of
      {!Hydro_types.object_dispatcher}, and can be used in server
      interfaces like {!Hydro_endpoint.Master.bind_adapter}.
   *)

class object_adapter : unit -> object_adapter_t
  (** Implements the object adapter *)

val object_adapter : unit -> object_adapter_t
  (** Creates a new object adapter *)
