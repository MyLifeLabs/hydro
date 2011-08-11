(* $Id$ *)

(** Base definitions for the language mapping *)

(** Specific types and functions for the O'Caml language mapping.
    These definitions are used by generated code.
 *)

type +'t proxy_reference
  (** The language-mapped type of the Slice type "T*" where ['t] is the
      language-mapped name of the proxy
   *)

val pr_of_address : 
       Hydro_types.proxy_addr -> [ `Ice_Object ] proxy_reference
  (** Create a reference from a (low-level) proxy address *)

val pr_of_string :
       string -> [ `Ice_Object ] proxy_reference
  (** Create a reference from a stringified proxy address *)


type client_response =
    Hydro_endpoint.Client.response
  (** The client response type *)


class type ['response] call_suspension_t =
object
  method scall : 'response
    (** Do the RPC call synchronously. Note that this implies a
        [Unixqueue.run] of the event system of the proxy environment
      *)

  method acall : ('response -> unit) -> unit
    (** Do the RPC call asynchronously. The callback function is
        invoked when the response arrives. Note that this function
        does nothing unless the user calls [Unixqueue.run] afterwards.
     *)

  method params : Hydro_types.call_params
    (** Get the call parameters *)

  method with_params : Hydro_types.call_params -> 'response call_suspension_t
    (** Get a new call suspension with updated call parameters *)

end
  (** This is an object describing a prepared RPC call that is not yet done.
      It provides methods to trigger the call. The class type parameter
      is later instantiated by the concrete response type used in the
      language mapping.
   *)


class ['response] call_suspension : 
  (Hydro_types.call_params -> (Hydro_endpoint.Client.response -> unit) -> unit) ->
  (Hydro_endpoint.Client.response -> 'response) ->
  Unixqueue.event_system ->
    ['response] call_suspension_t

val call_suspension :
  (Hydro_types.call_params -> (Hydro_endpoint.Client.response -> unit) -> unit) ->
  (Hydro_endpoint.Client.response -> 'response) ->
  Unixqueue.event_system ->
    'response call_suspension_t


class type sliced_base =
object
  method hydro_slices : Hydro_types.slice list
  method hydro_effective_id : string
end
  (** This is a class type for {!Hydro_types.sliced_value} *)


class type object_base =
object
  inherit sliced_base
  method hydro_inflate : string -> Hydro_types.noreturn
  method hydro_invoke_operation : string -> Hydro_types.value array -> Hydro_types.session -> unit
end
  (** This is the full class type of objects. It is a subtype of
     {!Hydro_types.object_value} with respect to the value
     characteristics, and a subtype of
     {!Hydro_types.operation_dispatcher} with respect
     to the operational characteristics.
   *)

class type interface_base =
object
   method hydro_effective_id : string
   method hydro_invoke_operation : string -> Hydro_types.value array -> Hydro_types.session -> unit
end
  (** This is the class type of object interfaces. It is a subtype of
      {!Hydro_lm.object_base} and corresponds to 
      {!Hydro_types.operation_dispatcher}.
   *)


val value_of_object : #object_base -> Hydro_types.value
  (** Create a value from any object *)

val object_of_class_repr : Hydro_types.class_repr ref -> object_base
  (** Get the `Value *)


exception Error of int
  (** An error produced by the generated lang mapping code. The int 
      is a generated number to identify the position in the generated code
   *)

val error : int -> 'a
  (** Raise [Error] *)

exception Invalid_coercion
  (** A coercion cannot be done *)


val create_system : unit -> Hydro_types.system
  (** Create an empty type system *)


(** {1 Special mappings} *)

val seq_to_option : 't array -> 't option
val option_to_seq : 't option -> 't array
  (** Used with [ ["hydro:mapping:option"] ] *)

val dict_to_hashtbl : ('s * 't) list -> ('s,'t) Hashtbl.t
val hashtbl_to_dict : ('s,'t) Hashtbl.t -> ('s * 't) list
  (** Used with [ ["hydro:mapping:hashtbl"] ] *)

module StrMap : Map.S with type key = string
type ('s,'t) strmap = 't StrMap.t constraint 's=string
val dict_to_strmap : (string * 't) list -> (string, 't) strmap
val strmap_to_dict : (string, 't) strmap -> (string * 't) list
  (** Used with [ ["hydro:mapping:strmap"] ] *)

type ('s,'t) complex = Complex.t constraint 's=float constraint 't=float
val pair_to_complex : (float * float) -> Complex.t
val complex_to_pair : Complex.t -> (float * float)
  (** Used with [ ["hydro:mapping:complex"] ] *)

val pair_to_identity : (string * string) -> Hydro_types.identity
val identity_to_pair : Hydro_types.identity -> (string * string)
  (** Used with [ ["hydro:mapping:identity"] ] *)


(** {1 Internals} *)

module Unsafe : sig
  (** Functions that can break the type safety of the language mapping *)

  val wrap_proxy : Hydro_types.proxy_addr -> 't proxy_reference
    (** Create a proxy reference. Note that the correctness of ['t] is
        not checked
     *)

  val unwrap_proxy : 't proxy_reference -> Hydro_types.proxy_addr
    (** Get the proxy specification from a reference *)

  val of_proxy_reference : 't proxy_reference option -> Hydro_types.value
  val to_proxy_reference : Hydro_types.value -> 't proxy_reference option
    (** Conversion from/to value *)
end
