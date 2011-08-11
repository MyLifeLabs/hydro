(* $Id$ *)

(** Hydro unmarshalling *)

open Hydro_types

val decapsulate : encap_buffer -> encap_buffer
  (** [decapsulate (nb,pos,len)]: Interprets the [len] bytes at [pos]
     in [nb] as encapsulation, and returns its contents also in the
     format [(nb',pos',len')]. [nb=nb'] is allowed.
   *)

val unmarshal : system -> htype -> bool -> encap_buffer -> value
  (** [unmarshal sys ht class_flag eb]: Decodes the value at [pos] with
     length [len] in [nb] which is supposed to be of type [ht] and returns
     it.

     The [class_flag] must be true if [ht] contains classes.

     Logically, a whole encapsulation body is decoded (incl. the appendix
     for class values).
   *)

val unmarshal_exn : system -> encap_buffer -> sliced_value
  (** [unmarshal_exn sys (nb,pos,len) ]: Decodes the exception value
     at [pos] with length [len] in [nb] and returns it. The exception
     can be any exception listed in [sys].

     Logically, a whole encapsulation body is decoded (incl. the appendix
     for class values).
   *)

val unmarshal_msg : system -> msg_header -> encap_buffer -> msg
  (** Returns a message for a message buffer (header and body) *)


(** {2 Low-level} *)

(** Internal & Low-level. These functions are also called by generated
    language-mapping code
 *)

val read_bool : string -> int -> bool
val read_byte : string -> int -> int
val read_short : string -> int -> int
val read_int : string -> int -> int
val read_int32 : string -> int -> int32
val read_int64 : string -> int -> int64
val read_float : string -> int -> float
val read_double : string -> int -> float

val unmarshal_bool : string -> int ref -> int -> bool
val unmarshal_byte : string -> int ref -> int -> int
val unmarshal_short : string -> int ref -> int -> int
val unmarshal_int : string -> int ref -> int -> int
val unmarshal_int32 : string -> int ref -> int -> int32
val unmarshal_int64 : string -> int ref -> int -> int64
val unmarshal_float : string -> int ref -> int -> float
val unmarshal_double : string -> int ref -> int -> float
val unmarshal_string : string -> int ref -> int -> string

val unmarshal_sequence : (string -> int ref -> int -> 'a) ->
                         string -> int ref -> int -> 'a array
val unmarshal_dictionary : (string -> int ref -> int -> 'a) ->
                           (string -> int ref -> int -> 'b) ->
                           string -> int ref -> int -> ('a * 'b) array
val unmarshal_enum : int (* size of enum *) -> string -> int ref -> int -> int
