(* $Id$ *)

(** Hydro marshalling *)

open Hydro_types

val encapsulate : (Netbuffer.t -> int -> unit) -> Netbuffer.t -> int -> unit
  (** [encapsulate f b e]: Calls [f b] and prepends the encapsulation
     header. [e] is the minor encoding version. (The major encoding version
     is always 1.)
   *)

val marshal : system -> htype -> bool -> value -> Netbuffer.t -> int -> unit
  (** [marshal sys ht class_flag val nb e]: Encodes the value [val] which is
     supposed to be of type [ht] and appends the string to [nb].
     The [class_flag] must be true if [ht] contains classes.
     Logically, an encapsulation is created, but the encapsulation
     header is not written. [e] is the minor encoding version. 
     (The major encoding version is always 1.)
   *)

val marshal_exn : system -> hexn -> sliced_value -> Netbuffer.t -> int -> unit
  (** [marshal_exn sys hx val nb e]: Encodes the exception value [val] which is
     supposed to be of (static) exn type [hx] and appends the string to [nb].
     Logically, an encapsulation is created, but the encapsulation
     header is not written. [e] is the minor encoding version. 
     (The major encoding version is always 1.)
   *)

val marshal_msg : system -> compression_status -> msg -> int -> int -> 
       msg_buffer
  (** [marshal_msg sys zstat msg p e]: 
     Returns a message buffer (header and bodies) for a message.
     [p] is the minor protocol version. [e] is the minor encoding version
     (which must have been used for all encapsulations). (The major version
     for protocol and encoding is always 1.)
   *)

val max_proto_minor : int
val max_enc_minor : int
  (** Maximum versions we can support *)


(** {2 Low-level} *)

(** Internal & Low-level. These functions are also called by generated
    language-mapping code
 *)

val print_bool : Netbuffer.t -> bool -> unit
val print_byte : Netbuffer.t -> int -> unit
val print_short : Netbuffer.t -> int -> unit
val print_int : Netbuffer.t -> int -> unit
val print_int32 : Netbuffer.t -> int32 -> unit
val print_int64 : Netbuffer.t -> int64 -> unit
val print_float : Netbuffer.t -> float -> unit
val print_double : Netbuffer.t -> float -> unit
val print_size : Netbuffer.t -> int -> unit
val print_string : Netbuffer.t -> string -> unit
  (** Various printers *)

val marshal_sequence : 
       (Netbuffer.t -> 'a -> unit) -> Netbuffer.t -> 'a array -> unit
val marshal_dictionary :
        (Netbuffer.t -> 'a -> unit) -> 
        (Netbuffer.t -> 'b -> unit) -> 
        Netbuffer.t -> ('a * 'b) array -> unit
val marshal_enum : int (* size of enum *) -> Netbuffer.t -> int -> unit


val encoded_int : int -> string
