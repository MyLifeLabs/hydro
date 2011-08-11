(** (Packed) Hydro messages *)

open Hydro_types

val read_msg_header : string -> int -> msg_header
  (* [read_msg_header s pos]: reads the 14 bytes of the message header at
     position [pos] of the string [s], and returns it

     Exceptions:
     - Invalid_argument: if there are less than 14 bytes at [pos]
     - Protocol_violation `MessageFormatViolation
   *)

type msg_reader

val read_msg : msg_header -> msg_reader
  (* [read_msg hd]: Sets up a message reader that reads the message body
     chunk by chunk from a string. If the message is found compressed,
     it is uncompressed while reading.

     Use [read_msg_chunk] to read the chunks.

     Exceptions:
     - [Protocol_violation `CompressionNotSupported]: as long as compression
       is not implemented but requested
   *)

val read_msg_chunk : msg_reader -> string -> int -> int -> Netbuffer.t -> int
  (* [read_msg_chunk mh s pos len b]: read the chunk at [pos] with length
     [len] from [s], and append the decoded bytes to [b]. Return the number
     of decoded bytes.

     Returns the number of read bytes (which is > 0 if len > 0).

     Exceptions:
     - [End_of_file] if the end of the message is reached
     - [Invalid_argument]: if there are less than [len] bytes at [pos]
   *)

val read_done : msg_reader -> bool
  (* Whether all bytes of the message have been read *)

val write_msg_header : string -> int -> msg_header -> unit
  (* [write_msg_header s pos mh]: writes the 14 bytes of the message header
     [mh] at position [pos] into the string [s].

     Exceptions:
     - Invalid_argument: if there are less than 14 bytes at [pos]
   *)

type msg_writer

val write_msg : msg_buffer -> msg_writer
  (* [write_msg mb]: Sets up a message writer that writes the message body
     chunk by chunk into a string. If the message is to be compressed,
     it is written in its compressed form.

     Use [write_msg_chunk] to write the chunks.
   *)

val write_msg_chunk : msg_writer -> string -> int -> int -> int
  (* [write_msg_chunk mw s pos len]: Writes the next bytes of the message
     body. These bytes are written into [s] at [pos], and at most [len]
     bytes are written.

     Returns the number of written bytes (which is > 0 if len > 0).

     Exceptions:
     - [End_of_file] if the end of the message is reached
     - [Invalid_argument]: if there are less than [len] bytes at [pos]
     - [Failure]: if the msg_buffer has less bytes than announces in the
       header
     - [Protocol_violation `CompressionNotSupported]: as long as compression
       is not implemented but requested
   *)

val write_done : msg_writer -> bool
  (* Whether all bytes of the message have been written *)

