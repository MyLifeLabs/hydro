(** Creating and modifying runtime parameters *)

open Hydro_types

val call_params :
      ?msg_timeout:float ->
      ?destination:Unix.sockaddr ->
      ?context:(string * string) list ->
      unit ->
        call_params
  (** Create a set of call parameters. Omitted values appear as [None] in the
      set:
      - [msg_timeout]: If set to [tmo], the number [tmo] is the maximum
        number of seconds until when a response must have been arrived.
        Otherwise the call times out with a
        [Client_condition `Message_timeout]. If [None], there is no
        such timeout condition. (Only used for twoway calls.)
      - [destination]: For unconnected UDP sockets, this parameter must
        be passed to set the destination of the call. For connected
        sockets this parameter must not be passed.
      - [context]: If set to [ctx], the key/value pairs in [ctx] are to
        be passed as context. If not set a reasonable default is passed.
  *)

val update_call_params :
      ?msg_timeout:float ->
      ?destination:Unix.sockaddr ->
      ?context:(string * string) list ->
      call_params ->
        call_params
  (** Modifies the given call params *)


val client_params :
       ?trans_timeout:float ->
       ?msg_timeout:float ->
       ?idle_timeout:float ->
       ?exception_handler: exn_handler ->
       ?max_proto_minor : int ->
       ?max_enc_minor : int ->
       unit ->
         client_params
   (** Create a set of client parameters:
     - [trans_timeout]: If non-negative, a timeout on the transport
       level is set. This means that connections that block for more
       than this number of seconds time out. In this case, the whole
       client fails, and all pending calls see a
       [Client_condition `Transport_timeout] if it happens in the middle
       of a connection, or [Client_condition `Connect_timeout] if it
       happens in the connect phase. (Default: -1)
     - [msg_timeout]:  If non-negative, the number [tmo] is the maximum
       number of seconds until when a response must have been arrived.
       Otherwise the call times out with a
       [Client_condition `Message_timeout]. This error is non-fatal,
       and only the single call times out. (Only used for twoway calls.)
       (Default: -1)
     - [idle_timeout]: when the client has been idle for this number of
       seconds, it is automatically shut down. "Idle" means that there is
       neither something to send nor a message is expected to be received.
       (Default: -1)
     - The [exception_handler] is invoked for exceptions that cannot be handled
       by the client. The handler is never invoked for [Unbound_exception]s -
       these always fall through to the caller. In any case the client is shut
       down for exceptions that cannot be handled by the client.
       (Default: default_exception_handler)
     - [max_proto_minor] and [max_enc_minor] are the maximum minor versions
       of the server. These values are only used in the datagram case; for
       stream connections they can be obtained directly from the server.
       If not set, minor numbers of 0 are assumed.
    *)

val update_client_params :
       ?trans_timeout:float ->
       ?msg_timeout:float ->
       ?idle_timeout:float ->
       ?exception_handler: exn_handler ->
       ?max_proto_minor : int option ->
       ?max_enc_minor : int option ->
       client_params ->
         client_params
    (** Modifies the given client params *)

val update_client_params_by_endpoint :
       endpoint -> client_params -> client_params
   (** Updates the [trans_timeout], [max_proto_minor] and [max_enc_minor]
       parameters by the values found in the endpoint:
       - [trans_timeout]: is compared with the [timeout] of the endpoint,
         and the smaller value is taken
       - [max_proto_minor] and [max_enc_minor]: taken from the endpoint
    *)

val default_exception_handler : exn_handler
   (** The default handler just prints the exception to stderr *)


val client_params_cmp : client_params -> client_params -> int
   (** Compares two client params objects, and returns a result like
       [Pervasives.compare]. The functions compares the values of
       the methods.
    *)

val server_params :
       ?trans_timeout:float ->
       unit ->
         server_params
   (** Create a set of server parameters:
     - [trans_timeout]: If non-negative, a timeout on the transport
       level is set. This means that connections that block for more
       than this number of seconds time out. In this case, the
       connection is dropped
    *)

val update_server_params :
       ?trans_timeout:float ->
       server_params ->
         server_params
    (** Modifies the given server params *)
