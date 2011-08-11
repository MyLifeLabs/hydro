(* $Id$ *)

(** Basic type and exception definitions *)

(** {1 Exceptions} *)

(** These are the base exceptions of the runtime. Note that user code
    usually does not get them directly because they are wrapped into
    the [Client_condition] exception. E.g. user code could get
    [Client_condition(`Connect_error(Proxy_error(`NoEndpointIsReachable)))].

    For technical reasons, [Client_condition] is defined below.
 *)

open Hydro_prelim


type protocol_violation =
    [ `MessageFormatViolation of string
    | `CompressionNotSupported
    | `BadMessageType
    ]

exception Protocol_violation of protocol_violation
  (** A violation of the protocol has been detected:
      - [`MessageFormatViolation reason]: The data received over the wire
        cannot be even split into messages. [reason] explains why.
      - [`CompressionNotSupported]: A compressed message was received
        although Hydro announces that it does not support compression.
      - [`BadMessageType]: XXX
   *)

exception Marshal_error of string
  (** A value cannot be marshalled, i.e. converted into the string format
      that is sent over the wire. The argument explains the reason.
   *)

exception Unmarshal_error of string
  (** A received message cannot be decoded into a value.
      The argument explains the reason.
   *)

type limitation =
    [ `UnsupportedEncodingVersion
    | `UnsupportedProtocolVersion
    | `UnsupportedEndpointType of int
    ]

exception Limitation of limitation
  (** A Hydro limitation was hit:
      - [`UnsupportedEncodingVersion]: Hydro only understands version 1.0
      - [`UnsupportedProtocolVersion]: Hydro only understands version 1.0
   *)


type proxy_error =
    [ `NoCallableEndpointFound
    | `NoEndpointIsReachable
    | `NoLocatorIsReachable
    | `ProxyIsDown
    ]

exception Proxy_error of proxy_error
  (** The proxy is unable to fulfill the request:
      - [`NoCallableEndpointFound]: After the endpoint resolution procedure
        no endpoint turns out to be usable. Note that also DNS failures
        can lead to this error.
      - [`NoEndpointIsReachable]: None of the resolved endpoints is
        actually reachable.
      - [`NoLocatorIsReachable]: No locator (i.e. IceGrid node) is
        actually reachable, and the resolution procedure fails because of 
        this.
      - [`ProxyIsDown]: The proxy has been shut down
   *)

exception Domain_not_found of string
  (** A DNS name cannot be resolved. The argument is the name *)

exception Unbound_exception of exn
  (** An exception that has not to be caught by a exception handler
      built into Hydro. Such exceptions always fall through to the
      user code.
    *)

exception Unimplemented_operation of string
  (** This operation was called, but the actual definition is missing *)

(** See also the exception [Client_condition] below! *)


(** {1 Values} *)

(** This defines [value], the abstract (non-mapped) representation of
    values that can be sent over the wire. [value] is mostly internally
    used, because [hydrogen] generates a user-friendlier representation
    of values that uses directly OCaml types.
 *)


type noreturn
  (** A type without value. May be used to indicate that functions or methods
      never return normally to the caller.
   *)

type value =
    VNothing
	(* The fictive value for `Void types *)
  | VBool of bool
  | VByte of int
  | VShort of int
  | VInt of int
  | VInt32 of int32
  | VLong of int64
  | VFloat of float
  | VDouble of float
  | VString of string   
      (* the UTF8-encoded string *)
  | VSequence of value array
      (* the elements of the sequence in order *)
  | VByteseq of string
      (* `Sequence `Byte, represented as ocaml string *)
  | VDictionary of (value * value) array
      (* the dictionary pairs *)
  | VEnum of int   
      (* the number is the position in the declaration *)
  | VStruct of value array
      (* the elements of the structure in order *)
  | VNull
  | VClass of class_repr ref
      (* This is a reference so we can patch the value during unmarshalling
       *)
  | VProxy of proxy_addr
  | VDirectWriter of (Netbuffer.t -> unit)
  | VDirectMapping of exn (** *)
  (** This is mostly clear: e.g. [VLong n] represents a value that is declared
      as "long" in the Ice IDL file. Some remarks:
      - [VNothing] is the fictive value for [`Void] types (think of [()] in
        OCaml)
      - There are two representations for "int": [`Int n] and [`Int32 n],
        so the user can select which representation to prefer
      - [VFloat f] means that [f] is only single-precision, although the
        OCaml [float] type corresponds to double-precision floats.
      - [VByteseq s] is used for values of  [sequence<byte>] Ice type.
      - [VEnum k]: The number [k] is the position in the declaration.
      - [VClass cls_ref]: We use a reference as argument to simply the
        unmarshalling algorithm. User code should never see [`Placeholder]
        inside this reference, but only [`Value].
      - [VDirectWriter f]: The function [f] is called to write out the
        byte encoding of the value.
      - [VDirectMapping]: The value is given in the language-mapped
        representation, wrapped into a special, generated exception
   *)


and class_repr =
    [ `Placeholder of int32
    | `Value of object_value
    ]

and sliced_value =
    < hydro_slices : slice list;
      hydro_effective_id : string;
     >
  (** Sliced values are used to marshal exceptions and object instances.
      For these there is an inheritance hierarchy.
      - [hydro_slices]: These are the slices in base-to-derived order,
        i.e. the root slice for the root of the inheritance hierarchy
        comes first, and the most derived slice comes last.
      - [hydro_effective_id]:  The effective type ID of the class or 
        exception. This is the most derived ID the local type system 
        knows = the latest element in [hydro_slices] with a decoded slice.

      This type also exists as class type {!Hydro_lm.sliced_base}.
   *)


and object_value =
    < (* This is an enhanced sliced_value *)
      hydro_slices : slice list;
      hydro_effective_id : string;
      hydro_inflate : string -> noreturn;
      hydro_invoke_operation : string -> value array -> session -> unit
    >
  (** Object values are subtypes of [sliced_value]. Additional
      methods:
      - [hydro_inflate id]: Used in the language-mapping layer to restore
        the full object type for the [object_value]. This is done by
        calling [hydro_inflate] with the class [id] of the desired class.
        The method in turn raises a special exception [E obj]
        that has to be caught by the caller in order to get [obj].
        The argument [obj] is the object itself, but with a richer type
        that corresponds to [id].
      - [hydro_invoke_operation name args sess]: invoke the operation
        [name] (always passed in lowercase letters) with arguments [args]
        and session [sess].

      Basically this means that an [object_value] is a [sliced_value]
      that can be recovered as rich class type by the language mapping layer.

      For an [object_value] that has callable operations, use 
      the class type {!Hydro_lm.object_base}.
   *)


and slice =
    [ `Decoded of string * value array
    | `Opaque of string * string
    ]
  (** It is possible that the ID of a slice is known or not. In the first
      case Hydro represents it as [`Decoded(id,values)] where [id] is the
      type ID of the slice, and [values] are the decoded values in order.
      If Hydro does not know the type ID, the slice is represented as
      [`Opaque(id,values)], and the values remain unmarshalled. It is still
      possible to forward an opaque value to another Ice node.
   *)


and proxy_addr =
    (* Only non-Null proxies *)
    < id : identity;
      facet : string option;
      mode : proxy_mode;
      secure : bool;
      parameters : proxy_parameters
    >
  (** The address of a remote object (for creating a proxy):
      - [id] is the identity of the remote object
      - [facet] is the facet if any
      - [mode] is the mode of the proxy
      - [secure] may demand that only SSL endpoints are chose in order
        to access the remote object
      - [parameters] denotes the endpoint(s) where the object is reachable
        in a transport-dependent form
   *)

and proxy_mode = 
    [ `Twoway | `Oneway | `Batch_oneway | `Datagram | `Batch_datagram ]

and proxy_parameters =
    [ `Endpoints of endpoint array
    | `Well_known
    | `Adapter of string
    ]
    (** Where to find the Internet port(s) of the service behind the proxy:
        - [`Endpoints e]: The proxy is a direct proxy with the non-empty
          array of endpoints [e]
        - [`Well_known]: The proxy is an indirect proxy referring to a
          well known object
        - [`Adapter s]: The proxy is an indirect proxy referring to an
          adapter [s] (the string is non-empty)
     *)


and endpoint =
    [ `TCP of tcp_endpoint
    | `UDP of udp_endpoint
    | `SSL of ssl_endpoint
    | `Unknown of int * string
    ]

and endpoint_type =
    [ `TCP | `UDP | `SSL | `Unknown of int ]

and tcp_endpoint =
    < host : string; 
      port : int;
      timeout : int32;   (* milliseconds! *)
      compress : bool;
    >

and udp_endpoint =
    < host : string;
      port : int;
      proto_major : int;
      proto_minor : int;
      enc_major : int;
      enc_minor : int;
      compress : bool;
    >

and ssl_endpoint = tcp_endpoint

and identity =
    < name : string;
      category : string
    >
  (** The identity of a remote object *)


(** {1 Type system} *)

and htype =
    TVoid
  | TBool
  | TByte
  | TShort
  | TInt     (* Prefer int as O'Caml type *)
  | TInt32   (* Prefer int32 as O'Caml type *)
  | TLong
  | TFloat
  | TDouble
  | TString
  | TByteseq
  | TEnum of string array
  | TStruct of (string * htype) array
  | TSequence of htype
  | TDictionary of htype * htype
  | TProxy of string   (* name of the interface in [system] *)
  | TClass of string   (* name of the class in [system] *)
  | TDirectMapping of 
      htype * (Netbuffer.t -> exn -> unit) * (string -> int ref -> int -> exn)
      (* (ht, marshaller, unmarshaller) *)

and hexn =
    < name : string;             (* Absolute name *)
      super : hexn option;       (* The next exception in the hierarchy *)
      elements : (string * htype) array;
    >

and hintf =
    < name : string;             (* Absolute name *)
      super : hintf list;
      elements : hfunction list;
    >

and hfunction =
    < name : string;             (* relative name *)
      mode : op_mode;
      in_args : (string * htype) array;
      in_classes : bool;  (* Whether in_args type has classes *)
      out_args : (string * htype) array;
      result : htype;
        (* Unclear whether further properties are needed here *)
      out_classes : bool; (* Whether out_args or result type has classes *)
    >

and hclass =
    < name : string;             (* Absolute name *)
      super : hclass option;
      elements : (string * htype) array;
    >

and system =
    < types : htype CiHashtbl.t;
        (* Maps absolute type ID to type definition *)

      exceptions : hexn CiHashtbl.t;
        (* Maps absolute exception ID to exception definition *)

      interfaces : hintf CiHashtbl.t;
        (* Maps... *)

      classes : hclass CiHashtbl.t;
        (* Maps... *)

      ctors : (sliced_value -> object_value) CiHashtbl.t;
        (* Maps absolute class type IDs to object constructors *)
    >


and op_mode =
    [ `Normal | `Idempotent | `Nonmutating ]
  (* Notes on `Idempotent and `Nonmutating: It is unclear what the difference
     is. Old versions of Ice knew only `Nonmutating, and later the idempotent
     keyword was introduced. There is probably a semantic difference, because
     the ICE runtime does not treat both modes as equal, but:
     - An ICE server accepts `Nonmutating client invocations where `Idempotent
       is declared in the slice def
     - The reverse is not true: an operation declared as `Nonmutating in the
       server slice file must be invoked as `Nonmutating.

     Hydro does not have a backwards-compatibility problem (no old clients
     that did not know about idempotent). So servers generally treat `Idempotent
     and `Nonmutating as equivalent.
   *)


(** {1 Messages} *)

and msg_type =
    [ `Request 
    | `Batch_request
    | `Reply
    | `Validate_connection 
    | `Close_connection
    ]

and compression_status =
    [ `Compression_unsupported | `Uncompressed | `Compressed ]

and msg_header =
    < proto_major : int;
      proto_minor : int;
      enc_major : int;
      enc_minor : int;
      msg_type : msg_type;
      compression : compression_status;
      body_size : int;   (* message size w/o header *)
    >

and encap_buffer =
    { encap_buf : Netbuffer.t;
      encap_pos : int;
      encap_len : int;
      encap_enc_minor : int;
    }
  (* The substring of the [encap_buf] starting at [encap_pos] with length
     [encap_len] is to be encapsulated with major encoding version 1 and
     minor encoding version [encap_enc_minor].

     Currently, only the minor version of 0 is defined.
   *)

and msg_buffer =
    msg_header * encap_buffer list
  (* A message header with message bodies in buffers. For every buffer,
     the position and length of the substring is specified.
   *)

and msg =
    [ `Request of request_msg
    | `Batch_request of batch_request_msg list
    | `Reply of reply_msg
    | `Validate_connection
    | `Close_connection
    ]

and request_msg =
    < request_id : int32;
      id : identity;
      facet : string option;
      operation : string;
      mode : op_mode;
      context : (string * string) array;
      params : encap_buffer
        (* The parameters as marshalled string, prepared for encapsulation *)
    >

and batch_request_msg =
    < id : identity;
      facet : string option;
      operation : string;
      mode : op_mode;
      context : (string * string) array;
      params : encap_buffer
        (* The parameters as marshalled string, prepared for encapsulation *)
    >


and reply_msg =
    < request_id : int32;
      result : result;
    >

and result =
    [ `Success of encap_buffer
    | `User_exception of encap_buffer
    | `Object_does_not_exist of identity * string option * string
    | `Facet_does_not_exist of identity * string option * string
    | `Operation_does_not_exist of identity * string option * string
    | `Unknown_local_exception of string
    | `Unknown_user_exception of string
    | `Unknown_exception of string
    ]

(** {1 Parameters} *)

(** See the [Hydro_params] module for creating and modifying these
    objects
 *)

and call_params =
    < msg_timeout : float option;
      destination : Unix.sockaddr option;
      context : (string * string) list option;
    >
  (** Parameters for a single call:
      - [msg_timeout]: If [Some tmo], the number [tmo] is the maximum
        number of seconds until when a response must have been arrived.
        Otherwise the call times out with a
        [Client_condition `Message_timeout]. If [None], there is no
        such timeout condition. (Only used for twoway calls.)
      - [destination]: For unconnected UDP sockets, this parameter must
        be [Some addr] to set the destination of the call. For connected
        sockets this parameter must be [None].
      - [context]: If [Some ctx], the key/value pairs in [ctx] are to
        be passed as context. If [None] a reasonable default is passed.
   *)
  (* Later: add compression flag *)

and exn_handler =
    < handle : exn -> unit >
  (** An exception handler encapsulated as object *)

and client_params =
    < trans_timeout : float;
      msg_timeout : float;
      idle_timeout : float;
      exception_handler : exn_handler;
      max_proto_minor : int option;
      max_enc_minor : int option;
    >
  (** Parameters for clients:
     - [trans_timeout]: If non-negative, a timeout on the transport
       level is set. This means that connections that block for more
       than this number of seconds time out. In this case, the whole
       client fails, and all pending calls see a
       [Client_condition `Transport_timeout] if it happens in the middle
       of a connection, or [Client_condition `Connect_timeout] if it
       happens in the connect phase.
     - [msg_timeout]:  If non-negative, the number [tmo] is the maximum
       number of seconds until when a response must have been arrived.
       Otherwise the call times out with a
       [Client_condition `Message_timeout]. This error is non-fatal,
       and only the single call times out. (Only used for twoway calls.)
     - [idle_timeout]: when the client has been idle for this number of
       seconds, it is automatically shut down. "Idle" means that there is
       neither something to send nor a message is expected to be received.
     - The [exception_handler] is invoked for exceptions that cannot be handled
       by the client. The handler is never invoked for [Unbound_exception]s -
       these always fall through to the caller. In any case the client is shut
       down for exceptions that cannot be handled by the client.
     - [max_proto_minor] and [max_enc_minor] are the maximum minor versions
       of the server. These values are only used in the datagram case; for
       stream connections they can be obtained directly from the server.
       If not set, minor numbers of 0 are assumed.
   *)
  (* Later: add compression flag *)

and server_params =
    < trans_timeout : float;
    >

(** {1 Serving} *)

and server_ops =
    < endpoint : endpoint;
      shutdown_connection : unit -> unit;
      abort_connection : unit -> unit;
      event_system : Unixqueue.event_system;
      system : system;
      server_params : server_params;
      server_id : int;
    >
  (** - [endpoint] returns the endpoint name where this server is reachable
        by remote proxies
      - [shutdown_connection] initiates the shutdown of the connection
        for connection-oriented servers (nothing is done for connection-less
        servers)
      - [abort_connection] closes the connection immediately
        for connection-oriented servers (nothing is done for connection-less
        servers)
      - [event_system] is the Unixqueue the server is using
      - [system] is the type system
      - [server_params] are the server parameters
      - [server_id] identifies the connection for connection-oriented
        servers, otherwise just the server
   *)

and session =
    < is_responded : bool;
      response : reply_msg option;
      emit_result : value -> value array -> unit;
      emit_user_exception : sliced_value -> unit;
      emit_unknown_local_exception : string -> unit;
      emit_unknown_user_exception : string -> unit;
      emit_unknown_exception : string -> unit;
      request_id : int32;
      context : (string * string) list;
      server_ops : server_ops;
    >
  (** - [is_responded] is [true] when a response has been sent, or when
        a response can no longer be sent, and [false] when a response is
        still expected
      - [response] is [Some r] when the response [r] is emitted, and
        [None] otherwise
      - [emit_result r outvars] emits the main result [r] and the output
        parameters [outvars]
      - [emit_user_exception sv] emits the user exception contained in [sv]
      - [emit_unknown_local_exception s] emit the "unknown local exception",
        accompanied by the string [s]
      - [emit_unknown_user_exception s] emit the "unknown user exception",
        accompanied by the string [s]
      - [emit_unknown_exception s] emit the "unknown exception",
        accompanied by the string [s]
      - [request_id] is the request ID sent in the request. Fails if
        non applicable (e.g. oneway calls)
      - [context] is the list of key/value pairs sent as context in
        the request message
      - [server_ops] allows access to server-specific operations
   *)

and operation_dispatcher =
      < hydro_effective_id : string;
        hydro_invoke_operation : string -> (value array -> session -> unit) 
       >
    (** If you get [Not_found] after only passing the operation name to
        [hydro_invoke_operation] the operation does not exist.
     *)

and facet_dispatcher =
      < invoke_facet : string option -> operation_dispatcher >

and object_dispatcher =
      < invoke_object : identity -> facet_dispatcher;
	get_identity : operation_dispatcher -> identity;
        adapter_id : string option;
        replica_group_id : string option;
      >
    (** The [invoke_object] method either returns the next dispatcher, or
        raises [Not_found]. In the scope of a single object dispatcher we
        require that the mapping between identity and facet dispatcher
        is bijective, so there is also [get_identity] for the
        reverse mapping.
     *)

(** {1 Client Exceptions} *)

and client_condition =
    [ `Message_lost of bool
    | `Message_timeout
    | `Transport_timeout
    | `Connect_timeout
    | `Connect_error of exn
    | `Client_is_down
    | `User_exception of sliced_value
    | `Object_does_not_exist of identity * string option * string
    | `Facet_does_not_exist of identity * string option * string
    | `Operation_does_not_exist of identity * string option * string
    | `Unknown_local_exception of string
    | `Unknown_user_exception of string
    | `Unknown_exception of string
    | `Error of exn
    ]

exception Client_condition of client_condition
  (** These conditions may be passed back to the caller of a RPC function:
      - [`Message_lost]: The client was shut down before the response
        arrived. The reason is unrelated to the RPC call that
        sees [`Message_lost] (i.e. it is a follow-up error). The argument
        says whether the request message was tried to send ([true]) or
        not ([false]).
      - [`Message_timeout]: The maximum time for replying the RPC call
        has been exceeded.
      - [`Transport_timeout]: The transport level (TCP level) blocks for
        too long. (This is a fatal error, and the client is shut down.)
      - [`Connect_timeout]: The transport level (TCP level) blocks for
        too long while connecting to a server, or while doing the
        initial handshake. It is ensured that the server has never seen
        the RPC call. (This is a fatal error, and the client is shut down.)
      - [`Connect_error e]: An error happened while connecting to the
        server, or while doing the initial handshake. The exception [e]
        explains the error. It is ensured that the server has never seen
        the RPC call. (This is a fatal error, and the client is shut down.)
      - [`Client_is_down]: A call is tried with a client that is already
        shut down
      - [`User_exception sv]: The remote function raises a user-level
        exception, i.e. an exception that is declared in the Ice IDL.
        The sliced value [sv] represents the exception.
      - [`Object_does_not_exist(id,facet,op)]: Cannot call the function
        [op], optionally in [facet], of the remote object [id] because
        the object is unknown to the server.
      - [`Facet_does_not_exist(id,facet,op)]: Cannot call the function
        [op], optionally in [facet], of the remote object [id] because
        the facet is unknown to the server.
      - [`Operation_does_not_exist(id,facet,op)]: Cannot call the function
        [op], optionally in [facet], of the remote object [id] because
        the function is unknown to the server.
      - [`Unknown_local_exception s]: While performing the remote operation
        the server caught an exception that is declared in the IDL
        as local exception (such exception cannot be marshalled at all).
        The string [s] explains the exception.
      - [`Unknown_user_exception s]: While performing the remote operation
        the server caught an exception that is declared in the IDL
        but not for the operation. The string [s] explains the exception.
      - [`Unknown_exception s]: While performing the remote operation
        the server caught an unknown exception. 
        The string [s] explains the exception.
      - [`Error e]: An error happened that is not one of the above.
        The exception [e] explains the error. 
        (This is a fatal error, and the client is shut down.)
   *)


(** {1 Miscelleneous} *)

type transport_protocol_type = [`Stream | `Datagram]

class type descriptor =
object
   method file_descr : Unix.file_descr
   method proto_type : transport_protocol_type
   method is_master : bool
   method shutdown : unit -> unit
end

type network_port =
    [ `TCP of Unix.inet_addr * int
    | `UDP of Unix.inet_addr * int
    ]

