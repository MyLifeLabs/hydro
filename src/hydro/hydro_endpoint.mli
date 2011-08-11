(* $Id$ *)

open Hydro_types

(** Endpoints (clients and servers) *)


module GeneralizedEndpoint : sig
  (** A generalized view on endpoints, in particular to allow bidirectional
      endpoints that play both the client and server role
   *)

  type +'kind t
    (** An endpoint that can be enabled as client (and thus invoke remote
        functions), or as server (and thus provide functions for remote
        invocation), or as both. The argument says what is enabled.
     *)

  type kind =
      [ `ClientRole | `ServerRole ]

end


module Client : sig
  (** This module encapsulates the client interface *)

  type t = [`Client_role] GeneralizedEndpoint.t
    (** A live client *)

  val create : system -> 
               Hydro_connector.client_connector -> 
               Unixqueue.event_system -> 
                 t
    (** Create a new client *)

  val configure : t -> client_params -> unit
   (** Configures the client. There is a default configuration for freshly
      created clients (XXX).
    *)

  type response =
      < condition : [ `Success | client_condition ];
        out_args : value array;
	result : value;
           (* Raises [Client_condition] if [condition] is not [`Success] *)
	peer : Unix.sockaddr option;  (* peer's address *)
        addr : Unix.sockaddr option;  (* my address *)
	mode : op_mode;   (* Repeats the info whether idempotent *)
	client : t option;
	>
    (** A friendly version of response messages *)

  val twoway_call : 
        ?facet:string ->
         t -> identity -> hintf -> string -> call_params ->
         value array ->
         (response -> unit) ->
           unit
   (** Starts a twoway call *)

(* TODO:
   add_batch_call
 *)

  val queued_requests : t -> int
    (** Number of requests in the queue (that are not yet responded) *)

  val is_up : t -> bool
    (** Whether the client is up (accepts new requests) *)

  val is_idle : t -> bool
    (** Whether the client is up and idle *)

  val client_id : t -> int
    (** Returns an identifier for the client *)

  val shutdown : ?ondown:(unit->unit) -> t -> unit
    (** Shut the client gracefully down (by sending a close connection 
        message). This is an asynchronous shutdown. When it is done,
        the function [ondown] will be called.
     *)

  val shutdown_when_idle : t -> unit
    (** Shutdown gracefully the next time the client becomes idle *)

  val abort : t -> unit
    (** Closes the connection immediately (no close connection message). *)

  val graceful : t -> bool
    (** The graceful flag is initially true, but set to false when a fatal
        error or a non-graceful shutdown happens
     *)

  val pool_connect : t -> (unit -> unit) -> unit
    (** Used by [Hydro_proxy.pool]: The callback function is invoked when
        [is_up] becomes false
     *)
end


module Server : sig
  type t = [`Server_role] GeneralizedEndpoint.t
    (** A server endpoint for an existing connection *)

  val create : ?onabort:(t -> unit) ->
               system -> 
               endpoint_type -> 
               descriptor ->
               server_params ->
               Unixqueue.event_system -> 
                 t
    (** Create a new server *)

  (* TODO: socket-less servers, for local transport *)

  val server_id : t -> int
    (** Returns an identifier for the server *)

  val endpoint : t -> endpoint
    (** Returns the endpoint name (for creating proxies) *)

  val adapters : t -> object_dispatcher list
    (** Returns the bound adapters (object dispatchers) *)

  val bind_adapter : t -> object_dispatcher -> unit
    (** Binds an object dispatcher *)

  val unbind_adapter : t -> object_dispatcher -> unit
    (** Removes the object dispatcher binding *)

  val shutdown : t -> unit
    (** Shut the server gracefully down (by sending a close connection message) *)

  (* TODO: shutdown after a certain response is sent *)


  val abort : t -> unit
    (** Closes the connection immediately (no close connection message). *)


  (** Note that binding an adapter does not include the registration at
      the location service. See {!Hydro_locator.register_adapters} for
      this
   *)
end

module Master : sig
  type t

  val create : system -> 
               Hydro_connector.master_connector ->
               server_params ->
               Unixqueue.event_system -> 
                 t

  val adapters : t -> object_dispatcher list
    (** Returns the bound adapters (object dispatchers) *)

  val bind_adapter : t -> object_dispatcher -> unit
    (** Binds an object dispatcher *)

  val unbind_adapter : t -> object_dispatcher -> unit
    (** Removes the object dispatcher binding *)

  val shutdown : t -> unit


  (** Note that binding an adapter does not include the registration at
      the location service. See {!Hydro_locator.register_adapters} for
      this
   *)

end
