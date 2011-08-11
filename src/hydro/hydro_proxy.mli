(** Base proxy functionality *)

(** Proxies

    Proxies represent remote objects locally. A proxy is bound to
    remote objects, and forwards any RPC calls to these. The class
    type [proxy_t] accesses the core of the proxy functionality,
    which is also implemented as class [proxy] (below). The generator
    [hydrogen] creates more specialized proxies that can be much
    easier used. However, these proxies always extend the base class
    defined here.

    Logically, a proxy is bound to one object only. However, it is
    possible to configure proxies so they think this object is
    available on several servers simultaneously ([multiplicity]
    parameter). There are two ways of taking advantage of this:
    - Failover behavior: Normally, always the same server is used.
      In case of a problem, it is failed over to the next server
      in the list ([multiplicity=`Failover]).
    - Concurrent behavior: All given servers are used at the same time
      by maintaining connections to all servers. If servers fail,
      they are (temporarily) not used for new calls
      ([multiplicity=`Concurrent]).

    The connection management is fully automatic. The user needs not
    to indicate when to start a new connection, and for how long to
    use it. In case of failing servers it is possible, however, that
    individual RPC calls fail because of this. It is not tried to
    hide this fact. The user is free to repeat the call if it is
    semantically allowed.

    To be exact, proxies do not keep the connections. This is the task
    of the pools. It is supported that a proxy shares connections to
    servers with other proxies, but proxy-private connections are also
    possible.

    It is possible to tell the pools that certain hosts or certain
    services are currently unavailable - before proxies try to use
    them and run into problems. This is called temporary deactivation.
    Use the [deactivate_host] and [deactivate_port] methods to control
    this.

 *)

(** {1 Types} *)

open Hydro_types

type extended_proxy_addr =
    < id : identity;
      facet : string option;
      mode : proxy_mode;
      secure : bool;
      parameters : extended_proxy_parameters
    >
  (** A proxy address where the [parameters] can also specify connectors
      directly
   *)


and extended_proxy_parameters =
    [ proxy_parameters
    | `Connectors of
	  (Hydro_connector.client_connector * client_params) list
    ]

type multiplicity =
    [ `Failover | `Concurrent of int ]
  (** See below *)


type shared_or_private =
    [ `Shared
    | `Private of < >
    ]
  (** Internally used *)


type domain_resolver =
    Unixqueue.unix_event_system ->
    string ->
    (Unix.inet_addr option -> unit) ->
      unit
        (** A domain resolver is a function [r] called as [r esys name reply].
          * As [name] the domain name to resolve is passed. The resolver must
          * finally call [reply] with either the resolved address or
          * with [None], indicating an error in the latter case.
          * The event system [esys] can be used to carry out the resolution
          * process in an asynchronous way, but this is optional.
          *
          * Only 1:1 resolution is supported, 1:n resolution not.
         *)


class type proxy_conf_t =
object
  method shared_connections : bool
    (** Whether connections to servers are shared with other proxies ([true]),
        or are private to every proxy ([false]).
     *)

  method multiplicity : multiplicity
    (** How many connections to servers are held by every proxy.
        If [`Failover], there is only one connection to a server at a time,
        and alternate server endpoints are considered as alternate
        connections if the current one breaks. If [`Concurrent n],
        where [n>=1], to every server endpoint [n] concurrent connections
        are created.
     *)

  method max_reconnections : int
    (** How often an endpoint can be reconnected after the first attempt,
        before the endpoint is given up.
     *)

  method deactivation_period : float
    (** For how long endpoints are marked as "dead" when connections repeatedly
        break. In seconds. If 0, there is no deactivation. If negative,
        deactivation lasts forever.
     *)

  method resolution_period : float
    (** How often endpoints are resolved. In seconds. If 0, for every call
        a new resolution is performed. If negative,
        resolutions are kept forever, and are never repeated.
     *)

  method context : (string * string) list
    (** The context of key/value parameters sent with every request.
        This list initializes the context held in the proxy objects.
     *)

end
  (** Configures proxies *)

class type proxy_env_t =
object
  method event_system : Unixqueue.event_system
    (** The event system *)

  method system : system
    (** The ICE type system *)

  method proxy_resolver : proxy_resolver_t
    (** The resolver used to map names to addresses *)

  method client_pool : pool_t
    (** The pool of clients with active connections to servers *)

  method default_proxy_conf : proxy_conf_t
    (** The default configuration for new proxies (can be changed per proxy) *)
end
  (** The environment of proxies, usually shared by all proxies in the program
   *)

and proxy_resolver_t =
object
  method resolve :
           extended_proxy_addr ->
           ((Hydro_connector.client_connector *
	       client_params) list Lazy.t -> unit) ->
           proxy_env_t ->
             unit
    (** [resolve epa pass_result pe]: Resolves the names in [pa] asynchronously,
        and calls [pass_result l] with the lazy list [l] of connectors and
        client parameters.
     *)
end

and proxy_t =
object
  method hydro_env : proxy_env_t
    (** Return the proxy environment *)

  method hydro_id : identity
    (** Return the identity of the object the proxy represents *)

  method hydro_facet : string option
    (** Return the facet, if used *)

  method hydro_twoway_call :
           hintf -> string -> value array -> call_params ->
           (Hydro_endpoint.Client.response -> unit) ->
             unit
    (** [hydro_twoway_call hi name args params pass_result]:
        Perform a twoway call to the function [name] of interface [hi],
        and pass the arguments [args] as input. The [params] control the
        way the call is done. Once a result or error is available,
        [pass_result] is invoked with the response.

        This method takes care of resolving symbolic endpoints, and
        manages the connections held to possible servers. If connections
        cannot be established, alternate servers are tried. Idempotent
        function calls are repeated if connections break.
     *)

  method hydro_set_proxy_conf : proxy_conf_t -> unit
    (** Sets a new proxy configuration. This is best done before the first
        call is made. If done later, the current resolution is invalidated.
     *)

  method hydro_proxy_conf : proxy_conf_t
    (** Return the current proxy configuration *)

  method hydro_set_monitor : (proxy_t -> managed_client_t -> bool) -> unit
    (** [hydro_set_monitor mon]: The function [mon] is called whenever
        a call is to be done. It is called as [mon proxy mc]. If it returns
        [true], the call will be done using managed client [mc]. If it
        returns [false], this client is considered as dead, and is skipped.
        The function [mon] is free to deactivate the host or the port
        in the pool.
     *)

  method hydro_shutdown : unit -> unit
    (** Marks the proxy as "down". Currently, this does not have any effect
        on calls that are pending, and on active connections. New calls are
        refused, however. (In order to shut down connections, talk to the
        pool!)
     *)

  method hydro_reset : unit -> unit
    (** Resets the internal proxy state *)
end
   (** This is the base class type of every proxy. Proxies usually have
       more methods than just this, however.
    *)


and pool_t =
object
  method set_esys : Unixqueue.event_system -> unit
    (** Tell the pool the event system. Can only be done once *)

  method request_clients : system ->
                           shared_or_private ->
                           int ->     (* multiplicity *)
                           ( Hydro_connector.client_connector *
			     client_params
			   ) list ->
                                managed_client_t list
    (** Request clients from the pool *)

  method deactivate_host : Unix.inet_addr -> float -> unit
    (** Deactivate the host for this number of seconds *)

  method deactivate_port : network_port -> float -> unit
    (** Deactivate the port for this number of seconds *)

  method trigger_shutdown : unit -> unit
    (** Shuts all pooled clients down. Note that this is an asynchronous
        shutdown, i.e. the shutdown actions are performed first when the
        event system is run!

        After the shutdown, the pool is still usable, and new connections
        can again be created.
      *)

  method shutdown : unit -> unit
    (** Shuts all pooled clients down. This version is synchronous, and
        runs the event queue until all clients are down.

        Note that it is possible that other pending events will cause
        attempts to create new connections while the shutdown is in progress.
        The pool revokes such attempts, such that it is ensured that all
        connections are down when this method returns.

        After the shutdown, the pool is still usable, and new connections
        can again be created.
     *)

  method abort : unit -> unit
    (** Aborts all pooled clients (immediately) *)

  method reset : unit -> unit
    (** Shuts all clients down, and forget any deactivations *)

  method is_available : Unix.inet_addr -> network_port option ->
                        shared_or_private -> float -> bool
    (** This method is called by the pool implementation to check for
        the availability of a service. It is exposed in the public
        interface so it can be overridden by specialized pool
        implementations.
     *)

end
  (** A pool keeps connections from clients to servers *)

and managed_client_t =
object
  method host_port : (Unix.inet_addr * int) option
    (** Return the host and port of the server this managed client is
        bound to.
     *)

  method client : Hydro_endpoint.Client.t
    (** Return the current incarnation of the client *)

  method client_incarnation : int
    (** Return the incarnation number of the [client] just returned *)

  method available : float -> bool
    (** [available t]: Says whether the client is available for new calls
        at timestamp [t]
     *)

  method deactivate : float -> unit
    (** Deactivate this client for the passed number of seconds *)

  method unused_since : float option
    (** If this object is currently unused (i.e. no connection is held),
        this method tells since when
     *)

  method record_error : float -> unit
    (** Record an error situation. The float is the number of seconds the
        situation is to be remembered
     *)

  method clear_error : unit -> unit
    (** Clear all error state *)

  method error_count : int
    (** Return the number of recorded errors *)

  method trigger_shutdown : ?ondown:(unit->unit) -> unit -> unit
    (** Shut the client down. When it is down, the [ondown] callback is
        invoked
      *)

  method abort : unit -> unit
    (** Abort the client (immediate shutdown *)

end
  (** This is an internally used object that encapsulates clients *)


(** {1 Implementation} *)

val proxy_conf :
      ?shared_connections:bool ->
      ?multiplicity:multiplicity ->
      ?max_reconnections:int ->
      ?deactivation_period:float ->
      ?resolution_period:float ->
      ?context:(string * string) list ->
      unit ->
        proxy_conf_t
  (** Create a fresh proxy configuration. Unspecified parameters are set
      to an implementation-defined default value.
   *)

val modify_proxy_conf :
      ?shared_connections:bool ->
      ?multiplicity:multiplicity ->
      ?max_reconnections:int ->
      ?deactivation_period:float ->
      ?resolution_period:float ->
      ?context:(string * string) list ->
      proxy_conf_t ->
        proxy_conf_t
  (** Create a proxy configuration that modifies the values found in the
      input configuration.
   *)

val proxy_resolver :
       ?domain_resolver:domain_resolver ->
       client_params -> proxy_resolver_t
  (** The default resolver looks for available endpoints and returns them
      in the order they occur in the address. If possible, DNS lookups
      are done.

      The passed client params serve as the base params. Depending on the
      endpoint, the params are modified:
      - If the endpoint specifies a timeout, the [trans_timeout] of the
        client is set to the minimum of the endpoint timeout and the
        found [trans_timeout]
      - If the endpoint specifies protocol and encoding numbers, these
        override the found numbers.

     This resolver does not support indirect proxies.

   *)

val shuffle :
      Random.State.t ->
      proxy_resolver_t ->
        proxy_resolver_t
  (** [shuffle rnd (proxy_resolver params)]: shuffles the endpoints so that
      they are in random order
   *)

val proxy :
        env:proxy_env_t ->
        addr:extended_proxy_addr ->
        unit ->
          proxy_t
  (** Create a proxy for this environment and this proxy address *)


val pool :
        unit ->
          pool_t
  (** Default implementation of a pool. *)


(** {1 Delegations} *)

class proxy_delegation : proxy_t -> proxy_t
class proxy_env_delegation : proxy_env_t -> proxy_env_t
class proxy_resolver_delegation : proxy_resolver_t -> proxy_resolver_t
class pool_delegation : pool_t -> pool_t
