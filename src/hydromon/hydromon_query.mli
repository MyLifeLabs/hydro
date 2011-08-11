(** Query a running Hydromon instance for the liveliness of a port *)

type cache
  (** Caches previous queries *)

val create_cache : port:int -> period:int -> unit -> cache
  (** [create_cache port period () ]: Create empty cache that is connected
      with the Hydromon instance [port]. In [port] pass the port number of
      the Hydromon server running on the same machine. (Other configurations
      are not supported.)

      [period] is the number of seconds the monitored objects are checked
      in the background without requiring a new RPC call.
   *)

exception No_result of string
  (** An error prevented the check from being done *)

val check_object : cache:cache -> 
                   monobj:string -> 
                   operation:string ->
                   idempotent:bool ->
                   unit ->
                    int
  (** Checks whether the object [monobj] is alive with respect to the
      given [operation]. Returns the number of recently failed pings,
      i.e. 0 means that the service is good.

      [monobj] is given as stringified proxy. Note that indirect proxies
      are not supported by [check_object].
   *)

val configure_proxy : cache:cache -> 
                      proxy:Hydro_proxy.proxy_t -> 
                      operation:string -> 
                      idempotent:bool ->
                      ?threshold:int ->
                      ?deactivate:bool ->
                      unit ->
                        unit
  (** Configures the proxy such that the liveliness of the server port is
      checked before every call. The string is the name of the operation
      to be used for pinging.
     
      [threshold]: After this number of failed pings the server is considered
      as being down. Default: 1

      [deactivate]: If the server is not reachable, the server port is marked
      as dead in the pool object. Note that this
      also affects other proxies 
      that share this pool even if the other proxies are not configured for
      using Hydromon. Default: false
   *)
