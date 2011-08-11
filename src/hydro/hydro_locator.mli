(* $Id$ *)

(** Use locator service for proxy resolution (IceGrid) *)

open Hydro_builtin

val proxy_resolver : 
       ?domain_resolver:Hydro_proxy.domain_resolver ->
      Hydro_types.client_params -> 
      pr_Ice_Locator ->
        Hydro_proxy.proxy_resolver_t
  (** A proxy_resolver that also supports indirect lookups through a 
      locator service like icegridnode. The parameters and the 
      proxy of the locator have to be passed.

      Note that there is no caching of results!
 *)

val get_Ice_Locator_of_port : string -> int -> pr_Ice_Locator
  (** Return a proxy referring to [::Ice::Locator] for a host and a port *)

val get_Ice_Locator_of_string : string -> pr_Ice_Locator
  (** Return a proxy referring to [::Ice::Locator] from the stringified
      representation
   *)



exception Error of string

val register_adapter : ?dynamic_ip:bool ->
                       pr_Ice_Locator -> 
                       string ->
                       string option -> 
                       Hydro_types.endpoint -> 
                         unit
  (** [register_adapter loc adapterId replicaGroupId ep]: Register the endpoint
      [ep] as adapter with name [adapterId] and the optional 
      [replicaGroupId] in the locator service.

      This function makes only synchronous calls, and uses a private
      proxy environment, and private TCP connections. If an error occurs,
      the exception [Error] is raised.

      Note that in the case of IceGrid as location service, you need to set
      [IceGrid.Registry.DynamicRegistration] to 1 in order to allow the
      self-registration of servers.

      @param dynamic_ip If true, the host name in the registered endpoint is
      replaced by the IP address used to talk to the locator service.
      (Default: [false])
   *)

val register_adapters : ?dynamic_ip:bool ->
                        pr_Ice_Locator ->
                        Hydro_types.object_dispatcher list ->
                        Hydro_types.endpoint -> 
                         unit
  (** [register_adapters loc l ep]: Registers all adapters in [l] for the
      endpoint [ep] in the locator service [loc].

      @param dynamic_ip If true, the host name in the registered endpoint is
      replaced by the IP address used to talk to the locator service.
      (Default: [false])
   *)


val unregister_adapter :  pr_Ice_Locator -> 
                          string ->
                          string option -> 
                            unit
  (** [unregister_adapter loc adapterId replicaGroupId]: Remove the registration
      entry for the adapter called [adapterId]  and the optional 
      [replicaGroupId] from the locator service.

      See also [register_adapter].
   *)   

val unregister_adapters : pr_Ice_Locator ->
                          Hydro_types.object_dispatcher list ->
                             unit
  (** [unregister_adapters loc l]: Remove the registeration of all adapters 
      in [l] for the from the locator service [loc].
   *)



val test_indirect_resolver : Hydro_proxy.proxy_resolver_t -> string -> 
                               (Hydro_types.endpoint * 
				  Hydro_types.network_port option) list
 (* Returns endpoint and port for an indirect proxy string *)

 (* e.g.:

    # let loc = Hydro_locator.get_Ice_Locator_of_port "127.0.0.1" 4061;;
    val loc : Hydro_builtin.pr_Ice_Locator = <abstr>
    # let res = Hydro_locator.proxy_resolver (Hydro_params.client_params()) loc;;
    val res : Hydro_proxy.proxy_resolver_t = <obj>
    # Hydro_locator.test_indirect_resolver res "any@adapter" ;;
    - : (Hydro_types.endpoint * Hydro_types.network_port option) list =
    [(`TCP <obj>, Some (`TCP (<abstr>, 5566)))]


    Note that the object name "any" is ignored during the resolution of
    adapter names.
  *)
