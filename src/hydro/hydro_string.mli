(** Parse/print various data structures as strings *)

(** The syntax of the string representations is defined in the
    ICE manual:

    identities and stringified proxies: Appendix D.1, e.g.
    http://zeroc.com/doc/Ice-3.3.1/manual/ProxyEndpointRef.html

    endpoints: Appendix D.2, e.g.
    http://zeroc.com/doc/Ice-3.3.1/manual/ProxyEndpointRef.51.2.html

 *)

val identity_of_string : string -> Hydro_types.identity
val string_of_identity : Hydro_types.identity -> string

val endpoints_of_string : string -> Hydro_types.endpoint array
val string_of_endpoints : Hydro_types.endpoint array -> string

val proxy_addr_of_string : string -> Hydro_types.proxy_addr
val string_of_proxy_addr : Hydro_types.proxy_addr -> string

