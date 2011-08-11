(** Establish connections between a local endpoint and a remote endpoint *)

(** {1 Managing connections} *)


open Hydro_types


class type multiplexed_descriptor =
object
  inherit descriptor
  method ctrl : Hydro_transport.hydro_multiplex_controller
end
    (** This is a multiplexed connection, used for both clients and
        servers
     *)

type client_connector =
    [ `Endpoint of endpoint * network_port option
    ]
   (** [`Endpoint(ep,port)]: [port] is the DNS-resolved port of the
       port mentioned in [ep] (if a DNS resolution happened)
    *)

val client_endpoint_type : client_connector -> endpoint_type


type master_connector =
    [ `Anon_endpoint_IPv4 of endpoint_type
    | `Anon_endpoint_IPv6 of endpoint_type
    | `Named_endpoint of Unix.sockaddr * endpoint_type
    ]

val master_endpoint_type : master_connector -> endpoint_type


class type transporter =
object
  method endpoint_type : Hydro_types.endpoint_type
    (** Every transporter is for a specific endpoint type *)

  method proxy_modes : proxy_mode list
    (** Supported proxy modes *)

  method client_connect_engine : client_connector ->
                                 Unixqueue.event_system ->
                                   multiplexed_descriptor Uq_engines.engine
  (** An engine that is able to create and, if requested, to connect the socket
   *)

  method server_endpoint : descriptor -> Hydro_types.server_params -> endpoint
    (** Get the endpoint where the server is reachable *)

  method server_multiplex_connection : descriptor ->
                                       Unixqueue.event_system ->
                                         multiplexed_descriptor Uq_engines.engine
  (** Get the multiplexed connection for an  existing server connection
   *)

  method server_create_engine : master_connector ->
                                Unixqueue.event_system ->
                                  descriptor Uq_engines.engine
  (** Creates a server socket. For stream sockets, a master descriptor is
      created. For datagram sockets, a normal socket is created
   *)

  method server_accept_engine : descriptor ->
                                Unixqueue.event_system ->
                                  descriptor Uq_engines.engine
  (** Accepts a new connection. For a master socket, a new connection is
      established. For a non-master socket, the returned descriptor is
      identical to the one passed in
   *)

end


val register_transporter : transporter -> unit
  (** Registers new transporters. For [`TCP] and [`UDP] transporters are
      pre-registered.
   *)

val get_transporter : endpoint_type -> transporter
  (** Get transporter or raise [Limitation `UnsupportedEndpointType] *)


val descriptor : Unix.file_descr -> bool -> transport_protocol_type -> descriptor
  (** [descriptor fd is_master tpt]: Create a hydro descriptor *)

val tcp_endpoint_of_file_descr : Unix.sockaddr -> float -> bool ->
                                   tcp_endpoint
  (** Returns the TCP endpoint for the TCP address, the timeout
      value, and the compression flag.
   *)
  (* This function has been exported for use in Hydro_netplex *)
