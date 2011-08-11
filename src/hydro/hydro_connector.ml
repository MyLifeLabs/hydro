open Hydro_types
open Hydro_util


type client_sockname =
    [ `Connect_addr of Unix.sockaddr * transport_protocol_type
    | `Connect_inet of string * int * transport_protocol_type
    ]


class type multiplexed_descriptor =
object
  inherit descriptor
  method ctrl : Hydro_transport.hydro_multiplex_controller
end

type client_connector =
    [ `Endpoint of endpoint * network_port option
    ]

let client_endpoint_type =
  function 
    | `Endpoint(ep,_) -> endpoint_type ep

type master_connector =
    [ `Anon_endpoint_IPv4 of endpoint_type 
    | `Anon_endpoint_IPv6 of endpoint_type 
    | `Named_endpoint of Unix.sockaddr * endpoint_type
    ]

let master_endpoint_type =
  function 
    | `Anon_endpoint_IPv4 ept -> ept
    | `Anon_endpoint_IPv6 ept -> ept
    | `Named_endpoint(_,ept) -> ept

class type transporter =
object
  method endpoint_type : Hydro_types.endpoint_type
  method proxy_modes : proxy_mode list
  method client_connect_engine : client_connector -> 
                                 Unixqueue.event_system ->
                                   multiplexed_descriptor Uq_engines.engine
  method server_endpoint : descriptor -> Hydro_types.server_params -> endpoint
  method server_multiplex_connection : descriptor ->
                                       Unixqueue.event_system ->
                                         multiplexed_descriptor Uq_engines.engine
  method server_create_engine : master_connector -> 
                                Unixqueue.event_system ->
                                  descriptor Uq_engines.engine
  method server_accept_engine : descriptor ->
                                Unixqueue.event_system ->
                                  descriptor Uq_engines.engine
end


class type mplex_config =
object
  method multiplexing : 
    descriptor -> Unixqueue.event_system -> multiplexed_descriptor Uq_engines.engine
end


let mplex_of_descr descr esys =
  match descr # proto_type with
    | `Stream ->
	Hydro_transport.stream_hydro_multiplex_controller
	  ~close_inactive_descr:false  (* this is done by descr#shutdown *)
	  descr#file_descr 
	  esys

    | `Datagram ->
	Hydro_transport.datagram_hydro_multiplex_controller
	  ~close_inactive_descr:false  (* this is done by descr#shutdown *)
	  descr#file_descr 
	  esys


class default_mplex_config : mplex_config =
object
  method multiplexing descr esys =
    let eng =
      try
        let mplex = mplex_of_descr descr esys in
	let c =
	  ( object 
	      method ctrl = mplex
	      method file_descr = descr#file_descr
	      method proto_type = descr#proto_type
	      method is_master = descr#is_master
	      method shutdown() =
		mplex#inactivate();
		descr#shutdown()
	    end
	  ) in
        new Uq_engines.epsilon_engine (`Done c) esys
      with
        | error ->
            new Uq_engines.epsilon_engine (`Error error) esys in
    Uq_engines.when_state
      ~is_aborted:(fun () -> descr#shutdown())
      ~is_error:(fun _ -> descr#shutdown())
      eng;
    eng

end


let default_mplex_config = new default_mplex_config


let tp_reg = Hashtbl.create 10

let register_transporter tp =
  Hashtbl.replace tp_reg tp#endpoint_type tp

let get_transporter ept =
  try Hashtbl.find tp_reg ept
  with Not_found -> 
    let n =
      match ept with
	| `TCP -> 1
	| `UDP -> 2
	| `SSL -> 3
	| `Unknown n -> n in
    raise (Limitation (`UnsupportedEndpointType n))


let socket_type =
  function
    | `Stream -> Unix.SOCK_STREAM
    | `Datagram -> Unix.SOCK_DGRAM


let descriptor fd is_master tpt =
  ( object
      val mutable is_down = false
      method file_descr = assert(not is_down); fd
      method proto_type = tpt
      method is_master = is_master
      method shutdown() =
	if not is_down then Unix.close fd;
	is_down <- true;
    end
  )

let connect_engine_for_ca ca tpt cfg esys =
  new Uq_engines.seq_engine
    (try 
       Uq_engines.connector ca esys
     with Not_found as nf ->    (* HACK *)
       match ca with
	 | `Socket(`Sock_inet_byname(_,host,_),_) ->
	     new Uq_engines.epsilon_engine
	       (`Error (Domain_not_found host))
	       esys
	 | _ -> raise nf
    )
    (fun cs ->
       let fd = Uq_engines.client_socket cs in
       let descr = descriptor fd false tpt in
       cfg#multiplexing descr esys
    )


let socket_connect_engine sock cfg esys =
  match sock with
    | `Connect_addr(sa,tpt) ->
	let st = socket_type tpt in
	let ca =
	  `Socket ( (match sa with
		       | Unix.ADDR_INET(ia,p) -> `Sock_inet(st,ia,p)
		       | Unix.ADDR_UNIX path  -> `Sock_unix(st,path)
		    ),
		    Uq_engines.default_connect_options
		  ) in
	connect_engine_for_ca ca tpt cfg esys

    | `Connect_inet(host,port,tpt) ->
	let st = socket_type tpt in
	let ca =
	  `Socket ( `Sock_inet_byname(st,host,port),
		    Uq_engines.default_connect_options
		  ) in
	connect_engine_for_ca ca tpt cfg esys

(*
    | `Unconnected_datagram dom ->
	let st = Unix.SOCK_DGRAM in
	let fd = Unix.socket dom st 0 in
	let descr = descriptor fd `Datagram in
	cfg#multiplexing descr esys

    | `Descriptor f ->
	let descr = f() in
	cfg#multiplexing descr esys
 *)



let client_sockname ep port_opt ept =
  match port_opt with
    | Some(`TCP(ip,p)) ->
	if ept = `TCP then
	  `Connect_addr(Unix.ADDR_INET(ip,p), `Stream)
	else
	  failwith "Hydro_connector: endpoint type mismatch"
    | Some(`UDP(ip,p)) ->
	if ept = `UDP then
	  `Connect_addr(Unix.ADDR_INET(ip,p), `Datagram)
	else
	  failwith "Hydro_connector: endpoint type mismatch"
    | None ->
	( match ep with
	    | `TCP tcp ->
		if ept = `TCP then
		  `Connect_inet(tcp#host, tcp#port, `Stream)
		else
		  failwith "Hydro_connector: endpoint type mismatch"
	    | `UDP udp ->
		if ept = `UDP then
		  `Connect_inet(udp#host, udp#port, `Datagram)
		else
		  failwith "Hydro_connector: endpoint type mismatch"
	    | _ ->
		assert false
	)


(* Handlers for TCP and UDP: *)

let udp_transporter =
  let pmodes = [ `Datagram; `Batch_datagram ] in
  ( object
      method endpoint_type = `UDP
      method proxy_modes = pmodes
      method client_connect_engine cc esys =
	match cc with
	  | `Endpoint(ep,port_opt) ->
	      let cs = client_sockname ep port_opt `UDP in
	      socket_connect_engine cs default_mplex_config esys

      method server_endpoint descr params =
	match Unix.getsockname(descr#file_descr) with
	  | Unix.ADDR_INET(inet,port) ->
	      let host = Unix.string_of_inet_addr inet in
	      `UDP ( object
		       method host = host
		       method port = port
		       method proto_major = 1
		       method proto_minor = Hydro_marshal.max_proto_minor
		       method enc_major = 1
		       method enc_minor = Hydro_marshal.max_enc_minor
		       method compress = false
		     end
		   )
	  | _ ->
	      failwith "Endpoint handler for UDP: This is not an Internet socket"

      method server_multiplex_connection descr esys =
	let mc = default_mplex_config in
	mc # multiplexing descr esys

      method server_create_engine mc esys =
	let one_step_eng = 
	  new Uq_engines.epsilon_engine (`Done()) esys in
	new Uq_engines.map_engine
	  ~map_done:(fun () ->
		       try
			 let dom =
			   match mc with
			     | `Anon_endpoint_IPv4 _ -> Unix.PF_INET
			     | `Anon_endpoint_IPv6 _ -> Unix.PF_INET6
			     | `Named_endpoint(addr,_) -> 
				 Unix.domain_of_sockaddr addr in
			 let s = Unix.socket dom Unix.SOCK_DGRAM 0 in
			 ( try
			     let addr =
			       match mc with
				 | `Anon_endpoint_IPv4 _ ->
				     Unix.ADDR_INET(Unix.inet_addr_any, 0)
				 | `Anon_endpoint_IPv6 _ ->
				     Unix.ADDR_INET(Unix.inet6_addr_any, 0)
				 | `Named_endpoint(addr,_) ->
				     addr in
			     Unix.bind s addr;
			     `Done(descriptor s false `Datagram)
			   with
			     | err -> Unix.close s; raise err
			 )
		       with
			 | err -> `Error err
		    )
	  one_step_eng
	
      method server_accept_engine descr esys =
	new Uq_engines.epsilon_engine (`Done descr) esys

    end : transporter
  )


let tcp_endpoint_of_file_descr addr tmo_f (compress : bool) =
  match addr with
    | Unix.ADDR_INET(inet,port) ->
	let host = Unix.string_of_inet_addr inet in
	let tmo = 
	  if tmo_f < 0.0 then (-1l) else Int32.of_float (1000.0 *. tmo_f) in
	( object
	    method host = host
	    method port = port
	    method timeout = tmo
	    method compress = compress
	  end
	)
    | _ ->
	failwith "tcp_endpoint_of_file_descr: This is not an Internet socket"


let tcp_transporter =
  let pmodes = [ `Twoway; `Oneway; `Batch_oneway ] in
  ( object
      method endpoint_type = `TCP
      method proxy_modes = pmodes
      method client_connect_engine cc esys =
	match cc with
	  | `Endpoint(ep,port_opt) ->
	      let cs = client_sockname ep port_opt `TCP in
	      socket_connect_engine cs default_mplex_config esys

      method server_endpoint descr params =
	let addr = Unix.getsockname descr#file_descr in
	`TCP (tcp_endpoint_of_file_descr addr params#trans_timeout false)

      method server_multiplex_connection descr esys =
	let mc = default_mplex_config in
	mc # multiplexing descr esys

      method server_create_engine mc esys =
	let one_step_eng = 
	  new Uq_engines.epsilon_engine (`Done()) esys in
	new Uq_engines.map_engine
	  ~map_done:(fun () ->
		       try
			 let dom =
			   match mc with
			     | `Anon_endpoint_IPv4 _ -> Unix.PF_INET
			     | `Anon_endpoint_IPv6 _ -> Unix.PF_INET6
			     | `Named_endpoint(addr,_) -> 
				 Unix.domain_of_sockaddr addr in
			 let s = Unix.socket dom Unix.SOCK_STREAM 0 in
			 ( try
			     let addr =
			       match mc with
				 | `Anon_endpoint_IPv4 _ ->
				     Unix.ADDR_INET(Unix.inet_addr_any, 0)
				 | `Anon_endpoint_IPv6 _ ->
				     Unix.ADDR_INET(Unix.inet6_addr_any, 0)
				 | `Named_endpoint(addr,_) ->
				     addr in
			     Unix.setsockopt s Unix.SO_REUSEADDR true;
			     Unix.bind s addr;
			     Unix.listen s 100;  (* TODO: configurable *)
			     `Done(descriptor s true `Stream)
			   with
			     | err -> Unix.close s; raise err
			 )
		       with
			 | err -> `Error err
		    )
	  one_step_eng
	
      method server_accept_engine descr esys =
	let acceptor =
	  new Uq_engines.direct_socket_acceptor
	    descr#file_descr
	    esys in
	let acc_eng =
	  acceptor # accept() in
	new Uq_engines.map_engine
	  ~map_done:(fun (fd,_) ->
		       `Done(descriptor fd false `Stream)
		    )
	  acc_eng

    end : transporter
  )




let () =
  register_transporter udp_transporter;
  register_transporter tcp_transporter
