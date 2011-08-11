(* This module is derived from OCamlnet's rpc_transport.mli *)

open Hydro_types


type 't result =
    [ `Ok of 't
    | `Error of exn
    ]

type 't result_eof =
    [ 't result
    | `End_of_file
    ]


type sockaddr =
    [ `Implied
    | `Sockaddr of Unix.sockaddr
    ]

class type hydro_multiplex_controller =
object
  method alive : bool
  method event_system : Unixqueue.event_system
  method getsockname : sockaddr
  method getpeername : sockaddr
  method peer_user_name : string option
  method transport_protocol_type : transport_protocol_type
  method reading : bool
  method read_eof : bool
  method start_reading : 
    ?peek: (unit -> unit) ->
    ?before_record:( int -> sockaddr -> unit ) ->
    when_done:( (msg_header * Netbuffer.t * sockaddr) result_eof -> unit) -> 
    unit -> unit
  method cancel_rd_polling : unit -> unit
  method abort_rw : unit -> unit
  method skip_message : unit -> unit
  method writing : bool
  method start_writing :
    when_done:(unit result -> unit) -> msg_buffer -> sockaddr -> unit
  method start_writing_eof :
    when_done:(unit result -> unit) -> unit -> unit
  method start_shutting_down :
    when_done:(unit result -> unit) -> unit -> unit
  method cancel_shutting_down : unit -> unit
  method set_timeout : notify:(unit -> unit) -> float -> unit
  method inactivate : unit -> unit
end


let e_dgram_too_short() =
  Protocol_violation(`MessageFormatViolation "Datagram too short")

let e_dgram_too_long() =
  Protocol_violation(`MessageFormatViolation "Datagram too long")

let e_unexpected_eof() =
  Protocol_violation(`MessageFormatViolation "Unexpected EOF")

let read_msg_dgram rd s pos len b =
  try
    let p = ref 0 in
    while true do
      let n = Hydro_message.read_msg_chunk rd s (pos + !p) (len - !p) b in
      if n=0 then raise(e_dgram_too_short());
      p := !p + n
    done
  with
    | End_of_file -> ()


let write_msg_dgram wr s pos len =
  let p = ref 0 in
  try
    while !p < len do
      let n = Hydro_message.write_msg_chunk wr s (pos + !p) (len - !p) in
      assert(n>0);
      p := !p + n
    done;
    if not (Hydro_message.write_done wr) then
      raise(e_dgram_too_long());
    !p
  with
    | End_of_file -> !p



class datagram_hydro_multiplex_controller 
        sockname peername_opt peer_user_name_opt
        (mplex : Uq_engines.datagram_multiplex_controller) esys 
      : hydro_multiplex_controller =
object(self)
  val rd_buffer = String.create 16384
  val wr_buffer = String.create 16384
    (* Max. size of an Internet datagram is 64 K. See RFC 760. However,
     * the Unix library uses a buffer size of only 16 K. Longer messages
     * can neither be received nor sent without truncation.
     *)

  method alive = mplex # alive
  method event_system = esys
  method getsockname = sockname
  method getpeername = 
    match peername_opt with
      | None -> failwith "#getpeername: not connected"
      | Some a -> a
  method transport_protocol_type = `Datagram
  method peer_user_name = peer_user_name_opt
  method reading = mplex # reading
  method read_eof = mplex # read_eof
  method writing = mplex # writing

  val mutable aborted = false
  val mutable skip_message = false

  method start_reading ?peek ?before_record ~when_done () =
    mplex # start_reading
      ?peek
      ~when_done:(fun exn_opt n ->
		    self # timer_event `Stop `R;
		    match exn_opt with
		      | None ->
			  let peer = `Sockaddr (mplex # received_from) in
			  if not skip_message then (
			    match before_record with
			      | None -> ()
			      | Some f -> 
				  f n peer
				    (* It can happen that reading is
                                     * aborted in the meantime!
                                     *)
			  );
			  if not aborted then (
			    if skip_message then
			      skip_message <- false
			    else (
			      try
				if n < 14 then raise(e_dgram_too_short());
				let hdr = 
				  Hydro_message.read_msg_header rd_buffer 0 in
				let rd =
				  Hydro_message.read_msg hdr in
				let b = Netbuffer.create n in
				read_msg_dgram rd rd_buffer 0 n b;
				when_done (`Ok(hdr, b, peer))
			      with
				| error -> when_done(`Error error)
			    )
			  )
		      | Some End_of_file ->
			  assert false
		      | Some Uq_engines.Cancelled ->
			  ()   (* Ignore *)
		      | Some error ->
			  when_done (`Error error)
		    )
      rd_buffer
      0
      (String.length rd_buffer);
    self # timer_event `Start `R
  
  method start_writing ~when_done mbuf addr =
    ( match addr with
	| `Implied ->
	    failwith "Hydro_transport.datagram_hydro_multiplex_controller: Cannot send datagram to implied address"
	| `Sockaddr a ->
	    mplex # send_to a
    );
    Hydro_message.write_msg_header wr_buffer 0 (fst mbuf);
    let wr = Hydro_message.write_msg mbuf in
    let len = write_msg_dgram wr wr_buffer 14 (String.length wr_buffer-14) in
    let tlen = len+14 in (* w/ header *)

    mplex # start_writing
      ~when_done:(fun exn_opt n ->
		    self # timer_event `Stop `W;
		    match exn_opt with
		      | None ->
			  if n = tlen then
			    when_done (`Ok ())
			  else
			    when_done (`Error (e_dgram_too_long()))
		      | Some Uq_engines.Cancelled ->
			  ()  (* ignore *)
		      | Some error ->
			  when_done (`Error error)
		 )
      wr_buffer
      0
      tlen;
    self # timer_event `Start `W

  method start_writing_eof ~when_done () =
    assert false  (* Cannot do that *)

  method cancel_rd_polling () =
    if mplex#reading then
      mplex # cancel_reading()

  method skip_message () =
    skip_message <- true

  method abort_rw () =
    aborted <- true;
    mplex # cancel_reading();
    mplex # cancel_writing()
    
  method start_shutting_down ~when_done () =
    mplex # start_shutting_down
      ~when_done:(fun exn_opt ->
		    self # timer_event `Stop `D;
		    match exn_opt with
		      | None -> when_done (`Ok ())
		      | Some error -> when_done (`Error error)
		 )
      ();
    self # timer_event `Start `D

  method cancel_shutting_down () =
    self # timer_event `Stop `D;
    mplex # cancel_shutting_down()

  method inactivate () =
    self # stop_timer();
    mplex # inactivate()

  val mutable timer = None
  val mutable timer_r = `Stop
  val mutable timer_w = `Stop
  val mutable timer_d = `Stop
  val mutable timer_group = None

  method set_timeout ~notify tmo =
    timer <- Some(notify, tmo)

  method private timer_event start_stop which =
    ( match timer with
	| None -> ()
	| Some(notify, tmo) ->
	    ( match which with
		| `R -> timer_r <- start_stop
		| `W -> timer_w <- start_stop
		| `D -> timer_d <- start_stop
	    );
	    let need_start =
	      timer_r = `Start || timer_w = `Start || timer_d = `Start in
	    self # stop_timer();
	    if need_start then (
	      let g = Unixqueue.new_group esys in
	      timer_group <- Some g;
	      Unixqueue.once esys g tmo
		(fun () -> 
		   timer_group <- None;
		   notify()
		)
	    );
    )


  method private stop_timer() =
    ( match timer_group with
	| None -> ()
	| Some g -> Unixqueue.clear esys g
    );
    timer_group <- None;
    timer_r <- `Stop;
    timer_w <- `Stop;
    timer_d <- `Stop


end



let datagram_hydro_multiplex_controller ?(close_inactive_descr=true) fd esys =
  let sockname = 
    try
      `Sockaddr(Unix.getsockname fd) 
    with
	(* The OCaml runtime sometimes returns EAFNOSUPPORT when asked
           for inaccessible socket names. EOPNOTSUPP is documented
           by SUS.
         *)
      | Unix.Unix_error((Unix.EAFNOSUPPORT|Unix.EOPNOTSUPP),_,_) -> `Implied in
  let peername_opt =
    try Some(`Sockaddr(Unix.getpeername fd))
    with
      | Unix.Unix_error((Unix.EAFNOSUPPORT|Unix.EOPNOTSUPP),_,_) -> 
	  Some `Implied
      | Unix.Unix_error(Unix.ENOTCONN,_,_) -> 
	  (* ENOTCONN is special because we allow to set the peer address
             per datagram in this case!
           *)
	  None in
  let mplex = 
    Uq_engines.create_multiplex_controller_for_datagram_socket
      ~close_inactive_descr
      fd esys in
  new datagram_hydro_multiplex_controller sockname peername_opt None mplex esys
;;


let read_msg_whole_chunk rd s pos len b =
  let p = ref 0 in
  try
    while !p < len do
      let n = Hydro_message.read_msg_chunk rd s (pos + !p) (len - !p) b in
      assert(n>0);
      p := !p + n
    done;
    (!p, Hydro_message.read_done rd)
  with
    | End_of_file -> (!p, true)


let write_msg_whole_chunk wr s pos len =
  let p = ref 0 in
  try
    while !p < len do
      let n = Hydro_message.write_msg_chunk wr s (pos + !p) (len - !p) in
      assert(n>0);
      p := !p + n
    done;
    (!p, Hydro_message.write_done wr)
  with
    | End_of_file -> (!p, true)


class stream_hydro_multiplex_controller sockname peername peer_user_name_opt
        (mplex : Uq_engines.multiplex_controller) esys 
      : hydro_multiplex_controller =
object(self)
  val mutable wr_buffer = String.create 16384

  val mutable rd_buffer = String.create 16384
  val mutable hdr_buffer = String.create 14
  val mutable rd_mode = `Header 0
  val mutable rd_continuation = None

  method alive = mplex # alive
  method event_system = esys
  method getsockname = sockname
  method getpeername = peername
  method transport_protocol_type = `Stream
  method peer_user_name = peer_user_name_opt
  method reading = mplex # reading
  method read_eof = mplex # read_eof
  method writing = mplex # writing

  val mutable aborted = false
  val mutable skip_message = false

  method start_reading ?peek ?(before_record = fun _ _ -> ()) ~when_done () =
    let rec est_reading() =
      rd_continuation <- None;
      mplex # start_reading
	?peek
	~when_done:(fun exn_opt n ->
		      self # timer_event `Stop `R;
		      match exn_opt with
			| None ->
			    process 0 n
			| Some End_of_file ->
			    ( match rd_mode with
				| `Header 0 ->
				    return_eof()   (* EOF between messages *)
				| _ ->
				    return_error (e_unexpected_eof())
			    )
			| Some Uq_engines.Cancelled ->
			    ()   (* Ignore *)
			| Some error ->
			    return_error error
		   )
	rd_buffer
	0
	(String.length rd_buffer);
      self # timer_event `Start `R

    and process pos len =
      if len > 0 then (
	match rd_mode with
	  | `Header hdr_len ->
	      (* Read the header *)
	      let m = min (14 - hdr_len) len in
	      String.blit rd_buffer pos hdr_buffer hdr_len m;

	      if hdr_len+m = 14 then (
		try
		  let hdr =
		    Hydro_message.read_msg_header hdr_buffer 0 in
		  let rd =
		    Hydro_message.read_msg hdr in
		  let buf =
		    Netbuffer.create hdr#body_size in
		  rd_mode <- `Body (hdr, rd, buf);
		  before_record (hdr#body_size + 14) `Implied;
		  if hdr#body_size = 0 then (
		    (* Special case! *)
		    rd_mode <- `Header 0;
		    if skip_message then (
		      skip_message <- false;
		      process (pos+m) (len-m)
		    ) else (
		      rd_continuation <- 
			Some(fun() -> process (pos+m) (len-m));
		      return_msg (hdr, buf)
		    )
		  )
		  else
		    process (pos+m) (len-m)
		with
		  | error -> return_error error
	      )
	      else (
		rd_mode <- `Header (hdr_len+m);
		est_reading()
	      )
		
	  | `Body(hdr, rd, buf) ->
	      (* Read body data *)
	      let real_len, msg_done = 
		read_msg_whole_chunk rd rd_buffer pos len buf in
	      if msg_done then (
		rd_mode <- `Header 0;
		if skip_message then (
		  skip_message <- false;
		  process (pos+real_len) (len-real_len)
		) else (
		  rd_continuation <- 
		    Some(fun() -> process (pos+real_len) (len-real_len));
		  return_msg (hdr, buf)
		)
	      )
	      else (
		assert(len = real_len);
		est_reading()
	      )
      )
      else
	est_reading()

    and return_msg (hdr,buf) =
      if not aborted then (
	when_done (`Ok(hdr, buf, peername))
      )

    and return_error e =
      rd_continuation <- None;
      if not aborted then
	when_done (`Error e)

    and return_eof () =
      rd_continuation <- None;
      if not aborted then
	when_done `End_of_file 

    in
    match rd_continuation with
      | None ->
	  est_reading()
      | Some f ->
	  f()


  method start_writing ~when_done (hdr,bufs) addr =
    let rec est_writing pos len wr wr_done =
      mplex # start_writing
	~when_done:(fun exn_opt n ->
		      self # timer_event `Stop `W;
		      match exn_opt with
			| None ->
			    assert(n <= len);
			    if n = len then (
			      if not aborted then (
				if wr_done then
				  when_done (`Ok ())
				else (
				  let m, wr_done =
				    write_msg_whole_chunk 
				      wr wr_buffer 
				      0 (String.length wr_buffer) in
				  est_writing 0 m wr wr_done
				)
			      )
			    )
			    else (
			      if not aborted then
				est_writing (pos+n) (len-n) wr wr_done
			    )
			| Some Uq_engines.Cancelled ->
			    ()  (* ignore *)
			| Some error ->
			    if not aborted then
			      when_done (`Error error)
		   )
	wr_buffer
	pos
	len;
      self # timer_event `Start `W
    in

    ( match addr with
	| `Implied -> ()
	| `Sockaddr a ->
	    if addr <> peername then
	      failwith "Hydro_transport.stream_hydro_multiplex_controller: cannot send to this address"
    );
    Hydro_message.write_msg_header wr_buffer 0 hdr;
    let wr = Hydro_message.write_msg (hdr,bufs) in
    let n, wr_done =
      write_msg_whole_chunk wr wr_buffer 14 (String.length wr_buffer - 14) in
    est_writing 0 (n+14) wr wr_done


  method cancel_rd_polling () =
    if mplex#reading then
      mplex # cancel_reading()

  method skip_message () =
    skip_message <- true

  method abort_rw () =
    aborted <- true;
    mplex # cancel_reading();
    mplex # cancel_writing()

  method start_writing_eof ~when_done () =
    mplex # start_writing_eof
      ~when_done:(fun exn_opt ->
		    self # timer_event `Stop `W;
		    match exn_opt with
		      | None -> when_done (`Ok ())
		      | Some error -> when_done (`Error error)
		 )
      ();
    self # timer_event `Start `W
    
  method start_shutting_down ~when_done () =
    mplex # start_shutting_down
      ~when_done:(fun exn_opt ->
		    self # timer_event `Stop `D;
		    match exn_opt with
		      | None -> when_done (`Ok ())
		      | Some error -> when_done (`Error error)
		 )
      ();
    self # timer_event `Start `D

  method cancel_shutting_down () =
    self # timer_event `Stop `D;
    mplex # cancel_shutting_down()

  method inactivate () =
    self # stop_timer();
    mplex # inactivate()

  val mutable timer = None
  val mutable timer_r = `Stop
  val mutable timer_w = `Stop
  val mutable timer_d = `Stop
  val mutable timer_group = None

  method set_timeout ~notify tmo =
    timer <- Some(notify, tmo)

  method private timer_event start_stop which =
    ( match timer with
	| None -> ()
	| Some(notify, tmo) ->
	    ( match which with
		| `R -> timer_r <- start_stop
		| `W -> timer_w <- start_stop
		| `D -> timer_d <- start_stop
	    );
	    let need_start =
	      timer_r = `Start || timer_w = `Start || timer_d = `Start in
	    self # stop_timer();
	    if need_start then (
	      let g = Unixqueue.new_group esys in
	      timer_group <- Some g;
	      Unixqueue.once esys g tmo
		(fun () -> 
		   timer_group <- None;
		   notify()
		)
	    );
    )


  method private stop_timer() =
    ( match timer_group with
	| None -> ()
	| Some g -> Unixqueue.clear esys g
    );
    timer_group <- None;
    timer_r <- `Stop;
    timer_w <- `Stop;
    timer_d <- `Stop

end



let stream_hydro_multiplex_controller ?(close_inactive_descr=true) fd esys =
  let sockname = 
    try
      `Sockaddr(Unix.getsockname fd) 
    with
	(* The OCaml runtime sometimes returns EAFNOSUPPORT when asked
           for inaccessible socket names. EOPNOTSUPP is documented
           by SUS. We also catch ENOTSOCK to allow using RPC with
           bidirectional pipes that are not socket-based (i.e. SysV
           bidirectional pipes).
         *)
      | Unix.Unix_error((Unix.EAFNOSUPPORT|Unix.EOPNOTSUPP|Unix.ENOTSOCK),
			_,_) -> `Implied in
  let peername = 
    try
      `Sockaddr(Unix.getpeername fd)
    with
	(* also catching ENOTCONN - which might happen for strange socket
           implementations
         *)
      | Unix.Unix_error((Unix.EAFNOSUPPORT|Unix.EOPNOTSUPP|Unix.ENOTSOCK|Unix.ENOTCONN),
			_,_) -> `Implied in
  let mplex = 
    Uq_engines.create_multiplex_controller_for_connected_socket
      ~close_inactive_descr
      ~supports_half_open_connection:true
      fd esys in
  new stream_hydro_multiplex_controller sockname peername None mplex esys
;;
