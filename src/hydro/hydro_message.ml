open Hydro_types

let read_msg_header s pos =
  if pos < 0 || pos+14 > String.length s then
    invalid_arg "Hydro_message.read_msg_header";

  try
    if String.sub s pos 4 <> "IceP" then
      failwith "Invalid message header";

    let proto_major = Char.code s.[pos+4] in
    let proto_minor = Char.code s.[pos+5] in
    let enc_major = Char.code s.[pos+6] in
    let enc_minor = Char.code s.[pos+7] in

    let msg_type_ch = s.[pos+8] in
    let zstatus_ch = s.[pos+9] in
    let size = Hydro_unmarshal.read_int s (pos+10) in
    if size < 14 then failwith "Invalid message size";

    let msg_type =
      match msg_type_ch with
	| '\000' -> `Request
	| '\001' -> `Batch_request
	| '\002' -> `Reply
	| '\003' -> `Validate_connection
	| '\004' -> `Close_connection
	| _ -> failwith "Invalid message type" in

    let zstatus =
      match zstatus_ch with
	| '\000' -> `Compression_unsupported
	| '\001' -> `Uncompressed
	| '\002' -> `Compressed
	| _ -> failwith "Invalid compression status" in

    ( object
	method proto_major = proto_major
	method proto_minor = proto_minor
	method enc_major = enc_major
	method enc_minor = enc_minor
	method msg_type = msg_type
	method compression = zstatus
	method body_size = size - 14
      end
    )

  with
    | Failure msg
    | Unmarshal_error msg ->
	raise(Protocol_violation (`MessageFormatViolation msg))


let write_msg_header  s pos (hdr:msg_header) =
  if pos < 0 || pos+14 > String.length s then
    invalid_arg "Hydro_message.write_msg_header";

  String.unsafe_set s pos 'I';
  String.unsafe_set s (pos+1) 'c';
  String.unsafe_set s (pos+2) 'e';
  String.unsafe_set s (pos+3) 'P';

  String.unsafe_set s (pos+4) (Char.chr hdr#proto_major);
  String.unsafe_set s (pos+5) (Char.chr hdr#proto_minor);
  String.unsafe_set s (pos+6) (Char.chr hdr#enc_major);
  String.unsafe_set s (pos+7) (Char.chr hdr#enc_minor);

  String.unsafe_set s (pos+8)
    ( match hdr#msg_type with
	| `Request -> '\000'
	| `Batch_request -> '\001'
	| `Reply -> '\002'
	| `Validate_connection -> '\003'
	| `Close_connection -> '\004'
    );

  String.unsafe_set s (pos+9)
    ( match hdr#compression with
	| `Compression_unsupported -> '\000'
	| `Uncompressed -> '\001'
	| `Compressed -> '\002'
    );

  String.unsafe_blit
    (Hydro_marshal.encoded_int (hdr#body_size+14)) 0 s (pos+10) 4;

  ()


type msg_reader =
    { rd_size : int;
      mutable rd_pos : int;
    }

let read_msg hdr =
  if hdr#compression = `Compressed then
    raise(Protocol_violation `CompressionNotSupported);
  { rd_size = hdr#body_size;
    rd_pos = 0
  }

let read_msg_chunk rd s pos len b =
  assert(rd.rd_pos <= rd.rd_size);
  if rd.rd_pos = rd.rd_size then
    raise End_of_file;
  let avail = rd.rd_size - rd.rd_pos in
  let n = min avail len in
  Netbuffer.add_sub_string b s pos n;
  rd.rd_pos <- rd.rd_pos + n;
  n

let read_done rd =
  assert(rd.rd_pos <= rd.rd_size);
  rd.rd_pos = rd.rd_size

type msg_writer =
    { wr_size : int;
      mutable wr_pos : int;
      mutable wr_bufs : encap_buffer list;
      mutable wr_bufs_ofs : int;
    }

let write_msg (hdr, bufs) =
  if hdr#compression = `Compressed then
    raise(Protocol_violation `CompressionNotSupported);

  (* Checking bufs integrity: *)
  List.iter
    (fun buf ->
       let l = Netbuffer.length buf.encap_buf in
       assert(buf.encap_pos >= 0);
       assert(buf.encap_pos <= l);
       assert(buf.encap_len >= 0);
       assert(buf.encap_pos + buf.encap_len <= l);
    )
    bufs;

  { wr_size = hdr#body_size;
    wr_pos = 0;
    wr_bufs_ofs = 0;
    wr_bufs = bufs
  }

let write_msg_chunk wr s pos len =
  assert(wr.wr_pos <= wr.wr_size);
  if len = 0 then
    0
  else (
    let n = ref 0 in
    let avail = wr.wr_size - wr.wr_pos in
    while !n = 0 do
      match wr.wr_bufs with
	| [] ->
	    if wr.wr_pos = wr.wr_size then
	      raise End_of_file
	    else
	      failwith "Hydro_message.write_msg_chunk: less data in body than announced in header"
	| eb :: bufs' ->
	    let eb_pos = eb.encap_pos + wr.wr_bufs_ofs in
	    let eb_len = eb.encap_len - wr.wr_bufs_ofs in
	    if avail = 0 && eb_len > 0 then
	      failwith "Hydro_message.write_msg_chunk: more data in body than announced in header";
	    n := min (min eb_len avail) len;
	    if !n = 0 then (
	      wr.wr_bufs <- bufs';
	      wr.wr_bufs_ofs <- 0;
	    )
	    else (
	      Netbuffer.blit eb.encap_buf eb_pos s pos !n;
	      wr.wr_bufs_ofs <- wr.wr_bufs_ofs + !n
	    )
    done;
    wr.wr_pos <- wr.wr_pos + !n;
    !n
  )

let write_done wr =
  assert(wr.wr_pos <= wr.wr_size);
  wr.wr_pos = wr.wr_size

