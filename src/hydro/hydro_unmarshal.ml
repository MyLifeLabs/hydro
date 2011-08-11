open Hydro_types
open Hydro_util
open Hydro_prelim

let e_exceed n =
  raise(Unmarshal_error ("Message exceeds encapsulation (#" ^
	  string_of_int n ^ ")"))

let e_int_range() =
  raise(Unmarshal_error "Int32 value outside the valid int range")

let e_bool() =
  raise(Unmarshal_error "Bool value out of range")

let read_bool s p =
  let n0 = Char.code (String.unsafe_get s p) in
  match n0 with
    | 0 -> false
    | 1 -> true
    | _ -> e_bool()

let unmarshal_bool s cur end_pos =
  if !cur >= end_pos then e_exceed 30;
  let b = read_bool s !cur in
  incr cur;
  b

let read_byte s p =
  Char.code (String.unsafe_get s p)

let unmarshal_byte s cur end_pos =
  if !cur >= end_pos then e_exceed 31;
  let n = read_byte s !cur in
  incr cur;
  n

let read_short s p =
  let n1 = Char.code (String.unsafe_get s (p+1)) in
  let n0 = Char.code (String.unsafe_get s p) in
  (n1 lsl 8) lor n0

let unmarshal_short s cur end_pos =
  if !cur+1 >= end_pos then e_exceed 32;
  let n = read_short s !cur in
  cur := !cur+2;
  n

IFDEF WORDSIZE_64 THEN
let read_int s p =
  let n3 = Char.code (String.unsafe_get s (p+3)) in
  let n2 = Char.code (String.unsafe_get s (p+2)) in
  let n1 = Char.code (String.unsafe_get s (p+1)) in
  let n0 = Char.code (String.unsafe_get s p) in
  ((n3 lsl 55) asr 31) lor (n2 lsl 16) lor (n1 lsl 8) lor n0
ELSE
let read_int s p =
  let n3 = Char.code (String.unsafe_get s (p+3)) in
  let n2 = Char.code (String.unsafe_get s (p+2)) in
  let n1 = Char.code (String.unsafe_get s (p+1)) in
  let n0 = Char.code (String.unsafe_get s p) in
  if n3 >= 64 && n3 <= 191 then e_int_range();
  ((n3 land 0x7f) lsl 24) lor (n2 lsl 16) lor (n1 lsl 8) lor n0
ENDIF

let unmarshal_int s cur end_pos =
  if !cur+3 >= end_pos then e_exceed 33;
  let n = read_int s !cur in
  cur := !cur+4;
  n

let read_int32 s p =
  let n3 = Int32.of_int (Char.code (String.unsafe_get s (p+3))) in
  let n2 = Char.code (String.unsafe_get s (p+2)) in
  let n1 = Char.code (String.unsafe_get s (p+1)) in
  let n0 = Char.code (String.unsafe_get s p) in
  Int32.logor
    (Int32.shift_left n3 24)
    (Int32.of_int ((n2 lsl 16) lor (n1 lsl 8) lor n0))

let unmarshal_int32 s cur end_pos =
  if !cur+3 >= end_pos then e_exceed 34;
  let n = read_int32 s !cur in
  cur := !cur+4;
  n

let read_int64 s p =
  let n7 = Int64.of_int (Char.code (String.unsafe_get s (p+7))) in
  let n6 = Int64.of_int (Char.code (String.unsafe_get s (p+6))) in
  let n5 = Int64.of_int (Char.code (String.unsafe_get s (p+5))) in
  let n4 = Int64.of_int (Char.code (String.unsafe_get s (p+4))) in
  let n3 = Int64.of_int (Char.code (String.unsafe_get s (p+3))) in
  let n2 = Char.code (String.unsafe_get s (p+2)) in
  let n1 = Char.code (String.unsafe_get s (p+1)) in
  let n0 = Char.code (String.unsafe_get s p) in
  Int64.logor
    (Int64.shift_left n7 56)
    (Int64.logor
       (Int64.shift_left n6 48)
       (Int64.logor
	  (Int64.shift_left n5 40)
	  (Int64.logor
	     (Int64.shift_left n4 32)
	     (Int64.logor
		(Int64.shift_left n3 24)
		(Int64.of_int ((n2 lsl 16) lor (n1 lsl 8) lor n0))))))

let unmarshal_int64 s cur end_pos =
  if !cur+7 >= end_pos then e_exceed 35;
  let n = read_int64 s !cur in
  cur := !cur+8;
  n

let read_float s p =
  let x = read_int32 s p in
  Int32.float_of_bits x

let unmarshal_float s cur end_pos =
  if !cur+3 >= end_pos then e_exceed 36;
  let n = read_float s !cur in
  cur := !cur+4;
  n

let read_double s p =
  let x = read_int64 s p in
  Int64.float_of_bits x

let unmarshal_double s cur end_pos =
  if !cur+7 >= end_pos then e_exceed 37;
  let n = read_double s !cur in
  cur := !cur+8;
  n

let unmarshal_size s cur end_pos =
  if !cur >= end_pos then e_exceed 10;
  let c = String.unsafe_get s !cur in
  if c = '\255' then (
    if !cur >= end_pos+4 then e_exceed 11;
    let n = read_int s (!cur+1) in
    if n < 255 then
      raise(Unmarshal_error "Bad size encoding");
    cur := !cur+5;
    n
  )
  else (
    incr cur;
    Char.code c
  )

let unmarshal_string s cur end_pos =
  let n = unmarshal_size s cur end_pos in
  if !cur+n > end_pos then e_exceed 20;
  let u = String.create n in
  String.unsafe_blit s !cur u 0 n;
  cur := !cur + n;
  u

let unmarshal_sequence unmarshal_element s cur end_pos =
  let n = unmarshal_size s cur end_pos in
  Array.init
    n
    (fun k ->
       unmarshal_element s cur end_pos
    )

let unmarshal_dictionary unmarshal_left unmarshal_right s cur end_pos =
  let n = unmarshal_size s cur end_pos in
  Array.init
    n
    (fun k ->
       let v1 = unmarshal_left s cur end_pos in
       let v2 = unmarshal_right s cur end_pos in
       (v1,v2)
    )

let unmarshal_enum n s cur end_pos =
  let k =
    if n <= 127 then
      unmarshal_byte s cur end_pos
    else if n <= 32767 then
      unmarshal_short s cur end_pos
    else
      unmarshal_int s cur end_pos in
  if k >= n then
    raise(Unmarshal_error "enum out of range");
  k

let decode_facet facet_seq =
  match facet_seq with
    | [| |] -> None
    | [| VString fc |] -> Some fc
    | _ -> raise(Unmarshal_error "Invalid facet sequence")


let decode_mode mode_num =
  match mode_num with
    | 0 -> `Normal
    | 1 -> `Nonmutating
    | 2 -> `Idempotent
    | _ -> raise(Unmarshal_error "Invalid request mode")


let decode_context ctx =
  Array.map
    (function
       | (VString n, VString v) -> (n,v)
       | _ -> raise(Unmarshal_error "Invalid context dictionary")
    )
    ctx


let decapsulate_str s pos len =
  if pos < 0 || len < 6 || pos+len > String.length s then
    raise(Unmarshal_error "Cannot break up encapsulation");

  let size = read_int s pos in
  if size > len then
    raise(Unmarshal_error "Cannot break up encapsulation");

  if s.[pos+4] <> '\001' then
    raise(Limitation `UnsupportedEncodingVersion);
  if s.[pos+5] <> '\000' then
    raise(Limitation `UnsupportedEncodingVersion);

  (pos+6, size-6)


let decapsulate eb =
  let nb_len = Netbuffer.length eb.encap_buf in
  if eb.encap_pos < 0 || eb.encap_len < 0 || eb.encap_pos+eb.encap_len > nb_len then
    invalid_arg "Hydro_unmarshal.decapsulate: invalid substring";

  let s = Netbuffer.unsafe_buffer eb.encap_buf in
  let pos', len' = decapsulate_str s eb.encap_pos eb.encap_len in

  { encap_buf = eb.encap_buf;
    encap_pos = pos';
    encap_len = len';
    encap_enc_minor = 0;  (* or read it from s *)
  }


type unmarshal_env =
    { cmap : (int32, CiStrSet.t * object_value) Hashtbl.t;
        (* Maps inst_id to (class_names, class_value) where
           - class_names are the absolute type IDs that occur in the slices
           - class_value is the object
           - inst_id is the instance ID used in the serialized string
         *)
      cpatches : (int32, (string * class_repr ref) list) Hashtbl.t;
        (* Maps inst_id to a list of placeholders (class_name, cvalref) where
           - class_name is the class expected where the placeholder occurred
           - cvalref is a [ref `Placeholder] that is to be replaced by
             a [ref `Value] ("patching")
	 *)
      tmap : (int, string) Hashtbl.t;
        (* Maps numeric values to absolute class type IDs *)
      next_type_id : int ref;
        (* The next type ID to allocate *)
      em : int;
        (* minor encoding version *)
    }


let rec unmarshal_value env (sys:system) ht s cur end_pos =
  (* Unmarshals the bytes in [s] at [!cur] and following, while not exceeding
     [end_pos].
   *)
(* prerr_endline ("cur=" ^ string_of_int !cur); *)
(* prerr_endline ("ht=" ^ Hydro_util.htype_to_string ht);*)
  match ht with
    | TVoid ->
	VNothing
    | TBool ->
	VBool(unmarshal_bool s cur end_pos)
    | TByte ->
	VByte(unmarshal_byte s cur end_pos)
    | TShort ->
	VShort(unmarshal_short s cur end_pos)
    | TInt ->
	VInt(unmarshal_int s cur end_pos)
    | TInt32 ->
	VInt32(unmarshal_int32 s cur end_pos)
    | TLong ->
	VLong(unmarshal_int64 s cur end_pos)
    | TFloat ->
	VFloat(unmarshal_float s cur end_pos)
    | TDouble ->
	VDouble(unmarshal_double s cur end_pos)
    | TString ->
	VString(unmarshal_string s cur end_pos)
    | TByteseq ->
	VByteseq(unmarshal_string s cur end_pos)
    | TSequence ht' ->
	VSequence(
	  unmarshal_sequence
	    (unmarshal_value env sys ht')
	    s cur end_pos
	)
    | TDictionary(ht1,ht2) ->
	VDictionary(
	  unmarshal_dictionary
	    (unmarshal_value env sys ht1)
	    (unmarshal_value env sys ht2)
	    s cur end_pos
	)
    | TEnum e ->
	let n = Array.length e in
	VEnum(unmarshal_enum n s cur end_pos)
    | TStruct st ->
	let n = Array.length st in
	let v = Array.make n VNothing in
	for k = 0 to n-1 do
	  v.(k) <- unmarshal_value env sys (snd st.(k)) s cur end_pos
	done;
	VStruct v
    | TClass cname ->
	if !cur+3 >= end_pos then e_exceed 41;
	let inst_id = read_int32 s !cur in
	cur := !cur+4;
	if inst_id = 0l then
	  VNull
	else
	  if inst_id < 0l then (
	    let inst_id = Int32.neg inst_id in
	    try
	      let (all_types, cval) = Hashtbl.find env.cmap inst_id in
	      if not (CiStrSet.mem cname all_types) then
		raise(Unmarshal_error "Class value has unexpected type");
	      VClass (ref (`Value cval))
	    with
	      | Not_found ->
		  let cval = ref (`Placeholder inst_id) in
		  let l =
		    try Hashtbl.find env.cpatches inst_id
		    with Not_found -> [] in
		  Hashtbl.replace env.cpatches inst_id ((cname,cval) :: l);
		  VClass cval
	  )
	  else (
	    let cval, known_class_id_list =
	      unmarshal_class_val env sys inst_id s cur end_pos in
	    if not (CiStrSet.mem cname known_class_id_list) then
	      raise(Unmarshal_error "Class value has unexpected type");
	    VClass (ref cval)
	  )
    | TProxy pname ->
	let id_name = unmarshal_string s cur end_pos in
	let id_cat = unmarshal_string s cur end_pos in
	if id_name = "" && id_cat = "" then
	  VNull
	else
	  let v =
	    unmarshal_value env sys t_proxy_alt s cur end_pos in
	  ( match v with
	      | VStruct [| VSequence facet_seq;
			   VByte mode_int;
			   VBool secure
			|] ->
		  let facet = decode_facet facet_seq in
		  let mode =
		    match mode_int with
		      | 0 -> `Twoway
		      | 1 -> `Oneway
		      | 2 -> `Batch_oneway
		      | 3 -> `Datagram
		      | 4 -> `Batch_datagram
		      | _ -> raise(Unmarshal_error("Invalid proxy mode " ^ string_of_int mode_int)) in
		  let n_params = unmarshal_size s cur end_pos in
		  let params =
		    if n_params > 0 then (
		      let ep_array = Array.make n_params `None in
		      for k = 0 to n_params-1 do
			if !cur+1 >= end_pos then e_exceed 42;
			let ep_t = read_short s !cur in
			cur := !cur+2;
			let (pos', len') =
			  decapsulate_str s !cur (end_pos - !cur) in
			let cur' = ref pos' in
			let end_pos' = pos' + len' in
			let ep =
			  match ep_t with
			    | 1 | 2 -> (* TCP and SSL *)
				let v =
				  unmarshal_value
				    env sys t_tcp_endpoint s cur' end_pos' in
				if !cur' <> end_pos' then
				  raise(Unmarshal_error "Illegal proxy endpoint");
				( match v with
				    | VStruct [| VString host;
						 VInt port;
						 VInt32 tmo;
						 VBool compress
					      |] ->
					let ep_obj =
					  ( object
					      method host = host
					      method port = port
					      method timeout = tmo
					      method compress = compress
					    end
					  ) in
					if ep_t = 1 then `TCP ep_obj
					else `SSL ep_obj
				    | _ -> assert false
				)
			    | 3 -> (* UDP *)
				let v =
				  unmarshal_value
				    env sys t_udp_endpoint s cur' end_pos' in
				if !cur' <> end_pos' then
				  raise(Unmarshal_error "Illegal proxy endpoint");
				( match v with
				    | VStruct [| VString host;
						 VInt port;
						 VInt proto_major;
						 VInt proto_minor;
						 VInt enc_major;
						 VInt enc_minor;
						 VBool compress
					      |] ->
					let ep_obj =
					  ( object
					      method host = host
					      method port = port
					      method proto_major = proto_major
					      method proto_minor = proto_minor
					      method enc_major = enc_major
					      method enc_minor = enc_minor
					      method compress = compress
					    end
					  ) in
					`UDP ep_obj
				    | _ -> assert false
				)
			    | n ->
				`Unknown(n, String.sub s pos' len')
			in
			ep_array.(k) <- ep;
			cur := end_pos';
		      done;
		      `Endpoints
			(Array.map
			   (function #endpoint as ep -> ep | _ -> assert false)
			   ep_array
			)
		    )
		    else (
		      (* Indirect proxy *)
		      let s = unmarshal_string s cur end_pos in
		      if s = "" then
			`Well_known  (* CHECK *)
		      else
			`Adapter s
		    ) in
		  let id_obj =
 	            (object method name=id_name method category=id_cat end) in
		  VProxy
		    ( object
			method id = id_obj
			method facet = facet
			method mode = mode
			method secure = secure
			method parameters = params
		      end
		    )
	      | _ -> assert false
	  )
    | TDirectMapping(ht',_,unmarshal) ->
	VDirectMapping(unmarshal s cur end_pos)


and unmarshal_class_val env sys inst_id s cur end_pos =
  if Hashtbl.mem env.cmap inst_id then
    raise(Unmarshal_error "Instance ID is defined twice");

  (* Now read the slices. There may be slices with known class IDs and with
     unknown class IDs. There _must_ be a slice with the class ID [cname],
     and following slices must match our own class definition.

     If we don't find [cname] we run over the end of the slices. There is
     no safe way to prevent that.

     As we want to keep the dynamic class of the class value, we start
     decoding when the first known class ID is found. (Alternatively,
     we could start when we see the [cname] slice.)

     Additionally, we collect all known class IDs, so that other references
     to the class value can be type-checked individually.
   *)

  let slices = ref [] in

  let first_class_id, first_slice_len =
    unmarshal_class_slice_header env s cur end_pos in

  let cur_class_id = ref first_class_id in
  let cur_slice_len = ref first_slice_len in

  while not (CiHashtbl.mem sys#classes !cur_class_id) do
    let slice =
      `Opaque(!cur_class_id,
	      String.sub s !cur (!cur_slice_len - 4)) in

    cur := !cur + !cur_slice_len - 4;
    if !cur > end_pos then e_exceed 50;

    slices := slice :: !slices;

    let next_class_id, next_slice_len =
      unmarshal_class_slice_header env s cur end_pos in

    cur_class_id := next_class_id;
    cur_slice_len := next_slice_len
  done;

  let known_class_id_list = ref CiStrSet.empty in
  let effective_id = !cur_class_id in

  (* Now we have read a slice header with a known class ID. This is where we
     start decoding the class value.
   *)

  let cur_class = ref (Some (CiHashtbl.find sys#classes !cur_class_id)) in
  while !cur_class <> None do
    match !cur_class with
      | Some clas ->
	  if clas#name <> !cur_class_id then
	    raise(Unmarshal_error "Class definition mismatch");
	  known_class_id_list := CiStrSet.add !cur_class_id !known_class_id_list;
	  let p0 = !cur in
	  let v =
	    unmarshal_value env sys (TStruct clas#elements) s cur end_pos in
	  if !cur - p0 + 4 <> !cur_slice_len then
	    raise(Unmarshal_error "Bad slice length");
	  ( match v with
	      | VStruct vals ->
		  slices := (`Decoded(clas#name, vals)) :: !slices
	      | _ -> assert false
	  );
	  cur_class := clas#super;
	  if !cur_class <> None then (
	    let next_class_id, next_slice_len =
	      unmarshal_class_slice_header env s cur end_pos in
    	    cur_class_id := next_class_id;
	    cur_slice_len := next_slice_len
	  )
      | None -> assert false
  done;

  let ctor =
    try CiHashtbl.find sys#ctors effective_id
    with Not_found ->
      raise(Unmarshal_error("No object constructor found for class ID: " ^
			      effective_id)) in
  let sliceval =
    ( object
	method hydro_slices = !slices
	method hydro_effective_id = effective_id
      end ) in
  let objval = ctor sliceval in
  let cval = `Value objval in

  (* Run through cpatches and update: *)

  let l =
    try Hashtbl.find env.cpatches inst_id with Not_found -> [] in
  List.iter
    (fun (cname, cvalref) ->
       if not (CiStrSet.mem cname !known_class_id_list) then
	 raise(Unmarshal_error "Class value has unexpected type");
       cvalref := cval
    )
    l;
  Hashtbl.remove env.cpatches inst_id;

  (* Add to cmap: *)
  Hashtbl.add env.cmap inst_id (!known_class_id_list, objval);

  cval, !known_class_id_list



and unmarshal_class_slice_header env s cur end_pos =
  if !cur >= end_pos then e_exceed 60;
  let c = s.[ !cur ] in
  incr cur;
  let type_id =
    if c = '\000' then (
      let s = unmarshal_string s cur end_pos in
      let tid = !(env.next_type_id) in
      incr env.next_type_id;
      Hashtbl.add env.tmap tid s;
      s
    )
    else
      if c = '\001' then (
	let tid = unmarshal_size s cur end_pos in
	try
	  Hashtbl.find env.tmap tid
	with
	  | Not_found ->
	      raise(Unmarshal_error "Invalid type ID number")
      )
      else
	raise(Unmarshal_error "Invalid slice header") in

  if !cur+3 >= end_pos then e_exceed 61;
  let slice_len = read_int s !cur in
  cur := !cur+4;

  (type_id, slice_len)


let dbg_message_too_long s cur end_pos =
  prerr_endline "Uninterpreted rest: ";
  for k = !cur to end_pos - 1 do
    prerr_endline ("At " ^ string_of_int k ^ ": " ^ string_of_int(Char.code(s.[k])));
  done


let unmarshal_class_appendix env sys s cur end_pos =
  let n = ref (unmarshal_size s cur end_pos) in
  while !n <> 0 do
    for k = 1 to !n do
      if !cur+3 >= end_pos then e_exceed 70;
      let inst_id = read_int32 s !cur in
      cur := !cur+4;
      if inst_id <= 0l then
	raise(Unmarshal_error "Non-positive instance ID found in appendix");
      let _, _ =
	unmarshal_class_val env sys inst_id s cur end_pos in
      ()
    done;
    n := unmarshal_size s cur end_pos
  done;
  if Hashtbl.length env.cpatches > 0 then
    raise(Unmarshal_error "There are unresolved class value references")


let unmarshal sys ht class_flag eb =
  let nb = eb.encap_buf
  and pos = eb.encap_pos
  and len = eb.encap_len in

  let nb_len = Netbuffer.length nb in
  if pos < 0 || pos > nb_len || len < 0 || pos+len > nb_len then
    invalid_arg "Hydro_unmarshal.unmarshal: invalid substring";

  if eb.encap_enc_minor <> 0 then
    raise(Limitation `UnsupportedEncodingVersion);

  let env =
    { cmap = Hashtbl.create 10;
      cpatches = Hashtbl.create 10;
      tmap = Hashtbl.create 10;
      next_type_id = ref 1;
      em = eb.encap_enc_minor
    } in

  let s = Netbuffer.unsafe_buffer nb in
  let cur = ref pos in
  let end_pos = pos + len in

  let v =
    unmarshal_value env sys ht s cur end_pos in

  if class_flag then
    unmarshal_class_appendix env sys s cur end_pos
  else
    if Hashtbl.length env.cpatches > 0 then
      raise(Unmarshal_error "Class values found although class flag is false");

  if !cur <> end_pos then (
    dbg_message_too_long s cur end_pos;
    raise(Unmarshal_error "Message too long #1")
  );

  v


let unmarshal_exn_slice_header env s cur end_pos =
  let type_id = unmarshal_string s cur end_pos in
  if !cur+3 >= end_pos then e_exceed 80;
  let slice_len = read_int s !cur in
  cur := !cur+4;
  (type_id, slice_len)


let unmarshal_exn sys eb =
  let nb = eb.encap_buf
  and pos = eb.encap_pos
  and len = eb.encap_len in

  let nb_len = Netbuffer.length nb in
  if pos < 0 || pos > nb_len || len < 0 || pos+len > nb_len then
    invalid_arg "Hydro_unmarshal.unmarshal_exn: invalid substring";

  if eb.encap_enc_minor <> 0 then
    raise(Limitation `UnsupportedEncodingVersion);

  let s = Netbuffer.unsafe_buffer nb in
  let cur = ref pos in
  let end_pos = pos + len in

  let env =
    { cmap = Hashtbl.create 10;
      cpatches = Hashtbl.create 10;
      tmap = Hashtbl.create 10;
      next_type_id = ref 1;
      em = eb.encap_enc_minor
    } in

  if !cur >= end_pos then e_exceed 90;
  let has_class_values =
    ( match s.[ !cur ] with
	| '\000' -> incr cur; false
	| '\001' -> incr cur; true
	| _ -> raise(Unmarshal_error "Bool value out of range")
    ) in

  (* Now read the slices. There may be slices with known exception IDs and with
     unknown exn IDs. There _must_ be a slice with a known exception ID
     and following slices must match our own exception definition.

     If we don't find a known exception we run over the end of the slices.
     There is no safe way to prevent that. (No "catch all" exception!)
   *)

  let slices = ref [] in

  let first_exn_id, first_slice_len =
    unmarshal_exn_slice_header env s cur end_pos in

  let cur_exn_id = ref first_exn_id in
  let cur_slice_len = ref first_slice_len in

  while not (CiHashtbl.mem sys#exceptions !cur_exn_id) do
    let slice =
      `Opaque(!cur_exn_id,
	      String.sub s !cur (!cur_slice_len - 4)) in

    cur := !cur + !cur_slice_len - 4;
    if !cur > end_pos then e_exceed 91;

    slices := slice :: !slices;

    let next_exn_id, next_slice_len =
      unmarshal_exn_slice_header env s cur end_pos in

    cur_exn_id := next_exn_id;
    cur_slice_len := next_slice_len
  done;

  (* Start decoding the exn value *)

  let effective_id = !cur_exn_id in

  let cur_exn = ref (Some (CiHashtbl.find sys#exceptions !cur_exn_id)) in
  while !cur_exn <> None do
    match !cur_exn with
      | Some exn ->
	  if exn#name <> !cur_exn_id then
	    raise(Unmarshal_error "Exception definition mismatch");
	  let p0 = !cur in
	  let v =
	    unmarshal_value env sys (TStruct exn#elements) s cur end_pos in
	  if !cur - p0 + 4 <> !cur_slice_len then
	    raise(Unmarshal_error "Bad slice length");
	  ( match v with
	      | VStruct vals ->
		  slices := (`Decoded(exn#name, vals)) :: !slices
	      | _ -> assert false
	  );
	  cur_exn := exn#super;
	  if !cur_exn <> None then (
	    let next_exn_id, next_slice_len =
	      unmarshal_exn_slice_header env s cur end_pos in
    	    cur_exn_id := next_exn_id;
	    cur_slice_len := next_slice_len
	  )
      | None -> assert false
  done;

  if has_class_values then (
    unmarshal_class_appendix env sys s cur end_pos
  ) else (
    if Hashtbl.length env.cpatches > 0 then
      raise(Unmarshal_error "There are unresolved class value references")
  );

  if !cur <> end_pos then (
    dbg_message_too_long s cur end_pos;
    raise(Unmarshal_error "Message too long #2")
  );

  let slice_val =
    ( object
	method hydro_slices = !slices
	method hydro_effective_id = effective_id
      end
    ) in

  slice_val


let unmarshal_msg sys (hdr:msg_header) eb : msg =
  if hdr#proto_major <> 1 || hdr#proto_minor <> 0 then
    raise(Limitation `UnsupportedProtocolVersion);
  if hdr#enc_major <> 1 || hdr#enc_minor <> 0 then
    raise(Limitation `UnsupportedEncodingVersion);

  let nb = eb.encap_buf
  and pos = eb.encap_pos
  and len = eb.encap_len in

  let nb_len = Netbuffer.length nb in
  if pos < 0 || pos > nb_len || len < 0 || pos+len > nb_len then
    invalid_arg "Hydro_unmarshal.unmarshal_msg: invalid substring";

  if hdr#enc_minor <> eb.encap_enc_minor then
    invalid_arg "Hydro_unmarshal.unmarshal_msg: the encoding version of body does not match the version announced in the header";

  let env =
    { cmap = Hashtbl.create 10;
      cpatches = Hashtbl.create 10;
      tmap = Hashtbl.create 10;
      next_type_id = ref 1;
      em = eb.encap_enc_minor
    } in

  let s = Netbuffer.unsafe_buffer nb in
  let cur = ref pos in
  let end_pos = pos + len in

  let m =
    match hdr#msg_type with
      | `Request ->
	  let v =
	    unmarshal_value env sys t_msg_request s cur end_pos in
	  ( match v with
	      | VStruct [| VInt32 req_id;
			   VString id_name;
			   VString id_cat;
			   VSequence facet_seq;
			   VString op;
			   VByte mode_num;
			   VDictionary ctx
			|] ->
		  let (p_pos, p_len) = decapsulate_str s !cur (end_pos - !cur) in
		  cur := p_pos + p_len;

		  let id =
		    (object method name = id_name method category = id_cat
		     end) in
		  let facet = decode_facet facet_seq in
		  let mode = decode_mode mode_num in
		  let context = decode_context ctx in
		  let req =
		    ( object
			method request_id = req_id
			method id = id
			method facet = facet
			method operation = op
			method mode = mode
			method context = context
			method params =
			  { encap_buf = nb;
			    encap_pos = p_pos;
			    encap_len = p_len;
			    encap_enc_minor = eb.encap_enc_minor
			  }
		      end
		    ) in
		  `Request req

	      | _ -> assert false
	  )
      | `Batch_request ->
	  if !cur+3 >= end_pos then e_exceed 100;
	  let n = read_int s !cur in
	  cur := !cur+4;

	  let reqs = ref [] in
	  for k = 1 to n do
	    let v =
	      unmarshal_value env sys t_msg_batch_request s cur end_pos in
	    ( match v with
		| VStruct [| VString id_name;
			     VString id_cat;
			     VSequence facet_seq;
			     VString op;
			     VByte mode_num;
			     VDictionary ctx
			  |] ->
		    let (p_pos, p_len) =
		      decapsulate_str s !cur (end_pos - !cur) in
		    cur := p_pos + p_len;

		    let id =
		      (object method name = id_name method category = id_cat
		       end) in
		    let facet = decode_facet facet_seq in
		    let mode = decode_mode mode_num in
		    let context = decode_context ctx in
		    let req =
		      ( object
			  method id = id
			  method facet = facet
			  method operation = op
			  method mode = mode
			  method context = context
			  method params =
			    { encap_buf = nb;
			      encap_pos = p_pos;
			      encap_len = p_len;
			      encap_enc_minor = eb.encap_enc_minor
			    }
			end
		      ) in
		    reqs := req :: !reqs

		| _ -> assert false
	    )
	  done;
	  `Batch_request (List.rev !reqs)

      | `Reply ->
	  let v =
	    unmarshal_value env sys t_msg_reply s cur end_pos in
	  ( match v with
	      | VStruct [| VInt32 req_id;
			   VByte reply_type
			|] ->
		  (* The Ice manual says there is always an encapsulation.
                     However, this seems not to be true for reply types
                     >= 2
                   *)
		  let result =
		    ( match reply_type with
			| 0 ->
			    let (p_pos, p_len) =
			      decapsulate_str s !cur (end_pos - !cur) in
			    cur := p_pos + p_len;
			    `Success
			      { encap_buf = nb;
				encap_pos = p_pos;
				encap_len = p_len;
				encap_enc_minor = eb.encap_enc_minor
			      }
			| 1 ->
			    let (p_pos, p_len) =
			      decapsulate_str s !cur (end_pos - !cur) in
			    cur := p_pos + p_len;
			    `User_exception
			      { encap_buf = nb;
				encap_pos = p_pos;
				encap_len = p_len;
				encap_enc_minor = eb.encap_enc_minor
			      }
			| 2 | 3 | 4 ->
			    let w =
			      unmarshal_value
				env sys t_msg_reply_error s cur end_pos in
			    if !cur <> end_pos then
			      raise(Unmarshal_error "Illegal reply");
			    let (e_id, e_facet, e_op) =
			      match w with
				| VStruct [| VString id_name;
					     VString id_cat;
					     VSequence facet_seq;
					     VString op
					  |] ->
				    let id =
				      (object
					 method name = id_name
					 method category = id_cat
				       end) in
				    (id, decode_facet facet_seq, op)
 				| _ -> assert false
			    in
			    ( match reply_type with
				| 2 -> `Object_does_not_exist(e_id,e_facet,e_op)
				| 3 -> `Facet_does_not_exist(e_id,e_facet,e_op)
				| 4 -> `Operation_does_not_exist(e_id,e_facet,e_op)
				| _ -> assert false
			    )

			| 5 | 6 | 7 ->
			    let e = unmarshal_string s cur end_pos in
			    if !cur <> end_pos then
			      raise(Unmarshal_error "Illegal reply");
			    ( match reply_type with
				| 5 -> `Unknown_local_exception e
				| 6 -> `Unknown_user_exception e
				| 7 -> `Unknown_exception e
				| _ -> assert false
			    )
			| _ ->
			    raise (Unmarshal_error "Unknown type of reply")
		    ) in
		  `Reply
		    ( object
			method request_id = req_id
			method result = result
		      end
		    )
	      | _ -> assert false
	  )

      | `Validate_connection -> `Validate_connection
      | `Close_connection -> `Close_connection
  in

  if !cur <> end_pos then (
    dbg_message_too_long s cur end_pos;
    raise(Unmarshal_error "Message too long #3")
  );
  m
