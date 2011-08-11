(* $Id$ *)

open Hydro_types
open Hydro_util
open Hydro_prelim

let patch_buffer b pos s =
  (* TODO: functions to overwrite are missing in Netbuffer *)
  let bs = Netbuffer.unsafe_buffer b in
  String.blit s 0 bs pos (String.length s)


let print_short b x =
  let n0 = x land 0xff in
  Netbuffer.add_char b (Char.unsafe_chr n0);
  let n1 = (x asr 8) land 0xff in
  Netbuffer.add_char b (Char.unsafe_chr n1)


let encoded_int x =
  let s = String.create 4 in
  let n3 = (x asr 24) land 0xff in
  String.unsafe_set s 3 (Char.unsafe_chr n3);
  let n2 = (x lsr 16) land 0xff in
  String.unsafe_set s 2 (Char.unsafe_chr n2);
  let n1 = (x lsr 8) land 0xff in
  String.unsafe_set s 1 (Char.unsafe_chr n1);
  let n0 = x land 0xff in
  String.unsafe_set s 0 (Char.unsafe_chr n0);
  s


let print_bool b x =
  Netbuffer.add_char b (if x then '\001' else '\000')


let print_byte b x =
  Netbuffer.add_char b (Char.unsafe_chr (x land 0xff))


let print_int b x =
  let n0 = x land 0xff in
  Netbuffer.add_char b (Char.unsafe_chr n0);
  let n1 = (x lsr 8) land 0xff in
  Netbuffer.add_char b (Char.unsafe_chr n1);
  let n2 = (x lsr 16) land 0xff in
  Netbuffer.add_char b (Char.unsafe_chr n2);
  let n3 = (x asr 24) land 0xff in
  Netbuffer.add_char b (Char.unsafe_chr n3)


let print_int32 b x =
  let n0 = Int32.to_int (Int32.logand x 0xffl) in
  Netbuffer.add_char b (Char.unsafe_chr n0);
  let n1 = Int32.to_int (Int32.shift_right_logical x 8) land 0xff in
  Netbuffer.add_char b (Char.unsafe_chr n1);
  let n2 = Int32.to_int (Int32.shift_right_logical x 16) land 0xff in
  Netbuffer.add_char b (Char.unsafe_chr n2);
  let n3 = Int32.to_int (Int32.shift_right_logical x 24) land 0xff in
  Netbuffer.add_char b (Char.unsafe_chr n3)


let print_int64 b x =
  let n0 = Int64.to_int (Int64.logand x 0xffL) in
  Netbuffer.add_char b (Char.unsafe_chr n0);
  let n1 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 8)
                           0xffL) in
  Netbuffer.add_char b (Char.unsafe_chr n1);
  let n2 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 16)
                           0xffL) in
  Netbuffer.add_char b (Char.unsafe_chr n2);
  let n3 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 24)
                           0xffL) in
  Netbuffer.add_char b (Char.unsafe_chr n3);
  let n4 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 32)
                           0xffL) in
  Netbuffer.add_char b (Char.unsafe_chr n4);
  let n5 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 40)
                           0xffL) in
  Netbuffer.add_char b (Char.unsafe_chr n5);
  let n6 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 48)
                           0xffL) in
  Netbuffer.add_char b (Char.unsafe_chr n6);
  let n7 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 56)
                           0xffL) in
  Netbuffer.add_char b (Char.unsafe_chr n7)


let print_float b x =
  print_int32 b (Int32.bits_of_float x)


let print_double b x =
  print_int64 b (Int64.bits_of_float x)


let print_size b n =
  if n < 255 then
    Netbuffer.add_char b (Char.unsafe_chr n)
  else (
    Netbuffer.add_char b '\255';
    print_int b n
  )


let print_string b s =
  print_size b (String.length s);
  Netbuffer.add_string b s


let encapsulate f b e =
  let p = Netbuffer.length b in
  Netbuffer.add_string b "\000\000\000\000\001";
  Netbuffer.add_char b (Char.chr e);
  f b e;
  let p' = Netbuffer.length b in
  let delta = encoded_int (p' - p) in
  patch_buffer b p delta

let encapsulate2 b1 eb2 =
  (* Add the encapsulation header for existing eb2 to the end of b1 *)
  let l = eb2.encap_len + 6 in
  print_int b1 l;
  Netbuffer.add_string b1 "\001";
  Netbuffer.add_char b1 (Char.chr eb2.encap_enc_minor);
  { encap_buf = b1;
    encap_pos = 0;
    encap_len = Netbuffer.length b1;
    encap_enc_minor = eb2.encap_enc_minor
  }

let marshal_sequence m b seq =
  print_size b (Array.length seq);
  Array.iter
    (fun v' -> m b v')
    seq

let marshal_dictionary m1 m2 b seq =
  print_size b (Array.length seq);
  Array.iter
    (fun (v1,v2) -> 
       m1 b v1;
       m2 b v2
    )
    seq

let marshal_enum n b k =
  if k < 0 || k >= n then
    raise(Marshal_error "Enum out of range");
  if n <= 127 then
    Netbuffer.add_char b (Char.unsafe_chr (k land 0xff))
  else
    if n <= 32767 then
      print_short b k
    else
      print_int b k
	

type marshal_env =
    { cmap : (int, string * object_value * int) Hashtbl.t;
        (* Maps id to (class_name, class_value, inst_id) where
           - class_name is the absolute type ID of the class,
           - class_value is the object so that [Oo.id class_value = id]
           - inst_id is the instance ID used in the serialized string
         *)
      cq : int Queue.t;
        (* The queue of class value IDs that needs to be appended *)
      next_inst_id : int ref;
        (* The next instance ID to allocate *)
      tmap : int CiHashtbl.t;
        (* Maps absolute class type IDs to numerical values *)
      next_type_id : int ref;
        (* The next type ID to allocate *)
      em : int;
        (* Minor encoding version *)
    }

let e_marshal() =
  raise(Marshal_error "Value does not match type")

let rec marshal_value env (sys:system) ht b v =
  match v with
    | VDirectWriter f ->
	f b
    | VNothing ->
	( match ht with
	    | TVoid -> ()
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VBool flag ->
	( match ht with
	    | TBool -> 
		print_bool b flag
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VByte n ->
	( match ht with
	    | TByte -> 
		print_byte b n
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VShort n ->
	( match ht with
	    | TShort ->
		print_short b n
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VInt32 n ->
	( match ht with
	    | TInt | TInt32 ->
		print_int32 b n
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VInt n ->
	(* CHECK: Marshalling error if n out of range on 64 bit platforms? *)
	( match ht with
	    | TInt | TInt32 ->
		print_int b n
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VLong n ->
	( match ht with
	    | TLong ->
		print_int64 b n
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VFloat x ->
	( match ht with
	    | TFloat ->
		print_float b x
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VDouble x ->
	( match ht with
	    | TDouble ->
		print_double b x
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VString s ->
	( match ht with
	    | TString ->
		print_string b s
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VByteseq s ->
	( match ht with
	    | TByteseq | TSequence TByte ->
		print_string b s
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VSequence seq ->
	( match ht with
	    | TSequence ht' ->
		marshal_sequence (marshal_value env sys ht') b seq
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VDictionary seq ->
	( match ht with
	    | TDictionary(ht1,ht2) ->
		marshal_dictionary
		  (marshal_value env sys ht1)
		  (marshal_value env sys ht2)
		  b seq
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VEnum k ->
	( match ht with
	    | TEnum e ->
		let n = Array.length e in
		marshal_enum n b k
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VStruct s ->
	( match ht with
	    | TStruct st ->
		if Array.length st <> Array.length s then
		  raise(Marshal_error "Struct does not match definition");
		Array.iteri
		  (fun k v' ->
		     let (_, ht') = st.(k) in
		     marshal_value env sys ht' b v'
		  )
		  s
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> e_marshal()
	)
    | VClass cvalref ->
	( match ht with
	    | TClass cname ->
		marshal_class env sys cname cvalref b
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> 
		e_marshal()
	)
    | VProxy p ->
	( match ht with
	    | TProxy name ->
		marshal_proxy env sys name p b
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ ->
		e_marshal()
	)

    | VNull ->
	( match ht with
	    | TClass _ ->
		print_int b 0
	    | TProxy _ ->
		print_size b 0;  (* empty name *)
		print_size b 0   (* empty category *)
	    | TDirectMapping(ht',_,_) ->
		marshal_value  env sys ht' b v
	    | _ -> 
		e_marshal()
	)
    | VDirectMapping e ->
	( match ht with
	    | TDirectMapping(ht',marshal,_) ->
		marshal b e
	    | _ ->
		e_marshal()
	)

and marshal_class env (sys:system) cname cvalref b =
  match !cvalref with
    | `Placeholder _ ->
	raise(Marshal_error "Cannot marshal class placeholder")
    | `Value cval ->
	let id = Oo.id cval in
	try
	  let (_, _, inst_id) = Hashtbl.find env.cmap id in
	  (* The class value is already known, and is either
             already marshalled, or scheduled for marshalling
	   *)
	  print_int b (-inst_id)
	with
	  | Not_found ->
	      (* The class value was not seen before *)
	      let inst_id = !(env.next_inst_id) in
	      incr env.next_inst_id;
	      Hashtbl.add env.cmap id (cname, cval, inst_id);
	      Queue.add id env.cq;
	      print_int b (-inst_id)

and marshal_proxy env (sys:system) name p b =
  if not (CiHashtbl.mem sys#interfaces name) then
    raise(Marshal_error("Proxy not found: " ^ name));
  if name = "" then
    raise(Marshal_error("Cannot marshal proxies with empty name"));
  marshal_value env sys t_proxy b
    (VStruct
       [| VString p#id#name;
	  VString p#id#category;
	  VSequence (match p#facet with
		       | None -> [| |]
		       | Some fc -> [| VString fc |]
		    );
	  VByte (match p#mode with
		   | `Twoway -> 0
		   | `Oneway -> 1
		   | `Batch_oneway -> 2
		   | `Datagram -> 3
		   | `Batch_datagram -> 4
		);
	  VBool p#secure;
       |]
    );
  ( match p#parameters with
      | `Endpoints eps ->
	  if Array.length eps = 0 then
	    raise(Marshal_error "A proxy with endpoints must have at least one endpoint");
	  print_size b (Array.length eps);
	  Array.iter
	    (fun ep ->
	       (* Note: The spec says that the endpoints are in their
                  own encapsulations. As the endpoint encoding does
                  not use class values, we can reuse [env].
		*)
	       match ep with
		 | `TCP ep ->
		     print_short b 1;
		     encapsulate
		       (fun b _ ->
			  marshal_value env sys t_tcp_endpoint b
			    (VStruct [| VString ep#host;
					VInt ep#port;
					VInt32 ep#timeout;
					VBool ep#compress
				     |])
		       )
		       b
		       env.em
		 | `UDP ep ->
		     print_short b 2;
		     encapsulate
		       (fun b _ ->
			  marshal_value env sys t_udp_endpoint b
			    (VStruct [| VString ep#host;
					VInt ep#port;
					VInt ep#proto_major;
					VInt ep#proto_minor;
					VInt ep#enc_major;
					VInt ep#enc_minor;
					VBool ep#compress
				     |])
		       )
		       b
		       env.em
		 | `SSL ep ->
		     print_short b 3;
		     encapsulate
		       (fun b _ ->
			  marshal_value env sys t_tcp_endpoint b
			    (VStruct [| VString ep#host;
					VInt ep#port;
					VInt32 ep#timeout;
					VBool ep#compress
				     |])
		       )
		       b
		       env.em
		 | `Unknown(n,s) ->
		     print_short b n;
		     encapsulate 
		       (fun b _ -> Netbuffer.add_string b s) b env.em
	    )
	    eps;
      | `Well_known ->
	  print_size b 0;
	  print_size b 0;
      | `Adapter s ->
	  print_size b 0;
	  print_size b (String.length s);
	  Netbuffer.add_string b s
  )
    


let marshal_type_id env type_id b =
  try
    let num_id = CiHashtbl.find env.tmap type_id in
    Netbuffer.add_char b '\001';
    print_size b num_id
  with Not_found ->
    let num_id = !(env.next_type_id) in
    incr env.next_type_id;
    CiHashtbl.add env.tmap type_id num_id;
    Netbuffer.add_char b '\000';
    print_size b (String.length type_id);
    Netbuffer.add_string b type_id


let marshal_class_val env sys cname cval inst_id b =
  let cdef =
    try CiHashtbl.find sys#classes cname
    with Not_found -> raise(Marshal_error("Class not found: " ^ cname)) in
  print_int b inst_id;
  
  (* Iterate over the slices: *)
  let slices = ref (List.rev (cval#hydro_slices)) in
  let next_class = ref (Some cdef) in

  while !next_class <> None do
    match !next_class with
      | Some next_cdef ->
	  ( match !slices with
	      | (`Decoded(type_id, sval)) :: slices' ->
		   if type_id <> next_cdef#name then
		     raise(Marshal_error("Class definition mismatch"));
   		   marshal_type_id env type_id b;
		   let p = Netbuffer.length b in
		   Netbuffer.add_string b "\000\000\000\000";
		   marshal_value 
		     env sys (TStruct next_cdef#elements) b (VStruct sval);
		   let p' = Netbuffer.length b in
		   let delta = encoded_int (p' - p) in
		   patch_buffer b p delta;

		   slices := slices'

	      | (`Opaque(type_id, data)) :: slices' ->
		   if type_id <> next_cdef#name then
		     raise(Marshal_error("Class definition mismatch"));
   		   marshal_type_id env type_id b;
		   let p = Netbuffer.length b in
		   Netbuffer.add_string b "\000\000\000\000";
		   Netbuffer.add_string b data;
		   let p' = Netbuffer.length b in
		   let delta = encoded_int (p' - p) in
		   patch_buffer b p delta;

		   slices := slices'

	      | [] ->
		  raise(Marshal_error("Missing slice to marshal class value"))
	  );
	  next_class := next_cdef # super
      | _ -> assert false
  done


let marshal_class_appendix env sys b =
  let cq' = Queue.create() in
  while not (Queue.is_empty env.cq) do
    let n = Queue.length env.cq in
    print_size b n;
    
    Queue.transfer env.cq cq';
    while not (Queue.is_empty cq') do
      let id = Queue.pop cq' in
      let (cname, cval, inst_id) = 
	try Hashtbl.find env.cmap id with Not_found -> assert false in
      marshal_class_val env sys cname cval inst_id b;
    done
  done;
  print_size b 0


let marshal sys ht class_flag v b em =
  if em < 0 then
    raise(Limitation `UnsupportedEncodingVersion);
  let em = 0 in  (* We can only do version 0! *)
  let cq = Queue.create() in
  let env =
    { cmap = Hashtbl.create 10;
      cq = cq;
      next_inst_id = ref 1;
      tmap = CiHashtbl.create 10;
      next_type_id = ref 1;
      em = em
    } in

  marshal_value env sys ht b v;

  if class_flag then
    marshal_class_appendix env sys b
  else
    if not (Queue.is_empty cq) then
      raise(Marshal_error("Classes found although the class flag is false"))


let marshal_exn sys hx (sv:sliced_value) b em =
  if em < 0 then
    raise(Limitation `UnsupportedEncodingVersion);
  let em = 0 in  (* We can only do version 0! *)
  let p_exn = Netbuffer.length b in
  print_size b 0;

  let cq = Queue.create() in
  let env =
    { cmap = Hashtbl.create 10;
      cq = cq;
      next_inst_id = ref 1;
      tmap = CiHashtbl.create 10;
      next_type_id = ref 1;
      em = em
    } in

  (* Iterate over the slices: *)
  let slices = List.rev sv#hydro_slices in
  let prev_eff_exn = ref None in
  let eff_type_found = ref false in
  let static_type_found = ref false in

  List.iter
    (fun slice ->
       match slice with
	 | `Decoded(type_id, sval) ->
	     (* Check value: *)
	     let eff_x =
	       try CiHashtbl.find sys#exceptions type_id
	       with Not_found ->
		 raise(Marshal_error("Exception ID not found: " ^ type_id)) in
	     if eff_x#name = hx#name then
	       static_type_found := true;
	     if not !eff_type_found then (
	       if type_id <> sv#hydro_effective_id then
		 raise(Marshal_error("Bad effective type of exception"));
	       eff_type_found := true
	     );
	     ( match !prev_eff_exn with
		 | None -> ()
		 | Some p_eff_x ->
		     let ok =
		       match p_eff_x#super with
			 | None -> false
			 | Some px -> px#name = type_id in
		     if not ok then
		       raise(Marshal_error("Exception definition mismatch"))
	     );
	     prev_eff_exn := Some eff_x;
	     (* Marshal value: *)
	     print_size b (String.length type_id);
	     Netbuffer.add_string b type_id;
	     let p = Netbuffer.length b in
	     Netbuffer.add_string b "\000\000\000\000";
	     marshal_value 
	       env sys (TStruct eff_x#elements) b (VStruct sval);
	     let p' = Netbuffer.length b in
	     let delta = encoded_int (p' - p) in
	     patch_buffer b p delta;

	 | `Opaque(type_id, data) ->
	     (* Check value: *)
	     if !eff_type_found then
	       raise(Marshal_error("Exception contains opaque data at wrong place"));
	     (* Marshal value: *)
	     print_size b (String.length type_id);
	     Netbuffer.add_string b type_id;
	     let p = Netbuffer.length b in
	     Netbuffer.add_string b "\000\000\000\000";
	     Netbuffer.add_string b data;
	     let p' = Netbuffer.length b in
	     let delta = encoded_int (p' - p) in
	     patch_buffer b p delta;
    )
    slices;

  (* Further checks: *)
  if not !eff_type_found then
    raise(Marshal_error "Bad effective type of exception");
  if not !static_type_found then
    raise(Marshal_error "Effective type of exception does not match static type");
  ( match !prev_eff_exn with
      | None -> assert false
      | Some last_exn ->
	  if last_exn#super <> None then
	    raise(Marshal_error("Exception definition mismatch"))
  );

  if not (Queue.is_empty cq) then (
    (* If no class values have been found, we don't have to marshal them *)
    (* CHECK whether this is ok! *)
    patch_buffer b p_exn "\001";
    marshal_class_appendix env sys b
  )


let make_header zstatus mt len pm em : msg_header =
  ( object
      method proto_major = 1
      method proto_minor = pm
      method enc_major = 1
      method enc_minor = em
      method msg_type = mt
      method compression = zstatus
      method body_size = len
    end
  )


let marshal_msg sys zstatus (msg:msg) pm em =
  if pm < 0 then
    raise(Limitation `UnsupportedProtocolVersion);
  if em < 0 then
    raise(Limitation `UnsupportedEncodingVersion);
  let pm = 0 in  (* We can only do version 0! *)
  let em = 0 in  (* We can only do version 0! *)
  match msg with
    | `Request req ->
	let v = 
	  VStruct [| VInt32 req#request_id;
		     VString req#id#name;
		     VString req#id#category;
		     VSequence ( match req#facet with
				   | None -> [| |]
				   | Some fc -> [| VString fc |]
			       );
		     VString req#operation;
		     VByte (match req#mode with
			      | `Normal -> 0
			      | `Nonmutating -> 1
			      | `Idempotent -> 2
			   );
		     VDictionary (Array.map 
				    (fun (k,v) -> (VString k, VString v))
				    req#context
				 );
		  |] in
	let b1 = Netbuffer.create 200 in
	marshal sys t_msg_request false v b1 em;
	let eb2 = req#params in
	let eb1 = encapsulate2 b1 eb2 in
	let total_len = eb1.encap_len + eb2.encap_len in
	let hdr = make_header zstatus `Request total_len pm em in
	(hdr, [ eb1; eb2 ] )

    | `Batch_request reqs ->
	let b0 = Netbuffer.create 10 in
	print_int b0 (List.length reqs);
	let bufs =
	  List.flatten
	    (List.map
	       (fun req ->
		  let dict =
		    Array.map 
		      (fun (k,v) -> (VString k, VString v))
		      req#context in
		  let v = 
		    VStruct [| VString req#id#name;
			       VString req#id#category;
			       VSequence ( match req#facet with
					     | None -> [| |]
					     | Some fc -> [| VString fc |]
					 );
			       VString req#operation;
			       VByte (match req#mode with
					| `Normal -> 0
					| `Nonmutating -> 1
					| `Idempotent -> 2
				     );
			       VDictionary dict;
			    |] in
		  let b1 = Netbuffer.create 200 in
		  marshal sys t_msg_batch_request false v b1 em;
		  let eb2 = req#params in
		  let eb1 = encapsulate2 b1 eb2 in
		  [ eb1; eb2 ]
	       )
	       reqs
	    ) in
	let eb0 = { encap_buf = b0; 
		    encap_pos = 0; 
		    encap_len = Netbuffer.length b0;
		    encap_enc_minor = em
		  } in
	let all_bufs = eb0 :: bufs in
	let total_len =
	  List.fold_left
	    (fun acc eb -> acc + eb.encap_len)
	    0
	    all_bufs in
	let hdr = make_header zstatus `Batch_request total_len pm em in
	(hdr, all_bufs)

    | `Reply rpy ->
	let v = 
	  VStruct [| VInt32 rpy#request_id;
		     VByte (match rpy#result with
			      | `Success _ -> 0
			      | `User_exception _ -> 1
			      | `Object_does_not_exist _ -> 2
			      | `Facet_does_not_exist _ -> 3
			      | `Operation_does_not_exist _ -> 4
			      | `Unknown_local_exception _ -> 5
			      | `Unknown_user_exception  _ -> 6
			      | `Unknown_exception _ -> 7
			   );
		  |] in
	let b1 = Netbuffer.create 200 in
	marshal sys t_msg_reply false v b1 em;
	let eb2_opt =
	  match rpy#result with
	    | `Success eb2 
	    | `User_exception eb2 ->
		Some eb2
 	  (* The Ice manual says there is always an encapsulation.
             However, this seems not to be true for reply types
             >= 2
           *)
	    | `Object_does_not_exist(id, facet, op)
	    | `Facet_does_not_exist(id, facet, op)
	    | `Operation_does_not_exist(id, facet, op) ->
		let e = VStruct [| VString id#name;
				   VString id#category;
				   VSequence ( match facet with
						 | None -> [| |]
						 | Some fc -> [| VString fc |]
					     );
				   VString op
				|] in
		marshal sys t_msg_reply_error false e b1 em;
		None
	    | `Unknown_local_exception s 
	    | `Unknown_user_exception s
	    | `Unknown_exception s ->
		marshal sys TString false (VString s) b1 em;
		None
	in
	( match eb2_opt with
	    | Some eb2 ->
		let eb1 = encapsulate2 b1 eb2 in
		let total_len = eb1.encap_len + eb2.encap_len in
		let hdr = make_header zstatus `Reply total_len pm em in
		(hdr, [ eb1; eb2 ])
	    | None ->
		let eb1 = 
		  { encap_buf = b1;
		    encap_pos = 0;
		    encap_len = Netbuffer.length b1;
		    encap_enc_minor = em
		  } in
		let total_len = eb1.encap_len in
		let hdr = make_header zstatus `Reply total_len pm em in
		(hdr, [ eb1 ])
	)

    | `Validate_connection ->
	let hdr = make_header `Uncompressed `Validate_connection 0 pm em in
	(hdr, [] )

    | `Close_connection ->
	let hdr = make_header `Uncompressed `Close_connection 0 pm em in
	(hdr, [] )


let max_proto_minor = 0
let max_enc_minor = 0
