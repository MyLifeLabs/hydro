open Hydro_types
open Hydro_util

let default_exception_handler =
  ( object
      method handle e =
	prerr_endline("Uncaught exception: " ^ Printexc.to_string e)
    end
  )


let client_params
      ?(trans_timeout = -1.0)
      ?(msg_timeout = -1.0)
      ?(idle_timeout = -1.0)
      ?(exception_handler = default_exception_handler)
      ?max_proto_minor
      ?max_enc_minor
      () : client_params =
  ( object
      method trans_timeout = trans_timeout
      method msg_timeout = msg_timeout
      method idle_timeout = idle_timeout
      method exception_handler = exception_handler
      method max_proto_minor = max_proto_minor
      method max_enc_minor = max_enc_minor
    end
  )



let upd m x =
  match m with
    | None -> x
    | Some y -> y



let update_client_params
      ?trans_timeout ?msg_timeout ?idle_timeout ?exception_handler
      ?max_proto_minor ?max_enc_minor cp =
  let eff_trans_timeout = upd trans_timeout cp#trans_timeout in
  let eff_msg_timeout = upd msg_timeout cp#msg_timeout in
  let eff_idle_timeout = upd idle_timeout cp#idle_timeout in
  let eff_exception_handler = upd exception_handler cp#exception_handler in
  let eff_max_proto_minor = upd max_proto_minor cp#max_proto_minor in
  let eff_max_enc_minor = upd max_enc_minor cp#max_enc_minor in
  ( object
      method trans_timeout = eff_trans_timeout
      method msg_timeout = eff_msg_timeout
      method idle_timeout = eff_idle_timeout
      method exception_handler = eff_exception_handler
      method max_proto_minor = eff_max_proto_minor
      method max_enc_minor = eff_max_enc_minor
    end
  )


let update_client_params_by_endpoint ep cp =
  match ep with
    | `TCP tcp ->
	let tmo =
	  if tcp#timeout < 0l then
	    cp#trans_timeout
	  else
	    let tcptmo = 0.001 *. (Int32.to_float tcp#timeout) in
	    if cp#trans_timeout < 0.0 then
	      tcptmo
	    else
	      min cp#trans_timeout tcptmo in
	update_client_params
	  ~trans_timeout:tmo
	  cp
    | `UDP udp ->
	update_client_params
	  ~max_proto_minor:(Some udp#proto_minor)
	  ~max_enc_minor:(Some udp#enc_minor)
	  cp
    | _ ->
	cp



let call_params ?msg_timeout ?destination ?context () : call_params =
  ( object
      method msg_timeout = msg_timeout
      method destination = destination
      method context = context
    end
  )

let oupd m x =
  match m with
    | None -> x
    | Some y -> m


let update_call_params ?msg_timeout ?destination ?context cp : call_params =
  let eff_msg_timeout = oupd msg_timeout cp#msg_timeout in
  let eff_destination = oupd destination cp#destination in
  let eff_context = oupd context cp#context in
  ( object
      method msg_timeout = eff_msg_timeout
      method destination = eff_destination
      method context = eff_context
    end
  )


let client_params_cmp (cl_params1:client_params)
                      (cl_params2:client_params) =
  or_cmp
    (float_cmp
       cl_params1#trans_timeout cl_params2#trans_timeout)
    (lazy
       (or_cmp
          (float_cmp
	     cl_params1#msg_timeout cl_params2#msg_timeout)
          (lazy
	     (or_cmp
		(float_cmp
		   cl_params1#idle_timeout cl_params2#idle_timeout)
		(lazy
		   (or_cmp
		      (int_opt_cmp
			 cl_params1#max_proto_minor
			 cl_params2#max_proto_minor)
		      (lazy
			 (or_cmp
			    (int_opt_cmp
			        cl_params1#max_enc_minor
				cl_params2#max_enc_minor)
			    (lazy
			       (Oo.id cl_params1#exception_handler -
				  Oo.id cl_params2#exception_handler))))))))))


let server_params
      ?(trans_timeout = -1.0)
      () : server_params =
  ( object
      method trans_timeout = trans_timeout
    end
  )

let update_server_params
      ?trans_timeout
      sp =
  let eff_trans_timeout = upd trans_timeout sp#trans_timeout in
  ( object
      method trans_timeout = eff_trans_timeout
    end
  )


