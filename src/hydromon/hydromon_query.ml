open Hydromon_proto
open Hydromon_util
open Printf

exception No_result of string



type ba =
    (char,Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array1.t

type cache =
    { proxy : po_Hydro_Mon;
      period : int;
      mutable shm_opt : ba option;
      alive : (key,t_Hydro_monitoredObject) Hashtbl.t
    }

let ignoring_handler : Hydro_types.exn_handler =
  ( object
      method handle _ = ()
    end
  )


let create_cache ~port ~period () = 
  let esys = Unixqueue.create_unix_event_system() in
  let sys = Hydro_lm.create_system() in
  fill_system sys;
  let cp = 
    Hydro_params.client_params
      ~msg_timeout:5.0           (* that has to be quick *)
      ~exception_handler:ignoring_handler
      () in
  let res = Hydro_proxy.proxy_resolver cp in
  let pool = Hydro_proxy.pool() in
  let pconf = Hydro_proxy.proxy_conf() in
  let pe = 
    ( object
	method event_system = esys
	method system = sys
	method proxy_resolver = res
	method client_pool = pool
	method default_proxy_conf = pconf
      end
    ) in
  let ps = sprintf "hydromon -t : tcp -h 127.0.0.1 -p %d" port in
  let pr = unchecked_pr_Hydro_Mon (Hydro_lm.pr_of_string ps) in
  let proxy = pc_Hydro_Mon pe pr in
  { proxy = proxy;
    period = period;
    shm_opt = None;
    alive = Hashtbl.create 100
  }


let get_shm cache name =
  match cache.shm_opt with
    | Some shm ->
	shm
    | None ->
	let fd = Netsys.shm_open name [Netsys.SHM_O_RDWR] 0 in
	let st = Unix.fstat fd in
	let size = st.Unix.st_size in
	let shm =
	  Bigarray.Array1.map_file 
	    fd Bigarray.char Bigarray.c_layout true size in
	Unix.close fd;
	cache.shm_opt <- Some shm;
	shm


let check_key ~cache ~key ~operation ~idempotent () = 
  (* First check whether we have that object in our cache *)
  let now = Int64.of_float (Unix.time()) in
  try
    let monobj = Hashtbl.find cache.alive key in (* or Not_found *)
    if now >= Int64.pred monobj.m_endTimestamp then (
      Hashtbl.remove cache.alive key;
      raise Not_found;    (* invalid or close to becoming it *)
    );
    let shm = get_shm cache monobj.m_shmName in
    let idx = Int32.to_int (monobj.m_shmIndex) in
    if idx < 0 || idx >= Bigarray.Array1.dim shm then (    (* bad index *)
      Hashtbl.remove cache.alive key;
      cache.shm_opt <- None;   (* could have been changed in size *)
      raise Not_found;
    );
    Char.code shm.{ idx }
  with
    | Not_found ->
	( try
	    (* Request the object *)
	    let t = Int64.add now (Int64.of_int cache.period) in
	    let target_pr = pr_of_key key in
	    let monobj =
	      ( cache.proxy # requestMonitoring 
		  (Some target_pr) operation idempotent t
	      ) # scall # result in
	    cache.proxy # hydro_env # client_pool # shutdown();
	    Hashtbl.replace cache.alive key monobj;
	    Int32.to_int (monobj.m_deadPings)
	  with
	    | User_exception ue ->
		( match ue # hydro_ops # exn_name with
		    | `Hydro_TooManyMonitoredObjects ->
			raise (No_result "Too many monitored objects")
		    | `Hydro_Error ->
			raise (No_result 
				 ("Error: " ^ 
				    ue # hydro_ops # as_Hydro_Error # msg))
		)
	    | e ->
		raise (No_result (Hydro_util.exn_to_string e))
	)



let check_object ~cache ~monobj ~operation ~idempotent () = 
  let pr = Hydro_lm.pr_of_string monobj in
  let key = key_of_pr pr operation idempotent in
  check_key ~cache ~key ~operation ~idempotent ()


let proxy_monitor cache operation idempotent th d proxy mc =
  try
    let (h,p) =
      match mc#host_port with
	| None -> raise Not_found   (* cannot do anything without address *)
	| Some (h,p) -> (h,p) in
    let id = proxy # hydro_id in
    let key =
      { host = h;
	port = p;
	id_name = id#name;
	id_cat = id#category;
	operation = operation;
	idempotent = idempotent
      } in
    let is_dead =
      check_key ~cache ~key ~operation ~idempotent () >= th in
    if is_dead && d then (
      let pool = proxy # hydro_env # client_pool in
      let conf = proxy # hydro_proxy_conf in
      pool # deactivate_port (`TCP(h,p)) conf#deactivation_period
    );
    not is_dead
  with
    | _ ->
	true   (* don't know, so let's try *)


let configure_proxy ~cache ~(proxy:Hydro_proxy.proxy_t) ~operation 
                    ~idempotent ?(threshold=1) 
                    ?(deactivate=false) () =
  proxy # hydro_set_monitor 
    (proxy_monitor cache operation idempotent threshold deactivate)

  
