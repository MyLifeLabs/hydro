#use "topfind";;
#require "unix,equeue,netstring";;
#load "hydro.cma";;
open Hydro_types;;

Hydro_client.debug := true;;

let esys = Unixqueue.create_unix_event_system();;

let locator = Hydro_locator.get_Ice_Locator_of_port "gate" 9018;;

let resolver = 
  Hydro_locator.proxy_resolver
    (Hydro_client.client_params())
    locator;;

let pool = new Hydro_proxy.pool()

let env =
  ( object
      method event_system = esys
      method system =
	( object
	    method types = Hashtbl.create 1
	    method interfaces = Hashtbl.create 1
	    method classes = Hashtbl.create 1
	    method exceptions = Hashtbl.create 1
	  end : system
	)
      method proxy_resolver = resolver
      method client_pool = pool
      method max_reconnections = 0
    end : Hydro_proxy.proxy_env_t
  );;

let doit() =
  let lz = ref (lazy (assert false)) in
  resolver#resolve
    ( object
	method id =
	  ( object method name = "IceImagePoster" method category = "" end )
	method facet = None
	method mode = `Twoway
	method secure = false
	method parameters = `Adapter "ts0"
      end
    )
    (fun lz' -> lz := lz')
    env;
  Unixqueue.run esys;
  try
    (Lazy.force !lz)
  with
    | (Hydro_builtin.User_exception x) as err ->
	prerr_endline("Exception ID: " ^ x#hydro_ops#exn_id);
	raise err
;;
