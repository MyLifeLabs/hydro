open Hydro_types

let system =
  Hydro_lm.create_system()

let () =
  Hydro_builtin.fill_system system

let derive_proxy_env resolver (pe : Hydro_proxy.proxy_env_t)  =
  (* Derive a proxy_env from pe that shares the pool and the esys *)
  ( object
      method event_system = pe # event_system
      method system = system
      method proxy_resolver = resolver
      method client_pool = pe # client_pool
      method default_proxy_conf = Hydro_proxy.proxy_conf()
    end : Hydro_proxy.proxy_env_t
  )
