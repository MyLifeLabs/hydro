(** Netplex support for Hydro servers (TCP only) *)

open Netplex_types

val hydro_factory :
      configure:(config_file -> address -> 
		   Hydro_oa.object_adapter_t array -> 'a) ->
      ?adapters:int ->
      ?hooks:(Hydro_oa.object_adapter_t array -> 'a -> processor_hooks) ->
      ?supported_ptypes:parallelization_type list ->
      ?setup:(Netplex_types.container -> Hydro_endpoint.Server.t -> 
		Hydro_oa.object_adapter_t array -> 'a -> unit) ->
      ?register_at:Hydro_builtin.pr_Ice_Locator -> 
      ?server_params:Hydro_types.server_params ->
      name:string ->
      sys:Hydro_types.system ->
      unit ->
         processor_factory
  (** A factory for TCP-based Hydro servers. In [name] the name of the processor
    * is defined. This name can be referenced from the config file like:
    *
    * {[
    *    processor {
    *        type = "the passed [name]";
    *        ...
    *    }
    * ]}
    *
    * Some other parameters in the [processor] section are also interpreted 
    * by the factory, see below. In general, however, the parameters and
    * subsections are defined by the user. These
    * parameters should be parsed and checked for correctness by the
    * [configure] callback. The result of [configure] is also passed 
    * to [setup] and other optional functions. The [configure] function is
    * called just before the service is
    * added to Netplex (i.e. from the controller context).
    *
    * The return value of [configure] can be of any type. This value
    * exists once for every time the processor is instantiated (used) by a
    * service. It is not only useful for passing configuration values
    * down, but also for storing intermediate results while establishing
    * the service.
    *
    * The {!Hydro_endpoint.Server.t} structure is created every time a new connection
    * is accepted. Of course, this is done from the context of the container.
    * This structure is created with as many object adapters as the [adapters]
    * argument says (which defaults to 0); the user can bind additional 
    * adapters in the [setup] function. This is achieved by calling
    * [Hydro_endpoint.Server.bind_adapter].
    *
    * The parameter [timeout] can be set to the timeout value. If connections
    * block for longer than [timeout], they are terminated. This value is
    * also announced in proxies created for this server. If [timeout] is
    * not set, the default is taken from the [server_params] argument.
    *
    * If the [adapters] argument is a positive number, the factory creates
    * as many object adapters. These adapters are passed as array to
    * [configure], [hooks], and [setup]. They are also automatically
    * bound in the server endpoint. The adapters can be filled by the user
    * with ICE objects. It is also possible to set the [adapter_id] and
    * [replica_group_id] properties of the adapters in the config file:
    * {[
    *    processor {
    *        type = "the passed [name]";
    *        adapter {         // about the first adapter in the array
    *            adapter_id = "some name";
    *            replica_group_id = "some name";  // optional
    *        };
    *        adapter { ... };  // about the second adapter in the array
    *        ...               // and so on
    *    }
    * ]}
    *
    * When the first container is started, and a locator registry is set,
    * the adapters in the adapter array are registered at this registry
    * with the configured names. Note that adapters created by the user
    * are not automatically registered. The adapters are unregistered
    * when the last container is shut down. Registration and unregistration
    * both happen in the context of the container.
    *
    * To set the registry, either define a config parameter
    * [register_at] with the stringified proxy of the registry, or
    * pass the [register_at] argument. (If both are set, the config file
    * has precedence.) E.g.
    * {[
    *    processor {
    *        ...
    *        register_at = "IceGrid/Locator:tcp -h <host> -p 4061"
    *        ...
    *    }
    * ]}
    *
    * As ICE can only register one Internet port per adapter, there is
    * the restriction that only the first [address] of the first [protocol]
    * is registered. There is one more parameter that plays a role in the
    * registration process: [register_hostname]. The server is registered
    * under this host name at the locator service. This can be a resolvable
    * name or an IP address, or one of the special strings
    * - [<hostname>]: The result of [Unix.gethostname()] is used
    * - [<ping>]: The locator service is pinged, and the IP address used
    *   for this ping is taken as name
    * - [<sockaddr_or_ping>]: If the socket address is 0.0.0.0, the ping
    *   method is used, otherwise the socket address. This is the default.
    *
    * @param configure Function for reading further parameters from the
    * config file, and for configuring the adapters in the array. The
    * return value is passed to the [hooks] and [setup] invocation.
    * If not needed, return [()]. The [configure] function is only called
    * once in the controller context for every [processor] section 
    * referring to this factory.
    *
    * @param adapters The size of the adapter array. Defaults to 0.
    * 
    * @param hooks An optional function returning the hooks to call.
    * See [Netplex_types.processor_hooks] for documentation.
    *
    * @param supported_ptypes Which parallelization types are supported
    * by the service. By default, only multi-processing 
    * is included in this list (Hydro still lacks multi-threading support).
    *
    * @param setup Function for setting up server details. It is called
    * for every accepted connection.
    *
    * @param register_at Where to register the named adapters in the
    * adapter array
    *
    * @param name Name of the processor. The [type] parameter of the
    * [processor] section of the config file can refer to these names.
    *
    * @param sys The ICE type system to use
   *)


(** {2 Note on multiprocessing}

The user has to fill ICE objects into the adapters. The question is when
to do this. For permanent objects that are available throughout the lifetime
of the service the answer is easy: Just add the objects in the [configure]
callback, e.g.

{[ 
    Hydro_netplex.hydro_factor
      ...
      ~adapters:1
      ~configure:(fun _ _ aa ->
                    aa.(0) # add identity ice_object
                 )
      ...
      ()
]}

This code is executed once per [processor] section in the controller
context, i.e. before subprocesses are forked off.

When objects have to be added later, things get complicated, because
[aa] exists independently in every subprocess. There is no really good
solution yet to broadcast changes from one subprocess to the others, 
so it is strongly recommended to consider changes of [aa] only if there
is exactly one subprocess (i.e. use a constant workload manager with
1 process). 

In this case it is possible to pass [aa] down, and to modify it later.
For instance, here one initial object [obj1] is added, and later a second
object [obj2]:


{[ 
    class class1 aa =
    object
      inherit skel_Obj1
      ...
      method doSomething () =
        parachute
          (fun session ->
             ...
             let obj2 = ... in
             aa.(0) # add id2 obj2
          )
    end

    Hydro_netplex.hydro_factor
      ...
      ~adapters:1
      ~configure:(fun _ _ aa ->
                    let obj1 = new class1 aa in
                    aa.(0) # add id1 obj1
                 )
      ...
      ()
]}

Note that it is also possible to do the initial addition of objects in the 
[post_start_hook]. This may be advantegeous when the objects consume a lot
memory, and it should be avoided that the memory is duplicated by forking,
or when database handles or other descriptors must not be duplicated.

 *)
