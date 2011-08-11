(** Hydromon: Monitor ICE services by pinging them constantly *)

type config =
    { state_file : string;
      shm_name : string;
      shm_size : int;
      ping_period : float;
      ping_timeout : float;
    }
    (** Explanations see below, at [hydromon_factory] *)


val hydromon_processor : config -> Netplex_types.processor
  (** Start the Hydromon processor for this configuration *)

val hydromon_factory : name:string -> unit -> Netplex_types.processor_factory
  (** Reads the config from the  config file, and starts the processor

      The config must look like
      {[
         processor {
             type = "hydromon";
             state_file = "<filename>";
             shm_name = "<shmname>";
             shm_size = <size>;
             ping_period = <seconds>;
             ping_timeout = <seconds>;
         }
         workload_manager {
             type = "constant";
             jobs = 1
         }
       ]}

      The [state_file] is used to save the state, so when the service is
      restarted it can continue where it was stopped. It is legal to
      delete this file before startup, but of course nothing is watched
      then, and the clients may get confused.

      The [shm_name] is a special name like "/hydromon.shm". It is used
      for efficient communication with the clients. The name is the
      name of a POSIX shared memory segment.

      The [shm_size] is the size of the shared memory segment in bytes.
      It is also the maximum number of services that can be watched at
      the same time. A small number like 256.

      The monitored services are pinged every [ping_period] seconds,
      and if there is no reply after [ping_timeout] seconds the ping
      is counted as failure.
   *)

