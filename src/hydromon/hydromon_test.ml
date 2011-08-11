(* $Id$ *)

(* Test server *)

open Printf

let main() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  let debug = ref false in

  let opts =
    [ "-debug", Arg.Set debug,
      "  enable debugging";
    ] @ opt_list in

  Arg.parse
    opts
    (fun s -> raise(Arg.Bad ("Don't know what to do with: " ^ s)))
    (sprintf "%s: [options]" Sys.argv.(0));

  if !debug then
    Hydro_dbg.set_debug_log prerr_endline;

  let parallelizer = Netplex_mp.mp() in  

  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories
    Netplex_workload.workload_manager_factories
    [ Hydromon_netplex.hydromon_factory ~name:"hydromon" () ]
    cmdline_cfg


let () =
  main()
  
