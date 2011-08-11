(* $Id$ *)

let debug_log = ref None
let set_debug_log f = debug_log := Some f
(* To enable debug logging, call set_debug_log with a function that
   outputs debug messages
 *)

let debug_log_enabled = ref (fun () -> !debug_log <> None)
let set_debug_log_enabled f = debug_log_enabled := f
(* The debug_log_enabled function checks whether debug logging is
   enabled. The default definition simply checks whether set_debug_log
   was called or not. It is possible to override this definition with
   something more fine-grained, especially if the debug logging function
   uses some criterion to decide whether to actually output a message.
 *)


let dlog msg =
  match !debug_log with
    | Some f -> f msg
    | None -> ()

let dlogf msgf =
  Printf.kprintf dlog msgf

let dlog_enabled() =
  !debug_log_enabled()
