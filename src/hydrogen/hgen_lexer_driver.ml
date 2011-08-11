(* $Id$ *)

open Hgen_types
open Hgen_util

let scan filename =
  let loc =
    { file = filename;
      line = 1;
      bol = 0;
      offset = 0;
    } in
  let bolloc_ref = ref loc in
  Hgen_lexer.scan bolloc_ref
