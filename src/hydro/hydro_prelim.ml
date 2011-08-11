(* $Id$ *)

module CiString = struct  (* case-insensitive *)
  type t = string
  let equal s1 s2 =
    String.lowercase s1 = String.lowercase s2
  let hash s =
    Hashtbl.hash (String.lowercase s)
  let compare s1 s2 =
    String.compare (String.lowercase s1) (String.lowercase s2)
end


module CiHashtbl = Hashtbl.Make(CiString)

module CiStrSet = Set.Make(CiString)

module CiStrMap = Map.Make(CiString)
