(* $Id$ *)

(** Preliminaries *)

module CiString : sig
  type t = string
  val equal : t -> t -> bool
  val hash : t -> int
  val compare : t -> t -> int
end
  (** Hashable strings with case-insentitive hash values and case-insensitive
      comparison
    *)

module CiHashtbl : Hashtbl.S with type key = CiString.t
  (** Case-insensitive hashtable of strings *)

module CiStrSet : Set.S with type elt = CiString.t
  (** Case-insensitive sets of strings *)

module CiStrMap : Map.S with type key = CiString.t
  (** Case-insensitive maps of strings *)
