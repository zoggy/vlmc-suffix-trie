(** *)

module type S = sig
  type seq
  type pos
  type t
  val empty : t
  val insert : seq -> t -> pos -> t

  (** return number of nodes and number of leaves *)
  val cardinal : t -> int * int
  val height : t -> int
  val saturation_level: t -> int
  val dot : ?depth:int -> ?rankdir:string -> ?dots:bool -> seq -> t -> string
end;;

module Make : functor (V:Vlmc.S) -> S with type pos = V.pos and type seq = V.t
