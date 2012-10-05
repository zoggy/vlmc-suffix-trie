(** *)

module type S = sig
  module Vlmc : Vlmc.S

  type t
  val empty : t
  val insert : Vlmc.t -> t -> Vlmc.pos -> t

  (** return number of nodes and number of leaves *)
  val cardinal : t -> int * int
  val height : t -> int
  val saturation_level: t -> int
  val dot : ?depth:int -> ?rankdir:string -> ?dots:bool -> Vlmc.t -> t -> string
end;;

module Make : functor (V:Vlmc.S) -> S
