(** *)

module type Symbol = sig
  type symbol
  val compare : symbol -> symbol -> int
  val string : symbol -> string
  val symbols : symbol array
end

module type Law = sig
  include Symbol
  val description : string
  val next : (int -> symbol option) -> symbol
end

module type S =
  sig
    module Law : Law
    type t
    type pos = int

    val description : string
    val create : int -> t
    val get : t -> pos -> Law.symbol
    val first_diff_pos : t -> pos -> pos -> pos
  end

module Make : functor (L : Law) -> S