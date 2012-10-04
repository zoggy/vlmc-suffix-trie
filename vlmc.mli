(** *)

module type Symbol = sig
  type symbol
  val compare : symbol -> symbol -> int
  val string : symbol -> string
  val symbols : symbol array
end

module type Law = sig
  include Symbol
  val next : (int -> symbol option) -> symbol
end

module type Vlmc =
  sig
    type symbol
    type t
    type pos = int

    val create : int -> t
    val get : t -> pos -> symbol
    val first_diff_pos : t -> pos -> pos -> pos
  end

module Make : functor (L : Law) -> Vlmc with type symbol = L.symbol