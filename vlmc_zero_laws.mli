(** *)

module type P =
  sig
    val prob : int -> float
    val description : string
    val id : string
  end;;

module Make : functor (P : P) -> Vlmc.Law