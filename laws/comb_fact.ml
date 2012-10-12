(** *)

module M =
  struct
    let prob n =  (1. /. ((float n) +. 2.))
    let description = "factorial comb"
    let id = "fact"
  end;;

module Law = Vlmc_zero_laws.Make(M);;
Vlmc_laws.register_law (module Law : Vlmc.Law);;