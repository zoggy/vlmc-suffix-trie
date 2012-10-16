(** *)

module M =
  struct
    let prob n =
      if n = 0
      then 1. /. 4.
      else (1. -. (4. /. ((float n) +. 4.)))
    let description = "logarithmic comb with c1 = 1/4"
    let id = "logcomb_1_4"
  end;;

module Law = Vlmc_zero_laws.Make(M);;
Vlmc_laws.register_law (module Law : Vlmc.Law);;