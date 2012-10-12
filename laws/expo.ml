(** *)

module M =
  struct
    let prob n =  if n = 0 then (1. /. 3.) else (1. /. 3.) +. (1. /. (float (1+n))**2.)
    let description = "expo"
    let id = "expo"
  end;;

module Law = Vlmc_zero_laws.Make(M);;
Vlmc_laws.register_law (module Law : Vlmc.Law);;