(** *)

module M =
  struct
    let alpha = 3. /. 2.
    let prob n = (1. -. (1. /. float (n+2))) ** alpha
    let description = Printf.sprintf "Vlmc prob1 -- alpha = %.02f" alpha
    let id = "prob1"
  end;;

module Law = Vlmc_zero_laws.Make(M);;
Vlmc_laws.register_law (module Law : Vlmc.Law);;