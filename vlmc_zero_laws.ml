(** *)

module Symbol = struct
  type symbol = int
  let compare = Pervasives.compare
  let string = string_of_int
  let symbols = [| 0 ; 1 |]
  end

module type P = sig
    val prob : int -> float
    val description : string
    val id : string
  end;;

module Make (P: P) = (struct
    include Symbol
    let count_zeros_back =
      let rec iter acc n get =
        match get n with
        | Some 0 -> iter (acc+1) (n+1) get
        | _ -> acc
      in
      iter 0 0

     let next get =
       let zeros = count_zeros_back get in
       let p = P.prob zeros in
       if p <= Random.float 1.0 then 1 else 0

    let description = P.description
    let id = P.id
end : Vlmc.Law)
