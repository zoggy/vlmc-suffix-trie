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
  val id : string
  val next : (int -> symbol option) -> symbol
end

module type S =
  sig
    module Law : Law
    type t
    type pos = int

    val create : int -> t
    val get : t -> pos -> Law.symbol
    val first_diff_pos : t -> pos -> pos -> pos
  end

module Make (L : Law) =
  struct
    module Law = L
    type t = {
        increment : int ;
        mutable seq : Law.symbol array ;
      }
    type pos = int

    let next curlen vlmc =
      let get i = if i >= curlen then None else Some vlmc.seq.(curlen - i -  1) in
      Law.next get

    let increase vlmc =
      let curlen = Array.length vlmc.seq in
      let seq = Array.create (curlen+vlmc.increment) L.symbols.(0) in
      Array.blit vlmc.seq 0 seq 0 curlen;
      vlmc.seq <- seq;
      for i = 0 to vlmc.increment - 1 do
        vlmc.seq.(curlen+i) <- next curlen vlmc
      done

    let create len =
      Random.self_init();
      let vlmc = { increment = len; seq = [| |] } in
      increase vlmc ;
      vlmc

    let rec get t pos =
      let len = Array.length t.seq in
      if pos >= len then (increase t; get t pos) else t.seq.(pos)

    let first_diff_pos t pos1 pos2 =
      let rec iter n =
        let c1 = get t (pos1+n) in
        let c2 = get t (pos2+n) in
        if c1 = c2 then iter (n+1) else n
      in
      iter 1
  end;;
