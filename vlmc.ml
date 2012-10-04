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

module Make (L : Law) =
  struct
    type symbol = L.symbol
    type t = {
        increment : int ;
        mutable seq : L.symbol array ;
      }
    type pos = int

    let next vlmc =
      let len = Array.length vlmc.seq in
      let get i = if i >= len then None else Some vlmc.seq.(len-i-1) in
      L.next get

    let increase vlmc =
      let curlen = Array.length vlmc.seq in
      let seq = Array.create (curlen+vlmc.increment) L.symbols.(0) in
      Array.blit vlmc.seq 0 seq 0 curlen;
      vlmc.seq <- seq;
      for i = 0 to vlmc.increment - 1 do
        vlmc.seq.(curlen+i) <- next vlmc
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
