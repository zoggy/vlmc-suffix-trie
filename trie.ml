(** *)

module type S = sig
    module Vlmc : Vlmc.S
    type t
    val empty : t
    val insert : Vlmc.t -> t -> Vlmc.pos -> t
    val cardinal : t -> int * int
    val height : t -> int
    val saturation_level: t -> int
    val dot : ?depth:int -> ?rankdir:string -> ?dots:bool -> Vlmc.t -> t -> string
end;;

module Make (V : Vlmc.S) =
  struct
    module Map = Map.Make (struct type t = V.Law.symbol let compare = V.Law.compare end)

    (** Trie storing position in sequence *)
    type t =
        | Node of t Map.t
        | Leaf of V.pos

    module Vlmc = V
    let empty = Node Map.empty
    let is_root t = t = empty

    let rec insert seq t ~from ~h =
      (*prerr_endline (Printf.sprintf "insert %d, h=%d" from h);*)
      if is_root t then
        (
         (*prerr_endline "is_root";*)
         Node (Map.add (V.get seq (from+h)) (Leaf from) Map.empty)
        )
      else
        match t with
          Leaf w2 ->
            (*prerr_endline (Printf.sprintf "Leaf(w2=%d)" w2);
            prerr_endline (Printf.sprintf "calling first_diff_pos %d %d"*
              (from+h) (w2+h));*)
            let pdiff = V.first_diff_pos seq (from+h) (w2+h) in
            (*assert (pdiff > 0);*)
            (*prerr_endline (Printf.sprintf "pdiff=%d" pdiff);*)
            (*let sub1 = Leaf (from+pdiff) in
            let sub2 = Leaf (w2+pdiff) in*)
            let sub1 = Leaf from in
            let sub2 = Leaf w2 in
            (*prerr_endline (Printf.sprintf "sub1 = Leaf(%d), sub2 = Leaf(%d)" from w2);*)
            let elt1 = V.get seq (from+h+pdiff) in
            let elt2 = V.get seq (w2+h+pdiff) in
            (*prerr_endline (Printf.sprintf "elt1=%d, elt2=%d" (Obj.magic elt1) (Obj.magic elt2));*)
            assert (elt1 <> elt2);
            let map =
              Map.add elt1 sub1
                (Map.add elt2 sub2 Map.empty)
            in
            let t = ref (Node map) in
            (*prerr_endline (Printf.sprintf "from=%d, h=%d, pdiff=%d" from h pdiff);*)
            for j = (from+h+pdiff-1) downto from+h do
              (*prerr_endline (Printf.sprintf "j=%d, seq.(j) = %d" j (Obj.magic (V.get seq j)));*)
              let map = Map.add (V.get seq j) !t Map.empty in
              t := Node map
            done;
            !t
        | Node map ->
            (*prerr_endline "Node";*)
            let elt = V.get seq (from+h) in
            try
              let t2 = Map.find elt map in
              (*prerr_endline (Printf.sprintf "recursive call insert with h = %d" (h+1));*)
              Node (Map.add elt (insert seq t2 ~from ~h: (h + 1)) map)
            with Not_found ->
              (*prerr_endline (Printf.sprintf "Insert (Leaf %d) in node" from);*)
              Node (Map.add elt (Leaf from) map)

    let insert seq t from = insert seq t ~from ~h: 0

    let rec cardinal ?(acc=(0,0)) t =
      let (nodes, leaves) = acc in
      match t with
        Leaf _ -> (nodes, leaves+1)
      | Node map when Map.is_empty map -> (nodes, leaves)
      | Node map ->
          Map.fold (fun _ t acc -> cardinal ~acc t) map (nodes+1, leaves)

    let cardinal t = cardinal t

    let rec height = function
      Leaf _ -> 1
    | Node map when Map.is_empty map -> 0
    | Node map ->
        Map.fold
         (fun _ t curmax ->
            let h = height t + 1 in
            max h curmax
          ) map 0

    let map_is_full map =
      try Array.iter (fun sym -> ignore(Map.find sym map)) V.Law.symbols; true
      with Not_found -> false

    let saturation_level t =
      let rec iter acc h = function
        Leaf n -> min h acc
      | Node map when Map.is_empty map -> assert false
      | Node map ->
          if map_is_full map then
            if h >= acc then
              acc
            else
              Map.fold
                (fun _ t acc -> iter acc (h+1) t)
                map acc
          else
            min h acc
     in
     (iter max_int 1 t) - 1

    let dot ?depth ?(rankdir="TB") ?(dots=false) seq t =
      let b = Buffer.create 1024 in
      Printf.bprintf b
       "digraph G {\nrankdir=%s;\n ratio=auto;\n"
       rankdir
       ;
      let id s = function
          Leaf sym ->
            let sym = V.Law.string (V.get seq sym) in
            Printf.sprintf "N%sL%s" s sym
        | Node _ -> Printf.sprintf "N%s" s
      in
      let continue =
        match depth with
          None -> (fun _ -> true)
        | Some n -> fun h -> h <= n
      in
      let rec try_map h t s sym map =
        match
          try Some(Map.find sym map)
          with Not_found -> None
        with
          Some t2 ->
            let s2 = Printf.sprintf "%s%s" s (V.Law.string sym) in
            iter h s2 t2;
            Printf.bprintf b "%s -> %s [ label=\"%s\"];\n" (id s t) (id s2 t2) (V.Law.string sym)
        | None ->
          ()

      and iter h s t =
        match t with
         Leaf n ->
           let s = if s = "" then V.Law.string (V.get seq n) else s in
           Printf.bprintf b "  %s [ shape=\"rect\", label=\"%s%s\" ];\n"
             (id s t)
             (if String.length s < 10 then s else (String.sub s 0 10)^"...")
             (if dots then "..." else "")
             (*(string_of_int n)*)
       | Node map ->
           Printf.bprintf b "  %s [ label=\"\", shape=\"point\"]; \n" (id s t);
           if continue h then
             begin
              Array.iter (fun sym -> try_map (h+1) t s sym map) V.Law.symbols
             end
      in
      iter 0 "" t;
      Buffer.add_string b "\n}\n";
      Buffer.contents b;;

  end;;

