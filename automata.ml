

type 'a context = 'a array

type 'a tree =
  Context of 'a
| Node of 'a tree array


let dot_of_context_tree f_sym f t =
  let b = Buffer.create 256 in
  let id_of_path l =
    "N"^(String.concat "_" (List.rev_map f_sym l))
  in
  Buffer.add_string b "digraph g {\nrankdir = TB;\n";
  let rec iter path = function
    Context c ->
      let (shape, label) = f c in
      Printf.bprintf b "%s [ shape=%S, label=%S ];\n" (id_of_path path) shape label
  | Node t ->
      let id = id_of_path path in
      Printf.bprintf b "%s [ shape=point ];\n" id;
      Array.iteri
        (fun i n ->
           let path = i :: path in
           Printf.bprintf b "%s -> %s [ label=%S ];\n" id (id_of_path path) (f_sym i);
           iter path n
        )
        t
  in
  iter [] t;
  Buffer.add_string b "}\n";
  Buffer.contents b
;;


module OrderedIntList = struct type t = int list let compare = Pervasives.compare end
(*module ACMap = Map.Make (OrderedIntList)*)
module ACSet = Set.Make (OrderedIntList)

type 'a automata_context = {
    ac_path : int list;
    ac_ctx : 'a context ;
    ac_next : int list context ;
    ac_prev : ACSet.t ;
  }

let automata_context_tree t =
  let rec map acc path = function
    Context c ->
      let ac =
        { ac_path = List.rev path ;
          ac_ctx = c ;
          ac_next = Array.make (Array.length c) [] ;
          ac_prev = ACSet.empty ;
        }
      in
      (Context ac, ac :: acc)
  | Node t ->
     let (trees, acc, _) =
        Array.fold_left
          (fun (trees, acc, i) n ->
             let (tree, acc) = map acc (i :: path) n in
             (tree :: trees, acc, i+1)
          )
          ([], acc, 0)
          t
      in
      (Node (Array.of_list trees), acc)
  in
  map [] [] t
;;

let sort_automata_context_by_path_size =
  let compare ac1 ac2 =
    match List.length ac2.ac_path - List.length ac1.ac_path with
      0 -> Pervasives.compare ac2.ac_path ac1.ac_path
    | n -> n
  in
  fun l -> List.sort compare l
;;

exception Need_break

let find_context =
  let rec iter = function
    ([], Context c) -> c
  | (_ :: _, Context c) -> c
  | ([], Node _) -> raise Not_found
  | (h :: q, Node t) ->  iter (q, t.(h))
  in
  fun path tree -> iter (path, tree)
;;

let replace_context tree ac ac2 =
  ignore(ac=ac2);
  let rec map = function
    ([], Context _) -> Context ac2
  | (_ :: _, Context _) -> assert false
  | ([], Node _) -> assert false
  | (h :: q, Node t) ->
      let a = Array.copy t in
      let th = map (q, t.(h)) in
      a.(h) <- th;
      Node a
  in
  map (ac.ac_path, tree)
;;

let complement_contexts =
  let rec iter tree = function
    [] -> tree
  | ac :: q ->
      try
        let f (tree, i) _ =
          let path = i :: ac.ac_path in
          let ac_target =
            try find_context path tree
            with Not_found -> raise Need_break
          in
          let ac2_next = Array.copy ac.ac_next in
          ac2_next.(i) <- ac_target.ac_path ;
          let ac2 = { ac with ac_next = ac2_next } in
          let ac_target2 = { ac_target with ac_prev = ACSet.add ac.ac_path ac_target.ac_prev } in
          let tree = replace_context tree ac ac2 in
          let tree = replace_context tree ac_target ac_target2 in
          (tree, i+1)
        in
        let (tree, _) = Array.fold_left f (tree, 0) ac.ac_ctx in
        tree
      with
        Need_break -> assert false
  in
  fun ctxs tree ->
    iter tree ctxs
;;

let complement_context_tree t =
  let (tree, contexts) = automata_context_tree t in
  let contexts = sort_automata_context_by_path_size contexts in
  complement_contexts contexts tree







let test_tree =
  Node [|
   Node [|
    Context [| 0.2 ; 0.8 |] ;
    Context [| 0.2 ; 0.8 |] ;
   |] ;
   Node [|
    Context [| 0.2 ; 0.8 |] ;
    Node [|
     Context [| 0.2 ; 0.8 |] ;
     Context [| 0.2 ; 0.8 |] ;
    |] ;
   |] ;
  |]
;;

let dot = dot_of_context_tree string_of_int (fun _ -> "triangle", "") test_tree;;
print_endline dot;;