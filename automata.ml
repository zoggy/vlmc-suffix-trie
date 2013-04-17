
(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)

type 'a context = 'a array

type 'a tree =
  Context of 'a
| Node of 'a tree array


let dot_of_context_tree f_sym f t =
  let b = Buffer.create 256 in
  let id_of_path l =
    "N"^(String.concat "_" (List.map f_sym l))
  in
  Buffer.add_string b "digraph g {\nrankdir = TB;\n";
  let rec iter path = function
    Context c ->
      let (shape, label) = f b c in
      Printf.bprintf b "%s [ shape=%S, label=%S ];\n"
        (id_of_path (List.rev path)) shape label
  | Node t ->
      let id = id_of_path (List.rev path) in
      Printf.bprintf b "%s [ shape=point ];\n" id;
      Array.iteri
        (fun i n ->
           let path = i :: path in
           Printf.bprintf b "%s -> %s [ label=%S ];\n" id 
             (id_of_path (List.rev path)) (f_sym i);
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

let dot_of_automata_context_tree f_sym t =
  let id_of_path l =
    "N"^(String.concat "_" (List.map f_sym l))
  in
  let f b ac =
    Array.iteri
      (fun i path ->
      match path with
           [] -> ()
         | _ ->
             Printf.bprintf b "%s -> %s [ label=%s, style=dashed, constraint=false];\n"
               (id_of_path ac.ac_path) 
               (id_of_path (List.rev path)) (f_sym i)
      )
      ac.ac_next;
    "triangle", ""
  in
  dot_of_context_tree f_sym f t
;;

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
      let trees = Array.of_list (List.rev trees) in
      (Node trees, acc)
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
  let rec iter exact = function
    ([], Context c) -> c
  | (_ :: _, Context c) -> if exact then assert false else c
  | ([], Node _) -> raise Not_found
  | (h :: q, Node t) ->  iter exact (q, t.(h))
  in
  fun ?(exact=false) path tree -> iter exact (path, tree)
;;

let replace_context tree ac ac2 =
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

let break tree path =
  let rec iter added tree path =
    match added with
      _ :: _ -> (tree, added)
    | [] ->
        match (path, tree) with
          ([], Context ac) ->
            let new_contexts =
              Array.mapi
                (fun i _ ->
                   let new_ac =
                     {
                       ac_path = ac.ac_path @ [i] ;
                       ac_ctx = ac.ac_ctx ;
                       ac_next = Array.map (fun _ -> []) ac.ac_ctx ;
                       ac_prev = ACSet.empty ;
                     }
                   in
                   new_ac
                )
                ac.ac_ctx
            in
            let ctx_nodes = Array.map (fun ac -> Context ac) new_contexts in
            (Node ctx_nodes, Array.to_list new_contexts)
        | (_ :: _, Context _) -> assert false
        | ([], Node _) -> assert false
        | (h :: q, Node t) ->
            let a = Array.copy t in
            let (th, new_contexts) = iter added t.(h) q in
            a.(h) <- th;
            (Node a, new_contexts)
  in
  let ac = find_context ~exact: true path tree in
  let (tree, added) = iter [] tree path in
  let f_prev prev_path tree =
    match List.rev prev_path with
      i :: p ->
        let prev_path = List.rev p in
        let old_ac = find_context ~exact: true prev_path tree in
        let new_ac_next = Array.copy old_ac.ac_next in
        (* set new link: search first context matching i :: old_ac.ac_path *)
        let new_target = find_context (i :: old_ac.ac_path) tree in
        new_ac_next.(i) <- new_target.ac_path ;
        let new_ac = { old_ac with ac_next = new_ac_next } in
        replace_context tree old_ac new_ac
    | _ -> assert false
  in
  let tree = ACSet.fold f_prev ac.ac_prev tree in
  (tree, added)
;;

let complement_contexts =
  let rec iter tree = function
    [] -> tree
  | ac :: q ->
      try
        let dot = dot_of_automata_context_tree string_of_int tree in
        file_of_string ~file: "/tmp/foo.dot" dot;
        let f (tree, i) _ =
          let path = i :: ac.ac_path in
          let ac_target =
            try find_context path tree
            with Not_found -> raise Need_break
          in
          let ac2_next = Array.copy ac.ac_next in
          ac2_next.(i) <- ac_target.ac_path ;
          let ac2 = { ac with ac_next = ac2_next } in
          let ac_target2 =
            { ac_target with ac_prev = ACSet.add (ac.ac_path @ [i]) ac_target.ac_prev }
          in
          let tree = replace_context tree ac ac2 in
          let tree = replace_context tree ac_target ac_target2 in
          (tree, i+1)
        in
        let (tree, _) = Array.fold_left f (tree, 0) ac.ac_ctx in
        iter tree q
      with
        Need_break ->
          let (tree, new_contexts) = break tree ac.ac_path in
          let l = sort_automata_context_by_path_size (new_contexts @ q) in
          iter tree l
  in
  fun ctxs tree ->
    iter tree ctxs
;;

let complement_context_tree t =
  let (tree, contexts) = automata_context_tree t in
  prerr_endline (Printf.sprintf "length(contexts)=%d" (List.length contexts));
  let contexts = sort_automata_context_by_path_size contexts in
  complement_contexts contexts tree

let test_tree =
  Node [|
   Node [|
    Node [|
     Context [| 0.2 ; 0.8 |] ;
     Node [|
      Context [| 0.2 ; 0.8 |] ;
      Context [| 0.2 ; 0.8 |] ;
     |] ;
    |] ;
    Context [| 0.2 ; 0.8 |] ;
   |] ;
   Context [| 0.2 ; 0.8 |] ;
  |]
;;


let dot = dot_of_context_tree string_of_int (fun _ _ -> "triangle", "") test_tree;;
print_endline dot;;
let t2 = complement_context_tree test_tree;;
file_of_string ~file: "/tmp/result.dot" (dot_of_automata_context_tree string_of_int t2);;
