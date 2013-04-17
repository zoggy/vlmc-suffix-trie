

type 'a context = 'a array

type 'a context_tree =
  Context of 'a context
| Node of 'a context_tree array


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

type ('sym, 'a) automata_context = {
    ac_path : 'sym list ;
    ac_ctx : 'a context ;
  }

let automata_contexts_of_context_tree t =
  let rec iter acc path = function
    Context c ->
      let ac =
        { ac_path = List.rev path ;
          ac_ctx = c ;
        }
      in
      ac :: acc
  | Node t ->
     let (acc, _) =
        Array.fold_left
          (fun (acc, i) n ->
             let acc = iter acc (i :: path) n in
             (acc, i+1)
          )
          (acc, 0)
          t
      in
      acc
  in
  iter [] [] t
;;

let sort_automata_context_by_path_size =
  let compare ac1 ac2 =
    match List.length ac1.ac_path - List.length ac2.ac_path with
      0 -> Pervasives.compare ac1.ac_path ac2.ac_path
    | n -> n
  in
  fun l -> List.sort compare
;;








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

let dot = dot_of_context_tree string_of_int (fun _ -> "point", "") test_tree;;
print_endline dot;;