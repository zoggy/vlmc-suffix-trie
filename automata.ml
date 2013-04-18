
(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)

let () = Random.self_init();;

type 'a context = 'a array

type 'a tree =
  Context of 'a
| Node of 'a tree array


let dot_of_context_tree ?(graphatts=[]) ?edgeatts f_sym f t =
  let b = Buffer.create 256 in
  let id_of_path l =
    "N"^(String.concat "_" (List.map f_sym l))
  in
  let string_of_atts atts =
    String.concat ", "
      (List.map (fun (att, v) -> Printf.sprintf "%s=%S" att v) atts)
  in
  Buffer.add_string b "digraph g {\nrankdir = TB;\nordering=\"out\";\n";
  List.iter
    (fun (att, v) -> Printf.bprintf b "%s = %S;\n" att v)
    graphatts;
  let rec iter path = function
    Context c ->
      let path = List.rev path in
      let atts = f b path c in
      Printf.bprintf b "%s [ %s ];\n"
        (id_of_path path) (string_of_atts atts)
  | Node t ->
      let id = id_of_path (List.rev path) in
      Printf.bprintf b "%s [ shape=point ];\n" id;
      Array.iteri
        (fun i n ->
           let ipath = i :: path in
           let atts =
              match edgeatts with
                None -> []
             | Some f -> f (List.rev path) (List.rev ipath)
           in
           let atts = ("label", f_sym i) :: atts in
           Printf.bprintf b "%s -> %s [ %s];\n" id
             (id_of_path (List.rev ipath)) (string_of_atts atts);
           iter ipath n
        )
        t
  in
  iter [] t;
  Buffer.add_string b "}\n";
  Buffer.contents b
;;

module OrderedIntList = struct type t = int list let compare = Pervasives.compare end
module ACMap = Map.Make (OrderedIntList)
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
  let f b _ ac =
    Array.iteri
      (fun i path ->
      match path with
           [] -> ()
         | _ ->
             Printf.bprintf b "%s -> %s [ label=%s, style=dashed, constraint=false];\n"
               (id_of_path ac.ac_path)
               (id_of_path path) (f_sym i)
      )
      ac.ac_next;
    ["shape", "triangle"; "label", ""]
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

let node_exists =
  let rec iter = function
    ([], _) -> true
  | (_ :: _, Context _) -> false
  | (h :: q, Node t) -> iter (q, t.(h))
  in
  fun tree path -> iter (path, tree)
;;

let find_context =
  let rec iter exact = function
    ([], Context c) -> c
  | (_ :: _, Context c) -> if exact then assert false else c
  | ([], Node _) -> raise Not_found
  | (h :: q, Node t) ->  iter exact (q, t.(h))
  in
  fun ?(exact=false) path tree -> iter exact (path, tree)
;;

let map_tree tree f =
  let rec map path = function
    Context c -> f (List.rev path) c
  | Node t ->
      let a =
        Array.mapi
          (fun i t -> map (i::path) t)
          t
      in
      Node a
  in
  map [] tree
;;

let replace_context tree path f =
  let rec map = function
    ([], Context c) -> f c
  | (_ :: _, Context _) -> assert false
  | ([], Node _) -> assert false
  | (h :: q, Node t) ->
      let a = Array.mapi
        (fun i x -> if i = h then map(q, t.(h)) else x)
        t
      in
      Node a
  in
  map (path, tree)
;;

let string_of_path = function
  [] -> "_"
| l ->  String.concat "-" (List.map string_of_int l);;

let break tree path =
  let added = ref [] in
  let old_ac = find_context ~exact: true path tree in
  let f ac =
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
    added := Array.to_list new_contexts ;
    let ctx_nodes = Array.map (fun ac -> Context ac) new_contexts in
    Node ctx_nodes
  in
  let tree = replace_context tree path f in
  let f_prev prev_path tree =
    match List.rev prev_path with
      i :: p ->
        let prev_path = List.rev p in
        let f old_ac =
          let new_next = Array.mapi
            (fun j x ->
               if j = i then
                 begin
                   (* set new link: search first context matching i :: old_ac.ac_path *)
                   let ac = find_context (i :: old_ac.ac_path) tree in
                   prerr_endline
                     (Printf.sprintf "break: prev=%s, old_next=%s, new_next=%s"
                      (string_of_path prev_path)
                        (string_of_path old_ac.ac_next.(i))
                        (string_of_path ac.ac_path)
                     );
                   ac.ac_path
                 end
               else
                 x
            )
            old_ac.ac_next
          in
          let new_ac = { old_ac with ac_next = new_next } in
          Context new_ac
        in
        replace_context tree prev_path f
    | _ -> assert false
  in
  let tree = ACSet.fold f_prev old_ac.ac_prev tree in
  (tree, !added)
;;

let gensym = let i = ref 0 in fun () -> incr i; !i;;

let complement_contexts =
  let rec iter tree = function
    [] -> tree
  | ac :: q ->
      try
        let dot = dot_of_automata_context_tree string_of_int tree in
        file_of_string ~file: ("/tmp/foo"^(string_of_int (gensym()))^".dot") dot;
        let f (tree, i) _ =
          let path = i :: ac.ac_path in
          let ac_target =
            try find_context path tree
            with Not_found -> raise Need_break
          in
          let tree = replace_context tree ac.ac_path
            (fun ac ->
               let ac2 =
                 { ac with
                   ac_next = Array.mapi
                     (fun j x -> if j = i then ac_target.ac_path else x) ac.ac_next
                 }
               in
               Context ac2
            )
          in
          let tree = replace_context tree ac_target.ac_path
            (fun ac_target ->
               let ac_target2 =
                 { ac_target with ac_prev = ACSet.add (ac.ac_path @ [i]) ac_target.ac_prev }
               in
               Context ac_target2
            )
          in
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

let automata_context_tree t =
  let (tree, contexts) = automata_context_tree t in
  prerr_endline (Printf.sprintf "length(contexts)=%d" (List.length contexts));
  let contexts = sort_automata_context_by_path_size contexts in
  complement_contexts contexts tree
;;

type 'a complemented_context_tree =
  { cct_map : int list array ACMap.t ;
    cct_tree : 'a tree ;
  }

let dot_of_cct f_sym cct =
  let id_of_path l =
    "N"^(String.concat "_" (List.map f_sym l))
  in
  let f b path c =
    let color = Printf.sprintf "/paired12/%d" (((Random.int 6) + 1) * 2) in
    Array.iteri
      (fun i next ->
         match path with
           [] -> ()
         | _ ->
             Printf.bprintf b "%s -> %s [ label=%s, style=dashed, constraint=false, color=%S, fontcolor=%S];\n"
               (id_of_path path)
               (id_of_path next) (f_sym i)
               color color
      )
      (ACMap.find path cct.cct_map);
    ["shape", "triangle" ; "color", color; "label", ""]
  in
  dot_of_context_tree f_sym f cct.cct_tree
;;

let complemented_context_tree tree =
  let map = ref ACMap.empty in
  let f path ac =
    map := ACMap.add path ac.ac_next !map ;
    Context ac.ac_ctx
  in
  let tree = map_tree tree f in
  { cct_map = !map ;
    cct_tree = tree ;
  }
;;

let dot_of_tree_diff f_sym tree cct =
  let edgeatts path ipath =
    let color = if node_exists tree ipath then "black" else "red" in
    ["color", color ; "fontcolor", color]
  in
  let f b path _ = ["shape", "point" ; "label", ""] in
  dot_of_context_tree ~edgeatts f_sym f cct.cct_tree
;;

(*c==v=[String.split_string]=1.1====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" ->
            if keep_empty then
              "" :: iter "" (pos + 1)
            else
              iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.1====*)
(*c==v=[String.strip_string]=1.0====*)
let strip_string s =
  let len = String.length s in
  let rec iter_first n =
    if n >= len then
      None
    else
      match s.[n] with
        ' ' | '\t' | '\n' | '\r' -> iter_first (n+1)
      | _ -> Some n
  in
  match iter_first 0 with
    None -> ""
  | Some first ->
      let rec iter_last n =
        if n <= first then
          None
        else
          match s.[n] with
            ' ' | '\t' | '\n' | '\r' -> iter_last (n-1)
          |	_ -> Some n
      in
      match iter_last (len-1) with
        None -> String.sub s first 1
      |	Some last -> String.sub s first ((last-first)+1)
(*/c==v=[String.strip_string]=1.0====*)
(*c==v=[File.string_of_file]=1.0====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_substring buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.0====*)

type tree_spec =
  { spec_symbols : char array ;
    spec_ctxs : (int list * float array) list ;
    spec_sym : int -> string ;
  }

let get_index_in_array t v =
  let len = Array.length t in
  let rec iter i =
    if i < len then
      if t.(i) = v then i else iter (i+1)
    else
      raise Not_found
  in
  iter 0
;;

let read_context symbols line =
  let ctx_of_string s =
    let s = strip_string s in
    let len = String.length s in
    let rec iter acc i =
      if i >= len then
        List.rev acc
      else
        try
          let n = get_index_in_array symbols s.[i] in
          iter (n :: acc) (i+1)
        with Not_found -> failwith (Printf.sprintf "Undeclared symbol '%c'" s.[i])
    in
    iter [] 0
  in
  match split_string (strip_string line) [':'] with
    [context] -> (ctx_of_string context, Array.map (fun _ -> 0.0) symbols)
  | context :: s_probs :: [] ->
      let probs = split_string s_probs [','] in
      if List.length probs <> Array.length symbols then
        failwith (Printf.sprintf "Number of probabilities differs from number of symbols: %s" s_probs);
      let probs =
        List.map (fun s ->
           try float_of_string (strip_string s)
           with _ -> failwith ("Invalid probability: "^s)
        ) probs
      in
      (ctx_of_string context, Array.of_list probs)
  | _ -> failwith ("Invalid syntax: "^line)
;;
let read_spec str =
  let lines = split_string str ['\n'; '\r'] in
  match lines with
    [] | [_] -> failwith "Not enough lines"
  | symbols :: lines ->
      let symbols =
        let s = strip_string symbols in
        (* remove blanks *)
        let s = String.concat "" (split_string s ['\n' ; ' ']) in
        let len = String.length s in
        let t = Array.make len 'a' in
        for i = 0 to len - 1 do t.(i) <- s.[i] done;
        t
      in
      let sym n =
        try String.make 1 symbols.(n)
        with _ -> failwith (Printf.sprintf "Invalid symbol index: %d" n)
      in
      let rec read_line acc n = function
        [] ->
          { spec_symbols = symbols ;
            spec_ctxs = acc ;
            spec_sym = sym ;
          }
      | line :: q ->
          try
            let ctx = read_context symbols line in
            read_line (ctx :: acc) (n+1) q
          with Failure msg ->
            let msg = Printf.sprintf "Line %d: %s" n msg in
            failwith msg
      in
      read_line [] 2 lines
;;

let test_spec = read_spec
"01
1: 0.2, 0.8
01: 0.2, 0.8
000
0010
0011"
;;
(*c==v=[List.list_diff]=1.0====*)
let list_diff ?(pred=(=)) l1 l2 =
  List.fold_right
    (fun el acc ->
       if not (List.exists (pred el) l2) then
         el :: acc
       else
         acc
    )
    l1 []
(*/c==v=[List.list_diff]=1.0====*)

let partition_paths symbols paths =
(*
  prerr_endline (Printf.sprintf "paritioning paths: %s"
   (String.concat ", " (List.map (fun (path, _, _) -> string_of_path path) paths)));
*)
  let rec gather i acc = function
    [] -> acc
  | (h, orig_path, ctx) :: q ->
      match h with
        x :: path when x = i -> gather i ((path, orig_path, ctx) :: acc) q
      | _ -> gather i acc q
  in
  let t = Array.mapi (fun i _ -> gather i [] paths) symbols in
(*  Array.iteri
    (fun i l ->
       prerr_endline
         (Printf.sprintf "[%d] => %s" i
          (String.concat ", " (List.map (fun (path, _, _) -> string_of_path path) l))
         )
    )
    t;
*)
  let (diff, _) = Array.fold_left
    (fun (acc,n) pl ->
       (list_diff acc
        (List.map (fun (p,orig_path,ctx) -> (n :: p, orig_path, ctx)) pl), n+1)
    )
    (paths,0) t
  in
  match diff with
    [] -> t
  | _ ->
    let msg =
        Printf.sprintf "The following contexts should not be defined: %s"
         (String.concat ", " (List.map (fun (_, orig_path, _) -> string_of_path orig_path) diff))
      in
    failwith msg
;;

let context_tree_of_spec spec =
  match spec.spec_ctxs with
    [] -> failwith "No context"
  | ctxs ->
      let rec build_node cur_revpath i paths =
        match paths with
          [] ->
            failwith ("Missing context: "^(string_of_path (List.rev (i::cur_revpath))))
        | [ ([], _, ctx) ] ->
            Context ctx
        | _ ->
            build (i :: cur_revpath) paths
      and build cur_revpath paths =
        let partitions = partition_paths spec.spec_symbols paths in
        Node (Array.mapi (build_node cur_revpath) partitions)
      in
      build [] (List.map (fun (path, ctx) -> (path, path, ctx)) ctxs)
;;
(*

let test_tree = context_tree_of_spec test_spec;;
(*
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
*)

let dot = dot_of_context_tree string_of_int (fun _ _ _ -> ["shape","triangle"; "label", ""]) test_tree;;
print_endline dot;;
let t2 = automata_context_tree test_tree;;
file_of_string ~file: "/tmp/result.dot" (dot_of_automata_context_tree string_of_int t2);;
let cct = complemented_context_tree t2;;
file_of_string ~file: "/tmp/cct.dot" (dot_of_cct string_of_int cct);;

file_of_string ~file: "/tmp/diff.dot" (dot_of_tree_diff string_of_int test_tree cct);;
*)

