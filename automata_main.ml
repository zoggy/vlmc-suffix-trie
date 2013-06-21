
open Automata;;

let main () =
  let args = ref [] in
  let max_iter = ref None in
  let options =
    [ "--max-iter", Arg.Int (fun n -> max_iter := Some n),
      "n set max iteration number when complementing, default is 120" ;
    ]
  in
  Arg.parse options
    (fun s -> args := !args @ [s])
    (Printf.sprintf "Usage: %s [options] file.tree\nwhere options are:" Sys.argv.(0));
  match List.rev !args with
    [] -> failwith "Please give at least one file"
  | file :: _ ->
      let spec_str = Automata.string_of_file file in
      let spec = Automata.read_spec spec_str in
      let tree = Automata.context_tree_of_spec spec in
      let basename = Filename.chop_extension file in

      let dot = Automata.dot_of_context_tree
        spec.spec_sym (fun _ _ _ -> ["shape","triangle"; "label",""]) tree
      in
      Automata.file_of_string ~file: (basename^".dot") dot;

      let cct = Automata.complemented_context_tree ?max_iter: !max_iter tree in
      Automata.file_of_string ~file: (basename^".cct.dot")
        (Automata.dot_of_cct spec.spec_sym cct);

      Automata.file_of_string ~file: (basename^".diff.dot")
        (Automata.dot_of_tree_diff spec.spec_sym tree cct)
;;

try main ()
with
  Sys_error msg
| Failure msg -> prerr_endline msg; exit 1;;