
open Automata;;

let main () =
  if Array.length Sys.argv < 2 then
    failwith (Printf.sprintf "Usage: %s file.tree" Sys.argv.(0));
  let file = Sys.argv.(1) in
  let spec_str = Automata.string_of_file file in
  let spec = Automata.read_spec spec_str in
  let tree = Automata.context_tree_of_spec spec in
  let basename = Filename.chop_extension file in

  let dot = Automata.dot_of_context_tree
    spec.spec_sym (fun _ _ _ -> ["shape","triangle"; "label",""]) tree
  in
  Automata.file_of_string ~file: (basename^".dot") dot;

  let act = Automata.automata_context_tree tree in
  Automata.file_of_string ~file: (basename^".ct.dot")
    (Automata.dot_of_automata_context_tree spec.spec_sym act);

  let cct = Automata.complemented_context_tree act in
  Automata.file_of_string ~file: (basename^".cct.dot")
    (Automata.dot_of_cct spec.spec_sym cct);

  Automata.file_of_string ~file: (basename^".diff.dot")
    (Automata.dot_of_tree_diff spec.spec_sym tree cct)
;;

try main ()
with
  Sys_error msg
|  Failure msg -> prerr_endline msg; exit 1;;