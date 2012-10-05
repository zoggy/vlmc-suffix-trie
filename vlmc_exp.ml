(** Running experiences with tries on vlmc. *)

(** {2 Command line arguments} *)

let measure_step = ref 10;;

let length = ref 1_000;;
let n_exp = ref 5_000;;

type mode = Single | Multi | Dynamic of int;;
let mode = ref Single;;

let output_file = ref "trie.dot";;
let auto_run_r = ref true;;

let options = [
    "--measure-step", Arg.Set_int measure_step,
    "<n> set measure step to <b>; default is "^(string_of_int !measure_step) ;

    "-o", Arg.Set_string output_file, "<file> output result to <file>" ;

    "-l", Arg.Set_int length, "<n> set length for single/dynamic experiment" ;

    "-m", Arg.Unit (fun () -> mode := Multi),
    "run various experiences in parallel, with different lengths" ;

    "-d", Arg.Int (fun n -> mode := Dynamic n),
    "<n> run <n> experiences and output average dynamic behaviour";

    "--no-run-r", Arg.Clear auto_run_r,
    " do not run R on the output file, when R code is generated" ;
  ]

let law_files = ref [];;

let usage =
  Printf.sprintf "Usage: %s [options] [laws.cm[o|a|x|xs] ...]\nwhere options are:" Sys.argv.(0);;

(** {2 Loading laws} *)

let make_trie law =
  let module L = (val law : Vlmc.Law) in
  let module V = Vlmc.Make (L) in
  let module T = Trie.Make(V) in
  (module T : Trie.S)
;;

let load_law_file file =
   try Dynlink.loadfile file
  with Dynlink.Error e ->
      failwith (Dynlink.error_message e)
;;


(** {2 Single experiment} *)

module Single (Trie: Trie.S) =
  struct
    let output_dot ?depth vlmc trie file =
      let oc = open_out file in
      output_string oc (Trie.dot ?depth vlmc trie);
      close_out oc


    let experiment ?(dot=false) len =
      let vlmc = Trie.Vlmc.create len in
      let trie = ref Trie.empty in
(*
  for i = 0 to 30 do print_int (Seq.get vlmc i); done;
  print_newline();
*)
      for i = 0 to len - 1 do
        trie := Trie.insert vlmc !trie i;
(*
    let (_,leaves) = Trie.cardinal !trie in
    prerr_endline (Printf.sprintf "leaves=%d" leaves);
*)
        if i < 15 && dot then
          begin
            let file = Printf.sprintf "trie%02d.dot" i in
            output_dot vlmc !trie file;
            ignore(Sys.command
             (Printf.sprintf "dot -Tpng -o %s.png %s" (Filename.quote file) (Filename.quote file)));
          end;
        (*
           assert (leaves = i+1)
        *)  done;
      (vlmc, !trie)

    let run length =
      let (vlmc, trie) = experiment ~dot: true length in
      let (nodes, leaves) = Trie.cardinal trie in
      prerr_endline (Printf.sprintf "vlmc=%s, nodes=%d, leaves=%d" Trie.Vlmc.Law.description nodes leaves);
      let height = Trie.height trie in
      prerr_endline (Printf.sprintf "height=%d" height);
      output_dot vlmc trie (Printf.sprintf "%s.dot" Trie.Vlmc.Law.id)
end;;

(** {2 Start} *)

let single_exp length trie =
  let module Trie = (val trie : Trie.S) in
  let module S = Single(Trie) in
  S.run length
;;

let main () =
  Arg.parse options (fun file -> law_files := file :: !law_files) usage;
  begin
    match !law_files with
      [] -> failwith "No law defined"
    | _ -> List.iter load_law_file !law_files
  end;
  let tries = List.map make_trie !Vlmc_laws.laws in
  match !mode with
    Single -> List.iter (single_exp !length) tries
  | _ -> failwith "not implemented"

;;

(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let () = safe_main main ;;
