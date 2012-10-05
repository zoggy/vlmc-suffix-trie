(** Running experiences with tries on vlmc. *)

(** {2 Command line arguments} *)

let measure_step = ref 10;;

let length = ref 1_000;;
let n_exp = ref 5_000;;

type mode = Single | Multi | Dynamic of int;;
let mode = ref Single;;

let auto_run_r = ref true;;

let options = [
    "--measure-step", Arg.Set_int measure_step,
    "<n> set measure step to <b>; default is "^(string_of_int !measure_step) ;

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

(** {2 Utilities} *)

(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)

let run_r r_file =
  let com = Printf.sprintf "R --vanilla --slave < %s" (Filename.quote r_file) in
  match Sys.command com with
    0 -> ()
  | n ->
      let msg = Printf.sprintf "Command %s returned error code %d"
        (Filename.quote com) n
      in
      failwith msg
;;

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
      file_of_string ~file (Trie.dot ?depth vlmc trie)

    let experiment ?(dot=false) len =
      let vlmc = Trie.Vlmc.create len in
      let trie = ref Trie.empty in
      for i = 0 to len - 1 do
        trie := Trie.insert vlmc !trie i;
        if i < 15 && dot then
          begin
            let file = Printf.sprintf "trie%02d.dot" i in
            output_dot vlmc !trie file;
            ignore(Sys.command
             (Printf.sprintf "dot -Tpng -o %s.png %s" (Filename.quote file) (Filename.quote file)));
          end;
      done;
      (vlmc, !trie)

    let run length =
      let (vlmc, trie) = experiment ~dot: true length in
      let (nodes, leaves) = Trie.cardinal trie in
      prerr_endline (Printf.sprintf "vlmc=%s, nodes=%d, leaves=%d" Trie.Vlmc.Law.description nodes leaves);
      let height = Trie.height trie in
      prerr_endline (Printf.sprintf "height=%d" height);
      output_dot vlmc trie (Printf.sprintf "%s.dot" Trie.Vlmc.Law.id)
end;;

module Dynamic (Trie : Trie.S) =
  struct
    let dynamic_experiment ?(dot=false) ?progress len =
      let vlmc = Trie.Vlmc.create len in
      let trie = ref Trie.empty in
      let heights = Array.create (len / !measure_step) 0 in
      let saturations = Array.create (len / !measure_step) 0 in

      for i = 0 to len - 1 do
        trie := Trie.insert vlmc !trie i ;
        if i mod !measure_step = 0 then
          heights.(i / !measure_step) <- Trie.height !trie ;
        saturations.(i / !measure_step) <- Trie.saturation_level !trie ;
        begin
          match progress with
            None -> ()
          | Some f ->
              f (vlmc, !trie, heights, saturations) i
        end;
      done;
      let file = "trie_final.dot" in
      file_of_string ~file (Trie.dot ~depth: 5 vlmc !trie);
      ignore(Sys.command
       (Printf.sprintf "dot -Tpng -o %s.png %s" (Filename.quote file) (Filename.quote file)));
      (vlmc, !trie, heights, saturations)

    let r_vector name to_s l =
      Printf.sprintf "%s=c(%s)\n"
      name
      (String.concat ", " (List.rev (List.rev_map to_s l)))

    let generate_R_dynamic_exp ?(exp=1) heights sats jpg_file =
      let r_var_prefix = Trie.Vlmc.Law.id in
      let len = Array.length heights in
      let b = Buffer.create 256 in
      let x = Array.init len (fun i -> (i+1) * !measure_step) in
      let max_y =
        Array.fold_left max
        (Array.fold_left max 0. heights)
        sats
      in
      let var_heights = Printf.sprintf "%sheights" r_var_prefix in
      let var_sats = Printf.sprintf "%ssats" r_var_prefix in
      let code_x = r_vector "x" string_of_int (Array.to_list x) in
      let code_heights = r_vector var_heights string_of_float (Array.to_list heights) in
      let code_sats = r_vector var_sats string_of_float (Array.to_list sats) in
      begin
        match r_var_prefix with
          "" -> ()
        | s ->
            let file = Printf.sprintf "%s.R" s in
            let oc = open_out file in
            List.iter
            (output_string oc) [ code_x ; code_heights ; code_sats ];
            Printf.fprintf oc "measure_step=%d;\n" !measure_step;
            close_out oc;
      end;
      Buffer.add_string b code_x;
      Buffer.add_string b code_heights;
      Buffer.add_string b code_sats;
      Printf.bprintf b "jpeg(\"%s\", width=800);\n" jpg_file;
      Printf.bprintf b "plot(0,0,xlab=\"Number of suffixes inserted\", ylab=\"height\",xlim=c(0,%d),ylim=c(0,%f),cex=.0)\n" (len * !measure_step) max_y;
      Printf.bprintf b "lines(x, %s, col=\"black\",lty=1, lwd=2)\n" var_heights;
      Printf.bprintf b "lines(x, %s, col=\"darkgrey\", lty=2, lwd=2)\n" var_sats;
      Printf.bprintf b "legend(x=\"topleft\", c(\"height\", \"saturation level\"), col = c(\"black\",\"darkgrey\"), lty=c(1,2), lwd=c(2,2))\n";
      Printf.bprintf b "title(main=\"%s%sdynamic behaviour -- %d simulation%s\")\n"
        Trie.Vlmc.Law.description
        (if Trie.Vlmc.Law.description = "" then "" else " -- ") exp
        (if exp > 1 then "s" else "");
      Buffer.contents b

    let run nb_exp length =
      let prefix = Printf.sprintf "%s-%dx%d" Trie.Vlmc.Law.id nb_exp length in
      prerr_endline (Printf.sprintf "Law %S - Dynamic (%d)" Trie.Vlmc.Law.id nb_exp);
      let (heights, sats) =
        let f i =
          let progress _ j =
            if j mod 10_000 = 0 then
              prerr_endline (Printf.sprintf "task %d: progress=%d" i j)
          in
          let res = dynamic_experiment ~progress length in
          prerr_endline (Printf.sprintf "task %d done" i);
          let (_,_,h,sats) = res in
          (h, sats)
        in
        Functory.Cores.set_number_of_cores 5;
        let results = Functory.Cores.map
          (*List.map*)
          f
          (Array.to_list (Array.init nb_exp (fun i -> i)))
        in
        let i_avg l i =
          let v =
            List.fold_left
            (fun acc t -> (float t.(i)) +. acc)
            0.0 l
          in
          v /. (float (List.length l))
        in
        let array_avg l = Array.init (length / !measure_step) (i_avg l) in
        let heights = List.map fst results in
        let sats = List.map snd results in
        let heights = array_avg heights in
        let sats = array_avg sats in
        (heights, sats)
      in
      let jpg_file = Printf.sprintf "%s.jpg" prefix in
      let r_code = generate_R_dynamic_exp
        ~exp: nb_exp heights sats jpg_file
      in
      let r_file = prefix ^ ".R" in
      file_of_string ~file: r_file r_code;
      if !auto_run_r then run_r r_file
  end

(** {2 Start} *)

let single_exp length trie =
  let module Trie = (val trie : Trie.S) in
  let module S = Single(Trie) in
  S.run length
;;

let dynamic_exp nb_exp length trie =
  let module Trie = (val trie : Trie.S) in
  let module D = Dynamic(Trie) in
  D.run nb_exp length
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
  | Dynamic nb_exp -> List.iter (dynamic_exp nb_exp !length) tries
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
