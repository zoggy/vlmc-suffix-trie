(** Drawing tries. *)

(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)

let () = Random.self_init();;
let seq = ref (Array.init 10 (fun _ -> Random.int 2));;
let output_file = ref "out.dot";;

let set_seq s =
  let len = String.length s in
  let a = Array.init len
    (fun i -> int_of_string (String.make 1 s.[i]))
  in
  seq := a
;;

let words = ref None;;

let options = [
  "-o", Arg.Set_string output_file, "<file> output to file instead of "^ !output_file ;
  "-s", Arg.String set_seq, "<{0|1}+> specify sequence to draw" ;
  "-l", Arg.Int (fun n -> words := Some n), "<n> insert only <n> first words of sequence" ;
]

module Fixed_symbol = struct
    type symbol = int
    let compare = Pervasives.compare
    let string = string_of_int
    let symbols = [| 0 ; 1 |]
  end
module Fixed_law = struct
    include Fixed_symbol
    let description = "fixed (0|1)"
    let id = "fixed"
    let next _ = assert false
  end


let main () =
  let law = ref false in
  let f_arg file =
    match !law with
      false -> Dynlink.loadfile file; law := true
    | true -> failwith (Printf.sprintf "Can't load %S. A law is already defined" file)
  in
  Arg.parse options
    f_arg (Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0));
  let module Fixed_vlmc =
  struct
    module Law = Fixed_law
      type t = int array
      type pos = int
    let create len = !seq
    let get t n =
      let b = Buffer.create 256 in
      Array.iter (fun n -> Printf.bprintf b "%d" n) t;
      prerr_endline (Printf.sprintf "getting %d in %s" n (Buffer.contents b));
      Array.get t n
    let first_diff_pos t pos1 pos2 =
      let rec iter n =
        let c1 = get t (pos1+n) in
        let c2 = get t (pos2+n) in
        if c1 = c2 then iter (n+1) else n
      in
      iter 0
  end
  in
  let module T = Trie.Make (Fixed_vlmc) in
  let t = ref T.empty in
  let vseq = T.Vlmc.create 100 in
  let len = match !words with None -> Array.length !seq | Some n -> n in
  for i = 0 to len - 1 do
    t:= T.insert vseq !t i
  done;
  let dot = T.dot ~rankdir: "BT" ~dots: true vseq !t in
  file_of_string ~file: !output_file dot
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

let () = safe_main main;;