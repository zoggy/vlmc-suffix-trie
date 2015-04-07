(*********************************************************************************)
(*                Vlmc-suffix-trie                                               *)
(*                                                                               *)
(*    Copyright (C) 2012 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License version 3             *)
(*    or later as published by the Free Software Foundation.                     *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software Foundation,    *)
(*    Inc., 59 Temple Place, Suite 330, Boston, MA                               *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

let () = Random.self_init();;

type direction = N | S | E | W;;
let directions = [| N ; S ; E ; W |];;

module type Walk_law = Vlmc.Law with type symbol = direction;;

module Random_walk (L:Walk_law) =
  struct
    module Vlmc = Vlmc.Make (L)

    let compute length =
      let seq = Vlmc.create length in
      let t = Array.make length (0, 0) in
      let vector = function
        N -> (0, 1)
      | S -> (0, -1)
      | E -> (1, 0)
      | W -> (-1, 0)
      in
      let add (x1,y1) (x2, y2) = (x1 + x2, y1 + y2) in
      for i = 1 to length - 1 do
        let dir =  Vlmc.get seq i in
        t.(i) <- add t.(i-1) (vector dir);
        if t.(i) = (0, 0) then
         prerr_endline (Printf.sprintf "bing, reached (0,0) for n = %d" i);
      done;
      t
  end
;;

let output_R r_file points =
  let oc = open_out r_file in
  let p = Printf.fprintf in
  let points = Array.to_list points in
  let min_x = ref max_int in
  let max_x = ref min_int in
  let min_y = ref max_int in
  let max_y = ref min_int in
  let x = Vlmc_misc.r_vector "x"
    (fun (x,_) -> min_x := min x !min_x ; max_x := max x !max_x; string_of_int x) points
  in
  let y = Vlmc_misc.r_vector "y"
    (fun (_,y) -> min_y := min y !min_y ; max_y := max y !max_y; string_of_int y) points
  in
  let out_file = (try Filename.chop_extension r_file with _ -> r_file)^".jpg" in
  p oc "jpeg(%s, width=1200)\n" (Filename.quote out_file);
  p oc "%s" x;
  p oc "%s" y;
  p oc "plot(0, 0, col=\"red\", xlim=c(%d,%d), ylim=c(%d,%d), cex=2, lwd=2)\n"
    (!min_x - 1) (!max_x + 1) (!min_y - 1) (!max_y + 1);
  p oc "lines(x, y, type=\"l\", col=\"black\", cex=.01, lwd=2)\n";
  p oc "points(0, 0, col=\"red\", cex=2, lwd=2)\n";
(*
  p oc "%s" "x1 <- x[2:length(x)]\n";
  p oc "%s" "y1 <- y[2:length(x)]\n";
  p oc "arrows(x, y, x1, y1, code = 2, length = 0.1, angle = 30, col=\"black\",  cex=.01, lwd=2)\n";
*)
  p oc "dev.off()\n";
  close_out oc
;;

let make_law law ~desc ~id =
  let module Law =
    struct
     type symbol = direction = N | S | E | W
     let compare = Pervasives.compare
     let string = function N -> "N" | S -> "S" | E -> "E" | W -> "W"
     let symbols = directions
     let description = desc
     let id = id
     let next = law
    end
  in
  (module Law : Walk_law)
;;

let count_back =
  let rec iter get sym pos =
    match get pos with
      None -> (sym, pos)
    | Some s when s = sym -> iter get sym (pos+1)
    | _ -> (sym, pos)
  in
  fun get ->
    match get 0 with
    None -> (directions.(Random.int (Array.length directions)), 1)
  | Some d -> iter get d 1
;;

let complement_dir = function
  | N | S -> [| E ; W |]
  | E | W -> [| N ; S |]
;;

let p_log n = (1. -. (4. /. ((float n) +. 4.)));;
let p_novar n = (1. -. (2. /. ((float n) +. 2.)));;
let p_fact n = (1. /. ((float n) +. 2.));;

let make_simple_law f_p get =
  let (d,n) = count_back get in
  let p = f_p n in
  let q = Random.float 1. in
  if p >= q then
    d
  else
    (
     let t = complement_dir d in
     t.(Random.int 2)
    )
;;
let law_log = make_law (make_simple_law p_log) ~desc: "log_comb" ~id: "log_comb";;
let law_fact = make_law (make_simple_law p_fact) ~desc: "fact" ~id: "fact";;
let law_novar = make_law (make_simple_law p_novar) ~desc: "no variance" ~id: "novar";;
module Law_log = (val law_log : Walk_law);;
module Law_fact = (val law_fact : Walk_law);;
module Law_novar = (val law_novar : Walk_law);;

(*
module W = Random_walk (Law_log);;
module W = Random_walk (Law_fact);;
*)
module W = Random_walk (Law_novar);;

let result = W.compute 10000;;
let file = "walk.R";;
let () = output_R file result;;
let () = Vlmc_misc.run_r file;;






