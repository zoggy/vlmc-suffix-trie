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

(** *)

(*c==v=[List.make_int_list]=1.0====*)
let make_int_list ~low ~high =
  if low > high then
    []
  else
    let rec iter acc = function
	n when n <= high -> iter (n :: acc) (n+1)
      |	_ -> List.rev acc
    in
    iter [] low
(*/c==v=[List.make_int_list]=1.0====*)

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

let r_vector name to_s l =
  Printf.sprintf "%s=c(%s)\n"
  name
  (String.concat ", " (List.rev (List.rev_map to_s l)))
;;

