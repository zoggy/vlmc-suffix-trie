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

module Symbol = struct
  type symbol = int
  let compare = Pervasives.compare
  let string = string_of_int
  let symbols = [| 0 ; 1 |]
  end

module type P = sig
    val prob : int -> float
    val description : string
    val id : string
  end;;

module Make (P: P) = (struct
    include Symbol
    let count_zeros_back =
      let rec iter acc n get =
        match get n with
        | Some 0 -> iter (acc+1) (n+1) get
        | _ -> acc
      in
      iter 0 0

     let next get =
       let zeros = count_zeros_back get in
       let p = P.prob zeros in
       if p <= Random.float 1.0 then 1 else 0

    let description = P.description
    let id = P.id
end : Vlmc.Law)
