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

module type Symbol = sig
  type symbol
  val compare : symbol -> symbol -> int
  val string : symbol -> string
  val symbols : symbol array
end

module type Law = sig
  include Symbol
  val description : string
  val id : string
  val next : (int -> symbol option) -> symbol
end

module type S =
  sig
    module Law : Law
    type t
    type pos = int

    val create : int -> t
    val get : t -> pos -> Law.symbol
    val first_diff_pos : t -> pos -> pos -> pos
  end

module Make : functor (L : Law) -> S