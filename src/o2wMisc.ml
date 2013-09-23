(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

(* List related functions *)
(* Retrieve the 'n' first elements of a list *)
let first_n nmax l =
  let rec aux acc n = function
    | hd :: tl when n > 0 -> aux (hd :: acc) (n - 1) tl
    | _ -> acc
  in
  List.rev (aux [] nmax l)

(* Date related functions *)

let month_of_string = function
  | "Jan" -> 0
  | "Feb" -> 1
  | "Mar" -> 2
  | "Apr" -> 3
  | "May" -> 4
  | "Jun" -> 5
  | "Jul" -> 6
  | "Aug" -> 7
  | "Sep" -> 8
  | "Oct" -> 9
  | "Nov" -> 10
  | "Dec" -> 11
  | unknown -> failwith ("Unknown month: " ^ unknown)

let string_of_month = function
  | 0  -> "Jan"
  | 1  -> "Feb"
  | 2  -> "Mar"
  | 3  -> "Apr"
  | 4  -> "May"
  | 5  -> "Jun"
  | 6  -> "Jul"
  | 7  -> "Aug"
  | 8  -> "Sep"
  | 9  -> "Oct"
  | 10 -> "Nov"
  | 11 -> "Dec"
  | unknown -> failwith ("Unknown month: " ^ (string_of_int unknown))

let string_of_timestamp ?(short = false) time =
  let tm = Unix.gmtime time in
  let open Unix in
  let month_str = string_of_month tm.tm_mon in
  let year = 1900 + tm.tm_year in
  if short then
    Printf.sprintf "%s %d" month_str tm.tm_mday
  else
    Printf.sprintf "%s %d, %d" month_str tm.tm_mday year
