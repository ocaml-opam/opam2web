(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Miscelaneous functions *)

(** Return the [n] first elements of a list *)
val first_n: int -> 'a list -> 'a list

(** Return the month number (starting from 0) *)
val month_of_string: string -> int

(** Return the string representation of a month number *)
val string_of_month: int -> string

(** Return the string representation of a timestamp *)
val string_of_timestamp: ?short:bool -> float -> string
