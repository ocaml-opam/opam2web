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

(** Packages *)

open OpamTypes
open O2wTypes

(** Comparison function using string representation of an OpamPackage *)
val compare_alphanum: package -> package -> int

(** Compare packages by modification date *)
val compare_date: ?reverse:bool -> float package_map ->
  package -> package -> int

(** Compare packages by popularity *)
val compare_popularity: ?reverse:bool -> int64 name_map ->
  package -> package -> int

(** Return package info *)
val get_info: dates:float package_map ->
  OpamTypes.repository -> string option -> package -> package_info option

(** Returns a HTML description of the given package info *)
val to_html: href_prefix:string -> statistics:statistics_set option ->
  universe_info -> package_info -> Cow.Html.t

(** Return the hyper link for a given package *)
val href: ?href_prefix:string -> name -> version -> string

(** Predicate to determine if DNF predicates are satisfied *)
val are_preds_satisfied: universe_info -> package -> bool
