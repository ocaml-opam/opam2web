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

(** Remove base packages *)
val remove_base_packages: package_set -> package_set

(** Unify package versions by keeping the max one *)
val unify_versions: package_set -> package_set

(** Comparison function using string representation of an OpamPackage *)
val compare_alphanum: package -> package -> int

(** Compare packages by modification date *)
val compare_date: ?reverse:bool -> float package_map ->
  package -> package -> int

(** Compare packages by popularity *)
val compare_popularity: ?reverse:bool -> int64 name_map ->
  package -> package -> int

(** Return package info *)
val get_info:
  ?href_prefix:string ->
  dates:float package_map ->
  dirname -> package -> package_info

(** Returns a HTML description of the given package info *)
val to_html:
  dirname ->
  unique_packages:package_set ->
  reverse_dependencies:name_set name_map ->
  versions:version_set ->
  all_statistics:statistics_set option ->
  package_info -> Cow.Html.t
