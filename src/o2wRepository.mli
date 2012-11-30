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

(** Repositories *)

open OpamTypes
open O2wTypes

(** Get all available packages *)
val get_dated_packages: OpamPath.Repository.r -> float package_map

(** Create a list of package pages to generate for a repository *)
val to_pages:
  statistics:statistics_set option ->
  dates:float package_map ->
  OpamPath.Repository.r -> page list

(** Generate the list of HTML links for a list of page names *)
val sortby_links:
  links:string list ->
  default:string ->
  active:string ->
  Cow.Html.t list

(** Returns a HTML list of the packages in the given repository *)
val to_html:
  sortby_links:(active:string -> Cow.Html.t list) ->
  dates:float package_map ->
  popularity:int64 name_map ->
  active:string ->
  compare_pkg:(package -> package -> int) ->
  OpamPath.Repository.r -> Cow.Html.t

(** Load a repository from the local OPAM installation *)
val of_opam: string -> OpamPath.Repository.r

(** Load a repository from a directory *)
val of_path: string -> OpamPath.Repository.r

(** Compute the reverse dependency matrix *)
val reverse_dependencies: OpamPath.Repository.r -> package_set -> name_set name_map
