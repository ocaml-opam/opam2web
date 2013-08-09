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

(** Create a list of package pages to generate for a repository *)
val to_pages: href_prefix:string -> statistics:statistics_set option ->
  repository_info -> page list

(** Generate the list of HTML links for a list of page names *)
val sortby_links:
  href_prefix:string ->
  links:string list ->
  default:string ->
  active:string ->
  Cow.Html.t list

(** Returns a HTML list of the packages in the given repository *)
val to_html:
  sortby_links:(active:string -> Cow.Html.t list) ->
  popularity:int64 name_map ->
  active:string ->
  compare_pkg:(package -> package -> int) ->
  repository_info -> Cow.Html.t

(** Load a repository from the local OPAM installation *)
val of_opam: href_prefix:string -> repository_name -> repository_info

(** Load a repository from a directory *)
val of_path: href_prefix:string -> dirname -> repository_info
