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
  universe_info -> page list

(** Generate the list of HTML links for a list of page names *)
val sortby_links:
  href_prefix:string ->
  links:string list ->
  default:string ->
  active:string ->
  Cow.Html.t list

(** Returns a HTML list of the packages in the given repository *)
val to_html:
  href_prefix:string ->
  content_dir:string ->
  sortby_links:(active:string -> Cow.Html.t list) ->
  popularity:int64 name_map ->
  active:string ->
  compare_pkg:(package -> package -> int) ->
  universe_info -> Cow.Xml.signal list

(** Generate a universe from a list of repositories *)
val of_repositories: ?preds:pred list list -> repository list -> universe_info
