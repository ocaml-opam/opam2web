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

(** Universe *)

open OpamTypes
open O2wTypes

(** Create a list of package pages to generate for a repository *)
val to_pages: statistics:statistics_set option ->
  Cow.Html.t OpamfUniverse.t -> page list

(** Generate the list of HTML links for a list of page names *)
val sortby_links:
  links:string list ->
  default:string ->
  active:string ->
  Cow.Html.t list

(** Returns a HTML list of the packages in the given repository *)
val to_html:
  content_dir:string ->
  sortby_links:(active:string -> Cow.Html.t list) ->
  popularity:int64 name_map ->
  active:string ->
  compare_pkg:(package -> package -> int) ->
  Cow.Html.t OpamfUniverse.t -> Cow.Xml.signal list

(** Generate a universe from a list of repositories *)
val of_repositories:
  ?preds:OpamfUniverse.pred list list -> OpamfUniverse.index ->
  OpamfUniverse.repository list -> Cow.Html.t OpamfUniverse.t
