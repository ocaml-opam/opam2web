(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  Opam is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

(** Universe *)

open O2wTypes

(** Create a list of package pages to generate for a repository *)
val to_pages: prefix:string -> univ -> page list

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
  active:string ->
  compare_pkg:(package -> package -> int) ->
  univ -> Cow.Xml.signal list


val latest_version_packages: 'a OpamStateTypes.switch_state -> package_set

val load_opam_state : dirname list -> OpamStateTypes.unlocked OpamStateTypes.switch_state

val load: statistics_set option -> dirname list -> univ

(** Generate json file `stats.json` containing: all packages with timestamps,
    last month all downloads and non-adjacent downloads. The format is:
    [
      {"name": string <pkg name>,
       "versions":[
          {"version": string <pkg version>,
           "date": string <human readable date: month day, year>,
           "downloads": int <nb dl>,
           "month_downloads": int <nb dl>},
           {"version": ...
           }],
       "name": ...
    ]
*)
val generate_json : ?statistics:statistics_set -> univ -> unit

