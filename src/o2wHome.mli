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

(** Home page *)

open O2wTypes

val packages_prefix : string

(** Generate the homepage *)
val to_html:
  content_dir:string
  -> news:Cow.Xml.t
  -> home_datasets
  -> Cow.Xml.signal list

(** Build top10 last update dataset *)
val make_datasets: univ -> home_datasets


(** Generate a json files:
  - `last10_updates.json`: last 10 updated packages
    [
      {"package": string <pkg>,
       "downloads": int <nb dl>},
      {"package": ...
    ]
 *)
val generate_json: home_datasets -> unit
