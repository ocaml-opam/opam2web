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

open O2wTypes

(** Basic json functions : construct the association with the content. Fields
    are named as follow: "package", "name", "version", "date", "downloads",
    "month_downloads". *)
val json_package: package -> string * Yojson.t
val json_name: name -> string * Yojson.t
val json_version: version -> string * Yojson.t
val json_downloads: int64 -> string * Yojson.t
val json_month_downloads: int64 -> string * Yojson.t
val json_timestamp: float -> string * Yojson.t

(** Basic json function with option argument. If [None], the value of the
    string * Yojson.tiation is `null` *)
val json_downloads_opt: int64 option -> string * Yojson.t
val json_month_downloads_opt: int64 option -> string * Yojson.t
val json_timestamp_opt: float option -> string * Yojson.t

(** [write filename json] outputs json into json/[filename] file *)
val write: string -> Yojson.t -> unit
