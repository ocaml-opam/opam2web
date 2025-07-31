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

(** Statistics *)

open O2wTypes

(** Generate statistics on log entries *)
val statistics_set: filename list -> dirname list -> statistics_set option

(** Return the top packages *)
val top_packages: ?ntop:int -> ?reverse:bool -> (package -> 'a) ->
  package_set -> (package * 'a) list

(*
(** Export the popularity list into CSV format *)
val to_csv: int64 package_map -> string -> unit

(** Export the popularity list into JSON format *)
val to_json: int64 package_map -> string -> unit
*)
