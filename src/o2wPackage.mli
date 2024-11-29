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

(** Packages *)

open O2wTypes

(** Comparison function using string representation of an OpamPackage *)
val compare_alphanum: package -> package -> int

(** Compare packages by modification date *)
val compare_date: ?reverse:bool -> float package_map ->
  package -> package -> int

(** Compare packages by popularity *)
val compare_popularity: ?reverse:bool -> int64 name_map ->
  package -> package -> int

(** An HTML fragment for the description of a package *)
val html_descr : string * string -> Cow.Html.t

(** Returns a HTML description of the given package info *)
val to_html: prefix:string -> univ -> package -> Cow.Html.t

(** Returns the local link to get to a package page def *)
val pkg_href: ?href_base:Uri.t -> package -> Uri.t
