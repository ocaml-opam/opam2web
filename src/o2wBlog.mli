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

(** Home page *)

open OpamTypes
open O2wTypes

type post


(** Generate the blog *)
val get_entries: content_dir:string -> pages:string list -> post list

val make_pages: post list -> Cow.Xml.signal list list

(** Main entry (last) as "Blog", and invisible list of older entries *)
val make_menu: post list -> menu * menu list

(** xhtml to include in main page *)
val make_news: post list -> Cow.Xml.t

(** Atom feed *)
val make_feed: post list -> Cow.Xml.t

