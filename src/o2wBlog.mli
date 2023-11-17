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

type post


(** Generate the blog *)
val get_entries: content_dir:string -> pages:string list -> post list

val make_pages: post list -> Cow.Xml.signal list list

(** Main entry (last) as "Blog", and invisible list of older entries *)
val make_menu: ?srcurl:string -> post list -> menu list * menu list

(** xhtml to include in main page *)
val make_news: post list -> Cow.Xml.t

(** Atom feed *)
val make_feed: root:Uri.t -> post list -> Cow.Xml.t

(** Generate an html redirect to the latest post *)
val make_redirect: root:Uri.t -> post list -> Cow.Xml.t
