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

type param_prop = Default of Cow.Xml.signal list | Mandatory
type field_prop = Optional | Required
type t = {
  path : string;
  fields : (string * (param_prop * field_prop)) list;
}
val xmlns : string
val serialize : Cow.Html.t -> Cow.Xml.signal list
val default : Cow.Html.t -> param_prop
val mandatory : unit -> param_prop
val generate :
  string -> t -> (string * Cow.Xml.signal list) list -> Cow.Xml.signal list
