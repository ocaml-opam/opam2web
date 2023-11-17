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

include module type of struct include OpamTypes end

type univ = {
  st: OpamStateTypes.unlocked OpamStateTypes.switch_state;
  dates: float package_map;
  depends: package_set package_map;
  rev_depends: package_set package_map;
  depopts: package_set package_map;
  rev_depopts: package_set package_map;
}

type page = {
  page_source  : string;
  page_link    : Uri.t;
  page_link_text: string;
  page_link_html: Cow.Html.t;
  page_depth   : int;
  page_contents: Cow.Xml.signal list;
  page_srcurl  : string option;
}

type menu = {
  menu_link: Uri.t;
  menu_link_text: string;
  menu_link_html: Cow.Html.t;
  menu_item: menu_item;
  menu_source: string;
  menu_srcurl: string option;
}

and menu_item =
  | Internal of int * Cow.Xml.signal list
  | No_menu of int * Cow.Xml.signal list
  | Submenu of menu list
  | Nav_header
  | Divider
  | External

type home_datasets = {
  nb_packages: int;
  last10_updates: (package * float) list ;
}
