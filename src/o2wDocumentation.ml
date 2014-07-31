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

open Cow.Html
open O2wTypes

type doc_kind =
  | Html
  | Markdown
  | Unknown of string
  | No_extension

let extension_of_kind: doc_kind -> string = function
  | Html -> "html"
  | Markdown -> "md"
  | Unknown ext -> ext
  | No_extension -> ""

let kind_of_extension: string -> doc_kind = function
  | "html" -> Html
  | "md" -> Markdown
  | "" -> No_extension
  | ext -> Unknown ext

(* Returns a pair (basename, extension) of the given filename *)
let split_filename (file: string): (string * string) =
  try
    let dot_index = String.rindex file '.' in
    (String.sub file 0 dot_index,
     String.sub file (dot_index + 1) (String.length file - dot_index - 1))
  with
    Not_found -> file, ""

(* Generate the HTML corresponding to a documentation page in the <content>/doc
   directory *)
let to_menu ~content_dir ~pages =

  (* Convert a content page to html *)
  let to_html doc_menu kind filename: Cow.Html.t =
    if not (OpamFilename.exists filename) then (
      OpamGlobals.warning "%s is not available." (OpamFilename.to_string filename);
      <:html< >>
    ) else
      let content = OpamFilename.read filename in
      match kind with
      | "html" -> Cow.Html.of_string content
      | "md" ->
        let md_content = Omd.of_string content in
        (* let md_toc = Omd.toc md_content in *)
        (* let html_toc = Cow.Html.of_string (Omd.to_html md_toc) in *)
        <:html<
          <div class="row">
            <div class="span3">
          <span> </span>
          <div class="bs-docs-menu"
              data-spy="affix"
              data-offset-top="0" data-offset-bottom="140">
            $doc_menu$
          </div>
          </div>
          <div class="span9">
          $Cow.Html.of_string (Omd.to_html md_content)$
          </div>
        </div>
      >>
    | _ -> <:html< >>
  in

  (* Documentation menu and links creation *)
  let aux_menu page =
    let title, extension = split_filename page in
    let human_title = Str.global_replace (Str.regexp "_") " " title in
    if String.length extension = 0 then
      let empty_filename = OpamFilename.of_string "" in
      if  String.length title > 0 then
        (empty_filename, "", { text=human_title; href="" }, Nav_header)
      else
        (empty_filename, "", { text=""; href="" }, Divider)
    else
      let source_file =
        Printf.sprintf "%s/doc/%s.%s" content_dir title extension
      in
      let source_filename = OpamFilename.of_string source_file in
      let dest_file = Printf.sprintf "%s.html" title in
      (source_filename, extension,
       { text=human_title; href=dest_file },
       Internal (1, Template.serialize Cow.Html.nil))
  in

  let menu_pages = List.map aux_menu pages in

  let documentation_menu active_src =
    let menu_items = List.map (fun (src, _, lnk, kind) -> match kind with
        | Submenu _ | No_menu _ -> Cow.Html.nil
        | Nav_header ->
          <:html<
            <li class="disabled">
              <a href="#"><strong>$str: lnk.text$</strong></a>
            </li>
          >>
        | Divider ->
          <:html<<li class="disabled divider"><a href="#"> </a></li>&>>
        | External | Internal _ ->
          let classes = if active_src = src then "active" else "" in
          <:html<<li class="$str: classes$">
            <a href="$str: lnk.href$"> $str: lnk.text$</a>
          </li>&>>
      ) menu_pages
    in
    <:html<
      <ul class="nav nav-pills nav-stacked">
        $list: menu_items$
      </ul>
    >>
  in

  (* Pages creation *)
  let aux_page (source_filename, extension, lnk, page) =
    match page with
    | Submenu _ | Nav_header | Divider | External ->
        {
          menu_link = lnk;
          menu_item = page
        }
    | Internal (level, _) | No_menu (level, _) ->
        let doc_menu = documentation_menu source_filename in
        let html_page = to_html doc_menu extension source_filename in
        {
          menu_link = { lnk with href = "doc/" ^ lnk.href };
          menu_item = Internal (level, Template.serialize html_page);
        }
  in

  List.map aux_page menu_pages
