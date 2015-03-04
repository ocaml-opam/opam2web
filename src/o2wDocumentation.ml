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
    let base = String.sub file 0 dot_index in
    let ext =
      String.sub file (dot_index + 1) (String.length file - dot_index - 1)
    in
    if String.contains ext ' ' then raise Not_found
    else base, ext
  with
    Not_found -> file, ""

(* Returns [(source_file, extension, link, menu_entry); ...] *)
let read_menu ~dir f =
  let lines = OpamProcess.read_lines f in
  let lines =
    List.filter (fun s -> String.length s = 0 || s.[0] <> '#') lines
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
        Printf.sprintf "%s/%s.%s" dir title extension
      in
      let source_filename = OpamFilename.of_string source_file in
      let dest_file = Printf.sprintf "%s.html" title in
      (source_filename, extension,
       { text=human_title; href=dest_file },
       Internal (1, Template.serialize Cow.Html.nil))
  in
  List.map aux_menu lines


let doc_toc md_content =
  let module O = Omd_representation in
  let toc = Omd.toc ~depth:4 md_content in
  (* Remove nested links *)
  let rec remove_links md =
    O.visit (function
        | O.Url (_,o,_) -> Some (remove_links o)
        | _ -> None)
      md
  in
  let toc =
    O.visit (function
        | O.Url (href,o,title) -> Some [O.Url (href, remove_links o, title)]
        | _ -> None)
      toc
  in
  (* Strip main title and what is before *)
  let toc = match toc with
    | [O.Ul first_level] ->
      (let rec skip_head = function
          | [] -> first_level
          | [O.Ul _] :: r -> skip_head r
          | h1 -> h1
       in
       match skip_head first_level with
       | [] -> []
       | [second_level] ->
         List.filter (function O.Ul _ -> true | _ -> false) second_level
       | _ -> [O.Ul first_level])
    | toc -> toc
  in
  (* Strip empty intermediate levels *)
  let toc =
    O.visit (function
        | O.Ul [[O.Ul _] as sub] -> Some sub
        | _ -> None)
      toc
  in
  toc

(* Generate the HTML corresponding to a documentation page in the <content>/doc
   directory *)
let to_menu_aux ~content_dir ~subdir ?(header=Cow.Html.nil) ~menu_pages =

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
        let md_toc = doc_toc md_content in
        let html_toc = Cow.Html.of_string (Omd.to_html md_toc) in
        <:html<
          <div class="row">
            <div class="span3">
          <div class="bs-docs-menu hidden-phone">
            $doc_menu$
            <div class="bs-docs-toc">
              $html_toc$
            </div>
          </div>
          </div>
          <div class="span9">
          $header$
          $Cow.Html.of_string (Omd.to_html md_content)$
          </div>
        </div>
      >>
    | _ -> <:html< >>
  in

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
          menu_source = OpamFilename.to_string source_filename;
          menu_link = lnk;
          menu_item = page
        }
    | Internal (level, _) | No_menu (level, _) ->
        let doc_menu = documentation_menu source_filename in
        let html_page = to_html doc_menu extension source_filename in
        {
          menu_source = OpamFilename.to_string source_filename;
          menu_link = { lnk with href = subdir ^ "/" ^ lnk.href };
          menu_item = Internal (level, Template.serialize html_page);
        }
  in

  let menu = List.map aux_page menu_pages in
  if List.exists (fun (src, _, _, _) ->
      OpamFilename.(Base.to_string (basename (chop_extension src)))
      = "index")
      menu_pages
    then menu
    else
      let doc_menu = documentation_menu (OpamFilename.of_string "index") in
      let html_index =
        <:html<
          <div class="row"><div class="span4 offset4">
            $doc_menu$
          </div></div>
        >>
      in
      {
        menu_source = "index.html";
        menu_link = { text = "Documentation index"; href = subdir ^ "/" };
        menu_item = No_menu (List.length (OpamMisc.split subdir '/'),
                             Template.serialize html_index)
      } :: menu

let to_menu ~content_dir =
  let (/) = Filename.concat in
  let get_header name =
    Cow.Html.of_string @@
    Omd.to_html @@
    Omd.of_string @@
    OpamFilename.read @@
    OpamFilename.of_string name
  in
  let menu_pages_11 =
    read_menu ~dir:(content_dir / "doc" / "1.1")
      (content_dir / "doc" / "1.1" / "index.menu")
  in
  let menu_11 =
    to_menu_aux ~content_dir ~subdir:("doc" / "1.1") ~menu_pages:menu_pages_11
      ~header:(get_header (content_dir / "doc" / "1.1" / "opam11_note.md"))
  in
  let menu_pages_12 =
    read_menu ~dir:(content_dir / "doc")
      (content_dir / "doc" / "index.menu")
  in
  let menu_12 =
    to_menu_aux ~content_dir ~subdir:"doc" ~menu_pages:menu_pages_12
      ?header:None
  in
  let menu_11 =
    OpamMisc.filter_map (function
        | {menu_item = Internal (_, html) | No_menu (_, html)} as m ->
            Some {m with menu_item = No_menu (2, html)}
        | _ -> None)
      menu_11
  in
  menu_11 @
  menu_12 @
  [{
    menu_source = "1.1";
    menu_link = { text = "Archives (OPAM 1.1)";
                  href = "/doc/1.1/" };
    menu_item = External;
  }]
