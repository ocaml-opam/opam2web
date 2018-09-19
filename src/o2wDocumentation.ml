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

open Cow
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

(* Returns [(source_file, extension, link, link_text, menu_entry); ...] *)
let read_menu ~dir f =
  let lines = OpamProcess.read_lines f in
  let srcurl =
    let prefix = "#source:" in
    try
      let s = List.find (OpamStd.String.starts_with ~prefix) lines in
      Some (String.trim (OpamStd.String.remove_prefix ~prefix s))
    with Not_found -> None
  in
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
        (empty_filename, "", Uri.empty, human_title, Nav_header)
      else
        (empty_filename, "", Uri.empty, "", Divider)
    else
      let source_file =
        Printf.sprintf "%s/%s.%s" dir title extension
      in
      let source_filename = OpamFilename.of_string source_file in
      let dest_file = Printf.sprintf "%s.html" title in
      (source_filename, extension,
       Uri.make ~path:dest_file (), human_title,
       Internal (1, Template.serialize Cow.Html.nil)
      )
  in
  List.map aux_menu lines, srcurl


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
let to_menu_aux ~content_dir ~subdir ?(header=Cow.Html.nil) ~menu_pages ~srcurl =

  (* Convert a content page to html *)
  let to_html doc_menu kind filename: Cow.Html.t =
    if not (OpamFilename.exists filename) then (
      OpamConsole.warning "%s is not available." (OpamFilename.to_string filename);
      Html.empty
    ) else
      let content = OpamFilename.read filename in
      match kind with
      | "html" -> Cow.Html.of_string content
      | "md" ->
        let md_content = Omd.of_string content in
        let md_toc = doc_toc md_content in
        let html_toc = Cow.Html.of_string (Omd.to_html md_toc) in
        Html.div ~cls:"row"
          (Html.div ~cls:"span3"
             (Html.div ~cls:"bs-docs-menu hidden-phone"
                (doc_menu
                 @ Html.div ~cls:"bs-docs-toc" html_toc))
           @ Html.div ~cls:"span9"
               (header
                @ Cow.Html.of_string (Omd.to_html md_content)))
    | _ -> Html.empty
  in

  let documentation_menu active_src =
    let menu_items =
      List.map (fun (src, _, lnk, lnk_text, kind) -> match kind with
        | Submenu _ | No_menu _ -> Cow.Html.nil
        | Nav_header ->
           Html.li ~cls:"disabled"
             (Html.a ~href:(Uri.make ~fragment:"" ())
                (Html.strong (Html.string lnk_text)))
        | Divider ->
           Html.li ~cls:"disabled divider"
             (Html.a ~href:(Uri.of_string "#") (Html.string " "))
        | External | Internal _ ->
           let classes = if active_src = src then Some "active" else None in
           Html.li ?cls:classes
             (Html.a ~href:lnk
                (Html.string (" " ^ lnk_text)))
      ) menu_pages
    in
    Html.ul ~add_li:false ~cls:"nav nav-pills nav-stacked" menu_items
  in

  (* Pages creation *)
  let aux_page (source_filename, extension, lnk, lnk_text, page) =
    match page with
    | Submenu _ | Nav_header | Divider | External ->
        {
          menu_source = OpamFilename.to_string source_filename;
          menu_link = lnk;
          menu_link_text = lnk_text;
          menu_link_html = Html.string lnk_text;
          menu_item = page;
          menu_srcurl = None;
        }
    | Internal (level, _) | No_menu (level, _) ->
        let doc_menu = documentation_menu source_filename in
        let html_page = to_html doc_menu extension source_filename in
        let srcurl = match srcurl with
          | None -> None
          | Some d ->
            Some (d ^ "/" ^
                  OpamFilename.(Base.to_string (basename source_filename)))
        in
        {
          menu_source = OpamFilename.to_string source_filename;
          menu_link = Uri.with_path lnk (subdir ^ "/" ^ Uri.path lnk);
          menu_link_text = lnk_text;
          menu_link_html = Html.string lnk_text;
          menu_item = Internal (level, Template.serialize html_page);
          menu_srcurl = srcurl;
        }
  in

  let menu = List.map aux_page menu_pages in
  if List.exists (fun (src, _, _, _, _) ->
      try
        OpamFilename.(Base.to_string (basename
           (chop_extension src)))
        = "index"
      with Invalid_argument _ -> false)
      menu_pages
    then menu
    else
      let doc_menu = documentation_menu (OpamFilename.of_string "index") in
      let html_index = Html.div ~cls:"row"
                         (Html.div ~cls:"span4 offset4" doc_menu) in
      let srcurl = match srcurl with
        | None -> None
        | Some src -> Some (src ^"/"^ "index.menu")
      in
      {
        menu_source = "index.html";
        menu_link = Uri.make ~path:(subdir ^ "/") ();
        menu_link_text = "Documentation index";
        menu_link_html = Html.string "Documentation index";
        menu_item = No_menu (List.length (OpamStd.String.split subdir '/'),
                             Template.serialize html_index);
        menu_srcurl = srcurl;
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
  let mk_menu ?header subdir =
    let menu_pages, srcurl =
      read_menu ~dir:(content_dir / subdir)
        (content_dir / subdir / "index.menu")
    in
    let menu = to_menu_aux ~content_dir ~subdir ~menu_pages ~srcurl ?header in
    menu, srcurl
  in
  (* Main (current) 1.2 help menu *)
  let menu_20, srcurl_20 =
    mk_menu "doc" ?header:None
  in
  (* Old (legacy) 1.1 help menu *)
  let menu_11, srcurl_11 =
    mk_menu ("doc" / "1.1")
      ~header:(get_header (content_dir / "doc" / "1.1" / "opam11_note.md"))
  in
  let menu_11 =
    OpamStd.List.filter_map (function
        | {menu_item = Internal (_, html) | No_menu (_, html)} as m ->
            Some {m with menu_item = No_menu (2, html)}
        | _ -> None)
      menu_11
  in
  (* Old (legacy) 1.2 help menu *)
  let menu_12, srcurl_12 =
    mk_menu ("doc" / "1.2")
  in
  let menu_12 =
    OpamStd.List.filter_map (function
        | {menu_item = Internal (_, html) | No_menu (_, html)} as m ->
            Some {m with menu_item = No_menu (2, html)}
        | _ -> None)
      menu_12
  in
  menu_11 @
  menu_12 @
  menu_20 @
  [{
    menu_source = "1.1";
    menu_link = Uri.make ~path:("doc/1.1/") ();
    menu_link_text = "Archives (OPAM 1.1)";
    menu_link_html = Html.string "Archives (OPAM 1.1)";
    menu_item = External;
    menu_srcurl =
      match srcurl_11 with
      | None -> None
      | Some u -> Some (u ^ "/index.menu");
  };
  {
    menu_source = "1.2";
    menu_link = Uri.make ~path:("doc/1.2/") ();
    menu_link_text = "Archives (OPAM 1.2)";
    menu_link_html = Html.string "Archives (OPAM 1.2)";
    menu_item = External;
    menu_srcurl =
      match srcurl_12 with
      | None -> None
      | Some u -> Some (u ^ "/index.menu");
  }]
