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

let header_separator = "^--BODY--$"

type post = {
  blog_title: string;
  blog_authors: (string * string option) list; (* name, url *)
  blog_date: float;
  blog_body: Cow.Html.t;
  blog_name: string;
}

(* format: 2014-08-09 17:44:00+02:00 *)
let parse_date filename d =
  let re =
    Re_str.regexp
      "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\
       \\( \\([0-9][0-9]\\):\\([0-9][0-9]\\)\\(:\\([0-9][0-9]\\)\\)?\\)?\
       \\(\\([-+][0-9][0-9]\\):\\([0-9][0-9]\\)\\)?"
  in
  if not (Re_str.string_match re d 0) then
    OpamGlobals.error_and_exit "Invalid date %S in %s"
      d
      (OpamFilename.to_string filename);
  let t =
    let ios i =
      try int_of_string (Re_str.matched_group i d)
      with Not_found | Invalid_argument _ -> 0 in
    Unix.({
        tm_year = ios 1 - 1900;
        tm_mon  = ios 2 - 1;
        tm_mday = ios 3;
        tm_hour = ios 5 - ios 10;
        tm_min  = (ios 6 - if ios 10 > 0 then ios 11 else 0 - ios 11);
        tm_sec  = ios 8;
        (* Initial dummy values *)
        tm_wday  = 0;
        tm_yday  = 0;
        tm_isdst = false;
      })
  in
  fst (Unix.mktime t)

let short_date timestamp =
  let open Unix in
  let d = gmtime timestamp in
  Printf.sprintf "%04d-%02d-%02d"
    (d.tm_year + 1900) (d.tm_mon + 1) d.tm_mday

let html_date timestamp =
  let d = Unix.(
      let d = gmtime timestamp in
      Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d"
        (d.tm_year + 1900) (d.tm_mon + 1) d.tm_mday
        d.tm_hour d.tm_min d.tm_sec
    ) in
  <:html< <time datetime="$str:d$">$str:short_date timestamp$</time>&>>

let to_entry ~content_dir filename =
  let name = Filename.chop_extension filename in
  let filename = OpamFilename.OP.(content_dir//filename) in
  let content = OpamFilename.read filename in
  match Re_str.bounded_split (Re_str.regexp header_separator) content 2 with
  | [] | [_] | _::_::_::_ ->
      OpamGlobals.note "Skipping %s: no header found"
        (OpamFilename.to_string filename);
      None
  | [header; body] ->
      let title, authors, date =
        let lexbuf = Lexing.from_string header in
        let s_filename = OpamFilename.to_string filename in
        lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
                                      Lexing.pos_fname = s_filename };
        let {OpamTypes.file_contents=s} = OpamParser.main OpamLexer.token lexbuf s_filename in
        let invalid = OpamFormat.invalid_fields s ["title";"authors";"date"] in
        if invalid <> [] then
          OpamGlobals.error_and_exit "Invalid fields %s in %s"
            (String.concat "," invalid) s_filename;
        let title = OpamFormat.assoc s "title" OpamFormat.parse_string in
        let authors =
          OpamFormat.(
            assoc_list s "authors"
              (parse_list (parse_option parse_string (fun x -> parse_string (List.hd x))))
          ) in
        let date = OpamFormat.assoc s "date" (fun f ->
            parse_date filename (OpamFormat.parse_string f)) in
        title, authors, date
      in
      let html_body =
        if OpamFilename.ends_with ".html" filename then
          Cow.Html.of_string body
        else if OpamFilename.ends_with ".md" filename then
          let md_content = Omd.of_string body in
          Cow.Html.of_string (Omd.to_html md_content)
        else OpamGlobals.error_and_exit "Unknown file extension for %s"
            (OpamFilename.to_string filename)
      in
      Some {
        blog_title = title;
        blog_authors = authors;
        blog_date = date;
        blog_body = html_body;
        blog_name = name;
      }

(* Generate the HTML corresponding to a blog page in the <content>/blog
   directory *)
let get_entries ~content_dir ~pages =

  let entries = List.map (to_entry ~content_dir:OpamFilename.OP.(OpamFilename.Dir.of_string content_dir / "blog")) pages in
  let entries = OpamMisc.filter_map (fun x -> x) entries in
  let entries =
    List.sort (fun {blog_date=a} {blog_date=b} -> compare b a) entries in

  OpamGlobals.msg "Correctly parsed %d blog entries:\n  - %s\n"
    (List.length entries)
    (String.concat "\n  - " (List.map (fun e -> e.blog_name) entries));
  entries

let make_pages entries =

  let nav_menu active_entry =
    let items =
      List.map (fun entry ->
          let classes = if active_entry = entry then "active" else "" in
          <:html<
            <li class="$str:classes$">
              <a href="$str:entry.blog_name^".html"$">$str: entry.blog_title$</a>
            </li>
          >>)
        entries
    in
    <:html<
      <ul class="nav nav-pills nav-stacked">
        $list:items$
      </ul>
    >>
  in
  (* Pages creation *)
  let aux_page entry =
    let html_authors =
      let to_html = function
        | author, Some url ->
            <:html< <a class="author" href="$str:url$">$str:author$</a> >>
        | author, None ->
            <:html< <a class="author">$str:author$</a> >>
      in
      match List.rev entry.blog_authors with
      | [] -> <:html< >>
      | [single] -> to_html single
      | last::secondlast::others ->
          List.fold_left (fun h a -> <:html< $to_html a$, $h >>)
            <:html< $to_html secondlast$ and $to_html last$ >>
            others
    in
    let html_body =
      <:html<
        <div class="page-header"><h3>
          $str:entry.blog_title$
          <div class="text-right"><small>
            On $html_date entry.blog_date$, by <span class="authors">$html_authors$</span>
          </small></div>
        </h3></div>
        $entry.blog_body$
      >>
    in
    let page =
      <:html<
        <div class="row">
          <div class="span3">
            <span> </span>
            <div class="bs-docs-menu" data-spy="affix"
                 data-offset-top="0" data-offset-bottom="140">
              $nav_menu entry$
            </div>
          </div>
          <div class="span9">
            $html_body$
          </div>
        </div>
      >>
    in
    Template.serialize page
  in

  List.map aux_page entries

let make_menu entries =
  let pages = make_pages entries in
  let link ?text entry = {
    text = OpamMisc.Option.default entry.blog_title text;
    href = "blog/" ^ entry.blog_name ^ ".html";
  } in
  let menu =
    List.map2 (fun entry page ->
        { menu_link = link entry;
          menu_item = No_menu (1, page) })
      entries pages
  in
  let latest = match entries, pages with
    | [], _ | _, [] -> []
    | first_entry::_, first_page::_ ->
      [{ menu_link = link ~text:"Blog" first_entry;
         menu_item = Internal (1, first_page) }]
  in
  latest, menu

let make_news entries =
  let oldest = Unix.time() -. 3600.*.24.*.365. in
  let news = List.filter (fun e -> e.blog_date > oldest ) entries in
  let mk entry =
    let link = "blog/" ^ entry.blog_name ^ ".html" in
    <:html<
      <p>
        <i class="icon-ok"> </i>
        <strong>$html_date entry.blog_date$</strong>
        <a href="$str:link$">$str:entry.blog_title$</a>
        <br/>
      </p>
    >>
  in
  List.fold_left (fun h e -> <:html< $h$ $mk e$ >>) <:html< >> news

let make_feed ~root entries =
  let open Cow.Atom in
  let to_atom_date date =
    let d = Unix.gmtime date in
    Unix.(d.tm_year + 1900, d.tm_mon + 1, d.tm_mday, d.tm_hour, d.tm_min)
  in

  let to_atom_entry entry = {
      entry = {
        id = entry.blog_name;
        title = entry.blog_title;
        subtitle = None;
        author = Some {
            name = OpamMisc.pretty_list (List.map fst entry.blog_authors);
            uri = snd (List.hd entry.blog_authors);
            email = None;
          };
        rights = None;
        updated = to_atom_date entry.blog_date;
        links = [ mk_link (Uri.of_string (root ^ "/blog/" ^ entry.blog_name ^ ".html")) ];
      };
      summary = None;
      content = entry.blog_body;
      base = None;
  }
  in

  let feed =
    let meta = {
      id = "ocaml-platform-blog";
      title = "The OCaml Platform Blog";
      subtitle = None;
      author = Some {
          name = "The OCaml Platform Team";
          uri = Some root;
          email = None;
        };
      rights = None;
      updated =
        to_atom_date (List.fold_left max 0. (List.map (fun e -> e.blog_date) entries));
      links = [ mk_link (Uri.of_string root) ];
    } in
    {
      feed = meta;
      entries = List.map to_atom_entry entries;
    }
  in
  Cow.Atom.xml_of_feed feed
