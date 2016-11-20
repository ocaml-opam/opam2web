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

let header_separator = "^--BODY--$"

type post = {
  blog_source: string;
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
  Html.time ~datetime:d (Html.string (short_date timestamp))

let to_entry ~content_dir filename =
  let name = Filename.chop_extension filename in
  let extension = OpamMisc.remove_prefix ~prefix:name filename in
  if extension <> ".md" && extension <> ".html" then None else
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
      let html_body = match extension with
        | ".html" -> Cow.Html.of_string body
        | ".md" ->
            let md_content = Omd.of_string body in
            Cow.Html.of_string (Omd.to_html md_content)
        | _ -> assert false
      in
      Some {
        blog_source = OpamFilename.to_string filename;
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
          Html.li ~cls:classes
            (Html.a ~href:(Uri.of_string("../"^entry.blog_name^"/"))
               (Html.string entry.blog_title)))
        entries
    in
    Html.ul items ~cls:"nav nav-pills nav-stacked"
  in
  (* Pages creation *)
  let aux_page entry =
    let html_authors =
      let to_html = function
        | author, Some url ->
           Html.a ~cls:"author" ~href:(Uri.of_string url) (Html.string author)
        | author, None ->
           Html.a ~cls:"author" (Html.string author)
             ~href:Uri.empty (* FIXME: remove with new version of Cow *)
      in
      match List.rev entry.blog_authors with
      | [] -> Html.empty
      | [single] -> to_html single
      | last::secondlast::others ->
         List.fold_left (fun h a -> to_html a @ Html.string ", " @ h)
           (to_html secondlast @ Html.string " and " @ to_html last)
           others
    in
    let html_body =
      Html.div ~cls:"page-header"
        (Html.h3
           (Html.string entry.blog_title
            @ Html.div ~cls:"text-right"
                (Html.small (Html.string "On "
                             @ html_date entry.blog_date
                             @ Html.string ", by "
                             @ Html.span ~cls:"authors" html_authors))))
      @ entry.blog_body
    in
    let page =
      Html.div ~cls:"row"
        (Html.div ~cls:"span3"
           (Html.div ~cls:"bs-docs-menu hidden-phone"
              (nav_menu entry))
         @ Html.div ~cls:"span9"
             html_body)
    in
    Template.serialize page
  in

  List.map aux_page entries

let make_menu ?srcurl entries =
  let pages = make_pages entries in
  let blog_link entry = "blog/" ^ entry.blog_name ^ "/" in
  let srcurl entry = match srcurl with
    | None -> None
    | Some u -> Some (u ^"/"^ Filename.basename entry.blog_source)
  in
  match entries, pages with
  | [], _ | _, [] -> [], []
  | first_entry::entries, first_page::pages ->
      let first =
        [{ menu_source = first_entry.blog_source;
           menu_link = blog_link first_entry;
           menu_link_text = "Platform Blog";
           menu_item = Internal (2, first_page);
           menu_srcurl = srcurl first_entry; }] in
      let others =
        List.map2 (fun entry page ->
            { menu_source = entry.blog_source;
              menu_link = blog_link entry;
              menu_link_text = entry.blog_title;
              menu_item = No_menu (2, page);
              menu_srcurl = srcurl entry; })
          entries pages
      in
      first, others

let make_news entries =
  let oldest = Unix.time() -. 3600.*.24.*.365. in
  let news = List.filter (fun e -> e.blog_date > oldest ) entries in
  let mk entry =
    let link = Uri.of_string("blog/" ^ entry.blog_name ^ "/") in
    Html.p
      (Html.i ~cls:"icon-ok" (Html.string " ")
       @ Html.strong (html_date entry.blog_date)
       @ Html.a ~href:link (Html.string entry.blog_title)
       @ Html.br Html.empty)
  in
  List.fold_left (fun h e -> h @Html.string " " @ mk e) Html.empty news

let make_redirect ~root entries =
  match entries with
  | [] -> Html.p (Html.string "No blog pages.")
  | first_entry::_ ->
      let blog_uri =
        Uri.(resolve "http" root (of_string "blog/"))
      in
      let post_uri =
        Uri.(resolve "http" blog_uri (of_string (first_entry.blog_name^"/")))
      in
      let redirect = Printf.sprintf "0;url=%s" (Uri.to_string post_uri) in
      Html.html
        (Html.head
           (Html.title (Html.string "Latest blog entry (redirect)")
            @ Html.meta ~attrs:["http-equiv", "refresh"; "content", redirect]
                Html.empty)
        @ Html.body Html.empty)

(*
let resolve_urls_in_attr base rewrites attrs =
  List.map (fun ((attr,v) as a) ->
    if List.mem attr rewrites
    then (attr,Uri.(to_string (resolve "http" base (of_string v))))
    else a
  ) attrs

let resolve_urls_in_tag base = function
  | (("","img"),attrs) ->
    (("","img"),resolve_urls_in_attr base ["","src"] attrs)
  | tag -> tag

let rec resolve_urls base = function
  | `Data d -> `Data d
  | `El (tag, body) ->
    `El (resolve_urls_in_tag base tag, List.map (resolve_urls base) body)
*)

let make_feed ~root entries =
  let open Cow.Atom in
  let to_atom_date date =
    let d = Unix.gmtime date in
    Unix.(d.tm_year + 1900, d.tm_mon + 1, d.tm_mday, d.tm_hour, d.tm_min)
  in

  let blog_uri = Uri.(resolve "http" root (of_string "blog/")) in
  let feed_uri = Uri.(resolve "http" blog_uri (of_string "feed.xml")) in
  let to_atom_entry entry =
    let entry_path = Uri.of_string (entry.blog_name ^ "/") in
    let entry_abs = Uri.resolve "http" blog_uri entry_path in
    let id = Uri.to_string entry_abs in
    {
      entry = {
        id;
        title = entry.blog_title;
        subtitle = None;
        author = Some {
          name = OpamMisc.pretty_list (List.map fst entry.blog_authors);
          uri = snd (List.hd entry.blog_authors);
          email = None;
        };
        rights = None;
        updated = to_atom_date entry.blog_date;
        links = [mk_link entry_abs;
                 mk_link ~rel:`alternate ~typ:"text/html" entry_abs];
      };
      summary = None;
      content = entry.blog_body;
      base = Some id;
    }
  in

  let feed =
    let meta = {
      id = Uri.to_string blog_uri;
      title = "The OCaml Platform Blog";
      subtitle = None;
      author = Some {
          name = "The OCaml Platform Team";
          uri = Some (Uri.to_string root);
          email = None;
        };
      rights = None;
      updated =
        to_atom_date (List.fold_left max 0. (List.map (fun e -> e.blog_date) entries));
      links = [ mk_link feed_uri ];
    } in
    {
      feed = meta;
      entries = List.map to_atom_entry entries;
    }
  in
  Cow.Atom.xml_of_feed feed
