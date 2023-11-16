(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
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

let header_separator = "--BODY--"

type post = {
  blog_source: string;
  blog_title: string;
  blog_authors: (string * string option) list; (* name, url *)
  blog_date: float;
  blog_body: Cow.Html.t;
  blog_name: string;
}

let empty_post = {
  blog_source = "";
  blog_title = "";
  blog_authors = [];
  blog_date = 0.;
  blog_body = Cow.Html.empty;
  blog_name = "";
}

(* format: 2014-08-09 17:44:00+02:00 *)
let parse_date =
  let digits n = Re.(repn digit n (Some n)) in
  let re =
    Re.(compile @@ whole_string @@ seq [
        group(digits 4); char '-'; group(digits 2); char '-'; group(digits 2);
        opt @@ seq [
          space; group(digits 2); char ':';
          group(digits 2);
          opt @@ seq [ char ':'; group(digits 2) ];
        ];
        opt @@ space;
        opt @@ str "UTC";
        opt @@ seq [group(seq [set "-+"; digits 2]); opt (char ':'); group(digits 2)];
      ])
  in
  fun s ->
    try
      let g = Re.exec re s in
      let get i =
        try int_of_string (Re.Group.get g i)
        with Not_found -> 0
      in
      let tm = {
        Unix.
        tm_year = get 1 - 1900;
        tm_mon  = get 2 - 1;
        tm_mday = get 3;
        tm_hour = get 4 - get 7;
        tm_min  = (get 5 - if get 7 > 0 then get 8 else 0 - get 8);
        tm_sec  = get 6;
        (* Initial dummy values *)
        tm_wday  = 0;
        tm_yday  = 0;
        tm_isdst = false;
      } in
      fst (Unix.mktime tm)
    with Not_found ->
      Printf.ksprintf failwith "Invalid date %S" s

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

let blog_header_parse =
  let open OpamPp.Op in
  let fields = [
    "title", OpamPp.ppacc
      (fun blog_title t -> {t with blog_title}) (fun t -> t.blog_title)
      OpamFormat.V.string;
    "authors", OpamPp.ppacc
      (fun blog_authors t -> {t with blog_authors}) (fun t -> t.blog_authors)
      (OpamFormat.V.map_list ~depth:1
         (OpamFormat.V.map_option OpamFormat.V.string
            (OpamPp.opt (OpamPp.singleton -| OpamFormat.V.string))));
    "date", OpamPp.ppacc
      (fun blog_date t -> {t with blog_date}) (fun t -> t.blog_date)
      (OpamFormat.V.string -| OpamPp.of_pair "date" (parse_date, short_date));
  ] in
  let pp =
    OpamFormat.I.map_file @@
    OpamFormat.I.fields ~name:"blog" ~empty:empty_post fields -|
    OpamFormat.I.show_errors ~name:"blog" ~strict:true ()
  in
  fun filename str ->
    let pos = OpamTypesBase.pos_file (OpamFile.filename filename) in
    OpamFile.Syntax.of_string filename str |>
    OpamPp.parse pp ~pos |>
    snd

let to_entry ~content_dir filename =
  let name = Filename.chop_extension filename in
  let extension = OpamStd.String.remove_prefix ~prefix:name filename in
  if extension <> ".md" && extension <> ".html" then None else
  let filename = OpamFilename.Op.(content_dir//filename) in
  let content = OpamFilename.read filename in
  let header_sep_re = Re.(compile @@ seq [ bol; str header_separator; eol ]) in
  match Re.split header_sep_re content with
  | [] | [_] ->
    OpamConsole.note "Skipping %s: no header found"
      (OpamFilename.to_string filename);
    None
  | header :: body ->
    let body = String.concat (header_separator^"\n") body in
    let blog = blog_header_parse (OpamFile.make filename) header in
    let html_body = match extension with
      | ".html" -> Cow.Html.of_string body
      | ".md" ->
        let md_content = Omd.of_string body in
        Cow.Html.of_string (Omd.to_html md_content)
      | _ -> assert false
    in
    Some { blog with
           blog_source = OpamFilename.to_string filename;
           blog_body = html_body;
           blog_name = name;
         }

(* Generate the HTML corresponding to a blog page in the <content>/blog
   directory *)
let get_entries ~content_dir ~pages =

  let entries = List.map (to_entry ~content_dir:OpamFilename.Op.(OpamFilename.Dir.of_string content_dir / "blog")) pages in
  let entries = OpamStd.List.filter_map (fun x -> x) entries in
  let entries =
    List.sort (fun a b -> compare b.blog_date a.blog_date) entries in

  OpamConsole.msg "Correctly parsed %d blog entries:\n  - %s\n"
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
    Html.ul ~add_li:false items ~cls:"nav nav-pills nav-stacked"
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
  let blog_link entry =
    Uri.make ~path:("blog/" ^ entry.blog_name ^ "/") () in
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
           menu_link_html = Html.string "Platform Blog";
           menu_item = Internal (2, first_page);
           menu_srcurl = srcurl first_entry; }] in
      let others =
        List.map2 (fun entry page ->
            { menu_source = entry.blog_source;
              menu_link = blog_link entry;
              menu_link_text = entry.blog_title;
              menu_link_html = Html.string entry.blog_title;
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
       @ Html.string " "
       @ Html.a ~href:link (Html.string entry.blog_title)
       @ Html.br)
  in
  List.fold_left (fun h e -> h @Html.string " " @ mk e) Html.empty news

let make_redirect ~root entries =
  match entries with
  | [] -> Html.p (Html.string "No blog pages.")
  | first_entry::_ ->
      let blog_uri =
        Uri.(resolve "http" root (of_string "/blog/"))
      in
      let post_uri =
        Uri.(resolve "http" blog_uri (of_string (first_entry.blog_name^"/")))
      in
      let redirect = Printf.sprintf "0;url=%s" (Uri.to_string post_uri) in
      Html.html
        (Html.head
           (Html.title (Html.string "Latest blog entry (redirect)")
            @ Html.meta ["http-equiv", "refresh"; "content", redirect])
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
    let entry_abs = Uri.resolve "http" root entry_path in
    let id = Uri.to_string entry_abs in
    {
      entry = {
        id;
        title = entry.blog_title;
        subtitle = None;
        author = Some {
          name = OpamStd.List.to_string fst entry.blog_authors;
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
