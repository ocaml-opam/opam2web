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

open Printf
open Cow
open O2wTypes

let prepend_root (depth: int) (src: Uri.t): Uri.t =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux ("../" ^ acc) (n-1)
  in
  let path = Uri.path src in
  if path <> "" && path.[0] = '/' then src
  else Uri.with_path src (aux path depth)

let create ~title ~header ~body ~footer ~depth =
  let title = Html.string title in
  let js_files = [
      "ext/js/google-code-prettify/prettify.js";
      "ext/js/jquery.js";
      "ext/js/site.js";
      "ext/js/search.js";
      "ext/js/bootstrap.min.js"
    ] in
  let prepend_root = prepend_root depth in
  let lnk ?attrs ~rel path =
    Html.link ~rel ?attrs (prepend_root (Uri.make ~path ()))
  in
  let head_html =
    Html.list [
      lnk ~rel:"icon" ~attrs:["type","image/png"] "ext/img/favicon.png";
      lnk ~rel:"stylesheet" "ext/css/bootstrap.css";
      lnk ~rel:"stylesheet" "ext/css/docs.css";
      lnk ~rel:"stylesheet" "ext/js/google-code-prettify/prettify.css";
      lnk ~rel:"stylesheet" "ext/css/site.css";
      lnk ~rel:"stylesheet" "ext/css/bootstrap-responsive.css";
      lnk ~rel:"stylesheet" "ext/css/opam2web.css";
      Html.meta ~name:"generator" ~content:("opam2web " ^ Version.string) []
    ]
  in
  let js_html =
    List.flatten
      (List.rev_map (fun f ->
           Html.script
             ~src:(prepend_root (Uri.make ~path:f ()))
             Html.empty)
          js_files)
  in
  let img_github =
    Html.img (prepend_root (Uri.make ~path:"ext/img/GitHub-Mark-32px.png" ()))
      ~alt:"opam on Github"
  in
  let js_html = List.rev js_html in [
      "title", Template.serialize title;
      "head",  Template.serialize head_html;
      "header",header;
      "githubimg", Template.serialize img_github;
      "body",  body;
      "footer",footer;
      "js",    Template.serialize js_html;
    ]

let make_nav (active, depth) pages =
  let category_of_href href =
    let path = Uri.path href in
    try Some (String.sub path 0 (String.index path '/'))
    with Not_found -> None in
  let active_category = category_of_href active in
  let rec make_item ?(subnav=false) m =
    let item_category = category_of_href m.menu_link in
    let class_attr =
      if subnav && active = m.menu_link then "active"
      else if (not subnav)
              && (active = m.menu_link
                 || active_category <> None
                   && active_category = item_category) then
        "active"
      else if (not subnav)
           && item_category = None
           && active = m.menu_link then
        "active"
      else
        ""
    in
    match m.menu_item with
    | External   ->
      let lnk =
        if Uri.host m.menu_link = None then
          prepend_root depth m.menu_link
        else m.menu_link
      in
      Html.li ~cls:class_attr
        (Html.a ~href:lnk m.menu_link_html)
    | Internal _ ->
      let lnk = prepend_root depth m.menu_link in
      Html.li ~cls:class_attr
        (Html.a ~href:lnk m.menu_link_html)
    | No_menu _ -> Html.empty
    | Nav_header ->
       Html.li ~cls:"nav-header" m.menu_link_html
    | Divider ->
       Html.li ~cls:"divider" Html.empty
    | Submenu sub_items ->
      let class_attr = match class_attr with
        | "" -> "dropdown"
        | _  -> sprintf "dropdown active" in
      Html.li ~cls:class_attr
        (Html.tag "a"
           ~cls:"dropdown-toggle"
           ~attrs:["href", "#";
                   "data-toggle", "dropdown"]
           (m.menu_link_html
            @ Html.b ~cls:"caret" Html.empty)
         @ Html.ul ~add_li:false ~cls:"dropdown-menu"
             (List.map (make_item ~subnav:true) sub_items)
        ) in
  Template.serialize
    (Html.ul ~add_li:false ~cls:"nav" (List.map make_item pages))

let make_footer srcurl depth =
  let icon file = prepend_root depth (Uri.make ~path:("ext/img/" ^ file) ()) in
  let srcurl = match srcurl with
    | None -> Html.empty
    | Some u ->
       Html.string "from " @
       (Html.a ~href:(Uri.of_string u) (* FIXME: u: uri? *)
          (Html.string (Filename.basename u)))
  in
  Template.serialize
    (Html.div ~cls:"icons"
       (Html.div ~cls:"icon"
          (Html.a ~href:(Uri.of_string "https://github.com/ocaml/opam2web")
             (Html.img (icon "github.png") ~alt:"Find us on GitHub")) (* FIXME: icon : Uri.t *)
        @ Html.div ~cls:"icon"
            (Html.a ~href:(Uri.of_string "http://www.ocamlpro.com/")
               (Html.img (icon "ocamlpro.png") ~alt:"Go to OCamlPro.com"))
        @ Html.div ~cls:"icon"
            (Html.a ~href:(Uri.of_string "http://www.ocaml.org/")
               (Html.img (icon "ocaml.png") ~alt:"Go to OCaml.org")))
     @ Html.div ~cls:"copyright"
         (Html.small
            (Html.string "Generated " @ srcurl
             @ Html.string "using "
             @ Html.a ~href:(Uri.of_string "http://github.com/ocaml/opam2web")
                 (Html.string "opam2web")
             @ Html.string ", courtesy of "
             @ Html.a ~href:(Uri.of_string "http://ocamlpro.com")
                 (Html.string "OCamlPro")
             @ Html.string ". "
             @ Html.a ~href:(Uri.of_string "http://opam.ocamlpro.com")
                 (Html.string "Commercial support")
             @ Html.string ".")))

let extract_links ~out_dir page =
  let pagedir =
    try Filename.chop_extension page.page_source
    with Invalid_argument _ -> page.page_source in
  let srcdir = Filename.dirname page.page_source in
  let dstdir = Filename.concat out_dir (Uri.path page.page_link) in
  let dstdir =
    if Sys.is_directory dstdir then dstdir else Filename.dirname dstdir
  in
  let file = Uri.to_string page.page_link in

  let rec get_links acc = function
    | [] -> acc
    | `El_start (_,attrs)::rest ->
      let acc =
        List.filter_map (function (("","href")|("","src")),url -> Some url | _ -> None)
          attrs
        @ acc
      in
      get_links acc rest
    | _::rest -> get_links acc rest
  in
  let rec find_id id = function
    | [] -> false
    | `El_start (_,attrs)::rest -> List.mem (("","id"),id) attrs || find_id id rest
    | _::rest -> find_id id rest
  in
  let links = get_links [] page.page_contents in
  List.iter (fun link ->
      let (/) = Filename.concat in
      let urlsep = Re.(compile @@ seq [rep any; str "://"]) in
      if Re.execp urlsep link then ()
      else
        let addr,id =
          OpamStd.Option.default (link,"") (OpamStd.String.cut_at link '#')
        in
        if addr = "" then
          if id = "" || find_id id page.page_contents then ()
          else
            OpamConsole.error "In %s: Broken self-reference to %s" file link
        else
          let target =
            if Filename.is_relative addr then dstdir / addr
            else out_dir / addr
          in
          if Sys.file_exists target then ()
          else
            let find f = if Sys.file_exists f then Some f else None in
            let src =
              match find (pagedir/addr) with
              | None -> find (srcdir/addr)
              | some -> some in
            match src with
            | Some src ->
               OpamConsole.msg "%s: Add resource %s to %s\n" file
                 src target;
               OpamSystem.copy_file src target;
            | None ->
               OpamConsole.error "In %s: Reference to resource %s not found"
                 file link
    )
    links

let generate ~content_dir ~out_dir menu pages =
  printf "++ Generating html files:\n%!";
  (* Filter out external links from the menu pages to generate *)
  let menu_pages =
    let rec aux acc pages =
      match pages with
      | [] -> acc
      | m :: t ->
        let mk page_depth page_contents =
          { page_source = m.menu_source;
            page_link = m.menu_link;
            page_link_text = m.menu_link_text;
            page_link_html = m.menu_link_html;
            page_depth;
            page_contents;
            page_srcurl = m.menu_srcurl; } in
        match m.menu_item with
        | External | Divider | Nav_header -> aux acc t
        | Submenu sub -> aux acc (sub @ t)
        | Internal (d, c) -> aux (mk d c :: acc) t
        | No_menu (d, c) -> aux (mk d c :: acc) t
    in aux [] menu
  in
  let c = ref 1 in
  let n = List.length pages + List.length menu_pages in
  let is_dir href =
    let len = String.length href in
    len = 0 || href.[len - 1] = '/'
  in
  let empty_uri = Uri.of_string "" in
  let aux page =
    printf "\r[%-5d/%d] %s %40s%!" !c n (Uri.to_string page.page_link) "";
    incr c;
    let header = make_nav (page.page_link, page.page_depth) menu in
    let footer = make_footer (page.page_srcurl) page.page_depth in
    let uri_dir = Uri.(
      resolve "http"
        (of_string out_dir) (with_path empty_uri (Uri.path page.page_link))
    ) in
    let dir = Uri.to_string uri_dir in
    let suffix =
      if is_dir dir
      then (OpamFilename.(mkdir (Dir.of_string dir));
            Uri.with_path empty_uri "index.html")
      else (OpamFilename.(mkdir (Dir.of_string (Filename.dirname dir)));
            empty_uri)
    in
    let path = Uri.(to_string (resolve "http" uri_dir suffix)) in
    let template = Template.({ path="template.xhtml"; fields=[
      "title", (default (Html.string "opam"), Optional);
      "head",  (default Html.empty,      Optional);
      "header",(default Html.empty,      Optional);
      "githubimg", (default Html.empty, Required);
      "body",  (mandatory (),            Required);
      "footer",(default Html.empty,      Optional);
      "js",    (default Html.empty,      Optional);
    ]}) in
    let chan = open_out path in
    let xml_out = Cow.Xml.make_output ~decl:false (`Channel chan) in
    Cow.Xml.output xml_out (`Dtd None);
    List.iter (Cow.Xml.output xml_out)
      (Template.generate content_dir template
         (create
            ~header ~footer
            ~title:page.page_link_text
            ~body:page.page_contents
            ~depth:page.page_depth));
    close_out chan
  in
  List.iter aux menu_pages;
  List.iter aux pages;
  printf "\r[%-5d/%d]\n%!" n n;
  List.iter (extract_links ~out_dir) menu_pages
