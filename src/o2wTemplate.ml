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

open Printf
open Cow
open O2wTypes

let string_of_time () =
  let t = Unix.localtime (Unix.time ()) in
  sprintf "%d/%d/%d" t.Unix.tm_mday (t.Unix.tm_mon + 1)
    (t.Unix.tm_year + 1900)

let prepend_root (depth: int) (src: string): string =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux ("../" ^ acc) (n-1)
  in
  if src <> "" && src.[0] = '/' then src else aux src depth

let create ~title ~header ~body ~footer ~depth =
  let title = Html.string title in
  let js_files = [
      "ext/js/jquery.js";
      "ext/js/site.js";
      "ext/js/search.js";
    ] in
  let prepend_root = prepend_root depth in
  let href_css = Uri.of_string(prepend_root "ext/css/opam2web.css") in
  let head_html =
    Html.link ~href:href_css ~attrs:["type", "text/css";
                                     "rel", "stylesheet"]
      Html.empty
    @ Html.meta ~name:"generator" ~content:("opam2web " ^ Version.string)
        Html.empty
  in
  let js_html =
    Html.script ~typ:"text/javascript"
      (Html.string
         "var _gaq = _gaq || []; \
          _gaq.push(['_setAccount', 'UA-22552764-5']); \
          _gaq.push(['_trackPageview']); \
          (function() { \
          var ga = document.createElement('script'); \
          ga.type = 'text/javascript'; ga.async = true; \
          ga.src = ('https:' == document.location.protocol ? 'https://ssl' : \
          'http://www') + '.google-analytics.com/ga.js'; \
          var s = document.getElementsByTagName('script')[0]; \
          s.parentNode.insertBefore(ga, s);
          })();")
    @ List.flatten (List.rev_map (fun f ->
                        Html.script ~src:(prepend_root f)
                          Html.empty)
                      js_files)
  in
  let js_html = List.rev js_html in [
      "title", Template.serialize title;
      "head",  Template.serialize head_html;
      "header",header;
      "body",  body;
      "footer",footer;
      "js",    Template.serialize js_html;
    ]

let make_nav (active, depth) pages =
  let category_of_href href =
    try Some (String.sub href 0 (String.index href '/'))
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
       Html.li ~cls:class_attr
         (Html.a ~href:(Uri.of_string m.menu_link)
            (Html.string m.menu_link_text))
    | Internal _ ->
      let lnk = Uri.of_string(prepend_root depth m.menu_link) in
      Html.li ~cls:class_attr
        (Html.a ~href:lnk (Html.string m.menu_link_text))
    | No_menu _ -> Html.empty
    | Nav_header ->
       Html.li ~cls:"nav-header" (Html.string m.menu_link_text)
    | Divider ->
       Html.li ~cls:"divider" Html.empty
    | Submenu sub_items ->
      let class_attr = match class_attr with
        | "" -> "dropdown"
        | _  -> sprintf "dropdown active" in
      Html.li ~cls:class_attr
        (Html.a ~href:(Uri.of_string "#") ~cls:"dropdown-toggle"
           (* ~attrs:["data-toggle", "dropdown"] FIXME *)
           (Html.string m.menu_link_text
            @ Html.b ~cls:"caret" Html.empty)
         @ Html.ul ~cls:"dropdown-menu"
             (List.map (make_item ~subnav:true) sub_items)
        ) in
  Template.serialize (Html.ul ~cls:"nav" (List.map make_item pages))

let make_footer srcurl depth =
  let icon file = Uri.of_string(prepend_root depth ("ext/img/" ^ file)) in
  let srcurl = match srcurl with
    | None -> Html.empty
    | Some u ->
       Html.tag "from" (Html.a ~href:(Uri.of_string u) (* FIXME: u: uri? *)
                          (Html.string (Filename.basename u)))
  in
  Template.serialize
    (Html.div ~cls:"icons"
       (Html.div ~cls:"icon"
          (Html.a ~href:(Uri.of_string "https://github.com/ocaml/opam2web")
             (Html.img (icon "github.png"))) (* FIXME: icon : Uri.t *)
        @ Html.div ~cls:"icon"
            (Html.a ~href:(Uri.of_string "http://www.ocamlpro.com/")
               (Html.img (icon "ocamlpro.png")))
        @ Html.div ~cls:"icon"
            (Html.a ~href:(Uri.of_string "http://www.ocaml.org/")
               (Html.img (icon "ocaml.png"))))
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

let rec extract_links ~content_dir ~out_dir page =
  let pagedir =
    try Filename.chop_extension page.page_source
    with Invalid_argument _ -> page.page_source in
  let srcdir = Filename.dirname page.page_source in
  let dstdir = Filename.concat out_dir page.page_link in
  let dstdir =
    if Sys.is_directory dstdir then dstdir else Filename.dirname dstdir
  in
  let file = page.page_link in

  let rec get_links acc = function
    | [] -> acc
    | `El_start (_,attrs)::rest ->
      let acc =
        OpamStd.List.filter_map (function (("","href")|("","src")),url -> Some url | _ -> None)
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
      if Re_str.string_match (Re_str.regexp ".*://") link 0 then ()
      else
        let addr,id =
          OpamStd.Option.default (link,"") (OpamStd.String.cut_at link '#')
        in
        if addr = "" then
          if id = "" || find_id id page.page_contents then ()
          else OpamConsole.error "In %s: Broken self-reference to %s" file link
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
               OpamConsole.msg "%s: Add resource %s to %s\n" file src target;
               OpamSystem.copy src target;
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
    printf "\r[%-5d/%d] %s %40s%!" !c n page.page_link "";
    incr c;
    let header = make_nav (page.page_link, page.page_depth) menu in
    let footer = make_footer (page.page_srcurl) page.page_depth in
    let uri_dir = Uri.(
      resolve "http"
        (of_string out_dir) (with_path empty_uri page.page_link)
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
      "title", (default (Html.string "OPAM"), Optional);
      "head",  (default Html.empty,      Optional);
      "header",(default Html.empty,      Optional);
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
  List.iter (extract_links ~content_dir ~out_dir) menu_pages
