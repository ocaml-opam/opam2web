(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012-2013 OCamlPro                                     *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open Cow
open Cow.Html
open O2wTypes

let string_of_time () =
  let t = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%d/%d/%d" t.Unix.tm_mday (t.Unix.tm_mon + 1)
      (t.Unix.tm_year + 1900)

let prepend_root (depth: int) (src: string): string =
  let path_of_depth =
    let rec aux acc = function
      | 0 -> acc
      | n -> aux ("../" ^ acc) (n-1)
    in
    aux "" depth
  in
  path_of_depth ^ src

let create ~title ~header ~body ~footer ~depth =
  let css_files = [
    "ext/css/bootstrap.css";
    "ext/css/bootstrap-responsive.css";
    "ext/css/docs.css";
    "ext/js/google-code-prettify/prettify.css";
    "ext/css/site.css";
  ] in
  let js_files = [
    "ext/js/jquery.js";
    "ext/js/google-code-prettify/prettify.js";
    "ext/js/bootstrap.min.js";
    "ext/js/site.js";
    "ext/js/search.js";
  ] in
  let prepend_root = prepend_root depth in
  let css_html = List.map (fun f ->
      <:html< <link href="$str: prepend_root f$" rel="stylesheet" /> >>) css_files
  in
  let js_html = List.map (fun f ->
      <:html< <script src="$str: prepend_root f$"> </script> >>) js_files
  in
  let home_href = prepend_root "index.html" in
  <:html<
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <title>OPAM - $str: title$</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <meta name="description"
        content="The homepage of OPAM, a package manager for OCaml" />
    <meta name="author" content="OCamlPro" />

    <!-- Le styles -->
    $list: css_html$

    <!-- Le HTML5 shim, for IE6-8 support of HTML5 elements -->
    <!--[if lt IE 9]>
      <script src="http://html5shim.googlecode.com/svn/trunk/html5.js"></script>
    <![endif]-->

  </head>

  <body>

    <div class="navbar navbar-fixed-top">
      <div class="navbar-inner">
        <div class="container">
          <a class="btn btn-navbar" data-toggle="collapse"
              data-target=".nav-collapse">
            <span class="icon-bar"> </span>
            <span class="icon-bar"> </span>
            <span class="icon-bar"> </span>
          </a>
          <a class="brand" href="$str: home_href$">OPAM</a>
          <div class="nav-collapse collapse">
            $header$
<!--
            <form class="navbar-form pull-right">
              <input id="search" class="span2" type="text" placeholder="Search packages" />
            </form>
-->
          </div><!--/.nav-collapse -->
        </div>
      </div>
    </div>

    <div class="container">

      $body$

      <hr />

      <footer>
        $footer$
      </footer>

    </div> <!-- /container -->

    <!-- Le javascript
    ================================================== -->
    <!-- Placed at the end of the document so the pages load faster -->
    $list: js_html$
    <script type="text/javascript">
      var _gaq = _gaq || []; _gaq.push(['_setAccount', 'UA-22552764-5']); _gaq.push(['_trackPageview']);
    (function() {
      var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
      ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
      var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
    })();
    </script>
  </body>
</html>
  >>

let make_nav (active, depth) pages: Cow.Html.t =
  let category_of_href href =
    try Some (String.sub href 0 (String.index href '/'))
    with Not_found -> None in
  let active_category = category_of_href active.href in
  let rec make_item ?(subnav=false) m =
    let item_category = category_of_href m.menu_link.href in
    let class_attr =
      if subnav && active.href = m.menu_link.href then "active"
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
      <:html< <li class="$str: class_attr$">$html_of_link m.menu_link$</li> >>
    | Internal _ ->
      let lnk = { m.menu_link with href = prepend_root depth m.menu_link.href } in
      <:html< <li class="$str: class_attr$">$html_of_link lnk$</li> >>
    | No_menu _ -> <:html< >>
    | Nav_header ->
      <:html< <li class="nav-header">$str:m.menu_link.text$</li> >>
    | Divider ->
      <:html< <li class="divider"></li> >>
    | Submenu sub_items ->
      let class_attr = match class_attr with
        | "" -> "dropdown"
        | _  -> Printf.sprintf "dropdown active" in
      <:html<
        <li class="$str:class_attr$">
          <a href="#" class="dropdown-toggle" data-toggle="dropdown">
            $str:m.menu_link.text$ <b class="caret"> </b>
          </a>
          <ul class="dropdown-menu">
            $list: List.map (make_item ~subnav:true) sub_items$
          </ul>
        </li>
      >>
  in
  <:html<
    <ul class="nav">
      $list: List.map make_item pages$
    </ul>
  >>

let generate ~out_dir menu pages =
  Printf.printf "Generating html files:\n%!";
  let footer = <:html<
      <p>
        Last modified: $str:string_of_time ()$ –
        Generated by <a href="https://github.com/OCamlPro/opam2web">
        opam2web</a>, using <a href="https://github.com/mirage/ocaml-cow"
        title="OCaml on the Web">COW</a>.<br />
        © <a href="http://www.ocamlpro.com/">OCamlPro</a> 2012-2013
      </p>
    >>
  in
  let aux page =
    Printf.printf "> %s... %!" page.page_link.href;
    let header = make_nav (page.page_link, page.page_depth) menu in
    let path =
      if String.length out_dir > 0
      then Printf.sprintf "%s/%s" out_dir page.page_link.href
      else page.page_link.href in
    let chan = open_out path in
    let page =
      create
        ~header ~footer
        ~title:page.page_link.text
        ~body:page.page_contents
        ~depth:page.page_depth in
    output_string chan (Html.to_string page);
    close_out chan;
    Printf.printf "[Done]\n"
  in
  (* Filter out external links from the menu pages to generate *)
  let menu_pages =
    let rec aux acc pages =
      match pages with
      | [] -> acc
      | m :: t ->
        let mk page_depth page_contents =
          { page_link = m.menu_link;
            page_depth;
            page_contents } in
        match m.menu_item with
        | External | Divider | Nav_header -> aux acc t
        | Submenu sub -> aux acc (sub @ t)
        | Internal (d, c) -> aux (mk d c :: acc) t
        | No_menu (d, c) -> aux (mk d c :: acc) t
    in aux [] menu
  in
  List.iter aux menu_pages;
  List.iter aux pages

