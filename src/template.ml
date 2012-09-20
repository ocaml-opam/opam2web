open Cow
open Cow.Html

open O2w_common

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
      <:xml< <link href="$str: prepend_root f$" rel="stylesheet" /> >>) css_files
  in
  let js_html = List.map (fun f ->
      <:xml< <script src="$str: prepend_root f$"> </script> >>) js_files
  in
  let home_href = prepend_root "index.html" in
  <:xml<
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

  </body>
</html>
  >>

let make_nav (active, depth) pages: Cow.Html.t =
  let rec make_item (lnk, c) =
    let class_attr = if lnk.href = active.href then "active" else "" in
    match c with
    | External _ ->
      <:xml< <li class="$str: class_attr$">$html_of_link lnk$</li> >>
    | Internal _ ->
      let lnk = { lnk with href = prepend_root depth lnk.href } in
      <:xml< <li class="$str: class_attr$">$html_of_link lnk$</li> >>
    | Nav_header ->
      <:xml< <li class="nav-header">lnk.text</li> >>
    | Divider ->
      <:xml< <li class="divider"></li> >>
    | Submenu sub_items ->
      <:xml<
        <li class="dropdown">
          <a href="#" class="dropdown-toggle" data-toggle="dropdown">
            $str: lnk.text$ <b class="caret"> </b>
          </a>
          <ul class="dropdown-menu">
            $list: List.map make_item sub_items$
          </ul>
        </li>
      >>
  in
  <:xml<
    <ul class="nav">
      $list: List.map make_item pages$
    </ul>
  >>

let generate ~out_dir (menu_links, pages) =
  Printf.printf "Generating html files:\n%!";
  let footer = <:xml<
      <p>
        Last modified: $str:string_of_time ()$ –
        Generated by <a href="https://github.com/OCamlPro/opam2web">
        opam2web</a>, using <a href="https://github.com/mirage/ocaml-cow"
        title="OCaml on the Web">COW</a>.<br />
        © <a href="http://www.ocamlpro.com/">OCamlPro</a> 2012
      </p>
    >>
  in
  let aux (link, depth, contents) =
    Printf.printf "> %s... %!" link.href;
    let header = make_nav (link, depth) menu_links in
    let path = if String.length out_dir > 0 then
        Printf.sprintf "%s/%s" out_dir link.href
      else
        link.href
    in
    let chan = open_out path in
    let page = create ~title:link.text ~header ~body:contents ~footer ~depth in
    output_string chan (Html.to_string page);
    close_out chan;
    Printf.printf "[Done]\n"
  in
  (* Filter out external links from the menu pages to generate *)
  let menu_pages =
    let rec aux acc pages =
      match pages with
      | [] -> acc
      | (l, m) :: t -> match m with
        | External | Divider | Nav_header -> aux acc t
        | Submenu sub -> aux acc (sub @ t)
        | Internal (d, c) -> aux ((l, d, c) :: acc) t
    in aux [] menu_links
  in
  List.iter aux menu_pages;
  List.iter aux pages

