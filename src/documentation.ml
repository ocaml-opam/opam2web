open Cow.Html
open O2w_common

type doc_kind =
  | Html
  | Markdown

(* Page list *)
let pages = [
  "Tutorial", "A user tutorial for first time users of OPAM", Markdown;
  "Packaging", "A tutorial introducing the creation of OPAM packages", Markdown;
]

let extension_of_kind: doc_kind -> string = function
  | Html -> "html"
  | Markdown -> "md"

(* Generate the index of the documentation *)
let index (src: string): Cow.Html.t =
  (* Returns a HTML link given a docpage *)
  let link_of_page (title, descr, _): Cow.Html.t =
    let href = title ^ ".html" in
    <:xml<
      <a href="$str: href$">$str: title$</a> –
      $str: descr$
    >>
  in

  let links = List.map (fun p -> <:xml< <li>$link_of_page p$</li> >>) pages in
  <:xml<
    <h2>Where to start?</h2>
    <ul class="unstyled">
      $list: links$
    </ul>
  >>

(* Generate pages links needed to generate the pages *)
let to_links (content_dir: string): (Cow.Html.link * int * Cow.Html.t) list =
  (* Convert a content page to html *)
  let of_kind kind filename: Cow.Html.t =
    let content = Types.Raw.to_string (Types.Filename.read filename) in
    match kind with
    | Html -> Cow.Html.of_string content
    | Markdown -> Markdown_github.to_html (Markdown_github.of_string content)
  in

  (* Single link creation *)
  let aux (title, descr, kind) =
    let extension = extension_of_kind kind in
    let source_file = Printf.sprintf "%s/doc/%s.%s"
        content_dir title extension in
    let source_filename = Types.Filename.of_string source_file in
    let page = of_kind kind source_filename in

    let dest_file = Printf.sprintf "doc/%s.html" title in

    ({ text=title; href=dest_file }, 1, page)
  in

  List.map aux pages

