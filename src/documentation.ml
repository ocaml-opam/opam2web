open Cow.Html
open O2w_common

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

(* Generate the HTML corresponding to a documentation page in the <content>/doc 
   directory *)
let to_links (content_dir: string) (pages: string list)
    : (Cow.Html.link * menu_item) list =

  let wrap_li ~depth c = <:xml<<li>$c$</li>&>> in

  let wrap_a ~depth ~heading c =
    let href = "#" ^ Markdown_github.id_of_heading heading in
    let html_a =
      if depth > 1 then <:xml<<a href="$str: href$"><small>$c$</small></a>&>>
      else <:xml<<a href="$str: href$"><strong>$c$</strong></a>&>>
    in wrap_li ~depth html_a
  in

  let wrap_ul ~depth l =
    if depth = 0 then
      <:xml<<ul class="nav nav-list bs-docs-sidenav affix">$l$</ul>&>>
    else
      <:xml<$l$&>>
  in

  (* Convert a content page to html *)
  let of_kind kind filename: Cow.Html.t =
    let content = Types.Raw.to_string (Types.Filename.read filename) in
    match kind with
    | "html" -> Cow.Html.of_string content
    | "md" ->
        let md_content = Markdown_github.of_string content in
        let html_toc =
          Markdown_github.to_html_toc
              ~wrap_list:wrap_ul ~wrap_item:wrap_a md_content
        in
        <:xml<
          <div class="row">
            <div class="span3 bs-docs-sidebar">
            <span> </span>
            $html_toc$
            </div>
            <div class="span9">
            $Markdown_github.to_html md_content$
            </div>
          </div>
        >>
    | _ -> <:xml< >>
  in

  (* Link creation *)
  let aux page =
    let title, extension = split_filename page in
    if String.length extension = 0 then
      if  String.length title > 0 then
        ({ text=title; href="" }, Nav_header)
      else
        ({ text=""; href="" }, Divider)
    else
      let source_file = Printf.sprintf "%s/doc/%s.%s" content_dir title extension in
      let source_filename = Types.Filename.of_string source_file in
      let page = of_kind extension source_filename in
      let dest_file = Printf.sprintf "doc/%s.html" title in

      ({ text=title; href=dest_file }, Internal (1, page))
  in

  List.map aux pages

