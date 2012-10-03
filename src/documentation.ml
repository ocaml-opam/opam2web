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
      <:xml<<ul class="nav nav-list bs-docs-sidenav">$l$</ul>&>>
    else
      <:xml<$l$&>>
  in

  (* Convert a content page to html *)
  let of_kind doc_menu kind filename: Cow.Html.t =
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
            <div class="span3">
            <span> </span>
            <div class="bs-docs-menu"
                data-spy="affix"
                data-offset-top="0" data-offset-bottom="140">
              $doc_menu$
              $html_toc$
            </div>
            </div>
            <div class="span9">
            $Markdown_github.to_html md_content$
            </div>
          </div>
        >>
    | _ -> <:xml< >>
  in

  (* Documentation menu and links creation *)
  let aux_menu page =
    let title, extension = split_filename page in
    let human_title = Str.global_replace (Str.regexp "_") " " title in
    if String.length extension = 0 then
      let empty_filename = Types.Filename.of_string "" in
      if  String.length title > 0 then
        (empty_filename, "", { text=human_title; href="" }, Nav_header)
      else
        (empty_filename, "", { text=""; href="" }, Divider)
    else
      let source_file =
        Printf.sprintf "%s/doc/%s.%s" content_dir title extension
      in
      let source_filename = Types.Filename.of_string source_file in
      let dest_file = Printf.sprintf "%s.html" title in
      (source_filename, extension,
          { text=human_title; href=dest_file }, Internal (1, Cow.Html.nil))
  in

  let menu_pages = List.map aux_menu pages in

  let documentation_menu active_src =
    let menu_items = List.map (fun (src, _, lnk, kind) -> match kind with
        | Submenu _ -> Cow.Html.nil
        | Nav_header ->
          <:xml<
            <li class="disabled">
              <a href="#"><strong>$str: lnk.text$</strong></a>
            </li>
          >>
        | Divider ->
          <:xml<<li class="disabled divider"><a href="#"> </a></li>&>>
        | External | Internal _ ->
          let classes = if active_src = src then "active" else "" in
          <:xml<<li class="$str: classes$">
            <a href="$str: lnk.href$"> $str: lnk.text$</a>
          </li>&>>
      ) menu_pages
    in
    <:xml<
      <ul class="nav nav-pills nav-stacked">
        $list: menu_items$
      </ul>
    >>
  in

  (* Pages creation *)
  let aux_page (source_filename, extension, lnk, page) =
    match page with
    | Submenu _ | Nav_header | Divider | External -> (lnk, page)
    | Internal (level, _) ->
        let doc_menu = documentation_menu source_filename in
        let html_page = of_kind doc_menu extension source_filename in
        ({ lnk with href = "doc/" ^ lnk.href }, Internal(level, html_page))
  in

  List.map aux_page menu_pages

