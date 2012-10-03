type menu_item =
  | Internal of int * Cow.Html.t
  | Submenu of ((Cow.Html.link * menu_item) list)
  | Nav_header
  | Divider
  | External

type package_info = {
  pkg_name     : string;
  pkg_version  : string;
  pkg_descr    : Cow.Html.t;
  pkg_synopsis : string;
  pkg_href     : string;
  pkg_title    : string;
}

(* Global values *)

(* The list contains elements with this syntax :
   <string> without extension -> A menu title
   <string> with 'md' extension -> A markdown page in content/doc
   empty <string> -> A menu divider
 *)
let documentation_pages = [
  "Primer";
  "About.md";
  "Quick_Install.md";
  "Basic_Usage.md";
  "";
  "Go Further";
  "Advanced_Install.md";
  "Advanced_Usage.md";
  "Developing.md";
  "";
  "For Packagers";
  "Packaging.md"
]
