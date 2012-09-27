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

