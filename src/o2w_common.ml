type menu_item =
  | Internal of int * Cow.Html.t
  | Submenu of ((Cow.Html.link * menu_item) list)
  | Nav_header
  | Divider
  | External

