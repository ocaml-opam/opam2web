open Cow.Html

(* Returns a HTML description of the given package *)
let to_html (pkg: Types.NV.t): Cow.Html.t =
  let pkg_name = Types.N.to_string (Types.NV.name pkg) in
  let pkg_version = Types.V.to_string (Types.NV.version pkg) in
  <:xml<
    <h2>$str: pkg_name$ $str: pkg_version$</h2>
    <p></p>
  >>
