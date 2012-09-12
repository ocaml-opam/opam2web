open Cow.Html

(* Returns a HTML description of the given package *)
let to_html (repository: Path.R.t) (pkg: Types.NV.t): Cow.Html.t =
  let pkg_name = Types.N.to_string (Types.NV.name pkg) in
  let pkg_version = Types.V.to_string (Types.NV.version pkg) in
  let pkg_descr =
    File.Descr.full (File.Descr.read (Path.R.descr repository pkg))
  in
  <:xml<
    <h2>$str: pkg_name$ $str: pkg_version$</h2>
    <p>$str: pkg_descr$</p>
  >>
