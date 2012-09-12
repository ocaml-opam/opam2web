open Cow.Html

(* Returns a HTML description of the given package *)
let to_html (repository: Path.R.t) (pkg: Types.NV.t): Cow.Html.t =
  let pkg_name = Types.N.to_string (Types.NV.name pkg) in
  let pkg_version = Types.V.to_string (Types.NV.version pkg) in
  let pkg_descr =
    File.Descr.full (File.Descr.read (Path.R.descr repository pkg))
  in
  let pkg_url =
    try
      let url_file = File.URL.read (Path.R.url repository pkg) in
      let kind = match File.URL.kind url_file with
        | Some k -> <:xml< [$str: k$] >>
        | None -> <:xml< >>
      in
      let checksum = match File.URL.checksum url_file with
        | Some c -> <:xml< <small>$str: c$</small> >>
        | None -> <:xml< >>
      in
      let url = File.URL.url url_file in
      <:xml<
        <tr>
          <th>Source $kind$</th>
          <td>
            <a href="$str: url$" title="Download source">$str: url$</a><br />
            $checksum$
          </td>
        </tr>
      >>
    with
      Globals.Exit 66 -> <:xml< >>
  in
  let opam_file = File.OPAM.read (Path.R.opam repository pkg) in
  let pkg_maintainer = File.OPAM.maintainer opam_file in
  <:xml<
    <h2>$str: pkg_name$ $str: pkg_version$</h2>

    <table class="table">
      <tbody>
        <tr>
          <th>Maintainer</th>
          <td>
            $str: pkg_maintainer$
          </td>
        </tr>
        $pkg_url$
        <tr>
          <th>Dependencies</th>
          <td></td>
        </tr>
        <tr>
          <th>Optional dependencies</th>
          <td></td>
        </tr>
      </tbody>
    </table>

    <p>$str: pkg_descr$</p>
  >>
