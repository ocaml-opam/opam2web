open Cow.Html

(* Load a repository from the local OPAM installation *)
let of_opam repo_name: Path.R.t =
  let global = Path.G.create () in
  let config = File.Config.read (Path.G.config global) in
  let all_repositories = File.Config.repositories config in
  let repo = List.find
    (fun r -> (Types.Repository.name r) = repo_name) all_repositories
  in
  Path.R.create repo

(* Load a repository from a directory *)
let of_path (name: string): Path.R.t =
  Path.R.of_dirname (Types.Dirname.of_string name)

(* Create a list of package pages to generate for a repository *)
let to_links (repository: Path.R.t): (Cow.Html.link * Cow.Html.t) list =
  let package_set = Path.R.available_packages repository in
  let packages = Types.NV.Set.elements package_set in
  List.map (fun pkg ->
      let pkg_name = Types.N.to_string (Types.NV.name pkg) in
      let pkg_version = Types.V.to_string (Types.NV.version pkg) in
      let pkg_href = Printf.sprintf "%s.%s.html" pkg_name pkg_version in
      let pkg_title = Printf.sprintf "%s %s" pkg_name pkg_version in
      { text=pkg_title; href=pkg_href }, (Package.to_html repository pkg))
    packages

(* Returns a HTML list of the packages in the given repository *)
let to_html (repository: Path.R.t): Cow.Html.t =
  let package_set = Path.R.available_packages repository in
  let packages = Types.NV.Set.elements package_set in
  let packages_html = List.map
    (fun (pkg: Types.NV.t) ->
      (* TODO: factorize the following operations with other functions *)
      let pkg_name = Types.N.to_string (Types.NV.name pkg) in
      let pkg_version = Types.V.to_string (Types.NV.version pkg) in
      let pkg_href = Printf.sprintf "%s.%s.html" pkg_name pkg_version in
      let pkg_synopsis =
        File.Descr.synopsis (File.Descr.read (Path.R.descr repository pkg))
      in
      <:xml<
        <tr>
          <td><a href="$str: pkg_href$">$str: pkg_name$</a></td>
          <td>$str: pkg_version$</td>
          <td>$str: pkg_synopsis$</td>
        </tr>
      >>)
    packages
  in
  <:xml<
    <table class="table">
      <thead>
        <tr>
          <th>Name</th>
          <th>Version</th>
          <th>Description</th>
        </tr>
      </thead>
      <tbody>
        $list: packages_html$
      </tbody>
    </table>
  >>
