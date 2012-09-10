open Cow.Html

(* Load the repository corresponding to the given name *)
let load_repository repo_name: Path.R.t =
  let global = Path.G.create () in
  let config = File.Config.read (Path.G.config global) in
  let all_repositories = File.Config.repositories config in
  let repo = List.find
    (fun r -> (Types.Repository.name r) = repo_name) all_repositories
  in
  Path.R.create repo

(* Test function that displays info of the packages in a repository.
   TODO: remove; probably not needed other than for debugging. *)
let print_packages repository =
  let packages = Path.R.available_packages repository in
  Types.NV.Set.iter
    (fun p -> Printf.printf "%s\n" (Types.N.to_string (Types.NV.name p)))
    packages

(* Returns a HTML list of the packages in the given repository *)
let html_of_repository (repository: Path.R.t): Cow.Html.t =
  let package_set = Path.R.available_packages repository in
  let packages = Types.NV.Set.elements package_set in
  let packages_html = List.map
    (fun pkg ->
      (* TODO: factorize the following operations with other functions *)
      let pkg_name = Types.N.to_string (Types.NV.name pkg) in
      let pkg_version = Types.V.to_string (Types.NV.version pkg) in
      let pkg_href = Printf.sprintf "%s.%s.html" pkg_name pkg_version in
      <:xml<
        <tr>
          <td><a href="$str: pkg_href$">$str: pkg_name$</a></td>
          <td>$str: pkg_version$</td>
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
        </tr>
      </thead>
      <tbody>
        $list: packages_html$
      </tbody>
    </table>
  >>

(* Returns a HTML description of the given package *)
let to_html (pkg: Types.NV.t): Cow.Html.t =
  let pkg_name = Types.N.to_string (Types.NV.name pkg) in
  let pkg_version = Types.V.to_string (Types.NV.version pkg) in
  <:xml<
    <h2>$str: pkg_name$ $str: pkg_version$</h2>
    <p></p>
  >>

(* Create a list of package pages to generate for a repository *)
let to_list (repository: Path.R.t): (Cow.Html.link * Cow.Html.t) list =
  let package_set = Path.R.available_packages repository in
  let packages = Types.NV.Set.elements package_set in
  List.map (fun pkg ->
      let pkg_name = Types.N.to_string (Types.NV.name pkg) in
      let pkg_version = Types.V.to_string (Types.NV.version pkg) in
      let pkg_href = Printf.sprintf "%s.%s.html" pkg_name pkg_version in
      let pkg_title = Printf.sprintf "%s %s" pkg_name pkg_version in
      { text=pkg_title; href=pkg_href }, (to_html pkg))
    packages
