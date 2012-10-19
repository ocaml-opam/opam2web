open OpamTypes
open Cow.Html

open O2w_common

(* Load a repository from the local OPAM installation *)
let of_opam (repo_name: string): OpamPath.Repository.r =
  let default_path = OpamPath.default () in
  let config = OpamFile.Config.read (OpamPath.config default_path) in
  let all_repositories = OpamFile.Config.repositories config in
  let repo = List.find
    (fun r -> (OpamRepositoryName.to_string r) = repo_name) all_repositories
  in
  OpamPath.Repository.create default_path repo

(* Load a repository from a directory *)
let of_path (name: string): OpamPath.Repository.r =
  OpamPath.Repository.raw (OpamFilename.Dir.of_string name)

(* Unify multiple versions of the same packages in a list of lists *)
let unify_versions (packages: OpamPackage.t list): OpamPackage.t list list =
  let (unique_packages, rest) = List.fold_left (fun (acc, pkgs) pkg ->
      match pkgs with
      | [] -> (acc, [pkg])
      | h :: r when (OpamPackage.name h = OpamPackage.name pkg) -> (acc, pkg :: pkgs)
      | l -> (l :: acc, [pkg]))
    ([], []) packages
  in
  List.rev (rest :: unique_packages)

(* Retrieve packages of a repository *)
let get_packages repository =
  let package_set = OpamRepository.packages repository in
  OpamPackage.Set.elements package_set

(* Create an association list (package_name -> reverse_dependencies) *)
let reverse_dependencies (repository: OpamPath.Repository.r)
    (packages: OpamPackage.t list) : (OpamPackage.Name.t * OpamPackage.t list list) list =
  let packages = get_packages repository in
  let revdeps_tbl: (OpamPackage.Name.t, OpamPackage.t) Hashtbl.t =
    Hashtbl.create 300
  in
  (* Fill a hash table with reverse dependecies (required by...) *)
  List.iter (fun pkg ->
      let opam_file = OpamFile.OPAM.read (OpamPath.Repository.opam repository pkg) in
      let dependencies = OpamFormula.atoms (OpamFile.OPAM.depends opam_file) in
      let deps =
        List.map (fun (name, _) -> name) dependencies
      in
      List.iter (fun dep -> Hashtbl.add revdeps_tbl dep pkg) deps)
    packages;
  (* Build the association list *)
  let acc, pkg_acc, prev_pkg =
    Hashtbl.fold (fun pkg reqby (acc, pkg_acc, prev_pkg) ->
        match prev_pkg with
        | Some p when p = pkg -> acc, reqby :: pkg_acc, Some pkg
        | Some p -> (p, pkg_acc) :: acc, [reqby], Some pkg
        | None -> acc, [reqby], Some pkg)
      revdeps_tbl ([], [], None)
  in
  (* FIXME: List.rev where necessary *)
  let revdeps = match prev_pkg with
    | Some p -> (p, pkg_acc) :: acc
    | None -> acc
  in
  List.map (fun (p, reqby) -> (p, unify_versions reqby)) revdeps


(* Create a list of package pages to generate for a repository *)
let to_links (repository: OpamPath.Repository.r) (statistics: statistics option)
    : (Cow.Html.link * int * Cow.Html.t) list =
  let packages = get_packages repository in
  let unique_packages = unify_versions packages in
  let reverse_dependencies = reverse_dependencies repository packages in
  let aux package_versions = List.map (fun (pkg: OpamPackage.t) ->
      let pkg_info = Package.get_info ~href_prefix:"pkg/" repository pkg in
      { text=pkg_info.pkg_title; href=pkg_info.pkg_href }, 1,
        (Package.to_html repository unique_packages
            reverse_dependencies package_versions statistics pkg))
    package_versions
  in
  List.flatten (List.map aux unique_packages)

(* Returns a HTML list of the packages in the given repository *)
let to_html (repository: OpamPath.Repository.r) (statistics: statistics option)
    : Cow.Html.t =
  let packages = get_packages repository in
  let unique_packages = unify_versions packages in
  let packages_html =
    List.map (fun (pkg_vers: OpamPackage.t list) ->
        let latest_pkg = Package.latest pkg_vers in
        let pkg_info = Package.get_info repository latest_pkg in
        <:xml<
          <tr>
            <td>
              <a href="$str: pkg_info.pkg_href$">
                $str: pkg_info.pkg_name$
              </a>
            </td>
            <td>$str: pkg_info.pkg_version$</td>
            <td>$str: pkg_info.pkg_synopsis$</td>
          </tr>
        >>)
      unique_packages
  in
  let stats_html = match statistics with
    | None -> <:xml< >>
    | Some s -> <:xml<
        <p class="span3">
          <em>Repository usage statistics:</em><br />
          <i class="icon-th-large"> </i> <strong>$str: Int64.to_string s.global_stats$</strong> package downloads<br />
          <i class="icon-refresh"> </i> <strong>$str: Int64.to_string s.update_stats$</strong> repository updates
        </p>
      >>
  in
  <:xml<
    <div class="row">
      <form class="span9 form-search">
        <div class="input-append">
          <input id="search" class="search-query" type="text" placeholder="Search packages" />
          <button id="search-button" class="btn add-on">
            <i class="icon-search"> </i>
          </button>
        </div>
      </form>
      $stats_html$
    </div>
    <table class="table"  id="packages">
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
