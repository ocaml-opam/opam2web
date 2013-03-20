(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open OpamTypes
open Cow.Html
open O2wTypes

(* Load a repository from the local OPAM installation *)
let of_opam repo_name =
  let default_path = OpamPath.default () in
  let config = OpamFile.Config.read (OpamPath.config default_path) in
  let all_repositories = OpamFile.Config.repositories config in
  let repo =
    List.find (fun r -> (OpamRepositoryName.to_string r) = repo_name) all_repositories
  in
  let root = OpamPath.Repository.create default_path repo in
  let prefix, packages = OpamRepository.packages root in
  { root; prefix; packages }

(* Load a repository from a directory *)
let of_path dirname =
  let root = OpamFilename.Dir.of_string dirname in
  let prefix, packages = OpamRepository.packages root in
  { root; prefix; packages }

(* Get the last update timestamp of a package in a given repository *)
let last_update repository package =
  let open Unix in
  let prefix = OpamRepository.find_prefix repository.prefix package in
  let opam_filename =
    OpamFilename.to_string (OpamPath.Repository.opam repository.root prefix package)
  in
  let opam_stat = Unix.stat opam_filename in
  opam_stat.st_mtime

let get_dated_packages repository =
  let packages = O2wPackage.remove_base_packages repository.packages in
  OpamPackage.Set.fold (fun pkg map ->
    let last_update = last_update repository pkg in
    OpamPackage.Map.add pkg last_update map
  ) packages OpamPackage.Map.empty

(* Create an association list (package_name -> reverse_dependencies) *)
let reverse_dependencies repository packages =
  let revdeps_tbl: (name, name) Hashtbl.t = Hashtbl.create 300 in
  (* Fill a hash table with reverse dependecies (required by...) *)
  OpamPackage.Set.iter (fun pkg ->
    let name = OpamPackage.name pkg in
    let prefix = OpamRepository.find_prefix repository.prefix pkg in
    let opam_file = OpamFile.OPAM.read (OpamPath.Repository.opam repository.root prefix pkg) in
    let dependencies = OpamFormula.atoms (OpamFile.OPAM.depends opam_file) in
    let deps = List.map (fun (name, _) -> name) dependencies in
    List.iter (fun dep -> Hashtbl.add revdeps_tbl dep name) deps)
    packages;
  let names =
    Hashtbl.fold (fun name _ acc -> name :: acc) revdeps_tbl [] in
  (* Build the association list *)
  List.fold_left (fun acc name ->
    let names = OpamPackage.Name.Set.of_list (Hashtbl.find_all revdeps_tbl name) in
    OpamPackage.Name.Map.add name names acc
  ) OpamPackage.Name.Map.empty names

(* Create a list of package pages to generate for a repository *)
let to_pages ~statistics ~dates repository =
  let packages = repository.packages in
  let unique_packages = O2wPackage.unify_versions packages in
  let reverse_dependencies = reverse_dependencies repository packages in
  let aux pkg acc =
    let name = OpamPackage.name pkg in
    let versions = OpamPackage.versions_of_name packages name in
    OpamPackage.Version.Set.fold (fun version acc ->
      let pkg = OpamPackage.create name version in
      match O2wPackage.get_info ~href_prefix:"pkg/" repository ~dates pkg with
      | None  ->
        Printf.printf "Skipping %s\n%!" (OpamPackage.to_string pkg);
        acc
      | Some pkg_info ->
        let page = {
          page_link     = { text=pkg_info.pkg_title; href=pkg_info.pkg_href };
          page_depth    = 1;
          page_contents =
            O2wPackage.to_html
              ~unique_packages ~reverse_dependencies ~versions ~statistics
              repository pkg_info
        } in
        page :: acc
    ) versions acc in
  OpamPackage.Set.fold aux unique_packages []

let sortby_links ~links ~default ~active =
  let mk_item title =
    let href_str =
      if title = default
      then "index.html"
      else Printf.sprintf "index-%s.html" (String.lowercase title)
    in
    let ahref =
      <:html< <a href="$str: href_str$">sort by $str: title$</a> >>
    in
    if title = active
    then <:html< <li class="active">$ahref$</li> >>
    else <:html< <li>$ahref$</li> >>
  in
  List.map mk_item links

(* Returns a HTML list of the packages in the given repository *)
let to_html ~sortby_links ~dates ~popularity ~active ~compare_pkg repository =
  let packages = repository.packages in
  let unique_packages = O2wPackage.unify_versions packages in
  let sortby_links_html = sortby_links ~active in
  let sorted_packages = List.sort compare_pkg (OpamPackage.Set.elements unique_packages) in
  let packages_html =
    List.fold_left (fun acc pkg ->
      match O2wPackage.get_info ~dates repository pkg with
      | None          -> acc
      | Some pkg_info ->
        let pkg_download =
          try
            let d = OpamPackage.Name.Map.find (OpamPackage.name pkg) popularity in
            Printf.sprintf "Downloads: %Ld | Last update: %s"
              d (O2wMisc.string_of_timestamp pkg_info.pkg_update)
          with Not_found ->
            Printf.sprintf "Last update: %s" (O2wMisc.string_of_timestamp pkg_info.pkg_update)
        in
        <:html<
          <tr>
            <td title=$str:pkg_download$>
             <a href=$str:pkg_info.pkg_href$>
               $str: pkg_info.pkg_name$
             </a>
            </td>
            <td>$str: pkg_info.pkg_version$</td>
            <td>$str: pkg_info.pkg_synopsis$</td>
          </tr>
        >> :: acc)
      []
      (List.rev sorted_packages)
  in
  <:html<
    <div class="row">
      <div class="span9">
        <ul class="nav nav-pills">
          $list: sortby_links_html$
        </ul>
      </div>
      <form class="span3 form-search">
        <div class="input-append">
          <input id="search" class="search-query" type="text" placeholder="Search packages" />
          <button id="search-button" class="btn add-on">
            <i class="icon-search"> </i>
          </button>
        </div>
      </form>
    </div>
    <table class="table table-hover"  id="packages">
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
