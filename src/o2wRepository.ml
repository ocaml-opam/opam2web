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

let packages repo =
  OpamPackage.Set.of_list (OpamPackage.Map.keys repo.packages)

let remove_base_packages packages =
  OpamPackage.Map.filter (fun pkg _ ->
    let name = OpamPackage.name pkg in
    not (OpamMisc.starts_with ~prefix:"base" (OpamPackage.Name.to_string name))
  ) packages

let versions packages =
  OpamPackage.Map.fold (fun nv _ map ->
      let name = OpamPackage.name nv in
      let versions, map =
        try
          let versions = OpamPackage.Name.Map.find name map in
          let map = OpamPackage.Name.Map.remove name map in
          versions, map
        with Not_found ->
          OpamPackage.Version.Set.empty, map in
      let versions = OpamPackage.Version.Set.add (OpamPackage.version nv) versions in
      OpamPackage.Name.Map.add name versions map
    ) packages OpamPackage.Name.Map.empty

let max_versions versions =
  OpamPackage.Name.Map.map (fun versions ->
      OpamPackage.Version.Set.max_elt versions
    ) versions

let max_packages max_versions =
  OpamPackage.Name.Map.fold (fun name version set ->
      OpamPackage.Set.add (OpamPackage.create name version) set
    ) max_versions OpamPackage.Set.empty

let infos repo dates packages =
  let n = OpamPackage.Map.cardinal packages in
  let c = ref 1 in
  let msg () =
    Printf.printf "\r++ Building the package infos: %-5d/%d%!" !c n;
    incr c in
  let result = OpamPackage.Map.fold (fun pkg prefix map ->
      msg ();
      let info = O2wPackage.get_info ~dates repo prefix pkg in
      OpamPackage.Map.add pkg info map
    ) packages OpamPackage.Map.empty in
  Printf.printf "\n%!";
  result

(* Get the last update timestamp of a package in a given repository *)
let last_update repo prefix package =
  let opam_filename =
      OpamFilename.to_string (OpamPath.Repository.opam repo prefix package) in
  (* XXX: the files are updated during the latest pull, need to look at
     the Git commit timestamp instead *)
  let opam_stat = Unix.stat opam_filename in
  opam_stat.Unix.st_mtime

let dates repo packages =
  OpamPackage.Map.fold (fun pkg prefix map ->
      let last_update = last_update repo prefix pkg in
      OpamPackage.Map.add pkg last_update map
    ) packages OpamPackage.Map.empty

(* Create an association list (package_name -> reverse_dependencies) *)
let reverse_dependencies repo packages =
  let revdeps_tbl: (name, name) Hashtbl.t = Hashtbl.create 300 in
  (* Fill a hash table with reverse dependecies (required by...) *)
  OpamPackage.Map.iter (fun pkg prefix ->
    let name = OpamPackage.name pkg in
    let opam_file = OpamFile.OPAM.read
        (OpamPath.Repository.opam repo prefix pkg) in
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

let mk_repo_info repo =
  let root = repo.repo_root in
  let packages = OpamRepository.packages_with_prefixes repo in
  let packages = remove_base_packages packages in
  let versions = versions packages in
  let max_versions = max_versions versions in
  let max_packages = max_packages max_versions in
  let reverse_deps = reverse_dependencies repo packages in
  let pkgs_dates = dates repo packages in
  let pkgs_infos = infos repo pkgs_dates packages in
  { root; repo; versions; packages; max_versions; max_packages; reverse_deps;
    pkgs_infos; pkgs_dates }

(* Load a repository from the local OPAM installation *)
let of_opam repo_name =
  let t = OpamState.load_state "opam2web" in
  let repo = OpamRepositoryName.Map.find repo_name t.OpamState.Types.repositories in
  mk_repo_info repo

(* Load a repository from a directory *)
let of_path root =
  let repo = OpamRepository.local root in
  mk_repo_info repo

let to_page ~href_prefix ~statistics repo_info pkg pkg_info acc =
  match pkg_info with
  | None  ->
    Printf.printf "Skipping %s\n%!" (OpamPackage.to_string pkg);
    acc
  | Some pkg_info ->
    let page = {
      page_link     = { text=pkg_info.pkg_title; href=pkg_info.pkg_href };
      page_depth    = 3;
      page_contents = Template.serialize
        (O2wPackage.to_html ~href_prefix ~statistics repo_info pkg_info)
    } in
    page :: acc

(* Create a list of package pages to generate for a repository *)
let to_pages ~href_prefix ~statistics repo_info =
  OpamPackage.Map.fold
    (to_page ~href_prefix ~statistics repo_info) repo_info.pkgs_infos []

let sortby_links ~href_prefix ~links ~default ~active =
  let mk_item title =
    let href_str =
      if title = default
      then Printf.sprintf "%spkg/" href_prefix
      else Printf.sprintf "%spkg/index-%s.html" href_prefix (String.lowercase title)
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
let to_html ~href_prefix ~content_dir ~sortby_links ~preds
    ~popularity ~active ~compare_pkg repo_info =
  let sortby_links_html = sortby_links ~active in
  let sorted_packages =
    let pkg_set = repo_info.max_packages in
    let pkg_set = OpamPackage.Set.filter
      (O2wPackage.are_preds_satisfied repo_info preds) pkg_set
    in
    let packages = OpamPackage.Set.elements pkg_set in
    List.sort compare_pkg packages
  in
  let packages_html =
    List.fold_left (fun acc pkg ->
        let info =
          try OpamPackage.Map.find pkg repo_info.pkgs_infos
          with Not_found -> None in
        match info with
        | None          -> acc
        | Some pkg_info ->
          let pkg_download =
            try
              let d = OpamPackage.Name.Map.find (OpamPackage.name pkg)
                popularity in
              Printf.sprintf "Downloads: %Ld | Last update: %s"
                d (O2wMisc.string_of_timestamp pkg_info.pkg_update)
            with Not_found ->
              Printf.sprintf "Last update: %s"
                (O2wMisc.string_of_timestamp pkg_info.pkg_update)
          in
          <:html<
            <tr>
              <td title=$str:pkg_download$>
               <a href=$str:href_prefix^pkg_info.pkg_href$>
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
  let template = Template.({ path="repo.xhtml"; fields=[
    "nav",   (default <:html< >>, Optional);
    "pkgs",  (mandatory (),       Required);
  ]}) in
  Template.(generate content_dir template [
    "nav", serialize <:html< $list: sortby_links_html$ >>;
    "pkgs", serialize <:html< <tbody> $list: packages_html$ </tbody> >>;
  ])
