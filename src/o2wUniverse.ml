(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open Cow.Html
open O2wTypes

module RepoMap = OpamRepositoryName.Map

let remove_base_packages pkg_idx =
  OpamPackage.Map.filter (fun pkg _ ->
    let name = OpamPackage.name pkg in
    not (OpamMisc.starts_with ~prefix:"base" (OpamPackage.Name.to_string name))
  ) pkg_idx

let versions pkg_idx =
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
    ) pkg_idx OpamPackage.Name.Map.empty

let max_versions versions =
  OpamPackage.Name.Map.map (fun versions ->
      OpamPackage.Version.Set.max_elt versions
    ) versions

let max_packages max_versions =
  OpamPackage.Name.Map.fold (fun name version set ->
      OpamPackage.Set.add (OpamPackage.create name version) set
    ) max_versions OpamPackage.Set.empty

let infos repos dates pkg_idx =
  let n = OpamPackage.Map.cardinal pkg_idx in
  let c = ref 1 in
  let msg () =
    Printf.printf "\r++ Building the package infos: %-5d/%d%!" !c n;
    incr c in
  let result = OpamPackage.Map.fold (fun pkg (repo,prefix) map ->
    msg ();
    let repo = RepoMap.find repo repos in
    let info = O2wPackage.get_info ~dates repo prefix pkg in
    OpamPackage.Map.add pkg info map
  ) pkg_idx OpamPackage.Map.empty in
  Printf.printf "\n%!";
  result

(* Get the last update timestamp of a package in a given repository *)
let last_update repo prefix package =
  let opam_filename = OpamPath.Repository.opam repo prefix package in
  try
    let command =
      [ "git"; "log"; "--reverse"; "--pretty=format:%ct"; "--";
        "*/" ^ OpamPackage.to_string package ^ "/opam" ] in
    let return =
      OpamFilename.in_dir
        (OpamFilename.dirname_dir (OpamPath.Repository.packages_dir repo))
        (fun () -> OpamSystem.read_command_output command) in
    match return with
    | ts::_ -> float_of_string ts
    | [] -> raise Not_found
  with
  | (OpamSystem.Process_error _ | Failure "float_of_string" | Not_found as e) ->
    OpamGlobals.warning "last_update of %s failed with %s\n" (OpamPackage.to_string package) (Printexc.to_string e);
    let opam_stat = Unix.stat (OpamFilename.to_string opam_filename) in
    opam_stat.Unix.st_mtime

let dates repos pkg_idx =
  OpamPackage.Map.fold (fun pkg (repo,prefix) map ->
      let last_update = last_update (RepoMap.find repo repos) prefix pkg in
      OpamPackage.Map.add pkg last_update map
    ) pkg_idx OpamPackage.Map.empty

(* Create an association list (package_name -> reverse_dependencies) *)
let reverse_dependencies opams =
  let revdeps_tbl: (name, name) Hashtbl.t = Hashtbl.create 300 in
  (* Fill a hash table with reverse dependencies (required by...) *)
  OpamPackage.Map.iter (fun pkg opam ->
    let depends = OpamFile.OPAM.depends opam in
    List.iter (fun (depname,_) ->
        Hashtbl.add revdeps_tbl depname (OpamPackage.name pkg)
      ) (OpamFormula.atoms depends)
  ) opams;
  let names =
    Hashtbl.fold (fun name _ acc -> name :: acc) revdeps_tbl [] in
  (* Build the association list *)
  List.fold_left (fun acc name ->
    let revdeps = Hashtbl.find_all revdeps_tbl name in
    let names = OpamPackage.Name.Set.of_list revdeps in
    OpamPackage.Name.Map.add name names acc
  ) OpamPackage.Name.Map.empty names

let mk_universe_info preds index repos pkg_idx opams =
  let pkg_idx = remove_base_packages pkg_idx in
  let versions = versions pkg_idx in
  let max_versions = max_versions versions in
  let max_packages = max_packages max_versions in
  let reverse_deps = reverse_dependencies opams in
  let pkgs_dates = dates repos pkg_idx in
  let pkgs_infos = infos repos pkgs_dates pkg_idx in
  { repos; preds; index; versions; pkg_idx; max_versions; max_packages;
    reverse_deps; pkgs_infos; pkgs_opams=opams; pkgs_dates }

(* Generate a universe from a stack of repositories *)
let of_repositories ?(preds=[]) index repo_stack =
  let t = OpamState.load_state "opam2web" in
  let opam_repos = t.OpamState.Types.repositories in
  let repos,_ = List.fold_left
    (fun (rmap,repo_priority) -> function
    | Path path ->
      let repo_name = OpamRepositoryName.of_string ("path:"^path) in
      RepoMap.add repo_name
        { OpamRepository.local (OpamFilename.Dir.of_string path)
          with repo_priority; repo_name } rmap,
      repo_priority - 1
    | Local remote ->
      let repo_name = OpamRepositoryName.of_string ("local:"^remote) in
      begin
        try
          let repo = RepoMap.find
            (OpamRepositoryName.of_string remote) opam_repos in
          RepoMap.add repo_name
            { repo with repo_priority; repo_name } rmap,
          repo_priority - 1
        with Not_found ->
          Printf.printf "Local opam remote '%s' not found, skipping.\n%!" remote;
          Printf.printf "Maybe you wanted the 'path' namespace?\n%!";
          rmap, repo_priority
      end
    | Opam ->
      List.fold_left (fun (m,i) r ->
        let k = r.repo_name in
        let repo_name = OpamRepositoryName.(of_string ("opam:"^(to_string k))) in
        RepoMap.add repo_name { r with repo_priority = i; repo_name } m, i - 1
      ) (rmap, repo_priority) (OpamRepository.sort opam_repos)
    ) (RepoMap.empty,256) repo_stack
  in
  let pkg_idx = OpamRepository.package_index repos in
  let packages = OpamPackage.Set.of_list (OpamPackage.Map.keys pkg_idx) in
  let opams = OpamPackage.Set.fold (fun nv map ->
    try
      let repo, prefix = OpamPackage.Map.find nv pkg_idx in
      let repo = OpamRepositoryName.Map.find repo repos in
      let file = OpamPath.Repository.opam repo prefix nv in
      let opam = OpamFile.OPAM.read file in
      OpamPackage.Map.add nv opam map
    with
    | Not_found ->
      Printf.printf "Cannot find an OPAM file for %s, skipping.\n"
        (OpamPackage.to_string nv);
      map
    | Parsing.Parse_error | OpamSystem.Internal_error _ ->
      Printf.printf "Errors while parsing %s OPAM file, skipping.\n"
        (OpamPackage.to_string nv);
      map
  ) packages OpamPackage.Map.empty
  in
  let universe_info = mk_universe_info preds index repos pkg_idx opams in
  let universe = {
    u_packages  = packages;
    u_action    = Depends;
    u_installed = OpamPackage.Set.empty;
    u_available = packages; (* TODO: ok? check opam's semantics *)
    u_depends   = OpamPackage.Map.map OpamFile.OPAM.depends opams;
    u_depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts opams;
    u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts opams;
    u_installed_roots = OpamPackage.Set.empty;
    u_pinned    = OpamPackage.Name.Map.empty;
  } in
  let dep_closure = OpamSolver.dependencies
    ~depopts:(List.mem [Depopt] preds) ~installed:false universe
    (OpamPackage.Set.filter
       (O2wPackage.are_preds_satisfied universe_info) packages)
  in
  let packages = OpamPackage.Set.of_list dep_closure in
  let pkg_idx = OpamPackage.Map.filter
    (fun k _ -> OpamPackage.Set.mem k packages) pkg_idx in
  mk_universe_info preds index repos pkg_idx opams

let to_page ~statistics universe pkg pkg_info acc =
  match pkg_info with
  | None  ->
    Printf.printf "Skipping %s\n%!" (OpamPackage.to_string pkg);
    acc
  | Some pkg_info ->
    try
      let page = {
        page_link     = { text=pkg_info.pkg_title;
                          href=Uri.to_string pkg_info.pkg_href };
        page_depth    = 3;
        page_contents = Template.serialize
            (O2wPackage.to_html ~statistics universe pkg_info)
      } in
      page :: acc
    with e ->
      Printf.printf "Skipping %s (%s)\n%!" (OpamPackage.to_string pkg)
        (Printexc.to_string e);
      acc

(* Create a list of package pages to generate for a universe *)
let to_pages ~statistics universe =
  OpamPackage.Map.fold
    (to_page ~statistics universe) universe.pkgs_infos []

let sortby_links ~links ~default ~active =
  let mk_item title =
    let href =
      if title = default
      then Uri.of_string "./"
      else Uri.of_string ("index-"^(String.lowercase title)^".html")
    in
    let ahref =
      <:html< <a href=$uri: href$>sort by $str: title$</a> >>
    in
    if title = active
    then <:html< <li class="active">$ahref$</li> >>
    else <:html< <li>$ahref$</li> >>
  in
  List.map mk_item links

(* Returns a HTML list of the packages in the given universe *)
let to_html ~content_dir ~sortby_links ~popularity ~active
    ~compare_pkg universe =
  let sortby_links_html = sortby_links ~active in
  let sorted_packages =
    let pkg_set = universe.max_packages in
    let pkg_set = match universe.index with
      | Index_all -> pkg_set
      | Index_pred -> OpamPackage.Set.filter
        (O2wPackage.are_preds_satisfied universe)
        pkg_set
    in
    let packages = OpamPackage.Set.elements pkg_set in
    List.sort compare_pkg packages
  in
  let repos_html =
    let repos = OpamRepository.sort universe.repos in
    List.map (fun r ->
      <:html< <tr><td>$str:OpamRepository.to_string r$</td></tr> >>
    ) repos
  in
  let packages_html =
    List.fold_left (fun acc pkg ->
        let info =
          try OpamPackage.Map.find pkg universe.pkgs_infos
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
          let pkg_href = Uri.(resolve "http"
                                (of_string "../") pkg_info.pkg_href) in
          <:html<
            <tr>
              <td title=$str:pkg_download$>
               <a href=$uri:pkg_href$>
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
  let template = Template.({ path="universe.xhtml"; fields=[
    "nav",   (default <:html< >>, Optional);
    "repos", (mandatory (),       Optional);
    "pkgs",  (mandatory (),       Required);
  ]}) in
  Template.(generate content_dir template [
    "nav",   serialize <:html< $list: sortby_links_html$ >>;
    "repos", serialize <:html< <tbody> $list: repos_html$ </tbody> >>;
    "pkgs",  serialize <:html< <tbody> $list: packages_html$ </tbody> >>;
  ])
