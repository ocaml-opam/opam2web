(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  Opam is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open Cow
open O2wTypes
open OpamStateTypes

let ( ++ ) = Html.( ++ )

let dates universe =
  let any_opam_path = "*/opam" in
  let parse_git_commit_times =
    let rec read_time found = function
      | [] -> found
      | ln::rest -> read_file found (float_of_string ln) rest
    and read_file found time = function
      | [] -> found
      | ""::rest -> read_time found rest
      | path::rest ->
        let suff = String.length any_opam_path - 1 in
        let path = String.(sub path 0 (length path - suff)) in
        let slash = try String.rindex path '/' + 1 with Not_found -> 0 in
        let pkg = String.(sub path slash (length path - slash)) in
        let found =
          match OpamPackage.of_string_opt pkg with
          | Some pkg -> OpamPackage.Map.add pkg time found
          | None -> found
        in
        read_file found time rest
    in
    read_time
  in
  let repos_list =
    match universe.switch_config.OpamFile.Switch_config.repos with
    | None -> invalid_arg "dates"
    | Some r -> r
  in
  let dates =
    List.fold_right (fun repo dates ->
        let command = [
          "git"; "log"; "--name-only"; "--diff-filter=ACR"; "--reverse";
          "--pretty=format:%ct"; "-m"; "--first-parent"; "--"; any_opam_path;
        ] in
        let repo_name = OpamRepositoryName.to_string repo in
        try
          let times =
            OpamFilename.in_dir
              (OpamFilename.dirname_dir
                 (OpamRepositoryPath.packages_dir
                    (OpamRepositoryState.get_root universe.switch_repos repo)))
              (fun () -> OpamSystem.read_command_output command)
          in
          parse_git_commit_times dates times
        with (OpamSystem.Process_error _ | Failure _ as e) ->
          OpamConsole.warning "Date retrieval for %s using" repo_name;
          OpamConsole.warning "%s" (String.concat " " command);
          OpamConsole.warning "failed with:\n%s" (Printexc.to_string e);
          dates)
      repos_list
      OpamPackage.Map.empty
  in
  let missing = OpamPackage.Set.diff universe.packages (OpamPackage.keys dates) in
  if not (OpamPackage.Set.is_empty missing)
  then (* begin *)
    OpamConsole.warning "Couldn't retrieve creation date for:\n%s"
      (OpamStd.List.concat_map " " OpamPackage.to_string (OpamPackage.Set.elements missing));
    (* OpamPackage.Set.fold (fun pkg map ->
   *       let opam = OpamPackage.Map.find pkg universe.opams in
   *       let dir = match OpamFile.OPAM.metadata_dir opam with
   *         | Some d -> d
   *         | None -> invalid_arg "dates"
   *       in
   *       let opam_stat =
   *         Unix.stat (OpamFilename.to_string OpamFilename.Op.(dir // "opam"))
   *       in
   *       OpamPackage.Map.add pkg opam_stat.Unix.st_mtime map
   *     ) missing dates
   * end
   * else *)
    dates

let depends st deps_fun =
  OpamPackage.Map.fold (fun pkg opam ->
      let env v = match OpamVariable.Full.to_string v with
        | "name" -> Some (S (OpamPackage.name_to_string pkg))
        | "version" -> Some (S (OpamPackage.version_to_string pkg))
        | _ -> None
      in
      let deps =
        OpamFormula.packages st.packages @@
        OpamFilter.filter_formula ~default:true env (deps_fun opam)
      in
      OpamPackage.Map.add pkg deps)
    st.opams OpamPackage.Map.empty

let rev_depends deps =
  OpamPackage.Map.fold (fun pkg ->
      OpamPackage.Set.fold (fun pkg1 ->
          OpamPackage.Map.update pkg1
            (OpamPackage.Set.add pkg) OpamPackage.Set.empty))
    deps OpamPackage.Map.empty


(* Dependencies used for the dependency cone. They differ from [depends] above.
   They are an approximation of what [opam list --required-by] would show because we cannot
   resolve variables that need a switch (os, arch, ...). To avoid including dependencies of multiple platforms
   (say both linux and macos), we remove disjunctions (by picking a single branch).

   [filter_default] is true to match OpamSwitchState.dependencies, which keeps a dependency whose
   filter it cannot decide.
 *)
let rec remove_disjunction = function
 | (OpamFormula.Empty as x) | (Atom _ as x) -> x
 | Block x -> remove_disjunction x
 | And (a, b) -> And (remove_disjunction a, remove_disjunction b)
 | Or (a, _) -> remove_disjunction a

let cone_depends st =
 OpamPackage.Map.fold (fun pkg opam ->
   let deps =
     OpamFormula.packages st.OpamStateTypes.packages @@
     remove_disjunction @@
     OpamPackageVar.all_depends ~build:true ~post:false ~test:false
       ~doc:false ~dev_setup:false ~depopts:false ~filter_default:true
       st opam
     in
     OpamPackage.Map.add pkg deps)
   st.opams OpamPackage.Map.empty


(* A graph where nodes are packages and there is an edge a->b if package a depends on package b. *)
module PkgGraph = struct
  type t = package_set package_map
  module V = OpamPackage
  let iter_vertex f deps = OpamPackage.Map.iter (fun pkg _ -> f pkg) deps
  let iter_succ f deps pkg =
    match OpamPackage.Map.find_opt pkg deps with
    | None -> ()
    | Some pkgs -> OpamPackage.Set.iter f pkgs
end

module PkgTopo = Graph.Topological.Make (PkgGraph)

let dependency_cone_sizes depends =
  (* packages in reverse topological order *)
  let pkgs = PkgTopo.fold (fun pkg acc -> pkg :: acc) depends [] in
  let cones =
    List.fold_left (fun acc pkg ->
        let deps =
          OpamStd.Option.default OpamPackage.Set.empty
            (OpamPackage.Map.find_opt pkg depends)
        in
        (* Some packages include [ocaml] as a dependency. We remove [ocaml] from their cone to avoid including
           also ocaml-base-compiler, ocaml-option-bytecode-only, ...
           Except for [ocaml] itself, for which we want to see the full cone. *)
        let deps = match OpamPackage.Name.to_string (OpamPackage.name pkg) with
          | "ocaml" -> deps
          | _ -> OpamPackage.Set.filter (fun p -> OpamPackage.Name.to_string p.name <> "ocaml") deps
        in
        let cone =
          OpamPackage.Set.fold (fun dep cone ->
              match OpamPackage.Map.find_opt dep acc with
              | Some dep_cone -> OpamPackage.Name.Set.union dep_cone cone
              | None -> cone)
            deps (OpamPackage.Name.Set.singleton pkg.name)
        in
        OpamPackage.Map.add pkg cone acc)
      OpamPackage.Map.empty pkgs
  in
  OpamPackage.Map.map OpamPackage.Name.Set.cardinal cones

let to_page ~prefix universe pkg acc =
  try
    if Unix.isatty Unix.stdout then
      Printf.printf "+++ Building page for %s%!\r\027[K"
        (OpamPackage.to_string pkg);
    let page = {
      page_source   = "packages/" ^ OpamPackage.Name.to_string pkg.name ^ "/" ^ OpamPackage.to_string pkg;
      page_link     =
        O2wPackage.pkg_href ~href_base:(Uri.of_string (prefix^"/")) pkg;
      page_link_text = OpamPackage.to_string pkg;
      page_link_html = Html.string (OpamPackage.to_string pkg);
      page_depth    = 3;
      page_contents = Template.serialize
        (O2wPackage.to_html ~prefix:"../../" universe pkg);
      page_srcurl = None;
    } in
    page :: acc
  with e ->
    Printf.printf "Skipping %s (%s)\n%!" (OpamPackage.to_string pkg)
      (Printexc.to_string e);
    Printexc.print_backtrace stdout;
    acc

(* Create a list of package pages to generate for a universe *)
let to_pages ~prefix universe =
  let projects = OpamPackage.Name.Map.fold (fun name versions acc ->
    let max_v = OpamPackage.Version.Set.max_elt versions in
    let pkg  = OpamPackage.create name max_v in
    let name = OpamPackage.Name.to_string name in
    let href = Uri.make ~path:(prefix ^ "/" ^ name ^ "/") () in
    let page = {
      page_source   = "packages/" ^ name;
      page_link     = href;
      page_link_text = name;
      page_link_html = Html.string name;
      page_depth    = 2;
      page_contents = Template.serialize
        (O2wPackage.to_html ~prefix:"../" universe pkg);
      page_srcurl = None;
    } in
    page :: acc
  ) (OpamPackage.to_map universe.st.packages) [] in
  OpamPackage.Set.fold
    (to_page ~prefix universe) universe.st.packages projects

let sortby_links ~links ~default ~active =
  let mk_item title =
    let href =
      if title = default
      then Uri.of_string "./"
      else Uri.of_string ("index-"^(String.lowercase_ascii title)^".html")
    in
    let ahref =
      Html.a ~href (Html.string "sort by " @ Html.string title)
    in
    Html.li ahref ?cls:(if title = active then Some "active" else None)
  in
  List.map mk_item links

let latest_version_packages universe =
  OpamPackage.Name.Map.fold (fun name vs acc ->
      OpamPackage.Set.add
        (OpamPackage.create name (OpamPackage.Version.Set.max_elt vs))
        acc)
    (OpamPackage.to_map universe.packages)
    OpamPackage.Set.empty

let load_opam_state repo_roots =
  let gt = {
    global_lock = OpamSystem.lock_none;
    root = OpamStateConfig.(!r.root_dir);
    config = OpamStd.Option.Op.(OpamStateConfig.(load ~lock_kind:`Lock_none !r.root_dir) +!
                                OpamFile.Config.empty);
    global_variables = OpamVariable.Map.empty;
    global_state_to_upgrade = {gtc_repo = false; gtc_switch = false};
  } in
  let repo_roots =
    List.map (fun r ->
        OpamRepositoryName.of_string (OpamFilename.Dir.to_string r),
        r)
      repo_roots
  in
  let repositories =
    List.fold_left (fun map (repo_name, _) ->
        let repo = {
          OpamTypes.
          repo_name; repo_url = OpamUrl.empty; repo_trust = None;
        } in
        OpamRepositoryName.Map.add repo_name repo map)
      OpamRepositoryName.Map.empty repo_roots
  in
  let repos_definitions =
    List.fold_left (fun map (repo_name, repo_root) ->
        OpamRepositoryName.Map.add repo_name
          (OpamFile.Repo.safe_read (OpamRepositoryPath.repo repo_root))
          map)
      OpamRepositoryName.Map.empty repo_roots
  in
  let repo_opams =
    List.fold_left (fun map (repo_name, repo_root) ->
        OpamRepositoryName.Map.add repo_name
          (OpamRepositoryState.load_opams_from_dir repo_name repo_root)
          map)
      OpamRepositoryName.Map.empty repo_roots
  in
  let repos_tmp =
    let repos_tmp = Hashtbl.create 2 in
    List.iter (fun (repo_name, repo_root) ->
        Hashtbl.add repos_tmp repo_name (lazy repo_root))
      repo_roots;
    repos_tmp
  in
  let rt = {
    repos_global = gt;
    repos_lock = OpamSystem.lock_none;
    repositories; repos_definitions; repo_opams; repos_tmp;
  } in
  OpamSwitchState.load_virtual ~repos_list:(fst (List.split repo_roots))
    gt rt

let load statistics repo_roots =
  Printf.printf "++ Loading opam state.\n%!";
  let st = load_opam_state repo_roots in
  Printf.printf "++ Gathering dependencies.\n%!";
  let deps = depends st OpamFile.OPAM.depends in
  let rdeps = rev_depends deps in
  let depopts = depends st OpamFile.OPAM.depopts in
  let rev_depopts = rev_depends depopts in
  Printf.printf "++ Computing dependency cones.\n%!";
  let dependency_cone_sizes = dependency_cone_sizes (cone_depends st) in
  Printf.printf "++ Getting package modification dates from git.\n%!";
  let dates = dates st in
  let version_downloads, name_popularity =
    match statistics with
    | None -> None, None
    | Some s ->
      let vp = s.month_stats.pkg_stats, s.hash_pkgs_map in
      let np =
        OpamPackage.Map.fold (fun nv x ->
            OpamPackage.Name.Map.update nv.name (Int64.add x) 0L)
          s.month_leaf_pkg_stats OpamPackage.Name.Map.empty
      in
      Some vp, Some np
  in
  {
    st;
    dates;
    version_downloads;
    name_popularity;
    depends = deps;
    rev_depends = rdeps;
    dependency_cone_sizes;
    depopts;
    rev_depopts;
  }

(* Returns a HTML list of the packages in the given universe *)
let to_html ~content_dir ~sortby_links ~active ~compare_pkg univ =
  let sortby_links_html = sortby_links ~active in
  let sorted_packages =
    let pkg_set = latest_version_packages univ.st in
    let packages = OpamPackage.Set.elements pkg_set in
    List.sort compare_pkg packages
  in
  let repos_html =
    let row r =
      let r =
        OpamRepositoryName.Map.find r
          univ.st.switch_repos.repositories
      in
      let s =
        Printf.sprintf "%s(%s)"
          (OpamRepositoryName.to_string r.repo_name)
          (OpamUrl.to_string r.repo_url)
      in
      [Html.string s]
    in
    match univ.st.switch_config.OpamFile.Switch_config.repos with
    | None -> Html.empty
    | Some repos -> Html.Create.table repos ~row
  in
  let packages_html =
    List.fold_left (fun acc pkg ->
        let info =
          try Some (OpamPackage.Map.find pkg univ.st.opams)
          with Not_found -> None in
        match info with
        | None          -> acc
        | Some pkg_info ->
          let pkg_name = OpamPackage.name pkg in
          let pkg_download =
            match
              OpamStd.Option.Op.(univ.name_popularity >>= OpamPackage.Name.Map.find_opt pkg_name)
            with
            | Some d -> [Printf.sprintf "Downloads: %Ld" d]
            | None -> []
          in
          let pkg_published = match OpamPackage.Map.find_opt pkg univ.dates with
            | Some timestamp -> [
              Printf.sprintf "Published: %s"
                (O2wMisc.string_of_timestamp timestamp)
            ]
            | None -> []
          in
          (* Number of reverse dependencies *)
          let pkg_nb_rev_depends =
            match OpamPackage.Map.find_opt pkg univ.rev_depends with
            | None -> 0
            | Some rdeps ->
              OpamPackage.Name.Set.cardinal (OpamPackage.names_of_packages rdeps)
          in
          (* Size of the dependency cone: everything an installation of this
             package pulls in, itself included. *)
          let pkg_dependency_cone_size =
            OpamStd.Option.default 0
              (OpamPackage.Map.find_opt pkg univ.dependency_cone_sizes)
          in
          let tags = String.concat " " (OpamFile.OPAM.tags pkg_info) in
          let pkg_tags = if tags = "" then [] else ["Tags: "^tags] in
          (* Dependency names, for the search box to filter on *)
          let deps =
            OpamFormula.fold_left (fun acc (dep_name, _) ->
                OpamPackage.Name.to_string dep_name :: acc)
              [] (OpamFile.OPAM.depends pkg_info)
            |> List.rev
            |> String.concat " "
          in
          let pkg_tooltip = String.concat " | " (pkg_download @ pkg_published @ pkg_tags) in
          let name = OpamPackage.Name.to_string pkg_name in
          let pkg_href = Uri.(resolve "http" (of_string "../packages/") (of_string name)) in
          let synopsis =
            OpamStd.Option.Op.((OpamFile.OPAM.synopsis pkg_info >>| Html.string)
                               +! Html.empty)
            ++ Html.span ~cls:"invisible" (Html.string tags)
          in
          (Html.tag "tr" ~attrs:["title", pkg_tooltip;
                                 "data-deps", deps]
             (Html.tag "td"
                (Html.a ~href:pkg_href
                   (Html.string (OpamPackage.name_to_string pkg)))
              @ Html.tag "td" (Html.string (OpamPackage.version_to_string pkg))
              @ Html.tag "td" synopsis
              @ Html.tag "td" (Html.int pkg_nb_rev_depends)
              @ Html.tag "td" (Html.int pkg_dependency_cone_size)))
          :: acc)
      []
      (List.rev sorted_packages)
  in
  let template = Template.({ path="universe.xhtml"; fields=[
    "nav",   (default Html.empty, Optional);
    "repos", (mandatory (),       Optional);
    "pkgs",  (mandatory (),       Required);
  ]}) in
  Template.(generate content_dir template [
    "nav",   serialize (List.concat sortby_links_html);
    "repos", serialize repos_html;
    "pkgs",  serialize(Html.tag "tbody" (List.concat packages_html));
  ])

let generate_json ?statistics universe =
  let open O2wJson in
  let open OpamPackage in
  let global_map =
    let dl_month =
      match statistics with
      | None -> fun _ -> None
      | Some s -> fun pkg -> Map.find_opt pkg s.month_leaf_pkg_stats
    in
    let dl =
      match universe.version_downloads with
      | None -> fun _ -> None
      | Some (vd,_) -> fun pkg -> Map.find_opt pkg vd
    in
    Set.fold (fun pkg map ->
        let n = name pkg in
        let v = version pkg in
        let tm = Map.find_opt pkg universe.dates in
        let upd_vmap = Version.Map.add v (tm, (dl pkg), (dl_month pkg)) in
        Name.Map.update n upd_vmap (upd_vmap Version.Map.empty) map)
      universe.st.packages Name.Map.empty
  in
  let json_gm =
    let lst =
      Name.Map.fold (fun name vmap lst ->
          let versions =
            Version.Map.fold (fun version (tm,dl,dl_month) lst ->
                `Assoc [
                  json_version version;
                  json_timestamp_opt tm;
                  json_downloads_opt dl;
                  json_month_downloads_opt dl_month
                ]::lst)
              vmap []
          in
          `Assoc [ json_name name; "versions", `List (List.rev versions)]::lst)
        global_map []
    in
    `List (List.rev lst)
  in
  write "stats" json_gm

(* (\** Generate a universe from a list of repositories *\)
 * let of_repositories ?preds index repos =
 *   map O2wPackage.html_descr (of_repositories ?preds index repos) *)
