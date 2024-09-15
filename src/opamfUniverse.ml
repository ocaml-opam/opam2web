(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012 INRIA                                                *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2013 David Sheets                                         *)
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

(* open OpamTypes
 * 
 * module RepoMap = OpamRepositoryName.Map
 * 
 * type 'a pkg = OpamFile.OPAM.t
 * 
 * type pkg_idx = (OpamTypes.repository_name * string option) OpamTypes.package_map
 * 
 * type repository = string
 * 
 * type pred =
 * | Tag of string
 * | Depopt
 * | Not of pred
 * | Repo of string
 * | Pkg of string
 * 
 * type index = Index_pred | Index_all
 * 
 * type pred_dnf = pred OpamFormula.dnf
 * 
 * type 'a t = {
 *   repos       : OpamTypes.repository repository_name_map;
 *   preds       : pred_dnf;
 *   index       : index;
 *   pkg_idx     : (repository_name * string option) package_map;
 *   versions    : version_set name_map;
 *   max_packages: package_set;
 *   max_versions: version name_map;
 *   rev_depends : OpamfuFormula.version_dnf name_map package_map;
 *   rev_depopts : OpamfuFormula.version_dnf name_map package_map;
 *   pkgs_infos  : 'a pkg package_map;
 *   pkgs_opams  : OpamFile.OPAM.t package_map;
 *   pkgs_dates  : float package_map;
 * }
 * 
 * module Repo = struct
 *   (\* Get the repository opam file corresponding to a repo *\)
 *   let links repo =
 *     let repo_file = OpamRepositoryPath.repo repo in
 *     OpamFile.Repo.safe_read repo_file    
 * end
 * 
 * module Pkg = struct
 *   (\* Get the repository corresponding to a package in a universe *\)
 *   let to_repo universe pkg =
 *     let { pkg_idx; repos } = universe in
 *     let repo_name, _ = OpamPackage.Map.find pkg pkg_idx in
 *     let repo = OpamRepositoryName.Map.find repo_name repos in
 *     repo
 * 
 *   let are_preds_satisfied opams pkg_idx preds pkg =
 *     try
 *       let pkg_opam = OpamPackage.Map.find pkg opams in
 *       let tags = OpamFile.OPAM.tags pkg_opam in
 *       let rec is_satisfied = function
 *         | Tag t -> List.mem t tags
 *         | Repo r ->
 *           let (rn,_) = OpamPackage.Map.find pkg pkg_idx in
 *           r = (OpamRepositoryName.to_string rn)
 *         | Not p -> not (is_satisfied p)
 *         | Depopt-> false (\* TODO: correct? *\)
 *         | Pkg p ->
 *           let name = OpamPackage.(Name.to_string (name pkg)) in
 *           p = name
 *       in
 *       let rec aux = function
 *         | [] -> false
 *         | pred::rest ->
 *           if List.for_all is_satisfied pred then true else aux rest
 *       in
 *       if preds = [] then true else aux preds
 *     with Not_found -> false
 * 
 *   let href ?href_base name version =
 *     let name = OpamPackage.Name.to_string name in
 *     let version = OpamPackage.Version.to_string version in
 *     let base = Printf.sprintf "%s/%s.%s/" name name version in
 *     let base = Uri.of_string base in
 *     match href_base with
 *     | None   -> base
 *     | Some p -> Uri.resolve "http" p base
 * 
 *   (\* Build a record representing information about a package *\)
 *   let get_info ~dates repo prefix pkg = pkg
 * end
 * (\*
 * let opam_universe_of_packages_and_opams packages opams = {
 *   OpamSolver.empty_universe with
 *     u_packages  = packages;
 *     u_action    = Query;
 *     u_available = packages; (\* TODO: ok? check opam's semantics *\)
 *     u_depends   = OpamPackage.Map.map OpamFile.OPAM.depends opams;
 *     u_depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts opams;
 *     u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts opams;
 *   }
 * *\)
 * let package_set_of_map m = OpamPackage.(Set.of_list (Map.keys m))
 * 
 * (\* let to_opam_universe t = opam_universe_of_packages_and_opams
 *  *   (package_set_of_map t.pkg_idx)
 *  *   t.pkgs_opams *\)
 * 
 * let index_by_repo pkg_idx =
 *   OpamPackage.Map.fold (fun pkg (repo,prefix) map ->
 *     let pkg_map = try RepoMap.find repo map with Not_found -> OpamPackage.Map.empty in
 *     RepoMap.add repo (OpamPackage.Map.add pkg prefix pkg_map) map
 *   ) pkg_idx RepoMap.empty
 * 
 * let versions pkg_idx =
 *   OpamPackage.Map.fold (fun nv _ map ->
 *     let name = OpamPackage.name nv in
 *     let versions, map =
 *       try
 *         let versions = OpamPackage.Name.Map.find name map in
 *         let map = OpamPackage.Name.Map.remove name map in
 *         versions, map
 *       with Not_found ->
 *         OpamPackage.Version.Set.empty, map in
 *     let versions = OpamPackage.Version.Set.add
 *       (OpamPackage.version nv) versions in
 *     OpamPackage.Name.Map.add name versions map
 *   ) pkg_idx OpamPackage.Name.Map.empty
 * 
 * let max_versions versions =
 *   OpamPackage.Name.Map.map (fun versions ->
 *     OpamPackage.Version.Set.max_elt versions
 *   ) versions
 * 
 * let max_packages max_versions =
 *   OpamPackage.Name.Map.fold (fun name version set ->
 *     OpamPackage.Set.add (OpamPackage.create name version) set
 *   ) max_versions OpamPackage.Set.empty
 * 
 * let infos repos dates pkg_idx =
 *   let n = OpamPackage.Map.cardinal pkg_idx in
 *   let c = ref 1 in
 *   let msg () =
 *     Printf.printf "\r++ Building the package infos: %-5d/%d%!" !c n;
 *     incr c in
 *   let result = OpamPackage.Map.fold (fun pkg (repo,prefix) map ->
 *     msg ();
 *     let repo = RepoMap.find repo repos in
 *     let info = Pkg.get_info ~dates repo prefix pkg in
 *     OpamPackage.Map.add pkg info map
 *   ) pkg_idx OpamPackage.Map.empty in
 *   Printf.printf "\n%!";
 *   result
 * 
 * let dates repos pkg_idx =
 *   let any_opam_path = "*/opam" in
 *   let parse_git_commit_times =
 *     let rec read_time pkgs found = function
 *       | [] -> pkgs,found
 *       | ln::rest -> read_file pkgs found (float_of_string ln) rest
 *     and read_file pkgs found time = function
 *       | [] -> pkgs,found
 *       | ""::rest -> read_time pkgs found rest
 *       | path::rest ->
 *         let suff = String.length any_opam_path - 1 in
 *         let path = String.(sub path 0 (length path - suff)) in
 *         let slash = try String.rindex path '/' + 1 with Not_found -> 0 in
 *         let pkg = String.(sub path slash (length path - slash)) in
 *         match OpamPackage.of_string_opt pkg with
 *         | Some pkg ->
 *           if OpamPackage.Map.mem pkg pkgs
 *           then read_file
 *             (OpamPackage.Map.remove pkg pkgs)
 *             (OpamPackage.Map.add pkg time found)
 *             time rest
 *           else read_file pkgs found time rest
 *         | None -> read_file pkgs found time rest
 *     in read_time
 *   in
 *   let repo_idx = index_by_repo pkg_idx in
 *   let missing, found = RepoMap.fold (fun repo pkg_map (missing,found) ->
 *     let command = [
 *       "git"; "log"; "--name-only"; "--diff-filter=A"; "--reverse";
 *       "--pretty=format:%ct"; "--"; any_opam_path;
 *     ] in
 *     let repo_name = OpamRepositoryName.to_string repo in
 *     let repo = RepoMap.find repo repos in
 *     try
 *       let times = OpamFilename.in_dir
 *         (OpamFilename.dirname_dir (OpamRepositoryPath.packages_dir repo))
 *         (fun () -> OpamSystem.read_command_output command)
 *       in
 *       let unmatched,found = parse_git_commit_times pkg_map found times in
 *       OpamPackage.Map.(fold add unmatched missing),found
 *     with (OpamSystem.Process_error _ | Failure _ as e) ->
 *       OpamConsole.warning "Date retrieval for %s using" repo_name;
 *       OpamConsole.warning "%s" (String.concat " " command);
 *       OpamConsole.warning "failed with:\n%s" (Printexc.to_string e);
 *       (OpamPackage.Map.(fold add pkg_map missing),found)
 *   ) repo_idx OpamPackage.Map.(empty, empty) in
 *   if OpamPackage.Map.cardinal missing > 0
 *   then begin
 *     OpamConsole.warning "Couldn't retrieve creation date for:";
 *     OpamPackage.Map.fold (fun pkg prefix map ->
 *       OpamConsole.warning "%s" (OpamPackage.to_string pkg);
 *       let (repo,prefix) = OpamPackage.Map.find pkg pkg_idx in
 *       let repo = RepoMap.find repo repos in
 *       let opam_filename = OpamRepositoryPath.opam repo prefix pkg in
 *       (\* TODO: errors? *\)
 *       let opam_stat = Unix.stat (OpamFile.to_string opam_filename) in
 *       OpamPackage.Map.add pkg opam_stat.Unix.st_mtime map
 *     ) missing found
 *   end
 *   else found
 * 
 * (\* Create a reverse version constraint map
 *    (package -> package_name -> version_constraint) *\)
 * let reverse_deps formulas pkg_idx versions =
 *   let open OpamPackage in
 *   let add_version map pkg revdep =
 *     let name = name revdep in
 *     let version = version revdep in
 *     let revmap =
 *       try Map.find pkg map
 *       with Not_found -> Name.Map.empty
 *     in
 *     let revdepvs =
 *       try Name.Map.find name revmap
 *       with Not_found -> Version.Set.empty
 *     in
 *     Map.add pkg
 *       (Name.Map.add name
 *          Version.Set.(add version revdepvs)
 *          revmap)
 *       map
 *   in
 *   let depnames_of_formula f = List.fold_left (fun depset (name,_) ->
 *       Name.Set.add name depset
 *   ) Name.Set.empty (OpamFormula.atoms f)
 *   in
 *   let add_satisfiers pkg f = Name.Set.fold (fun name map ->
 *     let versions =
 *       try Name.Map.find name versions
 *       with Not_found ->
 *         OpamConsole.warning "Missing dependency %s in package %s"
 *           (OpamPackage.Name.to_string name) (OpamPackage.to_string pkg);
 *         OpamPackage.Version.Set.empty in
 *     Version.Set.fold (fun v map ->
 *       let nvsetmap = Name.Map.singleton name (Version.Set.singleton v) in
 *       if OpamfuFormula.(could_satisfy nvsetmap (of_opam_formula f))
 *       then add_version map (create name v) pkg
 *       else map
 *           ) versions map
 *   ) in
 *   let revdeps = Map.fold (fun pkg f deps ->
 *     add_satisfiers pkg f (depnames_of_formula f) deps
 *   ) formulas Map.empty in
 *   Map.map (Name.Map.mapi (fun name subset ->
 *     OpamfuFormula.dnf_of_version_subset (Name.Map.find name versions) subset
 *   )) revdeps
 * 
 * let mk_universe_info preds index repos pkg_idx opams =
 *   let pkg_idx = remove_base_packages pkg_idx in
 *   let versions = versions pkg_idx in
 *   let max_versions = max_versions versions in
 *   let max_packages = max_packages max_versions in
 *   let depends =
 *     OpamPackage.Map.map (fun opam ->
 *         OpamTypesBase.filter_deps
 *           ~build:true ~test:true ~doc:true ~dev:false
 *           (OpamFile.OPAM.depends opam))
 *       opams in
 *   let rev_depends = reverse_deps depends pkg_idx versions in
 *   let depopts =
 *     OpamPackage.Map.map (fun opam ->
 *         OpamTypesBase.filter_deps
 *           ~build:true ~test:true ~doc:true ~dev:false
 *           (OpamFile.OPAM.depopts opam))
 *       opams in
 *   let rev_depopts = reverse_deps depopts pkg_idx versions in
 *   let pkgs_dates = dates repos pkg_idx in
 *   let pkgs_infos = infos repos pkgs_dates pkg_idx in
 *   { repos; preds; index; versions; pkg_idx; max_versions; max_packages;
 *     rev_depends; rev_depopts; pkgs_infos; pkgs_opams=opams; pkgs_dates }
 * 
 * let pred_sep = ':'
 * let repository_ns_sep = ':'
 * 
 * let repository_of_string s = s
 * let string_of_repository = fun s -> s
 * 
 * (\* Generate a universe from a stack of repositories *\)
 * let of_repositories ?(preds=[]) index repo_stack =
 *   OpamStateConfig.update ~root_dir:OpamStateConfig.(default.root_dir) ();
 *   let repos,_ = List.fold_left
 *     (fun (rmap,repo_priority) repo ->
 *       let repo_name = OpamRepositoryName.of_string repo in
 *       RepoMap.add repo_name
 *         { repo_name;
 *           repo_root = OpamFilename.Dir.of_string repo;
 *           repo_address  = ("<none>", None);
 *           repo_kind     = `local;
 *           repo_priority } rmap,
 *       repo_priority - 1
 *     ) (RepoMap.empty,256) repo_stack
 *   in
 *   let pkg_idx = OpamRepository.package_index repos in
 *   let packages = OpamPackage.Set.of_list (OpamPackage.Map.keys pkg_idx) in
 *   let opams = OpamPackage.Set.fold (fun nv map ->
 *     try
 *       let repo, prefix = OpamPackage.Map.find nv pkg_idx in
 *       let repo = OpamRepositoryName.Map.find repo repos in
 *       let file = OpamRepositoryPath.opam repo prefix nv in
 *       let opam = OpamFile.OPAM.read file in
 *       OpamPackage.Map.add nv opam map
 *     with
 *     | Not_found ->
 *       Printf.printf "Cannot find an OPAM file for %s, skipping.\n"
 *         (OpamPackage.to_string nv);
 *       map
 *     | Parsing.Parse_error | OpamSystem.Internal_error _ ->
 *       Printf.printf "Errors while parsing %s OPAM file, skipping.\n"
 *         (OpamPackage.to_string nv);
 *       map
 *   ) packages OpamPackage.Map.empty
 *   in
 *   let universe = opam_universe_of_packages_and_opams packages opams in
 *   let dep_closure = OpamSolver.dependencies
 *     ~build:true ~depopts:(List.mem [Depopt] preds) ~installed:false universe
 *     (OpamPackage.Set.filter
 *        (Pkg.are_preds_satisfied opams pkg_idx preds) packages)
 *   in
 *   let packages = OpamPackage.Set.of_list dep_closure in
 *   let pkg_idx = OpamPackage.Map.filter
 *     (fun k _ -> OpamPackage.Set.mem k packages) pkg_idx in
 *   mk_universe_info preds index repos pkg_idx opams
 * 
 * let map f u = 
 *   { u with pkgs_infos=OpamPackage.Map.map (fun x ->
 *     { x with descr = f x.descr }
 *   ) u.pkgs_infos} *)
