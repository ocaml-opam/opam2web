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

open Cow
open Cow.Html
open O2wTypes

exception Unknown_repository of string

type opam2web_operation =
  | Website_of_path of string
  | Website_of_opam of string

(* Options *)
type options = {
  mutable out_dir: string;
  mutable files_dir: string;
  mutable content_dir: string;
  mutable logfiles: string list;
  mutable operations: opam2web_operation list;
}

let user_options: options = {
  out_dir = "";
  files_dir = "";
  content_dir = "content";
  logfiles = [];
  operations = [];
}

let set_out_dir (dir: string) =
  user_options.out_dir <- dir

let set_files_dir (dir: string) =
  user_options.files_dir <- dir

let set_content_dir (dir: string) =
  user_options.content_dir <- dir

let add_logfile (filename: string) =
  user_options.logfiles <- filename :: user_options.logfiles

let add_website_path (path: string) =
  user_options.operations <- Website_of_path path :: user_options.operations

let add_website_opam (repo: string) =
  user_options.operations <- Website_of_opam repo :: user_options.operations

let include_files (path: string) files_path : unit =
  let subpathes = ["doc"; "pkg"] in
  let pathes =
    if String.length path > 0 then
      path :: List.map (fun p -> Printf.sprintf "%s/%s" path p) subpathes
    else
      subpathes
  in
  (* Check if output directory exists, create it if it doesn't *)
  List.iter (fun p ->
      let dir = OpamFilename.Dir.of_string p in
      if not (OpamFilename.exists_dir dir) then
        begin
          OpamFilename.mkdir dir;
          Printf.printf "Directory '%s' created\n%!" p
        end
      else
        (Printf.printf "Directory '%s' already exists\n%!" p))
    pathes
  (* ; *)
  (* Include static content *)
  (* FIXME: broken, 'copy' function fails *)
  (* if String.length files_path > 0 then *)
  (*   let dir = Types.Dirname.of_string path in *)
  (*   let files_dir = Types.Dirname.of_string files_path in *)
  (*   Types.Dirname.copy files_dir dir *)

(* Generate a whole static website using the given repository *)
let make_website repository =
  if List.length user_options.logfiles = 0 then
    user_options.logfiles <- ["access.log"];
  let statistics = O2wStatistics.basic_statistics_set user_options.logfiles in
  let dates = O2wRepository.get_dated_packages repository in
  let pages = O2wRepository.to_pages ~statistics ~dates repository in
  let menu_of_doc = O2wDocumentation.to_menu ~content_dir:user_options.content_dir in
  let criteria = ["name"; "popularity"; "date"] in
  let criteria_nostats = ["name"; "date"] in
  let sortby_links = match statistics with
    | None   -> O2wRepository.sortby_links ~links:criteria_nostats ~default:"name"
    | Some _ -> O2wRepository.sortby_links ~links:criteria ~default:"name" in
  let popularity =
    match statistics with
    | None   -> OpamPackage.Name.Map.empty
    | Some s ->
      let packages = OpamPackage.Set.of_list (OpamPackage.Map.keys dates) in
      O2wStatistics.aggregate_package_popularity s.alltime_stats.pkg_stats packages in
  let to_html = O2wRepository.to_html ~sortby_links ~dates ~popularity in
  let criteria_links =
    let compare_pkg = O2wPackage.compare_date ~reverse:true dates in
    let date = {
      menu_link = { text="Packages"; href="pkg/index-date.html" };
      menu_item = No_menu (1, to_html ~active:"date" ~compare_pkg repository);
    } in
    match statistics with
    | None -> [ date ]
    | Some s ->
      let compare_pkg = O2wPackage.compare_popularity ~reverse:true popularity in
      let popularity = {
        menu_link = { text="Packages"; href="pkg/index-popularity.html" };
        menu_item = No_menu (1, to_html ~active:"popularity" ~compare_pkg repository);
      } in
      [ popularity; date ]
  in
  include_files user_options.out_dir user_options.files_dir;
  let about_page =
    <:html<
      <div class="container">
      <div class="page-header"><h2>OPAM</h2></div>

      <p>OPAM has been created and is maintained by
      <a href="http://www.ocamlpro.com">OCamlPro</a>.</p>

      <p>Bug reports and feature requests for the OPAM tool should be
      reported <a href="http://github.com/OCamlPro/opam/issues">here</a>.</p>

      <p>Packaging issues or requests for a new package should be reported
      <a href="hhttp://github.com/OCamlPro/opam-repository/issues">here</a>.</p>

      <p>General queries both the tool and the packages should be addressed
      <a href="http://lists.ocaml.org/listinfo/platform">here</a> and for
      for the tool and its evolution
      <a href="http://lists.ocaml.org/listinfo/opam-devel">here</a>.</p>

      Standard commercial terms and support, as well as training and consulting
      services on OPAM are provided by <a href="mailto:contact@ocamlpro.com">OCamlPro</a>.
      </div>

    >>
  in
  O2wTemplate.generate ~out_dir: user_options.out_dir
    ([
      { menu_link = { text="Home"; href="index.html" };
        menu_item = Internal
          (0, O2wHome.to_html ~statistics ~dates ~popularity repository ) };

      { menu_link = { text="Packages"; href="pkg/index.html" };
        menu_item = Internal (1, to_html ~active:"name" ~compare_pkg:O2wPackage.compare_alphanum repository) };

      { menu_link = { text="Documentation"; href="doc/index.html" };
        menu_item = Submenu (menu_of_doc ~pages:O2wGlobals.documentation_pages); };

      { menu_link = { text="About"; href="about.html" };
        menu_item = Internal (0, about_page) };

     ] @ criteria_links)
    pages;
  match statistics with
  | None   -> ()
  | Some s ->
    let popularity = s.alltime_stats.pkg_stats in
    O2wStatistics.to_csv popularity "stats.csv";
    O2wStatistics.to_json popularity "stats.json"


(* Generate a website from the current working directory, assuming that it's an
   OPAM repository *)
let website_of_cwd () =
  Printf.printf "=== Repository: current working directory ===\n%!";
  make_website (OpamFilename.cwd ())

(* Generate a website from the given directory, assuming that it's an OPAM
   repository *)
let website_of_path dirname =
  Printf.printf "=== Repository: %s ===\n%!" dirname;
  make_website (O2wRepository.of_path dirname)

(* Generate a website from the given repository name, trying to find it in local
   OPAM installation *)
let website_of_opam repo_name =
  Printf.printf "=== Repository: %s [opam] ===\n%!" repo_name;
  let load_repo r =
    try O2wRepository.of_opam r
    with Not_found -> raise (Unknown_repository r)
  in
  try make_website (load_repo repo_name)
  with Unknown_repository repo_name ->
    OpamGlobals.error "Opam repository '%s' not found!" repo_name

(* Command-line arguments *)
let specs = [
  ("-s", Arg.String add_logfile, "");
  ("--statistics", Arg.String add_logfile,
    "An Apache server log file containing download statistics of packages");

  ("-o", Arg.String set_out_dir, "");
  ("--output", Arg.String set_out_dir,
    "The directory where to write the generated HTML files");

  ("-c", Arg.String set_content_dir, "");
  ("--content", Arg.String set_content_dir,
    "The directory where to find OPAM documentation to include");

  ("-d", Arg.String add_website_path, "");
  ("--directory", Arg.String add_website_path,
    "Generate a website from the opam repository in 'directory'");

  ("-l", Arg.String add_website_opam, "");
  ("--local", Arg.String add_website_opam,
    "Generate a website from an opam repository in the local opam installation");

  (* ("-i", Arg.String set_files_dir, ""); *)
  (* ("--include", Arg.String set_files_dir, *)
  (*   "Copy this directory content to the output directory"); *)

]

(* Main *)
let () =
  (* Anonymous arguments are interpreted as directories where to find
     repositories *)
  Arg.parse specs add_website_path
      (Printf.sprintf "%s [options]* [repository_name]*" Sys.argv.(0));
  if List.length user_options.operations = 0 then
    (* If the arguments didn't trigger any operation, try to interpret the
       current directory as a repository and make the website out of it *)
    website_of_cwd ()
  else
    let exec_operation = function
      | Website_of_path p -> website_of_path p
      | Website_of_opam r -> website_of_opam r
    in
    List.iter exec_operation user_options.operations

