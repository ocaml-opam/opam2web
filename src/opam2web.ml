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
open OpamTypes

exception Unknown_repository of string

type opam2web_operation =
  | Website_of_path of string
  | Website_of_opam of string

(* Options *)
type options = {
  mutable out_dir: string;
  mutable files_dir: string;
  mutable content_dir: string;
  mutable logfiles: filename list;
  mutable operations: opam2web_operation list;
  mutable href_prefix: string;
}

let user_options: options = {
  out_dir = "";
  files_dir = "";
  content_dir = "content";
  logfiles = [];
  operations = [];
  href_prefix = "";
}

let set_out_dir (dir: string) =
  user_options.out_dir <- dir

let set_files_dir (dir: string) =
  user_options.files_dir <- dir

let set_content_dir (dir: string) =
  user_options.content_dir <- dir

let add_logfile (filename: string) =
  user_options.logfiles <- (OpamFilename.of_string filename) :: user_options.logfiles

let add_website_path (path: string) =
  user_options.operations <- Website_of_path path :: user_options.operations

let add_website_opam (repo: string) =
  user_options.operations <- Website_of_opam repo :: user_options.operations

let set_href_prefix name =
  user_options.href_prefix <- name

let include_files (path: string) files_path : unit =
  let subpathes = ["doc"; "pkg"] in
  let pathes =
    if String.length path > 0 then
      path :: List.map (fun p -> Printf.sprintf "%s/%s" path p) subpathes
    else
      subpathes
  in
  (* Check if output directory exists, create it if it doesn't *)
  List.iter (fun dir -> OpamFilename.mkdir (OpamFilename.Dir.of_string dir)) pathes

(* Generate a whole static website using the given repository *)
let make_website repo_info =
  Printf.printf "++ Building the new stats from %s.\n%!"
    (OpamMisc.string_of_list OpamFilename.prettify user_options.logfiles);
  let statistics = O2wStatistics.statistics_set user_options.logfiles in
  let href_prefix = user_options.href_prefix in
  Printf.printf "++ Building the package pages.\n%!";
  let pages = O2wRepository.to_pages ~href_prefix ~statistics repo_info in
  Printf.printf "++ Building the documentation pages.\n%!";
  let menu_of_doc = O2wDocumentation.to_menu ~content_dir:user_options.content_dir in
  let criteria = ["name"; "popularity"; "date"] in
  let criteria_nostats = ["name"; "date"] in
  let sortby_links = match statistics with
    | None   ->
      O2wRepository.sortby_links ~href_prefix ~links:criteria_nostats ~default:"name"
    | Some _ ->
      O2wRepository.sortby_links ~href_prefix ~links:criteria ~default:"name" in
  let popularity =
    match statistics with
    | None   -> OpamPackage.Name.Map.empty
    | Some s -> O2wStatistics.aggregate_package_popularity
                  s.month_stats.pkg_stats repo_info.packages in
  let to_html = O2wRepository.to_html ~href_prefix ~sortby_links ~popularity in
  Printf.printf "++ Building the package indexes.\n%!";
  let package_links =
    let compare_pkg = O2wPackage.compare_date ~reverse:true repo_info.pkgs_dates in
    let date = {
      menu_link = { text="Packages"; href="pkg/index-date.html" };
      menu_item = No_menu (1, to_html ~active:"date" ~compare_pkg repo_info);
    } in
    match statistics with
    | None -> [ date ]
    | Some s ->
      let compare_pkg = O2wPackage.compare_popularity ~reverse:true popularity in
      let popularity = {
        menu_link = { text="Packages"; href="pkg/index-popularity.html" };
        menu_item = No_menu (1, to_html ~active:"popularity" ~compare_pkg repo_info);
      } in
      [ popularity; date ]
  in
  include_files user_options.out_dir user_options.files_dir;
  let about_page =
    let filename = Printf.sprintf "%s/doc/About.md" user_options.content_dir in
    try
      let filename = OpamFilename.of_string filename in
      let contents = OpamFilename.read filename in
      let contents = Cow.Markdown_github.of_string contents in
      let contents = Cow.Markdown.to_html contents in
      <:html<
        <div class="container">
        $contents$
        </div>
      >>
    with _ ->
      OpamGlobals.warning "%s is not available." filename;
      <:html< >> in
  let home_index = O2wHome.to_html ~href_prefix ~statistics ~popularity repo_info in
  let package_index =
    to_html ~active:"name" ~compare_pkg:O2wPackage.compare_alphanum repo_info in
  let doc_menu = menu_of_doc ~pages:O2wGlobals.documentation_pages in
  O2wTemplate.generate ~out_dir:user_options.out_dir
    ([
      { menu_link = { text="Home"; href="index.html" };
        menu_item = Internal (0, home_index) };

      { menu_link = { text="Packages"; href="pkg/index.html" };
        menu_item = Internal (1, package_index) };

      { menu_link = { text="Documentation"; href="doc/index.html" };
        menu_item = Submenu doc_menu; };

      { menu_link = { text="About"; href="about.html" };
        menu_item = Internal (0, about_page) };

    ] @ package_links)
    pages;
  match statistics with
  | None   -> ()
  | Some s ->
    let popularity = s.month_stats.pkg_stats in
    O2wStatistics.to_csv popularity "stats.csv";
    O2wStatistics.to_json popularity "stats.json"

(* Generate a website from the current working directory, assuming that it's an
   OPAM repository *)
let website_of_cwd () =
  Printf.printf "=== Repository: current working directory ===\n%!";
  make_website (O2wRepository.of_path (OpamFilename.cwd ()))

(* Generate a website from the given directory, assuming that it's an OPAM
   repository *)
let website_of_path dirname =
  let dirname = OpamFilename.Dir.of_string dirname in
  Printf.printf "=== Repository: %s ===\n%!" (OpamFilename.Dir.to_string dirname);
  make_website (O2wRepository.of_path dirname)

(* Generate a website from the given repository name, trying to find it in local
   OPAM installation *)
let website_of_opam repo_name =
  Printf.printf "=== Repository: %s [opam] ===\n%!" repo_name;
  let load_repo r =
    try O2wRepository.of_opam (OpamRepositoryName.of_string r)
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

  ("-p", Arg.String set_href_prefix, "");
  ("--prefix", Arg.String set_href_prefix,
   "Specify the prefix hyper-link (default is the contents of [--output])");
]

let normalize d =
  if d.[String.length d - 1] <> '/' then
    d ^ "/"
  else
    d

(* Main *)
let () =
  (* Anonymous arguments are interpreted as directories where to find
     repositories *)
  Arg.parse specs add_website_path
    (Printf.sprintf "%s [options]* [repository_name]*" Sys.argv.(0));

  if List.length user_options.logfiles = 0 then
    user_options.logfiles <- [OpamFilename.of_string "access.log"];
  if user_options.out_dir = "" then
    user_options.out_dir <- Sys.getcwd ();
  if user_options.href_prefix = "" then
    user_options.href_prefix <- user_options.out_dir;

  user_options.href_prefix <- normalize user_options.href_prefix;
  user_options.out_dir <- normalize user_options.out_dir;

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
