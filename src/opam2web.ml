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

open Cmdliner
open Cow.Html
open O2wTypes
open OpamTypes

(* Options *)
type options = {
  out_dir: string;
  files_dir: string;
  content_dir: string;
  logfiles: filename list;
  repositories: OpamfUniverse.repository list;
}

let version = Version.string

let packages_prefix = O2wHome.packages_prefix

let include_files (path: string) files_path : unit =
  let subpathes = ["doc"; packages_prefix] in
  let pathes =
    if String.length path > 0 then
      path :: List.map (fun p -> Printf.sprintf "%s/%s" path p) subpathes
    else
      subpathes
  in
  (* Check if output directory exists, create it if it doesn't *)
  List.iter (fun dir -> OpamFilename.mkdir (OpamFilename.Dir.of_string dir)) pathes

(* Generate a whole static website using the given repository stack *)
let make_website user_options universe =
  Printf.printf "++ Building the new stats from %s.\n%!"
    (OpamMisc.string_of_list OpamFilename.prettify user_options.logfiles);
  let statistics = O2wStatistics.statistics_set user_options.logfiles in
  let content_dir = user_options.content_dir in
  Printf.printf "++ Building the package pages.\n%!";
  let pages = O2wUniverse.to_pages ~statistics universe in
  Printf.printf "++ Building the documentation pages.\n%!";
  let menu_of_doc = O2wDocumentation.to_menu ~content_dir in
  let criteria = ["name"; "popularity"; "date"] in
  let criteria_nostats = ["name"; "date"] in
  let sortby_links = match statistics with
    | None   ->
      O2wUniverse.sortby_links ~links:criteria_nostats ~default:"name"
    | Some _ ->
      O2wUniverse.sortby_links ~links:criteria ~default:"name" in
  let popularity =
    match statistics with
    | None   -> OpamPackage.Name.Map.empty
    | Some s -> OpamfUniverse.(O2wStatistics.aggregate_package_popularity
                                 s.month_stats.pkg_stats universe.pkg_idx) in
  let to_html = O2wUniverse.to_html ~content_dir ~sortby_links ~popularity in
  Printf.printf "++ Building the package indexes.\n%!";
  let package_links =
    let compare_pkg =
      O2wPackage.compare_date ~reverse:true universe.OpamfUniverse.pkgs_dates
    in
    let date = {
      menu_link = { text="Packages"; href=packages_prefix^"/index-date.html" };
      menu_item = No_menu (1, to_html ~active:"date" ~compare_pkg universe);
    } in
    match statistics with
    | None -> [ date ]
    | Some s ->
      let compare_pkg = O2wPackage.compare_popularity ~reverse:true popularity in
      let popularity = {
        menu_link = { text="Packages";
                      href=packages_prefix^"/index-popularity.html" };
        menu_item = No_menu (1, to_html ~active:"popularity" ~compare_pkg universe);
      } in
      [ popularity; date ]
  in
  include_files user_options.out_dir user_options.files_dir;
  let about_page =
    let filename = Printf.sprintf "%s/doc/About.md" content_dir in
    try
      let filename = OpamFilename.of_string filename in
      let contents = OpamFilename.read filename in
      let contents = Cow.Markdown.of_string contents in
      <:html<
        <div class="container">
        $contents$
        </div>
      >>
    with _ ->
      OpamGlobals.warning "%s is not available." filename;
      <:html< >> in
  let home_index = O2wHome.to_html
    ~content_dir ~statistics ~popularity universe in
  let package_index =
    to_html ~active:"name" ~compare_pkg:O2wPackage.compare_alphanum universe in
  let doc_menu = menu_of_doc ~pages:O2wGlobals.documentation_pages in
  O2wTemplate.generate
    ~content_dir ~out_dir:user_options.out_dir
    ([
      { menu_link = { text="Home"; href="/" };
        menu_item = Internal (0, home_index) };

      { menu_link = { text="Packages"; href=packages_prefix^"/" };
        menu_item = Internal (1, package_index) };

      { menu_link = { text="Documentation"; href="doc/" };
        menu_item = Submenu doc_menu; };

      { menu_link = { text="About"; href="about.html" };
        menu_item = Internal (0, Template.serialize about_page) };

    ] @ package_links)
    pages;
  match statistics with
  | None   -> ()
  | Some s ->
    let popularity = s.month_stats.pkg_stats in
    O2wStatistics.to_csv popularity "stats.csv";
    O2wStatistics.to_json popularity "stats.json"

let normalize d =
  let len = String.length d in
  if len <> 0 && d.[len - 1] <> '/' then
    d ^ "/"
  else
    d

let log_files = Arg.(
  value & opt_all string [] & info ["s"; "statistics"]
    ~docv:"LOG_FILE"
    ~doc:"An Apache server log file containing download statistics of packages")

let out_dir = Arg.(
  value & opt string (Sys.getcwd ()) & info ["o"; "output"]
    ~docv:"OUT_DIR"
    ~doc:"The directory where to write the generated HTML files")

let content_dir = Arg.(
  value & opt string "content" & info ["c"; "content"]
    ~docv:"CONTENT_DIR"
    ~doc:"The directory where to find documentation to include")

let build logfiles out_dir content_dir repositories preds index =
  let () = List.iter (function
    | `path path -> Printf.printf "=== Repository: %s ===\n%!" path;
    | `local local -> Printf.printf "=== Repository: %s [opam] ===\n%!" local;
    | `opam -> Printf.printf "=== Universe: current opam universe ===\n%!";
  ) repositories in
  let out_dir = normalize out_dir in
  let logfiles = List.map OpamFilename.of_string logfiles in
  let user_options = {
    out_dir;
    files_dir = "";
    content_dir;
    logfiles;
    repositories;
  } in
  make_website user_options
    (O2wUniverse.of_repositories ~preds index repositories)

let default_cmd =
  let doc = "generate a web site from an opam universe" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,opam2web) generates a web site from an opam universe.";
    `S "BUGS";
    `P "Report bugs on the web at <https://github.com/ocaml/opam2web>.";
  ] in
  Term.(pure build $ log_files $ out_dir $ content_dir
          $ OpamfuCli.repositories $ OpamfuCli.pred $ OpamfuCli.index),
  Term.info "opam2web" ~version ~doc ~man

;;

match Term.eval default_cmd with
| `Error _ -> exit 1
| _ -> exit 0
