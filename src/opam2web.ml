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
open Cow
open O2wTypes
open OpamTypes
open OpamStateTypes

let ( ++ ) = Html.( ++ )

(* Options *)
type options = {
  out_dir: string;
  files_dir: string;
  content_dir: string;
  logfiles: filename list;
  repositories: string list;
  root_uri: Uri.t;
  blog_source_uri: string;
  gen_dcache: bool;
}

let version = Version.string

let packages_prefix = O2wHome.packages_prefix

let include_files (path: string) files_path : unit =
  let subpathes = ["doc"; "blog"; packages_prefix] in
  let pathes =
    if String.length path > 0 then
      path :: List.map (fun p -> Printf.sprintf "%s/%s" path p) subpathes
    else
      subpathes
  in
  (* Check if output directory exists, create it if it doesn't *)
  List.iter (fun dir -> OpamFilename.mkdir (OpamFilename.Dir.of_string dir)) pathes

(* Generate a whole static website using the given repository stack *)
let make_website user_options =
  if user_options.gen_dcache then
    (Printf.printf "Generating dependencies cache\n";
     O2wStatistics.generate_dependencies_cache
       (List.map OpamFilename.Dir.of_string user_options.repositories);
     Printf.printf"...done\n")
  else
  (Printf.printf "++ Building the new stats from %s.\n%!"
    (OpamStd.List.to_string OpamFilename.prettify user_options.logfiles);
  let statistics =
    O2wStatistics.statistics_set user_options.logfiles
      (List.map OpamFilename.Dir.of_string user_options.repositories)
  in
  let content_dir = user_options.content_dir in
  let universe =
    O2wUniverse.load statistics
      (List.map OpamFilename.Dir.of_string user_options.repositories)
  in
  Printf.printf "++ Building the package pages.\n%!";
  let pages =
    O2wUniverse.to_pages ~prefix:packages_prefix universe
  in
  Printf.printf "++ Building the documentation pages.\n%!";
  let menu_of_doc () = O2wDocumentation.to_menu ~content_dir in
  Printf.printf "++ Building the blog.\n%!";
  let blog_pages =
    let files =
      OpamFilename.files OpamFilename.Op.(
          OpamFilename.Dir.of_string content_dir / "blog"
        ) in
    List.map (fun f -> OpamFilename.Base.to_string (OpamFilename.basename f))
      files
  in
  let blog_entries = O2wBlog.get_entries ~content_dir ~pages:blog_pages in
  let news = (O2wBlog.make_news blog_entries) in
  let blog_latest, blog_links =
    O2wBlog.make_menu ~srcurl:user_options.blog_source_uri blog_entries
  in
  let blog_feed = O2wBlog.make_feed ~root:user_options.root_uri blog_entries in
  let criteria = ["name"; "popularity"; "date"] in
  let criteria_nostats = ["name"; "date"] in
  let sortby_links = match statistics with
    | None   ->
      O2wUniverse.sortby_links ~links:criteria_nostats ~default:"name"
    | Some _ ->
      O2wUniverse.sortby_links ~links:criteria ~default:"name" in
  let to_html =
    O2wUniverse.to_html ~content_dir ~sortby_links universe
  in
  Printf.printf "++ Building the package indexes.\n%!";
  let package_links =
    let compare_pkg =
      O2wPackage.compare_date ~reverse:true universe.dates
    in
    let date = {
      menu_source = content_dir;
      menu_link = Uri.make ~path:(packages_prefix^"/index-date.html") ();
      menu_link_text = "Packages";
      menu_link_html = Html.string "Packages";
      menu_item = No_menu (1, to_html ~active:"date" ~compare_pkg);
      menu_srcurl = None;
    } in
    match universe.name_popularity with
    | None -> [ date ]
    | Some s ->
      let compare_pkg =
        O2wPackage.compare_popularity ~reverse:true s
      in
      let popularity = {
        menu_source = content_dir;
        menu_link = Uri.make ~path:(packages_prefix ^ "/index-popularity.html") ();
        menu_link_text = "Packages";
        menu_link_html = Html.string "Packages";
        menu_item = No_menu (1, to_html ~active:"popularity" ~compare_pkg);
        menu_srcurl = None;
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
      Html.div ~cls:"container" contents
    with _ ->
      OpamConsole.warning "%s is not available." filename;
      Html.empty in
  let doc_menu = menu_of_doc () in
  let home_index = O2wHome.to_html ~content_dir ~statistics ~news universe in
  let package_index =
    to_html ~active:"name" ~compare_pkg:O2wPackage.compare_alphanum in
  let opam_title =
    Html.img (Uri.make ~path:("/ext/img/favicon.png") ()) ++
    Html.span ~cls:"opam-title" (Html.string " opam")
  in
  O2wTemplate.generate
    ~content_dir ~out_dir:user_options.out_dir
    ([
      { menu_source = content_dir;
        menu_link = Uri.make ~path:"." ();
        menu_link_text = "opam";
        menu_link_html = opam_title;
        menu_item = Internal (0, home_index);
        menu_srcurl = None; };

      { menu_source = content_dir;
        menu_link = Uri.make ~path:(packages_prefix^"/") ();
        menu_link_text = "Packages";
        menu_link_html = Html.string "Packages";
        menu_item = Internal (1, package_index);
        menu_srcurl = None; };

      { menu_source = content_dir^"/doc";
        menu_link = Uri.make ~path:"doc/" ();
        menu_link_text = "Documentation";
        menu_link_html = Html.string "Documentation";
        menu_item = Submenu doc_menu;
        menu_srcurl = None; };

      { menu_source = content_dir;
        menu_link = Uri.make ~path:"about.html" ();
        menu_link_text = "About opam";
        menu_link_html = Html.string "About opam";
        menu_item = Internal (0, Template.serialize about_page);
        menu_srcurl = None; };

     ]
     @ blog_latest
     @ package_links
     @ blog_links)
    pages;
  OpamFilename.write
    (OpamFilename.Op.(OpamFilename.Dir.of_string user_options.out_dir / "blog" // "feed.xml"))
    (Cow.Xml.to_string blog_feed);
  OpamFilename.write
    (OpamFilename.Op.(OpamFilename.Dir.of_string user_options.out_dir / "blog" // "index.html"))
    (Cow.Xml.to_string (O2wBlog.make_redirect ~root:user_options.root_uri blog_entries));
  match statistics with
  | None   -> ()
  | Some s ->
    let popularity = s.month_stats.pkg_stats in
    O2wStatistics.to_csv popularity "stats.csv";
    O2wStatistics.to_json popularity "stats.json")

let normalize d =
  let len = String.length d in
  if len <> 0 && d.[len - 1] <> '/' then
    d ^ "/"
  else
    d

let generate_dcache = Arg.(
  value & flag & info ["generate-dcache"]
  ~doc:"Generate packages dependencies cache and quit")

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

let root_uri = Arg.(
    value & opt string (Sys.getcwd () ^ "/") & info ["r"; "root"]
      ~docv:"URI"
      ~doc:"The root URI from which we'll be serving pages (e.g. 'http://opam.ocaml.org/')")

let blog_source_uri = Arg.(
    value & opt string "https://github.com/ocaml/platform-blog/blob/master" & info ["blog"]
      ~docv:"URI"
      ~doc:"The base address to point to blog source files")

let build logfiles out_dir content_dir repositories root_uri blog_source_uri
    gen_dcache =
  List.iter (Printf.printf "=== Repository: %s ===\n%!") repositories;
  let out_dir = normalize out_dir in
  let logfiles = List.map OpamFilename.of_string logfiles in
  let root_uri = Uri.of_string root_uri in
  let user_options = {
    out_dir;
    files_dir = "";
    content_dir;
    logfiles;
    repositories;
    root_uri;
    blog_source_uri;
    gen_dcache;
  } in
  make_website user_options

let default_cmd =
  let doc = "generate a web site from an opam universe" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,opam2web) generates a web site from an opam universe.";
    `S "BUGS";
    `P "Report bugs on the web at <https://github.com/ocaml/opam2web>.";
  ] in
  let repositories_arg =
    Arg.(value & pos_all dir ["."] & info []
           ~docv:"REPOSITORY"
           ~doc:"Directories containing the repositories to consider")
  in
  Term.(pure build $ log_files $ out_dir $ content_dir
          $ repositories_arg $ root_uri $ blog_source_uri $ generate_dcache),
  Term.info "opam2web" ~version ~doc ~man

let () =
  OpamFormatConfig.init ();
  OpamStd.Config.init ();
  OpamRepositoryConfig.init ();
  OpamSolverConfig.init ();
  OpamStateConfig.init ();
  match Term.eval default_cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
