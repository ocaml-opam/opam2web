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

open Cmdliner
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
  out_dir: string;
  files_dir: string;
  content_dir: string;
  logfiles: filename list;
  operations: opam2web_operation list;
  href_prefix: string;
  preds: pred list list;
}

let version = Version.string

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
let make_website user_options repo_info =
  Printf.printf "++ Building the new stats from %s.\n%!"
    (OpamMisc.string_of_list OpamFilename.prettify user_options.logfiles);
  let statistics = O2wStatistics.statistics_set user_options.logfiles in
  let href_prefix = user_options.href_prefix in
  let content_dir = user_options.content_dir in
  let preds = user_options.preds in
  Printf.printf "++ Building the package pages.\n%!";
  let pages = O2wRepository.to_pages ~href_prefix ~statistics repo_info in
  Printf.printf "++ Building the documentation pages.\n%!";
  let menu_of_doc = O2wDocumentation.to_menu ~content_dir in
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
  let to_html = O2wRepository.to_html
    ~href_prefix ~content_dir ~sortby_links ~preds ~popularity in
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
    let filename = Printf.sprintf "%s/doc/About.md" content_dir in
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
  let home_index = O2wHome.to_html
    ~href_prefix ~statistics ~preds ~popularity repo_info in
  let package_index =
    to_html ~active:"name" ~compare_pkg:O2wPackage.compare_alphanum repo_info in
  let doc_menu = menu_of_doc ~pages:O2wGlobals.documentation_pages in
  O2wTemplate.generate
    ~content_dir ~out_dir:user_options.out_dir
    ([
      { menu_link = { text="Home"; href="/" };
        menu_item = Internal (0, Template.serialize home_index) };

      { menu_link = { text="Packages"; href="pkg/" };
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

(* Generate a website from the current working directory, assuming that it's an
   OPAM repository *)
let website_of_cwd user_options =
  Printf.printf "=== Repository: current working directory ===\n%!";
  make_website user_options (O2wRepository.of_path (OpamFilename.cwd ()))

(* Generate a website from the given directory, assuming that it's an OPAM
   repository *)
let website_of_path user_options dirname =
  let dirname = OpamFilename.Dir.of_string dirname in
  Printf.printf "=== Repository: %s ===\n%!" (OpamFilename.Dir.to_string dirname);
  make_website user_options (O2wRepository.of_path dirname)

(* Generate a website from the given repository name, trying to find it in local
   OPAM installation *)
let website_of_opam user_options repo_name =
  Printf.printf "=== Repository: %s [opam] ===\n%!" repo_name;
  let load_repo r =
    try O2wRepository.of_opam (OpamRepositoryName.of_string r)
    with Not_found -> raise (Unknown_repository r)
  in
  try make_website user_options (load_repo repo_name)
  with Unknown_repository repo_name ->
    OpamGlobals.error "Opam repository '%s' not found!" repo_name

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
    ~doc:"The directory where to find OPAM documentation to include")

let opam_path = Arg.(
  value & opt_all string [] & info ["d"; "directory"]
    ~docv:"OPAM_REPO_PATH"
    ~doc:"Generate a website from the opam repository in 'OPAM_REPO_PATH'")

let opam_remote = Arg.(
  value & opt_all string [] & info ["remote"]
    ~docv:"OPAM_REPO_REMOTE"
    ~doc:"Generate a website from an opam remote in the local opam installation")

let href_prefix = Arg.(
  value & opt string "/" & info ["prefix"]
    ~docv:"HREF_PREFIX"
    ~doc:"The hyperlink prefix")

let pred = Arg.(
  value & opt_all string [] & info ["where"]
    ~docv:"WHERE_OR"
    ~doc:"Satisfaction of all of the predicates in any comma-separated list implies inclusion")

let build logfiles out_dir content_dir opam_path opam_remote href_prefix preds =
  let preds = List.rev_map (fun pred ->
    List.rev_map (fun pred ->
      match Re_str.(bounded_split (regexp_string ":") pred 2) with
      | ["tag";tag] -> Tag tag
      | [] -> failwith "filter predicate empty"
      | p::_ -> failwith ("unknown predicate "^p)
    ) Re_str.(split (regexp_string ",") pred)
  ) preds in
  let href_prefix = normalize href_prefix in
  let out_dir = normalize out_dir in
  let logfiles = List.map OpamFilename.of_string logfiles in
  let user_options = {
    out_dir;
    files_dir = "";
    content_dir;
    logfiles;
    operations = List.rev_append
      (List.rev_map (fun o -> Website_of_path o) opam_path)
      (List.rev_map (fun o -> Website_of_opam o) opam_remote);
    href_prefix;
    preds;
  } in
  if List.length user_options.operations = 0 then
    (* If the arguments didn't trigger any operation, try to interpret the
       current directory as a repository and make the website out of it *)
    website_of_cwd user_options
  else
    let exec_operation = function
      | Website_of_path p -> website_of_path user_options p
      | Website_of_opam r -> website_of_opam user_options r
    in
    List.iter exec_operation user_options.operations

let default_cmd =
  let doc = "generate a web site from an opam repository and extensions" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,opam2web) generates a web site from an opam repository and repository extensions.";
    `S "BUGS";
    `P "Report bugs on the web at <https://github.com/OCamlPro/opam2web>.";
  ] in
  Term.(pure build $ log_files $ out_dir $ content_dir
          $ opam_path $ opam_remote $ href_prefix $ pred),
  Term.info "opam2web" ~version ~doc ~man

;;

match Term.eval default_cmd with
| `Error _ -> exit 1
| _ -> exit 0
