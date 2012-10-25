open Cow
open Cow.Html

open O2w_common

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
let make_website (repository: OpamPath.Repository.r): unit =
  if List.length user_options.logfiles = 0 then
    user_options.logfiles <- ["access.log"];
  let statistics = Statistics.basic_stats_of_logfiles user_options.logfiles in
  let packages = Repository.to_links repository statistics in
  let links_of_doc = Documentation.to_links user_options.content_dir in
  let criteria = ["name"; "popularity"] in
  let sortby_links = match statistics with
    | None -> fun _ -> []
    | Some _ ->  Repository.sortby_links criteria "name"
  in
  let criteria_links = match statistics with
    | None -> []
    | Some s ->
      [
        { text="Packages"; href="pkg/index-popularity.html" },
            No_menu (1, (Repository.to_html (sortby_links, "popularity",
                  Package.compare_popularity ~reverse: true s.pkg_stats)
                repository statistics));
      ]
  in
  include_files user_options.out_dir user_options.files_dir;
  Template.generate ~out_dir: user_options.out_dir ([
    { text="Home"; href="index.html" }, Internal (0, Home.static_html);
    { text="Packages"; href="pkg/index.html" },
        Internal (1, (Repository.to_html
            (sortby_links, "name", Package.compare_alphanum)
            repository statistics));
    { text="Documentation"; href="doc/index.html" },
        Submenu (links_of_doc documentation_pages);
  ] @ criteria_links, packages)

(* Generate a website from the current working directory, assuming that it's an 
   OPAM repository *)
let website_of_cwd () =
  Printf.printf "=== Repository: current working directory ===\n%!";
  make_website (OpamPath.Repository.raw (OpamFilename.cwd ()))

(* Generate a website from the given directory, assuming that it's an OPAM 
   repository *)
let website_of_path dirname =
  Printf.printf "=== Repository: %s ===\n%!" dirname;
  make_website (Repository.of_path dirname)

(* Generate a website from the given repository name, trying to find it in local 
   OPAM installation *)
let website_of_opam repo_name =
  Printf.printf "=== Repository: %s [opam] ===\n%!" repo_name;
  let load_repo r =
    try
      Repository.of_opam r
    with
      Not_found -> raise (Unknown_repository r)
  in
  try
    make_website (load_repo repo_name)
  with
    Unknown_repository repo_name ->
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

