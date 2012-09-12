open Cow
open Cow.Html

open O2w_common

exception Unknown_repository of string

(* Flag to check if at least one operation is triggered by arguments *)
let no_operation = ref true

(* Options *)
type options = {
  mutable out_dir: string;
  mutable files_dir: string;
}

let user_options: options = {
  out_dir = "www";
  files_dir = "";
}

let set_out_dir (dir: string) =
  user_options.out_dir <- dir

let set_files_dir (dir: string) =
  user_options.files_dir <- dir

let include_files (path: string) files_path : unit =
  let subpathes = ["pkg"] in
  let subpathes =
    List.map (fun p -> Printf.sprintf "%s/%s" path p) subpathes
  in
  (* Check if output directory exists, create it if it doesn't *)
  List.iter (fun p ->
      let dir = Types.Dirname.of_string p in
      if not (Types.Dirname.exists dir) then
        begin
          Types.Dirname.mkdir dir;
          Printf.printf "Directory '%s' created\n%!" p
        end
      else
        (Printf.printf "Directory '%s' already exists\n%!" p))
    (path :: subpathes)
  (* ; *)
  (* Include static content *)
  (* FIXME: broken, 'copy' function fails *)
  (* if String.length files_path > 0 then *)
  (*   let files_dir = Types.Dirname.of_string files_path in *)
  (*   Types.Dirname.copy files_dir dir *)

(* Generate a whole static website using the given repository *)
let make_website (repository: Path.R.t): unit =
  let packages = Repository.to_links repository in
  include_files user_options.out_dir user_options.files_dir;
  Template.generate ~out_dir: user_options.out_dir ([
    { text="Home"; href="index.html" }, Internal (0, Home.static_html);
    { text="Packages"; href="pkg/index.html" },
        Internal (1, (Repository.to_html repository));
    { text="Documentation";
        href="https://github.com/OCamlPro/opam/wiki/Tutorial" },
        External;
  ], packages)

(* Generate a website from the current working directory, assuming that it's an 
   OPAM repository *)
let website_of_cwd () =
  no_operation := false;
  Printf.printf "=== Repository: current working directory ===\n%!";
  make_website (Path.R.cwd ())

(* Generate a website from the given directory, assuming that it's an OPAM 
   repository *)
let website_of_path dirname =
  no_operation := false;
  Printf.printf "=== Repository: %s ===\n%!" dirname;
  make_website (Repository.of_path dirname)

(* Generate a website from the given repository name, trying to find it in local 
   OPAM installation *)
let website_of_opam repo_name =
  no_operation := false;
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
      Globals.error "Opam repository '%s' not found!" repo_name

(* Command-line arguments *)
let specs = [
  ("-d", Arg.String website_of_path, "");
  ("--directory", Arg.String website_of_path,
    "Generate a website from the opam repository in 'directory'");

  ("-l", Arg.String website_of_opam, "");
  ("--local", Arg.String website_of_opam,
    "Generate a website from an opam repository in the local opam installation");

  ("-o", Arg.String set_out_dir, "");
  ("--output", Arg.String set_out_dir,
    "The directory where to write the generated HTML files");

  (* ("-i", Arg.String set_files_dir, ""); *)
  (* ("--include", Arg.String set_files_dir, *)
  (*   "Copy this directory content to the output directory"); *)
]

(* Main *)
let () =
  (* Anonymous arguments are interpreted as directories where to find 
     repositories *)
  Arg.parse specs website_of_path
      (Printf.sprintf "%s [options]* [repository_name]*" Sys.argv.(0));
  if !no_operation then
    (* If the arguments didn't trigger any operation, try to interpret the 
       current directory as a repository and make the website out of it *)
    website_of_cwd ()

