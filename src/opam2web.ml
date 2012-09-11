open Cow
open Cow.Html

open O2w_common

exception Unknown_repository of string

(* Flag to check if at least one operation is triggered by arguments *)
let no_operation = ref true

(* Options *)
type options = {
  mutable out_dir: string;
}

let user_options: options = {
  out_dir = "www";
}

let set_out_dir (dir: string) =
  user_options.out_dir <- dir

(* Generate a whole static website using the given repository *)
let make_website (repository: Path.R.t): unit =
  (* TODO: create out_dir if it doesn't exist, warn and exit if it's a file *)
  let packages = Repository.to_links repository in
  Template.generate ~out_dir:user_options.out_dir ([
    { text="Home"; href="index.html" }, Internal Home.static_html;
    { text="Packages"; href="packages.html" },
        Internal (Repository.to_html repository);
    { text="Documentation"; href="https://github.com/OCamlPro/opam/wiki" },
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
  ("-o", Arg.String set_out_dir, "");
  ("--output", Arg.String set_out_dir,
    "The directory where to write the generated HTML files");

  ("-d", Arg.String website_of_path, "");
  ("--directory", Arg.String website_of_path,
    "Generate a website from the opam repository in 'directory'");

  ("-l", Arg.String website_of_opam, "");
  ("--local", Arg.String website_of_opam,
    "Generate a website from an opam repository in the local opam installation.");
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

