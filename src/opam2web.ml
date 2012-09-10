open Cow
open Cow.Html

(* Options *)
type options = {
  mutable out_dir: string;
}

let user_options: options = {
  out_dir = "www/";
}

let set_out_dir (dir: string) =
  user_options.out_dir <- dir

let specs = [
  ("-o", Arg.String set_out_dir, "");
  ("--output", Arg.String set_out_dir,
    "The directory where to write the generated HTML files");
]

(* Generate a whole static website using the given repository *)
let make_website repo_name =
  (* TODO: create out_dir if it doesn't exist, warn and exit if it's a file *)
  try
    let repository = Packages.load_repository repo_name in
    let packages = Packages.to_list repository in
    (* print_packages repository *)
    Template.generate ~out_dir:user_options.out_dir ([
      { text="A package manager for OCaml"; href="index.html" },
        Home.static_html;
      { text="Packages"; href="packages.html" },
        Packages.html_of_repository repository;
    ] @ packages);
  with
    (* TODO: notify that an error happened by setting an exit code <> 0 *)
    Not_found -> Globals.error "Repository '%s' not found!" repo_name

(* Main *)
let () =
  if (Array.length Sys.argv) < 2 then
    (* If no argument supplied, generate the static website using the 'default' 
       repository.  *)
    make_website "default"
  else
    Arg.parse specs make_website
      (Printf.sprintf "%s [options]* [repository_name]*" Sys.argv.(0))

