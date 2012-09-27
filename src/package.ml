open Cow.Html
open O2w_common

(* Build a record representing information about a package *)
let get_info ?(href_prefix="") (repository: Path.R.t) (pkg: Types.NV.t)
    : package_info =
  let pkg_name = Types.N.to_string (Types.NV.name pkg) in
  let pkg_version = Types.V.to_string (Types.NV.version pkg) in
  let pkg_href =
    Printf.sprintf "%s%s.%s.html" href_prefix pkg_name pkg_version
  in
  let pkg_synopsis =
    File.Descr.synopsis
      (File.Descr.read (Path.R.descr repository pkg))
  in
  let pkg_descr_markdown =
    File.Descr.full (File.Descr.read (Path.R.descr repository pkg))
  in
  let pkg_descr =
    Cow.Markdown.to_html (Cow.Markdown.of_string pkg_descr_markdown)
  in
  let pkg_title = Printf.sprintf "%s %s" pkg_name pkg_version in
  {
    pkg_name     = pkg_name;
    pkg_version  = pkg_version;
    pkg_descr    = pkg_descr;
    pkg_synopsis = pkg_synopsis;
    pkg_href     = pkg_href;
    pkg_title    = pkg_title;
  }


(* Returns the latest version of a list containing multiple versions of the same
   package *)
let latest: Types.NV.t list -> Types.NV.t = function
  | h :: _ -> h
  (* | [] -> failwith "Repository.to_html: error building unique_packages" *)
  | [] -> raise Not_found

(* Find the latest version of a package in the two-dimensions list representing
   package versions *)
let find_latest_version (unique_packages: Types.NV.t list list)
    (pkg_name: string) : string option =
  let packages =
    try
      List.find (fun versions ->
          if Types.N.to_string (Types.NV.name (latest versions)) = pkg_name then
            true
          else
            false)
        unique_packages
    with
      Not_found ->
        Printf.eprintf "Warning: missing dependency '%s'\n%!" pkg_name;
        []
  in
    try
      Some (Types.V.to_string (Types.NV.version (latest packages)))
    with
      Not_found -> None

(* Returns a HTML description of the given package *)
let to_html (repository: Path.R.t) (unique_packages: Types.NV.t list list)
    (reverse_dependencies: (Types.N.t * Types.NV.t list list) list)
    (versions: Types.NV.t list) (pkg: Types.NV.t): Cow.Html.t =
  let pkg_info = get_info repository pkg in
  let pkg_url =
    try
      let url_file = File.URL.read (Path.R.url repository pkg) in
      let kind = match File.URL.kind url_file with
        | Some k -> <:xml< [$str: k$] >>
        | None -> <:xml< >>
      in
      let checksum = match File.URL.checksum url_file with
        | Some c -> <:xml< <small>$str: c$</small> >>
        | None -> <:xml< >>
      in
      let url = File.URL.url url_file in
      <:xml<
        <tr>
          <th>Source $kind$</th>
          <td>
            <a href="$str: url$" title="Download source">$str: url$</a><br />
            $checksum$
          </td>
        </tr>
      >>
    with
      Globals.Exit 66 -> <:xml< >>
  in
  let opam_file = File.OPAM.read (Path.R.opam repository pkg) in
  let version_links = List.map (fun (pkg: Types.NV.t) ->
      let version = Types.V.to_string (Types.NV.version pkg) in
      let href = Printf.sprintf "%s.%s.html" pkg_info.pkg_name version in
      if pkg_info.pkg_version = version then
        <:xml<
          <li class="active">
            <a href="#">version $str: version$</a>
          </li>
        >>
      else
        <:xml< <li><a href="$str: href$">$str: version$</a></li> >>)
    versions
  in
  let pkg_maintainer = File.OPAM.maintainer opam_file in
  let html_of_dependencies title dependencies =
    let deps = List.map (fun ((name, _), constr_opt) ->
        let latest_version = find_latest_version unique_packages name in
        let href = match latest_version with
          | None -> <:xml< $str: name$ >>
          | Some v ->
              let href_str = Printf.sprintf "%s.%s.html" name v in
              <:xml< <a href="$str: href_str$">$str: name$</a> >>
        in
        let version = match constr_opt with
          | None -> ""
          | Some (r, v) -> Printf.sprintf "( %s %s )" r v
        in
        <:xml<
          <tr>
            <td>
              $href$
              <small>$str: version$</small>
            </td>
          </tr>
        >>)
      (List.flatten dependencies)
    in
    match deps with
    | [] -> []
    | _ -> <:xml<
        <tr class="well">
          <th>$str: title$</th>
        </tr>
      >> :: deps
  in
  let dependencies = html_of_dependencies "Dependencies"
      (File.OPAM.depends opam_file)
  in
  let depopts = html_of_dependencies "Optional"
      (File.OPAM.depopts opam_file)
  in
  let requiredby =
    try
      List.assoc (Types.NV.name pkg) reverse_dependencies
    with
      Not_found -> []
  in
  let requiredby_deps = List.map (fun req ->
    [((Types.N.to_string (Types.NV.name (List.hd req)), ""), None)] ) requiredby
  in
  let requiredby_html =
    html_of_dependencies "Required by" requiredby_deps
  in
  let nodeps = <:xml< <tr><td>No dependency</td></tr> >> in
  <:xml<
    <h2>$str: pkg_info.pkg_name$</h2>

    <div class="row">
      <div class="span9">
        <div>
          <ul class="nav nav-pills">
            $list: version_links$
          </ul>
        </div>

        <table class="table">
          <tbody>
            $pkg_url$
            <tr>
              <th>Maintainer</th>
              <td>
                $str: pkg_maintainer$
              </td>
            </tr>
          </tbody>
        </table>

        <div class="well">$pkg_info.pkg_descr$</div>
      </div>

      <div class="span3">
        <table class="table table-bordered">
          <tbody>
            $list: dependencies$
            $list: depopts$
            $list: requiredby_html$
            $match (dependencies, depopts, requiredby) with
              | ([], [], []) -> nodeps
              | _ -> Cow.Html.nil$
          </tbody>
        </table>
      </div>
    </div>
  >>
