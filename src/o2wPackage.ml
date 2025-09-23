(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
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

open OpamTypesBase
open Cow
open O2wTypes
open OpamStateTypes
open OpamStd.Option.Op

let ( ++ ) = Html.( ++ )

let compare_alphanum = OpamPackage.compare

let pkg_href ?href_base nv =
  let name = OpamPackage.Name.to_string nv.name in
  let version = OpamPackage.Version.to_string nv.version in
  let base = Printf.sprintf "%s/%s.%s/" name name version in
  let base = Uri.of_string base in
  match href_base with
  | None   -> base
  | Some p -> Uri.resolve "http" p base

let name_href ?href_base name =
 let base = Uri.of_string (OpamPackage.Name.to_string name) in
  match href_base with
  | None   -> base
  | Some p -> Uri.resolve "http" p base


(* Comparison function using number of downloads for each package *)
let compare_popularity ?(reverse = false) pkg_stats p1 p2 =
  let pkg_count pkg =
    try OpamPackage.Name.Map.find (OpamPackage.name pkg) pkg_stats
    with Not_found -> Int64.zero in
  match pkg_count p1, pkg_count p2 with
  | c1, c2 when c1 <> c2 ->
    let c1, c2 = if reverse then c2, c1 else c1, c2 in
    Int64.compare c1 c2
  | _ -> compare_alphanum p1 p2

(* Comparison function using the publication time of packages *)
let compare_date ?(reverse = false) pkg_dates p1 p2 =
  let pkg_date pkg =
    try OpamPackage.Map.find pkg pkg_dates
    with Not_found -> 0. in
  match pkg_date p1, pkg_date p2 with
  | d1, d2 when d1 <> d2 ->
    let d1, d2 = if reverse then d2, d1 else d1, d2 in
    compare d1 d2
  | _ -> compare_alphanum p1 p2

(* An HTML fragment for the description of a package *)
let html_descr (short,long) =
  let to_html md =
    try Cow.Markdown.of_string md
    with Invalid_argument _ ->
      OpamConsole.error "BAD MARKDOWN in %s" short;
      Html.string md in
  Html.h4 (Html.string short)
  @ to_html long

(* copied from opamFormula.ml:92 , and ported from string to html *)
let html_of_formula html_of_a f =
  let rec aux ?(in_and=false) f =
    let paren_if ?(cond=false) s =
      if cond || OpamFormatConfig.(!r.all_parens)
      then Html.string "(" ++ s ++ Html.string ")"
      else s
    in
    match f with
    | Empty    -> Html.empty
    | Atom a   -> paren_if (html_of_a a)
    | Block x  -> Html.string "(" ++ aux x ++ Html.string ")"
    | And(x,y) ->
      paren_if
        (aux ~in_and:true x ++ Html.string " & " ++ aux ~in_and:true y)
    | Or(x,y)  ->
      paren_if ~cond:in_and
        (aux x ++ Html.string " | " ++ aux y)
  in
  aux f

let html_atom ~prefix st pkg (name, f) =
  let env v = match OpamVariable.Full.to_string v with
    | "name" -> Some (S (OpamPackage.name_to_string pkg))
    | "version" -> Some (S (OpamPackage.version_to_string pkg))
    | _ -> None
  in
  let f = OpamFilter.partial_filter_formula env (Atom (name, f)) in
  let constr = match f with
    | Atom (_, c) -> c
    | Empty -> Empty
    | _ -> assert false
  in
  let solutions =
    OpamFormula.packages st.packages
      (OpamFilter.filter_formula ~default:true (fun _ -> None) f)
  in
  let name_link =
    let hname =
      Html.span ~cls:"formula-package"
        (Html.string (OpamPackage.Name.to_string name))
    in
    let has_constraints =
      OpamFormula.fold_left (fun acc at ->
          acc || match at with Constraint _ -> true | Filter _ -> false)
        false constr
    in
    match has_constraints, OpamPackage.Set.max_elt solutions with
    | exception Not_found ->
      if OpamPackage.has_name st.packages name
      then Html.a ~href:(name_href ~href_base:prefix name) hname
      else hname
    | false, _ ->
      if OpamPackage.has_name st.packages name
      then Html.a ~href:(name_href ~href_base:prefix name) hname
      else hname
    | _, nv ->
      Html.a ~href:(pkg_href ~href_base:prefix nv) hname
  in
  let hconstr =
    if constr = Empty then None else
      Some
        (html_of_formula (function
             | Constraint (op, FString s) ->
               Html.string (OpamPrinter.relop op) ++
               Html.span ~cls:"package-version" (Html.string s)
             | Constraint (op, v) ->
               Html.string (OpamPrinter.relop op) ++
               Html.span ~cls:"label" (Html.string (OpamFilter.to_string v))
             | Filter f ->
               Html.span ~cls:"label" (Html.string (OpamFilter.to_string f)))
            constr)
  in
  let hconstr =
    if OpamPackage.Set.is_empty solutions then
      let warn = Html.i ~cls:"icon-warning-sign" Html.empty in
      match hconstr with
      | None -> Some warn
      | Some hc -> Some (hc ++ Html.string " " ++ warn)
    else hconstr
  in
  name_link,
  hconstr


(* Returns a HTML description of the given package *)
let to_html ~prefix univ pkg =
  let href =
    let ensure_slash s =
      if s = "" || s.[String.length s - 1] = '/' then s else s ^ "/"
    in
    pkg_href ~href_base:Uri.(of_string (ensure_slash prefix))
  in
  let {name; version} = pkg in
  let pkg = OpamPackage.create pkg.name pkg.version in
  let pkg_opam = OpamSwitchState.opam univ.st pkg in
  let pkg_latest_version =
    let version_set = OpamPackage.versions_of_name univ.st.packages pkg.name in
    OpamPackage.Version.Set.max_elt version_set
  in
  let version_links =
    let versions =
        (OpamPackage.versions_of_name univ.st.packages pkg.name)
    in
    let print_v v =
      Html.span ~cls:"package-version"
        (Html.string (OpamPackage.Version.to_string v)) ++
      if v = pkg_latest_version then Html.string " (latest)"
      else Html.empty
    in
    Html.div ~cls:"btn-group" @@
    Html.tag "a" ~cls:"btn dropdown-toggle"
      ~attrs:["data-toggle","dropdown"; "href","#"]
      (print_v version ++ Html.string " "
       ++ Html.span ~cls:"caret" Html.empty)
    ++
    (Html.ul ~cls:"dropdown-menu" @@
     List.map
       (fun version ->
          let href = href (OpamPackage.create name version) in
          if pkg.version = version then
            Html.li ~cls:"active"
              (Html.a ~href:(Uri.of_string "#") (print_v version))
          else
            Html.li (Html.a ~href (print_v version)))
       (OpamPackage.Version.Set.elements versions))
  in
  let doc_button =
    let name = OpamPackage.Name.to_string name in
    let version =
      if pkg.version = pkg_latest_version then "latest"
      else OpamPackage.Version.to_string pkg.version
    in
    let href =
      Uri.of_string @@
      "https://ocaml.org/p/" ^ name ^ "/" ^ version ^ "/doc/index.html"
    in
    Html.(a ~href (string "Documentation on ocaml.org"))
  in
  let pkg_descr =
    let to_html = function
      | None -> Html.empty
      | Some md ->
        try Cow.Markdown.of_string md
        with Invalid_argument _ | Parsing.Parse_error->
          OpamConsole.error "BAD MARKDOWN in %s descr"
            (OpamPackage.to_string pkg);
          Html.string md
    in
    (OpamFile.OPAM.synopsis pkg_opam >>| Html.string >>| Html.h4) +! Html.empty
    @ to_html (OpamFile.OPAM.descr_body pkg_opam)
  in
  let pkg_url = match OpamFile.OPAM.url pkg_opam with
    | None -> Html.empty
    | Some url_file ->
      let url = OpamFile.URL.url url_file in
      let kind =
        let k = OpamUrl.string_of_backend url.backend in
        Html.string " [" @ Html.string k @ Html.string "] "
      in
      let cksums =
        List.fold_right (fun c acc ->
            Html.br ::
            Html.small (Html.string (OpamHash.to_string c)) ::
            acc)
          (OpamFile.URL.checksum url_file) []
      in
      let url = OpamUrl.base_url url in
      Html.tag "tr"
        (Html.tag "th" (Html.string "Source " @ kind)
         @ Html.tag "td"
             (Html.a ~href:(Uri.of_string url) ~title:"Download source"
                (Html.string url)
              @ Html.list cksums))  in
  let list name l : (string * Cow.Xml.t) option = match l with
    | []  -> None
    | [e] -> Some (name      , Html.string e)
    | l   -> Some (name ^ "s", Html.string (OpamStd.Format.pretty_list l)) in
  let links name = function
    | [] -> None
    | (_::t) as l ->
       let urls =
         List.map (fun e -> Html.a ~href:(Uri.of_string e) (Html.string e)) l
         |> List.concat in
       let name = match t with [] -> name | _ -> name  ^ "s" in
       Some(name, urls) in
  let pkg_author = list "Author" (OpamFile.OPAM.author pkg_opam) in
  let pkg_maintainer = list "Maintainer" (OpamFile.OPAM.maintainer pkg_opam) in
  let pkg_license = list "License" (OpamFile.OPAM.license pkg_opam) in
  let pkg_homepage = links "Homepage" (OpamFile.OPAM.homepage pkg_opam) in
  let pkg_issues = links "Issue Tracker" (OpamFile.OPAM.bug_reports pkg_opam) in
  let pkg_tags = match OpamFile.OPAM.tags pkg_opam with
    | [] -> None
    | tags ->
      Some ("Tags",
            Html.list @@ List.map
              (fun t ->
                 Html.string " " ++
                 Html.a ~href:(Uri.with_fragment
                                 (Uri.of_string prefix)
                                 (Some t))
                   (Html.span ~cls:"label label-info" (Html.string t)))
              tags)
  in
  let pkg_published = match OpamPackage.Map.find_opt pkg univ.dates with
    | None -> None
    | Some timestamp ->
      Some ("Published", O2wMisc.html_of_timestamp timestamp)
  in
  let rec formula_list = function
    | [] -> []
    | Empty :: r -> formula_list r
    | Block f :: r -> formula_list (f::r)
    | (And _) as a :: r ->
      Html.string "all of" ++ and_formula a :: formula_list r
    | (Or _ as o) :: r ->
      Html.string "any of" ++ or_formula o :: formula_list r
    | Atom (name, f) :: r ->
      let name, constr =
        html_atom univ.st ~prefix:(Uri.of_string prefix) pkg (name, f)
      in
      Html.span name ++
      (match constr with
       | None -> Html.empty
       | Some c -> Html.span ~cls:"version-constraint" c) ::
      formula_list r
  and and_formula : filtered_formula -> Html.t = function
    | Empty -> Html.empty
    | f ->
      Html.ul ~cls:"formula" @@
      formula_list (OpamFormula.ands_to_list f)
  and or_formula = function
    | Empty -> Html.empty
    | f ->
      Html.ul ~add_li:true ~cls:"formula" @@
      formula_list (OpamFormula.ors_to_list f)
  in
  let pkg_depends =
    match OpamFile.OPAM.depends pkg_opam with
    | Empty -> None
    | f -> Some ("Dependencies", and_formula f)
  in
  let pkg_depopts =
    match OpamFile.OPAM.depopts pkg_opam with
    | Empty -> None
    | f -> Some ("Optional dependencies", or_formula f)
  in
  let pkg_conflicts =
    match OpamFile.OPAM.conflicts pkg_opam with
    | Empty -> None
    | f -> Some ("Conflicts", or_formula f)
  in
  let pkg_available =
    match OpamFile.OPAM.available pkg_opam with
    | FBool true -> None
    | filter     ->
      let filter_str = OpamFilter.to_string filter in
      Some ("Available", Html.string filter_str)
  in

  let pkg_stats = match univ.version_downloads with
    | None -> Html.empty
    | Some (stats, hashs) ->
      let checksum =
        OpamStd.Option.default []
          (OpamFile.OPAM.url pkg_opam >>| OpamFile.URL.checksum)
      in
      if checksum = [] then Html.empty else
      let pkg_count =
        let rec pkg_same_hash = function
          | h::r ->
            (let hs = (String.concat "/" (OpamHash.to_path h)) in
             match OpamStd.String.Map.find_opt hs hashs with
             | Some (p,s) ->
               if OpamPackage.Set.mem pkg s then Some p else pkg_same_hash r
             | None -> pkg_same_hash r)
          | [] -> None
        in
        try OpamPackage.Map.find pkg stats
        with Not_found ->
          (match pkg_same_hash checksum with
           | Some p ->
             (try OpamPackage.Map.find p stats
              with Not_found -> Int64.zero)
           | None -> Int64.zero)
      in
      let pkg_count_html = match pkg_count with
        | c when c = Int64.zero ->
           Html.string "Not installed in the last month."
        | c when c = Int64.one ->
           Html.string "Installed "
           @ Html.strong (Html.string "once")
           @ Html.string " last month."
        | c ->
           Html.string "Installed "
           @ Html.strong (Html.string (Int64.to_string c))
           @ Html.string " times last month."
      in
      Html.tag "tr"
        (Html.tag "th" (Html.string "Statistics")
         @ Html.tag "td" pkg_count_html)
  in
  let mk_revdeps depends_f rdepends =
    OpamPackage.Name.Map.fold (fun name versions acc ->
        let vf =
          OpamFormula.formula_of_version_set
            (OpamPackage.versions_of_name univ.st.packages name)
            versions
        in
        let latest =
          OpamPackage.create name (OpamPackage.Version.Set.max_elt versions)
        in
        let flags =
          match OpamSwitchState.opam_opt univ.st latest with
          | None -> OpamStd.String.Set.empty
          | Some o ->
            let flags_ll =
              OpamFormula.fold_left (fun acc (n, f) ->
                  if n <> pkg.name then acc else
                  let flags =
                    List.fold_left (fun acc -> function
                        | Atom (Filter (FIdent ([], v, None))) ->
                          OpamStd.String.Set.add
                            (OpamVariable.to_string v) acc
                        | _ -> acc)
                      OpamStd.String.Set.empty
                      (OpamFormula.ands_to_list f)
                  in
                  flags :: acc)
                [] (depends_f o)
            in
            match flags_ll with
            | [] -> OpamStd.String.Set.empty
            | fs::r -> List.fold_left OpamStd.String.Set.inter fs r
        in
        let formula =
          OpamFormula.ands @@
          List.rev_append
            (List.rev_map (fun flag ->
                 Atom (Filter (FIdent ([], OpamVariable.of_string flag, None))))
                (OpamStd.String.Set.elements flags))
            [OpamFormula.map (fun (op,v) ->
                 Atom (Constraint (op,
                                   FString (OpamPackage.Version.to_string v))))
                vf]
        in
        html_atom ~prefix:(Uri.of_string prefix)
          univ.st latest (name, formula) :: acc)
      (OpamPackage.to_map @@
       OpamStd.Option.default OpamPackage.Set.empty @@
       OpamPackage.Map.find_opt pkg rdepends)
      []
    |> List.rev
  in
  let rev_deps =
    match mk_revdeps OpamFile.OPAM.depends univ.rev_depends with
    | [] -> Html.empty
    | rd ->
      Html.div ~cls:"revdeps" @@
      Html.span ~cls:"revdeps-title" (Html.string "Required by") ++
      (Html.ul ~add_li:true ~cls:"formula" @@
       List.map (fun (name, constr) ->
           Html.span name ++
           match constr with
           | None -> Html.empty
           | Some c -> Html.span ~cls:"version-constraint" c) rd)
  in
  let rev_depopts =
    match mk_revdeps OpamFile.OPAM.depopts univ.rev_depopts with
    | [] -> Html.empty
    | rd ->
      Html.div ~cls:"revdeps" @@
      Html.span ~cls:"revdeps-title" (Html.string "Optionally used by") ++
      (Html.ul ~add_li:true ~cls:"formula" @@
       List.map (fun (name, constr) ->
           Html.span name ++
           match constr with
           | None -> Html.empty
           | Some c -> Html.span ~cls:"version-constraint" c) rd)
  in

  let mk_tr = function
    | None                   -> Html.empty
    | Some (title, contents) ->
       Html.tag "tr" (Html.tag "th" (Html.string title)
                      @ Html.tag "td" contents) in
  let repo_edit =
    univ.st.switch_config.OpamFile.Switch_config.repos >>=
    OpamStd.Option.of_Not_found (List.find (fun r ->
        OpamPackage.Map.mem pkg
          (OpamRepositoryName.Map.find r univ.st.switch_repos.repo_opams))) >>|
    OpamRepositoryState.get_repo univ.st.switch_repos >>= fun r ->
    OpamFile.Repo.read_opt (OpamRepositoryPath.repo r.repo_root) >>=
    OpamFile.Repo.upstream >>= fun upstream ->
    OpamFile.OPAM.metadata_dir pkg_opam >>| fun pkgdir ->
    let base = Uri.of_string upstream in
    let rel =
      OpamFilename.remove_prefix r.repo_root OpamFilename.Op.(pkgdir // "opam")
    in
    let url = Uri.(resolve "" base (of_string rel)) in
    let loc = Uri.to_string url in
    mk_tr (Some ("Edit", Html.a ~title:"Edit this package description"
                   ~href:url
                   (Html.string loc)))
  in
  let repo_edit = repo_edit +! Html.empty in
  Html.(
    h2 (string (OpamPackage.Name.to_string name) ++
        small (span ~cls:"versions" (string "version " ++ version_links)) ++
        span ~cls:"doc_button" doc_button)
    @ div ~cls:"row"
        (div ~cls:"span9"
           (div ~cls:"well" pkg_descr
            @ tag "table" ~cls:"table package-info"
                (tag "tbody" (mk_tr pkg_tags
                              @ mk_tr pkg_author
                              @ mk_tr pkg_license
                              @ mk_tr pkg_published
                              @ mk_tr pkg_homepage
                              @ mk_tr pkg_issues
                              @ mk_tr pkg_maintainer
                              @ mk_tr pkg_available
                              @ mk_tr pkg_depends
                              @ mk_tr pkg_depopts
                              @ mk_tr pkg_conflicts
                              @ pkg_url
                              @ pkg_stats
                              @ repo_edit)))
         @ div ~cls:"span3 revdeps-column"
             (if rev_deps = Html.empty && rev_depopts = Html.empty
              then span ~cls:"revdeps-title" (string "No package is dependent")
              else rev_deps ++ rev_depopts))
  )
