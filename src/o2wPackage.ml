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

open OpamTypes
open OpamTypesBase
open Cow
open O2wTypes

let compare_alphanum = OpamPackage.compare

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

(* Returns a HTML description of the given package *)
let to_html ~statistics ~prefix universe pkg_info =
  let open OpamfUniverse in
  let href = Pkg.href ~href_base:Uri.(of_string prefix) in
  let name = OpamPackage.Name.of_string pkg_info.name in
  let version = OpamPackage.Version.of_string pkg_info.version in
  let pkg = OpamPackage.create name version in
  let pkg_opam = OpamPackage.Map.find pkg universe.pkgs_opams in
  let version_links =
    let versions =
      try OpamPackage.Version.Set.elements
            (OpamPackage.Name.Map.find name universe.versions)
      with Not_found -> [version] in
    List.map
      (fun version ->
         let href = href name version in
         let version = OpamPackage.Version.to_string version in
         if pkg_info.version = version then
           Html.li ~cls:"active"
             (Html.a ~href:(Uri.of_string "#")
                (Html.string "version " @ Html.string version))
         else
           Html.li (Html.a ~href (Html.string version)))
      versions in
  let pkg_url = match pkg_info.url with
    | None          -> Html.empty
    | Some url_file ->
      let kind =
        let k = OpamFile.URL.kind url_file in
        Html.string " ["
        @ Html.string (string_of_repository_kind k) @ Html.string "] " in
      let checksum = match OpamFile.URL.checksum url_file with
        | Some c -> Html.small (Html.string c)
        | None -> Html.empty in
      let url = string_of_address (OpamFile.URL.url url_file) in
      Html.tag "tr"
        (Html.tag "th" (Html.string "Source " @ kind)
         @ Html.tag "td"
             (Html.a ~href:(Uri.of_string url) ~title:"Download source"
                (Html.string url)
              @ Html.br Html.empty
              @ checksum)) in
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
  let pkg_tags = list "Tag" (OpamFile.OPAM.tags pkg_opam) in
  let pkg_published = match pkg_info.published with
    | None -> None
    | Some timestamp ->
      Some ("Published", Html.string (O2wMisc.string_of_timestamp timestamp))
  in
  let html_conj = Html.string "&" in
  let html_disj = Html.string "|" in
  let vset_of_name name =
    try
      OpamPackage.Name.Map.find name universe.versions
    with Not_found -> OpamPackage.Version.Set.empty
  in
  let html_of_name name vset =
    let name_str = OpamPackage.Name.to_string name in
    if OpamPackage.Version.Set.cardinal vset = 0
    then Html.string name_str
    else let v = OpamPackage.Version.Set.max_elt vset in
         Html.a ~href:(href name v) (Html.string name_str)
  in
  let html_of_vc attrs name vset ((relop,v) as vc) =
    let vstr = OpamPackage.Version.to_string v in
    let rhtml = Html.string (OpamFormula.string_of_relop relop)
                @ Html.string "&nbsp;" in
    match OpamfuFormula.extremum_of_version_constraint vset vc with
    | Some v ->
       Html.tag "td" ~attrs
         (rhtml @ Html.a ~href:(href name v) (Html.string vstr))
    | None ->
       Html.tag "td" ~attrs (rhtml @ Html.string vstr) in
  let html_of_namevc name vc =
    let vset = vset_of_name name in
    let vvset = OpamfuFormula.(filter_versions (Some (Atom vc)) vset) in
    let vchtml = html_of_vc ["colspan","3"] name vset vc in
    Html.tag "td" (html_of_name name vvset) @ vchtml
  in
  let html_of_namevconj name lo hi =
    let vset = vset_of_name name in
    let vvset = OpamfuFormula.(
      filter_versions (Some (And [Atom lo; Atom hi])) vset
    ) in
    let lo = html_of_vc [] name vset lo in
    let hi = html_of_vc [] name vset hi in
    Html.tag "td" (html_of_name name vvset) @ lo
    @ Html.tag "td" html_conj @ hi
  in
  let html_of_namevdisj name lo hi =
    let vset = vset_of_name name in
    let vvset = OpamfuFormula.(
      filter_versions (Some (Or [Atom lo; Atom hi])) vset
    ) in
    let lo = html_of_vc [] name vset lo in
    let hi = html_of_vc [] name vset hi in
    Html.tag "td" (html_of_name name vvset) @ lo
    @ Html.tag "td" html_disj @ hi
  in
  let enrow ?(attrs=[]) tds = Html.tag "tr" ~attrs tds in
  let html_of_operator k ophtml hd argl =
    let rows = List.length argl + 1 in
    Html.tag "td" ~attrs:["colspan", "4"]
      (Html.tag "table" ~attrs:["class", "formula"]
         (Html.tag "tr"
            (Html.tag "th" ophtml ~cls:"operator"
                                  ~attrs:["rowspan", string_of_int rows]
             @ k hd)
          @ List.concat(List.map (fun x -> enrow (k x)) argl))) in
  let rec html_of_formula html_of_atom = OpamfuFormula.(function
    | Atom x -> html_of_atom x
    | And []  | Or []  -> Html.tag "td" Html.empty
    | And [x] | Or [x] -> html_of_formula html_of_atom x
    | And (hd::conjl)  ->
      html_of_operator (html_of_formula html_of_atom) html_conj hd conjl
    | Or  (hd::disjl)  ->
      html_of_operator (html_of_formula html_of_atom) html_disj hd disjl
  ) in
  let html_of_namevf = OpamfuFormula.(function
    | (name,(None | Some ((And []) | (Or [])))) ->
       Html.tag "td" (html_of_name name (vset_of_name name))
       @ Html.tag "td" ~attrs:["colspan", "3"] Html.empty
    | (name,Some (Atom vc)) -> html_of_namevc name vc
    | (name,Some (And [Atom x; Atom y])) -> html_of_namevconj name x y
    | (name,((Some (And (c::cl))) as vf)) ->
      let vset = vset_of_name name in
      let vvset = OpamfuFormula.filter_versions vf vset in
      let ophtml = html_of_operator
        (html_of_formula (html_of_vc ["colspan","3"] name vset)) html_conj c cl
      in
      Html.tag "td" ~cls:"pkgname" (html_of_name name vvset)
      @ ophtml
    | (name,Some (Or [Atom x; Atom y])) -> html_of_namevdisj name x y
    | (name,((Some (Or (d::dl))) as vf)) ->
      let vset = vset_of_name name in
      let vvset = OpamfuFormula.filter_versions vf vset in
      let ophtml = html_of_operator
        (html_of_formula (html_of_vc ["colspan","3"] name vset)) html_disj d dl
      in
      Html.tag "td" ~cls:"pkgname" (html_of_name name vvset)
      @ ophtml
  ) in
  let mk_formula name f =
    let f = OpamfuFormula.of_opam_formula (f pkg_opam) in
    let f = OpamfuFormula.(sort_formula (simplify f)) in
    match f with
    | None -> None
    | Some f ->
      Some (name, Html.tag "table" ~cls:"formula-wrap"
                    (Html.tag "tr" (html_of_formula html_of_namevf f)))
  in
  let pkg_depends = mk_formula "Dependencies"
      (fun opam -> filter_deps ~build:true ~test:true ~doc:true ~dev:false (OpamFile.OPAM.depends opam)) in
  let pkg_depopts = mk_formula "Optional dependencies"
      (fun opam -> filter_deps ~build:true ~test:true ~doc:true ~dev:false (OpamFile.OPAM.depopts opam)) in
  let html_of_revdeps title revdeps =
    let deps = List.map (fun (dep_name, vdnf) ->
      let vf = OpamfuFormula.(simplify_expr (expr_of_version_dnf vdnf)) in
      let vset = vset_of_name dep_name in
      let vvset = OpamfuFormula.filter_versions vf vset in
      let name_html =
        Html.tag "tr" (Html.tag "td" ~attrs:["colspan", "3"]
                                (html_of_name dep_name vvset)) in
      let rec html_of_vf span = OpamfuFormula.(function
        | None | Some ((And []) | (Or [])) -> []
        | Some (Atom vc) ->
          [html_of_vc ["colspan",string_of_int span] dep_name vset vc]
        | Some (And ((Atom vc)::cl)) ->
          let rows = 1 + List.length cl in
          (Html.tag "th" ~cls:"operator" ~attrs:["rowspan", string_of_int rows]
             html_conj
           @ html_of_vc ["colspan",string_of_int (span - 1)] dep_name vset vc)
          :: (List.fold_right (fun c l ->
                  l @ (html_of_vf (span - 1) (Some c))
                ) cl [])
        | Some (Or (d::dl)) ->
          let rows = OpamfuFormula.expr_width (Or (d::dl)) in
          begin match html_of_vf (span - 1) (Some d) with
          | h::t ->
             (Html.tag "th" html_disj ~cls:"operator"
                                      ~attrs:["rowspan", string_of_int rows]
              @ h)
             :: (List.fold_right (fun d l ->
                     l @ (html_of_vf (span - 1) (Some d))
                   ) dl t)
          | [] -> []
          end
        | _ -> assert false (* FIXME: pattern not exhaustive *)
      ) in
      let attrs = ["class","embedded-formula"] in
      name_html @ List.concat (List.map (enrow ~attrs) (html_of_vf 3 vf))
    ) revdeps
    in
    match deps with
    | [] -> []
    | _ -> Html.tag "tr" ~cls:"well"
             (Html.tag "th" ~attrs:["colspan", "3"] (Html.string title))
           :: deps
  in

  let requiredby =
    try OpamPackage.Map.find pkg universe.rev_depends
    with Not_found -> OpamPackage.Name.Map.empty in
  let requiredby_deps = OpamPackage.Name.Map.bindings requiredby in
  let requiredby_html =
    html_of_revdeps "Necessary for" requiredby_deps in

  let usedby =
    try OpamPackage.Map.find pkg universe.rev_depopts
    with Not_found -> OpamPackage.Name.Map.empty in
  let usedby_deps = OpamPackage.Name.Map.bindings usedby in
  let usedby_html =
    html_of_revdeps "Optional for" usedby_deps in

  let norevdeps = Html.tag "tr" (Html.tag "td"
                                   (Html.string "No package is dependent")) in

  let pkg_compiler = match OpamFile.OPAM.ocaml_version pkg_opam with
    | None -> None
    | Some cvf ->
      let formula_str = OpamFormula.(string_of_formula (fun (relop,v) ->
        (string_of_relop relop)^" "^(OpamCompiler.Version.to_string v)
      ) cvf) in
      Some ("OCaml", Html.string formula_str)
  in

  let pkg_available =
    match OpamFile.OPAM.available pkg_opam with
    | FBool true -> None
    | filter     ->
      let filter_str = OpamFilter.to_string filter in
      Some ("Available", Html.string filter_str)
  in

  let pkg_os = OpamFormula.(
    match OpamFile.OPAM.os pkg_opam with
    | Empty -> None
    | f ->
      let formula_str = string_of_formula (fun (b,s) ->
        if b then s else "!"^s
      ) f in
      Some ("OS", Html.string formula_str)
  ) in

  let pkg_stats = match statistics with
    | None -> Html.empty
    | Some sset ->
      let s = sset.month_stats in
      let pkg_count =
        try OpamPackage.Map.find pkg s.pkg_stats
        with Not_found -> Int64.zero
      in
      let pkg_count_html = match pkg_count with
        | c when c = Int64.zero ->
           Html.string "Not installed in the last month."
        | c when c = Int64.one ->
           Html.string "Installed "
           @ Html.strong (Html.string "once")
           @ Html.string " in last month."
        | c ->
           Html.string "Installed "
           @ Html.strong (Html.string (Int64.to_string c))
           @ Html.string " times in last month."
      in
      Html.tag "tr"
        (Html.tag "th" (Html.string "Statistics")
         @ Html.tag "td" pkg_count_html)
  in
  let mk_tr = function
    | None                   -> Html.empty
    | Some (title, contents) ->
       Html.tag "tr" (Html.tag "th" (Html.string title)
                      @ Html.tag "td" contents) in
  let repo = Pkg.to_repo universe pkg in
  let links = Repo.links repo in
  let _, prefix = OpamPackage.Map.find pkg universe.pkg_idx in
  let pkg_edit = match OpamFile.Repo.upstream links with
    | None -> Html.empty
    | Some url_base ->
      let base = Uri.of_string url_base in
      let opam_loc = OpamFilename.remove_prefix
        repo.OpamTypes.repo_root
        (OpamRepositoryPath.opam repo prefix pkg) in
      let url = Uri.(resolve "" base (of_string opam_loc)) in
      let loc = Uri.to_string url in
      mk_tr (Some ("Edit", Html.a ~title:"Edit this package description"
                                  ~href:url
                                  (Html.string loc)))
  in
  Html.(
    h2 (string pkg_info.name)
    @ div ~cls:"row"
        (div ~cls:"span9"
           (div (ul ~cls:"nav nav-pills" version_links)
            @ tag "table" ~cls:"table"
                (tag "tbody" (mk_tr pkg_author
                              @ mk_tr pkg_license
                              @ mk_tr pkg_homepage
                              @ mk_tr pkg_issues
                              @ mk_tr pkg_tags
                              @ mk_tr pkg_maintainer
                              @ mk_tr pkg_depends
                              @ mk_tr pkg_depopts
                              @ mk_tr pkg_compiler
                              @ mk_tr pkg_available
                              @ mk_tr pkg_os
                              @ mk_tr pkg_published
                              @ pkg_url
                              @ pkg_stats
                              @ pkg_edit))
            @ div ~cls:"well" pkg_info.descr)
         @ div ~cls:"span3"
             (tag "table" ~cls:"table table-bordered"
                (tag "tbody" (List.concat requiredby_html
                              @ List.concat usedby_html
                              @ match requiredby_deps,usedby_deps with
                                | [], [] -> norevdeps
                                | _ , _  -> Html.empty)))
        ))
