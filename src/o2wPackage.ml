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
open Cow.Html
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
    try Cow.Markdown.of_string md with
      Invalid_argument _ ->
        OpamGlobals.error "BAD MARKDOWN in %s" short;
        <:html< $str:md$ >>
  in
  <:html<
    <h4>$str:short$</h4>
    $to_html long$
  >>

let vset_of_name universe name =
  let open OpamfUniverse in
  try OpamPackage.Name.Map.find name universe.versions
  with Not_found -> OpamPackage.Version.Set.empty

let pkg_href =
  let open OpamfUniverse in
  Pkg.href ~href_base:Uri.(of_string "../../")

let html_conj = <:html<&amp;>>

let html_disj = <:html<|>>

let html_of_name name vset =
  let name_str = OpamPackage.Name.to_string name in
  if OpamPackage.Version.Set.cardinal vset = 0
  then <:html< $str:name_str$&>>
  else
    let v = OpamPackage.Version.Set.max_elt vset in
    <:html< <a href=$uri: pkg_href name v$>$str:name_str$</a>&>>

let html_of_vc name vset ((relop,v) as vc) =
  let vstr = OpamPackage.Version.to_string v in
  let rhtml = <:html<$str:OpamFormula.string_of_relop relop$&nbsp;>> in
  match OpamfuFormula.extremum_of_version_constraint vset vc with
  | Some v ->
    let vhtml = <:html<<a href=$uri: pkg_href name v$>$str:vstr$</a>&>> in
    <:html<$rhtml$<span class="version">$vhtml$</span>&>>
  | None   -> <:html<$rhtml$$str:vstr$&>>

let html_of_namevc universe name vc =
  let vset = vset_of_name universe name in
  let vvset = OpamfuFormula.(filter_versions (Some (Atom vc)) vset) in
  let vchtml = html_of_vc name vset vc in
  <:html<$html_of_name name vvset$ ($vchtml$)>>

let html_of_namevconj universe name lo hi =
  let open OpamfuFormula in
  let vset = vset_of_name universe name in
  let vvset = filter_versions (Some (And [Atom lo; Atom hi])) vset in
  let lo = html_of_vc name vset lo in
  let hi = html_of_vc name vset hi in
  <:html<$html_of_name name vvset$ ($lo$ $html_conj$ $hi$)>>

let html_of_namevdisj universe name lo hi =
  let open OpamfuFormula in
  let vset = vset_of_name universe name in
  let vvset = filter_versions (Some (Or [Atom lo; Atom hi])) vset in
  let lo = html_of_vc name vset lo in
  let hi = html_of_vc name vset hi in
  <:html<$html_of_name name vvset$ ($lo$ $html_disj$ $hi$)>>

let html_of_formula html_of_atom f =
  let open OpamfuFormula in
  let rec aux = function
    | Atom x -> html_of_atom x
    | And []  | Or []  -> <:html<&>>
    | And l -> <:html<$list:O2wMisc.intercalate html_conj (List.map aux l)$>>
    | Or  l -> <:html<$list:O2wMisc.intercalate html_disj (List.map aux l)$>>
  in
  aux f

let html_of_namevf universe namevf =
  let open OpamfuFormula in
  match namevf with
  | (name,(None | Some ((And []) | (Or [])))) ->
    <:html<$html_of_name name (vset_of_name universe name)$>>
  | (name,Some (Atom vc)) -> html_of_namevc universe name vc
  | (name,Some (And [Atom x; Atom y])) -> html_of_namevconj universe name x y
  | (name,((Some (And l)) as vf)) ->
    let vset = vset_of_name universe name in
    let vvset = OpamfuFormula.filter_versions vf vset in
    let ophtml = O2wMisc.intercalate html_conj
        (List.map (html_of_formula (html_of_vc name vset)) l)
    in
    <:html<$html_of_name name vvset$ $list:ophtml$>>
  | (name,Some (Or [Atom x; Atom y])) -> html_of_namevdisj universe name x y
  | (name,((Some (Or l)) as vf)) ->
    let vset = vset_of_name universe name in
    let vvset = OpamfuFormula.filter_versions vf vset in
    let ophtml = O2wMisc.intercalate html_disj
        (List.map (html_of_formula (html_of_vc name vset)) l)
    in
    <:html<$html_of_name name vvset$ $list:ophtml$>>

let mk_formula universe pkg_opam name f =
  let f = OpamfuFormula.of_opam_formula (f pkg_opam) in
  let f = OpamfuFormula.(sort_formula (simplify f)) in
  match f with
  | None -> None
  | Some f ->
    Some (name, html_of_formula (html_of_namevf universe) f)

let html_of_revdeps universe title revdeps =
  let deps = List.map (fun (dep_name, vdnf) ->
      let vf = OpamfuFormula.(simplify_expr (expr_of_version_dnf vdnf)) in
      <:html<<tr><td>$html_of_namevf universe (dep_name, vf)$</td></tr>&>>
    ) revdeps
  in
  match deps with
  | [] -> []
  | _ -> <:html<
             <tr class="well"><th colspan='3'>$str: title$</th></tr>
           >> :: deps

let version_links universe ~pkg_href name version =
  let open OpamfUniverse in
  let versions =
    try OpamPackage.Version.Set.elements
          (OpamPackage.Name.Map.find name universe.versions)
    with Not_found -> match version with
      | None   -> []
      | Some v -> [v]
  in
  let versions = List.map
      (fun v ->
         let href = pkg_href name v in
         let v_str = OpamPackage.Version.to_string v in
         if Some v = version then
           <:html<
             <span class="version">
               <strong>$str: v_str$</strong>
             </span>
           >>
         else
           <:html<
             <span class="version">
               <a href=$uri: href$>$str: v_str$</a>
             </span> >>)
      versions
  in
  let versions = O2wMisc.intercalate <:html<, >> versions in
  <:html<$list:versions$>>

(* Returns a HTML description of the given package *)
let to_html ~statistics universe pkg_info =
  let open OpamfUniverse in
  let name = OpamPackage.Name.of_string pkg_info.name in
  let version = OpamPackage.Version.of_string pkg_info.version in
  let pkg = OpamPackage.create name version in
  let pkg_opam = OpamPackage.Map.find pkg universe.pkgs_opams in
  let version_links = version_links ~pkg_href universe name (Some version) in
  let pkg_url = match pkg_info.url with
    | None          -> <:html< >>
    | Some url_file ->
      let kind =
        let k = OpamFile.URL.kind url_file in
        <:html< [$str: string_of_repository_kind k$] >> in
      let checksum = match OpamFile.URL.checksum url_file with
        | Some c -> <:html< <small>$str: c$</small> >>
        | None -> <:html< >> in
      let url = string_of_address (OpamFile.URL.url url_file) in
      <:html<
        <tr>
        <th>Source $kind$</th>
        <td>
          <a href=$str: url$ title="Download source">$str: url$</a><br />
          $checksum$
        </td>
        </tr>
      >> in
  let list name l : (string * Cow.Xml.t) option = match l with
    | []  -> None
    | [e] -> Some (name      , <:html<$str:e$>>)
    | l   -> Some (name ^ "s", <:html<$str:OpamMisc.pretty_list l$>>) in
  let links name = function
    | [] -> None
    | (_::t) as l ->
      let urls = List.map (fun e -> <:html< <a href="$str:e$">$str:e$</a> >>) l in
      let name = match t with [] -> name | _ -> name  ^ "s" in
      Some(name, <:html< $list:urls$ >>) in
  let pkg_versions = Some ("Versions", version_links) in
  let pkg_author = list "Author" (OpamFile.OPAM.author pkg_opam) in
  let pkg_maintainer = list "Maintainer" (OpamFile.OPAM.maintainer pkg_opam) in
  let pkg_license = list "License" (OpamFile.OPAM.license pkg_opam) in
  let pkg_homepage = links "Homepage" (OpamFile.OPAM.homepage pkg_opam) in
  let pkg_issues = links "Issue Tracker" (OpamFile.OPAM.bug_reports pkg_opam) in
  let pkg_tags = list "Tag" (OpamFile.OPAM.tags pkg_opam) in
  let pkg_published = match pkg_info.published with
    | None -> None
    | Some timestamp ->
      Some ("Published",<:html<$str:O2wMisc.string_of_timestamp timestamp$>>)
  in
  let pkg_depends = mk_formula universe pkg_opam "Dependencies"
      (fun opam -> filter_deps (OpamFile.OPAM.depends opam)) in
  let pkg_depopts = mk_formula universe pkg_opam "Optional dependencies"
      (fun opam -> filter_deps (OpamFile.OPAM.depopts opam)) in
  let requiredby =
    try OpamPackage.Map.find pkg universe.rev_depends
    with Not_found -> OpamPackage.Name.Map.empty in
  let requiredby_deps = OpamPackage.Name.Map.bindings requiredby in
  let requiredby_html =
    html_of_revdeps universe "Necessary for" requiredby_deps
  in

  let usedby =
    try OpamPackage.Map.find pkg universe.rev_depopts
    with Not_found -> OpamPackage.Name.Map.empty in
  let usedby_deps = OpamPackage.Name.Map.bindings usedby in
  let usedby_html = html_of_revdeps universe "Optional for" usedby_deps in

  let norevdeps = <:html< <tr><td>No package is dependent</td></tr>&>> in

  let pkg_compiler = match OpamFile.OPAM.ocaml_version pkg_opam with
    | None -> None
    | Some cvf ->
      let formula_str =
        let open OpamFormula in
        string_of_formula (fun (relop,v) ->
            string_of_relop relop
            ^ " " ^
            OpamCompiler.Version.to_string v
          ) cvf
      in
      Some ("OCaml",<:html<$str:formula_str$>>)
  in

  let pkg_os =
    let open OpamFormula in
    match OpamFile.OPAM.os pkg_opam with
    | Empty -> None
    | f ->
      let formula_str = string_of_formula (fun (b,s) ->
          if b then s else "!"^s
        ) f in
      Some ("OS",<:html<$str:formula_str$>>)
  in

  let pkg_stats = match statistics with
    | None -> <:html< >>
    | Some sset ->
      let s = sset.month_stats in
      let pkg_count =
        try OpamPackage.Map.find pkg s.pkg_stats
        with Not_found -> Int64.zero
      in
      let pkg_count_html = match pkg_count with
        | c when c = Int64.zero -> <:html< Not installed in the last month. >>
        | c when c = Int64.one ->
          <:html< Installed <strong>once</strong> in last month. >>
        | c ->
          <:html< Installed <strong>$str: Int64.to_string c$</strong> times in last month. >>
      in
      <:html<
        <tr>
          <th>Statistics</th>
          <td>
            $pkg_count_html$
          </td>
        </tr>
      >>
  in
  let mk_tr = function
    | None                   -> <:html<&>>
    | Some (title, contents) ->
      <:html<
            <tr>
              <th>$str: title$</th>
              <td>$contents$</td>
            </tr>
      >> in
  let repo = Pkg.to_repo universe pkg in
  let links = Repo.links repo in
  let _, prefix = OpamPackage.Map.find pkg universe.pkg_idx in
  let pkg_edit = match OpamFile.Repo.upstream links with
    | None -> <:html<&>>
    | Some url_base ->
      let base = Uri.of_string url_base in
      let opam_loc = OpamFilename.remove_prefix
        repo.OpamTypes.repo_root
        (OpamPath.Repository.opam repo prefix pkg) in
      let url = Uri.(resolve "" base (of_string opam_loc)) in
      let loc = Uri.to_string url in
      mk_tr (Some ("Edit",<:html<
        <a title="Edit this package description" href=$uri:url$>$str:loc$</a>
      >>))
  in
  <:html<
    <h2><a href="../">$str: pkg_info.name$</a> $str: pkg_info.version$</h2>

    <div class="row">
      <div class="span9">
        <div class="well">$pkg_info.descr$</div>
        <table class="table">
          <tbody>
            $mk_tr pkg_versions$
            $mk_tr pkg_author$
            $mk_tr pkg_license$
            $mk_tr pkg_homepage$
            $mk_tr pkg_issues$
            $mk_tr pkg_maintainer$
            $mk_tr pkg_tags$
            $mk_tr pkg_depends$
            $mk_tr pkg_depopts$
            $mk_tr pkg_compiler$
            $mk_tr pkg_os$
            $mk_tr pkg_published$
            $pkg_url$
            $pkg_stats$
            $pkg_edit$
          </tbody>
        </table>
      </div>
      <div class="span3">
        <table class="table table-bordered">
          <tbody>
            $list: requiredby_html$
            $list: usedby_html$
            $match requiredby_deps,usedby_deps with
              | [], [] -> norevdeps
              | _ , _  -> Cow.Html.nil$
          </tbody>
        </table>
      </div>
    </div>
  >>
