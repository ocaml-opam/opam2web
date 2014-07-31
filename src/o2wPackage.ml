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
  let to_html md = Cow.Markdown.of_string md in
  begin <:html<
    <h4>$str:short$</h4>
    $to_html long$
  >> end

(* Returns a HTML description of the given package *)
let to_html ~statistics universe pkg_info =
  let open OpamfUniverse in
  let href = Pkg.href ~href_base:Uri.(of_string "../../") in
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
           <:html<
             <li class="active">
               <a href="#">version $str: version$</a>
             </li>
           >>
         else
           <:html< <li><a href=$uri: href$>$str: version$</a></li> >>)
      versions in
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
  let pkg_author = list "Author" (OpamFile.OPAM.author pkg_opam) in
  let pkg_maintainer = list "Maintainer" (OpamFile.OPAM.maintainer pkg_opam) in
  let pkg_license = list "License" (OpamFile.OPAM.license pkg_opam) in
  let pkg_homepage = links "Homepage" (OpamFile.OPAM.homepage pkg_opam) in
  let pkg_issues = links "Issue Tracker" (OpamFile.OPAM.bug_reports pkg_opam) in
  let pkg_tags = list "Tag" (OpamFile.OPAM.tags pkg_opam) in
  let pkg_published = O2wMisc.string_of_timestamp pkg_info.published in
  let html_conj = <:html<&#x2227;>> in
  let html_disj = <:html<&#x2228;>> in
  let vset_of_name name =
    try
      OpamPackage.Name.Map.find name universe.versions
    with Not_found -> OpamPackage.Version.Set.empty
  in
  let html_of_name name vset =
    let name_str = OpamPackage.Name.to_string name in
    if OpamPackage.Version.Set.cardinal vset = 0
    then <:html< $str:name_str$&>>
    else let v = OpamPackage.Version.Set.max_elt vset in
         <:html< <a href=$uri: href name v$>$str:name_str$</a>&>>
  in
  let html_of_vc attrs name vset ((relop,v) as vc) =
    let vstr = OpamPackage.Version.to_string v in
    let rhtml = <:html<$str:(OpamFormula.string_of_relop relop)$&nbsp;>> in
    match OpamfuFormula.extremum_of_version_constraint vset vc with
    | Some v -> <:html<
      <td $alist:attrs$>$rhtml$<a href=$uri: href name v$>$str:vstr$</a></td>&>>
    | None -> <:html<<td $alist:attrs$>$rhtml$$str:vstr$</td>&>>
  in
  let html_of_namevc name vc =
    let vset = vset_of_name name in
    let vvset = OpamfuFormula.(filter_versions (Some (Atom vc)) vset) in
    let vchtml = html_of_vc ["colspan","3"] name vset vc in
    <:html<<td>$html_of_name name vvset$</td>$vchtml$>>
  in
  let html_of_namevconj name lo hi =
    let vset = vset_of_name name in
    let vvset = OpamfuFormula.(
      filter_versions (Some (And [Atom lo; Atom hi])) vset
    ) in
    let lo = html_of_vc [] name vset lo in
    let hi = html_of_vc [] name vset hi in
    <:html<<td>$html_of_name name vvset$</td>$lo$<td>$html_conj$</td>$hi$>>
  in
  let html_of_namevdisj name lo hi =
    let vset = vset_of_name name in
    let vvset = OpamfuFormula.(
      filter_versions (Some (Or [Atom lo; Atom hi])) vset
    ) in
    let lo = html_of_vc [] name vset lo in
    let hi = html_of_vc [] name vset hi in
    <:html<<td>$html_of_name name vvset$</td>$lo$<td>$html_disj$</td>$hi$>>
  in
  let enrow ?(attrs=[]) tds = <:html<<tr $alist:attrs$>$tds$</tr>&>> in
  let html_of_operator k ophtml hd argl =
    let rows = List.length argl + 1 in
    <:html<
      <td colspan='4'><table class="formula">
        <tr>
          <th class="operator" rowspan=$int:rows$>$ophtml$</th>
          $k hd$
        </tr>
        $list:List.map (fun x -> enrow (k x)) argl$
      </table></td>
    >> in
  let rec html_of_formula html_of_atom = OpamfuFormula.(function
    | Atom x -> html_of_atom x
    | And []  | Or []  -> <:html<<td/>&>>
    | And [x] | Or [x] -> html_of_formula html_of_atom x
    | And (hd::conjl)  ->
      html_of_operator (html_of_formula html_of_atom) html_conj hd conjl
    | Or  (hd::disjl)  ->
      html_of_operator (html_of_formula html_of_atom) html_disj hd disjl
  ) in
  let html_of_namevf = OpamfuFormula.(function
    | (name,(None | Some ((And []) | (Or [])))) -> <:html<
      <td>$html_of_name name (vset_of_name name)$</td><td colspan='3'/>
    >>
    | (name,Some (Atom vc)) -> html_of_namevc name vc
    | (name,Some (And [Atom x; Atom y])) -> html_of_namevconj name x y
    | (name,((Some (And (c::cl))) as vf)) ->
      let vset = vset_of_name name in
      let vvset = OpamfuFormula.filter_versions vf vset in
      let ophtml = html_of_operator
        (html_of_formula (html_of_vc ["colspan","3"] name vset)) html_conj c cl
      in <:html<
        <td class="pkgname">$html_of_name name vvset$</td>
        $ophtml$
      >>
    | (name,Some (Or [Atom x; Atom y])) -> html_of_namevdisj name x y
    | (name,((Some (Or (d::dl))) as vf)) ->
      let vset = vset_of_name name in
      let vvset = OpamfuFormula.filter_versions vf vset in
      let ophtml = html_of_operator
        (html_of_formula (html_of_vc ["colspan","3"] name vset)) html_disj d dl
      in <:html<
        <td class="pkgname">$html_of_name name vvset$</td>
        $ophtml$
      >>
  ) in
  let mk_formula name f =
    let f = OpamfuFormula.of_opam_formula (f pkg_opam) in
    let f = OpamfuFormula.(sort_formula (simplify f)) in
    match f with
    | None -> None
    | Some f ->
      Some (name, <:html<<table class="formula-wrap">
        <tr>$html_of_formula html_of_namevf f$</tr>
      </table>&>>)
  in
  let pkg_depends = mk_formula "Dependencies"
      (fun opam -> filter_deps (OpamFile.OPAM.depends opam)) in
  let pkg_depopts = mk_formula "Optional dependencies"
      (fun opam -> filter_deps (OpamFile.OPAM.depopts opam)) in
  let html_of_revdeps title revdeps =
    let deps = List.map (fun (dep_name, vdnf) ->
      let vf = OpamfuFormula.(simplify_expr (expr_of_version_dnf vdnf)) in
      let vset = vset_of_name dep_name in
      let vvset = OpamfuFormula.filter_versions vf vset in
      let name_html = <:html<
        <tr><td colspan='3'>$html_of_name dep_name vvset$</td></tr>
      >> in
      let rec html_of_vf span = OpamfuFormula.(function
        | None | Some ((And []) | (Or [])) -> []
        | Some (Atom vc) ->
          [html_of_vc ["colspan",string_of_int span] dep_name vset vc]
        | Some (And ((Atom vc)::cl)) ->
          let rows = 1 + List.length cl in
          (<:html<
            <th class="operator" rowspan=$int:rows$>$html_conj$</th>
            $html_of_vc ["colspan",string_of_int (span - 1)] dep_name vset vc$
          >>)::(List.fold_right (fun c l ->
            l @ (html_of_vf (span - 1) (Some c))
          ) cl [])
        | Some (Or (d::dl)) ->
          let rows = OpamfuFormula.expr_width (Or (d::dl)) in
          begin match html_of_vf (span - 1) (Some d) with
          | h::t -> (<:html<
            <th class="operator" rowspan=$int:rows$>$html_disj$</th>
            $h$
          >>)::(List.fold_right (fun d l ->
            l @ (html_of_vf (span - 1) (Some d))
          ) dl t)
          | [] -> []
          end
      ) in
      let attrs = ["class","embedded-formula"] in
      <:html<$name_html$$list:List.map (enrow ~attrs) (html_of_vf 3 vf)$>>
    ) revdeps
    in
    match deps with
    | [] -> []
    | _ -> <:html<
             <tr class="well">
             <th colspan='3'>$str: title$</th>
             </tr>
           >> :: deps
  in

  let requiredby =
    try OpamPackage.Map.find pkg universe.rev_depends
    with Not_found -> OpamPackage.Name.Map.empty in
  let requiredby_deps = OpamPackage.Name.Map.bindings requiredby in
  let requiredby_html =
    html_of_revdeps "Required by" requiredby_deps in

  let usedby =
    try OpamPackage.Map.find pkg universe.rev_depopts
    with Not_found -> OpamPackage.Name.Map.empty in
  let usedby_deps = OpamPackage.Name.Map.bindings usedby in
  let usedby_html =
    html_of_revdeps "Used by" usedby_deps in

  let norevdeps = <:html< <tr><td>No package is dependent</td></tr>&>> in

  let pkg_compiler = match OpamFile.OPAM.ocaml_version pkg_opam with
    | None -> None
    | Some cvf ->
      let formula_str = OpamFormula.(string_of_formula (fun (relop,v) ->
        (string_of_relop relop)^" "^(OpamCompiler.Version.to_string v)
      ) cvf) in
      Some ("OCaml",<:html<$str:formula_str$>>)
  in

  let pkg_os = OpamFormula.(
    match OpamFile.OPAM.os pkg_opam with
    | Empty -> None
    | f ->
      let formula_str = string_of_formula (fun (b,s) ->
        if b then s else "!"^s
      ) f in
      Some ("OS",<:html<$str:formula_str$>>)
  ) in

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
    <h2>$str: pkg_info.name$</h2>

    <div class="row">
      <div class="span9">
        <div>
          <ul class="nav nav-pills">
            $list: version_links$
          </ul>
        </div>

        <table class="table">
          <tbody>
            $mk_tr pkg_author$
            $mk_tr pkg_license$
            $mk_tr pkg_homepage$
            $mk_tr pkg_issues$
            $mk_tr pkg_tags$
            $mk_tr pkg_maintainer$
            $mk_tr pkg_depends$
            $mk_tr pkg_depopts$
            $mk_tr pkg_compiler$
            $mk_tr pkg_os$
            <tr>
              <th>Published</th>
              <td>
                $str: pkg_published$
              </td>
            </tr>
            $pkg_url$
            $pkg_stats$
            $pkg_edit$
          </tbody>
        </table>

        <div class="well">$pkg_info.descr$</div>

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
