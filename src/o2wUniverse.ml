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
open OpamfUniverse
open Cow.Html
open O2wTypes

let to_page ~statistics universe pkg pkg_info acc =
  match pkg_info with
  | None  ->
    Printf.printf "Skipping %s\n%!" (OpamPackage.to_string pkg);
    acc
  | Some pkg_info ->
    try
      let page = {
        page_link     = { Cow.Html.text=pkg_info.title;
                          href=Uri.to_string pkg_info.OpamfUniverse.href };
        page_depth    = 3;
        page_contents = Template.serialize
            (O2wPackage.to_html ~statistics universe pkg_info)
      } in
      page :: acc
    with e ->
      Printf.printf "Skipping %s (%s)\n%!" (OpamPackage.to_string pkg)
        (Printexc.to_string e);
      Printexc.print_backtrace stdout;
      acc

(* Create a list of package pages to generate for a universe *)
let to_pages ~statistics universe =
  OpamPackage.Map.fold
    (to_page ~statistics universe) universe.pkgs_infos []

let sortby_links ~links ~default ~active =
  let mk_item title =
    let href =
      if title = default
      then Uri.of_string "./"
      else Uri.of_string ("index-"^(String.lowercase title)^".html")
    in
    let ahref =
      <:html< <a href=$uri: href$>sort by $str: title$</a> >>
    in
    if title = active
    then <:html< <li class="active">$ahref$</li> >>
    else <:html< <li>$ahref$</li> >>
  in
  List.map mk_item links

(* Returns a HTML list of the packages in the given universe *)
let to_html ~content_dir ~sortby_links ~popularity ~active
    ~compare_pkg universe =
  let sortby_links_html = sortby_links ~active in
  let sorted_packages =
    let pkg_set = universe.max_packages in
    let pkg_set = match universe.index with
      | Index_all -> pkg_set
      | Index_pred -> OpamPackage.Set.filter
        (Pkg.are_preds_satisfied
           universe.pkgs_opams
           universe.pkg_idx
           universe.preds)
        pkg_set
    in
    let packages = OpamPackage.Set.elements pkg_set in
    List.sort compare_pkg packages
  in
  let repos_html =
    let repos = OpamRepository.sort universe.repos in
    List.map (fun r ->
      <:html< <tr><td>$str:OpamRepository.to_string r$</td></tr> >>
    ) repos
  in
  let packages_html =
    List.fold_left (fun acc pkg ->
        let info =
          try OpamPackage.Map.find pkg universe.pkgs_infos
          with Not_found -> None in
        match info with
        | None          -> acc
        | Some pkg_info ->
          let pkg_download =
            try
              let d = OpamPackage.Name.Map.find (OpamPackage.name pkg)
                popularity in
              Printf.sprintf "Downloads: %Ld | Published: %s"
                d (O2wMisc.string_of_timestamp pkg_info.published)
            with Not_found ->
              Printf.sprintf "Published: %s"
                (O2wMisc.string_of_timestamp pkg_info.published)
          in
          let pkg_href = Uri.(resolve "http"
                                (of_string "../") pkg_info.OpamfUniverse.href) in
          <:html<
            <tr>
              <td title=$str:pkg_download$>
               <a href=$uri:pkg_href$>
                 $str: pkg_info.name$
               </a>
              </td>
              <td>$str: pkg_info.version$</td>
              <td>$str: pkg_info.synopsis$</td>
            </tr>
          >> :: acc)
      []
      (List.rev sorted_packages)
  in
  let template = Template.({ path="universe.xhtml"; fields=[
    "nav",   (default <:html< >>, Optional);
    "repos", (mandatory (),       Optional);
    "pkgs",  (mandatory (),       Required);
  ]}) in
  Template.(generate content_dir template [
    "nav",   serialize <:html< $list: sortby_links_html$ >>;
    "repos", serialize <:html< <tbody> $list: repos_html$ </tbody> >>;
    "pkgs",  serialize <:html< <tbody> $list: packages_html$ </tbody> >>;
  ])

(** Generate a universe from a list of repositories *)
let of_repositories ?preds index repos =
  map O2wPackage.html_descr (of_repositories ?preds index repos)
