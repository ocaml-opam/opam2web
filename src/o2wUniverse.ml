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
open Cow
open O2wTypes

let to_page ~statistics universe pkg pkg_info acc =
  try
    let page = {
      page_source   = pkg_info.OpamfUniverse.name;
      page_link     = Uri.to_string pkg_info.OpamfUniverse.href;
      page_link_text = pkg_info.OpamfUniverse.title;
      page_depth    = 3;
      page_contents = Template.serialize
        (O2wPackage.to_html ~statistics ~prefix:"../../" universe pkg_info);
      page_srcurl = None;
    } in
    page :: acc
  with e ->
    Printf.printf "Skipping %s (%s)\n%!" (OpamPackage.to_string pkg)
      (Printexc.to_string e);
    Printexc.print_backtrace stdout;
    acc

(* Create a list of package pages to generate for a universe *)
let to_pages ~statistics ~prefix universe =
  let projects = OpamPackage.Name.Map.fold (fun name max_v acc ->
    let pkg  = OpamPackage.create name max_v in
    let info = OpamPackage.Map.find pkg universe.pkgs_infos in
    let name = OpamPackage.Name.to_string name in
    let href = Filename.(concat prefix (concat name "")) in
    let page = {
      page_source   = name;
      page_link     = href;
      page_link_text = name;
      page_depth    = 2;
      page_contents = Template.serialize
        (O2wPackage.to_html ~statistics ~prefix:"../" universe info);
      page_srcurl = None;
    } in
    page :: acc
  ) universe.max_versions [] in
  OpamPackage.Map.fold
    (to_page ~statistics universe) universe.pkgs_infos projects

let sortby_links ~links ~default ~active =
  let mk_item title =
    let href =
      if title = default
      then Uri.of_string "./"
      else Uri.of_string ("index-"^(String.lowercase title)^".html")
    in
    let ahref =
      Html.a ~href (Html.string "sort by " @ Html.string title)
    in
    Html.li ahref ?cls:(if title = active then Some "active" else None)
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
    Html.Create.table repos
      ~row:(fun r -> [Html.string (OpamRepository.to_string r)])
  in
  let packages_html =
    List.fold_left (fun acc pkg ->
        let info =
          try Some (OpamPackage.Map.find pkg universe.pkgs_infos)
          with Not_found -> None in
        match info with
        | None          -> acc
        | Some pkg_info ->
          let pkg_name = OpamPackage.name pkg in
          let pkg_download =
            try
              let d = OpamPackage.Name.Map.find pkg_name popularity in
              [Printf.sprintf "Downloads: %Ld" d]
            with Not_found -> []
          in
          let pkg_published = match pkg_info.published with
            | Some timestamp -> [
              Printf.sprintf "Published: %s"
                (O2wMisc.string_of_timestamp timestamp)
            ]
            | None -> []
          in
          let pkg_tooltip = String.concat " | " (pkg_download @ pkg_published) in
          let name = OpamPackage.Name.to_string pkg_name in
          let pkg_href = Uri.(resolve "http" (of_string "../packages/") (of_string name)) in
          Html.tag "tr"
            (Html.tag "td" ~attrs:["title", pkg_tooltip]
               (Html.a ~href:pkg_href (Html.string pkg_info.name))
             @ Html.tag "td" (Html.string pkg_info.version)
             @ Html.tag "td" (Html.string pkg_info.synopsis))
          :: acc)
      []
      (List.rev sorted_packages)
  in
  let template = Template.({ path="universe.xhtml"; fields=[
    "nav",   (default Html.empty, Optional);
    "repos", (mandatory (),       Optional);
    "pkgs",  (mandatory (),       Required);
  ]}) in
  Template.(generate content_dir template [
    "nav",   serialize (List.concat sortby_links_html);
    "repos", serialize repos_html;
    "pkgs",  serialize(Html.tag "tbody" (List.concat packages_html));
  ])

(** Generate a universe from a list of repositories *)
let of_repositories ?preds index repos =
  map O2wPackage.html_descr (of_repositories ?preds index repos)
