(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  Opam is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open Cow
open O2wTypes

let packages_prefix = "packages"

let make_datasets universe =
  let latest_packages = O2wUniverse.latest_version_packages universe.st in
  let last10_updates =
    let dates_fn pkg =
      try OpamPackage.Map.find pkg universe.dates
      with Not_found -> 0. in
    O2wStatistics.top_packages ~reverse:true ~ntop:10
      dates_fn latest_packages
  in
  let nb_packages = OpamPackage.Set.cardinal latest_packages in
  { nb_packages; last10_updates }

(* Opam website homepage *)
let to_html ~content_dir ~news ds =
  let updates_last10 =
    let mk_update_li (pkg, update_tm) =
      let pkg_name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let pkg_version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
      let pkg_href = O2wPackage.pkg_href ~href_base:Uri.(of_string (packages_prefix^"/")) pkg in
      let pkg_date = O2wMisc.html_of_timestamp ~short:true update_tm in
      Html.tag "tr"
        (Html.tag "td"
           (Html.a ~href:pkg_href (Html.string pkg_name
                                   @ Html.string " "
                                   @ Html.string pkg_version))
         @ Html.tag "td" pkg_date)
    in
    let updated_items = List.map mk_update_li ds.last10_updates in
    Html.div ~cls:"span4"
      (Html.tag "table" ~cls:"table table-striped"
         (Html.tag "thead"
            (Html.tag "tr" (Html.tag "th" ~attrs:["colspan", "2"]
                              (Html.string "New packages")))
          @ Html.tag "tbody"
              (List.concat updated_items
               @ Html.tag "tr"
                   (Html.tag "td" ~cls:"btn-more" ~attrs:["colspan","2"]
                      (let h = packages_prefix^"/index-date.html" in
                        Html.a ~cls:"btn btn-small" ~href:(Uri.of_string h)
                          (Html.string "all packages"))))))
  in

  let number_of_packages nb packages =
    Html.div ~cls:"page-header text-center"
      (Html.h1 ~cls:"text-error" (Html.int nb @ Html.string " "
                                  @ Html.small (Html.string packages))) in
  let number_of_packages =
    let packages =
      match ds.nb_packages with
      | 0 | 1 -> "package"
      | _ -> "packages"
    in
    number_of_packages ds.nb_packages packages in

  let stats = Html.div ~cls:"span4" number_of_packages in

  let template = Template.({ path="home.xhtml"; fields=[
    "news",           (mandatory (), Optional);
    "stats",          (mandatory (), Optional);
    "updates_last10", (mandatory (), Optional);
  ]}) in
  Template.(generate content_dir template [
    "news",           serialize news;
    "stats",          serialize stats;
    "updates_last10", serialize updates_last10;
  ])

let generate_json ds =
  let open O2wJson in
  (* last 10 packages *)
  let last10_updates =
    `List (List.map (fun (pkg,tm) ->
        `Assoc [ json_package pkg; json_timestamp tm ]) ds.last10_updates)
  in
  write "last10_updates" last10_updates
