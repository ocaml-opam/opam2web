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
  let top10_pkgs =
    match universe.name_popularity with
    | None -> None
    | Some sset ->
      let popularity_fn pkg =
        try OpamPackage.Name.Map.find pkg.name sset
        with Not_found -> 0L in
      let top =
        O2wStatistics.top_packages ~ntop: 10 popularity_fn latest_packages
      in
      Some top
  in
  let nb_packages = OpamPackage.Set.cardinal latest_packages in
  { nb_packages; last10_updates; top10_pkgs }

(* Opam website homepage *)
let to_html ~content_dir ~news ?statistics ds =
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

  let packages_top10 =
    match ds.top10_pkgs with
    | None -> Html.empty
    | Some top10_pkgs ->
      let mk_top_li (pkg, pkg_count) =
        let name = OpamPackage.name pkg in
        let pkg_name = OpamPackage.Name.to_string name in
        let pkg_href =
          O2wPackage.pkg_href ~href_base:Uri.(of_string (packages_prefix^"/")) pkg in
        Html.tag "tr"
          (Html.tag "td"
             (Html.a ~href:pkg_href (Html.string pkg_name))
           @ Html.tag "td" (Html.string(Int64.to_string pkg_count)))
      in
      let top10_items = List.map mk_top_li top10_pkgs in
      Html.div ~cls:"span4"
        (Html.tag "table" ~cls:"table table-striped"
           (Html.tag "thead"
              (Html.tag "tr"
                 (Html.tag "th" ~attrs:["colspan","2"]
                    (Html.string "Most Downloaded Packages (this month)")))
            @ Html.tag "tbody"
                (List.concat top10_items
                 @ Html.tag "tr"
                     (Html.tag "td" ~cls:"btn-more" ~attrs:["colspan","2"]
                        (let h = packages_prefix^"/index-popularity.html" in
                         Html.a ~cls:"btn btn-small" ~href:(Uri.of_string h)
                           (Html.string "all packages"))))))
  in

  let mk_stats (title: string) (stats: statistics): Cow.Html.t =
    let space = Html.string " " in
    let strong_int64 n = Html.strong (Html.string (Int64.to_string n)) in
    Html.tag "table" ~cls:"table table-condensed"
      (Html.tag "thead"
         (Html.tag "tr" (Html.tag "th" (Html.string title)))
       @ Html.tag "tbody"
           (Html.tag "tr"
              (Html.tag "td" (Html.i ~cls:"icon-user" space
                              @ space
                              @ strong_int64 stats.users_stats
                              @ Html.string " users"))
            @ Html.tag "tr" (Html.tag "td"
                               (Html.i ~cls:"icon-th-large" space
                                @ space
                                @ strong_int64 stats.global_stats
                                @ Html.string " package installations"))
            @ Html.tag "tr" (Html.tag "td"
                               (Html.i ~cls:"icon-refresh" space
                                @ space
                                @ strong_int64 stats.update_stats
                                @ Html.string " repository updates"))))
  in

  (*let tag_cloud = *)

  let stats_html = match statistics with
    | None -> [ Html.empty ]
    | Some s -> [
        mk_stats "Last week" s.week_stats;
        mk_stats "Last month" s.month_stats;
(*        mk_stats "All-time" s.alltime_stats; *)
      ]
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

  let stats = Html.div ~cls:"span4"
                (number_of_packages @ List.concat stats_html) in

  let template = Template.({ path="home.xhtml"; fields=[
    "news",           (mandatory (), Optional);
    "stats",          (mandatory (), Optional);
    "updates_last10", (mandatory (), Optional);
    "packages_top10", (mandatory (), Optional);
  ]}) in
  Template.(generate content_dir template [
    "news",           serialize news;
    "stats",          serialize stats;
    "updates_last10", serialize updates_last10;
    "packages_top10", serialize packages_top10;
  ])

let generate_json ds =
  let open O2wJson in
  (* top 10 packages *)
  let top10 =
    match ds.top10_pkgs with
    | None ->
      OpamConsole.error "No top10 packages statistics";
      `Null
    | Some top ->
      `List (List.map (fun (pkg,dl) ->
          `Assoc [ json_package pkg; json_downloads dl ]) top)
  in
  write "top10" top10;
  (* last 10 packages *)
  let last10_updates =
    `List (List.map (fun (pkg,tm) ->
        `Assoc [ json_package pkg; json_timestamp tm ]) ds.last10_updates)
  in
  write "last10_updates" last10_updates
