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

open OpamfUniverse
open O2wTypes

let packages_prefix = "packages"

(* OPAM website homepage *)
let to_html ~content_dir ~statistics ~popularity universe =
  let universe = { universe with
    max_packages = OpamPackage.Set.filter
      (Pkg.are_preds_satisfied
         universe.pkgs_opams universe.pkg_idx universe.preds)
      universe.max_packages
  } in
  let updates_last10 =
    let mk_update_li (pkg, update_tm) =
      let pkg_name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let pkg_version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
      let pkg_href = Pkg.href ~href_base:Uri.(of_string (packages_prefix^"/"))
        (OpamPackage.name pkg) (OpamPackage.version pkg) in
      let pkg_date = O2wMisc.string_of_timestamp ~short:true update_tm in
      <:html<
        <tr>
          <td>
            <a href=$uri: pkg_href$>$str: pkg_name$ $str: pkg_version$</a>
          </td>
          <td>$str: pkg_date$</td>
        </tr>
      >>
    in
    let dates_fn pkg =
      try OpamPackage.Map.find pkg universe.pkgs_dates
      with Not_found -> 0. in
    let last_updates =
      O2wStatistics.top_packages ~reverse:true ~ntop:10
        dates_fn universe.max_packages in
    let updated_items = List.map mk_update_li last_updates in
    <:html<
      <div class="span4">
        <table class="table table-striped">
          <thead>
            <tr><th colspan="2">New packages</th></tr>
          </thead>
          <tbody>
            $list: updated_items$
            <tr>
              <td class="btn-more" colspan="2">
                <a href=$str:packages_prefix^"/index-date.html"$>
                  <button class="btn btn-small">all packages</button>
                </a>
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    >>
  in

  let nb_packages, packages_top10 = match statistics with
    | None      -> OpamPackage.Set.cardinal universe.max_packages, <:html< >>
    | Some sset ->
      let mk_top_li (pkg, pkg_count) =
        let name = OpamPackage.name pkg in
        let version = OpamPackage.version pkg in
        let pkg_name = OpamPackage.Name.to_string name in
        let pkg_href = Pkg.href ~href_base:Uri.(of_string (packages_prefix^"/"))
          name version in
        <:html<
          <tr>
            <td>
              <a href=$uri: pkg_href$>$str: pkg_name$</a>
            </td>
            <td>$str: Int64.to_string pkg_count$</td>
          </tr>
        >>
      in
      let popularity_fn pkg =
        try OpamPackage.Name.Map.find (OpamPackage.name pkg) popularity
        with Not_found -> 0L in
      let packages = universe.max_packages in
      let nb_packages = OpamPackage.Set.cardinal packages in
      let top10_pkgs = O2wStatistics.top_packages ~ntop: 10 popularity_fn packages in
      let top10_items = List.map mk_top_li top10_pkgs in
      nb_packages,
      <:html<
        <div class="span4">
          <table class="table table-striped">
            <thead>
              <tr><th colspan="2">Most Downloaded Packages (this month)</th></tr>
            </thead>
            <tbody>
              $list: top10_items$
              <tr>
                <td class="btn-more" colspan="2">
                  <a href=$str:packages_prefix^"/index-popularity.html"$>
                   <button class="btn btn-small" type="button">all packages</button>
                  </a>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      >>
  in

  let mk_stats (title: string) (stats: statistics): Cow.Html.t =
    <:html<
      <table class="table table-condensed">
        <thead>
          <tr><th>$str: title$</th></tr>
        </thead>
        <tbody>
          <tr>
            <td>
              <i class="icon-user"> </i> <strong>$str: Int64.to_string stats.users_stats$</strong> users
            </td>
          </tr>
          <tr>
            <td>
              <i class="icon-th-large"> </i> <strong>$str: Int64.to_string stats.global_stats$</strong> package installations
            </td>
          </tr>
          <tr>
            <td>
              <i class="icon-refresh"> </i> <strong>$str: Int64.to_string stats.update_stats$</strong> repository updates
            </td>
          </tr>
        </tbody>
      </table>
    >>
  in

  (*let tag_cloud = *)

  let stats_html = match statistics with
    | None -> [ <:html< &>> ]
    | Some s -> [
        mk_stats "Last week" s.week_stats;
        mk_stats "Last month" s.month_stats;
(*        mk_stats "All-time" s.alltime_stats; *)
      ]
  in

  let number_of_packages nb packages =
    <:html<
      <div class="page-header text-center">
      <h2 class="text-error">$int:nb$ <small>$str:packages$</small></h2>
      </div>
    >> in
  let number_of_packages =
    let packages = match nb_packages with
      | 0
      | 1 -> "package"
      | _ -> "packages" in
    number_of_packages nb_packages packages in

  let stats = <:html<
                <div class="span4">
                  $number_of_packages$
                  $list: stats_html$
                </div>
              >> in

  let template = Template.({ path="home.xhtml"; fields=[
    "stats",          (mandatory (), Optional);
    "updates_last10", (mandatory (), Optional);
    "packages_top10", (mandatory (), Optional);
  ]}) in
  Template.(generate content_dir template [
    "stats",          serialize stats;
    "updates_last10", serialize updates_last10;
    "packages_top10", serialize packages_top10;
  ])
