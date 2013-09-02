(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012-2013 OCamlPro                                     *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open O2wTypes

(* OPAM website homepage *)
let to_html ~href_prefix ~statistics ~popularity repo_info =
  let url str = href_prefix ^ str in
  let updates_last10 =
    let mk_update_li (pkg, update_tm) =
      let pkg_name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let pkg_version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
      let pkg_href =
        O2wPackage.href ~href_prefix (OpamPackage.name pkg) (OpamPackage.version pkg) in
      let pkg_date = O2wMisc.string_of_timestamp ~short:true update_tm in
      <:html<
        <tr>
          <td>
            <a href="$str: pkg_href$">$str: pkg_name$ $str: pkg_version$</a>
          </td>
          <td>$str: pkg_date$</td>
        </tr>
      >>
    in
    let dates_fn pkg =
      try OpamPackage.Map.find pkg repo_info.pkgs_dates
      with Not_found -> 0. in
    let last_updates =
      O2wStatistics.top_packages ~reverse:true ~ntop:10
        dates_fn repo_info.max_packages in
    let updated_items = List.map mk_update_li last_updates in
    <:html<
      <div class="span4">
        <table class="table table-striped">
          <thead>
            <tr><th colspan="2">Recent updates</th></tr>
          </thead>
          <tbody>
            $list: updated_items$
            <tr>
              <td class="btn-more" colspan="2">
                <a href=$str:url "pkg/index-date.html"$>
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
    | None      -> 0, <:html< >>
    | Some sset ->
      let mk_top_li (pkg, pkg_count) =
        let name = OpamPackage.name pkg in
        let version = OpamPackage.version pkg in
        let pkg_name = OpamPackage.Name.to_string name in
        let pkg_href = O2wPackage.href ~href_prefix name version in
        <:html<
          <tr>
            <td>
              <a href="$str: pkg_href$">$str: pkg_name$</a>
            </td>
            <td>$str: Int64.to_string pkg_count$</td>
          </tr>
        >>
      in
      let popularity_fn pkg =
        try OpamPackage.Name.Map.find (OpamPackage.name pkg) popularity
        with Not_found -> 0L in
      let packages = repo_info.max_packages in
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
                  <a href=$str:url "pkg/index-popularity.html"$>
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

  <:html<
      <!-- Main hero unit for a primary marketing message or call to action -->
      <div class="hero-unit">
        <h1>OCaml Package Manager</h1>
        <p>OPAM is a source-based package manager for OCaml. It supports multiple simultaneous
           compiler installations, flexible package constraints, and a Git-friendly development
           workflow. Managing your OCaml installation can be as simple as:</p>
        <pre class="prettyprint lang-sh linenums">
opam list            # List the available packages
opam install lwt     # Install LWT
opam update          # Update the package list
...
opam upgrade         # Upgrade the installed packages to their latest version
</pre>
        <br/>
        <br/>
        <div class="text-right">
        <div class="btn-group">
          <a class="btn btn-large"
              href=$str:url "doc/Quick_Install.html"$>
            Download and install OPAM »
          </a>
          <a class="btn btn-large"
              href=$str:url "doc/Basic_Usage.html"$>
            How to use OPAM »
          </a>
        </div>
        </div>
      </div>

      <!-- Example row of columns -->
      <div class="row">
        <div class="offset1 span4">
          <h2>News</h2>
          <p><i class="icon-ok"> </i> <strong>08/2013</strong> Package metadata are moving to <a href="https://github.com/OCamlPro/opam-repository/issues/955">CC0</a></p>
          <p><i class="icon-ok"> </i> <strong>14/03/2013</strong> Version 1.0 is out!<br/></p>
          <p><i class="icon-ok"> </i> <strong>15/01/2013</strong> Version 0.9 (BETA release) is out!<br/></p>
          <p><i class="icon-eye-open"> </i> <strong>14/09/2012</strong> Talk at <a href="http://oud.ocaml.org/2012/">OUD 2012</a>
                 [ <a href="http://gazagnaire.org/ocamlpro/oud-opam.pdf">slides</a>
                  | <a href="http://www.youtube.com/watch?v=ivLqeRZJTGs">video</a> ]</p>
        </div>
        <div class="span4">
          <h2>Contribute</h2>
          <p>
          <i class="icon-tag"> </i>
          <a href="https://github.com/OCamlPro/opam" title="OCamlPro/opam">Report</a> and
          <a href="https://github.com/OCamlPro/opam" title="OCamlPro/opam">ask for feature requests</a>
          the OPAM tool<br/>
          <i class="icon-tags"> </i>
          <a href="https://github.com/OCamlPro/opam-repository" title="OCamlPro/opam-repository">Report</a> packaging issues or
          <a href="https://github.com/OCamlPro/opam-repository" title="OCamlPro/opam-repository">request</a> new packages<br/></p>
          <p>
          <i class="icon-pencil"> </i> <a href="http://lists.ocaml.org/listinfo/platform">Address</a> general queries on the tool and packages<br/>
          <i class="icon-pencil"> </i> <a href="http://lists.ocaml.org/listinfo/opam-devel">Discuss</a> the tool internals<br/>
          </p>
        </div>

        <div class="span2 text-right">
          <h2>Tutorials</h2>
          <p><a href=$str:url "doc/About.html"$ title="Getting started with OPAM">Getting started</a></p>
          <p><a href=$str:url "doc/Quick_Install.html"$ title="Installing OPAM">Installing OPAM</a></p>
          <p><a href=$str:url "doc/Packaging.html"$ title="Creating OPAM packages">Creating Packages</a></p>
          <p><a href="https://github.com/OCamlPro/opam/raw/master/doc/dev-manual/dev-manual.pdf" title="Developer Manual for OPAM">Developer Manual</a></p>
        </div>
<!--
        <div class="span2">
          <img src=$str:url "ext/img/camel_rider.png"$ alt="Camel Rider" />
        </div>
-->
      </div>
      <hr />
      <div class="row">
        <div class="span4">
          $number_of_packages$
          $list: stats_html$
        </div>
        $updates_last10$
        $packages_top10$
      </div>
  >>
