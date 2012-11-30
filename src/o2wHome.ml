(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
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
let to_html ~statistics ~dates ~popularities ~packages repository =

  let updates_last10 =
    let mk_update_li (pkg, update_tm) =
      let pkg_name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let pkg_version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
      let pkg_href = Printf.sprintf "pkg/%s.%s.html" pkg_name pkg_version in
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
      try OpamPackage.Map.find pkg dates
      with Not_found -> 0. in
    let last_updates = O2wStatistics.top_packages ~reverse:true ~ntop:10 dates_fn packages in
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
                <button class="btn btn-small" type="button">
                  <a href="pkg/index-date.html">all packages</a>
                </button>
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    >>
  in

(*  let maintainers_top10 =
    let mk_top_li (name, npkg) =
      let npkg_str = string_of_int npkg in
      <:html<
        <tr>
          <td>$str: name$</td>
          <td>$str: npkg_str$</td>
        </tr>
      >>
    in
    let top10_maintainers = Statistics.top_maintainers ~ntop: 10 repository in
    (* If a name is present, keep it and remove e-mail address, e.g.
       John Doe <john.doe@foobar.com> -> John Doe
       john.doe@foobar.com -> john.doe@foobar.com
     *)
    let truncate_email (full_name, n) =
      let blank_index =
        try
          String.rindex full_name ' '
        with
          Not_found -> String.length full_name
      in
      String.sub full_name 0 blank_index, n
    in
    let top10_names = List.map truncate_email top10_maintainers in
    let top10_items = List.map mk_top_li top10_names in
    <:html<
      <div class="span3">
        <table class="table">
          <thead>
            <tr><th colspan="2">Most active maintainers</th></tr>
          </thead>
          <tbody>
            $list: top10_items$
          </tbody>
        </table>
      </div>
    >>
  in
*)
  let packages_top10 = match statistics with
    | None -> <:html< >>
    | Some sset ->
      let mk_top_li (pkg, pkg_count) =
        let pkg_name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
        let pkg_version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
        let pkg_href = Printf.sprintf "pkg/%s.%s.html" pkg_name pkg_version in
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
        try OpamPackage.Name.Map.find (OpamPackage.name pkg) popularities
        with Not_found -> 0L in
      let packages = O2wPackage.unify_versions packages in
      let top10_pkgs = O2wStatistics.top_packages ~ntop: 10 popularity_fn packages in
      let top10_items = List.map mk_top_li top10_pkgs in
      <:html<
        <div class="span4">
          <table class="table table-striped">
            <thead>
              <tr><th colspan="2">Most popular packages</th></tr>
            </thead>
            <tbody>
              $list: top10_items$
              <tr>
                <td class="btn-more" colspan="2">
                  <button class="btn btn-small" type="button">
                    <a href="pkg/index-popularity.html">all packages</a>
                  </button>
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
        mk_stats "All-time" s.alltime_stats;
      ]
  in

  <:html<
      <!-- Main hero unit for a primary marketing message or call to action -->
      <div class="hero-unit">
        <h1>OCaml Package Manager</h1>
        <p>OPAM is a package manager for OCaml. Managing your OCaml installation can be as simple as:</p>
        <pre class="prettyprint lang-sh linenums">
opam install lwt     # Install lwt
opam switch 4.00.0   # Switch to OCaml 4.00.0 environment
opam pin lwt 2.3.2   # Mark version 2.3.2 to be used in place of the latest one
...
</pre>
        <br/>
        <br/>
        <div class="offset6 span6">
        <div class="btn-group">
          <a class="btn btn-large"
              href="doc/Quick_Install.html">
            Download and install OPAM »
          </a>
          <a class="btn btn-large"
              href="doc/Basic_Usage.html">
            How to use OPAM »
          </a>
        </div>
        </div>
        <br/>
        <br/>
      </div>

      <!-- Example row of columns -->
      <div class="row">
        <div class="offset1 span3">
          <h2>News</h2>
          <ul>
            <li><strong>14/11/2012</strong> Version 0.8 is out</li>
            <li><strong>21/09/2012</strong> Version 0.7 is out</li>
            <li><strong>14/09/2012</strong> Talk at <a href="http://oud.ocaml.org/2012/">OUD 2012</a>
               <p>[ <a href="http://gazagnaire.org/ocamlpro/oud-opam.pdf">slides</a>
                  | <a href="http://www.youtube.com/watch?v=ivLqeRZJTGs">video</a> ]</p>
              </li>
            <li><strong>11/09/2012</strong> Version 0.6 is out</li>
          </ul>
        </div>
        <div class="span3">
          <h2>Contribute</h2>
          <ul>
          <li>Contribute to the
             <a href="https://github.com/OCamlPro/opam-repository" title="OCamlPro/opam-repository">packages</a>.
          </li>
          <li>Contribute to the
             <a href="https://github.com/OCamlPro/opam" title="OCamlPro/opam">installer</a>.
          </li>
          <li>Contribute to this
             <a href="https://github.com/OCamlPro/opam2web" title="OCamlPro/opam">website</a>.
          </li>
        </ul>
        </div>

        <div class="span3">
          <h2>Tutorials</h2>
          <ul>
            <li><a href="doc/About.html" title="Getting started with OPAM">Getting started</a></li>
            <li><a href="doc/Packaging.html" title="Creating OPAM packages">Create packages</a></li>
            <li><a href="https://github.com/OCamlPro/opam/issues" title="Issues - OCamlPro/opam">Report bugs</a></li>
          </ul>
        </div>
        <div class="span2">
          <img src="ext/img/camel_rider.png" alt="Camel Rider" />
        </div>
      </div>
      <hr />
      <div class="row">
        <div class="span4">
          $list: stats_html$
        </div>
        $updates_last10$
        $packages_top10$
      </div>
  >>
