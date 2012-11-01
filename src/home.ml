open O2w_common

(* OPAM website homepage *)
let to_html (repository: OpamPath.Repository.r)
    (all_statistics: statistics_set option) package_dates =

  let updates_last10 =
    let mk_update_li (pkg, update_tm) =
      let pkg_name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let pkg_version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
      let pkg_href = Printf.sprintf "pkg/%s.%s.html" pkg_name pkg_version in
      let pkg_date = string_of_timestamp update_tm in
      <:xml<
        <tr>
          <td>
            <a href="$str: pkg_href$">$str: pkg_name$</a>
          </td>
          <td>$str: pkg_date$</td>
        </tr>
      >>
    in
    let last_updates = Repository.last_packages ~nlast: 10 package_dates in
    let updated_items = List.map mk_update_li last_updates in
    <:xml<
      <div class="span3">
        <table class="table">
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

  let maintainers_top10 =
    let mk_top_li (name, npkg) =
      let npkg_str = string_of_int npkg in
      <:xml<
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
    <:xml<
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

  let packages_top10 = match all_statistics with
    | None -> <:xml< >>
    | Some sset ->
      let s = sset.alltime_stats in
      let mk_top_li (pkg, _) =
        let pkg_name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
        let pkg_version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
        let pkg_href = Printf.sprintf "pkg/%s.%s.html" pkg_name pkg_version in
        let pkg_popularity =
          try
            Int64.to_string (List.assoc pkg s.pkg_stats)
          with
            Not_found -> ""
        in
        <:xml<
          <tr>
            <td>
              <a href="$str: pkg_href$">$str: pkg_name$</a>
            </td>
            <td>$str: pkg_popularity$</td>
          </tr>
        >>
      in
      let top10_pkgs = Statistics.top_packages ~ntop: 10 s.pkg_stats in
      let top10_items = List.map mk_top_li top10_pkgs in
      <:xml<
        <div class="span3">
          <table class="table">
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

  let mk_stats (title: string) (stats: statistics)
      : Cow.Html.t =
    <:xml<
      <table class="table">
        <thead>
          <tr><th>$str: title$</th></tr>
        </thead>
        <tbody>
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

  let stats_html = match all_statistics with
    | None -> [ <:xml< &>> ]
    | Some s -> [
        mk_stats "Last 24 hours statistics" s.day_stats;
        mk_stats "Last week statistics" s.week_stats;
        mk_stats "All-time (unique IPs)" s.alltime_stats;
      ]
  in

  <:xml<
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
        <p>
          <a class="btn btn-primary btn-inverse btn-large"
              href="doc/Quick_Install.html">
            Download and install OPAM »
          </a>
          <a class="btn btn-primary btn-inverse btn-large"
              href="doc/Basic_Usage.html">
            How to use OPAM »
          </a>
        </p>
      </div>

      <!-- Example row of columns -->
      <div class="row">
        <div class="offset1 span3">
          <h2>News</h2>
          <ul>
            <li><strong>21/09/2012</strong> Version 0.7 is out</li>
            <li><strong>14/09/2012</strong> Talk at <a href="http://oud.ocaml.org/2012/">OUD 2012</a></li>
            <li><strong>11/09/2012</strong> Version 0.6 is out</li>
          </ul>
        </div>
        <div class="span3">
          <h2>Contribute</h2>
          <p class="contribute-element">Contribute to packages <button class="btn btn-large btn-block" type="button">
             <a href="https://github.com/OCamlPro/opam-repository" title="OCamlPro/opam-repository">OCamlPro/opam-repository</a>
          </button></p>
          <p class="contribute-element">Contribute to the installer <button class="btn btn-large btn-block" type="button">
             <a href="https://github.com/OCamlPro/opam" title="OCamlPro/opam">OCamlPro/opam</a>
          </button></p>
<!--
          <table class="table tooltip-packages">
            <tbody>
              <tr>
                <td><i class="icon-asterisk"> </i> <a href="#" rel="tooltip" data-original-title="'opam' package added">opam 0.4</a></td>
              </tr>
              <tr>
                <td><i class="icon-repeat"> </i> <a href="#" rel="tooltip" data-original-title="'ocsigen' package updated">ocsigen 2.2.2</a></td>
              </tr>
            </tbody>
          </table>
-->
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
        <div class="span3">
          $list: stats_html$
        </div>
        $updates_last10$
        $packages_top10$
        $maintainers_top10$
      </div>
  >>
