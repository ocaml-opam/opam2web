(* OPAM website homepage *)

let static_html =
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
              href="https://github.com/OCamlPro/opam/wiki/Tutorial">
            Try OPAM (early version) »
          </a>
        </p>
      </div>

      <!-- Example row of columns -->
      <div class="row">
        <div class="span4">
          <h2></h2>
<!--
          <ul>
            <li><strong>31/08/2012</strong> Dummy news 2</li>
            <li><strong>30/08/2012</strong> Dummy news 1</li>
          </ul>
-->
        </div>
        <div class="span3">
          <h2></h2>
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
          <h2>Contribute</h2>
          <ul>
            <li><a href="https://github.com/OCamlPro/opam/issues" title="Issues - OCamlPro/opam">Report bugs</a></li>
            <li>Create packages</li>
            <li><a href="https://github.com/OCamlPro/opam" title="OCamlPro/opam">Submit patches</a></li>
          </ul>
          <p>Github repository: <a href="https://github.com/OCamlPro/opam" title="OCamlPro/opam">OCamlPro/opam</a></p>
        </div>
        <div class="span2">
          <img src="ext/img/camel_rider.png" alt="Camel Rider" />
        </div>
      </div>
  >>
