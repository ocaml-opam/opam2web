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
          <p><button class="btn btn-large btn-block" type="button">
             <a href="https://github.com/OCamlPro/opam" title="OCamlPro/opam">OCamlPro/opam</a>
          </button></p>
          <p><button class="btn btn-large btn-block" type="button">
             <a href="https://github.com/OCamlPro/opam-repository" title="OCamlPro/opam-repository">OCamlPro/opam-repository</a>
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
            <li><a href="doc/Tutorial.html" title="Getting started with OPAM">Getting started</a></li>
            <li><a href="doc/Packaging.html" title="Creating OPAM packages">Create packages</a></li>
            <li><a href="https://github.com/OCamlPro/opam/issues" title="Issues - OCamlPro/opam">Report bugs</a></li>
          </ul>
        </div>
        <div class="span2">
          <img src="ext/img/camel_rider.png" alt="Camel Rider" />
        </div>
      </div>
  >>
