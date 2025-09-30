opam2web
========

A tool to generate a website from an opam universe

This utility creates a static website from an opam universe, listing all
available packages and their details. A homepage and opam documentation is
included as well.

The latest release of opam2web is available via
[opam](http://opam.ocaml.org). To install, simply run:

```bash
opam install opam2web
```

### Prerequisties

Optionally create a local switch for the project:

``` bash
opam switch create . ocaml.4.14.0 --with-test
```

- re [github.com/ocaml/ocaml-re](https://github.com/ocaml/ocaml-re)
- uri [github.com/avsm/ocaml-uri](https://github.com/avsm/ocaml-uri)
- opam [github.com/ocaml/opam](https://github.com/ocaml/opam)
- cow [github.com/mirage/ocaml-cow](https://github.com/mirage/ocaml-cow)
- cmdliner [erratique.ch/software/cmdliner](http://erratique.ch/software/cmdliner)
- js_of_ocaml [ocsigen.org/js_of_ocaml](http://ocsigen.org/js_of_ocaml/)

### Build

`dune exec -- opam2web` will compile and run the utility directly, alternatively you can run:

```bash
dune build @all
```

and the binary will be located in `_build/install/default/bin/opam2web` after compilation.

At this point you'll either want to try a `docker` build or running the `opam2web` cli locally.

### Docker

The website generation for opam.ocaml.org uses a combination of `docker` and `ocurrent-deployer` to rebuild the site. 
To replicate the docker image run:

``` bash
DOCKER_BUILDKIT=1 docker build -t opam2web .
```

which uses the local Dockerfile and creates an image called `opam2web`. Note this image is rather large at over 18Gb and takes a while to build.
Once built it can be run as `docker run -p 127.0.0.1:8080:80/tcp opam2web --root /usr/share/caddy` and viewable on http://localhost:8080.

To run the image produced by deploy.ci.ocaml.org run (note the image is multi-arch with x86 and ARM64 support):

``` bash
docker run -p 127.0.0.1:8080:80/tcp --name opam2web ocurrent/opam.ocaml.org:live --root /usr/share/caddy
```

### CLI Usage

```bash
opam2web [options]* [repositories]*
```

Each repository is namespaced by its type:
- `path:*`
    A repository at the following local file system path.
- `local:*`
    The repository corresponding to a named opam remote.
- `opam:`
    The current local opam universe.

The order of repositories determines their priority when generating the
opam universe to use. Earlier repositories are higher priority.

If no repository is given, the current local opam universe is used.

Some available options are:
- `--output / -o [directory]`
    The directory where to write the generated HTML files.
- `--content / -c [direcory]`
    The directory where to find documentation and templates to use.
    Defaults to 'content'.
- `--where [comma-separated predicate list]`
    A package's satisfaction of all of the predicates in any of the
    lists implies generation.
- `--index (all|where)`
    Control which packages to include in the universe index.
- `--help / -help`
    Display the list of options.

Some available predicates are:
- `tag:*`
- `depopt`
- `not:*`
- `repo:*`
- `pkg:*`

For complete command-line configuration options, run

```bash
opam2web --help
```

#### Example

```bash
opam2web -o website path:~/myrepo local:default
```
will generate the HTML files corresponding to the repository located in 
`~/myrepo` and the remote named `default` in the local opam installation.
Resulting files will be located in the `website` directory.


## Deployment

The deployment uses [ocurrent-deployer][] to watch particular branches on this repository, plus the default branches of [opam-repository][] and [platform-blog][]. 
When any of them change, it calculates if it needs to rebuild the docker image. A site rebuild is therefore triggered by any merge to opam-repository.

 * `live` branch is deployed on [opam.ocaml.org][]
 * `staging` branch is deployed on [staging.opam.ocaml.org][]

The deployer service is available at https://deploy.ci.ocaml.org/?repo=ocaml-opam/opam2web and the code for the ocurrent pipeline is in [ocurrent-deployer]().

[ocurrent-deployer]: https://github.com/ocurrent/ocurrent-deployer
[opam-repository]: https://github.com/ocaml/opam-repository
[platform-blog]: https://github.com/ocaml/platform-blog
[opam.ocaml.org]: https://opam.ocaml.org
[staging.opam.ocaml.org]: https://staging.opam.ocaml.org
