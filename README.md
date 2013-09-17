opam2web
========

A tool to generate a website from an OPAM repository

This utility creates a static website from an OPAM repository, listing all 
available packages and their details. A homepage and OPAM documentation is 
included as well.

### Prerequisties

- re [github.com/ocaml/ocaml-re](https://github.com/ocaml/ocaml-re)
- opam [github.com/OCamlPro/opam](https://github.com/OCamlPro/opam)
- cow [github.com/mirage/ocaml-cow](https://github.com/mirage/ocaml-cow)
- cmdliner [erratique.ch/software/cmdliner](http://erratique.ch/software/cmdliner)

If you have opam installed:
```bash
opam install re opam-lib cow cmdliner
```

### Build

To build the `opam2web` utility, enter:
```bash
make
```
The binary will be located in src/_build/opam2web.native after compilation.

To generate the static website corresponding to the `default` remote in the
local OPAM installation, enter:
```bash
make -C src run
```

### Usage

```bash
opam2web [options]* [repositories]*
```

If no repository is given, the current working directory is used.

Some available options are:
- `--directory / -d [directory]`
    Generate a website with the repository located in a directory.
- `--remote [repository_name]`
    Generate a website with a remote in the local OPAM installation.
- `--output / -o [directory]`
    The directory where to write the generated HTML files
- `--where [comma-separated predicate list]`
    A package's satisfaction of all of the predicates in any of the lists implies generation
- `--help / -help`
    Display the list of options

Some available predicates are:
- `tag:*`

For complete command-line configuration options, run
```bash
opam2web --help
```

#### Example

```bash
opam2web -o website -d ~/myrepo --remote default
```
will generate the HTML files corresponding to the repository located in 
`~/myrepo` and the remote named `default` in the local OPAM installation.
Resulting files will be located in the `website` directory.

### TODO

- Merge each repository to generate a site of their ordered union
- More complex news system (one page per news, Markdown...)
- More complex statistics (graphics over time...)
