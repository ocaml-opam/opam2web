opam2web
========

A tool to generate a website from an OPAM repository

This utility creates a static website from an OPAM repository, listing all 
available packages and their details. A homepage and OPAM documentation is 
included as well.

### Prerequisties

- opam [github.com/OCamlPro/opam](https://github.com/OCamlPro/opam)
- cow [github.com/mirage/ocaml-cow](https://github.com/mirage/ocaml-cow)

If you have opam installed:
```bash
opam install opam cow
```

### Build

To build the `opam2web` utility, enter:
```bash
make
```
The binary will be located in src/_build/opam2web.native after compilation.

To generate the static website corresponding to the `default` repository in the 
local OPAM installation, enter:
```bash
make run
```

### Usage

```bash
opam2web [options]* [repositories]*
```

If no repository is given, the current working directory is used.

Available options are:
- `--directory / -d [directory]`
    Generate a website with the repository located in a directory.
- `--local / -l [repository_name]`
    Generate a website with a repository in the local OPAM installation.
- `--output / -o [directory]`
    The directory where to write the generated HTML files
- `--help / -help`
    Display the list of options

#### Example

```bash
opam2web -o website ~/myrepo -l default
```
will generate the HTML files corresponding to the repository located in 
`~/myrepo` and the repository named `default` in the local OPAM installation.  
Resulting files will be located in the `website` directory.


### TODO

- Drop package files in different directories for each repository
- More complex news system (one page per news, Markdown...)
- More complex statistics (graphics over time...)
