opam2web
========

A tool to generate a website from an OPAM repository

This utility creates a static website from an OPAM repository, listing all 
available packages and their details. A homepage and OPAM documentation is 
included as well.

### Prerequisties

- opam [github.com/OCamlPro/opam](https://github.com/OCamlPro/opam)
- cow [github.com/mirage/ocaml-cow](https://github.com/mirage/ocaml-cow)

### Build

To build the opam-web utility and then generate the static website, enter:

```bash
make
```

Other available operations are:
```bash
make build
make run
make clean
```

### Usage

opam2web (which for now is located in src/_build/opam2web.native after 
compilation) can be used this way:
```bash
opam2web [options]* [repositories]*
```

If no repository is given, 'default' will be used.

Available options are:
- --output / -o [directory]
    The directory where to write the generated HTML files
- --help / -help
    Display the list of options

#### Example

```bash
opam2web -o website default local
```
will generate the HTML files corresponding to the 'default' and 'local' 
repositories in the 'website' directory.


### TODO

- Drop package files in different directories for each repository
- Create the output dir if it doesn't exist, fails if it's a file
- Links between packages

- Add news
- Add updates
- Include documentation
- Use markdown for static pages (home, news...)
