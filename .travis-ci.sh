# Install OCaml and OPAM PPAs
case "$OCAML_VERSION" in
  4.00.1) ppa=avsm/ocaml40+opam11 ;;
  4.01.0) ppa=avsm/ocaml41+opam11 ;;
  4.02.0) ppa=avsm/ocaml41+opam11 ;;
  *) echo Unknown $OCAML_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time
sudo apt-get install -qq xdg-utils

export OPAMYES=1

opam init git://github.com/ocaml/opam-repository >/dev/null 2>&1

case "$OCAML_VERSION" in
  4.02.0)
    opam switch 4.02.0+rc1 ;;
  *)
    echo OCaml version
    ocaml -version ;;
esac

eval `opam config env`
export OPAMPREFIX=`opam config var prefix`

# opam-lib
opam install ocamlgraph cmdliner dose.3.2.2+opam cudf re ocamlfind
git clone https://github.com/ocaml/opam.git
cd opam
./configure
make
opam-installer --prefix=$OPAMPREFIX opam-lib.install
cd ..

# opamfu
opam install uri
git clone https://github.com/ocamllabs/opamfu.git
cd opamfu
make build
make install
cd ..

opam install cow js_of_ocaml
make
#make test
#make fulltest
make PREFIX=$OPAMPREFIX install

# Test installation
#echo "let x = O2wProject.pkg_href" > foo.ml
#ocamlfind ocamlc -package opam2web -predicates byte -linkpkg foo.ml
#./a.out
