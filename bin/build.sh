#!/bin/sh

set -ex

docker system prune -f
docker build -t opam-archive .
docker volume rm opam-website -f
docker run -it -v opam-persist:/persist -v opam-website:/www opam-archive /usr/local/bin/opam-web.sh opam.ocaml.org
