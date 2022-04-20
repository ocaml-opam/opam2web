#!/bin/sh

set -ex

if [ $1 = "" ]; then
  echo usage: $0 domain-name
  exit 1
fi

tar -C /var/lib/docker/volumes/opam-website/_data/ -cf - . | docker image import - opam-website-data
DOCKER_BUILDKIT=1 docker build -t opam-website-live -f Dockerfile.image .
docker kill opam-website-live || true
docker rm opam-website-live || true
docker run -d -p 80:80 -p 443:443 -v caddy-data:/data --name opam-website-live opam-website-live caddy file-server -domain $1
docker system prune -f
