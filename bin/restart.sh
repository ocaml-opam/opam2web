#!/bin/sh

set -ex

if [ $1 = "" ]; then
  echo usage: $0 domain-name
  exit 1
fi

docker kill opam-website-live || true
docker run -d -p 80:80 -p 443:443 -v caddy-data:/data --name opam-website-live opam-website-live caddy file-server -domain $1
docker system prune -f
