#!/bin/sh

set -uex

if [[ $# -eq 4 ]] ; then
    echo 'Usage: $0 BASEURL OPAM_REPO_GIT_SHA BLOG_GIT_SHA'
    exit 2
fi

BASEURL=$1
OPAM_REPO_GIT_SHA=$2
BLOG_GIT_SHA=$3

cd /www
# Checkout a specific commit as supplied by ocurrent-deployer pipeline.
git clone https://github.com/ocaml/opam-repository.git --single-branch --branch master opam-repository &&
    cd opam-repository &&
    git checkout ${OPAM_REPO_GIT_SHA} &&
    cd ..

mv opam-repository/* .
mv opam-repository/.git .
rm -rf opam-repository

# Append to the 'repo' file, and dispatch all non-standard versions
cat <<EOF >>repo
redirect: [
  "https://${BASEURL}/1.1" { opam-version < "1.2" }
  "https://${BASEURL}/1.2.0" { opam-version < "1.2.2" }
  "https://${BASEURL}/1.2.2" { opam-version < "2.0~" }
]
EOF
# Remove the archive-mirrors field to avoid duplication when 'opam admin cache' below will add archive-mirrors: "cache"
sed '/^archive-mirrors: /d' repo > repo.tmp
mv repo.tmp repo
# Adds archive-mirrors: "cache" to the repo file
opam admin cache --link=archives /cache
# Adds the 'stamp' fields to the repo file
opam admin index --minimal-urls-txt

cp -r /usr/local/share/opam2web/content /tmp/
git clone https://github.com/ocaml/platform-blog --single-branch --branch master /tmp/content/blog &&
    cd /tmp/content/blog &&
    git checkout ${BLOG_GIT_SHA} &&
    cd -

rm -rf /www/ext
mkdir -p /www/ext
cp -r -L /usr/local/share/opam2web/css /www/ext
cp -r -L /usr/local/share/opam2web/img /www/ext
cp -r -L /usr/local/share/opam2web/js /www/ext

if [ -r /logs/access.log ]; then
    STATS_ARG="--statistics=/logs/access.log"
else
    STATS_ARG=""
fi

opam2web \
  --content=/tmp/content \
  --blog=https://github.com/ocaml/platform-blog/blob/master \
  $STATS_ARG \
  --root=$BASEURL \
  --output=/www

# Add some redirects
ln -sf . /www/doc/2.0

