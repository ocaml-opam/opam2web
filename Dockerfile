# syntax=docker/dockerfile:1.3
FROM ocaml/opam:alpine-3.15-ocaml-4.14 as build-opam2web
RUN sudo apk add g++ gmp-dev
RUN git clone https://github.com/ocaml/opam2web.git --depth 1 /home/opam/opam2web
WORKDIR /home/opam/opam2web
ENV OCAMLRUNPARAM b
RUN sudo mkdir -p /opt/opam2web && sudo chown opam:opam /opt/opam2web
RUN opam repo set-url default git+https://github.com/ocaml/opam-repository.git#${OPAM_REPO_GIT_SHA}
RUN opam install . --destdir /opt/opam2web
RUN cp -r content /opt/opam2web/share/opam2web/
RUN rm -rf /opt/opam2web/share/opam2web/lib
RUN rm -rf /opt/opam2web/share/opam2web/doc
# Add legacy docs (1.1, 1.2) statically
RUN git clone https://github.com/ocaml/opam.wiki.git --depth 1 -b old_wiki /opt/opam2web/share/opam2web/content/doc/1.1 \
    && rm -rf /opt/opam2web/share/opam2web/content/doc/1.1/.git
RUN git clone https://github.com/ocaml/opam --depth 1 -b 1.2 /tmp/opam-1.2 \
    && mv /tmp/opam-1.2/doc/pages /opt/opam2web/share/opam2web/content/doc/1.2 \
    && rm -rf /tmp/opam-1.2

FROM ocaml/opam:alpine-3.15-ocaml-4.14 as build-opam-doc
RUN sudo apk add cgit groff
RUN sudo mkdir -p /usr/local/bin \
    && echo -e '#!/bin/sh -e\n\
                echo\n\
                echo\n\
                echo "<!DOCTYPE html>"\n\
                echo "<HTML><HEAD><TITLE>$(basename $2 .1) manpage</TITLE></HEAD><BODY>"\n\
                /usr/lib/cgit/filters/html-converters/man2html <$2\n\
                echo "</BODY></HTML>\n' \
       | sudo tee /usr/local/bin/man2html \
    && sudo chmod a+x /usr/local/bin/man2html
RUN sudo mv /usr/bin/opam-2.1 /usr/bin/opam && opam update
RUN opam install odoc
RUN git clone https://github.com/ocaml/opam --single-branch --depth 1 --branch master /home/opam/opam
WORKDIR /home/opam/opam
RUN opam exec -- ./configure --with-vendored-deps --without-mccs && opam exec -- make lib-ext && opam exec -- make
RUN echo '(vendored_dirs src_ext)' >> dune
RUN opam exec -- make -C doc html man-html DUNE=dune
RUN sudo mkdir -p /opt/opam/doc && sudo chown -R opam:opam /opt/opam
RUN cp -r doc/html /opt/opam/doc/api
RUN cp -r doc/man-html /opt/opam/doc/man
RUN cp -r doc/pages/* /opt/opam/doc/

FROM --platform=linux/amd64 ocaml/opam:archive as opam-archive
FROM ocaml/opam.ocaml.org-legacy as opam-legacy
FROM alpine:3.15 as opam2web
RUN apk add --update git curl rsync libstdc++ rdfind
COPY --from=opam-legacy . /www
COPY --from=build-opam2web /opt/opam2web /usr/local
COPY --from=build-opam-doc /usr/bin/opam /usr/local/bin/opam
COPY --from=build-opam-doc /opt/opam/doc /usr/local/share/opam2web/content/doc
RUN --mount=type=bind,target=/cache,from=opam-archive rsync -aH /cache/cache/ /www/cache/
COPY ext/key/opam-dev-team.pgp /www/opam-dev-pubkey.pgp
ADD bin/opam-web.sh /usr/local/bin
ARG DOMAIN=opam.ocaml.org
ARG OPAM_REPO_GIT_SHA=master
ARG BLOG_GIT_SHA=master
RUN echo ${OPAM_REPO_GIT_SHA} >> /www/opam_git_sha
RUN echo ${BLOG_GIT_SHA} >> /www/blog_git_sha
RUN /usr/local/bin/opam-web.sh ${DOMAIN} ${OPAM_REPO_GIT_SHA} ${BLOG_GIT_SHA}

FROM caddy:2.5.2-alpine
WORKDIR /srv
COPY --from=opam2web /www /usr/share/caddy
ENTRYPOINT ["caddy", "file-server"]