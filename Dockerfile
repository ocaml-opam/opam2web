# syntax=docker/dockerfile:1.3
FROM ocaml/opam:alpine-3.20-ocaml-4.14 as build-opam2web
RUN sudo apk add g++ gmp-dev
COPY --chown=opam:opam . /home/opam/opam2web
WORKDIR /home/opam/opam2web
ENV OCAMLRUNPARAM b
RUN sudo mkdir -p /opt/opam2web && sudo chown opam:opam /opt/opam2web
RUN sudo mv /usr/bin/opam-2.3 /usr/bin/opam && opam update
ARG OPAM_REPO_GIT_SHA=master
RUN opam repo set-url default git+https://github.com/ocaml/opam-repository.git#${OPAM_REPO_GIT_SHA}
RUN opam option --global 'archive-mirrors+="https://opam.ocaml.org/cache"'
ENV OPAMSOLVERTIMEOUT 120
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

FROM ocaml/opam:alpine-3.20-ocaml-4.14 as build-opam-doc
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
RUN sudo mv /usr/bin/opam-2.3 /usr/bin/opam && opam update
RUN opam option --global 'archive-mirrors+="https://opam.ocaml.org/cache"'
RUN opam install odoc
ARG OPAM_GIT_SHA=master
RUN git clone https://github.com/ocaml/opam /home/opam/opam \
    && git -C /home/opam/opam checkout ${OPAM_GIT_SHA}
WORKDIR /home/opam/opam
RUN opam exec -- ./configure --with-vendored-deps --without-mccs && opam exec -- make lib-ext && opam exec -- make
RUN echo '(vendored_dirs src_ext)' >> dune
RUN opam exec -- make -C doc html man-html DUNE=dune
RUN sudo mkdir -p /opt/opam/doc && sudo chown -R opam:opam /opt/opam
RUN cp -r doc/html /opt/opam/doc/api
RUN cp -r doc/man-html /opt/opam/doc/man
RUN cp -r doc/pages/* /opt/opam/doc/

FROM ocaml/opam:archive
RUN apk add --update git curl rsync libstdc++ rdfind caddy
COPY --from=build-opam2web /opt/opam2web /usr/local
COPY --from=build-opam-doc /usr/bin/opam-dev /usr/local/bin/opam
COPY --from=build-opam-doc /opt/opam/doc /usr/local/share/opam2web/content/doc
COPY ext/key/opam-dev-team.pgp /www/opam-dev-pubkey.pgp
ADD bin/opam-web.sh /usr/local/bin
ARG DOMAIN=opam.ocaml.org
ARG OPAM_REPO_GIT_SHA=master
ARG BLOG_GIT_SHA=master
ARG OPAM_GIT_SHA=master
RUN echo ${OPAM_REPO_GIT_SHA} >> /www/opam_git_sha
RUN echo ${BLOG_GIT_SHA} >> /www/blog_git_sha
RUN echo ${OPAM_GIT_SHA} >> /www/doc_git_sha
RUN /usr/local/bin/opam-web.sh ${DOMAIN} ${OPAM_REPO_GIT_SHA} ${BLOG_GIT_SHA}
WORKDIR /srv
COPY Caddyfile /etc/caddy/Caddyfile
ENTRYPOINT ["caddy", "run", "--config", "/etc/caddy/Caddyfile", "--adapter", "caddyfile"]
