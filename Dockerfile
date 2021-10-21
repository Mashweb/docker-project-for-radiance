# vim: set expandtab
#### BASE image has SBCL, quicklisp, and Radiance
FROM alpine:edge AS base
# SBCL
RUN apk add --no-cache sbcl
RUN ln -s /root/quicklisp/local-projects /apps && \
    ln -s /root/.config/radiance/default /db

WORKDIR /root

# Quicklisp
ADD https://beta.quicklisp.org/quicklisp.lisp /tmp
RUN /usr/bin/sbcl --load '/tmp/quicklisp.lisp' \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql-util:without-prompting (ql-dist:install-dist "http://dist.tymoon.eu/shirakumo.txt"))' \
         --eval '(ql:quickload :ironclad)' \
         --eval '(ql:quickload :verbose)' \
         --eval '(ql:quickload :radiance)' \
         --eval '(setf (radiance-core:environment) "default")' \
         --eval '(radiance:startup)' \
         --eval '(sb-ext:quit)' && \
    rm /tmp/quicklisp.lisp
COPY files/.sbclrc files/init.lisp ./
COPY files/quickload.sh files/quickload-radiance-app.sh /usr/bin/

EXPOSE 8080

ENV APP ""
ARG DOMAIN
ENV DOMAIN $DOMAIN
CMD sbcl --load init.lisp && sbcl --radiance

#### Development image adds swank and an open port to connect to it
FROM base AS development

RUN quickload.sh swank
EXPOSE 4005
ARG DOMAIN
ENV DOMAIN $DOMAIN
CMD sbcl --load init.lisp && sbcl --swank --radiance

#### Development image with Radiance's sample projects
FROM development AS samples

RUN quickload-radiance-app.sh plaster filebox keyword-reviews purplish reader

ARG DOMAIN
ENV DOMAIN $DOMAIN
CMD sbcl --load init.lisp && sbcl --swank --radiance plaster filebox keyword-reviews purplish reader
