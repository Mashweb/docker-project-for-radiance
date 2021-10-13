#### BASE image has sbcl, quicklisp, and Radiance
FROM alpine:latest AS base

# SBCL
RUN apk add --no-cache sbcl && adduser -h /home/radiance -D radiance && \
    ln -s /home/radiance/quicklisp/local-projects /apps && \
    ln -s /home/radiance/.config/radiance/default /db
USER radiance
WORKDIR /home/radiance

# Quicklisp
RUN wget https://beta.quicklisp.org/quicklisp.lisp && \
    echo '\
' | sbcl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql-dist:install-dist "http://dist.tymoon.eu/shirakumo.txt")' \
         --eval '(ql:quickload :ironclad)' \
         --eval '(ql:quickload :verbose)' \
         --eval '(ql:quickload :radiance)' \
         --eval '(setf (radiance-core:environment) "default")' \
         --eval '(radiance:startup)' \
         --eval '(sb-ext:quit)'
COPY files/.sbclrc /home/radiance/
COPY files/quickload.sh files/quickload-radiance-app.sh /usr/bin/

VOLUME /apps
VOLUME /db
EXPOSE 8080

ENTRYPOINT echo "!!!! base image only has Radiance's landing page. Use other stages to make this Dockerfile do something useful !!!!" && sbcl --radiance

#### Development image adds swank and an open port to connect to it
FROM base AS development

RUN quickload.sh swank
EXPOSE 4005
ENTRYPOINT sbcl --swank --radiance

#### Development image with Radiance's sample projects
FROM development AS samples

RUN quickload-radiance-app.sh plaster filebox keyword-reviews purplish reader

ENTRYPOINT sbcl --swank --radiance plaster filebox keyword-reviews purplish reader

#### Base image with configurable application to start
FROM base AS production

ARG APP
RUN test -n "$APP" || ( echo "!!!! APP argument is required for production images !!!!" && exit 1 )
ENTRYPOINT sbcl --radiance $APP
# Setup Radiance environment based on a build option
