#SHELL := /bin/bash

# App name for 'production' image
#    This is the name of docker's build target.
APP         ?=

# Stage name; one of: base, development, samples
#    Each is the base of the next, base is the base of development and
#    production is the base of samples. (See the Dockerfile and
#    https://docs.docker.com/develop/develop-images/multistage-build/ )

STAGE       ?= base
# Image name
IMAGE       ?= radiance
# Image tag
TAG         ?= latest
# Container name
CONT        ?= radiance
# Container port
HTTP_PORT   ?= 8080
# SWANK port
SWANK_PORT  ?= 4005
# Docker Hub username
DHUB_UNAME  ?= tomelam
# Docker platforms
PLATFORMS   ?= linux/amd64,linux/arm64
# Path to db/settings volume
DB_VOLUME   ?= db
# Path to local-projects volume
APPS_VOLUME ?= apps

# All stages
STAGES          = base development samples

## Example uses of this makefile:
##
##   make development-run
##   make samples-run

help:
	@echo "Please choose one of the following targets:"
	@echo "STAGE, STAGE-run, stop, STAGE-publish, STAGE-publish-multi"
	@echo "create_buildkit, list, rm, rmi, clean, distclean"
	@echo "Options:"
	@echo "  APP         - system to load for 'production' image"
	@echo "  HTTP_PORT   - host network port to make Radiance available on. Default: 8080"
	@echo "  SWANK_PORT  - host network port to make SWANK available on. Default: 4005"
	@echo "  DB_VOLUME   - path to the directory with Radiance's databases and settings. Default: ./db"
	@echo "  APPS_VOLUME - path to the directory with local asdf systems. Default: ./apps"

build-all: $(STAGES)

$(STAGES):%: Dockerfile Makefile files/*.sh files/.sbclrc
	docker build --target $@   --build-arg APP=$(APP) -t $(IMAGE)-$@:$(TAG) .

$(STAGES:=-run):%-run: %
	docker run --rm --name $(CONT) -d \
		-p $(SWANK_PORT):4005 -p $(HTTP_PORT):8080 \
		-v $(DB_VOLUME):/db -v $(APPS_VOLUME):/apps \
		$(IMAGE)-$<:$(TAG)

stop:
	@if ! docker stop $(CONT); then \
		echo $(CONT) is not running.; \
	else \
		echo $(CONT) stopped.; \
	fi;

$(STAGES:=-publish):%-publish: %
	docker tag $(IMAGE)-$@:$(TAG) $(DHUB_UNAME)/$(IMAGE)-$<:$(TAG)
	docker push $(DHUB_UNAME)/$(IMAGE)-$@:$(TAG)

$(STAGES:=-publish-multi):%-publish-multi: %
	docker buildx build \
	    --platform $(PLATFORMS) \
	    --target production \
	    -t $(DHUB_UNAME)/$(IMAGE)-$<:$(TAG) . --push

create_buildkit:
	docker buildx create --use

list:
	docker images
	@echo
	docker ps

rm:
	docker rm $(CONT)

rmi:
	docker rmi $(IMAGE)

clean:
	docker image prune -af

distclean: stop rm rmi clean

