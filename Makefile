#SHELL := /bin/bash

# App name for 'production' image
APP            ?= 
# Stage name
STAGE          ?= production
# Image name
IMAGE          ?= radiance
# Image tag
TAG            ?= latest
# Container name
CONT           ?= radiance
# Container port
HTTP_PORT      ?= 8080
# SWANK port
SWANK_PORT     ?= 4005
# Docker Hub username
DHUB_UNAME     ?= tomelam
# Docker platforms
PLATFORMS      ?= linux/amd64,linux/arm64

# All stages
STAGES          = base development samples production

help:
	@echo "Please choose one of the following targets:"
	@echo "STAGE, STAGE-run, stop, restart, deploy,"
	@echo "list, rm, rmi, clean"
	@echo "Options:"
	@echo "  APP        - system to load for 'production' image"
	@echo "  HTTP_PORT  - host network port to make Radiance available on. Default: 8080"
	@echo "  SWANK_PORT - host network port to make SWANK available on. Default: 4005"

build-all: $(STAGES)

$(STAGES):%: Dockerfile Makefile files/*.sh files/.sbclrc
	docker build --network host --target $@   --build-arg APP=$(APP) -t $(IMAGE)-$@:$(TAG) .

$(STAGES:=-run):%-run: %
	docker run --rm --name $(CONT) -d -p $(SWANK_PORT):4005 -p $(HTTP_PORT):8080 $(IMAGE)-$<:$(TAG)

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
	    --network host \
	    -t $(DHUB_UNAME)/$(IMAGE)-$<:$(TAG) . --push

create_buildkit:
	docker buildx create --use --buildkitd-flags '--allow-insecure-entitlement network.host'

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
