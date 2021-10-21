# App name for 'production' image
#    This is the name of docker's build target.
APP         ?=

# Stage name; one of: base, development, samples
#    The "production" and "devolopment" images are built upon the
#    "base" image. The "samples" image is built upon the "development"
#    image. (See the Dockerfile and
#    <https://docs.docker.com/develop/develop-images/multistage-build/>.)
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
DB_DIR      ?= $(shell pwd)/db
# Path to local-projects volume
APPS_DIR    ?= $(shell pwd)/apps

# Digital Ocean droplet username
DROPLET_UNAME  ?= root
# Internet-exposed IP address of the Digital Ocean droplet
DROPLET_IP     ?= 164.90.211.183
# Droplet domain
DROPLET_DOMAIN ?= $(DROPLET_IP)

# All stages
STAGES          = base development samples

## Example uses of this makefile:
##
##   make development-run
##   make samples-run

help:
	@echo "Please choose one of the following targets (substituting the
	@echo "name of the stage for STAGE):"
	@echo "  STAGE, STAGE-run, stop, STAGE-publish, STAGE-publish-multi,"
	@echo "  create_buildkit, list, rm, rmi, clean, distclean"
	@echo
	@echo "Variables:"
	@echo "  APP        - system to load for 'production' image"
	@echo "  HTTP_PORT  - host network port to make Radiance available on. Default: 8080"
	@echo "  SWANK_PORT - host network port to make SWANK available on. Default: 4005"
	@echo "  DB_DIR     - path to the directory with Radiance's databases and settings. Default: db"
	@echo "  APPS_DIR   - path to the directory with local asdf systems. Default: apps"

build-all: $(STAGES)

$(STAGES):%: Dockerfile Makefile files/*.sh files/.sbclrc
	docker build --target $@ \
		--build-arg APP=$(APP) \
		-t $(IMAGE)-$@:$(TAG) .

$(STAGES:=-run):%-run: %
	docker run --rm --name $(CONT) -d \
		-p $(SWANK_PORT):4005 -p $(HTTP_PORT):8080 \
		--mount src=$(DB_DIR),target=/db,type=bind \
		--mount src=$(APPS_DIR),target=/apps,type=bind \
		$(IMAGE)-$<:$(TAG)

stop:
	@if ! docker stop $(CONT); then \
		echo $(CONT) is not running.; \
	else \
		echo $(CONT) stopped.; \
	fi;

$(STAGES:=-publish):%-publish: %
	docker tag $(IMAGE)-$<:$(TAG) $(DHUB_UNAME)/$(IMAGE)-$<:$(TAG)
	docker push $(DHUB_UNAME)/$(IMAGE)-$<:$(TAG)

$(STAGES:=-publish-multi):%-publish-multi: % create_buildkit
	docker buildx build \
		--platform $(PLATFORMS) \
		--target $< \
		-t $(DHUB_UNAME)/$(IMAGE)-$<:$(TAG) . --push

create_buildkit:
	docker buildx create --use

$(STAGES:=-deploy):%-deploy: % %-publish
	ssh $(DROPLET_UNAME)@$(DROPLET_IP) " \
	  docker pull $(DHUB_UNAME)/$(IMAGE)-$<:$(TAG); \
	  docker stop $(CONT); \
	  echo Stopped container $(CONT); \
	  docker rm $(CONT); \
	  echo Removed container $(CONT); \
	  mkdir -p $(DB_DIR) $(APPS_DIR); \
	  docker run --rm --name $(CONT) -d \
	    -p $(SWANK_PORT):4005 -p $(HTTP_PORT):8080 \
	    -e DOMAIN=$(DROPLET_DOMAIN) \
	    --mount src=$(DB_DIR),target=/db,type=bind \
	    --mount src=$(APPS_DIR),target=/apps,type=bind \
	    $(DHUB_UNAME)/$(IMAGE)-$<:$(TAG); \
	  echo Started container $(CONT) with $(DHUB_UNAME)/$(IMAGE)-$<:$(TAG); \
	"

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
