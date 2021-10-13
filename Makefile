#SHELL := /bin/bash

# Image name
IMAGE          ?= radiance
# Image tag
TAG            ?= latest
# Container name
CONT           ?= radiance
# Container port
CONT_PORT      ?= 8080
# Docker Hub username
DHUB_UNAME     ?= tomelam
# Docker platforms
PLATFORMS      ?= linux/amd64,linux/arm64

help:
	@echo "Please choose one of the following targets:"
	@echo "build, run, stop, restart, deploy,"
	@echo "list, rm, rmi, clean"
	@echo "Options:"
	@echo "  STAGE - stage to build. 'base', 'development', 'samples', 'production'. By default builds production"
	@echo "  APP   - system to load for 'production' image"

build:
ifdef STAGE
	docker build --network host --target $(STAGE)   --build-arg APP=$(APP) -t $(IMAGE)-$(STAGE):$(TAG) .
else
	docker build --network host --target production --build-arg APP=$(APP) -t $(IMAGE):$(TAG) .
endif

run:
ifdef STAGE
	docker run --rm --name $(CONT) -d -p $(CONT_PORT):8080 $(IMAGE)-$(STAGE):$(TAG)
else
	docker run --rm --name $(CONT) -d -p $(CONT_PORT):8080 $(IMAGE):$(TAG)
endif

stop:
	@if ! docker stop $(CONT); then \
		echo $(CONT) is not running.; \
	else \
		echo $(CONT) stopped.; \
	fi;

restart: stop run

publish:
ifdef STAGE
	docker tag $(IMAGE)-$(STAGE):$(TAG) $(DHUB_UNAME)/$(IMAGE)-$(STAGE):$(TAG)
	docker push $(DHUB_UNAME)/$(IMAGE)-$(STAGE):$(TAG)
else
	docker tag $(IMAGE):$(TAG) $(DHUB_UNAME)/$(IMAGE):$(TAG)
	docker push $(DHUB_UNAME)/$(IMAGE):$(TAG)
endif

publish_multi:
ifdef STAGE
	docker buildx build \
	    --platform $(PLATFORMS) \
	    --target production \
	    --network host \
	    -t $(DHUB_UNAME)/$(IMAGE)-$(STAGE):$(TAG) . --push
else
	docker buildx build \
	    --platform $(PLATFORMS) \
	    --target production \
	    --network host \
	    -t $(DHUB_UNAME)/$(IMAGE):$(TAG) . --push
endif

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
