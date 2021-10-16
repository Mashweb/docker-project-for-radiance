
## Building

`make %` - build an image

`make %-run` - run an image in a new container

`make %-publish` - publish an image

`make stop` - stop the last started container

Where `%` is the name of a Dockerfile stage. One of `base`, `development` or `samples`.

Examples:

`make samples-run` - run the `samples` image

`make base-publish` - publish `base` image to DockerHub

### Dockerfile stages

#### `base` - just Radiance

Exposes:

Port 8080 - HTTP

Mountpoint /apps - quicklisp's local-projects

Mountpoint /db - Radiance settings and database

#### `development` - base image with swank

Exposes:

Port 4005 - swank

#### `samples` - development image with Radiance's sample projects

### Makefile parameters

APP - extra system to load. If this parameter is not empty container will run `(ql:quickload <APP>)` after initializing.

HTTP\_PORT  - host network port to make Radiance available on. Default: 8080

SWANK\_PORT - host network port to make SWANK available on. Default: 4005

DB\_DIR     - path to the directory with Radiance's databases and settings. Default: db

APPS\_DIR   - path to the directory with local asdf systems. Default: apps

## Deploying

There are two options. Either use the makefile or use docker compose.

### Using the makefile

`make %-deploy` - deploy an image to a VPS container

Parameters:

DROPLET\_UNAME - username on the droplet. Default: root

DROPLET\_IP - droplet IP. Should have publickey authentication configured

Example:

`make samples-deploy DROPLET_UNAME=root DROPLET_IP=192.168.0.10`

### Using `docker compose`

1. Clone this repo on the remove server.
2. Edit docker-compose.yaml to suit your needs (it's documented in the file itself).
3. Run `docker-compose up`.

## Scenarios

Assuming you have emacs with slime installed on localhost, and docker on both localhost and a remote server.

### Running locally

1. `make samples-run`

Any stage will work with different image contents.

### Debugging locally

1. `make-samples-run`
2. In emacs: `M-x slime-connect`

Only development and samples stages support debugging with slime.

### Running on a VPS

1. Clone this repo on the VPS
2. Edit docker-compose.yaml
3. Run `docker-compose up`

### Debugging on a VPS

1. Make sure the container you need to debug uses a development or samples image
2. Make an ssh tunnel for slime: `ssh -fNL 4005:<remote IP>:4005 <remote IP>`
3. Connect to the remote lisp with slime: `M-x slime-connect`
