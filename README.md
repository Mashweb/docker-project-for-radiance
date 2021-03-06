# Docker Project for an Instance of the Radiance Web Application Environment

For a background on the Radiance web application environment, see
[the Radiance homepage](https://shirakumo.github.io/radiance-homepage/),
[the Radiance project](https://github.com/Shirakumo/radiance), and
[the Radiance tutorial](https://github.com/Shirakumo/radiance-tutorial/blob/master/Part%200.md).

## Notes for using the makefile on Linux

1. On Linux, which many users of this Docker project will be using, it is good to manage Docker as a non-root user.
See [Post-installation steps for Linux](https://docs.docker.com/engine/install/linux-postinstall/) on the Docker website.
2. The `deploy` stage requires a passwordless login to the VPS or private server. The `ssh-copy-id` command can be used to set that up, but the command uses the user's password to connect to the host, which is a problem if the VPS or private server was already set up to allow _passwordless-only_ access from some other computer. For many Linux servers, one way to solve this problem is described in mjmare's answer to the question [ssh-copy-id not working Permission denied (publickey).](https://www.digitalocean.com/community/questions/ssh-copy-id-not-working-permission-denied-publickey) on DigitalOcean. Basically, this solution involves temporarily turning password authentication back on.
3. For clean output of the [script(1) command](https://man7.org/linux/man-pages/man1/script.1.html), it is useful to set the `TERM` variable to `dumb` in the terminal where `script` will be run: `TERM=dumb`.

## Building, running, stopping, publishing

The `Dockerfile` and `Makefile` in this Git project support the maintainance and use
of four stages of image and container.
(See [the overview of Docker multi-stage builds](https://docs.docker.com/develop/develop-images/multistage-build/).)
The "production" and "devolopment" images are built upon the "base" image.
The "samples" image is built upon the "development" image.
After cloning this Git project, `cd` into the working directory and
substitute the name of a Dockerfile stage, one of `base`, `development`, or `samples`,
in place of `%` in any of the following `make` commands:

    make %             # Build an image.

    make %-run         # Run an image in a new container.

    make stop          # Stop the last started container.

    make %-publish     # Publish an image.

For example:

    make samples-run   # Run the `samples` image.

    make base-publish  # Publish the `base` image to DockerHub.

### Dockerfile stages

#### `base` - just Radiance

Exposes:

| Port or Volume | Use |
| --- | --- |
| Port 8080 | HTTP port |
| Mountpoint /apps | Quicklisp's local-projects directory |
| Mountpoint /db | Radiance settings and database |

#### `development` - base image with swank

Exposes:

| Port | Use |
| ---  | --- |
| 4005 | swank |

#### `samples` - development image with Radiance's sample projects

### Makefile variables

| Variable | Use |
| --- | --- |
| APP | Extra system to load. If this parameter is not empty, the container |
| | will run `(ql:quickload <APP>)` after initializing. |
| HTTP_PORT | Host network port to make Radiance available on. |
| | Default: 8080 |
| SWANK_PORT | Host network port to make SWANK available on. |
| | Default: 4005 |
| DB_DIR | Path to the directory with Radiance's databases and settings. |
| | Default: db |
| APPS_DIR | Path to the directory with local asdf systems. |
| | Default: apps |

## Deploying

There are two options. Either use the makefile or use docker compose.

### Using the makefile

To deploy an image to a VPS container,
substitute the name of a Dockerfile stage, one of base, development, or samples, in place of `%` in the following `make` command:

    make %-deploy

Variables:

| Variable | Purpose |
| --- | --- |
| DROPLET_UNAME | username on the droplet |
| | Default: root |
| DROPLET_IP | droplet IP |
| | The droplet should have publickey authentication configured. |

For example:

    make samples-deploy DROPLET_UNAME=root DROPLET_IP=192.168.0.10

### Using `docker compose`

1. Clone this repo on the remove server.
2. Edit docker-compose.yaml to suit your needs (it's documented in the file itself).
3. Run `docker-compose up`.

## Scenarios

Assuming you have emacs with slime installed on localhost, and docker on both localhost and a remote server.

### Running locally

    make samples-run

Any stage can work with different image contents.

### Debugging locally

1. `make samples-run`
2. In emacs: `M-x slime-connect`

Only the `development` and `samples` stages support debugging with slime.

### Running on a VPS

1. Clone this repo on the VPS.
2. Edit `docker-compose.yaml`.
3. Run `docker-compose up`.

### Debugging on a VPS

1. Make sure the container you need to debug uses a development or samples image.
2. Make an ssh tunnel for slime: `ssh -fNL 4005:<remote IP>:4005 <remote IP>`.
3. Connect to the remote Lisp with slime: `M-x slime-connect`.
