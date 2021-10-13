
`make %` - build an image

`make %-run` - run an image

`make %-publish` - publish an image

`make stop` - stop the last started container

Where `%` is the name of a Dockerfile stage. One of `base`, `development`, `samples`, or `production`.

### Dockerfile stages

#### `base` - Common base stage for the rest

Exposes:

Port 8080 - HTTP

Volume /apps - quicklisp's local-projects

Volume /db - Radiance's settings and database

#### `development` - includes swank

Exposes:

Port 4005 - swank

#### `samples` - development image with Radiance's sample projects

#### `production` - Base image that runs a lisp system on startup

Parameters:

APP - name of the system to run

### Makefile parameters

APP - system to run for `production` stage

