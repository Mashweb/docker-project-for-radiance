#!/bin/sh

PACKAGES="$*"

sbcl --eval '(ql:quickload :radiance)' \
     --eval '(setf (radiance-core:environment) "default")' \
     --eval "(handler-bind ((error (lambda (e) (princ e) (sb-ext:exit :code 1))))
               (ql:quickload '($PACKAGES))
               (sb-ext:exit :code 0))"
