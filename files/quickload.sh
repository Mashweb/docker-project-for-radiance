#!/bin/sh

PACKAGES="$*"

sbcl --eval "(handler-bind ((error (lambda (e) (princ e) (sb-ext:exit :code 1))))
               (ql:quickload '($PACKAGES))
               (sb-ext:exit :code 0))"
