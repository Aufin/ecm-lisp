#!/bin/sh

sbcl --dynamic-space-size 8192 --disable-ldb \
 --eval '(declaim (optimize (speed 3) (compilation-speed 0) (safety 1) (space 3)))'\
 --eval   '(asdf:load-system :ecm/make)' --eval "(ecm/make::make)"
