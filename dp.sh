#!/usr/bin/env bash

build () {
    cabal build
}

show () {
    ./site rebuild
    ./site preview
}

case "$1" in
    set )
        build;;
    show )
        show;;
    ss )
        build;
        show;;
    * )
    echo "oops";;
  esac

