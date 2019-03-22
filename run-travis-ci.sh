#!/bin/sh -xe

cd "$(dirname "$0")"

make test REPO_DOTFILES=/home/travis/build/ardumont/dot-files CI=y
