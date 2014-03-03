#!/bin/bash

REPO_PERSO=$1

## prelude

[ ! -d $REPO_PERSO/prelude ] && git clone git@github.com:ardumont/prelude.git $REPO_PERSO/prelude

cd $REPO_PERSO/prelude && git checkout personal && cd -

ln -nsf $REPO_PERSO/prelude $HOME/.emacs.d

## prelude-packs

[ ! -d $REPO_PERSO/prelude-packs ] && git clone --recurse-submodules git@github.com:ardumont/prelude-packs.git $REPO_PERSO/prelude-packs

$REPO_PERSO/prelude-packs/deploy.sh
