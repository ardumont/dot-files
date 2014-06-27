#!/bin/bash

REPO_PERSO=$1
CI=${2-"n"}

## prelude

[ ! -d $REPO_PERSO/prelude ] && git clone git@github.com:ardumont/prelude.git $REPO_PERSO/prelude

cd $REPO_PERSO/prelude && git checkout personal && cd -

[ ! -f $HOME/.emacs.d ] && ln -nsf $REPO_PERSO/prelude $HOME/.emacs.d

## prelude-packs

if [ ! -d $REPO_PERSO/prelude-packs ]; then
    git clone git@github.com:ardumont/prelude-packs.git $REPO_PERSO/prelude-packs
    if [ $CI = "y" ]; then
        sed -e 's/git@github.com:/http:\/\/git@github.com\//g' -i .gitmodules
    fi
    git submodule update --init
fi

bash $REPO_PERSO/prelude-packs/deploy.sh
