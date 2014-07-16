#!/usr/bin/env bash

REPO_PERSO=$1
CI=${2:"n"}

## emacs-live

[ ! -d $REPO_PERSO/emacs-live ] && git clone git@github.com:ardumont/emacs-live.git $REPO_PERSO/emacs-live

[ ! -f $HOME/.emacs.d ] && ln -nsf $REPO_PERSO/emacs-live $HOME/.emacs.d

## emacs-live-packs

if [ ! -d $REPO_PERSO/emacs-live-packs ]; then
    git clone git@github.com:ardumont/emacs-live-packs.git $REPO_PERSO/emacs-live-packs
    if [ $CI = "y" ]; then
        sed -e 's/git@github.com:/http:\/\/git@github.com\//g' -i .gitmodules
    fi
    git submodule update --init
fi

bash $REPO_PERSO/emacs-live-packs/deploy.sh
