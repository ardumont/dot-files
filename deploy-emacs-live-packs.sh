#!/bin/bash

REPO_PERSO=$1

## emacs-live

[ ! -d $REPO_PERSO/emacs-live ] && git clone git@github.com:ardumont/emacs-live.git $REPO_PERSO/emacs-live

ln -nsf $REPO_PERSO/emacs-live $HOME/.emacs.d

## emacs-live-packs

[ ! -d $REPO_PERSO/emacs-live-packs ] && git clone git@github.com:ardumont/emacs-live-packs.git $REPO_PERSO/emacs-live-packs

$REPO_PERSO/emacs-live-packs/deploy.sh
