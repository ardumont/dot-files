#!/bin/bash -xe

# This script deploys the links to your home:
# Use: /path/to/the/script/$0
# Example: ~/repo/perso/dot-files/deploy.sh
# The use of the absolute name for calling the script is unavoidable

## Functions

create-links-from-list() {
    WDIR=$1 # folder from where the source files will be (working dir)
    DDIR=$2 # folder to where deploy the links to the files (deploy dir)
    shift
    shift

    for i in $*; do
        # destroy any existing file
        rm -f $DDIR/$i
        # then create the link
        ln -nsf $WDIR/$i $DDIR/
    done
}

## Main

REPO_DOTFILES=$(dirname $0)

# standard files
FILES=".stumpwmrc .stumpwm-functions.lisp .profile .bashrc .bashrc-env .bashrc-path .bash_aliases .bashrc-prompt .tmux.conf .ratpoisonrc .vimrc .git-completion.bash .conkerorrc .sbclrc .xmonad .Xmodmap quicklisp .ctags .gitignore_global .zshrc Makefile .gitconfig .xinitrc .stalonetrayrc"

create-links-from-list $REPO_DOTFILES $HOME $FILES

# specific setup

## clojure

mkdir -p $HOME/.lein
rm -f $HOME/.lein/profiles.clj
ln -nsf $REPO_DOTFILES/profiles.clj $HOME/.lein/

## gradle

mkdir -p $HOME/.gradle
ln -nsf $REPO_DOTFILES/gradle.properties $HOME/.gradle/
ln -nsf $REPO_DOTFILES/init.gradle $HOME/.gradle/

## sbt/scala

mkdir -p $HOME/.sbt/plugins
ln -nsf $REPO_DOTFILES/.sbt/plugins/plugins.sbt $HOME/.sbt/plugins

mkdir -p $HOME/project/plugins/
ln -nsf $REPO_DOTFILES/project/plugins/plugins.sbt $HOME/project/plugins

## org

REPO_PERSO=$REPO_DOTFILES/..

[ ! -d $REPO_PERSO/org ] && git clone git@github.com:ardumont/org.git $REPO_PERSO/org

ln -nsf $REPO_PERSO/org $HOME/

## sh

[ ! -d $REPO_PERSO/sh ] && git clone git@github.com:ardumont/sh.git $REPO_PERSO/sh

ln -nsf $REPO_PERSO/sh $HOME/bin

## emacs-live

[ ! -d $REPO_PERSO/emacs-live ] && git clone git@github.com:ardumont/emacs-live.git $REPO_PERSO/emacs-live

ln -nsf $REPO_PERSO/emacs-live $HOME/.emacs.d

## emacs-live-packs

[ ! -d $REPO_PERSO/emacs-live-packs ] && git clone git@github.com:ardumont/emacs-live-packs.git $REPO_PERSO/emacs-live-packs

$REPO_PERSO/emacs-live-packs/deploy.sh

## oh-my-zsh

[ ! -d $REPO_PERSO/oh-my-zsh ] && git clone git@github.com:ardumont/oh-my-zsh.git $REPO_PERSO/oh-my-zsh

ln -nsf $REPO_PERSO/oh-my-zsh $HOME/.oh-my-zsh

## KeySnail

$REPO_DOTFILES/deploy-keysnail.sh

## Work files

REPO_WORK=$HOME/work
FILES_WORK=".bashrc-work"

[ -d $REPO_WORK/ ] && create-links-from-list $REPO_WORK $HOME $FILES_WORK
