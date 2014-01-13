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
FILES=".stumpwmrc .stumpwm-functions.lisp .profile .bashrc .bashrc-env .bashrc-path .bash_aliases .bashrc-prompt .tmux.conf .ratpoisonrc .xmodmaprc .vimrc .git-completion.bash .conkerorrc .sbclrc .xmonad quicklisp .ctags .gitignore_global"

create-links-from-list $REPO_DOTFILES $HOME $FILES

# specific setup

## clojure

mkdir -p $HOME/.lein
rm -f $HOME/.lein/profiles.clj
ln -nsf $REPO_DOTFILES/profiles.clj $HOME/.lein/

mkdir -p $HOME/.gradle
ln -nsf $REPO_DOTFILES/gradle.properties $HOME/.gradle/
ln -nsf $REPO_DOTFILES/init.gradle $HOME/.gradle/

mkdir -p $HOME/.sbt/plugins
ln -nsf $REPO_DOTFILES/.sbt/plugins/plugins.sbt $HOME/.sbt/plugins

## scala

mkdir -p $HOME/project/plugins/
ln -nsf $REPO_DOTFILES/project/plugins/plugins.sbt $HOME/project/plugins

## KeySnail

$REPO_DOTFILES/deploy-keysnail.sh

# Work files

REPO_WORK=$HOME/work
FILES_WORK=".bashrc-work"

[ -d $REPO_WORK/ ] && create-links-from-list $REPO_WORK $HOME $FILES_WORK

## sh

[ ! -d $HOME/repo/perso/sh ] && git clone git@github.com:ardumont/sh.git $HOME/repo/perso/sh

ln -nsf $HOME/repo/perso/sh $HOME/bin

## oh-my-zsh

[ ! -d $HOME/repo/perso/oh-my-zsh ] && git clone git@github.com:ardumont/oh-my-zsh.git $HOME/repo/perso/oh-my-zsh

ln -nsf $HOME/repo/perso/oh-my-zsh $HOME/.oh-my-zsh
