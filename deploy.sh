#!/bin/bash -xe

# This script deploys the links to your home:
# Use: /path/to/the/script/$0
# Example: ~/repo/perso/dot-files/deploy.sh
# The use of the absolute name for calling the script is unavoidable

## Functions

create-links-from-list() {
    for i in $*; do
        # destroy any existing file
        rm -f $HOME/$i
        # then create the link
        ln -nsf $REPO_DOTFILES/$i $HOME/
    done
}

## Main

REPO_DOTFILES=$(dirname $0)
REPO_WORK=~/work/

# standard files
FILES=".stumpwmrc .stumpwm-functions.lisp .profile .bashrc .bashrc-env .bashrc-path .bash_aliases .bashrc-prompt .bashrc-work .tmux.conf .ratpoisonrc .xmodmaprc .vimrc .keysnail.js .git-completion.bash .conkerorrc .sbclrc .xmonad quicklisp .ctags"

# some more specific work files
FILES_WORK=".bashrc-work"

# Simple dot-files

create-links-from-list $FILES

# possible work dot-files

if [ -d $REPO_WORK ]; then
    create-links-from-list $FILES_WORK
fi

# specific setup

## for clojure

mkdir -p $HOME/.lein
rm -f $HOME/.lein/profiles.clj
ln -nsf $REPO_DOTFILES/profiles.clj $HOME/.lein/

mkdir -p $HOME/.gradle
ln -nsf $REPO_DOTFILES/gradle.properties $HOME/.gradle/
ln -nsf $REPO_DOTFILES/init.gradle $HOME/.gradle/

mkdir -p $HOME/.sbt/plugins
ln -nsf $REPO_DOTFILES/.sbt/plugins/plugins.sbt ~/.sbt/plugins

## for scala

mkdir -p ~/project/plugins/
ln -nsf $REPO_DOTFILES/project/plugins/plugins.sbt $HOME/project/plugins

# global gitignore

ln -nsf $REPO_DOTFILES/.global-gitignore $HOME/.gitignore
