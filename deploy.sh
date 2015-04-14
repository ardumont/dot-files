#!/usr/bin/env bash

# This script deploys the links to your home:
# Use: /path/to/the/script/$0
# Example: ~/repo/perso/dot-files/deploy.sh
# The use of the absolute name for calling the script is unavoidable

### Setup from CLI

WITH_EMACS_ENV=${1-"prelude"}
CI=${2-"n"}

### Functions definition

create-links-from-list() {
    WDIR=$1 # folder from where the source files will be (working dir)
    DDIR=$2 # folder to where deploy the links to the files (deploy dir)
    shift
    shift

    # Create the destination folder if it does not exist
    mkdir -p $DDIR

    for i in $*; do
        [ -f $DDIR/$i ] && rm -f $DDIR/$i         # cleanup any previous existing directory

        [ -f $WDIR/$i -o -d $WDIR/$i ] && ln -sf $WDIR/$i $DDIR/ # then create the link
    done
}

### Main

REPO_DOTFILES=$(dirname $0)

# standard files (in home directory)
FILES=".xmonad.start .stumpwm.start .stumpwmrc .stumpwm-functions.lisp .profile .bashrc .shrc .shrc-env .shrc-path .shrc-aliases .shrc-prompt .tmux.conf .ratpoisonrc .vimrc .git-completion.bash .conkerorrc .sbclrc .xmonad .Xmodmap quicklisp .ctags .gitignore_global .zshrc Makefile .gitconfig .stalonetrayrc .FBReader .xscreensaver .gimpgimp-layout-1920x1080 .gimpgimp-rules .offlineimaprc .signature .signature2 .offlineimap-helpers.py .xmobarrc .Xresources .urxvt .layouts .nixpkgs .xinit .ghc .zshenv"

create-links-from-list $REPO_DOTFILES $HOME $FILES

# security files (in .gnupg directory)
SECURITY_FILES="gpg.conf gpg-agent.conf"

create-links-from-list $REPO_DOTFILES $HOME/.gnupg $SECURITY_FILES

# specific setup

## LightTable

mkdir -p $HOME/.config/LightTable
ln -nsf $REPO_DOTFILES/lighttable-settings $HOME/.config/LightTable/settings

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

## bin

[ ! -d $REPO_PERSO/sh ] && git clone git@github.com:ardumont/sh.git $REPO_PERSO/sh

ln -nsf $REPO_PERSO/sh $HOME/bin

## emacs

bash $REPO_DOTFILES/deploy-emacs.sh $REPO_PERSO $WITH_EMACS_ENV $CI

## oh-my-zsh

[ ! -d $REPO_PERSO/oh-my-zsh ] && git clone git@github.com:ardumont/oh-my-zsh.git $REPO_PERSO/oh-my-zsh

ln -nsf $REPO_PERSO/oh-my-zsh $HOME/.oh-my-zsh

## KeySnail

bash $REPO_DOTFILES/deploy-keysnail.sh

## emacs

ln -nsf $REPO_DOTFILES/.mc-lists.el ~/.emacs.d/

### Adjust system configuration

bash $WDIR/adjust-system-configuration.sh

[ ! -d $HOME/work ] && mkdir -p $HOME/work && ln -s $WDIR/.shrc-work $HOME/work/.shrc
