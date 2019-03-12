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
FILES=".profile .bashrc .shrc .shrc-env .shrc-path .shrc-aliases .shrc-prompt .shrc-completion .tmux.conf .conkerorrc .conkeror .Xmodmap .zshrc .xmobarrc .Xresources .urxvt .layouts .xinit .zshenv .nix .devscripts .tigrc"

create-links-from-list $REPO_DOTFILES $HOME $FILES

# specific setup

# gtk themes
mkdir -p $HOME/.themes

for i in $($REPO_DOT_FILES/.themes); do
    ln -nsf $REPO_DOTFILES/.themes/$i $HOME/.themes/
done

# systemd user service
mkdir -p $REPO_DOTFILES/.config/
ln -nsf $REPO_DOTFILES/.config/systemd $HOME/.config/
ln -nsf $REPO_DOTFILES/.config/nixpkgs $HOME/.config/

## org

REPO_PERSO=$REPO_DOTFILES/..

[ ! -d $REPO_PERSO/org ] && git clone git@github.com:ardumont/org.git $REPO_PERSO/org

ln -nsf $REPO_PERSO/org $HOME/

## bin

[ ! -d $REPO_PERSO/sh ] && git clone git@github.com:ardumont/sh.git $REPO_PERSO/sh

ln -nsf $REPO_PERSO/sh $HOME/bin

## oh-my-zsh

[ ! -d $REPO_PERSO/oh-my-zsh ] && git clone git@github.com:ardumont/oh-my-zsh.git $REPO_PERSO/oh-my-zsh

ln -nsf $REPO_PERSO/oh-my-zsh $HOME/.oh-my-zsh

## emacs

if [ ! -d $HOME/work ]; then
    mkdir -p $HOME/work
    ln -s $WDIR/work/.shrc $HOME/work/.shrc
    ln -s $WDIR/work/.emacs.d $HOME/work/.emacs.d
fi
