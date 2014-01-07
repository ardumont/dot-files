#!/bin/bash -x

# Use: $0
# Destined to be called from deploy.sh in dot-files git repository

######### STATIC

REPO_DOTFILES=$(dirname $0)
FF_FOLDER=$HOME/.mozilla/firefox

######### PREPARE

mkdir -p $FF_FOLDER

######### DEPLOY

FOLDER_DESTINATION_RELATIVE=$(grep "Path=" $FF_FOLDER/profiles.ini | tail -1 | cut -f2 -d'=')
FOLDER_DESTINATION=$FF_FOLDER/$FOLDER_DESTINATION_RELATIVE
FOLDER_KEYSNAIL=$FOLDER_DESTINATION/keysnail

# Cleanup old folder
[ -d $FOLDER_KEYSNAIL -a ! -h $FOLDER_KEYSNAIL ] && rm -rf $FOLDER_KEYSNAIL

# Force link creation
ln -nsf $REPO_DOTFILES/keysnail $FOLDER_KEYSNAIL
ln -nsf $REPO_DOTFILES/.keysnail.js $HOME
