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

# Cleanup eventual old folder
[ -d $FOLDER_KEYSNAIL -a ! -h $FOLDER_KEYSNAIL ] && rm -rf $FOLDER_KEYSNAIL

# Reference the keysnail extension
cp "$FOLDER_DESTINATION/extensions.ini" "$FOLDER_DESTINATION/extensions.bak.ini"
runhaskell $REPO_DOTFILES/hs/LoadAndUpdateIni.hs "$FOLDER_DESTINATION/extensions.bak.ini" "$FOLDER_KEYSNAIL/extensions/keysnail@mooz.github.com" > "$FOLDER_DESTINATION/extensions.ini"

# Force link creation
ln -nsf $REPO_DOTFILES/keysnail $FOLDER_KEYSNAIL
ln -nsf $REPO_DOTFILES/.keysnail.js $HOME
