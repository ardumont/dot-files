#!/usr/bin/env bash

# Use: $0
# Destined to be called from deploy.sh in dot-files git repository

######### STATIC

REPO_DOTFILES=$(dirname $0)
FF_FOLDER=$HOME/.mozilla/firefox

if [ -d $FF_FOLDER ]; then
    ######### PREPARE

    bash ~/bin/deploy/install.sh haskell-platform

    mkdir -p $FF_FOLDER

    ######### DEPLOY

    FOLDER_USER_PROFILE_RELATIVE=$(grep "Path=" $FF_FOLDER/profiles.ini | tail -1 | cut -f2 -d'=')
    FOLDER_USER_PROFILE=$FF_FOLDER/$FOLDER_USER_PROFILE_RELATIVE
    FOLDER_FIREFOX_EXTENSIONS=$FOLDER_USER_PROFILE/extensions
    KEYSNAIL_EXTENSION=keysnail@mooz.github.com
    FOLDER_FIREFOX_EXTENSION_KEYSNAIL=$FOLDER_FIREFOX_EXTENSIONS/$KEYSNAIL_EXTENSION
    FOLDER_KEYSNAIL_EXTENSIONS=$FOLDER_USER_PROFILE/keysnail

    # Cleanup eventual old installation (hard-coded file and not a symlink)
    [ -d $FOLDER_FIREFOX_EXTENSION_KEYSNAIL -a ! -h $FOLDER_FIREFOX_EXTENSION_KEYSNAIL ] && rm -rf $FOLDER_FIREFOX_EXTENSION_KEYSNAIL
    [ -d $FOLDER_KEYSNAIL_EXTENSIONS -a ! -h $FOLDER_KEYSNAIL_EXTENSIONS ] && rm -rf $FOLDER_KEYSNAIL_EXTENSIONS

    # Reference the keysnail extension
    cp "$FOLDER_USER_PROFILE/extensions.ini" "$FOLDER_USER_PROFILE/extensions.bak.ini"

    [ "$(grep -c $FOLDER_FIREFOX_EXTENSIONS/$KEYSNAIL_EXTENSION $FOLDER_USER_PROFILE/extensions.ini)" != "1" ] &&
    runhaskell $REPO_DOTFILES/hs/LoadAndUpdateIni.hs "$FOLDER_USER_PROFILE/extensions.bak.ini" "$FOLDER_FIREFOX_EXTENSIONS/$KEYSNAIL_EXTENSION" > "$FOLDER_USER_PROFILE/extensions.ini"

    # Force link creation
    ln -nsf $REPO_DOTFILES/$KEYSNAIL_EXTENSION $FOLDER_FIREFOX_EXTENSIONS # install firefox extension keysnail
    ln -nsf $REPO_DOTFILES/keysnail $FOLDER_KEYSNAIL_EXTENSIONS           # install keysnail extensions
    ln -nsf $REPO_DOTFILES/.keysnail.js $HOME                             # install user's keysnail preference file
fi
