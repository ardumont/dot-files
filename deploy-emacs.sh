#!/bin/bash
# Use: $0 <PACKS>
# PACKS - live for emacs-live, anything else for prelude

WDIR=$(dirname $0)
REPO_PERSO=${1-"$HOME/repo/perso"}
EMACS=${2-"prelude"}
CI=${3-"n"}

[ "$EMACS" = "live" ] && $WDIR/deploy-emacs-live-packs.sh $REPO_PERSO $CI || $WDIR/deploy-emacs-prelude-packs.sh $REPO_PERSO $CI
