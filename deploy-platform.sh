#!/usr/bin/env bash

WDIR=$(dirname $0)

# deploy dot-files (including repositories)
$WDIR/deploy.sh

# deploy platform installation
~/bin/deploy/deploy-platform.sh
