#!/bin/sh -xe

cd "$(dirname "$0")"

CURRENT_EMACS=${EMACS:-$(which emacs)}

echo "*** Emacs version ***"
echo "CURRENT_EMACS = $CURRENT_EMACS"
"$CURRENT_EMACS" --version

$CURRENT_EMACS --version

make test
