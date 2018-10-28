#!/usr/bin/env bash

if [ `notmuch count tag:deleted` != 0 ]; then
    notmuch search --format=text0 --output=files tag:deleted | xargs -0 --no-run-if-empty rm
fi

if [ `notmuch count tag:spam` != 0 ]; then
    notmuch search --format=text0 --output=files tag:spam | xargs -0 --no-run-if-empty rm
fi
