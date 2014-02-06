#!/bin/bash

# compile (using ghc) Test.hs with Main.hs and run
# Usage:
# run-test-with-main.sh Test.hs Main.hs opts

MYTEMP="$(mktemp -d)"

trap 'kill $(jobs -pr) 2> /dev/null; exit 1' SIGINT SIGTERM
trap "rm -rf $MYTEMP" EXIT

cp "$1" "$MYTEMP/Test.hs"
cp "$2" "$MYTEMP/Main.hs"

(cd "$MYTEMP"; ghc $GHC_OPTIONS Main.hs) >&2

shift 2

"$MYTEMP/Main" "$@"

