#!/bin/bash

# Usage:
# with-grep.sh hipspec-total "Proved prop_0" hipspec -au "$@"

trap 'kill $(jobs -pr) 2> /dev/null; echo; exit 1' SIGINT SIGTERM

testname="${!#}"
opts="${@:3:$(($#-3))}"

cp "reformatted/$1/$testname/Test.hs" "$MY_TEMP_DIR/Test.hs"

cd "$MY_TEMP_DIR"

$opts Test.hs | tee >(grep -q "$2"; echo $? > grep_result)

res="$?"
grepres="$(cat grep_result)"

if [[ -z "$grepres" ]]; then
	exit 1
else
	exit "$grepres"
fi

