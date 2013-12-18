#!/bin/bash

# Usage: TIMEOUT=60 run.sh "command to run" "dir to store results"

if [[ -z "$1" ]]; then
	echo "I need a command to run"
	exit 1
fi

if [[ -z "$2" ]]; then
        echo "I need a directory to store results"
        exit 1
fi

if [[ -z "$TIMEOUT" ]]; then
	TIMEOUT="1h"
fi

if [[ -z "$TIMEFORMAT" ]]; then
	TIMEFORMAT="elapsed %e\nuser %U\nsystem %S\nmemory %K\nexit-code %x"
fi


echo -e "\n$1\n" >> /dev/stderr

export MY_TEMP_DIR="$2"

/usr/bin/time --quiet -f "$TIMEFORMAT" -o "$2/stat" \
timeout --kill-after=2 "$TIMEOUT" \
	bash -c "ulimit -Sv $MEM_LIMIT; $1" > >(tee "$2/out" >> /dev/stdout) 2> >(tee "$2/err" >> /dev/stderr)

#echo -e "exit-code $?" >> "$2/stat"

cat "$2/stat" >> /dev/stderr

