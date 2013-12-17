#!/bin/bash

TIMEOUT=60
REPEAT=1
REPEATALL=1
COMMAND=""
PARALLEL=1
MEM_LIMIT=4000000

while getopts "hvo:c:s:t:r:R:p:km:" opt; do
	case "$opt" in
	h)  echo "Usage: runset.sh -c ./target/start -o reports/report1 -s testset -t 60 -p 2 -- 'optset1' 'optset2' ..."
            exit 0
            ;;
	v)  VERBOSE=1
            ;;
	o)  OUTDIR="$OPTARG"
            ;;
	c)  COMMAND="$OPTARG"
	    ;;
	s)  TESTSET="$OPTARG"
	    ;;
	t)  TIMEOUT="$OPTARG"
	    ;;
	r)  REPEAT="$OPTARG"
	    ;;
	R)  REPEATALL="$OPTARG"
	    ;;
	p)  PARALLEL="$OPTARG"
	    ;;
	k)  KEEP_TEMPDIR=1
	    ;;
	m)  MEM_LIMIT="$OPTARG"
	    ;;
	esac
done

shift $((OPTIND-1))

if [[ -e "$OUTDIR" ]]; then
	echo "$OUTDIR exists"
	exit 1
fi

if [[ ! -f "$TESTSET" ]]; then
        echo "$TESTSET does not exist"
        exit 1
fi

mkdir "$OUTDIR" || (echo "Cannot create $OUTDIR"; exit 1)
mkdir "$OUTDIR/lastrun"

export TIMEOUT
export OUTDIR
export VERBOSE
export COMMAND
export TESTSET
export KEEP_TEMPDIR
export MEM_LIMIT

cat "$TESTSET" | grep -v "^[\s]*$" > "$OUTDIR/testset"
git diff HEAD > "$OUTDIR/gitdiff"
COMMIT="$(git rev-parse --short HEAD)"

(
echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
echo "<report>"
echo "<conf>"
echo "<name>$(basename $OUTDIR)</name>"
echo "<date>$(date)</date>"
if [[ -n "$COMMIT" ]]; then
	echo "<commit>$COMMIT</commit>"
fi
echo "<base-command>$COMMAND</base-command>"
echo "<option-list>"
for o in $@; do
	echo "<options>$o</options>"
done
echo "</option-list>"
echo "<testset>$TESTSET</testset>"
echo "<test-list>"
for t1 in `cat $TESTSET`; do
        echo "<test>$t1</test>"
done
echo "</test-list>"
echo "<timeout>$TIMEOUT</timeout>"
echo "<mem-limit>$MEM_LIMIT</mem-limit>"
echo "<threads>$PARALLEL</threads>"
echo "</conf>"
) > "$OUTDIR/report.xml"

trap 'kill $(jobs -pr) 2> /dev/null; echo; exit 1' SIGINT SIGTERM

(for j in `seq 1 $REPEATALL`; do
for o in "$@"; do
for t1 in `cat $TESTSET`; do
for i in `seq 1 $REPEAT`; do
	echo "$j"
	echo "$i"
	echo "$o"
	echo "$t1"
done
done
done
done) | xargs -d "\n" -n 4 -P "$PARALLEL" "$(dirname $0)/single-run-wrapper.sh"

cat "$OUTDIR"/part-*-report.xml >> "$OUTDIR/report.xml"
echo "</report>" >> "$OUTDIR/report.xml"

if [[ -z "$KEEP_TEMPDIR" ]]; then
	rm "$OUTDIR"/part-*-report.xml
fi

