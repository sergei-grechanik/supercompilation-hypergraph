#!/bin/bash

j=$1
i=$2
o=$3
t1=$4
t="$(dirname "$TESTSET")/$t1"

MYTEMP="$(mktemp -d "$OUTDIR/tmpXXXXX")"

if [[ -z "$KEEP_TEMPDIR" ]]; then
	trap "rm -r '$MYTEMP'" SIGINT SIGTERM EXIT
fi

trap 'kill $(jobs -pr) 2> /dev/null; echo; exit 1' SIGINT SIGTERM

STARTDATE="$(date)"
echo -e "\n$STARTDATE\nRunning $j.$i $COMMAND $o $t" >&2
if [[ -z "$VERBOSE" ]]; then
	"$(dirname $0)/single-run.sh" "$COMMAND $o $t" "$MYTEMP" >> "$OUTDIR/log" 2>&1 &
else
	"$(dirname $0)/single-run.sh" "$COMMAND $o $t" "$MYTEMP" > >(tee -a "$OUTDIR/log") 2> >(tee -a "$OUTDIR/log" >&2) &
fi
wait

(echo ""
echo "<run j=$j i=$i>"
echo "<full-command>$COMMAND $o $t</full-command>"
echo "<base-command>$COMMAND</base-command>"
echo "<options>$o</options>"
echo "<test>$t1</test>"
echo "<date>$STARTDATE</date>"
echo "<temp-dir>$MYTEMP</temp-dir>"
cat "$MYTEMP/stat" | sed "s/^\([^ ]\+\) \([^ ]\+\)$/<\1>\2<\/\1>/"
echo "</run>") >> "$OUTDIR/part-$(basename "$MYTEMP")-report.xml"
