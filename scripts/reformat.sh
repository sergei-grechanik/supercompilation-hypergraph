#!/bin/bash


filenm="$1"
scripts=$(dirname "$0")
dirnm=$(dirname "$1")

shift

if [ "$#" == 0 ]; then
	FMTS="hipspec-total hipspec-partial zeno-total zeno-partial hosc"
else
	FMTS="$@"
fi

for f in $FMTS; do
	cat "$filenm" | grep -v "^[\s]*$" | while read t; do
		echo "reformatted/$f/$dirnm/$t/"
		mkdir -p "reformatted/$f/$dirnm/$t"
		./graphsc --reformat "$f" "$dirnm/$t" > "reformatted/$f/$dirnm/$t/Test.hs"
		if [ "$f" = hosc ]; then
			grep -v "right-hand-side" "reformatted/$f/$dirnm/$t/Test.hs" > "reformatted/$f/$dirnm/$t/Test1.hs"
			grep -v "left-hand-side" "reformatted/$f/$dirnm/$t/Test.hs" > "reformatted/$f/$dirnm/$t/Test2.hs"
		elif [ "$f" = hipspec-total ] || [ "$f" = hipspec-partial ]; then
			(cd  "reformatted/$f/$dirnm/$t"
			cat Test.hs | sed -n "s/-- function \(.\+\)$/:t Test.\1/p" | ghci Test.hs | sed "s/^.*[^-]> /@@@/" | tr "\n" " " | sed "s/@@@/\n/g" | sed -n "s/^Test.\([^ ]\+ \+::.\+\)$/\1/p" >> Test.hs)
		elif [ "$f" = speed-benchmark ]; then
                     GHC_OPTIONS="-O" $scripts/run-test-with-main.sh "reformatted/$f/$dirnm/$t/Test.hs" "$scripts/Speed.hs" gen > "reformatted/$f/$dirnm/$t/data"
                fi
	done
done

