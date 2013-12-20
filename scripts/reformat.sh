#!/bin/bash


dirnm=$(dirname "$1")

for f in hipspec-total hipspec hosc; do
	cat "$1" | grep -v "^[\s]*$" | while read t; do
		echo "reformatted/$f/$dirnm/$t/"
		mkdir -p "reformatted/$f/$dirnm/$t"
		./graphsc --reformat "$f" "$dirnm/$t" > "reformatted/$f/$dirnm/$t/Test.hs"
		if [ "$f" = hosc ]; then
			grep -v "right-hand-side" "reformatted/$f/$dirnm/$t/Test.hs" > "reformatted/$f/$dirnm/$t/Test1.hs"
			grep -v "left-hand-side" "reformatted/$f/$dirnm/$t/Test.hs" > "reformatted/$f/$dirnm/$t/Test2.hs"
		fi
	done
done

