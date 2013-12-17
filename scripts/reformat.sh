#!/bin/bash


dirnm=$(dirname "$1")

for f in hipspec-total; do
	cat "$1" | grep -v "^[\s]*$" | while read t; do
		mkdir -p "reformatted/$f/$dirnm/$t"
		./graphsc --reformat "$f" "$dirnm/$t" > "reformatted/$f/$dirnm/$t/Test.hs"
	done
done

