#!/bin/bash

filename="${!#}"
opts="${@:1:$(($#-1))}"

myhome="/srv/chroot/natty/home/$USER"
mytemp="$(mktemp -d "$myhome/zenotmpXXXXX")"
chrtemp="/home/$USER/$(basename "$mytemp")"

cp "$filename" "$mytemp/$(basename "$filename")"
cp "$myhome/zeno-0.2.0.1/Zeno.hs" "$mytemp"

exec schroot -c natty -d "$chrtemp" -- "/home/$USER/.cabal/bin/zeno" -m prop "$opts" "$chrtemp/$(basename "$filename")"

