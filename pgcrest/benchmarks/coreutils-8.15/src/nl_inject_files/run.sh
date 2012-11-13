#!/bin/bash
PROGRAM=nl
COUNT=0
while [ $COUNT -le $1 ]
do
echo "$COUNT"
fn="currentCheck""$COUNT"
res="$PROGRAM""_path_""$COUNT"".log"
cp "$PROGRAM""_inject_files/""$fn" currentCheck
make crest
make run
cp "$PROGRAM""_path" "$PROGRAM""_inject_files/""$res"
COUNT=$[$COUNT+1];
done
