#!/bin/bash
PROGRAM=wc
COUNT=0
while [ $COUNT -le $1 ]
do
echo "$COUNT"
fn="currentCheck""$COUNT"
res="$PROGRAM""_path_""$COUNT"".log"
cp wc_inject_files/$fn currentCheck
make crest
make run
cp "$PROGRAM""_path" wc_inject_files/$res
COUNT=$[$COUNT+1];
done
