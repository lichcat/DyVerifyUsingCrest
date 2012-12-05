#!/bin/bash
PROGRAM=pt
COUNT=0
while [ $COUNT -le $1 ]
do
echo "$COUNT"
fn="currentCheck""$COUNT"
res="$PROGRAM""_path_""$COUNT"".log"
cp $fn currentCheck
make crest
make run
cp "$PROGRAM""_path" $res
COUNT=$[$COUNT+1];
done
