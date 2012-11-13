#!/bin/bash
CREST_PROGRAM="tcas"
COUNT=0
while [ $COUNT -le $1 ]
do
echo "$COUNT"
fn="currentCheck""$COUNT"
res="$CREST_PROGRAM""_path_""$COUNT"".log"
cp $fn currentCheck
make crest
make run
cp "$CREST_PROGRAM""_path" $res
COUNT=$[$COUNT+1];
done
