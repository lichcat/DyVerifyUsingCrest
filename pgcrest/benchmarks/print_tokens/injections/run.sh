#!/bin/bash
COUNT=0
while [ $COUNT -le $1 ]
do
echo "$COUNT"
fn="currentCheck""$COUNT"
res="pt_path_""$COUNT"".log"
cp $fn currentCheck
make crest
make run
cp pt_path $res
COUNT=$[$COUNT+1];
done
