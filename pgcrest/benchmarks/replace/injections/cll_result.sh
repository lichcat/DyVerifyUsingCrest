#!/bin/bash
COUNT=0
while [ $COUNT -le $1 ]
do
echo "warning $COUNT 's results:"
file="replace_path_""$COUNT"".log"
grep -w -q "Bloat" $file
BLOAT=$?
grep -w -q "L_NOT_Leak" $file
L_NOT_LEAK=$?
grep -w -q "Leak" $file
LEAK=$?
if [ $LEAK -eq 0 ]
then
echo "LEAK"
elif [ $BLOAT -eq 0 ]
then
echo "BLOAT"
elif [ $L_NOT_LEAK -eq 0 ]
then
echo "L_NOT_LEAK"
else
echo "MAY-LEAK"
fi
COUNT=$[$COUNT+1];
done
exit 0
