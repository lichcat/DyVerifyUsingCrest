#!/bin/bash
COUNT=$1
n_iter=1000
prog=expand
while [ $COUNT -le $2 ]
do
echo "--------------------------"
echo "warning $COUNT 's results:"
file="$prog""_path_""$COUNT"".log"
grep -w -q "Bloat" $file
BLOAT=$?
grep -w -q "L_NOT_Leak" $file
L_NOT_LEAK=$?
grep -w -q "Leak" $file
LEAK=$?
if [ $LEAK -eq 0 ]
then
echo "LEAK"
grep -w -c "Leak" $file
grep -w "Iteration $n_iter" $file
elif [ $BLOAT -eq 0 ]
then
echo "BLOAT"
grep -w -c "Bloat" $file
grep -w "Iteration $n_iter" $file
elif [ $L_NOT_LEAK -eq 0 ]
then
echo "L_NOT_LEAK"
grep -w -c "L_NOT_Leak" $file
grep -w "Iteration $n_iter" $file
else
echo "MAY-LEAK"
grep -w "Iteration $n_iter" $file
fi
COUNT=$[$COUNT+1];
done
exit 0
