#!/bin/sh
COUNT=0
while [ $COUNT -le $1 ]
do
    file="nl_path_""$COUNT"".log"
    if test -f $file ; then
        echo $file
        grep -w "Iteration" $file      
    fi
    COUNT=`expr $COUNT + 1`
done
exit 0
