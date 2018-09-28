#!/bin/bash
# create delete directory if it does not exist
[ -d nef-to-delete ] || mkdir  nef-to-delete

for n in *.NEF; do 
    sj='jpg'
    sn='NEF'
    j="${n/$sn/$sj}"
    
    [ -f $j ] || echo 'Moving ' $n
    [ -f $j ] || mv $n nef-to-delete/
    
    #echo $j $n
    
done
