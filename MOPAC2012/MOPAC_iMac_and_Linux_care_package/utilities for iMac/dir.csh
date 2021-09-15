#!/bin/sh
echo $1
if [ "$1" ]   
then 
ls -lst | grep $1 | head -15
else
ls -ls -t  | head -15
fi

