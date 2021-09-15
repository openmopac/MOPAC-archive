#!/bin/sh
#
# Remove last four characters of file, and add text ".out"
#
file=$1
if [ `expr "$file" : ".*.mop"` -gt 0 ]; then file=${file:0:`expr $file : '.*'` -4}.out; fi
if [ `expr "$file" : ".*.dat"` -gt 0 ]; then file=${file:0:`expr $file : '.*'` -4}.out; fi
if [ `expr "$file" : ".*.arc"` -gt 0 ]; then file=${file:0:`expr $file : '.*'` -4}.out; fi
if [ `expr "$file" : ".*.out"` -lt 1 ]; then file=$file.out; fi
cat $file | grep -i  HEA
tail -6f $file  | grep -i HEA 

