#!/bin/sh
#
# Remove last four characters of file, and add text ".out"
#
file=$1
if [ `expr "$file" : ".*.mop"` -gt 0 ]; then file=${file:0:`expr $file : '.*'` -4}.out; fi
if [ `expr "$file" : ".*.dat"` -gt 0 ]; then file=${file:0:`expr $file : '.*'` -4}.out; fi
if [ `expr "$file" : ".*.arc"` -gt 0 ]; then file=${file:0:`expr $file : '.*'` -4}.out; fi
if [ `expr "$file" : ".*.den"` -gt 0 ]; then file=${file:0:`expr $file : '.*'` -4}.out; fi
if [ `expr "$file" : ".*.res"` -gt 0 ]; then file=${file:0:`expr $file : '.*'` -4}.out; fi
echo $file
 tail -56f $file  

