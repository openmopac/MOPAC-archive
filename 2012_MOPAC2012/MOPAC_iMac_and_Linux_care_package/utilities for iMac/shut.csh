#!/bin/sh
#
#  SHUTDOWN command
# Remove last four characters of file.
#
file=$1
if [ `expr "$file" : ".*.mop"` -gt 0 ]; then file=${file:0:`expr $file : '.*'` -4}; fi
if [ `expr "$file" : ".*.out"` -gt 0 ]; then file=${file:0:`expr $file : '.*'` -4}; fi
if [ `expr "$file" : ".*.dat"` -gt 0 ]; then file=${file:0:`expr $file : '.*'` -4}; fi
if [ `expr "$file" : ".*.arc"` -gt 0 ]; then file=${file:0:`expr $file : '.*'` -4}; fi
echo Shutdown > $file.end

