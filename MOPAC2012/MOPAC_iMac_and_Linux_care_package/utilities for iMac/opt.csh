echo off
#
##   Utility to optimize parameters for a single element
#
echo "*" > $1.dat
echo "*" > $1.dat
echo "*  Optimize PM7 parameters for a single element" >> $1.dat
echo "*" >> $1.dat
echo "set=PM7/all_ref_data.txt  ref=norm;leve;erra params=PM7/params.txt" >> $1.dat
echo   " ($1)(All)" >> $1.dat
echo   " end" >> $1.dat
echo *:
echo *:                   Parameter optimization of element $1 started.
echo *:
   /Users/jstewart/PARAM_Source/PARAM.exe $1  &
sleep 3
tail -56f $1.out
