#!/bin/sh

rm test.timings

for RUN in `seq -w 1 79`

do echo ..running test $RUN | tee -a test.timings
/usr/bin/time ../src/mopac5022mn.exe < test$RUN.dat > test$RUN.out  \
   2>>test.timings 
done

if [ -e "/usr/bin/perl" ]
then
  echo "Perl is there."
  ./check.pl
else
  echo "Could not check with Perl script.  Install Perl on system and/or change its location to /usr/bin/perl."
  exit
fi

