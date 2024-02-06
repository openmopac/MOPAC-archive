#!/bin/csh
#This script runs AMSOL in batch mode       
#Usage:  amsolab.c tr1
#This script reads tr1.dat and produces tr1.arc, tr1.out, and other files
#! The following assignments need to be made by the user ################
 set username = username
# NOTE: THE NEXT THREE SET STATEMENTS CAN USE EITHER ABSOLUTE OR RELATIVE
# PATHNAMES.
 set amsolexe = /fullpathnamefortheexecutablefile
 set extsmfile = /fullpathnamefortheEXTSMfile
 set xkwfile = /setfullpathnamefortheXKWfile
# set working directory to directory for tr1.dat and output tr1 files:
# set workd = /workingdirectory
# or set working directory to current working directory and submit script
#    so that current working directory contains tr1.dat:
 set workd = `pwd`
# end of assignment section #############################################
#
	clear
#
	if ($#argv >= 1) then 
		set input = $argv[1]
		goto checkinput
	endif
#
 inputfile:
	echo -n 'Input file : '
	set input = $<
	if ($input == '') goto inputfile
#
 checkinput:
	switch ($input)
		case *.dat
	echo ' Please use filename without extension, e.g. test (not test.dat)'
		goto inputfile	
	breaksw
	endsw
	if (!(-e $input'.dat')) then
		echo 'The file' $input'.dat does not exist '
		goto inputfile
	endif	 
#
	if ($#argv >= 2) then
		set time = $argv[2]
		goto priook
	endif
priook:
#
	set outfile = ${workd}/${input}.com
	echo '# user='$username > $outfile
	echo '#' >> $outfile
	echo 'if ( -e '${workd}/${input}'.ja ) /bin/rm '${workd}/${input}'.ja' >> $outfile
	echo 'cd '${workd} >> $outfile
	echo 'ja '${workd}/${input}'.ja' >> $outfile
	echo 'alias amsol '$amsolexe >> $outfile
        echo 'assign -a fort.19 '$extsmfile'' >> $outfile
        echo 'assign -a fort.20 '$xkwfile'' >> $outfile
	echo 'setenv FILENV '$input'.ass' >> $outfile
	echo 'assign -a '$input'.res fort.9' >> $outfile
	echo 'assign -a '$input'.den fort.10' >> $outfile
	echo 'assign -a '$input'.arc fort.12' >> $outfile
	echo 'assign -a '$input'.gpt fort.13' >> $outfile
	echo 'assign -a '$input'.dmt fort.15' >> $outfile
        echo 'assign -a '$input'.inp fort.18' >> $outfile
        echo 'assign -a '$extsmfile' fort.19' >> $outfile
        echo 'assign -a '$xkwfile' fort.20' >> $outfile
        echo 'assign -a '$input'.rin fort.21 >> $outfile
	echo 'amsol <'$input'.dat >'$input'.out' >> $outfile
# Cray-2 version of ja:
#       echo 'ja -sct '${workd}/${input}'.ja >> ' $input'.out' >> $outfile
        echo 'ja -flt '${workd}/${input}'.ja >> ' $input'.out' >> $outfile
# Cray X-MP version of ja; can use maximum memory option on X-MP:
#       echo 'ja -fhlt '${workd}/${input}'.ja >> ' $input'.out' >> $outfile
#	echo 'if ( -z '${workd}/${input}'.qe ) /bin/rm '${workd}/${input}'.qe'  >> $outfile
	echo '/bin/rm '${workd}/${input}'.qo' >> $outfile
        chmod 750 ${workd}/${input}.com
	qsub -lT 5000 -lM 8MW -e ${workd}/${input}.qe -o ${workd}/${input}.qo ${workd}/${input}.com  
