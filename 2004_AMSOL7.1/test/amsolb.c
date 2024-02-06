#!/bin/csh
#This script runs AMSOL in interactive mode       
#Usage:  amsolb.c tr1
#This script reads tr1.dat and produces tr1.arc, tr1.out, and other files
#! The following assignments need to be made by the user ################
 set username = username
# NOTE: ONLY ABSOLUTE PATHNAMES MAY BE USED IN THE NEXT THREE SET STATEMENTS
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
        echo ' Please use filename without extension e.g.( test not test.dat)'
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
        echo '#! /bin/csh -f ' > $outfile
        echo '# user='$username >> $outfile
        echo '#' >> $outfile
# The following line echos com file to standard error (your window):
        echo 'set echo' >> $outfile
        echo 'if ( -e '${workd}/${input}'.acc ) /bin/rm '${workd}/${input}'.acc' >> $outfile
        echo 'cd '${workd} >> $outfile
        echo 'alias amsol '$amsolexe >> $outfile
        echo 'mkdir '${workd}/${input} >> $outfile
        echo 'cd '${workd}/${input} >> $outfile
        echo 'if ( -e '${workd}/${input}'.res ) ln '${workd}/${input}'.res fort.9' >> $outfile
        echo 'if ( -e '${workd}/${input}'.den ) ln '${workd}/${input}'.den fort.10' >> $outfile
        echo 'if (-f '${extsmfile}') cp '${extsmfile}' fort.19' >> $outfile
        echo 'if (-f '${xkwfile}') cp '${xkwfile}' fort.20' >>$outfile
        echo 'setenv FILENV '$input'.ass' >> $outfile
        echo 'amsol < '${workd}/$input'.dat > '${workd}/$input'.out' >> $outfile
        echo 'if (-f '${workd}/${input}/'fort.9 ) mv fort.9 '${workd}/$input'.res' >> $outfile
        echo 'if (-f '${workd}/${input}/'fort.10 ) mv fort.10 '${workd}/$input'.den' >> $outfile
        echo 'if (-f '${workd}/${input}/'fort.12 ) mv fort.12 '${workd}/$input'.arc' >> $outfile
        echo 'if (-f '${workd}/${input}/'fort.13 ) mv fort.13 '${workd}/$input'.gpt' >> $outfile
        echo 'if (-f '${workd}/${input}/'fort.15 ) mv fort.15 '${workd}/$input'.dmt' >> $outfile
        echo 'if (-f '${workd}/${input}/'fort.18 ) mv fort.18 '${workd}/$input'.inp' >> $outfile
        echo 'rm *' >> $outfile
        echo 'cd '${workd} >> $outfile
        echo 'rmdir '${input} >> $outfile
#
        chmod 750 ${workd}/${input}.com
        qsub -lT 5000 -lM 8MW -e ${workd}/${input}.qe -o ${workd}/${input}.qo ${workd}/${input}.com

