#!/bin/csh
#
#   Install file for MOPAC-93.  This file should be run whenever MOPAC-93 is
#   to be installed for the first time.  If necessary, it can be run more than
#   once.  A question and answer session is involved.
echo This is the command file to install MOPAC-93.  It involves a question
echo and answer session.  Most of the time, the default options will be suitable.
echo Default options are indicated in parentheses, for example, \`"(y)"\' would mean
echo that the user can select the default by pressing the return key as a response.
echo " "
echo Allowed options are indicated in square brackers, for example "[y/n]" would
echo mean that the user should type either \`y\' or \`n\'. 
echo "("\`y\' and \`n\' are abbreviations for \`yes\' and \`no\'")"
echo " " 
ready:
echo Are you ready to start the installation  "(y) [y/n]?"
set response = $<
if ( $response != n) goto install
echo " "
echo "Do you want to stop the installation? (y) [y/n]"
set response = $<
if ($response == n) goto start
echo Please type \`make_mopac.csh\' when you want to install MOPAC-93.
exit
install:
#***********************************************
echo " "
echo "Ford's ElectroStatic Potential calculation uses a large amount of memory."
echo By not including the ESP, you can run larger systems.
echo " "
echo If you decide not to include ESP at this time, you will still be able to
echo include it later.
echo " "
echo "Do you want MOPAC-93. to be able to run Ford's ESP calculations (n) [y/n]?"
set response = $<
if ($response == y) then
cp pmep.for pmep.f
else
cp dummy_pmep.for pmep.f
endif
#***********************************************
echo " "
echo "Merz's ElectroStatic Potential calculation uses a large amount of memory."
echo By not including the ESP, you can run larger systems.
echo " "
echo If you decide not to include ESP at this time, you will still be able to
echo include it later. 
echo " "
echo "Do you want MOPAC-93. to be able to run Merz's ESP calculations (n) [y/n]?"
set response = $<
if ($response == y) then
cp esp.for esp.f
else
cp dummy_esp.for esp.f
endif
#***********************************************
echo " "
echo The Greens Function correction to the Ionization Potential 
echo uses a large amount of memory.
echo " "
echo By not including it, you can run larger systems.
echo " "
echo If you decide not to include the Greens Function at this time, you will 
echo still be able to include it later.
echo " "
echo Do you want MOPAC-93 to be able to run Green Function correction to the I.P. "(n) [y/n]?"
set response = $<
if ($response == y) then
cp greenf.for greenf.f
else
cp dummy_greenf.for greenf.f
endif
#***********************************************
echo " "
echo The COSMO "(COnductor-like Screening MOdel)" for modeling solvents
echo uses a large amount of memory.
echo " "
echo By not including it, you can run larger systems.
echo " "
echo If you decide not to include the COSMO function at this time, you will 
echo still be able to include it later.
echo " "
echo Do you want MOPAC-93 to be able to run COSMO calculations "(n) [y/n]?"
set response = $<
if ($response == y) then
cp cosmo.for  cosmo.f
else
cp dummy_cosmo.for  cosmo.f
endif
#***********************************************
echo " "
echo The Polarizability calculation uses a large amount of memory.
echo " "
echo By not including it, you can run larger systems.
echo " "
echo If you decide not to include the polarizability at this time, you will
echo still be able to include it later.
echo " "
echo Do you want MOPAC-93 to be able to run polarizability calculations "(y) [y/n]?"
set response = $<
if ($response == n) then
cp dummy_polar.for polar.f
else
cp polar.for polar.f
endif
echo " "
echo The size of MOPAC depends on the number of heavy "(MAXHEV)" and 
echo light "(MAXLIT)" atoms  defined in the file SIZES.  
echo Examples of the sizes of MOPAC "(not including the ESP or Greens Function)" are
echo " "
echo "MAXHEV    MAXLIT    Size of MOPAC (Kb)"
echo "  10        10        3,388"
echo "  20        20        5,979"
echo "  30        30       16,199"
echo "  40        40       23,779"
echo "  50        50       33,006"
echo " 100       100       86,383"
echo " "
echo What value do you want for MAXHEV "(30) ?"
set response = $<
if ($response =="")set response=30
echo What value do you want for MAXLIT "(30) ?"
set response1 = $<
if ($response1 =="")set response1=30
if -e SIZES rm SIZES
cp  SIZES1 SIZES
echo    "      PARAMETER ("MAXHEV=$response,   MAXLIT=$response1")" >> SIZES
#***********************************************
echo " "
echo "    Default maximum job time limit and automatic checkpoint time"
echo " "
echo "MOPAC jobs have a default maximum time of 3600 seconds. "
echo "MOPAC will automatically write a checkpoint or dump file every hour"
echo Do you want to modify the default maximum job time 
echo and/or the time between checkpoints" (n)[y/n]?"
set response = $<
if $response != y then
echo "      PARAMETER (MAXTIM=3600, MAXDMP=3600)" >> SIZES
else
echo "What do you want as the default maximum time for a job, in seconds?"
set response = $<
echo "What do you want as the default checkpoint or dump time, in seconds?"
set response1 = $<
echo "      PARAMETER (MAXTIM=$response, MAXDMP=$response1)" >> SIZES
endif
#***********************************************
echo "Do you want MOPAC to be able to write files for use in SYBYL (n)?"
set response = $<
if $response == y then
echo "      PARAMETER (ISYBYL=1)" >> SIZES
else
echo "      PARAMETER (ISYBYL=0)" >> SIZES
endif
#***********************************************
echo "Do you want the DRC restart files to be unformatted instead of formatted (y)?"
set response = $<
if $response == n then
echo "      PARAMETER (MDRCRS=1)" >> SIZES
else
echo "      PARAMETER (MDRCRS=0)" >> SIZES
endif
#***********************************************
cat SIZES2 >> SIZES
echo "If memory is limited, MOPAC can be run without the MECI option"
echo "and/or without analytical C.I. derivatives"
echo "and/or without Pulay's SCF DIIS converger"
echo " "
echo "Do you want to include these three functions (y) [y/n]?"
set response = $<
if $response != n then
echo "       PARAMETER (NMECI=11,  NPULAY=MPACK, MMCI=60)" >> SIZES
else
echo "Do you want the MECI option? (n) [y/n] ?"
set response = $<
if $response != y then
set opt1 = 1
else
set opt1 = 11
endif
echo "Do you want the analytical C.I. derivative calculation (y) [y/n]?"
set response = $<
if $response == n then
set opt2 = 1
else
set opt2 = MPACK
endif
echo "Do you want Pulay's SCF DIIS converger (n) [y/n]?"
set response = $<
if $response != y then
set opt3 = 1
else
set opt3 = 60
endif
echo  "       PARAMETER (NMECI=$opt1,  NPULAY=$opt2, MMCI=$opt3)" >> SIZES
endif
#***********************************************
echo "      PARAMETER (MESP=50000)" >> SIZES
echo "      PARAMETER (LENABC=400)" >> SIZES
echo "      PARAMETER (LENAB2=LENABC*(LENABC+5))" >> SIZES
echo "      PARAMETER (NPPA = 1082, MAXNSS = 500)" >> SIZES
echo "      PARAMETER (MAXDEN=10*MAXHEV+MAXLIT)" >> SIZES
echo "************************************************************************" >> SIZES
echo "*DECK MOPAC ">> SIZES
echo " "
echo " The file SIZES is now ready."
echo " The next step is to compile MOPAC."
machine_prompt:
echo " "
echo " This will take a while - you should have a break now."
echo " "
make 
echo " "
echo " "
echo " MOPAC compiled"
