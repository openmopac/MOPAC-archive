#!/bin/csh
set job = $1
if ( $1 == "" ) set job = test
foreach file (out log mep brz gpt esp ump arc syb end)
    if -e $job.$file mv $job.$file $job.$file.$$
  end
if !(-e $job.log) mkfile -n 1 $job.log
if -e mol.out rm mol.out
if -e mol.mep rm mol.mep
if -e mol.arc rm mol.arc
if -e mol.pot rm mol.pot
if -e mol.sol rm mol.sol
setenv FOR005 $job.dat
setenv FOR006 $job.out
setenv FOR009 $job.res
setenv FOR010 $job.den
setenv FOR011 $job.log
setenv FOR012 $job.arc
setenv FOR013 $job.gpt
setenv FOR016 $job.syb
setenv FOR020 $job.ump
setenv SETUP  SETUP.DAT
setenv SHUTDOWN $job.end
set exists = `ls -ls | grep " core " | wc -l`
if "$exists" == 0 goto cont
rm -f core
cont:
ln -s /core
if -e $job.err_log rm $job.err_log
#mopac.exe $job >& $job.err_log
debugger -q mopac.exe  -Wp  570 0 
#time mopac.exe $job <$job.dat &
#sleep 5
#tail -67f $job.out
if -e mol.mep mv mol.mep $job.mep
if -e mol.arc mv mol.arc $job.mol
if -e mol.pot mv mol.pot $job.pot
if -e mol.sol mv mol.sol $job.sol
vi $job.out
#grep FINAL $job.out
if -e core rm core
