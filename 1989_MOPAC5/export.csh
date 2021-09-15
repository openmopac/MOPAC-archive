#!/bin/csh
#set echo 
#
#    Command file to prepare a complete copy of MOPAC-93 and MOPAC 7
#    suitable for export
#
set to_dir = /scratch/export/mopac
if !(-e $to_dir) mkdir $to_dir        
cd $to_dir          
echo " Do you want this session to be interactive (n)?"
set response = $<
if ($response != y) then
set talk=n
else
set talk=y
endif
if ($talk == y) then
echo "Do you want to delete all files in $to_dir?"
set response = $<
if ($response != y) goto  mopac
else
echo Deleting all files in $to_dir
endif
rm -rf *
#----------------------------------------Export MOPAC, DYNAMIC version
mopac:
if ($talk == y)  then
echo Do you want to export MOPAC, DYNAMIC version"?"
set response = $<
if ($response != y) goto  mopac_vp
endif
set from_dir = /home/Anna/jstewart/mopac
if -e  $to_dir/mopac_dynamic_source then
echo "The MOPAC directory already exists."
echo "Do you want to delete it?"
set response = $<
if ($response == y) then
rm -rf  $to_dir/mopac_dynamic_source
if -e  $to_dir/mopac_dynamic_examples rm -rf  $to_dir/mopac_dynamic_examples
mkdir  $to_dir/mopac_dynamic_source
mkdir  $to_dir/mopac_dynamic_examples
else
if !(-e  $to_dir/mopac_dynamic_examples) mkdir  $to_dir/mopac_dynamic_examples
endif
else 
mkdir  $to_dir/mopac_dynamic_source
mkdir  $to_dir/mopac_dynamic_examples
endif
echo Copying MOPAC 93, dynamic version to $to_dir   
cd $from_dir        
cp -p *.f $to_dir/mopac_dynamic_source
cd  $to_dir/mopac_dynamic_source
cd $from_dir         
cp -p SIZE* $to_dir/mopac_dynamic_source
cp -p mopac.csh $to_dir/mopac.csh
cp -p Makefile    $to_dir/mopac_dynamic_source/Makefile
cp -p make_mopac.csh $to_dir/mopac_dynamic_source
cp -p README $to_dir
set from_dir = /home/Anna/jstewart/testdata
cd $to_dir/mopac_dynamic_examples
makedatasets
cp $from_dir/port.out $to_dir/mopac_dynamic_examples
cp $from_dir/mnrsk3.key $to_dir/mopac_dynamic_examples
cp $from_dir/port.parameters $to_dir/mopac_dynamic_examples
cp $from_dir/all.dat  $to_dir/mopac_dynamic_examples
cp $from_dir/all.out  $to_dir/mopac_dynamic_examples
cp $from_dir/paras  $to_dir/mopac_dynamic_examples
cp $from_dir/SETUP  $to_dir/mopac_dynamic_examples
cp $from_dir/testall.dat $to_dir/mopac_dynamic_examples
cp $from_dir/testall.out $to_dir/mopac_dynamic_examples
cp $from_dir/1scf.setUP  $to_dir/mopac_dynamic_examples
cp $from_dir/setup.iter  $to_dir/mopac_dynamic_examples
cp $from_dir/setup1    $to_dir/mopac_dynamic_examples
cd /scratch/export/mopac/mopac_dynamic_source

#echo "                                                 Compiling MOPAC for DYNAMIC"
#make 
