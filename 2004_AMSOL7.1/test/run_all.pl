#!/usr/bin/perl
#
#

$amsol_exe='../amsol7.1.exe';
$cp_command = 'cp -f';


#uncomment the next 2 lines if running in Windows
#$amsol_exe='..\amsol7.1.exe';
#$cp_command = 'copy /Y';



######################################################################################
# delete any files from this list if you do not want to run any particular test runs #
######################################################################################


$test_input_files="
tr01a.dat                   tr11n.dat     tr18.dat      tr23a.dat     tr32a.dat     tr42a.dat
tr01.dat      tr07a.dat     tr12a.dat     tr18d.dat     tr23.dat      tr33a.dat     tr42n.dat
tr01n.dat     tr07.dat      tr12CM3P.dat  tr19a2.dat    tr24a2.dat    tr34a.dat     tr43.dat
tr02a2.dat    tr07n.dat     tr12.dat      tr19a.dat     tr24a.dat     tr34n.dat     tr4CM3A.dat
tr02a.dat     tr08a.dat     tr12n.dat     tr19.dat      tr24.dat      tr35a.dat     tr4CM3P.dat
tr02.dat      tr08.dat      tr13a.dat     tr1CM3A.dat   tr25a2.dat    tr35f.dat     tr5CM3A.dat
tr02n.dat     tr08n.dat     tr13.dat      tr1CM3P.dat   tr25a3.dat    tr35n.dat     tr5CM3P.dat
tr03a.dat     tr09a2.dat    tr13n.dat     tr20a.dat     tr25a.dat     tr36a.dat     tr6CM3A.dat
tr03.dat      tr09a.dat     tr14a.dat     tr20.dat      tr26a2.dat    tr36n.dat     tr6CM3P.dat
tr03n.dat     tr09.dat      tr14.dat      tr21a.dat     tr26a.dat     tr37a.dat     tr7CM3A.dat
tr04a.dat     tr09n.dat     tr14n.dat     tr21.dat      tr27a.dat     tr38a.dat     tr7CM3P.dat
tr04.dat      tr10a.dat     tr15a.dat     tr22a2.dat    tr28a.dat     tr38n.dat     tr8CM3A.dat
tr04n.dat     tr10CM3A.dat  tr15.dat                    tr28n.dat     tr39a.dat     tr8CM3P.dat
tr05a.dat     tr10CM3P.dat  tr15n.dat     tr22a4.dat    tr29a.dat     tr39n.dat     tr9CM3A.dat
tr05.dat      tr10.dat      tr16a.dat     tr22a5.dat    tr2CM3A.dat   tr3CM3A.dat   tr9CM3P.dat
tr05n.dat     tr10n.dat     tr16.dat                    tr2CM3P.dat   tr3CM3P.dat
              tr11a.dat     tr16n.dat     tr22a7.dat    tr30a.dat     tr40.dat
tr06a.dat     tr11CM3A.dat  tr17a.dat     tr22a8.dat    tr30n.dat     tr41.dat
tr06.dat      tr11CM3P.dat  tr17.dat      tr22a.dat     tr31a.dat     tr41d.dat
tr06n.dat     tr11.dat      tr18a.dat     tr22.dat      tr31n.dat     tr41z.dat";

$test_input_files2 = "tr06a2.dat  tr07a2.dat  tr22a3.dat ";

$test_input_files3 = "tr22a6.dat";

@files  = ($test_input_files  =~ /\S{1,}/g);
@files2 = ($test_input_files2 =~ /\S{1,}/g);
@files3 = ($test_input_files3 =~ /\S{1,}/g);


foreach $file (@files){
  chop $file;chop $file;
  chop $file;chop $file;
  print "running $file  ...";
  $error_flag = system ("$amsol_exe \< $file.dat \> $file.out");
  if ($error_flag) {print "error running $file \n"}
  else{print "finished \n"}
}

foreach $file (@files2){
  chop $file;chop $file;
  chop $file;chop $file;
  print "running $file  ...";
  system ("$cp_command $file.xsm fort.19");
  $error_flag = system ("$amsol_exe \< $file.dat \> $file.out");
  if ($error_flag) {print "error running $file \n"}
  else{print "finished \n"}
}

foreach $file (@files3){
  chop $file;chop $file;
  chop $file;chop $file;
  print "running $file  ...";
  system ("$cp_command $file.xkw fort.20");
  $error_flag = system ("$amsol_exe \< $file.dat \> $file.out");
  if ($error_flag) {print "error running $file \n"}
  else{print "finished \n"}
}
  
  
 


