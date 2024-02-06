#!/usr/bin/perl 
format STDOUT =
    @<<<<<<<<<<<<<<<<<<  @###.###     @###.###    @###.###
$label{$j},$data[$j],$data2[$j],$diff
.


$label{0}= "(0)  E-EN(g)";
$label{1}= "(1)  E-EN(sol)" ;
$label{2}= "(2)  G-P(sol) ";
$label{3}= "(3)  G-ENP(sol)" ;
$label{4}= "(4)  G-CDS(sol) ";
$label{5}= "(5)  G-P-CDS(sol)";
$label{6}= "(6)  G-S(sol) ";
$label{7}= "(7)  DeltaE-EN(sol)";
$label{8}= "(8)  DeltaG-ENP(sol)" ;
$label{9}= "(9)  DeltaG-S(sol) ";
$label{10}="CM3 dipole moment ";
$label{11}="DeltaG-S";

sub collect1 {
open (FILE, "<$file");
@amsol="";
 while (<FILE>) {
  if (/\(0\)\s+E-EN\(g\) gas-phase electronic-nuclear energy\s+([-]?\d*\.\d+)\s*/){
    $amsol[0]=$1;
  }
  if (/\(1\)\s+E-EN\(sol\) electronic-nuclear energy of solute\s+([-]?\d*\.\d+)\s*/){
    $amsol[1]=$1;
  }
  if (/\(2\)\s+G-P\(sol\) polarization free energy of solvation\s+([-]?\d*\.\d+)\s*/){
    $amsol[2]=$1;
  }
  if (/\(3\)\s+G-ENP\(sol\) elect.-nuc.-pol. free energy of system\s+([-]?\d*\.\d+)\s*/){
    $amsol[3]=$1;
  }
  if (/\(4\)\s+G-CDS\(sol\) cavity-dispersion-solvent structure free energy\s+([-]?\d*\.\d+)\s*/){
    $amsol[4]=$1;
  }
  if (/(\(5\)\s+G-P-CDS\(sol\) \= G-P\(sol\) \+ G-CDS\(sol\) \= \(2\) \+ \(4\)\s+)([-]?\d*\.\d+)\s*/){
    $amsol[5]=$2;
  }
  if (/\(6\)\s+G-S\(sol\) free energy of system \= \(1\) \+ \(5\)\s+([-]?\d*\.\d+)\s*/){
    $amsol[6]=$1;
  }
  if (/\(7\)\s+DeltaE-EN\(sol\) elect.-nuc. reorganization energy of solute\s+([-]?\d*\.\d+)\s*/){
    $amsol[7]=$1;
  }
  if (/\(8\)  DeltaG-ENP.+elect.-nuc.-pol. free energy of solvation\s+([-]?\d*\.\d+)\s+/){
    $amsol[8]=$1;
  }
  if (/\(9\)  DeltaG-S.+free energy of  solvation\s+([-]?\d*\.\d+)\s+/){
    $amsol[9]=$1;
  }
  if (/Final heat of formation.+\s+([-]?\d*\.\d+)\s+.+/){
    $amsol[0]=$1;
  }
  if (/^ Free energy of  solvation calculated with\s+([-]?\d*\.\d+)\s+/){
    $amsol[11]=$1;
  }
 }
 return (@amsol);
}

sub collect2 {
 open(FILE, "<$file");
 @amsol = "";
 @line = "";
 @line = <FILE>;
 close(FILE);
  for $i (0  .. $#line) {
  if ($line[$i] =~ m/^For the charges calculated by CM3:/){
     $start = $i;
  }
  if ($line[$i] =~ m/^\s+using Mulliken population analysis/){
     $end = $i;
  }
  }
  for $i ($start .. $end) {
     chomp($line[$i]);
     if ($line[$i] =~ m/^ from point charges /) {
        ($crap,$crap,$crap,$crap,$crap,$crap,$crap,$amsol[10])
        = split/\s+/, $line[$i];
     }
  }
 return (@amsol);
}

foreach $i (0 .. $#ARGV) {
 $file = $ARGV[$i];
 if ($file =~ m/tr\d\d?CM3/) {
   #print "$file\n";
   @data = collect2 ($file);
   #print "$file\t@data\n";
   $file= "../testo/$file";
   @data2 = collect2 ($file);
   #print "$file2\t@data2\n";
 } else {
   @data = collect1 ($file);
   $file = "../testo/$file";
   @data2 = collect1 ($file);
 }
   print "$ARGV[$i]\n";
 for $j (0 .. 11){
  if ($data2[$j] != 0.0){
    $diff=$data[$j] - $data2[$j];
    write;
  }
 }
}

