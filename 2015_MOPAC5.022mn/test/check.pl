#!/usr/bin/perl
# Written by Luke Fiedler, Feb. 2011.


# Collect output data.
foreach $INDEX (1..79) {
   $FILENM=sprintf("test%02d%s",$INDEX,".out");
   # print $FILENM."\n";

   open(MYFILE,"<$FILENM");
   @LINES=<MYFILE>;
   close MYFILE;
   $COUNT=0;
   $IS_DONE=0;
   $LINECT=0;
   foreach $TEXT (@LINES) {
      $HEAT_FORM[$INDEX]=$1 if $TEXT=~m/^\s+FINAL HEAT OF FORMATION\s+=\s+(\S+)\s+KCAL/i;
      ($TOTAL_ENERGY[$INDEX],$TOTAL_KCM[$INDEX])=($1,$2) if $TEXT=~m/^\s+TOTAL ENERGY\s+=\s+(\S+)\s+EV\s+=\s+(\S+)\s+KCAL\/MOL/i;
      $FIRST_GRADT[$INDEX]=$1 if $TEXT=~m/^CYCLE:\s+1\s+TIME.*GRAD\.:\s*(\S+)/i;
      $GRAD_NORM[$INDEX]=$1 if $TEXT=~m/^\s+GRADIENT NORM\s+=\s+(\S+)/i;
      $DIP_POLAR[$INDEX]=$2 if $TEXT=~m/^\s+(\S+)\s+(\S+)\s+ANG\.\*\*3/i;
      $DIPOLE[$INDEX][++$COUNT]=$1 if $TEXT=~m/^\s+SUM\s+\S+\s+\S+\s+\S+\s+(\S+)/i;
      $HP1_HP2[$INDEX]=$1 if $TEXT=~m/^\s+2\s+Hp\s+(\S+)/i;
      $IS_DONE=1 if $TEXT=~m/== MOPAC DONE ==/i;
      $XDIP[$INDEX]=$1 if $TEXT=~m/ X          \s+\S+\s+(\S+)/;
      $YDIP[$INDEX]=$1 if $TEXT=~m/ Y          \s+\S+\s+(\S+)/;
      $ZDIP[$INDEX]=$1 if $TEXT=~m/ Z          \s+\S+\s+(\S+)/;
      $TDIP[$INDEX]=$1 if $TEXT=~m/\s(\S+)\s+\(A\.U\.\)/;
      if ($TEXT=~/DIP POLARIZABILITY TENSOR/) {
         ($XTEXT,$YTEXT,$ZTEXT)=($LINES[$LINECT+4],$LINES[$LINECT+5],$LINES[$LINECT+6]);
         ($POLAR_XX[$INDEX])=$XTEXT=~/^\s+1\s+(\S+)/;
         ($POLAR_YY[$INDEX])=$YTEXT=~/^\s+2\s+\S+\s+(\S+)/;
         ($POLAR_ZZ[$INDEX])=$ZTEXT=~/^\s+3\s+\S+\s+\S+\s+(\S+)/;
      }
      $LINECT++;
      $ACC_HF[$INDEX]=$1 if $TEXT=~m/ENERGY OF "REORIENTED" SYSTEM WITHOUT FIELD:\s+(\S+)/i;  # only available if POLAR keyword is in input file
   }
   die "$FILENM did not complete.\n" if !($IS_DONE);
}
foreach $INDEX (1..72) {
      ##print "$INDEX  xdip is $XDIP[$INDEX]  $TDIP[$INDEX]\n";
}

# Check the gathered output data.
die "test1.out wrong.\n"  if (abs($HEAT_FORM   [ 1]-(  -36.87 )) > 0.01 /2 );
die "test2.out wrong.\n"  if (abs($HEAT_FORM   [ 2]-(  -47.545)) > 0.001/2 );
die "test3.out wrong.\n"  if (abs($HEAT_FORM   [ 3]-( -102.744)) > 0.001/2 );
die "test4.out wrong.\n"  if (abs($HEAT_FORM   [ 4]-(-14.18939)) > 0.00001 /2 );
die "test5.out wrong.\n"  if (abs($HEAT_FORM   [ 5]-(  -18.858)) > 0.001/2 );
die "test6.out wrong.\n"  if (abs($HEAT_FORM   [ 6]-(   35.181)) > 0.001/2 );
die "test7.out wrong.\n"  if (abs($HEAT_FORM   [ 7]-(  -31.372)) > 0.001/2 );
die "test7.out wrong.\n"  if (abs($GRAD_NORM   [ 7]-(  103.668)) > 0.001/2 );
die "test8.out wrong.\n"  if (abs($HEAT_FORM   [ 8]-(  219.415)) > 0.001/2 );
die "test9.out wrong.\n"  if (abs($HEAT_FORM   [ 9]-(  133.461)) > 0.001/2 );
die "test10.out wrong.\n" if (abs($HEAT_FORM   [10]-(  -83.322)) > 0.001/2 );
die "test11.out wrong.\n" if (abs($HEAT_FORM   [11]-(  219.933)) > 0.001/2 );
die "test13.out wrong.\n" if (abs($HEAT_FORM   [13]-( -107.496)) > 0.001/2 );
die "test14.out wrong.\n" if (abs($HEAT_FORM   [14]-(  -95.216)) > 0.001/2 );
die "test15.out wrong.\n" if (abs($HEAT_FORM   [15]-(   46.073)) > 0.001/2 );
die "test16.out wrong.\n" if (abs($HEAT_FORM   [16]-(   59.808)) > 0.001/2 );
die "test17.out wrong.\n" if (abs($HEAT_FORM   [17]-(   24.1  )) > 0.1  /2 );
die "test18.out wrong.\n" if (abs($HEAT_FORM   [18]-(  220.4  )) > 0.1  /2 );
die "test19.out wrong.\n" if (abs($HEAT_FORM   [19]-(   12.508)) > 0.001/2 );
die "test20.out wrong.\n" if (abs($HEAT_FORM   [20]-( -387.98 )) > 0.01 /2 );
die "test21.out wrong.\n" if (abs($HEAT_FORM   [21]-( -112.52 )) > 0.01 /2 );
die "test22.out wrong.\n" if (abs($HEAT_FORM   [22]-(  -43.65 )) > 0.01 /2 );
die "test23.out wrong.\n" if (abs($TOTAL_ENERGY[23]-( -219.65 )) > 0.01 /2 );
die "test24.out wrong.\n" if (abs($TOTAL_ENERGY[24]-( -985.50 )) > 0.01 /2 );
die "test25.out wrong.\n" if (abs($TOTAL_ENERGY[25]-( -319.07 )) > 0.01 /2 );
die "test26.out wrong.\n" if (abs($HEAT_FORM   [26]-(  -14.81 )) > 0.01 /2 );
die "test26.out wrong.\n" if (abs($FIRST_GRADT [26]-(   24.188)) > 0.001/2 );
die "test27.out wrong.\n" if (abs($HEAT_FORM   [27]-( -178.3  )) > 0.1  /2 );
die "test28.out wrong.\n" if (abs($HEAT_FORM   [28]-(   59.01 )) > 0.01 /2 );
die "test29.out wrong.\n" if (abs($DIPOLE   [29][1]- (    0.966  )) > 0.001  /2 );
die "test29.out wrong.\n" if (abs($DIPOLE   [29][2]- (    1.201  )) > 0.001  /2 );
die "test30.out wrong.\n" if (abs($TOTAL_ENERGY[30]-( -355.76 )) > 0.01 /2 );
die "test31.out wrong.\n" if (abs($HEAT_FORM   [31]-(  165.40 )) > 0.01 /2 );
die "test32.out wrong.\n" if (abs($HEAT_FORM   [32]-( -386.38 )) > 0.01 /2 );
die "test33.out wrong.\n" if (abs($TOTAL_ENERGY[33]-( -319.10 )) > 0.01 /2 );
die "test34.out wrong.\n" if (abs($TOTAL_ENERGY[34]-( -355.89 )) > 0.01 /2 );
die "test35.out wrong.\n" if (abs($TOTAL_ENERGY[35]-( -260.83 )) > 0.01 /2 );
die "test36.out wrong.\n" if (abs($TOTAL_ENERGY[36]-( -269.68 )) > 0.01 /2 );
die "test37.out wrong.\n" if (abs($TOTAL_ENERGY[37]-( -227.29 )) > 0.01 /2 );
die "test38.out wrong.\n" if (abs($HP1_HP2     [38]-(    0.749)) > 0.001/2 );
die "test39.out wrong.\n" if (abs($TOTAL_ENERGY[39]-(-1532.3  )) > 0.1  /2 );
die "test40.out wrong.\n" if (abs($TOTAL_ENERGY[40]-(-1404.0  )) > 0.1  /2 );
die "test41.out wrong.\n" if (abs($TOTAL_ENERGY[41]-(  -22.50 )) > 0.01 /2 );
die "test42.out wrong.\n" if (abs($TOTAL_ENERGY[42]-( -390.98 )) > 0.01 /2 );
die "test43.out wrong.\n" if (abs($TOTAL_ENERGY[43]-( -413.66 )) > 0.01 /2 );
die "test44.out wrong.\n" if (abs($TOTAL_ENERGY[44]-( -413.56 )) > 0.01 /2 );
die "test45.out wrong.\n" if (abs($TOTAL_ENERGY[45]-( -412.95 )) > 0.01 /2 );
die "test46.out wrong.\n" if (abs($TOTAL_ENERGY[46]-( -390.58 )) > 0.01 /2 );
die "test47.out wrong.\n" if (abs($TOTAL_ENERGY[47]-( -390.9  )) > 0.1  /2 );
die "test48.out wrong.\n" if (abs($TOTAL_ENERGY[48]-( -390.36 )) > 0.01 /2 );
die "test49.out wrong.\n" if (abs($TOTAL_ENERGY[49]-( -778.7  )) > 0.1  /2 );
die "test49.out wrong.\n" if (abs($DIP_POLAR   [49]-(    2.06 )) > 0.01 /2 );
die "test50.out wrong.\n" if (abs($TOTAL_ENERGY[50]-( -395.4  )) > 0.1  /2 );
die "test51.out wrong.\n" if (abs($TOTAL_ENERGY[51]-( -781.4  )) > 0.1  /2 );
die "test52.out wrong.\n" if (abs($TOTAL_ENERGY[52]-(-1663.09 )) > 0.01 /2 );
die "test53.out wrong.\n" if (abs($TOTAL_ENERGY[53]-(-1703.052)) > 0.001/2 );
die "test54.out wrong.\n" if (abs($TOTAL_ENERGY[54]-(-1804.462)) > 0.001/2 );
die "test55.out wrong.\n" if (abs($TOTAL_ENERGY[55]-(-1472.55 )) > 0.01 /2 );
die "test56.out wrong.\n" if (abs($TOTAL_ENERGY[56]-$TOTAL_ENERGY[51]) > 0.1 /2 );
die "test57.out wrong.\n" if (abs($TOTAL_KCM[57]-(-9007.66 )) > 0.01 /2 );
die "test58.out wrong.\n" if (abs($TOTAL_KCM[58]-(-18019.91 )) > 0.01 /2 );
die "test59.out wrong.\n" if (abs($TOTAL_KCM[59]-(-54088.21 )) > 0.01 /2 );
die "test70.out wrong.\n" if (abs($TOTAL_KCM[70]-(-20900.16 )) > 0.01 /2 );
die "test71.out wrong.\n" if (abs($TOTAL_KCM[71]-(-20900.16 )) > 0.01 /2 );
die "test70.out and/or test71.out wrong.\n" if (abs($TOTAL_KCM[70]-$TOTAL_KCM[71]) > 0.01 /2 );
die "test72.out wrong.\n" if (abs($TOTAL_KCM[72]-(-30722.20 )) > 0.01 /2 );
die "test72.out wrong.\n" if (abs($DIP_POLAR   [72]-(    6.21 )) > 0.01 /2 );
# Check polarizability of dihydrogen along axes by dipole first numerical derivative.
  # Coulomb's law in atomic units for -/+ pair of point charges at 50 angstroms from origin in opposite directions.
  $ELEC_FIELD_AU=1.0/(1.0*(50.0/0.529167)**2)+(-1.0*-1.0)/(1.0*(50.0/0.529167)**2);
  # print "Elec field is (in a.u.): $ELEC_FIELD_AU\n";
  $M_XDIP=$XDIP[60];
  $M_YDIP=$YDIP[60];
  $M_ZDIP=$ZDIP[60];
  ##print "M_XDIP:     $M_XDIP\n";
  $MF_XDIP=$XDIP[61];
  $MF_YDIP=$YDIP[62];
  $MF_ZDIP=$ZDIP[63];
  ##print "MF_XDIP:    $MF_XDIP\n";
  $XF_DIP=$XDIP[67];
  $YF_DIP=$YDIP[68];
  $ZF_DIP=$ZDIP[69];
  ##print "XF_DIP:     $XF_DIP\n";
  $XDIFF=$MF_XDIP-$M_XDIP-$XF_DIP;
  $YDIFF=$MF_YDIP-$M_YDIP-$YF_DIP;
  $ZDIFF=$MF_ZDIP-$M_ZDIP-$ZF_DIP;
  ##print "XDIFF:      $XDIFF\n";
  $XPOL_AU=$XDIFF/$ELEC_FIELD_AU;
  $YPOL_AU=$YDIFF/$ELEC_FIELD_AU;
  $ZPOL_AU=$ZDIFF/$ELEC_FIELD_AU;
  ##print "ELEC_FIELD_AU:  $ELEC_FIELD_AU\n";
  ##print "XPOL_AU:    $XPOL_AU\n";
  $XPOL_CA=$XPOL_AU*(0.529167**3);
  $YPOL_CA=$YPOL_AU*(0.529167**3);
  $ZPOL_CA=$ZPOL_AU*(0.529167**3);
  $XPOL_ERROR=$XPOL_AU-$POLAR_XX[60];
  $YPOL_ERROR=$YPOL_AU-$POLAR_YY[60];
  $ZPOL_ERROR=$ZPOL_AU-$POLAR_ZZ[60];
  ##print "water calc: $XPOL_AU\n";
  ##print "water goal: $POLAR_XX[60]\n";
  die "test60.out xx polarizability wrong (uses test61.out and test67.out also).\n" if (abs($XPOL_ERROR) > 0.0010 );
  die "test60.out yy polarizability wrong (uses test62.out and test68.out also).\n" if (abs($YPOL_ERROR) > 0.0010 );
  die "test60.out zz polarizability wrong (uses test63.out and test69.out also).\n" if (abs($ZPOL_ERROR) > 0.0010 );
# Check polarizability of dihydrogen along axes by energy second numerical derivative.
  # Coulomb's law in atomic units for -/+ pair of point charges at 50 angstroms from origin in opposite directions.
  $ELEC_FIELD_AU=1.0/(1.0*(50.0/0.529167)**2)+(-1.0*-1.0)/(1.0*(50.0/0.529167)**2);
  $MOL_E=$ACC_HF[60]/627.515;
  $X_SPARKLES_E=$ACC_HF[67]/627.515;
  $Y_SPARKLES_E=$ACC_HF[68]/627.515;
  $Z_SPARKLES_E=$ACC_HF[69]/627.515;
  $MOL_PX_FIELD_E=$ACC_HF[61]/627.515-$X_SPARKLES_E;
  $MOL_PY_FIELD_E=$ACC_HF[62]/627.515-$Y_SPARKLES_E;
  $MOL_PZ_FIELD_E=$ACC_HF[63]/627.515-$Z_SPARKLES_E;
  $MOL_NX_FIELD_E=$ACC_HF[64]/627.515-$X_SPARKLES_E;
  $MOL_NY_FIELD_E=$ACC_HF[65]/627.515-$Y_SPARKLES_E;
  $MOL_NZ_FIELD_E=$ACC_HF[66]/627.515-$Z_SPARKLES_E;
  $E_POLAR_XX_AU=(2.0 * $MOL_E - $MOL_PX_FIELD_E - $MOL_NX_FIELD_E)/($ELEC_FIELD_AU**2);
  $E_POLAR_YY_AU=(2.0 * $MOL_E - $MOL_PY_FIELD_E - $MOL_NY_FIELD_E)/($ELEC_FIELD_AU**2);
  $E_POLAR_ZZ_AU=(2.0 * $MOL_E - $MOL_PZ_FIELD_E - $MOL_NZ_FIELD_E)/($ELEC_FIELD_AU**2);
  $E_POLAR_XX=$E_POLAR_XX_AU*0.529167**3;
  $E_POLAR_YY=$E_POLAR_YY_AU*0.529167**3;
  $E_POLAR_ZZ=$E_POLAR_ZZ_AU*0.529167**3;
  ##print "POLARIZABILITY OF test60 XX COMPONENT IS (cubic angstroms): $E_POLAR_XX\n";
  ##print "POLARIZABILITY OF test60 YY COMPONENT IS (cubic angstroms): $E_POLAR_YY\n";
  ##print "POLARIZABILITY OF test60 ZZ COMPONENT IS (cubic angstroms): $E_POLAR_ZZ\n";
  $XPOL_ERROR=$E_POLAR_XX-$POLAR_XX[60];
  $XPOL_ERROR=$E_POLAR_YY-$POLAR_YY[60];
  $XPOL_ERROR=$E_POLAR_ZZ-$POLAR_ZZ[60];
  die "test60.out xx polarizability wrong (uses test61.dat, test64.dat, and test67.dat also).\n" if (abs($XPOL_ERROR) > 0.0010 );
  die "test60.out yy polarizability wrong (uses test62.dat, test65.dat, and test68.dat also).\n" if (abs($YPOL_ERROR) > 0.0010 );
  die "test60.out zz polarizability wrong (uses test63.dat, test66.dat, and test69.dat also).\n" if (abs($ZPOL_ERROR) > 0.0010 );
die "test73.out wrong.\n" if (abs($TOTAL_KCM[73]-(-57334.20 )) > 0.01 /2 );
die "test74.out wrong.\n" if (abs($TOTAL_KCM[74]-(-57334.20 )) > 0.01 /2 );  # test73 and test74 should match
die "test75.out wrong.\n" if (abs($TOTAL_KCM[75]-(-33130.27 )) > 0.01 /2 );
die "test76.out wrong.\n" if (abs($TOTAL_KCM[76]-(-33130.27 )) > 0.01 /2 );  # test75 and test76 should match
die "test77.out wrong.\n" if (abs($TOTAL_KCM[77]-( -6204.10 )) > 0.01 /2 );
die "test78.out wrong.\n" if (abs($TOTAL_KCM[78]-( -6204.10 )) > 0.01 /2 );  # test77 and test78 should match
die "test78.out wrong.\n" if (abs($DIPOLE   [78][1]- (    1.307  )) > 0.01  /2 );
die "test79.out wrong.\n" if (abs($TOTAL_KCM[79]-(-111903.50)) > 0.01 /2 );  # used for reaction energ calculation check
$RXN_ENERGY= ($TOTAL_KCM[79]-3*$TOTAL_KCM[76]-2*$TOTAL_KCM[78])/5;
#printf "The reaction energy for 1/5 of 3a2n --> 3 H2SO4 + 2 NH3 is %14.8f.\n", $RXN_ENERGY;
die "test76.out, test78.out, and/or test79.out wrong.\n" if (abs($RXN_ENERGY - (  -20.90 ) ) > 0.01 /2 );

print "All test**.out files appear okay.\n";

