#!/usr/bin/perl -w


##############################################
# SOME USEFUL CONVERSION FACTORS:
#
#   1 A**3 = 6.74855 au polarizability
#   1 D    = 0.393456 au dipole
#   1 au   = 2.54 debye
#
##############################################

# Distances here are in angstroms (get converted to a.u. before being used in Coulomb equation).
$ymin=   0;
$ymax=   0;
$ystep=  1;

$zmin=   0;
$zmax=   0;
$zstep=  1;

$xval=   50;

$charge= 1;  # each individual point charge in a.u.

$angtoau=1.0/0.529177;
print "Taking distances in angstroms and converting to a.u. with $angtoau.\n";

# Point to check electric field.
$xx=0;
$yy=0;
$zz=0;

if (scalar(@ARGV)>0) {
   die "ERROR!!  Need three components for point to check electric field at.\n" if (scalar(@ARGV)<3);
   $xx=$ARGV[0];
   $yy=$ARGV[1];
   $zz=$ARGV[2];
}



# Calculate electric field at ($xx,$yy,$zz).
$elx=0;
$ely=0;
$elz=0;

$total=0;
$y=$ymin;
$z=$zmin;

$x=$xval;
while ($y<=$ymax) {
  while ($z<=$zmax) {
    $total++;
    $dist=sqrt(($xx-$x)**2+($yy-$y)**2+($zz-$z)**2);
    #print "Distance: $dist\n";
    $xunit=($xx-$x)/$dist;
    $yunit=($yy-$y)/$dist;
    $zunit=($zz-$z)/$dist;
    $z+=$zstep;
    #printf "Unit vector:   %10.6f  %10.6f  %10.6f\n",$xunit,$yunit,$zunit;
    $newelx=$xunit*$charge/(($dist*$angtoau)**2);
    $newely=$yunit*$charge/(($dist*$angtoau)**2);
    $newelz=$zunit*$charge/(($dist*$angtoau)**2);
    #printf "Electric field contribution:   %20.16f  %20.16f  %20.16f\n",$newelx,$newely,$newelz;
    $elx = $elx+$newelx;
    $ely = $ely+$newely;
    $elz = $elz+$newelz;
  }
  $y+=$ystep;
  $z=$zmin;
}

printf "\nElectric field at point (%10.6f,%10.6f,%10.6f) (in Angstroms):  %14.10f  %14.10f  %14.10f (a.u.)\n", $xx,$yy,$zz,$elx,$ely,$elz;
