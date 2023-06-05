      SUBROUTINE RESETC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C   This subroutine resets all common block variables for successive
C   executions of the MOPAC subroutine (i.e., new MOPAC runs).
C   The DATA statements for literature reference character strings
C   are not reinitialized in this subroutine.
C

C  Common blocks in BLOCK DATA
      COMMON /NATORB/ NATORB(120)
      COMMON /ELEMTS/ ELEMNT(120)
     1       /ALPHA / ALP(120)
     2       /CORE  / CORE(120)
     3       /MULTIP/ DD(120),QQ(120),AM(120),AD(120),AQ(120)
     4       /EXPONT/ ZS(120),ZP(120),ZD(120)
     5       /ONELEC/ USS(120),UPP(120),UDD(120)
     6       /BETAS / BETAS(120),BETAP(120),BETAD(120)
     7       /TWOELE/ GSS(120),GSP(120),GPP(120),GP2(120),HSP(120),
     8                GSD(120),GPD(120),GDD(120)
     9       /ATOMIC/ EISOL(120),EHEAT(120)
     1       /VSIPS / VS(120),VP(120),VD(120)
     2       /ISTOPE/ AMS(120)
     3       /IDEAS / GAUSS1(120,10),GAUSS2(120,10),GAUSS3(120,10)
     4       /IDEAP / GAUSP1(120,10),GAUSP2(120,10),GAUSP3(120,10)
     5       /IDEAR / GAUSR1(120,10),GAUSR2(120,10),GAUSR3(120,10)
     6       /GAUSSB/ FN1(120),FN2(120)
     7       /IONPOT/ ATOMIP(120)
      COMMON /PM3DSB/ ALPPMD,R0PMD(120),S6PMD,C6PMD(120),USSPMD(120),
     &   UPPPMD(120),BETASD(120),BETAPD(120),ALPHAD(120),EISOLD(120)
      COMMON /AM1DSB/ ALPAMD,R0AMD(120),S6AMD,C6AMD(120),USSAMD(120),
     &   UPPAMD(120),BETASE(120),BETAPE(120),ALPHAE(120),EISOLE(120)
      COMMON /MNDODS/ ALPMND,R0MND(120),S6MND,C6MND(120),USSMND(120),
     &   UPPMND(120),BETASF(120),BETAPF(120),ALPHAF(120),EISOLF(120),
     &   GSSMND(120),GSPMND(120),GPPMND(120),GP2MND(120),HSPMND(120),
     &   ZSMND(120),ZPMND(120)
      COMMON /RM1DSB/ ALPRMD,R0RMD(120),S6RMD,C6RMD(120),USSRMD(120),
     &   UPPRMD(120),BETASI(120),BETAPI(120),ALPHAI(120),EISOLI(120)
      COMMON /PMOV1CB/ ALPPMOV1, R0PMOV1(120), S6PMOV1, C6PMOV1(120),   LF1010
     &                 USSPMOV1(120), UPPPMOV1(120), BETASPMOV1(120),
     &                 BETAPPMOV1(120), ALPHAPMOV1(120), 
     &                 EISOLPMOV1(120), GSSPMOV1(120), GSPPMOV1(120), 
     &                 GPPPMOV1(120), GP2PMOV1(120), HSPPMOV1(120), 
     &                 ZSPMOV1(120), ZPPMOV1(120), 
     &                 AMPMOV1(120), ADPMOV1(120), AQPMOV1(120),
     &                 DDPMOV1(120), QQPMOV1(120)
      COMMON /PMO2CB/  PRBETAPMO2 (120,120,2,2),KPAIRPMO2 (120,120,2,2),
     &                 C0ABPMO2 (120,120), C1ABPMO2 (120,120), 
     &                 C2ABPMO2 (120,120), PRALPPMO2 (120,120), 
     &                 C3ABPMO2 (120,120), C3RPWRPMO2 (120,120), 
     &                 PR3ALPPMO2 (120,120), CPAIRPMO2 (120,120), 
     &                 DPAIRPMO2 (120,120),
     &                 USSPMO2(120), UPPPMO2(120), ZSPMO2(120), 
     &                 ZPPMO2(120), GSSPMO2(120), GSPPMO2(120), 
     &                 GPPPMO2(120), GP2PMO2(120), HSPPMO2(120), 
     &                 ALPHAPMO2(120), AMPMO2(120), ADPMO2(120), 
     &                 AQPMO2(120), DDPMO2(120), QQPMO2(120), 
     &                 EISOLPMO2(120), C6PMO2(120), RVDWPMO2(120),
     &                 RPNUMPMO2, RPDENPMO2, DPDENPMO2, SPDENPMO2, 
     &                 S6PMO2, ALPDPMO2
      REAL*8           KPAIRPMO2
      COMMON /PMO2ACB/ PRBETAPMO2A(120,120,2,2),KPAIRPMO2A(120,120,2,2),
     &                 C0ABPMO2A (120,120), C1ABPMO2A (120,120), 
     &                 C2ABPMO2A (120,120), PRALPPMO2A (120,120), 
     &                 C3ABPMO2A (120,120), C3RPWRPMO2A (120,120), 
     &                 PR3ALPPMO2A (120,120), CPAIRPMO2A (120,120), 
     &                 DPAIRPMO2A (120,120),
     &                 USSPMO2A(120), UPPPMO2A(120), ZSPMO2A(120), 
     &                 ZPPMO2A(120), GSSPMO2A(120), GSPPMO2A(120), 
     &                 GPPPMO2A(120), GP2PMO2A(120), HSPPMO2A(120), 
     &                 ALPHAPMO2A(120), AMPMO2A(120), ADPMO2A(120), 
     &                 AQPMO2A(120), DDPMO2A(120), QQPMO2A(120), 
     &                 EISOLPMO2A(120), C6PMO2A(120), RVDWPMO2A(120),
     &                 RPNUMPMO2A, RPDENPMO2A, DPDENPMO2A, SPDENPMO2A, 
     &                 S6PMO2A, ALPDPMO2A
      REAL*8           KPAIRPMO2A
      COMMON /PM6DSB/ ALPP6D,R0P6D(120),S6P6D,C6P6D(120),USSP6D(120),
     &   UPPP6D(120),BETASJ(120),BETAPJ(120),ALPHAJ(120),EISOLJ(120)
      COMMON /PM6BLO/ USSPM6(120), UPPPM6(120), UDDPM6(120),ZSPM6(120),
     &ZPPM6(120), ZDPM6(120), BETASX(120), BETAPX(120), BETADX(120),
     &GSSPM6(120),GSPPM6(120),GPPPM6(120),GP2PM6(120),HSPPM6(120),
     &EISOLX(120), DDPM6(120), QQPM6(120), AMPM6(120), ADPM6(120),
     &AQPM6(120), GAUSX1(120,10), GAUSX2(120,10), GAUSX3(120,10),
     &ALPPM6(18,18), XPM6(18,18), ZSNPM6(18), ZPNPM6(18), ZDNPM6(18)
      COMMON /PDDGP/  USSPDG(120), UPPPDG(120), UDDPDG(120), ZSPDG(120),
     1ZPPDG(120), ZDPDG(120), BETASG(120), BETAPG(120), BETADG(120),
     2ALPPDG(120), EISOLG(120), DDPDG(120), QQPDG(120), AMPDG(120),
     3ADPDG(120), AQPDG(120) ,GSSPDG(120), GSPPDG(120), GPPPDG(120),
     4GP2PDG(120), HSPPDG(120),POLVOG(120),PAPDG(120),PBPDG(120),
     5DAPDG(120),DBPDG(120)
      COMMON /IDEAG / GAUSG1(120,10),GAUSG2(120,10),GAUSG3(120,10)
     1               ,NGAUSG(120)
      COMMON /PDDGM/  USSMDG(120), UPPMDG(120), UDDMDG(120), ZSMDG(120),
     1ZPMDG(120), ZDMDG(120), BETASH(120), BETAPH(120), BETADH(120),
     2ALPMDG(120), EISOLH(120), DDMDG(120), QQMDG(120), AMMDG(120),
     3ADMDG(120), AQMDG(120) ,GSSMDG(120), GSPMDG(120), GPPMDG(120),
     4GP2MDG(120), HSPMDG(120),POLVOH(120),PAMDG(120),PBMDG(120),
     5DAMDG(120),DBMDG(120)
      COMMON /RM1BLO/USSRM1(120), UPPRM1(120), UDDRM1(120), ZSRM1(120),
     1ZPRM1(120), ZDRM1(120), BETASR(120), BETAPR(120), BETADR(120),
     2ALPRM1(120), EISOLR(120), DDRM1(120), QQRM1(120), AMRM1(120),
     3ADRM1(120), AQRM1(120) ,GSSRM1(120), GSPRM1(120), GPPRM1(120),
     4GP2RM1(120), HSPRM1(120),POLVOR(120)
      COMMON /MNDO/  USSM(120), UPPM(120), UDDM(120), ZSM(120),
     1ZPM(120), ZDM(120), BETASM(120), BETAPM(120), BETADM(120),
     2ALPM(120), EISOLM(120), DDM(120), QQM(120), AMM(120), ADM(120),
     3AQM(120) ,GSSM(120), GSPM(120), GPPM(120), GP2M(120), HSPM(120),
     4POLVOM(120)
      COMMON /PM3 /  USSPM3(120), UPPPM3(120), UDDPM3(120), ZSPM3(120),
     1ZPPM3(120), ZDPM3(120), BETASP(120), BETAPP(120), BETADP(120),
     2ALPPM3(120), EISOLP(120), DDPM3(120), QQPM3(120), AMPM3(120),
     3ADPM3(120), AQPM3(120) ,GSSPM3(120), GSPPM3(120), GPPPM3(120),
     4GP2PM3(120), HSPPM3(120),POLVOP(120)
     5       /REFS/ REFMN(120), REFM3(120), REFAM(120), REFPM3(120),
     6REFPDG(120), REFMDG(120), REFRM(120), REFPMD(120),
     7REFAMD(120), REFPM6(120), REFMND(120),REFRMD(120),REFP6D(120),
     8REFPMOV1(120),REFD3(120), REFPMO2(120), REFPMO2A(120)             LF1211/LF0113/LF0614
      COMMON /ONELE3 /  USS3(18),UPP3(18)
     1       /TWOEL3 /  F03(120)
     2       /ATOMI3 /  EISOL3(18),EHEAT3(18)
     3       /BETA3  /  BETA3(153)
     4       /ALPHA3 /  ALP3(153)
     5       /EXPON3 /  ZS3(18),ZP3(18)
      COMMON /FIELD/ EFIELD(3)



C  Special variable types
      CHARACTER ELEMNT*2, REFMN*80, REFM3*80, REFAM*80, REFPM3*80
      CHARACTER REFPDG*80, REFMDG*80,REFRM*80,REFPMD*80,
     1          REFAMD*80, REFPM6*80, REFMND*80, REFRMD*80, REFP6D*80,
     2          REFPMOV1*80,REFD3*80, REFPMO2*80, REFPMO2A*80           LF1211 / LF0113 / LF0614



C  Set up special arrays I use for quickly replacing common block
C  arrays with array values that I specify in a source array.      
      CHARACTER*2  ELEMNT_INIT(120)
      DIMENSION  NATORB_INIT(120)
      DIMENSION  AMS_INIT(120)
      DIMENSION  CORE_INIT(120)
      DIMENSION  F03_INIT(120)

C  Here are the source arrays that are assigned later:
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.
      PMODVL(1)=  2.466D0                                               LF1010
      PMODVL(2)=  3.304D0                                               LF1010
      PMODVL(3)=  0.030D0                                               LF1010
      PMODVL(4)=  0.150D0                                               LF1010
      PMODVL(5)=  1.280D0                                               LF1010
      PMODVL(6)=  2.764D0                                               LF1010
      PMODVL(7)=  0.24325D0                                             LF0111

      DATA ELEMNT_INIT/                                                 LF1110
C        1    2    3    4    5    6    7    8    9   10                   ..
     & ' H','He','Li','Be',' B',' C',' N',' O',' F','Ne',                  ..
     1 'Na','Mg','Al','Si',' P',' S','Cl','Ar',' K','Ca',                  ..
     2 'Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu','Zn',                  ..
     3 'Ga','Ge','As','Se','Br','Kr','Rb','Sr',' Y','Zr',                  ..
     4 'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn',                  ..
     5 'Sb','Te',' I','Xe','Cs','Ba','La','Ce','Pr','Nd',                  ..
     6 'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb',                  ..
     7 'Lu','Hf','Ta',' W','Re','Os','Ir','Pt','Au','Hg',                  ..
     8 'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th',                  ..
     9 'Pa',' U','Np','Pu','Am','Cm','Bk','Cf','XX','Fm',                  ..
     & 'Md','Cb','++',' +','--',' -','Tv','Hp', '' , '' ,                  ..
     1  '' , '' , '' , '' , '' , '' , '' , '' , '' , '' /               LF1110
      DATA NATORB_INIT/                                                 LF1110
C        1    2    3    4    5    6    7    8    9   10                   ..
     &   1 ,  1 ,  4 ,  4 ,  4 ,  4 ,  4 ,  4 ,  4 ,  4 ,                 ..
     1   0 ,  4 ,  4 ,  4 ,  4 ,  4 ,  4 ,  4 ,  0 ,  4 ,                 ..
     2   9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  4 ,                 ..
     3   4 ,  4 ,  4 ,  4 ,  4 ,  4 ,  4 ,  4 ,  9 ,  9 ,                 ..
     4   9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  4 ,  4 ,  4 ,                 ..
     5   4 ,  4 ,  4 ,  4 ,  2 ,  2 ,  8 ,  8 ,  8 ,  8 ,                 ..
     6   8 ,  8 ,  8 ,  8 ,  8 ,  8 ,  8 ,  8 ,  8 ,  8 ,                 ..
     7   9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  4 ,                 ..
     8   4 ,  4 ,  4 ,  4 ,  4 ,  4 ,  0 ,  0 ,  0 ,  0 ,                 ..
     9   0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,                 ..
     &   0 ,  1 ,  0 ,  0 ,  0 ,  0 ,  0 ,  4 ,  0 ,  0 ,                 ..
     1   0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 /               LF1110
      DATA  AMS_INIT /  1.00790D0,  4.00260D0,  6.94000D0,  9.01218D0,
     110.81000D0, 12.01100D0, 14.00670D0, 15.99940D0, 18.99840D0,
     220.17900D0, 22.98977D0, 24.30500D0, 26.98154D0, 28.08550D0,
     330.97376D0, 32.06000D0, 35.45300D0, 39.94800D0, 39.09830D0,
     440.08000D0, 44.95590D0, 47.90000D0, 50.94150D0, 51.99600D0,
     554.93800D0, 55.84700D0, 58.93320D0, 58.71000D0, 63.54600D0,
     665.38000D0, 69.73500D0, 72.59000D0, 74.92160D0, 78.96000D0,
     779.90400D0, 83.80000D0, 85.46780D0, 87.62000D0, 88.90590D0,
     891.22000D0, 92.90640D0, 95.94000D0, 98.90620D0, 101.0700D0,
     9102.9055D0, 106.4000D0, 107.8680D0, 112.4100D0, 114.8200D0,
     1118.6900D0, 121.7500D0, 127.6000D0, 126.9045D0, 131.3000D0,
     2132.9054D0, 137.3300D0, 15*0.000D0, 178.4900D0, 180.9479D0,
     3183.8500D0, 186.2070D0, 190.2000D0, 192.2200D0, 195.0900D0,
     4196.9665D0, 200.5900D0, 204.3700D0, 207.2000D0, 208.9804D0,
     518*0.000D0,   1.0079D0,  5*0.000D0,
     &13*0.0D0/                                                         LF0710
      DATA CORE_INIT/1.D0,0.D0,
     1 1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,0.D0,
     2 1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,0.D0,
     3 1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,8.D0,9.D0,10.D0,11.D0,2.D0,
     4 3.D0,4.D0,5.D0,6.D0,7.D0,0.D0,
     5 1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,8.D0,9.D0,10.D0,11.D0,2.D0,
     6 3.D0,4.D0,5.D0,6.D0,7.D0,0.D0,
     7 1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,8.D0,9.D0,10.D0,
     8 11.D0,12.D0,13.D0,14.D0,15.D0,16.D0,
     9 3.D0,4.D0,5.D0,6.D0,7.D0,8.D0,9.D0,10.D0,11.D0,2.D0,
     1 3.D0,4.D0,5.D0,6.D0,7.D0,0.D0,
     2  15*0.D0,1.D0,2.D0,1.D0,-2.D0,-1.D0,0.D0,
     & 1.D0,12*0.D0/                                                    LF0610
      DATA F03_INIT         /  12.848D0, 10.0D0, 10.0D0, 0.0D0,
     1  8.958D0, 10.833D0, 12.377D0, 13.985D0, 16.250D0,
     2         10.000D0, 10.000D0, 0.000D0, 0.000D0,7.57D0 ,  9.00D0 ,
     3         10.20D0 , 11.73,10.0D0,35*0.D0,10.D0,53*10.D0,
     &         13*0.00D0/                                               LF0710

C  Now set the arrays
      ELEMNT=ELEMNT_INIT
      NATORB=NATORB_INIT
      AMS=AMS_INIT
      CORE=CORE_INIT
      F03=F03_INIT



C  Set variables
      EFIELD = 0.0D00

      POLVOM(1) =0.2287D0 
      POLVOM(6) =0.2647D0 
      POLVOM(7) =0.3584D0 
      POLVOM(8) =0.2324D0 
      POLVOM(9) =0.1982D0 
      POLVOM(17)=1.3236D0 
      POLVOM(35)=2.2583D0 
      POLVOM(53)=4.0930D0 


* ALL THE OTHER ELEMENTS ARE TAKEN FROM CRC HANDBOOK 1981-1982
      EHEAT(1)  = 52.102D0
      EHEAT(2)  =  0.000D0
      EHEAT(3)  = 38.410D0
      EHEAT(4)  = 76.960D0
      EHEAT(5)  =135.700D0
      EHEAT(6)  =170.890D0
      EHEAT(7)  =113.000D0
      EHEAT(8)  = 59.559D0
      EHEAT(9)  = 18.890D0
      EHEAT(10) =  0.000D0
C#      DATA EHEAT(11) = 25.850D0
      EHEAT(12) = 35.000D0
      EHEAT(13) = 79.490D0
      EHEAT(14) =108.390D0
      EHEAT(15) = 75.570D0
      EHEAT(16) = 66.400D0
      EHEAT(17) = 28.990D0
      EHEAT(18) =  0.000D0
C#      DATA EHEAT(19) = 21.420D0
      EHEAT(20) = 42.600D0
      EHEAT(21) = 90.300D0
      EHEAT(22) =112.300D0
      EHEAT(23) =122.900D0
      EHEAT(24) = 95.000D0
      EHEAT(25) = 67.700D0
      EHEAT(26) = 99.300D0
      EHEAT(27) =102.400D0
      EHEAT(28) =102.800D0
      EHEAT(29) = 80.700D0
      EHEAT(30) = 31.170D0
      EHEAT(31) = 65.400D0
      EHEAT(32) = 89.500D0
      EHEAT(33) = 72.300D0
      EHEAT(34) = 54.300D0
      EHEAT(35) = 26.740D0
      EHEAT(36) =  0.000D0
      EHEAT(37) = 19.600D0
      EHEAT(38) = 39.100D0
      EHEAT(39) =101.500D0
      EHEAT(40) =145.500D0
      EHEAT(41) =172.400D0
      EHEAT(42) =157.300D0
      EHEAT(44) =155.500D0
      EHEAT(45) =133.000D0
      EHEAT(46) = 90.000D0
      EHEAT(47) = 68.100D0
      EHEAT(48) = 26.720D0
      EHEAT(49) = 58.000D0
      EHEAT(50) = 72.200D0
      EHEAT(51) = 63.200D0
      EHEAT(52) = 47.000D0
      EHEAT(53) = 25.517D0
      EHEAT(54) =  0.000D0
      EHEAT(55) = 18.700D0
      EHEAT(56) = 42.500D0
      EHEAT(58) =101.300D0
      EHEAT(62) = 49.400D0
      EHEAT(68) = 75.800D0
      EHEAT(70) = 36.350D0
      EHEAT(72) =148.000D0
      EHEAT(73) =186.900D0
      EHEAT(74) =203.100D0
      EHEAT(75) =185.000D0
      EHEAT(76) =188.000D0
      EHEAT(77) =160.000D0
      EHEAT(78) =135.200D0
      EHEAT(79) = 88.000D0
      EHEAT(80) = 14.690D0
      EHEAT(81) = 43.550D0
      EHEAT(82) = 46.620D0
      EHEAT(83) = 50.100D0
      EHEAT(86) =  0.000D0
      EHEAT(102) = 207.0D0
      EHEAT(108) = 52.102D0 

      VS(1) =  -13.605  
      VS(5)=-15.16D00
      VS(6)=-21.34D00
      VS(7)=-27.51D00
      VS(8)=-35.30D00
      VS(9)=-43.70D00
      VS(14)=-17.82D00
      VS(15)=-21.10D00
      VS(16)=-23.84D00
      VS(17)=-25.26D00
      VP(1)  =  0.0D00  
      VP(5)=-8.52D00
      VP(6)=-11.54D00
      VP(7)=-14.34D00
      VP(8)=-17.91D00
      VP(9)=-20.89D00
      VP(14)=-8.51D00
      VP(15)=-10.29D00
      VP(16)=-12.41D00
      VP(17)=-15.09D00

C     NPQ is not a common block variable, so skip this:
C      DATA NPQ/1,1, 2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3, 4,4,4,4,4,4,4,4,
C     +4,4,4,4,4,4,4,4,4,4, 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5/




      GSSM(1)  =  12.848D00 
      GSSM(4) = 9.00D00
      GSSM(5) = 10.59D00
      GSSM(6)  =  12.23D00 
      GSSM(7) = 13.59D00
      GSSM(8) = 15.42D00
      GSSM(9) = 16.92D00
      GSSM(13) = 8.09D00
      GSSM(14) = 9.82D00
      GSSM(15) = 11.56D00
      GSSM(16) = 12.88D00
      GSSM(17) = 15.03D00
      GSSM(35) = 15.03643948D0
      GSSM(53) = 15.04044855D0
      GPPM(4) = 6.97D00
      GPPM(5) = 8.86D00
      GPPM(6)  =  11.08D00 
      GPPM(7) = 12.98D00
      GPPM(8) = 14.52D00
      GPPM(9) = 16.71D00
      GPPM(13) = 5.98D00
      GPPM(14) = 7.31D00
      GPPM(15) = 8.64D00
      GPPM(16) = 9.90D00
      GPPM(17) = 11.30D00
      GPPM(35) = 11.27632539D0
      GPPM(53) = 11.14778369D0
      GSPM(4) = 7.43D00
      GSPM(5) = 9.56D00
      GSPM(6)  =  11.47D00 
      GSPM(7) = 12.66D00
      GSPM(8) = 14.48D00
      GSPM(9) = 17.25D00
      GSPM(13) = 6.63D00
      GSPM(14) = 8.36D00
      GSPM(15) = 10.08D00
      GSPM(16) = 11.26D00
      GSPM(17) = 13.16D00
      GSPM(35) = 13.03468242D0
      GSPM(53) = 13.05655798D0
      GP2M(4) = 6.22D00
      GP2M(5) = 7.86D00
      GP2M(6)  =  9.84D00 
      GP2M(7) = 11.59D00
      GP2M(8) = 12.98D00
      GP2M(9) = 14.91D00
      GP2M(13) = 5.40D00
      GP2M(14) = 6.54D00
      GP2M(15) = 7.68D00
      GP2M(16) = 8.83D00
      GP2M(17) = 9.97D00
      GP2M(35) = 9.85442552D0
      GP2M(53) = 9.91409071D0
      HSPM(4) = 1.28D00
      HSPM(5) = 1.81D00
      HSPM(6)  =  2.43D00 
      HSPM(7) = 3.14D00
      HSPM(8) = 3.94D00
      HSPM(9) = 4.83D00
      HSPM(13) = 0.70D00
      HSPM(14) = 1.32D00
      HSPM(15) = 1.92D00
      HSPM(16) = 2.26D00
      HSPM(17) = 2.42D00
      HSPM(35) = 2.45586832D0
      HSPM(53) = 2.45638202D0



C     START OF MINDO-3 PARAMETERS

      REFM3  ( 1)='  H: (MINDO/3): R.C.BINGHAM ET.AL., '//
     &                  'J.AM.CHEM.SO C. 97,1285,1294,1302,1307 (1975)'
      REFM3  ( 5)='  B: (MINDO/3): R.C.BINGHAM ET.AL., '//
     &                  'J.AM.CHEM.SO C. 97,1285,1294,1302,1307 (1975)'
      REFM3  ( 6)='  C: (MINDO/3): R.C.BINGHAM ET.AL., '//
     &                  'J.AM.CHEM.SO C. 97,1285,1294,1302,1307 (1975)'
      REFM3  ( 7)='  N: (MINDO/3): R.C.BINGHAM ET.AL., '//
     &                  'J.AM.CHEM.SO C. 97,1285,1294,1302,1307 (1975)'
      REFM3  ( 8)='  O: (MINDO/3): R.C.BINGHAM ET.AL., '//
     &                  'J.AM.CHEM.SO C. 97,1285,1294,1302,1307 (1975)'
      REFM3  ( 9)='  F: (MINDO/3): R.C.BINGHAM ET.AL., '//
     &                  'J.AM.CHEM.SO C. 97,1285,1294,1302,1307 (1975)'
      REFM3  (14)=' Si: (MINDO/3): R.C.BINGHAM ET.AL., '//
     &                  'J.AM.CHEM.SO C. 97,1285,1294,1302,1307 (1975)'
      REFM3  (15)='  P: (MINDO/3): R.C.BINGHAM ET.AL., '//
     &                  'J.AM.CHEM.SO C. 97,1285,1294,1302,1307 (1975)'
      REFM3  (16)='  S: (MINDO/3): R.C.BINGHAM ET.AL., '//
     &                  'J.AM.CHEM.SO C. 97,1285,1294,1302,1307 (1975)'
      REFM3  (17)=' Cl: (MINDO/3): R.C.BINGHAM ET.AL., '//
     &                  'J.AM.CHEM.SO C. 97,1285,1294,1302,1307 (1975)'

      USS3( 1)= -12.505D0
      USS3( 2)=   0.000D0
      USS3( 3)=   0.000D0
      USS3( 4)=   0.000D0
      USS3( 5)= -33.61D0
      USS3( 6)= -51.79D0
      USS3( 7)= -66.06D0
      USS3( 8)= -91.73D0
      USS3( 9)= -129.86D0
      USS3(10)=   0.0000D0 
      USS3(11)=   0.000D0 
      USS3(12)=   0.000D0 
      USS3(13)=   0.000D0 
      USS3(14)= -39.82D0 
      USS3(15)= -56.23D0 
      USS3(16)= -73.39D0 
      USS3(17)= -98.99D0 
      USS3(18)=   0.0D0

      UPP3( 1)=    0.0D0
      UPP3( 2)=    0.0D0
      UPP3( 3)=    0.0D0
      UPP3( 4)=    0.0D0
      UPP3( 5)=       -25.11D0 
      UPP3( 6)=    -39.18D0 
      UPP3( 7)=    -56.40D0 
      UPP3( 8)=    -78.80D0 
      UPP3( 9)=    -105.93D0 
      UPP3(10)=       0.000D0 
      UPP3(11)=    0.000D0 
      UPP3(12)=    0.000D0 
      UPP3(13)=    0.000D0 
      UPP3(14)=       -29.15D0 
      UPP3(15)=    -42.31D0 
      UPP3(16)=    -57.25D0 
      UPP3(17)=    -76.43D0 
      UPP3(18)=   0.0D0

      EISOL3( 1)=  -12.505D0 
      EISOL3( 2)= 0.0D0 
      EISOL3( 3)= 0.0D0 
      EISOL3( 4)=0.0D0 
      EISOL3( 5)= -61.70D0 
      EISOL3( 6)=-119.47D0 
      EISOL3( 7)= -187.51D0 
      EISOL3( 8)= -307.07D0 
      EISOL3( 9)= -475.00D0 
      EISOL3(10)= 0.0D0 
      EISOL3(11)= 0.0D0 
      EISOL3(12)= 0.0D0 
      EISOL3(13)= 0.0D0 
      EISOL3(14)= -90.98D0 
      EISOL3(15)= -150.81D0 
      EISOL3(16)= -229.15D0 
      EISOL3(17)= -345.93D0 
      EISOL3(18)= 0.0D0

      EHEAT3( 1)=   52.102D0 
      EHEAT3( 2)= 0.0D0 
      EHEAT3( 3)= 0.0D0 
      EHEAT3( 4)= 0.0D0 
      EHEAT3( 5)= 135.7 D0 
      EHEAT3( 6)= 170.89D0 
      EHEAT3( 7)=  113.0 D0 
      EHEAT3( 8)=  59.559D0 
      EHEAT3( 9)=  18.86D0 
      EHEAT3(10)= 0.0D0 
      EHEAT3(11)= 0.0D0 
      EHEAT3(12)= 0.0D0 
      EHEAT3(13)= 0.0D0 
      EHEAT3(14)= 106.0D0 
      EHEAT3(15)=   79.8D0 
      EHEAT3(16)=  65.65D0 
      EHEAT3(17)=  28.95D0 
      EHEAT3(18)= 0.0D0 





      BETA3(1)=   0.244770D0 
      ALP3(1) =   1.489450D0  
      BETA3(11)=   0.185347D0 
      ALP3(11)=    2.090352D0    
      BETA3(15)=   0.151324D0 
      ALP3(15)=    2.280544D0    
      BETA3(16)=   0.315011D0 
      ALP3(16)=    1.475836D0    
      BETA3(20)=   0.250031D0 
      ALP3(20)=    2.138291D0    
      BETA3(21)=   0.419907D0 
      ALP3(21)=    1.371208D0    
      BETA3(22)=   0.360776D0 
      ALP3(22)=    0.589380D0    
      BETA3(26)=   0.310959D0 
      ALP3(26)=    1.909763D0    
      BETA3(27)=   0.410886D0 
      ALP3(27)=    1.635259D0    
      BETA3(28)=   0.377342D0 
      ALP3(28)=    2.029618D0  
      BETA3(29)=   0.417759D0 
      ALP3(29)=    0.478901D0  
      BETA3(33)=   0.349745D0 
      ALP3(33)=    2.484827D0  
      BETA3(34)=   0.464514D0 
      ALP3(34)=    1.820975D0  
      BETA3(35)=   0.458110D0 
      ALP3(35)=    1.873859D0  
      BETA3(36)=   0.659407D0 
      ALP3(36)=    1.537190D0  
      BETA3(37)=   0.195242D0 
      ALP3(37)=    3.771362D0  
      BETA3(41)=   0.219591D0 
      ALP3(41)=    2.862183D0  
      BETA3(42)=   0.247494D0 
      ALP3(42)=    2.725913D0  
      BETA3(43)=   0.205347D0 
      ALP3(43)=    2.861667D0  
      BETA3(44)=   0.334044D0 
      ALP3(44)=    2.266949D0  
      BETA3(45)=   0.197464D0 
      ALP3(45)=    3.864997D0  
      BETA3(92)=   0.289647D0 
      ALP3(92)=    0.940789D0  
      BETA3(97)=   0.411377D0 
      ALP3(97)=    1.101382D0  
      BETA3(105)=   0.291703D0 
      ALP3(105)=    0.918432D0  
      BETA3(106)=   0.320118D0 
      ALP3(106)=    0.923170D0  
      BETA3(111)=   0.457816D0 
      ALP3(111)=    1.029693D0  
      BETA3(120)=   0.311790D0 
      ALP3(120)=    1.186652D0  
      BETA3(121)=   0.220654D0 
      ALP3(121)=    1.700698D0  
      BETA3(126)=   0.284620D0 
      ALP3(126)=    1.761370D0  
      BETA3(127)=   0.313170D0 
      ALP3(127)=    1.878176D0 
      BETA3(128)=   0.422890D0 
      ALP3(128)=    2.077240D0  
      BETA3(129)=   0.000000D0 
      ALP3(129)=    0.000000D0   
      BETA3(136)=   0.202489D0 
      ALP3(136)=    1.751617D0  
      BETA3(137)=   0.231653D0 
      ALP3(137)=    2.089404D0  
      BETA3(142)=   0.315480D0 
      ALP3(142)=    1.676222D0  
      BETA3(143)=   0.302298D0 
      ALP3(143)=    1.817064D0  
      BETA3(144)=   0.000000D0 
      ALP3(144)=    0.000000D0  
      BETA3(151)=   0.277322D0 
      ALP3(151)=    1.543720D0  
      BETA3(152)=   0.221764D0 
      ALP3(152)=    1.950318D0  
      BETA3(153)=   0.258969D0 
      ALP3(153)=    1.792125D0  

      ZS3(1)=   1.3D0       
      ZP3(1)=    0.0D0            
      ZS3(5)=   1.211156D0 
      ZP3(5)=    0.972826D0       
      ZS3(6)=   1.739391D0 
      ZP3(6)=    1.709645D0       
      ZS3(7)=   2.704546D0 
      ZP3(7)=    1.870839D0       
      ZS3(8)=   3.640575D0 
      ZP3(8)=    2.168448D0       
      ZS3(9)=   3.111270D0 
      ZP3(9)=    1.41986D0       
      ZS3(14)=   1.629173D0 
      ZP3(14)=    1.381721D0     
      ZS3(15)=   1.926108D0 
      ZP3(15)=    1.590665D0     
      ZS3(16)=   1.719480D0 
      ZP3(16)=    1.403205D0     
      ZS3(17)=   3.430887D0 
      ZP3(17)=    1.627017D0     

      EHEAT(103)=        0.0D0
      VS(103)=          10.0D0
      ALP(103)=          1.5D0
      EISOL(103)=        0.0D0
      AM(103)=           0.5D0
      ALPM(103)=          1.5D0
      EISOLM(103)=        0.0D0
      AMM(103)=           0.5D0
      ALPPM3(103)=          1.5D0
      EISOLP(103)=        0.0D0
      AMPM3(103)=           0.5D0

      EHEAT(104)=        0.0D0
      VS(104)=          10.0D0
      ALP(104)=          1.5D0
      EISOL(104)=        0.0D0
      AM(104)=           0.5D0
      ALPM(104)=          1.5D0
      EISOLM(104)=        0.0D0
      AMM(104)=           0.5D0
      ALPPM3(104)=          1.5D0
      EISOLP(104)=        0.0D0
      AMPM3(104)=           0.5D0

      EHEAT(105)=        0.0D0
      VS(105)=          10.0D0
      ALP(105)=          1.5D0
      EISOL(105)=        0.0D0
      AM(105)=           0.5D0
      ALPM(105)=          1.5D0
      EISOLM(105)=        0.0D0
      AMM(105)=           0.5D0
      ALPPM3(105)=          1.5D0
      EISOLP(105)=        0.0D0
      AMPM3(105)=           0.5D0

      EHEAT(106)=        0.0D0
      VS(106)=          10.0D0
      ALP(106)=          1.5D0
      EISOL(106)=        0.0D0
      AM(106)=           0.5D0
      ALPM(106)=          1.5D0
      EISOLM(106)=        0.0D0
      AMM(106)=           0.5D0
      ALPPM3(106)=          1.5D0
      EISOLP(106)=        0.0D0
      AMPM3(106)=           0.5D0

      ALPRM1(103)=       1.5D0
      EISOLR(103)=       0.0D0
      AMRM1(103)=        0.5D0
      ALPRM1(104)=       1.5D0
      EISOLR(104)=       0.0D0
      AMRM1(104)=       0.5D0      
      ALPRM1(105)=       1.5D0
      EISOLR(105)=       0.0D0
      AMRM1(105)=        0.5D0      
      ALPRM1(106)=       1.5D0
      EISOLR(106)=       0.0D0
      AMRM1(106)=        0.5D0      



C     START OF MNDO PARAMETERS

      REFMN  ( 1)='  H: (MNDO):  '//
     &'M.J.S. DEWAR, W. THIEL, J. AM. CHEM. SOC., 99, 4899, (1977)'
      REFMN  ( 3)=' Li: (MNDO):  '//
     &'TAKEN FROM MNDOC BY W.THIEL, QCPE NO.438, V. 2, P.63, (1982).'
      REFMN  ( 4)=' Be: (MNDO):  '//
     &'M.J.S. DEWAR, H.S. RZEPA, J. AM. CHEM. SOC., 100, 777, (1978)'
      REFMN  ( 5)='  B: (MNDO):  '//
     &'M.J.S. DEWAR, M.L. MCKEE, J. AM. CHEM. SOC., 99, 5231, (1977)'
      REFMN  ( 6)='  C: (MNDO):  '//
     &'M.J.S. DEWAR, W. THIEL, J. AM. CHEM. SOC., 99, 4899, (1977)'
      REFMN  ( 7)='  N: (MNDO):  '//
     &'M.J.S. DEWAR, W. THIEL, J. AM. CHEM. SOC., 99, 4899, (1977)'
      REFMN  ( 8)='  O: (MNDO):  '//
     &'M.J.S. DEWAR, W. THIEL, J. AM. CHEM. SOC., 99, 4899, (1977)'
      REFMN  ( 9)='  F: (MNDO):  '//
     &'M.J.S. DEWAR, H.S. RZEPA, J. AM. CHEM. SOC., 100, 777, (1978)'
      REFMN  (11)=' Na: (MNDO):  '//
     &'SODIUM-LIKE SPARKLE.   USE WITH CARE.'
      REFMN  (13)=' Al: (MNDO):  '//
     &'L.P. DAVIS, ET.AL.  J. COMP. CHEM. , 2, 433, (1981) SEE MANUAL.'
      REFMN  (14)=' Si: (MNDO):  '//
     &'M.J.S.DEWAR, ET. AL. ORGANOMETALLICS  5, 375 (1986)'
      REFMN  (15)='  P: (MNDO):  M.J.S.'//
     &'DEWAR, M.L.MCKEE, H.S.RZEPA, J. AM. CHEM. SOC., 100 3607 1978'
      REFMN  (16)='  S: (MNDO):  '//
     &'M.J.S.DEWAR, C.H. REYNOLDS, J. COMP. CHEM. 7, 140-143 (1986)'
      REFMN  (17)=' Cl: (MNDO):  '//
     &'M.J.S.DEWAR, H.S.RZEPA, J. COMP. CHEM., 4, 158, (1983)'
      REFMN  (19)='  K: (MNDO):  '//
     &'POTASSIUM-LIKE SPARKLE.  USE WITH CARE.'
      REFMN  (24)=' Cr: (MNDO):  '//
     &'M.J.S. DEWAR, E.F. HEALY, J.J.P. STEWART (IN PREPARATION)'
      REFMN  (30)=' Zn: (MNDO):  '//
     &'M.J.S. DEWAR, K.M. MERZ, ORGANOMETALLICS, 5, 1494-1496 (1986)'
      REFMN  (32)=' Ge: (MNDO):  M.J.S.'//
     &'DEWAR, G.L.GRADY, E.F.HEALY,ORGANOMETALLICS 6 186-189, (1987)'
      REFMN  (35)=' Br: (MNDO):  '//
     &'M.J.S.DEWAR, E.F. HEALY, J. COMP. CHEM., 4, 542, (1983)'
      REFMN  (50)=' Sn: (MNDO):  M.J.S.'//
     &'DEWAR,G.L.GRADY,J.J.P.STEWART, J.AM.CHEM.SOC.,106 6771 (1984)'
      REFMN  (53)='  I: (MNDO):  M.J.S.'//
     &'DEWAR, E.F. HEALY, J.J.P. STEWART, J.COMP.CHEM., 5,358,(1984)'
      REFMN  (80)=' Hg: (MNDO):  '//
     &'M.J.S.DEWAR,  ET. AL. ORGANOMETALLICS 4, 1964, (1985) SEE MANUAL'
      REFMN  (82)=' Pb: (MNDO):  '//
     &'M.J.S.DEWAR, ET.AL ORGANOMETALLICS 4 1973-1980 (1985)'
      REFMN  (90)=' Si: (MNDO):  M.J.S.'//
     &'DEWAR, M.L.MCKEE, H.S.RZEPA, J. AM. CHEM. SOC., 100 3607 1978'
      REFMN  (91)='  S: (MNDO):  M.J.S.'//
     &'DEWAR, H.S. RZEPA, M.L.MCKEE, J.AM.CHEM.SOC.100, 3607 (1978).'
      REFMN (102)=' Cb: (MNDO):  '//
     &'Capped Bond  (Hydrogen-like, takes on a zero charge.)'
      REFMN (108)=' Hp: (MNDO):  '//
     &'Hydrogen (with p basis functions)  '
                              
      USSM   ( 1)=        -11.9062760D0
      BETASM ( 1)=         -6.9890640D0
      ZSM    ( 1)=          1.3319670D0
      ALPM   ( 1)=          2.5441341D0
      EISOLM ( 1)=        -11.9062760D0
      AMM    ( 1)=          0.4721793D0
      ADM    ( 1)=          0.4721793D0
      AQM    ( 1)=          0.4721793D0
      USSM   (  3)=         -5.1280000D0
      UPPM   (  3)=         -2.7212000D0
      BETASM (  3)=         -1.3500400D0
      BETAPM (  3)=         -1.3500400D0
      ZSM    (  3)=          0.7023800D0
      ZPM    (  3)=          0.7023800D0
      ALPM   (  3)=          1.2501400D0
      EISOLM (  3)=         -5.1280000D0
      GSSM   (  3)=          7.3000000D0
      GSPM   (  3)=          5.4200000D0
      GPPM   (  3)=          5.0000000D0
      GP2M   (  3)=          4.5200000D0
      HSPM   (  3)=          0.8300000D0
      DDM    (  3)=          2.0549783D0
      QQM    (  3)=          1.7437069D0
      AMM    (  3)=          0.2682837D0
      ADM    (  3)=          0.2269793D0
      AQM    (  3)=          0.2614581D0
      USSM   ( 4)=        -16.6023780D0
      UPPM   ( 4)=        -10.7037710D0
      BETASM ( 4)=         -4.0170960D0
      BETAPM ( 4)=         -4.0170960D0
      ZSM    ( 4)=          1.0042100D0
      ZPM    ( 4)=          1.0042100D0
      ALPM   ( 4)=          1.6694340D0
      EISOLM ( 4)=        -24.2047560D0
      DDM    ( 4)=          1.4373245D0
      QQM    ( 4)=          1.2196103D0
      AMM    ( 4)=          0.3307607D0
      ADM    ( 4)=          0.3356142D0
      AQM    ( 4)=          0.3846373D0
      USSM   ( 5)=        -34.5471300D0
      UPPM   ( 5)=        -23.1216900D0
      BETASM ( 5)=         -8.2520540D0
      BETAPM ( 5)=         -8.2520540D0
      ZSM    ( 5)=          1.5068010D0
      ZPM    ( 5)=          1.5068010D0
      ALPM   ( 5)=          2.1349930D0
      EISOLM ( 5)=        -64.3159500D0
      DDM    ( 5)=          0.9579073D0
      QQM    ( 5)=          0.8128113D0
      AMM    ( 5)=          0.3891951D0
      ADM    ( 5)=          0.4904730D0
      AQM    ( 5)=          0.5556979D0
      USSM   ( 6)=        -52.2797450D0
      UPPM   ( 6)=        -39.2055580D0
      BETASM ( 6)=        -18.9850440D0
      BETAPM ( 6)=         -7.9341220D0
      ZSM    ( 6)=          1.7875370D0
      ZPM    ( 6)=          1.7875370D0
      ALPM   ( 6)=          2.5463800D0
      EISOLM ( 6)=       -120.5006060D0
      DDM    ( 6)=          0.8074662D0
      QQM    ( 6)=          0.6851578D0
      AMM    ( 6)=          0.4494671D0
      ADM    ( 6)=          0.6149474D0
      AQM    ( 6)=          0.6685897D0
      USSM   ( 7)=        -71.9321220D0
      UPPM   ( 7)=        -57.1723190D0
      BETASM ( 7)=        -20.4957580D0
      BETAPM ( 7)=        -20.4957580D0
      ZSM    ( 7)=          2.2556140D0
      ZPM    ( 7)=          2.2556140D0
      ALPM   ( 7)=          2.8613420D0
      EISOLM ( 7)=       -202.5662010D0
      DDM    ( 7)=          0.6399037D0
      QQM    ( 7)=          0.5429763D0
      AMM    ( 7)=          0.4994487D0
      ADM    ( 7)=          0.7843643D0
      AQM    ( 7)=          0.8126445D0
      USSM   ( 8)=        -99.6443090D0
      UPPM   ( 8)=        -77.7974720D0
      BETASM ( 8)=        -32.6880820D0
      BETAPM ( 8)=        -32.6880820D0
      ZSM    ( 8)=          2.6999050D0
      ZPM    ( 8)=          2.6999050D0
      ALPM   ( 8)=          3.1606040D0
      EISOLM ( 8)=       -317.8685060D0
      DDM    ( 8)=          0.5346024D0
      QQM    ( 8)=          0.4536252D0
      AMM    ( 8)=          0.5667034D0
      ADM    ( 8)=          0.9592562D0
      AQM    ( 8)=          0.9495934D0
      USSM   ( 9)=       -131.0715480D0
      UPPM   ( 9)=       -105.7821370D0
      BETASM ( 9)=        -48.2904660D0
      BETAPM ( 9)=        -36.5085400D0
      ZSM    ( 9)=          2.8484870D0
      ZPM    ( 9)=          2.8484870D0
      ALPM   ( 9)=          3.4196606D0
      EISOLM ( 9)=       -476.6837810D0
      DDM    ( 9)=          0.5067166D0
      QQM    ( 9)=          0.4299633D0
      AMM    ( 9)=          0.6218302D0
      ADM    ( 9)=          1.0850301D0
      AQM    ( 9)=          1.0343643D0
      EHEAT(11)=        0.0D0
      VS(11)=          10.0D0
      ALP(11)=          1.32D0
      EISOL(11)=        0.0D0
      AM(11)=           0.5D0
      ALPM(11)=          1.32D0
      EISOLM(11)=        0.0D0
      AMM(11)=           0.5D0
      USSM   (13)=        -23.8070970D0
      UPPM   (13)=        -17.5198780D0
      BETASM (13)=         -2.6702840D0
      BETAPM (13)=         -2.6702840D0
      ZSM    (13)=          1.4441610D0
      ZPM    (13)=          1.4441610D0
      ZDM    (13)=          1.0000000D0
      ALPM   (13)=          1.8688394D0
      EISOLM (13)=        -44.4840720D0
      DDM    (13)=          1.3992387D0
      QQM    (13)=          1.1586797D0
      AMM    (13)=          0.2973172D0
      ADM    (13)=          0.2635574D0
      AQM    (13)=          0.3673560D0
      USSM  (14)=        -37.0375330D0
      UPPM  (14)=        -27.7696780D0
      BETASM(14)=         -9.0868040D0
      BETAPM(14)=         -1.0758270D0
      ZSM   (14)=          1.3159860D0
      ZPM   (14)=          1.7099430D0
      ZDM   (14)=          1.0000000D0
      ALPM  (14)=          2.2053160D0
      EISOLM(14)=        -82.8394220D0
      DDM   (14)=          1.2580349D0
      QQM   (14)=          0.9785824D0
      AMM   (14)=          0.3608967D0
      ADM   (14)=          0.3664244D0
      AQM   (14)=          0.4506740D0
      USSM   (15)=        -56.1433600D0
      UPPM   (15)=        -42.8510800D0
      BETASM (15)=         -6.7916000D0
      BETAPM (15)=         -6.7916000D0
      ZSM    (15)=          2.1087200D0
      ZPM    (15)=          1.7858100D0
      ZDM    (15)=          1.0000000D0
      ALPM   (15)=          2.4152800D0
      EISOLM (15)=       -152.9599600D0
      DDM    (15)=          1.0129699D0
      QQM    (15)=          0.9370090D0
      AMM    (15)=          0.4248438D0
      ADM    (15)=          0.4882420D0
      AQM    (15)=          0.4979406D0
      USSM   (16)=        -72.2422810D0
      UPPM   (16)=        -56.9732070D0
      BETASM (16)=        -10.7616700D0
      BETAPM (16)=        -10.1084330D0
      ZSM    (16)=          2.3129620D0
      ZPM    (16)=          2.0091460D0
      ZDM    (16)=          1.0000000D0
      ALPM   (16)=          2.4780260D0
      EISOLM (16)=       -226.0123900D0
      DDM    (16)=          0.9189935D0
      QQM    (16)=          0.8328514D0
      AMM    (16)=          0.4733554D0
      ADM    (16)=          0.5544502D0
      AQM    (16)=          0.5585244D0
      USSM   (17)=       -100.2271660D0
      UPPM   (17)=        -77.3786670D0
      BETASM (17)=        -14.2623200D0
      BETAPM (17)=        -14.2623200D0
      ZSM    (17)=          3.7846450D0
      ZPM    (17)=          2.0362630D0
      ZDM    (17)=          1.0000000D0
      ALPM   (17)=          2.5422010D0
      EISOLM (17)=       -353.1176670D0
      DDM    (17)=          0.4986870D0
      QQM    (17)=          0.8217603D0
      AMM    (17)=          0.5523705D0
      ADM    (17)=          0.8061220D0
      AQM    (17)=          0.6053435D0
      EHEAT(19)=        0.0D0
      VS(19)=          10.0D0
      ALP(19)=          1.16D0
      EISOL(19)=        0.0D0
      AM(19)=           0.5D0
      ALPM(19)=          1.16D0
      EISOLM(19)=        0.0D0
      AMM(19)=           0.5D0
      USSM   (24)=        -17.5170270D0
      UPPM  (24)=        -12.5337290D0
      UDDM  (24)=        -44.1249280D0
      BETASM (24)=         -0.1000000D0
      BETAPM (24)=         -0.1000000D0
      BETADM (24)=         -8.7766360D0
      ZSM    (24)=          1.5000000D0
      ZPM    (24)=          1.5000000D0
      ZDM    (24)=          2.8845490D0
      ALPM   (24)=          3.0683070D0
      EISOLM (24)=       -134.8187920D0
      GSSM   (24)=          6.0000000D0
      GSPM   (24)=          4.1500000D0
      GPPM   (24)=          5.0000000D0
      GP2M   (24)=          3.5000000D0
      HSPM   (24)=          1.0000000D0
      GSD    (24)=          2.8746410D0
      GPD    (24)=          3.0000000D0
      GDD    (24)=          8.8949670D0
      DDM    (24)=          1.7320508D0
      QQM    (24)=          1.4142136D0
      AMM    (24)=          0.2205072D0
      ADM    (24)=          0.2711332D0
      AQM    (24)=          0.4464656D0
      USSM  ( 30)=        -20.8397160D0
      UPPM  ( 30)=        -19.6252240D0
      BETASM( 30)=         -1.0000000D0
      BETAPM( 30)=         -2.0000000D0
      ZSM   ( 30)=          2.0473590D0
      ZPM   ( 30)=          1.4609460D0
      ZDM   ( 30)=          1.0000000D0
      ALPM  ( 30)=          1.5064570D0
      EISOLM( 30)=        -29.8794320D0
      GSSM  ( 30)=         11.8000000D0
      GSPM  ( 30)=         11.1820180D0
      GPPM  ( 30)=         13.3000000D0
      GP2M  ( 30)=         12.9305200D0
      HSPM  ( 30)=          0.4846060D0
      DDM   ( 30)=          1.3037826D0
      QQM   ( 30)=          1.4520183D0
      AMM   ( 30)=          0.4336641D0
      ADM   ( 30)=          0.2375912D0
      AQM   ( 30)=          0.2738858D0
      USSM  ( 32)=        -33.9493670D0
      UPPM  ( 32)=        -27.4251050D0
      BETASM( 32)=         -4.5164790D0
      BETAPM( 32)=         -1.7555170D0
      ZSM   ( 32)=          1.2931800D0
      ZPM   ( 32)=          2.0205640D0
      ALPM  ( 32)=          1.9784980D0
      EISOLM( 32)=        -76.2489440D0
      GSSM  ( 32)=          9.8000000D0
      GSPM  ( 32)=          8.3000000D0
      GPPM  ( 32)=          7.3000000D0
      GP2M  ( 32)=          6.5000000D0
      HSPM  ( 32)=          1.3000000D0
      DDM   ( 32)=          1.2556091D0
      QQM   ( 32)=          1.0498655D0
      AMM   ( 32)=          0.3601617D0
      ADM   ( 32)=          0.3643722D0
      AQM   ( 32)=          0.4347337D0
      USSM   (35)=        -99.9864405D0
      UPPM   (35)=        -75.6713075D0
      BETASM (35)=         -8.9171070D0
      BETAPM (35)=         -9.9437400D0
      ZSM    (35)=          3.8543019D0
      ZPM    (35)=          2.1992091D0
      ZDM    (35)=          1.0000000D0
      ALPM   (35)=          2.4457051D0
      EISOLM (35)=       -346.6812500D0
      DDM    (35)=          0.6051074D0
      QQM    (35)=          0.9645873D0
      AMM    (35)=          0.5526068D0
      ADM    (35)=          0.7258330D0
      AQM    (35)=          0.5574589D0
      USSM  (50)=        -40.8518020D0
      UPPM   (50)=        -28.5602490D0
      BETASM (50)=         -3.2351470D0
      BETAPM (50)=         -4.2904160D0
      ZSM    (50)=          2.0803800D0
      ZPM   (50)=          1.9371060D0
      ALPM   (50)=          1.8008140D0
      EISOLM (50)=        -92.3241020D0
      GSSM   (50)=          9.8000000D0
      GSPM   (50)=          8.3000000D0
      GPPM   (50)=          7.3000000D0
      GP2M   (50)=          6.5000000D0
      HSPM   (50)=          1.3000000D0
      DDM    (50)=          1.5697766D0
      QQM    (50)=          1.3262292D0
      AMM    (50)=          0.3601617D0
      ADM    (50)=          0.3219998D0
      AQM    (50)=          0.3713827D0
      USSM   (53)=       -100.0030538D0
      UPPM   (53)=        -74.6114692D0
      BETASM (53)=         -7.4144510D0
      BETAPM (53)=         -6.1967810D0
      ZSM    (53)=          2.2729610D0
      ZPM    (53)=          2.1694980D0
      ZDM    (53)=          1.0000000D0
      ALPM   (53)=          2.2073200D0
      EISOLM (53)=       -340.5983600D0
      DDM    (53)=          1.4253233D0
      QQM    (53)=          1.1841707D0
      AMM    (53)=          0.5527541D0
      ADM    (53)=          0.4593451D0
      AQM    (53)=          0.4585376D0
      USSM   ( 80)=        -19.8095740D0
      UPPM   ( 80)=        -13.1025300D0
      BETASM ( 80)=         -0.4045250D0
      BETAPM ( 80)=         -6.2066830D0
      ZSM    ( 80)=          2.2181840D0
      ZPM    ( 80)=          2.0650380D0
      ALPM   ( 80)=          1.3356410D0
      EISOLM ( 80)=        -28.8191480D0
      GSSM   ( 80)=         10.8000000D0
      GSPM   ( 80)=          9.3000000D0
      GPPM   ( 80)=         14.3000000D0
      GP2M   ( 80)=         13.5000000D0
      HSPM   ( 80)=          1.3000000D0
      DDM    ( 80)=          1.7378048D0
      QQM    ( 80)=          1.4608064D0
      AMM    ( 80)=          0.3969129D0
      ADM    ( 80)=          0.3047694D0
      AQM    ( 80)=          0.3483102D0
      USSM   ( 82)=        -47.3196920D0
      UPPM   ( 82)=        -28.8475600D0
      BETASM ( 82)=         -8.0423870D0
      BETAPM ( 82)=         -3.0000000D0
      ZSM    ( 82)=          2.4982860D0
      ZPM    ( 82)=          2.0820710D0
      ALPM   ( 82)=          1.7283330D0
      EISOLM ( 82)=       -105.8345040D0
      GSSM   ( 82)=          9.8000000D0
      GSPM   ( 82)=          8.3000000D0
      GPPM   ( 82)=          7.3000000D0
      GP2M   ( 82)=          6.5000000D0
      HSPM   ( 82)=          1.3000000D0
      DDM    ( 82)=          1.5526624D0
      QQM    ( 82)=          1.4488558D0
      AMM    ( 82)=          0.3601617D0
      ADM    ( 82)=          0.3239309D0
      AQM    ( 82)=          0.3502057D0
      USSM   (90)=        -40.5682920D0
      UPPM   (90)=        -28.0891870D0
      BETASM (90)=         -4.2562180D0
      BETAPM (90)=         -4.2562180D0
      ZSM    (90)=          1.4353060D0
      ZPM    (90)=          1.4353060D0
      ZDM    (90)=          1.0000000D0
      ALPM   (90)=          2.1961078D0
      EISOLM (90)=        -90.5399580D0
      DDM    (90)=          1.4078712D0
      QQM    (90)=          1.1658281D0
      AMM    (90)=          0.3608967D0
      ADM    (90)=          0.3441817D0
      AQM    (90)=          0.3999442D0
      HSPM   (90)=          1.32D00
      GP2M   (90)=          6.54D00
      GPPM   (90)=          7.31D00
      GSPM   (90)=          8.36D00
      GSSM   (90)=          9.82D00
      USSM   (91)=        -75.2391520D0
      UPPM   (91)=        -57.8320130D0
      BETASM (91)=        -11.1422310D0
      BETAPM (91)=        -11.1422310D0
      ZSM    (91)=          2.6135910D0
      ZPM    (91)=          2.0343930D0
      ZDM    (91)=          1.0000000D0
      ALPM   (91)=          2.4916445D0
      EISOLM (91)=       -235.4413560D0
      DDM    (91)=          0.8231596D0
      QQM    (91)=          0.8225156D0
      AMM    (91)=          0.4733554D0
      ADM    (91)=          0.5889395D0
      AQM    (91)=          0.5632724D0
      USSM  (102)=        -11.9062760D0
      BETASM(102)=   -9999999.0000000D0
      ZSM   (102)=          4.0000000D0
      ZPM   (102)=          0.3000000D0
      ZDM   (102)=          0.3000000D0
      ALPM  (102)=          2.5441341D0
      EISOLM(102)=          4.0000000D0
      GSSM  (102)=         12.8480000D0
      HSPM  (102)=          0.1000000D0
      DDM   (102)=          0.0684105D0
      QQM   (102)=          1.0540926D0
      AMM   (102)=          0.4721793D0
      ADM   (102)=          0.9262742D0
      AQM   (102)=          0.2909059D0
      USSM  (108)=         -7.3964270D0
      UPPM  (108)=         -1.1486568D0
      BETASM(108)=         -9.6737870D0
      BETAPM(108)=         -1.3000000D0
      ZSM   (108)=          0.9880780D0
      ZPM   (108)=          0.3000000D0
      ALPM  (108)=          2.9323240D0
      EISOLM(108)=         28.7118620D0
      GSSM  (108)=         12.8480000D0
      GSPM  (108)=          2.1600000D0
      GPPM  (108)=          3.0100000D0
      GP2M  (108)=          2.4400000D0
      HSPM  (108)=          1.9200000D0
      DDM   (108)=          0.9675816D0
      QQM   (108)=          4.0824829D0
      AMM   (108)=          0.4721603D0
      ADM   (108)=          0.5007132D0
      AQM   (108)=          0.1603621D0



C     START OF AM1 PARAMETERS

      REFAM  ( 1)='  H: (AM1):  '//
     &            'M.J.S. DEWAR ET AL, J. AM. CHEM. SOC. 107 3902-'//
     &            '3909 (1985)'
      REFAM  ( 3)=' Li: (MNDO): '//
     &            'TAKEN FROM MNDOC BY W.THIEL, QCPE NO.438, V. 2,'//
     &            ' P.63, (1982).'
      REFAM  ( 4)=' Be: (MNDO): '//
     &            'M.J.S. DEWAR, H.S. RZEPA, J. AM. CHEM. SOC., 10'//
     &            '0, 777, (1978)'
      REFAM  ( 5)='  B: (AM1):  '//
     &            'M.J.S. DEWAR, C. JIE, E. G. ZOEBISCH ORGANOMETA'//
     &            'LLICS 7, 513 (1988)'
      REFAM  ( 6)='  C: (AM1):  '//
     &            'M.J.S. DEWAR ET AL, J. AM. CHEM. SOC. 107 3902-'//
     &            '3909 (1985)'
      REFAM  ( 7)='  N: (AM1):  '//
     &            'M.J.S. DEWAR ET AL, J. AM. CHEM. SOC. 107 3902-'//
     &            '3909 (1985)'
      REFAM  ( 8)='  O: (AM1):  '//
     &            'M.J.S. DEWAR ET AL, J. AM. CHEM. SOC. 107 3902-'//
     &            '3909 (1985)'
      REFAM  ( 9)='  F: (AM1):  '//
     &            'DEWAR AND ZOEBISCH, UNPUBLISHED WORK'
      REFAM  (11)=' Na: (MNDO): '//
     &            'SODIUM-LIKE SPARKLE.  USE WITH CARE.'
      REFAM  (13)=' Al: (AM1):  '//
     &            'M. J. S. Dewar, A. J. Holder, Organometallics, '//
     &            '9, 508-511 (1990).'
      REFAM  (14)=' Si: (AM1):  '//
     &            'M.J.S.DEWAR, C. JIE, ORGANOMETALLICS, 6, 1486-1'//
     &            '490 (1987).'
      REFAM  (15)='  P: (AM1):  '//
     &            'M.J.S.DEWAR, JIE, C, THEOCHEM, 187, 1 (1989)'
      REFAM  (16)='  S: (AM1):  '//
     &            'M.J.S.DEWAR, Y-C YUAN, THEOCHEM, IN PRESS'
      REFAM  (17)=' Cl: (AM1):  '//
     &            'DEWAR AND ZOEBISCH, UNPUBLISHED WORK'
      REFAM  (19)='  K: (AM1):  '//
     &            'POTASSIUM-LIKE SPARKLE.  USE WITH CARE.'
      REFAM  (30)=' Zn: (MNDO): '//
     &            'M.J.S. DEWAR, K.M. MERZ, ORGANOMETALLICS, 5, 14'//
     &            '94-1496 (1986)'
      REFAM  (35)=' Br: (AM1):  '//
     &            'DEWAR AND ZOEBISCH, UNPUBLISHED WORK'
      REFAM  (53)='  I: (AM1):  '//
     &            'DEWAR AND ZOEBISCH, UNPUBLISHED WORK'
      REFAM  (90)=' Si: (MNDO): '//
     &            'M.J.S.DEWAR, M.L.MCKEE, H.S.RZEPA, J. AM. CHEM.'//
     &            ' SOC., 100 3607 1978'
      REFAM  (91)='  S: (MNDO): '//
     &            'M.J.S.DEWAR, H.S. RZEPA, M.L.MCKEE, J.AM.CHEM.S'//
     &            'OC.100, 3607 (1978).'
      REFAM (102)=' Cb: (AM1):  '//
     &            'Capped Bond  (Hydrogen-like, takes on zero charge.)'
      REFAM (108)=' Hp: (AM1):  Hydrogen (with p basis functions)'

      USS   ( 1)=        -11.3964270D0
      BETAS ( 1)=         -6.1737870D0
      ZS    ( 1)=          1.1880780D0
      ALP   ( 1)=          2.8823240D0
      EISOL ( 1)=        -11.3964270D0
      GSS   ( 1)=         12.8480000D0
      AM    ( 1)=          0.4721793D0
      AD    ( 1)=          0.4721793D0
      AQ    ( 1)=          0.4721793D0
      GAUSS1( 1,1)=          0.1227960D0
      GAUSS2( 1,1)=          5.0000000D0
      GAUSS3( 1,1)=          1.2000000D0
      GAUSS1( 1,2)=          0.0050900D0
      GAUSS2( 1,2)=          5.0000000D0
      GAUSS3( 1,2)=          1.8000000D0
      GAUSS1( 1,3)=         -0.0183360D0
      GAUSS2( 1,3)=          2.0000000D0
      GAUSS3( 1,3)=          2.1000000D0
      USS   (  3)=         -5.1280000D0
      UPP   (  3)=         -2.7212000D0
      BETAS (  3)=         -1.3500400D0
      BETAP (  3)=         -1.3500400D0
      ZS    (  3)=          0.7023800D0
      ZP    (  3)=          0.7023800D0
      ALP   (  3)=          1.2501400D0
      EISOL (  3)=         -5.1280000D0
      GSS   (  3)=          7.3000000D0
      GSP   (  3)=          5.4200000D0
      GPP   (  3)=          5.0000000D0
      GP2   (  3)=          4.5200000D0
      HSP   (  3)=          0.8300000D0
      DD    (  3)=          2.0549783D0
      QQ    (  3)=          1.7437069D0
      AM    (  3)=          0.2682837D0
      AD    (  3)=          0.2269793D0
      AQ    (  3)=          0.2614581D0
      USS   ( 4)=        -16.6023780D0
      UPP   ( 4)=        -10.7037710D0
      BETAS ( 4)=         -4.0170960D0
      BETAP ( 4)=         -4.0170960D0
      ZS    ( 4)=          1.0042100D0
      ZP    ( 4)=          1.0042100D0
      ALP   ( 4)=          1.6694340D0
      EISOL ( 4)=        -24.2047560D0
      GSS   ( 4)=          9.0000000D0
      GSP   ( 4)=          7.4300000D0
      GPP   ( 4)=          6.9700000D0
      GP2   ( 4)=          6.2200000D0
      HSP   ( 4)=          1.2800000D0
      DD    ( 4)=          1.4373245D0
      QQ    ( 4)=          1.2196103D0
      AM    ( 4)=          0.3307607D0
      AD    ( 4)=          0.3356142D0
      AQ    ( 4)=          0.3846373D0
      USS   (  5)=        -34.4928700D0
      UPP   (  5)=        -22.6315250D0
      BETAS (  5)=         -9.5991140D0
      BETAP (  5)=         -6.2737570D0
      ZS    (  5)=          1.6117090D0
      ZP    (  5)=          1.5553850D0
      ALP   (  5)=          2.4469090D0
      EISOL (  5)=        -63.7172650D0
      GSS   (  5)=         10.5900000D0
      GSP   (  5)=          9.5600000D0
      GPP   (  5)=          8.8600000D0
      GP2   (  5)=          7.8600000D0
      HSP   (  5)=          1.8100000D0
      DD    (  5)=          0.9107622D0
      QQ    (  5)=          0.7874223D0
      AM    (  5)=          0.3891951D0
      AD    (  5)=          0.5045152D0
      AQ    (  5)=          0.5678856D0
      USS   ( 6)=        -52.0286580D0
      UPP   ( 6)=        -39.6142390D0
      BETAS ( 6)=        -15.7157830D0
      BETAP ( 6)=         -7.7192830D0
      ZS    ( 6)=          1.8086650D0
      ZP    ( 6)=          1.6851160D0
      ALP   ( 6)=          2.6482740D0
      EISOL ( 6)=       -120.8157940D0
      GSS   ( 6)=         12.2300000D0
      GSP   ( 6)=         11.4700000D0
      GPP   ( 6)=         11.0800000D0
      GP2   ( 6)=          9.8400000D0
      HSP   ( 6)=          2.4300000D0
      DD    ( 6)=          0.8236736D0
      QQ    ( 6)=          0.7268015D0
      AM    ( 6)=          0.4494671D0
      AD    ( 6)=          0.6082946D0
      AQ    ( 6)=          0.6423492D0
      GAUSS1( 6,1)=          0.0113550D0
      GAUSS2( 6,1)=          5.0000000D0
      GAUSS3( 6,1)=          1.6000000D0
      GAUSS1( 6,2)=          0.0459240D0
      GAUSS2( 6,2)=          5.0000000D0
      GAUSS3( 6,2)=          1.8500000D0
      GAUSS1( 6,3)=         -0.0200610D0
      GAUSS2( 6,3)=          5.0000000D0
      GAUSS3( 6,3)=          2.0500000D0
      GAUSS1( 6,4)=         -0.0012600D0
      GAUSS2( 6,4)=          5.0000000D0
      GAUSS3( 6,4)=          2.6500000D0
      USS   ( 7)=        -71.8600000D0
      UPP   ( 7)=        -57.1675810D0
      BETAS ( 7)=        -20.2991100D0
      BETAP ( 7)=        -18.2386660D0
      ZS    ( 7)=          2.3154100D0
      ZP    ( 7)=          2.1579400D0
      ALP   ( 7)=          2.9472860D0
      EISOL ( 7)=       -202.4077430D0
      GSS   ( 7)=         13.5900000D0
      GSP   ( 7)=         12.6600000D0
      GPP   ( 7)=         12.9800000D0
      GP2   ( 7)=         11.5900000D0
      HSP   ( 7)=          3.1400000D0
      DD    ( 7)=          0.6433247D0
      QQ    ( 7)=          0.5675528D0
      AM    ( 7)=          0.4994487D0
      AD    ( 7)=          0.7820840D0
      AQ    ( 7)=          0.7883498D0
      GAUSS1( 7,1)=          0.0252510D0
      GAUSS2( 7,1)=          5.0000000D0
      GAUSS3( 7,1)=          1.5000000D0
      GAUSS1( 7,2)=          0.0289530D0
      GAUSS2( 7,2)=          5.0000000D0
      GAUSS3( 7,2)=          2.1000000D0
      GAUSS1( 7,3)=         -0.0058060D0
      GAUSS2( 7,3)=          2.0000000D0
      GAUSS3( 7,3)=          2.4000000D0
      USS   ( 8)=        -97.8300000D0
      UPP   ( 8)=        -78.2623800D0
      BETAS ( 8)=        -29.2727730D0
      BETAP ( 8)=        -29.2727730D0
      ZS    ( 8)=          3.1080320D0
      ZP    ( 8)=          2.5240390D0
      ALP   ( 8)=          4.4553710D0
      EISOL ( 8)=       -316.0995200D0
      GSS   ( 8)=         15.4200000D0
      GSP   ( 8)=         14.4800000D0
      GPP   ( 8)=         14.5200000D0
      GP2   ( 8)=         12.9800000D0
      HSP   ( 8)=          3.9400000D0
      DD    ( 8)=          0.4988896D0
      QQ    ( 8)=          0.4852322D0
      AM    ( 8)=          0.5667034D0
      AD    ( 8)=          0.9961066D0
      AQ    ( 8)=          0.9065223D0
      GAUSS1( 8,1)=          0.2809620D0
      GAUSS2( 8,1)=          5.0000000D0
      GAUSS3( 8,1)=          0.8479180D0
      GAUSS1( 8,2)=          0.0814300D0
      GAUSS2( 8,2)=          7.0000000D0
      GAUSS3( 8,2)=          1.4450710D0
      USS   ( 9)=       -136.1055790D0
      UPP   ( 9)=       -104.8898850D0
      BETAS ( 9)=        -69.5902770D0
      BETAP ( 9)=        -27.9223600D0
      ZS    ( 9)=          3.7700820D0
      ZP    ( 9)=          2.4946700D0
      ALP   ( 9)=          5.5178000D0
      EISOL ( 9)=       -482.2905830D0
      GSS   ( 9)=         16.9200000D0
      GSP   ( 9)=         17.2500000D0
      GPP   ( 9)=         16.7100000D0
      GP2   ( 9)=         14.9100000D0
      HSP   ( 9)=          4.8300000D0
      DD    ( 9)=          0.4145203D0
      QQ    ( 9)=          0.4909446D0
      AM    ( 9)=          0.6218302D0
      AD    ( 9)=          1.2088792D0
      AQ    ( 9)=          0.9449355D0
      GAUSS1( 9,1)=          0.2420790D0
      GAUSS2( 9,1)=          4.8000000D0
      GAUSS3( 9,1)=          0.9300000D0
      GAUSS1( 9,2)=          0.0036070D0
      GAUSS2( 9,2)=          4.6000000D0
      GAUSS3( 9,2)=          1.6600000D0
      USS   ( 13)=        -24.3535850D0
      UPP   ( 13)=        -18.3636450D0
      BETAS ( 13)=         -3.8668220D0
      BETAP ( 13)=         -2.3171460D0
      ZS    ( 13)=          1.5165930D0
      ZP    ( 13)=          1.3063470D0
      ZD    ( 13)=          1.0000000D0
      ALP   ( 13)=          1.9765860D0
      EISOL ( 13)=        -46.4208150D0
      GSS   ( 13)=          8.0900000D0
      GSP   ( 13)=          6.6300000D0
      GPP   ( 13)=          5.9800000D0
      GP2   ( 13)=          5.4000000D0
      HSP   ( 13)=          0.7000000D0
      DD    ( 13)=          1.4040443D0
      QQ    ( 13)=          1.2809154D0
      AM    ( 13)=          0.2973172D0
      AD    ( 13)=          0.2630229D0
      AQ    ( 13)=          0.3427832D0
      GAUSS1( 13,1)=          0.0900000D0
      GAUSS2( 13,1)=         12.3924430D0
      GAUSS3( 13,1)=          2.0503940D0
      USS   (14)=        -33.9536220D0
      UPP   (14)=        -28.9347490D0
      BETAS (14)=         -3.784852D0
      BETAP (14)=         -1.968123D0
      ZS    (14)=          1.830697D0
      ZP    (14)=          1.2849530D0
      ZD    (14)=          1.0000000D0
      ALP   (14)=          2.257816D0
      EISOL (14)=        -79.0017420D0
      GSS   (14)=          9.8200000D0
      GSP   (14)=          8.3600000D0
      GPP   (14)=          7.3100000D0
      GP2   (14)=          6.5400000D0
      HSP   (14)=          1.3200000D0
      DD    (14)=          1.1631107D0
      QQ    (14)=          1.3022422D0
      AM    (14)=          0.3608967D0
      AD    (14)=          0.3829813D0
      AQ    (14)=          0.3712106D0
      GAUSS1(14,1)=          0.25D0
      GAUSS2(14,1)=          9.000D0
      GAUSS3(14,1)=          0.911453D0
      GAUSS1(14,2)=          0.061513D0
      GAUSS2(14,2)=          5.00D0
      GAUSS3(14,2)=          1.995569D0
      GAUSS1(14,3)=          0.0207890D0
      GAUSS2(14,3)=          5.00D0
      GAUSS3(14,3)=          2.990610D0
      USS   ( 15)=        -42.0298630D0
      UPP   ( 15)=        -34.0307090D0
      BETAS ( 15)=         -6.3537640D0
      BETAP ( 15)=         -6.5907090D0
      ZS    ( 15)=          1.9812800D0
      ZP    ( 15)=          1.8751500D0
      ZD    ( 15)=          1.0000000D0
      ALP   ( 15)=          2.4553220D0
      EISOL ( 15)=       -124.4368355D0
      GSS   ( 15)=         11.5600050D0
      GSP   ( 15)=          5.2374490D0
      GPP   ( 15)=          7.8775890D0
      GP2   ( 15)=          7.3076480D0
      HSP   ( 15)=          0.7792380D0
      DD    ( 15)=          1.0452022D0
      QQ    ( 15)=          0.8923660D0
      AM    ( 15)=          0.4248440D0
      AD    ( 15)=          0.3275319D0
      AQ    ( 15)=          0.4386854D0
      GAUSS1( 15,1)=         -0.0318270D0
      GAUSS2( 15,1)=          6.0000000D0
      GAUSS3( 15,1)=          1.4743230D0
      GAUSS1( 15,2)=          0.0184700D0
      GAUSS2( 15,2)=          7.0000000D0
      GAUSS3( 15,2)=          1.7793540D0
      GAUSS1( 15,3)=          0.0332900D0
      GAUSS2( 15,3)=          9.0000000D0
      GAUSS3( 15,3)=          3.0065760D0
      USS   (16)=        -56.6940560D0
      UPP   (16)=        -48.7170490D0
      BETAS (16)=         -3.9205660D0
      BETAP (16)=         -7.9052780D0
      ZS    (16)=          2.3665150D0
      ZP    (16)=          1.6672630D0
      ZD    (16)=          1.0000000D0
      ALP   (16)=          2.4616480D0
      EISOL (16)=       -191.7321930D0
      GSS   (16)=         11.7863290D0
      GSP   (16)=          8.6631270D0
      GPP   (16)=         10.0393080D0
      GP2   (16)=          7.7816880D0
      HSP   (16)=          2.5321370D0
      DD    (16)=          0.9004265D0
      QQ    (16)=          1.0036329D0
      AM    (16)=          0.4331617D0
      AD    (16)=          0.5907115D0
      AQ    (16)=          0.6454943D0
      GAUSS1(16,1)=         -0.5091950D0
      GAUSS2(16,1)=          4.5936910D0
      GAUSS3(16,1)=          0.7706650D0
      GAUSS1(16,2)=         -0.0118630D0
      GAUSS2(16,2)=          5.8657310D0
      GAUSS3(16,2)=          1.5033130D0
      GAUSS1(16,3)=          0.0123340D0
      GAUSS2(16,3)=         13.5573360D0
      GAUSS3(16,3)=          2.0091730D0
      USS   (17)=       -111.6139480D0
      UPP   (17)=        -76.6401070D0
      BETAS (17)=        -24.5946700D0
      BETAP (17)=        -14.6372160D0
      ZS    (17)=          3.6313760D0
      ZP    (17)=          2.0767990D0
      ZD    (17)=          1.0000000D0
      ALP   (17)=          2.9193680D0
      EISOL (17)=       -372.1984310D0
      GSS   (17)=         15.0300000D0
      GSP   (17)=         13.1600000D0
      GPP   (17)=         11.3000000D0
      GP2   (17)=          9.9700000D0
      HSP   (17)=          2.4200000D0
      DD    (17)=          0.5406286D0
      QQ    (17)=          0.8057208D0
      AM    (17)=          0.5523705D0
      AD    (17)=          0.7693200D0
      AQ    (17)=          0.6133369D0
      GAUSS1(17,1)=          0.0942430D0
      GAUSS2(17,1)=          4.0000000D0
      GAUSS3(17,1)=          1.3000000D0
      GAUSS1(17,2)=          0.0271680D0
      GAUSS2(17,2)=          4.0000000D0
      GAUSS3(17,2)=          2.1000000D0
      USS   ( 30)=        -20.8397160D0
      UPP   ( 30)=        -19.6252240D0
      BETAS ( 30)=         -1.0000000D0
      BETAP ( 30)=         -2.0000000D0
      ZS    ( 30)=          2.0473590D0
      ZP    ( 30)=          1.4609460D0
      ZD    ( 30)=          1.0000000D0
      ALP   ( 30)=          1.5064570D0
      EISOL ( 30)=        -29.8794320D0
      GSS   ( 30)=         11.8000000D0
      GSP   ( 30)=         11.1820180D0
      GPP   ( 30)=         13.3000000D0
      GP2   ( 30)=         12.9305200D0
      HSP   ( 30)=          0.4846060D0
      DD    ( 30)=          1.3037826D0
      QQ    ( 30)=          1.4520183D0
      AM    ( 30)=          0.4336641D0
      AD    ( 30)=          0.2375912D0
      AQ    ( 30)=          0.2738858D0
      USS   (35)=       -104.6560630D0
      UPP   (35)=        -74.9300520D0
      BETAS (35)=        -19.3998800D0
      BETAP (35)=         -8.9571950D0
      ZS    (35)=          3.0641330D0
      ZP    (35)=          2.0383330D0
      ZD    (35)=          1.0000000D0
      ALP   (35)=          2.5765460D0
      EISOL (35)=       -352.3142087D0
      GSS   (35)=         15.0364395D0
      GSP   (35)=         13.0346824D0
      GPP   (35)=         11.2763254D0
      GP2   (35)=          9.8544255D0
      HSP   (35)=          2.4558683D0
      DD    (35)=          0.8458104D0
      QQ    (35)=          1.0407133D0
      AM    (35)=          0.5526071D0
      AD    (35)=          0.6024598D0
      AQ    (35)=          0.5307555D0
      GAUSS1(35,1)=          0.0666850D0
      GAUSS2(35,1)=          4.0000000D0
      GAUSS3(35,1)=          1.5000000D0
      GAUSS1(35,2)=          0.0255680D0
      GAUSS2(35,2)=          4.0000000D0
      GAUSS3(35,2)=          2.3000000D0
      USS   (53)=       -103.5896630D0
      UPP   (53)=        -74.4299970D0
      BETAS (53)=         -8.4433270D0
      BETAP (53)=         -6.3234050D0
      ZS    (53)=          2.1028580D0
      ZP    (53)=          2.1611530D0
      ZD    (53)=          1.0000000D0
      ALP   (53)=          2.2994240D0
      EISOL (53)=       -346.8642857D0
      GSS   (53)=         15.0404486D0
      GSP   (53)=         13.0565580D0
      GPP   (53)=         11.1477837D0
      GP2   (53)=          9.9140907D0
      HSP   (53)=          2.4563820D0
      DD    (53)=          1.4878778D0
      QQ    (53)=          1.1887388D0
      AM    (53)=          0.5527544D0
      AD    (53)=          0.4497523D0
      AQ    (53)=          0.4631775D0
      GAUSS1(53,1)=          0.0043610D0
      GAUSS2(53,1)=          2.3000000D0
      GAUSS3(53,1)=          1.8000000D0
      GAUSS1(53,2)=          0.0157060D0
      GAUSS2(53,2)=          3.0000000D0
      GAUSS3(53,2)=          2.2400000D0
      USS   (90)=        -40.5682920D0
      UPP   (90)=        -28.0891870D0
      BETAS (90)=         -4.2562180D0
      BETAP (90)=         -4.2562180D0
      ZS    (90)=          1.4353060D0
      ZP    (90)=          1.4353060D0
      ZD    (90)=          1.0000000D0
      ALP   (90)=          2.1961078D0
      EISOL (90)=        -90.5399580D0
      DD    (90)=          1.4078712D0
      QQ    (90)=          1.1658281D0
      AM    (90)=          0.3608967D0
      AD    (90)=          0.3441817D0
      AQ    (90)=          0.3999442D0
      HSP(90)=   1.32D00
      GP2(90)=   6.54D00
      GPP(90)=   7.31D00
      GSP(90)=   8.36D00
      GSS(90)=   9.82D00
      USS   (91)=        -75.2391520D0
      UPP   (91)=        -57.8320130D0
      BETAS (91)=        -11.1422310D0
      BETAP (91)=        -11.1422310D0
      ZS    (91)=          2.6135910D0
      ZP    (91)=          2.0343930D0
      ZD    (91)=          1.0000000D0
      ALP   (91)=          2.4916445D0
      EISOL (91)=       -235.4413560D0
      GSS   (91)=         12.8800000D0
      GSP   (91)=         11.2600000D0
      GPP   (91)=          9.9000000D0
      GP2   (91)=          8.8300000D0
      HSP   (91)=          2.2600000D0
      DD    (91)=          0.8231596D0
      QQ    (91)=          0.8225156D0
      AM    (91)=          0.4733554D0
      AD    (91)=          0.5889395D0
      AQ    (91)=          0.5632724D0
      USS   (102)=        -11.9062760D0
      BETAS (102)=   -9999999.0000000D0
      ZS    (102)=          4.0000000D0
      ZP    (102)=          0.3000000D0
      ZD    (102)=          0.3000000D0
      ALP   (102)=          2.5441341D0
      EISOL (102)=          4.0000000D0
      GSS   (102)=         12.8480000D0
      HSP   (102)=          0.1000000D0
      DD    (102)=          0.0684105D0
      QQ    (102)=          1.0540926D0
      AM    (102)=          0.4721793D0
      AD    (102)=          0.9262742D0
      AQ    (102)=          0.2909059D0
      USS  (108)=         -9.0000000D0
      UPP  (108)=         -4.0000000D0
      BETAS(108)=         -6.4741330D0
      BETAP(108)=          0.0000000D0
      ZS   (108)=          0.9005230D0
      ZP   (108)=          0.5919010D0
      ALP  (108)=          3.0268400D0
      EISOL(108)=         -9.0000000D0
      GSS  (108)=         12.2730000D0
      GSP  (108)=         15.0000000D0
      GPP  (108)=          8.0000000D0
      GP2  (108)=          9.0000000D0
      HSP  (108)=          0.0000000D0
      DD   (108)=          0.9955286D0
      QQ   (108)=          2.0691718D0
      AM   (108)=          0.4510292D0
      AD   (108)=          0.0015478D0
      AQ   (108)=          0.1782205D0
      GAUSS1(108,1)=          0.0000000D0
      GAUSS2(108,1)=          0.0000000D0
      GAUSS3(108,1)=          0.0000000D0
      GAUSS1(108,2)=          0.0000000D0
      GAUSS2(108,2)=          0.0000000D0
      GAUSS3(108,2)=          0.0000000D0
      GAUSS1(108,3)=          0.0000000D0
      GAUSS2(108,3)=          0.0000000D0
      GAUSS3(108,3)=          0.0000000D0
      GAUSS1(108,4)=          0.0000000D0
      GAUSS2(108,4)=          0.0000000D0
      GAUSS3(108,4)=          0.0000000D0



C     START OF RM1 PARAMETERS

      REFRM  ( 1)='  H: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, '//
     &                                              '1101-1111 (2006)'
      REFRM  ( 6)='  C: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, '//
     &                                              '1101-1111 (2006)'
      REFRM  ( 7)='  N: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, '//
     &                                              '1101-1111 (2006)'
      REFRM  ( 8)='  O: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, '//
     &                                              '1101-1111 (2006)'
      REFRM  ( 9)='  F: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, '//
     &                                              '1101-1111 (2006)'
      REFRM  (15)='  P: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, '//
     &                                              '1101-1111 (2006)'
      REFRM  (16)='  S: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, '//
     &                                              '1101-1111 (2006)'
      REFRM  (17)=' Cl: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, '//
     &                                              '1101-1111 (2006)'
      REFRM  (35)=' Br: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, '//
     &                                              '1101-1111 (2006)'
      REFRM  (53)='  I: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, '//
     &                                              '1101-1111 (2006)'
      REFRM (108)=' Hp: (RM1): Hydrogen (with p basis functions)'

      USSRM1(  1)=         -11.96067697
      BETASR(  1)=          -5.76544469
      ZSRM1(  1)=           1.08267366
      ALPRM1(  1)=           3.06835947
      EISOLR(  1)=         -11.96067697
      GSSRM1(  1)=          13.98321296
      AMRM1(  1)=           0.51387341
      ADRM1(  1)=           0.51387341
      AQRM1(  1)=           0.51387341
      GAUSR1(  1,1)=           0.10288875
      GAUSR2(  1,1)=           5.90172268
      GAUSR3(  1,1)=           1.17501185
      GAUSR1(  1,2)=           0.06457449
      GAUSR2(  1,2)=           6.41785671
      GAUSR3(  1,2)=           1.93844484
      GAUSR1(  1,3)=          -0.03567387
      GAUSR2(  1,3)=           2.80473127
      GAUSR3(  1,3)=           1.63655241
      USSRM1(  6)=         -51.72556032
      UPPRM1(  6)=         -39.40728943
      BETASR(  6)=         -15.45932428
      BETAPR(  6)=          -8.23608638
      ZSRM1(  6)=           1.85018803
      ZPRM1(  6)=           1.76830093
      ALPRM1(  6)=           2.79282078
      EISOLR(  6)=        -117.86734441
      GSSRM1(  6)=          13.05312440
      GSPRM1(  6)=          11.33479389
      GPPRM1(  6)=          10.95113739
      GP2RM1(  6)=           9.72395099
      HSPRM1(  6)=           1.55215133
      DDRM1(  6)=           0.79675711
      QQRM1(  6)=           0.69261111
      AMRM1(  6)=           0.47969330
      ADRM1(  6)=           0.50974061
      AQRM1(  6)=           0.66147546
      GAUSR1(  6,1)=           0.07462271
      GAUSR2(  6,1)=           5.73921605
      GAUSR3(  6,1)=           1.04396983
      GAUSR1(  6,2)=           0.01177053
      GAUSR2(  6,2)=           6.92401726
      GAUSR3(  6,2)=           1.66159571
      GAUSR1(  6,3)=           0.03720662
      GAUSR2(  6,3)=           6.26158944
      GAUSR3(  6,3)=           1.63158721
      GAUSR1(  6,4)=          -0.00270657
      GAUSR2(  6,4)=           9.00003735
      GAUSR3(  6,4)=           2.79557901
      USSRM1(  7)=         -70.85123715        
      UPPRM1(  7)=         -57.97730920
      BETASR(  7)=         -20.87124548
      BETAPR(  7)=         -16.67171853
      ZSRM1(  7)=           2.37447159
      ZPRM1(  7)=           1.97812569
      ALPRM1(  7)=           2.96422542
      EISOLR(  7)=        -205.08764187
      GSSRM1(  7)=          13.08736234
      GSPRM1(  7)=          13.21226834
      GPPRM1(  7)=          13.69924324
      GP2RM1(  7)=          11.94103953
      HSPRM1(  7)=           5.00000846
      DDRM1(  7)=           0.64956197
      QQRM1(  7)=           0.61914411
      AMRM1(  7)=           0.48095152
      ADRM1(  7)=           0.97060232
      AQRM1(  7)=           0.80235450
      GAUSR1(  7,1)=           0.06073380
      GAUSR2(  7,1)=           4.58892946
      GAUSR3(  7,1)=           1.37873881
      GAUSR1(  7,2)=           0.02438558
      GAUSR2(  7,2)=           4.62730519
      GAUSR3(  7,2)=           2.08370698
      GAUSR1(  7,3)=          -0.02283430
      GAUSR2(  7,3)=           2.05274659
      GAUSR3(  7,3)=           1.86763816
      USSRM1(  8)=         -96.94948069
      UPPRM1(  8)=         -77.89092978
      BETASR(  8)=         -29.85101212
      BETAPR(  8)=         -29.15101314
      ZSRM1(  8)=           3.17936914
      ZPRM1(  8)=           2.55361907
      ALPRM1(  8)=           4.17196717
      EISOLR(  8)=        -312.04035400
      GSSRM1(  8)=          14.00242788
      GSPRM1(  8)=          14.95625043
      GPPRM1(  8)=          14.14515138
      GP2RM1(  8)=          12.70325497
      HSPRM1(  8)=           3.93217161
      DDRM1(  8)=           0.48867006
      QQRM1(  8)=           0.47961142
      AMRM1(  8)=           0.51457955
      ADRM1(  8)=           1.00656044
      AQRM1(  8)=           0.89532625
      GAUSR1(  8,1)=           0.23093552
      GAUSR2(  8,1)=           5.21828736
      GAUSR3(  8,1)=           0.90363555
      GAUSR1(  8,2)=           0.05859873
      GAUSR2(  8,2)=           7.42932932
      GAUSR3(  8,2)=           1.51754610
      USSRM1(  9)=        -134.18369591
      UPPRM1(  9)=        -107.84660920
      BETASR(  9)=         -70.00000512
      BETAPR(  9)=         -32.67982711
      ZSRM1(  9)=           4.40337913
      ZPRM1(  9)=           2.64841556
      ALPRM1(  9)=           6.00000062
      EISOLR(  9)=        -484.59570233
      GSSRM1(  9)=          16.72091319
      GSPRM1(  9)=          16.76142629
      GPPRM1(  9)=          15.22581028
      GP2RM1(  9)=          14.86578679
      HSPRM1(  9)=           1.99766171
      DDRM1(  9)=           0.34889273
      QQRM1(  9)=           0.46244437
      AMRM1(  9)=           0.61448200
      ADRM1(  9)=           0.92253078
      AQRM1(  9)=           0.62364852
      GAUSR1(  9,1)=           0.40302025
      GAUSR2(  9,1)=           7.20441959
      GAUSR3(  9,1)=           0.81653013
      GAUSR1(  9,2)=           0.07085831
      GAUSR2(  9,2)=           9.00001562
      GAUSR3(  9,2)=           1.43802381
      USSRM1( 15)=         -41.81533184
      UPPRM1( 15)=         -34.38342529
      BETASR( 15)=          -6.13514969
      BETAPR( 15)=          -5.94442127
      ZSRM1( 15)=           2.12240118
      ZPRM1( 15)=           1.74327954
      ALPRM1( 15)=           1.90993294
      EISOLR( 15)=        -123.17977886
      GSSRM1( 15)=          11.08059265
      GSPRM1( 15)=           5.68339201
      GPPRM1( 15)=           7.60417563
      GP2RM1( 15)=           7.40265182
      HSPRM1( 15)=           1.16181792
      DDRM1( 15)=           1.01069548
      QQRM1( 15)=           0.95986904
      AMRM1( 15)=           0.40720412
      ADRM1( 15)=           0.39321392
      AQRM1( 15)=           0.31234367
      GAUSR1( 15,1)=          -0.41063467
      GAUSR2( 15,1)=           6.08752832
      GAUSR3( 15,1)=           1.31650261
      GAUSR1( 15,2)=          -0.16299288
      GAUSR2( 15,2)=           7.09472602
      GAUSR3( 15,2)=           1.90721319
      GAUSR1( 15,3)=          -0.04887125
      GAUSR2( 15,3)=           8.99979308
      GAUSR3( 15,3)=           2.65857780
      USSRM1( 16)=         -55.16775121
      UPPRM1( 16)=         -46.52930422
      BETASR( 16)=          -1.95910719
      BETAPR( 16)=          -8.77430652
      ZSRM1( 16)=           2.13344308
      ZPRM1( 16)=           1.87460650
      ALPRM1( 16)=           2.44015636
      EISOLR( 16)=        -185.38613819
      GSSRM1( 16)=          12.48828408
      GSPRM1( 16)=           8.56910574
      GPPRM1( 16)=           8.52301167
      GP2RM1( 16)=           7.66863296
      HSPRM1( 16)=           3.88978932
      DDRM1( 16)=           0.99369208
      QQRM1( 16)=           0.89262470
      AMRM1( 16)=           0.45893581
      ADRM1( 16)=           0.69307504
      AQRM1( 16)=           0.49593354
      GAUSR1( 16,1)=          -0.74601055
      GAUSR2( 16,1)=           4.81038002
      GAUSR3( 16,1)=           0.59380129
      GAUSR1( 16,2)=          -0.06519286
      GAUSR2( 16,2)=           7.20760864
      GAUSR3( 16,2)=           1.29492008
      GAUSR1( 16,3)=          -0.00655977
      GAUSR2( 16,3)=           9.00000180
      GAUSR3( 16,3)=           1.80060151
      USSRM1( 17)=        -118.47306918
      UPPRM1( 17)=         -76.35330340
      BETASR( 17)=         -19.92430432
      BETAPR( 17)=         -11.52935197
      ZSRM1( 17)=           3.86491071
      ZPRM1( 17)=           1.89593144
      ALPRM1( 17)=           3.69358828
      EISOLR( 17)=        -382.47009370
      GSSRM1( 17)=          15.36023105
      GSPRM1( 17)=          13.30671171
      GPPRM1( 17)=          12.56502640
      GP2RM1( 17)=           9.66397083
      HSPRM1( 17)=           1.76489897
      DDRM1( 17)=           0.45417885
      QQRM1( 17)=           0.88258469
      AMRM1( 17)=           0.56447787
      ADRM1( 17)=           0.74895844
      AQRM1( 17)=           0.77071593
      GAUSR1( 17,1)=           0.12947108
      GAUSR2( 17,1)=           2.97724424
      GAUSR3( 17,1)=           1.46749784
      GAUSR1( 17,2)=           0.00288899
      GAUSR2( 17,2)=           7.09827589
      GAUSR3( 17,2)=           2.50002723
      USSRM1( 35)=        -113.48398183
      UPPRM1( 35)=         -76.18720023
      BETASR( 35)=          -1.34139841
      BETAPR( 35)=          -8.20225991
      ZSRM1( 35)=           5.73157215
      ZPRM1( 35)=           2.03147582
      ALPRM1( 35)=           2.86710532
      EISOLR( 35)=        -357.11642720
      GSSRM1( 35)=          17.11563074
      GSPRM1( 35)=          15.62419251
      GPPRM1( 35)=          10.73546293
      GP2RM1( 35)=           8.86056199
      HSPRM1( 35)=           2.23512762
      DDRM1( 35)=           0.20990053
      QQRM1( 35)=           1.04422623
      AMRM1( 35)=           0.62898760
      ADRM1( 35)=           1.31654993
      AQRM1( 35)=           0.58633620
      GAUSR1( 35,1)=           0.98689937
      GAUSR2( 35,1)=           4.28484191
      GAUSR3( 35,1)=           2.00019696
      GAUSR1( 35,2)=          -0.92731247
      GAUSR2( 35,2)=           4.54005910
      GAUSR3( 35,2)=           2.01617695
      USSRM1( 53)=         -74.89997837
      UPPRM1( 53)=         -51.41023805
      BETASR( 53)=          -4.19316149
      BETAPR( 53)=          -4.40038412
      ZSRM1( 53)=           2.53003753
      ZPRM1( 53)=           2.31738678
      ALPRM1( 53)=           2.14157092
      EISOLR( 53)=        -248.49332414
      GSSRM1( 53)=          19.99974131
      GSPRM1( 53)=           7.68957672
      GPPRM1( 53)=           7.30488343
      GP2RM1( 53)=           6.85424614
      HSPRM1( 53)=           1.41602940
      DDRM1( 53)=           1.29634250
      QQRM1( 53)=           1.10859635
      AMRM1( 53)=           0.73497667
      ADRM1( 53)=           0.37173273
      AQRM1( 53)=           0.35126442
      GAUSR1( 53,1)=          -0.08147724
      GAUSR2( 53,1)=           1.56065072
      GAUSR3( 53,1)=           2.00002063
      GAUSR1( 53,2)=           0.05914991
      GAUSR2( 53,2)=           5.76111270
      GAUSR3( 53,2)=           2.20488800
      USSRM1(108)=         -7.3964270D0
      UPPRM1(108)=         -1.1486568D0
      BETASR(108)=         -9.6737870D0
      BETAPR(108)=         -1.3000000D0
      ZSRM1 (108)=          0.9880780D0
      ZPRM1 (108)=          0.3000000D0
      ALPRM1(108)=          2.9323240D0
      EISOLR(108)=         28.7118620D0
      GSSRM1(108)=         12.8480000D0
      GSPRM1(108)=          2.1600000D0
      GPPRM1(108)=          3.0100000D0
      GP2RM1(108)=          2.4400000D0
      HSPRM1(108)=          1.9200000D0
      DDRM1 (108)=          0.9675816D0
      QQRM1 (108)=          4.0824829D0
      AMRM1 (108)=          0.4721603D0
      ADRM1 (108)=          0.5007132D0
      AQRM1 (108)=          0.1603621D0
      GAUSR1(108,1)=          0.1227960D0
      GAUSR2(108,1)=          5.0000000D0
      GAUSR3(108,1)=          1.2000000D0
      GAUSR1(108,2)=          0.0050900D0
      GAUSR2(108,2)=          5.0000000D0
      GAUSR3(108,2)=          1.8000000D0
      GAUSR1(108,3)=         -0.0183360D0
      GAUSR2(108,3)=          2.0000000D0
      GAUSR3(108,3)=          2.1000000D0
      GAUSR1(108,4)=          0.0000000D0
      GAUSR2(108,4)=          0.0000000D0
      GAUSR3(108,4)=          0.0000000D0



C     START OF PM3 PARAMETERS

      REFPM3 ( 1) = '  H: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1989 10(2), 209.               '
      REFPM3 ( 4) = ' Be: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 ( 6) = '  C: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1989 10(2), 209.               '
      REFPM3 ( 7) = '  N: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1989 10(2), 209.               '
      REFPM3 ( 8) = '  O: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1989 10(2), 209.               '
      REFPM3 ( 9) = '  F: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1989 10(2), 209.               '
      REFPM3 (12) = ' Mg: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (13) = ' Al: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1989 10(2), 209.               '
      REFPM3 (14) = ' Si: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1989 10(2), 209.               '
      REFPM3 (15) = '  P: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1989 10(2), 209.               '
      REFPM3 (16) = '  S: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1989 10(2), 209.               '
      REFPM3 (17) = ' Cl: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1989 10(2), 209.               '
      REFPM3 (30) = ' Zn: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (31) = ' Ga: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (32) = ' Ge: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (33) = ' As: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (34) = ' Se: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (35) = ' Br: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1989 10(2), 209.               '
      REFPM3 (48) = ' Cd: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (49) = ' In: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (50) = ' Sn: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (51) = ' Sb: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (52) = ' Te: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (53) = '  I: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1989 10(2), 209.               '
      REFPM3 (80) = ' Hg: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (81) = ' Tl: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (82) = ' Pb: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (83) = ' Bi: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     & 1991 12(3), 320.               '
      REFPM3 (102) = ' Cb: (PM3):  Capped Bond  (Hydrogen-like, takes
     & on a  zero charge.)             '
      REFPM3(108) = ' Hp: (PM3): Hydrogen (with p basis functions)  
     &                               '

      USSPM3(  1)=        -13.0733210D0
      BETASP(  1)=         -5.6265120D0
      ZSPM3 (  1)=          0.9678070D0
      ALPPM3(  1)=          3.3563860D0
      EISOLP(  1)=        -13.0733210D0
      GSSPM3(  1)=         14.7942080D0
      AMPM3 (  1)=          0.5437048D0
      ADPM3 (  1)=          0.5437048D0
      AQPM3 (  1)=          0.5437048D0
      GAUSP1(  1,1)=          1.1287500D0
      GAUSP2(  1,1)=          5.0962820D0
      GAUSP3(  1,1)=          1.5374650D0
      GAUSP1(  1,2)=         -1.0603290D0
      GAUSP2(  1,2)=          6.0037880D0
      GAUSP3(  1,2)=          1.5701890D0
      USSPM3(  4)=        -17.2647520D0
      UPPPM3(  4)=        -11.3042430D0
      BETASP(  4)=         -3.9620530D0
      BETAPP(  4)=         -2.7806840D0
      ZSPM3 (  4)=          0.8774390D0
      ZPPM3 (  4)=          1.5087550D0
      ALPPM3(  4)=          1.5935360D0
      EISOLP(  4)=        -25.5166530D0
      GSSPM3(  4)=          9.0128510D0
      GSPPM3(  4)=          6.5761990D0
      GPPPM3(  4)=          6.0571820D0
      GP2PM3(  4)=          9.0052190D0
      HSPPM3(  4)=          0.5446790D0
      DDPM3 (  4)=          1.0090531D0
      QQPM3 (  4)=          0.8117586D0
      AMPM3 (  4)=          0.3312330D0
      ADPM3 (  4)=          0.2908996D0
      AQPM3 (  4)=          0.3530008D0
      GAUSP1(  4,1)=          1.6315720D0
      GAUSP2(  4,1)=          2.6729620D0
      GAUSP3(  4,1)=          1.7916860D0
      GAUSP1(  4,2)=         -2.1109590D0
      GAUSP2(  4,2)=          1.9685940D0
      GAUSP3(  4,2)=          1.7558710D0
      USSPM3(  6)=        -47.2703200D0
      UPPPM3(  6)=        -36.2669180D0
      BETASP(  6)=        -11.9100150D0
      BETAPP(  6)=         -9.8027550D0
      ZSPM3 (  6)=          1.5650850D0
      ZPPM3 (  6)=          1.8423450D0
      ALPPM3(  6)=          2.7078070D0
      EISOLP(  6)=       -111.2299170D0
      GSSPM3(  6)=         11.2007080D0
      GSPPM3(  6)=         10.2650270D0
      GPPPM3(  6)=         10.7962920D0
      GP2PM3(  6)=          9.0425660D0
      HSPPM3(  6)=          2.2909800D0
      DDPM3 (  6)=          0.8332396D0
      QQPM3 (  6)=          0.6647750D0
      AMPM3 (  6)=          0.4116394D0
      ADPM3 (  6)=          0.5885862D0
      AQPM3 (  6)=          0.7647667D0
      GAUSP1(  6,1)=          0.0501070D0
      GAUSP2(  6,1)=          6.0031650D0
      GAUSP3(  6,1)=          1.6422140D0
      GAUSP1(  6,2)=          0.0507330D0
      GAUSP2(  6,2)=          6.0029790D0
      GAUSP3(  6,2)=          0.8924880D0
      USSPM3(  7)=        -49.3356720D0
      UPPPM3(  7)=        -47.5097360D0
      BETASP(  7)=        -14.0625210D0
      BETAPP(  7)=        -20.0438480D0
      ZSPM3 (  7)=          2.0280940D0
      ZPPM3 (  7)=          2.3137280D0
      ALPPM3(  7)=          2.8305450D0
      EISOLP(  7)=       -157.6137755D0
      GSSPM3(  7)=         11.9047870D0
      GSPPM3(  7)=          7.3485650D0
      GPPPM3(  7)=         11.7546720D0
      GP2PM3(  7)=         10.8072770D0
      HSPPM3(  7)=          1.1367130D0
      DDPM3 (  7)=          0.6577006D0
      QQPM3 (  7)=          0.5293383D0
      AMPM3 (  7)=          0.4375151D0
      ADPM3 (  7)=          0.5030995D0
      AQPM3 (  7)=          0.7364933D0
      GAUSP1(  7,1)=          1.5016740D0
      GAUSP2(  7,1)=          5.9011480D0
      GAUSP3(  7,1)=          1.7107400D0
      GAUSP1(  7,2)=         -1.5057720D0
      GAUSP2(  7,2)=          6.0046580D0
      GAUSP3(  7,2)=          1.7161490D0
      USSPM3(  8)=        -86.9930020D0
      UPPPM3(  8)=        -71.8795800D0
      BETASP(  8)=        -45.2026510D0
      BETAPP(  8)=        -24.7525150D0
      ZSPM3 (  8)=          3.7965440D0
      ZPPM3 (  8)=          2.3894020D0
      ALPPM3(  8)=          3.2171020D0
      EISOLP(  8)=       -289.3422065D0
      GSSPM3(  8)=         15.7557600D0
      GSPPM3(  8)=         10.6211600D0
      GPPPM3(  8)=         13.6540160D0
      GP2PM3(  8)=         12.4060950D0
      HSPPM3(  8)=          0.5938830D0
      DDPM3 (  8)=          0.4086173D0
      QQPM3 (  8)=          0.5125738D0
      AMPM3 (  8)=          0.5790430D0
      ADPM3 (  8)=          0.5299630D0
      AQPM3 (  8)=          0.8179630D0
      GAUSP1(  8,1)=         -1.1311280D0
      GAUSP2(  8,1)=          6.0024770D0
      GAUSP3(  8,1)=          1.6073110D0
      GAUSP1(  8,2)=          1.1378910D0
      GAUSP2(  8,2)=          5.9505120D0
      GAUSP3(  8,2)=          1.5983950D0
      USSPM3(  9)=       -110.4353030D0
      UPPPM3(  9)=       -105.6850470D0
      BETASP(  9)=        -48.4059390D0
      BETAPP(  9)=        -27.7446600D0
      ZSPM3 (  9)=          4.7085550D0
      ZPPM3 (  9)=          2.4911780D0
      ALPPM3(  9)=          3.3589210D0
      EISOLP(  9)=       -437.5171690D0
      GSSPM3(  9)=         10.4966670D0
      GSPPM3(  9)=         16.0736890D0
      GPPPM3(  9)=         14.8172560D0
      GP2PM3(  9)=         14.4183930D0
      HSPPM3(  9)=          0.7277630D0
      DDPM3 (  9)=          0.3125302D0
      QQPM3 (  9)=          0.4916328D0
      AMPM3 (  9)=          0.3857650D0
      ADPM3 (  9)=          0.6768503D0
      AQPM3 (  9)=          0.6120047D0
      GAUSP1(  9,1)=         -0.0121660D0
      GAUSP2(  9,1)=          6.0235740D0
      GAUSP3(  9,1)=          1.8568590D0
      GAUSP1(  9,2)=         -0.0028520D0
      GAUSP2(  9,2)=          6.0037170D0
      GAUSP3(  9,2)=          2.6361580D0
      USSPM3( 12)=        -14.6236880D0
      UPPPM3( 12)=        -14.1734600D0
      BETASP( 12)=         -2.0716910D0
      BETAPP( 12)=         -0.5695810D0
      ZSPM3 ( 12)=          0.6985520D0
      ZPPM3 ( 12)=          1.4834530D0
      ALPPM3( 12)=          1.3291470D0
      EISOLP( 12)=        -22.5530760D0
      GSSPM3( 12)=          6.6943000D0
      GSPPM3( 12)=          6.7939950D0
      GPPPM3( 12)=          6.9104460D0
      GP2PM3( 12)=          7.0908230D0
      HSPPM3( 12)=          0.5433000D0
      DDPM3 ( 12)=          1.1403950D0
      QQPM3 ( 12)=          1.1279899D0
      AMPM3 ( 12)=          0.2460235D0
      ADPM3 ( 12)=          0.2695751D0
      AQPM3 ( 12)=          0.2767522D0
      GAUSP1( 12,1)=          2.1170500D0
      GAUSP2( 12,1)=          6.0094770D0
      GAUSP3( 12,1)=          2.0844060D0
      GAUSP1( 12,2)=         -2.5477670D0
      GAUSP2( 12,2)=          4.3953700D0
      GAUSP3( 12,2)=          2.0636740D0
      USSPM3( 13)=        -24.8454040D0
      UPPPM3( 13)=        -22.2641590D0
      BETASP( 13)=         -0.5943010D0
      BETAPP( 13)=         -0.9565500D0
      ZSPM3 ( 13)=          1.7028880D0
      ZPPM3 ( 13)=          1.0736290D0
      ZDPM3 ( 13)=          1.0000000D0
      ALPPM3( 13)=          1.5217030D0
      EISOLP( 13)=        -46.8647630D0
      GSSPM3( 13)=          5.7767370D0
      GSPPM3( 13)=         11.6598560D0
      GPPPM3( 13)=          6.3477900D0
      GP2PM3( 13)=          6.1210770D0
      HSPPM3( 13)=          4.0062450D0
      DDPM3 ( 13)=          1.2102799D0
      QQPM3 ( 13)=          1.5585645D0
      AMPM3 ( 13)=          0.2123020D0
      ADPM3 ( 13)=          0.6418584D0
      AQPM3 ( 13)=          0.2262838D0
      GAUSP1( 13,1)=         -0.4730900D0
      GAUSP2( 13,1)=          1.9158250D0
      GAUSP3( 13,1)=          1.4517280D0
      GAUSP1( 13,2)=         -0.1540510D0
      GAUSP2( 13,2)=          6.0050860D0
      GAUSP3( 13,2)=          2.5199970D0
      USSPM3( 14)=        -26.7634830D0
      UPPPM3( 14)=        -22.8136350D0
      BETASP( 14)=         -2.8621450D0
      BETAPP( 14)=         -3.9331480D0
      ZSPM3 ( 14)=          1.6350750D0
      ZPPM3 ( 14)=          1.3130880D0
      ZDPM3 ( 14)=          1.0000000D0
      ALPPM3( 14)=          2.1358090D0
      EISOLP( 14)=        -67.7882140D0
      GSSPM3( 14)=          5.0471960D0
      GSPPM3( 14)=          5.9490570D0
      GPPPM3( 14)=          6.7593670D0
      GP2PM3( 14)=          5.1612970D0
      HSPPM3( 14)=          0.9198320D0
      DDPM3 ( 14)=          1.3144550D0
      QQPM3 ( 14)=          1.2743396D0
      AMPM3 ( 14)=          0.1854905D0
      ADPM3 ( 14)=          0.3060715D0
      AQPM3 ( 14)=          0.4877432D0
      GAUSP1( 14,1)=         -0.3906000D0
      GAUSP2( 14,1)=          6.0000540D0
      GAUSP3( 14,1)=          0.6322620D0
      GAUSP1( 14,2)=          0.0572590D0
      GAUSP2( 14,2)=          6.0071830D0
      GAUSP3( 14,2)=          2.0199870D0
      GAUSP1( 14,3)=          0.0207890D0
      GAUSP2( 14,3)=          5.0000000D0
      GAUSP3( 14,3)=          2.9906100D0
      USSPM3( 15)=        -40.4130960D0
      UPPPM3( 15)=        -29.5930520D0
      BETASP( 15)=        -12.6158790D0
      BETAPP( 15)=         -4.1600400D0
      ZSPM3 ( 15)=          2.0175630D0
      ZPPM3 ( 15)=          1.5047320D0
      ZDPM3 ( 15)=          1.0000000D0
      ALPPM3( 15)=          1.9405340D0
      EISOLP( 15)=       -117.9591740D0
      GSSPM3( 15)=          7.8016150D0
      GSPPM3( 15)=          5.1869490D0
      GPPPM3( 15)=          6.6184780D0
      GP2PM3( 15)=          6.0620020D0
      HSPPM3( 15)=          1.5428090D0
      DDPM3 ( 15)=          1.0644947D0
      QQPM3 ( 15)=          1.1120386D0
      AMPM3 ( 15)=          0.2867187D0
      ADPM3 ( 15)=          0.4309446D0
      AQPM3 ( 15)=          0.3732517D0
      GAUSP1( 15,1)=         -0.6114210D0
      GAUSP2( 15,1)=          1.9972720D0
      GAUSP3( 15,1)=          0.7946240D0
      GAUSP1( 15,2)=         -0.0939350D0
      GAUSP2( 15,2)=          1.9983600D0
      GAUSP3( 15,2)=          1.9106770D0
      USSPM3( 16)=        -49.8953710D0
      UPPPM3( 16)=        -44.3925830D0
      BETASP( 16)=         -8.8274650D0
      BETAPP( 16)=         -8.0914150D0
      ZSPM3 ( 16)=          1.8911850D0
      ZPPM3 ( 16)=          1.6589720D0
      ZDPM3 ( 16)=          1.0000000D0
      ALPPM3( 16)=          2.2697060D0
      EISOLP( 16)=       -183.4537395D0
      GSSPM3( 16)=          8.9646670D0
      GSPPM3( 16)=          6.7859360D0
      GPPPM3( 16)=          9.9681640D0
      GP2PM3( 16)=          7.9702470D0
      HSPPM3( 16)=          4.0418360D0
      DDPM3 ( 16)=          1.1214313D0
      QQPM3 ( 16)=          1.0086488D0
      AMPM3 ( 16)=          0.3294622D0
      ADPM3 ( 16)=          0.6679118D0
      AQPM3 ( 16)=          0.6137472D0
      GAUSP1( 16,1)=         -0.3991910D0
      GAUSP2( 16,1)=          6.0006690D0
      GAUSP3( 16,1)=          0.9621230D0
      GAUSP1( 16,2)=         -0.0548990D0
      GAUSP2( 16,2)=          6.0018450D0
      GAUSP3( 16,2)=          1.5799440D0
      USSPM3( 17)=       -100.6267470D0
      UPPPM3( 17)=        -53.6143960D0
      BETASP( 17)=        -27.5285600D0
      BETAPP( 17)=        -11.5939220D0
      ZSPM3 ( 17)=          2.2462100D0
      ZPPM3 ( 17)=          2.1510100D0
      ZDPM3 ( 17)=          1.0000000D0
      ALPPM3( 17)=          2.5172960D0
      EISOLP( 17)=       -315.1949480D0
      GSSPM3( 17)=         16.0136010D0
      GSPPM3( 17)=          8.0481150D0
      GPPPM3( 17)=          7.5222150D0
      GP2PM3( 17)=          7.5041540D0
      HSPPM3( 17)=          3.4811530D0
      DDPM3 ( 17)=          0.9175856D0
      QQPM3 ( 17)=          0.7779230D0
      AMPM3 ( 17)=          0.5885190D0
      ADPM3 ( 17)=          0.6814522D0
      AQPM3 ( 17)=          0.3643694D0
      GAUSP1( 17,1)=         -0.1715910D0
      GAUSP2( 17,1)=          6.0008020D0
      GAUSP3( 17,1)=          1.0875020D0
      GAUSP1( 17,2)=         -0.0134580D0
      GAUSP2( 17,2)=          1.9666180D0
      GAUSP3( 17,2)=          2.2928910D0
      USSPM3( 30)=        -18.5321980D0
      UPPPM3( 30)=        -11.0474090D0
      BETASP( 30)=         -0.7155780D0
      BETAPP( 30)=         -6.3518640D0
      ZSPM3 ( 30)=          1.8199890D0
      ZPPM3 ( 30)=          1.5069220D0
      ZDPM3 ( 30)=          1.0000000D0
      ALPPM3( 30)=          1.3501260D0
      EISOLP( 30)=        -27.3872000D0
      GSSPM3( 30)=          9.6771960D0
      GSPPM3( 30)=          7.7362040D0
      GPPPM3( 30)=          4.9801740D0
      GP2PM3( 30)=          4.6696560D0
      HSPPM3( 30)=          0.6004130D0
      DDPM3 ( 30)=          1.5005758D0
      QQPM3 ( 30)=          1.4077174D0
      AMPM3 ( 30)=          0.3556485D0
      ADPM3 ( 30)=          0.2375689D0
      AQPM3 ( 30)=          0.2661069D0
      GAUSP1( 30,1)=         -0.1112340D0
      GAUSP2( 30,1)=          6.0014780D0
      GAUSP3( 30,1)=          1.5160320D0
      GAUSP1( 30,2)=         -0.1323700D0
      GAUSP2( 30,2)=          1.9958390D0
      GAUSP3( 30,2)=          2.5196420D0
      USSPM3( 31)=        -29.8555930D0
      UPPPM3( 31)=        -21.8753710D0
      BETASP( 31)=         -4.9456180D0
      BETAPP( 31)=         -0.4070530D0
      ZSPM3 ( 31)=          1.8470400D0
      ZPPM3 ( 31)=          0.8394110D0
      ALPPM3( 31)=          1.6051150D0
      EISOLP( 31)=        -57.3280250D0
      GSSPM3( 31)=          8.4585540D0
      GSPPM3( 31)=          8.9256190D0
      GPPPM3( 31)=          5.0868550D0
      GP2PM3( 31)=          4.9830450D0
      HSPPM3( 31)=          2.0512600D0
      DDPM3 ( 31)=          0.9776692D0
      QQPM3 ( 31)=          2.5271534D0
      AMPM3 ( 31)=          0.3108620D0
      ADPM3 ( 31)=          0.5129360D0
      AQPM3 ( 31)=          0.1546208D0
      GAUSP1( 31,1)=         -0.5601790D0
      GAUSP2( 31,1)=          5.6232730D0
      GAUSP3( 31,1)=          1.5317800D0
      GAUSP1( 31,2)=         -0.2727310D0
      GAUSP2( 31,2)=          1.9918430D0
      GAUSP3( 31,2)=          2.1838640D0
      USSPM3( 32)=        -35.4671955D0
      UPPPM3( 32)=        -31.5863583D0
      BETASP( 32)=         -5.3250024D0
      BETAPP( 32)=         -2.2501567D0
      ZSPM3 ( 32)=          2.2373526D0
      ZPPM3 ( 32)=          1.5924319D0
      ALPPM3( 32)=          1.9723370D0
      EISOLP( 32)=        -84.0156006D0
      GSSPM3( 32)=          5.3769635D0
      GSPPM3( 32)=         10.2095293D0
      GPPPM3( 32)=          7.6718647D0
      GP2PM3( 32)=          6.9242663D0
      HSPPM3( 32)=          1.3370204D0
      DDPM3 ( 32)=          1.1920304D0
      QQPM3 ( 32)=          1.3321263D0
      AMPM3 ( 32)=          0.1976098D0
      ADPM3 ( 32)=          0.3798182D0
      AQPM3 ( 32)=          0.3620669D0
      GAUSP1( 32,1)=          0.9631726D0
      GAUSP2( 32,1)=          6.0120134D0
      GAUSP3( 32,1)=          2.1633655D0
      GAUSP1( 32,2)=         -0.9593891D0
      GAUSP2( 32,2)=          5.7491802D0
      GAUSP3( 32,2)=          2.1693724D0
      USSPM3( 33)=        -38.5074240D0
      UPPPM3( 33)=        -35.1524150D0
      BETASP( 33)=         -8.2321650D0
      BETAPP( 33)=         -5.0173860D0
      ZSPM3 ( 33)=          2.6361770D0
      ZPPM3 ( 33)=          1.7038890D0
      ALPPM3( 33)=          1.7944770D0
      EISOLP( 33)=       -122.6326140D0
      GSSPM3( 33)=          8.7890010D0
      GSPPM3( 33)=          5.3979830D0
      GPPPM3( 33)=          8.2872500D0
      GP2PM3( 33)=          8.2103460D0
      HSPPM3( 33)=          1.9510340D0
      DDPM3 ( 33)=          0.9679655D0
      QQPM3 ( 33)=          1.2449874D0
      AMPM3 ( 33)=          0.3230063D0
      ADPM3 ( 33)=          0.5042239D0
      AQPM3 ( 33)=          0.2574219D0
      GAUSP1( 33,1)=         -0.4600950D0
      GAUSP2( 33,1)=          1.9831150D0
      GAUSP3( 33,1)=          1.0867930D0
      GAUSP1( 33,2)=         -0.0889960D0
      GAUSP2( 33,2)=          1.9929440D0
      GAUSP3( 33,2)=          2.1400580D0
      USSPM3( 34)=        -55.3781350D0
      UPPPM3( 34)=        -49.8230760D0
      BETASP( 34)=         -6.1578220D0
      BETAPP( 34)=         -5.4930390D0
      ZSPM3 ( 34)=          2.8280510D0
      ZPPM3 ( 34)=          1.7325360D0
      ALPPM3( 34)=          3.0439570D0
      EISOLP( 34)=       -192.7748115D0
      GSSPM3( 34)=          7.4325910D0
      GSPPM3( 34)=         10.0604610D0
      GPPPM3( 34)=          9.5683260D0
      GP2PM3( 34)=          7.7242890D0
      HSPPM3( 34)=          4.0165580D0
      DDPM3 ( 34)=          0.8719813D0
      QQPM3 ( 34)=          1.2244019D0
      AMPM3 ( 34)=          0.2731566D0
      ADPM3 ( 34)=          0.7509697D0
      AQPM3 ( 34)=          0.5283737D0
      GAUSP1( 34,1)=          0.0478730D0
      GAUSP2( 34,1)=          6.0074000D0
      GAUSP3( 34,1)=          2.0817170D0
      GAUSP1( 34,2)=          0.1147200D0
      GAUSP2( 34,2)=          6.0086720D0
      GAUSP3( 34,2)=          1.5164230D0
      USSPM3( 35)=       -116.6193110D0
      UPPPM3( 35)=        -74.2271290D0
      BETASP( 35)=        -31.1713420D0
      BETAPP( 35)=         -6.8140130D0
      ZSPM3 ( 35)=          5.3484570D0
      ZPPM3 ( 35)=          2.1275900D0
      ZDPM3 ( 35)=          1.0000000D0
      ALPPM3( 35)=          2.5118420D0
      EISOLP( 35)=       -352.5398970D0
      GSSPM3( 35)=         15.9434250D0
      GSPPM3( 35)=         16.0616800D0
      GPPPM3( 35)=          8.2827630D0
      GP2PM3( 35)=          7.8168490D0
      HSPPM3( 35)=          0.5788690D0
      DDPM3 ( 35)=          0.2759025D0
      QQPM3 ( 35)=          0.9970532D0
      AMPM3 ( 35)=          0.5859399D0
      ADPM3 ( 35)=          0.6755383D0
      AQPM3 ( 35)=          0.3823719D0
      GAUSP1( 35,1)=          0.9604580D0
      GAUSP2( 35,1)=          5.9765080D0
      GAUSP3( 35,1)=          2.3216540D0
      GAUSP1( 35,2)=         -0.9549160D0
      GAUSP2( 35,2)=          5.9447030D0
      GAUSP3( 35,2)=          2.3281420D0
      USSPM3( 48)=        -15.8285840D0
      UPPPM3( 48)=          8.7497950D0
      BETASP( 48)=         -8.5819440D0
      BETAPP( 48)=         -0.6010340D0
      ZSPM3 ( 48)=          1.6793510D0
      ZPPM3 ( 48)=          2.0664120D0
      ALPPM3( 48)=          1.5253820D0
      EISOLP( 48)=        -22.4502080D0
      GSSPM3( 48)=          9.2069600D0
      GSPPM3( 48)=          8.2315390D0
      GPPPM3( 48)=          4.9481040D0
      GP2PM3( 48)=          4.6696560D0
      HSPPM3( 48)=          1.6562340D0
      DDPM3 ( 48)=          1.5982681D0
      QQPM3 ( 48)=          1.2432402D0
      AMPM3 ( 48)=          0.3383668D0
      ADPM3 ( 48)=          0.3570290D0
      AQPM3 ( 48)=          0.2820582D0
      USSPM3( 49)=        -26.1762050D0
      UPPPM3( 49)=        -20.0058220D0
      BETASP( 49)=         -2.9933190D0
      BETAPP( 49)=         -1.8289080D0
      ZSPM3 ( 49)=          2.0161160D0
      ZPPM3 ( 49)=          1.4453500D0
      ALPPM3( 49)=          1.4183850D0
      EISOLP( 49)=        -51.9750470D0
      GSSPM3( 49)=          6.5549000D0
      GSPPM3( 49)=          8.2298730D0
      GPPPM3( 49)=          6.2992690D0
      GP2PM3( 49)=          4.9842110D0
      HSPPM3( 49)=          2.6314610D0
      DDPM3 ( 49)=          1.5766241D0
      QQPM3 ( 49)=          1.7774563D0
      AMPM3 ( 49)=          0.2409004D0
      ADPM3 ( 49)=          0.4532655D0
      AQPM3 ( 49)=          0.3689812D0
      GAUSP1( 49,1)=         -0.3431380D0
      GAUSP2( 49,1)=          1.9940340D0
      GAUSP3( 49,1)=          1.6255160D0
      GAUSP1( 49,2)=         -0.1095320D0
      GAUSP2( 49,2)=          5.6832170D0
      GAUSP3( 49,2)=          2.8670090D0
      USSPM3( 50)=        -34.5501920D0
      UPPPM3( 50)=        -25.8944190D0
      BETASP( 50)=         -2.7858020D0
      BETAPP( 50)=         -2.0059990D0
      ZSPM3 ( 50)=          2.3733280D0
      ZPPM3 ( 50)=          1.6382330D0
      ALPPM3( 50)=          1.6996500D0
      EISOLP( 50)=        -78.8877790D0
      GSSPM3( 50)=         10.1900330D0
      GSPPM3( 50)=          7.2353270D0
      GPPPM3( 50)=          5.6738100D0
      GP2PM3( 50)=          5.1822140D0
      HSPPM3( 50)=          1.0331570D0
      DDPM3 ( 50)=          1.3120038D0
      QQPM3 ( 50)=          1.5681814D0
      AMPM3 ( 50)=          0.3744959D0
      ADPM3 ( 50)=          0.3218163D0
      AQPM3 ( 50)=          0.2832529D0
      GAUSP1( 50,1)=         -0.1503530D0
      GAUSP2( 50,1)=          6.0056940D0
      GAUSP3( 50,1)=          1.7046420D0
      GAUSP1( 50,2)=         -0.0444170D0
      GAUSP2( 50,2)=          2.2573810D0
      GAUSP3( 50,2)=          2.4698690D0
      USSPM3( 51)=        -56.4321960D0
      UPPPM3( 51)=        -29.4349540D0
      BETASP( 51)=        -14.7942170D0
      BETAPP( 51)=         -2.8179480D0
      ZSPM3 ( 51)=          2.3430390D0
      ZPPM3 ( 51)=          1.8999920D0
      ALPPM3( 51)=          2.0343010D0
      EISOLP( 51)=       -148.9382890D0
      GSSPM3( 51)=          9.2382770D0
      GSPPM3( 51)=          5.2776800D0
      GPPPM3( 51)=          6.3500000D0
      GP2PM3( 51)=          6.2500000D0
      HSPPM3( 51)=          2.4244640D0
      DDPM3 ( 51)=          1.4091903D0
      QQPM3 ( 51)=          1.3521354D0
      AMPM3 ( 51)=          0.3395177D0
      ADPM3 ( 51)=          0.4589010D0
      AQPM3 ( 51)=          0.2423472D0
      GAUSP1( 51,1)=          3.0020280D0
      GAUSP2( 51,1)=          6.0053420D0
      GAUSP3( 51,1)=          0.8530600D0
      GAUSP1( 51,2)=         -0.0188920D0
      GAUSP2( 51,2)=          6.0114780D0
      GAUSP3( 51,2)=          2.7933110D0
      USSPM3( 52)=        -44.9380360D0
      UPPPM3( 52)=        -46.3140990D0
      BETASP( 52)=         -2.6651460D0
      BETAPP( 52)=         -3.8954300D0
      ZSPM3 ( 52)=          4.1654920D0
      ZPPM3 ( 52)=          1.6475550D0
      ALPPM3( 52)=          2.4850190D0
      EISOLP( 52)=       -168.0945925D0
      GSSPM3( 52)=         10.2550730D0
      GSPPM3( 52)=          8.1691450D0
      GPPPM3( 52)=          7.7775920D0
      GP2PM3( 52)=          7.7551210D0
      HSPPM3( 52)=          3.7724620D0
      DDPM3 ( 52)=          0.3484177D0
      QQPM3 ( 52)=          1.5593085D0
      AMPM3 ( 52)=          0.3768862D0
      ADPM3 ( 52)=          1.1960743D0
      AQPM3 ( 52)=          0.2184786D0
      GAUSP1( 52,1)=          0.0333910D0
      GAUSP2( 52,1)=          5.9563790D0
      GAUSP3( 52,1)=          2.2775750D0
      GAUSP1( 52,2)=         -1.9218670D0
      GAUSP2( 52,2)=          4.9732190D0
      GAUSP3( 52,2)=          0.5242430D0
      USSPM3( 53)=        -96.4540370D0
      UPPPM3( 53)=        -61.0915820D0
      BETASP( 53)=        -14.4942340D0
      BETAPP( 53)=         -5.8947030D0
      ZSPM3 ( 53)=          7.0010130D0
      ZPPM3 ( 53)=          2.4543540D0
      ZDPM3 ( 53)=          1.0000000D0
      ALPPM3( 53)=          1.9901850D0
      EISOLP( 53)=       -288.3160860D0
      GSSPM3( 53)=         13.6319430D0
      GSPPM3( 53)=         14.9904060D0
      GPPPM3( 53)=          7.2883300D0
      GP2PM3( 53)=          5.9664070D0
      HSPPM3( 53)=          2.6300350D0
      DDPM3 ( 53)=          0.1581469D0
      QQPM3 ( 53)=          1.0467302D0
      AMPM3 ( 53)=          0.5009902D0
      ADPM3 ( 53)=          1.6699104D0
      AQPM3 ( 53)=          0.5153082D0
      GAUSP1( 53,1)=         -0.1314810D0
      GAUSP2( 53,1)=          5.2064170D0
      GAUSP3( 53,1)=          1.7488240D0
      GAUSP1( 53,2)=         -0.0368970D0
      GAUSP2( 53,2)=          6.0101170D0
      GAUSP3( 53,2)=          2.7103730D0
      USSPM3( 80)=        -17.7622290D0
      UPPPM3( 80)=        -18.3307510D0
      BETASP( 80)=         -3.1013650D0
      BETAPP( 80)=         -3.4640310D0
      ZSPM3 ( 80)=          1.4768850D0
      ZPPM3 ( 80)=          2.4799510D0
      ALPPM3( 80)=          1.5293770D0
      EISOLP( 80)=        -28.8997380D0
      GSSPM3( 80)=          6.6247200D0
      GSPPM3( 80)=         10.6392970D0
      GPPPM3( 80)=         14.7092830D0
      GP2PM3( 80)=         16.0007400D0
      HSPPM3( 80)=          2.0363110D0
      DDPM3 ( 80)=          1.2317811D0
      QQPM3 ( 80)=          1.2164033D0
      AMPM3 ( 80)=          0.2434664D0
      ADPM3 ( 80)=          0.4515472D0
      AQPM3 ( 80)=          0.2618394D0
      GAUSP1( 80,1)=          1.0827200D0
      GAUSP2( 80,1)=          6.4965980D0
      GAUSP3( 80,1)=          1.1951460D0
      GAUSP1( 80,2)=         -0.0965530D0
      GAUSP2( 80,2)=          3.9262810D0
      GAUSP3( 80,2)=          2.6271600D0
      USSPM3( 81)=        -30.0531700D0
      UPPPM3( 81)=        -26.9206370D0
      BETASP( 81)=         -1.0844950D0
      BETAPP( 81)=         -7.9467990D0
      ZSPM3 ( 81)=          6.8679210D0
      ZPPM3 ( 81)=          1.9694450D0
      ALPPM3( 81)=          1.3409510D0
      EISOLP( 81)=        -56.6492050D0
      GSSPM3( 81)=         10.4604120D0
      GSPPM3( 81)=         11.2238830D0
      GPPPM3( 81)=          4.9927850D0
      GP2PM3( 81)=          8.9627270D0
      HSPPM3( 81)=          2.5304060D0
      DDPM3 ( 81)=          0.0781362D0
      QQPM3 ( 81)=          1.5317110D0
      AMPM3 ( 81)=          0.3844326D0
      ADPM3 ( 81)=          2.5741815D0
      AQPM3 ( 81)=          0.2213264D0
      GAUSP1( 81,1)=         -1.3613990D0
      GAUSP2( 81,1)=          3.5572260D0
      GAUSP3( 81,1)=          1.0928020D0
      GAUSP1( 81,2)=         -0.0454010D0
      GAUSP2( 81,2)=          2.3069950D0
      GAUSP3( 81,2)=          2.9650290D0
      USSPM3( 82)=        -30.3227560D0
      UPPPM3( 82)=        -24.4258340D0
      BETASP( 82)=         -6.1260240D0
      BETAPP( 82)=         -1.3954300D0
      ZSPM3 ( 82)=          3.1412890D0
      ZPPM3 ( 82)=          1.8924180D0
      ALPPM3( 82)=          1.6200450D0
      EISOLP( 82)=        -73.4660775D0
      GSSPM3( 82)=          7.0119920D0
      GSPPM3( 82)=          6.7937820D0
      GPPPM3( 82)=          5.1837800D0
      GP2PM3( 82)=          5.0456510D0
      HSPPM3( 82)=          1.5663020D0
      DDPM3 ( 82)=          0.9866290D0
      QQPM3 ( 82)=          1.5940562D0
      AMPM3 ( 82)=          0.2576991D0
      ADPM3 ( 82)=          0.4527678D0
      AQPM3 ( 82)=          0.2150175D0
      GAUSP1( 82,1)=         -0.1225760D0
      GAUSP2( 82,1)=          6.0030620D0
      GAUSP3( 82,1)=          1.9015970D0
      GAUSP1( 82,2)=         -0.0566480D0
      GAUSP2( 82,2)=          4.7437050D0
      GAUSP3( 82,2)=          2.8618790D0
      USSPM3( 83)=        -33.4959380D0
      UPPPM3( 83)=        -35.5210260D0
      BETASP( 83)=         -5.6072830D0
      BETAPP( 83)=         -5.8001520D0
      ZSPM3 ( 83)=          4.9164510D0
      ZPPM3 ( 83)=          1.9349350D0
      ALPPM3( 83)=          1.8574310D0
      EISOLP( 83)=       -109.2774910D0
      GSSPM3( 83)=          4.9894800D0
      GSPPM3( 83)=          6.1033080D0
      GPPPM3( 83)=          8.6960070D0
      GP2PM3( 83)=          8.3354470D0
      HSPPM3( 83)=          0.5991220D0
      DDPM3 ( 83)=          0.2798609D0
      QQPM3 ( 83)=          1.5590294D0
      AMPM3 ( 83)=          0.1833693D0
      ADPM3 ( 83)=          0.6776013D0
      AQPM3 ( 83)=          0.2586520D0
      GAUSP1( 83,1)=          2.5816930D0
      GAUSP2( 83,1)=          5.0940220D0
      GAUSP3( 83,1)=          0.4997870D0
      GAUSP1( 83,2)=          0.0603200D0
      GAUSP2( 83,2)=          6.0015380D0
      GAUSP3( 83,2)=          2.4279700D0
      USSPM3(102)=        -11.9062760D0
      BETASP(102)=   -9999999.0000000D0
      ZSPM3 (102)=          4.0000000D0
      ZPPM3 (102)=          0.3000000D0
      ZDPM3 (102)=          0.3000000D0
      ALPPM3(102)=          2.5441341D0
      EISOLP(102)=          4.0000000D0
      GSSPM3(102)=         12.8480000D0
      HSPPM3(102)=          0.1000000D0
      DDPM3 (102)=          0.0684105D0
      QQPM3 (102)=          1.0540926D0
      AMPM3 (102)=          0.4721793D0
      ADPM3 (102)=          0.9262742D0
      AQPM3 (102)=          0.2909059D0
      USSPM3(108)=         -7.3964270D0
      UPPPM3(108)=         -1.1486568D0
      BETASP(108)=         -9.6737870D0
      BETAPP(108)=         -1.3000000D0
      ZSPM3 (108)=          0.9880780D0
      ZPPM3 (108)=          0.3000000D0
      ALPPM3(108)=          2.9323240D0
      EISOLP(108)=         28.7118620D0
      GSSPM3(108)=         12.8480000D0
      GSPPM3(108)=          2.1600000D0
      GPPPM3(108)=          3.0100000D0
      GP2PM3(108)=          2.4400000D0
      HSPPM3(108)=          1.9200000D0
      DDPM3 (108)=          0.9675816D0
      QQPM3 (108)=          4.0824829D0
      AMPM3 (108)=          0.4721603D0
      ADPM3 (108)=          0.5007132D0
      AQPM3 (108)=          0.1603621D0
      GAUSP1(108,1)=          0.1227960D0
      GAUSP2(108,1)=          5.0000000D0
      GAUSP3(108,1)=          1.2000000D0
      GAUSP1(108,2)=          0.0050900D0
      GAUSP2(108,2)=          5.0000000D0
      GAUSP3(108,2)=          1.8000000D0
      GAUSP1(108,3)=         -0.0183360D0
      GAUSP2(108,3)=          2.0000000D0
      GAUSP3(108,3)=          2.1000000D0
      GAUSP1(108,4)=          0.0000000D0
      GAUSP2(108,4)=          0.0000000D0
      GAUSP3(108,4)=          0.0000000D0




C     START OF PDDG/PM3 PARAMETERS

      REFPDG( 1) =' H:  (PDDG/PM3): REPASKY, CHADRASEKHAR, '//
     &                            'JORGENSEN, JCC 2002, 23(16), 1601.'
      REFPDG( 6) =' C:  (PDDG/PM3): REPASKY, CHADRASEKHAR, '//
     &                            'JORGENSEN, JCC 2002, 23(16), 1601.'
      REFPDG( 7) =' N:  (PDDG/PM3): REPASKY, CHADRASEKHAR, '//
     &                            'JORGENSEN, JCC 2002, 23(16), 1601.'
      REFPDG( 8) =' O:  (PDDG/PM3): REPASKY, CHADRASEKHAR, '//
     &                            'JORGENSEN, JCC 2002, 23(16), 1601.'
      REFPDG( 9) =' F:  (PDDG/PM3): TUBERT-BROHMAN,GUIMARAES,'//
     &                            'REPASKY,JORGENSEN,JCC 2004 25,138.'
      REFPDG(14) =' Si: (PDDG/PM3): TUBERT-BROHMAN,GUIMARAE'//
     &                            'S,JORGENSEN, JCTC 2005 1, 817.'
      REFPDG(15) =' P:  (PDDG/PM3): TUBERT-BROHMAN,GUIMARAE'//
     &                            'S,JORGENSEN, JCTC 2005 1, 817.'
      REFPDG(16) =' S:  (PDDG/PM3): TUBERT-BROHMAN,GUIMARAE'//
     &                            'S,JORGENSEN, JCTC 2005 1, 817.'
      REFPDG(17) =' F:  (PDDG/PM3): TUBERT-BROHMAN,GUIMARAE'//
     &                            'S,REPASKY,JORGENSEN,JCC 2004 25,138.'
      REFPDG(35) =' Br: (PDDG/PM3): TUBERT-BROHMAN,GUIMARAE'//
     &                            'S,REPASKY,JORGENSEN,JCC 2004 25,138.'
      REFPDG(53) =' I:  (PDDG/PM3): TUBERT-BROHMAN,GUIMARAE'//
     &                            'S,REPASKY,JORGENSEN,JCC 2004 25,138.'

      USSPDG(1)=     -12.893272003385D0
      BETASG(1)=     -6.1526542062173D0
      ZSPDG (1)=     0.97278550084430D0
      ALPPDG(1)=     3.38168610300700D0
      EISOLG(1)=     -13.120566198192D0
      GSSPDG(1)=           14.7942080D0
      AMPDG (1)=     0.54370481704970D0
      ADPDG (1)=     0.54370481704970D0
      AQPDG (1)=     0.54370481704970D0
      GAUSG1(1,1)=   1.12224395962630D0
      GAUSG2(1,1)=   4.70779030777590D0
      GAUSG3(1,1)=   1.54709920873910D0
      GAUSG1(1,2)=   -1.0697373657305D0
      GAUSG2(1,2)=   5.85799464741120D0
      GAUSG3(1,2)=   1.56789274832050D0
      PAPDG(1)=      0.05719290135800D0
      PBPDG(1)=      -0.0348228612590D0
      DAPDG(1)=      0.66339504047230D0
      DBPDG(1)=      1.08190071942210D0
      NGAUSG(1)=            2
      USSPDG(6)=     -48.241240946951D0
      UPPPDG(6)=     -36.461255999939D0
      BETASG(6)=     -11.952818190434D0
      BETAPG(6)=     -9.9224112120852D0
      ZSPDG (6)=     1.56786358751710D0
      ZPPDG (6)=     1.84665852120070D0
      ALPPDG(6)=     2.72577212540530D0
      EISOLG(6)=     -113.42824208974D0
      GSSPDG(6)=           11.2007080D0
      GSPPDG(6)=           10.2650270D0
      GPPPDG(6)=           10.7962920D0
      GP2PDG(6)=            9.0425660D0
      HSPPDG(6)=            2.2909800D0
      DDPDG (6)=     0.83141317761820D0
      QQPDG (6)=     0.66322216984400D0
      AMPDG (6)=     0.41163939928160D0
      ADPDG (6)=     0.58929751233060D0
      AQPDG (6)=     0.76594914927100D0
      GAUSG1(6,1)=   0.04890550330860D0
      GAUSG2(6,1)=   5.76533980799120D0
      GAUSG3(6,1)=   1.68223169651660D0
      GAUSG1(6,2)=   0.04769663311610D0
      GAUSG2(6,2)=   5.97372073873460D0
      GAUSG3(6,2)=   0.89440631619350D0
      PAPDG(6)=      -0.0007433618099D0
      PBPDG(6)=      0.00098516072940D0
      DAPDG(6)=      0.83691519687330D0
      DBPDG(6)=      1.58523608520060D0
      NGAUSG(6)=            2
      USSPDG(7)=     -49.454546358059D0
      UPPPDG(7)=     -47.757406358412D0
      BETASG(7)=     -14.117229602371D0
      BETAPG(7)=     -19.938508878969D0
      ZSPDG (7)=     2.03580684361910D0
      ZPPDG (7)=     2.32432725808280D0
      ALPPDG(7)=     2.84912399973850D0
      EISOLG(7)=     -158.41620481951D0
      GSSPDG(7)=           11.9047870D0
      GSPPDG(7)=            7.3485650D0
      GPPPDG(7)=           11.7546720D0
      GP2PDG(7)=           10.8072770D0
      HSPPDG(7)=            1.1367130D0
      DDPDG (7)=     0.65485453533410D0
      QQPDG (7)=     0.52692445400390D0
      AMPDG (7)=     0.43751514361910D0
      ADPDG (7)=     0.50442060161600D0
      AQPDG (7)=     0.73887610477190D0
      GAUSG1(7,1)=   1.51332030575080D0
      GAUSG2(7,1)=   5.90439402634500D0
      GAUSG3(7,1)=   1.72837621719040D0
      GAUSG1(7,2)=   -1.5118916914302D0
      GAUSG2(7,2)=   6.03001440913320D0
      GAUSG3(7,2)=   1.73410826456840D0
      PAPDG(7)=      -0.0031600751673D0
      PBPDG(7)=      0.01250092178130D0
      DAPDG(7)=      1.00417177651930D0
      DBPDG(7)=      1.51633618021020D0
      NGAUSG(7)=            2
      USSPDG(8)=     -87.412505208248D0
      UPPPDG(8)=     -72.183069806393D0
      BETASG(8)=     -44.874553472211D0
      BETAPG(8)=     -24.601939339720D0
      ZSPDG (8)=     3.81456531095080D0
      ZPPDG (8)=     2.31801122165690D0
      ALPPDG(8)=     3.22530882036500D0
      EISOLG(8)=     -292.18876564023D0
      GSSPDG(8)=           15.7557600D0
      GSPPDG(8)=           10.6211600D0
      GPPPDG(8)=           13.6540160D0
      GP2PDG(8)=           12.4060950D0
      HSPPDG(8)=            0.5938830D0
      DDPDG (8)=     0.40374106034030D0
      QQPDG (8)=     0.52836019944550D0
      AMPDG (8)=     0.57904300171250D0
      ADPDG (8)=     0.53403626392780D0
      AQPDG (8)=     0.80090914697820D0
      GAUSG1(8,1)=   -1.1384554300359D0
      GAUSG2(8,1)=   6.00004254473730D0
      GAUSG3(8,1)=   1.62236167639400D0
      GAUSG1(8,2)=   1.14600702743950D0
      GAUSG2(8,2)=   5.96349383486760D0
      GAUSG3(8,2)=   1.61478803799000D0
      PAPDG(8)=     -0.00099962677420D0
      PBPDG(8)=     -0.00152161350520D0
      DAPDG(8)=     1.36068502987020D0
      DBPDG(8)=     1.36640659538530D0
      NGAUSG(8)=           2
      USSPDG(9)=     -111.400432D0
      UPPPDG(9)=     -106.395264D0
      BETASG(9)=      -50.937301D0
      BETAPG(9)=      -31.636976D0
      ZSPDG (9)=        5.538033D0
      ZPPDG (9)=        2.538066D0
      ALPPDG(9)=        3.200571D0
      EISOLG(9)=     -442.457133D0
      GSSPDG(9)=      10.4966670D0
      GSPPDG(9)=      16.0736890D0
      GPPPDG(9)=      14.8172560D0
      GP2PDG(9)=      14.4183930D0
      HSPPDG(9)=       0.7277630D0
      DDPDG (9)=       0.246601D0
      QQPDG (9)=       0.482551D0
      AMPDG (9)=      0.3857650D0
      ADPDG (9)=      0.7878569D0
      AQPDG (9)=      0.6204998D0
      GAUSG1(9,1)=     -0.008079D0
      GAUSG2(9,1)=      5.938969D0
      GAUSG3(9,1)=      1.863949D0
      GAUSG1(9,2)=     -0.002659D0
      GAUSG2(9,2)=      5.925105D0
      GAUSG3(9,2)=      2.388864D0
      PAPDG (9)=       -0.012866D0
      PBPDG (9)=        0.007315D0
      DAPDG (9)=        1.305681D0
      DBPDG (9)=        1.842572D0
      NGAUSG(9)=        2         
      USSPDG(14)=      -26.332522D0
      UPPPDG(14)=      -22.602540D0
      BETASG(14)=       -3.376445D0
      BETAPG(14)=       -3.620969D0
      ZSPDG (14)=        1.586389D0
      ZPPDG (14)=        1.485958D0
      ALPPDG(14)=        2.215157D0
      EISOLG(14)=      -66.839000D0
      GSSPDG(14)=       5.0471960D0
      GSPPDG(14)=       5.9490570D0
      GPPPDG(14)=       6.7593670D0
      GP2PDG(14)=       5.1612970D0
      HSPPDG(14)=       0.9198320D0
      DDPDG (14)=        1.310515D0
      QQPDG (14)=        1.126089D0
      AMPDG (14)=        0.185490D0
      ADPDG (14)=        0.306606D0
      AQPDG (14)=        0.526759D0
      GAUSG1(14,1)=     -0.071314D0
      GAUSG2(14,1)=      6.000000D0
      GAUSG3(14,1)=      0.237995D0
      GAUSG1(14,2)=      0.089451D0
      GAUSG2(14,2)=      6.000000D0
      GAUSG3(14,2)=      1.897728D0
      PAPDG (14)=       -0.091928D0
      PBPDG (14)=       -0.040753D0
      DAPDG (14)=        1.163190D0
      DBPDG (14)=        2.190526D0
      NGAUSG(14)=        2         
      USSPDG(15)=      -37.882113D0
      UPPPDG(15)=      -30.312979D0
      BETASG(15)=      -12.676297D0
      BETAPG(15)=       -7.093318D0
      ZSPDG (15)=        2.395882D0
      ZPPDG (15)=        1.742213D0
      ALPPDG(15)=        2.005294D0
      EISOLG(15)=     -117.212854D0
      GSSPDG(15)=        7.8016150D0
      GSPPDG(15)=        5.1869490D0
      GPPPDG(15)=        6.6184780D0
      GP2PDG(15)=        6.0620020D0
      HSPPDG(15)=        1.5428090D0
      DDPDG (15)=        0.893978D0
      QQPDG (15)=        0.960457D0
      AMPDG (15)=        0.286719D0
      ADPDG (15)=        0.475805D0
      AQPDG (15)=        0.413597D0
      GAUSG1(15,1)=     -0.398055D0
      GAUSG2(15,1)=      1.997272D0
      GAUSG3(15,1)=      0.950073D0
      GAUSG1(15,2)=     -0.079653D0
      GAUSG2(15,2)=      1.998360D0
      GAUSG3(15,2)=      2.336959D0
      PAPDG (15)=        0.462741D0
      PBPDG (15)=       -0.020444D0
      DAPDG (15)=        0.714296D0
      DBPDG (15)=        2.041209D0
      NGAUSG(15)=        2          
      USSPDG(16)=      -43.906366D0
      UPPPDG(16)=      -43.461348D0
      BETASG(16)=       -2.953912D0
      BETAPG(16)=       -8.507779D0
      ZSPDG (16)=        1.012002D0
      ZPPDG (16)=        1.876999D0
      ALPPDG(16)=        2.539751D0
      EISOLG(16)=     -166.336554D0
      GSSPDG(16)=        8.9646670D0
      GSPPDG(16)=        6.7859360D0
      GPPPDG(16)=        9.9681640D0
      GP2PDG(16)=        7.9702470D0
      HSPPDG(16)=        4.0418360D0
      DDPDG (16)=        1.006989D0
      QQPDG (16)=        0.891487D0
      AMPDG (16)=        0.329462D0
      ADPDG (16)=        0.702571D0
      AQPDG (16)=        0.662835D0
      GAUSG1(16,1)=     -0.330692D0
      GAUSG2(16,1)=      6.000000D0
      GAUSG3(16,1)=      0.823837D0
      GAUSG1(16,2)=      0.024171D0
      GAUSG2(16,2)=      6.000000D0
      GAUSG3(16,2)=      2.017756D0
      PAPDG (16)=        0.120434D0
      PBPDG (16)=       -0.002663D0
      DAPDG (16)=        0.672870D0
      DBPDG (16)=        2.032340D0
      NGAUSG(16)=        2         
      USSPDG(17)=      -95.094434D0
      UPPPDG(17)=      -53.921715D0
      BETASG(17)=      -26.913129D0
      BETAPG(17)=      -14.991178D0
      ZSPDG (17)=        2.548268D0
      ZPPDG (17)=        2.284624D0
      ALPPDG(17)=        2.497617D0
      EISOLG(17)=     -305.715201D0
      GSSPDG(17)=       16.0136010D0
      GSPPDG(17)=        8.0481150D0
      GPPPDG(17)=        7.5222150D0
      GP2PDG(17)=        7.5041540D0
      HSPPDG(17)=        3.4811530D0
      DDPDG (17)=        0.827561D0
      QQPDG (17)=        0.732427D0
      AMPDG (17)=        0.5885190D0
      ADPDG (17)=        0.7182216D0
      AQPDG (17)=        0.2174760D0
      GAUSG1(17,1)=     -0.112222D0
      GAUSG2(17,1)=      5.963719D0
      GAUSG3(17,1)=      1.027719D0
      GAUSG1(17,2)=     -0.013061D0
      GAUSG2(17,2)=      1.999556D0
      GAUSG3(17,2)=      2.286377D0
      PAPDG (17)=       -0.016552D0
      PBPDG (17)=       -0.016646D0
      DAPDG (17)=        1.727690D0
      DBPDG (17)=        1.784655D0
      NGAUSG(17)=        2           
      USSPDG(35)=     -115.841963D0
      UPPPDG(35)=      -74.205146D0
      BETASG(35)=      -21.538044D0
      BETAPG(35)=       -8.524764D0
      ZSPDG (35)=        4.345079D0
      ZPPDG (35)=        2.190961D0
      ALPPDG(35)=        2.424673D0
      EISOLG(35)=     -351.013887D0
      GSSPDG(35)=         15.9434250D0
      GSPPDG(35)=         16.0616800D0
      GPPPDG(35)=          8.2827630D0
      GP2PDG(35)=          7.8168490D0
      HSPPDG(35)=          0.5788690D0
      DDPDG (35)=          0.473860D0                   
      QQPDG (35)=          0.968214D0            
      AMPDG (35)=          0.585940D0            
      ADPDG (35)=          0.477815D0            
      AQPDG (35)=          0.390429D0              
      GAUSG1(35,1)=        0.961362D0 
      GAUSG2(35,1)=        6.013600D0
      GAUSG3(35,1)=        2.340445D0
      GAUSG1(35,2)=       -0.948834D0   
      GAUSG2(35,2)=        5.976329D0
      GAUSG3(35,2)=        2.348745D0
      PAPDG (35)=         -0.013772D0
      PBPDG (35)=          0.008849D0   
      DAPDG (35)=          1.852030D0
      DBPDG (35)=          2.338958D0
      NGAUSG(35)=          2         
      USSPDG(53)=      -97.664174D0
      UPPPDG(53)=      -61.167137D0
      BETASG(53)=      -16.592621D0
      BETAPG(53)=       -6.599816D0
      ZSPDG (53)=        5.062801D0
      ZPPDG (53)=        2.417757D0
      ALPPDG(53)=        1.978170D0
      EISOLG(53)=     -291.537869D0
      GSSPDG(53)=         13.6319430D0
      GSPPDG(53)=         14.9904060D0
      GPPPDG(53)=          7.2883300D0
      GP2PDG(53)=          5.9664070D0
      HSPPDG(53)=          2.6300350D0
      DDPDG (53)=          0.407261D0 
      QQPDG (53)=          1.062574D0 
      AMPDG (53)=          0.500990D0 
      ADPDG (53)=          0.939338D0 
      AQPDG (53)=          0.510317D0 
      GAUSG1(53,1)=       -0.136003D0 
      GAUSG2(53,1)=        3.852912D0   
      GAUSG3(53,1)=        1.697455D0   
      GAUSG1(53,2)=       -0.037287D0   
      GAUSG2(53,2)=        5.229264D0   
      GAUSG3(53,2)=        2.768669D0   
      PAPDG (53)=          0.012901D0    
      PBPDG (53)=         -0.012825D0   
      DAPDG (53)=          1.994299D0   
      DBPDG (53)=          2.263417D0   
      NGAUSG(53)=          2            



*     START OF PDDG/MNDO PARAMETERS

      REFMDG( 1) =' H:  (PDDG/MNDO): REPASKY, CHADRASEKHAR,'//
     &                            ' JORGENSEN, JCC 2002, 23(16), 1601.' 
      REFMDG( 6) =' C:  (PDDG/MNDO): REPASKY, CHADRASEKHAR,'//
     &                            ' JORGENSEN, JCC 2002, 23(16), 1601.'    
      REFMDG( 7) =' N:  (PDDG/MNDO): REPASKY, CHADRASEKHAR,'//
     &                            ' JORGENSEN, JCC 2002, 23(16), 1601.'    
      REFMDG( 8) =' O:  (PDDG/MNDO): REPASKY, CHADRASEKHAR,'//
     &                            ' JORGENSEN, JCC 2002, 23(16), 1601.'    
      REFMDG( 9) =' F:  (PDDG/MNDO): TUBERT-BROHMAN,GUIMARA'//
     &                           'ES,REPASKY,JORGENSEN,JCC 2004 25,138.'
      REFMDG(17) =' Cl: (PDDG/MNDO): TUBERT-BROHMAN,GUIMARA'//
     &                           'ES,REPASKY,JORGENSEN,JCC 2004 25,138.'
      REFMDG(35) =' Br: (PDDG/MNDO): TUBERT-BROHMAN,GUIMARA'//
     &                           'ES,REPASKY,JORGENSEN,JCC 2004 25,138.'
      REFMDG(53) =' I:  (PDDG/MNDO): TUBERT-BROHMAN,GUIMARA'//
     &                           'ES,REPASKY,JORGENSEN,JCC 2004 25,138.'

      USSMDG(1)=    -11.724114276410D0
      BETASH(1)=    -7.4935039195719D0
      ZSMDG(1)=     1.32243115467370D0
      ALPMDG(1)=    2.49181323064320D0
      EISOLH(1)=    -12.015955786557D0
      GSSMDG(1)=       12.848D00 
      AMMDG(1)=     0.47217934812430D0
      ADMDG(1)=     0.47217934812430D0
      AQMDG(1)=     0.47217934812430D0
      PAMDG(1)=     -0.1088607444359D0
      PBMDG(1)=     -0.0247060666203D0
      DAMDG(1)=     0.46072116172000D0
      DBMDG(1)=     1.29873123436820D0
      USSMDG(6)=    -53.837582488984D0
      UPPMDG(6)=    -39.936408766823D0
      BETASH(6)=    -18.841334137411D0
      BETAPH(6)=    -7.9222341225346D0
      ZSMDG(6)=     1.80981702301050D0
      ZPMDG(6)=     1.82500792388930D0
      ALPMDG(6)=    2.55552238806810D0
      EISOLH(6)=    -123.86441152368D0
      GSSMDG(6)=     12.23D00 
      GPPMDG(6)=     11.08D00 
      GSPMDG(6)=     11.47D00 
      GP2MDG(6)=     9.84D00 
      HSPMDG(6)=     2.43D00 
      DDMDG(6)=     0.79415790778110D0
      QQMDG(6)=     0.67109016643690D0
      AMMDG(6)=     0.44946710986610D0
      ADMDG(6)=     0.62058137837870D0
      AQMDG(6)=     0.67810055293470D0
      PAMDG(6)=     -0.0068893327627D0
      PBMDG(6)=     -0.0277514418977D0
      DAMDG(6)=     1.19245557326430D0
      DBMDG(6)=     1.32952163414800D0
      USSMDG(7)=    -71.87189435530930D0
      UPPMDG(7)=    -58.21661676886340D0
      BETASH(7)=    -20.37577411084280D0
      BETAPH(7)=    -21.08537341050740D0
      ZSMDG(7)=     2.23142379586030D0
      ZPMDG(7)=     2.25345995688440D0
      ALPMDG(7)=    2.84367788492060D0
      EISOLH(7)=    -206.46662581723320D0
      GSSMDG(7)=    13.59D00
      GPPMDG(7)=    12.98D00
      GSPMDG(7)=    12.66D00
      GP2MDG(7)=    11.59D00
      HSPMDG(7)=    3.14D00
      DDMDG(7)=     0.64362354949890D0
      QQMDG(7)=     0.54349528938820D0
      AMMDG(7)=     0.49944873451190D0
      ADMDG(7)=     0.78188576956680D0
      AQMDG(7)=     0.81211139492050D0
      PAMDG(7)=     0.03502693409010D0
      PBMDG(7)=     -0.00172140634590D0
      DAMDG(7)=     1.01162987147870D0
      DBMDG(7)=     2.27842256966070D0
      USSMDG(8)=    -97.884970179897D0
      UPPMDG(8)=    -77.342673804072D0
      BETASH(8)=    -33.606335624658D0
      BETAPH(8)=    -27.984442042827D0
      ZSMDG(8)=     2.56917199926090D0
      ZPMDG(8)=     2.69715151721790D0
      ALPMDG(8)=    3.23884200872830D0
      EISOLH(8)=    -310.87974465179D0
      GSSMDG(8)=    15.42D00
      GPPMDG(8)=    14.52D00
      GSPMDG(8)=    14.48D00
      GP2MDG(8)=    12.98D00
      HSPMDG(8)=    3.94D00
      DDMDG(8)=     0.54734406013390D0
      QQMDG(8)=     0.45408827185760D0
      AMMDG(8)=     0.56670342061620D0
      ADMDG(8)=     0.94709991169520D0
      AQMDG(8)=     0.94892420489320D0
      PAMDG(8)=     0.08634413812890D0
      PBMDG(8)=     0.03040342779910D0
      DAMDG(8)=     0.72540784783600D0
      DBMDG(8)=     0.70972848794410D0      
      USSMDG(9)=     -134.220379D0
      UPPMDG(9)=     -107.155961D0
      BETASH(9)=      -67.827612D0
      BETAPH(9)=      -40.924818D0
      ZSMDG (9)=        4.328519D0
      ZPMDG (9)=        2.905042D0
      ALPMDG(9)=        3.322382D0
      EISOLH(9)=     -488.703243D0
      GSSMDG(9)=     16.92D00
      GPPMDG(9)=     16.71D00
      GSPMDG(9)=     17.25D00
      GP2MDG(9)=     14.91D00
      HSPMDG(9)=     4.83D00
      DDMDG(9)=      0.361556D0
      QQMDG(9)=      0.421593D0
      AMMDG(9)=      0.621830D0
      ADMDG(9)=      1.303601D0
      AQMDG(9)=      1.048409D0
      PAMDG(9)=      -0.011579D0
      PBMDG(9)=      -0.012943D0
      DAMDG(9)=       0.834606D0
      DBMDG(9)=       1.875603D0   
      USSMDG(17)=    -111.133653D0
      UPPMDG(17)=     -78.062493D0
      BETASH(17)=     -15.663317D0
      BETAPH(17)=     -15.399331D0
      ZSMDG (17)=       4.212404D0
      ZPMDG (17)=       2.037647D0
      ALPMDG(17)=       2.602846D0
      EISOLH(17)=    -378.909727D0
      GSSMDG(17)=    15.03D00
      GPPMDG(17)=    11.30D00
      GSPMDG(17)=    13.16D00
      GP2MDG(17)=    9.97D00
      HSPMDG(17)=    2.42D00
      DDMDG (17)=    0.411609D0
      QQMDG (17)=    0.821202D0
      AMMDG (17)=    0.552370D0
      ADMDG (17)=    0.902123D0
      AQMDG (17)=    0.605617D0
      PAMDG (17)=    -0.017119D0
      PBMDG (17)=     0.005497D0
      DAMDG (17)=     1.466335D0
      DBMDG (17)=     2.236842D0
      USSMDG(35)=    -100.637007D0
      UPPMDG(35)=     -76.015735D0
      BETASH(35)=      -7.054170D0
      BETAPH(35)=     -10.221030D0
      ZSMDG (35)=       3.999975D0
      ZPMDG (35)=       2.245040D0
      ALPMDG(35)=       2.414265D0
      EISOLH(35)=    -349.564096D0
      GSSMDG(35)=    15.03643948D0
      GPPMDG(35)=    11.27632539D0
      GSPMDG(35)=    13.03468242D0
      GP2MDG(35)=    9.85442552D0
      HSPMDG(35)=    2.45586832D0
      DDMDG (35)=     0.574623D0
      QQMDG (35)=     0.944892D0
      AMMDG (35)=     0.552607D0
      ADMDG (35)=     0.747533D0
      AQMDG (35)=     0.564986D0
      PAMDG (35)=    -0.017133D0
      PBMDG (35)=    -0.016964D0
      DAMDG (35)=     2.201539D0
      DBMDG (35)=     2.255764D0
      USSMDG(53)=    -106.588422D0
      UPPMDG(53)=     -75.282605D0
      BETASH(53)=      -6.698375D0
      BETAPH(53)=      -5.693814D0
      ZSMDG (53)=       2.718404D0
      ZPMDG (53)=       2.461813D0
      ALPMDG(53)=       2.242446D0
      EISOLH(53)=    -356.076398D0
      GSSMDG(53)=    15.04044855D0
      GPPMDG(53)=    11.14778369D0
      GSPMDG(53)=    13.05655798D0
      GP2MDG(53)=    9.91409071D0
      HSPMDG(53)=    2.45638202D0
      DDMDG (53)=     1.209529D0
      QQMDG (53)=     1.043559D0
      AMMDG (53)=     0.552754D0
      ADMDG (53)=     0.498848D0
      AQMDG (53)=     0.504032D0
      PAMDG (53)=     0.009616D0
      PBMDG (53)=    -0.007505D0
      DAMDG (53)=     2.572332D0
      DBMDG (53)=     2.936456D0




*    START OF AM1-D PARAMETERS

      REFAMD ( 1)='  H: (AM1-D): '//
     &            'MCNAMARA, HILLIER, PCCP 2007 9(19), 2362.'
      REFAMD ( 6)='  C: (AM1-D): '//
     &            'MCNAMARA, HILLIER, PCCP 2007 9(19), 2362.'
      REFAMD ( 7)='  N: (AM1-D): '//
     &            'MCNAMARA, HILLIER, PCCP 2007 9(19), 2362.'
      REFAMD ( 8)='  O: (AM1-D): '//
     &            'MCNAMARA, HILLIER, PCCP 2007 9(19), 2362.'
      REFAMD ( 9)='  F: (AM1-D): '//
     &            'GRIMME, JCC 2004, 25(12), 1463.'
      REFAMD (10)=' Ne: (AM1-D): '//
     &            'GRIMME, JCC 2004, 25(12), 1463.'
      REFAMD (16)='  S: (AM1-D): MORGADO,MCNAMARA,HILLIER,'//
     &            'BURTON, VINCENT, JCTC 2007, 3(5), 1656.'
      REFAMD(108)=' Hp: (AM1-D): '//
     &            'Hydrogen (with p basis functions)'

      S6AMD  =     1.4D0
      ALPAMD =    23.0D0
      R0AMD ( 1)=       1.110000D0
      C6AMD ( 1)=       0.160000D0 
      USSAMD( 1)=     -11.223791D0
      UPPAMD( 1)=       0.000000D0
      BETASE( 1)=      -6.376265D0
      BETAPE( 1)=       0.000000D0
      ALPHAE( 1)=       3.577756D0
      EISOLE( 1)=     -11.223791D0
      R0AMD ( 6)=       1.610000D0
      C6AMD ( 6)=       1.650000D0 
      USSAMD( 6)=     -52.183798D0
      UPPAMD( 6)=     -39.368413D0
      BETASE( 6)=     -15.682341D0
      BETAPE( 6)=      -7.804762D0
      ALPHAE( 6)=       2.625506D0
      EISOLE( 6)=     -120.634422D0
      R0AMD ( 7)=       1.550000D0
      C6AMD ( 7)=       1.110000D0 
      USSAMD( 7)=     -71.997845D0
      UPPAMD( 7)=     -57.401718D0
      BETASE( 7)=     -20.092408D0
      BETAPE( 7)=     -18.470679D0
      ALPHAE( 7)=       2.968737D0
      EISOLE( 7)=     -203.385844D0
      R0AMD ( 8)=       1.490000D0
      C6AMD ( 8)=       0.700000D0 
      USSAMD( 8)=     -97.610588D0
      UPPAMD( 8)=     -78.589700D0
      BETASE( 8)=     -29.502481D0
      BETAPE( 8)=     -29.495380D0
      ALPHAE( 8)=       4.633699D0
      EISOLE( 8)=     -316.969976D0
      R0AMD ( 9)=       1.430000D0
      C6AMD ( 9)=       0.570000D0 
      R0AMD (10)=       1.380000D0
      C6AMD (10)=       0.450000D0 
      R0AMD (16)=       1.870000D0
      C6AMD (16)=      10.300000D0 
      USSAMD(16)=     -57.235044D0
      UPPAMD(16)=     -48.307513D0
      BETASE(16)=      -3.311308D0
      BETAPE(16)=      -7.256468D0
      ALPHAE(16)=       2.309315D0
      EISOLE(16)=     -191.176025D0
      R0AMD (108)=      1.110000D0
      C6AMD (108)=      0.160000D0 



*    START OF PM3-D PARAMETERS

      REFPMD( 1)= '  H: (PM3-D): MCNAMARA, HILLIER, PCCP 2007 9(19), '//
     &                                                        '2362.'
      REFPMD( 6)= '  C: (PM3-D): MCNAMARA, HILLIER, PCCP 2007 9(19), '//
     &                                                        '2362.'
      REFPMD( 7)= '  N: (PM3-D): MCNAMARA, HILLIER, PCCP 2007 9(19), '//
     &                                                        '2362.'
      REFPMD( 8)= '  O: (PM3-D): MCNAMARA, HILLIER, PCCP 2007 9(19), '//
     &                                                        '2362.'
      REFPMD( 9)= '  F: (PM3-D): GRIMME, JCC 2004, 25(12), 1463.'
      REFPMD(10)= ' Ne: (PM3-D): GRIMME, JCC 2004, 25(12), 1463.'
      REFPMD(16)= '  S: (PM3-D): MORGADO,MCNAMARA,HILLIER,BURTON, '//
     &                               'VINCENT, JCTC 2007, 3(5), 1656.'
      REFPMD(108)=' Hp: (PM3-D): Hydrogen (with p basis functions)'

      S6PMD      =     1.4D0
      ALPPMD     =    23.0D0
      R0PMD ( 1)=       1.110000D0
      C6PMD ( 1)=       0.160000D0 
      USSPMD( 1)=     -13.054076D0
      UPPPMD( 1)=       0.000000D0
      BETASD( 1)=      -5.628901D0
      BETAPD( 1)=       0.000000D0
      ALPHAD( 1)=       3.417532D0
      EISOLD( 1)=     -13.054076D0
      R0PMD ( 6)=       1.610000D0
      C6PMD ( 6)=       1.650000D0 
      USSPMD( 6)=     -47.275431D0
      UPPPMD( 6)=     -36.268916D0
      BETASD( 6)=     -11.941466D0
      BETAPD( 6)=      -9.819760D0
      ALPHAD( 6)=       2.721152D0
      EISOLD( 6)=     -111.244135D0
      R0PMD ( 7)=       1.550000D0
      C6PMD ( 7)=       1.110000D0 
      USSPMD( 7)=     -49.348460D0
      UPPPMD( 7)=     -47.543768D0
      BETASD( 7)=     -14.068411D0
      BETAPD( 7)=     -20.039292D0
      ALPHAD( 7)=       3.060404D0
      EISOLD( 7)=     -157.741448D0
      R0PMD ( 8)=       1.490000D0
      C6PMD ( 8)=       0.700000D0 
      USSPMD( 8)=     -86.960302D0
      UPPPMD( 8)=     -71.926845D0
      BETASD( 8)=     -45.234302D0
      BETAPD( 8)=     -24.788037D0
      ALPHAD( 8)=       3.387806D0
      EISOLD( 8)=     -289.465867D0
      R0PMD ( 9)=       1.430000D0
      C6PMD ( 9)=       0.570000D0 
      R0PMD (10)=       1.380000D0
      C6PMD (10)=       0.450000D0 
      R0PMD (16)=       1.870000D0
      C6PMD (16)=      10.300000D0 
      USSPMD(16)=     -50.249536D0
      UPPPMD(16)=     -43.968965D0
      BETASD(16)=      -8.397415D0
      BETAPD(16)=      -7.594232D0
      ALPHAD(16)=       2.234331D0
      EISOLD(16)=    -182.467598D0
      R0PMD (108)=      1.110000D0
      C6PMD (108)=      0.160000D0 




*    START OF MNDO-D PARAMETERS

      REFMND( 1) ='  H: (MNDO-D): USING PM3-D DISPERSION PARAMETERS.'
      REFMND( 6) ='  C: (MNDO-D): USING PM3-D DISPERSION PARAMETERS.'
      REFMND( 7) ='  N: (MNDO-D): USING PM3-D DISPERSION PARAMETERS.'
      REFMND( 8) ='  O: (MNDO-D): USING PM3-D DISPERSION PARAMETERS.'
      REFMND( 9) ='  F: (MNDO-D): USING PM3-D DISPERSION PARAMETERS.'
      REFMND(10) =' Ne: (MNDO-D): USING PM3-D DISPERSION PARAMETERS.'
      REFMND(16) ='  S: (MNDO-D): USING PM3-D DISPERSION PARAMETERS.'
      REFMND(108)=' Hp: (MNDO-D): USING PM3-D DISPERSION PARAMETERS.'

      S6MND      =     1.4D0
      ALPMND     =    23.0D0
      R0MND ( 1)=      1.110000D0
      C6MND ( 1)=      0.160000D0 
      R0MND ( 6)=      1.610000D0
      C6MND ( 6)=      1.650000D0 
      R0MND ( 7)=      1.550000D0
      C6MND ( 7)=      1.110000D0 
      R0MND ( 8)=      1.490000D0
      C6MND ( 8)=      0.700000D0 
      R0MND ( 9)=      1.430000D0
      C6MND ( 9)=      0.570000D0 
      R0MND (10)=      1.380000D0
      C6MND (10)=      0.450000D0 
      R0MND (16)=      1.870000D0
      C6MND (16)=     10.300000D0 
      R0MND (108)=    1.110000D0
      C6MND (108)=    0.160000D0 



*    START OF RM1-D PARAMETERS

      REFRMD ( 1)='  H: (RM1-D):  USING AM1-D DISPERSION PARAMETERS.'
      REFRMD ( 6)='  C: (RM1-D):  USING AM1-D DISPERSION PARAMETERS.'
      REFRMD ( 7)='  N: (RM1-D):  USING AM1-D DISPERSION PARAMETERS.'
      REFRMD ( 8)='  O: (RM1-D):  USING AM1-D DISPERSION PARAMETERS.'
      REFRMD ( 9)='  F: (RM1-D):  USING AM1-D DISPERSION PARAMETERS.'
      REFRMD (10)=' Ne: (RM1-D):  USING AM1-D DISPERSION PARAMETERS.'
      REFRMD (16)='  S: (RM1-D):  USING AM1-D DISPERSION PARAMETERS.'
      REFRMD(108)=' Hp: (RM1-D): Hydrogen (with p basis functions)'

      S6RMD      =     1.4D0
      ALPRMD     =    23.0D0
      R0RMD ( 1)=       1.110000D0
      C6RMD ( 1)=       0.160000D0 
      R0RMD ( 6)=       1.610000D0
      C6RMD ( 6)=       1.650000D0 
      R0RMD ( 7)=       1.550000D0
      C6RMD ( 7)=       1.110000D0 
      R0RMD ( 8)=       1.490000D0
      C6RMD ( 8)=       0.700000D0 
      R0RMD ( 9)=       1.430000D0
      C6RMD ( 9)=       0.570000D0 
      R0RMD (10)=       1.380000D0
      C6RMD (10)=       0.450000D0 
      R0RMD (16)=       1.870000D0
      C6RMD (16)=      10.300000D0 
      R0RMD (108)=      1.110000D0
      C6RMD (108)=      0.160000D0 




*    START OF PM6-D PARAMETERS

      REFP6D ( 1)='  H: (PM6-D):  USING PM3-D DISPERSION PARAMETERS.'
      REFP6D ( 6)='  C: (PM6-D):  USING PM3-D DISPERSION PARAMETERS.'
      REFP6D ( 7)='  N: (PM6-D):  USING PM3-D DISPERSION PARAMETERS.'
      REFP6D ( 8)='  O: (PM6-D):  USING PM3-D DISPERSION PARAMETERS.'
      REFP6D ( 9)='  F: (PM6-D):  USING PM3-D DISPERSION PARAMETERS.'
      REFP6D (10)=' Ne: (PM6-D):  USING PM3-D DISPERSION PARAMETERS.'
      REFP6D (16)='  S: (PM6-D):  USING PM3-D DISPERSION PARAMETERS.'
      REFP6D(108)=' Hp: (PM6-D):  Hydrogen (with p basis functions)'

      S6P6D      =        1.4D0 
      ALPP6D     =       23.0D0 
      R0P6D ( 1) =      1.110000D0
      C6P6D ( 1) =      0.160000D0 
      R0P6D ( 6) =      1.610000D0
      C6P6D ( 6) =      1.650000D0 
      R0P6D ( 7) =      1.550000D0
      C6P6D ( 7) =      1.110000D0 
      R0P6D ( 8) =      1.490000D0
      C6P6D ( 8) =      0.700000D0 
      R0P6D ( 9) =      1.430000D0
      C6P6D ( 9) =      0.570000D0 
      R0P6D (10) =      1.380000D0
      C6P6D (10) =      0.450000D0 
      R0P6D (16) =      1.870000D0
      C6P6D (16) =     10.300000D0 
      R0P6D (108)=      1.110000D0
      C6P6D (108)=      0.160000D0 



*     START OF PM6 MONOATOMIC PARAMETERS

      REFPM6 ( 1)='  H: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
     &                                                   '1173, (2007).'
      REFPM6( 2) ='  2: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
     &                                                   '1173, (2007).'
      REFPM6( 3) ='  3: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
     &                                                   '1173, (2007).'
      REFPM6( 4) ='  4: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
     &                                                   '1173, (2007).'
      REFPM6( 5) ='  5: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
     &                                                   '1173, (2007).'
      REFPM6( 6) ='  C: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
     &                                                   '1173, (2007).'
      REFPM6( 7) ='  N: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
     &                                                   '1173, (2007).'
      REFPM6( 8) ='  O: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
     &                                                   '1173, (2007).'
      REFPM6( 9) ='  9: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
     &                                                   '1173, (2007).'
      REFPM6(10) =' 10: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
     &                                                   '1173, (2007).'
      REFPM6(11) =' 11: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
     &                                                   '1173, (2007).'
      REFPM6(12) =' 12: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
     &                                                   '1173, (2007).'
C      REFPM6(13) =' 13: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
C     &                                                   '1173, (2007).'
C      REFPM6(14) =' 14: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
C     &                                                   '1173, (2007).'
C      REFPM6(15) =' 15: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
C     &                                                   '1173, (2007).'
C      REFPM6(16) =' 16: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
C     &                                                   '1173, (2007).'
C      REFPM6(17) =' 17: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
C     &                                                   '1173, (2007).'
C      REFPM6(18) =' 18: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, '//
C     &                                                   '1173, (2007).'
      REFPM6(108)=' Hp: (PM6): Hydrogen (with p basis functions)'

      USSPM6( 1)=           -11.246958D0   
      BETASX( 1)=            -8.352984D0   
      ZSPM6 ( 1)=             1.268641D0   
      EISOLX( 1)=           -11.2469580D0  
      GSSPM6( 1)=            14.448686D0   
      AMPM6 ( 1)=             0.94165599D0   
      ADPM6 ( 1)=             0.94165599D0   
      AQPM6 ( 1)=             0.94165599D0   
      GAUSX1( 1, 1)=          0.024184D0   
      GAUSX2( 1, 1)=          3.055953D0   
      GAUSX3( 1, 1)=          1.786011D0   
      USSPM6( 2)=           -31.770969D0   
      UPPPM6( 2)=            -5.856382D0   
      BETASX( 2)=           -58.903774D0   
      BETAPX( 2)=           -37.039974D0   
      ZSPM6 ( 2)=             3.313204D0   
      ZPPM6 ( 2)=             3.657133D0   
      EISOLX( 2)=             0.000000D0   
      GSSPM6( 2)=             9.445299D0   
      GSPPM6( 2)=            11.201419D0   
      GPPPM6( 2)=             9.214548D0   
      GP2PM6( 2)=            13.046115D0   
      HSPPM6( 2)=             0.299954D0   
      DDPM6 ( 2)=             0.2475819D0  
      QQPM6 ( 2)=             0.2118043D0  
      AMPM6 ( 2)=             0.3471120D0  
      ADPM6 ( 2)=             0.5756142D0  
      AQPM6 ( 2)=             0.9768631D0  
      USSPM6( 3)=            -4.709912D0   
      UPPPM6( 3)=            -2.722581D0   
      BETASX( 3)=            -2.283946D0   
      BETAPX( 3)=            -7.535573D0   
      ZSPM6 ( 3)=             0.981041D0   
      ZPPM6 ( 3)=             2.953445D0   
      EISOLX( 3)=            -4.7099120D0  
      GSSPM6( 3)=            11.035907D0   
      GSPPM6( 3)=            19.998647D0   
      GPPPM6( 3)=            11.543650D0   
      GP2PM6( 3)=             9.059036D0   
      HSPPM6( 3)=             1.641886D0   
      DDPM6 ( 3)=             0.3558537D0  
      QQPM6 ( 3)=             0.4146835D0  
      AMPM6 ( 3)=             0.4055664D0  
      ADPM6 ( 3)=             0.8444977D0  
      AQPM6 ( 3)=             1.1762157D0  
      USSPM6( 4)=           -16.360315D0   
      UPPPM6( 4)=           -16.339216D0   
      BETASX( 4)=            -3.199549D0   
      BETAPX( 4)=            -4.451920D0   
      ZSPM6 ( 4)=             1.212539D0   
      ZPPM6 ( 4)=             1.276487D0   
      EISOLX( 4)=           -25.1678260D0  
      GSSPM6( 4)=             7.552804D0   
      GSPPM6( 4)=            10.203146D0   
      GPPPM6( 4)=            12.862153D0   
      GP2PM6( 4)=            13.602858D0   
      HSPPM6( 4)=             1.501452D0   
      DDPM6 ( 4)=             1.1578786D0  
      QQPM6 ( 4)=             0.9594652D0  
      AMPM6 ( 4)=             0.2775633D0  
      ADPM6 ( 4)=             0.4062438D0  
      AQPM6 ( 4)=             0.3118248D0  
      GAUSX1( 4, 1)=          0.164180D0   
      GAUSX2( 4, 1)=          1.704828D0   
      GAUSX3( 4, 1)=          1.785591D0   
      USSPM6( 5)=           -25.967679D0   
      UPPPM6( 5)=           -19.115864D0   
      BETASX( 5)=            -4.959706D0   
      BETAPX( 5)=            -4.656753D0   
      ZSPM6 ( 5)=             1.634174D0   
      ZPPM6 ( 5)=             1.479195D0   
      EISOLX( 5)=           -49.5366840D0  
      GSSPM6( 5)=             8.179341D0   
      GSPPM6( 5)=             7.294021D0   
      GPPPM6( 5)=             7.829395D0   
      GP2PM6( 5)=             6.401072D0   
      HSPPM6( 5)=             1.252845D0   
      DDPM6 ( 5)=             0.9214783D0  
      QQPM6 ( 5)=             0.8279807D0  
      AMPM6 ( 5)=             0.3005884D0  
      ADPM6 ( 5)=             0.4282435D0  
      AQPM6 ( 5)=             0.6170044D0  
      USSPM6( 6)=           -51.089653D0   
      UPPPM6( 6)=           -39.937920D0   
      BETASX( 6)=           -15.385236D0   
      BETAPX( 6)=            -7.471929D0   
      ZSPM6 ( 6)=             2.047558D0   
      ZPPM6 ( 6)=             1.702841D0   
      EISOLX( 6)=          -115.2015800D0  
      GSSPM6( 6)=            13.335519D0   
      GSPPM6( 6)=            11.528134D0   
      GPPPM6( 6)=            10.778326D0   
      GP2PM6( 6)=             9.486212D0   
      HSPPM6( 6)=             0.717322D0   
      DDPM6 ( 6)=             0.7535643D0  
      QQPM6 ( 6)=             0.7192362D0  
      AMPM6 ( 6)=             0.4900764D0  
      ADPM6 ( 6)=             0.3870451D0  
      AQPM6 ( 6)=             0.6555882D0  
      GAUSX1( 6, 1)=          0.046302D0   
      GAUSX2( 6, 1)=          2.100206D0   
      GAUSX3( 6, 1)=          1.333959D0   
      USSPM6( 7)=           -57.784823D0   
      UPPPM6( 7)=           -49.893036D0   
      BETASX( 7)=           -17.979377D0   
      BETAPX( 7)=           -15.055017D0   
      ZSPM6 ( 7)=             2.380406D0   
      ZPPM6 ( 7)=             1.999246D0   
      EISOLX( 7)=          -174.9514445D0  
      GSSPM6( 7)=            12.357026D0   
      GSPPM6( 7)=             9.636190D0   
      GPPPM6( 7)=            12.570756D0   
      GP2PM6( 7)=            10.576425D0   
      HSPPM6( 7)=             2.871545D0   
      DDPM6 ( 7)=             0.6467179D0  
      QQPM6 ( 7)=             0.6126034D0  
      AMPM6 ( 7)=             0.4541171D0  
      ADPM6 ( 7)=             0.7490156D0  
      AQPM6 ( 7)=             0.8432463D0  
      GAUSX1( 7, 1)=         -0.001436D0   
      GAUSX2( 7, 1)=          0.495196D0   
      GAUSX3( 7, 1)=          1.704857D0   
      USSPM6( 8)=           -91.678761D0   
      UPPPM6( 8)=           -70.460949D0   
      BETASX( 8)=           -65.635137D0   
      BETAPX( 8)=           -21.622604D0   
      ZSPM6 ( 8)=             5.421751D0   
      ZPPM6 ( 8)=             2.270960D0   
      EISOLX( 8)=          -287.1272180D0  
      GSSPM6( 8)=            11.304042D0   
      GSPPM6( 8)=            15.807424D0   
      GPPPM6( 8)=            13.618205D0   
      GP2PM6( 8)=            10.332765D0   
      HSPPM6( 8)=             5.010801D0   
      DDPM6 ( 8)=             0.2371131D0  
      QQPM6 ( 8)=             0.5393071D0  
      AMPM6 ( 8)=             0.4154203D0  
      ADPM6 ( 8)=             1.6843332D0  
      AQPM6 ( 8)=             1.0935465D0  
      GAUSX1( 8, 1)=         -0.017771D0   
      GAUSX2( 8, 1)=          3.058310D0   
      GAUSX3( 8, 1)=          1.896435D0   
      USSPM6( 9)=          -140.225626D0   
      UPPPM6( 9)=           -98.778044D0   
      BETASX( 9)=           -69.922593D0   
      BETAPX( 9)=           -30.448165D0   
      ZSPM6 ( 9)=             6.043849D0   
      ZPPM6 ( 9)=             2.906722D0   
      EISOLX( 9)=          -468.1575840D0  
      GSSPM6( 9)=            12.446818D0   
      GSPPM6( 9)=            18.496082D0   
      GPPPM6( 9)=             8.417366D0   
      GP2PM6( 9)=            12.179816D0   
      HSPPM6( 9)=             2.604382D0   
      DDPM6 ( 9)=             0.2324062D0  
      QQPM6 ( 9)=             0.4213492D0  
      AMPM6 ( 9)=             0.4574169D0  
      ADPM6 ( 9)=             1.3108408D0  
      AQPM6 ( 9)=             0.5777487D0  
      GAUSX1( 9, 1)=         -0.010792D0   
      GAUSX2( 9, 1)=          6.004648D0   
      GAUSX3( 9, 1)=          1.847724D0   
      USSPM6(10)=            -2.978729D0   
      UPPPM6(10)=           -85.441118D0   
      BETASX(10)=           -69.793475D0   
      BETAPX(10)=           -33.261962D0   
      ZSPM6 (10)=             6.000148D0   
      ZPPM6 (10)=             3.834528D0   
      EISOLX(10)=          -512.6467080D0  
      GSSPM6(10)=            19.999574D0   
      GSPPM6(10)=            16.896951D0   
      GPPPM6(10)=             8.963560D0   
      GP2PM6(10)=            16.027799D0   
      HSPPM6(10)=             1.779280D0   
      DDPM6 (10)=             0.2592291D0  
      QQPM6 (10)=             0.3193991D0  
      AMPM6 (10)=             0.7349785D0  
      ADPM6 (10)=             1.0591897D0  
      AQPM6 (10)=             0.7132117D0  
      USSPM6(11)=            -4.537153D0   
      UPPPM6(11)=            -2.433015D0   
      BETASX(11)=             0.244853D0   
      BETAPX(11)=             0.491998D0   
      ZSPM6 (11)=             0.686327D0   
      ZPPM6 (11)=             0.950068D0   
      EISOLX(11)=            -4.5371530D0  
      GSSPM6(11)=             4.059972D0   
      GSPPM6(11)=             7.061183D0   
      GPPPM6(11)=             9.283540D0   
      GP2PM6(11)=            17.034978D0   
      HSPPM6(11)=             0.640715D0   
      DDPM6 (11)=             2.2523841D0  
      QQPM6 (11)=             1.7612635D0  
      AMPM6 (11)=             0.1492028D0  
      ADPM6 (11)=             0.1925461D0  
      AQPM6 (11)=             0.2000658D0  
      GAUSX1(11, 1)=         -1.026036D0   
      GAUSX2(11, 1)=          2.014506D0   
      GAUSX3(11, 1)=          1.271202D0   
      USSPM6(12)=           -14.574226D0   
      UPPPM6(12)=            -7.583850D0   
      BETASX(12)=            -9.604932D0   
      BETAPX(12)=             3.416908D0   
      ZSPM6 (12)=             1.310830D0   
      ZPPM6 (12)=             1.388897D0   
      EISOLX(12)=           -22.0331240D0  
      GSSPM6(12)=             7.115328D0   
      GSPPM6(12)=             3.253024D0   
      GPPPM6(12)=             4.737311D0   
      GP2PM6(12)=             8.428485D0   
      HSPPM6(12)=             0.877379D0   
      DDPM6 (12)=             1.4926089D0  
      QQPM6 (12)=             1.2047834D0  
      AMPM6 (12)=             0.2614862D0  
      ADPM6 (12)=             0.2787598D0  
      AQPM6 (12)=             0.2636855D0  
      USSPM6(13)=           -24.546778D0   
      UPPPM6(13)=           -20.104434D0   
      UDDPM6(13)=             8.004394D0   
      BETASX(13)=           -18.375229D0   
      BETAPX(13)=            -9.382700D0   
      BETADX(13)=           -20.840474D0   
      ZSPM6 (13)=             2.364264D0   
      ZPPM6 (13)=             1.749102D0   
      ZDPM6 (13)=             1.269384D0   
      ZSNPM6(13)=             4.742341D0   
      ZPNPM6(13)=             4.669626D0   
      ZDNPM6(13)=             7.131138D0   
      EISOLX(13)=           -48.0620250D0  
      GSSPM6(13)=             6.652155D0   
      GSPPM6(13)=             7.459435D0   
      GPPPM6(13)=             7.668857D0   
      GP2PM6(13)=             6.673299D0   
      HSPPM6(13)=             0.435060D0   
      DDPM6 (13)=             0.9077315D0  
      QQPM6 (13)=             0.9566738D0  
      AMPM6 (13)=             0.2444648D0  
      ADPM6 (13)=             0.2853390D0  
      AQPM6 (13)=             0.4968328D0  
      GAUSX1(13, 1)=          1.002222D0   
      GAUSX2(13, 1)=          1.517400D0   
      GAUSX3(13, 1)=          0.659101D0   
      USSPM6(14)=           -27.358058D0   
      UPPPM6(14)=           -20.490578D0   
      UDDPM6(14)=           -22.751900D0   
      BETASX(14)=            -8.686909D0   
      BETAPX(14)=            -1.856482D0   
      BETADX(14)=            -6.360627D0   
      ZSPM6 (14)=             1.752741D0   
      ZPPM6 (14)=             1.198413D0   
      ZDPM6 (14)=             2.128593D0   
      ZSNPM6(14)=             8.388111D0   
      ZPNPM6(14)=             1.843048D0   
      ZDNPM6(14)=             0.708600D0   
      EISOLX(14)=           -68.4282675D0  
      GSSPM6(14)=             5.194805D0   
      GSPPM6(14)=             5.090534D0   
      GPPPM6(14)=             5.185150D0   
      GP2PM6(14)=             4.769775D0   
      HSPPM6(14)=             1.425012D0   
      DDPM6 (14)=             1.2076673D0  
      QQPM6 (14)=             1.3962800D0  
      AMPM6 (14)=             0.1909076D0  
      ADPM6 (14)=             0.3877505D0  
      AQPM6 (14)=             0.2915117D0  
      GAUSX1(14, 1)=          0.208571D0   
      GAUSX2(14, 1)=          6.000483D0   
      GAUSX3(14, 1)=          1.185245D0   
      USSPM6(15)=           -48.729905D0   
      UPPPM6(15)=           -40.354689D0   
      UDDPM6(15)=            -7.349246D0   
      BETASX(15)=           -14.583780D0   
      BETAPX(15)=           -11.744725D0   
      BETADX(15)=           -20.099893D0   
      ZSPM6 (15)=             2.158033D0   
      ZPPM6 (15)=             1.805343D0   
      ZDPM6 (15)=             1.230358D0   
      ZSNPM6(15)=             6.042706D0   
      ZPNPM6(15)=             2.376473D0   
      ZDNPM6(15)=             7.147750D0   
      EISOLX(15)=          -139.6679330D0  
      GSSPM6(15)=             8.758856D0   
      GSPPM6(15)=             8.483679D0   
      GPPPM6(15)=             8.662754D0   
      GP2PM6(15)=             7.734264D0   
      HSPPM6(15)=             0.871681D0   
      DDPM6 (15)=             0.9917164D0  
      QQPM6 (15)=             0.9268710D0  
      AMPM6 (15)=             0.3218854D0  
      ADPM6 (15)=             0.3535243D0  
      AQPM6 (15)=             0.4962641D0  
      GAUSX1(15, 1)=         -0.034320D0   
      GAUSX2(15, 1)=          6.001394D0   
      GAUSX3(15, 1)=          2.296737D0   
      USSPM6(16)=           -47.530706D0   
      UPPPM6(16)=           -39.191045D0   
      UDDPM6(16)=           -46.306944D0   
      BETASX(16)=           -13.827440D0   
      BETAPX(16)=            -7.664613D0   
      BETADX(16)=            -9.986172D0   
      ZSPM6 (16)=             2.192844D0   
      ZPPM6 (16)=             1.841078D0   
      ZDPM6 (16)=             3.109401D0   
      ZSNPM6(16)=             0.479722D0   
      ZPNPM6(16)=             1.015507D0   
      ZDNPM6(16)=             4.317470D0   
      EISOLX(16)=          -171.7430195D0  
      GSSPM6(16)=             9.170350D0   
      GSPPM6(16)=             5.944296D0   
      GPPPM6(16)=             8.165473D0   
      GP2PM6(16)=             7.301878D0   
      HSPPM6(16)=             5.005404D0   
      DDPM6 (16)=             0.9754548D0  
      QQPM6 (16)=             0.9088806D0  
      AMPM6 (16)=             0.3370077D0  
      ADPM6 (16)=             0.7993884D0  
      AQPM6 (16)=             0.4914933D0  
      GAUSX1(16, 1)=         -0.036928D0   
      GAUSX2(16, 1)=          1.795067D0   
      GAUSX3(16, 1)=          2.082618D0   
      USSPM6(17)=           -61.389930D0   
      UPPPM6(17)=           -54.482801D0   
      UDDPM6(17)=           -38.258155D0   
      BETASX(17)=            -2.367988D0   
      BETAPX(17)=           -13.802139D0   
      BETADX(17)=            -4.037751D0   
      ZSPM6 (17)=             2.637050D0   
      ZPPM6 (17)=             2.118146D0   
      ZDPM6 (17)=             1.324033D0   
      ZSNPM6(17)=             0.956297D0   
      ZPNPM6(17)=             2.464067D0   
      ZDNPM6(17)=             6.410325D0   
      EISOLX(17)=          -252.9093760D0  
      GSSPM6(17)=            11.142654D0   
      GSPPM6(17)=             7.487881D0   
      GPPPM6(17)=             9.551886D0   
      GP2PM6(17)=             8.128436D0   
      HSPPM6(17)=             5.004267D0   
      DDPM6 (17)=             0.8150043D0  
      QQPM6 (17)=             0.7899928D0  
      AMPM6 (17)=             0.4094893D0  
      ADPM6 (17)=             0.8689914D0  
      AQPM6 (17)=             0.6357327D0  
      GAUSX1(17, 1)=         -0.013213D0   
      GAUSX2(17, 1)=          3.687022D0   
      GAUSX3(17, 1)=          2.544635D0   
      USSPM6(18)=            -7.797931D0   
      UPPPM6(18)=           -83.211487D0   
      BETASX(18)=            -8.839842D0   
      BETAPX(18)=           -28.427303D0   
      ZSPM6 (18)=             6.000272D0   
      ZPPM6 (18)=             5.949170D0   
      EISOLX(18)=          -499.2689220D0  
      GSSPM6(18)=            17.858776D0   
      GSPPM6(18)=             4.168451D0   
      GPPPM6(18)=            11.852500D0   
      GP2PM6(18)=            15.669543D0   
      HSPPM6(18)=             4.574549D0   
      DDPM6 (18)=             0.3381910D0  
      QQPM6 (18)=             0.2812695D0  
      AMPM6 (18)=             0.6563048D0  
      ADPM6 (18)=             1.3215046D0  
      AQPM6 (18)=             0.7859276D0  
      USSPM6(108)=         -7.3964270D0
      UPPPM6(108)=         -1.1486568D0
      BETASX(108)=         -9.6737870D0
      BETAPX(108)=         -1.3000000D0
      ZSPM6 (108)=          0.9880780D0
      ZPPM6 (108)=          0.3000000D0
      EISOLX(108)=         28.7118620D0
      GSSPM6(108)=         12.8480000D0
      GSPPM6(108)=          2.1600000D0
      GPPPM6(108)=          3.0100000D0
      GP2PM6(108)=          2.4400000D0
      HSPPM6(108)=          1.9200000D0
      DDPM6 (108)=          0.9675816D0
      QQPM6 (108)=          4.0824829D0
      AMPM6 (108)=          0.4721603D0
      ADPM6 (108)=          0.5007132D0
      AQPM6 (108)=          0.1603621D0
      GAUSX1(108,1)=          0.1227960D0
      GAUSX2(108,1)=          5.0000000D0
      GAUSX3(108,1)=          1.2000000D0
      GAUSX1(108,2)=          0.0050900D0
      GAUSX2(108,2)=          5.0000000D0
      GAUSX3(108,2)=          1.8000000D0
      GAUSX1(108,3)=         -0.0183360D0
      GAUSX2(108,3)=          2.0000000D0
      GAUSX3(108,3)=          2.1000000D0
      GAUSX1(108,4)=          0.0000000D0
      GAUSX2(108,4)=          0.0000000D0
      GAUSX3(108,4)=          0.0000000D0




*     START OF PM6 DIATOMIC PARAMETERS

      ALPPM6( 1, 1)=        3.540942D0  
      ALPPM6( 2, 1)=        2.989881D0  
      ALPPM6( 2, 2)=        3.783559D0  
      ALPPM6( 3, 1)=        2.136265D0       
      ALPPM6( 3, 2)=        3.112403D0       
      ALPPM6( 3, 3)=        4.714674D0       
      ALPPM6( 4, 1)=        2.475418D0       
      ALPPM6( 4, 2)=        3.306702D0       
      ALPPM6( 4, 3)=        2.236728D0       
      ALPPM6( 4, 4)=        1.499907D0       
      ALPPM6( 5, 1)=        2.615231D0       
      ALPPM6( 5, 2)=        3.16314D0        
      ALPPM6( 5, 3)=        3.759397D0       
      ALPPM6( 5, 4)=        1.888998D0       
      ALPPM6( 5, 5)=        3.318624D0       
      ALPPM6( 6, 1)=        1.027806D0      
      ALPPM6( 6, 2)=        3.042705D0       
      ALPPM6( 6, 3)=        3.241874D0       
      ALPPM6( 6, 4)=        4.212882D0       
      ALPPM6( 6, 5)=        2.919007D0       
      ALPPM6( 6, 6)=        2.613713D0  
      ALPPM6( 7, 1)=        0.969406D0  
      ALPPM6( 7, 2)=        2.814339D0  
      ALPPM6( 7, 3)=        2.640623D0  
      ALPPM6( 7, 4)=        2.580895D0  
      ALPPM6( 7, 5)=        2.477004D0  
      ALPPM6( 7, 6)=        2.686108D0  
      ALPPM6( 7, 7)=        2.574502D0  
      ALPPM6( 8, 1)=        1.260942D0  
      ALPPM6( 8, 2)=        3.653775D0  
      ALPPM6( 8, 3)=        2.584442D0  
      ALPPM6( 8, 4)=        3.051867D0  
      ALPPM6( 8, 5)=        2.695351D0  
      ALPPM6( 8, 6)=        2.889607D0  
      ALPPM6( 8, 7)=        2.784292D0  
      ALPPM6( 8, 8)=        2.623998D0  
      ALPPM6( 9, 1)=        3.13674D0   
      ALPPM6( 9, 2)=        2.856543D0  
      ALPPM6( 9, 3)=        3.043901D0  
      ALPPM6( 9, 4)=        3.726923D0  
      ALPPM6( 9, 5)=        2.823837D0  
      ALPPM6( 9, 6)=        3.0276D0    
      ALPPM6( 9, 7)=        2.856646D0  
      ALPPM6( 9, 8)=        3.015444D0  
      ALPPM6( 9, 9)=        3.175759D0  
      ALPPM6(10, 1)=        5.99968D0   
      ALPPM6(10, 2)=        3.677758D0  
      ALPPM6(10, 3)=        2.193666D0  
      ALPPM6(10, 4)=        1.316588D0  
      ALPPM6(10, 5)=        2.75619D0   
      ALPPM6(10, 6)=        3.441188D0  
      ALPPM6(10, 7)=        4.42637D0   
      ALPPM6(10, 8)=        2.889587D0  
      ALPPM6(10, 9)=        3.675611D0  
      ALPPM6(10,10)=        3.974567D0  
      ALPPM6(11, 1)=        0.500326D0  
      ALPPM6(11, 2)=        1.703029D0  
      ALPPM6(11, 3)=        1.267299D0  
      ALPPM6(11, 4)=        1.25548D0   
      ALPPM6(11, 5)=        1.569961D0  
      ALPPM6(11, 6)=        2.19605D0   
      ALPPM6(11, 7)=        2.494384D0  
      ALPPM6(11, 8)=        1.981449D0  
      ALPPM6(11, 9)=        2.619551D0  
      ALPPM6(11,10)=        1.774236D0  
      ALPPM6(11,11)=        0.446435D0  
      ALPPM6(12, 1)=        2.651594D0  
      ALPPM6(12, 2)=        2.210603D0  
      ALPPM6(12, 3)=        1.18438D0   
      ALPPM6(12, 4)=        1.557591D0  
      ALPPM6(12, 5)=        2.527441D0  
      ALPPM6(12, 6)=        3.040946D0  
      ALPPM6(12, 7)=        2.079125D0  
      ALPPM6(12, 8)=        2.25152D0   
      ALPPM6(12, 9)=        3.362208D0  
      ALPPM6(12,10)=        2.031676D0  
      ALPPM6(12,11)=        1.506773D0  
      ALPPM6(12,12)=        1.093573D0  
      ALPPM6(13, 1)=        2.025996D0  
      ALPPM6(13, 2)=        2.25583D0   
      ALPPM6(13, 3)=        1.581593D0  
      ALPPM6(13, 4)=        1.938237D0  
      ALPPM6(13, 5)=        2.059569D0  
      ALPPM6(13, 6)=        2.26744D0   
      ALPPM6(13, 7)=        2.009754D0  
      ALPPM6(13, 8)=        2.49866D0   
      ALPPM6(13, 9)=        3.084258D0  
      ALPPM6(13,10)=        2.447869D0  
      ALPPM6(13,11)=        1.202871D0  
      ALPPM6(13,12)=        1.97253D0   
      ALPPM6(13,13)=        1.387714D0  
      ALPPM6(14, 1)=        1.89695D0   
      ALPPM6(14, 2)=        2.040498D0  
      ALPPM6(14, 3)=        1.789609D0  
      ALPPM6(14, 4)=        1.263132D0  
      ALPPM6(14, 5)=        1.982653D0  
      ALPPM6(14, 6)=        1.984498D0  
      ALPPM6(14, 7)=        1.818988D0  
      ALPPM6(14, 8)=        1.9236D0    
      ALPPM6(14, 9)=        2.131028D0  
      ALPPM6(14,10)=        2.867784D0  
      ALPPM6(14,11)=        2.007615D0  
      ALPPM6(14,12)=        3.139749D0  
      ALPPM6(14,13)=        1.9D0       
      ALPPM6(14,14)=        1.329D0     
      ALPPM6(15, 1)=        1.926537D0  
      ALPPM6(15, 2)=        2.093158D0  
      ALPPM6(15, 3)=        1.394544D0  
      ALPPM6(15, 4)=        1.80007D0   
      ALPPM6(15, 5)=        1.923168D0  
      ALPPM6(15, 6)=        1.994653D0  
      ALPPM6(15, 7)=        2.147042D0  
      ALPPM6(15, 8)=        2.220768D0  
      ALPPM6(15, 9)=        2.234356D0  
      ALPPM6(15,10)=        2.219036D0  
      ALPPM6(15,11)=        1.50032D0   
      ALPPM6(15,12)=        1.383773D0  
      ALPPM6(15,13)=        1.980727D0  
      ALPPM6(15,14)=        3.313466D0  
      ALPPM6(15,15)=        1.505792D0  
      ALPPM6(16, 1)=        2.215975D0  
      ALPPM6(16, 2)=        1.959149D0  
      ALPPM6(16, 3)=        2.294275D0  
      ALPPM6(16, 4)=        2.781736D0  
      ALPPM6(16, 5)=        2.403696D0  
      ALPPM6(16, 6)=        2.210305D0  
      ALPPM6(16, 7)=        2.28999D0   
      ALPPM6(16, 8)=        2.383289D0  
      ALPPM6(16, 9)=        2.187186D0  
      ALPPM6(16,10)=        2.787058D0  
      ALPPM6(16,11)=        1.40085D0   
      ALPPM6(16,12)=        1.500163D0  
      ALPPM6(16,13)=        1.976705D0  
      ALPPM6(16,14)=        1.885916D0  
      ALPPM6(16,15)=        1.595325D0  
      ALPPM6(16,16)=        1.794556D0  
      ALPPM6(17, 1)=        2.402886D0  
      ALPPM6(17, 2)=        1.671677D0  
      ALPPM6(17, 3)=        2.783001D0  
      ALPPM6(17, 4)=        2.822676D0  
      ALPPM6(17, 5)=        2.259323D0  
      ALPPM6(17, 6)=        2.162197D0  
      ALPPM6(17, 7)=        2.172134D0  
      ALPPM6(17, 8)=        2.323236D0  
      ALPPM6(17, 9)=        2.31327D0   
      ALPPM6(17,10)=        1.703151D0  
      ALPPM6(17,11)=        1.816429D0  
      ALPPM6(17,12)=        2.391806D0  
      ALPPM6(17,13)=        2.125939D0  
      ALPPM6(17,14)=        1.684978D0  
      ALPPM6(17,15)=        1.468306D0  
      ALPPM6(17,16)=        1.715435D0  
      ALPPM6(17,17)=        1.823239D0  
      ALPPM6(18, 1)=        4.056167D0  
      ALPPM6(18, 2)=        2.716562D0  
      ALPPM6(18, 3)=        3.122895D0  
      ALPPM6(18, 4)=        3.044007D0  
      ALPPM6(18, 5)=        2.415471D0  
      ALPPM6(18, 6)=        1.471309D0  
      ALPPM6(18, 7)=        2.326805D0  
      ALPPM6(18, 8)=        2.240673D0  
      ALPPM6(18, 9)=        3.920658D0  
      ALPPM6(18,10)=        2.963747D0  
      ALPPM6(18,11)=        2.167677D0  
      ALPPM6(18,12)=        2.092664D0  
      ALPPM6(18,13)=        2.645165D0  
      ALPPM6(18,14)=        1.78035D0
      ALPPM6(18,15)=        4.372516D0  
      ALPPM6(18,16)=        2.049398D0  
      ALPPM6(18,17)=        2.554449D0  
      ALPPM6(18,18)=        2.306432D0  
      
       XPM6  ( 1, 1)=        2.243587D0  
       XPM6  ( 2, 1)=        2.371199D0  
       XPM6  ( 2, 2)=        3.4509D0    
       XPM6  ( 3, 1)=        2.191985D0  
       XPM6  ( 3, 2)=        9.273676D0  
       XPM6  ( 3, 3)=       16.116384D0  
       XPM6  ( 4, 1)=        2.562831D0  
       XPM6  ( 4, 2)=       12.544878D0  
       XPM6  ( 4, 3)=        3.287165D0  
       XPM6  ( 4, 4)=        0.238633D0  
       XPM6  ( 5, 1)=        1.321394D0  
       XPM6  ( 5, 2)=        1.97417D0   
       XPM6  ( 5, 3)=        7.886018D0  
       XPM6  ( 5, 4)=        1.151792D0  
       XPM6  ( 5, 5)=        3.593619D0  
       XPM6  ( 6, 1)=        0.216506D0  
       XPM6  ( 6, 2)=        3.213971D0  
       XPM6  ( 6, 3)=       16.180002D0  
       XPM6  ( 6, 4)=       25.035879D0  
       XPM6  ( 6, 5)=        1.874859D0  
       XPM6  ( 6, 6)=        0.81351D0   
       XPM6  ( 7, 1)=        0.175506D0  
       XPM6  ( 7, 2)=        1.077861D0  
       XPM6  ( 7, 3)=        2.823403D0  
       XPM6  ( 7, 4)=        1.740605D0  
       XPM6  ( 7, 5)=        0.952882D0  
       XPM6  ( 7, 6)=        0.859949D0  
       XPM6  ( 7, 7)=        0.675313D0  
       XPM6  ( 8, 1)=        0.192295D0  
       XPM6  ( 8, 2)=        6.684525D0  
       XPM6  ( 8, 3)=        1.968598D0  
       XPM6  ( 8, 4)=        3.218155D0  
       XPM6  ( 8, 5)=        1.269801D0  
       XPM6  ( 8, 6)=        0.990211D0  
       XPM6  ( 8, 7)=        0.764756D0  
       XPM6  ( 8, 8)=        0.535112D0  
       XPM6  ( 9, 1)=        0.815802D0  
       XPM6  ( 9, 2)=        0.745107D0  
       XPM6  ( 9, 3)=        1.975985D0  
       XPM6  ( 9, 4)=        3.882993D0  
       XPM6  ( 9, 5)=        0.862761D0  
       XPM6  ( 9, 6)=        0.732968D0  
       XPM6  ( 9, 7)=        0.635854D0  
       XPM6  ( 9, 8)=        0.674251D0  
       XPM6  ( 9, 9)=        0.681343D0  
       XPM6  (10, 1)=        5.535021D0  
       XPM6  (10, 2)=        1.960924D0  
       XPM6  (10, 3)=        0.704958D0  
       XPM6  (10, 4)=        0.392628D0  
       XPM6  (10, 5)=        2.76414D0   
       XPM6  (10, 6)=        5.46878D0   
       XPM6  (10, 7)=       29.999609D0  
       XPM6  (10, 8)=        0.763899D0  
       XPM6  (10, 9)=        2.706754D0  
       XPM6  (10,10)=        2.79483D0   
       XPM6  (11, 1)=        0.207831D0  
       XPM6  (11, 2)=        4.282517D0  
       XPM6  (11, 3)=        0.881482D0  
       XPM6  (11, 4)=        3.12162D0   
       XPM6  (11, 5)=        3.188608D0  
       XPM6  (11, 6)=        4.520429D0  
       XPM6  (11, 7)=        8.586387D0  
       XPM6  (11, 8)=        3.270079D0  
       XPM6  (11, 9)=        7.047351D0  
       XPM6  (11,10)=        1.343037D0  
       XPM6  (11,11)=        0.287137D0  
       XPM6  (12, 1)=        7.758237D0  
       XPM6  (12, 2)=        3.72585D0   
       XPM6  (12, 3)=        2.49025D0   
       XPM6  (12, 4)=        2.066392D0  
       XPM6  (12, 5)=        6.146701D0  
       XPM6  (12, 6)=       10.51769D0   
       XPM6  (12, 7)=        1.208075D0  
       XPM6  (12, 8)=        1.535734D0  
       XPM6  (12, 9)=        5.859023D0  
       XPM6  (12,10)=        1.214859D0  
       XPM6  (12,11)=        8.675619D0  
       XPM6  (12,12)=        0.465645D0  
       XPM6  (13, 1)=        2.958379D0  
       XPM6  (13, 2)=        2.7014D0    
       XPM6  (13, 3)=        1.106819D0  
       XPM6  (13, 4)=        5.037214D0  
       XPM6  (13, 5)=        2.741479D0  
       XPM6  (13, 6)=        2.928056D0  
       XPM6  (13, 7)=        1.345202D0  
       XPM6  (13, 8)=        2.131396D0  
       XPM6  (13, 9)=        1.975635D0  
       XPM6  (13,10)=        1.7092D0    
       XPM6  (13,11)=        2.071847D0  
       XPM6  (13,12)=       13.472443D0  
       XPM6  (13,13)=        2.1392D0    
       XPM6  (14, 1)=        0.924196D0  
       XPM6  (14, 2)=        1.853583D0  
       XPM6  (14, 3)=        3.090791D0  
       XPM6  (14, 4)=        0.623433D0  
       XPM6  (14, 5)=        1.028287D0  
       XPM6  (14, 6)=        0.785745D0  
       XPM6  (14, 7)=        0.592972D0  
       XPM6  (14, 8)=        0.751095D0  
       XPM6  (14, 9)=        0.543516D0  
       XPM6  (14,10)=       14.378676D0  
       XPM6  (14,11)=        9.237644D0  
       XPM6  (14,12)=       29.99452D0   
       XPM6  (14,13)=        2.D0        
       XPM6  (14,14)=        0.273477D0  
       XPM6  (15, 1)=        1.234986D0  
       XPM6  (15, 2)=        1.490218D0  
       XPM6  (15, 3)=        1.12295D0   
       XPM6  (15, 4)=        1.684831D0  
       XPM6  (15, 5)=        1.450886D0  
       XPM6  (15, 6)=        0.979512D0  
       XPM6  (15, 7)=        0.972154D0  
       XPM6  (15, 8)=        0.878705D0  
       XPM6  (15, 9)=        0.514575D0  
       XPM6  (15,10)=        0.774954D0  
       XPM6  (15,11)=        2.837095D0  
       XPM6  (15,12)=        1.177881D0  
       XPM6  (15,13)=        5.050816D0  
       XPM6  (15,14)=       13.239121D0  
       XPM6  (15,15)=        0.902501D0  
       XPM6  (16, 1)=        0.849712D0  
       XPM6  (16, 2)=        0.437618D0  
       XPM6  (16, 3)=        2.642502D0  
       XPM6  (16, 4)=        3.791565D0  
       XPM6  (16, 5)=        1.125394D0  
       XPM6  (16, 6)=        0.666849D0  
       XPM6  (16, 7)=        0.73871D0   
       XPM6  (16, 8)=        0.747215D0  
       XPM6  (16, 9)=        0.375251D0  
       XPM6  (16,10)=        3.29616D0   
       XPM6  (16,11)=        0.852434D0  
       XPM6  (16,12)=        0.500748D0  
       XPM6  (16,13)=        2.347384D0  
       XPM6  (16,14)=        0.876658D0  
       XPM6  (16,15)=        0.562266D0  
       XPM6  (16,16)=        0.473856D0  
       XPM6  (17, 1)=        0.754831D0  
       XPM6  (17, 2)=        0.272964D0  
       XPM6  (17, 3)=        4.227794D0  
       XPM6  (17, 4)=        2.507275D0  
       XPM6  (17, 5)=        0.822129D0  
       XPM6  (17, 6)=        0.515787D0  
       XPM6  (17, 7)=        0.520745D0  
       XPM6  (17, 8)=        0.58551D0   
       XPM6  (17, 9)=        0.411124D0  
       XPM6  (17,10)=        0.125133D0  
       XPM6  (17,11)=        1.357894D0  
       XPM6  (17,12)=        2.430856D0  
       XPM6  (17,13)=        2.153451D0  
       XPM6  (17,14)=        0.513D0     
       XPM6  (17,15)=        0.352361D0  
       XPM6  (17,16)=        0.356971D0  
       XPM6  (17,17)=        0.332919D0  
       XPM6  (18, 1)=        3.933445D0  
       XPM6  (18, 2)=        1.177211D0  
       XPM6  (18, 3)=        3.36291D0   
       XPM6  (18, 4)=        2.755492D0  
       XPM6  (18, 5)=        1.931586D0  
       XPM6  (18, 6)=        0.122309D0  
       XPM6  (18, 7)=        0.562581D0  
       XPM6  (18, 8)=        0.355795D0  
       XPM6  (18, 9)=        9.269715D0  
       XPM6  (18,10)=        1.304697D0  
       XPM6  (18,11)=        3.398138D0  
       XPM6  (18,12)=        1.970638D0  
       XPM6  (18,13)=        1.852009D0  
       XPM6  (18,14)=        1.06789D0   
       XPM6  (18,15)=        0.171014D0  
       XPM6  (18,16)=        0.653769D0  
       XPM6  (18,17)=        2.256094D0  
       XPM6  (18,18)=        0.972699D0  





*     START OF ATOMIC FIRST IONIZATION POTENTIALS

      ATOMIP( 1)=       13.598443D0  
      ATOMIP( 2)=       24.587387D0  
      ATOMIP( 3)=        5.391719D0  
      ATOMIP( 4)=        9.32270 D0  
      ATOMIP( 5)=        8.29802 D0  
      ATOMIP( 6)=       11.26030 D0  
      ATOMIP( 7)=       14.5341  D0  
      ATOMIP( 8)=       13.61805 D0  
      ATOMIP( 9)=       17.4228  D0  
      ATOMIP(10)=       21.56454 D0  
      ATOMIP(11)=        5.139076D0  
      ATOMIP(12)=        7.646235D0  
      ATOMIP(13)=        5.985768D0  
      ATOMIP(14)=        8.15168 D0  
      ATOMIP(15)=       10.48669 D0  
      ATOMIP(16)=       10.36001 D0  
      ATOMIP(17)=       12.96763 D0  
      ATOMIP(18)=       15.759610D0  
      ATOMIP(19)=        4.3406633D0 
      ATOMIP(20)=        6.11316 D0  
      ATOMIP(21)=        6.56149 D0  
      ATOMIP(22)=        6.82812 D0  
      ATOMIP(23)=        6.74619 D0  
      ATOMIP(24)=        6.76651 D0  
      ATOMIP(25)=        7.43402 D0  
      ATOMIP(26)=        7.9024  D0  
      ATOMIP(27)=        7.88101 D0  
      ATOMIP(28)=        7.6398  D0  
      ATOMIP(29)=        7.72638 D0  
      ATOMIP(30)=        9.394199D0  
      ATOMIP(31)=        5.999301D0  
      ATOMIP(32)=        7.89943 D0  
      ATOMIP(33)=        9.7886  D0  
      ATOMIP(34)=        9.75239 D0  
      ATOMIP(35)=       11.8138  D0  
      ATOMIP(36)=       13.99961 D0  
      ATOMIP(37)=        4.177128D0  
      ATOMIP(38)=        5.69485 D0  
      ATOMIP(39)=        6.2173  D0  
      ATOMIP(40)=        6.63390 D0  
      ATOMIP(41)=        6.75885 D0  
      ATOMIP(42)=        7.09243 D0  
      ATOMIP(43)=        7.28    D0  
      ATOMIP(44)=        7.36050 D0  
      ATOMIP(45)=        7.45890 D0  
      ATOMIP(46)=        8.3369  D0  
      ATOMIP(47)=        7.57623 D0  
      ATOMIP(48)=        8.99382 D0  
      ATOMIP(49)=        5.78636 D0  
      ATOMIP(50)=        7.34392 D0  
      ATOMIP(51)=        8.60839 D0  
      ATOMIP(52)=        9.0096  D0  
      ATOMIP(53)=       10.45126 D0  
      ATOMIP(54)=       12.12984 D0  
      ATOMIP(55)=        3.893905D0  
      ATOMIP(56)=        5.211664D0  
      ATOMIP(57)=        5.5769  D0  
      ATOMIP(58)=        5.5387  D0  
      ATOMIP(59)=        5.473   D0  
      ATOMIP(60)=        5.5250  D0  
      ATOMIP(61)=        5.582   D0  
      ATOMIP(62)=        5.6437  D0  
      ATOMIP(63)=        5.67038 D0  
      ATOMIP(64)=        6.14980 D0  
      ATOMIP(65)=        5.8638  D0  
      ATOMIP(66)=        5.9389  D0  
      ATOMIP(67)=        6.0215  D0  
      ATOMIP(68)=        6.1077  D0  
      ATOMIP(69)=        6.18431 D0  
      ATOMIP(70)=        6.25416 D0  
      ATOMIP(71)=        5.42586 D0  
      ATOMIP(72)=        6.82507 D0  
      ATOMIP(73)=        7.54957 D0  
      ATOMIP(74)=        7.86403 D0  
      ATOMIP(75)=        7.83352 D0  
      ATOMIP(76)=        8.43823 D0  
      ATOMIP(77)=        8.96702 D0  
      ATOMIP(78)=        8.9588  D0  
      ATOMIP(79)=        9.22553 D0  
      ATOMIP(80)=       10.4375  D0  
      ATOMIP(81)=        6.108194D0  
      ATOMIP(82)=        7.41663 D0  
      ATOMIP(83)=        7.2855  D0  
      ATOMIP(84)=        8.414   D0  
      ATOMIP(86)=       10.7485  D0  
      ATOMIP(87)=        4.072741D0  
      ATOMIP(88)=        5.278423D0  
      ATOMIP(89)=        5.17    D0  
      ATOMIP(90)=        6.3067  D0  
      ATOMIP(91)=        5.89    D0  
      ATOMIP(92)=        6.1941  D0  
      ATOMIP(93)=        6.2657  D0  
      ATOMIP(94)=        6.0260  D0  
      ATOMIP(95)=        5.9738  D0  
      ATOMIP(96)=        5.9914  D0  
      ATOMIP(97)=        6.1979  D0  
      ATOMIP(98)=        6.2817  D0  
      ATOMIP(99)=        6.42    D0  
      ATOMIP(100)=        6.50    D0  
      ATOMIP(101)=        6.58    D0  
      ATOMIP(102)=        6.65    D0  
      ATOMIP(103)=        4.9     D0  
      ATOMIP(104)=        6.0     D0  
      ATOMIP(108)=       13.598443D0  

*     START OF PMOv1 PARAMETERS                                          LF1010

      REFPMOV1(108)=' Hp: (PMOv1):  Zhang, Fiedler, Leverentz, Truhlar, 
     &Gao, JCTC, 7, 857 (2011).'
      REFPMOV1(  8)='  O: (PMOv1):  Zhang, Fiedler, Leverentz, Truhlar, 
     &Gao, JCTC, 7, 857 (2011).'

      USSPMOV1   (8)=  -114.78169D0 
      UPPPMOV1   (8)=   -78.04828D0 
      ZSPMOV1    (8)=     3.19623D0 
      ZPPMOV1    (8)=     3.11976D0 
      BETASPMOV1 (8)=   -31.5177D0  
      BETAPPMOV1 (8)=   -35.10436D0 
      GSSPMOV1   (8)=    18.22143D0
      GSPPMOV1   (8)=    12.73220D0  
      GPPPMOV1   (8)=    15.03924D0
      GP2PMOV1   (8)=    13.52768D0 
      HSPPMOV1   (8)=    4.19786D0  
      ALPHAPMOV1 (8)=    3.44202D0  
      AMPMOV1    (8)=    0.6696322D0 
      ADPMOV1    (8)=    1.0756814D0 
      AQPMOV1    (8)=    1.0439748D0 
      DDPMOV1    (8)=    0.4568870D0 
      QQPMOV1    (8)=    0.3925766D0 
      EISOLPMOV1 (8)= -358.0586100D0 
      USSPMOV1  (108)=   -11.22813D0  
      UPPPMOV1  (108)=    -9.95254D0  
      ZSPMOV1   (108)=     1.08419D0  
      ZPPMOV1   (108)=     0.88997D0  
      BETASPMOV1(108)=    -6.89857D0  
      BETAPPMOV1(108)=    -3.77765D0  
      GSSPMOV1  (108)=    12.65697D0  
      GSPPMOV1  (108)=    11.34825D0  
      GPPPMOV1  (108)=     6.17416D0  
      GP2PMOV1  (108)=    10.04410D0  
      HSPPMOV1  (108)=     2.3256D0   
      ALPHAPMOV1(108)=     3.16046D0  
      AMPMOV1   (108)=     0.4651400D0 
      ADPMOV1   (108)=     0.5680773D0
      AQPMOV1   (108)=     0.2392509D0
      DDPMOV1   (108)=     0.9001912D0 
      QQPMOV1   (108)=     1.3761642D0 
      EISOLPMOV1(108)=   -11.2281300D0 

*     START OF PMO2 PARAMETERS                                           LF0113

      REFPMO2 (  6)='   C: (PMO2): Isegawa, Fiedler, Leverentz, Wang, et
     & al., JCTC, 9, 33 (2013).'
      REFPMO2 (  8)='   O: (PMO2): Isegawa, Fiedler, Leverentz, Wang, et
     & al., JCTC, 9, 33 (2013).'
      REFPMO2 (108)='  Hp: (PMO2): Isegawa, Fiedler, Leverentz, Wang, et
     & al., JCTC, 9, 33 (2013).'

      RPNUMPMO2      =       8.000D0 
      RPDENPMO2      =       2.000D0 
      DPDENPMO2      =       2.000D0 
      SPDENPMO2      =       4.000D0 
      S6PMO2         =       1.400D0 
      ALPDPMO2       =      23.000D0 
      USSPMO2  (108) =     -10.589D0  
      UPPPMO2  (108) =      -7.235D0  
      ZSPMO2   (108) =       1.189D0  
      ZPPMO2   (108) =       0.878D0  
      GSSPMO2  (108) =      12.376D0  
      GSPPMO2  (108) =       8.160D0  
      GPPPMO2  (108) =       7.364D0  
      GP2PMO2  (108) =      11.346D0  
      HSPPMO2  (108) =       1.882D0  
      ALPHAPMO2(108) =       3.615D0  
      AMPMO2   (108) =    0.4548143955959149D0 
      ADPMO2   (108) =    0.5543733050460202D0 
      AQPMO2   (108) =    0.2369012584161755D0 
      DDPMO2   (108) =    0.7942489820338561D0 
      QQPMO2   (108) =    1.3949258216305112D0 
      EISOLPMO2(108) =     -10.58900000000D0 
      C6PMO2   (108) =       0.32000D0 
      RVDWPMO2 (108) =       1.11000D0 
      USSPMO2  (  6) =     -48.566D0  
      UPPPMO2  (  6) =     -40.922D0  
      ZSPMO2   (  6) =       1.998D0  
      ZPPMO2   (  6) =       1.637D0  
      GSSPMO2  (  6) =      11.795D0  
      GSPPMO2  (  6) =      11.207D0  
      GPPPMO2  (  6) =      12.541D0  
      GP2PMO2  (  6) =       9.870D0  
      HSPPMO2  (  6) =       1.944D0  
      ALPHAPMO2(  6) =       3.014D0  
      AMPMO2   (  6) =    0.4334628148071927D0 
      ADPMO2   (  6) =    0.5701996085172113D0 
      AQPMO2   (  6) =    0.8247703886618970D0 
      DDPMO2   (  6) =    0.7747173324559324D0 
      QQPMO2   (  6) =    0.7481642464212517D0 
      EISOLPMO2(  6) =    -117.70650000000D0 
      C6PMO2   (  6) =       0.92000D0 
      RVDWPMO2 (  6) =       1.61000D0 
      USSPMO2  (  8) =     -91.714D0  
      UPPPMO2  (  8) =     -72.331D0  
      ZSPMO2   (  8) =       2.833D0  
      ZPPMO2   (  8) =       3.468D0  
      GSSPMO2  (  8) =      21.437D0  
      GSPPMO2  (  8) =      12.017D0  
      GPPPMO2  (  8) =      12.166D0  
      GP2PMO2  (  8) =      12.686D0  
      HSPPMO2  (  8) =       4.396D0  
      ALPHAPMO2(  8) =       4.548D0  
      AMPMO2   (  8) =    0.7878035066572099D0 
      ADPMO2   (  8) =    1.1121102402179852D0 
      AQPMO2   (  8) =    0.6606565534640932D0 
      DDPMO2   (  8) =    0.4465978581709464D0 
      QQPMO2   (  8) =    0.3531559606088780D0 
      EISOLPMO2(  8) =    -296.38700000000D0 
      C6PMO2   (  8) =       2.60000D0 
      RVDWPMO2 (  8) =       1.48000D0 
      PRBETAPMO2 (108,108,1,1) =   -6.098000D0  
      PRBETAPMO2 (108,108,1,2) =    0.000000D0  
      PRBETAPMO2 (108,108,2,1) =    0.000000D0  
      PRBETAPMO2 (108,108,2,2) =   -0.760000D0  
      PRBETAPMO2 (  6,108,1,1) =  -13.704000D0  
      PRBETAPMO2 (108,  6,1,1) =  -13.704000D0  
      PRBETAPMO2 (  6,108,1,2) =   -0.282000D0  
      PRBETAPMO2 (108,  6,2,1) =   -0.282000D0  
      PRBETAPMO2 (  6,  6,1,1) =  -13.168000D0  
      PRBETAPMO2 (108,  6,1,2) =   -6.701000D0  
      PRBETAPMO2 (  6,108,2,1) =   -6.701000D0  
      PRBETAPMO2 (  6,108,2,2) =   -0.762000D0  
      PRBETAPMO2 (108,  6,2,2) =   -0.762000D0  
      PRBETAPMO2 (  6,  6,1,2) =  -14.640000D0  
      PRBETAPMO2 (  6,  6,2,1) =  -14.640000D0  
      PRBETAPMO2 (  6,  6,2,2) =   -6.851000D0  
      PRBETAPMO2 (  8,108,1,1) =  -14.923000D0  
      PRBETAPMO2 (108,  8,1,1) =  -14.923000D0  
      PRBETAPMO2 (  8,108,1,2) =   -0.347000D0  
      PRBETAPMO2 (108,  8,2,1) =   -0.347000D0  
      PRBETAPMO2 (  8,  6,1,1) =  -27.678000D0  
      PRBETAPMO2 (  6,  8,1,1) =  -27.678000D0  
      PRBETAPMO2 (  8,  6,1,2) =  -17.943000D0  
      PRBETAPMO2 (  6,  8,2,1) =  -17.943000D0  
      PRBETAPMO2 (  8,  8,1,1) =  -20.932000D0  
      PRBETAPMO2 (108,  8,1,2) =  -25.614000D0  
      PRBETAPMO2 (  8,108,2,1) =  -25.614000D0  
      PRBETAPMO2 (  8,108,2,2) =   -1.639000D0  
      PRBETAPMO2 (108,  8,2,2) =   -1.639000D0  
      PRBETAPMO2 (  6,  8,1,2) =  -39.434000D0  
      PRBETAPMO2 (  8,  6,2,1) =  -39.434000D0  
      PRBETAPMO2 (  8,  6,2,2) =  -26.349000D0  
      PRBETAPMO2 (  6,  8,2,2) =  -26.349000D0  
      PRBETAPMO2 (  8,  8,1,2) =  -18.304000D0  
      PRBETAPMO2 (  8,  8,2,1) =  -18.304000D0  
      PRBETAPMO2 (  8,  8,2,2) =  -37.915000D0  
      KPAIRPMO2 (108,108,1,1) =   -0.247000D0  
      KPAIRPMO2 (108,108,1,2) =    0.754000D0  
      KPAIRPMO2 (108,108,2,1) =    0.754000D0  
      KPAIRPMO2 (108,108,2,2) =    0.132000D0  
      KPAIRPMO2 (  6,108,1,1) =   -0.325000D0  
      KPAIRPMO2 (108,  6,1,1) =   -0.325000D0  
      KPAIRPMO2 (  6,108,1,2) =    0.389000D0  
      KPAIRPMO2 (108,  6,2,1) =    0.389000D0  
      KPAIRPMO2 (  6,  6,1,1) =   -0.318000D0  
      KPAIRPMO2 (108,  6,1,2) =    0.059000D0  
      KPAIRPMO2 (  6,108,2,1) =    0.059000D0  
      KPAIRPMO2 (  6,108,2,2) =    0.021000D0  
      KPAIRPMO2 (108,  6,2,2) =    0.021000D0  
      KPAIRPMO2 (  6,  6,1,2) =   -0.125000D0  
      KPAIRPMO2 (  6,  6,2,1) =   -0.125000D0  
      KPAIRPMO2 (  6,  6,2,2) =    0.100000D0  
      KPAIRPMO2 (  8,108,1,1) =   -0.147000D0  
      KPAIRPMO2 (108,  8,1,1) =   -0.147000D0  
      KPAIRPMO2 (  8,108,1,2) =    0.252000D0  
      KPAIRPMO2 (108,  8,2,1) =    0.252000D0  
      KPAIRPMO2 (  8,  6,1,1) =   -0.260000D0  
      KPAIRPMO2 (  6,  8,1,1) =   -0.260000D0  
      KPAIRPMO2 (  8,  6,1,2) =   -0.066000D0  
      KPAIRPMO2 (  6,  8,2,1) =   -0.066000D0  
      KPAIRPMO2 (  8,  8,1,1) =   -0.913000D0  
      KPAIRPMO2 (108,  8,1,2) =   -0.013000D0  
      KPAIRPMO2 (  8,108,2,1) =   -0.013000D0  
      KPAIRPMO2 (  8,108,2,2) =    0.308000D0  
      KPAIRPMO2 (108,  8,2,2) =    0.308000D0  
      KPAIRPMO2 (  6,  8,1,2) =   -0.157000D0  
      KPAIRPMO2 (  8,  6,2,1) =   -0.157000D0  
      KPAIRPMO2 (  8,  6,2,2) =    0.036000D0  
      KPAIRPMO2 (  6,  8,2,2) =    0.036000D0  
      KPAIRPMO2 (  8,  8,1,2) =    0.871000D0  
      KPAIRPMO2 (  8,  8,2,1) =    0.871000D0  
      KPAIRPMO2 (  8,  8,2,2) =   -0.907000D0  
      C0ABPMO2 (108,108)   =    0.353000D0  
      C0ABPMO2 (108,  6)   =    0.538000D0  
      C0ABPMO2 (  6,108)   =    0.538000D0  
      C0ABPMO2 (108,  8)   =    0.573000D0  
      C0ABPMO2 (  8,108)   =    0.573000D0  
      C0ABPMO2 (  6,  6)   =    0.811000D0  
      C0ABPMO2 (  6,  8)   =    0.530000D0  
      C0ABPMO2 (  8,  6)   =    0.530000D0  
      C0ABPMO2 (  8,  8)   =    0.558000D0  
      C1ABPMO2 (108,108)   =    0.388000D0  
      C1ABPMO2 (108,  6)   =    0.173000D0  
      C1ABPMO2 (  6,108)   =    0.562000D0  
      C1ABPMO2 (108,  8)   =    0.663000D0  
      C1ABPMO2 (  8,108)   =    0.477000D0  
      C1ABPMO2 (  6,  6)   =    0.297000D0  
      C1ABPMO2 (  6,  8)   =    0.695000D0  
      C1ABPMO2 (  8,  6)   =    0.799000D0  
      C1ABPMO2 (  8,  8)   =    0.338000D0  
      C2ABPMO2 (108,108)   =    0.388000D0  
      C2ABPMO2 (108,  6)   =    0.562000D0  
      C2ABPMO2 (  6,108)   =    0.173000D0  
      C2ABPMO2 (108,  8)   =    0.477000D0  
      C2ABPMO2 (  8,108)   =    0.663000D0  
      C2ABPMO2 (  6,  6)   =    0.297000D0  
      C2ABPMO2 (  6,  8)   =    0.799000D0  
      C2ABPMO2 (  8,  6)   =    0.695000D0  
      C2ABPMO2 (  8,  8)   =    0.338000D0  
      PRALPPMO2 (108,108)   =    2.494000D0  
      PRALPPMO2 (108,  6)   =    2.179000D0  
      PRALPPMO2 (  6,108)   =    2.179000D0  
      PRALPPMO2 (108,  8)   =    2.970000D0  
      PRALPPMO2 (  8,108)   =    2.970000D0  
      PRALPPMO2 (  6,  6)   =    3.038000D0  
      PRALPPMO2 (  6,  8)   =    2.571000D0  
      PRALPPMO2 (  8,  6)   =    2.571000D0  
      PRALPPMO2 (  8,  8)   =    3.984000D0  
      C3ABPMO2 (108,108)   =    0.349000D0  
      C3ABPMO2 (108,  6)   =    0.530000D0  
      C3ABPMO2 (  6,108)   =    0.530000D0  
      C3ABPMO2 (108,  8)   =    0.645000D0  
      C3ABPMO2 (  8,108)   =    0.645000D0  
      C3ABPMO2 (  6,  6)   =    0.629000D0  
      C3ABPMO2 (  6,  8)   =    0.350000D0  
      C3ABPMO2 (  8,  6)   =    0.350000D0  
      C3ABPMO2 (  8,  8)   =    0.621000D0  
      C3RPWRPMO2 (108,108)   =    0.000000D0  
      C3RPWRPMO2 (108,  6)   =    0.000000D0  
      C3RPWRPMO2 (  6,108)   =    0.000000D0  
      C3RPWRPMO2 (108,  8)   =    1.000000D0  
      C3RPWRPMO2 (  8,108)   =    1.000000D0  
      C3RPWRPMO2 (  6,  6)   =    0.000000D0  
      C3RPWRPMO2 (  6,  8)   =    0.000000D0  
      C3RPWRPMO2 (  8,  6)   =    0.000000D0  
      C3RPWRPMO2 (  8,  8)   =    0.000000D0  
      PR3ALPPMO2 (108,108)   =    3.993000D0  
      PR3ALPPMO2 (108,  6)   =    2.474000D0  
      PR3ALPPMO2 (  6,108)   =    2.474000D0  
      PR3ALPPMO2 (108,  8)   =    3.071000D0  
      PR3ALPPMO2 (  8,108)   =    3.071000D0  
      PR3ALPPMO2 (  6,  6)   =    2.083000D0  
      PR3ALPPMO2 (  6,  8)   =    2.895000D0  
      PR3ALPPMO2 (  8,  6)   =    2.895000D0  
      PR3ALPPMO2 (  8,  8)   =    5.438000D0  
      CPAIRPMO2 (108,108)   =    0.200000D0  
      CPAIRPMO2 (108,  6)   =    0.291713D0  
      CPAIRPMO2 (  6,108)   =    0.291713D0  
      CPAIRPMO2 (108,  8)   =    0.260465D0  
      CPAIRPMO2 (  8,108)   =    0.260465D0  
      CPAIRPMO2 (  6,  6)   =    1.650000D0  
      CPAIRPMO2 (  6,  8)   =    0.982979D0  
      CPAIRPMO2 (  8,  6)   =    0.982979D0  
      CPAIRPMO2 (  8,  8)   =    0.700000D0  
      DPAIRPMO2 (108,108)   =    1.270000D0  
      DPAIRPMO2 (108,  6)   =    1.770000D0  
      DPAIRPMO2 (  6,108)   =    1.770000D0  
      DPAIRPMO2 (108,  8)   =    1.650000D0  
      DPAIRPMO2 (  8,108)   =    1.650000D0  
      DPAIRPMO2 (  6,  6)   =    3.260000D0  
      DPAIRPMO2 (  6,  8)   =    3.140000D0  
      DPAIRPMO2 (  8,  6)   =    3.140000D0  
      DPAIRPMO2 (  8,  8)   =    2.190000D0  

*     Grimme's D3 dispersion references
*     Reference:   J. Chem. Phys. 132, 154104 (2010)

      REFD3 ( 1) ='  H: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, '//
     &                                                 '154104 (2010)'
      REFD3 ( 6) ='  C: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, '//
     &                                                 '154104 (2010)'
      REFD3 ( 7) ='  N: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, '//
     &                                                 '154104 (2010)'
      REFD3 ( 8) ='  O: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, '//
     &                                                 '154104 (2010)'
      REFD3 (15) ='  P: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, '//
     &                                                 '154104 (2010)'
      REFD3 (16) ='  S: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, '//
     &                                                 '154104 (2010)'
      REFD3 (17) =' Cl: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, '//
     &                                                 '154104 (2010)'

*     START OF PMO2A PARAMETERS                                         LF0614

      REFPMO2A (  6)='   C: (PMO2a): Fiedler, Leverentz, Nachimuthu, et 
     &al., JCTC, In press (2014).'
      REFPMO2A (  7)='   N: (PMO2a): Fiedler, Leverentz, Nachimuthu, et 
     &al., JCTC, In press (2014).'
      REFPMO2A (  8)='   O: (PMO2a): Fiedler, Leverentz, Nachimuthu, et 
     &al., JCTC, In press (2014).'
      REFPMO2A ( 16)='   S: (PMO2a): Fiedler, Leverentz, Nachimuthu, et 
     &al., JCTC, In press (2014).'
      REFPMO2A (108)='  Hp: (PMO2a): Fiedler, Leverentz, Nachimuthu, et 
     &al., JCTC, In press (2014).'

      RPNUMPMO2A     =       8.000D0 
      RPDENPMO2A     =       2.000D0 
      DPDENPMO2A     =       2.000D0 
      SPDENPMO2A     =       4.000D0 
      S6PMO2A        =       1.400D0 
      ALPDPMO2A      =      23.000D0 

      USSPMO2A  (108) =     -10.58900000D0
      UPPPMO2A  (108) =      -7.23500000D0
      ZSPMO2A   (108) =       1.18900000D0
      ZPPMO2A   (108) =       0.87800000D0
      GSSPMO2A  (108) =      12.37600000D0
      GSPPMO2A  (108) =       8.16000000D0
      GPPPMO2A  (108) =       7.36400000D0
      GP2PMO2A  (108) =      11.34600000D0
      HSPPMO2A  (108) =       1.88200000D0
      ALPHAPMO2A(108) =       3.61500000D0

      AMPMO2A   (108) =    0.4548144D0
      ADPMO2A   (108) =    0.5543733D0
      AQPMO2A   (108) =    0.2369013D0
      DDPMO2A   (108) =    0.7942490D0
      QQPMO2A   (108) =    1.3949258D0

!      AMPMO2A   (108) =    0.4548144D0
!      ADPMO2A   (108) =    1.0404448D0
!      AQPMO2A   (108) =    0.6508466D0
!      DDPMO2A   (108) =    0.2761383D0
!      QQPMO2A   (108) =    0.3601683D0
!      AMPMO2A   (108) =    0.4721603D0
!      ADPMO2A   (108) =    0.4721603D0
!      AQPMO2A   (108) =    0.4721603D0
!      DDPMO2A   (108) =    0.0000000D0
!      QQPMO2A   (108) =    0.0000000D0
      EISOLPMO2A(108) =  -10.5890000D0
      C6PMO2A   (108) =    0.3200000D0
      RVDWPMO2A (108) =    1.110000D0

      USSPMO2A  (  6) =     -48.56600000D0
      UPPPMO2A  (  6) =     -40.92200000D0
      ZSPMO2A   (  6) =       1.99800000D0
      ZPPMO2A   (  6) =       1.63700000D0
      GSSPMO2A  (  6) =      11.79500000D0
      GSPPMO2A  (  6) =      11.20700000D0
      GPPPMO2A  (  6) =      12.54100000D0
      GP2PMO2A  (  6) =       9.87000000D0
      HSPPMO2A  (  6) =       1.94400000D0
      ALPHAPMO2A(  6) =       3.01400000D0
      AMPMO2A   (  6) =    0.4334628D0
      ADPMO2A   (  6) =    0.5701996D0
      AQPMO2A   (  6) =    0.8247704D0
      DDPMO2A   (  6) =    0.7747173D0
      QQPMO2A   (  6) =    0.7481642D0
      EISOLPMO2A(  6) = -117.7065000D0
      C6PMO2A   (  6) =    0.920000D0
      RVDWPMO2A (  6) =    1.610000D0

      USSPMO2A  (  7) =     -78.44083866D0
      UPPPMO2A  (  7) =     -56.33446978D0
      ZSPMO2A   (  7) =       2.39588030D0
      ZPPMO2A   (  7) =       2.60888778D0
      GSSPMO2A  (  7) =      14.56053342D0
      GSPPMO2A  (  7) =       9.69651068D0
      GPPPMO2A  (  7) =      12.76047594D0
      GP2PMO2A  (  7) =      12.86954664D0
      HSPPMO2A  (  7) =       3.34838262D0
      ALPHAPMO2A(  7) =       3.14896860D0
      AMPMO2A   (  7) =    0.5350954D0
      ADPMO2A   (  7) =    0.8568614D0
      AQPMO2A   (  7) =    0.5323549D0
      DDPMO2A   (  7) =    0.5741917D0
      QQPMO2A   (  7) =    0.4694510D0
      EISOLPMO2A(  7) = -224.4183910D0
      C6PMO2A   (  7) =    1.1100000D0
      RVDWPMO2A (  7) =    1.5500000D0

      USSPMO2A  (  8) =     -91.65196181D0
      UPPPMO2A  (  8) =     -72.33067440D0
      ZSPMO2A   (  8) =       2.83757193D0
      ZPPMO2A   (  8) =       3.46800663D0
      GSSPMO2A  (  8) =      21.34991937D0
      GSPPMO2A  (  8) =      12.01724024D0
      GPPPMO2A  (  8) =      12.17005477D0
      GP2PMO2A  (  8) =      12.68607389D0
      HSPPMO2A  (  8) =       4.41146676D0
      ALPHAPMO2A(  8) =       4.55953057D0
      AMPMO2A   (  8) =    0.7846033D0
      ADPMO2A   (  8) =    1.1140740D0
      AQPMO2A   (  8) =    0.6606575D0
      DDPMO2A   (  8) =    0.4464540D0
      QQPMO2A   (  8) =    0.3531553D0
      EISOLPMO2A(  8) = -296.4101941D0
      C6PMO2A   (  8) =    2.6000000D0
      RVDWPMO2A (  8) =    1.4800000D0

      USSPMO2A  ( 16) =     -68.46462932D0
      UPPPMO2A  ( 16) =     -54.06071032D0
      ZSPMO2A   ( 16) =       2.30068472D0
      ZPPMO2A   ( 16) =       1.97750018D0
      GSSPMO2A  ( 16) =      11.62004067D0
      GSPPMO2A  ( 16) =      11.65607793D0
      GPPPMO2A  ( 16) =      10.03446162D0
      GP2PMO2A  ( 16) =       8.86706338D0
      HSPPMO2A  ( 16) =       2.22088236D0
      ALPHAPMO2A( 16) =       2.45273565D0
      AMPMO2A   ( 16) =    0.4270331D0
      ADPMO2A   ( 16) =    0.5477972D0
      AQPMO2A   ( 16) =    0.5684654D0
      DDPMO2A   ( 16) =    0.9259312D0
      QQPMO2A   ( 16) =    0.8461795D0
      EISOLPMO2A( 16) = -204.5682841D0
      C6PMO2A   ( 16) =   10.3000000D0
      RVDWPMO2A ( 16) =    1.8700000D0

      PRBETAPMO2A (108, 108, 1, 1) =     -6.09800000D0
      PRBETAPMO2A (108, 108, 1, 2) =      0.00000000D0
      PRBETAPMO2A (108, 108, 2, 1) =      0.00000000D0
      PRBETAPMO2A (108, 108, 2, 2) =     -0.76000000D0
      PRBETAPMO2A (6  , 108, 1, 1) =    -13.70400000D0
      PRBETAPMO2A (108, 6  , 1, 1) =    -13.70400000D0
      PRBETAPMO2A (6  , 108, 1, 2) =     -0.28200000D0
      PRBETAPMO2A (108, 6  , 2, 1) =     -0.28200000D0
      PRBETAPMO2A (6  , 6  , 1, 1) =    -13.16800000D0
      PRBETAPMO2A (108, 6  , 1, 2) =     -6.70100000D0
      PRBETAPMO2A (6  , 108, 2, 1) =     -6.70100000D0
      PRBETAPMO2A (6  , 108, 2, 2) =     -0.76200000D0
      PRBETAPMO2A (108, 6  , 2, 2) =     -0.76200000D0
      PRBETAPMO2A (6  , 6  , 1, 2) =    -14.64000000D0
      PRBETAPMO2A (6  , 6  , 2, 1) =    -14.64000000D0
      PRBETAPMO2A (6  , 6  , 2, 2) =     -6.85100000D0
      PRBETAPMO2A (7  , 108, 1, 1) =    -13.37385143D0
      PRBETAPMO2A (108, 7  , 1, 1) =    -13.37385143D0
      PRBETAPMO2A (7  , 108, 1, 2) =     -2.06049076D0
      PRBETAPMO2A (108, 7  , 2, 1) =     -2.06049076D0
      PRBETAPMO2A (7  , 6  , 1, 1) =    -21.60837468D0
      PRBETAPMO2A (6  , 7  , 1, 1) =    -21.60837468D0
      PRBETAPMO2A (7  , 6  , 1, 2) =    -18.23458235D0
      PRBETAPMO2A (6  , 7  , 2, 1) =    -18.23458235D0
      PRBETAPMO2A (7  , 7  , 1, 1) =     -7.61147280D0
      PRBETAPMO2A (108, 7  , 1, 2) =    -14.24165312D0
      PRBETAPMO2A (7  , 108, 2, 1) =    -14.24165312D0
      PRBETAPMO2A (7  , 108, 2, 2) =     -2.42535576D0
      PRBETAPMO2A (108, 7  , 2, 2) =     -2.42535576D0
      PRBETAPMO2A (6  , 7  , 1, 2) =    -15.06535770D0
      PRBETAPMO2A (7  , 6  , 2, 1) =    -15.06535770D0
      PRBETAPMO2A (7  , 6  , 2, 2) =    -13.44473587D0
      PRBETAPMO2A (6  , 7  , 2, 2) =    -13.44473587D0
      PRBETAPMO2A (7  , 7  , 1, 2) =    -27.36524884D0
      PRBETAPMO2A (7  , 7  , 2, 1) =    -27.36524884D0
      PRBETAPMO2A (7  , 7  , 2, 2) =    -32.15811573D0
      PRBETAPMO2A (8  , 108, 1, 1) =    -15.54656605D0
      PRBETAPMO2A (108, 8  , 1, 1) =    -15.54656605D0
      PRBETAPMO2A (8  , 108, 1, 2) =     -0.34884280D0
      PRBETAPMO2A (108, 8  , 2, 1) =     -0.34884280D0
      PRBETAPMO2A (8  , 6  , 1, 1) =    -27.67800000D0
      PRBETAPMO2A (6  , 8  , 1, 1) =    -27.67800000D0
      PRBETAPMO2A (8  , 6  , 1, 2) =    -17.94300000D0
      PRBETAPMO2A (6  , 8  , 2, 1) =    -17.94300000D0
      PRBETAPMO2A (8  , 7  , 1, 1) =    -25.97732642D0
      PRBETAPMO2A (7  , 8  , 1, 1) =    -25.97732642D0
      PRBETAPMO2A (8  , 7  , 1, 2) =     -5.74288965D0
      PRBETAPMO2A (7  , 8  , 2, 1) =     -5.74288965D0
      PRBETAPMO2A (8  , 8  , 1, 1) =    -20.93200000D0
      PRBETAPMO2A (108, 8  , 1, 2) =    -24.84173761D0
      PRBETAPMO2A (8  , 108, 2, 1) =    -24.84173761D0
      PRBETAPMO2A (8  , 108, 2, 2) =     -1.78875071D0
      PRBETAPMO2A (108, 8  , 2, 2) =     -1.78875071D0
      PRBETAPMO2A (6  , 8  , 1, 2) =    -39.43400000D0
      PRBETAPMO2A (8  , 6  , 2, 1) =    -39.43400000D0
      PRBETAPMO2A (8  , 6  , 2, 2) =    -26.34900000D0
      PRBETAPMO2A (6  , 8  , 2, 2) =    -26.34900000D0
      PRBETAPMO2A (7  , 8  , 1, 2) =    -34.88117656D0
      PRBETAPMO2A (8  , 7  , 2, 1) =    -34.88117656D0
      PRBETAPMO2A (8  , 7  , 2, 2) =    -34.79981512D0
      PRBETAPMO2A (7  , 8  , 2, 2) =    -34.79981512D0
      PRBETAPMO2A (8  , 8  , 1, 2) =    -18.30400000D0
      PRBETAPMO2A (8  , 8  , 2, 1) =    -18.30400000D0
      PRBETAPMO2A (8  , 8  , 2, 2) =    -37.91500000D0
      PRBETAPMO2A (16 , 108, 1, 1) =     -8.87536700D0
      PRBETAPMO2A (108, 16 , 1, 1) =     -8.87536700D0
      PRBETAPMO2A (16 , 108, 1, 2) =      0.00000000D0
      PRBETAPMO2A (108, 16 , 2, 1) =      0.00000000D0
      PRBETAPMO2A (16 , 6  , 1, 1) =    -14.87335700D0
      PRBETAPMO2A (6  , 16 , 1, 1) =    -14.87335700D0
      PRBETAPMO2A (16 , 6  , 1, 2) =     -9.34789600D0
      PRBETAPMO2A (6  , 16 , 2, 1) =     -9.34789600D0
      PRBETAPMO2A (16 , 7  , 1, 1) =    -15.62871400D0
      PRBETAPMO2A (7  , 16 , 1, 1) =    -15.62871400D0
      PRBETAPMO2A (16 , 7  , 1, 2) =    -15.62871400D0
      PRBETAPMO2A (7  , 16 , 2, 1) =    -15.62871400D0
      PRBETAPMO2A (16 , 8  , 1, 1) =    -23.03583102D0
      PRBETAPMO2A (8  , 16 , 1, 1) =    -23.03583102D0
      PRBETAPMO2A (16 , 8  , 1, 2) =    -21.26910128D0
      PRBETAPMO2A (8  , 16 , 2, 1) =    -21.26910128D0
      PRBETAPMO2A (16 , 16 , 1, 1) =    -10.76167000D0
      PRBETAPMO2A (108, 16 , 1, 2) =     -8.54874850D0
      PRBETAPMO2A (16 , 108, 2, 1) =     -8.54874850D0
      PRBETAPMO2A (16 , 108, 2, 2) =      0.00000000D0
      PRBETAPMO2A (108, 16 , 2, 2) =      0.00000000D0
      PRBETAPMO2A (6  , 16 , 1, 2) =    -14.54673850D0
      PRBETAPMO2A (16 , 6  , 2, 1) =    -14.54673850D0
      PRBETAPMO2A (16 , 6  , 2, 2) =     -9.02127750D0
      PRBETAPMO2A (6  , 16 , 2, 2) =     -9.02127750D0
      PRBETAPMO2A (7  , 16 , 1, 2) =    -15.30209550D0
      PRBETAPMO2A (16 , 7  , 2, 1) =    -15.30209550D0
      PRBETAPMO2A (16 , 7  , 2, 2) =    -15.30209550D0
      PRBETAPMO2A (7  , 16 , 2, 2) =    -15.30209550D0
      PRBETAPMO2A (8  , 16 , 1, 2) =    -21.19321606D0
      PRBETAPMO2A (16 , 8  , 2, 1) =    -21.19321606D0
      PRBETAPMO2A (16 , 8  , 2, 2) =    -21.68267220D0
      PRBETAPMO2A (8  , 16 , 2, 2) =    -21.68267220D0
      PRBETAPMO2A (16 , 16 , 1, 2) =    -10.43505150D0
      PRBETAPMO2A (16 , 16 , 2, 1) =    -10.43505150D0
      PRBETAPMO2A (16 , 16 , 2, 2) =    -10.10843300D0

      KPAIRPMO2A (108, 108, 1, 1) =     -0.24700000D0
      KPAIRPMO2A (108, 108, 1, 2) =      0.75400000D0
      KPAIRPMO2A (108, 108, 2, 1) =      0.75400000D0
      KPAIRPMO2A (108, 108, 2, 2) =      0.13200000D0
      KPAIRPMO2A (6  , 108, 1, 1) =     -0.32500000D0
      KPAIRPMO2A (108, 6  , 1, 1) =     -0.32500000D0
      KPAIRPMO2A (6  , 108, 1, 2) =      0.38900000D0
      KPAIRPMO2A (108, 6  , 2, 1) =      0.38900000D0
      KPAIRPMO2A (6  , 6  , 1, 1) =     -0.31800000D0
      KPAIRPMO2A (108, 6  , 1, 2) =      0.05900000D0
      KPAIRPMO2A (6  , 108, 2, 1) =      0.05900000D0
      KPAIRPMO2A (6  , 108, 2, 2) =      0.02100000D0
      KPAIRPMO2A (108, 6  , 2, 2) =      0.02100000D0
      KPAIRPMO2A (6  , 6  , 1, 2) =     -0.12500000D0
      KPAIRPMO2A (6  , 6  , 2, 1) =     -0.12500000D0
      KPAIRPMO2A (6  , 6  , 2, 2) =      0.10000000D0
      KPAIRPMO2A (7  , 108, 1, 1) =      0.00000000D0
      KPAIRPMO2A (108, 7  , 1, 1) =      0.00000000D0
      KPAIRPMO2A (7  , 108, 1, 2) =      0.00000000D0
      KPAIRPMO2A (108, 7  , 2, 1) =      0.00000000D0
      KPAIRPMO2A (7  , 6  , 1, 1) =      0.00000000D0
      KPAIRPMO2A (6  , 7  , 1, 1) =      0.00000000D0
      KPAIRPMO2A (7  , 6  , 1, 2) =      0.00000000D0
      KPAIRPMO2A (6  , 7  , 2, 1) =      0.00000000D0
      KPAIRPMO2A (7  , 7  , 1, 1) =      0.00000000D0
      KPAIRPMO2A (108, 7  , 1, 2) =      0.00000000D0
      KPAIRPMO2A (7  , 108, 2, 1) =      0.00000000D0
      KPAIRPMO2A (7  , 108, 2, 2) =      0.00000000D0
      KPAIRPMO2A (108, 7  , 2, 2) =      0.00000000D0
      KPAIRPMO2A (6  , 7  , 1, 2) =      0.00000000D0
      KPAIRPMO2A (7  , 6  , 2, 1) =      0.00000000D0
      KPAIRPMO2A (7  , 6  , 2, 2) =      0.00000000D0
      KPAIRPMO2A (6  , 7  , 2, 2) =      0.00000000D0
      KPAIRPMO2A (7  , 7  , 1, 2) =      0.00000000D0
      KPAIRPMO2A (7  , 7  , 2, 1) =      0.00000000D0
      KPAIRPMO2A (7  , 7  , 2, 2) =      0.00000000D0
      KPAIRPMO2A (8  , 108, 1, 1) =     -0.14700000D0
      KPAIRPMO2A (108, 8  , 1, 1) =     -0.14700000D0
      KPAIRPMO2A (8  , 108, 1, 2) =      0.25200000D0
      KPAIRPMO2A (108, 8  , 2, 1) =      0.25200000D0
      KPAIRPMO2A (8  , 6  , 1, 1) =     -0.26000000D0
      KPAIRPMO2A (6  , 8  , 1, 1) =     -0.26000000D0
      KPAIRPMO2A (8  , 6  , 1, 2) =     -0.06600000D0
      KPAIRPMO2A (6  , 8  , 2, 1) =     -0.06600000D0
      KPAIRPMO2A (8  , 7  , 1, 1) =      0.00000000D0
      KPAIRPMO2A (7  , 8  , 1, 1) =      0.00000000D0
      KPAIRPMO2A (8  , 7  , 1, 2) =      0.00000000D0
      KPAIRPMO2A (7  , 8  , 2, 1) =      0.00000000D0
      KPAIRPMO2A (8  , 8  , 1, 1) =     -0.91300000D0
      KPAIRPMO2A (108, 8  , 1, 2) =     -0.01300000D0
      KPAIRPMO2A (8  , 108, 2, 1) =     -0.01300000D0
      KPAIRPMO2A (8  , 108, 2, 2) =      0.30800000D0
      KPAIRPMO2A (108, 8  , 2, 2) =      0.30800000D0
      KPAIRPMO2A (6  , 8  , 1, 2) =     -0.15700000D0
      KPAIRPMO2A (8  , 6  , 2, 1) =     -0.15700000D0
      KPAIRPMO2A (8  , 6  , 2, 2) =      0.03600000D0
      KPAIRPMO2A (6  , 8  , 2, 2) =      0.03600000D0
      KPAIRPMO2A (7  , 8  , 1, 2) =      0.00000000D0
      KPAIRPMO2A (8  , 7  , 2, 1) =      0.00000000D0
      KPAIRPMO2A (8  , 7  , 2, 2) =      0.00000000D0
      KPAIRPMO2A (7  , 8  , 2, 2) =      0.00000000D0
      KPAIRPMO2A (8  , 8  , 1, 2) =      0.87100000D0
      KPAIRPMO2A (8  , 8  , 2, 1) =      0.87100000D0
      KPAIRPMO2A (8  , 8  , 2, 2) =     -0.90700000D0
      KPAIRPMO2A (16 , 108, 1, 1) =      0.00000000D0
      KPAIRPMO2A (108, 16 , 1, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 108, 1, 2) =      0.00000000D0
      KPAIRPMO2A (108, 16 , 2, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 6  , 1, 1) =      0.00000000D0
      KPAIRPMO2A (6  , 16 , 1, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 6  , 1, 2) =      0.00000000D0
      KPAIRPMO2A (6  , 16 , 2, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 7  , 1, 1) =      0.00000000D0
      KPAIRPMO2A (7  , 16 , 1, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 7  , 1, 2) =      0.00000000D0
      KPAIRPMO2A (7  , 16 , 2, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 8  , 1, 1) =      0.00000000D0
      KPAIRPMO2A (8  , 16 , 1, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 8  , 1, 2) =      0.00000000D0
      KPAIRPMO2A (8  , 16 , 2, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 16 , 1, 1) =      0.00000000D0
      KPAIRPMO2A (108, 16 , 1, 2) =      0.00000000D0
      KPAIRPMO2A (16 , 108, 2, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 108, 2, 2) =      0.00000000D0
      KPAIRPMO2A (108, 16 , 2, 2) =      0.00000000D0
      KPAIRPMO2A (6  , 16 , 1, 2) =      0.00000000D0
      KPAIRPMO2A (16 , 6  , 2, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 6  , 2, 2) =      0.00000000D0
      KPAIRPMO2A (6  , 16 , 2, 2) =      0.00000000D0
      KPAIRPMO2A (7  , 16 , 1, 2) =      0.00000000D0
      KPAIRPMO2A (16 , 7  , 2, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 7  , 2, 2) =      0.00000000D0
      KPAIRPMO2A (7  , 16 , 2, 2) =      0.00000000D0
      KPAIRPMO2A (8  , 16 , 1, 2) =      0.00000000D0
      KPAIRPMO2A (16 , 8  , 2, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 8  , 2, 2) =      0.00000000D0
      KPAIRPMO2A (8  , 16 , 2, 2) =      0.00000000D0
      KPAIRPMO2A (16 , 16 , 1, 2) =      0.00000000D0
      KPAIRPMO2A (16 , 16 , 2, 1) =      0.00000000D0
      KPAIRPMO2A (16 , 16 , 2, 2) =      0.00000000D0

      C0ABPMO2A (108,108) =        0.35300000D0 
      C0ABPMO2A (108,  6) =        0.53800000D0 
      C0ABPMO2A (  6,108) =        0.53800000D0 
      C0ABPMO2A (108,  7) =        1.09085904D0 
      C0ABPMO2A (  7,108) =        1.09085904D0 
      C0ABPMO2A (108,  8) =        0.57431362D0 
      C0ABPMO2A (  8,108) =        0.57431362D0 
      C0ABPMO2A (108, 16) =        0.00000000D0 
      C0ABPMO2A ( 16,108) =        0.00000000D0 
      C0ABPMO2A (  6,  6) =        0.81100000D0 
      C0ABPMO2A (  6,  7) =        0.00031158D0 
      C0ABPMO2A (  7,  6) =        0.00031158D0 
      C0ABPMO2A (  6,  8) =        0.53000000D0 
      C0ABPMO2A (  8,  6) =        0.53000000D0 
      C0ABPMO2A (  6, 16) =        0.00000000D0 
      C0ABPMO2A ( 16,  6) =        0.00000000D0 
      C0ABPMO2A (  7,  7) =        0.00029053D0 
      C0ABPMO2A (  7,  8) =        0.00025541D0 
      C0ABPMO2A (  8,  7) =        0.00025541D0 
      C0ABPMO2A (  7, 16) =        0.00000000D0 
      C0ABPMO2A ( 16,  7) =        0.00000000D0 
      C0ABPMO2A (  8,  8) =        0.55800000D0 
      C0ABPMO2A (  8, 16) =        0.01509525D0 
      C0ABPMO2A ( 16,  8) =        0.01509525D0 
      C0ABPMO2A ( 16, 16) =        0.00000000D0 
      
      C1ABPMO2A (108,108) =        0.38800000D0 
      C1ABPMO2A (108,  6) =        0.17300000D0 
      C2ABPMO2A (  6,108) =        0.17300000D0 
      C1ABPMO2A (108,  7) =        0.00025507D0 
      C2ABPMO2A (  7,108) =        0.00025507D0 
      C1ABPMO2A (108,  8) =        0.66569748D0 
      C2ABPMO2A (  8,108) =        0.66569748D0 
      C1ABPMO2A (108, 16) =        1.00000000D0 
      C2ABPMO2A ( 16,108) =        1.00000000D0 
      C1ABPMO2A (  6,  6) =        0.29700000D0 
      C1ABPMO2A (  6,  7) =        1.46715309D0 
      C2ABPMO2A (  7,  6) =        1.46715309D0 
      C1ABPMO2A (  6,  8) =        0.69500000D0 
      C2ABPMO2A (  8,  6) =        0.69500000D0 
      C1ABPMO2A (  6, 16) =        1.00000000D0 
      C2ABPMO2A ( 16,  6) =        1.00000000D0 
      C1ABPMO2A (  7,  7) =        1.76879051D0 
      C1ABPMO2A (  7,  8) =        1.20708881D0 
      C2ABPMO2A (  8,  7) =        1.20708881D0 
      C1ABPMO2A (  7, 16) =        1.00000000D0 
      C2ABPMO2A ( 16,  7) =        1.00000000D0 
      C1ABPMO2A (  8,  8) =        0.33800000D0 
      C1ABPMO2A (  8, 16) =        1.05160738D0 
      C2ABPMO2A ( 16,  8) =        1.05160738D0 
      C1ABPMO2A ( 16, 16) =        1.00000000D0 
      
      C2ABPMO2A (108,108) =        0.38800000D0 
      C2ABPMO2A (108,  6) =        0.56200000D0 
      C1ABPMO2A (  6,108) =        0.56200000D0 
      C2ABPMO2A (108,  7) =        0.00031046D0 
      C1ABPMO2A (  7,108) =        0.00031046D0 
      C2ABPMO2A (108,  8) =        0.47680421D0 
      C1ABPMO2A (  8,108) =        0.47680421D0 
      C2ABPMO2A (108, 16) =        1.00000000D0 
      C1ABPMO2A ( 16,108) =        1.00000000D0 
      C2ABPMO2A (  6,  6) =        0.29700000D0 
      C2ABPMO2A (  6,  7) =        0.88553336D0 
      C1ABPMO2A (  7,  6) =        0.88553336D0 
      C2ABPMO2A (  6,  8) =        0.79900000D0 
      C1ABPMO2A (  8,  6) =        0.79900000D0 
      C2ABPMO2A (  6, 16) =        1.00000000D0 
      C1ABPMO2A ( 16,  6) =        1.00000000D0 
      C2ABPMO2A (  7,  7) =        1.76879051D0 
      C2ABPMO2A (  7,  8) =        1.94411846D0 
      C1ABPMO2A (  8,  7) =        1.94411846D0 
      C2ABPMO2A (  7, 16) =        1.00000000D0 
      C1ABPMO2A ( 16,  7) =        1.00000000D0 
      C2ABPMO2A (  8,  8) =        0.33800000D0 
      C2ABPMO2A (  8, 16) =        0.99360658D0 
      C1ABPMO2A ( 16,  8) =        0.99360658D0 
      C2ABPMO2A ( 16, 16) =        1.00000000D0 
      
      PRALPPMO2A(108,108) =        2.49400000D0 
      PRALPPMO2A(108,  6) =        2.17900000D0 
      PRALPPMO2A(  6,108) =        2.17900000D0 
      PRALPPMO2A(108,  7) =        2.96905295D0 
      PRALPPMO2A(  7,108) =        2.96905295D0 
      PRALPPMO2A(108,  8) =        2.97106615D0 
      PRALPPMO2A(  8,108) =        2.97106615D0 
      PRALPPMO2A(108, 16) =        0.00000000D0 
      PRALPPMO2A( 16,108) =        0.00000000D0 
      PRALPPMO2A(  6,  6) =        3.03800000D0 
      PRALPPMO2A(  6,  7) =        0.00031661D0 
      PRALPPMO2A(  7,  6) =        0.00031661D0 
      PRALPPMO2A(  6,  8) =        2.57100000D0 
      PRALPPMO2A(  8,  6) =        2.57100000D0 
      PRALPPMO2A(  6, 16) =        0.00000000D0 
      PRALPPMO2A( 16,  6) =        0.00000000D0 
      PRALPPMO2A(  7,  7) =        0.00022846D0 
      PRALPPMO2A(  7,  8) =        0.00032903D0 
      PRALPPMO2A(  8,  7) =        0.00032903D0 
      PRALPPMO2A(  7, 16) =        0.00000000D0 
      PRALPPMO2A( 16,  7) =        0.00000000D0 
      PRALPPMO2A(  8,  8) =        3.98400000D0 
      PRALPPMO2A(  8, 16) =        2.25223657D0 
      PRALPPMO2A( 16,  8) =        2.25223657D0 
      PRALPPMO2A( 16, 16) =        0.00000000D0 
      
      C3ABPMO2A (108,108) =        0.34900000D0 
      C3ABPMO2A (108,  6) =        0.53000000D0 
      C3ABPMO2A (  6,108) =        0.53000000D0 
      C3ABPMO2A (108,  7) =        0.82829263D0 
      C3ABPMO2A (  7,108) =        0.82829263D0 
      C3ABPMO2A (108,  8) =        0.64501418D0 
      C3ABPMO2A (  8,108) =        0.64501418D0 
      C3ABPMO2A (108, 16) =        0.00000000D0 
      C3ABPMO2A ( 16,108) =        0.00000000D0 
      C3ABPMO2A (  6,  6) =        0.62900000D0 
      C3ABPMO2A (  6,  7) =        0.00033606D0 
      C3ABPMO2A (  7,  6) =        0.00033606D0 
      C3ABPMO2A (  6,  8) =        0.35000000D0 
      C3ABPMO2A (  8,  6) =        0.35000000D0 
      C3ABPMO2A (  6, 16) =        0.00000000D0 
      C3ABPMO2A ( 16,  6) =        0.00000000D0 
      C3ABPMO2A (  7,  7) =        0.00026814D0 
      C3ABPMO2A (  7,  8) =        0.00032077D0 
      C3ABPMO2A (  8,  7) =        0.00032077D0 
      C3ABPMO2A (  7, 16) =        0.00000000D0 
      C3ABPMO2A ( 16,  7) =        0.00000000D0 
      C3ABPMO2A (  8,  8) =        0.62100000D0 
      C3ABPMO2A (  8, 16) =        0.06220832D0 
      C3ABPMO2A ( 16,  8) =        0.06220832D0 
      C3ABPMO2A ( 16, 16) =        0.00000000D0 
      
      C3RPWRPMO2A(108,108) =        0.00000000D0 
      C3RPWRPMO2A(108,  6) =        0.00000000D0 
      C3RPWRPMO2A(  6,108) =        0.00000000D0 
      C3RPWRPMO2A(108,  7) =        1.00852441D0 
      C3RPWRPMO2A(  7,108) =        1.00852441D0 
      C3RPWRPMO2A(108,  8) =        1.00069979D0 
      C3RPWRPMO2A(  8,108) =        1.00069979D0 
      C3RPWRPMO2A(108, 16) =        0.00000000D0 
      C3RPWRPMO2A( 16,108) =        0.00000000D0 
      C3RPWRPMO2A(  6,  6) =        0.00000000D0 
      C3RPWRPMO2A(  6,  7) =        1.15631052D0 
      C3RPWRPMO2A(  7,  6) =        1.15631052D0 
      C3RPWRPMO2A(  6,  8) =        0.00000000D0 
      C3RPWRPMO2A(  8,  6) =        0.00000000D0 
      C3RPWRPMO2A(  6, 16) =        0.00000000D0 
      C3RPWRPMO2A( 16,  6) =        0.00000000D0 
      C3RPWRPMO2A(  7,  7) =        0.00000000D0 
      C3RPWRPMO2A(  7,  8) =        0.00000000D0 
      C3RPWRPMO2A(  8,  7) =        0.00000000D0 
      C3RPWRPMO2A(  7, 16) =        0.00000000D0 
      C3RPWRPMO2A( 16,  7) =        0.00000000D0 
      C3RPWRPMO2A(  8,  8) =        0.00000000D0 
      C3RPWRPMO2A(  8, 16) =       -2.01216983D0 
      C3RPWRPMO2A( 16,  8) =       -2.01216983D0 
      C3RPWRPMO2A( 16, 16) =        0.00000000D0 
      
      PR3ALPPMO2A(108,108) =        3.99300000D0
      PR3ALPPMO2A(108,  6) =        2.47400000D0
      PR3ALPPMO2A(  6,108) =        2.47400000D0
      PR3ALPPMO2A(108,  7) =        2.80818225D0
      PR3ALPPMO2A(  7,108) =        2.80818225D0
      PR3ALPPMO2A(108,  8) =        3.07245155D0
      PR3ALPPMO2A(  8,108) =        3.07245155D0
      PR3ALPPMO2A(108, 16) =        0.00000000D0
      PR3ALPPMO2A( 16,108) =        0.00000000D0
      PR3ALPPMO2A(  6,  6) =        2.08300000D0
      PR3ALPPMO2A(  6,  7) =        1.71869316D0
      PR3ALPPMO2A(  7,  6) =        1.71869316D0
      PR3ALPPMO2A(  6,  8) =        2.89500000D0
      PR3ALPPMO2A(  8,  6) =        2.89500000D0
      PR3ALPPMO2A(  6, 16) =        0.00000000D0
      PR3ALPPMO2A( 16,  6) =        0.00000000D0
      PR3ALPPMO2A(  7,  7) =        0.00000000D0
      PR3ALPPMO2A(  7,  8) =        0.00000000D0
      PR3ALPPMO2A(  8,  7) =        0.00000000D0
      PR3ALPPMO2A(  7, 16) =        0.00000000D0
      PR3ALPPMO2A( 16,  7) =        0.00000000D0
      PR3ALPPMO2A(  8,  8) =        5.43800000D0
      PR3ALPPMO2A(  8, 16) =        1.90729059D0
      PR3ALPPMO2A( 16,  8) =        1.90729059D0
      PR3ALPPMO2A( 16, 16) =        0.00000000D0

      CPAIRPMO2A (108,108) =   0.200000D0
      CPAIRPMO2A (  6,108) =   0.291713D0
      CPAIRPMO2A (  7,108) =   0.279685D0
      CPAIRPMO2A (  8,108) =   0.260465D0
      CPAIRPMO2A ( 16,108) =   0.315105D0
      CPAIRPMO2A (  6,  6) =   1.650000D0
      CPAIRPMO2A (  6,  7) =   1.327174D0
      CPAIRPMO2A (  6,  8) =   0.982979D0
      CPAIRPMO2A (  6, 16) =   2.844351D0
      CPAIRPMO2A (  7,  7) =   1.110000D0
      CPAIRPMO2A (  7,  8) =   0.858564D0
      CPAIRPMO2A (  7, 16) =   2.004032D0
      CPAIRPMO2A (  8,  8) =   0.700000D0
      CPAIRPMO2A (  8, 16) =   1.310909D0
      CPAIRPMO2A ( 16, 16) =  10.300000D0

      DPAIRPMO2A (108,108) =   1.270000D0
      DPAIRPMO2A (  6,108) =   1.770000D0
      DPAIRPMO2A (  7,108) =   1.710000D0
      DPAIRPMO2A (  8,108) =   1.650000D0
      DPAIRPMO2A ( 16,108) =   2.030000D0
      DPAIRPMO2A (  6,  6) =   3.260000D0
      DPAIRPMO2A (  6,  7) =   3.200000D0
      DPAIRPMO2A (  6,  8) =   3.140000D0
      DPAIRPMO2A (  6, 16) =   3.520000D0
      DPAIRPMO2A (  7,  7) =   2.660000D0
      DPAIRPMO2A (  7,  8) =   2.600000D0
      DPAIRPMO2A (  7, 16) =   2.980000D0
      DPAIRPMO2A (  8,  8) =   2.190000D0
      DPAIRPMO2A (  8, 16) =   2.570000D0
      DPAIRPMO2A ( 16, 16) =  12.170000D0

      RETURN
      END
