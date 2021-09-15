      BLOCK DATA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /NATORB/ NATORB(107)
      COMMON /ELEMTS/ ELEMNT(107)
***********************************************************************
*
*     COMMON BLOCKS FOR AM1
*
***********************************************************************
     +       /ALPHA / ALP(107)
     1       /CORE  / CORE(107)
     2       /MULTIP/ DD(107),QQ(107),AM(107),AD(107),AQ(107)
     3       /EXPONT/ ZS(107),ZP(107),ZD(107)
     4       /ONELEC/ USS(107),UPP(107),UDD(107)
     5       /BETAS / BETAS(107),BETAP(107),BETAD(107)
     6       /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107),
     +                GSD(107),GPD(107),GDD(107)
     7       /ATOMIC/ EISOL(107),EHEAT(107)
     8       /AM1REF/ AM1REF(107)
     9       /VSIPS / VS(107),VP(107),VD(107)
     A       /ISTOPE/ AMS(107)
     B       /IDEAS / GUESS1(107,10),GUESS2(107,10),GUESS3(107,10)
     C       /GAUSS / FN1(107),FN2(107)
***********************************************************************
*
*     COMMON BLOCKS FOR MNDO
*
***********************************************************************
      COMMON /MNDO/  USSM(107), UPPM(107), UDDM(107), ZSM(107),
     1ZPM(107), ZDM(107), BETASM(107), BETAPM(107), BETADM(107),
     2ALPM(107), EISOLM(107), DDM(107), QQM(107), AMM(107), ADM(107),
     3AQM(107) ,GSSM(107), GSPM(107), GPPM(107), GP2M(107), HSPM(107),
     4POLVOM(107)
*
*  COMMON BLOCKS FOR MINDO/3
*
      COMMON /ONELE3 /  USS3(18),UPP3(18)
     1       /TWOEL3 /  F03(107)
     2       /ATOMI3 /  EISOL3(18),EHEAT3(18)
     3       /BETA3  /  BETA3(153)
     4       /ALPHA3 /  ALP3(153)
     5       /EXPON3 /  ZS3(18),ZP3(18)
*
*  END OF MINDO/3 COMMON BLOCKS
*
      CHARACTER*2 ELEMNT
      DATA ELEMNT/' H','He',
     1 'Li','Be',' B',' C',' N',' O',' F','Ne',
     2 'Na','Mg','Al','Si',' P',' S','Cl','Ar',
     3 ' K','Ca','Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu',
     4 'Zn','Ga','Ge','As','Se','Br','Kr',
     5 'Rb','Sr',' Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
     6 'Cd','In','Sn','Sb','Te',' I','Xe',
     7 'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy',
     8 'Ho','Er','Tm','Yb','Lu','Hf','Ta',' W','Re','Os','Ir','Pt',
     9 'Au','Hg','Tl','Pb','Bi','Po','At','Rn',
     1 'Fr','Ra','Ac','Th','Pa',' U','Np','Pu','Am','Cm','Bk','Cf','XX',
     2 'Fm','Md','No','++',' +','--',' -','Tv'/
C
C   NATORB IS THE NUMBER OF ATOMIC ORBITALS PER ATOM.
C
      DATA NATORB/2*1, 0, 7*4, 0, 7*4, 0, 4, 9*9, 7*4,
     12*4, 9*9, 7*4, 2*2, 14*8, 9*9, 7*4, 21*0/
***********************************************************************
*                      VALENCE SHELLS ARE DEFINED AS                  *
*  PQN   VALENCE SHELLS                                               *
*                 P-GROUP              F-GROUP    TRANSITION METALS   *
*   1       1S                                                        *
*   2       2S 2P                                                     *
*   3       3S 3P  OR  3S 3P 3D                                       *
*   4       4S 4P                                    4S 4P 3D         *
*   5       5S 5P                                    5S 5P 4D         *
*   6       6S 6P                       6S 4F        6S 6P 5D         *
*   7  NOT ASSIGNED YET  ****DO  NOT  USE****                         *
***********************************************************************
      DATA      POLVOM(1) /0.2287D0/
      DATA      POLVOM(6) /0.2647D0/
      DATA      POLVOM(7) /0.3584D0/
      DATA      POLVOM(8) /0.2324D0/
      DATA      POLVOM(9) /0.1982D0/
      DATA      POLVOM(17)/1.3236D0/
      DATA      POLVOM(35)/2.2583D0/
      DATA      POLVOM(53)/4.0930D0/
C
C                STANDARD ATOMIC MASSES
C
      DATA  AMS /  1.00790D0,  4.00260D0,  6.94000D0,  9.01218D0,
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
     524*0.000D0/
C
C   CORE IS THE CHARGE ON THE ATOM AS SEEN BY THE ELECTRONS
C
      DATA CORE/1.D0,0.D0,
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
     2  16*0.D0,2.D0,1.D0,-2.D0,-1.D0,0.D0/
C
C     ENTHALPIES OF FORMATION OF GASEOUS ATOMS ARE TAKEN FROM \ANNUAL
C     REPORTS,1974,71B,P 117\  THERE ARE SOME SIGNIFICANT DIFFERENCES
C     BETWEEN THE VALUES REPORTED THERE AND THE VALUES PREVIOUSLY IN
C     THE BLOCK DATA OF THIS PROGRAM.  ONLY THE THIRD  ROW ELEMENTS
C     HAVE BEEN UPDATED.
C
* ALL THE OTHER ELEMENTS ARE TAKEN FROM CRC HANDBOOK 1981-1982
      DATA EHEAT(1)  / 52.102D0/
      DATA EHEAT(2)  /  0.000D0/
C
C#      DATA EHEAT(3)  / 38.600D0/
      DATA EHEAT(4)  / 76.960D0/
      DATA EHEAT(5)  /135.700D0/
      DATA EHEAT(6)  /170.890D0/
      DATA EHEAT(7)  /113.000D0/
      DATA EHEAT(8)  / 59.559D0/
      DATA EHEAT(9)  / 18.890D0/
      DATA EHEAT(10) /  0.000D0/
C
C#      DATA EHEAT(11) / 25.850D0/
      DATA EHEAT(12) / 35.000D0/
      DATA EHEAT(13) / 79.490D0/
      DATA EHEAT(14) /108.390D0/
      DATA EHEAT(15) / 75.570D0/
      DATA EHEAT(16) / 66.400D0/
      DATA EHEAT(17) / 28.990D0/
      DATA EHEAT(18) /  0.000D0/
C
C#      DATA EHEAT(19) / 21.420D0/
      DATA EHEAT(20) / 42.600D0/
      DATA EHEAT(21) / 90.300D0/
      DATA EHEAT(22) /112.300D0/
      DATA EHEAT(23) /122.900D0/
      DATA EHEAT(24) / 95.000D0/
      DATA EHEAT(25) / 67.700D0/
      DATA EHEAT(26) / 99.300D0/
      DATA EHEAT(27) /102.400D0/
      DATA EHEAT(28) /102.800D0/
      DATA EHEAT(29) / 80.700D0/
      DATA EHEAT(30) / 31.170D0/
      DATA EHEAT(31) / 65.400D0/
      DATA EHEAT(32) / 89.500D0/
      DATA EHEAT(33) / 72.300D0/
      DATA EHEAT(34) / 54.300D0/
      DATA EHEAT(35) / 26.740D0/
      DATA EHEAT(36) /  0.000D0/
C
      DATA EHEAT(37) / 19.600D0/
      DATA EHEAT(38) / 39.100D0/
      DATA EHEAT(39) /101.500D0/
      DATA EHEAT(40) /145.500D0/
      DATA EHEAT(41) /172.400D0/
      DATA EHEAT(42) /157.300D0/
      DATA EHEAT(44) /155.500D0/
      DATA EHEAT(45) /133.000D0/
      DATA EHEAT(46) / 90.000D0/
      DATA EHEAT(47) / 68.100D0/
      DATA EHEAT(48) / 26.720D0/
      DATA EHEAT(49) / 58.000D0/
      DATA EHEAT(50) / 72.200D0/
      DATA EHEAT(51) / 63.200D0/
      DATA EHEAT(52) / 47.000D0/
      DATA EHEAT(53) / 25.517D0/
      DATA EHEAT(54) /  0.000D0/
C
      DATA EHEAT(55) / 18.700D0/
      DATA EHEAT(56) / 42.500D0/
      DATA EHEAT(58) /101.300D0/
      DATA EHEAT(62) / 49.400D0/
      DATA EHEAT(68) / 75.800D0/
      DATA EHEAT(70) / 36.350D0/
      DATA EHEAT(72) /148.000D0/
      DATA EHEAT(73) /186.900D0/
      DATA EHEAT(74) /203.100D0/
      DATA EHEAT(75) /185.000D0/
      DATA EHEAT(76) /188.000D0/
      DATA EHEAT(77) /160.000D0/
      DATA EHEAT(78) /135.200D0/
      DATA EHEAT(79) / 88.000D0/
      DATA EHEAT(80) / 14.690D0/
      DATA EHEAT(81) / 43.550D0/
      DATA EHEAT(82) / 46.620D0/
      DATA EHEAT(83) / 50.100D0/
      DATA EHEAT(86) /  0.000D0/
C
      DATA VS(1) /  -13.605  /
      DATA VS(5)/-15.16D00/
      DATA VS(6)/-21.34D00/
      DATA VS(7)/-27.51D00/
      DATA VS(8)/-35.30D00/
      DATA VS(9)/-43.70D00/
      DATA VS(14)/-17.82D00/
      DATA VS(15)/-21.10D00/
      DATA VS(16)/-23.84D00/
      DATA VS(17)/-25.26D00/
      DATA VP(1)  /  0.0D00  /
      DATA VP(5)/-8.52D00/
      DATA VP(6)/-11.54D00/
      DATA VP(7)/-14.34D00/
      DATA VP(8)/-17.91D00/
      DATA VP(9)/-20.89D00/
      DATA VP(14)/-8.51D00/
      DATA VP(15)/-10.29D00/
      DATA VP(16)/-12.41D00/
      DATA VP(17)/-15.09D00/
C      DATA NPQ/1,1, 2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3, 4,4,4,4,4,4,4,4,
C     +4,4,4,4,4,4,4,4,4,4, 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5/
C
C *** ONE CENTER REPULSION INTEGRALS
C     GSS ::= (SS,SS)
C     GPP ::= (PP,PP)
C     GSP ::= (SS,PP)
C     GP2 ::= (PP,P*P*)
C     HSP ::= (SP,SP)
************************************************************************
      DATA GSSM(1) / 12.848D00 /
      DATA GSSM(4)/9.00D00/
      DATA GSSM(5)/10.59D00/
      DATA GSSM(6) / 12.23D00 /
      DATA GSSM(7)/13.59D00/
      DATA GSSM(8)/15.42D00/
      DATA GSSM(9)/16.92D00/
      DATA GSSM(13)/8.09D00/
      DATA GSSM(14)/9.82D00/
      DATA GSSM(15)/11.56D00/
      DATA GSSM(16)/12.88D00/
      DATA GSSM(17)/15.03D00/
      DATA GSSM(35)/15.03643948D0/
      DATA GSSM(53)/15.04044855D0/
      DATA GPPM(4)/6.97D00/
      DATA GPPM(5)/8.86D00/
      DATA GPPM(6) / 11.08D00 /
      DATA GPPM(7)/12.98D00/
      DATA GPPM(8)/14.52D00/
      DATA GPPM(9)/16.71D00/
      DATA GPPM(13)/5.98D00/
      DATA GPPM(14)/7.31D00/
      DATA GPPM(15)/8.64D00/
      DATA GPPM(16)/9.90D00/
      DATA GPPM(17)/11.30D00/
      DATA GPPM(35)/11.27632539D0/
      DATA GPPM(53)/11.14778369D0/
      DATA GSPM(4)/7.43D00/
      DATA GSPM(5)/9.56D00/
      DATA GSPM(6) / 11.47D00 /
      DATA GSPM(7)/12.66D00/
      DATA GSPM(8)/14.48D00/
      DATA GSPM(9)/17.25D00/
      DATA GSPM(13)/6.63D00/
      DATA GSPM(14)/8.36D00/
      DATA GSPM(15)/10.08D00/
      DATA GSPM(16)/11.26D00/
      DATA GSPM(17)/13.16D00/
      DATA GSPM(35)/13.03468242D0/
      DATA GSPM(53)/13.05655798D0/
      DATA GP2M(4)/6.22D00/
      DATA GP2M(5)/7.86D00/
      DATA GP2M(6) / 9.84D00 /
      DATA GP2M(7)/11.59D00/
      DATA GP2M(8)/12.98D00/
      DATA GP2M(9)/14.91D00/
      DATA GP2M(13)/5.40D00/
      DATA GP2M(14)/6.54D00/
      DATA GP2M(15)/7.68D00/
      DATA GP2M(16)/8.83D00/
      DATA GP2M(17)/9.97D00/
      DATA GP2M(35)/9.85442552D0/
      DATA GP2M(53)/9.91409071D0/
      DATA HSPM(4)/1.28D00/
      DATA HSPM(5)/1.81D00/
      DATA HSPM(6) / 2.43D00 /
      DATA HSPM(7)/3.14D00/
      DATA HSPM(8)/3.94D00/
      DATA HSPM(9)/4.83D00/
      DATA HSPM(13)/0.70D00/
      DATA HSPM(14)/1.32D00/
      DATA HSPM(15)/1.92D00/
      DATA HSPM(16)/2.26D00/
      DATA HSPM(17)/2.42D00/
      DATA HSPM(35)/2.45586832D0/
      DATA HSPM(53)/2.45638202D0/
C
C     THE MONOCENTRIC INTEGRALS HSP AND GSP FOR ALUMINIUM ARE ONLY
C     ESTIMATES. A VALUE OF G1 FOR AL IS NEEDED TO RESOLVE OLEARIS
C     INTEGRALS.
C
C     OPTIMIZED MNDO PARAMETERS FOR H, BE, B, C, N, O, F
C                                                     CL
C     ESTIMATED MNDO PARAMETERS FOR       AL,SI, P, S
C
C     ELEMENTS H, C, N, O WERE PARAMETERIZED BY WALTER THIEL
C     ELEMENTS B,SI,P,S   WERE      ..          MICHAEL MCKEE
C     ELEMENTS BE,F,AL,CL WERE      ..          HENRY RZEPA
C
***********************************************************************
*
*    START OF MINDO/3 PARAMETERS
*
***********************************************************************
C *** F03 IS THE ONE CENTER AVERAGED REPULSION INTEGRAL FOR USE IN THE
C        TWO CENTER ELECTRONIC REPULSION INTEGRAL EVALUATION.
      DATA F03              /  12.848D0, 10.0D0, 10.0D0, 0.0D0,
     1  8.958D0, 10.833D0, 12.377D0, 13.985D0, 16.250D0,
     2         10.000D0, 10.000D0, 0.000D0, 0.000D0,7.57D0 ,  9.00D0 ,
     3         10.20D0 , 11.73,10.0D0,35*0.D0,10.D0,53*10.D0/
C *** USS AND UPP ARE THE ONE-CENTER CORE ELECTRON ATTRACTION AND KINETI
C     ENERGY INTEGRALS FOR S AND P ELECTRONS RESPECTIVELY IN E.V.
      DATA USS3             / -12.505D0, 0.000D0, 0.000D0, 0.000D0,
     1                       -33.61D0, -51.79D0, -66.06D0, -91.73D0 ,
     2                       -129.86D0,
     3                        0.0000D0 , 0.000 D0 ,0.000D0 , 0.000D0 ,
     4          -39.82D0 , -56.23D0 , -73.39D0 , -98.99D0 ,.0D0/
      DATA UPP3             /   0.0D0, 0.0D0, 0.0D0, 0.0D0,
     1     -25.11D0 , -39.18D0 , -56.40D0 , -78.80D0 , -105.93D0 ,
     2                        0.000D0 , 0.000D0 , 0.000D0 , 0.000D0 ,
     3         -29.15D0 , -42.31D0 , -57.25D0 , -76.43D0 ,.0D0/
C *** EISOL3 AND EHEAT3 ARE THE GS ELECTRONIC ENERGY OF THE NEUTRAL ATOM
C     (IN E.V.) AND THE HEAT OF FORMATION IF THE FREE ATOM (IN KCAL/MOL)
      DATA EISOL3             /-12.505D0 , 0.0D0 , 0.0D0 ,0.0D0 ,
     1        -61.70D0 ,-119.47D0 , -187.51D0 , -307.07D0 , -475.00D0 ,
     2                         0.0D0 , 0.0D0 , 0.0D0 , 0.0D0 ,
     3          -90.98D0 , -150.81D0 , -229.15D0 , -345.93D0 , 0.0D0/
      DATA EHEAT3             / 52.102D0 , 0.0D0 , 0.0D0 , 0.0D0 ,
     1     135.7 D0 , 170.89D0 ,  113.0 D0 ,  59.559D0 ,  18.86D0 ,
     2                         0.0D0 , 0.0D0 , 0.0D0 , 0.0D0 ,
     3     106.0D0 ,   79.8D0 ,  65.65D0 ,  28.95D0 , 0.0D0 /
C *** BETA3 AND ALP3 ARE THE BOND PARAMETERS USED IN THE
C     RESONANCE INTEGRAL AND THE CORE CORE REPULSION INTEGRAL RESPECTIVE
C     THAT IS ACCORDING TO THE FOLLOWING CONVENTION
C
C     HERE IS THE
C     BOND TYPE DESIGNATION
C
C
C         H   B   C   N   O   F  SI   P   S  CL
C       -----------------------------------------
C      H  1  11  16  22  29  37  92 106 121 137
C      B     15  20  26  33  41
C      C         21  27  34  42  97 111 126 142
C      N             28  35  43         127 143
C      O                 36  44         128 144
C      F                     45         129
C     SI                        105
C      P                            120     151
C      S                                136 152
C     CL                                    153
      DATA BETA3(1),ALP3(1)   /  0.244770D0 ,  1.489450D0 /
      DATA BETA3(11),ALP3(11)   /  0.185347D0 ,  2.090352D0 /
      DATA BETA3(15),ALP3(15)   /  0.151324D0 ,  2.280544D0 /
      DATA BETA3(16),ALP3(16)   /  0.315011D0 ,  1.475836D0 /
      DATA BETA3(20),ALP3(20)   /  0.250031D0 ,  2.138291D0 /
      DATA BETA3(21),ALP3(21)   /  0.419907D0 ,  1.371208D0 /
      DATA BETA3(22),ALP3(22)   /  0.360776D0 ,  0.589380D0 /
      DATA BETA3(26),ALP3(26)   /  0.310959D0 ,  1.909763D0 /
      DATA BETA3(27),ALP3(27)   /  0.410886D0 ,  1.635259D0 /
      DATA BETA3(28),ALP3(28) /  0.377342D0 ,  2.029618D0 /
      DATA BETA3(29),ALP3(29) /  0.417759D0 ,  0.478901D0 /
      DATA BETA3(33),ALP3(33) /  0.349745D0 ,  2.484827D0 /
      DATA BETA3(34),ALP3(34) /  0.464514D0 ,  1.820975D0 /
      DATA BETA3(35),ALP3(35) /  0.458110D0 ,  1.873859D0 /
      DATA BETA3(36),ALP3(36) /  0.659407D0 ,  1.537190D0 /
      DATA BETA3(37),ALP3(37) /  0.195242D0 ,  3.771362D0 /
      DATA BETA3(41),ALP3(41) /  0.219591D0 ,  2.862183D0 /
      DATA BETA3(42),ALP3(42) /  0.247494D0 ,  2.725913D0 /
      DATA BETA3(43),ALP3(43) /  0.205347D0 ,  2.861667D0 /
      DATA BETA3(44),ALP3(44) /  0.334044D0 ,  2.266949D0 /
      DATA BETA3(45),ALP3(45) /  0.197464D0 ,  3.864997D0 /
      DATA BETA3(92),ALP3(92) /  0.289647D0 ,  0.940789D0 /
      DATA BETA3(97),ALP3(97) /  0.411377D0 ,  1.101382D0 /
      DATA BETA3(105),ALP3(105) /  0.291703D0 ,  0.918432D0 /
      DATA BETA3(106),ALP3(106) /  0.320118D0 ,  0.923170D0 /
      DATA BETA3(111),ALP3(111) /  0.457816D0 ,  1.029693D0 /
      DATA BETA3(120),ALP3(120) /  0.311790D0 ,  1.186652D0 /
      DATA BETA3(121),ALP3(121) /  0.220654D0 ,  1.700698D0 /
      DATA BETA3(126),ALP3(126) /  0.284620D0 ,  1.761370D0 /
      DATA BETA3(127),ALP3(127) /  0.313170D0 ,  1.878176D0/
      DATA BETA3(128),ALP3(128) /  0.422890D0 ,  2.077240D0 /
      DATA BETA3(129),ALP3(129)  /  0.000000D0 ,  0.000000D0 /
      DATA BETA3(136),ALP3(136) /  0.202489D0 ,  1.751617D0 /
      DATA BETA3(137),ALP3(137) /  0.231653D0 ,  2.089404D0 /
      DATA BETA3(142),ALP3(142) /  0.315480D0 ,  1.676222D0 /
      DATA BETA3(143),ALP3(143) /  0.302298D0 ,  1.817064D0 /
      DATA BETA3(144),ALP3(144) /  0.000000D0 ,  0.000000D0 /
      DATA BETA3(151),ALP3(151) /  0.277322D0 ,  1.543720D0 /
      DATA BETA3(152),ALP3(152) /  0.221764D0 ,  1.950318D0 /
      DATA BETA3(153),ALP3(153) /  0.258969D0 ,  1.792125D0 /
C *** HERE COMES THE OPTIMIZED SLATER_S EXPONENTS FOR THE EVALUATION
C     OF THE OVERLAP INTEGRALS AND MOLECULAR DIPOLE MOMENTS.
      DATA ZS3(1),ZP3(1)      /  1.3D0       ,  0.0D0      /
      DATA ZS3(5),ZP3(5)      /  1.211156D0 ,  0.972826D0 /
      DATA ZS3(6),ZP3(6)      /  1.739391D0 ,  1.709645D0 /
      DATA ZS3(7),ZP3(7)      /  2.704546D0 ,  1.870839D0 /
      DATA ZS3(8),ZP3(8)      /  3.640575D0 ,  2.168448D0 /
      DATA ZS3(9),ZP3(9)      /  3.111270D0 ,  1.41986D0 /
      DATA ZS3(14),ZP3(14)    /  1.629173D0 ,  1.381721D0 /
      DATA ZS3(15),ZP3(15)    /  1.926108D0 ,  1.590665D0 /
      DATA ZS3(16),ZP3(16)    /  1.719480D0 ,  1.403205D0 /
      DATA ZS3(17),ZP3(17)    /  3.430887D0 ,  1.627017D0 /
*************************************************************
*                                                           *
*               DATA FOR THE SPARKLES                       *
*                                                           *
*************************************************************
*                               DATA FOR THE " ++ " SPARKLE
      DATA EHEAT(103)    / 0.0D0/
      DATA VS(103)       /10.0D0/
      DATA ALP(103)      / 1.5D0/
      DATA EISOL(103)    / 0.0D0/
      DATA AM(103)       / 0.5D0/
      DATA ALPM(103)      / 1.5D0/
      DATA EISOLM(103)    / 0.0D0/
      DATA AMM(103)       / 0.5D0/
*                               DATA FOR THE " + " SPARKLE
      DATA EHEAT(104)    / 0.0D0/
      DATA VS(104)       /10.0D0/
      DATA ALP(104)      / 1.5D0/
      DATA EISOL(104)    / 0.0D0/
      DATA AM(104)       / 0.5D0/
      DATA ALPM(104)      / 1.5D0/
      DATA EISOLM(104)    / 0.0D0/
      DATA AMM(104)       / 0.5D0/
*                               DATA FOR THE " -- " SPARKLE
      DATA EHEAT(105)    / 0.0D0/
      DATA VS(105)       /10.0D0/
      DATA ALP(105)      / 1.5D0/
      DATA EISOL(105)    / 0.0D0/
      DATA AM(105)       / 0.5D0/
      DATA ALPM(105)      / 1.5D0/
      DATA EISOLM(105)    / 0.0D0/
      DATA AMM(105)       / 0.5D0/
*                               DATA FOR THE " - " SPARKLE
      DATA EHEAT(106)    / 0.0D0/
      DATA VS(106)       /10.0D0/
      DATA ALP(106)      / 1.5D0/
      DATA EISOL(106)    / 0.0D0/
      DATA AM(106)       / 0.5D0/
      DATA ALPM(106)      / 1.5D0/
      DATA EISOLM(106)    / 0.0D0/
      DATA AMM(106)       / 0.5D0/
***********************************************************************
*
*    START OF MNDO PARAMETERS
*
***********************************************************************
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA USSM   ( 1)/     -11.9062760D0/
      DATA BETASM ( 1)/      -6.9890640D0/
      DATA ZSM    ( 1)/       1.3319670D0/
      DATA ALPM   ( 1)/       2.5441341D0/
      DATA EISOLM ( 1)/     -11.9062760D0/
      DATA AMM    ( 1)/       0.4721793D0/
      DATA ADM    ( 1)/       0.4721793D0/
      DATA AQM    ( 1)/       0.4721793D0/
*                               DATA FOR THE LITHIUM-LIKE SPARKLE
      DATA EHEAT(3)    / 0.0D0/
      DATA VS(3)       /10.0D0/
      DATA ALP(3)      / 1.53D0/
      DATA EISOL(3)    / 0.0D0/
      DATA AM(3)       / 0.5D0/
      DATA ALPM(3)      / 1.53D0/
      DATA EISOLM(3)    / 0.0D0/
      DATA AMM(3)       / 0.5D0/
C                    DATA FOR ELEMENT  4        BERYLLIUM
      DATA USSM   ( 4)/     -16.6023780D0/
      DATA UPPM   ( 4)/     -10.7037710D0/
      DATA BETASM ( 4)/      -4.0170960D0/
      DATA BETAPM ( 4)/      -4.0170960D0/
      DATA ZSM    ( 4)/       1.0042100D0/
      DATA ZPM    ( 4)/       1.0042100D0/
      DATA ALPM   ( 4)/       1.6694340D0/
      DATA EISOLM ( 4)/     -24.2047560D0/
      DATA DDM    ( 4)/       1.4373245D0/
      DATA QQM    ( 4)/       1.2196103D0/
      DATA AMM    ( 4)/       0.3307607D0/
      DATA ADM    ( 4)/       0.3356142D0/
      DATA AQM    ( 4)/       0.3846373D0/
C                    DATA FOR ELEMENT  5        BORON
      DATA USSM   ( 5)/     -34.5471300D0/
      DATA UPPM   ( 5)/     -23.1216900D0/
      DATA BETASM ( 5)/      -8.2520540D0/
      DATA BETAPM ( 5)/      -8.2520540D0/
      DATA ZSM    ( 5)/       1.5068010D0/
      DATA ZPM    ( 5)/       1.5068010D0/
      DATA ALPM   ( 5)/       2.1349930D0/
      DATA EISOLM ( 5)/     -64.3159500D0/
      DATA DDM    ( 5)/       0.9579073D0/
      DATA QQM    ( 5)/       0.8128113D0/
      DATA AMM    ( 5)/       0.3891951D0/
      DATA ADM    ( 5)/       0.4904730D0/
      DATA AQM    ( 5)/       0.5556979D0/
C                    DATA FOR ELEMENT  6        CARBON
      DATA USSM   ( 6)/     -52.2797450D0/
      DATA UPPM   ( 6)/     -39.2055580D0/
      DATA BETASM ( 6)/     -18.9850440D0/
      DATA BETAPM ( 6)/      -7.9341220D0/
      DATA ZSM    ( 6)/       1.7875370D0/
      DATA ZPM    ( 6)/       1.7875370D0/
      DATA ALPM   ( 6)/       2.5463800D0/
      DATA EISOLM ( 6)/    -120.5006060D0/
      DATA DDM    ( 6)/       0.8074662D0/
      DATA QQM    ( 6)/       0.6851578D0/
      DATA AMM    ( 6)/       0.4494671D0/
      DATA ADM    ( 6)/       0.6149474D0/
      DATA AQM    ( 6)/       0.6685897D0/
C                    DATA FOR ELEMENT  7        NITROGEN
      DATA USSM   ( 7)/     -71.9321220D0/
      DATA UPPM   ( 7)/     -57.1723190D0/
      DATA BETASM ( 7)/     -20.4957580D0/
      DATA BETAPM ( 7)/     -20.4957580D0/
      DATA ZSM    ( 7)/       2.2556140D0/
      DATA ZPM    ( 7)/       2.2556140D0/
      DATA ALPM   ( 7)/       2.8613420D0/
      DATA EISOLM ( 7)/    -202.5812010D0/
      DATA DDM    ( 7)/       0.6399037D0/
      DATA QQM    ( 7)/       0.5429763D0/
      DATA AMM    ( 7)/       0.4994487D0/
      DATA ADM    ( 7)/       0.7843643D0/
      DATA AQM    ( 7)/       0.8144720D0/
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA USSM   ( 8)/     -99.6443090D0/
      DATA UPPM   ( 8)/     -77.7974720D0/
      DATA BETASM ( 8)/     -32.6880820D0/
      DATA BETAPM ( 8)/     -32.6880820D0/
      DATA ZSM    ( 8)/       2.6999050D0/
      DATA ZPM    ( 8)/       2.6999050D0/
      DATA ALPM   ( 8)/       3.1606040D0/
      DATA EISOLM ( 8)/    -317.8685060D0/
      DATA DDM    ( 8)/       0.5346024D0/
      DATA QQM    ( 8)/       0.4536252D0/
      DATA AMM    ( 8)/       0.5667034D0/
      DATA ADM    ( 8)/       0.9592562D0/
      DATA AQM    ( 8)/       0.9495934D0/
C                    DATA FOR ELEMENT  9        FLUORINE
      DATA USSM   ( 9)/    -131.0715480D0/
      DATA UPPM   ( 9)/    -105.7821370D0/
      DATA BETASM ( 9)/     -48.2904660D0/
      DATA BETAPM ( 9)/     -36.5085400D0/
      DATA ZSM    ( 9)/       2.8484870D0/
      DATA ZPM    ( 9)/       2.8484870D0/
      DATA ALPM   ( 9)/       3.4196606D0/
      DATA EISOLM ( 9)/    -476.6837810D0/
      DATA DDM    ( 9)/       0.5067166D0/
      DATA QQM    ( 9)/       0.4299633D0/
      DATA AMM    ( 9)/       0.6218302D0/
      DATA ADM    ( 9)/       1.0850301D0/
      DATA AQM    ( 9)/       1.0343643D0/
*                               DATA FOR THE SODIUM-LIKE SPARKLE
      DATA EHEAT(11)    / 0.0D0/
      DATA VS(11)       /10.0D0/
      DATA ALP(11)      / 1.32D0/
      DATA EISOL(11)    / 0.0D0/
      DATA AM(11)       / 0.5D0/
      DATA ALPM(11)      / 1.32D0/
      DATA EISOLM(11)    / 0.0D0/
      DATA AMM(11)       / 0.5D0/
C                    DATA FOR ELEMENT 13        ALUMINUM
      DATA USSM   (13)/     -23.8070970D0/
      DATA UPPM   (13)/     -17.5198780D0/
      DATA BETASM (13)/      -2.6702840D0/
      DATA BETAPM (13)/      -2.6702840D0/
      DATA ZSM    (13)/       1.4441610D0/
      DATA ZPM    (13)/       1.4441610D0/
      DATA ZDM    (13)/       1.0000000D0/
      DATA ALPM   (13)/       1.8688394D0/
************************************************************************
*
*   THE FOLLOWING PARAMETER IS IN ERROR, DO NOT CORRECT IT AS THE ERROR
*   WAS INTRODUCED IN THE ORIGINAL PARAMETRIZATION, AND HAS BEENABSORBED
*   BY THE OTHER PARAMETERS.
*
      DATA EISOLM (13)/     -44.4840711D0/
*
*   THE CORRECT VALUE SHOULD BE -43.0840720D0
*
************************************************************************
      DATA DDM    (13)/       1.3992387D0/
      DATA QQM    (13)/       1.1586797D0/
      DATA AMM    (13)/       0.2973172D0/
      DATA ADM    (13)/       0.2635574D0/
      DATA AQM    (13)/       0.3673560D0/
C#C                    DATA FOR ELEMENT 14        SILICON
C    PARAMETERS FOR SILICON STARTING WITH -40.568... ARE OLD PARAMETERS
C#      DATA USSM   (14)/     -40.5682920D0/
C#      DATA UPPM   (14)/     -28.0891870D0/
C#      DATA BETASM (14)/      -4.2562180D0/
C#      DATA BETAPM (14)/      -4.2562180D0/
C#      DATA ZSM    (14)/       1.4353060D0/
C#      DATA ZPM    (14)/       1.4353060D0/
C#      DATA ZDM    (14)/       1.0000000D0/
C#      DATA ALPM   (14)/       2.1961078D0/
C#      DATA EISOLM (14)/     -90.5399580D0/
C#      DATA DDM    (14)/       1.4078712D0/
C#      DATA QQM    (14)/       1.1658281D0/
C#      DATA AMM    (14)/       0.3608967D0/
C#      DATA ADM    (14)/       0.3441817D0/
C#      DATA AQM    (14)/       0.3999442D0/
C                    DATA FOR ELEMENT 14
C    PARAMETERS FOR SILICON STARTING WITH -37.0375 ARE GILL'S PARAMETERS
      DATA USS   M(14)/     -37.0375330D0/
      DATA UPP   M(14)/     -27.7696780D0/
      DATA BETAS M(14)/      -9.0868040D0/
      DATA BETAP M(14)/      -1.0758270D0/
      DATA ZS    M(14)/       1.3159860D0/
      DATA ZP    M(14)/       1.7099430D0/
      DATA ZD    M(14)/       1.0000000D0/
      DATA ALP   M(14)/       2.2053160D0/
      DATA EISOL M(14)/     -82.8394220D0/
      DATA DD    M(14)/       1.2580349D0/
      DATA QQ    M(14)/       0.9785824D0/
      DATA AM    M(14)/       0.3608967D0/
      DATA AD    M(14)/       0.3664244D0/
      DATA AQ    M(14)/       0.4506740D0/
C                    DATA FOR ELEMENT 15        PHOSPHORUS
      DATA USSM   (15)/     -56.1433600D0/
      DATA UPPM   (15)/     -42.8510800D0/
      DATA BETASM (15)/      -6.7916000D0/
      DATA BETAPM (15)/      -6.7916000D0/
      DATA ZSM    (15)/       2.1087200D0/
      DATA ZPM    (15)/       1.7858100D0/
      DATA ZDM    (15)/       1.0000000D0/
      DATA ALPM   (15)/       2.4152800D0/
      DATA EISOLM (15)/    -152.9599600D0/
      DATA DDM    (15)/       1.0129699D0/
      DATA QQM    (15)/       0.9370090D0/
      DATA AMM    (15)/       0.4248438D0/
      DATA ADM    (15)/       0.4882420D0/
      DATA AQM    (15)/       0.4979406D0/
C                    DATA FOR ELEMENT 16        SULFUR
      DATA USSM   (16)/     -75.2391520D0/
      DATA UPPM   (16)/     -57.8320130D0/
      DATA BETASM (16)/     -11.1422310D0/
      DATA BETAPM (16)/     -11.1422310D0/
      DATA ZSM    (16)/       2.6135910D0/
      DATA ZPM    (16)/       2.0343930D0/
      DATA ZDM    (16)/       1.0000000D0/
      DATA ALPM   (16)/       2.4916445D0/
      DATA EISOLM (16)/    -235.4413560D0/
      DATA DDM    (16)/       0.8231596D0/
      DATA QQM    (16)/       0.8225156D0/
      DATA AMM    (16)/       0.4733554D0/
      DATA ADM    (16)/       0.5889395D0/
      DATA AQM    (16)/       0.5632724D0/
C                    DATA FOR ELEMENT 17        CHLORINE
      DATA USSM   (17)/    -100.2271660D0/
      DATA UPPM   (17)/     -77.3786670D0/
      DATA BETASM (17)/     -14.2623200D0/
      DATA BETAPM (17)/     -14.2623200D0/
      DATA ZSM    (17)/       3.7846450D0/
      DATA ZPM    (17)/       2.0362630D0/
      DATA ZDM    (17)/       1.0000000D0/
      DATA ALPM   (17)/       2.5422010D0/
      DATA EISOLM (17)/    -353.1176670D0/
      DATA DDM    (17)/       0.4986870D0/
      DATA QQM    (17)/       0.8217603D0/
      DATA AMM    (17)/       0.5523705D0/
      DATA ADM    (17)/       0.8061220D0/
      DATA AQM    (17)/       0.6053435D0/
*                               DATA FOR THE POTASSIUM-LIKE SPARKLE
      DATA EHEAT(19)    / 0.0D0/
      DATA VS(19)       /10.0D0/
      DATA ALP(19)      / 1.16D0/
      DATA EISOL(19)    / 0.0D0/
      DATA AM(19)       / 0.5D0/
      DATA ALPM(19)      / 1.16D0/
      DATA EISOLM(19)    / 0.0D0/
      DATA AMM(19)       / 0.5D0/
C                    DATA FOR ELEMENT 32        GERMANIUM
      DATA USSM  ( 32)/     -33.9493670D0/
      DATA UPPM  ( 32)/     -27.4251050D0/
      DATA BETASM( 32)/      -4.5164790D0/
      DATA BETAPM( 32)/      -1.7555170D0/
      DATA ZSM   ( 32)/       1.2931800D0/
      DATA ZPM   ( 32)/       2.0205640D0/
      DATA ALPM  ( 32)/       1.9784980D0/
      DATA EISOLM( 32)/     -76.2489440D0/
      DATA GSSM  ( 32)/       9.8000000D0/
      DATA GSPM  ( 32)/       8.3000000D0/
      DATA GPPM  ( 32)/       7.3000000D0/
      DATA GP2M  ( 32)/       6.5000000D0/
      DATA HSPM  ( 32)/       1.3000000D0/
      DATA DDM   ( 32)/       1.2556091D0/
      DATA QQM   ( 32)/       1.0498655D0/
      DATA AMM   ( 32)/       0.3601617D0/
      DATA ADM   ( 32)/       0.3643722D0/
      DATA AQM   ( 32)/       0.4347337D0/
C                    DATA FOR ELEMENT 35        BROMINE
      DATA USSM   (35)/     -99.9864405D0/
      DATA UPPM   (35)/     -75.6713075D0/
      DATA BETASM (35)/      -8.9171070D0/
      DATA BETAPM (35)/      -9.9437400D0/
      DATA ZSM    (35)/       3.8543019D0/
      DATA ZPM    (35)/       2.1992091D0/
      DATA ZDM    (35)/       1.0000000D0/
      DATA ALPM   (35)/       2.4457051D0/
      DATA EISOLM (35)/    -346.6812500D0/
      DATA DDM    (35)/       0.6051074D0/
      DATA QQM    (35)/       0.9645873D0/
      DATA AMM    (35)/       0.5526068D0/
      DATA ADM    (35)/       0.7258330D0/
      DATA AQM    (35)/       0.5574589D0/
C                    DATA FOR ELEMENT 50        TIN
      DATA USSM  (50)/     -40.8518020D0/
      DATA UPPM   (50)/     -28.5602490D0/
      DATA BETASM (50)/      -3.2351470D0/
      DATA BETAPM (50)/      -4.2904160D0/
      DATA ZSM    (50)/       2.0803800D0/
      DATA ZPM   (50)/       1.9371060D0/
      DATA ALPM   (50)/       1.8008140D0/
      DATA EISOLM (50)/     -92.3241020D0/
      DATA GSSM   (50)/       9.8000000D0/
      DATA GSPM   (50)/       8.3000000D0/
      DATA GPPM   (50)/       7.3000000D0/
      DATA GP2M   (50)/       6.5000000D0/
      DATA HSPM   (50)/       1.3000000D0/
      DATA DDM    (50)/       1.5697766D0/
      DATA QQM    (50)/       1.3262292D0/
      DATA AMM    (50)/       0.3601617D0/
      DATA ADM    (50)/       0.3219998D0/
      DATA AQM    (50)/       0.3713827D0/
C                    DATA FOR ELEMENT 53        IODINE
      DATA USSM   (53)/    -100.0030538D0/
      DATA UPPM   (53)/     -74.6114692D0/
      DATA BETASM (53)/      -7.4144510D0/
      DATA BETAPM (53)/      -6.1967810D0/
      DATA ZSM    (53)/       2.2729610D0/
      DATA ZPM    (53)/       2.1694980D0/
      DATA ZDM    (53)/       1.0000000D0/
      DATA ALPM   (53)/       2.2073200D0/
      DATA EISOLM (53)/    -340.5983600D0/
      DATA DDM    (53)/       1.4253233D0/
      DATA QQM    (53)/       1.1841707D0/
      DATA AMM    (53)/       0.5527541D0/
      DATA ADM    (53)/       0.4593451D0/
      DATA AQM    (53)/       0.4585376D0/
C                    DATA FOR ELEMENT 80        MERCURY
      DATA USSM   ( 80)/     -19.8095740D0/
      DATA UPPM   ( 80)/     -13.1025300D0/
      DATA BETASM ( 80)/      -0.4045250D0/
      DATA BETAPM ( 80)/      -6.2066830D0/
      DATA ZSM    ( 80)/       2.2181840D0/
      DATA ZPM    ( 80)/       2.0650380D0/
      DATA ALPM   ( 80)/       1.3356410D0/
      DATA EISOLM ( 80)/     -28.8191480D0/
      DATA GSSM   ( 80)/      10.8000000D0/
      DATA GSPM   ( 80)/       9.3000000D0/
      DATA GPPM   ( 80)/      14.3000000D0/
      DATA GP2M   ( 80)/      13.5000000D0/
      DATA HSPM   ( 80)/       1.3000000D0/
      DATA DDM    ( 80)/       1.7378048D0/
      DATA QQM    ( 80)/       1.4608064D0/
      DATA AMM    ( 80)/       0.3969129D0/
      DATA ADM    ( 80)/       0.3047694D0/
      DATA AQM    ( 80)/       0.3483102D0/
C                    DATA FOR ELEMENT 82        LEAD
      DATA USSM   ( 82)/     -47.3196920D0/
      DATA UPPM   ( 82)/     -28.8475600D0/
      DATA BETASM ( 82)/      -8.0423870D0/
      DATA BETAPM ( 82)/      -3.0000000D0/
      DATA ZSM    ( 82)/       2.4982860D0/
      DATA ZPM    ( 82)/       2.0820710D0/
      DATA ALPM   ( 82)/       1.7283330D0/
      DATA EISOLM ( 82)/    -105.8345040D0/
      DATA GSSM   ( 82)/       9.8000000D0/
      DATA GSPM   ( 82)/       8.3000000D0/
      DATA GPPM   ( 82)/       7.3000000D0/
      DATA GP2M   ( 82)/       6.5000000D0/
      DATA HSPM   ( 82)/       1.3000000D0/
      DATA DDM    ( 82)/       1.5526624D0/
      DATA QQM    ( 82)/       1.4488558D0/
      DATA AMM    ( 82)/       0.3601617D0/
      DATA ADM    ( 82)/       0.3239309D0/
      DATA AQM    ( 82)/       0.3502057D0/
***********************************************************************
*
*    START OF AM1 PARAMETERS
*
***********************************************************************
C                    DATA FOR ELEMENT  1
      DATA USS   ( 1)/     -11.3964270D0/
      DATA BETAS ( 1)/      -6.1737870D0/
      DATA ZS    ( 1)/       1.1880780D0/
      DATA ALP   ( 1)/       2.8823240D0/
      DATA EISOL ( 1)/     -11.3964270D0/
      DATA GSS   ( 1)/      12.8480000D0/
      DATA AM    ( 1)/       0.4721793D0/
      DATA AD    ( 1)/       0.4721793D0/
      DATA AQ    ( 1)/       0.4721793D0/
      DATA GUESS1( 1,1)/       0.1227960D0/
      DATA GUESS2( 1,1)/       5.0000000D0/
      DATA GUESS3( 1,1)/       1.2000000D0/
      DATA GUESS1( 1,2)/       0.0050900D0/
      DATA GUESS2( 1,2)/       5.0000000D0/
      DATA GUESS3( 1,2)/       1.8000000D0/
      DATA GUESS1( 1,3)/      -0.0183360D0/
      DATA GUESS2( 1,3)/       2.0000000D0/
      DATA GUESS3( 1,3)/       2.1000000D0/
C                    DATA FOR ELEMENT  4
      DATA USS   ( 4)/     -16.6023780D0/
      DATA UPP   ( 4)/     -10.7037710D0/
      DATA BETAS ( 4)/      -4.0170960D0/
      DATA BETAP ( 4)/      -4.0170960D0/
      DATA ZS    ( 4)/       1.0042100D0/
      DATA ZP    ( 4)/       1.0042100D0/
      DATA ALP   ( 4)/       1.6694340D0/
      DATA EISOL ( 4)/     -24.2047560D0/
      DATA GSS   ( 4)/       9.0000000D0/
      DATA GSP   ( 4)/       7.4300000D0/
      DATA GPP   ( 4)/       6.9700000D0/
      DATA GP2   ( 4)/       6.2200000D0/
      DATA HSP   ( 4)/       1.2800000D0/
      DATA DD    ( 4)/       1.4373245D0/
      DATA QQ    ( 4)/       1.2196103D0/
      DATA AM    ( 4)/       0.3307607D0/
      DATA AD    ( 4)/       0.3356142D0/
      DATA AQ    ( 4)/       0.3846373D0/
C                    DATA FOR ELEMENT  5
      DATA USS   ( 5)/     -34.5471300D0/
      DATA UPP   ( 5)/     -23.1216900D0/
      DATA BETAS ( 5)/      -8.2520540D0/
      DATA BETAP ( 5)/      -8.2520540D0/
      DATA ZS    ( 5)/       1.5068010D0/
      DATA ZP    ( 5)/       1.5068010D0/
      DATA ALP   ( 5)/       2.1349930D0/
      DATA EISOL ( 5)/     -64.3159500D0/
      DATA GSS   ( 5)/      10.5900000D0/
      DATA GSP   ( 5)/       9.5600000D0/
      DATA GPP   ( 5)/       8.8600000D0/
      DATA GP2   ( 5)/       7.8600000D0/
      DATA HSP   ( 5)/       1.8100000D0/
      DATA DD    ( 5)/       0.9579073D0/
      DATA QQ    ( 5)/       0.8128113D0/
      DATA AM    ( 5)/       0.3891951D0/
      DATA AD    ( 5)/       0.4904730D0/
      DATA AQ    ( 5)/       0.5556979D0/
C                    DATA FOR ELEMENT  6
      DATA USS   ( 6)/     -52.0286580D0/
      DATA UPP   ( 6)/     -39.6142390D0/
      DATA BETAS ( 6)/     -15.7157830D0/
      DATA BETAP ( 6)/      -7.7192830D0/
      DATA ZS    ( 6)/       1.8086650D0/
      DATA ZP    ( 6)/       1.6851160D0/
      DATA ALP   ( 6)/       2.6482740D0/
      DATA EISOL ( 6)/    -120.8157940D0/
      DATA GSS   ( 6)/      12.2300000D0/
      DATA GSP   ( 6)/      11.4700000D0/
      DATA GPP   ( 6)/      11.0800000D0/
      DATA GP2   ( 6)/       9.8400000D0/
      DATA HSP   ( 6)/       2.4300000D0/
      DATA DD    ( 6)/       0.8236736D0/
      DATA QQ    ( 6)/       0.7268015D0/
      DATA AM    ( 6)/       0.4494671D0/
      DATA AD    ( 6)/       0.6082946D0/
      DATA AQ    ( 6)/       0.6423492D0/
      DATA GUESS1( 6,1)/       0.0113550D0/
      DATA GUESS2( 6,1)/       5.0000000D0/
      DATA GUESS3( 6,1)/       1.6000000D0/
      DATA GUESS1( 6,2)/       0.0459240D0/
      DATA GUESS2( 6,2)/       5.0000000D0/
      DATA GUESS3( 6,2)/       1.8500000D0/
      DATA GUESS1( 6,3)/      -0.0200610D0/
      DATA GUESS2( 6,3)/       5.0000000D0/
      DATA GUESS3( 6,3)/       2.0500000D0/
      DATA GUESS1( 6,4)/      -0.0012600D0/
      DATA GUESS2( 6,4)/       5.0000000D0/
      DATA GUESS3( 6,4)/       2.6500000D0/
C                    DATA FOR ELEMENT  7
      DATA USS   ( 7)/     -71.8600000D0/
      DATA UPP   ( 7)/     -57.1675810D0/
      DATA BETAS ( 7)/     -20.2991100D0/
      DATA BETAP ( 7)/     -18.2386660D0/
      DATA ZS    ( 7)/       2.3154100D0/
      DATA ZP    ( 7)/       2.1579400D0/
      DATA ALP   ( 7)/       2.9472860D0/
      DATA EISOL ( 7)/    -202.4077430D0/
      DATA GSS   ( 7)/      13.5900000D0/
      DATA GSP   ( 7)/      12.6600000D0/
      DATA GPP   ( 7)/      12.9800000D0/
      DATA GP2   ( 7)/      11.5900000D0/
      DATA HSP   ( 7)/       3.1400000D0/
      DATA DD    ( 7)/       0.6433247D0/
      DATA QQ    ( 7)/       0.5675528D0/
      DATA AM    ( 7)/       0.4994487D0/
      DATA AD    ( 7)/       0.7820840D0/
      DATA AQ    ( 7)/       0.7883498D0/
      DATA GUESS1( 7,1)/       0.0252510D0/
      DATA GUESS2( 7,1)/       5.0000000D0/
      DATA GUESS3( 7,1)/       1.5000000D0/
      DATA GUESS1( 7,2)/       0.0289530D0/
      DATA GUESS2( 7,2)/       5.0000000D0/
      DATA GUESS3( 7,2)/       2.1000000D0/
      DATA GUESS1( 7,3)/      -0.0058060D0/
      DATA GUESS2( 7,3)/       2.0000000D0/
      DATA GUESS3( 7,3)/       2.4000000D0/
C                    DATA FOR ELEMENT  8
      DATA USS   ( 8)/     -97.8300000D0/
      DATA UPP   ( 8)/     -78.2623800D0/
      DATA BETAS ( 8)/     -29.2727730D0/
      DATA BETAP ( 8)/     -29.2727730D0/
      DATA ZS    ( 8)/       3.1080320D0/
      DATA ZP    ( 8)/       2.5240390D0/
      DATA ALP   ( 8)/       4.4553710D0/
      DATA EISOL ( 8)/    -316.0995200D0/
      DATA GSS   ( 8)/      15.4200000D0/
      DATA GSP   ( 8)/      14.4800000D0/
      DATA GPP   ( 8)/      14.5200000D0/
      DATA GP2   ( 8)/      12.9800000D0/
      DATA HSP   ( 8)/       3.9400000D0/
      DATA DD    ( 8)/       0.4988896D0/
      DATA QQ    ( 8)/       0.4852322D0/
      DATA AM    ( 8)/       0.5667034D0/
      DATA AD    ( 8)/       0.9961066D0/
      DATA AQ    ( 8)/       0.9065223D0/
      DATA GUESS1( 8,1)/       0.2809620D0/
      DATA GUESS2( 8,1)/       5.0000000D0/
      DATA GUESS3( 8,1)/       0.8479180D0/
      DATA GUESS1( 8,2)/       0.0814300D0/
      DATA GUESS2( 8,2)/       7.0000000D0/
      DATA GUESS3( 8,2)/       1.4450710D0/
C                    DATA FOR ELEMENT  9
      DATA USS   ( 9)/    -131.0715480D0/
      DATA UPP   ( 9)/    -105.7821370D0/
      DATA BETAS ( 9)/     -48.2904660D0/
      DATA BETAP ( 9)/     -36.5085400D0/
      DATA ZS    ( 9)/       2.8484870D0/
      DATA ZP    ( 9)/       2.8484870D0/
      DATA ALP   ( 9)/       3.4196606D0/
      DATA EISOL ( 9)/    -476.6837810D0/
      DATA GSS   ( 9)/      16.9200000D0/
      DATA GSP   ( 9)/      17.2500000D0/
      DATA GPP   ( 9)/      16.7100000D0/
      DATA GP2   ( 9)/      14.9100000D0/
      DATA HSP   ( 9)/       4.8300000D0/
      DATA DD    ( 9)/       0.5067166D0/
      DATA QQ    ( 9)/       0.4299633D0/
      DATA AM    ( 9)/       0.6218302D0/
      DATA AD    ( 9)/       1.0850301D0/
      DATA AQ    ( 9)/       1.0343643D0/
C                    DATA FOR ELEMENT 13
      DATA USS   (13)/     -23.8070970D0/
      DATA UPP   (13)/     -17.5198780D0/
      DATA BETAS (13)/      -2.6702840D0/
      DATA BETAP (13)/      -2.6702840D0/
      DATA ZS    (13)/       1.4441610D0/
      DATA ZP    (13)/       1.4441610D0/
      DATA ZD    (13)/       1.0000000D0/
      DATA ALP   (13)/       1.8688394D0/
      DATA EISOL (13)/     -44.4840720D0/
      DATA GSS   (13)/       8.0900000D0/
      DATA GSP   (13)/       6.6300000D0/
      DATA GPP   (13)/       5.9800000D0/
      DATA GP2   (13)/       5.4000000D0/
      DATA HSP   (13)/       0.7000000D0/
      DATA DD    (13)/       1.3992387D0/
      DATA QQ    (13)/       1.1586797D0/
      DATA AM    (13)/       0.2973172D0/
      DATA AD    (13)/       0.2635574D0/
      DATA AQ    (13)/       0.3673560D0/
C                    DATA FOR ELEMENT 14
      DATA USS   (14)/     -40.5682920D0/
      DATA UPP   (14)/     -28.0891870D0/
      DATA BETAS (14)/      -4.2562180D0/
      DATA BETAP (14)/      -4.2562180D0/
      DATA ZS    (14)/       1.4353060D0/
      DATA ZP    (14)/       1.4353060D0/
      DATA ZD    (14)/       1.0000000D0/
      DATA ALP   (14)/       2.1961078D0/
      DATA EISOL (14)/     -90.5399580D0/
      DATA GSS   (14)/       9.8200000D0/
      DATA GSP   (14)/       8.3600000D0/
      DATA GPP   (14)/       7.3100000D0/
      DATA GP2   (14)/       6.5400000D0/
      DATA HSP   (14)/       1.3200000D0/
      DATA DD    (14)/       1.4078712D0/
      DATA QQ    (14)/       1.1658281D0/
      DATA AM    (14)/       0.3608967D0/
      DATA AD    (14)/       0.3441817D0/
      DATA AQ    (14)/       0.3999442D0/
C                    DATA FOR ELEMENT 15
      DATA USS   (15)/     -56.1433600D0/
      DATA UPP   (15)/     -42.8510800D0/
      DATA BETAS (15)/      -6.7916000D0/
      DATA BETAP (15)/      -6.7916000D0/
      DATA ZS    (15)/       2.1087200D0/
      DATA ZP    (15)/       1.7858100D0/
      DATA ZD    (15)/       1.0000000D0/
      DATA ALP   (15)/       2.4152800D0/
      DATA EISOL (15)/    -152.9599600D0/
      DATA GSS   (15)/      11.5600000D0/
      DATA GSP   (15)/      10.0800000D0/
      DATA GPP   (15)/       8.6400000D0/
      DATA GP2   (15)/       7.6800000D0/
      DATA HSP   (15)/       1.9200000D0/
      DATA DD    (15)/       1.0129699D0/
      DATA QQ    (15)/       0.9370090D0/
      DATA AM    (15)/       0.4248438D0/
      DATA AD    (15)/       0.4882420D0/
      DATA AQ    (15)/       0.4979406D0/
C                    DATA FOR ELEMENT 16
      DATA USS   (16)/     -75.2391520D0/
      DATA UPP   (16)/     -57.8320130D0/
      DATA BETAS (16)/     -11.1422310D0/
      DATA BETAP (16)/     -11.1422310D0/
      DATA ZS    (16)/       2.6135910D0/
      DATA ZP    (16)/       2.0343930D0/
      DATA ZD    (16)/       1.0000000D0/
      DATA ALP   (16)/       2.4916445D0/
      DATA EISOL (16)/    -235.4413560D0/
      DATA GSS   (16)/      12.8800000D0/
      DATA GSP   (16)/      11.2600000D0/
      DATA GPP   (16)/       9.9000000D0/
      DATA GP2   (16)/       8.8300000D0/
      DATA HSP   (16)/       2.2600000D0/
      DATA DD    (16)/       0.8231596D0/
      DATA QQ    (16)/       0.8225156D0/
      DATA AM    (16)/       0.4733554D0/
      DATA AD    (16)/       0.5889395D0/
      DATA AQ    (16)/       0.5632724D0/
C                    DATA FOR ELEMENT 17
      DATA USS   (17)/    -100.2271660D0/
      DATA UPP   (17)/     -77.3786670D0/
      DATA BETAS (17)/     -14.2623200D0/
      DATA BETAP (17)/     -14.2623200D0/
      DATA ZS    (17)/       3.7846450D0/
      DATA ZP    (17)/       2.0362630D0/
      DATA ZD    (17)/       1.0000000D0/
      DATA ALP   (17)/       2.5422010D0/
      DATA EISOL (17)/    -353.1176670D0/
      DATA GSS   (17)/      15.0300000D0/
      DATA GSP   (17)/      13.1600000D0/
      DATA GPP   (17)/      11.3000000D0/
      DATA GP2   (17)/       9.9700000D0/
      DATA HSP   (17)/       2.4200000D0/
      DATA DD    (17)/       0.4986870D0/
      DATA QQ    (17)/       0.8217603D0/
      DATA AM    (17)/       0.5523705D0/
      DATA AD    (17)/       0.8061220D0/
      DATA AQ    (17)/       0.6053435D0/
C                    DATA FOR ELEMENT 35
      DATA USS   (35)/     -99.9864405D0/
      DATA UPP   (35)/     -75.6713075D0/
      DATA BETAS (35)/      -8.9171070D0/
      DATA BETAP (35)/      -9.9437400D0/
      DATA ZS    (35)/       3.8543019D0/
      DATA ZP    (35)/       2.1992091D0/
      DATA ZD    (35)/       1.0000000D0/
      DATA ALP   (35)/       2.4457051D0/
      DATA EISOL (35)/    -346.6812412D0/
      DATA GSS   (35)/      15.0364395D0/
      DATA GSP   (35)/      13.0346824D0/
      DATA GPP   (35)/      11.2763254D0/
      DATA GP2   (35)/       9.8544255D0/
      DATA HSP   (35)/       2.4558683D0/
      DATA DD    (35)/       0.6051090D0/
      DATA QQ    (35)/       0.9645833D0/
      DATA AM    (35)/       0.5526071D0/
      DATA AD    (35)/       0.7258329D0/
      DATA AQ    (35)/       0.5574631D0/
C                    DATA FOR ELEMENT 53
      DATA USS   (53)/    -100.0030538D0/
      DATA UPP   (53)/     -74.6114692D0/
      DATA BETAS (53)/      -7.4144510D0/
      DATA BETAP (53)/      -6.1967810D0/
      DATA ZS    (53)/       2.2729610D0/
      DATA ZP    (53)/       2.1694980D0/
      DATA ZD    (53)/       1.0000000D0/
      DATA ALP   (53)/       2.2073200D0/
      DATA EISOL (53)/    -340.5984283D0/
      DATA GSS   (53)/      15.0404486D0/
      DATA GSP   (53)/      13.0565580D0/
      DATA GPP   (53)/      11.1477837D0/
      DATA GP2   (53)/       9.9140907D0/
      DATA HSP   (53)/       2.4563820D0/
      DATA DD    (53)/       1.4253210D0/
      DATA QQ    (53)/       1.1841663D0/
      DATA AM    (53)/       0.5527544D0/
      DATA AD    (53)/       0.4593457D0/
      DATA AQ    (53)/       0.4643289D0/
C                    DATA FOR ELEMENT 54
      DATA USS   (54)/    -100.0030538D0/
      DATA UPP   (54)/     -70.0000000D0/
      DATA BETAS (54)/      -7.4144510D0/
      DATA BETAP (54)/      -6.1967810D0/
      DATA ZS    (54)/       2.2729610D0/
      DATA ZP    (54)/       1.1694980D0/
      DATA ALP   (54)/       2.2073200D0/
      DATA EISOL (54)/    -191.0061076D0/
      DATA GSS   (54)/       9.0000000D0/
      DATA GSP   (54)/      11.2000000D0/
      DATA GPP   (54)/      11.0000000D0/
      DATA GP2   (54)/       9.2300000D0/
      DATA HSP   (54)/       2.4000000D0/
      DATA DD    (54)/       1.0775919D0/
      DATA QQ    (54)/       2.5794150D0/
      DATA AM    (54)/       0.3307607D0/
      DATA AD    (54)/       0.5236997D0/
      DATA AQ    (54)/       0.3397123D0/
      END
