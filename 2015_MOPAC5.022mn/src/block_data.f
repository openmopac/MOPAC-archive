C
C   **** THIS FORTRAN BLOCK DATA FILE IS NO LONGER BEING USED.
C   **** THE RESETC SUBROUTINE NOW REPLACES THE ASSIGNMENTS FORMERLY
C   **** DONE IN THIS BLOCK SO THAT THE MOPAC PROGRAM CAN BE CALLED
C   **** RECURSIVELY WITHOUT EXITING AND RESTARTING THE PROGRAM.


      BLOCK DATA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C   This blockdata file has been updated so that the Aluminum, Phosphorus,
C   and Sulfur AM1 parameters are from MOPAC-version 6.0, as well as the
C   following PM3 parameters: Beryllium, Magnesium, Zinc, Gallium, Germanium,
C   Arsenic, Selenium, Cadmium, Indium, Tin, Antimony, Tellurium, Mercury, 
C   Thallium, Lead, and Bismuth.  
C
      COMMON /NATORB/ NATORB(120)
***********************************************************************
*
*     COMMON BLOCKS FOR AM1 AND RM1
*
***********************************************************************
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
*Added by Luke Fiedler Mar 2009:
***********************************************************************
*
*  COMMON BLOCKS FOR MNDO-D, AM1-D, AND OTHER -D  (D=DISPERSION) METHODS
*
***********************************************************************
      COMMON /PM3DSB/ ALPPMD,R0PMD(120),S6PMD,C6PMD(120),USSPMD(120),   LF0309
     &   UPPPMD(120),BETASD(120),BETAPD(120),ALPHAD(120),EISOLD(120)    LF0309
      COMMON /AM1DSB/ ALPAMD,R0AMD(120),S6AMD,C6AMD(120),USSAMD(120),   LF0509
     &   UPPAMD(120),BETASE(120),BETAPE(120),ALPHAE(120),EISOLE(120)    LF0509
      COMMON /MNDODS/ ALPMND,R0MND(120),S6MND,C6MND(120),USSMND(120),
     &   UPPMND(120),BETASF(120),BETAPF(120),ALPHAF(120),EISOLF(120),
     &   GSSMND(120),GSPMND(120),GPPMND(120),GP2MND(120),HSPMND(120),
     &   ZSMND(120),ZPMND(120)
      COMMON /RM1DSB/ ALPRMD,R0RMD(120),S6RMD,C6RMD(120),USSRMD(120),   LF0310
     &   UPPRMD(120),BETASI(120),BETAPI(120),ALPHAI(120),EISOLI(120)    LF0310
      COMMON /PM6DSB/ ALPP6D,R0P6D(120),S6P6D,C6P6D(120),USSP6D(120),   LF0310
     &   UPPP6D(120),BETASJ(120),BETAPJ(120),ALPHAJ(120),EISOLJ(120)    LF0310
*Added by Luke Fiedler Oct 2010:
***********************************************************************
*
*  COMMON BLOCK FOR PMOv1 METHOD
*
***********************************************************************
      COMMON /PMOV1CB/ ALPPMOV1, R0PMOV1(120), S6PMOV1, C6PMOV1(120),
     &                 USSPMOV1(120), UPPPMOV1(120), BETASPMOV1(120),
     &                 BETAPPMOV1(120), ALPHAPMOV1(120), 
     &                 EISOLPMOV1(120), GSSPMOV1(120), GSPPMOV1(120), 
     &                 GPPPMOV1(120), GP2PMOV1(120), HSPPMOV1(120), 
     &                 ZSPMOV1(120), ZPPMOV1(120), 
     &                 AMPMOV1(120), ADPMOV1(120), AQPMOV1(120),
     &                 DDPMOV1(120), QQPMOV1(120)
*Added by Luke Fiedler Jan 2013:
***********************************************************************
*
*  COMMON BLOCK FOR PMO2 METHOD
*
***********************************************************************
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
*Added by Luke Fiedler Jun 2014:
***********************************************************************
*
*  COMMON BLOCK FOR PMO2a METHOD
*
***********************************************************************
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
*Added by Luke Fiedler July 2009:
***********************************************************************
*
*  COMMON BLOCK FOR PM6 METHOD
*
***********************************************************************
      COMMON /PM6BLO/ USSPM6(120), UPPPM6(120), UDDPM6(120),ZSPM6(120), LF0709
     &ZPPM6(120), ZDPM6(120), BETASX(120), BETAPX(120), BETADX(120),    LF0709
     &GSSPM6(120),GSPPM6(120),GPPPM6(120),GP2PM6(120),HSPPM6(120),      LF0709
     &EISOLX(120), DDPM6(120), QQPM6(120), AMPM6(120), ADPM6(120),      LF0709
     &AQPM6(120), GAUSX1(120,10), GAUSX2(120,10), GAUSX3(120,10),       LF0709
     &ALPPM6(18,18), XPM6(18,18), ZSNPM6(18), ZPNPM6(18), ZDNPM6(18)    LF1009
*Added by ZJJ July 25 2006
***********************************************************************
*
*  COMMON BLOCKS FOR PDDG METHODS
*
***********************************************************************
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
*Added by Jingjing Zheng Aug 10 2006
***********************************************************************
*
*     COMMON BLOCKS FOR RM1
*
***********************************************************************
      COMMON /RM1BLO/USSRM1(120), UPPRM1(120), UDDRM1(120), ZSRM1(120),
     1ZPRM1(120), ZDRM1(120), BETASR(120), BETAPR(120), BETADR(120),
     2ALPRM1(120), EISOLR(120), DDRM1(120), QQRM1(120), AMRM1(120),
     3ADRM1(120), AQRM1(120) ,GSSRM1(120), GSPRM1(120), GPPRM1(120),
     4GP2RM1(120), HSPRM1(120),POLVOR(120)

***********************************************************************
*
*     COMMON BLOCKS FOR MNDO
*
***********************************************************************
      COMMON /MNDO/  USSM(120), UPPM(120), UDDM(120), ZSM(120),
     1ZPM(120), ZDM(120), BETASM(120), BETAPM(120), BETADM(120),
     2ALPM(120), EISOLM(120), DDM(120), QQM(120), AMM(120), ADM(120),
     3AQM(120) ,GSSM(120), GSPM(120), GPPM(120), GP2M(120), HSPM(120),
     4POLVOM(120)
***********************************************************************
*
*     COMMON BLOCKS FOR PM3
*
***********************************************************************
      COMMON /PM3 /  USSPM3(120), UPPPM3(120), UDDPM3(120), ZSPM3(120),
     1ZPPM3(120), ZDPM3(120), BETASP(120), BETAPP(120), BETADP(120),
     2ALPPM3(120), EISOLP(120), DDPM3(120), QQPM3(120), AMPM3(120),
     3ADPM3(120), AQPM3(120) ,GSSPM3(120), GSPPM3(120), GPPPM3(120),
     4GP2PM3(120), HSPPM3(120),POLVOP(120)
     5       /REFS/ REFMN(120), REFM3(120), REFAM(120), REFPM3(120),
     6REFPDG(120), REFMDG(120), REFRM(120), REFPMD(120),                LF0309
     7REFAMD(120), REFPM6(120), REFMND(120),REFRMD(120),REFP6D(120),    LF0310
     8REFPMOV1(120), REFD3(120), REFPMO2(120), REFPMO2A(120)            LF1010/LF1211/LF0113/LF0614
*
*  COMMON BLOCKS FOR MINDO/3
*
      COMMON /ONELE3 /  USS3(18),UPP3(18)
     1       /TWOEL3 /  F03(120)
     2       /ATOMI3 /  EISOL3(18),EHEAT3(18)
     3       /BETA3  /  BETA3(153)
     4       /ALPHA3 /  ALP3(153)
     5       /EXPON3 /  ZS3(18),ZP3(18)
*
*  END OF MINDO/3 COMMON BLOCKS
*
C
C ELECTRIC FIELD OPTIONS FOR POLARIZABILITY
C
      COMMON /FIELD/ EFIELD(3)
C COMMON BLOCK FOR PARAMETERS USED IN SEMIEMPIRICAL THEORY MODIFICATIONS
      include 'pmodsb.f'   ! Common block declaration for /PMODSB/.     LF1010
      include 'corgen.f'   ! Common block declaration for /CORGEN/.     LF1010
      DATA PMODVL / 2.466D0,     3.304D0,      0.03D0,        0.15D0,   LF0111
     &              1.280D0,     2.764D0,      0.24325D0,               LF0111
     &                                193*0.0D0 /                       LF0111
      DATA C0AB   / MAXELMSQ*0.0D0 /                                    LF0211
      DATA C1AB   / MAXELMSQ*1.0D0 /                                    LF0211
      DATA C2AB   / MAXELMSQ*1.0D0 /                                    LF0211
      DATA PRALP  / MAXELMSQ*0.0D0 /                                    LF0211
      DATA KPAIR  / KPAIRSZ *0.0D0 /                                    LF0211
      DATA C3AB   / MAXELMSQ*0.0D0 /                                    LF0211
      DATA C3RPWR / MAXELMSQ*0.0D0 /                                    LF0211
      DATA PR3ALP / MAXELMSQ*0.0D0 /                                    LF0211
C     Default values for PAIRBT need to be set elsewhere. (LF0211)

      CHARACTER ELEMNT*2, REFMN*80, REFM3*80, REFAM*80, REFPM3*80
      CHARACTER REFPDG*80, REFMDG*80,REFRM*80,REFPMD*80,                LF0309
     1          REFAMD*80, REFPM6*80, REFMND*80, REFRMD*80, REFP6D*80,  LF0310
     2          REFPMOV1*80, REFD3*80, REFPMO2*80, REFPMO2A*80          LF1010/LF1211/LF0113/LF0614
      DATA EFIELD/0.0D00,0.0D00,0.0D00/
      DATA ELEMNT/                                                      LF1110
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
C
C   NATORB IS THE NUMBER OF ATOMIC ORBITALS PER ATOM.
C
      DATA NATORB/                                                      LF1110
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
C      DATA NATORB/2*1, 4, 7*4, 0, 7*4, 0, 4, 9*9, 7*4,
C     &   2*4, 9*9, 7*4, 2*2, 14*8, 9*9, 7*4, 15*0,1,5*0,
C     &   4,   12*0/                                                     LF0610
C
C   PERIODIC TABLE OF THE ELEMENTS REPRODUCED HERE FOR CONVENIENCE: (WITH NATORB VALUES)
C
C  1A   2A   3B   4B   5B   6B   7B   8B   9B   10B  11B  12B  3A   4A   5A   6A   7A   8A
C
C  H                                                                                    He
C  1                                                                                     1 
C                                                                                          
C  Li   Be                                                      B    C    N    O    F   Ne
C  4     4                                                      4    4    4    4    4    4 
C                                                                                          
C  Na   Mg                                                     Al   Si    P    S   Cl   Ar
C  0     4                                                      4    4    4    4    4    4 
C                                                                                          
C  K    Ca   Sc   Ti    V   Cr   Mn   Fe   Co   Ni   Cu   Zn   Ga   Ge   As   Se   Br   Kr
C  0     4    9    9    9    9    9    9    9    9    9    4    4    4    4    4    4    4 
C                                                                                          
C  Rb   Sr    Y   Zr   Nb   Mo   Tc   Ru   Rh   Pd   Ag   Cd   In   Sn   Sb   Te    I   Xe
C   4    4    9    9    9    9    9    9    9    9    9    4    4    4    4    4    4    4 
C                                                                                          
C  Cs   Ba   Lu   Hf   Ta    W   Re   Os   Ir   Pt   Au   Hg   Tl   Pb   Bi   Po   At   Rn
C   2    2    9    9    9    9    9    9    9    9    9    4    4    4    4    4    4    4 
C                                                                                          
C  Fr   Ra   Lr   Rf   Db   Sg   Bh   Hs   Mt   Ds   Rg    *    *    *    *    *    *    *
C   0    0    0    0    0    0    0                                                        
C                                                   
C                                                   
C            La   Ce   Pr   Nd   Pm   Sm   Eu   Gd   Tb   Dy   Ho   Er   Tm   Yb      
C             8    8    8    8    8    8    8    8    8    8    8    8    8    8           
C                                                                                          
C            Ac   Th   Pa   U    Np   Pu   Am   Cm   Bk   Cf   Es   Fm   Md   No      
C             0    0    0   0     0    0    0    0    0    0    0    0    0    1           
C                                                                                          
C                                                                                          
C   PERIODIC TABLE OF THE ELEMENTS REPRODUCED HERE FOR CONVENIENCE: (WITH ATOMIC NUMBERS)
C
C  1A   2A   3B   4B   5B   6B   7B   8B   9B   10B  11B  12B  3A   4A   5A   6A   7A   8A
C
C  H                                                                                    He
C  1                                                                                     2 
C                                                                                          
C  Li   Be                                                      B    C    N    O    F   Ne
C  3     4                                                      5    6    7    8    9   10 
C                                                                                          
C  Na   Mg                                                     Al   Si    P    S   Cl   Ar
C  11   12                                                     13   14   15   16   17   18 
C                                                                                          
C  K    Ca   Sc   Ti    V   Cr   Mn   Fe   Co   Ni   Cu   Zn   Ga   Ge   As   Se   Br   Kr
C  19   20   21   22   23   24   25   26   27   28   29   30   31   32   33   34   35   36 
C                                                                                          
C  Rb   Sr    Y   Zr   Nb   Mo   Tc   Ru   Rh   Pd   Ag   Cd   In   Sn   Sb   Te    I   Xe
C  37   38   39   40   41   42   43   44   45   46   47   48   49   50   51   52   53   54 
C                                                                                          
C  Cs   Ba   Lu   Hf   Ta    W   Re   Os   Ir   Pt   Au   Hg   Tl   Pb   Bi   Po   At   Rn
C  55   56   71   72   73   74   75   76   77   78   79   80   81   82   83   84   85   86 
C                                                                                          
C  Fr   Ra   Lr   Rf   Db   Sg   Bh   Hs   Mt   Ds   Rg    *    *    *    *    *    *    *
C  87   88  103  104  105  106  107  108  109  110  111  112  113  114  115  116  117  118 
C
C                                                   
C            La   Ce   Pr   Nd   Pm   Sm   Eu   Gd   Tb   Dy   Ho   Er   Tm   Yb      
C            57   58   59   60   61   62   63   64   65   66   67   68   69   70           
C                                                                                          
C            Ac   Th   Pa   U    Np   Pu   Am   Cm   Bk   Cf   Es   Fm   Md   No      
C            89   90   91   92   93   94   95   96   97   98   99  100  101  102           
C                                                                                          
C
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
     518*0.000D0,   1.0079D0,  5*0.000D0,
     & 13*0.0D0/                                                        LF0710
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
     2  15*0.D0,1.D0,2.D0,1.D0,-2.D0,-1.D0,0.D0,
     & 1.D0, 12*0.D0/                                                   LF0210 / LF0710
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
      DATA EHEAT(3)  / 38.410D0/
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
      DATA EHEAT(102) / 207.0D0/
      DATA EHEAT(108) / 52.102D0/                                       LF0210
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
      DATA REFM3  ( 1)/'  H: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 5)/'  B: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 6)/'  C: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 7)/'  N: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 8)/'  O: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 9)/'  F: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  (14)/' Si: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  (15)/'  P: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  (16)/'  S: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  (17)/' Cl: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA F03              /  12.848D0, 10.0D0, 10.0D0, 0.0D0,
     1  8.958D0, 10.833D0, 12.377D0, 13.985D0, 16.250D0,
     2         10.000D0, 10.000D0, 0.000D0, 0.000D0,7.57D0 ,  9.00D0 ,
     3         10.20D0 , 11.73,10.0D0,35*0.D0,10.D0,53*10.D0,
     &         13*0.D0/                                                 LF0710
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
      DATA ALPPM3(103)      / 1.5D0/
      DATA EISOLP(103)    / 0.0D0/
      DATA AMPM3(103)       / 0.5D0/
*                               DATA FOR THE " + " SPARKLE
      DATA EHEAT(104)    / 0.0D0/
      DATA VS(104)       /10.0D0/
      DATA ALP(104)      / 1.5D0/
      DATA EISOL(104)    / 0.0D0/
      DATA AM(104)       / 0.5D0/
      DATA ALPM(104)      / 1.5D0/
      DATA EISOLM(104)    / 0.0D0/
      DATA AMM(104)       / 0.5D0/
      DATA ALPPM3(104)      / 1.5D0/
      DATA EISOLP(104)    / 0.0D0/
      DATA AMPM3(104)       / 0.5D0/
*                               DATA FOR THE " -- " SPARKLE
      DATA EHEAT(105)    / 0.0D0/
      DATA VS(105)       /10.0D0/
      DATA ALP(105)      / 1.5D0/
      DATA EISOL(105)    / 0.0D0/
      DATA AM(105)       / 0.5D0/
      DATA ALPM(105)      / 1.5D0/
      DATA EISOLM(105)    / 0.0D0/
      DATA AMM(105)       / 0.5D0/
      DATA ALPPM3(105)      / 1.5D0/
      DATA EISOLP(105)    / 0.0D0/
      DATA AMPM3(105)       / 0.5D0/
*                               DATA FOR THE " - " SPARKLE
      DATA EHEAT(106)    / 0.0D0/
      DATA VS(106)       /10.0D0/
      DATA ALP(106)      / 1.5D0/
      DATA EISOL(106)    / 0.0D0/
      DATA AM(106)       / 0.5D0/
      DATA ALPM(106)      / 1.5D0/
      DATA EISOLM(106)    / 0.0D0/
      DATA AMM(106)       / 0.5D0/
      DATA ALPPM3(106)      / 1.5D0/
      DATA EISOLP(106)    / 0.0D0/
      DATA AMPM3(106)       / 0.5D0/
*                             DATA FOR RM1
      DATA ALPRM1(103)   / 1.5D0/
      DATA EISOLR(103)   / 0.0D0/
      DATA AMRM1(103)    / 0.5D0/
      DATA ALPRM1(104)   / 1.5D0/
      DATA EISOLR(104)   / 0.0D0/
      DATA AMRM1(104)   / 0.5D0/      
      DATA ALPRM1(105)   / 1.5D0/
      DATA EISOLR(105)   / 0.0D0/
      DATA AMRM1(105)    / 0.5D0/      
      DATA ALPRM1(106)   / 1.5D0/
      DATA EISOLR(106)   / 0.0D0/
      DATA AMRM1(106)    / 0.5D0/      
***********************************************************************
*
*    START OF MNDO PARAMETERS
*
*
*                              Units
*     USSM,   UPPM,   UDDM:    eV
*     BETASM, BETAPM, BETADM:  eV
*     ALPM:                    angstrom**(-1)
*     ZSM,    ZPM,    ZDM:     bohr**(-1)
*     GSSM,   GSPM,   GPPM:    eV          ! see above in file for these
*     GP2M,   HSPM:            eV          ! see above in file for these
*     EISOLM:                  eV
*     AMM,    ADM,    AQM:     bohr
*     DDM,    QQM:             bohr
*
***********************************************************************
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFMN  ( 1)/'  H: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHE
     1M. SOC., 99, 4899, (1977)       '/
      DATA USSM   ( 1)/     -11.9062760D0/
      DATA BETASM ( 1)/      -6.9890640D0/
      DATA ZSM    ( 1)/       1.3319670D0/
      DATA ALPM   ( 1)/       2.5441341D0/
      DATA EISOLM ( 1)/     -11.9062760D0/
      DATA AMM    ( 1)/       0.4721793D0/
      DATA ADM    ( 1)/       0.4721793D0/
      DATA AQM    ( 1)/       0.4721793D0/
C                    DATA FOR ELEMENT  3        LITHIUM
      DATA REFMN  ( 3)/' Li: (MNDO):  TAKEN FROM MNDOC BY W.THIEL,
     1QCPE NO.438, V. 2, P.63, (1982).'/
      DATA USSM   (  3)/      -5.1280000D0/
      DATA UPPM   (  3)/      -2.7212000D0/
      DATA BETASM (  3)/      -1.3500400D0/
      DATA BETAPM (  3)/      -1.3500400D0/
      DATA ZSM    (  3)/       0.7023800D0/
      DATA ZPM    (  3)/       0.7023800D0/
      DATA ALPM   (  3)/       1.2501400D0/
      DATA EISOLM (  3)/      -5.1280000D0/
      DATA GSSM   (  3)/       7.3000000D0/
      DATA GSPM   (  3)/       5.4200000D0/
      DATA GPPM   (  3)/       5.0000000D0/
      DATA GP2M   (  3)/       4.5200000D0/
      DATA HSPM   (  3)/       0.8300000D0/
      DATA DDM    (  3)/       2.0549783D0/
      DATA QQM    (  3)/       1.7437069D0/
      DATA AMM    (  3)/       0.2682837D0/
      DATA ADM    (  3)/       0.2269793D0/
      DATA AQM    (  3)/       0.2614581D0/
C                    DATA FOR ELEMENT  4        BERYLLIUM
      DATA REFMN  ( 4)/' Be: (MNDO):  M.J.S. DEWAR, H.S. RZEPA, J. AM. C
     1HEM. SOC., 100, 777, (1978)     '/
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
      DATA REFMN  ( 5)/'  B: (MNDO):  M.J.S. DEWAR, M.L. MCKEE, J. AM. C
     1HEM. SOC., 99, 5231, (1977)     '/
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
      DATA REFMN  ( 6)/'  C: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHE
     1M. SOC., 99, 4899, (1977)       '/
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
      DATA REFMN  ( 7)/'  N: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHE
     1M. SOC., 99, 4899, (1977)       '/
      DATA USSM   ( 7)/     -71.9321220D0/
      DATA UPPM   ( 7)/     -57.1723190D0/
      DATA BETASM ( 7)/     -20.4957580D0/
      DATA BETAPM ( 7)/     -20.4957580D0/
      DATA ZSM    ( 7)/       2.2556140D0/
      DATA ZPM    ( 7)/       2.2556140D0/
      DATA ALPM   ( 7)/       2.8613420D0/
      DATA EISOLM ( 7)/    -202.5662010D0/
      DATA DDM    ( 7)/       0.6399037D0/
      DATA QQM    ( 7)/       0.5429763D0/
      DATA AMM    ( 7)/       0.4994487D0/
      DATA ADM    ( 7)/       0.7843643D0/
      DATA AQM    ( 7)/       0.8126445D0/
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFMN  ( 8)/'  O: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHE
     1M. SOC., 99, 4899, (1977)       '/
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
      DATA REFMN  ( 9)/'  F: (MNDO):  M.J.S. DEWAR, H.S. RZEPA, J. AM. C
     1HEM. SOC., 100, 777, (1978)     '/
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
      DATA REFMN  (11)/' Na: (MNDO):  SODIUM-LIKE SPARKLE.   USE WITH CA
     1RE.                             '/
      DATA EHEAT(11)    / 0.0D0/
      DATA VS(11)       /10.0D0/
      DATA ALP(11)      / 1.32D0/
      DATA EISOL(11)    / 0.0D0/
      DATA AM(11)       / 0.5D0/
      DATA ALPM(11)      / 1.32D0/
      DATA EISOLM(11)    / 0.0D0/
      DATA AMM(11)       / 0.5D0/
C                    DATA FOR ELEMENT 13        ALUMINUM
      DATA REFMN  (13)/' Al: (MNDO):  L.P. DAVIS, ET.AL.  J. COMP. CHEM.
     1, 2, 433, (1981) SEE MANUAL.    '/
      DATA USSM   (13)/     -23.8070970D0/
      DATA UPPM   (13)/     -17.5198780D0/
      DATA BETASM (13)/      -2.6702840D0/
      DATA BETAPM (13)/      -2.6702840D0/
      DATA ZSM    (13)/       1.4441610D0/
      DATA ZPM    (13)/       1.4441610D0/
      DATA ZDM    (13)/       1.0000000D0/
      DATA ALPM   (13)/       1.8688394D0/
      DATA EISOLM (13)/     -44.4840720D0/
      DATA DDM    (13)/       1.3992387D0/
      DATA QQM    (13)/       1.1586797D0/
      DATA AMM    (13)/       0.2973172D0/
      DATA ADM    (13)/       0.2635574D0/
      DATA AQM    (13)/       0.3673560D0/
C                    DATA FOR ELEMENT 14          SILICON
      DATA REFMN  (14)/' Si: (MNDO): M.J.S.DEWAR, ET. AL. ORGANOMETALLIC
     1S  5, 375 (1986)                '/
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
      DATA REFMN  (15)/'  P: (MNDO): M.J.S.DEWAR, M.L.MCKEE, H.S.RZEPA,
     1J. AM. CHEM. SOC., 100 3607 1978'/
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
C
      DATA REFMN  (16)/'  S: (MNDO): M.J.S.DEWAR, C.H. REYNOLDS, J. COM
     1P. CHEM. 7, 140-143 (1986)      '/
      DATA USSM   (16)/     -72.2422810D0/
      DATA UPPM   (16)/     -56.9732070D0/
      DATA BETASM (16)/     -10.7616700D0/
      DATA BETAPM (16)/     -10.1084330D0/
      DATA ZSM    (16)/       2.3129620D0/
      DATA ZPM    (16)/       2.0091460D0/
      DATA ZDM    (16)/       1.0000000D0/
      DATA ALPM   (16)/       2.4780260D0/
      DATA EISOLM (16)/    -226.0123900D0/
      DATA DDM    (16)/       0.9189935D0/
      DATA QQM    (16)/       0.8328514D0/
      DATA AMM    (16)/       0.4733554D0/
      DATA ADM    (16)/       0.5544502D0/
      DATA AQM    (16)/       0.5585244D0/
C                    DATA FOR ELEMENT 17        CHLORINE
      DATA REFMN  (17)/' Cl: (MNDO): M.J.S.DEWAR, H.S.RZEPA, J. COMP. CH
     1EM., 4, 158, (1983)             '/
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
      DATA REFMN  (19)/'  K: (MNDO):  POTASSIUM-LIKE SPARKLE.  USE WITH
     1CARE.                           '/
*                               DATA FOR THE POTASSIUM-LIKE SPARKLE
      DATA REFAM  (19)/' K:  (AM1):  POTASSIUM-LIKE SPARKLE.   USE WITH
     1 CARE.                          '/
      DATA EHEAT(19)    / 0.0D0/
      DATA VS(19)       /10.0D0/
      DATA ALP(19)      / 1.16D0/
      DATA EISOL(19)    / 0.0D0/
      DATA AM(19)       / 0.5D0/
      DATA ALPM(19)      / 1.16D0/
      DATA EISOLM(19)    / 0.0D0/
      DATA AMM(19)       / 0.5D0/
C                    DATA FOR ELEMENT 24  CHROMIUM
      DATA REFMN  (24)/' Cr: (MNDO):  M.J.S. DEWAR, E.F. HEALY, J.J.P.
     1STEWART (IN PREPARATION)        '/
      DATA USSM   (24)/     -17.5170270D0/
      DATA UPPM  (24)/     -12.5337290D0/
      DATA UDDM  (24)/     -44.1249280D0/
      DATA BETASM (24)/      -0.1000000D0/
      DATA BETAPM (24)/      -0.1000000D0/
      DATA BETADM (24)/      -8.7766360D0/
      DATA ZSM    (24)/       1.5000000D0/
      DATA ZPM    (24)/       1.5000000D0/
      DATA ZDM    (24)/       2.8845490D0/
      DATA ALPM   (24)/       3.0683070D0/
      DATA EISOLM (24)/    -134.8187920D0/
      DATA GSSM   (24)/       6.0000000D0/
      DATA GSPM   (24)/       4.1500000D0/
      DATA GPPM   (24)/       5.0000000D0/
      DATA GP2M   (24)/       3.5000000D0/
      DATA HSPM   (24)/       1.0000000D0/
      DATA GSD    (24)/       2.8746410D0/
      DATA GPD    (24)/       3.0000000D0/
      DATA GDD    (24)/       8.8949670D0/
      DATA DDM    (24)/       1.7320508D0/
      DATA QQM    (24)/       1.4142136D0/
      DATA AMM    (24)/       0.2205072D0/
      DATA ADM    (24)/       0.2711332D0/
      DATA AQM    (24)/       0.4464656D0/
C                    DATA FOR ELEMENT 30        ZINC
      DATA REFMN  (30)/' Zn: (MNDO):  M.J.S. DEWAR, K.M. MERZ, ORGANOMET
     1ALLICS, 5, 1494-1496 (1986)     '/
C                    DATA FOR ELEMENT 30
      DATA USSM  ( 30)/     -20.8397160D0/
      DATA UPPM  ( 30)/     -19.6252240D0/
      DATA BETASM( 30)/      -1.0000000D0/
      DATA BETAPM( 30)/      -2.0000000D0/
      DATA ZSM   ( 30)/       2.0473590D0/
      DATA ZPM   ( 30)/       1.4609460D0/
      DATA ZDM   ( 30)/       1.0000000D0/
      DATA ALPM  ( 30)/       1.5064570D0/
      DATA EISOLM( 30)/     -29.8794320D0/
      DATA GSSM  ( 30)/      11.8000000D0/
      DATA GSPM  ( 30)/      11.1820180D0/
      DATA GPPM  ( 30)/      13.3000000D0/
      DATA GP2M  ( 30)/      12.9305200D0/
      DATA HSPM  ( 30)/       0.4846060D0/
      DATA DDM   ( 30)/       1.3037826D0/
      DATA QQM   ( 30)/       1.4520183D0/
      DATA AMM   ( 30)/       0.4336641D0/
      DATA ADM   ( 30)/       0.2375912D0/
      DATA AQM   ( 30)/       0.2738858D0/
C                    DATA FOR ELEMENT 32        GERMANIUM
      DATA REFMN  (32)/' Ge: (MNDO): M.J.S.DEWAR, G.L.GRADY, E.F.HEALY,O
     1RGANOMETALLICS 6 186-189, (1987)'/
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
      DATA REFMN  (35)/' Br: (MNDO): M.J.S.DEWAR, E.F. HEALY, J. COMP. C
     1HEM., 4, 542, (1983)            '/
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
      DATA REFMN  (50)/' Sn: (MNDO): M.J.S.DEWAR,G.L.GRADY,J.J.P.STEWART
     1, J.AM.CHEM.SOC.,106 6771 (1984)'/
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
      DATA REFMN  (53)/'  I: (MNDO): M.J.S.DEWAR, E.F. HEALY, J.J.P. STE
     1WART, J.COMP.CHEM., 5,358,(1984)'/
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
      DATA REFMN  (80)/' Hg: (MNDO): M.J.S.DEWAR,  ET. AL. ORGANOMETALLI
     1CS 4, 1964, (1985) SEE MANUAL   '/
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
      DATA REFMN  (82)/' Pb: (MNDO): M.J.S.DEWAR, ET.AL ORGANOMETALLICS
     14 1973-1980 (1985)              '/
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
C
C     START OF "OLD" ELEMENTS: THESE ARE OLD PARAMETERS WHICH
C     CAN BE USED, IF DESIRED, BY SPECIFYING "<CHEMICAL SYMBOL>YEAR"
C     AS IN SI1978 OR  S1983.
C
C                    DATA FOR ELEMENT 90        SILICON
      DATA REFMN  (90)/' Si: (MNDO): M.J.S.DEWAR, M.L.MCKEE, H.S.RZEPA,
     1J. AM. CHEM. SOC., 100 3607 1978'/
      DATA USSM   (90)/     -40.5682920D0/
      DATA UPPM   (90)/     -28.0891870D0/
      DATA BETASM (90)/      -4.2562180D0/
      DATA BETAPM (90)/      -4.2562180D0/
      DATA ZSM    (90)/       1.4353060D0/
      DATA ZPM    (90)/       1.4353060D0/
      DATA ZDM    (90)/       1.0000000D0/
      DATA ALPM   (90)/       2.1961078D0/
      DATA EISOLM (90)/     -90.5399580D0/
      DATA DDM    (90)/       1.4078712D0/
      DATA QQM    (90)/       1.1658281D0/
      DATA AMM    (90)/       0.3608967D0/
      DATA ADM    (90)/       0.3441817D0/
      DATA AQM    (90)/       0.3999442D0/
      DATA HSPM   (90)/       1.32D00/
      DATA GP2M   (90)/       6.54D00/
      DATA GPPM   (90)/       7.31D00/
      DATA GSPM   (90)/       8.36D00/
      DATA GSSM   (90)/       9.82D00/
      DATA REFMN  (91)/'  S: (MNDO): M.J.S.DEWAR, H.S. RZEPA, M.L.MCKEE,
     1 J.AM.CHEM.SOC.100, 3607 (1978).'/
      DATA USSM   (91)/     -75.2391520D0/
      DATA UPPM   (91)/     -57.8320130D0/
      DATA BETASM (91)/     -11.1422310D0/
      DATA BETAPM (91)/     -11.1422310D0/
      DATA ZSM    (91)/       2.6135910D0/
      DATA ZPM    (91)/       2.0343930D0/
      DATA ZDM    (91)/       1.0000000D0/
      DATA ALPM   (91)/       2.4916445D0/
      DATA EISOLM (91)/    -235.4413560D0/
      DATA DDM    (91)/       0.8231596D0/
      DATA QQM    (91)/       0.8225156D0/
      DATA AMM    (91)/       0.4733554D0/
      DATA ADM    (91)/       0.5889395D0/
      DATA AQM    (91)/       0.5632724D0/
      DATA REFMN (102)/' Cb: (MNDO):  Capped Bond  (Hydrogen-like, takes
     1 on a  zero charge.)            '/
      DATA USSM  (102)/     -11.9062760D0/
      DATA BETASM(102)/-9999999.0000000D0/
      DATA ZSM   (102)/       4.0000000D0/
      DATA ZPM   (102)/       0.3000000D0/
      DATA ZDM   (102)/       0.3000000D0/
      DATA ALPM  (102)/       2.5441341D0/
      DATA EISOLM(102)/       4.0000000D0/
      DATA GSSM  (102)/      12.8480000D0/
      DATA HSPM  (102)/       0.1000000D0/
      DATA DDM   (102)/       0.0684105D0/
      DATA QQM   (102)/       1.0540926D0/
      DATA AMM   (102)/       0.4721793D0/
      DATA ADM   (102)/       0.9262742D0/
      DATA AQM   (102)/       0.2909059D0/
C
C     LF0410: SPECIAL HYDROGEN CONTAINING P ORBITALS.  CAN BE USED BY SPECIFYING
C     "Hp".  ADDED BY LUKE FIEDLER, APR. 2010.
C           DATA FOR ELEMENT  108     MNDO:  HYDROGEN (WITH P-ORBITALS)
      DATA REFMN (108)/' Hp: (MNDO): Hydrogen (with p basis functions)  
     1                              '/
      DATA USSM  (108)/      -7.3964270D0/
      DATA UPPM  (108)/      -1.1486568D0/
      DATA BETASM(108)/      -9.6737870D0/
      DATA BETAPM(108)/      -1.3000000D0/
      DATA ZSM   (108)/       0.9880780D0/
      DATA ZPM   (108)/       0.3000000D0/
      DATA ALPM  (108)/       2.9323240D0/
      DATA EISOLM(108)/      28.7118620D0/
      DATA GSSM  (108)/      12.8480000D0/
      DATA GSPM  (108)/       2.1600000D0/
      DATA GPPM  (108)/       3.0100000D0/
      DATA GP2M  (108)/       2.4400000D0/
      DATA HSPM  (108)/       1.9200000D0/
      DATA DDM   (108)/       0.9675816D0/
      DATA QQM   (108)/       4.0824829D0/
      DATA AMM   (108)/       0.4721603D0/
      DATA ADM   (108)/       0.5007132D0/
      DATA AQM   (108)/       0.1603621D0/
C
***********************************************************************
*
*    START OF AM1 PARAMETERS
*
*    Reference  #1:  "Development and use of quantum mechanical molecular
*                     models. 76. AM1: a new general purpose quantum
*                     mechanical molecular model"
*                  Michael J. S. Dewar, Eve G. Zoebisch, 
*                  Eamonn F. Healy, James J. P. Stewart
*                  J. Amer. Chem. Soc., 107(13), 3902-3909 (June 1985).
*
*                              Units
*     USS,    UPP,    UDD:     eV
*     BETAS,  BETAP,  BETAD:   eV
*     ALP:                     angstrom**(-1)
*     ZS,     ZP,     ZD:      bohr**(-1)
*     GSS,    GSP,    GPP:     eV 
*     GP2,    HSP:             eV
*     EISOL:                   eV
*     GAUSS1:                  eV * angstrom 
*     GAUSS2:                  angstrom**(-2)
*     GAUSS3:                  angstrom
*     AM,    AD,    AQ:        bohr
*     DD,    QQ:               bohr
*
***********************************************************************
C                    DATA FOR ELEMENT  1       AM1:   HYDROGEN
      DATA REFAM  ( 1)/'  H: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC
     1. 107 3902-3909 (1985)          '/
      DATA USS   ( 1)/     -11.3964270D0/
      DATA BETAS ( 1)/      -6.1737870D0/
      DATA ZS    ( 1)/       1.1880780D0/
      DATA ALP   ( 1)/       2.8823240D0/
      DATA EISOL ( 1)/     -11.3964270D0/
      DATA GSS   ( 1)/      12.8480000D0/
      DATA AM    ( 1)/       0.4721793D0/
      DATA AD    ( 1)/       0.4721793D0/
      DATA AQ    ( 1)/       0.4721793D0/
      DATA GAUSS1( 1,1)/       0.1227960D0/
      DATA GAUSS2( 1,1)/       5.0000000D0/
      DATA GAUSS3( 1,1)/       1.2000000D0/
      DATA GAUSS1( 1,2)/       0.0050900D0/
      DATA GAUSS2( 1,2)/       5.0000000D0/
      DATA GAUSS3( 1,2)/       1.8000000D0/
      DATA GAUSS1( 1,3)/      -0.0183360D0/
      DATA GAUSS2( 1,3)/       2.0000000D0/
      DATA GAUSS3( 1,3)/       2.1000000D0/
C                    DATA FOR ELEMENT  3       AM1:   LITHIUM    *
      DATA REFAM  ( 3)/' Li: (MNDO):  TAKEN FROM MNDOC BY W.THIEL,
     1QCPE NO.438, V. 2, P.63, (1982).'/
      DATA USS   (  3)/      -5.1280000D0/
      DATA UPP   (  3)/      -2.7212000D0/
      DATA BETAS (  3)/      -1.3500400D0/
      DATA BETAP (  3)/      -1.3500400D0/
      DATA ZS    (  3)/       0.7023800D0/
      DATA ZP    (  3)/       0.7023800D0/
      DATA ALP   (  3)/       1.2501400D0/
      DATA EISOL (  3)/      -5.1280000D0/
      DATA GSS   (  3)/       7.3000000D0/
      DATA GSP   (  3)/       5.4200000D0/
      DATA GPP   (  3)/       5.0000000D0/
      DATA GP2   (  3)/       4.5200000D0/
      DATA HSP   (  3)/       0.8300000D0/
      DATA DD    (  3)/       2.0549783D0/
      DATA QQ    (  3)/       1.7437069D0/
      DATA AM    (  3)/       0.2682837D0/
      DATA AD    (  3)/       0.2269793D0/
      DATA AQ    (  3)/       0.2614581D0/
C                    DATA FOR ELEMENT  4       AM1:   BERYLLIUM  *
      DATA REFAM  ( 4)/' Be: (MNDO):  M.J.S. DEWAR, H.S. RZEPA, J. AM. C
     1HEM. SOC., 100, 777, (1978)     '/
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
C                    DATA FOR ELEMENT  5       AM1:   BORON  *
      DATA REFAM  ( 5)/'  B: (AM1):  M.J.S. DEWAR, C. JIE, E. G. ZOEBISC
     1H ORGANOMETALLICS 7, 513 (1988) '/
C                    DATA FOR ELEMENT  5
      DATA USS   (  5)/     -34.4928700D0/
      DATA UPP   (  5)/     -22.6315250D0/
      DATA BETAS (  5)/      -9.5991140D0/
      DATA BETAP (  5)/      -6.2737570D0/
      DATA ZS    (  5)/       1.6117090D0/
      DATA ZP    (  5)/       1.5553850D0/
      DATA ALP   (  5)/       2.4469090D0/
      DATA EISOL (  5)/     -63.7172650D0/
      DATA GSS   (  5)/      10.5900000D0/
      DATA GSP   (  5)/       9.5600000D0/
      DATA GPP   (  5)/       8.8600000D0/
      DATA GP2   (  5)/       7.8600000D0/
      DATA HSP   (  5)/       1.8100000D0/
      DATA DD    (  5)/       0.9107622D0/
      DATA QQ    (  5)/       0.7874223D0/
      DATA AM    (  5)/       0.3891951D0/
      DATA AD    (  5)/       0.5045152D0/
      DATA AQ    (  5)/       0.5678856D0/
C                    DATA FOR ELEMENT  6       AM1:   CARBON
      DATA REFAM  ( 6)/'  C: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC
     1. 107 3902-3909 (1985)          '/
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
      DATA GAUSS1( 6,1)/       0.0113550D0/
      DATA GAUSS2( 6,1)/       5.0000000D0/
      DATA GAUSS3( 6,1)/       1.6000000D0/
      DATA GAUSS1( 6,2)/       0.0459240D0/
      DATA GAUSS2( 6,2)/       5.0000000D0/
      DATA GAUSS3( 6,2)/       1.8500000D0/
      DATA GAUSS1( 6,3)/      -0.0200610D0/
      DATA GAUSS2( 6,3)/       5.0000000D0/
      DATA GAUSS3( 6,3)/       2.0500000D0/
      DATA GAUSS1( 6,4)/      -0.0012600D0/
      DATA GAUSS2( 6,4)/       5.0000000D0/
      DATA GAUSS3( 6,4)/       2.6500000D0/
C                    DATA FOR ELEMENT  7       AM1:   NITROGEN
      DATA REFAM  ( 7)/'  N: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC
     1. 107 3902-3909 (1985)          '/
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
      DATA GAUSS1( 7,1)/       0.0252510D0/
      DATA GAUSS2( 7,1)/       5.0000000D0/
      DATA GAUSS3( 7,1)/       1.5000000D0/
      DATA GAUSS1( 7,2)/       0.0289530D0/
      DATA GAUSS2( 7,2)/       5.0000000D0/
      DATA GAUSS3( 7,2)/       2.1000000D0/
      DATA GAUSS1( 7,3)/      -0.0058060D0/
      DATA GAUSS2( 7,3)/       2.0000000D0/
      DATA GAUSS3( 7,3)/       2.4000000D0/
C                    DATA FOR ELEMENT  8       AM1:   OXYGEN
      DATA REFAM  ( 8)/'  O: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC
     1. 107 3902-3909 (1985)          '/
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
      DATA GAUSS1( 8,1)/       0.2809620D0/
      DATA GAUSS2( 8,1)/       5.0000000D0/
      DATA GAUSS3( 8,1)/       0.8479180D0/
      DATA GAUSS1( 8,2)/       0.0814300D0/
      DATA GAUSS2( 8,2)/       7.0000000D0/
      DATA GAUSS3( 8,2)/       1.4450710D0/
C                    DATA FOR ELEMENT  9       AM1:   FLUORINE  *
C
C THE FOLLOWING NUMBERS ARE PARAMETRIZED AT THE AM1 LEVEL, BUT
C ARE NOT YET PUBLISHED.  WHEN PUBLISHED UNCOMMENT THE FOLLOWING
C BLOCK, AND REMOVE THE PRECEDING SET OF PARAMETERS.
      DATA REFAM  ( 9)/'  F: (AM1): DEWAR AND ZOEBISCH, UNPUBLISHED WORK
     1                                '/
      DATA USS   ( 9)/    -136.1055790D0/
      DATA UPP   ( 9)/    -104.8898850D0/
      DATA BETAS ( 9)/     -69.5902770D0/
      DATA BETAP ( 9)/     -27.9223600D0/
      DATA ZS    ( 9)/       3.7700820D0/
      DATA ZP    ( 9)/       2.4946700D0/
      DATA ALP   ( 9)/       5.5178000D0/
      DATA EISOL ( 9)/    -482.2905830D0/
      DATA GSS   ( 9)/      16.9200000D0/
      DATA GSP   ( 9)/      17.2500000D0/
      DATA GPP   ( 9)/      16.7100000D0/
      DATA GP2   ( 9)/      14.9100000D0/
      DATA HSP   ( 9)/       4.8300000D0/
      DATA DD    ( 9)/       0.4145203D0/
      DATA QQ    ( 9)/       0.4909446D0/
      DATA AM    ( 9)/       0.6218302D0/
      DATA AD    ( 9)/       1.2088792D0/
      DATA AQ    ( 9)/       0.9449355D0/
      DATA GAUSS1( 9,1)/       0.2420790D0/
      DATA GAUSS2( 9,1)/       4.8000000D0/
      DATA GAUSS3( 9,1)/       0.9300000D0/
      DATA GAUSS1( 9,2)/       0.0036070D0/
      DATA GAUSS2( 9,2)/       4.6000000D0/
      DATA GAUSS3( 9,2)/       1.6600000D0/
*                               DATA FOR THE SODIUM-LIKE SPARKLE
      DATA REFAM  (11)/' Na: (MNDO):  SODIUM-LIKE SPARKLE.   USE WITH CA
     1RE.                             '/
C                    DATA FOR ELEMENT 13       AM1:   ALUMINUM  *
      DATA REFAM  (13)/' Al: (AM1):  M. J. S. Dewar, A. J. Holder, Organ
     1ometallics, 9, 508-511 (1990).  '/
      DATA USS   ( 13)/     -24.3535850D0/
      DATA UPP   ( 13)/     -18.3636450D0/
      DATA BETAS ( 13)/      -3.8668220D0/
      DATA BETAP ( 13)/      -2.3171460D0/
      DATA ZS    ( 13)/       1.5165930D0/
      DATA ZP    ( 13)/       1.3063470D0/
      DATA ZD    ( 13)/       1.0000000D0/
      DATA ALP   ( 13)/       1.9765860D0/
      DATA EISOL ( 13)/     -46.4208150D0/
      DATA GSS   ( 13)/       8.0900000D0/
      DATA GSP   ( 13)/       6.6300000D0/
      DATA GPP   ( 13)/       5.9800000D0/
      DATA GP2   ( 13)/       5.4000000D0/
      DATA HSP   ( 13)/       0.7000000D0/
      DATA DD    ( 13)/       1.4040443D0/
      DATA QQ    ( 13)/       1.2809154D0/
      DATA AM    ( 13)/       0.2973172D0/
      DATA AD    ( 13)/       0.2630229D0/
      DATA AQ    ( 13)/       0.3427832D0/
      DATA GAUSS1( 13,1)/       0.0900000D0/
      DATA GAUSS2( 13,1)/      12.3924430D0/
      DATA GAUSS3( 13,1)/       2.0503940D0/
C                    DATA FOR ELEMENT 14       AM1:   SILICON  *
      DATA REFAM  (14)/' Si: (AM1): M.J.S.DEWAR, C. JIE, ORGANOMETALLICS
     1, 6, 1486-1490 (1987).          '/
      DATA USS   (14)/     -33.9536220D0/
      DATA UPP   (14)/     -28.9347490D0/
      DATA BETAS (14)/      -3.784852D0/
      DATA BETAP (14)/      -1.968123D0/
      DATA ZS    (14)/       1.830697D0/
      DATA ZP    (14)/       1.2849530D0/
      DATA ZD    (14)/       1.0000000D0/
      DATA ALP   (14)/       2.257816D0/
      DATA EISOL (14)/     -79.0017420D0/
      DATA GSS   (14)/       9.8200000D0/
      DATA GSP   (14)/       8.3600000D0/
      DATA GPP   (14)/       7.3100000D0/
      DATA GP2   (14)/       6.5400000D0/
      DATA HSP   (14)/       1.3200000D0/
      DATA DD    (14)/       1.1631107D0/
      DATA QQ    (14)/       1.3022422D0/
      DATA AM    (14)/       0.3608967D0/
      DATA AD    (14)/       0.3829813D0/
      DATA AQ    (14)/       0.3712106D0/
      DATA GAUSS1(14,1)/       0.25D0/
      DATA GAUSS2(14,1)/       9.000D0/
      DATA GAUSS3(14,1)/       0.911453D0/
      DATA GAUSS1(14,2)/       0.061513D0/
      DATA GAUSS2(14,2)/       5.00D0/
      DATA GAUSS3(14,2)/       1.995569D0/
      DATA GAUSS1(14,3)/       0.0207890D0/
      DATA GAUSS2(14,3)/       5.00D0/
      DATA GAUSS3(14,3)/       2.990610D0/
C                    DATA FOR ELEMENT 15        PHOSPHORUS
      DATA REFAM  (15)/'  P: (AM1): M.J.S.DEWAR, JIE, C, THEOCHEM, 187,
     11 (1989)                        '/
      DATA USS   ( 15)/     -42.0298630D0/
      DATA UPP   ( 15)/     -34.0307090D0/
      DATA BETAS ( 15)/      -6.3537640D0/
      DATA BETAP ( 15)/      -6.5907090D0/
      DATA ZS    ( 15)/       1.9812800D0/
      DATA ZP    ( 15)/       1.8751500D0/
      DATA ZD    ( 15)/       1.0000000D0/
      DATA ALP   ( 15)/       2.4553220D0/
      DATA EISOL ( 15)/    -124.4368355D0/
      DATA GSS   ( 15)/      11.5600050D0/
      DATA GSP   ( 15)/       5.2374490D0/
      DATA GPP   ( 15)/       7.8775890D0/
      DATA GP2   ( 15)/       7.3076480D0/
      DATA HSP   ( 15)/       0.7792380D0/
      DATA DD    ( 15)/       1.0452022D0/
      DATA QQ    ( 15)/       0.8923660D0/
      DATA AM    ( 15)/       0.4248440D0/
      DATA AD    ( 15)/       0.3275319D0/
      DATA AQ    ( 15)/       0.4386854D0/
      DATA GAUSS1( 15,1)/      -0.0318270D0/
      DATA GAUSS2( 15,1)/       6.0000000D0/
      DATA GAUSS3( 15,1)/       1.4743230D0/
      DATA GAUSS1( 15,2)/       0.0184700D0/
      DATA GAUSS2( 15,2)/       7.0000000D0/
      DATA GAUSS3( 15,2)/       1.7793540D0/
      DATA GAUSS1( 15,3)/       0.0332900D0/
      DATA GAUSS2( 15,3)/       9.0000000D0/
      DATA GAUSS3( 15,3)/       3.0065760D0/
C                    DATA FOR ELEMENT 16       AM1:   SULFUR  *
C
      DATA REFAM  (16)/'  S: (AM1): M.J.S.DEWAR, Y-C YUAN, THEOCHEM, IN
     1 PRESS                          '/
      DATA USS   (16)/     -56.6940560D0/
      DATA UPP   (16)/     -48.7170490D0/
      DATA BETAS (16)/      -3.9205660D0/
      DATA BETAP (16)/      -7.9052780D0/
      DATA ZS    (16)/       2.3665150D0/
      DATA ZP    (16)/       1.6672630D0/
      DATA ZD    (16)/       1.0000000D0/
      DATA ALP   (16)/       2.4616480D0/
      DATA EISOL (16)/    -191.7321930D0/
      DATA GSS   (16)/      11.7863290D0/
      DATA GSP   (16)/       8.6631270D0/
      DATA GPP   (16)/      10.0393080D0/
      DATA GP2   (16)/       7.7816880D0/
      DATA HSP   (16)/       2.5321370D0/
      DATA DD    (16)/       0.9004265D0/
      DATA QQ    (16)/       1.0036329D0/
      DATA AM    (16)/       0.4331617D0/
      DATA AD    (16)/       0.5907115D0/
      DATA AQ    (16)/       0.6454943D0/
      DATA GAUSS1(16,1)/      -0.5091950D0/
      DATA GAUSS2(16,1)/       4.5936910D0/
      DATA GAUSS3(16,1)/       0.7706650D0/
      DATA GAUSS1(16,2)/      -0.0118630D0/
      DATA GAUSS2(16,2)/       5.8657310D0/
      DATA GAUSS3(16,2)/       1.5033130D0/
      DATA GAUSS1(16,3)/       0.0123340D0/
      DATA GAUSS2(16,3)/      13.5573360D0/
      DATA GAUSS3(16,3)/       2.0091730D0/
C                    DATA FOR ELEMENT 17       AM1:   CHLORINE  *
C
C THE FOLLOWING NUMBERS ARE PARAMETRIZED AT THE AM1 LEVEL, BUT
C ARE NOT YET PUBLISHED.  WHEN PUBLISHED UNCOMMENT THE FOLLOWING
C BLOCK, AND REMOVE THE PRECEDING SET OF PARAMETERS.
      DATA REFAM  (17)/' Cl: (AM1): DEWAR AND ZOEBISCH, UNPUBLISHED WORK
     1                                '/
      DATA USS   (17)/    -111.6139480D0/
      DATA UPP   (17)/     -76.6401070D0/
      DATA BETAS (17)/     -24.5946700D0/
      DATA BETAP (17)/     -14.6372160D0/
      DATA ZS    (17)/       3.6313760D0/
      DATA ZP    (17)/       2.0767990D0/
      DATA ZD    (17)/       1.0000000D0/
      DATA ALP   (17)/       2.9193680D0/
      DATA EISOL (17)/    -372.1984310D0/
      DATA GSS   (17)/      15.0300000D0/
      DATA GSP   (17)/      13.1600000D0/
      DATA GPP   (17)/      11.3000000D0/
      DATA GP2   (17)/       9.9700000D0/
      DATA HSP   (17)/       2.4200000D0/
      DATA DD    (17)/       0.5406286D0/
      DATA QQ    (17)/       0.8057208D0/
      DATA AM    (17)/       0.5523705D0/
      DATA AD    (17)/       0.7693200D0/
      DATA AQ    (17)/       0.6133369D0/
      DATA GAUSS1(17,1)/       0.0942430D0/
      DATA GAUSS2(17,1)/       4.0000000D0/
      DATA GAUSS3(17,1)/       1.3000000D0/
      DATA GAUSS1(17,2)/       0.0271680D0/
      DATA GAUSS2(17,2)/       4.0000000D0/
      DATA GAUSS3(17,2)/       2.1000000D0/
C                    DATA FOR ELEMENT 30        ZINC
      DATA REFAM  (30)/' Zn: (MNDO):  M.J.S. DEWAR, K.M. MERZ, ORGANOMET
     1ALLICS, 5, 1494-1496 (1986)     '/
      DATA USS   ( 30)/     -20.8397160D0/
      DATA UPP   ( 30)/     -19.6252240D0/
      DATA BETAS ( 30)/      -1.0000000D0/
      DATA BETAP ( 30)/      -2.0000000D0/
      DATA ZS    ( 30)/       2.0473590D0/
      DATA ZP    ( 30)/       1.4609460D0/
      DATA ZD    ( 30)/       1.0000000D0/
      DATA ALP   ( 30)/       1.5064570D0/
      DATA EISOL ( 30)/     -29.8794320D0/
      DATA GSS   ( 30)/      11.8000000D0/
      DATA GSP   ( 30)/      11.1820180D0/
      DATA GPP   ( 30)/      13.3000000D0/
      DATA GP2   ( 30)/      12.9305200D0/
      DATA HSP   ( 30)/       0.4846060D0/
      DATA DD    ( 30)/       1.3037826D0/
      DATA QQ    ( 30)/       1.4520183D0/
      DATA AM    ( 30)/       0.4336641D0/
      DATA AD    ( 30)/       0.2375912D0/
      DATA AQ    ( 30)/       0.2738858D0/
C                    DATA FOR ELEMENT 35       AM1:   BROMINE  *
C
C THE FOLLOWING NUMBERS ARE PARAMETRIZED AT THE AM1 LEVEL, BUT
C ARE NOT YET PUBLISHED.  WHEN PUBLISHED UNCOMMENT THE FOLLOWING
C BLOCK, AND REMOVE THE PRECEDING SET OF PARAMETERS.
      DATA REFAM  (35)/' Br: (AM1): DEWAR AND ZOEBISCH, UNPUBLISHED WORK
     1                                '/
      DATA USS   (35)/    -104.6560630D0/
      DATA UPP   (35)/     -74.9300520D0/
      DATA BETAS (35)/     -19.3998800D0/
      DATA BETAP (35)/      -8.9571950D0/
      DATA ZS    (35)/       3.0641330D0/
      DATA ZP    (35)/       2.0383330D0/
      DATA ZD    (35)/       1.0000000D0/
      DATA ALP   (35)/       2.5765460D0/
      DATA EISOL (35)/    -352.3142087D0/
      DATA GSS   (35)/      15.0364395D0/
      DATA GSP   (35)/      13.0346824D0/
      DATA GPP   (35)/      11.2763254D0/
      DATA GP2   (35)/       9.8544255D0/
      DATA HSP   (35)/       2.4558683D0/
      DATA DD    (35)/       0.8458104D0/
      DATA QQ    (35)/       1.0407133D0/
      DATA AM    (35)/       0.5526071D0/
      DATA AD    (35)/       0.6024598D0/
      DATA AQ    (35)/       0.5307555D0/
      DATA GAUSS1(35,1)/       0.0666850D0/
      DATA GAUSS2(35,1)/       4.0000000D0/
      DATA GAUSS3(35,1)/       1.5000000D0/
      DATA GAUSS1(35,2)/       0.0255680D0/
      DATA GAUSS2(35,2)/       4.0000000D0/
      DATA GAUSS3(35,2)/       2.3000000D0/
C                    DATA FOR ELEMENT 53       AM1:   IODINE  *
C
C THE FOLLOWING NUMBERS ARE PARAMETRIZED AT THE AM1 LEVEL, BUT
C ARE NOT YET PUBLISHED.  WHEN PUBLISHED UNCOMMENT THE FOLLOWING
C BLOCK, AND REMOVE THE PRECEDING SET OF PARAMETERS.
      DATA REFAM  (53)/'  I: (AM1): DEWAR AND ZOEBISCH, UNPUBLISHED WORK
     1                                '/
      DATA USS   (53)/    -103.5896630D0/
      DATA UPP   (53)/     -74.4299970D0/
      DATA BETAS (53)/      -8.4433270D0/
      DATA BETAP (53)/      -6.3234050D0/
      DATA ZS    (53)/       2.1028580D0/
      DATA ZP    (53)/       2.1611530D0/
      DATA ZD    (53)/       1.0000000D0/
      DATA ALP   (53)/       2.2994240D0/
      DATA EISOL (53)/    -346.8642857D0/
      DATA GSS   (53)/      15.0404486D0/
      DATA GSP   (53)/      13.0565580D0/
      DATA GPP   (53)/      11.1477837D0/
      DATA GP2   (53)/       9.9140907D0/
      DATA HSP   (53)/       2.4563820D0/
      DATA DD    (53)/       1.4878778D0/
      DATA QQ    (53)/       1.1887388D0/
      DATA AM    (53)/       0.5527544D0/
      DATA AD    (53)/       0.4497523D0/
      DATA AQ    (53)/       0.4631775D0/
      DATA GAUSS1(53,1)/       0.0043610D0/
      DATA GAUSS2(53,1)/       2.3000000D0/
      DATA GAUSS3(53,1)/       1.8000000D0/
      DATA GAUSS1(53,2)/       0.0157060D0/
      DATA GAUSS2(53,2)/       3.0000000D0/
      DATA GAUSS3(53,2)/       2.2400000D0/
C
C     START OF "OLD" ELEMENTS: THESE ARE OLD PARAMETERS WHICH
C     CAN BE USED, IF DESIRED, BY SPECIFYING "<CHEMICAL SYMBOL>YEAR"
C     AS IN SI1978 OR  S1983.
C
C                    DATA FOR ELEMENT 90        SILICON
      DATA REFAM  (90)/' Si: (MNDO): M.J.S.DEWAR, M.L.MCKEE, H.S.RZEPA,
     1J. AM. CHEM. SOC., 100 3607 1978'/
      DATA USS   (90)/     -40.5682920D0/
      DATA UPP   (90)/     -28.0891870D0/
      DATA BETAS (90)/      -4.2562180D0/
      DATA BETAP (90)/      -4.2562180D0/
      DATA ZS    (90)/       1.4353060D0/
      DATA ZP    (90)/       1.4353060D0/
      DATA ZD    (90)/       1.0000000D0/
      DATA ALP   (90)/       2.1961078D0/
      DATA EISOL (90)/     -90.5399580D0/
      DATA DD    (90)/       1.4078712D0/
      DATA QQ    (90)/       1.1658281D0/
      DATA AM    (90)/       0.3608967D0/
      DATA AD    (90)/       0.3441817D0/
      DATA AQ    (90)/       0.3999442D0/
      DATA HSP(90)/1.32D00/
      DATA GP2(90)/6.54D00/
      DATA GPP(90)/7.31D00/
      DATA GSP(90)/8.36D00/
      DATA GSS(90)/9.82D00/
      DATA REFAM  (91)/'  S: (MNDO): M.J.S.DEWAR, H.S. RZEPA, M.L.MCKEE,
     1 J.AM.CHEM.SOC.100, 3607 (1978).'/
      DATA USS   (91)/     -75.2391520D0/
      DATA UPP   (91)/     -57.8320130D0/
      DATA BETAS (91)/     -11.1422310D0/
      DATA BETAP (91)/     -11.1422310D0/
      DATA ZS    (91)/       2.6135910D0/
      DATA ZP    (91)/       2.0343930D0/
      DATA ZD    (91)/       1.0000000D0/
      DATA ALP   (91)/       2.4916445D0/
      DATA EISOL (91)/    -235.4413560D0/
      DATA GSS   (91)/      12.8800000D0/
      DATA GSP   (91)/      11.2600000D0/
      DATA GPP   (91)/       9.9000000D0/
      DATA GP2   (91)/       8.8300000D0/
      DATA HSP   (91)/       2.2600000D0/
      DATA DD    (91)/       0.8231596D0/
      DATA QQ    (91)/       0.8225156D0/
      DATA AM    (91)/       0.4733554D0/
      DATA AD    (91)/       0.5889395D0/
      DATA AQ    (91)/       0.5632724D0/
      DATA REFAM (102)/' Cb: (AM1):  Capped Bond  (Hydrogen-like, takes
     1 on zero charge.)               '/
C                    DATA FOR ELEMENT102
      DATA USS   (102)/     -11.9062760D0/
      DATA BETAS (102)/-9999999.0000000D0/
      DATA ZS    (102)/       4.0000000D0/
      DATA ZP    (102)/       0.3000000D0/
      DATA ZD    (102)/       0.3000000D0/
      DATA ALP   (102)/       2.5441341D0/
      DATA EISOL (102)/       4.0000000D0/
      DATA GSS   (102)/      12.8480000D0/
      DATA HSP   (102)/       0.1000000D0/
      DATA DD    (102)/       0.0684105D0/
      DATA QQ    (102)/       1.0540926D0/
      DATA AM    (102)/       0.4721793D0/
      DATA AD    (102)/       0.9262742D0/
      DATA AQ    (102)/       0.2909059D0/
C
C     LF0210: SPECIAL HYDROGEN CONTAINING P ORBITALS.  CAN BE USED BY SPECIFYING
C     "Hp".  ADDED BY LUKE FIEDLER, FEB. 2010.
C           DATA FOR ELEMENT  108     AM1:   HYDROGEN (WITH P-ORBITALS)
      DATA REFAM (108)/' Hp: (AM1): Hydrogen (with p basis functions)  
     1                               '/
      DATA USS  (108)/      -9.0000000D0/
      DATA UPP  (108)/      -4.0000000D0/
      DATA BETAS(108)/      -6.4741330D0/
      DATA BETAP(108)/       0.0000000D0/
      DATA ZS   (108)/       0.9005230D0/
      DATA ZP   (108)/       0.5919010D0/
      DATA ALP  (108)/       3.0268400D0/
      DATA EISOL(108)/      -9.0000000D0/
      DATA GSS  (108)/      12.2730000D0/
      DATA GSP  (108)/      15.0000000D0/
      DATA GPP  (108)/       8.0000000D0/
      DATA GP2  (108)/       9.0000000D0/
      DATA HSP  (108)/       0.0000000D0/
      DATA DD   (108)/       0.9955286D0/
      DATA QQ   (108)/       2.0691718D0/
      DATA AM   (108)/       0.4510292D0/
      DATA AD   (108)/       0.0015478D0/
      DATA AQ   (108)/       0.1782205D0/
      DATA GAUSS1(108,1)/       0.0000000D0/
      DATA GAUSS2(108,1)/       0.0000000D0/
      DATA GAUSS3(108,1)/       0.0000000D0/
      DATA GAUSS1(108,2)/       0.0000000D0/
      DATA GAUSS2(108,2)/       0.0000000D0/
      DATA GAUSS3(108,2)/       0.0000000D0/
      DATA GAUSS1(108,3)/       0.0000000D0/
      DATA GAUSS2(108,3)/       0.0000000D0/
      DATA GAUSS3(108,3)/       0.0000000D0/
      DATA GAUSS1(108,4)/       0.0000000D0/
      DATA GAUSS2(108,4)/       0.0000000D0/
      DATA GAUSS3(108,4)/       0.0000000D0/
C
***********************************************************************
*
*    START OF RM1 PARAMETERS
*
*                              Units
*     USSRM1, UPPRM1, UDDRM1:  eV
*     BETASR, BETAPR, BETADR:  eV
*     ALPRM1:                  angstrom**(-1)
*     ZSRM1, ZPRM1, ZDRM1:     bohr**(-1)
*     GSSRM1, GSPRM1, GPPRM1:  eV 
*     GP2RM1, HSPRM1:          eV
*     EISOLR:                  eV
*     GAUSR1:                  eV * angstrom
*     GAUSR2:                  angstrom**(-2)
*     GAUSR3:                  angstrom
*     AMRM1, ADRM1, AQRM1:     bohr
*     DDRM1, QQRM1:            bohr
*
***********************************************************************
C                    DATA FOR ELEMENT  1       RM1:   HYDROGEN
      DATA REFRM( 1)/'  H: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, 
     11101-1111 (2006) '/
      DATA USSRM1(  1)/      -11.96067697/
      DATA BETASR(  1)/       -5.76544469/
      DATA ZSRM1(  1)/        1.08267366/
      DATA ALPRM1(  1)/        3.06835947/
      DATA EISOLR(  1)/      -11.96067697/
      DATA GSSRM1(  1)/       13.98321296/
      DATA AMRM1(  1)/        0.51387341/
      DATA ADRM1(  1)/        0.51387341/
      DATA AQRM1(  1)/        0.51387341/
      DATA GAUSR1(  1,1)/        0.10288875/
      DATA GAUSR2(  1,1)/        5.90172268/
      DATA GAUSR3(  1,1)/        1.17501185/
      DATA GAUSR1(  1,2)/        0.06457449/
      DATA GAUSR2(  1,2)/        6.41785671/
      DATA GAUSR3(  1,2)/        1.93844484/
      DATA GAUSR1(  1,3)/       -0.03567387/
      DATA GAUSR2(  1,3)/        2.80473127/
      DATA GAUSR3(  1,3)/        1.63655241/
C                    DATA FOR ELEMENT  6       RM1:   CARBON
      DATA REFRM  ( 6)/'  C: (RM1): G.B.ROCHA, R.O.FREIRE ET AL,  JCC 27
     1, 1101-1111 (2006) '/
        DATA USSRM1(  6)/      -51.72556032/
        DATA UPPRM1(  6)/      -39.40728943/
        DATA BETASR(  6)/      -15.45932428/
        DATA BETAPR(  6)/       -8.23608638/
        DATA ZSRM1(  6)/        1.85018803/
        DATA ZPRM1(  6)/        1.76830093/
        DATA ALPRM1(  6)/        2.79282078/
        DATA EISOLR(  6)/     -117.86734441/
        DATA GSSRM1(  6)/       13.05312440/
        DATA GSPRM1(  6)/       11.33479389/
        DATA GPPRM1(  6)/       10.95113739/
        DATA GP2RM1(  6)/        9.72395099/
        DATA HSPRM1(  6)/        1.55215133/
        DATA DDRM1(  6)/        0.79675711/
        DATA QQRM1(  6)/        0.69261111/
        DATA AMRM1(  6)/        0.47969330/
        DATA ADRM1(  6)/        0.50974061/
        DATA AQRM1(  6)/        0.66147546/
        DATA GAUSR1(  6,1)/        0.07462271/
        DATA GAUSR2(  6,1)/        5.73921605/
        DATA GAUSR3(  6,1)/        1.04396983/
        DATA GAUSR1(  6,2)/        0.01177053/
        DATA GAUSR2(  6,2)/        6.92401726/
        DATA GAUSR3(  6,2)/        1.66159571/
        DATA GAUSR1(  6,3)/        0.03720662/
        DATA GAUSR2(  6,3)/        6.26158944/
        DATA GAUSR3(  6,3)/        1.63158721/
        DATA GAUSR1(  6,4)/       -0.00270657/
        DATA GAUSR2(  6,4)/        9.00003735/
        DATA GAUSR3(  6,4)/        2.79557901/
C                    DATA FOR ELEMENT  7       RM1:   NITROGEN
      DATA REFRM  ( 7)/'  N: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, 
     11101-1111 (2006) '/
        DATA USSRM1(  7)/      -70.85123715/        
        DATA UPPRM1(  7)/      -57.97730920/
        DATA BETASR(  7)/      -20.87124548/
        DATA BETAPR(  7)/      -16.67171853/
        DATA ZSRM1(  7)/        2.37447159/
        DATA ZPRM1(  7)/        1.97812569/
        DATA ALPRM1(  7)/        2.96422542/
        DATA EISOLR(  7)/     -205.08764187/
        DATA GSSRM1(  7)/       13.08736234/
        DATA GSPRM1(  7)/       13.21226834/
        DATA GPPRM1(  7)/       13.69924324/
        DATA GP2RM1(  7)/       11.94103953/
        DATA HSPRM1(  7)/        5.00000846/
        DATA DDRM1(  7)/        0.64956197/
        DATA QQRM1(  7)/        0.61914411/
        DATA AMRM1(  7)/        0.48095152/
        DATA ADRM1(  7)/        0.97060232/
        DATA AQRM1(  7)/        0.80235450/
        DATA GAUSR1(  7,1)/        0.06073380/
        DATA GAUSR2(  7,1)/        4.58892946/
        DATA GAUSR3(  7,1)/        1.37873881/
        DATA GAUSR1(  7,2)/        0.02438558/
        DATA GAUSR2(  7,2)/        4.62730519/
        DATA GAUSR3(  7,2)/        2.08370698/
        DATA GAUSR1(  7,3)/       -0.02283430/
        DATA GAUSR2(  7,3)/        2.05274659/
        DATA GAUSR3(  7,3)/        1.86763816/
C                    DATA FOR ELEMENT  8       RM1:   OXYGEN
      DATA REFRM  ( 8)/'  O: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, 
     1 1101-1111 (2006) '/
        DATA USSRM1(  8)/      -96.94948069/
        DATA UPPRM1(  8)/      -77.89092978/
        DATA BETASR(  8)/      -29.85101212/
        DATA BETAPR(  8)/      -29.15101314/
        DATA ZSRM1(  8)/        3.17936914/
        DATA ZPRM1(  8)/        2.55361907/
        DATA ALPRM1(  8)/        4.17196717/
        DATA EISOLR(  8)/     -312.04035400/
        DATA GSSRM1(  8)/       14.00242788/
        DATA GSPRM1(  8)/       14.95625043/
        DATA GPPRM1(  8)/       14.14515138/
        DATA GP2RM1(  8)/       12.70325497/
        DATA HSPRM1(  8)/        3.93217161/
        DATA DDRM1(  8)/        0.48867006/
        DATA QQRM1(  8)/        0.47961142/
        DATA AMRM1(  8)/        0.51457955/
        DATA ADRM1(  8)/        1.00656044/
        DATA AQRM1(  8)/        0.89532625/
        DATA GAUSR1(  8,1)/        0.23093552/
        DATA GAUSR2(  8,1)/        5.21828736/
        DATA GAUSR3(  8,1)/        0.90363555/
        DATA GAUSR1(  8,2)/        0.05859873/
        DATA GAUSR2(  8,2)/        7.42932932/
        DATA GAUSR3(  8,2)/        1.51754610/
C                    DATA FOR ELEMENT  9       RM1:   FLUORINE  *
      DATA REFRM  ( 9)/'  F: (RM1): G.B.ROCHA, R.O.FREIRE ET AL,  JCC 27
     1, 1101-1111 (2006) '/
        DATA USSRM1(  9)/     -134.18369591/
        DATA UPPRM1(  9)/     -107.84660920/
        DATA BETASR(  9)/      -70.00000512/
        DATA BETAPR(  9)/      -32.67982711/
        DATA ZSRM1(  9)/        4.40337913/
        DATA ZPRM1(  9)/        2.64841556/
        DATA ALPRM1(  9)/        6.00000062/
        DATA EISOLR(  9)/     -484.59570233/
        DATA GSSRM1(  9)/       16.72091319/
        DATA GSPRM1(  9)/       16.76142629/
        DATA GPPRM1(  9)/       15.22581028/
        DATA GP2RM1(  9)/       14.86578679/
        DATA HSPRM1(  9)/        1.99766171/
        DATA DDRM1(  9)/        0.34889273/
        DATA QQRM1(  9)/        0.46244437/
        DATA AMRM1(  9)/        0.61448200/
        DATA ADRM1(  9)/        0.92253078/
        DATA AQRM1(  9)/        0.62364852/
        DATA GAUSR1(  9,1)/        0.40302025/
        DATA GAUSR2(  9,1)/        7.20441959/
        DATA GAUSR3(  9,1)/        0.81653013/
        DATA GAUSR1(  9,2)/        0.07085831/
        DATA GAUSR2(  9,2)/        9.00001562/
        DATA GAUSR3(  9,2)/        1.43802381/
C                    DATA FOR ELEMENT 15      RM1:   PHOSPHORUS *
      DATA REFRM  (15)/'  P: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, 
     11101-1111 (2006) '/
        DATA USSRM1( 15)/      -41.81533184/
        DATA UPPRM1( 15)/      -34.38342529/
        DATA BETASR( 15)/       -6.13514969/
        DATA BETAPR( 15)/       -5.94442127/
        DATA ZSRM1( 15)/        2.12240118/
        DATA ZPRM1( 15)/        1.74327954/
        DATA ALPRM1( 15)/        1.90993294/
        DATA EISOLR( 15)/     -123.17977886/
        DATA GSSRM1( 15)/       11.08059265/
        DATA GSPRM1( 15)/        5.68339201/
        DATA GPPRM1( 15)/        7.60417563/
        DATA GP2RM1( 15)/        7.40265182/
        DATA HSPRM1( 15)/        1.16181792/
        DATA DDRM1( 15)/        1.01069548/
        DATA QQRM1( 15)/        0.95986904/
        DATA AMRM1( 15)/        0.40720412/
        DATA ADRM1( 15)/        0.39321392/
        DATA AQRM1( 15)/        0.31234367/
        DATA GAUSR1( 15,1)/       -0.41063467/
        DATA GAUSR2( 15,1)/        6.08752832/
        DATA GAUSR3( 15,1)/        1.31650261/
        DATA GAUSR1( 15,2)/       -0.16299288/
        DATA GAUSR2( 15,2)/        7.09472602/
        DATA GAUSR3( 15,2)/        1.90721319/
        DATA GAUSR1( 15,3)/       -0.04887125/
        DATA GAUSR2( 15,3)/        8.99979308/
        DATA GAUSR3( 15,3)/        2.65857780/
C                    DATA FOR ELEMENT 16       RM1:   SULFUR  *
C
        DATA REFRM  (16)/'  S: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 2
     17, 1101-1111 (2006) '/
        DATA USSRM1( 16)/      -55.16775121/
        DATA UPPRM1( 16)/      -46.52930422/
        DATA BETASR( 16)/       -1.95910719/
        DATA BETAPR( 16)/       -8.77430652/
        DATA ZSRM1( 16)/        2.13344308/
        DATA ZPRM1( 16)/        1.87460650/
        DATA ALPRM1( 16)/        2.44015636/
        DATA EISOLR( 16)/     -185.38613819/
        DATA GSSRM1( 16)/       12.48828408/
        DATA GSPRM1( 16)/        8.56910574/
        DATA GPPRM1( 16)/        8.52301167/
        DATA GP2RM1( 16)/        7.66863296/
        DATA HSPRM1( 16)/        3.88978932/
        DATA DDRM1( 16)/        0.99369208/
        DATA QQRM1( 16)/        0.89262470/
        DATA AMRM1( 16)/        0.45893581/
        DATA ADRM1( 16)/        0.69307504/
        DATA AQRM1( 16)/        0.49593354/
        DATA GAUSR1( 16,1)/       -0.74601055/
        DATA GAUSR2( 16,1)/        4.81038002/
        DATA GAUSR3( 16,1)/        0.59380129/
        DATA GAUSR1( 16,2)/       -0.06519286/
        DATA GAUSR2( 16,2)/        7.20760864/
        DATA GAUSR3( 16,2)/        1.29492008/
        DATA GAUSR1( 16,3)/       -0.00655977/
        DATA GAUSR2( 16,3)/        9.00000180/
        DATA GAUSR3( 16,3)/        1.80060151/
C                    DATA FOR ELEMENT 17       RM1:   CHLORINE  *
        DATA REFRM  (17)/' Cl: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 
     127, 1101-1111 (2006) '/
        DATA USSRM1( 17)/     -118.47306918/
        DATA UPPRM1( 17)/      -76.35330340/
        DATA BETASR( 17)/      -19.92430432/
        DATA BETAPR( 17)/      -11.52935197/
        DATA ZSRM1( 17)/        3.86491071/
        DATA ZPRM1( 17)/        1.89593144/
        DATA ALPRM1( 17)/        3.69358828/
        DATA EISOLR( 17)/     -382.47009370/
        DATA GSSRM1( 17)/       15.36023105/
        DATA GSPRM1( 17)/       13.30671171/
        DATA GPPRM1( 17)/       12.56502640/
        DATA GP2RM1( 17)/        9.66397083/
        DATA HSPRM1( 17)/        1.76489897/
        DATA DDRM1( 17)/        0.45417885/
        DATA QQRM1( 17)/        0.88258469/
        DATA AMRM1( 17)/        0.56447787/
        DATA ADRM1( 17)/        0.74895844/
        DATA AQRM1( 17)/        0.77071593/
        DATA GAUSR1( 17,1)/        0.12947108/
        DATA GAUSR2( 17,1)/        2.97724424/
        DATA GAUSR3( 17,1)/        1.46749784/
        DATA GAUSR1( 17,2)/        0.00288899/
        DATA GAUSR2( 17,2)/        7.09827589/
        DATA GAUSR3( 17,2)/        2.50002723/
C                    DATA FOR ELEMENT 35       RM1:   BROMINE  *
      DATA REFRM  (35)/' Br: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27,
     1 1101-1111 (2006) '/
        DATA USSRM1( 35)/     -113.48398183/
        DATA UPPRM1( 35)/      -76.18720023/
        DATA BETASR( 35)/       -1.34139841/
        DATA BETAPR( 35)/       -8.20225991/
        DATA ZSRM1( 35)/        5.73157215/
        DATA ZPRM1( 35)/        2.03147582/
        DATA ALPRM1( 35)/        2.86710532/
        DATA EISOLR( 35)/     -357.11642720/
        DATA GSSRM1( 35)/       17.11563074/
        DATA GSPRM1( 35)/       15.62419251/
        DATA GPPRM1( 35)/       10.73546293/
        DATA GP2RM1( 35)/        8.86056199/
        DATA HSPRM1( 35)/        2.23512762/
        DATA DDRM1( 35)/        0.20990053/
        DATA QQRM1( 35)/        1.04422623/
        DATA AMRM1( 35)/        0.62898760/
        DATA ADRM1( 35)/        1.31654993/
        DATA AQRM1( 35)/        0.58633620/
        DATA GAUSR1( 35,1)/        0.98689937/
        DATA GAUSR2( 35,1)/        4.28484191/
        DATA GAUSR3( 35,1)/        2.00019696/
        DATA GAUSR1( 35,2)/       -0.92731247/
        DATA GAUSR2( 35,2)/        4.54005910/
        DATA GAUSR3( 35,2)/        2.01617695/
C                    DATA FOR ELEMENT 53       RM1:   IODINE  *
      DATA REFRM  (53)/'  I: (RM1): G.B.ROCHA, R.O.FREIRE ET AL, JCC 27, 
     11101-1111 (2006) '/
        DATA USSRM1( 53)/      -74.89997837/
        DATA UPPRM1( 53)/      -51.41023805/
        DATA BETASR( 53)/       -4.19316149/
        DATA BETAPR( 53)/       -4.40038412/
        DATA ZSRM1( 53)/        2.53003753/
        DATA ZPRM1( 53)/        2.31738678/
        DATA ALPRM1( 53)/        2.14157092/
        DATA EISOLR( 53)/     -248.49332414/
        DATA GSSRM1( 53)/       19.99974131/
        DATA GSPRM1( 53)/        7.68957672/
        DATA GPPRM1( 53)/        7.30488343/
        DATA GP2RM1( 53)/        6.85424614/
        DATA HSPRM1( 53)/        1.41602940/
        DATA DDRM1( 53)/        1.29634250/
        DATA QQRM1( 53)/        1.10859635/
        DATA AMRM1( 53)/        0.73497667/
        DATA ADRM1( 53)/        0.37173273/
        DATA AQRM1( 53)/        0.35126442/
        DATA GAUSR1( 53,1)/       -0.08147724/
        DATA GAUSR2( 53,1)/        1.56065072/
        DATA GAUSR3( 53,1)/        2.00002063/
        DATA GAUSR1( 53,2)/        0.05914991/
        DATA GAUSR2( 53,2)/        5.76111270/
        DATA GAUSR3( 53,2)/        2.20488800/
C
C     LF0410: SPECIAL HYDROGEN CONTAINING P ORBITALS.  CAN BE USED BY SPECIFYING
C     "Hp".  ADDED BY LUKE FIEDLER, APR. 2010.
C           DATA FOR ELEMENT  108     RM1:   HYDROGEN (WITH P-ORBITALS)
      DATA REFRM (108)/' Hp: (RM1): Hydrogen (with p basis functions)  
     1                               '/
      DATA USSRM1(108)/      -7.3964270D0/
      DATA UPPRM1(108)/      -1.1486568D0/
      DATA BETASR(108)/      -9.6737870D0/
      DATA BETAPR(108)/      -1.3000000D0/
      DATA ZSRM1 (108)/       0.9880780D0/
      DATA ZPRM1 (108)/       0.3000000D0/
      DATA ALPRM1(108)/       2.9323240D0/
      DATA EISOLR(108)/      28.7118620D0/
      DATA GSSRM1(108)/      12.8480000D0/
      DATA GSPRM1(108)/       2.1600000D0/
      DATA GPPRM1(108)/       3.0100000D0/
      DATA GP2RM1(108)/       2.4400000D0/
      DATA HSPRM1(108)/       1.9200000D0/
      DATA DDRM1 (108)/       0.9675816D0/
      DATA QQRM1 (108)/       4.0824829D0/
      DATA AMRM1 (108)/       0.4721603D0/
      DATA ADRM1 (108)/       0.5007132D0/
      DATA AQRM1 (108)/       0.1603621D0/
      DATA GAUSR1(108,1)/       0.1227960D0/
      DATA GAUSR2(108,1)/       5.0000000D0/
      DATA GAUSR3(108,1)/       1.2000000D0/
      DATA GAUSR1(108,2)/       0.0050900D0/
      DATA GAUSR2(108,2)/       5.0000000D0/
      DATA GAUSR3(108,2)/       1.8000000D0/
      DATA GAUSR1(108,3)/      -0.0183360D0/
      DATA GAUSR2(108,3)/       2.0000000D0/
      DATA GAUSR3(108,3)/       2.1000000D0/
      DATA GAUSR1(108,4)/       0.0000000D0/
      DATA GAUSR2(108,4)/       0.0000000D0/
      DATA GAUSR3(108,4)/       0.0000000D0/
C
***********************************************************************
*
*    START OF PM3 PARAMETERS
*
*    Reference #1:  "Optimization of Parameters for Semiempircal
*                    Methods I. Method"
*                James J. P. Stewart
*                J. Comput. Chem., 10(2), 209-220 (March 1989).
*    Reference #2:  "Optimization of Parameters for Semiempirical
*                    Methods II. Applications"
*                James J. P. Stewart
*                J. Comput. Chem., 10(2), 221-264 (March 1989).
*                [Note: PM3 was referred to as "MNDO-PM3" in the earliest papers.]
*    Reference #3:  "Optimization of Parameters for Semiempirical Methods.
*                3. Extension of PM3 to Be, Mg, Zn, Ga, Ge, As, Se, Cd,
*                In, Sn, Sb, Te, Hg, Tl, Pb, and Bi"
*                James J. P. Stewart
*                J. Comput. Chem., 12(3), 320-341 (April 1991).
*
*                              Units
*     USSPM3, UPPPM3, UDDPM3:  eV
*     BETASP, BETAPP, BETADP:  eV
*     ALPPM3:                  angstrom**(-1)
*     ZSPM3, ZPPM3, ZDPM3:     bohr**(-1)
*     GSSPM3, GSPPM3, GPPPM3:  eV 
*     GP2PM3, HSPPM3:          eV
*     EISOLP:                  eV
*     GAUSP1:                  eV * angstrom
*     GAUSP2:                  angstrom**(-2)
*     GAUSP3:                  angstrom
*     AMPM3, ADPM3, AQPM3:     bohr
*     DDPM3, QQPM3:            bohr
*
***********************************************************************
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFPM3 ( 1)/'  H: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1989 10(2), 209.               '/
      DATA USSPM3(  1)/     -13.0733210D0/
      DATA BETASP(  1)/      -5.6265120D0/
      DATA ZSPM3 (  1)/       0.9678070D0/
      DATA ALPPM3(  1)/       3.3563860D0/
      DATA EISOLP(  1)/     -13.0733210D0/
      DATA GSSPM3(  1)/      14.7942080D0/
      DATA AMPM3 (  1)/       0.5437048D0/
      DATA ADPM3 (  1)/       0.5437048D0/
      DATA AQPM3 (  1)/       0.5437048D0/
      DATA GAUSP1(  1,1)/       1.1287500D0/
      DATA GAUSP2(  1,1)/       5.0962820D0/
      DATA GAUSP3(  1,1)/       1.5374650D0/
      DATA GAUSP1(  1,2)/      -1.0603290D0/
      DATA GAUSP2(  1,2)/       6.0037880D0/
      DATA GAUSP3(  1,2)/       1.5701890D0/
C                    DATA FOR ELEMENT  4        BERYLLIUM
      DATA REFPM3 ( 4)/' Be: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3(  4)/     -17.2647520D0/
      DATA UPPPM3(  4)/     -11.3042430D0/
      DATA BETASP(  4)/      -3.9620530D0/
      DATA BETAPP(  4)/      -2.7806840D0/
      DATA ZSPM3 (  4)/       0.8774390D0/
      DATA ZPPM3 (  4)/       1.5087550D0/
      DATA ALPPM3(  4)/       1.5935360D0/
      DATA EISOLP(  4)/     -25.5166530D0/
      DATA GSSPM3(  4)/       9.0128510D0/
      DATA GSPPM3(  4)/       6.5761990D0/
      DATA GPPPM3(  4)/       6.0571820D0/
      DATA GP2PM3(  4)/       9.0052190D0/
      DATA HSPPM3(  4)/       0.5446790D0/
      DATA DDPM3 (  4)/       1.0090531D0/
      DATA QQPM3 (  4)/       0.8117586D0/
      DATA AMPM3 (  4)/       0.3312330D0/
      DATA ADPM3 (  4)/       0.2908996D0/
      DATA AQPM3 (  4)/       0.3530008D0/
      DATA GAUSP1(  4,1)/       1.6315720D0/
      DATA GAUSP2(  4,1)/       2.6729620D0/
      DATA GAUSP3(  4,1)/       1.7916860D0/
      DATA GAUSP1(  4,2)/      -2.1109590D0/
      DATA GAUSP2(  4,2)/       1.9685940D0/
      DATA GAUSP3(  4,2)/       1.7558710D0/
C                    DATA FOR ELEMENT  6      CARBON
      DATA REFPM3 ( 6)/'  C: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1989 10(2), 209.               '/
      DATA USSPM3(  6)/     -47.2703200D0/
      DATA UPPPM3(  6)/     -36.2669180D0/
      DATA BETASP(  6)/     -11.9100150D0/
      DATA BETAPP(  6)/      -9.8027550D0/
      DATA ZSPM3 (  6)/       1.5650850D0/
      DATA ZPPM3 (  6)/       1.8423450D0/
      DATA ALPPM3(  6)/       2.7078070D0/
      DATA EISOLP(  6)/    -111.2299170D0/
      DATA GSSPM3(  6)/      11.2007080D0/
      DATA GSPPM3(  6)/      10.2650270D0/
      DATA GPPPM3(  6)/      10.7962920D0/
      DATA GP2PM3(  6)/       9.0425660D0/
      DATA HSPPM3(  6)/       2.2909800D0/
      DATA DDPM3 (  6)/       0.8332396D0/
      DATA QQPM3 (  6)/       0.6647750D0/
      DATA AMPM3 (  6)/       0.4116394D0/
      DATA ADPM3 (  6)/       0.5885862D0/
      DATA AQPM3 (  6)/       0.7647667D0/
      DATA GAUSP1(  6,1)/       0.0501070D0/
      DATA GAUSP2(  6,1)/       6.0031650D0/
      DATA GAUSP3(  6,1)/       1.6422140D0/
      DATA GAUSP1(  6,2)/       0.0507330D0/
      DATA GAUSP2(  6,2)/       6.0029790D0/
      DATA GAUSP3(  6,2)/       0.8924880D0/
C                    DATA FOR ELEMENT  7      NITROGEN
      DATA REFPM3 ( 7)/'  N: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1989 10(2), 209.               '/
      DATA USSPM3(  7)/     -49.3356720D0/
      DATA UPPPM3(  7)/     -47.5097360D0/
      DATA BETASP(  7)/     -14.0625210D0/
      DATA BETAPP(  7)/     -20.0438480D0/
      DATA ZSPM3 (  7)/       2.0280940D0/
      DATA ZPPM3 (  7)/       2.3137280D0/
      DATA ALPPM3(  7)/       2.8305450D0/
      DATA EISOLP(  7)/    -157.6137755D0/
      DATA GSSPM3(  7)/      11.9047870D0/
      DATA GSPPM3(  7)/       7.3485650D0/
      DATA GPPPM3(  7)/      11.7546720D0/
      DATA GP2PM3(  7)/      10.8072770D0/
      DATA HSPPM3(  7)/       1.1367130D0/
      DATA DDPM3 (  7)/       0.6577006D0/
      DATA QQPM3 (  7)/       0.5293383D0/
      DATA AMPM3 (  7)/       0.4375151D0/
      DATA ADPM3 (  7)/       0.5030995D0/
      DATA AQPM3 (  7)/       0.7364933D0/
      DATA GAUSP1(  7,1)/       1.5016740D0/
      DATA GAUSP2(  7,1)/       5.9011480D0/
      DATA GAUSP3(  7,1)/       1.7107400D0/
      DATA GAUSP1(  7,2)/      -1.5057720D0/
      DATA GAUSP2(  7,2)/       6.0046580D0/
      DATA GAUSP3(  7,2)/       1.7161490D0/
C                    DATA FOR ELEMENT  8      OXYGEN
      DATA REFPM3 ( 8)/'  O: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1989 10(2), 209.               '/
      DATA USSPM3(  8)/     -86.9930020D0/
      DATA UPPPM3(  8)/     -71.8795800D0/
      DATA BETASP(  8)/     -45.2026510D0/
      DATA BETAPP(  8)/     -24.7525150D0/
      DATA ZSPM3 (  8)/       3.7965440D0/
      DATA ZPPM3 (  8)/       2.3894020D0/
      DATA ALPPM3(  8)/       3.2171020D0/
      DATA EISOLP(  8)/    -289.3422065D0/
      DATA GSSPM3(  8)/      15.7557600D0/
      DATA GSPPM3(  8)/      10.6211600D0/
      DATA GPPPM3(  8)/      13.6540160D0/
      DATA GP2PM3(  8)/      12.4060950D0/
      DATA HSPPM3(  8)/       0.5938830D0/
      DATA DDPM3 (  8)/       0.4086173D0/
      DATA QQPM3 (  8)/       0.5125738D0/
      DATA AMPM3 (  8)/       0.5790430D0/
      DATA ADPM3 (  8)/       0.5299630D0/
      DATA AQPM3 (  8)/       0.8179630D0/
      DATA GAUSP1(  8,1)/      -1.1311280D0/
      DATA GAUSP2(  8,1)/       6.0024770D0/
      DATA GAUSP3(  8,1)/       1.6073110D0/
      DATA GAUSP1(  8,2)/       1.1378910D0/
      DATA GAUSP2(  8,2)/       5.9505120D0/
      DATA GAUSP3(  8,2)/       1.5983950D0/
C                    DATA FOR ELEMENT  9      FLUORINE
      DATA REFPM3 ( 9)/'  F: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1989 10(2), 209.               '/
      DATA USSPM3(  9)/    -110.4353030D0/
      DATA UPPPM3(  9)/    -105.6850470D0/
      DATA BETASP(  9)/     -48.4059390D0/
      DATA BETAPP(  9)/     -27.7446600D0/
      DATA ZSPM3 (  9)/       4.7085550D0/
      DATA ZPPM3 (  9)/       2.4911780D0/
      DATA ALPPM3(  9)/       3.3589210D0/
      DATA EISOLP(  9)/    -437.5171690D0/
      DATA GSSPM3(  9)/      10.4966670D0/
      DATA GSPPM3(  9)/      16.0736890D0/
      DATA GPPPM3(  9)/      14.8172560D0/
      DATA GP2PM3(  9)/      14.4183930D0/
      DATA HSPPM3(  9)/       0.7277630D0/
      DATA DDPM3 (  9)/       0.3125302D0/
      DATA QQPM3 (  9)/       0.4916328D0/
      DATA AMPM3 (  9)/       0.3857650D0/
      DATA ADPM3 (  9)/       0.6768503D0/
      DATA AQPM3 (  9)/       0.6120047D0/
      DATA GAUSP1(  9,1)/      -0.0121660D0/
      DATA GAUSP2(  9,1)/       6.0235740D0/
      DATA GAUSP3(  9,1)/       1.8568590D0/
      DATA GAUSP1(  9,2)/      -0.0028520D0/
      DATA GAUSP2(  9,2)/       6.0037170D0/
      DATA GAUSP3(  9,2)/       2.6361580D0/
C                    DATA FOR ELEMENT 12        MAGNESIUM
      DATA REFPM3 (12)/' Mg: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 12)/     -14.6236880D0/
      DATA UPPPM3( 12)/     -14.1734600D0/
      DATA BETASP( 12)/      -2.0716910D0/
      DATA BETAPP( 12)/      -0.5695810D0/
      DATA ZSPM3 ( 12)/       0.6985520D0/
      DATA ZPPM3 ( 12)/       1.4834530D0/
      DATA ALPPM3( 12)/       1.3291470D0/
      DATA EISOLP( 12)/     -22.5530760D0/
      DATA GSSPM3( 12)/       6.6943000D0/
      DATA GSPPM3( 12)/       6.7939950D0/
      DATA GPPPM3( 12)/       6.9104460D0/
      DATA GP2PM3( 12)/       7.0908230D0/
      DATA HSPPM3( 12)/       0.5433000D0/
      DATA DDPM3 ( 12)/       1.1403950D0/
      DATA QQPM3 ( 12)/       1.1279899D0/
      DATA AMPM3 ( 12)/       0.2460235D0/
      DATA ADPM3 ( 12)/       0.2695751D0/
      DATA AQPM3 ( 12)/       0.2767522D0/
      DATA GAUSP1( 12,1)/       2.1170500D0/
      DATA GAUSP2( 12,1)/       6.0094770D0/
      DATA GAUSP3( 12,1)/       2.0844060D0/
      DATA GAUSP1( 12,2)/      -2.5477670D0/
      DATA GAUSP2( 12,2)/       4.3953700D0/
      DATA GAUSP3( 12,2)/       2.0636740D0/
C                    DATA FOR ELEMENT 13      ALUMINUM
      DATA REFPM3 (13)/' Al: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1989 10(2), 209.               '/
      DATA USSPM3( 13)/     -24.8454040D0/
      DATA UPPPM3( 13)/     -22.2641590D0/
      DATA BETASP( 13)/      -0.5943010D0/
      DATA BETAPP( 13)/      -0.9565500D0/
      DATA ZSPM3 ( 13)/       1.7028880D0/
      DATA ZPPM3 ( 13)/       1.0736290D0/
      DATA ZDPM3 ( 13)/       1.0000000D0/
      DATA ALPPM3( 13)/       1.5217030D0/
      DATA EISOLP( 13)/     -46.8647630D0/
      DATA GSSPM3( 13)/       5.7767370D0/
      DATA GSPPM3( 13)/      11.6598560D0/
      DATA GPPPM3( 13)/       6.3477900D0/
      DATA GP2PM3( 13)/       6.1210770D0/
      DATA HSPPM3( 13)/       4.0062450D0/
      DATA DDPM3 ( 13)/       1.2102799D0/
      DATA QQPM3 ( 13)/       1.5585645D0/
      DATA AMPM3 ( 13)/       0.2123020D0/
      DATA ADPM3 ( 13)/       0.6418584D0/
      DATA AQPM3 ( 13)/       0.2262838D0/
      DATA GAUSP1( 13,1)/      -0.4730900D0/
      DATA GAUSP2( 13,1)/       1.9158250D0/
      DATA GAUSP3( 13,1)/       1.4517280D0/
      DATA GAUSP1( 13,2)/      -0.1540510D0/
      DATA GAUSP2( 13,2)/       6.0050860D0/
      DATA GAUSP3( 13,2)/       2.5199970D0/
C                    DATA FOR ELEMENT 14      SILICON
      DATA REFPM3 (14)/' Si: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1989 10(2), 209.               '/
      DATA USSPM3( 14)/     -26.7634830D0/
      DATA UPPPM3( 14)/     -22.8136350D0/
      DATA BETASP( 14)/      -2.8621450D0/
      DATA BETAPP( 14)/      -3.9331480D0/
      DATA ZSPM3 ( 14)/       1.6350750D0/
      DATA ZPPM3 ( 14)/       1.3130880D0/
      DATA ZDPM3 ( 14)/       1.0000000D0/
      DATA ALPPM3( 14)/       2.1358090D0/
      DATA EISOLP( 14)/     -67.7882140D0/
      DATA GSSPM3( 14)/       5.0471960D0/
      DATA GSPPM3( 14)/       5.9490570D0/
      DATA GPPPM3( 14)/       6.7593670D0/
      DATA GP2PM3( 14)/       5.1612970D0/
      DATA HSPPM3( 14)/       0.9198320D0/
      DATA DDPM3 ( 14)/       1.3144550D0/
      DATA QQPM3 ( 14)/       1.2743396D0/
      DATA AMPM3 ( 14)/       0.1854905D0/
      DATA ADPM3 ( 14)/       0.3060715D0/
      DATA AQPM3 ( 14)/       0.4877432D0/
      DATA GAUSP1( 14,1)/      -0.3906000D0/
      DATA GAUSP2( 14,1)/       6.0000540D0/
      DATA GAUSP3( 14,1)/       0.6322620D0/
      DATA GAUSP1( 14,2)/       0.0572590D0/
      DATA GAUSP2( 14,2)/       6.0071830D0/
      DATA GAUSP3( 14,2)/       2.0199870D0/
      DATA GAUSP1( 14,3)/       0.0207890D0/
      DATA GAUSP2( 14,3)/       5.0000000D0/
      DATA GAUSP3( 14,3)/       2.9906100D0/
C                    DATA FOR ELEMENT 15      PHOSPHORUS
      DATA REFPM3 (15)/'  P: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1989 10(2), 209.               '/
      DATA USSPM3( 15)/     -40.4130960D0/
      DATA UPPPM3( 15)/     -29.5930520D0/
      DATA BETASP( 15)/     -12.6158790D0/
      DATA BETAPP( 15)/      -4.1600400D0/
      DATA ZSPM3 ( 15)/       2.0175630D0/
      DATA ZPPM3 ( 15)/       1.5047320D0/
      DATA ZDPM3 ( 15)/       1.0000000D0/
      DATA ALPPM3( 15)/       1.9405340D0/
      DATA EISOLP( 15)/    -117.9591740D0/
      DATA GSSPM3( 15)/       7.8016150D0/
      DATA GSPPM3( 15)/       5.1869490D0/
      DATA GPPPM3( 15)/       6.6184780D0/
      DATA GP2PM3( 15)/       6.0620020D0/
      DATA HSPPM3( 15)/       1.5428090D0/
      DATA DDPM3 ( 15)/       1.0644947D0/
      DATA QQPM3 ( 15)/       1.1120386D0/
      DATA AMPM3 ( 15)/       0.2867187D0/
      DATA ADPM3 ( 15)/       0.4309446D0/
      DATA AQPM3 ( 15)/       0.3732517D0/
      DATA GAUSP1( 15,1)/      -0.6114210D0/
      DATA GAUSP2( 15,1)/       1.9972720D0/
      DATA GAUSP3( 15,1)/       0.7946240D0/
      DATA GAUSP1( 15,2)/      -0.0939350D0/
      DATA GAUSP2( 15,2)/       1.9983600D0/
      DATA GAUSP3( 15,2)/       1.9106770D0/
C                    DATA FOR ELEMENT 16      SULFUR
      DATA REFPM3 (16)/'  S: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1989 10(2), 209.               '/
      DATA USSPM3( 16)/     -49.8953710D0/
      DATA UPPPM3( 16)/     -44.3925830D0/
      DATA BETASP( 16)/      -8.8274650D0/
      DATA BETAPP( 16)/      -8.0914150D0/
      DATA ZSPM3 ( 16)/       1.8911850D0/
      DATA ZPPM3 ( 16)/       1.6589720D0/
      DATA ZDPM3 ( 16)/       1.0000000D0/
      DATA ALPPM3( 16)/       2.2697060D0/
      DATA EISOLP( 16)/    -183.4537395D0/
      DATA GSSPM3( 16)/       8.9646670D0/
      DATA GSPPM3( 16)/       6.7859360D0/
      DATA GPPPM3( 16)/       9.9681640D0/
      DATA GP2PM3( 16)/       7.9702470D0/
      DATA HSPPM3( 16)/       4.0418360D0/
      DATA DDPM3 ( 16)/       1.1214313D0/
      DATA QQPM3 ( 16)/       1.0086488D0/
      DATA AMPM3 ( 16)/       0.3294622D0/
      DATA ADPM3 ( 16)/       0.6679118D0/
      DATA AQPM3 ( 16)/       0.6137472D0/
      DATA GAUSP1( 16,1)/      -0.3991910D0/
      DATA GAUSP2( 16,1)/       6.0006690D0/
      DATA GAUSP3( 16,1)/       0.9621230D0/
      DATA GAUSP1( 16,2)/      -0.0548990D0/
      DATA GAUSP2( 16,2)/       6.0018450D0/
      DATA GAUSP3( 16,2)/       1.5799440D0/
C                    DATA FOR ELEMENT 17      CHLORINE
      DATA REFPM3 (17)/' Cl: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1989 10(2), 209.               '/
      DATA USSPM3( 17)/    -100.6267470D0/
      DATA UPPPM3( 17)/     -53.6143960D0/
      DATA BETASP( 17)/     -27.5285600D0/
      DATA BETAPP( 17)/     -11.5939220D0/
      DATA ZSPM3 ( 17)/       2.2462100D0/
      DATA ZPPM3 ( 17)/       2.1510100D0/
      DATA ZDPM3 ( 17)/       1.0000000D0/
      DATA ALPPM3( 17)/       2.5172960D0/
      DATA EISOLP( 17)/    -315.1949480D0/
      DATA GSSPM3( 17)/      16.0136010D0/
      DATA GSPPM3( 17)/       8.0481150D0/
      DATA GPPPM3( 17)/       7.5222150D0/
      DATA GP2PM3( 17)/       7.5041540D0/
      DATA HSPPM3( 17)/       3.4811530D0/
      DATA DDPM3 ( 17)/       0.9175856D0/
      DATA QQPM3 ( 17)/       0.7779230D0/
      DATA AMPM3 ( 17)/       0.5885190D0/
      DATA ADPM3 ( 17)/       0.6814522D0/
      DATA AQPM3 ( 17)/       0.3643694D0/
      DATA GAUSP1( 17,1)/      -0.1715910D0/
      DATA GAUSP2( 17,1)/       6.0008020D0/
      DATA GAUSP3( 17,1)/       1.0875020D0/
      DATA GAUSP1( 17,2)/      -0.0134580D0/
      DATA GAUSP2( 17,2)/       1.9666180D0/
      DATA GAUSP3( 17,2)/       2.2928910D0/
C                    DATA FOR ELEMENT 30        ZINC
      DATA REFPM3 (30)/' Zn: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 30)/     -18.5321980D0/
      DATA UPPPM3( 30)/     -11.0474090D0/
      DATA BETASP( 30)/      -0.7155780D0/
      DATA BETAPP( 30)/      -6.3518640D0/
      DATA ZSPM3 ( 30)/       1.8199890D0/
      DATA ZPPM3 ( 30)/       1.5069220D0/
      DATA ZDPM3 ( 30)/       1.0000000D0/
      DATA ALPPM3( 30)/       1.3501260D0/
      DATA EISOLP( 30)/     -27.3872000D0/
      DATA GSSPM3( 30)/       9.6771960D0/
      DATA GSPPM3( 30)/       7.7362040D0/
      DATA GPPPM3( 30)/       4.9801740D0/
      DATA GP2PM3( 30)/       4.6696560D0/
      DATA HSPPM3( 30)/       0.6004130D0/
      DATA DDPM3 ( 30)/       1.5005758D0/
      DATA QQPM3 ( 30)/       1.4077174D0/
      DATA AMPM3 ( 30)/       0.3556485D0/
      DATA ADPM3 ( 30)/       0.2375689D0/
      DATA AQPM3 ( 30)/       0.2661069D0/
      DATA GAUSP1( 30,1)/      -0.1112340D0/
      DATA GAUSP2( 30,1)/       6.0014780D0/
      DATA GAUSP3( 30,1)/       1.5160320D0/
      DATA GAUSP1( 30,2)/      -0.1323700D0/
      DATA GAUSP2( 30,2)/       1.9958390D0/
      DATA GAUSP3( 30,2)/       2.5196420D0/
C                    DATA FOR ELEMENT 31        GALLIUM
      DATA REFPM3 (31)/' Ga: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 31)/     -29.8555930D0/
      DATA UPPPM3( 31)/     -21.8753710D0/
      DATA BETASP( 31)/      -4.9456180D0/
      DATA BETAPP( 31)/      -0.4070530D0/
      DATA ZSPM3 ( 31)/       1.8470400D0/
      DATA ZPPM3 ( 31)/       0.8394110D0/
      DATA ALPPM3( 31)/       1.6051150D0/
      DATA EISOLP( 31)/     -57.3280250D0/
      DATA GSSPM3( 31)/       8.4585540D0/
      DATA GSPPM3( 31)/       8.9256190D0/
      DATA GPPPM3( 31)/       5.0868550D0/
      DATA GP2PM3( 31)/       4.9830450D0/
      DATA HSPPM3( 31)/       2.0512600D0/
      DATA DDPM3 ( 31)/       0.9776692D0/
      DATA QQPM3 ( 31)/       2.5271534D0/
      DATA AMPM3 ( 31)/       0.3108620D0/
      DATA ADPM3 ( 31)/       0.5129360D0/
      DATA AQPM3 ( 31)/       0.1546208D0/
      DATA GAUSP1( 31,1)/      -0.5601790D0/
      DATA GAUSP2( 31,1)/       5.6232730D0/
      DATA GAUSP3( 31,1)/       1.5317800D0/
      DATA GAUSP1( 31,2)/      -0.2727310D0/
      DATA GAUSP2( 31,2)/       1.9918430D0/
      DATA GAUSP3( 31,2)/       2.1838640D0/
C                    DATA FOR ELEMENT 32        GERMANIUM
      DATA REFPM3 (32)/' Ge: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 32)/     -35.4671955D0/
      DATA UPPPM3( 32)/     -31.5863583D0/
      DATA BETASP( 32)/      -5.3250024D0/
      DATA BETAPP( 32)/      -2.2501567D0/
      DATA ZSPM3 ( 32)/       2.2373526D0/
      DATA ZPPM3 ( 32)/       1.5924319D0/
      DATA ALPPM3( 32)/       1.9723370D0/
      DATA EISOLP( 32)/     -84.0156006D0/
      DATA GSSPM3( 32)/       5.3769635D0/
      DATA GSPPM3( 32)/      10.2095293D0/
      DATA GPPPM3( 32)/       7.6718647D0/
      DATA GP2PM3( 32)/       6.9242663D0/
      DATA HSPPM3( 32)/       1.3370204D0/
      DATA DDPM3 ( 32)/       1.1920304D0/
      DATA QQPM3 ( 32)/       1.3321263D0/
      DATA AMPM3 ( 32)/       0.1976098D0/
      DATA ADPM3 ( 32)/       0.3798182D0/
      DATA AQPM3 ( 32)/       0.3620669D0/
      DATA GAUSP1( 32,1)/       0.9631726D0/
      DATA GAUSP2( 32,1)/       6.0120134D0/
      DATA GAUSP3( 32,1)/       2.1633655D0/
      DATA GAUSP1( 32,2)/      -0.9593891D0/
      DATA GAUSP2( 32,2)/       5.7491802D0/
      DATA GAUSP3( 32,2)/       2.1693724D0/
C                    DATA FOR ELEMENT 33        ARSENIC
      DATA REFPM3 (33)/' As: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 33)/     -38.5074240D0/
      DATA UPPPM3( 33)/     -35.1524150D0/
      DATA BETASP( 33)/      -8.2321650D0/
      DATA BETAPP( 33)/      -5.0173860D0/
      DATA ZSPM3 ( 33)/       2.6361770D0/
      DATA ZPPM3 ( 33)/       1.7038890D0/
      DATA ALPPM3( 33)/       1.7944770D0/
      DATA EISOLP( 33)/    -122.6326140D0/
      DATA GSSPM3( 33)/       8.7890010D0/
      DATA GSPPM3( 33)/       5.3979830D0/
      DATA GPPPM3( 33)/       8.2872500D0/
      DATA GP2PM3( 33)/       8.2103460D0/
      DATA HSPPM3( 33)/       1.9510340D0/
      DATA DDPM3 ( 33)/       0.9679655D0/
      DATA QQPM3 ( 33)/       1.2449874D0/
      DATA AMPM3 ( 33)/       0.3230063D0/
      DATA ADPM3 ( 33)/       0.5042239D0/
      DATA AQPM3 ( 33)/       0.2574219D0/
      DATA GAUSP1( 33,1)/      -0.4600950D0/
      DATA GAUSP2( 33,1)/       1.9831150D0/
      DATA GAUSP3( 33,1)/       1.0867930D0/
      DATA GAUSP1( 33,2)/      -0.0889960D0/
      DATA GAUSP2( 33,2)/       1.9929440D0/
      DATA GAUSP3( 33,2)/       2.1400580D0/
C                    DATA FOR ELEMENT 34        SELENIUM
      DATA REFPM3 (34)/' Se: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 34)/     -55.3781350D0/
      DATA UPPPM3( 34)/     -49.8230760D0/
      DATA BETASP( 34)/      -6.1578220D0/
      DATA BETAPP( 34)/      -5.4930390D0/
      DATA ZSPM3 ( 34)/       2.8280510D0/
      DATA ZPPM3 ( 34)/       1.7325360D0/
      DATA ALPPM3( 34)/       3.0439570D0/
      DATA EISOLP( 34)/    -192.7748115D0/
      DATA GSSPM3( 34)/       7.4325910D0/
      DATA GSPPM3( 34)/      10.0604610D0/
      DATA GPPPM3( 34)/       9.5683260D0/
      DATA GP2PM3( 34)/       7.7242890D0/
      DATA HSPPM3( 34)/       4.0165580D0/
      DATA DDPM3 ( 34)/       0.8719813D0/
      DATA QQPM3 ( 34)/       1.2244019D0/
      DATA AMPM3 ( 34)/       0.2731566D0/
      DATA ADPM3 ( 34)/       0.7509697D0/
      DATA AQPM3 ( 34)/       0.5283737D0/
      DATA GAUSP1( 34,1)/       0.0478730D0/
      DATA GAUSP2( 34,1)/       6.0074000D0/
      DATA GAUSP3( 34,1)/       2.0817170D0/
      DATA GAUSP1( 34,2)/       0.1147200D0/
      DATA GAUSP2( 34,2)/       6.0086720D0/
      DATA GAUSP3( 34,2)/       1.5164230D0/
C                    DATA FOR ELEMENT 35      BROMINE
      DATA REFPM3 (35)/' Br: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1989 10(2), 209.               '/
      DATA USSPM3( 35)/    -116.6193110D0/
      DATA UPPPM3( 35)/     -74.2271290D0/
      DATA BETASP( 35)/     -31.1713420D0/
      DATA BETAPP( 35)/      -6.8140130D0/
      DATA ZSPM3 ( 35)/       5.3484570D0/
      DATA ZPPM3 ( 35)/       2.1275900D0/
      DATA ZDPM3 ( 35)/       1.0000000D0/
      DATA ALPPM3( 35)/       2.5118420D0/
      DATA EISOLP( 35)/    -352.5398970D0/
      DATA GSSPM3( 35)/      15.9434250D0/
      DATA GSPPM3( 35)/      16.0616800D0/
      DATA GPPPM3( 35)/       8.2827630D0/
      DATA GP2PM3( 35)/       7.8168490D0/
      DATA HSPPM3( 35)/       0.5788690D0/
      DATA DDPM3 ( 35)/       0.2759025D0/
      DATA QQPM3 ( 35)/       0.9970532D0/
      DATA AMPM3 ( 35)/       0.5859399D0/
      DATA ADPM3 ( 35)/       0.6755383D0/
      DATA AQPM3 ( 35)/       0.3823719D0/
      DATA GAUSP1( 35,1)/       0.9604580D0/
      DATA GAUSP2( 35,1)/       5.9765080D0/
      DATA GAUSP3( 35,1)/       2.3216540D0/
      DATA GAUSP1( 35,2)/      -0.9549160D0/
      DATA GAUSP2( 35,2)/       5.9447030D0/
      DATA GAUSP3( 35,2)/       2.3281420D0/
C                    DATA FOR ELEMENT 48        CADMIUM
      DATA REFPM3 (48)/' Cd: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 48)/     -15.8285840D0/
      DATA UPPPM3( 48)/       8.7497950D0/
      DATA BETASP( 48)/      -8.5819440D0/
      DATA BETAPP( 48)/      -0.6010340D0/
      DATA ZSPM3 ( 48)/       1.6793510D0/
      DATA ZPPM3 ( 48)/       2.0664120D0/
      DATA ALPPM3( 48)/       1.5253820D0/
      DATA EISOLP( 48)/     -22.4502080D0/
      DATA GSSPM3( 48)/       9.2069600D0/
      DATA GSPPM3( 48)/       8.2315390D0/
      DATA GPPPM3( 48)/       4.9481040D0/
      DATA GP2PM3( 48)/       4.6696560D0/
      DATA HSPPM3( 48)/       1.6562340D0/
      DATA DDPM3 ( 48)/       1.5982681D0/
      DATA QQPM3 ( 48)/       1.2432402D0/
      DATA AMPM3 ( 48)/       0.3383668D0/
      DATA ADPM3 ( 48)/       0.3570290D0/
      DATA AQPM3 ( 48)/       0.2820582D0/
C                    DATA FOR ELEMENT 49        INDIUM
      DATA REFPM3 (49)/' In: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 49)/     -26.1762050D0/
      DATA UPPPM3( 49)/     -20.0058220D0/
      DATA BETASP( 49)/      -2.9933190D0/
      DATA BETAPP( 49)/      -1.8289080D0/
      DATA ZSPM3 ( 49)/       2.0161160D0/
      DATA ZPPM3 ( 49)/       1.4453500D0/
      DATA ALPPM3( 49)/       1.4183850D0/
      DATA EISOLP( 49)/     -51.9750470D0/
      DATA GSSPM3( 49)/       6.5549000D0/
      DATA GSPPM3( 49)/       8.2298730D0/
      DATA GPPPM3( 49)/       6.2992690D0/
      DATA GP2PM3( 49)/       4.9842110D0/
      DATA HSPPM3( 49)/       2.6314610D0/
      DATA DDPM3 ( 49)/       1.5766241D0/
      DATA QQPM3 ( 49)/       1.7774563D0/
      DATA AMPM3 ( 49)/       0.2409004D0/
      DATA ADPM3 ( 49)/       0.4532655D0/
      DATA AQPM3 ( 49)/       0.3689812D0/
      DATA GAUSP1( 49,1)/      -0.3431380D0/
      DATA GAUSP2( 49,1)/       1.9940340D0/
      DATA GAUSP3( 49,1)/       1.6255160D0/
      DATA GAUSP1( 49,2)/      -0.1095320D0/
      DATA GAUSP2( 49,2)/       5.6832170D0/
      DATA GAUSP3( 49,2)/       2.8670090D0/
C                    DATA FOR ELEMENT 50        TIN
      DATA REFPM3 (50)/' Sn: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 50)/     -34.5501920D0/
      DATA UPPPM3( 50)/     -25.8944190D0/
      DATA BETASP( 50)/      -2.7858020D0/
      DATA BETAPP( 50)/      -2.0059990D0/
      DATA ZSPM3 ( 50)/       2.3733280D0/
      DATA ZPPM3 ( 50)/       1.6382330D0/
      DATA ALPPM3( 50)/       1.6996500D0/
      DATA EISOLP( 50)/     -78.8877790D0/
      DATA GSSPM3( 50)/      10.1900330D0/
      DATA GSPPM3( 50)/       7.2353270D0/
      DATA GPPPM3( 50)/       5.6738100D0/
      DATA GP2PM3( 50)/       5.1822140D0/
      DATA HSPPM3( 50)/       1.0331570D0/
      DATA DDPM3 ( 50)/       1.3120038D0/
      DATA QQPM3 ( 50)/       1.5681814D0/
      DATA AMPM3 ( 50)/       0.3744959D0/
      DATA ADPM3 ( 50)/       0.3218163D0/
      DATA AQPM3 ( 50)/       0.2832529D0/
      DATA GAUSP1( 50,1)/      -0.1503530D0/
      DATA GAUSP2( 50,1)/       6.0056940D0/
      DATA GAUSP3( 50,1)/       1.7046420D0/
      DATA GAUSP1( 50,2)/      -0.0444170D0/
      DATA GAUSP2( 50,2)/       2.2573810D0/
      DATA GAUSP3( 50,2)/       2.4698690D0/
C                    DATA FOR ELEMENT 51        ANTIMONY
      DATA REFPM3 (51)/' Sb: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 51)/     -56.4321960D0/
      DATA UPPPM3( 51)/     -29.4349540D0/
      DATA BETASP( 51)/     -14.7942170D0/
      DATA BETAPP( 51)/      -2.8179480D0/
      DATA ZSPM3 ( 51)/       2.3430390D0/
      DATA ZPPM3 ( 51)/       1.8999920D0/
      DATA ALPPM3( 51)/       2.0343010D0/
      DATA EISOLP( 51)/    -148.9382890D0/
      DATA GSSPM3( 51)/       9.2382770D0/
      DATA GSPPM3( 51)/       5.2776800D0/
      DATA GPPPM3( 51)/       6.3500000D0/
      DATA GP2PM3( 51)/       6.2500000D0/
      DATA HSPPM3( 51)/       2.4244640D0/
      DATA DDPM3 ( 51)/       1.4091903D0/
      DATA QQPM3 ( 51)/       1.3521354D0/
      DATA AMPM3 ( 51)/       0.3395177D0/
      DATA ADPM3 ( 51)/       0.4589010D0/
      DATA AQPM3 ( 51)/       0.2423472D0/
      DATA GAUSP1( 51,1)/       3.0020280D0/
      DATA GAUSP2( 51,1)/       6.0053420D0/
      DATA GAUSP3( 51,1)/       0.8530600D0/
      DATA GAUSP1( 51,2)/      -0.0188920D0/
      DATA GAUSP2( 51,2)/       6.0114780D0/
      DATA GAUSP3( 51,2)/       2.7933110D0/
C                    DATA FOR ELEMENT 52        TELLURIUM
      DATA REFPM3 (52)/' Te: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 52)/     -44.9380360D0/
      DATA UPPPM3( 52)/     -46.3140990D0/
      DATA BETASP( 52)/      -2.6651460D0/
      DATA BETAPP( 52)/      -3.8954300D0/
      DATA ZSPM3 ( 52)/       4.1654920D0/
      DATA ZPPM3 ( 52)/       1.6475550D0/
      DATA ALPPM3( 52)/       2.4850190D0/
      DATA EISOLP( 52)/    -168.0945925D0/
      DATA GSSPM3( 52)/      10.2550730D0/
      DATA GSPPM3( 52)/       8.1691450D0/
      DATA GPPPM3( 52)/       7.7775920D0/
      DATA GP2PM3( 52)/       7.7551210D0/
      DATA HSPPM3( 52)/       3.7724620D0/
      DATA DDPM3 ( 52)/       0.3484177D0/
      DATA QQPM3 ( 52)/       1.5593085D0/
      DATA AMPM3 ( 52)/       0.3768862D0/
      DATA ADPM3 ( 52)/       1.1960743D0/
      DATA AQPM3 ( 52)/       0.2184786D0/
      DATA GAUSP1( 52,1)/       0.0333910D0/
      DATA GAUSP2( 52,1)/       5.9563790D0/
      DATA GAUSP3( 52,1)/       2.2775750D0/
      DATA GAUSP1( 52,2)/      -1.9218670D0/
      DATA GAUSP2( 52,2)/       4.9732190D0/
      DATA GAUSP3( 52,2)/       0.5242430D0/
C                    DATA FOR ELEMENT 53      IODINE
      DATA REFPM3 (53)/'  I: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1989 10(2), 209.               '/
      DATA USSPM3( 53)/     -96.4540370D0/
      DATA UPPPM3( 53)/     -61.0915820D0/
      DATA BETASP( 53)/     -14.4942340D0/
      DATA BETAPP( 53)/      -5.8947030D0/
      DATA ZSPM3 ( 53)/       7.0010130D0/
      DATA ZPPM3 ( 53)/       2.4543540D0/
      DATA ZDPM3 ( 53)/       1.0000000D0/
      DATA ALPPM3( 53)/       1.9901850D0/
      DATA EISOLP( 53)/    -288.3160860D0/
      DATA GSSPM3( 53)/      13.6319430D0/
      DATA GSPPM3( 53)/      14.9904060D0/
      DATA GPPPM3( 53)/       7.2883300D0/
      DATA GP2PM3( 53)/       5.9664070D0/
      DATA HSPPM3( 53)/       2.6300350D0/
      DATA DDPM3 ( 53)/       0.1581469D0/
      DATA QQPM3 ( 53)/       1.0467302D0/
      DATA AMPM3 ( 53)/       0.5009902D0/
      DATA ADPM3 ( 53)/       1.6699104D0/
      DATA AQPM3 ( 53)/       0.5153082D0/
      DATA GAUSP1( 53,1)/      -0.1314810D0/
      DATA GAUSP2( 53,1)/       5.2064170D0/
      DATA GAUSP3( 53,1)/       1.7488240D0/
      DATA GAUSP1( 53,2)/      -0.0368970D0/
      DATA GAUSP2( 53,2)/       6.0101170D0/
      DATA GAUSP3( 53,2)/       2.7103730D0/
C                    DATA FOR ELEMENT 80        MERCURY
      DATA REFPM3 (80)/' Hg: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 80)/     -17.7622290D0/
      DATA UPPPM3( 80)/     -18.3307510D0/
      DATA BETASP( 80)/      -3.1013650D0/
      DATA BETAPP( 80)/      -3.4640310D0/
      DATA ZSPM3 ( 80)/       1.4768850D0/
      DATA ZPPM3 ( 80)/       2.4799510D0/
      DATA ALPPM3( 80)/       1.5293770D0/
      DATA EISOLP( 80)/     -28.8997380D0/
      DATA GSSPM3( 80)/       6.6247200D0/
      DATA GSPPM3( 80)/      10.6392970D0/
      DATA GPPPM3( 80)/      14.7092830D0/
      DATA GP2PM3( 80)/      16.0007400D0/
      DATA HSPPM3( 80)/       2.0363110D0/
      DATA DDPM3 ( 80)/       1.2317811D0/
      DATA QQPM3 ( 80)/       1.2164033D0/
      DATA AMPM3 ( 80)/       0.2434664D0/
      DATA ADPM3 ( 80)/       0.4515472D0/
      DATA AQPM3 ( 80)/       0.2618394D0/
      DATA GAUSP1( 80,1)/       1.0827200D0/
      DATA GAUSP2( 80,1)/       6.4965980D0/
      DATA GAUSP3( 80,1)/       1.1951460D0/
      DATA GAUSP1( 80,2)/      -0.0965530D0/
      DATA GAUSP2( 80,2)/       3.9262810D0/
      DATA GAUSP3( 80,2)/       2.6271600D0/
C                    DATA FOR ELEMENT 81        THALLIUM
      DATA REFPM3 (81)/' Tl: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 81)/     -30.0531700D0/
      DATA UPPPM3( 81)/     -26.9206370D0/
      DATA BETASP( 81)/      -1.0844950D0/
      DATA BETAPP( 81)/      -7.9467990D0/
      DATA ZSPM3 ( 81)/       6.8679210D0/
      DATA ZPPM3 ( 81)/       1.9694450D0/
      DATA ALPPM3( 81)/       1.3409510D0/
      DATA EISOLP( 81)/     -56.6492050D0/
      DATA GSSPM3( 81)/      10.4604120D0/
      DATA GSPPM3( 81)/      11.2238830D0/
      DATA GPPPM3( 81)/       4.9927850D0/
      DATA GP2PM3( 81)/       8.9627270D0/
      DATA HSPPM3( 81)/       2.5304060D0/
      DATA DDPM3 ( 81)/       0.0781362D0/
      DATA QQPM3 ( 81)/       1.5317110D0/
      DATA AMPM3 ( 81)/       0.3844326D0/
      DATA ADPM3 ( 81)/       2.5741815D0/
      DATA AQPM3 ( 81)/       0.2213264D0/
      DATA GAUSP1( 81,1)/      -1.3613990D0/
      DATA GAUSP2( 81,1)/       3.5572260D0/
      DATA GAUSP3( 81,1)/       1.0928020D0/
      DATA GAUSP1( 81,2)/      -0.0454010D0/
      DATA GAUSP2( 81,2)/       2.3069950D0/
      DATA GAUSP3( 81,2)/       2.9650290D0/
C                    DATA FOR ELEMENT 82        LEAD
      DATA REFPM3 (82)/' Pb: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 82)/     -30.3227560D0/
      DATA UPPPM3( 82)/     -24.4258340D0/
      DATA BETASP( 82)/      -6.1260240D0/
      DATA BETAPP( 82)/      -1.3954300D0/
      DATA ZSPM3 ( 82)/       3.1412890D0/
      DATA ZPPM3 ( 82)/       1.8924180D0/
      DATA ALPPM3( 82)/       1.6200450D0/
      DATA EISOLP( 82)/     -73.4660775D0/
      DATA GSSPM3( 82)/       7.0119920D0/
      DATA GSPPM3( 82)/       6.7937820D0/
      DATA GPPPM3( 82)/       5.1837800D0/
      DATA GP2PM3( 82)/       5.0456510D0/
      DATA HSPPM3( 82)/       1.5663020D0/
      DATA DDPM3 ( 82)/       0.9866290D0/
      DATA QQPM3 ( 82)/       1.5940562D0/
      DATA AMPM3 ( 82)/       0.2576991D0/
      DATA ADPM3 ( 82)/       0.4527678D0/
      DATA AQPM3 ( 82)/       0.2150175D0/
      DATA GAUSP1( 82,1)/      -0.1225760D0/
      DATA GAUSP2( 82,1)/       6.0030620D0/
      DATA GAUSP3( 82,1)/       1.9015970D0/
      DATA GAUSP1( 82,2)/      -0.0566480D0/
      DATA GAUSP2( 82,2)/       4.7437050D0/
      DATA GAUSP3( 82,2)/       2.8618790D0/
C                    DATA FOR ELEMENT 83        BISMUTH
      DATA REFPM3 (83)/' Bi: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 1991 12(3), 320.               '/
      DATA USSPM3( 83)/     -33.4959380D0/
      DATA UPPPM3( 83)/     -35.5210260D0/
      DATA BETASP( 83)/      -5.6072830D0/
      DATA BETAPP( 83)/      -5.8001520D0/
      DATA ZSPM3 ( 83)/       4.9164510D0/
      DATA ZPPM3 ( 83)/       1.9349350D0/
      DATA ALPPM3( 83)/       1.8574310D0/
      DATA EISOLP( 83)/    -109.2774910D0/
      DATA GSSPM3( 83)/       4.9894800D0/
      DATA GSPPM3( 83)/       6.1033080D0/
      DATA GPPPM3( 83)/       8.6960070D0/
      DATA GP2PM3( 83)/       8.3354470D0/
      DATA HSPPM3( 83)/       0.5991220D0/
      DATA DDPM3 ( 83)/       0.2798609D0/
      DATA QQPM3 ( 83)/       1.5590294D0/
      DATA AMPM3 ( 83)/       0.1833693D0/
      DATA ADPM3 ( 83)/       0.6776013D0/
      DATA AQPM3 ( 83)/       0.2586520D0/
      DATA GAUSP1( 83,1)/       2.5816930D0/
      DATA GAUSP2( 83,1)/       5.0940220D0/
      DATA GAUSP3( 83,1)/       0.4997870D0/
      DATA GAUSP1( 83,2)/       0.0603200D0/
      DATA GAUSP2( 83,2)/       6.0015380D0/
      DATA GAUSP3( 83,2)/       2.4279700D0/
      DATA REFPM3 (102)/' Cb: (PM3):  Capped Bond  (Hydrogen-like, takes
     1 on a  zero charge.)             '/
      DATA USSPM3(102)/     -11.9062760D0/
      DATA BETASP(102)/-9999999.0000000D0/
      DATA ZSPM3 (102)/       4.0000000D0/
      DATA ZPPM3 (102)/       0.3000000D0/
      DATA ZDPM3 (102)/       0.3000000D0/
      DATA ALPPM3(102)/       2.5441341D0/
      DATA EISOLP(102)/       4.0000000D0/
      DATA GSSPM3(102)/      12.8480000D0/
      DATA HSPPM3(102)/       0.1000000D0/
      DATA DDPM3 (102)/       0.0684105D0/
      DATA QQPM3 (102)/       1.0540926D0/
      DATA AMPM3 (102)/       0.4721793D0/
      DATA ADPM3 (102)/       0.9262742D0/
      DATA AQPM3 (102)/       0.2909059D0/
C
C     LF0410: SPECIAL HYDROGEN CONTAINING P ORBITALS.  CAN BE USED BY SPECIFYING
C     "Hp".  ADDED BY LUKE FIEDLER, APR. 2010.
C           DATA FOR ELEMENT  108     PM3:   HYDROGEN (WITH P-ORBITALS)
      DATA REFPM3(108)/' Hp: (PM3): Hydrogen (with p basis functions)  
     1                               '/
      DATA USSPM3(108)/      -7.3964270D0/
      DATA UPPPM3(108)/      -1.1486568D0/
      DATA BETASP(108)/      -9.6737870D0/
      DATA BETAPP(108)/      -1.3000000D0/
      DATA ZSPM3 (108)/       0.9880780D0/
      DATA ZPPM3 (108)/       0.3000000D0/
      DATA ALPPM3(108)/       2.9323240D0/
      DATA EISOLP(108)/      28.7118620D0/
      DATA GSSPM3(108)/      12.8480000D0/
      DATA GSPPM3(108)/       2.1600000D0/
      DATA GPPPM3(108)/       3.0100000D0/
      DATA GP2PM3(108)/       2.4400000D0/
      DATA HSPPM3(108)/       1.9200000D0/
      DATA DDPM3 (108)/       0.9675816D0/
      DATA QQPM3 (108)/       4.0824829D0/
      DATA AMPM3 (108)/       0.4721603D0/
      DATA ADPM3 (108)/       0.5007132D0/
      DATA AQPM3 (108)/       0.1603621D0/
      DATA GAUSP1(108,1)/       0.1227960D0/
      DATA GAUSP2(108,1)/       5.0000000D0/
      DATA GAUSP3(108,1)/       1.2000000D0/
      DATA GAUSP1(108,2)/       0.0050900D0/
      DATA GAUSP2(108,2)/       5.0000000D0/
      DATA GAUSP3(108,2)/       1.8000000D0/
      DATA GAUSP1(108,3)/      -0.0183360D0/
      DATA GAUSP2(108,3)/       2.0000000D0/
      DATA GAUSP3(108,3)/       2.1000000D0/
      DATA GAUSP1(108,4)/       0.0000000D0/
      DATA GAUSP2(108,4)/       0.0000000D0/
      DATA GAUSP3(108,4)/       0.0000000D0/
C

C Added by ZJJ July 25 2006:      
***********************************************************************
*
*     START OF PDDG/PM3 PARAMETERS
*     Reference:  Repasky, Chandrashakar, Jorgensen
*
*                              Units
*     USSPDG, UPPPDG, UDDPDG:  eV
*     BETASG, BETAPG, BETADG:  eV
*     ALPPDG:                  angstrom**(-1)
*     ZSPDG,  ZPPDG,  ZDPDG:   bohr**(-1)
*     GSSPDG, GSPPDG, GPPPDG:  eV 
*     GP2PDG, HSPPDG:          eV
*     EISOLG:                  eV
*     GAUSG1:                  eV * angstrom
*     GAUSG2:                  angstrom**(-2)
*     GAUSG3:                  angstrom
*     AMPDG, ADPDG, AQPDG:     bohr
*     DDPDG, QQPDG:            bohr
*
*
***********************************************************************
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFPDG(1)/' H: (PDDG/PM3): REPASKY, CHADRASEKHAR, JORGENSEN, 
     1 JCC 2002, 23(16), 1601.'/
      DATA USSPDG(1)  /-12.893272003385D0/
      DATA BETASG(1)  /-6.1526542062173D0/
      DATA ZSPDG (1)  /0.97278550084430D0/
      DATA ALPPDG(1)  /3.38168610300700D0/
      DATA EISOLG(1)  /-13.120566198192D0/
      DATA GSSPDG(1)  /      14.7942080D0/
      DATA AMPDG (1)  /0.54370481704970D0/
      DATA ADPDG (1)  /0.54370481704970D0/
      DATA AQPDG (1)  /0.54370481704970D0/
      DATA GAUSG1(1,1)/1.12224395962630D0/
      DATA GAUSG2(1,1)/4.70779030777590D0/
      DATA GAUSG3(1,1)/1.54709920873910D0/
      DATA GAUSG1(1,2)/-1.0697373657305D0/
      DATA GAUSG2(1,2)/5.85799464741120D0/
      DATA GAUSG3(1,2)/1.56789274832050D0/
      DATA PAPDG(1)   /0.05719290135800D0/
      DATA PBPDG(1)   /-0.0348228612590D0/
      DATA DAPDG(1)   /0.66339504047230D0/
      DATA DBPDG(1)   /1.08190071942210D0/
      DATA NGAUSG(1)  /       2/
C                    DATA FOR ELEMENT  6        CARBON
      DATA REFPDG(6)/' C: (PDDG/PM3): REPASKY, CHADRASEKHAR, JORGENSEN,
     1 JCC 2002, 23(16), 1601.'/
      DATA USSPDG(6)  /-48.241240946951D0/
      DATA UPPPDG(6)  /-36.461255999939D0/
      DATA BETASG(6)  /-11.952818190434D0/
      DATA BETAPG(6)  /-9.9224112120852D0/
      DATA ZSPDG (6)  /1.56786358751710D0/
      DATA ZPPDG (6)  /1.84665852120070D0/
      DATA ALPPDG(6)  /2.72577212540530D0/
      DATA EISOLG(6)  /-113.42824208974D0/
      DATA GSSPDG(6)  /      11.2007080D0/
      DATA GSPPDG(6)  /      10.2650270D0/
      DATA GPPPDG(6)  /      10.7962920D0/
      DATA GP2PDG(6)  /       9.0425660D0/
      DATA HSPPDG(6)  /       2.2909800D0/
      DATA DDPDG (6)  /0.83141317761820D0/
      DATA QQPDG (6)  /0.66322216984400D0/
      DATA AMPDG (6)  /0.41163939928160D0/
      DATA ADPDG (6)  /0.58929751233060D0/
      DATA AQPDG (6)  /0.76594914927100D0/
      DATA GAUSG1(6,1)/0.04890550330860D0/
      DATA GAUSG2(6,1)/5.76533980799120D0/
      DATA GAUSG3(6,1)/1.68223169651660D0/
      DATA GAUSG1(6,2)/0.04769663311610D0/
      DATA GAUSG2(6,2)/5.97372073873460D0/
      DATA GAUSG3(6,2)/0.89440631619350D0/
      DATA PAPDG(6)   /-0.0007433618099D0/
      DATA PBPDG(6)   /0.00098516072940D0/
      DATA DAPDG(6)   /0.83691519687330D0/
      DATA DBPDG(6)   /1.58523608520060D0/
      DATA NGAUSG(6)  /       2/
C                    DATA FOR ELEMENT  7        NITROGEN
      DATA REFPDG(7)/' N: (PDDG/PM3): REPASKY, CHADRASEKHAR, JORGENSEN,
     1 JCC 2002, 23(16), 1601.'/
      DATA USSPDG(7)  /-49.454546358059D0/
      DATA UPPPDG(7)  /-47.757406358412D0/
      DATA BETASG(7)  /-14.117229602371D0/
      DATA BETAPG(7)  /-19.938508878969D0/
      DATA ZSPDG (7)  /2.03580684361910D0/
      DATA ZPPDG (7)  /2.32432725808280D0/
      DATA ALPPDG(7)  /2.84912399973850D0/
      DATA EISOLG(7)  /-158.41620481951D0/
      DATA GSSPDG(7)  /      11.9047870D0/
      DATA GSPPDG(7)  /       7.3485650D0/
      DATA GPPPDG(7)  /      11.7546720D0/
      DATA GP2PDG(7)  /      10.8072770D0/
      DATA HSPPDG(7)  /       1.1367130D0/
      DATA DDPDG (7)  /0.65485453533410D0/
      DATA QQPDG (7)  /0.52692445400390D0/
      DATA AMPDG (7)  /0.43751514361910D0/
      DATA ADPDG (7)  /0.50442060161600D0/
      DATA AQPDG (7)  /0.73887610477190D0/
      DATA GAUSG1(7,1)/1.51332030575080D0/
      DATA GAUSG2(7,1)/5.90439402634500D0/
      DATA GAUSG3(7,1)/1.72837621719040D0/
      DATA GAUSG1(7,2)/-1.5118916914302D0/
      DATA GAUSG2(7,2)/6.03001440913320D0/
      DATA GAUSG3(7,2)/1.73410826456840D0/
      DATA PAPDG(7)   /-0.0031600751673D0/
      DATA PBPDG(7)   /0.01250092178130D0/
      DATA DAPDG(7)   /1.00417177651930D0/
      DATA DBPDG(7)   /1.51633618021020D0/
      DATA NGAUSG(7)  /       2/
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFPDG(8)/' O: (PDDG/PM3): REPASKY, CHADRASEKHAR, JORGENSEN,
     1 JCC 2002, 23(16), 1601.'/
      DATA USSPDG(8)  /-87.412505208248D0/
      DATA UPPPDG(8)  /-72.183069806393D0/
      DATA BETASG(8)  /-44.874553472211D0/
      DATA BETAPG(8)  /-24.601939339720D0/
      DATA ZSPDG (8)  /3.81456531095080D0/
      DATA ZPPDG (8)  /2.31801122165690D0/
      DATA ALPPDG(8)  /3.22530882036500D0/
      DATA EISOLG(8)  /-292.18876564023D0/
      DATA GSSPDG(8)  /      15.7557600D0/
      DATA GSPPDG(8)  /      10.6211600D0/
      DATA GPPPDG(8)  /      13.6540160D0/
      DATA GP2PDG(8)  /      12.4060950D0/
      DATA HSPPDG(8)  /       0.5938830D0/
      DATA DDPDG (8)  /0.40374106034030D0/
      DATA QQPDG (8)  /0.52836019944550D0/
      DATA AMPDG (8)  /0.57904300171250D0/
      DATA ADPDG (8)  /0.53403626392780D0/
      DATA AQPDG (8)  /0.80090914697820D0/
      DATA GAUSG1(8,1)/-1.1384554300359D0/
      DATA GAUSG2(8,1)/6.00004254473730D0/
      DATA GAUSG3(8,1)/1.62236167639400D0/
      DATA GAUSG1(8,2)/1.14600702743950D0/
      DATA GAUSG2(8,2)/5.96349383486760D0/
      DATA GAUSG3(8,2)/1.61478803799000D0/
      DATA PAPDG(8)  /-0.00099962677420D0/
      DATA PBPDG(8)  /-0.00152161350520D0/
      DATA DAPDG(8)  /1.36068502987020D0/
      DATA DBPDG(8)  /1.36640659538530D0/
      DATA NGAUSG(8) /       2/
C                    DATA FOR ELEMENT 9 Fluorine
      DATA REFPDG(9) /' F: (PDDG/PM3): TUBERT-BROHMAN,GUIMARAES,REPASKY,
     1JORGENSEN,JCC 2004 25,138.'/
      DATA USSPDG(9)  /-111.400432D0/
      DATA UPPPDG(9)  /-106.395264D0/
      DATA BETASG(9)  / -50.937301D0/
      DATA BETAPG(9)  / -31.636976D0/
      DATA ZSPDG (9)  /   5.538033D0/
      DATA ZPPDG (9)  /   2.538066D0/
      DATA ALPPDG(9)  /   3.200571D0/
      DATA EISOLG(9)  /-442.457133D0/
      DATA GSSPDG(9)/   10.4966670D0/
      DATA GSPPDG(9)/   16.0736890D0/
      DATA GPPPDG(9)/   14.8172560D0/
      DATA GP2PDG(9)/   14.4183930D0/
      DATA HSPPDG(9)/    0.7277630D0/
      DATA DDPDG (9)  /  0.246601D0/
      DATA QQPDG (9)  /  0.482551D0/
      DATA AMPDG (9)/   0.3857650D0/
      DATA ADPDG (9)/   0.7878569D0/
      DATA AQPDG (9)/   0.6204998D0/
      DATA GAUSG1(9,1)/  -0.008079D0/
      DATA GAUSG2(9,1)/   5.938969D0/
      DATA GAUSG3(9,1)/   1.863949D0/
      DATA GAUSG1(9,2)/  -0.002659D0/
      DATA GAUSG2(9,2)/   5.925105D0/
      DATA GAUSG3(9,2)/   2.388864D0/
      DATA PAPDG (9)  /  -0.012866D0/
      DATA PBPDG (9)  /   0.007315D0/
      DATA DAPDG (9)  /   1.305681D0/
      DATA DBPDG (9)  /   1.842572D0/
      DATA NGAUSG(9)  /   2         /
C                      DATA FOR ELEMENT 14 SILICON
      DATA REFPDG(14)  /' Si: (PDDG/PM3): TUBERT-BROHMAN,GUIMARAES,JORGE
     1NSEN, JCTC 2005 1, 817.'/
      DATA USSPDG(14)  / -26.332522D0/
      DATA UPPPDG(14)  / -22.602540D0/
      DATA BETASG(14)  /  -3.376445D0/
      DATA BETAPG(14)  /  -3.620969D0/
      DATA ZSPDG (14)  /   1.586389D0/
      DATA ZPPDG (14)  /   1.485958D0/
      DATA ALPPDG(14)  /   2.215157D0/
      DATA EISOLG(14)  / -66.839000D0/
      DATA GSSPDG(14)  /  5.0471960D0/
      DATA GSPPDG(14)  /  5.9490570D0/
      DATA GPPPDG(14)  /  6.7593670D0/
      DATA GP2PDG(14)  /  5.1612970D0/
      DATA HSPPDG(14)  /  0.9198320D0/
      DATA DDPDG (14)  /   1.310515D0/
      DATA QQPDG (14)  /   1.126089D0/
      DATA AMPDG (14)  /   0.185490D0/
      DATA ADPDG (14)  /   0.306606D0/
      DATA AQPDG (14)  /   0.526759D0/
      DATA GAUSG1(14,1)/  -0.071314D0/
      DATA GAUSG2(14,1)/   6.000000D0/
      DATA GAUSG3(14,1)/   0.237995D0/
      DATA GAUSG1(14,2)/   0.089451D0/
      DATA GAUSG2(14,2)/   6.000000D0/
      DATA GAUSG3(14,2)/   1.897728D0/
      DATA PAPDG (14)  /  -0.091928D0/
      DATA PBPDG (14)  /  -0.040753D0/
      DATA DAPDG (14)  /   1.163190D0/
      DATA DBPDG (14)  /   2.190526D0/
      DATA NGAUSG(14)  /   2         /
C                      DATA FOR ELEMENT 15 PHOSPHORUS
      DATA REFPDG(15)  /' P: (PDDG/PM3): TUBERT-BROHMAN,GUIMARAES,JORGEN
     1SEN, JCTC 2005 1, 817.'/
      DATA USSPDG(15)  / -37.882113D0/
      DATA UPPPDG(15)  / -30.312979D0/
      DATA BETASG(15)  / -12.676297D0/
      DATA BETAPG(15)  /  -7.093318D0/
      DATA ZSPDG (15)  /   2.395882D0/
      DATA ZPPDG (15)  /   1.742213D0/
      DATA ALPPDG(15)  /   2.005294D0/
      DATA EISOLG(15)  /-117.212854D0/
      DATA GSSPDG(15)  /   7.8016150D0/
      DATA GSPPDG(15)  /   5.1869490D0/
      DATA GPPPDG(15)  /   6.6184780D0/
      DATA GP2PDG(15)  /   6.0620020D0/
      DATA HSPPDG(15)  /   1.5428090D0/
      DATA DDPDG (15)  /   0.893978D0/
      DATA QQPDG (15)  /   0.960457D0/
      DATA AMPDG (15)  /   0.286719D0/
      DATA ADPDG (15)  /   0.475805D0/
      DATA AQPDG (15)  /   0.413597D0/
      DATA GAUSG1(15,1)/  -0.398055D0/
      DATA GAUSG2(15,1)/   1.997272D0/
      DATA GAUSG3(15,1)/   0.950073D0/
      DATA GAUSG1(15,2)/  -0.079653D0/
      DATA GAUSG2(15,2)/   1.998360D0/
      DATA GAUSG3(15,2)/   2.336959D0/
      DATA PAPDG (15)  /   0.462741D0/
      DATA PBPDG (15)  /  -0.020444D0/
      DATA DAPDG (15)  /   0.714296D0/
      DATA DBPDG (15)  /   2.041209D0/
      DATA NGAUSG(15)  /   2         / 
C                      DATA FOR ELEMENT 16 SULFUR
      DATA REFPDG(16)  /' S: (PDDG/PM3): TUBERT-BROHMAN,GUIMARAES,JORGEN
     1SEN, JCTC 2005 1, 817.'/
      DATA USSPDG(16)  / -43.906366D0/
      DATA UPPPDG(16)  / -43.461348D0/
      DATA BETASG(16)  /  -2.953912D0/
      DATA BETAPG(16)  /  -8.507779D0/
      DATA ZSPDG (16)  /   1.012002D0/
      DATA ZPPDG (16)  /   1.876999D0/
      DATA ALPPDG(16)  /   2.539751D0/
      DATA EISOLG(16)  /-166.336554D0/
      DATA GSSPDG(16)  /   8.9646670D0/
      DATA GSPPDG(16)  /   6.7859360D0/
      DATA GPPPDG(16)  /   9.9681640D0/
      DATA GP2PDG(16)  /   7.9702470D0/
      DATA HSPPDG(16)  /   4.0418360D0/
      DATA DDPDG (16)  /   1.006989D0/
      DATA QQPDG (16)  /   0.891487D0/
      DATA AMPDG (16)  /   0.329462D0/
      DATA ADPDG (16)  /   0.702571D0/
      DATA AQPDG (16)  /   0.662835D0/
      DATA GAUSG1(16,1)/  -0.330692D0/
      DATA GAUSG2(16,1)/   6.000000D0/
      DATA GAUSG3(16,1)/   0.823837D0/
      DATA GAUSG1(16,2)/   0.024171D0/
      DATA GAUSG2(16,2)/   6.000000D0/
      DATA GAUSG3(16,2)/   2.017756D0/
      DATA PAPDG (16)  /   0.120434D0/
      DATA PBPDG (16)  /  -0.002663D0/
      DATA DAPDG (16)  /   0.672870D0/
      DATA DBPDG (16)  /   2.032340D0/
      DATA NGAUSG(16)  /   2         /
C                      DATA FOR ELEMENT 17 CHLORINE
      DATA REFPDG(17)  /' F: (PDDG/PM3): TUBERT-BROHMAN,GUIMARAES,REPASK
     1Y,JORGENSEN,JCC 2004 25,138.'/
      DATA USSPDG(17)  / -95.094434D0/
      DATA UPPPDG(17)  / -53.921715D0/
      DATA BETASG(17)  / -26.913129D0/
      DATA BETAPG(17)  / -14.991178D0/
      DATA ZSPDG (17)  /   2.548268D0/
      DATA ZPPDG (17)  /   2.284624D0/
      DATA ALPPDG(17)  /   2.497617D0/
      DATA EISOLG(17)  /-305.715201D0/
      DATA GSSPDG(17)  /  16.0136010D0/
      DATA GSPPDG(17)  /   8.0481150D0/
      DATA GPPPDG(17)  /   7.5222150D0/
      DATA GP2PDG(17)  /   7.5041540D0/
      DATA HSPPDG(17)  /   3.4811530D0/
      DATA DDPDG (17)  /   0.827561D0/
      DATA QQPDG (17)  /   0.732427D0/
      DATA AMPDG (17)  /   0.5885190D0/
      DATA ADPDG (17)  /   0.7182216D0/
      DATA AQPDG (17)  /   0.2174760D0/
      DATA GAUSG1(17,1)/  -0.112222D0/
      DATA GAUSG2(17,1)/   5.963719D0/
      DATA GAUSG3(17,1)/   1.027719D0/
      DATA GAUSG1(17,2)/  -0.013061D0/
      DATA GAUSG2(17,2)/   1.999556D0/
      DATA GAUSG3(17,2)/   2.286377D0/
      DATA PAPDG (17)  /  -0.016552D0/
      DATA PBPDG (17)  /  -0.016646D0/
      DATA DAPDG (17)  /   1.727690D0/
      DATA DBPDG (17)  /   1.784655D0/
      DATA NGAUSG(17)  /   2         /  
C                       DATA FOR ELEMENT 35 BROMINE
      DATA REFPDG(35)  /' Br: (PDDG/PM3): TUBERT-BROHMAN,GUIMARAES,REPAS
     1KY,JORGENSEN,JCC 2004 25,138.'/       
      DATA USSPDG(35)  /-115.841963D0/
      DATA UPPPDG(35)  / -74.205146D0/
      DATA BETASG(35)  / -21.538044D0/
      DATA BETAPG(35)  /  -8.524764D0/
      DATA ZSPDG (35)  /   4.345079D0/
      DATA ZPPDG (35)  /   2.190961D0/
      DATA ALPPDG(35)  /   2.424673D0/
      DATA EISOLG(35)  /-351.013887D0/
      DATA GSSPDG(35)/      15.9434250D0/
      DATA GSPPDG(35)/      16.0616800D0/
      DATA GPPPDG(35)/       8.2827630D0/
      DATA GP2PDG(35)/       7.8168490D0/
      DATA HSPPDG(35)/       0.5788690D0/
      DATA DDPDG (35)/       0.473860D0/                   
      DATA QQPDG (35)/       0.968214D0/            
      DATA AMPDG (35)/       0.585940D0/            
      DATA ADPDG (35)/       0.477815D0/            
      DATA AQPDG (35)/       0.390429D0/              
      DATA GAUSG1(35,1)/     0.961362D0/ 
      DATA GAUSG2(35,1)/     6.013600D0/
      DATA GAUSG3(35,1)/     2.340445D0/
      DATA GAUSG1(35,2)/    -0.948834D0/   
      DATA GAUSG2(35,2)/     5.976329D0/
      DATA GAUSG3(35,2)/     2.348745D0/
      DATA PAPDG (35)  /    -0.013772D0/
      DATA PBPDG (35)  /     0.008849D0/   
      DATA DAPDG (35)  /     1.852030D0/
      DATA DBPDG (35)  /     2.338958D0/
      DATA NGAUSG(35)  /     2         /
C                       DATA FOR ELEMENT 53 IODINE
      DATA REFPDG(53)  /' I: (PDDG/PM3): TUBERT-BROHMAN,GUIMARAES,REPASK
     1Y,JORGENSEN,JCC 2004 25,138.'/ 
      DATA USSPDG(53)  / -97.664174D0/
      DATA UPPPDG(53)  / -61.167137D0/
      DATA BETASG(53)  / -16.592621D0/
      DATA BETAPG(53)  /  -6.599816D0/
      DATA ZSPDG (53)  /   5.062801D0/
      DATA ZPPDG (53)  /   2.417757D0/
      DATA ALPPDG(53)  /   1.978170D0/
      DATA EISOLG(53)  /-291.537869D0/
      DATA GSSPDG(53)/      13.6319430D0/
      DATA GSPPDG(53)/      14.9904060D0/
      DATA GPPPDG(53)/       7.2883300D0/
      DATA GP2PDG(53)/       5.9664070D0/
      DATA HSPPDG(53)/       2.6300350D0/
      DATA DDPDG (53)/       0.407261D0 /
      DATA QQPDG (53)/       1.062574D0 /
      DATA AMPDG (53)/       0.500990D0 /
      DATA ADPDG (53)/       0.939338D0 /
      DATA AQPDG (53)/       0.510317D0 /
      DATA GAUSG1(53,1)/    -0.136003D0 /
      DATA GAUSG2(53,1)/     3.852912D0   /
      DATA GAUSG3(53,1)/     1.697455D0   /
      DATA GAUSG1(53,2)/    -0.037287D0   /
      DATA GAUSG2(53,2)/     5.229264D0   /
      DATA GAUSG3(53,2)/     2.768669D0   /
      DATA PAPDG (53)  /     0.012901D0   / 
      DATA PBPDG (53)  /    -0.012825D0   /
      DATA DAPDG (53)  /     1.994299D0   /
      DATA DBPDG (53)  /     2.263417D0   /
      DATA NGAUSG(53)  /     2            /

***********************************************************************
*
*     START OF PDDG/MNDO PARAMETERS
*     Reference:  Repasky, Chandrashakar, Jorgensen
*
*
*                              Units
*     USSMDG, UPPMDG, UDDMDG:  eV
*     BETASH, BETAPH, BETADH:  eV
*     ALPMDG:                  angstrom**(-1)
*     ZSMDG,  ZPMDG,  ZDMDG:   bohr**(-1)
*     GSSMDG, GSPMDG, GPPMDG:  eV 
*     GP2MDG, HSPMDG:          eV
*     EISOLH:                  eV
*     GAUSH1:                  eV * angstrom
*     GAUSH2:                  angstrom**(-2)
*     GAUSH3:                  angstrom
*     AMMDG, ADMDG, AQMDG:     bohr
*     DDMDG, QQMDG:            bohr
*
***********************************************************************
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFMDG(1)/' H: (PDDG/MNDO): REPASKY, CHADRASEKHAR, JORGENSEN,
     1 JCC 2002, 23(16), 1601.'/  
      DATA USSMDG(1) /-11.724114276410D0/
      DATA BETASH(1) /-7.4935039195719D0/
      DATA ZSMDG(1)  /1.32243115467370D0/
      DATA ALPMDG(1) /2.49181323064320D0/
      DATA EISOLH(1) /-12.015955786557D0/
      DATA GSSMDG(1)   / 12.848D00 /
      DATA AMMDG(1)  /0.47217934812430D0/
      DATA ADMDG(1)  /0.47217934812430D0/
      DATA AQMDG(1)  /0.47217934812430D0/
      DATA PAMDG(1)  /-0.1088607444359D0/
      DATA PBMDG(1)  /-0.0247060666203D0/
      DATA DAMDG(1)  /0.46072116172000D0/
      DATA DBMDG(1)  /1.29873123436820D0/
C                    DATA FOR ELEMENT  6        CARBON
      DATA REFMDG(6)/' C: (PDDG/MNDO): REPASKY, CHADRASEKHAR, JORGENSEN,
     1 JCC 2002, 23(16), 1601.'/     
      DATA USSMDG(6) /-53.837582488984D0/
      DATA UPPMDG(6) /-39.936408766823D0/
      DATA BETASH(6) /-18.841334137411D0/
      DATA BETAPH(6) /-7.9222341225346D0/
      DATA ZSMDG(6)  /1.80981702301050D0/
      DATA ZPMDG(6)  /1.82500792388930D0/
      DATA ALPMDG(6) /2.55552238806810D0/
      DATA EISOLH(6) /-123.86441152368D0/
      DATA GSSMDG(6) / 12.23D00 /
      DATA GPPMDG(6) / 11.08D00 /
      DATA GSPMDG(6) / 11.47D00 /
      DATA GP2MDG(6) / 9.84D00 /
      DATA HSPMDG(6) / 2.43D00 /
      DATA DDMDG(6)  /0.79415790778110D0/
      DATA QQMDG(6)  /0.67109016643690D0/
      DATA AMMDG(6)  /0.44946710986610D0/
      DATA ADMDG(6)  /0.62058137837870D0/
      DATA AQMDG(6)  /0.67810055293470D0/
      DATA PAMDG(6)  /-0.0068893327627D0/
      DATA PBMDG(6)  /-0.0277514418977D0/
      DATA DAMDG(6)  /1.19245557326430D0/
      DATA DBMDG(6)  /1.32952163414800D0/
C                    DATA FOR ELEMENT  7        NITROGEN
      DATA REFMDG(7)/' N: (PDDG/MNDO): REPASKY, CHADRASEKHAR, JORGENSEN,
     1 JCC 2002, 23(16), 1601.'/     
      DATA USSMDG(7) /-71.87189435530930D0/
      DATA UPPMDG(7) /-58.21661676886340D0/
      DATA BETASH(7) /-20.37577411084280D0/
      DATA BETAPH(7) /-21.08537341050740D0/
      DATA ZSMDG(7)  /2.23142379586030D0/
      DATA ZPMDG(7)  /2.25345995688440D0/
      DATA ALPMDG(7) /2.84367788492060D0/
      DATA EISOLH(7) /-206.46662581723320D0/
      DATA GSSMDG(7) /13.59D00/
      DATA GPPMDG(7) /12.98D00/
      DATA GSPMDG(7) /12.66D00/
      DATA GP2MDG(7) /11.59D00/
      DATA HSPMDG(7) /3.14D00/
      DATA DDMDG(7)  /0.64362354949890D0/
      DATA QQMDG(7)  /0.54349528938820D0/
      DATA AMMDG(7)  /0.49944873451190D0/
      DATA ADMDG(7)  /0.78188576956680D0/
      DATA AQMDG(7)  /0.81211139492050D0/
      DATA PAMDG(7)  /0.03502693409010D0/
      DATA PBMDG(7)  /-0.00172140634590D0/
      DATA DAMDG(7)  /1.01162987147870D0/
      DATA DBMDG(7)  /2.27842256966070D0/
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFMDG(8)/' O: (PDDG/MNDO): REPASKY, CHADRASEKHAR, JORGENSEN,
     1 JCC 2002, 23(16), 1601.'/     
      DATA USSMDG(8) /-97.884970179897D0/
      DATA UPPMDG(8) /-77.342673804072D0/
      DATA BETASH(8) /-33.606335624658D0/
      DATA BETAPH(8) /-27.984442042827D0/
      DATA ZSMDG(8)  /2.56917199926090D0/
      DATA ZPMDG(8)  /2.69715151721790D0/
      DATA ALPMDG(8) /3.23884200872830D0/
      DATA EISOLH(8) /-310.87974465179D0/
      DATA GSSMDG(8) /15.42D00/
      DATA GPPMDG(8) /14.52D00/
      DATA GSPMDG(8) /14.48D00/
      DATA GP2MDG(8) /12.98D00/
      DATA HSPMDG(8) /3.94D00/
      DATA DDMDG(8)  /0.54734406013390D0/
      DATA QQMDG(8)  /0.45408827185760D0/
      DATA AMMDG(8)  /0.56670342061620D0/
      DATA ADMDG(8)  /0.94709991169520D0/
      DATA AQMDG(8)  /0.94892420489320D0/
      DATA PAMDG(8)  /0.08634413812890D0/
      DATA PBMDG(8)  /0.03040342779910D0/
      DATA DAMDG(8)  /0.72540784783600D0/
      DATA DBMDG(8)  /0.70972848794410D0/      
C                    DATA FOR ELEMENT  9       FLUORINE
      DATA REFMDG(9)  /' F: (PDDG/MNDO): TUBERT-BROHMAN,GUIMARAES,REPASK
     1Y,JORGENSEN,JCC 2004 25,138.'/
      DATA USSMDG(9)  /-134.220379D0/
      DATA UPPMDG(9)  /-107.155961D0/
      DATA BETASH(9)  / -67.827612D0/
      DATA BETAPH(9)  / -40.924818D0/
      DATA ZSMDG (9)  /   4.328519D0/
      DATA ZPMDG (9)  /   2.905042D0/
      DATA ALPMDG(9)  /   3.322382D0/
      DATA EISOLH(9)  /-488.703243D0/
      DATA GSSMDG(9)  /16.92D00/
      DATA GPPMDG(9)  /16.71D00/
      DATA GSPMDG(9)  /17.25D00/
      DATA GP2MDG(9)  /14.91D00/
      DATA HSPMDG(9)  /4.83D00/
      DATA DDMDG(9)   /0.361556D0/
      DATA QQMDG(9)   /0.421593D0/
      DATA AMMDG(9)   /0.621830D0/
      DATA ADMDG(9)   /1.303601D0/
      DATA AQMDG(9)   /1.048409D0/
      DATA PAMDG(9)   /-0.011579D0/
      DATA PBMDG(9)   /-0.012943D0/
      DATA DAMDG(9)   / 0.834606D0/
      DATA DBMDG(9)   / 1.875603D0/   
C                    DATA FOR ELEMENT 17    CHLORINE
      DATA REFMDG(17)/' Cl:(PDDG/MNDO): TUBERT-BROHMAN,GUIMARAES,REPASKY
     1,JORGENSEN,JCC 2004 25,138.'/
      DATA USSMDG(17) /-111.133653D0/
      DATA UPPMDG(17) / -78.062493D0/
      DATA BETASH(17) / -15.663317D0/
      DATA BETAPH(17) / -15.399331D0/
      DATA ZSMDG (17) /   4.212404D0/
      DATA ZPMDG (17) /   2.037647D0/
      DATA ALPMDG(17) /   2.602846D0/
      DATA EISOLH(17) /-378.909727D0/
      DATA GSSMDG(17) /15.03D00/
      DATA GPPMDG(17) /11.30D00/
      DATA GSPMDG(17) /13.16D00/
      DATA GP2MDG(17) /9.97D00/
      DATA HSPMDG(17) /2.42D00/
      DATA DDMDG (17) /0.411609D0/
      DATA QQMDG (17) /0.821202D0/
      DATA AMMDG (17) /0.552370D0/
      DATA ADMDG (17) /0.902123D0/
      DATA AQMDG (17) /0.605617D0/
      DATA PAMDG (17) /-0.017119D0/
      DATA PBMDG (17) / 0.005497D0/
      DATA DAMDG (17) / 1.466335D0/
      DATA DBMDG (17) / 2.236842D0/
C                     DATA FOR ELEMENT 35   BROMINE
      DATA REFMDG(35)/' Br:(PDDG/MNDO): TUBERT-BROHMAN,GUIMARAES,REPASKY
     1,JORGENSEN,JCC 2004 25,138.'/
      DATA USSMDG(35) /-100.637007D0/
      DATA UPPMDG(35) / -76.015735D0/
      DATA BETASH(35) /  -7.054170D0/
      DATA BETAPH(35) / -10.221030D0/
      DATA ZSMDG (35) /   3.999975D0/
      DATA ZPMDG (35) /   2.245040D0/
      DATA ALPMDG(35) /   2.414265D0/
      DATA EISOLH(35) /-349.564096D0/
      DATA GSSMDG(35) /15.03643948D0/
      DATA GPPMDG(35) /11.27632539D0/
      DATA GSPMDG(35) /13.03468242D0/
      DATA GP2MDG(35) /9.85442552D0/
      DATA HSPMDG(35) /2.45586832D0/
      DATA DDMDG (35) / 0.574623D0/
      DATA QQMDG (35) / 0.944892D0/
      DATA AMMDG (35) / 0.552607D0/
      DATA ADMDG (35) / 0.747533D0/
      DATA AQMDG (35) / 0.564986D0/
      DATA PAMDG (35) /-0.017133D0/
      DATA PBMDG (35) /-0.016964D0/
      DATA DAMDG (35) / 2.201539D0/
      DATA DBMDG (35) / 2.255764D0/
C                     DATA FOR ELEMENT 53 IODINE
      DATA REFMDG(53)/' I: (PDDG/MNDO): TUBERT-BROHMAN,GUIMARAES,REPASKY
     1,JORGENSEN,JCC 2004 25,138.'/
      DATA USSMDG(53) /-106.588422D0/
      DATA UPPMDG(53) / -75.282605D0/
      DATA BETASH(53) /  -6.698375D0/
      DATA BETAPH(53) /  -5.693814D0/
      DATA ZSMDG (53) /   2.718404D0/
      DATA ZPMDG (53) /   2.461813D0/
      DATA ALPMDG(53) /   2.242446D0/
      DATA EISOLH(53) /-356.076398D0/
      DATA GSSMDG(53) /15.04044855D0/
      DATA GPPMDG(53) /11.14778369D0/
      DATA GSPMDG(53) /13.05655798D0/
      DATA GP2MDG(53) /9.91409071D0/
      DATA HSPMDG(53) /2.45638202D0/
      DATA DDMDG (53) / 1.209529D0/
      DATA QQMDG (53) / 1.043559D0/
      DATA AMMDG (53) / 0.552754D0/
      DATA ADMDG (53) / 0.498848D0/
      DATA AQMDG (53) / 0.504032D0/
      DATA PAMDG (53) / 0.009616D0/
      DATA PBMDG (53) /-0.007505D0/
      DATA DAMDG (53) / 2.572332D0/
      DATA DBMDG (53) / 2.936456D0/

C Added by Luke Fiedler May 2009:                                       LF0509
***********************************************************************
*
*    START OF AM1-D PARAMETERS
*    Reference:  
*      J. P. McNamara and I. H. Hillier, Phys. Chem. Chem. Phys., 2007, 9, 2362–2370
*
*    The units of the overriding parameters are consistent with those
*    of original AM1 parameters.
*
*                              Units
*     USSAMD, UPPAMD:          eV
*     BETASE, BETAPE:          eV
*     ALPHAE:                  angstrom**(-1)
*     EISOLE:                  eV
*     R0AMD:                   angstrom
*     C6AMD:                   J (nm**6)/mol
*     S6AMD:                   unitless
*     ALPAMD:                  unitless
*
***********************************************************************
C                    DATA COMMON TO ALL ELEMENTS
      DATA S6AMD      /     1.4D0 /
      DATA ALPAMD     /    23.0D0 /
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFAMD( 1)/'  H: (AM1-D): MCNAMARA, HILLIER, PCCP 2007 9(19),
     &2362.'/
      DATA R0AMD ( 1) /   1.110000D0/
      DATA C6AMD ( 1) /   0.160000D0/ 
      DATA USSAMD( 1) / -11.223791D0/
      DATA UPPAMD( 1) /   0.000000D0/
      DATA BETASE( 1) /  -6.376265D0/
      DATA BETAPE( 1) /   0.000000D0/
      DATA ALPHAE( 1) /   3.577756D0/
      DATA EISOLE( 1) / -11.223791D0/
C                    DATA FOR ELEMENT  6        CARBON
      DATA REFAMD( 6)/'  C: (AM1-D): MCNAMARA, HILLIER, PCCP 2007 9(19),
     &2362.'/
      DATA R0AMD ( 6) /   1.610000D0/
      DATA C6AMD ( 6) /   1.650000D0/ 
      DATA USSAMD( 6) / -52.183798D0/
      DATA UPPAMD( 6) / -39.368413D0/
      DATA BETASE( 6) / -15.682341D0/
      DATA BETAPE( 6) /  -7.804762D0/
      DATA ALPHAE( 6) /   2.625506D0/
      DATA EISOLE( 6) / -120.634422D0/
C                    DATA FOR ELEMENT  7        NITROGEN
      DATA REFAMD( 7)/'  N: (AM1-D): MCNAMARA, HILLIER, PCCP 2007 9(19),
     &2362.'/
      DATA R0AMD ( 7) /   1.550000D0/
      DATA C6AMD ( 7) /   1.110000D0/ 
      DATA USSAMD( 7) / -71.997845D0/
      DATA UPPAMD( 7) / -57.401718D0/
      DATA BETASE( 7) / -20.092408D0/
      DATA BETAPE( 7) / -18.470679D0/
      DATA ALPHAE( 7) /   2.968737D0/
      DATA EISOLE( 7) / -203.385844D0/
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFAMD( 8)/'  O: (AM1-D): MCNAMARA, HILLIER, PCCP 2007 9(19),
     &2362.'/
      DATA R0AMD ( 8) /   1.490000D0/
      DATA C6AMD ( 8) /   0.700000D0/ 
      DATA USSAMD( 8) / -97.610588D0/
      DATA UPPAMD( 8) / -78.589700D0/
      DATA BETASE( 8) / -29.502481D0/
      DATA BETAPE( 8) / -29.495380D0/
      DATA ALPHAE( 8) /   4.633699D0/
      DATA EISOLE( 8) / -316.969976D0/
C                    DATA FOR ELEMENT  9        FLUORINE
      DATA REFAMD( 9)/'  F: (AM1-D): GRIMME, JCC 2004,
     & 25(12), 1463.'/
      DATA R0AMD ( 9) /   1.430000D0/
      DATA C6AMD ( 9) /   0.570000D0/ 
C                    DATA FOR ELEMENT 10        NEON
      DATA REFAMD(10)/' Ne: (AM1-D): GRIMME, JCC 2004,
     & 25(12), 1463.'/
      DATA R0AMD (10) /   1.380000D0/
      DATA C6AMD (10) /   0.450000D0/ 
C                    DATA FOR ELEMENT 16        SULFUR
      DATA REFAMD(16)/'  S: (AM1-D): MORGADO,MCNAMARA,HILLIER,BURTON,
     &VINCENT, JCTC 2007, 3(5), 1656.'/
      DATA R0AMD (16) /   1.870000D0/
      DATA C6AMD (16) /  10.300000D0/ 
      DATA USSAMD(16) / -57.235044D0/
      DATA UPPAMD(16) / -48.307513D0/
      DATA BETASE(16) /  -3.311308D0/
      DATA BETAPE(16) /  -7.256468D0/
      DATA ALPHAE(16) /   2.309315D0/
      DATA EISOLE(16) / -191.176025D0/
C             SPECIAL HYDROGEN WITH P ORBITALS                          LF0410
C           DATA FOR ELEMENT  108     AM1-D: HYDROGEN (WITH P-ORBITALS)
      DATA REFAMD(108)/' Hp: (AM1-D): Hydrogen (with p basis functions)  
     1                               '/
      DATA R0AMD (108)/   1.110000D0/
      DATA C6AMD (108)/   0.160000D0/ 

C Added by Luke Fiedler Mar 2009:                                       LF0309
***********************************************************************
*
*    START OF PM3-D PARAMETERS
*    Reference:  
*      J. P. McNamara and I. H. Hillier, Phys. Chem. Chem. Phys., 2007, 9, 2362–2370
*
*    The units of the overriding parameters are consistent with those
*    of original PM3 parameters.
*
*                              Units
*     USSPMD, UPPPMD:          eV
*     BETASD, BETAPD:          eV
*     ALPHAD:                  angstrom**(-1)
*     EISOLD:                  eV
*     R0PMD:                   angstrom
*     C6PMD:                   J (nm**6)/mol
*     S6PMD:                   unitless
*     ALPPMD:                  unitless
*
***********************************************************************
C                    DATA COMMON TO ALL ELEMENTS
      DATA S6PMD      /     1.4D0 /
      DATA ALPPMD     /    23.0D0 /
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFPMD( 1)/'  H: (PM3-D): MCNAMARA, HILLIER, PCCP 2007 9(19),
     &2362.'/
      DATA R0PMD ( 1) /   1.110000D0/
      DATA C6PMD ( 1) /   0.160000D0/ 
      DATA USSPMD( 1) / -13.054076D0/
      DATA UPPPMD( 1) /   0.000000D0/
      DATA BETASD( 1) /  -5.628901D0/
      DATA BETAPD( 1) /   0.000000D0/
      DATA ALPHAD( 1) /   3.417532D0/
      DATA EISOLD( 1) / -13.054076D0/
C                    DATA FOR ELEMENT  6        CARBON
      DATA REFPMD( 6)/'  C: (PM3-D): MCNAMARA, HILLIER, PCCP 2007 9(19),
     &2362.'/
      DATA R0PMD ( 6) /   1.610000D0/
      DATA C6PMD ( 6) /   1.650000D0/ 
      DATA USSPMD( 6) / -47.275431D0/
      DATA UPPPMD( 6) / -36.268916D0/
      DATA BETASD( 6) / -11.941466D0/
      DATA BETAPD( 6) /  -9.819760D0/
      DATA ALPHAD( 6) /   2.721152D0/
      DATA EISOLD( 6) / -111.244135D0/
C                    DATA FOR ELEMENT  7        NITROGEN
      DATA REFPMD( 7)/'  N: (PM3-D): MCNAMARA, HILLIER, PCCP 2007 9(19),
     &2362.'/
      DATA R0PMD ( 7) /   1.550000D0/
      DATA C6PMD ( 7) /   1.110000D0/ 
      DATA USSPMD( 7) / -49.348460D0/
      DATA UPPPMD( 7) / -47.543768D0/
      DATA BETASD( 7) / -14.068411D0/
      DATA BETAPD( 7) / -20.039292D0/
      DATA ALPHAD( 7) /   3.060404D0/
      DATA EISOLD( 7) / -157.741448D0/
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFPMD( 8)/'  O: (PM3-D): MCNAMARA, HILLIER, PCCP 2007 9(19),
     &2362.'/
      DATA R0PMD ( 8) /   1.490000D0/
      DATA C6PMD ( 8) /   0.700000D0/ 
      DATA USSPMD( 8) / -86.960302D0/
      DATA UPPPMD( 8) / -71.926845D0/
      DATA BETASD( 8) / -45.234302D0/
      DATA BETAPD( 8) / -24.788037D0/
      DATA ALPHAD( 8) /   3.387806D0/
      DATA EISOLD( 8) / -289.465867D0/
C                    DATA FOR ELEMENT  9        FLUORINE
      DATA REFPMD( 9)/'  F: (PM3-D): GRIMME, JCC 2004,
     & 25(12), 1463.'/
      DATA R0PMD ( 9) /   1.430000D0/
      DATA C6PMD ( 9) /   0.570000D0/ 
C                    DATA FOR ELEMENT 10        NEON
      DATA REFPMD(10)/' Ne: (PM3-D): GRIMME, JCC 2004,
     & 25(12), 1463.'/
      DATA R0PMD (10) /   1.380000D0/
      DATA C6PMD (10) /   0.450000D0/ 
C                    DATA FOR ELEMENT 16        SULFUR
      DATA REFPMD(16)/'  S: (PM3-D): MORGADO,MCNAMARA,HILLIER,BURTON,
     &VINCENT, JCTC 2007, 3(5), 1656.'/
      DATA R0PMD (16) /   1.870000D0/
      DATA C6PMD (16) /  10.300000D0/ 
      DATA USSPMD(16) / -50.249536D0/
      DATA UPPPMD(16) / -43.968965D0/
      DATA BETASD(16) /  -8.397415D0/
      DATA BETAPD(16) /  -7.594232D0/
      DATA ALPHAD(16) /   2.234331D0/
      DATA EISOLD(16) /-182.467598D0/
C             SPECIAL HYDROGEN WITH P ORBITALS                          LF0410
C           DATA FOR ELEMENT  108     PM3-D: HYDROGEN (WITH P-ORBITALS)
      DATA REFPMD(108)/' Hp: (PM3-D): Hydrogen (with p basis functions)  
     1                               '/
      DATA R0PMD (108)/   1.110000D0/
      DATA C6PMD (108)/   0.160000D0/ 
C Added by Luke Fiedler Mar 2010:                                       LF0310
***********************************************************************
*
*    START OF MNDO-D PARAMETERS
*
*    Because the literature contains no parameters for extending
*    MNDO to MNDO-D, the parameters specified for PM3-D have been
*    chosen until a new parameterization is undertaken.
*
*    The units of the overriding parameters are consistent with those
*    of original MNDO parameters.
*
*                              Units
*     USSMND, UPPMND:          eV
*     BETASF, BETAPF:          eV
*     ALPHAF:                  angstrom**(-1)
*     EISOLF:                  eV
*     R0MND:                   angstrom
*     C6MND:                   J (nm**6)/mol
*     S6MND:                   unitless
*     ALPMND:                  unitless
*
***********************************************************************
C                    DATA COMMON TO ALL ELEMENTS
      DATA S6MND      /     1.4D0 /
      DATA ALPMND     /    23.0D0 /
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFMND( 1)/'  H: (MNDO-D): USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0MND ( 1) /   1.110000D0/        ! PM3-D parameters
      DATA C6MND ( 1) /   0.160000D0/        ! PM3-D parameters
C                    DATA FOR ELEMENT  6        CARBON
      DATA REFMND( 6)/'  C: (MNDO-D): USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0MND ( 6) /   1.610000D0/        ! PM3-D parameters
      DATA C6MND ( 6) /   1.650000D0/        ! PM3-D parameters
C                    DATA FOR ELEMENT  7        NITROGEN
      DATA REFMND( 7)/'  N: (MNDO-D): USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0MND ( 7) /   1.550000D0/        ! PM3-D parameters
      DATA C6MND ( 7) /   1.110000D0/        ! PM3-D parameters
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFMND( 8)/'  O: (MNDO-D): USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0MND ( 8) /   1.490000D0/        ! PM3-D parameters
      DATA C6MND ( 8) /   0.700000D0/        ! PM3-D parameters
C                    DATA FOR ELEMENT  9        FLUORINE
      DATA REFMND( 9)/'  F: (MNDO-D): USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0MND ( 9) /   1.430000D0/
      DATA C6MND ( 9) /   0.570000D0/ 
C                    DATA FOR ELEMENT 10        NEON
      DATA REFMND(10)/' Ne: (MNDO-D): USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0MND (10) /   1.380000D0/
      DATA C6MND (10) /   0.450000D0/ 
C                    DATA FOR ELEMENT 16        SULFUR
      DATA REFMND(16)/'  S: (MNDO-D): USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0MND (16) /   1.870000D0/        ! PM3-D parameters
      DATA C6MND (16) /  10.300000D0/        ! PM3-D parameters
C             SPECIAL HYDROGEN WITH P ORBITALS                          LF0410
C           DATA FOR ELEMENT  108     PM3-D: HYDROGEN (WITH P-ORBITALS)
      DATA REFMND(108)/' Hp: (MNDO-D): USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0MND (108)/   1.110000D0/
      DATA C6MND (108)/   0.160000D0/ 
C Added by Luke Fiedler Mar 2010:                                       LF0310
***********************************************************************
*
*    START OF RM1-D PARAMETERS
*
*    Because the literature contains no dispersion parameters for extending
*    RM1 to RM1-D, the dispersion parameters specified for AM1-D have been
*    chosen until a new parameterization is undertaken.
*
*    The units of the overriding parameters are consistent with those
*    of original RM1 parameters.
*
*                              Units
*     USSRMD, UPPRMD:          eV
*     BETASI, BETAPI:          eV
*     ALPHAI:                  angstrom**(-1)
*     EISOLI:                  eV
*     R0RMD:                   angstrom
*     C6RMD:                   J (nm**6)/mol
*     S6RMD:                   unitless
*     ALPRMD:                  unitless
*
***********************************************************************
C                    DATA COMMON TO ALL ELEMENTS
      DATA S6RMD      /     1.4D0 /
      DATA ALPRMD     /    23.0D0 /
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFRMD( 1)/'  H: (RM1-D):  USING AM1-D DISPERSION 
     &PARAMETERS.'/
      DATA R0RMD ( 1) /   1.110000D0/
      DATA C6RMD ( 1) /   0.160000D0/
Cobs      DATA USSRMD( 1) / -11.223791D0/
Cobs      DATA UPPRMD( 1) /   0.000000D0/
Cobs      DATA BETASI( 1) /  -6.376265D0/
Cobs      DATA BETAPI( 1) /   0.000000D0/
Cobs      DATA ALPHAI( 1) /   3.577756D0/
Cobs      DATA EISOLI( 1) / -11.223791D0/
C                    DATA FOR ELEMENT  6        CARBON
      DATA REFRMD( 6)/'  C: (RM1-D):  USING AM1-D DISPERSION 
     &PARAMETERS.'/
      DATA R0RMD ( 6) /   1.610000D0/
      DATA C6RMD ( 6) /   1.650000D0/ 
Cobs      DATA USSRMD( 6) / -52.183798D0/
Cobs      DATA UPPRMD( 6) / -39.368413D0/
Cobs      DATA BETASI( 6) / -15.682341D0/
Cobs      DATA BETAPI( 6) /  -7.804762D0/
Cobs      DATA ALPHAI( 6) /   2.625506D0/
Cobs      DATA EISOLI( 6) / -120.634422D0/
C                    DATA FOR ELEMENT  7        NITROGEN
      DATA REFRMD( 7)/'  N: (RM1-D):  USING AM1-D DISPERSION 
     &PARAMETERS.'/
      DATA R0RMD ( 7) /   1.550000D0/
      DATA C6RMD ( 7) /   1.110000D0/ 
Cobs      DATA USSRMD( 7) / -71.997845D0/
Cobs      DATA UPPRMD( 7) / -57.401718D0/
Cobs      DATA BETASI( 7) / -20.092408D0/
Cobs      DATA BETAPI( 7) / -18.470679D0/
Cobs      DATA ALPHAI( 7) /   2.968737D0/
Cobs      DATA EISOLI( 7) / -203.385844D0/
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFRMD( 8)/'  O: (RM1-D):  USING AM1-D DISPERSION 
     &PARAMETERS.'/
      DATA R0RMD ( 8) /   1.490000D0/
      DATA C6RMD ( 8) /   0.700000D0/ 
Cobs      DATA USSRMD( 8) / -97.610588D0/
Cobs      DATA UPPRMD( 8) / -78.589700D0/
Cobs      DATA BETASI( 8) / -29.502481D0/
Cobs      DATA BETAPI( 8) / -29.495380D0/
Cobs      DATA ALPHAI( 8) /   4.633699D0/
Cobs      DATA EISOLI( 8) / -316.969976D0/
C                    DATA FOR ELEMENT  9        FLUORINE
      DATA REFRMD( 9)/'  F: (RM1-D):  USING AM1-D DISPERSION 
     &PARAMETERS.'/
      DATA R0RMD ( 9) /   1.430000D0/
      DATA C6RMD ( 9) /   0.570000D0/ 
C                    DATA FOR ELEMENT 10        NEON
      DATA REFRMD(10)/' Ne: (RM1-D):  USING AM1-D DISPERSION 
     &PARAMETERS.'/
      DATA R0RMD (10) /   1.380000D0/
      DATA C6RMD (10) /   0.450000D0/ 
C                    DATA FOR ELEMENT 16        SULFUR
      DATA REFRMD(16)/'  S: (RM1-D):  USING AM1-D DISPERSION 
     &PARAMETERS.'/
      DATA R0RMD (16) /   1.870000D0/
      DATA C6RMD (16) /  10.300000D0/ 
Cobs      DATA USSRMD(16) / -57.235044D0/
Cobs      DATA UPPRMD(16) / -48.307513D0/
Cobs      DATA BETASI(16) /  -3.311308D0/
Cobs      DATA BETAPI(16) /  -7.256468D0/
Cobs      DATA ALPHAI(16) /   2.309315D0/
Cobs      DATA EISOLI(16) / -191.176025D0/
C             SPECIAL HYDROGEN WITH P ORBITALS                          LF0410
C           DATA FOR ELEMENT  108     RM1-D: HYDROGEN (WITH P-ORBITALS)
      DATA REFRMD(108)/' Hp: (RM1-D): Hydrogen (with p basis functions)  
     1                               '/
      DATA R0RMD (108)/   1.110000D0/
      DATA C6RMD (108)/   0.160000D0/ 
C Added by Luke Fiedler Mar 2010:                                       LF0310
***********************************************************************
*
*    START OF PM6-D PARAMETERS
*
*    Because the literature contains no dispersion parameters for extending
*    PM6 to PM6-D, the dispersion parameters specified for PM3-D have been
*    chosen until a new parameterization is undertaken.
*
*    The units of the overriding parameters are consistent with those
*    of original PM6 parameters.
*
*                              Units
*     USSP6D, UPPP6D:          eV
*     BETASJ, BETAPJ:          eV
*     ALPHAJ:                  angstrom**(-1)
*     EISOLJ:                  eV
*     R0P6D:                   angstrom
*     C6P6D:                   J (nm**6)/mol
*     S6P6D:                   unitless
*     ALPP6D:                  unitless
*
***********************************************************************
C                    DATA COMMON TO ALL ELEMENTS
      DATA S6P6D      /     1.4D0 /
      DATA ALPP6D     /    23.0D0 /
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFP6D( 1)/'  H: (PM6-D):  USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0P6D ( 1) /   1.110000D0/
      DATA C6P6D ( 1) /   0.160000D0/ 
Cobs      DATA USSP6D( 1) / -13.054076D0/
Cobs      DATA UPPP6D( 1) /   0.000000D0/
Cobs      DATA BETASJ( 1) /  -5.628901D0/
Cobs      DATA BETAPJ( 1) /   0.000000D0/
Cobs      DATA ALPHAJ( 1) /   3.417532D0/
Cobs      DATA EISOLJ( 1) / -13.054076D0/
C                    DATA FOR ELEMENT  6        CARBON
      DATA REFP6D( 6)/'  C: (PM6-D):  USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0P6D ( 6) /   1.610000D0/
      DATA C6P6D ( 6) /   1.650000D0/ 
Cobs      DATA USSP6D( 6) / -47.275431D0/
Cobs      DATA UPPP6D( 6) / -36.268916D0/
Cobs      DATA BETASJ( 6) / -11.941466D0/
Cobs      DATA BETAPJ( 6) /  -9.819760D0/
Cobs      DATA ALPHAJ( 6) /   2.721152D0/
Cobs      DATA EISOLJ( 6) / -111.244135D0/
C                    DATA FOR ELEMENT  7        NITROGEN
      DATA REFP6D( 7)/'  N: (PM6-D):  USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0P6D ( 7) /   1.550000D0/
      DATA C6P6D ( 7) /   1.110000D0/ 
Cobs      DATA USSP6D( 7) / -49.348460D0/
Cobs      DATA UPPP6D( 7) / -47.543768D0/
Cobs      DATA BETASJ( 7) / -14.068411D0/
Cobs      DATA BETAPJ( 7) / -20.039292D0/
Cobs      DATA ALPHAJ( 7) /   3.060404D0/
Cobs      DATA EISOLJ( 7) / -157.741448D0/
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFP6D( 8)/'  O: (PM6-D):  USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0P6D ( 8) /   1.490000D0/
      DATA C6P6D ( 8) /   0.700000D0/ 
Cobs      DATA USSP6D( 8) / -86.960302D0/
Cobs      DATA UPPP6D( 8) / -71.926845D0/
Cobs      DATA BETASJ( 8) / -45.234302D0/
Cobs      DATA BETAPJ( 8) / -24.788037D0/
Cobs      DATA ALPHAJ( 8) /   3.387806D0/
Cobs      DATA EISOLJ( 8) / -289.465867D0/
C                    DATA FOR ELEMENT  9        FLUORINE
      DATA REFP6D( 9)/'  F: (PM6-D):  USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0P6D ( 9) /   1.430000D0/
      DATA C6P6D ( 9) /   0.570000D0/ 
C                    DATA FOR ELEMENT 10        NEON
      DATA REFP6D(10)/' Ne: (PM6-D):  USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0P6D (10) /   1.380000D0/
      DATA C6P6D (10) /   0.450000D0/ 
C                    DATA FOR ELEMENT 16        SULFUR
      DATA REFP6D(16)/'  S: (PM6-D):  USING PM3-D DISPERSION 
     &PARAMETERS.'/
      DATA R0P6D (16) /   1.870000D0/
      DATA C6P6D (16) /  10.300000D0/ 
Cobs      DATA USSP6D(16) / -50.249536D0/
Cobs      DATA UPPP6D(16) / -43.968965D0/
Cobs      DATA BETASJ(16) /  -8.397415D0/
Cobs      DATA BETAPJ(16) /  -7.594232D0/
Cobs      DATA ALPHAJ(16) /   2.234331D0/
Cobs      DATA EISOLJ(16) / -182.467598D0/
C             SPECIAL HYDROGEN WITH P ORBITALS                          LF0410
C           DATA FOR ELEMENT  108     PM6-D: HYDROGEN (WITH P-ORBITALS)
      DATA REFP6D(108)/' Hp: (PM6-D): Hydrogen (with p basis functions)  
     1                               '/
      DATA R0P6D (108)/   1.110000D0/
      DATA C6P6D (108)/   0.160000D0/ 
C Added by Luke Fiedler Jul 2009:                                       LF0709
***********************************************************************
*
*     START OF PM6 MONOATOMIC PARAMETERS
*     Reference:  "Optimization of parameters for semiempirical 
*                  methods V: Modification of NDDO approximations and 
*                  application to 70 elements"
*               James J. P. Stewart
*               J. Molecular Modeling, 13(12), 1173-1213 (Dec 2007).
*
*     Note that of the first 18 elements of the periodic table, five
*     of them have been deactivated (Al, Si, P, S, and Cl) because of
*     their requirement in PM6 to include d-orbitals.  Deactivation was
*     accomplished by commenting out the REFPM6 data lines for the
*     respective elements below.
*
*                              Units
*     USSPM6, UPPPM6, UDDPM6:  eV
*     BETASX, BETAPX, BETADX:  eV
*     ZSPM6, ZPPM6, ZDPM6:     bohr**(-1)
*     GSSPM6, GSPPM6, GPPPM6:  eV 
*     GP2PM6, HSPPM6:          eV
*     EISOLX:                  eV
*     GAUSX1:                  eV * angstrom
*     GAUSX2:                  angstrom**(-2)
*     GAUSX3:                  angstrom
*     ZSNPM6, ZPNPM6, ZDNPM6:  bohr**(-1)
*     AMPM6, ADPM6, AQPM6:     bohr
*     DDPM6, QQPM6:            bohr
*
***********************************************************************
C                    DATA FOR ELEMENT  1       PM6:   HYDROGEN
      DATA REFPM6 ( 1)/'  H: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
     &13, 1173, (2007).'/
      DATA USSPM6( 1)   /     -11.246958D0   /
      DATA BETASX( 1)   /      -8.352984D0   /
      DATA ZSPM6 ( 1)   /       1.268641D0   /
      DATA EISOLX( 1)   /     -11.2469580D0  /
      DATA GSSPM6( 1)   /      14.448686D0   /
      DATA AMPM6 ( 1)   /       0.94165599D0   /
      DATA ADPM6 ( 1)   /       0.94165599D0   /
      DATA AQPM6 ( 1)   /       0.94165599D0   /
      DATA GAUSX1( 1, 1)/       0.024184D0   /
      DATA GAUSX2( 1, 1)/       3.055953D0   /
      DATA GAUSX3( 1, 1)/       1.786011D0   /
C                    DATA FOR ELEMENT  2       PM6:   HELIUM
      DATA REFPM6( 2)  /'  2: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
     &13, 1173, (2007).'/
      DATA USSPM6( 2)   /     -31.770969D0   /
      DATA UPPPM6( 2)   /      -5.856382D0   /
      DATA BETASX( 2)   /     -58.903774D0   /
      DATA BETAPX( 2)   /     -37.039974D0   /
      DATA ZSPM6 ( 2)   /       3.313204D0   /
      DATA ZPPM6 ( 2)   /       3.657133D0   /
      DATA EISOLX( 2)   /       0.000000D0   /
      DATA GSSPM6( 2)   /       9.445299D0   /
      DATA GSPPM6( 2)   /      11.201419D0   /
      DATA GPPPM6( 2)   /       9.214548D0   /
      DATA GP2PM6( 2)   /      13.046115D0   /
      DATA HSPPM6( 2)   /       0.299954D0   /
      DATA DDPM6 ( 2)   /       0.2475819D0  /
      DATA QQPM6 ( 2)   /       0.2118043D0  /
      DATA AMPM6 ( 2)   /       0.3471120D0  /
      DATA ADPM6 ( 2)   /       0.5756142D0  /
      DATA AQPM6 ( 2)   /       0.9768631D0  /
C                    DATA FOR ELEMENT  3       PM6:   LITHIUM
      DATA REFPM6( 3)  /'  3: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
     &13, 1173, (2007).'/
      DATA USSPM6( 3)   /      -4.709912D0   /
      DATA UPPPM6( 3)   /      -2.722581D0   /
      DATA BETASX( 3)   /      -2.283946D0   /
      DATA BETAPX( 3)   /      -7.535573D0   /
      DATA ZSPM6 ( 3)   /       0.981041D0   /
      DATA ZPPM6 ( 3)   /       2.953445D0   /
      DATA EISOLX( 3)   /      -4.7099120D0  /
      DATA GSSPM6( 3)   /      11.035907D0   /
      DATA GSPPM6( 3)   /      19.998647D0   /
      DATA GPPPM6( 3)   /      11.543650D0   /
      DATA GP2PM6( 3)   /       9.059036D0   /
      DATA HSPPM6( 3)   /       1.641886D0   /
      DATA DDPM6 ( 3)   /       0.3558537D0  /
      DATA QQPM6 ( 3)   /       0.4146835D0  /
      DATA AMPM6 ( 3)   /       0.4055664D0  /
      DATA ADPM6 ( 3)   /       0.8444977D0  /
      DATA AQPM6 ( 3)   /       1.1762157D0  /
C                    DATA FOR ELEMENT  4       PM6:   BERYLLIUM
      DATA REFPM6( 4)  /'  4: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
     &13, 1173, (2007).'/
      DATA USSPM6( 4)   /     -16.360315D0   /
      DATA UPPPM6( 4)   /     -16.339216D0   /
      DATA BETASX( 4)   /      -3.199549D0   /
      DATA BETAPX( 4)   /      -4.451920D0   /
      DATA ZSPM6 ( 4)   /       1.212539D0   /
      DATA ZPPM6 ( 4)   /       1.276487D0   /
      DATA EISOLX( 4)   /     -25.1678260D0  /
      DATA GSSPM6( 4)   /       7.552804D0   /
      DATA GSPPM6( 4)   /      10.203146D0   /
      DATA GPPPM6( 4)   /      12.862153D0   /
      DATA GP2PM6( 4)   /      13.602858D0   /
      DATA HSPPM6( 4)   /       1.501452D0   /
      DATA DDPM6 ( 4)   /       1.1578786D0  /
      DATA QQPM6 ( 4)   /       0.9594652D0  /
      DATA AMPM6 ( 4)   /       0.2775633D0  /
      DATA ADPM6 ( 4)   /       0.4062438D0  /
      DATA AQPM6 ( 4)   /       0.3118248D0  /
      DATA GAUSX1( 4, 1)/       0.164180D0   /
      DATA GAUSX2( 4, 1)/       1.704828D0   /
      DATA GAUSX3( 4, 1)/       1.785591D0   /
C                    DATA FOR ELEMENT  5       PM6:   BORON
      DATA REFPM6( 5)  /'  5: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
     &13, 1173, (2007).'/
      DATA USSPM6( 5)   /     -25.967679D0   /
      DATA UPPPM6( 5)   /     -19.115864D0   /
      DATA BETASX( 5)   /      -4.959706D0   /
      DATA BETAPX( 5)   /      -4.656753D0   /
      DATA ZSPM6 ( 5)   /       1.634174D0   /
      DATA ZPPM6 ( 5)   /       1.479195D0   /
      DATA EISOLX( 5)   /     -49.5366840D0  /
      DATA GSSPM6( 5)   /       8.179341D0   /
      DATA GSPPM6( 5)   /       7.294021D0   /
      DATA GPPPM6( 5)   /       7.829395D0   /
      DATA GP2PM6( 5)   /       6.401072D0   /
      DATA HSPPM6( 5)   /       1.252845D0   /
      DATA DDPM6 ( 5)   /       0.9214783D0  /
      DATA QQPM6 ( 5)   /       0.8279807D0  /
      DATA AMPM6 ( 5)   /       0.3005884D0  /
      DATA ADPM6 ( 5)   /       0.4282435D0  /
      DATA AQPM6 ( 5)   /       0.6170044D0  /
C                    DATA FOR ELEMENT  6       PM6:   CARBON
      DATA REFPM6( 6)/'  C: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, 
     &1173, (2007).'/
      DATA USSPM6( 6)   /     -51.089653D0   /
      DATA UPPPM6( 6)   /     -39.937920D0   /
      DATA BETASX( 6)   /     -15.385236D0   /
      DATA BETAPX( 6)   /      -7.471929D0   /
      DATA ZSPM6 ( 6)   /       2.047558D0   /
      DATA ZPPM6 ( 6)   /       1.702841D0   /
      DATA EISOLX( 6)   /    -115.2015800D0  /
      DATA GSSPM6( 6)   /      13.335519D0   /
      DATA GSPPM6( 6)   /      11.528134D0   /
      DATA GPPPM6( 6)   /      10.778326D0   /
      DATA GP2PM6( 6)   /       9.486212D0   /
      DATA HSPPM6( 6)   /       0.717322D0   /
      DATA DDPM6 ( 6)   /       0.7535643D0  /
      DATA QQPM6 ( 6)   /       0.7192362D0  /
      DATA AMPM6 ( 6)   /       0.4900764D0  /
      DATA ADPM6 ( 6)   /       0.3870451D0  /
      DATA AQPM6 ( 6)   /       0.6555882D0  /
      DATA GAUSX1( 6, 1)/       0.046302D0   /
      DATA GAUSX2( 6, 1)/       2.100206D0   /
      DATA GAUSX3( 6, 1)/       1.333959D0   /
C                    DATA FOR ELEMENT  7       PM6:   NITROGEN
      DATA REFPM6( 7)/'  N: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, 
     &1173, (2007).'/
      DATA USSPM6( 7)   /     -57.784823D0   /
      DATA UPPPM6( 7)   /     -49.893036D0   /
      DATA BETASX( 7)   /     -17.979377D0   /
      DATA BETAPX( 7)   /     -15.055017D0   /
      DATA ZSPM6 ( 7)   /       2.380406D0   /
      DATA ZPPM6 ( 7)   /       1.999246D0   /
      DATA EISOLX( 7)   /    -174.9514445D0  /
      DATA GSSPM6( 7)   /      12.357026D0   /
      DATA GSPPM6( 7)   /       9.636190D0   /
      DATA GPPPM6( 7)   /      12.570756D0   /
      DATA GP2PM6( 7)   /      10.576425D0   /
      DATA HSPPM6( 7)   /       2.871545D0   /
      DATA DDPM6 ( 7)   /       0.6467179D0  /
      DATA QQPM6 ( 7)   /       0.6126034D0  /
      DATA AMPM6 ( 7)   /       0.4541171D0  /
      DATA ADPM6 ( 7)   /       0.7490156D0  /
      DATA AQPM6 ( 7)   /       0.8432463D0  /
      DATA GAUSX1( 7, 1)/      -0.001436D0   /
      DATA GAUSX2( 7, 1)/       0.495196D0   /
      DATA GAUSX3( 7, 1)/       1.704857D0   /
C                    DATA FOR ELEMENT  8       PM6:   OXYGEN
      DATA REFPM6( 8)/'  O: (PM6): J. J. P. STEWART, J. MOL. MODEL., 13, 
     &1173, (2007).'/
      DATA USSPM6( 8)   /     -91.678761D0   /
      DATA UPPPM6( 8)   /     -70.460949D0   /
      DATA BETASX( 8)   /     -65.635137D0   /
      DATA BETAPX( 8)   /     -21.622604D0   /
      DATA ZSPM6 ( 8)   /       5.421751D0   /
      DATA ZPPM6 ( 8)   /       2.270960D0   /
      DATA EISOLX( 8)   /    -287.1272180D0  /
      DATA GSSPM6( 8)   /      11.304042D0   /
      DATA GSPPM6( 8)   /      15.807424D0   /
      DATA GPPPM6( 8)   /      13.618205D0   /
      DATA GP2PM6( 8)   /      10.332765D0   /
      DATA HSPPM6( 8)   /       5.010801D0   /
      DATA DDPM6 ( 8)   /       0.2371131D0  /
      DATA QQPM6 ( 8)   /       0.5393071D0  /
      DATA AMPM6 ( 8)   /       0.4154203D0  /
      DATA ADPM6 ( 8)   /       1.6843332D0  /
      DATA AQPM6 ( 8)   /       1.0935465D0  /
      DATA GAUSX1( 8, 1)/      -0.017771D0   /
      DATA GAUSX2( 8, 1)/       3.058310D0   /
      DATA GAUSX3( 8, 1)/       1.896435D0   /
C                    DATA FOR ELEMENT  9       PM6:   FLUORINE
      DATA REFPM6( 9)  /'  9: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
     &13, 1173, (2007).'/
      DATA USSPM6( 9)   /    -140.225626D0   /
      DATA UPPPM6( 9)   /     -98.778044D0   /
      DATA BETASX( 9)   /     -69.922593D0   /
      DATA BETAPX( 9)   /     -30.448165D0   /
      DATA ZSPM6 ( 9)   /       6.043849D0   /
      DATA ZPPM6 ( 9)   /       2.906722D0   /
      DATA EISOLX( 9)   /    -468.1575840D0  /
      DATA GSSPM6( 9)   /      12.446818D0   /
      DATA GSPPM6( 9)   /      18.496082D0   /
      DATA GPPPM6( 9)   /       8.417366D0   /
      DATA GP2PM6( 9)   /      12.179816D0   /
      DATA HSPPM6( 9)   /       2.604382D0   /
      DATA DDPM6 ( 9)   /       0.2324062D0  /
      DATA QQPM6 ( 9)   /       0.4213492D0  /
      DATA AMPM6 ( 9)   /       0.4574169D0  /
      DATA ADPM6 ( 9)   /       1.3108408D0  /
      DATA AQPM6 ( 9)   /       0.5777487D0  /
      DATA GAUSX1( 9, 1)/      -0.010792D0   /
      DATA GAUSX2( 9, 1)/       6.004648D0   /
      DATA GAUSX3( 9, 1)/       1.847724D0   /
C                    DATA FOR ELEMENT  10      PM6:   NEON
      DATA REFPM6(10)  /' 10: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
     &13, 1173, (2007).'/
      DATA USSPM6(10)   /      -2.978729D0   /
      DATA UPPPM6(10)   /     -85.441118D0   /
      DATA BETASX(10)   /     -69.793475D0   /
      DATA BETAPX(10)   /     -33.261962D0   /
      DATA ZSPM6 (10)   /       6.000148D0   /
      DATA ZPPM6 (10)   /       3.834528D0   /
      DATA EISOLX(10)   /    -512.6467080D0  /
      DATA GSSPM6(10)   /      19.999574D0   /
      DATA GSPPM6(10)   /      16.896951D0   /
      DATA GPPPM6(10)   /       8.963560D0   /
      DATA GP2PM6(10)   /      16.027799D0   /
      DATA HSPPM6(10)   /       1.779280D0   /
      DATA DDPM6 (10)   /       0.2592291D0  /
      DATA QQPM6 (10)   /       0.3193991D0  /
      DATA AMPM6 (10)   /       0.7349785D0  /
      DATA ADPM6 (10)   /       1.0591897D0  /
      DATA AQPM6 (10)   /       0.7132117D0  /
C                    DATA FOR ELEMENT  11      PM6:   SODIUM
      DATA REFPM6(11)  /' 11: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
     &13, 1173, (2007).'/
      DATA USSPM6(11)   /      -4.537153D0   /
      DATA UPPPM6(11)   /      -2.433015D0   /
      DATA BETASX(11)   /       0.244853D0   /
      DATA BETAPX(11)   /       0.491998D0   /
      DATA ZSPM6 (11)   /       0.686327D0   /
      DATA ZPPM6 (11)   /       0.950068D0   /
      DATA EISOLX(11)   /      -4.5371530D0  /
      DATA GSSPM6(11)   /       4.059972D0   /
      DATA GSPPM6(11)   /       7.061183D0   /
      DATA GPPPM6(11)   /       9.283540D0   /
      DATA GP2PM6(11)   /      17.034978D0   /
      DATA HSPPM6(11)   /       0.640715D0   /
      DATA DDPM6 (11)   /       2.2523841D0  /
      DATA QQPM6 (11)   /       1.7612635D0  /
      DATA AMPM6 (11)   /       0.1492028D0  /
      DATA ADPM6 (11)   /       0.1925461D0  /
      DATA AQPM6 (11)   /       0.2000658D0  /
      DATA GAUSX1(11, 1)/      -1.026036D0   /
      DATA GAUSX2(11, 1)/       2.014506D0   /
      DATA GAUSX3(11, 1)/       1.271202D0   /
C                    DATA FOR ELEMENT  12      PM6:   MAGNESIUM
      DATA REFPM6(12)  /' 12: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
     &13, 1173, (2007).'/
      DATA USSPM6(12)   /     -14.574226D0   /
      DATA UPPPM6(12)   /      -7.583850D0   /
      DATA BETASX(12)   /      -9.604932D0   /
      DATA BETAPX(12)   /       3.416908D0   /
      DATA ZSPM6 (12)   /       1.310830D0   /
      DATA ZPPM6 (12)   /       1.388897D0   /
      DATA EISOLX(12)   /     -22.0331240D0  /
      DATA GSSPM6(12)   /       7.115328D0   /
      DATA GSPPM6(12)   /       3.253024D0   /
      DATA GPPPM6(12)   /       4.737311D0   /
      DATA GP2PM6(12)   /       8.428485D0   /
      DATA HSPPM6(12)   /       0.877379D0   /
      DATA DDPM6 (12)   /       1.4926089D0  /
      DATA QQPM6 (12)   /       1.2047834D0  /
      DATA AMPM6 (12)   /       0.2614862D0  /
      DATA ADPM6 (12)   /       0.2787598D0  /
      DATA AQPM6 (12)   /       0.2636855D0  /
C                    DATA FOR ELEMENT  13      PM6:   ALUMINUM
C     DATA REFPM6(13)  /' 13: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
C    &13, 1173, (2007).'/
      DATA USSPM6(13)   /     -24.546778D0   /
      DATA UPPPM6(13)   /     -20.104434D0   /
      DATA UDDPM6(13)   /       8.004394D0   /
      DATA BETASX(13)   /     -18.375229D0   /
      DATA BETAPX(13)   /      -9.382700D0   /
      DATA BETADX(13)   /     -20.840474D0   /
      DATA ZSPM6 (13)   /       2.364264D0   /
      DATA ZPPM6 (13)   /       1.749102D0   /
      DATA ZDPM6 (13)   /       1.269384D0   /
      DATA ZSNPM6(13)   /       4.742341D0   /
      DATA ZPNPM6(13)   /       4.669626D0   /
      DATA ZDNPM6(13)   /       7.131138D0   /
      DATA EISOLX(13)   /     -48.0620250D0  /
      DATA GSSPM6(13)   /       6.652155D0   /
      DATA GSPPM6(13)   /       7.459435D0   /
      DATA GPPPM6(13)   /       7.668857D0   /
      DATA GP2PM6(13)   /       6.673299D0   /
      DATA HSPPM6(13)   /       0.435060D0   /
      DATA DDPM6 (13)   /       0.9077315D0  /
      DATA QQPM6 (13)   /       0.9566738D0  /
      DATA AMPM6 (13)   /       0.2444648D0  /
      DATA ADPM6 (13)   /       0.2853390D0  /
      DATA AQPM6 (13)   /       0.4968328D0  /
      DATA GAUSX1(13, 1)/       1.002222D0   /
      DATA GAUSX2(13, 1)/       1.517400D0   /
      DATA GAUSX3(13, 1)/       0.659101D0   /
C                    DATA FOR ELEMENT  14      PM6:   SILICON
C     DATA REFPM6(14)  /' 14: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
C    &13, 1173, (2007).'/
      DATA USSPM6(14)   /     -27.358058D0   /
      DATA UPPPM6(14)   /     -20.490578D0   /
      DATA UDDPM6(14)   /     -22.751900D0   /
      DATA BETASX(14)   /      -8.686909D0   /
      DATA BETAPX(14)   /      -1.856482D0   /
      DATA BETADX(14)   /      -6.360627D0   /
      DATA ZSPM6 (14)   /       1.752741D0   /
      DATA ZPPM6 (14)   /       1.198413D0   /
      DATA ZDPM6 (14)   /       2.128593D0   /
      DATA ZSNPM6(14)   /       8.388111D0   /
      DATA ZPNPM6(14)   /       1.843048D0   /
      DATA ZDNPM6(14)   /       0.708600D0   /
      DATA EISOLX(14)   /     -68.4282675D0  /
      DATA GSSPM6(14)   /       5.194805D0   /
      DATA GSPPM6(14)   /       5.090534D0   /
      DATA GPPPM6(14)   /       5.185150D0   /
      DATA GP2PM6(14)   /       4.769775D0   /
      DATA HSPPM6(14)   /       1.425012D0   /
      DATA DDPM6 (14)   /       1.2076673D0  /
      DATA QQPM6 (14)   /       1.3962800D0  /
      DATA AMPM6 (14)   /       0.1909076D0  /
      DATA ADPM6 (14)   /       0.3877505D0  /
      DATA AQPM6 (14)   /       0.2915117D0  /
      DATA GAUSX1(14, 1)/       0.208571D0   /
      DATA GAUSX2(14, 1)/       6.000483D0   /
      DATA GAUSX3(14, 1)/       1.185245D0   /
C                    DATA FOR ELEMENT  15      PM6:   PHOSPHORUS
C     DATA REFPM6(15)  /' 15: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
C    &13, 1173, (2007).'/
      DATA USSPM6(15)   /     -48.729905D0   /
      DATA UPPPM6(15)   /     -40.354689D0   /
      DATA UDDPM6(15)   /      -7.349246D0   /
      DATA BETASX(15)   /     -14.583780D0   /
      DATA BETAPX(15)   /     -11.744725D0   /
      DATA BETADX(15)   /     -20.099893D0   /
      DATA ZSPM6 (15)   /       2.158033D0   /
      DATA ZPPM6 (15)   /       1.805343D0   /
      DATA ZDPM6 (15)   /       1.230358D0   /
      DATA ZSNPM6(15)   /       6.042706D0   /
      DATA ZPNPM6(15)   /       2.376473D0   /
      DATA ZDNPM6(15)   /       7.147750D0   /
      DATA EISOLX(15)   /    -139.6679330D0  /
      DATA GSSPM6(15)   /       8.758856D0   /
      DATA GSPPM6(15)   /       8.483679D0   /
      DATA GPPPM6(15)   /       8.662754D0   /
      DATA GP2PM6(15)   /       7.734264D0   /
      DATA HSPPM6(15)   /       0.871681D0   /
      DATA DDPM6 (15)   /       0.9917164D0  /
      DATA QQPM6 (15)   /       0.9268710D0  /
      DATA AMPM6 (15)   /       0.3218854D0  /
      DATA ADPM6 (15)   /       0.3535243D0  /
      DATA AQPM6 (15)   /       0.4962641D0  /
      DATA GAUSX1(15, 1)/      -0.034320D0   /
      DATA GAUSX2(15, 1)/       6.001394D0   /
      DATA GAUSX3(15, 1)/       2.296737D0   /
C                    DATA FOR ELEMENT  16      PM6:   SULFUR
C     DATA REFPM6(16)  /' 16: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
C    &13, 1173, (2007).'/
      DATA USSPM6(16)   /     -47.530706D0   /
      DATA UPPPM6(16)   /     -39.191045D0   /
      DATA UDDPM6(16)   /     -46.306944D0   /
      DATA BETASX(16)   /     -13.827440D0   /
      DATA BETAPX(16)   /      -7.664613D0   /
      DATA BETADX(16)   /      -9.986172D0   /
      DATA ZSPM6 (16)   /       2.192844D0   /
      DATA ZPPM6 (16)   /       1.841078D0   /
      DATA ZDPM6 (16)   /       3.109401D0   /
      DATA ZSNPM6(16)   /       0.479722D0   /
      DATA ZPNPM6(16)   /       1.015507D0   /
      DATA ZDNPM6(16)   /       4.317470D0   /
      DATA EISOLX(16)   /    -171.7430195D0  /
      DATA GSSPM6(16)   /       9.170350D0   /
      DATA GSPPM6(16)   /       5.944296D0   /
      DATA GPPPM6(16)   /       8.165473D0   /
      DATA GP2PM6(16)   /       7.301878D0   /
      DATA HSPPM6(16)   /       5.005404D0   /
      DATA DDPM6 (16)   /       0.9754548D0  /
      DATA QQPM6 (16)   /       0.9088806D0  /
      DATA AMPM6 (16)   /       0.3370077D0  /
      DATA ADPM6 (16)   /       0.7993884D0  /
      DATA AQPM6 (16)   /       0.4914933D0  /
      DATA GAUSX1(16, 1)/      -0.036928D0   /
      DATA GAUSX2(16, 1)/       1.795067D0   /
      DATA GAUSX3(16, 1)/       2.082618D0   /
C                    DATA FOR ELEMENT  17      PM6:   CHLORINE
C      DATA REFPM6(17)  /' 17: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
C    &13, 1173, (2007).'/
      DATA USSPM6(17)   /     -61.389930D0   /
      DATA UPPPM6(17)   /     -54.482801D0   /
      DATA UDDPM6(17)   /     -38.258155D0   /
      DATA BETASX(17)   /      -2.367988D0   /
      DATA BETAPX(17)   /     -13.802139D0   /
      DATA BETADX(17)   /      -4.037751D0   /
      DATA ZSPM6 (17)   /       2.637050D0   /
      DATA ZPPM6 (17)   /       2.118146D0   /
      DATA ZDPM6 (17)   /       1.324033D0   /
      DATA ZSNPM6(17)   /       0.956297D0   /
      DATA ZPNPM6(17)   /       2.464067D0   /
      DATA ZDNPM6(17)   /       6.410325D0   /
      DATA EISOLX(17)   /    -252.9093760D0  /
      DATA GSSPM6(17)   /      11.142654D0   /
      DATA GSPPM6(17)   /       7.487881D0   /
      DATA GPPPM6(17)   /       9.551886D0   /
      DATA GP2PM6(17)   /       8.128436D0   /
      DATA HSPPM6(17)   /       5.004267D0   /
      DATA DDPM6 (17)   /       0.8150043D0  /
      DATA QQPM6 (17)   /       0.7899928D0  /
      DATA AMPM6 (17)   /       0.4094893D0  /
      DATA ADPM6 (17)   /       0.8689914D0  /
      DATA AQPM6 (17)   /       0.6357327D0  /
      DATA GAUSX1(17, 1)/      -0.013213D0   /
      DATA GAUSX2(17, 1)/       3.687022D0   /
      DATA GAUSX3(17, 1)/       2.544635D0   /
C                    DATA FOR ELEMENT  18      PM6:   ARGON
      DATA REFPM6(18)  /' 18: (PM6): J. J. P. STEWART, J. MOL. MODEL., 
     &13, 1173, (2007).'/
      DATA USSPM6(18)   /      -7.797931D0   /
      DATA UPPPM6(18)   /     -83.211487D0   /
      DATA BETASX(18)   /      -8.839842D0   /
      DATA BETAPX(18)   /     -28.427303D0   /
      DATA ZSPM6 (18)   /       6.000272D0   /
      DATA ZPPM6 (18)   /       5.949170D0   /
      DATA EISOLX(18)   /    -499.2689220D0  /
      DATA GSSPM6(18)   /      17.858776D0   /
      DATA GSPPM6(18)   /       4.168451D0   /
      DATA GPPPM6(18)   /      11.852500D0   /
      DATA GP2PM6(18)   /      15.669543D0   /
      DATA HSPPM6(18)   /       4.574549D0   /
      DATA DDPM6 (18)   /       0.3381910D0  /
      DATA QQPM6 (18)   /       0.2812695D0  /
      DATA AMPM6 (18)   /       0.6563048D0  /
      DATA ADPM6 (18)   /       1.3215046D0  /
      DATA AQPM6 (18)   /       0.7859276D0  /
C
C     LF0410: SPECIAL HYDROGEN CONTAINING P ORBITALS.  CAN BE USED BY SPECIFYING
C     "Hp".  ADDED BY LUKE FIEDLER, APR. 2010.
C           DATA FOR ELEMENT  108     PM6:   HYDROGEN (WITH P-ORBITALS)
      DATA REFPM6(108)/' Hp: (PM6): Hydrogen (with p basis functions)  
     1                               '/
      DATA USSPM6(108)/      -7.3964270D0/
      DATA UPPPM6(108)/      -1.1486568D0/
      DATA BETASX(108)/      -9.6737870D0/
      DATA BETAPX(108)/      -1.3000000D0/
      DATA ZSPM6 (108)/       0.9880780D0/
      DATA ZPPM6 (108)/       0.3000000D0/
      DATA EISOLX(108)/      28.7118620D0/
      DATA GSSPM6(108)/      12.8480000D0/
      DATA GSPPM6(108)/       2.1600000D0/
      DATA GPPPM6(108)/       3.0100000D0/
      DATA GP2PM6(108)/       2.4400000D0/
      DATA HSPPM6(108)/       1.9200000D0/
      DATA DDPM6 (108)/       0.9675816D0/
      DATA QQPM6 (108)/       4.0824829D0/
      DATA AMPM6 (108)/       0.4721603D0/
      DATA ADPM6 (108)/       0.5007132D0/
      DATA AQPM6 (108)/       0.1603621D0/
      DATA GAUSX1(108,1)/       0.1227960D0/
      DATA GAUSX2(108,1)/       5.0000000D0/
      DATA GAUSX3(108,1)/       1.2000000D0/
      DATA GAUSX1(108,2)/       0.0050900D0/
      DATA GAUSX2(108,2)/       5.0000000D0/
      DATA GAUSX3(108,2)/       1.8000000D0/
      DATA GAUSX1(108,3)/      -0.0183360D0/
      DATA GAUSX2(108,3)/       2.0000000D0/
      DATA GAUSX3(108,3)/       2.1000000D0/
      DATA GAUSX1(108,4)/       0.0000000D0/
      DATA GAUSX2(108,4)/       0.0000000D0/
      DATA GAUSX3(108,4)/       0.0000000D0/
C
C Added by Luke Fiedler Jul 2009:                                       LF0709
***********************************************************************
*
*     START OF PM6 DIATOMIC PARAMETERS
*     Reference:  "Optimization of parameters for semiempirical 
*                  methods V: Modification of NDDO approximations and 
*                  application to 70 elements"
*               James J. P. Stewart
*               J. Molecular Modeling, 13(12), 1173-1213 (Dec 2007).
*
*     Note that these are lower triangular matrices for the values, so
*     there are no entries for ALPPM6(i,j) or XPM6(i,j) when i<j.
*
*                              Units
*     ALPPM6:                  angstrom**(-1)
*                              angstrom**(-2) for CH, NH, and OH pairs
*     XPM6:                    unitless
*
***********************************************************************
      DATA ALPPM6( 1, 1)/     3.540942D0  /
      DATA ALPPM6( 2, 1)/     2.989881D0  /
      DATA ALPPM6( 2, 2)/     3.783559D0  /
      DATA ALPPM6( 3, 1)/     2.136265D0  /     
      DATA ALPPM6( 3, 2)/     3.112403D0  /     
      DATA ALPPM6( 3, 3)/     4.714674D0  /     
      DATA ALPPM6( 4, 1)/     2.475418D0  /     
      DATA ALPPM6( 4, 2)/     3.306702D0  /     
      DATA ALPPM6( 4, 3)/     2.236728D0  /     
      DATA ALPPM6( 4, 4)/     1.499907D0  /     
      DATA ALPPM6( 5, 1)/     2.615231D0  /     
      DATA ALPPM6( 5, 2)/     3.16314D0   /     
      DATA ALPPM6( 5, 3)/     3.759397D0  /     
      DATA ALPPM6( 5, 4)/     1.888998D0  /     
      DATA ALPPM6( 5, 5)/     3.318624D0  /     
      DATA ALPPM6( 6, 1)/     1.027806D0  /    
      DATA ALPPM6( 6, 2)/     3.042705D0  /     
      DATA ALPPM6( 6, 3)/     3.241874D0  /     
      DATA ALPPM6( 6, 4)/     4.212882D0  /     
      DATA ALPPM6( 6, 5)/     2.919007D0  /     
      DATA ALPPM6( 6, 6)/     2.613713D0  /
      DATA ALPPM6( 7, 1)/     0.969406D0  /
      DATA ALPPM6( 7, 2)/     2.814339D0  /
      DATA ALPPM6( 7, 3)/     2.640623D0  /
      DATA ALPPM6( 7, 4)/     2.580895D0  /
      DATA ALPPM6( 7, 5)/     2.477004D0  /
      DATA ALPPM6( 7, 6)/     2.686108D0  /
      DATA ALPPM6( 7, 7)/     2.574502D0  /
      DATA ALPPM6( 8, 1)/     1.260942D0  /
      DATA ALPPM6( 8, 2)/     3.653775D0  /
      DATA ALPPM6( 8, 3)/     2.584442D0  /
      DATA ALPPM6( 8, 4)/     3.051867D0  /
      DATA ALPPM6( 8, 5)/     2.695351D0  /
      DATA ALPPM6( 8, 6)/     2.889607D0  /
      DATA ALPPM6( 8, 7)/     2.784292D0  /
      DATA ALPPM6( 8, 8)/     2.623998D0  /
      DATA ALPPM6( 9, 1)/     3.13674D0   /
      DATA ALPPM6( 9, 2)/     2.856543D0  /
      DATA ALPPM6( 9, 3)/     3.043901D0  /
      DATA ALPPM6( 9, 4)/     3.726923D0  /
      DATA ALPPM6( 9, 5)/     2.823837D0  /
      DATA ALPPM6( 9, 6)/     3.0276D0    /
      DATA ALPPM6( 9, 7)/     2.856646D0  /
      DATA ALPPM6( 9, 8)/     3.015444D0  /
      DATA ALPPM6( 9, 9)/     3.175759D0  /
      DATA ALPPM6(10, 1)/     5.99968D0   /
      DATA ALPPM6(10, 2)/     3.677758D0  /
      DATA ALPPM6(10, 3)/     2.193666D0  /
      DATA ALPPM6(10, 4)/     1.316588D0  /
      DATA ALPPM6(10, 5)/     2.75619D0   /
      DATA ALPPM6(10, 6)/     3.441188D0  /
      DATA ALPPM6(10, 7)/     4.42637D0   /
      DATA ALPPM6(10, 8)/     2.889587D0  /
      DATA ALPPM6(10, 9)/     3.675611D0  /
      DATA ALPPM6(10,10)/     3.974567D0  /
      DATA ALPPM6(11, 1)/     0.500326D0  /
      DATA ALPPM6(11, 2)/     1.703029D0  /
      DATA ALPPM6(11, 3)/     1.267299D0  /
      DATA ALPPM6(11, 4)/     1.25548D0   /
      DATA ALPPM6(11, 5)/     1.569961D0  /
      DATA ALPPM6(11, 6)/     2.19605D0   /
      DATA ALPPM6(11, 7)/     2.494384D0  /
      DATA ALPPM6(11, 8)/     1.981449D0  /
      DATA ALPPM6(11, 9)/     2.619551D0  /
      DATA ALPPM6(11,10)/     1.774236D0  /
      DATA ALPPM6(11,11)/     0.446435D0  /
      DATA ALPPM6(12, 1)/     2.651594D0  /
      DATA ALPPM6(12, 2)/     2.210603D0  /
      DATA ALPPM6(12, 3)/     1.18438D0   /
      DATA ALPPM6(12, 4)/     1.557591D0  /
      DATA ALPPM6(12, 5)/     2.527441D0  /
      DATA ALPPM6(12, 6)/     3.040946D0  /
      DATA ALPPM6(12, 7)/     2.079125D0  /
      DATA ALPPM6(12, 8)/     2.25152D0   /
      DATA ALPPM6(12, 9)/     3.362208D0  /
      DATA ALPPM6(12,10)/     2.031676D0  /
      DATA ALPPM6(12,11)/     1.506773D0  /
      DATA ALPPM6(12,12)/     1.093573D0  /
      DATA ALPPM6(13, 1)/     2.025996D0  /
      DATA ALPPM6(13, 2)/     2.25583D0   /
      DATA ALPPM6(13, 3)/     1.581593D0  /
      DATA ALPPM6(13, 4)/     1.938237D0  /
      DATA ALPPM6(13, 5)/     2.059569D0  /
      DATA ALPPM6(13, 6)/     2.26744D0   /
      DATA ALPPM6(13, 7)/     2.009754D0  /
      DATA ALPPM6(13, 8)/     2.49866D0   /
      DATA ALPPM6(13, 9)/     3.084258D0  /
      DATA ALPPM6(13,10)/     2.447869D0  /
      DATA ALPPM6(13,11)/     1.202871D0  /
      DATA ALPPM6(13,12)/     1.97253D0   /
      DATA ALPPM6(13,13)/     1.387714D0  /
      DATA ALPPM6(14, 1)/     1.89695D0   /
      DATA ALPPM6(14, 2)/     2.040498D0  /
      DATA ALPPM6(14, 3)/     1.789609D0  /
      DATA ALPPM6(14, 4)/     1.263132D0  /
      DATA ALPPM6(14, 5)/     1.982653D0  /
      DATA ALPPM6(14, 6)/     1.984498D0  /
      DATA ALPPM6(14, 7)/     1.818988D0  /
      DATA ALPPM6(14, 8)/     1.9236D0    /
      DATA ALPPM6(14, 9)/     2.131028D0  /
      DATA ALPPM6(14,10)/     2.867784D0  /
      DATA ALPPM6(14,11)/     2.007615D0  /
      DATA ALPPM6(14,12)/     3.139749D0  /
      DATA ALPPM6(14,13)/     1.9D0       /
      DATA ALPPM6(14,14)/     1.329D0     /
      DATA ALPPM6(15, 1)/     1.926537D0  /
      DATA ALPPM6(15, 2)/     2.093158D0  /
      DATA ALPPM6(15, 3)/     1.394544D0  /
      DATA ALPPM6(15, 4)/     1.80007D0   /
      DATA ALPPM6(15, 5)/     1.923168D0  /
      DATA ALPPM6(15, 6)/     1.994653D0  /
      DATA ALPPM6(15, 7)/     2.147042D0  /
      DATA ALPPM6(15, 8)/     2.220768D0  /
      DATA ALPPM6(15, 9)/     2.234356D0  /
      DATA ALPPM6(15,10)/     2.219036D0  /
      DATA ALPPM6(15,11)/     1.50032D0   /
      DATA ALPPM6(15,12)/     1.383773D0  /
      DATA ALPPM6(15,13)/     1.980727D0  /
      DATA ALPPM6(15,14)/     3.313466D0  /
      DATA ALPPM6(15,15)/     1.505792D0  /
      DATA ALPPM6(16, 1)/     2.215975D0  /
      DATA ALPPM6(16, 2)/     1.959149D0  /
      DATA ALPPM6(16, 3)/     2.294275D0  /
      DATA ALPPM6(16, 4)/     2.781736D0  /
      DATA ALPPM6(16, 5)/     2.403696D0  /
      DATA ALPPM6(16, 6)/     2.210305D0  /
      DATA ALPPM6(16, 7)/     2.28999D0   /
      DATA ALPPM6(16, 8)/     2.383289D0  /
      DATA ALPPM6(16, 9)/     2.187186D0  /
      DATA ALPPM6(16,10)/     2.787058D0  /
      DATA ALPPM6(16,11)/     1.40085D0   /
      DATA ALPPM6(16,12)/     1.500163D0  /
      DATA ALPPM6(16,13)/     1.976705D0  /
      DATA ALPPM6(16,14)/     1.885916D0  /
      DATA ALPPM6(16,15)/     1.595325D0  /
      DATA ALPPM6(16,16)/     1.794556D0  /
      DATA ALPPM6(17, 1)/     2.402886D0  /
      DATA ALPPM6(17, 2)/     1.671677D0  /
      DATA ALPPM6(17, 3)/     2.783001D0  /
      DATA ALPPM6(17, 4)/     2.822676D0  /
      DATA ALPPM6(17, 5)/     2.259323D0  /
      DATA ALPPM6(17, 6)/     2.162197D0  /
      DATA ALPPM6(17, 7)/     2.172134D0  /
      DATA ALPPM6(17, 8)/     2.323236D0  /
      DATA ALPPM6(17, 9)/     2.31327D0   /
      DATA ALPPM6(17,10)/     1.703151D0  /
      DATA ALPPM6(17,11)/     1.816429D0  /
      DATA ALPPM6(17,12)/     2.391806D0  /
      DATA ALPPM6(17,13)/     2.125939D0  /
      DATA ALPPM6(17,14)/     1.684978D0  /
      DATA ALPPM6(17,15)/     1.468306D0  /
      DATA ALPPM6(17,16)/     1.715435D0  /
      DATA ALPPM6(17,17)/     1.823239D0  /
      DATA ALPPM6(18, 1)/     4.056167D0  /
      DATA ALPPM6(18, 2)/     2.716562D0  /
      DATA ALPPM6(18, 3)/     3.122895D0  /
      DATA ALPPM6(18, 4)/     3.044007D0  /
      DATA ALPPM6(18, 5)/     2.415471D0  /
      DATA ALPPM6(18, 6)/     1.471309D0  /
      DATA ALPPM6(18, 7)/     2.326805D0  /
      DATA ALPPM6(18, 8)/     2.240673D0  /
      DATA ALPPM6(18, 9)/     3.920658D0  /
      DATA ALPPM6(18,10)/     2.963747D0  /
      DATA ALPPM6(18,11)/     2.167677D0  /
      DATA ALPPM6(18,12)/     2.092664D0  /
      DATA ALPPM6(18,13)/     2.645165D0  /
      DATA ALPPM6(18,14)/     1.78035D0/
      DATA ALPPM6(18,15)/     4.372516D0  /
      DATA ALPPM6(18,16)/     2.049398D0  /
      DATA ALPPM6(18,17)/     2.554449D0  /
      DATA ALPPM6(18,18)/     2.306432D0  /
      
      DATA  XPM6  ( 1, 1)/     2.243587D0  /
      DATA  XPM6  ( 2, 1)/     2.371199D0  /
      DATA  XPM6  ( 2, 2)/     3.4509D0    /
      DATA  XPM6  ( 3, 1)/     2.191985D0  /
      DATA  XPM6  ( 3, 2)/     9.273676D0  /
      DATA  XPM6  ( 3, 3)/    16.116384D0  /
      DATA  XPM6  ( 4, 1)/     2.562831D0  /
      DATA  XPM6  ( 4, 2)/    12.544878D0  /
      DATA  XPM6  ( 4, 3)/     3.287165D0  /
      DATA  XPM6  ( 4, 4)/     0.238633D0  /
      DATA  XPM6  ( 5, 1)/     1.321394D0  /
      DATA  XPM6  ( 5, 2)/     1.97417D0   /
      DATA  XPM6  ( 5, 3)/     7.886018D0  /
      DATA  XPM6  ( 5, 4)/     1.151792D0  /
      DATA  XPM6  ( 5, 5)/     3.593619D0  /
      DATA  XPM6  ( 6, 1)/     0.216506D0  /
      DATA  XPM6  ( 6, 2)/     3.213971D0  /
      DATA  XPM6  ( 6, 3)/    16.180002D0  /
      DATA  XPM6  ( 6, 4)/    25.035879D0  /
      DATA  XPM6  ( 6, 5)/     1.874859D0  /
      DATA  XPM6  ( 6, 6)/     0.81351D0   /
      DATA  XPM6  ( 7, 1)/     0.175506D0  /
      DATA  XPM6  ( 7, 2)/     1.077861D0  /
      DATA  XPM6  ( 7, 3)/     2.823403D0  /
      DATA  XPM6  ( 7, 4)/     1.740605D0  /
      DATA  XPM6  ( 7, 5)/     0.952882D0  /
      DATA  XPM6  ( 7, 6)/     0.859949D0  /
      DATA  XPM6  ( 7, 7)/     0.675313D0  /
      DATA  XPM6  ( 8, 1)/     0.192295D0  /
      DATA  XPM6  ( 8, 2)/     6.684525D0  /
      DATA  XPM6  ( 8, 3)/     1.968598D0  /
      DATA  XPM6  ( 8, 4)/     3.218155D0  /
      DATA  XPM6  ( 8, 5)/     1.269801D0  /
      DATA  XPM6  ( 8, 6)/     0.990211D0  /
      DATA  XPM6  ( 8, 7)/     0.764756D0  /
      DATA  XPM6  ( 8, 8)/     0.535112D0  /
      DATA  XPM6  ( 9, 1)/     0.815802D0  /
      DATA  XPM6  ( 9, 2)/     0.745107D0  /
      DATA  XPM6  ( 9, 3)/     1.975985D0  /
      DATA  XPM6  ( 9, 4)/     3.882993D0  /
      DATA  XPM6  ( 9, 5)/     0.862761D0  /
      DATA  XPM6  ( 9, 6)/     0.732968D0  /
      DATA  XPM6  ( 9, 7)/     0.635854D0  /
      DATA  XPM6  ( 9, 8)/     0.674251D0  /
      DATA  XPM6  ( 9, 9)/     0.681343D0  /
      DATA  XPM6  (10, 1)/     5.535021D0  /
      DATA  XPM6  (10, 2)/     1.960924D0  /
      DATA  XPM6  (10, 3)/     0.704958D0  /
      DATA  XPM6  (10, 4)/     0.392628D0  /
      DATA  XPM6  (10, 5)/     2.76414D0   /
      DATA  XPM6  (10, 6)/     5.46878D0   /
      DATA  XPM6  (10, 7)/    29.999609D0  /
      DATA  XPM6  (10, 8)/     0.763899D0  /
      DATA  XPM6  (10, 9)/     2.706754D0  /
      DATA  XPM6  (10,10)/     2.79483D0   /
      DATA  XPM6  (11, 1)/     0.207831D0  /
      DATA  XPM6  (11, 2)/     4.282517D0  /
      DATA  XPM6  (11, 3)/     0.881482D0  /
      DATA  XPM6  (11, 4)/     3.12162D0   /
      DATA  XPM6  (11, 5)/     3.188608D0  /
      DATA  XPM6  (11, 6)/     4.520429D0  /
      DATA  XPM6  (11, 7)/     8.586387D0  /
      DATA  XPM6  (11, 8)/     3.270079D0  /
      DATA  XPM6  (11, 9)/     7.047351D0  /
      DATA  XPM6  (11,10)/     1.343037D0  /
      DATA  XPM6  (11,11)/     0.287137D0  /
      DATA  XPM6  (12, 1)/     7.758237D0  /
      DATA  XPM6  (12, 2)/     3.72585D0   /
      DATA  XPM6  (12, 3)/     2.49025D0   /
      DATA  XPM6  (12, 4)/     2.066392D0  /
      DATA  XPM6  (12, 5)/     6.146701D0  /
      DATA  XPM6  (12, 6)/    10.51769D0   /
      DATA  XPM6  (12, 7)/     1.208075D0  /
      DATA  XPM6  (12, 8)/     1.535734D0  /
      DATA  XPM6  (12, 9)/     5.859023D0  /
      DATA  XPM6  (12,10)/     1.214859D0  /
      DATA  XPM6  (12,11)/     8.675619D0  /
      DATA  XPM6  (12,12)/     0.465645D0  /
      DATA  XPM6  (13, 1)/     2.958379D0  /
      DATA  XPM6  (13, 2)/     2.7014D0    /
      DATA  XPM6  (13, 3)/     1.106819D0  /
      DATA  XPM6  (13, 4)/     5.037214D0  /
      DATA  XPM6  (13, 5)/     2.741479D0  /
      DATA  XPM6  (13, 6)/     2.928056D0  /
      DATA  XPM6  (13, 7)/     1.345202D0  /
      DATA  XPM6  (13, 8)/     2.131396D0  /
      DATA  XPM6  (13, 9)/     1.975635D0  /
      DATA  XPM6  (13,10)/     1.7092D0    /
      DATA  XPM6  (13,11)/     2.071847D0  /
      DATA  XPM6  (13,12)/    13.472443D0  /
      DATA  XPM6  (13,13)/     2.1392D0    /
      DATA  XPM6  (14, 1)/     0.924196D0  /
      DATA  XPM6  (14, 2)/     1.853583D0  /
      DATA  XPM6  (14, 3)/     3.090791D0  /
      DATA  XPM6  (14, 4)/     0.623433D0  /
      DATA  XPM6  (14, 5)/     1.028287D0  /
      DATA  XPM6  (14, 6)/     0.785745D0  /
      DATA  XPM6  (14, 7)/     0.592972D0  /
      DATA  XPM6  (14, 8)/     0.751095D0  /
      DATA  XPM6  (14, 9)/     0.543516D0  /
      DATA  XPM6  (14,10)/    14.378676D0  /
      DATA  XPM6  (14,11)/     9.237644D0  /
      DATA  XPM6  (14,12)/    29.99452D0   /
      DATA  XPM6  (14,13)/     2.D0        /
      DATA  XPM6  (14,14)/     0.273477D0  /
      DATA  XPM6  (15, 1)/     1.234986D0  /
      DATA  XPM6  (15, 2)/     1.490218D0  /
      DATA  XPM6  (15, 3)/     1.12295D0   /
      DATA  XPM6  (15, 4)/     1.684831D0  /
      DATA  XPM6  (15, 5)/     1.450886D0  /
      DATA  XPM6  (15, 6)/     0.979512D0  /
      DATA  XPM6  (15, 7)/     0.972154D0  /
      DATA  XPM6  (15, 8)/     0.878705D0  /
      DATA  XPM6  (15, 9)/     0.514575D0  /
      DATA  XPM6  (15,10)/     0.774954D0  /
      DATA  XPM6  (15,11)/     2.837095D0  /
      DATA  XPM6  (15,12)/     1.177881D0  /
      DATA  XPM6  (15,13)/     5.050816D0  /
      DATA  XPM6  (15,14)/    13.239121D0  /
      DATA  XPM6  (15,15)/     0.902501D0  /
      DATA  XPM6  (16, 1)/     0.849712D0  /
      DATA  XPM6  (16, 2)/     0.437618D0  /
      DATA  XPM6  (16, 3)/     2.642502D0  /
      DATA  XPM6  (16, 4)/     3.791565D0  /
      DATA  XPM6  (16, 5)/     1.125394D0  /
      DATA  XPM6  (16, 6)/     0.666849D0  /
      DATA  XPM6  (16, 7)/     0.73871D0   /
      DATA  XPM6  (16, 8)/     0.747215D0  /
      DATA  XPM6  (16, 9)/     0.375251D0  /
      DATA  XPM6  (16,10)/     3.29616D0   /
      DATA  XPM6  (16,11)/     0.852434D0  /
      DATA  XPM6  (16,12)/     0.500748D0  /
      DATA  XPM6  (16,13)/     2.347384D0  /
      DATA  XPM6  (16,14)/     0.876658D0  /
      DATA  XPM6  (16,15)/     0.562266D0  /
      DATA  XPM6  (16,16)/     0.473856D0  /
      DATA  XPM6  (17, 1)/     0.754831D0  /
      DATA  XPM6  (17, 2)/     0.272964D0  /
      DATA  XPM6  (17, 3)/     4.227794D0  /
      DATA  XPM6  (17, 4)/     2.507275D0  /
      DATA  XPM6  (17, 5)/     0.822129D0  /
      DATA  XPM6  (17, 6)/     0.515787D0  /
      DATA  XPM6  (17, 7)/     0.520745D0  /
      DATA  XPM6  (17, 8)/     0.58551D0   /
      DATA  XPM6  (17, 9)/     0.411124D0  /
      DATA  XPM6  (17,10)/     0.125133D0  /
      DATA  XPM6  (17,11)/     1.357894D0  /
      DATA  XPM6  (17,12)/     2.430856D0  /
      DATA  XPM6  (17,13)/     2.153451D0  /
      DATA  XPM6  (17,14)/     0.513D0     /
      DATA  XPM6  (17,15)/     0.352361D0  /
      DATA  XPM6  (17,16)/     0.356971D0  /
      DATA  XPM6  (17,17)/     0.332919D0  /
      DATA  XPM6  (18, 1)/     3.933445D0  /
      DATA  XPM6  (18, 2)/     1.177211D0  /
      DATA  XPM6  (18, 3)/     3.36291D0   /
      DATA  XPM6  (18, 4)/     2.755492D0  /
      DATA  XPM6  (18, 5)/     1.931586D0  /
      DATA  XPM6  (18, 6)/     0.122309D0  /
      DATA  XPM6  (18, 7)/     0.562581D0  /
      DATA  XPM6  (18, 8)/     0.355795D0  /
      DATA  XPM6  (18, 9)/     9.269715D0  /
      DATA  XPM6  (18,10)/     1.304697D0  /
      DATA  XPM6  (18,11)/     3.398138D0  /
      DATA  XPM6  (18,12)/     1.970638D0  /
      DATA  XPM6  (18,13)/     1.852009D0  /
      DATA  XPM6  (18,14)/     1.06789D0   /
      DATA  XPM6  (18,15)/     0.171014D0  /
      DATA  XPM6  (18,16)/     0.653769D0  /
      DATA  XPM6  (18,17)/     2.256094D0  /
      DATA  XPM6  (18,18)/     0.972699D0  /
C Added by Luke Fiedler Mar 2010:                                       LF0310
***********************************************************************
*
*     START OF ATOMIC FIRST IONIZATION POTENTIALS                       LF0310
*     Reference:  Lide, D. "CRC Handbook of Chemistry and Physics",
*                 90th ed.; CRC Press: Boca Raton, FL. 2009. 
*
*     All values are in eV.
*
***********************************************************************
      DATA ATOMIP( 1)/    13.598443D0  /
      DATA ATOMIP( 2)/    24.587387D0  /
      DATA ATOMIP( 3)/     5.391719D0  /
      DATA ATOMIP( 4)/     9.32270 D0  /
      DATA ATOMIP( 5)/     8.29802 D0  /
      DATA ATOMIP( 6)/    11.26030 D0  /
      DATA ATOMIP( 7)/    14.5341  D0  /
      DATA ATOMIP( 8)/    13.61805 D0  /
      DATA ATOMIP( 9)/    17.4228  D0  /
      DATA ATOMIP(10)/    21.56454 D0  /
      DATA ATOMIP(11)/     5.139076D0  /
      DATA ATOMIP(12)/     7.646235D0  /
      DATA ATOMIP(13)/     5.985768D0  /
      DATA ATOMIP(14)/     8.15168 D0  /
      DATA ATOMIP(15)/    10.48669 D0  /
      DATA ATOMIP(16)/    10.36001 D0  /
      DATA ATOMIP(17)/    12.96763 D0  /
      DATA ATOMIP(18)/    15.759610D0  /
      DATA ATOMIP(19)/     4.3406633D0 /
      DATA ATOMIP(20)/     6.11316 D0  /
      DATA ATOMIP(21)/     6.56149 D0  /
      DATA ATOMIP(22)/     6.82812 D0  /
      DATA ATOMIP(23)/     6.74619 D0  /
      DATA ATOMIP(24)/     6.76651 D0  /
      DATA ATOMIP(25)/     7.43402 D0  /
      DATA ATOMIP(26)/     7.9024  D0  /
      DATA ATOMIP(27)/     7.88101 D0  /
      DATA ATOMIP(28)/     7.6398  D0  /
      DATA ATOMIP(29)/     7.72638 D0  /
      DATA ATOMIP(30)/     9.394199D0  /
      DATA ATOMIP(31)/     5.999301D0  /
      DATA ATOMIP(32)/     7.89943 D0  /
      DATA ATOMIP(33)/     9.7886  D0  /
      DATA ATOMIP(34)/     9.75239 D0  /
      DATA ATOMIP(35)/    11.8138  D0  /
      DATA ATOMIP(36)/    13.99961 D0  /
      DATA ATOMIP(37)/     4.177128D0  /
      DATA ATOMIP(38)/     5.69485 D0  /
      DATA ATOMIP(39)/     6.2173  D0  /
      DATA ATOMIP(40)/     6.63390 D0  /
      DATA ATOMIP(41)/     6.75885 D0  /
      DATA ATOMIP(42)/     7.09243 D0  /
      DATA ATOMIP(43)/     7.28    D0  /
      DATA ATOMIP(44)/     7.36050 D0  /
      DATA ATOMIP(45)/     7.45890 D0  /
      DATA ATOMIP(46)/     8.3369  D0  /
      DATA ATOMIP(47)/     7.57623 D0  /
      DATA ATOMIP(48)/     8.99382 D0  /
      DATA ATOMIP(49)/     5.78636 D0  /
      DATA ATOMIP(50)/     7.34392 D0  /
      DATA ATOMIP(51)/     8.60839 D0  /
      DATA ATOMIP(52)/     9.0096  D0  /
      DATA ATOMIP(53)/    10.45126 D0  /
      DATA ATOMIP(54)/    12.12984 D0  /
      DATA ATOMIP(55)/     3.893905D0  /
      DATA ATOMIP(56)/     5.211664D0  /
      DATA ATOMIP(57)/     5.5769  D0  /
      DATA ATOMIP(58)/     5.5387  D0  /
      DATA ATOMIP(59)/     5.473   D0  /
      DATA ATOMIP(60)/     5.5250  D0  /
      DATA ATOMIP(61)/     5.582   D0  /
      DATA ATOMIP(62)/     5.6437  D0  /
      DATA ATOMIP(63)/     5.67038 D0  /
      DATA ATOMIP(64)/     6.14980 D0  /
      DATA ATOMIP(65)/     5.8638  D0  /
      DATA ATOMIP(66)/     5.9389  D0  /
      DATA ATOMIP(67)/     6.0215  D0  /
      DATA ATOMIP(68)/     6.1077  D0  /
      DATA ATOMIP(69)/     6.18431 D0  /
      DATA ATOMIP(70)/     6.25416 D0  /
      DATA ATOMIP(71)/     5.42586 D0  /
      DATA ATOMIP(72)/     6.82507 D0  /
      DATA ATOMIP(73)/     7.54957 D0  /
      DATA ATOMIP(74)/     7.86403 D0  /
      DATA ATOMIP(75)/     7.83352 D0  /
      DATA ATOMIP(76)/     8.43823 D0  /
      DATA ATOMIP(77)/     8.96702 D0  /
      DATA ATOMIP(78)/     8.9588  D0  /
      DATA ATOMIP(79)/     9.22553 D0  /
      DATA ATOMIP(80)/    10.4375  D0  /
      DATA ATOMIP(81)/     6.108194D0  /
      DATA ATOMIP(82)/     7.41663 D0  /
      DATA ATOMIP(83)/     7.2855  D0  /
      DATA ATOMIP(84)/     8.414   D0  /
C     LF0310: NO DATA AVAILABLE IN CRC FOR ELEMENT ASTATINE.
C      DATA ATOMIP(85)/                /
      DATA ATOMIP(86)/    10.7485  D0  /
      DATA ATOMIP(87)/     4.072741D0  /
      DATA ATOMIP(88)/     5.278423D0  /
      DATA ATOMIP(89)/     5.17    D0  /
      DATA ATOMIP(90)/     6.3067  D0  /
      DATA ATOMIP(91)/     5.89    D0  /
      DATA ATOMIP(92)/     6.1941  D0  /
      DATA ATOMIP(93)/     6.2657  D0  /
      DATA ATOMIP(94)/     6.0260  D0  /
      DATA ATOMIP(95)/     5.9738  D0  /
      DATA ATOMIP(96)/     5.9914  D0  /
      DATA ATOMIP(97)/     6.1979  D0  /
      DATA ATOMIP(98)/     6.2817  D0  /
      DATA ATOMIP(99)/     6.42    D0  /
      DATA ATOMIP(100)/     6.50    D0  /
      DATA ATOMIP(101)/     6.58    D0  /
      DATA ATOMIP(102)/     6.65    D0  /
      DATA ATOMIP(103)/     4.9     D0  /
      DATA ATOMIP(104)/     6.0     D0  /
      DATA ATOMIP(108)/    13.598443D0  /
C
C Added by Luke Fiedler Oct 2010:                                       LF1010
***********************************************************************
*
*     START OF PMO version 1 (PMOv1) PARAMETERS
*     Reference:  Zhang, Fiedler, Leverentz, Truhlar, Gao
*                 JCTC, 7(4), 857-867 (2011).
*
*     The PMOv1 method only is specified for systems containing
*     hydrogen (Hp-type) and oxygen atoms.  Future version of PMO
*     will contain parameters for additional elements.
*
*                                   Units
*     USSPMOV1, UPPPMOV1:            eV
*     ZSPMOV1,  ZPPMOV1:             bohr**(-1)
*     BETASPMOV1, BETAPMOV1:         eV
*     GSSPMOV1, GSPPMOV1, GPPPMOV1:  eV 
*     GP2PMOV1, HSPPMOV1:            eV
*     ALPHAPMOV1:                    angstrom**(-1)
*     EISOLPMOV1:                    eV
*     R0PMOV1:                       angstrom
*     C6PMOV1:                       J (nm**6)/mol
*     S6PMOV1:                       unitless
*     ALPPMOV1:                      unitless
*
*
***********************************************************************
C                    DATA FOR ELEMENT  108      HYDROGEN (Hp-type)
      DATA REFPMOV1  (108) /' Hp: (PMOv1):  Zhang, Fiedler, Leverentz, T
     &ruhlar, Gao, JCTC, 7, 857 (2011).'
      DATA USSPMOV1  (108) /   -11.22813D0 / 
      DATA UPPPMOV1  (108) /    -9.95254D0 / 
      DATA ZSPMOV1   (108) /     1.08419D0 / 
      DATA ZPPMOV1   (108) /     0.88997D0 / 
      DATA BETASPMOV1(108) /    -6.89857D0 / 
      DATA BETAPPMOV1(108) /    -3.77765D0 / 
      DATA GSSPMOV1  (108) /    12.65697D0 / 
      DATA GSPPMOV1  (108) /    11.34825D0 / 
      DATA GPPPMOV1  (108) /     6.17416D0 / 
      DATA GP2PMOV1  (108) /    10.04410D0 / 
      DATA HSPPMOV1  (108) /     2.3256D0  / 
      DATA ALPHAPMOV1(108) /     3.16046D0 / 
      DATA AMPMOV1   (108) /     0.465139961265807D0 /
      DATA ADPMOV1   (108) /     0.568077345927494D0 /
      DATA AQPMOV1   (108) /     0.239250932573985D0 /
      DATA DDPMOV1   (108) /     0.900191176906711D0 /
      DATA QQPMOV1   (108) /     1.376164220582250D0 /
      DATA EISOLPMOV1(108) /   -11.228130000000000D0 /
C      DATA FN11PMOV1 (108) /     0.0D0     /
C      DATA FN12PMOV1 (108) /     0.0D0     /
C      DATA FN13PMOV1 (108) /     0.0D0     /
C      DATA FN14PMOV1 (108) /     0.0D0     /
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFPMOV1  (  8) /'  O: (PMOv1):  Zhang, Fiedler, Leverentz, T
     &ruhlar, Gao, JCTC, 7, 857 (2011).'
      DATA USSPMOV1   (8)  /  -114.78169D0 /
      DATA UPPPMOV1   (8)  /   -78.04828D0 /
      DATA ZSPMOV1    (8)  /     3.19623D0 /
      DATA ZPPMOV1    (8)  /     3.11976D0 /
      DATA BETASPMOV1 (8)  /   -31.51770D0 /
      DATA BETAPPMOV1 (8)  /   -35.10436D0 /
      DATA GSSPMOV1   (8)  /    18.22143D0 /
      DATA GSPPMOV1   (8)  /    12.73220D0 /
      DATA GPPPMOV1   (8)  /    15.03924D0 /
      DATA GP2PMOV1   (8)  /    13.52768D0 /
      DATA HSPPMOV1   (8)  /     4.19786D0  /
      DATA ALPHAPMOV1 (8)  /     3.44202D0  /
      DATA AMPMOV1    (8)  /     0.669632245664453D0 /
      DATA ADPMOV1    (8)  /     1.075681372993430D0 /
      DATA AQPMOV1    (8)  /     1.043974857993200D0 /
      DATA DDPMOV1    (8)  /     0.456886972954541D0 /
      DATA QQPMOV1    (8)  /     0.392576631340741D0 /
      DATA EISOLPMOV1 (8)  /  -358.0586100D0 /
C      DATA FN11PMOV1  (8)  /     0.0D0      /
C      DATA FN12PMOV1  (8)  /     0.0D0      /
C      DATA FN13PMOV1  (8)  /     0.0D0      /
C      DATA FN14PMOV1  (8)  /     0.0D0      /
C
C Added by Luke Fiedler Jan 2013:                                       LF0113
***********************************************************************
*
*     START OF PMO version 2 (PMO2) PARAMETERS
*     Reference:  Isegawa, Fiedler, Leverentz, Wang, Nachimuthu, Gao, Truhlar
*                 JCTC, 9(1), 33-45 (2013).
*
*     The PMO2 method only is specified for systems containing
*     hydrogen (Hp-type), carbon, and oxygen atoms.  Future version of PMO
*     will contain parameters for additional elements.
*
*                                   Units
*     USSPMO2, UPPPMO2:              eV
*     ZSPMO2,  ZPPMO2:               bohr**(-1)
*     BETASPMO2, BETAPMO2:           eV
*     GSSPMO2, GSPPMO2, GPPPMO2:     eV 
*     GP2PMO2, HSPPMO2:              eV
*     ALPHAPMO2:                     angstrom**(-1)
*     EISOLPMO2:                     eV
*     R0PMO2:                        angstrom
*     C6PMO2:                        J (nm**6)/mol
*     S6PMO2:                        unitless
*     ALPPMO2:                       unitless
*
*
***********************************************************************
C                    DATA FOR ALL ELEMENTS
      DATA RPNUMPMO2      /       8.000D0 /
      DATA RPDENPMO2      /       2.000D0 /
      DATA DPDENPMO2      /       2.000D0 /
      DATA SPDENPMO2      /       4.000D0 /
      DATA S6PMO2         /       1.400D0 /
      DATA ALPDPMO2       /      23.000D0 /

C                    DATA FOR ELEMENT  108      HYDROGEN (Hp-type)
      DATA REFPMO2  (108) / '  Hp: (PMO2): Isegawa, Fiedler, Leverentz, 
     &Wang, et al., JCTC, 9, 33 (2013).'/
      DATA USSPMO2  (108) /     -10.589D0 / 
      DATA UPPPMO2  (108) /      -7.235D0 / 
      DATA ZSPMO2   (108) /       1.189D0 / 
      DATA ZPPMO2   (108) /       0.878D0 / 
      DATA GSSPMO2  (108) /      12.376D0 / 
      DATA GSPPMO2  (108) /       8.160D0 / 
      DATA GPPPMO2  (108) /       7.364D0 / 
      DATA GP2PMO2  (108) /      11.346D0 / 
      DATA HSPPMO2  (108) /       1.882D0 / 
      DATA ALPHAPMO2(108) /       3.615D0 / 
      DATA AMPMO2   (108) /    0.4548143955959149D0 /
      DATA ADPMO2   (108) /    0.5543733050460202D0 /
      DATA AQPMO2   (108) /    0.2369012584161755D0 /
      DATA DDPMO2   (108) /    0.7942489820338561D0 /
      DATA QQPMO2   (108) /    1.3949258216305112D0 /
      DATA EISOLPMO2(108) /     -10.58900000000D0 /
      DATA C6PMO2   (108) /       0.32000D0 /
      DATA RVDWPMO2 (108) /       1.11000D0 /

C                    DATA FOR ELEMENT  6        CARBON
      DATA REFPMO2  (  6) / '  Hp: (PMO2): Isegawa, Fiedler, Leverentz, 
     &Wang, et al., JCTC, 9, 33 (2013).'/
      DATA USSPMO2  (  6) /     -48.566D0 / 
      DATA UPPPMO2  (  6) /     -40.922D0 / 
      DATA ZSPMO2   (  6) /       1.998D0 / 
      DATA ZPPMO2   (  6) /       1.637D0 / 
      DATA GSSPMO2  (  6) /      11.795D0 / 
      DATA GSPPMO2  (  6) /      11.207D0 / 
      DATA GPPPMO2  (  6) /      12.541D0 / 
      DATA GP2PMO2  (  6) /       9.870D0 / 
      DATA HSPPMO2  (  6) /       1.944D0 / 
      DATA ALPHAPMO2(  6) /       3.014D0 / 
      DATA AMPMO2   (  6) /    0.4334628148071927D0 /
      DATA ADPMO2   (  6) /    0.5701996085172113D0 /
      DATA AQPMO2   (  6) /    0.8247703886618970D0 /
      DATA DDPMO2   (  6) /    0.7747173324559324D0 /
      DATA QQPMO2   (  6) /    0.7481642464212517D0 /
      DATA EISOLPMO2(  6) /    -117.70650000000D0 /
      DATA C6PMO2   (  6) /       0.92000D0 /
      DATA RVDWPMO2 (  6) /       1.61000D0 /

C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFPMO2  (  8) / '  Hp: (PMO2): Isegawa, Fiedler, Leverentz, 
     &Wang, et al., JCTC, 9, 33 (2013).'/
      DATA USSPMO2  (  8) /     -91.714D0 / 
      DATA UPPPMO2  (  8) /     -72.331D0 / 
      DATA ZSPMO2   (  8) /       2.833D0 / 
      DATA ZPPMO2   (  8) /       3.468D0 / 
      DATA GSSPMO2  (  8) /      21.437D0 / 
      DATA GSPPMO2  (  8) /      12.017D0 / 
      DATA GPPPMO2  (  8) /      12.166D0 / 
      DATA GP2PMO2  (  8) /      12.686D0 / 
      DATA HSPPMO2  (  8) /       4.396D0 / 
      DATA ALPHAPMO2(  8) /       4.548D0 / 
      DATA AMPMO2   (  8) /    0.7878035066572099D0 /
      DATA ADPMO2   (  8) /    1.1121102402179852D0 /
      DATA AQPMO2   (  8) /    0.6606565534640932D0 /
      DATA DDPMO2   (  8) /    0.4465978581709464D0 /
      DATA QQPMO2   (  8) /    0.3531559606088780D0 /
      DATA EISOLPMO2(  8) /    -296.38700000000D0 /
      DATA C6PMO2   (  8) /       2.60000D0 /
      DATA RVDWPMO2 (  8) /       1.48000D0 /

C                    DATA FOR PAIRS OF ELEMENTS
      DATA PRBETAPMO2 (108,108,1,1) /   -6.098000D0  /
      DATA PRBETAPMO2 (108,108,1,2) /    0.000000D0  /
      DATA PRBETAPMO2 (108,108,2,1) /    0.000000D0  /
      DATA PRBETAPMO2 (108,108,2,2) /   -0.760000D0  /
      DATA PRBETAPMO2 (  6,108,1,1) /  -13.704000D0  /
      DATA PRBETAPMO2 (108,  6,1,1) /  -13.704000D0  /
      DATA PRBETAPMO2 (  6,108,1,2) /   -6.701000D0  /
      DATA PRBETAPMO2 (108,  6,2,1) /   -0.282000D0  /
      DATA PRBETAPMO2 (  6,  6,1,1) /  -13.168000D0  /
      DATA PRBETAPMO2 (108,  6,1,2) /   -6.701000D0  /
      DATA PRBETAPMO2 (  6,108,2,1) /   -6.701000D0  /
      DATA PRBETAPMO2 (  6,108,2,2) /   -0.762000D0  /
      DATA PRBETAPMO2 (108,  6,2,2) /   -0.762000D0  /
      DATA PRBETAPMO2 (  6,  6,1,2) /  -14.640000D0  /
      DATA PRBETAPMO2 (  6,  6,2,1) /  -14.640000D0  /
      DATA PRBETAPMO2 (  6,  6,2,2) /   -6.851000D0  /
      DATA PRBETAPMO2 (  8,108,1,1) /  -14.923000D0  /
      DATA PRBETAPMO2 (108,  8,1,1) /  -14.923000D0  /
      DATA PRBETAPMO2 (  8,108,1,2) /   -0.347000D0  /
      DATA PRBETAPMO2 (108,  8,2,1) /   -0.347000D0  /
      DATA PRBETAPMO2 (  8,  6,1,1) /  -27.678000D0  /
      DATA PRBETAPMO2 (  6,  8,1,1) /  -27.678000D0  /
      DATA PRBETAPMO2 (  8,  6,1,2) /  -17.943000D0  /
      DATA PRBETAPMO2 (  6,  8,2,1) /  -17.943000D0  /
      DATA PRBETAPMO2 (  8,  8,1,1) /  -20.932000D0  /
      DATA PRBETAPMO2 (108,  8,1,2) /  -25.614000D0  /
      DATA PRBETAPMO2 (  8,108,2,1) /  -25.614000D0  /
      DATA PRBETAPMO2 (  8,108,2,2) /   -1.639000D0  /
      DATA PRBETAPMO2 (108,  8,2,2) /   -1.639000D0  /
      DATA PRBETAPMO2 (  6,  8,1,2) /  -39.434000D0  /
      DATA PRBETAPMO2 (  8,  6,2,1) /  -39.434000D0  /
      DATA PRBETAPMO2 (  8,  6,2,2) /  -26.349000D0  /
      DATA PRBETAPMO2 (  6,  8,2,2) /  -26.349000D0  /
      DATA PRBETAPMO2 (  8,  8,1,2) /  -18.304000D0  /
      DATA PRBETAPMO2 (  8,  8,2,1) /  -18.304000D0  /
      DATA PRBETAPMO2 (  8,  8,2,2) /  -37.915000D0  /

      DATA KPAIRPMO2 (108,108,1,1) /   -0.247000D0  /
      DATA KPAIRPMO2 (108,108,1,2) /    0.754000D0  /
      DATA KPAIRPMO2 (108,108,2,1) /    0.754000D0  /
      DATA KPAIRPMO2 (108,108,2,2) /    0.132000D0  /
      DATA KPAIRPMO2 (  6,108,1,1) /   -0.325000D0  /
      DATA KPAIRPMO2 (108,  6,1,1) /   -0.325000D0  /
      DATA KPAIRPMO2 (  6,108,1,2) /    0.389000D0  /
      DATA KPAIRPMO2 (108,  6,2,1) /    0.389000D0  /
      DATA KPAIRPMO2 (  6,  6,1,1) /   -0.318000D0  /
      DATA KPAIRPMO2 (108,  6,1,2) /    0.059000D0  /
      DATA KPAIRPMO2 (  6,108,2,1) /    0.059000D0  /
      DATA KPAIRPMO2 (  6,108,2,2) /    0.021000D0  /
      DATA KPAIRPMO2 (108,  6,2,2) /    0.021000D0  /
      DATA KPAIRPMO2 (  6,  6,1,2) /   -0.125000D0  /
      DATA KPAIRPMO2 (  6,  6,2,1) /   -0.125000D0  /
      DATA KPAIRPMO2 (  6,  6,2,2) /    0.100000D0  /
      DATA KPAIRPMO2 (  8,108,1,1) /   -0.147000D0  /
      DATA KPAIRPMO2 (108,  8,1,1) /   -0.147000D0  /
      DATA KPAIRPMO2 (  8,108,1,2) /    0.252000D0  /
      DATA KPAIRPMO2 (108,  8,2,1) /    0.252000D0  /
      DATA KPAIRPMO2 (  8,  6,1,1) /   -0.260000D0  /
      DATA KPAIRPMO2 (  6,  8,1,1) /   -0.260000D0  /
      DATA KPAIRPMO2 (  8,  6,1,2) /   -0.066000D0  /
      DATA KPAIRPMO2 (  6,  8,2,1) /   -0.066000D0  /
      DATA KPAIRPMO2 (  8,  8,1,1) /   -0.913000D0  /
      DATA KPAIRPMO2 (108,  8,1,2) /   -0.013000D0  /
      DATA KPAIRPMO2 (  8,108,2,1) /   -0.013000D0  /
      DATA KPAIRPMO2 (  8,108,2,2) /    0.308000D0  /
      DATA KPAIRPMO2 (108,  8,2,2) /    0.308000D0  /
      DATA KPAIRPMO2 (  6,  8,1,2) /   -0.157000D0  /
      DATA KPAIRPMO2 (  8,  6,2,1) /   -0.157000D0  /
      DATA KPAIRPMO2 (  8,  6,2,2) /    0.036000D0  /
      DATA KPAIRPMO2 (  6,  8,2,2) /    0.036000D0  /
      DATA KPAIRPMO2 (  8,  8,1,2) /    0.871000D0  /
      DATA KPAIRPMO2 (  8,  8,2,1) /    0.871000D0  /
      DATA KPAIRPMO2 (  8,  8,2,2) /   -0.907000D0  /

      DATA C0ABPMO2 (108,108)   /    0.353000D0  /
      DATA C0ABPMO2 (108,  6)   /    0.538000D0  /
      DATA C0ABPMO2 (  6,108)   /    0.538000D0  /
      DATA C0ABPMO2 (108,  8)   /    0.573000D0  /
      DATA C0ABPMO2 (  8,108)   /    0.573000D0  /
      DATA C0ABPMO2 (  6,  6)   /    0.811000D0  /
      DATA C0ABPMO2 (  6,  8)   /    0.530000D0  /
      DATA C0ABPMO2 (  8,  6)   /    0.530000D0  /
      DATA C0ABPMO2 (  8,  8)   /    0.558000D0  /

      DATA C1ABPMO2 (108,108)   /    0.388000D0  /
      DATA C1ABPMO2 (108,  6)   /    0.173000D0  /
      DATA C1ABPMO2 (  6,108)   /    0.562000D0  /
      DATA C1ABPMO2 (108,  8)   /    0.663000D0  /
      DATA C1ABPMO2 (  8,108)   /    0.477000D0  /
      DATA C1ABPMO2 (  6,  6)   /    0.297000D0  /
      DATA C1ABPMO2 (  6,  8)   /    0.695000D0  /
      DATA C1ABPMO2 (  8,  6)   /    0.799000D0  /
      DATA C1ABPMO2 (  8,  8)   /    0.338000D0  /

      DATA C2ABPMO2 (108,108)   /    0.388000D0  /
      DATA C2ABPMO2 (108,  6)   /    0.562000D0  /
      DATA C2ABPMO2 (  6,108)   /    0.173000D0  /
      DATA C2ABPMO2 (108,  8)   /    0.477000D0  /
      DATA C2ABPMO2 (  8,108)   /    0.663000D0  /
      DATA C2ABPMO2 (  6,  6)   /    0.297000D0  /
      DATA C2ABPMO2 (  6,  8)   /    0.799000D0  /
      DATA C2ABPMO2 (  8,  6)   /    0.695000D0  /
      DATA C2ABPMO2 (  8,  8)   /    0.338000D0  /

      DATA PRALPPMO2 (108,108)   /    2.494000D0  /
      DATA PRALPPMO2 (108,  6)   /    2.179000D0  /
      DATA PRALPPMO2 (  6,108)   /    2.179000D0  /
      DATA PRALPPMO2 (108,  8)   /    2.970000D0  /
      DATA PRALPPMO2 (  8,108)   /    2.970000D0  /
      DATA PRALPPMO2 (  6,  6)   /    3.038000D0  /
      DATA PRALPPMO2 (  6,  8)   /    2.571000D0  /
      DATA PRALPPMO2 (  8,  6)   /    2.571000D0  /
      DATA PRALPPMO2 (  8,  8)   /    3.984000D0  /

      DATA C3ABPMO2 (108,108)   /    0.349000D0  /
      DATA C3ABPMO2 (108,  6)   /    0.530000D0  /
      DATA C3ABPMO2 (  6,108)   /    0.530000D0  /
      DATA C3ABPMO2 (108,  8)   /    0.645000D0  /
      DATA C3ABPMO2 (  8,108)   /    0.645000D0  /
      DATA C3ABPMO2 (  6,  6)   /    0.629000D0  /
      DATA C3ABPMO2 (  6,  8)   /    0.350000D0  /
      DATA C3ABPMO2 (  8,  6)   /    0.350000D0  /
      DATA C3ABPMO2 (  8,  8)   /    0.621000D0  /

      DATA C3RPWRPMO2 (108,108)   /    0.000000D0  /
      DATA C3RPWRPMO2 (108,  6)   /    0.000000D0  /
      DATA C3RPWRPMO2 (  6,108)   /    0.000000D0  /
      DATA C3RPWRPMO2 (108,  8)   /    1.000000D0  /
      DATA C3RPWRPMO2 (  8,108)   /    1.000000D0  /
      DATA C3RPWRPMO2 (  6,  6)   /    0.000000D0  /
      DATA C3RPWRPMO2 (  6,  8)   /    0.000000D0  /
      DATA C3RPWRPMO2 (  8,  6)   /    0.000000D0  /
      DATA C3RPWRPMO2 (  8,  8)   /    0.000000D0  /

      DATA PR3ALPPMO2 (108,108)   /    3.993000D0  /
      DATA PR3ALPPMO2 (108,  6)   /    2.474000D0  /
      DATA PR3ALPPMO2 (  6,108)   /    2.474000D0  /
      DATA PR3ALPPMO2 (108,  8)   /    3.071000D0  /
      DATA PR3ALPPMO2 (  8,108)   /    3.071000D0  /
      DATA PR3ALPPMO2 (  6,  6)   /    2.083000D0  /
      DATA PR3ALPPMO2 (  6,  8)   /    2.895000D0  /
      DATA PR3ALPPMO2 (  8,  6)   /    2.895000D0  /
      DATA PR3ALPPMO2 (  8,  8)   /    5.438000D0  /

      DATA CPAIRPMO2 (108,108)   /    0.200000D0  /
      DATA CPAIRPMO2 (108,  6)   /    0.291713D0  /
      DATA CPAIRPMO2 (  6,108)   /    0.291713D0  /
      DATA CPAIRPMO2 (108,  8)   /    0.260465D0  /
      DATA CPAIRPMO2 (  8,108)   /    0.260465D0  /
      DATA CPAIRPMO2 (  6,  6)   /    1.650000D0  /
      DATA CPAIRPMO2 (  6,  8)   /    0.982979D0  /
      DATA CPAIRPMO2 (  8,  6)   /    0.982979D0  /
      DATA CPAIRPMO2 (  8,  8)   /    0.700000D0  /

      DATA DPAIRPMO2 (108,108)   /    1.270000D0  /
      DATA DPAIRPMO2 (108,  6)   /    1.770000D0  /
      DATA DPAIRPMO2 (  6,108)   /    1.770000D0  /
      DATA DPAIRPMO2 (108,  8)   /    1.650000D0  /
      DATA DPAIRPMO2 (  8,108)   /    1.650000D0  /
      DATA DPAIRPMO2 (  6,  6)   /    3.260000D0  /
      DATA DPAIRPMO2 (  6,  8)   /    3.140000D0  /
      DATA DPAIRPMO2 (  8,  6)   /    3.140000D0  /
      DATA DPAIRPMO2 (  8,  8)   /    2.190000D0  /

C
C Added by Luke Fiedler Nov 2011:                                       LF1211
***********************************************************************
*
*     START OF GRIMME'S D3 DISPERSION REFERENCES
*     Reference:  Stefan Grimme, Jens Antony, Stephan Ehrlich, Helge Krieg
*                 J. Chem. Phys., 132, 154104 (2010).
*
***********************************************************************
      DATA REFD3 (  1) 
     &/'  H: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, 154104 (2010)'/
      DATA REFD3 (  6) 
     &/'  C: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, 154104 (2010)'/
      DATA REFD3 (  7) 
     &/'  N: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, 154104 (2010)'/
      DATA REFD3 (  8) 
     &/'  O: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, 154104 (2010)'/
      DATA REFD3 ( 15) 
     &/'  P: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, 154104 (2010)'/
      DATA REFD3 ( 16) 
     &/'  S: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, 154104 (2010)'/
      DATA REFD3 ( 17) 
     &/' Cl: (-D3): GRIMME ET AL., J. CHEM. PHYS., 132, 154104 (2010)'/


      END
