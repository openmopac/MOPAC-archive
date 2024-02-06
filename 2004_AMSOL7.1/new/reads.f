      SUBROUTINE READS                                                  GL0492
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'SIZES2.i'
      INCLUDE 'KEYS.i'                                                  DJG0995
      INCLUDE 'FFILES.i'                                                GDH1095
      CHARACTER*80 CDAT,KEY3                                            GDH1194
      CHARACTER*80 LINE,BLANK                                           DJG0995
      CHARACTER*50 CTYPE(100)                                           GDH0696
      CHARACTER SPACE*1,SPACE2*2,CH*1,CH2*2,MODEL*7
      CHARACTER SOLVNT*6, CCHEK*1                                       DJG0995
      LOGICAL INTR,AM1,MINDO,MNDO,PM3,MNDOC,LDER                        DJG0995
      LOGICAL TAREA,VOLM,FIRST,XKWFLG,SMODEL                            DJG0496
      DIMENSION LOPT(3,NUMATM)
      DIMENSION IZOK(107),JZOK(107),KZOK(107),LZOK(107),
     1          COORD(3,NUMATM),VALUE(40)
C ---------------------------------------------------------------------
C MODULE TO READ IN GEOMETRY FILE, OUTPUT IT TO THE USER,
C AND CHECK THE DATA TO SEE IF IT IS REASONABLE. EXIT IF NECESSARY.
C  ON EXIT NATOMS    = NUMBER OF ATOMS PLUS DUMMY ATOMS (IF ANY).
C          LABELS    = ARRAY OF ATOMIC LABELS INCLUDING DUMMY ATOMS.
C          GEO       = ARRAY OF INTERNAL COORDINATES.
C          LOPT      = FLAGS FOR OPTIMIZATION OF MOLECULE
C          NA        = ARRAY OF LABELS OF ATOMS, BOND LENGTHS.
C          NB        = ARRAY OF LABELS OF ATOMS, BOND ANGLES.
C          NC        = ARRAY OF LABELS OF ATOMS, DIHEDRAL ANGLES.
C          LATOM     = LABEL OF ATOM OF REACTION COORDINATE.
C          LPARAM    = RC: 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL
C          REACT(100)= REACTION COORDINATE PARAMETERS
C          LOC(1,I)  = LABEL OF ATOM TO BE OPTIMISED.
C          LOC(2,I)  = 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL.
C          NVAR      = NUMBER OF PARAMETERS TO BE OPTIMISED.
C          XPARAM    = STARTING VALUE OF PARAMETERS TO BE OPTIMISED.
C
C     REVISED (OPTIMIZATION IN CARTESIAN COORDS.), D.L., MAY 1989
C     MNDOC PARAMETERS INTRODUCED BY F.B., NOVEMBER 90
C ----------------------------------------------------------------------
C *** INPUT THE TRIAL GEOMETRY  \IE.  KGEOM=0\
C   LABEL(I) = THE ATOMIC NUMBER OF ATOM\I\.
C            = 99, THEN THE I-TH ATOM IS A DUMMY ATOM USED ONLY TO
C              SIMPLIFY THE DEFINITION OF THE MOLECULAR GEOMETRY.
C   GEO(1,I) = THE INTERNUCLEAR SEPARATION \IN ANGSTROMS\ BETWEEN ATOMS
C              NA(I) AND (I).
C   GEO(2,I) = THE ANGLE NB(I):NA(I):(I) INPUT IN DEGREES; STORED IN
C              RADIANS.
C   GEO(3,I) = THE ANGLE BETWEEN THE VECTORS NC(I):NB(I) AND NA(I):(I)
C              INPUT IN DEGREES - STORED IN RADIANS.
C  LOPT(J,I) = -1 IF GEO(J,I) IS THE REACTION COORDINATE.
C            = +1 IF GEO(J,I) IS A PARAMETER TO BE OPTIMISED
C            =  0 OTHERWISE.
C *** NOTE:    MUCH OF THIS DATA IS NOT INCLUDED FOR THE FIRST 3 ATOMS.
C     ATOM1  INPUT LABELS(1) ONLY.
C     ATOM2  INPUT LABELS(2) AND GEO(1,2) SEPARATION BETWEEN ATOMS 1+2
C     ATOM3  INPUT LABELS(3), GEO(1,3)    SEPARATION BETWEEN ATOMS 2+3
C              AND GEO(2,3)               ANGLE ATOM1 : ATOM2 : ATOM3
C ----------------------------------------------------------------------
C
      COMMON /MESH  / LATOM1, LPARA1, LATOM2, LPARA2                    DJG0995
     3       /ISTOPE/ AMS(107)
     4       /GEOM  / GEO(3,NUMATM)
     5       /GEOKST/ NATOMS,LABELS(NUMATM)
     6               ,NA(NUMATM),NB(NUMATM),NC(NUMATM)
     7       /GEPCM / GEPOL,DOTS                                        GDH1092
C    2       /GEOVAR/ NVAR, LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
C    3       /REPATH/ LATOM,LPARAM,REACT(100)
      COMMON /REPATH/ REACT(100),LATOM,LPARAM                           03GCL93
      COMMON /OPTIMI / IMP,IMP0
      COMMON /GEOVAR/ XPARAM(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY           3GL3092
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
     1       /PRECI / SCFCV,SCFTOL,EG(3),ESTIM(3),PMAX(3),KTYP(MAXPAR)
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),               DJG0195
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)           DJG0195
      COMMON /ASOLCM/ GASENG                                            GL0492
      COMMON /GEPCOM/ ISORTS,NDIV1,NDIV2,IPSUMC,IPSUMS                  GDH0992
      COMMON /AREACM/ NOPTI, TAREA,NINTEG                               GDH0793
      COMMON /VOLMCM/ VOLUM,VOLM                                        GDH0793
      COMMON /CYCLCM/ PCMIN, NGEOM, NSOLPR, NSCFS, JPCNT                GDH0893
      COMMON /OPTCOM/ LDER                                              GDH1093
      COMMON /DATECM/ CDAT                                              GDH1093
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /CMCOM/ ECMCG(NUMATM)                                      GDH1293
      COMMON /IVSZCM/ IVSZ(NUMATM), IVSZC                               GDH0194
      COMMON /SOLVCM/ CELEID, SLVRAD, SLVRD2, CSSIGM                    DJG1094
      COMMON /MODLCM/ SMODEL                                            DJG0496
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  GDH0297
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        GDH0297
     2                NCLOSE,NOPEN,NDUMY                                GDH0297
      INCLUDE 'PARAMS.i'                                                DJG1094
C ----------------------------------------------------------------------
C     PERIODIC TABLE OF THE ELEMENTS, A '1' MEANS THAT THE ELEMENT IS
C     ALLOWED AT THE MNDOC LEVEL.
CGROUP1 2     'F' SHELL                 'D' SHELL           3 4 5 6 7 8
C
      DATA LZOK/
     11,                                  0,
     20,0,                      0,1,1,1,0,0,
     30,0,                      0,0,0,0,0,0,
     40,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,
     50,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,
     60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,
     70,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1, 1,1,1,1,1/
C ----------------------------------------------------------------------
C     PERIODIC TABLE OF THE ELEMENTS, A '1' MEANS THAT THE ELEMENT IS
C     ALLOWED AT THE PM3 LEVEL.
CGROUP1 2     'F' SHELL                 'D' SHELL           3 4 5 6 7 8
C
      DATA KZOK/
     11,                                  0,
     21,0,                      0,1,1,1,1,0,
     30,0,                      1,1,1,1,1,0,
     40,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,1,0,
     50,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,1,0,
     60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,
     70,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1, 1,1,1,1,1/
C ----------------------------------------------------------------------
C     PERIODIC TABLE OF THE ELEMENTS, A '1' MEANS THAT THE ELEMENT IS
C     ALLOWED AT THE AM1 LEVEL.
CGROUP1 2     'F' SHELL                 'D' SHELL           3 4 5 6 7 8
C
      DATA JZOK/
     11,                                  0,
     20,0,                      1,1,1,1,1,0,
     31,0,                      1,1,1,1,1,0,
     40,0, 0,0,0,0,0,0,0,0,0,1, 0,1,0,0,1,0,
     50,0, 0,0,0,0,0,0,0,0,0,0, 0,1,0,0,1,0,
     60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,1, 0,0,0,0,0,0,
     70,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1, 1,1,1,1,1/
C ----------------------------------------------------------------------
C     PERIODIC TABLE OF THE ELEMENTS, A '1' MEANS THAT THE ELEMENT IS
C     ALLOWED AT THE MNDO LEVEL.
CGROUP1 2     'F' SHELL                 'D' SHELL           3 4 5 6 7 8
C
      DATA IZOK/
     11,                                  0,
     21,1,                      1,1,1,1,1,0,
     31,0,                      1,1,1,1,1,0,
     41,0, 0,0,0,0,0,0,0,0,0,1, 0,1,0,0,1,0,
     50,0, 0,0,0,0,0,0,0,0,0,0, 0,1,0,0,1,0,
     60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,1, 0,1,0,0,0,0,
     70,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1, 1,1,1,1,1/
C
C 99=DUMMY ATOM, 103-106 ARE SPARKLES
C ----------------------------------------------------------------------
      DATA SPACE, SPACE2/' ','  '/
      DATA BLANK/'                                                      CC5-91
     1                          '/                                      CC5-91
      DATA CTYPE/'any CARBON or attached HYDROGEN                   '   DT3-91
     2          ,'HYDROGEN attached to a nitrogen                   '   DT3-91
     3          ,'HYDROGEN attached to an oxygen                    '   DT3-91
     4          ,'HYDROGEN attached to a sulfur                     '   DT3-91
     5          ,'sp3 or amide NITROGEN                             '   DT3-91
     6          ,'sp2 or sp or aromatic NITROGEN                    '   DT3-91
     7          ,'sp3 OXYGEN                                        '   DT3-91
     8          ,'sp2 OXYGEN                                        '   DT3-91
     9          ,'FLUORINE ATOM                                     '   DT3-91
     t          ,'SULFUR ATOM                                       '   DT3-91
     1          ,'CHLORINE ATOM                                     '   DT3-91
     2          ,'BROMINE ATOM                                      '   DT3-91
     3          ,'IODINE ATOM                                       '   DT3-91
     4       ,87*'                                                  '/  DT3-91
      DATA FIRST /.TRUE./                                               GDH0195
      SAVE
      IF (ICR.EQ.2) THEN                                                GDH1294
         JDAT=JINP                                                      GDH1195
         REWIND(JINP)                                                   GDH1095
      ENDIF                                                             GDH1195
C                                                                       DJG0995
C        READ IN KEYWORDS AND SET KEYWORD FLAGS                         DJG0995
C                                                                       DJG0995
         CALL KEYFLG                                                    DJG0995
      IF (ISTOP.NE.0) RETURN                                            GDH1095
C                                                                       DJG0995
C        CHECK FOR KEYWORD INCOMPATIBILITIES                            DJG0995
C                                                                       DJG0995
         CALL KWNONO                                                    DJG0995
C
C     MAKE KEYWORD CORRECTIONS                                          GDH0696
C
      IF (ITRUES.EQ.1.AND.IHF.NE.1.AND.IHF.NE.2) ITRUES=2               GDH0696
      IF (ITRUES.GE.1.AND.IHFCAL.EQ.0.AND.IHF.LT.1) IHFCAL=1            GDH1/96
      IF (ITRUES.EQ.1.AND.IHF.GE.1) THEN                                GDH1/96
             GASENG=FIHF                                                GDH1195
      ELSE IF (ITRUES.EQ.2.AND.IHF.EQ.0) THEN                           GDH1/96
             ICR=1                                                      GDH1/96
      ELSE IF (ITRUES.EQ.0.AND.IHF.GE.1) THEN                           GDH1/96
             IHF=-2                                                     GDH1/96
      ENDIF                                                             GDH1/96
      IF (IPRGEO.NE.0.AND.ISYMME.NE.0) THEN                             PDW1199
         IPRGEO=0                                                       PDW1199
      ENDIF                                                             PDW1199

C     END OF KEYWORD CORRECTIONS                                        GDH0696
C
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         ASOC=ACOS(-1.D0)/180.D0                                        GDH0195
      VOLM=.FALSE.                                                      GDH0793
      NGEOM=0                                                           GDH0893
      NSOLPR=0                                                          GDH0893
      NSCFS=0                                                           GDH0993
      JPCNT=0                                                           GDH1093
      PCMIN=1000                                                        GDH1093
      TAREA=(IDOTS.EQ.0.AND.IGEPOL.EQ.0)                                DJG0995
C                                                                       DJG0996
C        NOPTI=3 CS3 SCF STRATEGY                                       DJG0996
C        NOPTI=2 CS2 SCF STRATEGY                                       DJG0996
C        NOPTI=1 CS1 SCF STRATEGY                                       DJG0996
C                                                                       DJG0996
      NOPTI=1                                                           DJG0195
      IF (ICS2.NE.0) THEN                                               DJG0995
         NOPTI=2                                                        DJG0195
      ELSE IF (ICS3.NE.0) THEN                                          DJG0995
         NOPTI=3                                                        DJG0195
      ENDIF                                                             DJG0195
      IRAD=-1                                                           GDH1093
C                                                                       DJG0996
C     ICHMOD=2 IF USING CM2, ICHMOD=1 IF USING CM1, 0 IF NOT            GDH0997
C     ICHMOD=3 IF USING CM3                                             !JT0802
C                                                                       DJG0996
      ICHMOD=0                                                          DJG1094
      NUMSLV=-1                                                         DJG1094
C                                                                       DJG0896
C     ICHGM =1 IF A SOLVENT MODEL WITH OUT CM1 IS USED, 2 IF THE SOLVNT DJG0896
C              MODEL USES CM1 CHARGES, 3 IF USING CM2 CHARGES           GDH0997
C                                                                       DJG0896
      ICHGM=0                                                           CCC0895
C                                                                       DJG1094
C     THE VALUE OF IRAD SPECIFIES THE TYPE OF CALCULATION               DJG1094
C                                                                       DJG1094
C     IRAD=-1  GAS PHASE                                                DJG1094
C     IRAD=0   AQUEOUS SM1-AM1                                          DJG1094
C     IRAD=1   AQUEOUS SM1A-AM1                                         DJG1094
C     IRAD=2   AQUEOUS SM2-AM1                                          DJG1094
C     IRAD=3   AQUEOUS SM3-PM3                                          DJG1094
C     IRAD=4   AQUEOUS AM1- or PM3-SM5.4U or AM1-SM5.4A or PM3-SM5.4P   DJG0796
C     IRAD=5   AQUEOUS PD-SM5U PD-SM5A or PD-SM5P or                    DJG0796
C                      SM5/A2PD or SM5/P2PD                             GDH0596
C     IRAD=6   ALKANE  SM4-AM1 OR PM3                                   DJG1094
C     IRAD=7   ORGANIC SM5.4 MODEL (/A OR /P)                           DJG1296
C     IRAD=8   CHLOROFORM,BENZENE, OR TOLUENE SM5.4 MODEL (/A OR /P)    DJG1296
C     IRAD=9   SM5.2R MODEL                                             GDH0897
C     IRAD=10  SM5.42R MODEL                                            GDH1197
C     IRAD=12  AQUEOUS SM2.1-AM1                                        DJG1094
C     IRAD=13  AQUEOUS SM3.1-PM3                                        DJG1094
C     IRAD=20  AQUEOUS SM5.0R                                           GDH0297
C     IRAD=21  ORGANIC SM5.0R                                           GDH0797
C     IRAD=22  AQUEOUS SM2.2-AM1                                        GDH0895
C     IRAD=23  AQUEOUS SM2.3-AM1                                        GDH0296
C     IRAD=33  AQUEOUS SM3.3-PM3                                        GDH0296
C                                                                       DJG1094
      IF (ISM1.GT.0) THEN                                               DJG0995
         NUMSLV=1                                                       DJG0195
         IRAD=0                                                         GDH1093
         ICHGM=1                                                        CCC0895
      ELSE IF (ISM1A.GT.0) THEN                                         DJG0995
         NUMSLV=1                                                       DJG0995
         IRAD=1                                                         DJG0995
         ICHGM=1                                                        DJG0995
      ELSE IF (ISM2.GT.0) THEN                                          DJG0995
         NUMSLV=1                                                       DJG0195
         IRAD=2                                                         GDH1093
         ICHGM=1                                                        CCC0895
      ELSE IF (ISM21.GT.0) THEN                                         DJG0995
         NUMSLV=1                                                       DJG0995
         IRAD=12                                                        DJG0995
         ICHGM=1                                                        DJG0995
      ELSE IF (ISM22.GT.0) THEN                                         DJG0995
         NUMSLV=1                                                       DJG0995
         IRAD=22                                                        DJG0995
         ICHGM=1                                                        DJG0995
      ELSE IF (ISM23.GT.0) THEN                                         DJG0995
         NUMSLV=1                                                       DJG0995
         IRAD=23                                                        DJG0995
         ICHGM=1                                                        DJG0995
      ELSE IF (ISM3.GT.0) THEN                                          DJG0995
         NUMSLV=1                                                       DJG0195
         IRAD=3                                                         GDH1093
         ICHGM=1                                                        CCC0895
      ELSE IF (ISM31.GT.0) THEN                                         DJG0995
         NUMSLV=1                                                       DJG0995
         IRAD=13                                                        DJG0995
         ICHGM=1                                                        DJG0995
      ELSE IF (ISM4.GT.0) THEN                                          DJG0995
         ICHGM=2                                                        CCC0895
         IF (CISOLV.EQ.'N5ANE') THEN                                    DJG1094
           NUMSLV=2                                                     DJG1094
         ELSE IF (CISOLV.EQ.'N6ANE') THEN                               DJG0995
           NUMSLV=3                                                     DJG1094
         ELSE IF (CISOLV.EQ.'N7ANE') THEN                               DJG0995
           NUMSLV=4                                                     DJG1094
         ELSE IF (CISOLV.EQ.'N8ANE') THEN                               DJG0995
           NUMSLV=5                                                     DJG1094
         ELSE IF (CISOLV.EQ.'N9ANE') THEN                               DJG0995
           NUMSLV=6                                                     DJG1094
         ELSE IF (CISOLV.EQ.'N10ANE') THEN                              DJG0995
           NUMSLV=7                                                     DJG1094
         ELSE IF (CISOLV.EQ.'N11ANE') THEN                              DJG0995
           NUMSLV=8                                                     DJG1094
         ELSE IF (CISOLV.EQ.'N12ANE') THEN                              DJG0995
           NUMSLV=9                                                     DJG1094
         ELSE IF (CISOLV.EQ.'N14ANE') THEN                              DJG0995
           NUMSLV=10                                                    DJG1094
         ELSE IF (CISOLV.EQ.'N15ANE') THEN                              DJG0995
           NUMSLV=11                                                    DJG1094
         ELSE IF (CISOLV.EQ.'N16ANE') THEN                              DJG0995
           NUMSLV=12                                                    DJG1094
         ELSE IF (CISOLV.EQ.'2M4ANE') THEN                              DJG0995
           NUMSLV=13                                                    DJG1094
         ELSE IF (CISOLV.EQ.'3M5ANE') THEN                              DJG0995
           NUMSLV=14                                                    DJG1094
         ELSE IF (CISOLV.EQ.'ISOOCT') THEN                              DJG0995
           NUMSLV=15                                                    DJG1094
         ELSE IF (CISOLV.EQ.'CYC6') THEN                                DJG0995
           NUMSLV=16                                                    DJG1094
         ELSE IF (CISOLV.EQ.'MCYC6') THEN                               DJG0995
           NUMSLV=17                                                    DJG1094
         ELSE IF (CISOLV.EQ.'DECLIN') THEN                              DJG0995
           NUMSLV=18                                                    DJG0195
         ELSE IF (CISOLV.EQ.'GENALK'.OR.CISOLV.EQ.'H2OSRP') THEN        DJG0995
           NUMSLV=12                                                    DJG0195
         ELSE                                                           DJG0195
           WRITE(JOUT,5420) CISOLV                                      DJG0995
           ISTOP=1                                                      GDH1095
           IWHERE=51                                                    GDH1095
           RETURN                                                       GDH1095
         ENDIF                                                          DJG1094
         ICHMOD=1                                                       DJG1094
         IRAD=6                                                         DJG0995
      ELSE IF (ISM5U+ISM5A+ISM5P.GT.0) THEN                             DJG0796
         IF(CISOLV(1:5).EQ.'WATER')THEN                                 DJG0896
            NUMSLV=19                                                   CCC1295
            IRAD=4                                                      DJG0896
         ELSE IF (CISOLV(1:5).EQ.'CHCL3') THEN                          DJG0896
            IF(IAM1.NE.0) THEN                                          DJG0896
               NUMSLV=21                                                DJG0896
            ELSE                                                        DJG0896
               NUMSLV=22                                                DJG0896
            ENDIF                                                       DJG0896
            IRAD=8                                                      DJG0896
         ELSE IF (CISOLV(1:7).EQ.'BENZENE') THEN                        DJG0297
            IF(IAM1.NE.0) THEN                                          DJG0297
               NUMSLV=23                                                DJG0297
            ELSE                                                        DJG0297
               NUMSLV=24                                                DJG0297
            ENDIF                                                       DJG0297
            IRAD=8                                                      DJG0297
         ELSE IF (CISOLV(1:7).EQ.'TOLUENE') THEN                        DJG0297
            IF(IAM1.NE.0) THEN                                          DJG0297
               NUMSLV=25                                                DJG0297
            ELSE                                                        DJG0297
               NUMSLV=26                                                DJG0297
            ENDIF                                                       DJG0297
            IRAD=8                                                      DJG0297
C        ELSE IF(CISOLV(1:5).EQ.'ARENE'.OR.CISOLV(1:6).EQ.'GENORG')THEN DJG0297
         ELSE IF(CISOLV(1:6).EQ.'GENORG')THEN                           DJG0297
            IRAD=7                                                      DJG1296
            NUMSLV=20                                                   DJG1296
         ELSE                                                           CCC1295
           WRITE(JOUT,5420) CISOLV                                      CCC1295
           ISTOP=1                                                      CCC1295
           IWHERE=51                                                    CCC1295
           RETURN                                                       CCC1295
         ENDIF                                                          CCC0895
         ICHGM=2                                                        CCC0895
         ICHMOD=1                                                       DJG1094
      ELSE IF (IPDS5U+IPDS5A+IPDS5P.GT.0) THEN                          DJG0796
         ICHGM=2                                                        GDH0496
         IF(CISOLV.EQ.'WATER')THEN                                      GDH0496
            NUMSLV=19                                                   GDH0496
         ELSE                                                           GDH0496
           WRITE(JOUT,5420) CISOLV                                      GDH0496
           ISTOP=1                                                      GDH0496
           IWHERE=51                                                    GDH0496
           RETURN                                                       GDH0496
         ENDIF                                                          GDH0496
         IRAD=5                                                         GDH0496
         ICHMOD=1                                                       DJG1094
      ELSE IF (IS5A2P+IS5P2P.GT.0) THEN                                 GDH0596
         ICHGM=1                                                        GDH0596
         IF(CISOLV.EQ.'WATER')THEN                                      GDH0596
            NUMSLV=19                                                   GDH0596
         ELSE                                                           GDH0596
           WRITE(JOUT,5420) CISOLV                                      GDH0596
           ISTOP=1                                                      GDH0596
           IWHERE=51                                                    GDH0596
           RETURN                                                       GDH0596
         ENDIF                                                          GDH0596
         IRAD=5                                                         GDH0596
         ICHMOD=0                                                       GDH0596
      ELSE IF (ISM50.GT.0) THEN                                         GDH0696
         IF(CISOLV.EQ.'WATER')THEN                                      GDH0596
            NUMSLV=19                                                   GDH0797
            ICHGM=1                                                     GDH0797
            ICHMOD=0                                                    GDH0797
            IRAD=20                                                     GDH0797
         ELSE IF(CISOLV(1:6).EQ.'GENORG') THEN                          GDH0797
            NUMSLV=20                                                   GDH0797
            ICHGM=1                                                     GDH0797
            ICHMOD=0                                                    GDH0797
            IRAD=21                                                     GDH0797
         ELSE                                                           GDH0797
           WRITE(JOUT,5420) CISOLV                                      GDH0596
           ISTOP=1                                                      GDH0596
           IWHERE=51                                                    GDH0596
           RETURN                                                       GDH0596
         ENDIF                                                          GDH0596
      ELSE IF ((ISM52R.GT.0).OR.(ISM52.GT.0)) THEN                      DAL0303
         IF(CISOLV.EQ.'WATER')THEN                                      GDH0997
            NUMSLV=19                                                   GDH0997
            ICHGM=1                                                     GDH0997
            ICHMOD=0                                                    GDH0997
            IRAD=9                                                      GDH0997
         ELSE IF(CISOLV(1:6).EQ.'GENORG') THEN                          GDH0997
            NUMSLV=20                                                   GDH0997
            ICHGM=1                                                     GDH0997
            ICHMOD=0                                                    GDH0997
            IRAD=9                                                      GDH0997
         ELSE                                                           GDH0997
           WRITE(JOUT,5420) CISOLV                                      GDH0997
           ISTOP=1                                                      GDH0997
           IWHERE=51                                                    GDH0997
           RETURN                                                       GDH0997
         ENDIF                                                          GDH0997
      ELSE IF ((ISM54R.GT.0).OR.(ISM542.GT.0)) THEN                     PDW1199
         IF(CISOLV.EQ.'WATER')THEN                                      GDH0997
            NUMSLV=19                                                   GDH0997
            ICHGM=3                                                     GDH0997
            ICHMOD=2                                                    GDH0997
            IRAD=10                                                     GDH0997
         ELSE IF(CISOLV(1:6).EQ.'GENORG') THEN                          GDH0997
            NUMSLV=20                                                   GDH0997
            ICHGM=3                                                     GDH0997
            ICHMOD=2                                                    GDH0997
            IRAD=10                                                     GDH0997
         ELSE                                                           GDH0997
           WRITE(JOUT,5420) CISOLV                                      GDH0997
           ISTOP=1                                                      GDH0997
           IWHERE=51                                                    GDH0997
           RETURN                                                       GDH0997
         ENDIF                                                          GDH0997
      ENDIF                                                             GDH0997
      IF (ICR.EQ.1) THEN                                                DJG0995
         IRAD=-1                                                        DJG0995
         ICHGM=0                                                        DJG0995
      ENDIF                                                             DJG0995
      IF (NUMSLV.NE.-1) THEN                                            DJG0195
         IF (IMSURF.NE.0) THEN                                          DJG0995
           CSSIGM=0.03332D0*FIMSUR+15.95D0                              DJG0995
         ELSE                                                           DJG0195
           CSSIGM=CSSURF(NUMSLV)                                        DJG0195
         ENDIF                                                          DJG0195
         IF (IDIELE.EQ.0) THEN                                          DJG0995
            CELEID=1/DIECON(NUMSLV)                                     DJG0195
         ELSE                                                           DJG0195
            CELEID=1/FIDIEL                                             DJG0995
         ENDIF                                                          DJG0195
         IF (IRAD.EQ.9.OR.IRAD.EQ.20.OR.IRAD.EQ.21)                     GDH1197
     .      THEN                                                        GDH0997
            SLVRAD=0.0D0                                                GDH0297
         ELSE IF (IRAD.EQ.10) THEN                                      GDH1197
            SLVRAD=0.00D0                                               GDH1197
         ELSE IF (ISVCDR.EQ.0) THEN                                     GDH0297
            SLVRAD=SRADCD(NUMSLV)                                       DJG0195
         ELSE                                                           DJG0195
            SLVRAD=FISVCD                                               DJG0995
         ENDIF                                                          DJG0195
         IF (IRAD.EQ.9.OR.IRAD.EQ.20.OR.IRAD.EQ.21)                     GDH1197
     .      THEN                                                        GDH0997
            SLVRD2=0.0D0                                                GDH0797
         ELSE IF (IRAD.EQ.10) THEN                                      GDH1197
            SLVRD2=0.00D0                                               GDH1197
         ELSE IF (ISVCSR.EQ.0) THEN                                     DJG0995
            SLVRD2=SRADCS(NUMSLV)                                       DJG0195
         ELSE                                                           DJG0195
            SLVRD2=FISVCS                                               DJG0995
         ENDIF                                                          DJG0195
      ENDIF                                                             DJG0195
      IF (IRAD.GE.0) THEN                                               GDH1093
C                                                                       DJG0996
C        NINTEG=3 Gauss-Legendre quadrature                             GDH1197
C        NINTEG=2 PAIRWISE DESCREENING                                  DJG0996
C        NINTEG=1 TRAPEZOIDAL QUADRATURE                                DJG0996
C        NINTEG=0 RECTANGULAR RULE                                      DJG0996
C                                                                       DJG0996
         IF(IRAD1.EQ.0) THEN                                            DJG0995
            ICHK2=ISM22+ISM23+IPDS5U+IPDS5A+IPDS5P+IS5P2P+IS5A2P        DJG0796
C           ICHK3=ISM54R+ISM542R+ISM52R+ISM52+ISM5A+ISM5P+ISM5U         DAL0303
C ---- As far as I can tell, ISM542R does not exist                     !JT0303
            ICHK3=ISM54R+ISM52R+ISM52+ISM5A+ISM5P+ISM5U                 !JT0303
            IF (ICHK3.GT.0) THEN                                        GDH1197
               NINTEG=3                                                 GDH1197
            ELSE IF (ICHK2.GT.0) THEN                                   GDH1197
               NINTEG=2                                                 DJG0995
            ELSE IF (IRAD.GE.4) THEN                                    GDH0895
               NINTEG=1                                                 DJG0995
            ELSE                                                        GDH1193
               NINTEG=0                                                 DJG0995
            ENDIF                                                       GDH1093
         ELSE
            NINTEG=IIRAD                                                DJG0995
         ENDIF
      ENDIF                                                             GDH1093
      MINDO=IMINDO.NE.0                                                 DJG0995
      AM1=IAM1.NE.0                                                     DJG0995
      PM3=IPM3.NE.0                                                     DJG0995
      MNDOC=IMNDOC.NE.0                                                 DJG0995
      MNDO =.NOT.(AM1.OR.MINDO.OR.PM3)
      IF (MNDO) THEN
         IF (MNDOC) THEN
            MODEL='MNDOC  '
         ELSE
            MODEL='MNDO   '
         ENDIF
      ELSE IF (AM1) THEN
         MODEL='    AM1'
      ELSE IF (PM3) THEN
         MODEL='    PM3'
      ELSE IF (MINDO) THEN
         MODEL='MINDO/3'
      ENDIF
      IF (IDOTS.NE.0) THEN                                              DJG0995
         NDIV1=45                                                       GDH0992
         IPSUMC=816                                                     GDH0992
         IPSUMS=2610                                                    GDH1192
         IF(INDOTC.NE.0) THEN                                           DJG0995
            NDIV1=IIDOTC                                                DJG0995
            IF (NDIV1.LT.2.OR.NDIV1.GT.MXPT.OR.NDIV1.GT.28761) THEN     GDH1192
              WRITE(JOUT,5080)                                          GDH1192
      ISTOP=1                                                           GDH1095
      IWHERE=52                                                         GDH1095
      RETURN                                                            GDH1095
            ENDIF                                                       GDH1192
            CALL ARCCH(NDIV1,NARCVL,IPSUM)                              GDH1192
            IPSUMS=IPSUM                                                GDH1192
            NDIV1=NARCVL                                                GDH1192
         ENDIF                                                          GDH0992
         NDIV2=25                                                       GDH0992
         IF(INDOTE.NE.0) THEN                                           DJG0995
            NDIV2=IIDOTE                                                DJG0995
            IF (NDIV2.LT.2.OR.NDIV2.GT.MXPT.OR.NDIV2.GT.28761) THEN     GDH1192
              WRITE(JOUT,5080)                                          GDH1192
      ISTOP=1                                                           GDH1095
      IWHERE=53                                                         GDH1095
      RETURN                                                            GDH1095
            ENDIF                                                       GDH1192
            CALL ARCCH(NDIV2,NARCVL,IPSUM)                              GDH1192
            IPSUMC=IPSUM                                                GDH1192
            NDIV2=NARCVL                                                GDH1192
         ENDIF                                                          GDH0992
C  OPTION REMOVED BY GDH0397 (GEPOL NO LONGER SUPPORTED)
C      ELSE IF (IGEPOL.NE.0) THEN                                        DJG0995
C         NDIV1=3                                                        GDH0992
C         IPSUMS=960                                                     GDH0193
C         IF(INDIVC.NE.0) THEN                                           DJG0995
C            NDIV1=IIDIVC                                                DJG0995
C            IPSUMS=(60*(4**(NDIV1-1)))                                  GDH0193
C         ENDIF                                                          GDH0193
C         IF ((NDIV1.LT.1).OR.(NDIV1.GT.5).OR.(IPSUMS.GT.MXPT)) THEN     GDH0992
C            WRITE(JOUT,5070)                                            GDH0992
C      ISTOP=1                                                           GDH1095
C      IWHERE=54                                                         GDH1095
C      RETURN                                                            GDH1095
C         ENDIF                                                          GDH0992
C         NDIV2=3                                                        GDH0992
C         IPSUMC=960                                                     GDH0193
C         IF(INDIVE.NE.0) THEN                                           DJG0995
C            NDIV2=IIDIVE                                                DJG0995
C            IPSUMC=(60*(4**(NDIV2-1)))                                  GDH0193
C         ENDIF                                                          GDH0193
C         IF ((NDIV2.LT.1).OR.(NDIV2.GT.5).OR.(IPSUMC.GT.MXPT)) THEN     GDH0992
C            WRITE(JOUT,5070)                                            GDH0992
C      ISTOP=1                                                           GDH1095
C      IWHERE=55                                                         GDH1095
C      RETURN                                                            GDH1095
C         ENDIF                                                          GDH0992
      ENDIF                                                             GDH0992
      ISORTS=0                                                          GDH0992
5070  FORMAT(/,'WARNING: "NDIVCD" AND "NDIVEP" MUST HAVE A VALUE ',     GDH0992
     1         'BETWEEN 1 AND 5.  ALSO               THE NUMBER OF',    GDH0593
     2         'POINTS MUST BE LESS THAN MXPT.')                        GDH0992
5080  FORMAT(/,'WARNING: "NDOTCD" AND "NDOTEP" MUST HAVE A VALUE ',     GDH1192
     1         'GREATER THAN 2 AND LESS THAN      "MXPT" WHICH IS ',    GDH0493
     2         'SET IN THE SIZES2.i INCLUDE FILE AND CANNOT EXCEED ',   GDH0493
     3         '28761')                                                 GDH1192
5420  FORMAT(/,'WARNING: SOLVENT NAME UNRECOGNIZED FOR THIS MODEL "',   DJG0896
     1          a6,'".')                                                DJG0896
      IF (ICR.LE.1) THEN                                                GDH1194
          CALL GETGEO(JDAT,LABELS,GEO,LOPT,NA,NB,NC,AMS,NATOMS,INTR)    GDH1095
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      ELSE                                                              GDH1095
          CALL GETGEO(JINP,LABELS,GEO,LOPT,NA,NB,NC,AMS,NATOMS,INTR)    GDH1095
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      ENDIF                                                             GDH1294
      CALL TPCHK(NATOMS,LABELS)                                         GDH0695
C
C OUTPUT FILE TO UNIT JOUT
C    WRITE HEADER
C
       CALL DATESV(JOUT)                                                GL0492
       WRITE (JOUT,'(A80)') CDAT                                        GDH1093
C  This section was added to correct for some non-standard default options
C  which occur if a given set of keywords is chosen.
       IF (ICR.EQ.1) THEN                                               GDH1194
          IF (ISM1.GT.0) THEN                                           DJG0995
             ISM1=-1                                                    DJG0995
          ELSE IF (ISM2.GT.0) THEN                                      DJG0995
             ISM2=-1                                                    DJG0995
          ELSE IF (ISM3.GT.0) THEN                                      DJG0995
             ISM3=-1                                                    DJG0995
          ELSE IF (ISM4.GT.0) THEN                                      DJG0995
             ISM4=-1                                                    DJG0995
          ELSE IF (ISM5U.GT.0) THEN                                     DJG0796
             ISM5U=-1                                                   DJG0796
          ELSE IF (ISM5A.GT.0) THEN                                     CCC0496
             ISM5A=-1                                                   CCC0496
          ELSE IF (ISM5P.GT.0) THEN                                     CCC0496
             ISM5P=-1                                                   CCC0496
          ELSE IF (IS5A2P.GT.0) THEN                                    GDH0596
             IS5A2P=-1                                                  GDH0596
          ELSE IF (IS5P2P.GT.0) THEN                                    GDH0596
             IS5P2P=-1                                                  GDH0596
          ELSE IF (ISM21.GT.0) THEN                                     DJG0995
             ISM21=-1                                                   DJG0995
          ELSE IF (ISM31.GT.0) THEN                                     DJG0995
             ISM31=-1                                                   DJG0995
          ELSE IF (ISM22.GT.0) THEN                                     DJG0995
             ISM22=-1                                                   DJG0995
          ELSE IF (ISM23.GT.0) THEN                                     GDH0296
             ISM23=-1                                                   DJG0995
          ELSE IF (IPDS5U.GT.0) THEN                                    DJG0796
             IPDS5U=-1                                                  DJG0796
          ELSE IF (IPDS5A.GT.0) THEN                                    GDH0496
             IPDS5A=-1                                                  GDH0496
          ELSE IF (IPDS5P.GT.0) THEN                                    GDH0496
             IPDS5P=-1                                                  GDH0496
          ELSE IF (ISM1A.GT.0) THEN                                     DJG0995
             ISM1A=-1                                                   DJG0995
          ELSE IF (ISM54R.GT.0) THEN                                    GDH0997
             ISM54R=-1                                                  GDH0997
          ELSE IF (ISM542.GT.0) THEN                                    PDW1199
             ISM542=-1                                                  PDW1199
          ELSE IF (ISM52R.GT.0) THEN                                    GDH0997
             ISM52R=-1                                                  GDH0997
          ELSE IF (ISM52.GT.0) THEN                                     PDW1199
             ISM52=-1                                                   PDW1199
          ENDIF                                                         DJG0995
          IF (I1SCF.GT.0) THEN                                          GDH1/96
             IF (IHFCAL.EQ.1.OR.IHFCAL.EQ.2) THEN                       GDH1/96
                IOPT=1                                                  GDH1/96
                I1SCF=0                                                 GDH1/96
             ENDIF                                                      GDH1/96
          ELSE IF (IHFCAL.EQ.3) THEN                                    GDH1/96
                 IOPT=0                                                 GDH1/96
                 I1SCF=1                                                GDH1/96
          ENDIF                                                         GDH1/96
          IRAD=-1                                                       GDH1194
          WRITE(JOUT,'(/,A,A,/)')'This first section contains ',        GDH1194
     .    'only gas phase information'                                  GDH1194
       ELSE IF (ICR.EQ.2) THEN                                          GDH1194
          WRITE(JOUT,'(/,A,A,/)')'The following section contains the',  PDW1099
     .    ' solvation information'                                      PDW1099
       ENDIF                                                            GDH1194
C      SMODEL IS TRUE IF A SOLVATION MODEL IS TURNED ON                 DJG0496
       SMODEL=(ISM1+ISM2+ISM3+ISM4+ISM5U+ISM5A+ISM5P+ISM21+ISM31+ISM22+ DJG1296
     1         ISM23+IPDS5U+IPDS5A+IPDS5P+IS5A2P+IS5P2P+ISM1A+ISM50     DJG1296
     2         +ISM52R+ISM54R+ISM52+ISM542.GT.0)                        PDW1199
       LDER=((SMODEL.AND.(I1SCF.EQ.0.AND.INOPOL.EQ.0.AND.ISM50.EQ.0))   GDH0297
     .        .OR.IDERIN.NE.0)                                          GDH0297
       XKWFLG=.TRUE.                                                    DJG0995
       NTPRT=0                                                          GDH1195
C
C       KEYWORD INPUT ECHO
C
C       CALL ECHOWD(JOUT,NTPRT,XKWFLG)                                   GDH1195
C
       CALL HEADER(JOUT)                                                GL0492
       WRITE(JOUT,'(/,1X,79(''*''))')                                   GL0492
C
C CHECK DATA
C
      DO 40 I=1,NATOMS
      IF (IEXTER.EQ.0) THEN                                             DJG0995
         IF ((KZOK(LABELS(I)).EQ.0.AND.PM3).OR.
     .       (JZOK(LABELS(I)).EQ.0.AND.AM1).OR.
     .       (IZOK(LABELS(I)).EQ.0.AND.MNDO.AND.(.NOT.MNDOC)).OR.
     .       (LZOK(LABELS(I)).EQ.0.AND.MNDOC)) THEN
            WRITE(JOUT,'('' ATOMIC NUMBER '',I3,
     .      '' IS NOT AVAILABLE IN '',A7)') LABELS(I),MODEL
            ISTOP=1                                                     GDH1095
            IWHERE=56                                                   GDH1095
            RETURN                                                      GDH1095
         ENDIF
      ENDIF
      IF (LABELS(I).LE.0) THEN
         WRITE(JOUT,'('' ATOMIC NUMBER OF '',I3,'' ?'')') LABELS(I)
         ISTOP=1                                                        GDH1095
         IWHERE=57                                                      GDH1095
         RETURN                                                         GDH1095
      ENDIF
      IF (NA(I).GE.I.OR.NB(I).GE.I.OR.NC(I).GE.I
     1.OR.(NA(I).EQ.NB(I)).AND.I.GT.1
     2.OR.(NA(I).EQ.NC(I).OR.NB(I).EQ.NC(I)).AND.I.GT.2) THEN
            WRITE(JOUT,'('' ATOM NUMBER '',I3,'' IS ILL DEFINED'')') I  GL0492
      ISTOP=1                                                           GDH1095
      IWHERE=58                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF
   40 CONTINUE
      IF(ICM1.NE.0) THEN                                                DJG0995
        ICHMOD=1                                                        DJG0195
      ELSEIF (ICM2.NE.0) THEN                                           GDH0997
        ICHMOD=2                                                        GDH0997
      ELSEIF (ICM3.NE.0) THEN                                           !JT0802
        ICHMOD=3                                                        !JT0802
      ENDIF                                                             DJG0794
C
C WRITE KEYWORDS BACK TO USER AS FEEDBACK
C
      CALL WRTKEY                                                       DJG0995
5002  FORMAT(/,1X,'True solvation energy will be calculated based on:', GL0492
     1       /,10X,'gas-phase heat of formation =',F13.6,' kcal')       GL0492
      IF (.NOT.TAREA) WRITE(JOUT,5003) IPSUMC,IPSUMS                    GDH0493
5003  FORMAT(/,1X,'Number of points on each sphere for ',               GDH0493
     1            'coulombic surface area calculation  ',
     2            I6,/,1X,'Number of points on each sphere for ',
     3            'surface tension calculation         ',I6)
      IF (IEXTSM.NE.0) THEN                                             DJG0995
         REWIND JXSM                                                    GDH1293
         IF (IRAD.EQ.6) THEN                                            DJG1094
           WRITE(JOUT,5007)                                             GDH1293
         ELSEIF (IRAD.EQ.4.OR.IRAD.EQ.5.OR.IRAD.EQ.7.OR.IRAD.EQ.8) THEN DJG0896
           WRITE(JOUT,5008)                                             CCC1295
         ELSE                                                           DJG1094
           WRITE(JOUT,5004)                                             DJG1094
         ENDIF                                                          DJG1094
5006     READ(JXSM,'(A)',END=5005) LINE                                 GDH1293
         WRITE(JOUT,'(A)') LINE                                         GDH1293
         GOTO 5006                                                      GDH1293
      ENDIF                                                             GDH1293
5004  FORMAT(/,1X,'The EXTSM keyword has been entered.  The parameters' GDH1293
     1   ,' altered are echoed in the ',/,' following order.',/,        GDH1293
     2   '  Element, R(k), Sigma(0), Sigma(1), Rho(0), Rho(1), '        DJG1094
     3   ,'q(0), and q(1).',/)                                          GDH0194
5007  FORMAT(/,1X,'The EXTSM keyword has been entered.  The by-element',DJG0195
     1   ' parameters altered are',/,' echoed in the following order.'  DJG1094
     2   ,/,'  Element, R(k), Sigma(CD), HSigma(CD), Rho(0), Rho(1), '  DJG1094
     3   ,'q(0), and q(1).',/,'  Where HSigma(CD) is Sigma(CD) for ',   DJG1094
     4   'hydrogen attached to the',/,'  element in question.',         DJG1094
     5   '  Special surface tensions such as the CS and CC',/,          DJG1094
     6   '  (Carbon-Carbon) surface tensions are echoed as ',/,         DJG1094
     7   '  CS x.xx (or CC x.xx).',/)                                   DJG1094
5008  FORMAT(/,1X,'The EXTSM keyword has been entered.  The by-element',CCC1295
     1   ' parameters altered are',/,' echoed in the following order.'  CCC1295
     2   ,/,'  Element, R(k), Sigma, HSigma, Rho(0), and Rho(1), '      CCC1295
     2   ,/,'  followed by two "blank parameters" which are not'        CCC1295
     2   ,/,'  used in the SM5-like models. HSigma is Sigma for'        CCC0496
     3   ,/,'  hydrogen attached to the "Element." Special surface'     CCC1295
     5   ,/,'  tensions such as the OC, ON, OO, SS, and NC are echoed'  CCC1295
     7   ,/,'  as OC x.xx. The CC (carbon-carbon) surface tensions are' CCC1295
     8   ,/,'  echoed as CC x.xx y.yy.',/)                              CCC1295
5005  CONTINUE
C CONVERT ANGLES TO RADIANS
      DO 50 I=1,NATOMS
      DO 50 J=2,3
   50 GEO(J,I)=GEO(J,I)*ASOC                                            GDH0195
C
C FILL IN GEO MATRIX IF NEEDED
      NDEP=0
      IF(ISYMME.NE.0) CALL GETSYM                                       DJG0995
      IF(NDEP.NE.0) CALL SYMTRY
      IF (ISTOP.NE.0) RETURN                                            GDH1095
C
C INITIALIZE FLAGS FOR OPTIMIZE AND PATH
      IFLAG = 0
      NVAR  = 0
      LATOM = 0
      NUMAT=0
      IMP0=IIPRIN                                                       DJG0995
      DO 60 I=1,NATOMS
      IF(LABELS(I).NE.99.AND.LABELS(I).NE.107)NUMAT=NUMAT+1
      DO 60 J=1,3
      IF (LOPT(J,I).LT.0) THEN
C        FLAG FOR PATH
         CONVRT=1.D0
         IF ( IFLAG .NE. 0 ) THEN
            IF (ISTEP1.NE.0) THEN                                       DJG0995
               LPARA1=LPARAM
               LATOM1=LATOM
               LPARA2=J
               LATOM2=I
               LATOM=0
               IFLAG=0
               GOTO 60
            ELSE
               WRITE(JOUT,'('' ONLY ONE REACTION COORD. ALLOWED'')')
               ISTOP=1                                                  GDH1095
               IWHERE=59                                                GDH1095
               RETURN                                                   GDH1095
            ENDIF
         ENDIF
         LATOM  = I
         LPARAM = J
         IF(J.GT.1) CONVRT=ASOC                                         GDH0195
         REACT(1)=GEO(J,I)
         IREACT=1
         IFLAG =1
      ELSE IF (LOPT(J,I).GT.0) THEN
C        FLAG FOR OPTIMIZE
         NVAR = NVAR+1
         LOC(1,NVAR)=I
         LOC(2,NVAR)=J
         XPARAM(NVAR)=GEO(J,I)
         KTYP(NVAR)=J
      ENDIF
   60 CONTINUE
      IF (IFLAG.NE.0) THEN
C        READ IN PATH VALUES
   70    READ(JDAT,'(A)',END=90) LINE                                   DJG0496
         IF (LINE.EQ.BLANK) GO TO  90                                   CC1191
         CALL NUCHAR(LINE,VALUE,NREACT)
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         IF (NREACT.EQ.0) GOTO 90
         DO 80 I=1,NREACT
         IJ=IREACT+I
         IF (IJ.GT.100) THEN
            WRITE(JOUT,'('' * * ONLY ONE HUNDRED POINTS ALLOWED''
     .      ,'' IN REACTION COORDINATE * *'')')
            ISTOP=1                                                     GDH1095
            IWHERE=60                                                   GDH1095
            RETURN                                                      GDH1095
         ENDIF
   80    REACT(IJ)=VALUE(I)*CONVRT
         IREACT=IREACT+NREACT
         GOTO 70
   90    DEGREE=1.D0
         IF(LPARAM.GT.1)DEGREE=1.D0/ASOC                                GDH0195
         IF (IREACT.LE.1) THEN
            WRITE(JOUT,'('' NO POINT SUPPLIED FOR REACTION PATH'')')
            WRITE(JOUT,'('' GEOMETRY AS READ IN IS AS FOLLOWS'')')
            CALL GEOUT
            IF (ICART.NE.1) THEN
            WRITE (JOUT, 91)
            ENDIF
   91 FORMAT (/,1X, 'Note: An asterisk (*) indicates this parameter ',  PDW1099
     .        'was or will be optimized.')                              PDW1099

            ISTOP=1                                                     GDH1095
            IWHERE=61                                                   GDH1095
            RETURN                                                      GDH1095
         ELSE
            WRITE(JOUT,'(/,10X,'' Points on reaction coordinate'')')    GL0592
            WRITE(JOUT,'(10X,8F8.2)')(REACT(I)*DEGREE,I=1,IREACT)
         ENDIF
         IEND=IREACT+1
         REACT(IEND)=-1.D12
      ENDIF
C
C       READ IN ATOM/CHEMICAL ENVIRONMENT TYPE SURFACE CORRECTION TERM
C       IDENTIFIERS
C
 92   IF(ISM1A.EQ.1) THEN                                               GDH0696
      DO 152 IENV=1,NUMAT                                               CC1290
152   READ(JDAT,'(I3)')ITYPE(IENV)                                      DJG0496
      WRITE(JOUT,'(/,''Environment specific atom types for aqueous '',  GL0393
     1               ''solvation energy'',/)')                          GL0393
      DO 156 I=1,NUMAT                                                  CC1290
156   WRITE(JOUT,158)I,CTYPE(ITYPE(I))                                  GL0492
      ENDIF                                                             GDH0696
158   FORMAT(' Atom #',I3,' assigned as ',A50)                          GL0492
C
C OUTPUT GEOMETRY AS FEEDBACK
C
160   CONTINUE                                                          GL0492
      IF (ICR.EQ.0.OR.ICR.EQ.1.OR.IPRGEO.EQ.1.OR.I1SCF.EQ.0) THEN
        CALL GEOUT
         IF (ICART.NE.1) THEN
           WRITE (JOUT, 170)
         ENDIF

170   FORMAT (/,1X, 'Note: An asterisk (*) indicates this parameter ',  PDW1099
     .        'was or will be optimized.')                              PDW1099

        CALL GMETRY(GEO,COORD)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (INOXYZ.EQ.0) THEN                                             DJG0995
         IF (IEXTCM.NE.0) THEN                                          DJG0995
         WRITE(JOUT,'(/10X,''Cartesian coordinates (angstroms) and ''   GDH1293
     .         ,''EXTCM atomic charges'')')                             GDH0794
         WRITE(JOUT,'(4X,''NO.'',7X,''Atom'',9X,''X'',
     .   9X,''Y'',9X,''Z'',9X,''Q'')')
         L=0
         DO 100 I=1,NATOMS
         IF (LABELS(I).NE.99.AND.LABELS(I).NE.107) THEN
            L=L+1
            WRITE(JOUT,'(I6,7X,I3,4X,3F10.5,F10.5)')                    GDH0794
     .      L,LABELS(I),(COORD(J,L),J=1,3),ECMCG(I)                     GDH0794
         ENDIF
  100    CONTINUE
         ELSE
         WRITE(JOUT,'(/10X,''Cartesian coordinates (angstroms)'')')     GDH1293
         WRITE(JOUT,'(4X,''NO.'',7X,''Atom'',9X,''X'',
     .   9X,''Y'',9X,''Z'')')
         L=0
         DO 102 I=1,NATOMS
         IF (LABELS(I).NE.99.AND.LABELS(I).NE.107) THEN
            L=L+1
            WRITE(JOUT,'(I6,7X,I3,4X,3F10.4,F10.4)')
     .      L,LABELS(I),(COORD(J,L),J=1,3)                              GDH1293
         ENDIF
  102    CONTINUE
         ENDIF                                                          GDH1293
      ENDIF
      ENDIF
      IF (IXYZ.NE.0) THEN                                               DJG0995
         IF (INTR.AND.NDEP.NE.0) THEN
            WRITE(JOUT,'(
     .      '' INTERNAL COORDINATES READ IN BUT CALCULATION'',
     .      '' TO BE RUN IN CARTESIAN COORDINATES.''/
     .      '' SYMMETRY NOT ALLOWED IN THIS CASE,''/
     .      '' SO THE CALCULATION IS TERMINATED AT THIS POINT.'')')
      ISTOP=1                                                           GDH1095
      IWHERE=62                                                         GDH1095
      RETURN                                                            GDH1095
         ENDIF
         SUMX=0.D0
         SUMY=0.D0
         SUMZ=0.D0
         DO 110 J=1,NUMAT
         SUMX=SUMX+COORD(1,J)
         SUMY=SUMY+COORD(2,J)
  110    SUMZ=SUMZ+COORD(3,J)
         SUMX=SUMX/NUMAT
         SUMY=SUMY/NUMAT
         SUMZ=SUMZ/NUMAT
         DO 120 J=1,NUMAT
         GEO(1,J)=COORD(1,J)-SUMX
         GEO(2,J)=COORD(2,J)-SUMY
  120    GEO(3,J)=COORD(3,J)-SUMZ
         NA(1)=99
         K=1
         NVAR1=NVAR
         NUMAT=0
         NVAR=0
         DO 140 I=1,NATOMS
         IF (LABELS(I).NE.99) THEN
            NUMAT=NUMAT+1
            LABELS(NUMAT)=LABELS(I)
            NVAR0=K
            DO 130 J=NVAR0,NVAR1
            IF (LOC(1,J).EQ.I) THEN
               NVAR=NVAR+1
               LOC(1,NVAR)=NUMAT
               LOC(2,NVAR)=LOC(2,J)
               XPARAM(NVAR)=GEO(LOC(2,NVAR),NUMAT)
               KTYP(NVAR)=1
               K=J
            ENDIF
  130       CONTINUE
         ENDIF
  140    CONTINUE
         NATOMS=NUMAT
         IF (NVAR.LT.3*NUMAT-6) WRITE(JOUT,'('' WARNING: ONLY'',I4,
     .   '' OPTIMIZED CARTESIAN COORDINATES WITH RESPECT TO''
     .   /14X,I4,'' DEGREES OF FREEDOM.'')')NVAR,3*NUMAT-6
      ELSE
         IF (.NOT.INTR.AND.(NDEP.NE.0.OR.NVAR.LT.3*NUMAT-6)) THEN
            WRITE(JOUT,'('' CARTESIAN COORDINATES READ IN BUT'',
     .      '' CALCULATION TO BE RUN IN INTERNAL COORDINATES.'')')
            IF(NDEP.NE.0) WRITE(JOUT,'('' SYMMETRY NOT ALLOWED.'')')
            IF(NVAR.LT.3*NUMAT-6) WRITE(JOUT,'('' NOT ALL '',
     .      ''COORDINATES MARKED FOR OPTIMISATION.'')')
      ISTOP=1                                                           GDH1095
      IWHERE=63                                                         GDH1095
      RETURN                                                            GDH1095
         ENDIF
      ENDIF
230   RETURN                                                            CC1290
      END
