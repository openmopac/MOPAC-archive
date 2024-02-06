      SUBROUTINE WRITES(TIME0,FUNCT)                                    GL0492
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION MECI                                             3GL2292
       INCLUDE 'SIZES.i'
       INCLUDE 'SIZES2.i'
      INCLUDE 'KEYS.i'                                                  DJG0995
      INCLUDE 'FFILES.i'                                                GDH1095
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /REPATH/ REACT(100),LATOM,LPARAM                           03GCL93
      COMMON /HBORDS/ HBORD(NUMATM),HB(NUMATM)                           CC8-91
      COMMON /HMATRX/ H(MPACK)
      COMMON /FOKMAT/ F(MPACK),FB(MPACK),FO(MPACK),FBO(MPACK)           JWS1094
      COMMON /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /GEOSYM/ NDEP, LOCPAR(MAXPAR), IDEPFN(MAXPAR)
     1               ,LOCDEP(MAXPAR)
      COMMON /NUMSCF/ NSCF,FROZEN                                       IR1294
      COMMON /CORE  / CORE(107)
      COMMON /CYCLCM/ PCMIN, NGEOM, NSOLPR, NSCFS, JPCNT                GDH0893
C     COMMON /SCRACH/ RXYZ(MPACK), XDUMY(MAXPAR**2-MPACK)
C
C     /SCRACH/ HAS BEEN CONVERTED TO /SCRCHR/ AND /SCRCHI/ FOR AMSOL
      COMMON /SCRCHR/ RXYZ(MPACK), XDUMY(6*MAXPAR**2+1-MPACK)           GCL0393
C
      COMMON /MESAGE/ IFLEP, ISCF                                       DJG0995
      COMMON /GRADNT/ GRAD(MAXPAR), GNORM
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
C     COMMON /GEOVAR/ NVAR, LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
      COMMON /GEOVAR/ XPARAM(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY           3GL3092
      COMMON /OPTIMI / IMP,IMP0                                         GDH1095
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),
     1                QEFF2(NUMATM),DRVPOL(MPACK)                       DJG1094
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),               DJG0195
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)           DJG0195
      COMMON /ASOLCM/ GASENG                                            GL0492
      COMMON /AREACM/ NOPTI, TAREA, NINTEG                              GDH0793
      COMMON /VOLMCM/ VOLUM,VOLM                                        GDH0793
      COMMON /DATECM/ CDAT                                              GDH1093
      COMMON /SOLVCM/ CELEID, SLVRAD, SLVRD2, CSSIGM                    DJG1094
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      COMMON /CONVCM/ JCONV                                             GDH1195
      COMMON /MODLCM/ SMODEL                                            DJG0496
      COMMON /QESTR/  SCOORD(3,NUMATM), ISPEC                           GDH0197
************************************************************************
*
*   WRITE PRINTS OUT MOST OF THE RESULTS.
*         IT SHOULD NOT ALTER ANY PARAMETERS, SO THAT IT CAN BE CALLED
*         AT ANY CONVENIENT TIME.
*
************************************************************************
      DIMENSION Q(NUMATM), COORD(3,NUMATM), GCOORD(MAXPAR)              DJG0995
      DIMENSION EVEC(3,3), BUFFER(NPACK)                                DL0397
      DIMENSION RAV(NUMATM)                                             GDH0793
      DIMENSION FGBHLD(NPACK),FGBHLD2(NPACK)                            GDH0497
      LOGICAL UHF, CI, PRTGRA, AQCHK, VOLM                              DJG0995
      LOGICAL AQNOP, WATSRP, AFAIL, XKWFLG, TAREA, FROZEN, SMODEL       DJG0496
      CHARACTER TYPE(3)*11, CALCN(2)*5, GTYPE*13,                       DJG0995
     1          FLEPO(13)*58, ITER(2)*58                                DJG0995
      CHARACTER*2 ELEMNT                                                DJG0995
      CHARACTER*10 STYPE                                                GDH0496
      CHARACTER*80 KOMENT,TITLE,CDAT                                    DJG0995
      INCLUDE 'PARAMS.i'                                                DJG1094
      SAVE
      DATA TYPE/'Bond       ','Angle      ','Dihedral   '/
      DATA CALCN /'     ','Alpha'/
      DATA FLEPO/
     1' 1SCF WAS SPECIFIED, SO GEOMETRY OPTIMIZATION WAS NOT USED',
     2' GRADIENTS WERE INITIALLY ACCEPTABLY SMALL                ',
     3' GRADIENT COMPONENT TEST PASSED : GEOMETRY CONVERGED      ',     DJG0495
     4' THE LINE MINIMISATION FAILED TWICE IN A ROW.  [TAKE CARE]',
     5' OPTIMIZATION FAILED DUE TO COUNTS EXCEEDED. [TAKE CARE]  ',
     6' THIS MESSAGE SHOULD NEVER APPEAR, [CONSULT A PROGRAMMER] ',     DJG0495
     7' THIS MESSAGE SHOULD NEVER APPEAR, [CONSULT A PROGRAMMER] ',
     8' THIS MESSAGE SHOULD NEVER APPEAR, [CONSULT A PROGRAMMER] ',     DJG0495
     9' A FAILURE HAS OCCURRED, TREAT RESULTS WITH CAUTION!!     ',
     O' GEOMETRY OPTIMISED : ENERGY MINIMISED                    ',
     1' GEOMETRY OPTIMISED : GRADIENT NORM MINIMISED             ',
     2' TIME OR CYCLES EXCEEDED, OPTIMIZATION NOT COMPLETED      ',
     3'                                                          '/
      DATA ITER/
     1' SCF FIELD WAS ACHIEVED                                   ',
     2'  ++++----**** FAILED TO ACHIEVE SCF. ****----++++        '/
C
C
C
C THIS PORTION CALCULATES THE MOLECULAR VOLUME USING THE BONDI RADII
C IT WAS REMOVED BY GDH0397 (GEPOL CAN NOT BE INCLUDED IN AMSOL POST VERSION
C AMSOL 6.0.
C
C       IF (IVOLUM.NE.0) THEN                                            DJG0995
C         DO 977 I=1,NUMAT
C977        RAV(I)=W1(NAT(I))
C         CALL GMETRY(GEO,COORD)
C      IF (ISTOP.NE.0) RETURN                                            GDH1095
C         CALL GEPOL(NUMAT,RAV,COORD)
C         VOLM=.TRUE.
C       ENDIF
C
C
C SUMMARY OF RESULTS (NOTE: THIS IS IN A SUBROUTINE SO IT
C          CAN BE USED BY THE PATH OPTION)
      IF(IFLEP.LE.0.OR.IFLEP.GT.13) IFLEP=7                             DJG0995
      ISUHF=MIN(IUHF,1)+1                                               DJG0995
      PRTGRA=(IGRADI.NE.0)                                              DJG0995
      AQCHK=SMODEL                                                      DJG0495
      AQNOP=AQCHK.AND.(INOPOL.NE.0.OR.ISM505.NE.0)                      GDH0297
      WATSRP=CISOLV.EQ.'H2OSRP'                                         DJG0995
      AFAIL=IFLEP.EQ.4.OR.IFLEP.EQ.5.OR.IFLEP.EQ.9.OR.IFLEP.EQ.12       DJG0995
      LINEAR=(NORBS*(NORBS+1))/2
      CI=(ICI.NE.0)                                                     DJG0995
      ENGAS=FUNCT                                                       GDH1194
      IF (ISM50.EQ.1.AND.ISM505.EQ.0) THEN                              GDH0197
         CALL DBRNPL(COORD,GEO)                                         GDH0197
         CALL SMX1(COORD,1,.FALSE.)                                     GDH0197
         CALL SRFCAL                                                    GDH0197
         CALL CDSCAL                                                    GDH0197
       ELSE IF(AQNOP) THEN                                              GDH0197
C GDH0597        IF (NOPTI.LE.2) THEN
           CALL BORNPL(1,.TRUE.,.TRUE.,.FALSE.)                         GDH0597
C GDH0597        ELSE
C GDH0597           CALL BRNPL2(1,0,1,.TRUE.,.TRUE.)                    DJG0295
C GDH0597        ENDIF
      ENDIF
      UHF=(ISUHF.EQ.2)                                                  DJG0995
      DEGREE=57.29577951D0
      IF(NA(1).EQ.99)THEN
      DEGREE=1.D0
      TYPE(1)='           '
      TYPE(2)='           '
      TYPE(3)='           '
      ENDIF
C                                                                       DJG0995
C      WRITE(JOUT,'(/)')                                                 DJG0995
      XKWFLG=.TRUE.                                                     DJG0995
      NTPRT=1                                                           GDH1195
      CALL ECHOWD(JOUT,NTPRT,XKWFLG)                                    GDH1195
C                                                                       DJG0995
C   PRINT OUT OPTIMIZATION MESSAGES                                     DJG0995
C                                                                       DJG0995
      IF (ISM50.EQ.0) THEN                                              GDH0197
      WRITE(JOUT,'(/4X,A58)')FLEPO(IFLEP)                               DJG0995
      WRITE(JOUT,'(4X,A58)')ITER(ISCF)                                  DJG0995
      WRITE (JOUT,'(A80)') CDAT                                         GDH1093
      IF(ISCF.EQ.2)THEN                                                 DJG0995
C
C   RESULTS ARE MEANINGLESS. DON'T PRINT ANYTHING
C
         WRITE(JOUT,'(//,'' FOR SOME REASON THE SCF CALCULATION FAILED.
     1'',/,'' THE RESULTS WOULD BE MEANINGLESS, SO WILL NOT BE PRINTED.
     2'',/,'' TRY TO FIND THE REASON FOR THE FAILURE BY USING "PL".'',/,
     3'' CHECK YOUR GEOMETRY AND ALSO TRY USING SHIFT OR PULAY. '')')
         CALL GEOUT
      ISTOP=1                                                           GDH1095
      IWHERE=161                                                        GDH1095
      RETURN                                                            GDH1095
      ENDIF
C                                                                       DJG0995
C   PRINT OUT LINE SUMMARY OF ENTIRE CALCULATION                        DJG0995
C                                                                       DJG0995
      CALL LINSUM(FUNCT,SUMW,DIP,SZ,SS2,KCHRGE,TIME0,TSOLVE,JOUT)       DJG0995
      IF (ISTOP.NE.0) RETURN                                            GDH1095
C                                                                       DJG0995
C   CONVERT INTERNAL COORDINATES FROM DEGREES TO RADIANS                DJG0995
C                                                                       DJG0995
      IF( NDEP .NE. 0 )CALL SYMTRY
      IF(NA(1).NE.99) THEN
         DO 50 J=1,NATOMS
         DO 50 I=1,3
            X=GEO(I,J)
            GOTO (40, 30, 20) I
   20       X=X-AINT(X/(2.D0*PI)+SIGN(0.4999D0,X)-0.0001D0)*PI*2.D0
            GEO(3,J)=X
            GOTO 40
   30       X=X - AINT(X/(2.D0*PI))*PI*2.D0
            IF(X.LT.0) X=X+PI*2.D0
            IF(X.GT.PI) THEN
               GEO(3,J)=GEO(3,J)+PI
               X=2.D0*PI-X
            ENDIF
            GEO(2,J)=X
   40       CONTINUE
   50    CONTINUE
      ENDIF
      DO 60 I=1,NVAR
   60 XPARAM(I)=GEO(LOC(2,I),LOC(1,I))
C                                                                       DJG0995
C   TRANSFER INTERNAL COORDINATES IN GEO TO XYZ IN COORD                DJG0995
C                                                                       DJG0995
      CALL GMETRY(GEO,COORD)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF(PRTGRA)THEN
         WRITE(JOUT,'(/,7X,''Final  point  and  derivatives'',/)')       GL0592
         WRITE(JOUT,'(''   Parameter     Atom    Type  ''                GL0592
     1    ,''          Value       Gradient'')')                         GL0592
      ENDIF
      SUM=0.5D0
      DO 70 I=1,NUMAT
   70 SUM=SUM+CORE(NAT(I))
      I=SUM
      KCHRGE=I-NCLOSE-NOPEN-NALPHA-NBETA
C
C    WRITE OUT THE GEOMETRIC VARIABLES
C
C                                                                       DJG0995
C   PRINT GRADIENTS                                                     DJG0995
C                                                                       DJG0995
      IF(PRTGRA) THEN
         DO 80 I=1,NVAR
            J=LOC(2,I)
            K=LOC(1,I)
            L=LABELS(K)
            XI=XPARAM(I)
            IF(J.NE.1) XI=XI*DEGREE
            IF(J.EQ.1.OR.NA(1).EQ.99)THEN
               GTYPE='kcal/angstrom'                                     GL0492
            ELSE
               GTYPE='kcal/radian  '                                     GL0492
            ENDIF
   80    WRITE(JOUT,'(I7,I11,1X,A2,4X,A11,F13.6,F13.6,2X,A13)')
     1         I,K,ELEMNT(L),TYPE(J),XI,GRAD(I),GTYPE
      ENDIF
C
C     WRITE OUT THE GEOMETRY --- IF A RIGID MODEL AND PRGEOM KEYWORD
C     NOT USED, DO NOT PRINT THE GEOMETRY                               PDW1099
C     THIS IS THE SECOND TIME THAT THE GEOMETRY IS PRINTED.
C      WRITE (JOUT,82) IOPT, I1SCF
C   82   FORMAT (/,'THE VALUE IF IOPT IS', I5, I5)
      IF (I1SCF.EQ.0.OR.IPRGEO.NE.0) THEN                               PDW1099
      CALL GEOUT
      IF (INOINT.EQ.0) THEN                                             DJG0995
C
C   WRITE OUT THE INTERATOMIC DISTANCES
C
      L=0
      DO 90 I=1,NUMAT
      DO 90 J=1,I
        L=L+1
         RXYZ(L)=SQRT((COORD(1,I)-COORD(1,J))**2+
     1                         (COORD(2,I)-COORD(2,J))**2+
     2                         (COORD(3,I)-COORD(3,J))**2)
   90 CONTINUE
CCC
          WRITE(JOUT,'(/,10X,''  Interatomic distances (angstroms)'')')  GL0492
          CALL VECPRT(RXYZ,NUMAT)
      ENDIF
      ENDIF
      IF (IVECTO.NE.0) THEN                                             DJG0995
          WRITE(JOUT,'(/,10X,A5,'' eigenvectors  '')')CALCN(ISUHF)      DJG0995
          CALL MATOUT (C,EIGS,NORBS,NORBS,NORBS)
      IF (UHF) THEN
         WRITE(JOUT,'(/,10X,'' Beta eigenvectors  '')')                  GL0492
         CALL MATOUT (CBETA,EIGB,NORBS,NORBS,NORBS)
      ENDIF
      ELSE
          WRITE(JOUT,'(/,10X,A5,''Orbital eigenvalues (eV)'',/)')       PDW1099
     .    CALCN(ISUHF)

          WRITE(JOUT,'(8F10.5)')(EIGS(I),I=1,NORBS)
      IF(UHF) THEN
          WRITE(JOUT,'(/,10X,'' Beta eigenvalues '')')                   GL0492
          WRITE(JOUT,'(8F10.5)')(EIGB(I),I=1,NORBS)
      ENDIF
C
C   SET UP C.I. PARAMETERS AND DO SOME CI PRINTING
C
      IF(CI.OR.((NOPEN.NE.NCLOSE).AND.(IMECI+IESR.NE.0))) CALL CIPARM   DJG0995
C                                                                       DJG0995
C   PRINT OUT DIPOLES AND PARTIAL CHARGES                               DJG0995
C                                                                       DJG0995
      CALL DIPRNT(DIP,Q,KCHRGE,SUMW,COORD)                              DJG0995
      ENDIF                                                             GDH0197
C                                                                       DJG0995
C   PRINT OUT CARTESIAN COORDINATES                                     DJG0995
C                                                                       DJG0995
      IF (ISM505.NE.0) THEN                                             GDH0197
      DO 128 I=1,NUMAT                                                  GDH0197
      COORD(1,I)=SCOORD(1,I)                                            GDH0197
      COORD(2,I)=SCOORD(2,I)                                            GDH0197
      COORD(3,I)=SCOORD(3,I)                                            GDH0197
128   CONTINUE                                                          GDH0197
      ENDIF                                                             GDH0197
      IF (I1SCF.EQ.0.OR.IPRGEO.NE.0) THEN                               PDW1099
      IF (INOXYZ.EQ.0) THEN 
         WRITE(JOUT,'(/,10X,''Cartesian coordinates (angstroms)'',/)')  GL0492
         WRITE(JOUT,'(4X,''NO.'',7X,''Atom'',15X,''X'',                 GL0492
     1         9X,''Y'',9X,''Z'',/)')
         WRITE(JOUT,'(I6,8X,A2,9X,3F10.5)')                             GDH0794
     1         (I,ELEMNT(NAT(I)),(COORD(J,I),J=1,3),I=1,NUMAT)
      ENDIF
      ENDIF
      ENDIF
      IF (ISM50.EQ.0) THEN                                              GDH0297
C                                                                       DJG0995
C   PRINT OUT FOCK MATRIX                                               DJG0995
C                                                                       DJG0995
      IF (IFOCK.NE.0) THEN                                              DJG0995
         WRITE(JOUT,'(/,'' Fock matrix is '')')                         GL0492
         CALL VECPRT(F,NORBS)
      END IF
C                                                                       DJG0995
C   PRINT OUT DENSITY MATRIX OR ORBITAL POPULATIONS                     DJG0995
C                                                                       DJG0995
      IF (IDENSI.NE.0) THEN                                             DJG0995
         WRITE(JOUT,'(/,'' Density matrix is '')')                      GL0492
         CALL VECPRT(P,NORBS)
      ELSE
         WRITE(JOUT,'(/,10X,''Atomic orbital electron populations'',/)')GL0492
         WRITE(JOUT,'(8F10.5)')(P((I*(I+1))/2),I=1,NORBS)
      END IF
C                                                                       DJG0995
C   PRINT OUT SIGMA AND PI BOND MATRIX                                  DJG0995
C                                                                       DJG0995
      IF(IPISIG.NE.0) THEN                                              DJG0995
         WRITE(JOUT,'(/,10X,''SIGMA-PI bond-order matrix'')')           GL0492
      CALL DENROT
      ENDIF
C                                                                       DJG0995
C   PRINT OUT UHF INFORMATION                                           DJG0995
C                                                                       DJG0995
      IF(UHF) CALL UHFPRT(SZ,SS2)                                       DJG0995
C                                                                       DJG0995
C   PRINT OUT BOND ORDERS                                               DJG0995
C                                                                       DJG0995
      IF (IBONDS.NE.0) THEN                                             DJG0995
         CALL BONDS(P,.TRUE.)                                           CC8-91
      ENDIF                                                             CC1091
      IF (ISM2+ISM3+ISM21+ISM31+ISM22+ISM23.GT.0) THEN                  GDH0296
         IF (IBONDS.EQ.0) CALL BONDS(P,.TRUE.)                          DJG0995
         WRITE(JOUT,'(/)')                                              CC1091
         DO 142 J=1,NUMAT                                               CC8-91
            IF(NAT(J).EQ.1) GO TO 142                                   CC8-91
            WRITE(JOUT, 1019) J, HBORD(J)                               GL0592
142      CONTINUE                                                       CC8-91
         WRITE(JOUT,'(/)')                                              CC8-91
      ENDIF                                                             CC8-91
C                                                                       DJG0995
C   PRINT OUT LOCALIZED MO'S                                            DJG0995
C                                                                       DJG0995
      I=NCLOSE+NALPHA
      IF (ILOCAL.NE.0) THEN                                             DJG0995
         CALL LOCAL(C,NORBS,I,EIGS)
         IF(NBETA.NE.0)THEN
            WRITE(JOUT,'(/,10X,'' Localized beta molecular orbitals'')')DJG0495
            CALL LOCAL(CBETA,NORBS,NBETA,EIGB)
         ENDIF
      ENDIF
C                                                                       DJG0995
C   PRINT OUT 1 ELECTRON MATRIX                                         DJG0995
C                                                                       DJG0995
      IF (I1ELEC.NE.0) THEN                                             DJG0995
         WRITE(JOUT,'(/,'' Final one-electron matrix '')')              GL0492
         CALL VECPRT(H,NORBS)
      END IF
C                                                                       DJG0995
C   PRINT OUT ENERGY PARTITIONING                                       DJG0995
C                                                                       DJG0995
      IF(IENPAR.NE.0) CALL ENPART(UHF,H,PA,PB,P,Q,COORD)                DJG0995
C                                                                       DJG0995
C   PRINT OUT DENSITY MATRIX IN UNFORMATTED FILE JDEN                   GDH1095
C                                                                       DJG0995
      IF(IDENOU.NE.0) THEN                                              DJG0995
         OPEN(JDEN,STATUS='UNKNOWN',FORM='UNFORMATTED')                 GDH1095
         REWIND JDEN                                                    GDH1095
         WRITE(JDEN)(PA(I),I=1,LINEAR)                                  GDH1095
         IF(UHF)WRITE(JDEN)(PB(I),I=1,LINEAR)                           GDH1095
         CLOSE (JDEN)                                                   GDH1095
      ENDIF
C                                                                       DJG0995
C   PRINT OUT DENSITY MATRIX IN FORMATTED (ASCII) FILE UNIT JDMT        DJG0995
C                                                                       DJG0995
      IF(IDENMA.NE.0) THEN                                              DJG0995
         REWIND JDMT                                                    GDH1095
         IF (UHF) THEN
            CALL VECWRT(PA,NORBS,JDMT)                                  GDH1095
            CALL VECWRT(PB,NORBS,JDMT)                                  GDH1095
         ELSE
            CALL VECWRT(P,NORBS,JDMT)                                   GDH1095
         ENDIF
      ENDIF
C                                                                       DJG0995
C   DO "REAL" (NOT LOWDIN) MULLIKEN POPULATION ANALYSIS AND INFO FOR    DJG0995
C   GRAPH IS PRINTED                                                    DJG0995
C                                                                       DJG0995
      IF (IMULLI+IGRAPH.NE.0) THEN                                      DJG0995
         IF (IMULLI.NE.0) THEN                                          DJG0995
            WRITE(JOUT,'(/10X,'' Mulliken population analysis'')')      GL0492
         ELSE
            WRITE(JOUT,'(/10X,'' Data for graph written to disk'')')    GL0492
         ENDIF
         CALL MULLIK(C,UHF,H,NORBS)
      END IF
      ENDIF                                                             GDH0297
C                                                                       DJG0995
C   IF NOT SMx RUN, PRINT OUT VOLUME AND ITERATION INFO, THEN SKIP SMx  DJG0995
C   STUFF                                                               DJG0995
C                                                                       DJG0995
      IF(.NOT.AQCHK) THEN
         IF (VOLM) WRITE(JOUT, 1053) VOLUM                              GDH0793
         WRITE(JOUT, 2010) NGEOM, NSCFS                                 DJG0496
         GO TO 2156
      ENDIF
C                                                                       DJG0995
C   PRINT OUT SASA INFO                                                 DJG0995
C                                                                       DJG0995
      IF (IAREAS.NE.0) CALL PRAREA                                      DJG0995
      IF (ISM50.NE.0.AND.ISM505.EQ.0) GOTO 2213                         GDH0297
C                                                                       DJG0995
C   PRINT OUT COULOMB INTEGRALS                                         DJG0995
C   NOTE: "FGB" HOLFS (14.3966).(1-CELEID)/Still(s FACTOR               DL0397
C                                                                       DJG0995
      CELCOE=-0.5D0*(1D0-CELEID)                                        DJG0495
      IF (IPRCOU.NE.O) THEN                                             PDW1199
      WRITE(JOUT, 1020) CELCOE,332.0D0*CELCOE                           DJG0495
      ENDIF                                                             PDW1199
      IF (ISM505.EQ.1) THEN                                             GDH0597
      IJ=0                                                              GDH0597
      DO 498 I=1,NUMAT                                                  GDH0597
      DO 497 J=1,I                                                      GDH0597
         IJ=IJ+1                                                        GDH0597
         FGBHLD2(IJ)=0.0D0                                              GDH0597
         FGBHLD(IJ)=QEFF2(I)*QEFF2(J)                                   GDH0597
497   CONTINUE                                                          GDH0597
      FGBHLD2(IJ)=1.0D0                                                 GDH0597
498   CONTINUE                                                          GDH0597
      DO 499 I=1,NPACK                                                  GDH0597
         IF (FGBHLD(I).EQ.0) THEN                                       GDH0597
            BUFFER(I)=0.0D0                                             GDH0597
         ELSE                                                           GDH0597
            BUFFER(I)=FGB(I)*23.061D0/(1D0-CELEID)                      GDH0597
         ENDIF                                                          GDH0597
  499 CONTINUE                                                          GDH0597
      ELSE                                                              GDH0597
      DO 500 I=1,NPACK                                                  DL0397
         BUFFER(I)=FGB(I)*23.061D0/(1D0-CELEID)                         DL0397
  500 CONTINUE                                                          DL0397
      ENDIF                                                             GDH0597
      IF (IPRCOU.NE.0) THEN
      CALL VECPRT(BUFFER,NUMAT)                                         DL0397
      ENDIF
C                                                                       DJG0995
C   PRINT OUT EFFECTIVE BORN RADII                                      DJG0995
C                                                                       DJG0995
      CELCOE=14.3966D0*(1D0-CELEID)                                     DL0397
      IF (IPRRAD.NE.O) THEN
      WRITE (JOUT, 1021)                                                GL0492
      ENDIF
      IF (ISM505.EQ.0) THEN                                             GDH0597
      DO 501 I=1,NPACK                                                  DL0397
         IF (FGB(I).NE.0D0) BUFFER(I)=CELCOE/FGB(I)                     DL0397
  501 CONTINUE                                                          DL0397
      ELSE                                                              GDH0597
      WRITE (JOUT,1022)                                                 GDH0497
      DO 505 I=1, NPACK                                                 GDH0497
         IF (FGBHLD(I).EQ.0) THEN                                       GDH0497
            BUFFER(I)=0.0D0                                             GDH0497
            FGB(I)=0.0D0                                                GDH0497
         ELSE                                                           GDH0497
            BUFFER(I)=CELCOE/FGB(I)                                     GDH0497
         ENDIF                                                          GDH0497
505   CONTINUE                                                          GDH0497
      DO 506 I=1,NUMAT                                                  GDH0597
         IF (QEFF2(I).EQ.0) BP(I)=0.0D0                                 GDH0597
506   CONTINUE                                                          GDH0597
      ENDIF                                                             GDH0497
      IF (IPRRAD.NE.O) THEN                                             PDW1199
      CALL VECPRT(BUFFER,NUMAT)                                         GDH0497
      ENDIF                                                             PDW1199
C                                                                       DJG0995
C   PRINT OUT POLARIZATION ENERGY MATRIX                                DJG0995
C                                                                       DJG0995
      IF (IPRPOL.NE.0) THEN
      CALL POLENM (BUFFER)                                              DL0397
      ENDIF
C                                                                       DJG0995
C   PRINT OUT SCRF ITERATION INFO                                       DJG0995
C                                                                       DJG0995
      WRITE(JOUT, 2020) NGEOM, NSOLPR, NSCFS                            GDH0495
2213  CONTINUE
C                                                                       DJG0995
C   PRINT OUT ATOM-BY-ATOM & ELEMENT-BY-ELEMENT SOLVATION SUMMARY       DL0397
C                                                                       DJG0995
      CALL SATBAT (BUFFER,CSTOT,IVSZT,IVSZMX,IVSZMN,JOUT)               DL0397
C      IF (ISM50.EQ.0.OR.ISM505.EQ.1) CALL SELBEL(BUFFER,BPGT)           GDH0597
      CALL SELBEL(BUFFER,BPGT)                                          GDH0597
C                                                                       DJG0995
C  PRINT INFO ON EXPANSION SHELLS                                       DJG0995
C                                                                       DJG0995
      RIVSZ=(IVSZT*1.D0)/(1.D0*NUMAT)                                   GDH0194
      WRITE(JOUT,2030) NUMAT,RIVSZ,IVSZMX,IVSZMN                        GDH0194
C                                                                       DJG0995
C   PRINT OUT SOLVATION CONTRIBUTION TO THE FOCK MATRIX                 DJG0995
C                                                                       DJG0995
      IF (IFOCK.NE.0) THEN                                              DL0397
         WRITE (JOUT,'(/)')                                             GL0592
         IF (ICHMOD.GE.1) THEN                                          GDH0997
            WRITE(JOUT, 1027)                                           DL0397
            CALL VECPRT(DRVPOL,NORBS)                                   DL0397
         ELSE                                                           DL0397
            DO 2154 J=1,NUMAT                                           DL0397
2154        WRITE(JOUT, 1026) J, BP(J)                                  DL0397
         ENDIF                                                          DL0397
      ENDIF                                                             GL0492
C                                                                       DJG0995
C   PRINT OUT VOLUME INFO                                               DJG0995
C                                                                       DJG0995
      IF (VOLM) WRITE(JOUT, 1053) VOLUM                                 GDH0793
C                                                                       DJG0995
C   PRINT OUT INFO MESSAGE ABOUT FINAL ENERGY                           DJG0995
C                                                                       DJG0995
      IF (ISM50.EQ.0)  THEN                                             GDH0297
      IF (INOPOL.EQ.0) THEN                                             DJG0995
         IF(ITRUES.EQ.0) THEN                                           DJG0995
333         WRITE(JOUT, 1030)                                           DJG0796
         ENDIF                                                          GL0492
      ELSE                                                              CC1091
         WRITE(JOUT, 1031)                                              GL0592
      ENDIF                                                             CC1091
      ENDIF                                                             GDH0297
      IF (AQCHK) THEN                                                   GL0492
C                                                                       DJG0995
C   PRINT OUT AREA BY ATOM TYPE FOR SM1A                                DJG0995
C                                                                       DJG0995
         IF (ISM1A.EQ.1) CALL SM1ARA                                    DJG0995
C                                                                       DJG0995
C   PRINT OUT COMMENT LINE AND TITLE                                    DJG0995
C                                                                       DJG0995
         WRITE(JOUT, 1033) KOMENT, TITLE                                GL0592
C                                                                       DJG0995
C   PRINT OUT SOLVATION SUMMARY BY TERM                                 DJG0995
C                                                                       DJG0995
         CALL STERMS(BPGT,FUNCT,TSOLVE,JOUT)                            DJG0995
      IF (ISM50.EQ.0) THEN                                              GDH1196
         WRITE(JOUT,'(/4X,A58)')FLEPO(IFLEP)                            DJG0995
      ENDIF                                                             GDH1196
C
      ENDIF                                                             GL0492
2156  CALL PORCPU(TFINAL)                                               DJG0995
      WRITE (JOUT, 1052) (TFINAL - TIME0)                               GL0492
      IF (.NOT.AFAIL.AND.IRESTA.EQ.-1) IRESTA=0                         DJG0995
      IF (.NOT.AQCHK.AND.IHF.EQ.0.AND.ICR.NE.1) IHF=-5                  GHD1195
      IF (IHFCAL.EQ.1.AND.ICR.EQ.1) IHF=-5                              GDH1/96
      IF (IHFCAL.GE.2.AND.ICR.EQ.1) IHF=-3                              GDH1/96
      IF((AQCHK).AND.(ISTDM.EQ.0.AND.IEXTM.EQ.0)) ISTDM=-1              DJG0995
C
C   *****************************************************************
C   *                                                               *
C   *      New Input file with optimized geometry ( .INP FILE)      *
C   *                                                               *
C   *****************************************************************
C
      IF (IINPUT.NE.0.OR.ICR.GT.0) CALL RESINP(FUNCT)                   DJG0995
C GDH0895      IF (ICR.EQ.1) THEN
C GDH0895         RETURN                                                GDH0495
C GDH0895      ENDIF                                                    GDH0195
C
C   *****************************************************************
C   *                                                               *
C   *      SUMMARY OF OUTPUT ON FILE JARC   ( . ARC FILE )          *
C   *                                                               *
C   *****************************************************************
C
C     NOT DONE IF OPTIMISATION NOT ACHIEVED
      IF (AFAIL) RETURN                                                 DJG0995
C
      IF((ICONTR(24).EQ.1.AND.LATOM.EQ.0.).OR.                          GDH0895
     1   (REACT(100).LT.-998.9D0.AND.REACT(100).GT.-999.1D0)) THEN      DJG0995
         OPEN(UNIT=JARC,STATUS='UNKNOWN')                               GDH1095
         REWIND JARC                                                    GDH1095
      ENDIF
      WRITE (JARC  ,'(A80)') CDAT                                       GDH1093
      XKWFLG=.TRUE.                                                     DJG0995
      NTPRT=0                                                           GDH1195
      CALL ECHOWD(JARC,NTPRT,XKWFLG)                                    GDH1195
      CALL HEADER(JARC)                                                 GDH1195
C                                                                       DJG0995
C   DETERMINE AND PRINT MOLECULAR FORMULA FOR .ARC FILE                 DJG0995
C                                                                       DJG0995
      CALL MFORM(JARC  )                                                DJG0995
      IF (ISM50.EQ.0) THEN                                              GDH0497
      WRITE(JARC  ,'(/4X,A58)')FLEPO(IFLEP)                             DJG0995
      WRITE(JARC  ,'(4X,A58)')ITER(ISCF)                                DJG0995
C                                                                       DJG0995
C   PRINT OUT LINE SUMMARY OF ENTIRE CALCULATION                        DJG0995
C                                                                       DJG0995
      CALL LINSUM(FUNCT,SUMW,DIP,SZ,SS2,KCHRGE,TIME0,TSOLVE,JARC)       DJG0995
      ENDIF                                                             GDH0497
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (AQCHK) THEN                                                   GL0492
C                                                                       DJG0995
C  PRINT OUT SOLVATION INFO ATOM BY ATOM                                DJG0995
C                                                                       DJG0995
          CALL SATBAT (BUFFER,CSTOT,IVSZT,IVSZMX,IVSZMN,JARC)           DL0397
C                                                                       DJG0995
C  PRINT INFO ON EXPANSION SHELLS                                       DJG0995
C                                                                       DJG0995
          WRITE(JARC  ,2030) NUMAT,RIVSZ,IVSZMX,IVSZMN                  GDH0194
C                                                                       DJG0995
C   PRINT OUT SOLVATION SUMMARY BY TERM                                 DJG0995
C                                                                       DJG0995
          CALL STERMS(BPGT,FUNCT,TSOLVE,JARC  )                         DJG0995
      ENDIF                                                             GL0492
      WRITE(JARC  ,'(//10X,''FINAL GEOMETRY OBTAINED'',33X,''CHARGE'')')
      CALL ZMPRNT(JARC,FUNCT)                                           DJG0995
      NSCF=0
      IF (ISPART.EQ.1.AND.(ICR.EQ.0.OR.ICR.EQ.2))                       DJG0496
     1    CALL SPAOUT(FUNCT)                                            DJG0496
C  This section allows the .arc file to be appended to if using         GDH0895
C  TRUES and CALC together (24), and an attempt to open the .inp file   DJG0995
C  will not occur a second time (57).                                   DJG0995
      IF (ICR.EQ.1) THEN                                                GDH0895
         DO 3 I=1,100                                                   GDH0895
3           ICONTR(I)=1                                                 GDH0895
         ICONTR(24)=2                                                   DJG0995
      ENDIF                                                             GDH0895
C
1019  FORMAT(1X, 'Atom #', I3, ' Total H-bond order', F6.2)             GL0592
1020  FORMAT(/,1X,'Polarization free energy', /,1X,T20,'= ',F7.4,       DJG0495
     1       '*Sum-over-k-and-k''(q[k]*q[k'']*gamma[k,k''])',           DJG0495
     2       /,1X,T20,'= ',F6.1,' kcal*angstrom*Sum-over-k-and-k''',    DJG0495
     3       '(q[k]*q[k'']/f[k,k''])',//,1X,'Coulomb integrals ',       DJG0495
     4       'gamma[k,k''] (kcal):')                                    DJG0495
1021  FORMAT(/,1X,'Effective Born radii f[k,k] and effective',/,1X,     GL0492
     1       'interatomic distances f[k,k''] (angstroms):')             GL0492
1022  FORMAT(/,1X,'A zero in the table indicates that the ',            GDH0497
     1       'entry is undefined for this model.')                      GDH0497
1026  FORMAT(1X,'Atom #',I3,' Polarization contribution to FOCK ',      GL0592
     1       'diagonal: ',F12.6,' eV')                                  GL0592
1027  FORMAT(' Polarization contribution to FOCK (eV)')                 DL0397
1030  FORMAT(/,1X,'**** NOTA BENE ****',/,1X,'This is the net ',        GL0492
     1       'solvation energy for this exact molecular structure ',/,  GL0492
     2       1X,'(nuclear and electronic)!  The standard-state',        DJG0995
     3       ' solvation energy should be ',/,1X,'obtained as ',        DJG0796
     4       'the difference between the heat of formation ',           GL0492
     5       'plus delta-G solvation',/,1X,'for the relaxed solvated ', DJG0496
     6       'system and that for the relaxed gas-phase system.')       GL0492
1031  FORMAT(/,1X,' *****  NOPOL  *****',/,7X,'The solvation ',         GL0592
     1       'energetics calculated here were not arrived at',/,2X,     GL0592
     2       'in a self-consistent fashion.  They have been calculated',GL0592
     3       ' using both',/,2X,'gas-phase geometry and gas-phase ',    GL0592
     4       'solute wave function.')                                   GL0592
1033   FORMAT(/,1X,A80,/,1X,A80)                                        GL0592
1052   FORMAT(/,' Total computer time = ',F10.2,' seconds')             GL0492
1053   FORMAT(/,1X,'The total molecular volume in cubic',               GDH0793
     1        ' angstroms:',F15.2)                                      GDH0793
2010   FORMAT(/,3X,'Number of geometries ',                             DJG0996
     1        32X,I6,/,3X,'The total number of SCF iterations',19X,I6,  DJG0996
     2        '.',/,4X,'Note: For optimizations, the number of ',       DJG1196
     3        'geometries may not',/,4X,'correspond to the',            PDW1299
     4        ' number of cycles',/,4X,'due to rejected geometry',      PDW1099
     5        ' changes.')                                              PDW1099
2020   FORMAT(/,3X,'Number of geometries',32X,                          PDW1099
     2        I7,/,3X,'Number of calculations of the screened coulomb ' GDH1093
     3        ,'radii '                                                 GDH0595
     4        ,I6,/,3X,'The total number of SCF iterations',19X,I6,'.', DJG0996
     6        /,4X,'Note: The number of geometries may not',/,4X,       PDW1299
     7        'correspond to the number of cycles due to ',             DJG0996
     8        'rejected geometry changes.')                             DJG0996
2030   FORMAT(/,3X,'The number of atoms in the molecule is ',I6,        GDH0194
     .        /,3X,'The average number of expansion shells was ',F7.2,  GDH0194
     1        /,3X,'The maximum number of expansion shells was ',I6,    GDH0194
     2        /,3X,'The minimum number of expansion shells was ',I6,    GDH0194
     3        /)                                                        GDH0194
      END
