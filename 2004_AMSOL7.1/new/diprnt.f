      SUBROUTINE DIPRNT(DIP,Q,KCHRGE,SUMW,COORD)                        DJG0995
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'KEYS.i'
      INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C******************************************************************************
C
C  THIS SUBROUTINE PRINTS OUT THE DIPOLE MOMENTS AND PARTIAL CHARGES.
C
C  ON INPUT
C    DIP = UNDEFINED
C    Q   = UNDEFINED
C    KCHRGE = INTEGER CHARGE ON MOLECULE
C    SUMW = MOLECULAR WEIGHT
C    COORD = XYZ COORDINATES
C
C  ON OUTPUT
C    DIP = DIPOLE MOMENT (CONSISTENT WITH CHARGE METHOD USED)
C    Q = ATOMIC CHARGES (CONSISTENT WITH CHARGE METHOD USED)
C    KCHRGE, SUMW = UNCHANGED
C    COORD = CENTER OF MASS COORDINATES IF ION, ELSE UNCHANGED
C
C  CREATED BY DJG0995 FROM EXISTING LINES IN WRITES
C
C******************************************************************************
      COMMON /OPTIMI / IMP,IMP0
      COMMON /CMCOM/  ECMCG(NUMATM)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /CORE  / CORE(107)
      DIMENSION Q(NUMATM), Q3(NUMATM), Q4(NUMATM), Q5(NUMATM), EVEC(3,3)
      LOGICAL WAVFUN, ALLOK, FARCE
      CHARACTER ELEMNT*2
       SAVE
C                                                                       DJG0895
C     IF ION, PUT CENTER OF MASS AT THE ORIGIN                          DJG0895
C     IF FARCE, THEN DOING A FORCE AND THERMO CALCULATION, SO SKIP      DJG0895
C     IF WAVFUN, THEN CALCULATE HYBRID AND WAVEFUNCTION DIPOLE MOMENTS  DJG0895
C        AND PRINT                                                      DJG0895
C                                                                       DJG0895
      FARCE=(IFORCE+ITHERM.NE.0)                                        DJG0995
      ALLOK=(KCHRGE.EQ.0.OR.(KCHRGE.NE.0.AND..NOT.FARCE))               DJG0995
      DIP=0.0D0                                                         DJG0895
      IF (KCHRGE.NE.0.AND.(.NOT.FARCE))                                 DJG0895
     1   CALL AXIS(COORD,NUMAT,AMOM,BMOM,CMOM,SUMW,7,EVEC)              CC2-92
      IF (IEXTCM.NE.0) THEN                                             DJG0995
         WRITE(JOUT,1000)                                               DJG0995
         WRITE(JOUT,1010)
         DO 100 I=1,NUMAT                                               GDH1293
            Q4(I)=ECMCG(I)                                              GDH1293
            L=NAT(I)                                                    GDH1293
            Q3(I)=CORE(L)-Q4(I)                                         GDH1293
100      WRITE(JOUT,1020) I,ELEMNT(L),Q4(I),Q3(I)                       DJG0995
         WAVFUN=.FALSE.                                                 DJG0895
         IF(ALLOK) THEN                                                 DJG0995
            WRITE(JOUT,1030)                                            DJG0995
            DIP= DIPOLE(P,Q4,COORD,DUMY,WAVFUN)                         DJG0895
         ENDIF                                                          CC2-92
         DIPE=DIP                                                       DJG0895
      ENDIF                                                             GDH1293
      IF (ICHMOD.EQ.1) THEN                                             DJG1095
         WRITE(JOUT,1040)                                               DJG0995
         WRITE(JOUT,1010)                                               DJG0995
         CALL CHGMP1(P,Q3)                                              DJG0995
         DO 200 I=1,NUMAT                                               DJG0995
            Q5(I)=CORE(NAT(I))-Q3(I)                                    DL0397
            WRITE(JOUT,1020) I,ELEMNT(NAT(I)),Q3(I),Q5(I)               DL0397
200      CONTINUE                                                       DJG0794
         WAVFUN=.FALSE.                                                 DJG0895
         IF(ALLOK) THEN                                                 DJG0995
            WRITE(JOUT,1060)                                            DJG0995
            DIP=DIPOLE(P,Q3,COORD,DUMY,WAVFUN)                          DL0397
         ENDIF                                                          DJG0895
         IF (IRAD.GT.0) WRITE (JOUT,1070)                               DJG0995
         DIPC=DIP                                                       DJG0895
      ELSEIF (ICHMOD.EQ.2) THEN                                         GDH0997
         WRITE(JOUT,1041)                                               GDH0997
         WRITE(JOUT,1010)                                               GDH0997
         CALL CHGMP2(P,Q3)                                              GDH0997
         DO 201 I=1,NUMAT                                               GDH0997
            Q5(I)=CORE(NAT(I))-Q3(I)                                    GDH0997
            WRITE(JOUT,1020) I,ELEMNT(NAT(I)),Q3(I),Q5(I)               GDH0997
201      CONTINUE                                                       GDH0997
         WAVFUN=.FALSE.                                                 GDH0997
         IF(ALLOK) THEN                                                 GDH0997
            WRITE(JOUT,1061)                                            GDH0997
            DIP=DIPOLE(P,Q3,COORD,DUMY,WAVFUN)                          GDH0997
         ENDIF                                                          GDH0997
         IF (IRAD.GT.0) WRITE (JOUT,1071)                               GDH0997
         DIPC=DIP                                                       GDH0997
C     ENDIF                                                             GDH0997
      ELSEIF (ICHMOD.EQ.3) THEN                                         !JT0802
         WRITE(JOUT,1042)                                               !JT0802
         WRITE(JOUT,1010)                                               !JT0802
         CALL CHGMP3(P,Q3)                                              !JT0802
         DO 202 I=1,NUMAT                                               !JT0802
            Q5(I)=CORE(NAT(I))-Q3(I)                                    !JT0802
            WRITE(JOUT,1020) I,ELEMNT(NAT(I)),Q3(I),Q5(I)               !JT0802
202      CONTINUE                                                       !JT0802
         WAVFUN=.FALSE.                                                 !JT0802
         IF(ALLOK) THEN                                                 !JT0802
            WRITE(JOUT,1062)                                            !JT0802
            DIP=DIPOLE(P,Q3,COORD,DUMY,WAVFUN)                          !JT0802
         ENDIF                                                          !JT0802
C        IF (IRAD.GT.0) WRITE (JOUT,1071)
         DIPC=DIP                                                       !JT0802
      ENDIF                                                             !JT0802
      WRITE(JOUT,1080)                                                  DJG0995
      CALL CHRGE(P,Q3)                                                  DJG0995
      WRITE(JOUT,1010)                                                  DJG0995
      DO 300 I=1,NUMAT
         L=NAT(I)
         Q(I)=CORE(L)-Q3(I)                                             DJG0995
300   WRITE(JOUT,1020) I,ELEMNT(L),Q(I),Q3(I)                           DJG0995
      WAVFUN=.TRUE.                                                     DJG0895
      IF(ALLOK) THEN                                                    DJG0995
          DIP= DIPOLE(P,Q,COORD,DUMY,WAVFUN)                            DJG0995
      ENDIF                                                             CC2-92
      IF (IEXTCM.NE.0) THEN                                             DJG0995
         DIP=DIPE                                                       DJG0895
         CALL SCOPY(NUMATM,Q4,1,Q,1)                                    DJG0995
      ELSE IF (ICHMOD.GE.1) THEN                                        DJG0895
         DIP=DIPC                                                       DJG0895
         CALL SCOPY(NUMATM,Q5,1,Q,1)                                    DJG0995
      ENDIF                                                             DJG0895
1000  FORMAT(/,13X,' Net atomic charges, atomic populations, and ',     PDW1299
     1       'dipole contributions',/,24X,' corresponding to EXTCM ',   DAL0303
     2       'atomic charges',/)                                        PDW1299
1010  FORMAT(8X,' Atom NO.   Type          Charge        No. ',         PDW1099
     1       'of electrons')                                            PDW1099
1020  FORMAT(I12,9X,A2,4X,F13.3,F16.3)                                  DJG0995
1030  FORMAT(/,'For the charges input with EXTCM:')                     DJG0995
1040  FORMAT(/,13X,' Net atomic charges, atomic populations, and ',     PDW1299
     1       'dipole contributions',/,35X,'calculated with CM1 ',/)     PDW1299
1041  FORMAT(/,13X,' Net atomic charges, atomic populations, and ',     PDW1299
     1       'dipole contributions',/,35X,'calculated with CM2 ',/)     PDW1299
1042  FORMAT(/,13X,' Net atomic charges, atomic populations, and ',     !JT0802
     1       'dipole contributions',/,35X,'calculated with CM3 ',/)     !JT0802
1060  FORMAT(/,'For the charges calculated by CM1:')                    DJG0995
1061  FORMAT(/,'For the charges calculated by CM2:')                    GDH0797
1062  FORMAT(/,'For the charges calculated by CM3:')                    !JT0802
1070  FORMAT (/,/,'Note:  The Mulliken population analysis charges ',   DJG0995
     1          'presented below were not',/,'used in the solvation ',  DJG1094
     2          'calculation but are provided for completeness.',/,     DJG1094
     3          'The chosen solvation model uses CM1 ',                 DJG0496
     4          'partial charges for calculating',/,                    CCC0496
     5          'solvation energies.')                                  CCC0496
1071  FORMAT (/,/,'Note:  The Mulliken population analysis charges ',   GDH0797
     1          'presented below were not',/,'used in the solvation ',  GDH0797
     2          'calculation but are provided for completeness.',/,     GDH0797
     3          'The chosen solvation model uses CM2 ',                 GDH0797
     4          'partial charges for calculating',/,                    GDH0797
     5          'solvation energies.')                                  GDH0797
1080  FORMAT(/,13X,' Net atomic charges, atomic populations, and ',     PDW1299
     1      'dipole contributions',/,28X,'using Mulliken population ',  PDW1299
     2      'analysis',/)                                               PDW1299
      RETURN
      END
