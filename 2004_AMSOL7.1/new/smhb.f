      SUBROUTINE SMHB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'
C ---------------------------------------------------------------------+
C     CALCULATE SURFACE TENSIONS WHICH ARE LATER MULTIPLIED TIMES SASA
C     TO GET THE CDS TERM.
C     SURFACE TENSION DEPENDS ON BOND ORDER
C
C     CREATED BY DJG 0995 FROM EXISTING LINES IN BORNPL AND BRNPL2
C     NO DERIVATIVES HERE: NOT SELF CONSISTENT SMs.                     DL0397
C ---------------------------------------------------------------------+
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS, NALPHA, NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),
     1                HBCORC(100),QKNOT(100),QKONE(100)
      COMMON /HBORDS/ HBORD(NUMATM), HB(NUMATM)
      COMMON /ONESCM/ ICONTR(100)
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM),RMAX(NUMATM)
     1               ,URLIO(3,NUMATM,NUMATM)                            DL0397
      COMMON /PDCOM/  SMXPD(100), SPSRFT(10)                            GDH1095
      IF (ICONTR(58).EQ.1) THEN
         ICONTR(58)=2
         SQRT3=SQRT(3.0D0)
      ENDIF
      CALL BONDS (P,.FALSE.)
      DO 100 IAT=1,NUMAT
         NATIAT=NAT(IAT)
         ARED3=ATAR(IAT)*1D-3
         IF (NATIAT.EQ.7) THEN
           TRIP=SPSRFT(4)                                               GDH0296
           CC=SPSRFT(3)                                                 GDH0296
           UPLIM=TRIP+CC                                                GDH0296
           BTLIM=CC-TRIP                                                GDH0296
            IF (HBORD(IAT).GT.BTLIM.AND.HBORD(IAT).LT.UPLIM) THEN       GDH0296
               COGW=-SPSRFT(2)                                          GDH0296
               COGH=SPSRFT(1)                                           GDH0296
               FNFF=COGH*EXP(COGW/(1D0-((HBORD(IAT)-CC)/TRIP)**2))
            ENDIF
         ELSE IF (NATIAT.EQ.8) THEN
           TRIP=SPSRFT(8)                                               GDH0296
           CC=SPSRFT(7)                                                 GDH0296
           UPLIM=TRIP+CC                                                GDH0296
           BTLIM=CC-TRIP                                                GDH0296
            IF (HBORD(IAT).GT.BTLIM.AND.HBORD(IAT).LT.UPLIM) THEN       GDH0296
               COGW=-SPSRFT(6)                                          GDH0296
               COGH=SPSRFT(5)                                           GDH0296
               FNFF=COGH*EXP(COGW/(1D0-((HBORD(IAT)-CC)/TRIP)**2))      GDH0296
            ENDIF
         ELSE
            FNFF=0.D0
         ENDIF
         HB(IAT)=(ATAN(SQRT3*HBORD(IAT))+FNFF)*HBCORC(NATIAT)*ARED3     DJG0995
         IF (NATIAT.EQ.16.AND.ISM23.EQ.1) CALL SMSSHB(IAT,ARED3)        GDH0296
100   CONTINUE
      RETURN
      END
