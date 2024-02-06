      SUBROUTINE STLFAC (BORN)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'KEYS.i'
      INCLUDE 'SIZES.i'
C*******************************************************************************
C    THIS SUBROUTINE CALCULATES THE STILL FACTOR AS A RUNCTION OF R AND ALPHA
C    CREATED BY DJG 0995 FROM EXISTING LINES IN SMX1 AND SRFCTY.
C    GLOBAL CHANGE AND DERIVATIVES ADDED BY DL 0397.
C ON INPUT
C    BORN  = EFFECTIVE BORN RADII
C    ODFAC = ELECTROSTATIC COGS VALUE
C    R     = INTERATOMIC DISTANCES
C ON OUTPUT
C    FGB  = CELCOE/Still's FACTORS, CANONICAL ORDER (eV)                DL0397
C    DFGB = CARTESIAN DERIVATIVES (eV/Angstroms)                        DL0397
C
C    NOTE:                                                              DL0397
C    CARTESIAN DERIVATIVES OF FGB: DFGB(NUMAT*(NUMAT+1)/2,3,NUMAT),     DL0397
C    DFGB(kl,i,j) = dFGB(kl)/dCOORD(i,j)                                DL0397
C*******************************************************************************
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),
     1                QEFF2(NUMATM),DRVPOL(MPACK)
      COMMON /ARACOM/ R(NPACK),R2(NPACK),ISAVE,DONE
     1               ,RAC(NUMATM),RAC2(NUMATM),DRAC(NUMATM,NUMATM)      DL0397
     2               ,RAS(NUMATM),RALG(NUMATM)
      COMMON /DKKCOM/ DKKG, DKKCH, DKKOO, DKKHH                         GDH0597
      COMMON /SOLVCM/ CELEID,SLVRAD,SLVRD2,CSSIGM                       DL0397
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM),RMAX(NUMATM)
     1               ,URLIO(3,NUMATM,NUMATM)                            DL0397
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL0397
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL0397
     2               ,DATLGR(3,NUMATM)                                  DL0397
     3               ,LGR,FLAGRD                                        DL0397
                      LOGICAL LGR,FLAGRD                                DL0397
      DIMENSION BORN(*)
      LOGICAL DONE,LODFAC,FLAG
      DATA ZERO /0D0/
      CELCOE=14.3966D0*(1D0-CELEID)
      IF (LGR) THEN                                                     DL0397
         NINEAR=NUMAT*(NUMAT+1)/2                                       DL0397
         DO 10 K=1,3                                                    DL0397
         DO 10 L=1,NUMAT                                                DL0397
   10    CALL SCOPY (NINEAR,ZERO,0,DFGB(1,K,L),1)                       DL0397
      ENDIF                                                             DL0397
      LODFAC=INOCOG.EQ.0.AND.(IRAD.LT.4.OR.IRAD.EQ.6.OR.IRAD.EQ.12      DL0397
     .   .OR.IRAD.EQ.13.OR.IRAD.EQ.22.OR.IRAD.EQ.23.OR.IRAD.EQ.33)
      IJ=0
      DO 100 I=1,NUMAT
      DO 100 J=1,I
      IJ=IJ+1
      ODFAC=0D0
      IF (LGR) DODFAC=0D0                                               DL0397
      IF (LODFAC) THEN
         K=MIN(NAT(I),NAT(J))
         L=MAX(NAT(I),NAT(J))
         KL=(L*(L-1))/2+K
         FLAG=.FALSE.
         IF (KL.EQ.22.AND.IRAD.NE.6) THEN
            IF (R(IJ).LE.2.399D0.AND.R(IJ).GE.1.401D0) THEN
               COGH=4.D0
               COGW=-1.75D0
               RNUM=1.9D0
               RDEN=0.5D0
               FLAG=.TRUE.
            ENDIF
         ELSE IF (KL.EQ.36) THEN
            IF (R(IJ).LE.2.599D0.AND.R(IJ).GE.0.801D0) THEN
               COGH=9.D0
               COGW=-1.75D0
               RNUM=1.7D0
               RDEN=0.9D0
               FLAG=.TRUE.
            ENDIF
         ENDIF
         IF (FLAG) THEN
            RDEN=RDEN**2
            TERM1=RDEN-(R(IJ)-RNUM)**2
            EXPON=RDEN*COGW/TERM1
            ODFAC=COGH*EXP(EXPON)
            IF (LGR) DODFAC=ODFAC*EXPON*(R(IJ)-RNUM)/TERM1              DL0397
         ENDIF
      ENDIF
      DCH=DKKG                                                          GDH0596
      IF (ISM50.NE.0) THEN                                              GDH0597
            IF (((NAT(I).EQ.8).AND.(NAT(J).EQ.8)).OR.((NAT(I).EQ.8).AND.GDH0297
     .           (NAT(J).EQ.8))) THEN                                   GDH0597
                 DCH=DKKOO                                              GDH0297
            ELSE IF (((NAT(I).EQ.1).AND.(NAT(J).EQ.1)).OR.((NAT(I).EQ.1)GDH0597
     .           .AND.(NAT(J).EQ.1))) THEN                              GDH0597
                 DCH=DKKHH                                              GDH0297
            ENDIF                                                       GDH0597
      ELSE IF (((NAT(I).EQ.6).AND.(NAT(J).EQ.1)).OR.((NAT(I).EQ.1).AND. GDH0596
     .           (NAT(J).EQ.6))) THEN                                   GDH0597
                 DCH=DKKCH                                              GDH0596
      ENDIF                                                             GDH0597
      AIJ=BORN(I)*BORN(J)
      IF (AIJ.EQ.0.D0.AND.ISM50.EQ.1) THEN                              GDH0897
         DD=0.0D0                                                       GDH0897
         FGBM2=0.0D0                                                    GDH0897
      ELSE                                                              GDH0897
         TERM1=R(IJ)/(DCH*AIJ)                                          GDH0897
         EXPON=R(IJ)*TERM1                                              GDH0897
         DD=EXP(-EXPON)                                                 GDH0897
         FGBM2=1D0/(R(IJ)**2+AIJ*(DD+ODFAC))                            GDH0897
      ENDIF                                                             GDH0897
      FGB(IJ)=CELCOE*SQRT(FGBM2)                                        DL0397
      IF (LGR) THEN                                                     DL0397
         FGBM3=-FGBM2*FGB(IJ)                                           DL0397
         TERM2=0.5D0*(DD*(EXPON+1D0)+ODFAC)*FGBM3                       DL0397
         DO 20 L=1,NUMAT                                                DL0397
         DO 20 K=1,3                                                    DL0397
         DFGB(IJ,K,L)=(BORN(J)*DBORN(K,L,I)+BORN(I)*DBORN(K,L,J))*TERM2 DL0397
   20    CONTINUE                                                       DL0397
         DFGBDR=(R(IJ)+AIJ*(DODFAC-DD*TERM1))*FGBM3                     DL0397
         DFGB(IJ,1,I)=DFGB(IJ,1,I)+DFGBDR*URLIO(1,J,I)                  DL0397
         DFGB(IJ,2,I)=DFGB(IJ,2,I)+DFGBDR*URLIO(2,J,I)                  DL0397
         DFGB(IJ,3,I)=DFGB(IJ,3,I)+DFGBDR*URLIO(3,J,I)                  DL0397
         DFGB(IJ,1,J)=DFGB(IJ,1,J)-DFGBDR*URLIO(1,J,I)                  DL0397
         DFGB(IJ,2,J)=DFGB(IJ,2,J)-DFGBDR*URLIO(2,J,I)                  DL0397
         DFGB(IJ,3,J)=DFGB(IJ,3,J)-DFGBDR*URLIO(3,J,I)                  DL0397
      ENDIF                                                             DL0397
  100 CONTINUE
      RETURN                                                            GDH0597
      END                                                               GDH0597
