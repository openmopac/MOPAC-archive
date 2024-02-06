      SUBROUTINE EBRPD (BORN)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'SIZES2.i'
      INCLUDE 'KEYS.i'
C******************************************************************************
C     THIS SUBROUTINE CALCULATES THE EFFECTIVE BORN RADII USING THE
C     PAIRWISE DESCREENING METHOD.
C
C ON INPUT
C     RAC = COULOMB RADII
C     SMXPD = PAIRWISE DESCREENING APPROXIMATION FACTORS
C ON OUTPUT
C     BORN = EFFECTIVE BORN RADII
C
C     REVISITED AND ADDED DERIVATIVES OF BORN: DBORN(3,NUMAT,NUMAT),
C     DBORN(i,j,k) = dBORN(k)/dCOORD(i,j)                               DL0397
C******************************************************************************
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /ARACOM/ R(NPACK),R2(NPACK),ISAVE,DONE
     1               ,RAC(NUMATM),RAC2(NUMATM),DRAC(NUMATM,NUMATM)      DL0397
     2               ,RAS(NUMATM),RALG(NUMATM)
      COMMON /ONESCM/ ICONTR(100)
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),
     1                HBCORC(100),QKNOT(100),QKONE(100)
      COMMON /PDCOM/  SMXPD(100), SPSRFT(10)
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM), RMAX(NUMATM)
     1               ,URLIO(3,NUMATM,NUMATM)                            DL0397
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL0397
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL0397
     2               ,DATLGR(3,NUMATM)                                  DL0397
     3               ,LGR,FLAGRD                                        DL0397
                      LOGICAL LGR,FLAGRD                                DL0397
      COMMON /SCRCHR/ BUF(NUMATM)
      DIMENSION BORN(*),SMPD(100,100)
      LOGICAL DONE                                                      GDH0895
      SAVE SMPD
      DATA ZERO /0D0/                                                   DL0397
      IF (ICONTR(50).EQ.1) THEN
         ICONTR(50)=2
         DO 10 IJJ=1,100                                                GDH0496
         SMPD( 1,IJJ)=SMXPD(1)                                          GDH0496
         SMPD( 6,IJJ)=SMXPD(6)                                          GDH0496
         SMPD( 8,IJJ)=SMXPD(7)                                          GDH0496
         SMPD( 7,IJJ)=SMXPD(10)                                         GDH0496
         SMPD(15,IJJ)=SMXPD(16)                                         GDH0496
         SMPD(16,IJJ)=SMXPD(13)                                         GDH0496
         SMPD( 9,IJJ)=SMXPD(25)                                         GDH0496
         SMPD(17,IJJ)=SMXPD(26)                                         GDH0496
         SMPD(35,IJJ)=SMXPD(27)                                         GDH0496
         SMPD(53,IJJ)=SMXPD(28)                                         GDH0496
  10     CONTINUE
         SMPD(1,6)=SMXPD(2)                                             GDH0496
         SMPD(1,8)=SMXPD(3)                                             GDH0496
         SMPD(6,1)=SMXPD(4)                                             GDH0496
         SMPD(8,1)=SMXPD(5)                                             GDH0496
         SMPD(1,7)=SMXPD(8)                                             GDH0496
         SMPD(7,1)=SMXPD(9)                                             GDH0496
         SMPD(1,16)=SMXPD(11)                                           GDH0496
         SMPD(16,1)=SMXPD(12)                                           GDH0496
         SMPD(1,15)=SMXPD(14)                                           GDH0496
         SMPD(15,1)=SMXPD(15)                                           GDH0496
         SMPD(1,9)=SMXPD(17)                                            GDH0496
         SMPD(9,1)=SMXPD(18)                                            GDH0496
         SMPD(1,17)=SMXPD(19)                                           GDH0496
         SMPD(17,1)=SMXPD(20)                                           GDH0496
         SMPD(1,35)=SMXPD(21)                                           GDH0496
         SMPD(35,1)=SMXPD(22)                                           GDH0496
         SMPD(1,53)=SMXPD(23)                                           GDH0496
         SMPD(53,1)=SMXPD(24)                                           GDH0496
      ENDIF
      DO 100 IAT=1,NUMAT
      RADA=RAC(IAT)                                                     DJG0995
      IF (RADA.LE.0.D0) THEN                                            DL0397
         BORN(IAT)=0D0                                                  DL0397
         GOTO 80                                                        DL0397
      ENDIF                                                             DL0397
      IF (LGR) THEN                                                     DL0397
      DO 110 K=1,NUMAT                                                  GDH0597
         CALL SCOPY (3*NUMAT,ZERO,0,DBORN(1,1,K),1)                     DL0397
110   CONTINUE                                                          GDH0597
         CALL SCOPY (NUMAT,ZERO,0,BUF,1)                                DL0397
         DSANGI=0D0                                                     DL0397
      ENDIF                                                             DL0397
      RADAI=1D0/RADA                                                    DL0397
      RADAI2=RADAI**2                                                   DL0397
      SANGB=0.D0                                                        GDH0895
      DO 20 I=1,NUMAT                                                   GDH0895
      IF (I.EQ.IAT) GOTO 20                                             GDH0895
      RAB=RLIO(I,IAT)                                                   GDH0895
      RADB=RAC(I)*SMPD(NAT(I),NAT(IAT))
      VUP=RAB+RADB                                                      DL0397
      IF (RADA.LE.VUP) THEN                                             DL0397
         VUPI=1D0/VUP                                                   DL0397
         RABI=1D0/RAB                                                   DL0397
         VLOW=RAB-RADB                                                  DL0397
         IF (RADA.LE.VLOW) THEN                                         DL0397
            VLOWI=1D0/VLOW                                              DL0397
            TERM1=0.25D0*LOG(VUP*VLOWI)                                 DL0397
            TERM2=-0.5D0*RADB*VUPI*VLOWI                                DL0397
            SANGB=SANGB+TERM1*RABI+TERM2                                DL0397
            IF (LGR) THEN                                               DL0397
C              DERIVATIVES: DSANG = VS RAB; BUF(I) = VS RAC(I)          DL0397
               DSANG=TERM2*(RABI+4D0*RAB*TERM2/RADB)-TERM1*RABI**2      DL0397
               BUF(I)=SMPD(NAT(I),NAT(IAT))*(0.25D0*RABI*(VUPI+VLOWI)   DL0397
     .         +0.5D0*VUPI*VLOWI*(VUPI-VLOWI-1D0))                      DL0397
            ENDIF                                                       DL0397
         ELSE                                                           DL0397
            TERM2=0.125D0*(VUP*RADAI2-VUPI)                             DL0397
            TERM=0.25D0*LOG(VUP*RADAI)+VLOW*TERM2                       DL0397
            SANGB=SANGB+0.5D0*(VUPI-RADAI)+TERM*RABI                    DL0397
            IF (LGR) THEN                                               DL0397
C              DERIVATIVES: DSANG = VS RAB;                             DL0397
C                           BUF(IAT) = VS RAD(IAT)...(0.5)RADAI FACTOR  DL0397
               DTERM=0.25D0*VUPI+TERM2+0.125D0*VLOW*(RADAI2+VUPI**2)    DL0397
               DSANG=-0.5D0*VUPI**2+RABI*(DTERM-TERM*RABI)              DL0397
               BUF(IAT)=RADAI-RABI*(0.5D0+VUP*VLOW*RADAI2)+BUF(IAT)     DL0397
            ENDIF                                                       DL0397
         ENDIF                                                          DL0397
         IF (LGR) THEN                                                  DL0397
            DBORN(1,I  ,IAT)=DBORN(1,I  ,IAT)+DSANG*URLIO(1,IAT,I)      DL0397
            DBORN(2,I  ,IAT)=DBORN(2,I  ,IAT)+DSANG*URLIO(2,IAT,I)      DL0397
            DBORN(3,I  ,IAT)=DBORN(3,I  ,IAT)+DSANG*URLIO(3,IAT,I)      DL0397
            DBORN(1,IAT,IAT)=DBORN(1,IAT,IAT)-DSANG*URLIO(1,IAT,I)      DL0397
            DBORN(2,IAT,IAT)=DBORN(2,IAT,IAT)-DSANG*URLIO(2,IAT,I)      DL0397
            DBORN(3,IAT,IAT)=DBORN(3,IAT,IAT)-DSANG*URLIO(3,IAT,I)      DL0397
         ENDIF                                                          DL0397
      ENDIF                                                             DL0397
   20 CONTINUE                                                          GDH0895
      BORN(IAT)=RADAI+SANGB                                             GDH0895
   80 IF (BORN(IAT).LE.0.D0) THEN
         BORN(IAT)=1.0E16                                               DL0397
         IF (LGR) CALL SCOPY (3*NUMAT,ZERO,0,DBORN(1,1,IAT),1)          DL0397
      ELSE                                                              DL0397
         BORN(IAT)=1.0D0/BORN(IAT)                                      GDH0895
         IF (LGR) THEN                                                  DL0397
            BUF(IAT)=RADAI*(0.5D0*BUF(IAT)-RADAI)                       DL0397
            DO 90 K=1,NUMAT                                             DL0397
            DO 90 J=1,NUMAT                                             DL0397
            DBORN(1,J,IAT)=DBORN(1,J,IAT)+BUF(K)*DRAC(J,K)*URLIO(1,K,J) DL0397
            DBORN(2,J,IAT)=DBORN(2,J,IAT)+BUF(K)*DRAC(J,K)*URLIO(2,K,J) DL0397
            DBORN(3,J,IAT)=DBORN(3,J,IAT)+BUF(K)*DRAC(J,K)*URLIO(3,K,J) DL0397
   90       CONTINUE                                                    DL0397
            CALL DSCAL (3*NUMAT,-BORN(IAT)**2,DBORN(1,1,IAT),1)         DL0397
         ENDIF                                                          DL0397
      ENDIF                                                             DL0397
  100 CONTINUE                                                          GDH0895
      END
