      SUBROUTINE SATBAT (BUF,CSTOT,IVSZT,IVSZMX,IVSZMN,NFILE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'KEYS.i'
      INCLUDE 'SIZES.i'
C******************************************************************************
C     THIS SUBROUTINE PRINTS OUT THE ATOM BY ATOM SUMMARY OF THE SOLVATION
C     CALCULATION.
C ON INPUT
C     ELEMNT = ELEMENT SYMBOL
C     Q      = PARTIAL NET ATOMIC CHARGE
C     BP     = CELCOE.FGB.Q  (eV)
C              SO THAT POLARIZATION ENERGY = -(1/2).Q'.BP (eV)
C     SRFACT = CDS ENERGY CONTRIBUTION      (kcal)
C     IVSZ   = NUMBER OF SHELLS TO ENCLOSE MOLECULE
C ON OUTPUT
C     BUF    = POLARIZATION ENERGY BY ATOM (kcal)                       DL0397
C
C   CREATED FROM EXISTING LINES BY DJG 0995
C   MADE CONSISTENT WITH NEW STORAGE CONVENTION FOR "Q", "BP"           DL0397
C******************************************************************************
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),
     1                Q(NUMATM),DRVPOL(MPACK)
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /IVSZCM/ IVSZ(NUMATM), IVSZC
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /ONESCM/ ICONTR(100)
      COMMON /AREACM/ NOPTI, TAREA, NINTEG
      LOGICAL WATSRP, TAREA
      CHARACTER*2 ELEMNT
      DIMENSION BUF(NUMATM)                                             DL0397
      SAVE
C
      IF (ICONTR(48).EQ.1) THEN
         ICONTR(48)=2
         WATSRP=CISOLV.EQ.'H2OSRP'                                      DJG0995
         TOTCH  = 0.D0                                                  GL0492
         TOTPFE = 0.D0                                                  GL0492
         TOTAR  = 0.D0                                                  GL0492
         TOTCDS = 0.D0                                                  GL0492
         TOTSFE = 0.D0                                                  GL0492
         IVSZT=0                                                        GDH0194
         IVSZMX=0                                                       GDH0194
         IVSZMN=IVSZ(1)                                                 GDH0194
         DO 150 L = 1, NUMAT                                            GL0492
             BUF(L) =-0.5D0*Q(L)*BP(L)*23.061D0                         DL0397
             IVSZT=IVSZT+IVSZ(L)                                        GDH0194
             IF (IVSZ(L).GT.IVSZMX) IVSZMX=IVSZ(L)                      GDH0194
             IF (IVSZ(L).LT.IVSZMN) IVSZMN=IVSZ(L)                      GDH0194
             TOTCH  = TOTCH  + Q(L)                                     DJG0995
             TOTPFE = TOTPFE + BUF(L)                                   DL0397
             TOTAR  = TOTAR  + ATAR(L)                                  GL0492
             TOTCDS = TOTCDS + SRFACT(L)                                GL0592
             TOTSFE = TOTSFE + BUF(L)+SRFACT(L)                         DL0397
150      CONTINUE                                                       GL0492
         IF ((IRAD.EQ.6.AND..NOT.WATSRP).OR.IRAD.EQ.7.OR.IRAD.EQ.8.OR.  GDH0797
     .        IRAD.EQ.21.OR.                                            GDH0797
     .    ((IRAD.EQ.9.OR.IRAD.EQ.10).AND.NUMSLV.EQ.20)) THEN            GDH0997
            CSTOT=ATLGAR*HEXLGS                                         DJG0195
            TOTCDS=TOTCDS + CSTOT                                       DJG1094
            TOTSFE=TOTSFE + CSTOT                                       DJG1094
         ENDIF                                                          DJG1094
      ENDIF
      IF (IEXTCM.NE.0) WRITE(NFILE,1500)                                DJG0995
      IF (ICHGM.EQ.2.OR.ICM1.NE.0) THEN                                 GDH1197
         IF (IEXTCM.NE.0) THEN                                          DJG0995
            WRITE(NFILE,1300) ' EXTCM  '                                DJG0895
         ELSE                                                           DJG0895
            WRITE(NFILE,1300) ' CM1    '                                DJG0495
         ENDIF                                                          DJG0895
      ELSE IF (ICHGM.EQ.3.OR.ICM2.NE.0) THEN                            GDH1197
         IF (IEXTCM.NE.0) THEN                                          GDH0997
            WRITE(NFILE,1300) ' EXTCM  '                                GDH0997
         ELSE                                                           GDH0997
            WRITE(NFILE,1300) ' CM2    '                                GDH0997
         ENDIF                                                          GDH0997
      ELSE                                                              GDH0997
         IF (IEXTCM.NE.0) THEN                                          GDH0997
            WRITE(NFILE,1000) ' EXTCM  '                                DJG0895
         ELSE IF (ISM505.EQ.1) THEN                                     GDH0497
            WRITE(NFILE,1000) 'SM5.05R '                                GDH0497
         ELSE IF (ISM50.EQ.1) THEN                                      GDH0497
            WRITE(NFILE,1000) 'SM5.0R  '                                GDH0497
         ELSE                                                           GDH0497
            WRITE(NFILE,1000) 'Mulliken'                                DJG0495
         ENDIF                                                          DJG0895
      ENDIF                                                             DJG1094

      IF (ISM505.EQ.1) THEN                                             GDH0297
      DO 202 L = 1, NUMAT                                               GL0492
      IF (ATAR(L) .LE. 1.0D-40) THEN                                    BJL1003
         WRITE(NFILE,1100)L, ELEMNT(NAT(L)), Q(L), BUF(L),              BJL1003
     1         ATAR(L),0.0D0, SRFACT(L),                                BJL1003
     2         BUF(L)+SRFACT(L),IVSZ(L)                                 BJL1003
      ELSE                                                              BJL1003
         WRITE(NFILE,1100)L, ELEMNT(NAT(L)), Q(L), BUF(L),              GDH0696
     1         ATAR(L),1000D0*SRFACT(L)/ATAR(L), SRFACT(L),             PDW1099
     2         BUF(L)+SRFACT(L),IVSZ(L)                                 GDH0197
      ENDIF                                                             BJL1003
202   CONTINUE                                                          GL0492
      WRITE(NFILE, 1400) TOTCH, TOTPFE , TOTAR, TOTCDS, TOTPFE+TOTCDS   GDH0197
      ELSE IF (ISM50.EQ.1) THEN                                         GDH0297
      DO 201 L = 1, NUMAT                                               GL0492
         IF (ATAR(L) .LE. 1.0D-40) THEN                                 BJL1003
         WRITE(NFILE,1100)L, ELEMNT(NAT(L)), Q(L), 0.D0,                BJL1003
     1         ATAR(L),0.0D0,SRFACT(L),SRFACT(L),0                      BJL1003
         ELSE                                                           BJL1003
         WRITE(NFILE,1100)L, ELEMNT(NAT(L)), Q(L), 0.D0,                GDH0696
     1         ATAR(L),1000D0*SRFACT(L)/ATAR(L),SRFACT(L),SRFACT(L),0   PDW1099
         ENDIF                                                          BJL1003
201   CONTINUE                                                          GL0492
      ELSE                                                              GDH0297
         DO 200 L = 1, NUMAT                                            GL0492
            IF (ATAR(L) .LE. 1.0D-40) THEN                              BJL1003
            WRITE(NFILE,1100)L, ELEMNT(NAT(L)), Q(L), BUF(L),           BJL1003
     1      ATAR(L),0.0D0,SRFACT(L),BUF(L)+SRFACT(L),                   BJL1003
     2      IVSZ(L)                                                     BJL1003
            ELSE                                                        BJL1003
            WRITE(NFILE,1100)L, ELEMNT(NAT(L)), Q(L), BUF(L),           DL0397
     1      ATAR(L),1000D0*SRFACT(L)/ATAR(L),SRFACT(L),BUF(L)+SRFACT(L),PDW1099
     2      IVSZ(L)                                                     PDW1099
            ENDIF                                                       BJL1003
200      CONTINUE                                                       GL0492
      ENDIF
         IF ((IRAD.EQ.6.AND..NOT.WATSRP).OR.IRAD.EQ.7.OR.IRAD.EQ.8.OR.  GDH0997
     .   IRAD.EQ.21.OR.((IRAD.EQ.9.OR.IRAD.EQ.10.).AND.NUMSLV.EQ.20))   GDH0997
     1    WRITE(NFILE,1200) ATLGAR, 1000D0*CSTOT/ATLGAR,CSTOT, CSTOT
          WRITE(NFILE,1400) TOTCH, TOTPFE, TOTAR, TOTCDS, TOTSFE
1000  FORMAT(/,1X,'In the following table subtotal= G_P + SS G_CDS.',   PDW1199
     1       /,/,2X,'Atom', T10, 'Chem.', T16, A8, T26,                 PDW1199
     2       'G_P', T35, 'Area', T45, 'Sigma k',T56,'SS G_CDS',T65,     PDW1199
     3       'Subtotal',T76,'M',/,1X,'number', T9,'symbol',             DJG1094
     4        T18,'chg.',T26,                                           PDW1099
     5       '(kcal)', T33, '(Ang**2)', T43,'cal/(Ang**2)',             PDW1099
     6        T57, '(kcal)', T66, '(kcal)',T74,'value',/)               DAL0303
1100  FORMAT(1X,I3,T12,A2,T15,F7.2,T23,F8.2,T33,F7.2,T44,F7.2,T55,F7.2, GDH0494
     1       T64,F7.2,T75,I3)                                           GDH0494
1200  FORMAT(/,1X,'LS Contribution',T33,F7.2,T44,F7.2,T55,F7.2,
     1       T64,F7.2)                                                  DJG1094
1300  FORMAT(/,1X,'In the following table subtotal= G_P + SS G_CDS.',   PDW1199
     1       /,/,2X,'Atom', T10, 'Chem.', T16, A8, T26,                 PDW1199
     2       'G_P', T35, 'Area', T45, 'Sigma k',T56,'SS G_CDS',T65,     PDW1199
     3       'Subtotal',T76,'M',/,1X,'number', T9,'symbol',             DJG1094
     4        T18,'chg.',T26,                                           PDW1099
     5       '(kcal)', T33, '(Ang**2)', T43,'cal/(Ang**2)',             PDW1099
     6        T57, '(kcal)', T66, '(kcal)',T74,'value',/)               PDW1099
1400  FORMAT(/,1X,'Total:',T15,F7.2,T23,F8.2,T33,F7.2,T55,F7.2,T64,F7.2)GDH0494
1500   FORMAT(/,'NOTE:  Since the keyword EXTCM was used, the ',        GDH0895
     1          'surface tensions applied in',/,'this calculation',     GDH0895
     2          'may not be consistant with the charge model input.',   GDH0895
     3          'The surface',/,'tensions used correspond to the ',     GDH0895
     4          'solvation model applied, and were developed with',     GDH0895
     5        /,'the standard charge model for the particular',         GDH0895
     6          'solvation model.')                                     GDH0895
      RETURN
      END
