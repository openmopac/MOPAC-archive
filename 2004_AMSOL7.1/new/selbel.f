      SUBROUTINE SELBEL(BUF,BPGT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'
      INCLUDE 'FFILES.i'                                                GDH1095
C******************************************************************************
C   THIS SUBROUTINE PRINTS OUT A SUMMARY OF THE SOLVATION ENERGY ON AN
C   ELEMENT BY ELEMENT BASIS.
C ON INPUT
C   BUF    = ATOM BY ATOM POLARIZATION ENERGY CONTRIBUTION (kcal)
C   SRFACT = ATOM BY ATOM CDS ENERGY CONTRIBUTION          (kcal)
C   ATLGAR = TOTAL SASA ASSOCIATED WITH LARGE SOLVENT RADIUS
C   HEXLGS = SURFACE TENSION ASSOCIATED WITH LARGE SOLVENT RADIUS
C ON OUTPUT
C   BPGT   = TOTAL POLARIZATION ENERGY (kcal)
C
C   CREATED BY DJG 0995 FROM EXISTING LINES IN WRITES
C   ADAPTATED TO NEW STORAGE CONVENTIONS                                DL0397
C******************************************************************************
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),
     1                QEFF2(NUMATM),DRVPOL(MPACK)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      LOGICAL WATSRP
      DIMENSION BUF(*)                                                  DL0397
      WATSRP=CISOLV.EQ.'H2OSRP'                                         DJG0995
      WRITE(JOUT, 1000)                                                 DJG0995
      BPGT=0.D0                                                         CC1290
      DO 100 M=1,100                                                    CC1290
         BPT=0.D0                                                       CC1290
         ST=0.D0                                                        CC1290
         DO 10 N=1,NUMAT                                                CC1290
            IF (NAT(N).EQ.M) THEN                                       CC1290
               BPT=BPT+BUF(N)                                           DL0397
               ST=ST+SRFACT(N)                                          GL0492
            ENDIF
   10    CONTINUE                                                       CC1290
         IF (ST.NE.0.D0.OR.BPT.NE.0.D0)                                 CC1290
     1   WRITE(JOUT, 1100) M,BPT,ST,BPT+ST                              GL0492
         BPGT=BPGT+BPT                                                  DL0397
  100 CONTINUE                                                          CC1290
      IF ((IRAD.EQ.6.AND..NOT.WATSRP).OR.IRAD.EQ.7.OR.IRAD.EQ.8.OR.     GDH0797
     .     IRAD.EQ.21.OR.                                               GDH0797
     .    ((IRAD.EQ.9.OR.IRAD.EQ.10).AND.NUMSLV.EQ.20)) THEN            GDH0997
         CSTOT=ATLGAR*HEXLGS                                            DJG1094
         WRITE(JOUT,1300) CSTOT,CSTOT                                   DJG1094
      ENDIF                                                             DJG1094
      WRITE(JOUT, 1200) BPGT, SURFCT, BPGT+SURFCT                       GL0492
1000  FORMAT(/,1X,'By element:',/)                                      GL0592
1100  FORMAT(1X,'Atomic #', I3, T15, 'Polarization:', T29, F7.2, T38,   GL0492
     1       'SS G_CDS:', T47, F7.2, T56, 'Total: ', T64, F7.2, ' kcal')PDW1199
1200  FORMAT(/,1X,'Total:', T29, F7.2, T47, F7.2, T64, F7.2, ' kcal')   GL0492
1300  FORMAT(1X,'Total LS contribution',T47,F7.2,T56,                   DJG1094
     1                              'Total: ', T64, F7.2, ' kcal')      DJG1094
      END
