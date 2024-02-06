      SUBROUTINE SASARD
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
C ---------------------------------------------------------------------+
C     CALCULATE THE RADII FOR THE SASA CALCULATION (BETA-K)
C  ON INPUT
C     RKCDS  = ATOMIC RADIUS FOR CDS TERM (R-K)
C     SLVRAD = SMALL SOLVENT RADIUS
C     SLVRD2 = LARGE SOLVENT RADIUS
C  ON OUTPUT
C     RAS    = CDS RADII (BETA-K)
C     RALG   = CDS RADII ASSOCIATED WITH LARGE SOLVENT RADIUS
C
C     CREATED BY DJG 0995 FROM EXISTING LINES IN SMX1 AND SRFCTY
C ---------------------------------------------------------------------+
      COMMON /ARACOM/ R(NPACK),R2(NPACK),ISAVE,DONE
     1               ,RAC(NUMATM),RAC2(NUMATM),DRAC(NUMATM,NUMATM)      DL0397
     2               ,RAS(NUMATM),RALG(NUMATM)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /SOLVCM/ CELEID, SLVRAD, SLVRD2, CSSIGM
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),
     1                HBCORC(100),QKNOT(100),QKONE(100)
      LOGICAL DONE
      DO 100 I=1,NUMAT                                                  GDH1293
         RAS(I)=RKCDS(NAT(I))+SLVRAD                                    DJG0995
100   CONTINUE                                                          GDH1293
      IF (IRAD.EQ.6.OR.IRAD.EQ.7.OR.IRAD.EQ.8.OR.IRAD.EQ.21.OR.         GDH0997
     .    ((IRAD.EQ.9.OR.IRAD.EQ.10).AND.NUMSLV.EQ.20)) THEN            GDH0997
         DO 200 I=1,NUMAT                                               DJG1094
            RALG(I)=RKCDS(NAT(I))+SLVRD2                                DJG0995
200      CONTINUE                                                       DJG1094
      ENDIF                                                             DJG1094
      END
