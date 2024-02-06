      SUBROUTINE CCT1DO(QEFF)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
         INCLUDE 'SIZES.i'
C******************************************************************************
C
C   THIS SUBROUTINE CALCULATES THE ENERGY CONTRIBUTION TERM CCT1.  THE
C   MEANING OF THIS TERM VARIES BETWEEN SOLVATION MODELS.
C
C   CREATED FROM EXISTING LINES IN BORNPL, BORNPL2, AND CM1DRV BY
C   DJG 0995
C
C******************************************************************************
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),
     1                QEFF2(NUMATM),DRVPOL(MPACK)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS, NALPHA, NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /SOLVCM/ CELEID, SLVRAD, SLVRD2, CSSIGM
      COMMON /CORE  / CORE(107)
      COMMON /ONESCM/ ICONTR(100)
      CCT1=0.0D0                                                        DJG0995
      IF (ICONTR(33).EQ.1) THEN
         ICONTR(33)=2
         DO 50 I=1,NUMAT                                                DJG0995
            ZEFF(I)=CORE(NAT(I))                                        DJG0995
50       CONTINUE                                                       DJG0995
      ENDIF
      IF (ICHMOD.EQ.0) THEN                                             GDH0997
         CCT1=-0.5D0*DOT(ZEFF,BP,NUMAT)                                 GDH0895
      ELSE                                                              GDH0895
         DO 100 I=1,NUMAT                                               DJG0995
            CCT1=CCT1+BP(I)                                             JWS0994
100      CONTINUE                                                       DJG0995
         CCT1=-7.1983D0*(1.D0-CELEID)*CCT1                              DJG0995
      ENDIF                                                             GDH0895
      CCT1=CCT1+SURFCT/23.061D0                                         DL0397
      RETURN
      END

