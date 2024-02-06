      SUBROUTINE CALCBP (QEFF)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
C ----------------------------------------------------------------------DL0397
C     CALCULATE THAT PART OF THE TOTAL SOLVATION ENERGY FOR SMs         DL0397
C     WHICH IS NOT ACCOUNTED FOR IN "ITER" via THE FOCK OPERATOR.       DL0397
C ON INPUT                                                              DL0397
C     QEFF   = ATOMIC NET CHARGES (Mulliken, CM1, or CM2)               GDH0897
C              OR CM3                                                   !JT0802
C     FGB    = CELCOE/Still's FACTORS (eV)                              DL0397
C     SURFCT = G-CD TOTAL ENERGY (kcal)                                 DL0397
C ON OUTPUT                                                             DL0397
C     CCT1 = TOTAL SOLVATION ENERGY (eV)                                DL0397
C                                                                       DL0397
C     CREATED BY DJG 0995, REVISITED BY DL 0397.                        DL0397
C ----------------------------------------------------------------------DL0397
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS, NALPHA, NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),
     1                QEFF2(NUMATM),DRVPOL(MPACK)
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /ONESCM/ ICONTR(100)
      COMMON /SOLVCM/ CELEID, SLVRAD, SLVRD2, CSSIGM
      COMMON /CORE  / CORE(107)
      DIMENSION QEFF(*)
      IF (ICONTR(55).EQ.1) THEN
         ICONTR(55)=2
         DO 10 I=1,NUMAT                                                DJG0995
            ZEFF(I)=CORE(NAT(I))                                        DJG0995
   10    CONTINUE                                                       DJG0995
      ENDIF
C
C     NOTE: "FGB" PROVIDES CELCOE/Still's FACTORS                       DL0397
C     BP = FGB.QEFF                                                     DL0397
C
      CALL SUPDOT (BP,FGB,QEFF,NUMAT,1)                                 DL0397
C
C     CALCULATE THE SOLVATION ENERGY CONTRIBUTION: CCT1 (eV)            DL0397
C
      IF (ICHMOD.EQ.1) THEN                                             GDH0895
C        CM1 CHARGES ARE IN USE:                                        DL0397
         CCT1=SURFCT/23.061D0-0.5D0*DOT(QEFF,BP,NUMAT)                  DL0397
C        SET DRVPOL, DERIVATIVES VS DENSITY, FOR FOCK OPERATOR.         DL0397
         CALL CM1DRV                                                    DL0397
      ELSE IF (ICHMOD.EQ.2) THEN                                        GDH0997
C        CM2 CHARGES ARE IN USE:                                        GDH0997
         CCT1=SURFCT/23.061D0-0.5D0*DOT(QEFF,BP,NUMAT)                  GDH0997
         CALL CALVK(QEFF)                                               GDH0997
         CALL GBSCRF                                                    GDH0997
      ELSE                                                              GDH0895
C        MULLIKEN CHARGES ARE IN USE:
         CCT1=SURFCT/23.061D0-0.5D0*DOT(ZEFF,BP,NUMAT)                  DL0397
      ENDIF                                                             GDH0895
      END
