      SUBROUTINE LOADSP
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
         INCLUDE 'SIZES.i'
         INCLUDE 'KEYS.i'
         INCLUDE 'FFILES.i'                                             GDH1095
C******************************************************************************
C
C   THIS SUBROUTINE LOADS IN THE SOLVENT PARAMETERS FROM PARAMS.i AND THE
C   EXTSM FILE IF NEEDED.
C
C       SIGMA0 = SURFACE TENSIONS (SIGMA ZERO)
C       RKCDS = SURFACE TENSION RADII (Rk)
C       RHONOT = COULOMB RADII PART I "rho zero"
C       RHOONE = COULOMB RADII PART II "rho one"
C       QKNOT = PART OF COULOMB RADII CALC "q not"
C       QKONE = PART OF COULOMB RADII CALC "q one"
C       HBCORC = SURFACE TENSION CORRECTION FOR H-BONDS
C                 (SM2(.X) AND SM3(.X))
C                                                                       DJG0995
C
C   CREATED FROM EXISTING LINES BY DJG 0995
C
C******************************************************************************
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /IVSZCM/ IVSZ(NUMATM), IVSZC
      COMMON /RADCOM/ SRFACI(NUMATM)
      COMMON /SOLVCM/ CELEID, SLVRAD, SLVRD2, CSSIGM                    DJG1094
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),
     1                HBCORC(100),QKNOT(100),QKONE(100)
      CALL LOADIN
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (IEXTSM.NE.0) CALL EXTSMX
      DO 100 I=1,NUMAT                                                  GDH1293
         IF (IVSZC.EQ.0) IVSZ(I)=0                                      GDH0294
         IF (IRAD.NE.1) THEN                                            GDH1293
            SRFACI(I)=SIGMA0(NAT(I))                                    DJG0995
         ELSE                                                           GDH1293
            SRFACI(I)=SIGMA0(ITYPE(I))                                  DJG0995
         ENDIF                                                          GDH1293
100   CONTINUE                                                          GDH1293
      IF (IRAD.EQ.6.OR.IRAD.EQ.7.OR.IRAD.EQ.8.OR.IRAD.EQ.21.OR          GDH0997
     .    .IRAD.EQ.9.OR.IRAD.EQ.10)                                     GDH0997
     .    HEXLGS=CSSIGM/1000.0D0                                        DJG0896
      RETURN
      END
