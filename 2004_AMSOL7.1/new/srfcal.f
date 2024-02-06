      SUBROUTINE SRFCAL
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'
C ---------------------------------------------------------------------
C     THIS SUBROUTINE CALCULATES SURFACE TENSIONS WHICH ARE LATER
C     MULTIPLIED TIMES SASA TO GET THE CDS TERM.
C
C     CREATED BY DJG 0995 FROM EXISTING LINES IN BORNPL AND BRNPL2
C     CHECKED BY  DL 0397 VS ANALYTICAL GRADIENT CAPABILITY.
C ---------------------------------------------------------------------
      COMMON /HBORDS/ HBORD(NUMATM), HB(NUMATM)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS, NALPHA, NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL0397
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL0397
     2               ,DATLGR(3,NUMATM)                                  DL0397
     3               ,LGR,FLAGRD                                        DL0397
                      LOGICAL LGR,FLAGRD                                DL0397
      DATA ZERO /0.D0/
      ISMHB=ISM2+ISM3+ISM21+ISM31+ISM22+ISM23                           GDH0296
      IF (ISMHB.GE.1) THEN                                              GDH0296
C        BOND ORDER DEPENDANT, NOT SELF-CONSISTENT: NO GRADIENT.        DL0397
         FLAGRD=.FALSE.                                                 DL0397
         LGR=.FALSE.                                                    DL0397
         CALL SMHB                                                      DJG0995
      ELSE IF (IRAD.EQ.4.OR.IRAD.EQ.5.OR.IRAD.EQ.7.OR.IRAD.EQ.8) THEN   DJG0896
C        GEOMETRY DEPENDANT, SELF-CONSISTENT: GRADIENT AVAILABLE.       DL0397
         CALL SM5HB                                                     DL0397
      ELSE IF (IRAD.EQ.6) THEN                                          DJG1094
C        BOND ORDER DEPENDANT, NOT SELF-CONSISTENT: NO GRADIENT.        DL0397
         FLAGRD=.FALSE.                                                 DL0397
         LGR=.FALSE.                                                    DL0397
         CALL SM4HB
      ELSEIF (IRAD.EQ.20.OR.IRAD.EQ.21.OR.IRAD.EQ.9.OR.IRAD.EQ.10) THEN GDH0997
C        GEOMETRY DEPENDANT, SELF-CONSISTENT: GRADIENT IMPLEMENTED.     DAL0303
         CALL SM5RHB                                                    DAL0303
      ELSE                                                              DJG0995
         CALL SCOPY (NUMAT,ZERO,0,HB,1)                                 DL0397
      ENDIF
      END
