      SUBROUTINE POLENM (BUFFER)
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        INCLUDE 'SIZES.i'
        INCLUDE 'FFILES.i'                                              GDH1095
C******************************************************************************
C
C   THIS SUBROUTINE CALCULATES AND PRINTS OUT THE POLARIZATION ENERGY
C   MATRIX FOR SOLVATION RUNS:
C
C   BUFFER(I,J) = -(1/2).(23.061).CELCOE.Q(I).Q(J)/Still(I,J)
C               = -(1/2).(23.061).       Q(I).Q(J)*FGB(I,J)             DL0397
C
C   CREATED BY DJG 0995 FROM EXISTING LINES IN WRITES
C   MADE CONSISTENT WITH STORAGE CONVENTIONS FOR FGB & QEFF,            DL0397
C   BUFFER = WORKING STORAGE.                                           DL0397
C******************************************************************************
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),
     1                QEFF(NUMATM),DRVPOL(MPACK)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /OPTIMI / IMP,IMP0
      DIMENSION BUFFER(NPACK)                                           DL0397
      AK=-23.061D0*0.5D0                                                DL0397
      IJ=0                                                              DL0397
      DO 100 I=1,NUMAT                                                  DL0397
         DO 100 J=1,I                                                   DL0397
            IJ=IJ+1                                                     DL0397
            BUFFER(IJ)=AK*QEFF(I)*QEFF(J)*FGB(IJ)                       DL0397
  100 CONTINUE                                                          DL0397
      WRITE(JOUT, 1000)                                                 DJG0995
      CALL VECPRT(BUFFER,NUMAT)                                         DL0397
1000  FORMAT(/,1X,'Generalized Born polarization energy decomposition', DJG0995
     1       ' (kcal)',/,1X,'Off-diagonal elements in the following ',  GL0492
     2       'table must be counted twice.')                            GL0492
      END
