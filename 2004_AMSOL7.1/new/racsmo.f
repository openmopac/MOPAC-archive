      SUBROUTINE RACSMO
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
         INCLUDE 'SIZES.i'
C******************************************************************************
C
C    THIS SUBROUTINE CALCULATES THE SMOOTHING FACTOR FOR CS3 CALCULATIONS
C
C    ON INPUT -
C    RAC = COULOMB RADII
C    RAL = 'SMOOTHING' FUNCTION
C
C    ON OUTPUT -
C    RAC = 'SMOOTHED' COULOMB RADII
C
C    CREATED BY DJG 0995 FROM EXISTING LINES IN SRFCTY
C
C    WARNING: ANALYTICAL DERIVATIVES FOR RAC NOT IMPLEMENTED HERE       DL0397
C******************************************************************************
      COMMON /SMOOTH/ RAL(NUMATM)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /ARACOM/ R(NPACK),R2(NPACK),ISAVE,DONE
     1               ,RAC(NUMATM),RAC2(NUMATM),DRAC(NUMATM,NUMATM)      DL0397
     2               ,RAS(NUMATM),RALG(NUMATM)
      COMMON /AREACM/ NOPTI, TAREA, NINTEG
      LOGICAL DONE,TAREA
      DO 100 I=1,NUMAT
C GDH0597         IF (NOPTI.NE.3) THEN                                  GDH0394
            IF(RAL(I).EQ.0) THEN
               RAL(I)=RAC(I)
            ELSE
               RAC(I)=(RAC(I)+RAL(I))/2.D0
               RAL(I)=RAC(I)
            ENDIF
C GDH0597         ENDIF                                                 GDH0394
         RAC2(I)=RAC(I)*RAC(I)
100   CONTINUE
      RETURN
      END
