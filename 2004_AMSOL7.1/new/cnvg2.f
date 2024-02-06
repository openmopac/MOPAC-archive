      SUBROUTINE CNVG2(PNEW,P,P1,WORK,NORBS,NCNVG,PL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GCL0493
      DIMENSION P1(*),P(*),PNEW(*),WORK(*)
      LOGICAL OKBOSS
C ----------------------------------------------------------------------
C     TWO-POINT DAMPED INTER/EXTRAPOLATION OF THE DENSITY MATRIX.
C     PRODUCE A PREDICTED DENSITY EVERY THREE CYCLES.
C INPUT  PNEW = NEW DENSITY MATRIX.
C        P    = PREVIOUS DENSITY MATRIX.
C        WORK = WORK ARRAY.
C        NCNVG= NUMBER OF CONSECUTIVE CALLS TO CNVG.
C OUTPUT PNEW = NEW DENSITY MATRIX, INTER/EXTRAPOLATED.
C        P    = DUPLICATUM OF PNEW WHEN INPUT.
C        P1   = DIAGONAL OF P WHEN INPUT.
C        PL   = LARGEST DIFFERENCE BETWEEN OLD AND NEW DENSITY MATRICES
C               (DIAGONAL ELEMENTS).
C ----------------------------------------------------------------------
      OKBOSS=NCNVG.GT.2.AND.MOD(NCNVG,3).EQ.0
      PL=0.D0
      IF(NCNVG.LT.4) PL=2.D0
      FACA=0.D0
      FACB=1.D-10
      K=0
      DO 10 I=1,NORBS
      K=K+I
      SA=ABS(PNEW(K)-P(K))
      PL=MAX(SA,PL)
      IF (OKBOSS) THEN
         FACA=FACA+SA**2
         FACB=FACB+(PNEW(K)-2.D0*P(K)+P1(K))**2
      ENDIF
   10 P1(K)=P(K)
      IF (OKBOSS) THEN
C        INTER/EXTRAPOLATE THE DENSITY MATRIX.
         FAC=SQRT(FACA/FACB)
         DO 20 I=1,K
   20    WORK(I)=PNEW(I)+FAC*(PNEW(I)-P(I))
C        DAMPING ON DIAGONAL ELEMENTS.
C        DAMP=5D1/FLOAT(10+NCNVG)**2
         DAMP=5D1/DBLE(10+NCNVG)**2
         K=0
         DO 30 I=1,NORBS
         K=K+I
         IF (ABS(WORK(K)-P(K)).GT.DAMP) THEN
            WORK(K)=0.5D0*(PNEW(K)+P(K))
         ENDIF
   30    CONTINUE
         DO 40 I=1,K
         P(I)=PNEW(I)
   40    PNEW(I)=WORK(I)
      ELSE
         CALL SCOPY (K,PNEW,1,P,1)
      ENDIF
      RETURN
      END
