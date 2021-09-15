      SUBROUTINE CNVG(PNEW, P, P1,NORBS, NITER, PL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P1(*), P(*), PNEW(*)
      LOGICAL EXTRAP
C***********************************************************************
C
C  CNVG IS A TWO-POINT INTERPOLATION ROUTINE FOR SPEEDING CONVERGENCE
C       OF THE DENSITY MATRIX.
C
C ON OUTPUT P      = NEW DENSITY MATRIX
C           P1     = DIAGONAL OF OLD DENSITY MATRIX
C           PL     = LARGEST DIFFERENCE BETWEEN OLD AND NEW DENSITY
C                    MATRICES
C***********************************************************************
C#      DATA DAMP/2.D0/
      PL=0.0D00
      FACA=0.0D00
      DAMP=1.D10
      IF(NITER.GT.3)DAMP=0.05D0
      FACB=0.0D00
      FAC=0.0D00
      II=MOD(NITER,3)
      EXTRAP=II.NE.0
      K=0
      DO 30 I=1,NORBS
         K=K+I
         A=PNEW(K)
         SA=ABS(A-P(K))
         IF (SA.GT.PL) PL=SA
         IF (EXTRAP) GO TO 20
         FACA=FACA+SA**2
         FACB=FACB+(A-2.D00*P(K)+P1(I))**2
   20    P1(I)=P(K)
   30 P(K)=A
      IF (FACB.LE.0.0D00) GO TO 40
      IF (FACA.LT.(100.D00*FACB)) FAC=SQRT(FACA/FACB)
   40 IE=0
      DO 80 I=1,NORBS
         II=I-1
         DO 60 J=1,II
            IE=IE+1
            A=PNEW(IE)
                P(IE)=A+FAC*(A-P(IE))
            PNEW(IE)=P(IE)
   60    CONTINUE
      IE=IE+1
            IF(ABS(P(IE)-P1(I)) .GT. DAMP) THEN
                P(IE)=P1(I)+SIGN(DAMP,P(IE)-P1(I))
            ELSE
                P(IE)=P(IE)+FAC*(P(IE)-P1(I))
            ENDIF
  80  PNEW(IE)=P(IE)
      RETURN
C
      END
