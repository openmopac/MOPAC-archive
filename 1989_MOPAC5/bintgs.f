      SUBROUTINE BINTGS (X,K)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /SETC/ A(7),B(7),SDUM(3),IDUM(2)
      DIMENSION FACT(17)
C**********************************************************************
C
C     BINTGS FORMS THE "B" INTEGRALS FOR THE OVERLAP CALCULATION.
C
C**********************************************************************
      DATA FACT/1.D0,2.D0,6.D0,24.D0,120.D0,720.D0,5040.D0,40320.D0,
     1362880.D0,3628800.D0,39916800.D0,479001600.D0,6227020800.D0,
     28.71782912D10,1.307674368D12,2.092278989D13,3.556874281D14/
      IO=0
      ABSX = ABS(X)
      IF (ABSX.GT.3.D00) GO TO 40
      IF (ABSX.LE.2.D00) GO TO 10
      IF (K.LE.10) GO TO 40
      LAST=15
      GO TO 60
   10 IF (ABSX.LE.1.D00) GO TO 20
      IF (K.LE.7) GO TO 40
      LAST=12
      GO TO 60
   20 IF (ABSX.LE.0.5D00) GO TO 30
      IF (K.LE.5) GO TO 40
      LAST=7
      GO TO 60
   30 IF (ABSX.LE.1.D-6) GOTO 90
      LAST=6
      GO TO 60
   40 EXPX=EXP(X)
      EXPMX=1.D00/EXPX
      B(1)=(EXPX-EXPMX)/X
      DO 50 I=1,K
   50 B(I+1)=(I*B(I)+(-1.D00)**I*EXPX-EXPMX)/X
      GO TO 110
   60 DO 80 I=IO,K
         Y=0.0D00
         DO 70 M=IO,LAST
            XF=1.0D00
            IF(M.NE.0) XF=FACT(M)
   70    Y=Y+(-X)**M*(2*MOD(M+I+1,2))/(XF*(M+I+1))
   80 B(I+1)=Y
      GO TO 110
   90 DO 100 I=IO,K
  100 B(I+1)=(2*MOD(I+1,2))/(I+1.D0)
  110 CONTINUE
      RETURN
C
      END
