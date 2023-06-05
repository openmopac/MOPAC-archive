      SUBROUTINE BFN(X,BF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION BF(13)
C**********************************************************************
C
C     BINTGS FORMS THE "B" INTEGRALS FOR THE OVERLAP CALCULATION.
C
C**********************************************************************
      DIMENSION FACT(17)
CSAV         SAVE                                                           GL0892
      DATA FACT/1.D0,2.D0,6.D0,24.D0,120.D0,720.D0,5040.D0,40320.D0,
     1362880.D0,3628800.D0,39916800.D0,479001600.D0,6227020800.D0,
     28.71782912D10,1.307674368D12,2.092278989D13,3.556874281D14/
      K=12
      IO=0
      ABSX = ABS(X)
      IF (ABSX.GT.3.D00) GO TO 40
      IF (ABSX.LE.2.D00) GO TO 10
      LAST=15
      GO TO 60
   10 IF (ABSX.LE.1.D00) GO TO 20
      LAST=12
      GO TO 60
   20 IF (ABSX.LE.0.5D00) GO TO 30
      LAST=7
      GO TO 60
   30 IF (ABSX.LE.1.D-6) GOTO 90
      LAST=6
      GO TO 60
   40 EXPX=EXP(X)
      EXPMX=1.D00/EXPX
      xinv = 1.0/x
      bf(1) = (expx - expmx)*xinv
      zk = 1.0
      DO 50 I=1,K
      zk = -1.0*zk
      bf(i+1) = (i*bf(i) + zk*expx - expmx)*xinv
50    continue
      return
   60 DO 80 I=IO,K
          zk = -x
          y = 2.0*mod(i+1,2)/(i+1)
          do 70 m=io+1,last
          y = y + 2.0*zk*mod(m+i+1,2)/(fact(m)*(m+i+1))
          zk = -x*zk
70        continue
   80 BF(I+1)=Y
      return
   90 DO 100 I=IO,K
  100 BF(I+1)=(2*MOD(I+1,2))/(I+1.D0)
      RETURN
C
      END
