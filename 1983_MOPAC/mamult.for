      SUBROUTINE MAMULT(A,B,C,N,ONE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(*),B(*),C(*)
*************************************************************************
*
*   MAMULT MULTIPLIES A BY B AND PUTS THE RESULT IN C
*
*************************************************************************
      L=0
      DO 1 I=1,N
      II=((I-1)*I)/2
      DO 1 J=1,I
      JJ=((J-1)*J)/2
      L=L+1
      SUM=0.D0
      DO 2 K=1,J
   2  SUM=SUM+A(II+K)*B(JJ+K)
      DO 3 K=J+1,I
   3  SUM=SUM+A(II+K)*B(((K-1)*K)/2+J)
      DO 4 K=I+1,N
      KK=(K*(K-1))/2
   4  SUM=SUM+A(KK+I)*B(KK+J)
   1  C(L)=SUM+ONE*C(L)
      RETURN
      END

