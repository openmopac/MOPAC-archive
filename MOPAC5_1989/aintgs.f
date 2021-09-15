      SUBROUTINE AINTGS (X,K)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /SETC/ A(7),B(7),SDUM(3),IDUM(2)
C***********************************************************************
C
C    AINTGS FORMS THE "A" INTEGRALS FOR THE OVERLAP CALCULATION.
C
C***********************************************************************
      C=EXP(-X)
      A(1)=C/X
      DO 10 I=1,K
         A(I+1)=(A(I)*I+C)/X
   10 CONTINUE
      RETURN
C
      END
