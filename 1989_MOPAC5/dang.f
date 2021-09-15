      SUBROUTINE DANG(A1,A2,B1,B2,RCOS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
**********************************************************************
*
*    DANG  DETERMINES THE ANGLE BETWEEN THE POINTS (A1,A2), (0,0),
*          AND (B1,B2).  THE RESULT IS PUT IN RCOS.
*
**********************************************************************
      PI=2.0D0* ASIN(1.0D00)
      ZERO=1.0D-6
      IF( ABS(A1).LT.ZERO.AND. ABS(A2).LT.ZERO) GO TO 10
      IF( ABS(B1).LT.ZERO.AND. ABS(B2).LT.ZERO) GO TO 10
      ANORM=1.0D0/ SQRT(A1**2+A2**2)
      BNORM=1.0D0/ SQRT(B1**2+B2**2)
      A1=A1*ANORM
      A2=A2*ANORM
      B1=B1*BNORM
      B2=B2*BNORM
      SINTH=(A1*B2)-(A2*B1)
      COSTH=A1*B1+A2*B2
      IF(COSTH.GT.1.0D0) COSTH=1.0D0
      IF(COSTH.LT.-1.0D0) COSTH=-1.0D0
      RCOS= ACOS(COSTH)
      IF( ABS(RCOS).LT.4.0D-4) GO TO 10
      IF(SINTH.GT.0.D0) RCOS=4.0D0* ASIN(1.0D00)-RCOS
      RCOS=-RCOS
      RETURN
   10 RCOS=0.0D0
      RETURN
      END
