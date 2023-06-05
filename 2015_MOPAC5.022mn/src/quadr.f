      SUBROUTINE QUADR(F0,F1,F2, X1,X2, A,B,C,*)                        CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CSAV         SAVE                                                           GL0892
****************************************************
*                                                  *
*    QUADR CALCULATES THE A, B AND C IN THE EQUNS. *
*                                                  *
*     A                   =   F0                   *
*     A + B.X0 + C.X0**2  =   F1                   *
*     A + B.X2 + C.X2**2  =   F2                   *
*                                                  *
****************************************************
      C=(X2*(F1-F0)-X1*(F2-F0))/(X2*X1**2-X1*X2**2)
      B=(F1-F0-C*X1**2)/X1
      A=F0
      RETURN
 9999 RETURN 1                                                          CSTP
      END
