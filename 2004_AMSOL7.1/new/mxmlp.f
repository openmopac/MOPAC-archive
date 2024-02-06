      SUBROUTINE MXMLP(A,B,C,NSZ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'                                                GDH1294
C     RECTANGULAR MATRIX PRODUCT C=A*B.
C     Emulates the mxm library routine on the Cray
      DIMENSION A(MAXORB,MAXORB),B(NSZ,NSZ),C(NSZ,NSZ)
      DO 20 J=1,NSZ
         DO 10 I=1,NSZ
   10    C(I,J)=0.D0
         DO 20 K=1,NSZ
            DO 20 I=1,NSZ
   20 C(I,J)=C(I,J)+A(I,K)*B(K,J)
      RETURN
      END
