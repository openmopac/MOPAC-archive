      SUBROUTINE DROT (N,X,IX,Y,IY,C,S)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GCL0493
      DIMENSION X(*),Y(*)
C ----------------------------------------------------------------------
C     APPLY A PLANE ROTATION TO REAL VECTORS (MIMIC BLAS STANDARD).
C  INPUT  N     NUMBER OF ELEMENTS IN VECTORS X AND Y.
C         X,IX  FIRST  VECTOR AND ITS STRIDE.
C         Y,IY  SECOND VECTOR AND ITS STRIDE.
C         C,S   COSINE AND SINE OF ROTATION ANGLE.
C  OUTPUT X,Y   ROTATED VECTORS SUCH THAT:
C               X(I)=C*X(I)+S*Y(I) AND Y(I)=-S*X(I)+C*Y(I)
C ----------------------------------------------------------------------
      J=1
      DO 10 I=1,IX*(N-1)+1,IX
      A=X(I)
      B=Y(J)
      Y(J)=C*B-S*A
      X(I)=C*A+S*B
   10 J=J+IY
      RETURN
      END
