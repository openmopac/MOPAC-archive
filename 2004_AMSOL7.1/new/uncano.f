      SUBROUTINE UNCANO(PAC,N,UNPAC,NDIM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GCL0493
      DIMENSION PAC(*),UNPAC(NDIM,*)
C ---------------------------------------------------------------------
C     PACK/UNPACK ROUTINE FOR SYMMETRIC SQUARE MATRIX.
C ---------------------------------------------------------------------
C     FROM PAC (CANONICAL) TO UNPAC (SYMMETRIZED)
      UNPAC(1,1)=PAC(1)
      K=1
      DO 20 I=2,N
      DO 10 J=1,I-1
      K=K+1
      UNPAC(I,J)=PAC(K)
   10 UNPAC(J,I)=PAC(K)
      K=K+1
   20 UNPAC(I,I)=PAC(K)
      RETURN
      ENTRY CANO (PAC,N,UNPAC,NDIM)
C     FROM UNPAC (LOWER LEFT TRIANGLE) TO PAC (CANONICAL)
      PAC(1)=UNPAC(1,1)
      K=1
      DO 40 I=2,N
      DO 30 J=1,I-1
      K=K+1
   30 PAC(K)=UNPAC(I,J)
      K=K+1
   40 PAC(K)=UNPAC(I,I)
      RETURN
      END
