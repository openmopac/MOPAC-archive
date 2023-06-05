      SUBROUTINE MTXM (A,NAR,B,NBR,C,NCC) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     MATRIX PRODUCT C(NAR,NCC) = (A(NBR,NAR))' * B(NBR,NCC) 
C     ALL MATRICES RECTANGULAR , PACKED. 
C  NOTE ... BEST VERSION ON CRAY-1: 1.4 HAS BEEN EMPIRICALLY ADJUSTED. 
      DIMENSION A(NBR,NAR),B(NBR,NCC),C(NAR,NCC) 
C     DO 10 I=1,NCC*NAR 
C  10 C(I,1)=0.D0 
      DO 10 I = 1, NCC
      DO 10 J = 1, NAR
   10    C(J,I) = 0D0
  
      IF (DBLE(NBR).LT.1.4D0*DBLE(NAR+NCC)) THEN                        GCL0393
         DO 20 K=1,NBR 
         DO 20 J=1,NCC 
         DO 20 I=1,NAR 
   20    C(I,J)=C(I,J)+A(K,I)*B(K,J) 
      ELSE 
         DO 30 I=1,NAR 
         DO 30 J=1,NCC 
         DO 30 K=1,NBR 
   30    C(I,J)=C(I,J)+A(K,I)*B(K,J) 
      ENDIF 
      RETURN 
      END 
