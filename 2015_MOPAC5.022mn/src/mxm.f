      SUBROUTINE MXM(A,NAR,B,NBR,C,NCC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     RECTANGULAR MATRIX PRODUCT C=A*B.
C     EMULATES THE MXM LIBRARY ROUTINE ON THE CRAY
      DIMENSION A(NAR,NBR),B(NBR,NCC),C(NAR,NCC)
      DO 20 J=1,NCC
         DO 10 I=1,NAR
   10    C(I,J)=0.D0
         DO 20 K=1,NBR
            DO 20 I=1,NAR
   20 C(I,J)=C(I,J)+A(I,K)*B(K,J)
      RETURN
      END

C     SUBROUTINE MXM(A,NAR,B,NAC,C,NBC)
C
C    Routine on VAX to emulate CRAY SCILIB routine, MXM p. 4-22
C         LIBRARY manual, implemented in VAX single precision.
C
C     INTEGER NAR,NAC,NBC,I,J,K
C     DOUBLE PRECISION A(NAR,1),B(NAC,1),C(NAR,1)

C     DO 30 J=1,NBC
C        DO 20 I = 1,NAR
C           C(I,J) = 0.0
C           DO 10 K = 1,NAC
C              C(I,J) = C(I,J) + A(I,K)*B(K,J)
C  10       CONTINUE
C  20    CONTINUE
C  30 CONTINUE
C     RETURN
C     END
