      SUBROUTINE OSINV (A,N,D)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(*)
************************************************************************
*
*    OSINV INVERTS A GENERAL SQUARE MATRIX OF ORDER UP TO 100. SEE
*          DIMENSION STATEMENTS BELOW.
*
*   ON INPUT       A = GENERAL SQUARE MATRIX STORED LINEARLY.
*                  N = DIMENSION OF MATRIX A.
*                  D = VARIABLE, NOT DEFINED ON INPUT.
*
*   ON OUTPUT      A = INVERSE OF ORIGINAL A.
*                  D = DETERMINANT OF ORIGINAL A, UNLESS A WAS SINGULAR,
*                      IN WHICH CASE D = 0.0
*
************************************************************************
      DIMENSION L(100), M(100)
************************************************************************
*
*    IF THE VALUE OF TOL GIVEN HERE IS UNSUITABLE, IT CAN BE CHANGED.
      TOL=1.D-8
*
*
************************************************************************
      D=1.0
      NK=-N
      DO 180 K=1,N
         NK=NK+N
         L(K)=K
         M(K)=K
         KK=NK+K
         BIGA=A(KK)
         DO 20 J=K,N
            IZ=N*(J-1)
            DO 20 I=K,N
               IJ=IZ+I
C
C     10 FOLLOWS
C
               IF (ABS(BIGA)-ABS(A(IJ))) 10,20,20
   10          BIGA=A(IJ)
               L(K)=I
               M(K)=J
   20    CONTINUE
         J=L(K)
         IF (J-K) 50,50,30
   30    KI=K-N
         DO 40 I=1,N
            KI=KI+N
            HOLO=-A(KI)
            JI=KI-K+J
            A(KI)=A(JI)
   40    A(JI)=HOLO
   50    I=M(K)
         IF (I-K) 80,80,60
   60    JP=N*(I-1)
         DO 70 J=1,N
            JK=NK+J
            JI=JP+J
            HOLO=-A(JK)
            A(JK)=A(JI)
   70    A(JI)=HOLO
   80    IF (ABS(BIGA)-TOL) 90,100,100
   90    D=0.0
         RETURN
  100    DO 120 I=1,N
            IF (I-K) 110,120,110
  110       IK=NK+I
            A(IK)=A(IK)/(-BIGA)
  120    CONTINUE
         DO 150 I=1,N
            IK=NK+I
            IJ=I-N
            DO 150 J=1,N
               IJ=IJ+N
               IF (I-K) 130,150,130
  130          IF (J-K) 140,150,140
  140          KJ=IJ-I+K
               A(IJ)=A(IK)*A(KJ)+A(IJ)
  150    CONTINUE
         KJ=K-N
         DO 170 J=1,N
            KJ=KJ+N
            IF (J-K) 160,170,160
  160       A(KJ)=A(KJ)/BIGA
  170    CONTINUE
         D=D*BIGA
         A(KK)=1.0/BIGA
  180 CONTINUE
      K=N
  190 K=K-1
      IF (K) 260,260,200
  200 I=L(K)
      IF (I-K) 230,230,210
  210 JQ=N*(K-1)
      JR=N*(I-1)
      DO 220 J=1,N
         JK=JQ+J
         HOLO=A(JK)
         JI=JR+J
         A(JK)=-A(JI)
  220 A(JI)=HOLO
  230 J=M(K)
      IF (J-K) 190,190,240
  240 KI=K-N
      DO 250 I=1,N
         KI=KI+N
         HOLO=A(KI)
         JI=KI+J-K
         A(KI)=-A(JI)
  250 A(JI)=HOLO
      GO TO 190
  260 RETURN
C
      END
