      SUBROUTINE tqlrat (N,D,E2,IERR,*)                                 CSTP
C
C***********************************************************************
C  TQLRAT
C***********************************************************************
C
C     CALLED BY:
C                RSP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION D(N),E2(N)
      DOUBLE PRECISION B,C,F,G,H,P,R,S,MACHEP
C
C     REAL SQRT,ABS,SIGN
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQLRAT,
C     ALGORITHM 464, COMM. ACM 16, 689(1973) BY REINSCH.
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC
C     TRIDIAGONAL MATRIX BY THE RATIONAL QL METHOD.
C
C     ON INPUT-
C
C        N IS THE ORDER OF THE MATRIX,
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX,
C
C        E2 CONTAINS THE SQUARES OF THE SUBDIAGONAL ELEMENTS OF THE
C          INPUT MATRIX IN ITS LAST N-1 POSITIONS.  E2(1) IS ARBITRARY.
C
C      ON OUTPUT-
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND
C          ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE
C          THE SMALLEST EIGENVALUES,
C
C        E2 HAS BEEN DESTROYED,
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
C                **********
C
      DATA ZERO,ONE,TWO / 0.0D0,1.0D0,2.0D0 /
C
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
C     MACHEP = TWO**(-46)
      MACHEP = TWO**(-45)                                               0519WH93
C
      IERR = 0
      IF (N.EQ.1) GO TO 140
C
      DO 10 I = 2, N
         E2(I-1) = E2(I)
   10 CONTINUE
C
      F = ZERO
      B = ZERO
      E2(N) = ZERO
C
      DO 120 L = 1, N
         J = 0
         H = MACHEP*(ABS(D(L))+SQRT(E2(L)))
         IF (B.GT.H) GO TO 20
         B = H
         C = B*B
C
C     ********** LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT **********
C
   20    DO 30 M = L, N
            IF (E2(M).LE.C) GO TO 40
C
C     ********** E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
C
   30    CONTINUE
C
   40    IF (M.EQ.L) GO TO 80
   50    IF (J.EQ.30) GO TO 130
         J = J+1
C
C     ********** FORM SHIFT **********
C
         L1 = L+1
         S = SQRT(E2(L))
         G = D(L)
         P = (D(L1)-G)/(TWO*S)
         R = SQRT(P*P+ONE)
         D(L) = S/(P+SIGN(R,P))
         H = G-D(L)
C
         DO 60 I = L1, N
            D(I) = D(I)-H
   60    CONTINUE
C
         F = F+H
C
C     ********** RATIONAL QL TRANSFORMATION **********
C
         G = D(M)
         IF (G.EQ.ZERO) G = B
         H = G
         S = ZERO
         MML = M-L
C
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
C
         DO 70 II = 1, MML
            I = M-II
            P = G*H
            R = P+E2(I)
            E2(I+1) = S*R
            S = E2(I)/R
            D(I+1) = H+S*(H+D(I))
            G = D(I)-E2(I)/G
            IF (G.EQ.ZERO) G = B
            H = G*P/R
   70    CONTINUE
C
         E2(L) = S*G
         D(L) = H
C
C     ********** GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST **********
C
         IF (H.EQ.ZERO) GO TO 80
         IF (ABS(E2(L)).LE.ABS(C/H)) GO TO 80
         E2(L) = H*E2(L)
         IF (E2(L).NE.ZERO) GO TO 50
   80    P = D(L)+F
C
C     ********** ORDER EIGENVALUES **********
C
         IF (L.EQ.1) GO TO 100
C
C     ********** FOR I=L STEP -1 UNTIL 2 DO -- **********
C
         DO 90 II = 2, L
            I = L+2-II
            IF (P.GE.D(I-1)) GO TO 110
            D(I) = D(I-1)
   90    CONTINUE
C
  100    I = 1
  110    D(I) = P
  120 CONTINUE
C
      GO TO 140
C
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
C
  130 IERR = L
  140 RETURN
C
C     ********** LAST CARD OF TQLRAT **********
C
 9999 RETURN 1                                                          CSTP
      END
