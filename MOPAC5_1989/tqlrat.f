      SUBROUTINE TQLRAT(N,D,E2,IERR,EPS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C               ===== PROCESSED BY AUGMENT, VERSION 4N =====
C     APPROVED FOR VAX 11/780 ON MAY 6,1980.  J.D.NEECE
C               ----- LOCAL VARIABLES -----
C               ----- GLOBAL VARIABLES -----
      DIMENSION D(N), E2(N)
C               ----- SUPPORTING PACKAGE FUNCTIONS -----
C               ===== TRANSLATED PROGRAM =====
C
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
C
      IERR = 0
      IF (N .EQ. 1) GO TO 140
C
      DO 10   I = 2, N
   10 E2(I-1) = E2(I)
C
      F=0.0D0
      B=0.0D0
      E2(N)=0.0D0
C
      DO 120   L = 1, N
         J = 0
         H=EPS*(ABS (D(L))+SQRT (E2(L)))
         IF (B .GT. H) GO TO 20
         B = H
         C = B * B
C     ********** LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT **********
   20    DO 30   M = L, N
            IF (E2(M) .LE. C) GO TO 40
C     ********** E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
   30    CONTINUE
C
   40    IF (M .EQ. L) GO TO 80
   50    IF (J .EQ. 30) GO TO 130
         J = J + 1
C     ********** FORM SHIFT **********
         L1 = L + 1
         S=SQRT (E2(L))
         G = D(L)
         P=(D(L1)-G)/(2.0D0*S)
         R=SQRT (P*P+1.0D0)
         D(L)=S/(P+SIGN (R,P))
         H = G - D(L)
C
         DO 60   I = L1, N
   60    D(I) = D(I) - H
C
         F = F + H
C     ********** RATIONAL QL TRANSFORMATION **********
         G = D(M)
         IF (G.EQ.0.0D0) G=B
         H = G
         S=0.0D0
         MML = M - L
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
         DO 70   II = 1, MML
            I = M - II
            P = G * H
            R = P + E2(I)
            E2(I+1) = S * R
            S = E2(I) / R
            D(I+1) = H + S * (H + D(I))
            G = D(I) - E2(I) / G
            IF (G.EQ.0.0D0) G=B
            H = G * P / R
   70    CONTINUE
C
         E2(L) = S * G
         D(L) = H
C     ********** GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST **********
         IF (H.EQ.0.0D0)  GO TO 80
         IF (ABS (E2(L)).LE.ABS (C/H))  GO TO 80
         E2(L) = H * E2(L)
         IF (E2(L).NE.0.0D0)  GO TO 50
   80    P = D(L) + F
C     ********** ORDER EIGENVALUES **********
         IF (L .EQ. 1) GO TO 100
C     ********** FOR I=L STEP -1 UNTIL 2 DO -- **********
         DO 90   II = 2, L
            I = L + 2 - II
            IF (P .GE. D(I-1)) GO TO 110
            D(I) = D(I-1)
   90    CONTINUE
C
  100    I = 1
  110    D(I) = P
  120 CONTINUE
C
      GO TO 140
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
  130 IERR = L
  140 RETURN
C     ********** LAST CARD OF TQLRAT **********
      END
