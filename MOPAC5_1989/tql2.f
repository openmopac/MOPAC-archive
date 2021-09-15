      SUBROUTINE TQL2(NM,N,D,E,Z,IERR,EPS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C               ===== PROCESSED BY AUGMENT, VERSION 4N =====
C     APPROVED FOR VAX 11/780 ON MAY 6,1980.  J.D.NEECE
C               ----- LOCAL VARIABLES -----
C               ----- GLOBAL VARIABLES -----
      DIMENSION D(N), E(N), Z(NM,N)
C               ----- SUPPORTING PACKAGE FUNCTIONS -----
C               ===== TRANSLATED PROGRAM =====
C
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQL2,
C     NUM. MATH. 11, 293-306(1968) BY BOWDLER, MARTIN, REINSCH, AND
C     WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971).
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
C     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE QL METHOD.
C     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO
C     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS
C     FULL MATRIX TO TRIDIAGONAL FORM.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX,
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
C          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY,
C
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
C          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS
C          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN
C          THE IDENTITY MATRIX.
C
C      ON OUTPUT-
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT
C          UNORDERED FOR INDICES 1,2,...,IERR-1,
C
C        E HAS BEEN DESTROYED,
C
C        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC
C          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,
C          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED
C          EIGENVALUES,
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
      IF (N .EQ. 1) GO TO 160
C
      DO 10   I = 2, N
   10 E(I-1) = E(I)
C
      F=0.0D0
      B=0.0D0
      E(N)=0.0D0
C
      DO 110   L = 1, N
         J = 0
         H=EPS*(ABS (D(L))+ABS (E(L)))
         IF (B .LT. H) B=H
C     ********** LOOK FOR SMALL SUB-DIAGONAL ELEMENT **********
         DO 20   M = L, N
            IF (ABS (E(M)).LE.B)  GO TO 30
C     ********** E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
   20    CONTINUE
C
   30    IF (M .EQ. L) GO TO 100
   40    IF (J .EQ. 30) GO TO 150
         J = J + 1
C     ********** FORM SHIFT **********
         L1 = L + 1
         G = D(L)
         P=(D(L1)-G)/(2.0D0*E(L))
         R=SQRT (P*P+1.0D0)
         D(L)=E(L)/(P+SIGN (R,P))
         H = G - D(L)
C
         DO 50   I = L1, N
   50    D(I) = D(I) - H
C
         F = F + H
C     ********** QL TRANSFORMATION **********
         P = D(M)
         C=1.0D0
         S=0.0D0
         MML = M - L
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
         DO 90   II = 1, MML
            I = M - II
            G = C * E(I)
            H = C * P
            IF (ABS (P).LT.ABS (E(I)))  GO TO 60
            C = E(I) / P
            R=SQRT (C*C+1.0D0)
            E(I+1) = S * P * R
            S = C / R
            C=1.0D0/R
            GO TO 70
   60       C = P / E(I)
            R=SQRT (C*C+1.0D0)
            E(I+1) = S * E(I) * R
            S=1.0D0/R
            C = C * S
   70       P = C * D(I) - S * G
            D(I+1) = H + S * (C * G + S * D(I))
C     ********** FORM VECTOR **********
            DO 80   K = 1, N
               H = Z(K,I+1)
               Z(K,I+1) = S * Z(K,I) + C * H
               Z(K,I) = C * Z(K,I) - S * H
   80       CONTINUE
C
   90    CONTINUE
C
         E(L) = S * P
         D(L) = C * P
         IF (ABS (E(L)).GT.B)  GO TO 40
  100    D(L) = D(L) + F
  110 CONTINUE
C     ********** ORDER EIGENVALUES AND EIGENVECTORS **********
      DO 140   II = 2, N
         I = II - 1
         K = I
         P = D(I)
C
         DO 120   J = II, N
            IF (D(J) .GE. P) GO TO 120
            K = J
            P = D(J)
  120    CONTINUE
C
         IF (K .EQ. I) GO TO 140
         D(K) = D(I)
         D(I) = P
C
         DO 130   J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
  130    CONTINUE
C
  140 CONTINUE
C
      GO TO 160
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
  150 IERR = L
  160 RETURN
C     ********** LAST CARD OF TQL2 **********
      END
