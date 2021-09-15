      SUBROUTINE RSP(A,N,MATZ,W,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C               ===== PROCESSED BY AUGMENT, VERSION 4N =====
C     APPROVED FOR VAX 11/780 ON MAY 6,1980. J.D.NEECE
C     MODIFIED TO REDUCE ARGUMENT LIST SEPT. 20 1982.
C               ----- LOCAL VARIABLES -----
C               ----- GLOBAL VARIABLES -----
      DIMENSION A(*),  W(N), Z(N,N)
C               ===== TRANSLATED PROGRAM =====
C
C
C     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF
C     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK)
C     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED)
C     OF A REAL SYMMETRIC PACKED MATRIX.
C
C     ON INPUT-
C
C        N  IS THE ORDER OF THE MATRIX  A,
C
C        A  CONTAINS THE LOWER TRIANGLE OF THE REAL SYMMETRIC
C        PACKED MATRIX STORED ROW-WISE,
C
C        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF
C        ONLY EIGENVALUES ARE DESIRED,  OTHERWISE IT IS SET TO
C        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS.
C
C     ON OUTPUT-
C
C        W  CONTAINS THE EIGENVALUES IN ASCENDING ORDER,
C
C        Z  CONTAINS THE EIGENVECTORS IF MATZ IS NOT ZERO,
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      DIMENSION FV1(200),FV2(200)
C
C   MAX. SIZE OF MATRICES = 200, NV= (200*(200+1))/2
C
      NV=20100
      NM=N
      IF (N .LE. NM) GO TO 5
      IERR = 10 * N
      GO TO 50
    5 IF (NV .GE. (N * (N + 1)) / 2) GO TO 10
      IERR = 20 * N
      GO TO 50
C
   10 CALL  TRED3(N,NV,A,W,FV1,FV2)
      IF (MATZ .NE. 0) GO TO 20
C     ********** FIND EIGENVALUES ONLY **********
      CALL  TQLRAT(N,W,FV2,IERR)
      GO TO 50
C     ********** FIND BOTH EIGENVALUES AND EIGENVECTORS **********
   20 DO 40    I = 1, N
C
      DO 30    J = 1, N
      Z(J,I)=0.0D0
   30    CONTINUE
C
      Z(I,I)=1.0D0
   40 CONTINUE
C
      CALL  TQL2(NM,N,W,FV1,Z,IERR)
      IF (IERR .NE. 0) GO TO 50
      CALL  TRBAK3(NM,N,NV,A,N,Z)
C     ********** LAST CARD OF RSP **********
   50 RETURN
      END
C
C     ------------------------------------------------------------------
C
      SUBROUTINE TQL2(NM,N,D,E,Z,IERR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C               ===== PROCESSED BY AUGMENT, VERSION 4N =====
C     APPROVED FOR VAX 11/780 ON MAY 6,1980.  J.D.NEECE
C               ----- LOCAL VARIABLES -----
      DOUBLE PRECISION MACHEP
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
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
C                **********
      MACHEP = 2.D0**(-51)
C
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
C
      DO 100   I = 2, N
  100 E(I-1) = E(I)
C
      F=0.0D0
      B=0.0D0
      E(N)=0.0D0
C
      DO 240   L = 1, N
         J = 0
      H=MACHEP*(ABS (D(L))+ABS (E(L)))
      IF (B .LT. H) B=H
C     ********** LOOK FOR SMALL SUB-DIAGONAL ELEMENT **********
      DO 110   M = L, N
      IF (ABS (E(M)).LE.B)  GO TO 120
C     ********** E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
  110    CONTINUE
C
  120    IF (M .EQ. L) GO TO 220
  130    IF (J .EQ. 30) GO TO 1000
         J = J + 1
C     ********** FORM SHIFT **********
         L1 = L + 1
         G = D(L)
      P=(D(L1)-G)/(2.0D0*E(L))
      R=SQRT (P*P+1.0D0)
      D(L)=E(L)/(P+SIGN (R,P))
         H = G - D(L)
C
      DO 140   I = L1, N
  140    D(I) = D(I) - H
C
         F = F + H
C     ********** QL TRANSFORMATION **********
         P = D(M)
      C=1.0D0
      S=0.0D0
         MML = M - L
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
      DO 200   II = 1, MML
            I = M - II
            G = C * E(I)
            H = C * P
      IF (ABS (P).LT.ABS (E(I)))  GO TO 150
            C = E(I) / P
      R=SQRT (C*C+1.0D0)
            E(I+1) = S * P * R
            S = C / R
      C=1.0D0/R
            GO TO 160
  150       C = P / E(I)
      R=SQRT (C*C+1.0D0)
            E(I+1) = S * E(I) * R
      S=1.0D0/R
            C = C * S
  160       P = C * D(I) - S * G
            D(I+1) = H + S * (C * G + S * D(I))
C     ********** FORM VECTOR **********
      DO 180   K = 1, N
               H = Z(K,I+1)
               Z(K,I+1) = S * Z(K,I) + C * H
               Z(K,I) = C * Z(K,I) - S * H
  180       CONTINUE
C
  200    CONTINUE
C
         E(L) = S * P
         D(L) = C * P
      IF (ABS (E(L)).GT.B)  GO TO 130
  220    D(L) = D(L) + F
  240 CONTINUE
C     ********** ORDER EIGENVALUES AND EIGENVECTORS **********
      DO 300   II = 2, N
         I = II - 1
         K = I
         P = D(I)
C
      DO 260   J = II, N
            IF (D(J) .GE. P) GO TO 260
            K = J
            P = D(J)
  260    CONTINUE
C
         IF (K .EQ. I) GO TO 300
         D(K) = D(I)
         D(I) = P
C
      DO 280   J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
  280    CONTINUE
C
  300 CONTINUE
C
      GO TO 1001
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
 1000 IERR = L
 1001 RETURN
C     ********** LAST CARD OF TQL2 **********
      END
C
C     ------------------------------------------------------------------
C
      SUBROUTINE TQLRAT(N,D,E2,IERR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C               ===== PROCESSED BY AUGMENT, VERSION 4N =====
C     APPROVED FOR VAX 11/780 ON MAY 6,1980.  J.D.NEECE
C               ----- LOCAL VARIABLES -----
      DOUBLE PRECISION MACHEP
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
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
C                **********
      MACHEP = 2.D0 **(-47)
C
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
C
      DO 100   I = 2, N
  100 E2(I-1) = E2(I)
C
      F=0.0D0
      B=0.0D0
      E2(N)=0.0D0
C
      DO 290   L = 1, N
         J = 0
      H=MACHEP*(ABS (D(L))+SQRT (E2(L)))
         IF (B .GT. H) GO TO 105
         B = H
         C = B * B
C     ********** LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT **********
  105 DO 110   M = L, N
            IF (E2(M) .LE. C) GO TO 120
C     ********** E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
  110    CONTINUE
C
  120    IF (M .EQ. L) GO TO 210
  130    IF (J .EQ. 30) GO TO 1000
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
      DO 140   I = L1, N
  140    D(I) = D(I) - H
C
         F = F + H
C     ********** RATIONAL QL TRANSFORMATION **********
         G = D(M)
      IF (G.EQ.0.0D0) G=B
         H = G
      S=0.0D0
         MML = M - L
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
      DO 200   II = 1, MML
            I = M - II
            P = G * H
            R = P + E2(I)
            E2(I+1) = S * R
            S = E2(I) / R
            D(I+1) = H + S * (H + D(I))
            G = D(I) - E2(I) / G
      IF (G.EQ.0.0D0) G=B
            H = G * P / R
  200    CONTINUE
C
         E2(L) = S * G
         D(L) = H
C     ********** GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST **********
      IF (H.EQ.0.0D0)  GO TO 210
      IF (ABS (E2(L)).LE.ABS (C/H))  GO TO 210
         E2(L) = H * E2(L)
      IF (E2(L).NE.0.0D0)  GO TO 130
  210    P = D(L) + F
C     ********** ORDER EIGENVALUES **********
         IF (L .EQ. 1) GO TO 250
C     ********** FOR I=L STEP -1 UNTIL 2 DO -- **********
      DO 230   II = 2, L
            I = L + 2 - II
            IF (P .GE. D(I-1)) GO TO 270
            D(I) = D(I-1)
  230    CONTINUE
C
  250    I = 1
  270    D(I) = P
  290 CONTINUE
C
      GO TO 1001
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
 1000 IERR = L
 1001 RETURN
C     ********** LAST CARD OF TQLRAT **********
      END
C
C     ------------------------------------------------------------------
C
      SUBROUTINE TRBAK3(NM,N,NV,A,M,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C               ===== PROCESSED BY AUGMENT, VERSION 4N =====
C     APPROVED FOR VAX 11/780 ON MAY 6,1980.  J.D.NEECE
C               ----- LOCAL VARIABLES -----
C               ----- GLOBAL VARIABLES -----
      DIMENSION A(NV), Z(NM,M)
C               ===== TRANSLATED PROGRAM =====
C
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRBAK3,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A REAL SYMMETRIC
C     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING
C     SYMMETRIC TRIDIAGONAL MATRIX DETERMINED BY  TRED3.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        NV MUST BE SET TO THE DIMENSION OF THE ARRAY PARAMETER A
C          AS DECLARED IN THE CALLING PROGRAM DIMENSION STATEMENT,
C
C        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANSFORMATIONS
C          USED IN THE REDUCTION BY  TRED3  IN ITS FIRST
C          N*(N+1)/2 POSITIONS,
C
C        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED,
C
C        Z CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED
C          IN ITS FIRST M COLUMNS.
C
C     ON OUTPUT-
C
C        Z CONTAINS THE TRANSFORMED EIGENVECTORS
C          IN ITS FIRST M COLUMNS.
C
C     NOTE THAT TRBAK3 PRESERVES VECTOR EUCLIDEAN NORMS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      IF (M .EQ. 0) GO TO 200
      IF (N .EQ. 1) GO TO 200
C
      DO 140   I = 2, N
         L = I - 1
         IZ = (I * L) / 2
         IK = IZ + I
         H = A(IK)
      IF (H.EQ.0.0D0)  GO TO 140
C
      DO 130   J = 1, M
      S=0.0D0
            IK = IZ
C
      DO 110   K = 1, L
               IK = IK + 1
               S = S + A(IK) * Z(K,J)
  110       CONTINUE
C     ********** DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW **********
            S = (S / H) / H
            IK = IZ
C
      DO 120   K = 1, L
               IK = IK + 1
               Z(K,J) = Z(K,J) - S * A(IK)
  120       CONTINUE
C
  130    CONTINUE
C
  140 CONTINUE
C
  200 RETURN
C     ********** LAST CARD OF TRBAK3 **********
      END
C
C     ------------------------------------------------------------------
C
      SUBROUTINE TRED3(N,NV,A,D,E,E2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C               ===== PROCESSED BY AUGMENT, VERSION 4N =====
C     APPROVED FOR VAX 11/780 ON MAY 6,1980.  J.D.NEECE
C               ----- LOCAL VARIABLES -----
C               ----- GLOBAL VARIABLES -----
      DIMENSION A(NV), D(N), E(N), E2(N)
C               ----- SUPPORTING PACKAGE FUNCTIONS -----
C               ===== TRANSLATED PROGRAM =====
C
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED3,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX, STORED AS
C     A ONE-DIMENSIONAL ARRAY, TO A SYMMETRIC TRIDIAGONAL MATRIX
C     USING ORTHOGONAL SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT-
C
C        N IS THE ORDER OF THE MATRIX,
C
C        NV MUST BE SET TO THE DIMENSION OF THE ARRAY PARAMETER A
C          AS DECLARED IN THE CALLING PROGRAM DIMENSION STATEMENT,
C
C        A CONTAINS THE LOWER TRIANGLE OF THE REAL SYMMETRIC
C          INPUT MATRIX, STORED ROW-WISE AS A ONE-DIMENSIONAL
C          ARRAY, IN ITS FIRST N*(N+1)/2 POSITIONS.
C
C     ON OUTPUT-
C
C        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL
C          TRANSFORMATIONS USED IN THE REDUCTION,
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX,
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
C          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO,
C
C        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.
C          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
C     ********** FOR I=N STEP -1 UNTIL 1 DO -- **********
      DO 300   II = 1, N
         I = N + 1 - II
         L = I - 1
         IZ = ( I * L ) / 2
      H=0.0D0
      SCALE=0.0D0
         IF (L .LT. 1) GO TO 130
C     ********** SCALE ROW (ALGOL TOL THEN NOT NEEDED) **********
      DO 120   K = 1, L
            IZ = IZ + 1
            D(K) = A(IZ)
      SCALE=SCALE+ABS( D(K) )
  120    CONTINUE
C
      IF ( SCALE.NE.0.0D0 ) GO TO 140
  130 E(I)=0.0D0
      E2(I)=0.0D0
         GO TO 290
C
  140 DO 150   K = 1, L
            D(K) = D(K) / SCALE
            H = H + D(K) * D(K)
  150    CONTINUE
C
         E2(I) = SCALE * SCALE * H
         F = D(L)
      G=-SIGN (SQRT (H),F)
         E(I) = SCALE * G
         H = H - F * G
         D(L) = F - G
         A(IZ) = SCALE * D(L)
         IF (L .EQ. 1) GO TO 290
      F=0.0D0
C
      DO 240   J = 1, L
      G=0.0D0
            JK = (J * (J-1)) / 2
C     ********** FORM ELEMENT OF A*U **********
            K = 0
  180          K = K + 1
               JK = JK + 1
               G = G + A(JK) * D(K)
          IF ( K .LT. J ) GO TO 180
          IF ( K .EQ. L ) GO TO 220
  200          JK = JK + K
               K = K + 1
               G = G + A(JK) * D(K)
          IF ( K .LT. L ) GO TO 200
C     ********** FORM ELEMENT OF P **********
  220 CONTINUE
            E(J) = G / H
            F = F + E(J) * D(J)
  240    CONTINUE
C
         HH = F / (H + H)
         JK = 0
C     ********** FORM REDUCED A **********
      DO 260   J = 1, L
            F = D(J)
            G = E(J) - HH * F
            E(J) = G
C
      DO 260   K = 1, J
               JK = JK + 1
               A(JK) = A(JK) - F * E(K) - G * D(K)
  260    CONTINUE
C
  290    D(I) = A(IZ+1)
      A(IZ+1)=SCALE*SQRT (H)
  300 CONTINUE
C
      RETURN
C     ********** LAST CARD OF TRED3 **********
      END
