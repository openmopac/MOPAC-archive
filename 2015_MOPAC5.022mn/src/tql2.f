      SUBROUTINE tql2 (NM,N,D,E,Z,IERR,*)                               CSTP
C
C***********************************************************************
C  TQL2
C***********************************************************************
C
C     CALLED BY:
C                RSP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION D(N),E(N),Z(NM,N)
      DOUBLE PRECISION B,C,F,G,H,P,R,S,MACHEP
C
C     REAL SQRT,ABS,SIGN
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
C                **********
C
      DATA ZERO,ONE,TWO / 0.0D0,1.0D0,2.0D0 /
C
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
C     MACHEP = TWO**(-46)
      MACHEP = TWO**(-45)                                               0519WH93
c#      write(6 ,*) "Entering TQL2 anew."
c#      write(24,*) "Entering TQL2 anew."
C
      IERR = 0
      IF (N.EQ.1) GO TO 160
C
      DO 10 I = 2, N
         E(I-1) = E(I)
   10 CONTINUE
C
      F = ZERO
      B = ZERO
      E(N) = ZERO
C
      DO 110 L = 1, N
         J = 0
         H = MACHEP*(ABS(D(L))+ABS(E(L)))
         IF (B.LT.H) B = H
C
C     ********** LOOK FOR SMALL SUB-DIAGONAL ELEMENT **********
C
         DO 20 M = L, N
            IF (ABS(E(M)).LE.B) GO TO 30
C
C     ********** E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
C
   20    CONTINUE
Ctry_this         M=N                                                            LF0113   ! avoid segmentation faults for case m=n+1 when leaves loop 20
         write( 6,*)"Leaked out of DO 20 loop in TQL2: calling bad run."
         write(24,*)"Leaked out of DO 20 loop in TQL2: calling bad run."
         goto 9999      ! try this instead; probably shouldn't be here anyway (LF0313)
C
   30    IF (M.EQ.L) GO TO 100
   40    IF (J.EQ.30) GO TO 150
         J = J+1
C
C     ********** FORM SHIFT **********
C
         L1 = L+1
         G = D(L)
         P = (D(L1)-G)/(TWO*E(L))
C
C        R = SQRT(P*P+ONE)
C
         IF (ABS(P).GT.1.0D+16) THEN
            R = ABS(P)
         ELSE
            R = SQRT(P*P+ONE)
         ENDIF
         D(L) = E(L)/(P+SIGN(R,P))
         H = G-D(L)
C
         DO 50 I = L1, N
            D(I) = D(I)-H
   50    CONTINUE
C
         F = F+H
C
C     ********** QL TRANSFORMATION **********
C
         P = D(M)
         C = ONE
         S = ZERO
         MML = M-L
C
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********
C
         DO 90 II = 1, MML
            I = M-II
            G = C*E(I)
            H = C*P
            IF (ABS(P).LT.ABS(E(I))) GO TO 60
            C = E(I)/P
            R = SQRT(C*C+ONE)
            E(I+1) = S*P*R
            S = C/R
            C = ONE/R
            GO TO 70
   60       C = P/E(I)
            R = SQRT(C*C+ONE)
            E(I+1) = S*E(I)*R
            S = ONE/R
            C = C*S
   70       P = C*D(I)-S*G
            D(I+1) = H+S*(C*G+S*D(I))
C
C     ********** FORM VECTOR **********
C
            DO 80 K = 1, N
               H = Z(K,I+1)
               Z(K,I+1) = S*Z(K,I)+C*H
               Z(K,I) = C*Z(K,I)-S*H
   80       CONTINUE
C
   90    CONTINUE
C
         E(L) = S*P
         D(L) = C*P
         IF (ABS(E(L)).GT.B) GO TO 40
  100    D(L) = D(L)+F
  110 CONTINUE
C
C     ********** ORDER EIGENVALUES AND EIGENVECTORS **********
C
      DO 140 II = 2, N
         I = II-1
         K = I
         P = D(I)
C
         DO 120 J = II, N
            IF (D(J).GE.P) GO TO 120
            K = J
            P = D(J)
  120    CONTINUE
C
         IF (K.EQ.I) GO TO 140
         D(K) = D(I)
         D(I) = P
C
         DO 130 J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
  130    CONTINUE
C
  140 CONTINUE
C
      GO TO 160
C
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
C
  150 IERR = L
  160 RETURN
C
C     ********** LAST CARD OF TQL2 **********
C
 9999 continue 
c#      write(6,*) "At 9999."
c#      write(24,*) "At 9999."
      RETURN 1                                                          CSTP
      END
