      SUBROUTINE trbak3 (NM,N,NV,A,M,Z,IERR,*)                          CSTP
C          
C***********************************************************************
C  TRBAK3
C***********************************************************************
C
C     CALLED BY:
C                RSP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION A(NV),Z(NM,M)
      DOUBLE PRECISION H,S
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
      DATA ZERO / 0.0D0 /
      IF (M.EQ.0) GO TO 60
      IF (N.EQ.1) GO TO 60
C
      DO 50 I = 2, N
         L = I-1
         IZ = (I*L)/2
         IK = IZ+I
         H = A(IK)
         IF (H.EQ.ZERO) GO TO 50
C
         DO 40 J = 1, M
            S = ZERO
            IK = IZ
C
            DO 10 K = 1, L
               IK = IK+1
               S = S+A(IK)*Z(K,J)
   10       CONTINUE
C
C     ********** DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW **********
C
            S = S/H
            IK = IZ
C
            DO 30 K = 1, L
               IK = IK+1
               IF (ABS(H).GT.1.0D0) GO TO 20
               SA = S*A(IK)
               IF (ABS(SA).LT.ABS(H)*1.6D+38) GO TO 20
               IERR = 1111
               RETURN
   20          CONTINUE
               Z(K,J) = Z(K,J)-S*A(IK)/H
   30       CONTINUE
C
   40    CONTINUE
C
   50 CONTINUE
C
   60 RETURN
C
C     ********** LAST CARD OF TRBAK3 **********
C
 9999 RETURN 1                                                          CSTP
      END
