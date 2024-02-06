      SUBROUTINE DIAGIV(A,N,MM,DIAG,EPS1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'FFILES.i'                                                GDH1095
      COMMON /SCRCHR/ B(MAXSIZ*(MAXSIZ-1)/2),U(MAXSIZ),V(MAXSIZ)        DL0397
     1               ,SUPRD(MAXSIZ)                                     DL0397
     2               ,DUM(6*MAXPAR**2+1-MAXSIZ*(MAXSIZ-1)/2-3*MAXSIZ)   DL0397
      COMMON /SCRCHI/ LOC(MAXSIZ)                                       DL0397
      DIMENSION A(N,N),DIAG(*)
C ---------------------------------------------------------------------+DL0397
C     ROBUST DIAGONALIZATION METHOD; INVERSION OF SYMMETRIC MATRICES.   DL0397
C                                                                       DL0397
C     REPLACE PREVIOUS ROUTINES "DIAGIV" & "DIAGV2"                     DL0397
C     SPEEDED UP AND VECTORIZED                                         DL0397
C     WARNING: NUMBER OF DUMMY ARGUMENTS MODIFIED FOR ENTRY INVRT1      DL0397
C              (REQUIRED FOR PORTABILITY ON SOME SYSTEMS).              DL0397
C ---------------------------------------------------------------------+DL0397
C     E1: THRESHOLD OF CONVERGENCE, RELATIVE TO THE MATRIX NORM
      DATA ZERO,ONE,TWO,E1,UPSIL /0D0, 1D0, 2D0, 1D-11, 1D-30/
C
C     DIAGONALIZATION BY THE GIVENS HOUSEHOLDER AND QL METHOD
C     -------------------------------------------------------
C     SYMMETRIC REAL MATRIX A IS DESTROYED AND GIVES EIGENVECTORS
C     (IN COLUMN)
C     DIAG    EIGENVALUES IN ASCENDING ORDER
C     N       ORDER OF MATRIX A
C     MM      NUMBER OF EIGENVALUES TO BE EVALUATED
C     EPS1    ABSOLUTE ERROR ON CALCULATED EIGENVALUES
      IROUTE=1
      M=MM
      GOTO 1000
C
      ENTRY INVERT (A,N,MM,DIAG,EPS1)
C
C     INVERSION PROCEDURE TRIANGULATION METHOD
C     ----------------------------------------
C     SYMMETRIC REAL MATRIX A IS INVERTED IN AREA A
C     DIAG INVERTED PIVOT VECTOR
C     N    ORDER OF MATRIX A
C     MM   NUMBER OF NEGATIVE EIGENVALUES (INDEX)
C     EPS1 ABSOLUTE VALUE OF PIVOT ENCOUNTERED
C
      IROUTE=2
      M=N
 1000 CONTINUE
C     -------------
      IF (M.GT.N) M=N
      IF (N.EQ.1) THEN
C        THE MATRIX A IS 1-DIMENSIONAL
         DIAG(1)=A(1,1)
         EPS1=ABS(DIAG(1))*E1+UPSIL
         GOTO (1,2),IROUTE
    1    A(1,1)=ONE
         RETURN
    2    MM=0
         IF(DIAG(1).LT.ZERO) MM=1
         IF(ABS(DIAG(1)).LT.UPSIL) DIAG(1)=UPSIL
         A(1,1)=ONE/DIAG(1)
         RETURN
      ENDIF
      IF(IROUTE.EQ.2) GOTO 400
C     HOUSEHOLDER SIMILARITY TRANSFORMATION TO CO-DIAGONAL FORM
C     ---------------------------------------------------------
C     ONLY THE LOWER LEFT TRIANGLE OF MATRIX A IS REQUIRED
C     REFERENCE: J. ORTEGA, MATHEMATICAL METHODS FOR DIGITAL
C                COMPUTERS, (VOL. 2) WILEY, NEW-YORK (1964)
   10 N2=N-2
      N1=N-1
      IF(N2.LT.1) GOTO 70
C     REDUCE ROW OF MATRIX
      IPOS=1
      DO 60 I=1,N2
      DIAG(I)=A(I,I)
      I1=I+1
      I2=I+2
      SUM=DOT(A(I2,I),A(I2,I),N-I1)
      TEMP=A(I1,I)
      IF (SUM.LE.ABS(TEMP)*E1) THEN
         U(I1)=ZERO
         SUPRD(I)=TEMP
         B(IPOS)=ZERO
         IPOS=IPOS+N-I
      ELSE
         SUM=SQRT(SUM+TEMP**2)
         SUPRD(I)=-SIGN(SUM,TEMP)
         V(I1)=ZERO
         U(I1)=SQRT(ONE+ABS(TEMP)/SUM)
         DIV=ONE/SIGN(U(I1)*SUM,TEMP)
         DO 20 J=I2,N
         V(J)=ZERO
   20    U(J)=A(J,I)*DIV
         DO 30 K=I1,N
         CALL SAXPY (K-I1,U(K),A(K,I1),N,V(I1),1)
   30    CALL SAXPY (N+1-K,U(K),A(K,K),1,V(K),1)
         SCLAR=-0.5D0*DOT(V(I1),U(I1),N-I)
         CALL SAXPY (N-I,SCLAR,U(I1),1,V(I1),1)
         DO 40 J=I1,N
         DO 40 K=J,N
   40    A(K,J)=A(K,J)-U(J)*V(K)-V(J)*U(K)
C        SAVE ROTATION FOR LATER APPLICATION TO CO-DIAGONAL VECTORS
         CALL SCOPY (N-I,U(I1),1,B(IPOS),1)
         IPOS=IPOS+N-I
      ENDIF
   60 CONTINUE
C     MOVE LAST CO-DIAGONAL ELEMENTS
   70 DIAG(N)=A(N,N)
      SUPRD(N)=ZERO
      DIAG(N1)=A(N1,N1)
      SUPRD(N1)=A(N,N1)
C     CALCULATE NORM OF MATRIX AND EIGENVALUE ACCURACY
C     ------------------------------------------------
      ANRM1=ABS(DIAG(1))+ABS(SUPRD(1))
      ANRM2= DIAG(1)**2
      DO 80 L=2,N
      ANRM1=MAX(ANRM1,ABS(DIAG(L))+ABS(SUPRD(L-1))+ABS(SUPRD(L)))
   80 ANRM2=ANRM2+DIAG(L)**2+TWO*SUPRD(L-1)**2
      ANORM=MIN(SQRT(ANRM2),ANRM1)
      EPS1=ANORM*E1+UPSIL
C     EIGENVALUES AND EIGENVECTORS OF THE TRIDIAGONAL MATRIX.
C     QL METHOD WITH IMPLICIT SHIFTS. D.L. (FEBRUARY 1992)
C     FOUND MORE ROBUST THAN WILKINSON AND ALSO FASTER WHEN M > N/4
C     REFERENCE: W.H. PRESS, B.P. FLANNERY, S.A. TEUKOLSKY,
C     W.T. VETTERLING, NUMERICAL RECIPES, CAMBRIDGE UNIVERSITY PRESS,
C     CAMBRIDGE (1986), REPRINTED 1988.
C     ------------------------------------------------------------
C     SET UP IDENTITY MATRIX IN A TO START CO-DIAGONAL EIGENVECTORS.
      CALL SCOPY (N**2,ZERO,0,A,1)
      CALL SCOPY (N,ONE,0,A,N+1)
      CALL SCOPY (N,DIAG,1,V,1)
      MMSAVE=M
      DO 150 L=1,N
      ITE=0
C     LOOK FOR A SMALL OFF-DIAGONAL ELEMENT TO SPLIT THE MATRIX.
  110 DO 120 M=L,N-1
      DD=(ABS(V(M))+ABS(V(M+1)))*E1
      IF(ABS(SUPRD(M)).LE.DD) GOTO 130
  120 CONTINUE
      M=N
  130 IF (M.NE.L) THEN
         IF (ITE.GE.30) THEN
            EPS1=ONE/UPSIL
            GOTO 150
         ENDIF
         ITE=ITE+1
C        FORM SHIFTS
         G=(V(L+1)-V(L))/(TWO*SUPRD(L))
         R=SQRT(G**2+ONE)
         G=V(M)-V(L)+SUPRD(L)/(G+SIGN(R,G))
         S=ONE
         C=ONE
         P=ZERO
C        PLANE ROTATION FOLLOWED BY GIVENS ROTATIONS TO RESTORE
C        TRIDIAGONAL FORM.
         DO 145 I=M-1,L,-1
         F=S*SUPRD(I)
         BF=C*SUPRD(I)
         IF (ABS(F).GE.ABS(G)) THEN
            C=G/F
            R=SQRT(C**2+ONE)
            SUPRD(I+1)=F*R
            S=ONE/R
            C=C*S
         ELSE
            S=F/G
            R=SQRT(S**2+ONE)
            SUPRD(I+1)=G*R
            C=ONE/R
            S=S*C
         ENDIF
         G=V(I+1)-P
         R=(V(I)-G)*S+TWO*C*BF
         P=S*R
         V(I+1)=G+P
         G=C*R-BF
C        FORM EIGENVECTORS IN MATRIX A
         CALL DROT (N,A(1,I),1,A(1,I+1),1,C,-S)
  145    CONTINUE
         V(L)=V(L)-P
         SUPRD(L)=G
         SUPRD(M)=ZERO
         GOTO 110
      ENDIF
  150 CONTINUE
C     QL METHOD COMPLETED.
C     EIGENVALUES "V" ARE ROUGHLY ORDERED BY ABSOLUTE MAGNITUDES.
C     SORT M EIGENVALUES AND EIGENVECTORS IN ALGEBRAIC ASCENDING ORDER.
      M=MMSAVE
      DO 160 I=1,N
  160 LOC(I)=I
      DO 165 I=1,MIN(M,N-1)
      DO 165 J=I+1,N
      IF (V(LOC(J)).LT.V(LOC(I))) THEN
         K=LOC(I)
         LOC(I)=LOC(J)
         LOC(J)=K
      ENDIF
  165 CONTINUE
      DO 170 I=1,M
  170 DIAG(I)=V(LOC(I))
      DO 190 J=1,M
      IF (LOC(J).NE.J) THEN
         CALL DSWAP (N,A(1,J),1,A(1,LOC(J)),1)
         DO 180 I=J+1,M
         IF (LOC(I).EQ.J) THEN
            LOC(I)=LOC(J)
            GOTO 190
         ENDIF
  180    CONTINUE
      ENDIF
  190 CONTINUE
C     ACCURACY INCLUDING ROUND-OFF CUMULATIVE EFFECTS
      EPS1=EPS1*SQRT(DBLE(N*(N-1)/2))
C     ROTATION OF CO-DIAGONAL VECTORS INTO MATRIX EIGENVECTORS
C     --------------------------------------------------------
      DO 210 I=1,N2
      I1=N-I
      IPOS=IPOS-I-1
      IF(B(IPOS).EQ.ZERO) GOTO 210
      DO 200 K=1,M
      SUM=-DOT(A(I1,K),B(IPOS),I+1)
  200 CALL SAXPY (I+1,SUM,B(IPOS),1,A(I1,K),1)
  210 CONTINUE
      IF (IROUTE.EQ.1) RETURN
C
C     INVERT MATRIX A IN THE SAME AREA A, FROM EIGENVALUES AND VECTORS
C     ----------------------------------------------------------------
      ENTRY INVRT1 (A,N,MM,DIAG,EPS1)
C     ON INPUT   EIGENVECTORS IN A (COLUMNWISE)
C                EIGENVALUES  IN DIAG
C     ON OUTPUT  INVERSE OF THE ORIGINAL MATRIX IN A
C     NOTE ... IF SOME EIGENVALUE IS ZERO, REPLACE WITH EPS1.
      DO 300 I=1,N
  300 IF(ABS(DIAG(I)).LT.EPS1) DIAG(I)=SIGN(EPS1,DIAG(I))
      DO 330 I=1,N
      DO 310 J=I,N
  310 SUPRD(J)=ZERO
      DO 320 K=1,N
      VTEMP=A(I,K)/DIAG(K)
      DO 320 J=I,N
  320 SUPRD(J)=SUPRD(J)+VTEMP*A(J,K)
      DO 330 J=I,N
  330 A(I,J)=SUPRD(J)
      DO 340 I=1,N-1
      DO 340 J=I+1,N
  340 A(J,I)=A(I,J)
      RETURN
C
C     DIRECT INVERSION SECTION (TRIANGULATION OR DIAGONALISATION)
C     -----------------------------------------------------------
  400 IF(ABS(A(1,1)).LT.UPSIL) GOTO 422
      EPS1=ONE/UPSIL
      J=1
      S=ZERO
      DO 401 I=1,N
      SUPRD(I)=A(J,1)
      S=S+ABS(SUPRD(I))
  401 J=J+N+1
      ANORM=UPSIL*S/DBLE(N)
C     FACTORISE IN UPPER A AND DIAG
      DIAG(1)=ONE/A(1,1)
      DO 402 I=1,N
  402 A(1,I)=A(I,1)*DIAG(1)
      DO 406 J=2,N
      J1=J-1
      S=SUPRD(J)
      DO 403 K=1,J1
  403 S=S-A(K,J)**2/DIAG(K)
      EPS1=MIN(EPS1,ABS(S))
      IF(ABS(S).LT.ANORM) GOTO 420
      DIAG(J)=ONE/S
      A(J,J)=ONE
      JP=J+1
      IF(JP.GT.N) GOTO 406
      DO 405 I=JP,N
      S=A(I,J)
      DO 404 K=1,J1
  404 S=S-A(K,I)*A(K,J)/DIAG(K)
  405 A(J,I)=S*DIAG(J)
  406 CONTINUE
C     INDEX MM
      MM=0
      DO 407 I=1,N
      IF(DIAG(I).LT.ZERO) MM=MM+1
  407 CONTINUE
C     INVERT UPPER TRIANGULAR A MATRIX
      DO 409 I=2,N
      I1=I-1
      DO 409 J=1,I1
      S=ZERO
      DO 408 K=J,I1
  408 S=S-A(K,I)*A(J,K)
  409 A(J,I)=S
C     FINAL LOWER A MATRIX =(UPPER A)' * DIAG * (UPPER A)
      DO 410 J=1,N
  410 A(N,J)=A(J,N)*DIAG(N)
      N1=N-1
      DO 412 I=N1,1,-1
      DO 411 J=1,I
  411 A(I,J)=A(J,I)*DIAG(I)
      IP=I+1
      DO 412 K=IP,N
      S=A(I,K)*DIAG(K)
      DO 412 J=1,I
  412 A(I,J)=A(I,J)+S*A(J,K)
      DO 413 I=2,N
      I1=I-1
      DO 413 J=1,I1
  413 A(J,I)=A(I,J)
      RETURN
C     IF A SINGULAR PIVOT IS ENCOUNTERED : RESTORE AND DIAGONALIZE .
  420 J=1
      DO 421 I=1,N
      A(J,1)=SUPRD(I)
  421 J=J+N+1
  422 IROUTE=3
      GOTO 10
      END
