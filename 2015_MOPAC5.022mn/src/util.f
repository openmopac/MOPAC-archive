      DOUBLE PRECISION FUNCTION SDOT(N,DX,INCX,DY,INCY)
C
C     FORMS THE DOT PRODUCT OF TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C     LINPACK ROUTINE DDOT CONVERTED TO SDOT BY GCL 03/25/92
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                              GCL0393
C     DIMENSION DX(1),DY(1)                                             GCL0393
      DIMENSION DX(*),DY(*)                                           
C     DOUBLE PRECISION DX(1),DY(1),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      SDOT = 0.0D0
      DTEMP = 0.0D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = DTEMP + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      SDOT = DTEMP
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DTEMP = DTEMP + DX(I)*DY(I) + DX(I + 1)*DY(I + 1) +
     *   DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
   60 SDOT = DTEMP
      RETURN
      END
      SUBROUTINE SUPDOT(S,H,G,N,IG) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      EXTERNAL SDOT                                                     GL0492
C     (S)=(H)*(G) WITH  H  IN PACKED FORM (CANONICAL ORDER). 
C     IG IS THE INCREMENT FOR THE VECTOR G. 
      DIMENSION S(*),H(*),G(*) 
C     CRAY-1 VERSION... BUT POORLY VECTORIZED. 
      K=1 
      L=1 
      DO 10 I=1,N 
C     S(I)=SDOT(I,H(K),1,G,IG,I) 
      S(I)=SDOT(I,H(K),1,G,IG)                                          GCL0393
      IF(I.GT.1) THEN 
         L=L+IG 
         CALL SAXPY(I-1,G(L),H(K),1,S,1) 
      ENDIF 
   10 K=K+I 
      RETURN 
      END 
      SUBROUTINE MTXMC (A,NAR,B,NBR,C) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     MATRIX PRODUCT C(NAR,NAR) = (A(NBR,NAR))' * B(NBR,NAR) 
C     A AND B RECTANGULAR , PACKED, 
C     C LOWER LEFT TRIANGLE ONLY, PACKED IN CANONICAL ORDER. 
      DIMENSION A(NBR,NAR),B(NBR,NAR),C(*) 
C  NOTE ... OPTIMUM VERSION ON CRAY-1. 
      L=1 
      DO 10 I=1,NAR 
      CALL MXM (A(1,I),1,B,NBR,C(L),I) 
   10 L=L+I 
      RETURN 
      END 
      SUBROUTINE MXMT (A,NAR,B,NBR,C,NCC) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     MATRIX PRODUCT C(NAR,NCC) = A(NAR,NBR) * (B(NCC,NBR))' 
C     ALL MATRICES RECTANGULAR , PACKED. 
C NOTE ... OPTIMUM VERSION ON CRAY-1. 
      DIMENSION A(NAR,NBR),B(NCC,NBR),C(NAR,NCC) 
      DO 10 I=1,NCC*NAR 
   10 C(I,1)=0.D0 
      DO 20 K=1,NBR 
      DO 20 J=1,NCC 
      DO 20 I=1,NAR 
   20 C(I,J)=C(I,J)+A(I,K)*B(J,K) 
      RETURN 
      END 
      DOUBLE PRECISION FUNCTION DOT(X,Y,N) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      EXTERNAL SDOT                                                     GL0492
      DIMENSION X(*), Y(*) 
C     DOT =    DOT PRODUCT OF X AND Y, LENGHT N. 
C     CRAY VERSION 
      DOT=SDOT(N,X,1,Y,1) 
      RETURN 
      END 

      SUBROUTINE SAXPY(N,DA,DX,INCX,DY,INCY)
C
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C     LINPACK ROUTINE DAXPY CONVERTED TO SAXPY BY GCL 03/25/92
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                              GCL0393
C     DIMENSION DX(1),DY(1)                                             GCL0393
      DIMENSION DX(*),DY(*)                                             
C     DOUBLE PRECISION DX(1),DY(1),DA
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF (DA .EQ. 0.0D0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DY(IY) + DA*DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
   50 CONTINUE
      RETURN
      END

      INTEGER FUNCTION ISMAX(N,DX,INCX)
C
C     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C     LINPACK ROUTINE IDAMAX CONVERTED TO ISMAX BY GCL 03/25/92
C     FINDS THE INDEX OF ELEMENT HAVING THE MAX. VALUE.
C
C
      DOUBLE PRECISION DX(1),DMAX
C     DOUBLE PRECISION DX(*),DMAX
      INTEGER I,INCX,IX,N
C
      ISMAX = 0
      IF( N .LT. 1 ) RETURN
      ISMAX = 1
      IF(N.EQ.1)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      DMAX = DX(1)                                                      03/25/GL92
      IX = IX + INCX
      DO 10 I = 2,N
         IF(DX(IX).LE.DMAX) GO TO 5                                     03/25/GL92
         ISMAX = I
         DMAX = DX(IX)                                                  03/25/GL92
    5    IX = IX + INCX
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
   20 DMAX = DX(1)                                                      03/25/GL92
      DO 30 I = 2,N
         IF(DX(I).LE.DMAX) GO TO 30                                     03/25/GL92
         ISMAX = I
         DMAX = DX(I)                                                   03/25/GL92
   30 CONTINUE
      RETURN
      END

      SUBROUTINE DROT (N,X,IX,Y,IY,C,S)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GCL0493
      DIMENSION X(*),Y(*)
C ----------------------------------------------------------------------
C     APPLY A PLANE ROTATION TO REAL VECTORS (MIMIC BLAS STANDARD).
C  INPUT  N     NUMBER OF ELEMENTS IN VECTORS X AND Y.
C         X,IX  FIRST  VECTOR AND ITS STRIDE.
C         Y,IY  SECOND VECTOR AND ITS STRIDE.
C         C,S   COSINE AND SINE OF ROTATION ANGLE.
C  OUTPUT X,Y   ROTATED VECTORS SUCH THAT:
C               X(I)=C*X(I)+S*Y(I) AND Y(I)=-S*X(I)+C*Y(I)
C ----------------------------------------------------------------------
      J=1
      DO 10 I=1,IX*(N-1)+1,IX
      A=X(I)
      B=Y(J)
      Y(J)=C*B-S*A
      X(I)=C*A+S*B
   10 J=J+IY
      RETURN
      END

