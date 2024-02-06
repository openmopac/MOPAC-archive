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
