      DOUBLE PRECISION FUNCTION READA(A,ISTART)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*1 A(80)
      NINE=ICHAR('9')
      IZERO=ICHAR('0')
      MINUS=ICHAR('-')
      IDOT=ICHAR('.')
      IDIG=0
      C1=0
      C2=0
      ONE=1.D0
      X = 1.D0
      DO 10 J=ISTART,80
         N=ICHAR(A(J))
         IF(N.LE.NINE.AND.N.GE.IZERO .OR. N.EQ.MINUS.OR.N.EQ.IDOT)GOTO 2
     10
   10 CONTINUE
      READA=0.D0
      RETURN
   20 CONTINUE
      DO 30 I=J,80
         N=ICHAR(A(I))
         IF(N.LE.NINE.AND.N.GE.IZERO) THEN
            IDIG=IDIG+1
            IF (IDIG.GT.10) GOTO 60
            C1=C1*10+N-IZERO
         ELSEIF(N.EQ.MINUS.AND.I.EQ.J) THEN
            ONE=-1.D0
         ELSEIF(N.EQ.IDOT) THEN
            GOTO 40
         ELSE
            GOTO 60
         ENDIF
   30 CONTINUE
   40 CONTINUE
      IDIG=0
      DO 50 II=I+1,80
         N=ICHAR(A(II))
         IF(N.LE.NINE.AND.N.GE.IZERO) THEN
            IDIG=IDIG+1
            IF (IDIG.GT.10) GOTO 60
            C2=C2*10+N-IZERO
            X = X /10
         ELSEIF(N.EQ.MINUS.AND.II.EQ.I) THEN
            X=-X
         ELSE
            GOTO 60
         ENDIF
   50 CONTINUE
C
C PUT THE PIECES TOGETHER
C
   60 CONTINUE
      READA= ONE * ( C1 + C2 * X)
      RETURN
      END
