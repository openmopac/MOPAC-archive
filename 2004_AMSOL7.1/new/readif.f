      DOUBLE PRECISION FUNCTION READIF (A,ISTART)                       GDH0593
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /ONESCM/ ICONTR(100)                                       DJG0295
      CHARACTER*80 A,B                                                  DJG0995
      SAVE
      IF (ICONTR(22).EQ.1) THEN                                         GDH0195
         ICONTR(22)=2                                                   GDH0195
      B(1:20)='                     '
      NINE=ICHAR('9')
      NSPA=ICHAR(' ')
      IZERO=ICHAR('0')
      MINUS=ICHAR('-')
      IDOT=ICHAR('.')
      ENDIF
      ISZA=80                                                           DJG0995
      DO 10 J=ISTART,ISZA                                               GDH0593
         N=ICHAR(A(J:J))
         IF(N.LE.NINE.AND.N.GE.IZERO .OR. N.EQ.MINUS.OR.N.EQ.IDOT
     .      ) GOTO 20
   10 CONTINUE
      READIF=0.D0
      RETURN
   20 NCNT=20
      DO 22 IJ=J,ISZA
         N=ICHAR(A(IJ:IJ))
         IF(N.NE.NSPA.AND.N.GT.42.AND.N.LT.71) THEN
            NCNT=NCNT + 1
            B(NCNT:NCNT)=A(IJ:IJ)
         ELSE
            GOTO 23
         ENDIF
   22 CONTINUE
   23 READ (B(NCNT-19:NCNT),'(F20.0)') VAL
      READIF=VAL
      RETURN
      END
