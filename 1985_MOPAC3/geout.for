      SUBROUTINE GEOUT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
**********************************************************************
*
*   GEOUT PRINTS THE CURRENT GEOMETRY.  IT CAN BE CALLED ANY TIME,
*         FROM ANY POINT IN THE PROGRAM AND DOES NOT AFFECT ANYTHING.
*
**********************************************************************
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR),IDUMY,XPARAM(MAXPAR)
      COMMON /PATH  / LATOM,LPARAM,REACT(100)
      COMMON /ELEMTS/ ELEMNT(107)
      DIMENSION COORD(3,NUMATM)
      CHARACTER Q(3)*1, ELEMNT*2
      LOGICAL CART
C
C *** OUTPUT THE PARAMETER DATA.
C
      CART=.FALSE.
      IF(NA(1).NE.0) THEN
         CART=.TRUE.
         CALL XYZINT(GEO,NATOMS,NA,NB,NC,1.D0,COORD)
         NA(1)=99
      ELSE
         DO 10 I=1,NATOMS
            DO 10 J=1,3
   10    COORD(J,I)=GEO(J,I)
      ENDIF
      DEGREE=57.29577951D00
      WRITE (6,20)
   20 FORMAT (/6X,'ATOM',4X,'CHEMICAL',3X,'BOND LENGTH',4X,'BOND ANGLE'
     1,4X ,'TWIST ANGLE',/5X,'NUMBER',3X,'SYMBOL', 5X,'(ANGSTROMS)',5
     2X,'(DEGREES)',5X,'(DEGREES)',/6X,'(I)',20X,'NA:I',10X,'NB:NA:I',5
     3X,'NC:NB:NA:I',4X,'NA',2X,'NB',2X,'NC',/)
      N=1
      WRITE (6,30) ELEMNT(LABELS(1))
   30 FORMAT (8X,1H1,5X,A2)
      IA=LOC(1,1)
      Q(1)=' '
      IF (LATOM.EQ.2) Q(1)='+'
      IF (LOC(1,1).NE.2) GO TO 40
      Q(1)='*'
      N=N+1
      IA=LOC(1,N)
   40 CONTINUE
      IF(LABELS(2).NE.0)
     1WRITE (6,50) ELEMNT(LABELS(2)),COORD(1,2),Q(1),NA(2)
   50 FORMAT (8X,1H2,5X,A2,F16.5,1X,A1,34X,I2)
      DO 60 J=1,2
         Q(J)=' '
         IF (IA.NE.3) GO TO 60
         IF (LOC(2,N).NE.J) GO TO 60
         Q(J)='*'
         N=N+1
         IA=LOC(1,N)
   60 CONTINUE
      W = COORD(2,3) * DEGREE
      IF (LATOM.NE.3) GO TO 70
      J=LPARAM
      Q(J)='+ '
   70 CONTINUE
      IF(LABELS(3).NE.0)
     1WRITE (6,80) ELEMNT(LABELS(3)),COORD(1,3),Q(1),
     2              W,Q(2),NA(3),NB(3)
   80 FORMAT (8X,1H3,5X,A2,F16.5,1X,A1,F15.5,1X,A1,17X,2(I2,2X))
      IF (NATOMS.LT.4) RETURN
      DO 120 I=4,NATOMS
         DO 90 J=1,3
            Q(J)=' '
            IF (IA.NE.I) GO TO 90
            IF (J.NE.LOC(2,N)) GO TO 90
            Q(J)='*'
            N=N+1
            IA=LOC(1,N)
   90    CONTINUE
         W = COORD(2,I) * DEGREE
         X = COORD(3,I) * DEGREE
         IF (LATOM.NE.I) GO TO 100
         J=LPARAM
         Q(J)='+ '
  100    WRITE (6,110) I,ELEMNT(LABELS(I)),COORD(1,I),Q(1),W,Q(2),
     1                 X,Q(3),NA(I),NB(I),NC(I)
  110    FORMAT (7X,I2 ,5X,A2,F16.5,1X,A1,F15.5,1X,A1,F12.5,1X,A1,I5,2I4
     1)
  120 CONTINUE
      RETURN
      END
