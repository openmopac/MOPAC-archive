      SUBROUTINE GEOUT(*)                                               CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
**********************************************************************
*
*   GEOUT PRINTS THE CURRENT GEOMETRY.  IT CAN BE CALLED ANY TIME,
*         FROM ANY POINT IN THE PROGRAM AND DOES NOT AFFECT ANYTHING.
*
**********************************************************************
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOVAR/ XPARAM(MAXPAR), NVAR, LOC(2,MAXPAR)               IR0394
C    changed common path for portability  (IR)
      COMMON /PATHI / LATOM,LPARAM
      COMMON /PATHR / REACT(200)
c
      COMMON /ELEMTS/ ELEMNT(120)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION COORD(3,NUMATM)
      CHARACTER Q(3)*1, ELEMNT*2
      LOGICAL CART
CSAV         SAVE                                                           GL0892
C
C *** OUTPUT THE PARAMETER DATA.
C
      CART=.FALSE.
      IF(NA(1).NE.0) THEN
         CART=.TRUE.
         CALL XYZINT(GEO,NATOMS,NA,NB,NC,1.D0,COORD,*9999)               CSTP(call)
         NA(1)=99
      ELSE
         DO 10 I=1,NATOMS
            DO 10 J=1,3
   10    COORD(J,I)=GEO(J,I)
      ENDIF
      DEGREE=57.29577951D00
      IF (DOPRNT) WRITE (6,20)                                          CIO
   20 FORMAT (/4X,'ATOM',3X,'CHEMICAL',2X,'BOND LENGTH',4X,'BOND ANGLE'
     1,4X ,' TWIST ANGLE',/3X,'NUMBER',2X,'SYMBOL', 4X,'(ANGSTROMS)',5
     2X,'(DEGREES)',5X,' (DEGREES)',/4X,'(I)',18X,'NA:I',10X,'NB:NA:I',5
     3X,' NC:NB:NA:I',5X,'NA',3X,'NB',3X,'NC',/)
      N=1
      IA=LOC(1,1)
      DO 50 I=1,NATOMS
         DO 30 J=1,3
            Q(J)=' '
            IF (IA.NE.I) GO TO 30
            IF (J.NE.LOC(2,N)) GO TO 30
            Q(J)='*'
            N=N+1
            IA=LOC(1,N)
   30    CONTINUE
         W = COORD(2,I) * DEGREE
         X = COORD(3,I) * DEGREE
         IF (LATOM.NE.I) GO TO 40
         J=LPARAM
         Q(J)='+ '
   40    CONTINUE
         IF(LABELS(I).NE.0)THEN
            IF(I.GT.3)THEN
               IF (DOPRNT) THEN                                         CIO
               WRITE (6,'(3X,I4 ,5X,A2,F16.5,1X,A1,F15.5,1X,A1,F12.5,1X,
     1A1,I5,2I5)') I,ELEMNT(LABELS(I)),COORD(1,I),Q(1),W,Q(2),X,Q(3),NA(
     2I),NB(I),NC(I)
               ENDIF                                                    CIO
            ELSEIF(I.EQ.3)THEN
               IF (DOPRNT) THEN                                         CIO
               WRITE (6,'(''      3'',5X,A2,F16.5,1X,A1,F15.5,1X,A1,14X,
     12I5)') ELEMNT(LABELS(3)),COORD(1,3),Q(1),W,Q(2),NA(3),NB(3)
               ENDIF                                                    CIO
            ELSEIF(I.EQ.2)THEN
               IF (DOPRNT) THEN                                         CIO
               WRITE (6,'(''      2'',5X,A2,F16.5,1X,A1,31X,I5)')
     1 ELEMNT(LABELS(2)),COORD(1,2),Q(1),NA(2)
               ENDIF                                                    CIO
            ELSE
               IF (DOPRNT)                                              CIO 
     &            WRITE (6,'(''      1'',5X,A2)') ELEMNT(LABELS(1))     CIO
            ENDIF
         ENDIF
   50 CONTINUE
      RETURN
 9999 RETURN 1                                                          CSTP
      END
