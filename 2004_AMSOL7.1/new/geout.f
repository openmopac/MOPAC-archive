      SUBROUTINE GEOUT 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
********************************************************************** 
* 
*   GEOUT PRINTS THE CURRENT GEOMETRY.  IT CAN BE CALLED ANY TIME, 
*         FROM ANY POINT IN THE PROGRAM AND DOES NOT AFFECT ANYTHING. 
* 
********************************************************************** 
      COMMON /GEOM  / GEO(3,NUMATM) 
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM), 
     1NA(NUMATM),NB(NUMATM),NC(NUMATM) 
C     COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR),IDUMY,XPARAM(MAXPAR) 
      COMMON /GEOVAR/ XPARAM(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY           3GL3092
      COMMON /REPATH/ REACT(100),LATOM,LPARAM                           03GCL93
      COMMON /ELEMTS/ ELEMNT(107) 
      DIMENSION COORD(3,NUMATM) 
      CHARACTER Q(3)*1, ELEMNT*2 
      LOGICAL CART 
       SAVE
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
C
      IREAL = 0                                                         GL0492
C
      WRITE (JOUT,20) 
   20 FORMAT(/,3X,'Atom',3X,'NO.',1X,'Chemical',2X,'Bond length',4X,    DJG0896
     1       'Bond angle',4X,'Dihedral angle',/,2X,'number',2X,'(k)',   PDW1199
     2       2X,'symbol',3X,'(angstroms)',5X,'(degrees)',6X,'(degrees)',PDW1199
     3       /,3X,'(I)',21X,'NA:I',9X,'NB:NA:I',8X,'NC:NB:NA:I',4X,     DJG0896
     4       'NA',2X,'NB',2X,'NC')                                      GL0492
      N=1 
      IF (LABELS(1) .EQ. 99 .OR.                                        GL0492
     1   (LABELS(1) .LE. 106 .AND. LABELS(1) .GE. 103)) THEN            GL0492
          WRITE (JOUT,30) ELEMNT(LABELS(1))                             GDH1095
      ELSE                                                              GL0492
          IREAL = IREAL + 1                                             GL0492
          WRITE (JOUT,31)IREAL,ELEMNT(LABELS(1))                        GL0492
      ENDIF                                                             GL0492
   30 FORMAT (4X,'1',10X,A2)                                            DJG0896
   31 FORMAT (4X,'1',5X,I3,2X,A2)                                       DJG0896
      IA=LOC(1,1) 
      Q(1)=' ' 
      IF (LATOM.EQ.2) Q(1)='+' 
      IF (LOC(1,1).NE.2) GO TO 40 
      Q(1)='*' 
      N=N+1 
      IA=LOC(1,N) 
   40 CONTINUE 
      IF (LABELS(2) .EQ. 99 .OR.                                        GL0492
     1   (LABELS(2) .LE. 106 .AND. LABELS(2) .GE. 103)) THEN            GL0492
         WRITE (JOUT,50) ELEMNT(LABELS(2)),COORD(1,2),Q(1),NA(2)        GL0492
      ELSEIF (LABELS(2) .NE. 0) THEN                                    GL0492
         IREAL = IREAL + 1                                              GL0492
         WRITE(JOUT,51)IREAL,ELEMNT(LABELS(2)), COORD(1,2), Q(1), NA(2) GL0492
      ENDIF                                                             GL0492
   50 FORMAT (4X,'2',10X,A2,2X,F16.5,1X,A1,32X,I2)                      DJG0896
   51 FORMAT (4X,'2',5X,I3,2X,A2,2X,F16.5,1X,A1,32X,I2)                 DJG0896
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
      IF (LABELS(3) .EQ. 99 .OR.                                        GL0492
     1   (LABELS(3) .LE. 106 .AND. LABELS(3) .GE. 103)) THEN            GL0492
         WRITE (JOUT,80) ELEMNT(LABELS(3)),COORD(1,3),Q(1),             GL0492
     1                W,Q(2),NA(3),NB(3)                                GL0492
      ELSEIF (LABELS(3) .NE. 0) THEN                                    GL0492
              IREAL = IREAL + 1                                         GL0492
              WRITE (JOUT,81) IREAL,ELEMNT(LABELS(3)),COORD(1,3),Q(1),  GL0492
     1                     W,Q(2),NA(3),NB(3)                           GL0492
      ENDIF                                                             GL0492
   80 FORMAT (4X,'3',10X,A2,2X,F16.5,1X,A1,2X,F10.5,1X,A1,18X,2(I2,2X)) DJG0896
   81 FORMAT (4X,'3',5X,I3,2X,A2,2X,F16.5,1X,A1,2X,F10.5,1X,A1,18X,     DJG0896
     1        2(I2,2X))                                                 GL0492
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
100   IF (LABELS(I) .EQ. 99 .OR.                                        GL0492
     1   (LABELS(I) .LE. 106 .AND. LABELS(I) .GE. 103)) THEN            GL0492
            WRITE(JOUT,110)I,ELEMNT(LABELS(I)),COORD(1,I),Q(1),W,Q(2),  GL0492
     1                     X,Q(3),NA(I),NB(I),NC(I)                     GL0492
         ELSE                                                           GL0492
             IREAL = IREAL + 1                                          GL0492
            WRITE(JOUT,111)I,IREAL,ELEMNT(LABELS(I)),COORD(1,I),Q(1),W, GL0492
     1                     Q(2),X,Q(3),NA(I),NB(I),NC(I)                GL0492
         ENDIF                                                          GL0492
  110 FORMAT(2X,I3,10X,A2,2X,F16.5,1X,A1,2X,F10.5,1X,A1,2X,F10.5,1X,A1, DJG0896
     1       2X,3I4)                                                    GL0492
  111 FORMAT(2X,I3,5X,I3,2X,A2,2X,F16.5,1X,A1,2X,F10.5,1X,A1,2X,F10.5,  DJG0896
     1       1X,A1,2X,3I4)                                              GL0492
  120 CONTINUE 
      RETURN 
      END 
