      SUBROUTINE GEOUT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES/NOLIST'
**********************************************************************
*
*   GEOUT PRINTS THE CURRENT GEOMETRY.  IT CAN BE CALLED ANY TIME,
*         FROM ANY POINT IN THE PROGRAM AND DOES NOT AFFECT ANYTHING. 
*
**********************************************************************
      COMMON /GMETRY/ GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     +NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR),XPARAM(MAXPAR)
      COMMON /PATH  / LATOM,LPARAM,REACT(100)
      CHARACTER Q(3)*1, ELEMNT(99)*2
      DATA ELEMNT/' H','He',
     2 'Li','Be',' B',' C',' N',' O',' F','Ne',
     3 'Na','Mg','Al','Si',' P',' S','Cl','Ar',
     4 ' K','Ca','Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu',
     4 'Zn','Ga','Ge','As','Se','Br','Kr',
     5 'Rb','Sr',' Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
     5 'Cd','In','Sn','Sb','Te',' I','Xe',
     6 'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy',
     6 'Ho','Er','Tm','Yb','Lu','Hf','Ta',' W','Re','Os','Ir','Pt',
     6 'Au','Hg','Tl','Pb','Bi','Po','At','Rn',
     7 'Fr','Ra','Ac','Th','Pa',' U','Np','Pu','Am','Cm','Bk','Cf','XX'/
C
C *** OUTPUT THE PARAMETER DATA.
C
      IF(NA(1).NE.0) THEN
      DEGREE=1.D0
      WRITE(6,'(/10X,'' IGNORE CONNECTIVITY'',/)')
      WRITE (6,131)
  131 FORMAT (/6X,'ATOM',4X,'CHEMICAL',7X,' CARTESIAN COORDINATES'
     1,/5X,'NUMBER',3X,'SYMBOL', 14X,'(ANGSTROMS)',/6X,'(I)',17X,'  X '
     +,12X,'   Y   ',7X,'   Z   ')
      ELSE      
      DEGREE=57.29577951D00
      WRITE (6,130)
  130 FORMAT (/6X,'ATOM',4X,'CHEMICAL',3X,'BOND LENGTH',4X,'BOND ANGLE'
     1,4X ,'TWIST ANGLE',/5X,'NUMBER',3X,'SYMBOL', 5X,'(ANGSTROMS)',5
     2X,'(DEGREES)',5X,'(DEGREES)',/6X,'(I)',20X,'NA:I',10X,'NB:NA:I',5
     3X,'NC:NB:NA:I',4X,'NA',2X,'NB',2X,'NC',/)
      ENDIF
      N=1
      WRITE (6,140) ELEMNT(LABELS(1))
  140 FORMAT (8X,1H1,5X,A2)
      IA=LOC(1,1)
      Q(1)=' '
      IF (LATOM.EQ.2) Q(1)='+'
      IF (LOC(1,1).NE.2) GO TO 60
      Q(1)='*'
      N=N+1
      IA=LOC(1,N)
   60 CONTINUE
      IF(LABELS(2).NE.0)
     +WRITE (6,150) ELEMNT(LABELS(2)),GEO(1,2),Q(1),NA(2)
  150 FORMAT (8X,1H2,5X,A2,F16.5,1X,A1,34X,I2)
      DO 70 J=1,2
         Q(J)=' '
         IF (IA.NE.3) GO TO 70
         IF (LOC(2,N).NE.J) GO TO 70
         Q(J)='*'
         N=N+1
         IA=LOC(1,N)
   70 CONTINUE
      W = GEO(2,3) * DEGREE
      IF (LATOM.NE.3) GO TO 80
      J=LPARAM
      Q(J)='+ '
   80 CONTINUE
      IF(LABELS(3).NE.0)
     +WRITE (6,160) ELEMNT(LABELS(3)),GEO(1,3),Q(1),W,Q(2),NA(3),NB(3)
  160 FORMAT (8X,1H3,5X,A2,F16.5,1X,A1,F15.5,1X,A1,17X,2(I2,2X))
      IF (NATOMS.LT.4) RETURN
      DO 110 I=4,NATOMS
         DO 90 J=1,3
            Q(J)=' '
            IF (IA.NE.I) GO TO 90
            IF (J.NE.LOC(2,N)) GO TO 90
            Q(J)='*'
            N=N+1
            IA=LOC(1,N)
   90    CONTINUE
         W = GEO(2,I) * DEGREE
         X = GEO(3,I) * DEGREE
         IF (LATOM.NE.I) GO TO 100
         J=LPARAM
         Q(J)='+ '
  100    WRITE (6,170) I,ELEMNT(LABELS(I)),GEO(1,I),Q(1),W,Q(2),
     +                 X,Q(3),NA(I),NB(I),NC(I)
  170 FORMAT (7X,I2 ,5X,A2,F16.5,1X,A1,F15.5,1X,A1,F12.5,1X,A1,I5,2I4)
  110 CONTINUE
      RETURN
      END
