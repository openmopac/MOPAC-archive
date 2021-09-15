      SUBROUTINE XYZINT(XYZ,NUMAT,NA,NB,NC,DEGREE,GEO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(3,*), NA(*), NB(*), NC(*), GEO(3,*)
***********************************************************************
*
* XYZINT WORKS OUT THE INTERNAL COORDINATES OF A MOLECULE.
*        THE "RULES" FOR THE CONNECTIVITY ARE AS FOLLOWS:
*        ATOM I IS DEFINED AS BEING AT A DISTANCE FROM THE NEAREST
*        ATOM J, ATOM J ALREADY HAVING BEEN DEFINED.
*        ATOM I MAKES AN ANGLE WITH ATOM J AND THE ATOM K, WHICH HAS
*        ALREADY BEEN DEFINED, AND IS THE NEAREST ATOM TO J
*        ATOM I MAKES A DIHEDRAL ANGLE WITH ATOMS J, K, AND L. L HAVING
*        BEEN DEFINED AND IS THE NEAREST ATOM TO K, AND J, K AND L
*        HAVE A CONTAINED ANGLE IN THE RANGE 15 TO 165 DEGREES,
*        IF POSSIBLE.
*
*        IF(NA(2).EQ.1 THEN THE ORIGINAL CONNECTIVITY IS USED.
*
*        NOTE THAT GEO AND XYZ MUST NOT BE THE SAME IN THE CALL.
*
*   ON INPUT XYZ    = CARTESIAN ARRAY OF NUMAT ATOMS
*            DEGREE = 1 IF ANGLES ARE TO BE IN RADIANS
*            DEGREE = 57.29578 IF ANGLES ARE TO BE IN DEGREES
*
***********************************************************************
      COMMON /GEOOK/ IGEOOK
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IGEOOK=99
      NAI1=0
      NAI2=0
      IF(.NOT.FIRST.AND.NA(2).EQ.-1)THEN
         NA(2)=1
         DO 10 I=2,NUMAT
            J=NA(I)
            IF(I.GT.3)CALL DIHED(XYZ,I,J,NB(I),NC(I),GEO(3,I))
            IF(I.GT.2)CALL BANGLE(XYZ,I,J,NB(I),GEO(2,I))
            GEO(1,I)=SQRT((XYZ(1,I)-XYZ(1,J))**2+
     1          (XYZ(2,I)-XYZ(2,J))**2+
     2          (XYZ(3,I)-XYZ(3,J))**2)
   10    CONTINUE
      ELSE
         IF(NA(2).EQ.-1)FIRST=.FALSE.
         DO 30 I=1,NUMAT
            NA(I)=2
            NB(I)=3
            NC(I)=4
            IM1=I-1
            IF(IM1.EQ.0)GOTO 30
            SUM=1.D30
            DO 20 J=1,IM1
               R=(XYZ(1,I)-XYZ(1,J))**2+
     1          (XYZ(2,I)-XYZ(2,J))**2+
     2          (XYZ(3,I)-XYZ(3,J))**2
               IF(R.LT.SUM.AND.NA(J).NE.J.AND.NB(J).NE.J) THEN
                  SUM=R
                  K=J
               ENDIF
   20       CONTINUE
C
C   ATOM I IS NEAREST TO ATOM K
C
            NA(I)=K
            IF(I.GT.2)NB(I)=NA(K)
            IF(I.GT.3)NC(I)=NB(K)
C
C   FIND ANY ATOM TO RELATE TO NA(I)
C
   30    CONTINUE
      ENDIF
      NA(1)=0
      NB(1)=0
      NC(1)=0
      NB(2)=0
      NC(2)=0
      NC(3)=0
      CALL XYZGEO(XYZ,NUMAT,NA,NB,NC,DEGREE,GEO)
      RETURN
      END
