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
*        BEEN DEFINED AND IS THE NEAREST ATOM TO K
*
*        NOTE THAT GEO AND XYZ MUST NOT BE THE SAME IN THE CALL.
*
*   ON INPUT XYZ    = CARTESIAN ARRAY OF NUMAT ATOMS
*            DEGREE = 1 IF ANGLES ARE TO BE IN RADIANS
*            DEGREE = 57.29578 IF ANGLES ARE TO BE IN DEGREES
*
***********************************************************************
      SAVE
      NAI1=0
      NAI2=0
      DO 20 I=1,NUMAT
         NA(I)=2
         NB(I)=3
         NC(I)=4
         IM1=I-1
         IF(IM1.EQ.0)GOTO 20
         SUM=100.D0
         DO 10 J=1,IM1
            R=(XYZ(1,I)-XYZ(1,J))**2+
     1          (XYZ(2,I)-XYZ(2,J))**2+
     2          (XYZ(3,I)-XYZ(3,J))**2
            IF(R.LT.SUM.AND.NA(J).NE.J.AND.NB(J).NE.J) THEN
               SUM=R
               K=J
            ENDIF
   10    CONTINUE
C
C   ATOM I IS NEAREST TO ATOM K
C
         NA(I)=K
         IF(I.GT.2)NB(I)=NA(K)
         IF(I.GT.3)NC(I)=NB(K)
         IF(I.LE.3) GOTO 20
C
CJIABO
C   CHECK IF ATOMS I,NA(I),NB(I)
C
      DISIA=SQRT((XYZ(1,I)-XYZ(1,NA(I)))**2+
     &           (XYZ(2,I)-XYZ(2,NA(I)))**2+
     &           (XYZ(3,I)-XYZ(3,NA(I)))**2)
      DISIB=SQRT((XYZ(1,I)-XYZ(1,NB(I)))**2+
     &           (XYZ(2,I)-XYZ(2,NB(I)))**2+
     &           (XYZ(3,I)-XYZ(3,NB(I)))**2)
      DISAB=SQRT((XYZ(1,NA(I))-XYZ(1,NB(I)))**2+
     &           (XYZ(2,NA(I))-XYZ(2,NB(I)))**2+
     &           (XYZ(3,NA(I))-XYZ(3,NB(I)))**2)
      IF(ABS(DISIB-DISIA-DISAB).LT.0.00000001D0) GOTO 20
C
C   CHECK IF ATOMS NA(I),NB(I),NC(I) are colinear or not
C
      DISBC=SQRT((XYZ(1,NC(I))-XYZ(1,NB(I)))**2+
     &           (XYZ(2,NC(I))-XYZ(2,NB(I)))**2+
     &           (XYZ(3,NC(I))-XYZ(3,NB(I)))**2)
      DISAC=SQRT((XYZ(1,NC(I))-XYZ(1,NA(I)))**2+
     &           (XYZ(2,NC(I))-XYZ(2,NA(I)))**2+
     &           (XYZ(3,NC(I))-XYZ(3,NA(I)))**2)
      IF(ABS(DISAC-DISAB-DISBC).LT.0.000000001D0) THEN
              DO 30 L=1,I-1
              IF(L.EQ.NA(I).OR.L.EQ.NB(I)) GOTO 30
              NC(I)=L
           DISBC=SQRT((XYZ(1,NC(I))-XYZ(1,NB(I)))**2+
     &                (XYZ(2,NC(I))-XYZ(2,NB(I)))**2+
     &                (XYZ(3,NC(I))-XYZ(3,NB(I)))**2)
           DISAC=SQRT((XYZ(1,NC(I))-XYZ(1,NA(I)))**2+
     &                (XYZ(2,NC(I))-XYZ(2,NA(I)))**2+
     &                (XYZ(3,NC(I))-XYZ(3,NA(I)))**2)
           IF(ABS(DISAC-DISAB-DISBC).GT.0.00000001D0) GOTO 40
  30          CONTINUE
  40          CONTINUE
      END IF
C
CLI
C
   20 CONTINUE
      NA(1)=0
      NB(1)=0
      NC(1)=0
      NB(2)=0
      NC(2)=0
      NC(3)=0
      CALL XYZGEO(XYZ,NUMAT,NA,NB,NC,DEGREE,GEO)
      RETURN
      END

