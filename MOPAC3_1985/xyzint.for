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
*            DEGREE = 57.29578 IF ANGLES ARE TO BE IN RADIANS
*
***********************************************************************
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
C
C   FIND ANY ATOM TO RELATE TO NA(I)
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
      SUBROUTINE XYZGEO(XYZ,NUMAT,NA,NB,NC,DEGREE,GEO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(3,*), NA(*), NB(*), NC(*), GEO(3,*)
***********************************************************************
*
*   XYZGEO CONVERTS COORDINATES FROM CARTESIAN TO INTERNAL.
*
*     ON INPUT XYZ  = ARRAY OF CARTESIAN COORDINATES
*              NUMAT= NUMBER OF ATOMS
*              NA   = NUMBERS OF ATOM TO WHICH ATOMS ARE RELATED
*                     BY DISTANCE
*              NB   = NUMBERS OF ATOM TO WHICH ATOMS ARE RELATED
*                     BY ANGLE
*              NC   = NUMBERS OF ATOM TO WHICH ATOMS ARE RELATED
*                     BY DIHEDRAL
*
*    ON OUTPUT GEO  = INTERNAL COORDINATES IN ANGSTROMS, RADIANS,
*                     AND RADIANS
*
***********************************************************************
      DO 10 I=2,NUMAT
         J=NA(I)
         K=NB(I)
         L=NC(I)
         IF(I.LT.3) GOTO 10
         II=I
         CALL BANGLE(XYZ,II,J,K,GEO(2,I))
         GEO(2,I)=GEO(2,I)*DEGREE
         IF(I.LT.4) GOTO 10
         CALL DIHED(XYZ,II,J,K,L,GEO(3,I))
         GEO(3,I)=GEO(3,I)*DEGREE
   10 GEO(1,I)= SQRT((XYZ(1,I)-XYZ(1,J))**2+
     1                   (XYZ(2,I)-XYZ(2,J))**2+
     2                   (XYZ(3,I)-XYZ(3,J))**2)
      GEO(1,1)=0.D0
      GEO(2,1)=0.D0
      GEO(3,1)=0.D0
      GEO(2,2)=0.D0
      GEO(3,2)=0.D0
      GEO(3,3)=0.D0
      RETURN
      END
      SUBROUTINE BANGLE(XYZ,I,J,K,ANGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(3,*)
*********************************************************************
*
* BANGLE CALCULATES THE ANGLE BETWEEN ATOMS I,J, AND K. THE
*        CARTESIAN COORDINATES ARE IN XYZ.
*
*********************************************************************
      D2IJ = (XYZ(1,I)-XYZ(1,J))**2+
     1       (XYZ(2,I)-XYZ(2,J))**2+
     2       (XYZ(3,I)-XYZ(3,J))**2
      D2JK = (XYZ(1,J)-XYZ(1,K))**2+
     1       (XYZ(2,J)-XYZ(2,K))**2+
     2       (XYZ(3,J)-XYZ(3,K))**2
      D2IK = (XYZ(1,I)-XYZ(1,K))**2+
     1       (XYZ(2,I)-XYZ(2,K))**2+
     2       (XYZ(3,I)-XYZ(3,K))**2
      XY = SQRT(D2IJ*D2JK)
      TEMP = 0.5D0 * (D2IJ+D2JK-D2IK) / XY
      IF (TEMP .GT. 1.0D0) TEMP=1.0D0
      IF (TEMP .LT. -1.0D0) TEMP=-1.0D0
      ANGLE = ACOS( TEMP )
      RETURN
      END
      SUBROUTINE DIHED(XYZ,I,J,K,L,ANGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XYZ(3,*)
*********************************************************************
*
*      DIHED CALCULATES THE DIHEDRAL ANGLE BETWEEN ATOMS I, J, K,
*            AND L.  THE CARTESIAN COORDINATES OF THESE ATOMS
*            ARE IN ARRAY XYZ.
*
*     DIHED IS A MODIFIED VERSION OF A SUBROUTINE OF THE SAME NAME
*           WHICH WAS WRITTEN BY DR. W. THEIL IN 1973.
*
*********************************************************************
      XI1=XYZ(1,I)-XYZ(1,K)
      XJ1=XYZ(1,J)-XYZ(1,K)
      XL1=XYZ(1,L)-XYZ(1,K)
      YI1=XYZ(2,I)-XYZ(2,K)
      YJ1=XYZ(2,J)-XYZ(2,K)
      YL1=XYZ(2,L)-XYZ(2,K)
      ZI1=XYZ(3,I)-XYZ(3,K)
      ZJ1=XYZ(3,J)-XYZ(3,K)
      ZL1=XYZ(3,L)-XYZ(3,K)
C      ROTATE AROUND Z AXIS TO PUT KJ ALONG Y AXIS
      DIST= SQRT(XJ1**2+YJ1**2+ZJ1**2)
      COSA=ZJ1/DIST
      IF(COSA.GT.1.0D0) COSA=1.0D0
      IF(COSA.LT.-1.0D0) COSA=-1.0D0
      DDD=1.0D0-COSA**2
      IF(DDD.LE.0.0) GO TO 10
      YXDIST=DIST* SQRT(DDD)
      IF(YXDIST.GT.1.0D-9) GO TO 20
   10 CONTINUE
      XI2=XI1
      XL2=XL1
      YI2=YI1
      YL2=YL1
      COSTH=COSA
      SINTH=0.D0
      GO TO 30
   20 COSPH=YJ1/YXDIST
      SINPH=XJ1/YXDIST
      XI2=XI1*COSPH-YI1*SINPH
      XJ2=XJ1*COSPH-YJ1*SINPH
      XL2=XL1*COSPH-YL1*SINPH
      YI2=XI1*SINPH+YI1*COSPH
      YJ2=XJ1*SINPH+YJ1*COSPH
      YL2=XL1*SINPH+YL1*COSPH
C      ROTATE KJ AROUND THE X AXIS SO KJ LIES ALONG THE Z AXIS
      COSTH=COSA
      SINTH=YJ2/DIST
   30 CONTINUE
      YI3=YI2*COSTH-ZI1*SINTH
      YL3=YL2*COSTH-ZL1*SINTH
      CALL DANG(XL2,YL3,XI2,YI3,ANGLE)
      IF (ANGLE .LT. 0.) ANGLE=6.2831853D0+ANGLE
      IF (ANGLE .GE. 6.2831853D0 ) ANGLE=0.D0
      RETURN
      END
      SUBROUTINE DANG(A1,A2,B1,B2,RCOS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
**********************************************************************
*
*    DANG  DETERMINES THE ANGLE BETWEEN THE POINTS (A1,A2), (0,0),
*          AND (B1,B2).  THE RESULT IS PUT IN RCOS.
*
**********************************************************************
      PI=2.0D0* ASIN(1.0D00)
      ZERO=1.0D-6
      IF( ABS(A1).LT.ZERO.AND. ABS(A2).LT.ZERO) GO TO 10
      IF( ABS(B1).LT.ZERO.AND. ABS(B2).LT.ZERO) GO TO 10
      ANORM=1.0D0/ SQRT(A1**2+A2**2)
      BNORM=1.0D0/ SQRT(B1**2+B2**2)
      A1=A1*ANORM
      A2=A2*ANORM
      B1=B1*BNORM
      B2=B2*BNORM
      SINTH=(A1*B2)-(A2*B1)
      COSTH=A1*B1+A2*B2
      IF(COSTH.GT.1.0D0) COSTH=1.0D0
      IF(COSTH.LT.-1.0D0) COSTH=-1.0D0
      RCOS= ACOS(COSTH)
      IF( ABS(RCOS).LT.4.0D-4) GO TO 10
      IF(SINTH.GT.0.D0) RCOS=6.2831853D0-RCOS
      RCOS=-RCOS
      RETURN
   10 RCOS=0.0D0
      RETURN
      END
