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
      DO 30 I=2,NUMAT
         J=NA(I)
         K=NB(I)
         L=NC(I)
         IF(I.LT.3) GOTO 30
         II=I
         CALL BANGLE(XYZ,II,J,K,GEO(2,I))
         GEO(2,I)=GEO(2,I)*DEGREE
         IF(I.LT.4) GOTO 30
C
C   MAKE SURE DIHEDRAL IS MEANINGLFUL
C
         CALL BANGLE(XYZ,J,K,L,ANGL)
         TOL=0.2617994D0
         IF(ANGL.GT.3.1415926D0-TOL.OR.ANGL.LT.TOL)THEN
C
C  ANGLE IS UNSATISFACTORY, LET'S SEARCH FOR ANOTHER ATOM FOR
C  DEFINING THE DIHEDRAL.
   10       SUM=100.D0
            DO 20 I1=1,II-1
               R=(XYZ(1,I1)-XYZ(1,K))**2+
     1          (XYZ(2,I1)-XYZ(2,K))**2+
     2          (XYZ(3,I1)-XYZ(3,K))**2
               IF(R.LT.SUM.AND.I1.NE.J.AND.I1.NE.K) THEN
                  CALL BANGLE(XYZ,J,K,I1,ANGL)
                  IF(ANGL.LT.3.1415926D0-TOL.AND.ANGL.GT.TOL)THEN
                     SUM=R
                     L=I1
                     NC(II)=L
                  ENDIF
               ENDIF
   20       CONTINUE
            IF(SUM.GT.99.D0.AND.TOL.GT.0.1D0)THEN
C
C ANYTHING WITHIN 5 DEGREES?
C
               TOL=0.087266D0
               GOTO 10
            ENDIF
         ENDIF
         CALL DIHED(XYZ,II,J,K,L,GEO(3,I))
         GEO(3,I)=GEO(3,I)*DEGREE
   30 GEO(1,I)= SQRT((XYZ(1,I)-XYZ(1,J))**2+
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
