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
