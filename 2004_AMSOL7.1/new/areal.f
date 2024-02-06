      FUNCTION AREAL(RAD,NBALL,K)                                       DL0397
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GCL0493
      INCLUDE 'SIZES.i'                                                 GCL0493
      INCLUDE 'SIZES2.i'                                                GDH1093
      PARAMETER (MXSS=2*NUMATM+1)
C ----------------------------------------------------------------------
C     ACCESSIBLE SOLID ANGLE OF SPHERES (RADIANS). ANALYTICAL.
C  INPUT
C     RAD(NBALL)    : RADII.
C     NBALL         : NUMBER OF SPHERES.
C     K             : No OF THE SPHERE STUDIED.
C  OUTPUT
C     AREAL         : ACCESSIBLE SOLID ANGLE FOR SPHERE No K.
C     D. LIOTARD, DECEMBER 1992.
C     INVOKE DAREAL, DL 0397.
C ----------------------------------------------------------------------
      DIMENSION RAD(*),NC(0:NUMATM),DAREA(3,1)                          DL0397
      CALL DAREAL (RAD,NBALL,AREA,DAREA,NCROSS,NC,K,.FALSE.             DL1097
     .            ,DAREA,.FALSE.)                                       DL1097
      AREAL=AREA                                                        DL0397
      END
