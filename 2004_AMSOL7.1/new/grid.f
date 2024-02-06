      SUBROUTINE GRID 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
************************************************************************ 
* 
*  GRID CALCULATES THE ENERGY-SURFACE RESULTING FROM VARIATION OF 
*       TWO COORDINATES. THE STEP-SIZE IS STEP1 AND STEP2, AND A 11 
*       BY 11 GRID OF POINTS IS GENERATED 
* 
************************************************************************ 
      COMMON /GEOM  / GEO(3,NUMATM) 
C     COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR) 
      COMMON /GEOVAR/ XPARAM(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY           3GL3092
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM 
      COMMON /GRAVEC/ COSINE 
      COMMON /MESH  / LATOM1, LPARA1, LATOM2, LPARA2 
      DIMENSION SURFAC(11,11) 
      LOGICAL BFGS                                                      DJG0495
       SAVE
      STEP1=FISTE1                                                      DJG0995
      STEP2=FISTE2                                                      DJG0995
      BFGS=(IBFGS.NE.0.OR.IDFP.NE.0)                                    DJG0995
C 
C  THE CENTRAL VALUE OF THE FIRST AND SECOND DIMENSIONS ARE 
C      GEO(LPARA1,LATOM1) AND GEO(LPARA2,LATOM2) 
      NPTS=11 
C NPTS MUST BE ODD, IN ORDER TO HAVE A CENTER POINT. 
      NPTS2=NPTS/2 
      DEGREE=180.D0/3.14159265359D0 
      IF(LPARA1.NE.1)STEP1=STEP1/DEGREE 
      IF(LPARA2.NE.1)STEP2=STEP2/DEGREE 
      START1=GEO(LPARA1,LATOM1)-(NPTS2+1)*STEP1 
      START2=GEO(LPARA2,LATOM2)-(NPTS2+1)*STEP2 
C 
C  NOW TO SWEEP THROUGH THE GRID OF POINTS LEFT TO RIGHT THEN RIGHT 
C  TO LEFT. THIS SHOULD AVOID THE GEOMETRY OR SCF GETTING MESSED UP. 
C 
      GEO(LPARA1,LATOM1)=START1 
      GEO(LPARA2,LATOM2)=START2 
      IONE=-1.D0 
      IF(LPARA1.NE.1) THEN 
         C1=DEGREE 
      ELSE 
         C1=1.D0 
      ENDIF 
      IF(LPARA2.NE.1) THEN 
         C2=DEGREE 
      ELSE 
         C2=1.D0 
      ENDIF 
      WRITE(JOUT,
     .     '(''   FIRST VARIABLE   SECOND VARIABLE   FUNCTION'')') 
      DO 20 ILOOP=1,NPTS 
         GEO(LPARA1,LATOM1)=GEO(LPARA1,LATOM1)+STEP1 
         IONE=-IONE 
         JLOOP1=0 
         IF(IONE.LT.0)JLOOP1=NPTS+1 
         DO 10 JLOOP=1,NPTS 
            JLOOP1=JLOOP1+IONE 
            GEO(LPARA2,LATOM2)=GEO(LPARA2,LATOM2)+STEP2*IONE 
            CALL FLEPO(XPARAM, NVAR, ESCF) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
            SURFAC(ILOOP,JLOOP1)=ESCF 
            WRITE(JOUT
     .      ,'('' :'',F16.5,F16.5,F13.6)')GEO(LPARA1,LATOM1)*C1, 
     1        GEO(LPARA2,LATOM2)*C2,ESCF 
   10    CONTINUE 
         GEO(LPARA2,LATOM2)=GEO(LPARA2,LATOM2)+STEP2*IONE 
   20 CONTINUE 
      WRITE(JOUT,'(/10X,''HORIZONTAL: VARYING SECOND PARAMETER,'', 
     1          /10X,''VERTICAL:   VARYING FIRST PARAMETER'')') 
      WRITE(JOUT,
     .    '(/10X,''WHOLE OF GRID, SUITABLE FOR PLOTTING'',//)') 
      DO 30 I=1,NPTS 
   30 WRITE(JOUT,'(11F7.2)')(SURFAC(J,I),J=1,NPTS) 
      RETURN                                                            GDH0495
      END 
