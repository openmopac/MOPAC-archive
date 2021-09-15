      SUBROUTINE AXIS(COORD,NUMAT,A,B,C,SUMW)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION COORD(3,NUMAT)
**************************************************************************
*
*  AXIS CALCULATES THE THREE MOMENTS OF INERTIA AND THE MOLECULAR
*       WEIGHT.  THE MOMENTS OF INERTIA ARE RETURNED IN A, B, AND C.
*       THE MOLECULAR WEIGHT IN SUMW.
*       THE UNITS OF INERTIA ARE 10**(-40)GRAM-CM**2,
*       AND MOL.WEIGHT IN ATOMIC-MASS-UNITS. (AMU'S)
**************************************************************************
      COMMON /ATMASS/ ATMASS(NUMATM)
      DIMENSION T(6), X(NUMATM), Y(NUMATM),
     +          Z(NUMATM), ROT(3), XYZMOM(3), EIG(3), EVEC(9)
      DATA T /6*0.D0/
**************************************************************************
*     CONST1 =  10**40/(N*A*A)
*               N = AVERGADRO'S NUMBER
*               A = CM IN AN ANGSTROM
*               10**40 IS TO ALLOW UNITS TO BE 10**(-40)GRAM-CM**2
*
**************************************************************************
      CONST1 = 1.66053D0
**************************************************************************
*
*     CONST2 = CONVERSION FACTOR FROM ANGSTROM-AMU TO CM**(-1)
*            = (PLANCK'S CONSTANT)/(4*PI*PI)
*
**************************************************************************
      CONST2=16.85803902
C    FIRST WE CENTRE THE MOLECULE ABOUT THE CENTRE OF GRAVITY,
C    THIS DEPENDS ON THE ISOTOPIC MASSES, AND THE CARTESIAN GEOMETRY.
C
      SUMW=0.D0
      SUMWX=0.D0
      SUMWY=0.D0
      SUMWZ=0.D0
      DO 10 I=1,NUMAT
          WEIGHT=ATMASS(I)
          SUMW=SUMW+WEIGHT
          SUMWX=SUMWX+WEIGHT*COORD(1,I)
          SUMWY=SUMWY+WEIGHT*COORD(2,I)
  10      SUMWZ=SUMWZ+WEIGHT*COORD(3,I)
      WRITE(6,'(/10X,'' MOLECULAR WEIGHT ='',F8.2,/)')SUMW
       SUMWX=SUMWX/SUMW
       SUMWY=SUMWY/SUMW
       SUMWZ=SUMWZ/SUMW
      DO 20 I=1,NUMAT
          X(I)=COORD(1,I)-SUMWX
          Y(I)=COORD(2,I)-SUMWY
  20      Z(I)=COORD(3,I)-SUMWZ
***************************************************************************
*
*    MATRIX FOR MOMENTS OF INERTIA IS OF FORM
*
*           |   Y**2+Z**2                         |
*           |    -Y*X       Z**2+X**2             | -I =0
*           |    -Z*X        -Z*Y       X**2+Y**2 |
*
**************************************************************************
      DO 30 I=1,NUMAT
          WEIGHT=ATMASS(I)
          T(1)=T(1)+WEIGHT*(Y(I)**2+Z(I)**2)
          T(2)=T(2)-WEIGHT*X(I)*Y(I)
          T(3)=T(3)+WEIGHT*(Z(I)**2+X(I)**2)
          T(4)=T(4)-WEIGHT*Z(I)*X(I)
          T(5)=T(5)-WEIGHT*Y(I)*Z(I)
  30      T(6)=T(6)+WEIGHT*(X(I)**2+Y(I)**2)
                CALL HQRII(T,3,3,EIG,EVEC)
      WRITE(6,'(//10X,'' PRINCIPAL MOMENTS OF INERTIA IN CM(-1)'',/)')
      DO 40 I=1,3
      ROT(I)   =CONST2/ABS(EIG(I)+1.D-10)
      IF(ROT(I).GT.1.D5)ROT(I)=0.D0
  40  XYZMOM(I)=EIG(I)*CONST1
      WRITE(6,'(10X,''A ='',F12.6,''   B ='',F12.6,
     +''   C ='',F12.6,/)')(ROT(I),I=1,3)
      WRITE(6,'(//10X,'' PRINCIPAL MOMENTS OF INERTIA IN '',
     +''UNITS OF 10**(-40)*GRAM-CM**2'',/)')
      WRITE(6,'(10X,''A ='',F12.6,''   B ='',F12.6,
     +''   C ='',F12.6,/)')(XYZMOM(I),I=1,3)
      C=ROT(1)
      B=ROT(2)
      A=ROT(3)
      END
