      FUNCTION DIAGI(IALPHA,IBETA,EIGA,XY,NMOS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION XY(NMECI,NMECI,NMECI,NMECI), EIGA(NMECI),
     1IALPHA(NMOS), IBETA(NMOS)
************************************************************************
*
*  CALCULATES THE ENERGY OF A MICROSTATE DEFINED BY IALPHA AND IBETA
*
************************************************************************
      X=0.0D0
      DO 20 I=1,NMOS
         IF (IALPHA(I).NE.0)THEN
            X=X+EIGA(I)
            DO 10  J=1,NMOS
               X=X+((XY(I,I,J,J)-XY(I,J,I,J))*IALPHA(J)*0.5D0 +
     1        (XY(I,I,J,J)            )*IBETA(J))
   10       CONTINUE
         ENDIF
   20 CONTINUE
      DO 40 I=1,NMOS
         IF (IBETA(I).NE.0) THEN
            X=X+EIGA(I)
            DO 30 J=1,I
   30       X=X+(XY(I,I,J,J)-XY(I,J,I,J))*IBETA(J)
         ENDIF
   40 CONTINUE
      DIAGI=X
      RETURN
      END
