      FUNCTION AABABC(IOCCA1, IOCCB1, IOCCA2, IOCCB2, NMOS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION IOCCA1(NMOS), IOCCB1(NMOS), IOCCA2(NMOS), IOCCB2(NMOS)
***********************************************************************
*
* AABABC EVALUATES THE C.I. MATRIX ELEMENT FOR TWO MICROSTATES DIFFERING
*       BY BETA ELECTRON. THAT IS, ONE MICROSTATE HAS A BETA ELECTRON
*       IN PSI(I) WHICH, IN THE OTHER MICROSTATE IS IN PSI(J)
*
***********************************************************************
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      COMMON /BASEOC/ OCCA(NMECI),KDUM(NMECI+2)                         JZ0315
CSAV         SAVE                                                           GL0892
      DO 10 I=1,NMOS
   10 IF(IOCCA1(I).NE.IOCCA2(I)) GOTO 20
   20 IJ=IOCCB1(I)
      DO 30 J=I+1,NMOS
         IF(IOCCA1(J).NE.IOCCA2(J)) GOTO 40
   30 IJ=IJ+IOCCA1(J)+IOCCB1(J)
   40 SUM=0.D0
      DO 50 K=1,NMOS
   50 SUM=SUM+ (XY(I,J,K,K)-XY(I,K,J,K))*(IOCCA1(K)-OCCA(K)) +
     1          XY(I,J,K,K)             *(IOCCB1(K)-OCCA(K))
      AABABC=SUM*((-1)**(IJ-(IJ/2)*2))
      RETURN
      END
