      DOUBLE PRECISION FUNCTION AABABC(IOCCA1, IOCCB1, IOCCA2, 
     1                                 IOCCB2, NMOS) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION IOCCA1(NMOS), IOCCB1(NMOS), IOCCA2(NMOS), IOCCB2(NMOS) 
       INCLUDE 'SIZES.i'
*********************************************************************** 
* 
* AABABC EVALUATES THE C.I. MATRIX ELEMENT FOR TWO MICROSTATES DIFFERING 
*       BY BETA ELECTRON. THAT IS, ONE MICROSTATE HAS A BETA ELECTRON 
*       IN PSI(I) WHICH, IN THE OTHER MICROSTATE IS IN PSI(J) 
* 
*********************************************************************** 
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI) 
C     COMMON /BASEOC/ OCCA(NMECI) 
      COMMON /BASEOC/ OCCA(NMECI), IBDUM(NMECI+2)                       3GL3092
       SAVE
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
      AABABC=SUM*DBLE(1-2*MOD(IJ,2))                                    GCL0393
      RETURN 
      END 
