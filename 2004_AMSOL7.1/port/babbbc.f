      DOUBLE PRECISION FUNCTION BABBBC(IOCCA1, IOCCB1, IOCCA2, 
     1                                 IOCCB2, NMOS) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION IOCCA1(NMOS), IOCCB1(NMOS), IOCCA2(NMOS), IOCCB2(NMOS) 
       INCLUDE 'SIZES.i'
*********************************************************************** 
* 
* BABBBC EVALUATES THE C.I. MATRIX ELEMENT FOR TWO MICROSTATES DIFFERING 
*       BY ONE BETA ELECTRON. THAT IS, ONE MICROSTATE HAS A BETA 
*       ELECTRON IN PSI(I) AND THE OTHER MICROSTATE HAS AN ELECTRON IN 
*       PSI(J). 
*********************************************************************** 
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI) 
C     COMMON /BASEOC/ OCCA(NMECI) 
      COMMON /BASEOC/ OCCA(NMECI), IBDUM(NMECI+2)                       3GL3092 
       SAVE
      DO 10 I=1,NMOS 
   10 IF(IOCCB1(I).NE.IOCCB2(I)) GOTO 20 
   20 IJ=0 
      DO 30 J=I+1,NMOS 
         IF(IOCCB1(J).NE.IOCCB2(J)) GOTO 40 
   30 IJ=IJ+IOCCA1(J)+IOCCB1(J) 
   40 IJ=IJ+IOCCA1(J) 
C 
C   THE UNPAIRED M.O.S ARE I AND J 
      SUM=0.D0 
      DO 50 K=1,NMOS 
   50 SUM=SUM+ (XY(I,J,K,K)-XY(I,K,J,K))*(IOCCB1(K)-OCCA(K)) + 
     1          XY(I,J,K,K)             *(IOCCA1(K)-OCCA(K)) 
      BABBBC=SUM*DBLE(1-2*MOD(IJ,2))                                    GCL0393
      RETURN 
      END 
