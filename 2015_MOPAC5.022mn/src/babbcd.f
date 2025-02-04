      FUNCTION BABBCD(IOCCA1, IOCCB1, IOCCA2, IOCCB2, NMOS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION IOCCA1(NMOS), IOCCB1(NMOS), IOCCA2(NMOS), IOCCB2(NMOS)
***********************************************************************
*
* BABBCD EVALUATES THE C.I. MATRIX ELEMENT FOR TWO MICROSTATES DIFFERING
*       BY TWO BETA MOS. ONE MICROSTATE HAS BETA ELECTRONS IN
*       M.O.S PSI(I) AND PSI(J) FOR WHICH THE OTHER MICROSTATE HAS
*       ELECTRONS IN PSI(K) AND PSI(L)
*
***********************************************************************
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
CSAV         SAVE                                                           GL0892
      IJ=0
      DO 10 I=1,NMOS
   10 IF(IOCCB1(I) .LT. IOCCB2(I)) GOTO 20
   20 DO 30 J=I+1,NMOS
         IF(IOCCB1(J) .LT. IOCCB2(J)) GOTO 40
   30 IJ=IJ+IOCCA2(J)+IOCCB2(J)
   40 IJ=IJ+IOCCA2(J)
      DO 50 K=1,NMOS
   50 IF(IOCCB1(K) .GT. IOCCB2(K)) GOTO 60
   60 DO 70 L=K+1,NMOS
         IF(IOCCB1(L) .GT. IOCCB2(L)) GOTO 80
   70 IJ=IJ+IOCCA1(L)+IOCCB1(L)
   80 IJ=IJ+IOCCA1(L)
      IF((IJ/2)*2.EQ.IJ) THEN
         ONE=1.D0
      ELSE
         ONE=-1.D0
      ENDIF
      BABBCD=(XY(I,K,J,L)-XY(I,L,J,K))*ONE
      RETURN
      END
