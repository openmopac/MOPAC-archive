      SUBROUTINE ROTATE (NI,NJ,XI,XJ,W,KR,E1B,E2A,ENUC,CUTOFF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XI(3),XJ(3),W(100),E1B(10),E2A(10)
      COMMON /NATORB/ NATORB(107)
      COMMON /TWOEL3/ F03(107)
      COMMON /ALPHA3/ ALP3(153)
      COMMON /ALPHA / ALP(107)
      COMMON /CORE  / TORE(107)
      COMMON /IDEAS / FN1(107,10),FN2(107,10),FN3(107,10)
C***********************************************************************
C
C   ROTATE CALCULATES THE TWO-PARTICLE INTERACTIONS.
C
C   ON INPUT  NI     = ATOMIC NUMBER OF FIRST ATOM.
C             NJ     = ATOMIC NUMBER OF SECOND ATOM.
C             XI     = COORDINATE OF FIRST ATOM.
C             XJ     = COORDINATE OF SECOND ATOM.
C
C ON OUTPUT W      = ARRAY OF TWO-ELECTRON REPULSION INTEGRALS.
C           E1B,E2A= ARRAY OF ELECTRON-NUCLEAR ATTRACTION INTEGRALS,
C                    E1B = ELECTRON ON ATOM NI ATTRACTING NUCLEUS OF NJ.
C           ENUC   = NUCLEAR-NUCLEAR REPULSION TERM.
C
C***********************************************************************
      COMMON /ROTDUM/ CSS1,CSP1,CPPS1,CPPP1,CSS2,CSP2,CPPS2,CPPP2
      COMMON /ROTDU2/ X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3
      COMMON /KEYWRD/ KEYWRD
      CHARACTER*80 KEYWRD
      DIMENSION X(3),Y(3),Z(3),RI(22),CORE(4,2)
      LOGICAL SI,SK, AM1
      EQUIVALENCE (CORE(1,1),CSS1),(X(1),X1),(Y(1),Y1),(Z(1),Z1)
      DATA ITYPE /1/
C
C *** THIS ROUTINE COMPUTES THE REPULSION AND NUCLEAR ATTRACTION
C     INTEGRALS OVER MOLECULAR-FRAME COORDINATES.  THE INTEGRALS OVER
C     LOCAL FRAME COORDINATES ARE EVALUATED BY SUBROUTINE REPP AND
C     STORED AS FOLLOWS (WHERE P-SIGMA = O,   AND P-PI = P AND P* )
C     IN RI
C     (SS/SS)=1,   (SO/SS)=2,   (OO/SS)=3,   (PP/SS)=4,   (SS/OS)=5,
C     (SO/SO)=6,   (SP/SP)=7,   (OO/SO)=8,   (PP/SO)=9,   (PO/SP)=10,
C     (SS/OO)=11,  (SS/PP)=12,  (SO/OO)=13,  (SO/PP)=14,  (SP/OP)=15,
C     (OO/OO)=16,  (PP/OO)=17,  (OO/PP)=18,  (PP/PP)=19,  (PO/PO)=20,
C     (PP/P*P*)=21,   (P*P/P*P)=22.
C
      RIJ=0.D0
      DO 10 I=1,3
         X(I)=XI(I)-XJ(I)
   10 RIJ=RIJ+X(I)**2
      IF(RIJ.LT.0.2D0) THEN
         DO 20 I=1,10
            E1B(I)=0.D0
   20    E2A(I)=0.D0
         W(KR)=0.D0
         ENUC=0.D0
         RETURN
      ENDIF
   30 GOTO (40,50,110) ITYPE
   40 CONTINUE
      IF(INDEX(KEYWRD,'MINDO') .NE. 0) THEN
         ITYPE=2
      ELSE
         AM1= (INDEX(KEYWRD,'AM1') .NE. 0)
         ITYPE=3
      ENDIF
      GOTO 30
   50 CONTINUE
      SUM=14.399D0/SQRT(RIJ+(7.1995D0/F03(NI)+7.1995D0/F03(NJ))**2)
      W(1)=SUM
      KR=KR+1
      L=0
      DO 70 I=1,4
         DO 60 J=1,I
            L=L+1
            E1B(L)=0.D0
   60    E2A(L)=0.D0
         E1B(L)=-SUM*TORE(NJ)
   70 E2A(L)=-SUM*TORE(NI)
      II=MAX(NI,NJ)
      NBOND=(II*(II-1))/2+NI+NJ-II
      RIJ=SQRT(RIJ)
      SCALE=0
      IF(NATORB(NI).EQ.0) SCALE=EXP(-ALP(NI)*RIJ)
      IF(NATORB(NJ).EQ.0) SCALE=SCALE+EXP(-ALP(NI)*RIJ)
      IF(NBOND.LT.154) THEN
         IF(NBOND.EQ.22 .OR. NBOND .EQ. 29) GO TO 80
         GO TO 90
   80    SCALE=ALP3(NBOND)*EXP(-RIJ)
         GO TO 100
   90    SCALE=EXP(-ALP3(NBOND)*RIJ)
  100    CONTINUE
      ENDIF
      IF(ABS(TORE(NI)).GT.20.AND.ABS(TORE(NJ)).GT.20) THEN
         ENUC=0.D0
      ELSEIF (RIJ.LT.1.D0.AND.NATORB(NI)*NATORB(NJ).EQ.0) THEN
         ENUC=0.D0
      ELSE
         ENUC=TORE(NI)*TORE(NJ)*SUM+
     1         ABS(TORE(NI)*TORE(NJ)*(14.399D0/RIJ-SUM)*SCALE)
      ENDIF
      RETURN
  110 CONTINUE
      RIJ=MIN(SQRT(RIJ),CUTOFF)
      CALL REPP(NI,NJ,RIJ,RI,CORE)
      GAM=RI(1)
C
C *** THE REPULSION INTEGRALS OVER MOLECULAR FRAME (W) ARE STORED IN THE
C     ORDER IN WHICH THEY WILL LATER BE USED.  IE.  (I,J/K,L) WHERE
C     J.LE.I  AND  L.LE.K     AND L VARIES MOST RAPIDLY AND I LEAST
C     RAPIDLY.  (ANTI-NORMAL COMPUTER STORAGE)
C
      A=1.D0/RIJ
      DO 120 I=1,3
  120 X(I)=X(I)*A
      Z(3)=0.D0
      IF(ABS(X(3)).GT.0.99999999D0) THEN
         X(3)=SIGN(1.D0,X(3))
         GOTO 130
      ENDIF
      Z(3)=SQRT(1.D0-X(3)**2)
      A=1.D0/Z(3)
      Y(1)=-A*X(2)*SIGN(1.D0,X(1))
      Y(2)=ABS(A*X(1))
      Y(3)=0.D0
      Z(1)=-A*X(1)*X(3)
      Z(2)=-A*X(2)*X(3)
      GOTO 140
  130 Y(1)=0.D0
      Y(2)=1.D0
      Y(3)=0.D0
      Z(1)=1.D0
      Z(2)=0.D0
  140 CONTINUE
      IB=MIN(NATORB(NI),4)
      JB=MIN(NATORB(NJ),4)
      KI=0
      DO 270 I=1,IB
         SI=I.EQ.1
         II=I-1
         DO 270 J=1,I
            JJ=J-1
            IJ=0
            IF (JJ.EQ.0) IJ=-1
            IF (SI) IJ=+1
            DO 270 K=1,JB
               KK=K-1
               SK=KK.GT.0
               DO 270 L=1,K
                  KI=KI+1
                  IF (SK) GO TO 180
C
C *** INTEGRAL (I,J/K,L) IS OF THE TYPE (I,J/S,S)
C
                  IF (IJ) 160, 170, 150
C
C     (SS/SS)
C
  150             W(KI)=RI(1)
                  GO TO 260
C
C     (PS/SS)
C
  160             W(KI)=RI(2)*X(II)
                  GO TO 260
C
C     (PP/SS)
C
  170             W(KI)=RI(3)*X(II)*X(JJ)+RI(4)*(Y(II)*Y(JJ)+Z(II)*Z(JJ)
     1)
                  GO TO 260
  180             LL=L-1
                  IF (LL.GT.0) GO TO 220
C
C *** INTEGRAL (I,J/K,L) IS OF THE TYPE (I,J/P,S)
C
                  IF (IJ) 200, 210, 190
C
C     (SS/PS)
C
  190             W(KI)=RI(5)*X(KK)
                  GO TO 260
C
C     (PS/PS)
C
  200             W(KI)=RI(6)*X(II)*X(KK)+RI(7)*(Y(II)*Y(KK)+Z(II)*Z(KK)
     1)
                  GO TO 260
C
C     (PP/PS)
C
  210             W(KI)=X(KK)*(RI(8)*X(II)*X(JJ)+RI(9)*(Y(II)*Y(JJ)+Z(II
     1)*Z(JJ)))   +RI(10)*(X(II)*(Y(JJ)*Y(KK)+Z(JJ)*Z(KK))+X(JJ)*(Y(II)*
     2Y(KK)+Z(I   I)*Z(KK)))
                  GO TO 260
C
C *** INTEGRAL (I,J/K,L) IS OF THE TYPE (I,J/P,P)
C
  220             IF (IJ) 240,250,230
C
C     (SS/PP)
C
  230             W(KI)=RI(11)*X(KK)*X(LL)+RI(12)*(Y(KK)*Y(LL)+Z(KK)*Z(L
     1L))
                  GO TO 260
C
C     (PS/PP)
C
  240             W(KI)=X(II)*(RI(13)*X(KK)*X(LL)+RI(14)*(Y(KK)*Y(LL)+Z(
     1KK)*Z(LL)   ))+RI(15)*(Y(II)*(Y(KK)*X(LL)+Y(LL)*X(KK))+Z(II)*(Z(KK
     2)*X(LL)+Z   (LL)*X(KK)))
                  GO TO 260
C
C     (PP/PP)
C
  250             W(KI)=(RI(16)*X(II)*X(JJ)+RI(17)*(Y(II)*Y(JJ)+Z(II)*Z(
     1JJ)))*X(K   K)*X(LL)+RI(18)*X(II)*X(JJ)*(Y(KK)*Y(LL)+Z(KK)*Z(LL))+
     2RI(19)*(Y   (II)*Y(JJ)*Y(KK)*Y(LL)+Z(II)*Z(JJ)*Z(KK)*Z(LL))+RI(20)
     3*(X(II)*(   X(KK)*(Y(JJ)*Y(LL)+Z(JJ)*Z(LL))+X(LL)*(Y(JJ)*Y(KK)+Z(J
     4J)*Z(KK))   )+X(JJ)*(X(KK)*(Y(II)*Y(LL)+Z(II)*Z(LL))+X(LL)*(Y(II)*
     5Y(KK)+Z(I   I)*Z(KK))))+RI(21)*(Y(II)*Y(JJ)*Z(KK)*Z(LL)+Z(II)*Z(JJ
     6)*Y(KK)*Y   (LL))+RI(22)*(Y(II)*Z(JJ)+Z(II)*Y(JJ))*(Y(KK)*Z(LL)+Z(
     7KK)*Y(LL)   )
  260             CONTINUE
  270 CONTINUE
  280 CONTINUE
      E1B(1)=-CSS1
      IF(NI.GT.3) THEN
         E1B(2) = -CSP1 *X1
         E1B(3) = -CPPS1*X1**2-CPPP1*(Y1**2+Z1**2)
         E1B(4) = -CSP1 *X2
         E1B(5) = -CPPS1*X1*X2-CPPP1*(Y1*Y2+Z1*Z2)
         E1B(6) = -CPPS1*X2*X2-CPPP1*(Y2*Y2+Z2*Z2)
         E1B(7) = -CSP1 *X3
         E1B(8) = -CPPS1*X1*X3-CPPP1*(Y1*Y3+Z1*Z3)
         E1B(9) = -CPPS1*X2*X3-CPPP1*(Y2*Y3+Z2*Z3)
         E1B(10)= -CPPS1*X3*X3-CPPP1*(Y3*Y3+Z3*Z3)
      END IF
      E2A(1)=-CSS2
      IF(NJ.GT.3) THEN
         E2A(2) = -CSP2 *X1
         E2A(3) = -CPPS2*X1**2-CPPP2*(Y1**2+Z1**2)
         E2A(4) = -CSP2 *X2
         E2A(5) = -CPPS2*X1*X2-CPPP2*(Y1*Y2+Z1*Z2)
         E2A(6) = -CPPS2*X2*X2-CPPP2*(Y2*Y2+Z2*Z2)
         E2A(7) = -CSP2 *X3
         E2A(8) = -CPPS2*X1*X3-CPPP2*(Y1*Y3+Z1*Z3)
         E2A(9) = -CPPS2*X2*X3-CPPP2*(Y2*Y3+Z2*Z3)
         E2A(10)= -CPPS2*X3*X3-CPPP2*(Y3*Y3+Z3*Z3)
      END IF
      IF(ABS(TORE(NI)).GT.20.AND.ABS(TORE(NJ)).GT.20) THEN
C SPARKLE-SPARKLE INTERACTION
         ENUC=0.D0
         RETURN
      ELSEIF (RIJ.LT.1.D0.AND.NATORB(NI)*NATORB(NJ).EQ.0) THEN
         ENUC=0.D0
         RETURN
      ENDIF
      SCALE = EXP(-ALP(NI)*RIJ)+EXP(-ALP(NJ)*RIJ)
      NT=NI+NJ
      IF(NT.EQ.8.OR.NT.EQ.9) THEN
         IF(NI.EQ.7.OR.NI.EQ.8) SCALE=SCALE+(RIJ-1.D0)*EXP(-ALP(NI)*RIJ)
         IF(NJ.EQ.7.OR.NJ.EQ.8) SCALE=SCALE+(RIJ-1.D0)*EXP(-ALP(NJ)*RIJ)
      ENDIF
      ENUC = TORE(NI)*TORE(NJ)*GAM
      SCALE=ABS(SCALE*ENUC)
      IF( AM1 )THEN
         DO 290 IG=1,10
            IF(ABS(FN1(NI,IG)).GT.0.D0)
     1SCALE=SCALE +TORE(NI)*TORE(NJ)/RIJ*
     2FN1(NI,IG)*EXP(-FN2(NI,IG)*(RIJ-FN3(NI,IG))**2)
            IF(ABS(FN1(NJ,IG)).GT.0.D0)
     1SCALE=SCALE +TORE(NI)*TORE(NJ)/RIJ*
     2FN1(NJ,IG)*EXP(-FN2(NJ,IG)*(RIJ-FN3(NJ,IG))**2)
  290    CONTINUE
      ENDIF
      ENUC=ENUC+SCALE
C
C *** NOW ROTATE THE NUCLEAR ATTRACTION INTEGRALS.
C *** THE STORAGE OF THE NUCLEAR ATTRACTION INTEGRALS  CORE(KL/IJ) IS
C     (SS/)=1,   (SO/)=2,   (OO/)=3,   (PP/)=4
C
C   DEBUG PRINTING
C
      KR=KR+KI
      RETURN
      END
