
     SUBROUTINE ROTATE (NI,NJ,XI,XJ,W,KR,E1B,E2A,ENUC)
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
     DIMENSION XI(3),XJ(3),W(100),E1B(10),E2A(10)
     COMMON /NATORB/ NATORB(54)
     COMMON /TWOEL3/ F03(18)
     COMMON /ALPHA3/ ALP3(153)
     COMMON /ALPHA / ALP(54)
     COMMON /CORE  / TORE(54)
****************************************************************************

   ROTATE CALCULATES THE TWO-PARTICLE INTERACTIONS.

   ON INPUT  NI     = ATOMIC NUMBER OF FIRST ATOM.
             NJ     = ATOMIC NUMBER OF SECOND ATOM.
             XI     = COORDINATE OF FIRST ATOM.
             XJ     = COORDINATE OF SECOND ATOM.

   ON OUTPUT W      = ARRAY OF TWO-ELECTRON REPULSION INTEGRALS.
             E1B,E2A= ARRAY OF ELECTRON-NUCLEAR ATTRACTION INTEGRALS,
                      E1B = ELECTRON ON ATOM NI ATTRACTING NUCLEUS OF NJ.
             ENUC   = NUCLEAR-NUCLEAR REPULSION TERM.

****************************************************************************
     COMMON /ROTDUM/ CSS1,CSP1,CPPS1,CPPP1,CSS2,CSP2,CPPS2,CPPP2
     COMMON /ROTDU2/ X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3
     COMMON /KEYWRD/ KEYWRD
     CHARACTER*80 KEYWRD
     DIMENSION X(3),Y(3),Z(3),RI(22),CORE(4,2)
     LOGICAL SI,SK
     EQUIVALENCE (CORE(1,1),CSS1),(X(1),X1),(Y(1),Y1),(Z(1),Z1)
     DATA ITYPE /1/
                                                                       
 *** THIS ROUTINE COMPUTES THE REPUSLION AND NUCLEAR ATTRACTION        
     INTEGRALS OVER MOLECULAR-FRAME COORDINATES.  THE INTEGRALS OVER   
     LOCAL FRAME COORDINATES ARE EVALUATED BY SUBROUTINE REPP AND STORED
     AS FOLLOWS (WHERE P-SIGMA = O,   AND P-PI = P AND P* ) IN RI      
     (SS/SS)=1,   (SO/SS)=2,   (OO/SS)=3,   (PP/SS)=4,   (SS/OS)=5,    
     (SO/SO)=6,   (SP/SP)=7,   (OO/SO)=8,   (PP/SO)=9,   (PO/SP)=10,   
     (SS/OO)=11,  (SS/PP)=12,  (SO/OO)=13,  (SO/PP)=14,  (SP/OP)=15,   
     (OO/OO)=16,  (PP/OO)=17,  (OO/PP)=18,  (PP/PP)=19,  (PO/PO)=20,   
     (PP/P*P*)=21,   (P*P/P*P)=22.                                     

     RIJ=0.D0
     DO 15 I=1,3
     X(I)=XI(I)-XJ(I)
 15  RIJ=RIJ+X(I)**2
 14  GOTO (100,200,300) ITYPE
 100 CONTINUE
     IF(INDEX(KEYWRD,'MINDO3') .NE. 0) THEN
         ITYPE=2
     ELSE
         ITYPE=3
     ENDIF
     GOTO 14
 200 CONTINUE
     SUM=14.399D0/SQRT(RIJ+(7.1995D0/F03(NI)+7.1995D0/F03(NJ))**2)
     W(1)=SUM
     KR=KR+1
     L=0
     DO 210 I=1,4
     DO 220 J=1,I
     L=L+1
     E1B(L)=0.D0
 220 E2A(L)=0.D0
     E1B(L)=-SUM*TORE(NJ)
 210 E2A(L)=-SUM*TORE(NI)
     II=MAX(NI,NJ)
     NBOND=(II*(II-1))/2+NI+NJ-II
     RIJ=SQRT(RIJ)
     IF(NBOND.EQ.22 .OR. NBOND .EQ. 29) GO TO 2
     GO TO 1
  2  SCALE=ALP3(NBOND)*EXP(-RIJ)
     GO TO 10
  1  SCALE=EXP(-ALP3(NBOND)*RIJ)
 10  CONTINUE
     ENUC=TORE(NI)*TORE(NJ)*(SUM+(14.399D0/RIJ-SUM)*SCALE)
     RETURN
 300 CONTINUE
     RIJ=SQRT(RIJ)
     CALL REPP(NI,NJ,RIJ,RI,CORE)
     GAM=RI(1)                                                         
                                                                       
 *** THE REPULSION INTEGRALS OVER MOLECULAR FRAME (W) ARE STORED IN THE
     ORDER IN WHICH THEY WILL LATER BE USED.  IE.  (I,J/K,L) WHERE     
     J.LE.I  AND  L.LE.K     AND L VARIES MOST RAPIDLY AND I LEAST     
     RAPIDLY.  (ANTI-NORMAL COMPUTER STORAGE)                          

     A=1.D0/RIJ
     DO 11 I=1,3
 11  X(I)=X(I)*A
     Z(3)=0.D0
     IF(ABS(X(3)).GT.0.999999D0) GOTO 12
     Z(3)=SQRT(1.D0-X(3)**2)
     A=1.D0/Z(3)
     Y(1)=-A*X(2)*SIGN(1.D0,X(1))
     Y(2)=ABS(A*X(1))
     Y(3)=0.D0
     Z(1)=-A*X(1)*X(3)
     Z(2)=-A*X(2)*X(3)
     GOTO 13
 12  Y(1)=0.D0
     Y(2)=1.D0
     Y(3)=0.D0
     Z(1)=1.D0
     Z(2)=0.D0
 13  CONTINUE
     IB=NATORB(NI)
     JB=NATORB(NJ)
     KI=0
     DO 130 I=1,IB
        SI=I.EQ.1                                                     
        II=I-1                                                        
     DO 130 J=1,I                                                     
        JJ=J-1                                                        
        IJ=0                                                           
        IF (JJ.EQ.0) IJ=-1                                             
        IF (SI) IJ=+1                                                  
     DO 130 K=1,JB                                                    
        KK=K-1                                                        
        SK=KK.GT.0                                                     
     DO 130 L=1,K                                                     
        KI=KI+1                                                        
        IF (SK) GO TO 50                                               
                                                                       
 *** INTEGRAL (I,J/K,L) IS OF THE TYPE (I,J/S,S)                       
                                                                       
        IF (IJ) 30,40,20                                               
                                                                       
     (SS/SS)                                                           
                                                                       
  20    W(KI)=RI(1)                                                    
        GO TO 131                                                      
                                                                       
     (PS/SS)                                                           
                                                                       
  30    W(KI)=RI(2)*X(II)                                              
        GO TO 131                                                      
                                                                       
     (PP/SS)                                                           
                                                                       
  40    W(KI)=RI(3)*X(II)*X(JJ)+RI(4)*(Y(II)*Y(JJ)+Z(II)*Z(JJ))        
        GO TO 131                                                      
  50    LL=L-1                                                        
        IF (LL.GT.0) GO TO 90                                          
                                                                       
 *** INTEGRAL (I,J/K,L) IS OF THE TYPE (I,J/P,S)                       
                                                                       
        IF (IJ) 70,80,60                                               
                                                                       
     (SS/PS)                                                           
                                                                       
  60    W(KI)=RI(5)*X(KK)                                              
        GO TO 131                                                      
                                                                       
     (PS/PS)                                                           
                                                                       
  70    W(KI)=RI(6)*X(II)*X(KK)+RI(7)*(Y(II)*Y(KK)+Z(II)*Z(KK))        
        GO TO 131                                                      
                                                                       
     (PP/PS)                                                           
                                                                       
  80    W(KI)=X(KK)*(RI(8)*X(II)*X(JJ)+RI(9)*(Y(II)*Y(JJ)+Z(II)*Z(JJ)))
    1   +RI(10)*(X(II)*(Y(JJ)*Y(KK)+Z(JJ)*Z(KK))+X(JJ)*(Y(II)*Y(KK)+Z(I
    2   I)*Z(KK)))                                                     
        GO TO 131                                                      
                                                                       
 *** INTEGRAL (I,J/K,L) IS OF THE TYPE (I,J/P,P)                       
                                                                       
  90    IF (IJ) 110,120,101                                            
                                                                       
     (SS/PP)                                                           
                                                                       
 101    W(KI)=RI(11)*X(KK)*X(LL)+RI(12)*(Y(KK)*Y(LL)+Z(KK)*Z(LL))      
        GO TO 131                                                      
                                                                       
     (PS/PP)                                                           
                                                                       
 110    W(KI)=X(II)*(RI(13)*X(KK)*X(LL)+RI(14)*(Y(KK)*Y(LL)+Z(KK)*Z(LL)
    1   ))+RI(15)*(Y(II)*(Y(KK)*X(LL)+Y(LL)*X(KK))+Z(II)*(Z(KK)*X(LL)+Z
    2   (LL)*X(KK)))                                                   
        GO TO 131                                                      
                                                                       
     (PP/PP)                                                           
                                                                       
 120    W(KI)=(RI(16)*X(II)*X(JJ)+RI(17)*(Y(II)*Y(JJ)+Z(II)*Z(JJ)))*X(K
    1   K)*X(LL)+RI(18)*X(II)*X(JJ)*(Y(KK)*Y(LL)+Z(KK)*Z(LL))+RI(19)*(Y
    2   (II)*Y(JJ)*Y(KK)*Y(LL)+Z(II)*Z(JJ)*Z(KK)*Z(LL))+RI(20)*(X(II)*(
    3   X(KK)*(Y(JJ)*Y(LL)+Z(JJ)*Z(LL))+X(LL)*(Y(JJ)*Y(KK)+Z(JJ)*Z(KK))
    4   )+X(JJ)*(X(KK)*(Y(II)*Y(LL)+Z(II)*Z(LL))+X(LL)*(Y(II)*Y(KK)+Z(I
    5   I)*Z(KK))))+RI(21)*(Y(II)*Y(JJ)*Z(KK)*Z(LL)+Z(II)*Z(JJ)*Y(KK)*Y
    6   (LL))+RI(22)*(Y(II)*Z(JJ)+Z(II)*Y(JJ))*(Y(KK)*Z(LL)+Z(KK)*Y(LL)
    7   )                                                              
 131 CONTINUE                                                          
 130 CONTINUE                                                          
 150 CONTINUE
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
     SCALE = 1.D0+EXP(-ALP(NI)*RIJ)+EXP(-ALP(NJ)*RIJ)
     NT=NI+NJ
     IF(NT.EQ.8.OR.NT.EQ.9) THEN
     IF(NI.EQ.7.OR.NI.EQ.8) SCALE=SCALE+(RIJ-1.D0)*EXP(-ALP(NI)*RIJ)
     IF(NJ.EQ.7.OR.NJ.EQ.8) SCALE=SCALE+(RIJ-1.D0)*EXP(-ALP(NJ)*RIJ)
     ELSE
     ENDIF
     ENUC = TORE(NI)*TORE(NJ)*GAM*SCALE   
                                                                       
 *** NOW ROTATE THE NUCLEAR ATTRACTION INTEGRALS.                      
 *** THE STORAGE OF THE NUCLEAR ATTRACTION INTEGRALS  CORE(KL/IJ) IS   
     (SS/)=1,   (SO/)=2,   (OO/)=3,   (PP/)=4                          
                                                                       
   DEBUG PRINTING                                                      
                                                                       
     KR=KR+KI
     RETURN                                                            
     END                                                               