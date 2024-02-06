      SUBROUTINE DIAT(NI,NJ,XI,XJ,DI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)         
***************************************************************************
*
*   DIAT CALCULATES THE DI-ATOMIC OVERLAP INTEGRALS BETWEEN ATOMS
*        CENTERED AT XI AND XJ. 
*
*   ON INPUT NI  = ATOMIC NUMBER OF THE FIRST ATOM.
*            NJ  = ATOMIC NUMBER OF THE SECOND ATOM.
*            XI  = CARTESIAN COORDINATES OF THE FIRST ATOM.
*            XJ  = CARTESIAN COORDINATES OF THE SECOND ATOM.
*
*  ON OUTPUT DI  = DIATOMIC OVERLAP, IN A 9 * 9 MATRIX. LAYOUT OF 
*                  ATOMIC ORBITALS IN DI IS
*                  1   2   3   4   5            6     7       8     9
*                  S   PX  PY  PZ  D(X**2-Y**2) D(XZ) D(Z**2) D(YZ) D(XY)
*
*   LIMITATIONS:  IN THIS FORMULATION, NI AND NJ MUST BE LESS THAN 54
*         EXPONENTS ARE ASSUMED TO BE PRESENT IN COMMON BLOCK EXPONT.
*
***************************************************************************
      INTEGER PQ1,A,PQ2,B,PQA,PQB,AA,BB,PVAL1,PVAL2,YETA       
      LOGICAL FIRST
      COMMON /EXPONT/ EMUS(54),EMUP(54),EMUD(54)
      DIMENSION DI(9,9),S(3,3,3),UL1(3),UL2(3),C(3,5,5),NPQ(54)
     +          ,XI(3),XJ(3)
      DATA NPQ/1,1, 2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3, 4,4,4,4,4,4,4,4,
     +4,4,4,4,4,4,4,4,4,4, 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5/
      DATA FIRST /.TRUE./
      X1=XI(1)
      X2=XJ(1)
      Y1=XI(2)
      Y2=XJ(2)
      Z1=XI(3)
      Z2=XJ(3)
      PQ1=NPQ(NI)
      PQA=PQ1
      PQ2=NPQ(NJ)
      PQB=PQ2
      UL1(1)=EMUS(NI)
      UL2(1)=EMUS(NJ)
      UL1(2)=EMUP(NI)
      UL2(2)=EMUP(NJ)
      DO 349 I=1,9  
      DO 350 J=1,9  
      DI(I,J)=0.0D0   
 350  CONTINUE      
 349  DI(I,I)=1.D0  
      CALL COE(X1,Y1,Z1,X2,Y2,Z2,PQ1,PQ2,C,R)     
          IF (PQ1.GT.3) GO TO 351 
          A=PQ1-1       
          GO TO 352     
 351      A=2 
 352      CONTINUE      
          IF (PQ2.GT.3) GO TO 353 
          B=PQ2-1       
          GO TO 354     
 353      B=2 
 354      CONTINUE      
          IA=A+1
          IB=B+1
       IF(PQ1.LT.4.AND.PQ2.LT.4) THEN
          CALL DIAT2(NI,EMUS(NI),EMUP(NI),R,NJ,EMUS(NJ),EMUP(NJ),S)
C      WRITE(6,'(3I4,F12.6)')(((I,J,K,S(I,J,K),I=1,2),J=1,2),K=1,2)
      ELSE
          DO 355 I=1,3  
              DO 355 J=1,3  
                  DO 355 K=1,3  
                  S(I,J,K)=0.0D0  
 355      CONTINUE      
          DO 363 I=1,IA 
              IB=B+1        
              DO 363 J=1,IB 
                  IF (A.LT.B) GO TO 357   
                  NEWK=B        
                  GO TO 358     
 357              NEWK=A        
 358              CONTINUE      
                  NK1=NEWK+1    
                  DO 363 K=1,NK1
                      IF(K.GT.I.OR.K.GT.J) GOTO 363     
                      U1=UL1(I)     
                      U2=UL2(J)     
                      YETA=PQA+PQB+3
                      S(I,J,K)=SS(PQA,PQB,I,J,K,U1,U2,R,YETA,FIRST)     
C      WRITE(6,'(3I4,F12.6)')I,J,K,S(I,J,K)
 363      CONTINUE      
      ENDIF
      DO 369 I=1,IA 
      DO 369 J=1,IB 
      IF (I.EQ.1) GO TO 10    
      IF (I.EQ.2) GO TO 11    
      KMIN=1        
      KMAX=5        
      GO TO 12      
 11   KMIN=2        
      KMAX=4        
      GO TO 12      
 10   KMIN=3        
      KMAX=3        
 12   DO 369 K=KMIN,KMAX      
      IF (J.EQ.1) GO TO 13    
      IF (J.EQ.2) GO TO 14    
      LMIN=1        
      LMAX=5        
      GO TO 15      
 14   LMIN=2        
      LMAX=4        
      GO TO 15      
 13   LMIN=3        
      LMAX=3        
 15   DO 369 L=LMIN,LMAX      
      IF (J.EQ.2) GO TO 364   
      AA=1
      GO TO 365     
 364  AA=-1         
 365  CONTINUE      
      IF (J.GT.2) GO TO 366   
      BB=1
      GO TO 367     
 366  BB=-1         
 367  CONTINUE      
      CALL VAL(I,K,PVAL1)     
      CALL VAL(J,L,PVAL2)     
      DI((PVAL1+1),(PVAL2+1))=S(I,J,1)*C(I,K,3)*C(J,L,3)*AA+(C(I,K,4)*C(        
     1J,L,4)+C(I,K,2)*C(J,L,2))*BB*S(I,J,2)+(C(I,K,5)*C(J,L,5)+C(I,K,1)*        
     2C(J,L,1))*S(I,J,3)      
 369  CONTINUE      
      RETURN        
      END 
      SUBROUTINE VAL(L,M,PVAL)
      INTEGER L,M,PVAL        
      IF (L.EQ.1) PVAL=0      
      IF ((L.EQ.2).AND.(M.EQ.2)) PVAL=2 
      IF ((L.EQ.2).AND.(M.EQ.3)) PVAL=3 
      IF ((L.EQ.2).AND.(M.EQ.4)) PVAL=1 
      IF (L.EQ.3) PVAL=12-L-M 
      RETURN        
      END 
      FUNCTION SS(NA,NB,LA,LB,M,UC,UD,R1,YETA,FIRST)    
      DOUBLE PRECISION UC,SS,UD,R1,R    
      LOGICAL FIRST
      INTEGER A,C,D,PP,B,Q,YETA         
      DOUBLE PRECISION ER,P,BA,EX,QUO,S1,S2,QQ,QEB,SUM,SUM1,X,      
     1FA(14),BI(13,13),AFF(3,3,3),AF(20),BF(20),UA,UB,SA    
      DATA AFF/27*0. D0/
      R=R1
      UA=UC         
      UB=UD         
      IF(UA.GT.0.D0) GOTO 88  
      SA=0.D0       
      GO TO 99      
   88 IF(UB.GT.0.D0) GOTO 299 
      SA=0.D0       
      GO TO 99      
 299  CONTINUE      
      R=R/0.529167D0
      ER=R
      GO TO 304     
  300 FA(1)=1.D0
      DO 301 I=1,13 
      FA(I+1)=FA(I)*I         
 301  CONTINUE      
      FIRST=.FALSE.
      DO 302 I=1,13 
      BI(I,1)=1.D0  
      BI(I,I)=1.D0  
 302  CONTINUE      
      DO 303 I=1,12 
      I1=I-1        
      DO 303 J=1,I1 
      BI(I+1,J+1)=BI(I,J+1)+BI(I,J)     
 303  CONTINUE      
      AFF(1,1,1)=1.D0         
      AFF(2,1,1)=1.D0         
      AFF(2,2,1)=1.D0         
      AFF(3,1,1)=1.5D0        
      AFF(3,2,1)=1.73205D0    
      AFF(3,3,1)=1.224745D0   
      AFF(3,1,3)=-0.5D0       
      GO TO 305     
  304 IF(FIRST)GOTO 300
 305  CONTINUE      
      P=(UA+UB)*ER*0.5D0      
      BA=(UA-UB)*ER*0.5D0     
      EX=EXP(BA)   
      QUO=1/P       
      AF(1)=QUO*EXP(-P)      
      NANB=NA+NB    
      DO 306 N=1,19 
      AF(N+1)=N*QUO*AF(N)+AF(1)         
 306  CONTINUE      
      NANB1=NANB+1  
      DO 309 N=1,13 
      IF(ABS(BA).LT.0.1D0) GOTO 308    
      S1=0.D0       
      S2=0.D0       
      QQ=1.D0       
      DO 307 I=1,N  
      QQ=QQ*BA      
      QEB=1.D0/(QQ*FA(N+1-I)) 
      S1=S1+QEB     
      INI=(N-I+1)/2 
      S2=S2+2.D0*((INI*2-(N-1)+I)-1.5D0)*QEB      
 307  CONTINUE      
      BF(N)=-FA(N)*(S1/EX+S2*EX)        
      GO TO 309     
 308  IN=N/2        
      BF(N)=0.D0
      IF(N.NE.IN*2) BF(N)=2.D0/N        
 309  CONTINUE      
      SUM=0.D0      
      LAM1=LA-M+1   
      LBM1=LB-M+1   
      DO 311 I=1,LAM1,2       
      DO 311 J=1,LBM1,2       
      A=NA+I-LA
      B=NB+J-LB
      C=LA-I-M+1
      D=LB-J-M+1
      SUM1=0.D0     
      IA=A+1        
      IB=B+1        
      IC=C+1        
      ID=D+1        
      AB=A+B-1
      DO 310 K1=1,IA
      DO 310 K2=1,IB
      DO 310 K3=1,IC
      DO 310 K4=1,ID
      DO 310 K5=1,M 
      DO 310 K6=1,M 
      Q=AB-K1-K2+K3+K4+2*K5
      PP=K1+K2+K3+K4+2*K6-5
      JX=M+K2+K4+K5+K6-5
      IX=JX/2       
      SUM1=SUM1+BI(IA,K1)*BI(IB,K2)*BI(IC,K3)*BI(ID,K4)*BI(M,K5)*BI(        
     1M,K6)*2.D0*(IX*2-JX+0.5D0)*AF(Q)*BF(PP) 
 310  CONTINUE      
      SUM=SUM+SUM1*AFF(LA,M,I)*AFF(LB,M,J)        
 311  CONTINUE      
      X=R 
      DO 312 I=1,NA 
      X=X*R*UA      
 312  CONTINUE      
      DO 313 I=1,NB 
      X=X*R*UB      
 313  CONTINUE      
      SA=SUM*X*SQRT(UA*UB/(FA(NA+NA+1)*FA(NB+NB+1))*((LA+LA-1        
     1)*(LB+LB-1)))/(2.D0**M) 
 99   CONTINUE      
      SS=SA         
      RETURN        
      END 
      SUBROUTINE COE(X1,Y1,Z1,X2,Y2,Z2,PQ1,PQ2,C,R)  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      INTEGER PQ1,PQ2,PQ,CO        
      DIMENSION C(3,5,5)  
      XY=(X2-X1)**2+(Y2-Y1)**2     
      R=SQRT(XY+(Z2-Z1)**2)       
      XY=SQRT(XY)        
      IF (XY.EQ.0.D0) GO TO 800     
      CA=(X2-X1)/XY       
      CB=(Z2-Z1)/R        
      SA=(Y2-Y1)/XY       
      SB=XY/R    
      GO TO 804  
 800  IF (Z2-Z1) 801,802,803       
 801  CA=-1.D0    
      CB=-1.D0    
      SA=0.D0     
      SB=0.D0     
      GO TO 804  
  802 CA=0.D0     
      CB=0.D0     
      SA=0.D0     
      SB=0.D0     
      GO TO 804  
  803 CA=1.D0     
      CB=1.D0        
      SA=0.D0        
      SB=0.D0        
 804  CONTINUE      
      CO=0
      DO 805 I=1,5  
      DO 805 J=1,5  
      DO 805 K=1,3  
      C(K,I,J)=0.D0  
 805  CONTINUE      
      IF (PQ1.GT.PQ2) GO TO 806         
      PQ=PQ2        
      GO TO 807     
 806  PQ=PQ1        
 807  CONTINUE      
      C(1,3,3)=1.D0  
      IF (PQ.LT.2) GO TO 808  
      C(2,4,4)=CA*CB
      C(2,4,3)=CA*SB
      C(2,4,2)=-SA  
      C(2,3,4)=-SB  
      C(2,3,3)=CB   
      C(2,3,2)=0.D0  
      C(2,2,4)=SA*CB
      C(2,2,3)=SA*SB
      C(2,2,2)=CA   
      IF (PQ.LT.3) GO TO 808  
      C2A=2*CA*CA-1.D0         
      C2B=2*CB*CB-1.D0         
      S2A=2*SA*CA   
      S2B=2*SB*CB   
      C(3,5,5)=C2A*CB*CB+0.5D0*C2A*SB*SB  
      C(3,5,4)=0.5D0*C2A*S2B    
      C(3,5,3)=0.8660254037841D0*C2A*SB*SB       
      C(3,5,2)=-S2A*SB        
      C(3,5,1)=-S2A*CB        
      C(3,4,5)=-0.5D0*CA*S2B    
      C(3,4,4)=CA*C2B         
      C(3,4,3)=0.8660254037841D0*CA*S2B
      C(3,4,2)=-SA*CB         
      C(3,4,1)=SA*SB
      C(3,3,5)=0.5773502691894D0*SB*SB*1.5D0        
      C(3,3,4)=-0.8660254037841D0*S2B  
      C(3,3,3)=CB*CB-0.5D0*SB*SB
      C(3,2,5)=-0.5D0*SA*S2B    
      C(3,2,4)=SA*C2B         
      C(3,2,3)=0.8660254037841D0*SA*S2B
      C(3,2,2)=CA*CB
      C(3,2,1)=-CA*SB         
      C(3,1,5)=S2A*CB*CB+0.5D0*S2A*SB*SB  
      C(3,1,4)=0.5D0*S2A*S2B    
      C(3,1,3)=0.8660254037841D0*S2A*SB*SB       
      C(3,1,2)=C2A*SB         
      C(3,1,1)=C2A*CB         
 808  CONTINUE      
      RETURN        
      END 
