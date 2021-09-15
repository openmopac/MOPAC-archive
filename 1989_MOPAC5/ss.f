      DOUBLE PRECISION FUNCTION SS(NA,NB,LA1,LB1,M1,UA,UB,R1,FIRST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL FIRST
      DIMENSION FA(0:13),AFF(0:2,0:2,0:2),AF(0:19),BF(0:19),
     1BI(0:12,0:12)
      DATA AFF/27*0. D0/
      DATA FA/1.D0,1.D0,2.D0,6.D0,24.D0,120.D0,720.D0,5040.D0,40320.D0,
     1362880.D0,3628800.D0,39916800.D0,479001600.D0,6227020800.D0/
      M=M1-1
      LB=LB1-1
      LA=LA1-1
      R=R1/0.529167D0
      IF(FIRST) THEN
         FIRST=.FALSE.
C
C           INITIALISE SOME CONSTANTS
C
C                  BINOMIALS
C
         DO 10 I=0,12
            BI(I,0)=1.D0
            BI(I,I)=1.D0
   10    CONTINUE
         DO 20 I=0,11
            I1=I-1
            DO 20 J=0,I1
               BI(I+1,J+1)=BI(I,J+1)+BI(I,J)
   20    CONTINUE
         AFF(0,0,0)=1.D0
         AFF(1,0,0)=1.D0
         AFF(1,1,0)=1.D0
         AFF(2,0,0)=1.5D0
         AFF(2,1,0)=1.73205D0
         AFF(2,2,0)=1.224745D0
         AFF(2,0,2)=-0.5D0
      ENDIF
      P=(UA+UB)*R*0.5D0
      B=(UA-UB)*R*0.5D0
      EX=EXP(B)
      QUO=1/P
      AF(0)=QUO*EXP(-P)
      DO 30 N=1,19
         AF(N)=N*QUO*AF(N-1)+AF(0)
   30 CONTINUE
      CALL BFN(B,BF)
      SUM=0.D0
      LAM1=LA-M
      LBM1=LB-M
C
C          START OF OVERLAP CALCULATION PROPER
C
      DO 50 I=0,LAM1,2
         IA=NA+I-LA
         IC=LA-I-M
         DO 50 J=0,LBM1,2
            IB=NB+J-LB
            ID=LB-J-M
            SUM1=0.D0
            IAB=IA+IB
            DO 40 K1=0,IA
               DO 40 K2=0,IB
                  DO 40 K3=0,IC
                     DO 40 K4=0,ID
                        DO 40 K5=0,M
                           IAF=IAB-K1-K2+K3+K4+2*K5
                           DO 40 K6=0,M
                              IBF=K1+K2+K3+K4+2*K6
                              JX=(-1)**(M+K2+K4+K5+K6)
                              SUM1=SUM1+BI(ID,K4)*
     1BI(M,K5)*BI(IC,K3)*BI(IB,K2)*BI(IA,K1)*
     2BI(M,K6)*JX*AF(IAF)*BF(IBF)
   40       CONTINUE
            SUM=SUM+SUM1*AFF(LA,M,I)*AFF(LB,M,J)
   50 CONTINUE
      SS=SUM*R**(NA+NB+1)*UA**NA*UB**NB/(2.D0**(M+1))*
     1SQRT(UA*UB/(FA(NA+NA)*FA(NB+NB))*((LA+LA+1)*(LB+LB+1)))
      RETURN
      END
