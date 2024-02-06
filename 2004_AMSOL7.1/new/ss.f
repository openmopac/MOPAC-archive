      DOUBLE PRECISION FUNCTION SS(NA,NB,LA,LB,M,UC,UD,R1,YETA) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      COMMON /ONESCM/ ICONTR(100)                                       DJG0295
      INTEGER A,PP,B,Q,YETA 
      DIMENSION FA(14),BI(13,13),AFF(3,3,3),AF(20),BF(20) 
       SAVE
      DATA AFF/27*0. D0/ 
      DATA FA/1.D0,1.D0,2.D0,6.D0,24.D0,120.D0,720.D0,5040.D0,40320.D0, 
     1362880.D0,3628800.D0,39916800.D0,479001600.D0,6227020800.D0/ 
      R=R1 
      UA=UC 
      UB=UD 
      R=R/0.529167D0 
      ER=R 
      IF (ICONTR(26).EQ.1) THEN                                         DJG0295
         ICONTR(26)=2                                                   DJG0295
         DO 10 I=1,13 
            BI(I,1)=1.D0 
            BI(I,I)=1.D0 
   10    CONTINUE 
         DO 20 I=1,12 
            I1=I-1 
            DO 20 J=1,I1 
               BI(I+1,J+1)=BI(I,J+1)+BI(I,J) 
   20    CONTINUE 
         AFF(1,1,1)=1.D0 
         AFF(2,1,1)=1.D0 
         AFF(2,2,1)=1.D0 
         AFF(3,1,1)=1.5D0 
         AFF(3,2,1)=1.73205D0 
         AFF(3,3,1)=1.224745D0 
         AFF(3,1,3)=-0.5D0 
      ENDIF 
      P=(UA+UB)*ER*0.5D0 
      BA=(UA-UB)*ER*0.5D0 
      EX=EXP(BA) 
      QUO=1/P 
      AF(1)=QUO*EXP(-P) 
      NANB=NA+NB 
      DO 30 N=1,19 
         AF(N+1)=N*QUO*AF(N)+AF(1) 
   30 CONTINUE 
      NANB1=NANB+1 
      CALL BFN(BA,BF) 
      SUM=0.D0 
      LAM1=LA-M+1 
      LBM1=LB-M+1 
      DO 50 I=1,LAM1,2 
         A=NA+I-LA 
         IC=LA+2-I-M 
         DO 50 J=1,LBM1,2 
            B=NB+J-LB 
            ID=LB-J-M+2 
            SUM1=0.D0 
            IA=A+1 
            IB=B+1 
            AB=A+B-1 
            DO 40 K1=1,IA 
               PART1=BI(IA,K1) 
               DO 40 K2=1,IB 
                  PART2=PART1*BI(IB,K2) 
                  DO 40 K3=1,IC 
                     PART3=PART2*BI(IC,K3) 
                     DO 40 K4=1,ID 
                        PART4=PART3*BI(ID,K4) 
                        DO 40 K5=1,M 
                           PART5=PART4*BI(M,K5) 
                           Q=AB-K1-K2+K3+K4+2*K5 
                           DO 40 K6=1,M 
                              PART6=PART5*BI(M,K6) 
                              PP=K1+K2+K3+K4+2*K6-5 
                              JX=M+K2+K4+K5+K6-5 
                              IX=JX/2 
                              SUM1=SUM1+PART6*(IX*2-JX+0.5D0)*AF(Q)*BF(P 
     1P) 
   40       CONTINUE 
            SUM=SUM+SUM1*AFF(LA,M,I)*AFF(LB,M,J)*2.D0 
   50 CONTINUE 
      X=R**(NA+NB+1)*UA**NA*UB**NB 
      SA=SUM*X*SQRT(UA*UB/(FA(NA+NA+1)*FA(NB+NB+1))*((LA+LA-1 
     1)*(LB+LB-1)))/(2.D0**M) 
   60 CONTINUE 
      SS=SA 
      RETURN 
      END 












