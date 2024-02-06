      SUBROUTINE DHC (P,PA,PB,XI,NAT,IF,IM,IL,JF,JM,JL, 
     1NORBS,DENER,CUC) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      INCLUDE 'KEYS.i'                                                  DJG0995
      DIMENSION P(*), PA(*), PB(*) 
      DIMENSION XI(3,*),NFIRST(2),NMIDLE(2),NLAST(2),NAT(*) 
C*********************************************************************** 
C 
C  DHC CALCULATES THE ENERGY CONTRIBUTIONS FROM THOSE PAIRS OF ATOMS 
C         THAT HAVE BEEN MOVED BY SUBROUTINE DERIV. 
C 
C*********************************************************************** 
      COMMON /ONELEC/ USS(107),UPP(107),UDD(107)                        DJG0995
      COMMON /EULER / TVEC(3,3), ID 
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      LOGICAL UHF, CUC 
      DIMENSION H(171), SHMAT(9,9), F(171), 
     1          WJ(100), E1B(10), E2A(10), WK(100), W(100) 
       SAVE
      IF (ICONTR(34).EQ.1) THEN                                          GDH0195
         ICONTR(34)=2                                                    GDH0195
         WLIM=1.D0 
         IF(ID.EQ.0)WLIM=0.D0 
         UHF=(IUHF.NE.0)                                                DJG0995
      ENDIF 
      NFIRST(1)=1 
      NMIDLE(1)=IM-IF+1 
      NLAST(1)=IL-IF+1 
      NFIRST(2)=NLAST(1)+1 
      NMIDLE(2)=NFIRST(2)+JM-JF 
      NLAST(2)=NFIRST(2)+JL-JF 
      LINEAR=(NLAST(2)*(NLAST(2)+1))/2 
      DO 10 I=1,LINEAR 
         F(I)=0.D0 
   10 H(I)=0.0D00 
      DO 20 I=1,2 
         NI=NAT(I) 
         J=NFIRST(I) 
         H((J*(J+1))/2)=USS(NI) 
         H(((J+1)*(J+2))/2)=UPP(NI) 
         H(((J+2)*(J+3))/2)=UPP(NI) 
         H(((J+3)*(J+4))/2)=UPP(NI) 
         H(((J+4)*(J+5))/2)=UDD(NI) 
         H(((J+5)*(J+6))/2)=UDD(NI) 
         H(((J+6)*(J+7))/2)=UDD(NI) 
         H(((J+7)*(J+8))/2)=UDD(NI) 
         H(((J+8)*(J+9))/2)=UDD(NI) 
         H(((J+9)*(J+10))/2)=UDD(NI) 
   20 CONTINUE 
      DO 30 I=1,LINEAR 
   30 F(I)=H(I) 
      JA=NFIRST(2) 
      JB=NLAST(2) 
      JC=NMIDLE(2) 
      IA=NFIRST(1) 
      IB=NLAST(1) 
      IC=NMIDLE(1) 
      JT=JB*(JB+1)/2 
      J=2 
      I=1 
      NJ=NAT(2) 
      NI=NAT(1) 
      CALL H1ELEC(NI,NJ,XI(1,1),XI(1,2),SHMAT) 
      J1=0 
      DO 40 J=JA,JB 
         JJ=J*(J-1)/2 
         J1=J1+1 
         I1=0 
         DO 40 I=IA,IB 
            JJ=JJ+1 
            I1=I1+1 
            H(JJ)=SHMAT(I1,J1) 
   40 F(JJ)=SHMAT(I1,J1) 
      KR=1 
      IF(ID.EQ.0)THEN 
         CALL ROTATE (NJ,NI,XI(1,2),XI(1,1),W(KR),KR,E2A,E1B,ENUCLR,100. 
     1D0) 
      ELSE 
         CALL SOLROT (NJ,NI,XI(1,2),XI(1,1),WJ,WK,KR,E2A,E1B,ENUCLR,100. 
     1D0) 
         IF(WJ(1).LT.WLIM)THEN 
            DO 50 I=1,KR-1 
   50       WK(I)=0.D0 
         ENDIF 
      ENDIF 
C 
C    * ENUCLR IS SUMMED OVER CORE-CORE REPULSION INTEGRALS. 
C 
      I2=0 
      DO 60 I1=IA,IC 
         II=I1*(I1-1)/2+IA-1 
         DO 60 J1=IA,I1 
            II=II+1 
            I2=I2+1 
            H(II)=H(II)+E1B(I2) 
   60 F(II)=F(II)+E1B(I2) 
      DO  70 I1=IC+1,IB 
         II=(I1*(I1+1))/2 
         F(II)=F(II)+E1B(1) 
   70 H(II)=H(II)+E1B(1) 
      I2=0 
      DO 80 I1=JA,JC 
         II=I1*(I1-1)/2+JA-1 
         DO 80 J1=JA,I1 
            II=II+1 
            I2=I2+1 
            H(II)=H(II)+E2A(I2) 
   80 F(II)=F(II)+E2A(I2) 
      DO 90 I1=JC+1,JB 
         II=(I1*(I1+1))/2 
         F(II)=F(II)+E2A(1) 
   90 H(II)=H(II)+E2A(1) 
      CALL FOCK2D(F,P,PA,W, WJ, WK,2,NFIRST,NMIDLE,NLAST) 
      EE=HELECT(NLAST(2),PA,H,F) 
      IF( UHF ) THEN 
         DO 100 I=1,LINEAR 
  100    F(I)=H(I) 
         CALL FOCK2D(F,P,PB,W, WJ, WK,2,NFIRST,NMIDLE,NLAST) 
         EE=EE+HELECT(NLAST(2),PB,H,F) 
      ELSE 
         EE=EE*2.D0 
      ENDIF 
      DENER=EE+ENUCLR 
      RETURN 
C 
      END 
