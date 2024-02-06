      SUBROUTINE FOCK1(F, PTOT, PA, PB) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
C ********************************************************************* 
C 
C *** COMPUTE THE REMAINING CONTRIBUTIONS TO THE ONE-CENTRE ELEMENTS. 
C 
C ********************************************************************* 
      DIMENSION F(*), PTOT(*), PA(*), PB(*) 
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /GAUSS / FN1(107),FN2(107) 
      COMMON /CORE  / CORE(107)                                         GDH1293
      COMMON /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107) 
     1                ,GSD(107),GPD(107),GDD(107) 
      COMMON /CMCOM/  ECMCG(NUMATM)                                     GDH1293
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      DIMENSION QTOT(NUMATM) 
       SAVE
      CALL CHRGE (PTOT,QTOT)
      KDIAG=0 
      DO 100 II=1,NUMAT 
         IA=NFIRST(II) 
         IB=NMIDLE(II) 
         IC=NLAST(II) 
         NI=NAT(II) 
         DTPOP=0.D0 
         DAPOP=0.D0 
         PTPOP=0.D0 
         PAPOP=0.D0 
         KDIAG=KDIAG+IA 
         KA=KDIAG 
         IF (IB.GT.IA) THEN 
            IAPLUS=IA+1 
            DO 20 I=IAPLUS,IB 
            KDIAG=KDIAG+I 
            PTPOP=PTPOP+PTOT(KDIAG) 
   20       PAPOP=PAPOP+PA  (KDIAG) 
            IF (IC.GT.IB) THEN 
               IBPLUS=IB+1 
               DO 30 I=IBPLUS,IC 
               KDIAG=KDIAG+I 
               DTPOP=DTPOP+PTOT(KDIAG) 
   30          DAPOP=DAPOP+PA  (KDIAG) 
            ENDIF 
         ENDIF 
C 
C     F(S,S) 
C 
         F(KA)=F(KA)+PB(KA)*GSS(NI)+PTPOP*GSP(NI) 
     1         -PAPOP*HSP(NI) + DTPOP*GSD(NI) 
         IF (NI.LT.3) GO TO 100 
         L=KA 
         DO 70 J=IAPLUS,IB 
            M=L+IA 
            L=L+J 
C 
C     F(P,P) 
C 
            F(L)=F(L)+PTOT(KA)*GSP(NI)-PA(KA)*HSP(NI)+ 
     1      PB(L)*GPP(NI)+(PTPOP-PTOT(L))*GP2(NI) 
     2      -0.5D0*(PAPOP-PA(L))*(GPP(NI)-GP2(NI)) 
     3      +DTPOP*GPD(NI) 
C 
C     F(S,P) 
C 
   70    F(M)=F(M)+2.D0*PTOT(M)*HSP(NI)-PA(M)*(HSP(NI)+GSP(NI)) 
C 
C     F(P,P*) 
C 
         IMINUS=IB-1 
         DO 80 J=IAPLUS,IMINUS 
            ICC=J+1 
            M=J*(J+3)/2 
            DO 75 L=ICC,IB 
            F(M)=F(M)+    PTOT(M)*(GPP(NI)-GP2(NI)) 
     1               -0.5D0*PA(M)*(GPP(NI)+GP2(NI)) 
   75       M=M+L 
   80    CONTINUE 
         M=M+1 
         DO 90 J=IBPLUS,IC 
         M=M+J 
   90    F(M)=F(M)+PTOT(KA)*GSD(NI)+PTPOP*GPD(NI)+(DTPOP-PA(M))*GDD(NI) 
  100 CONTINUE 
      RETURN 
      END 
