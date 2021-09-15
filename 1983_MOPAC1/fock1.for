
     SUBROUTINE FOCK1(F, PTOT, PA, PB)
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
     INCLUDE 'SIZES/NOLIST'
     DIMENSION F(*), PTOT(*), PA(*), PB(*)
 *********************************************************************

 *** COMPUTE THE REMAINING CONTRIBUTIONS TO THE ONE-CENTRE ELEMENTS.   
                                                                       
 *********************************************************************
     COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
    +                NLAST(NUMATM), NORBS, NELECS,
    1                NALPHA, NBETA, NCLOSE, NOPEN
     COMMON /TWOELE/ GSS(54),GSP(54),GPP(54),GP2(54),HSP(54)
     DIMENSION QTOT(NUMATM), QA(NUMATM), QB(NUMATM)
      CALL CHRGE(PTOT,QTOT)
      CALL CHRGE(PA,QA)
      DO 10 I=1,NUMAT
 10   QB(I)=QTOT(I)-QA(I)
      DO 100 II=1,NUMAT
        IA=NFIRST(II)                                                  
        IB=NLAST(II)                                                   
        NI=NAT(II)                                                     
                                                                       
     F(S,S)                                                            
                                                                       
        KA=(IA*(IA+1))/2                                               
        F(KA)=F(KA)+PB(KA)*GSS(NI)+(QTOT(II)-PTOT(KA))*GSP(NI)
    +         -(QA(II)-PA(KA))*HSP(NI)
        IF (NI.LT.3) GO TO 100                                         
        IPLUS=IA+1                                                     
        L=KA                                                           
        DO 80 J=IPLUS,IB                                               
           M=L+IA                                                      
           L=L+J                                                       
                                                                       
     F(P,P)                                                            
                                                                       
           F(L)=F(L)+PTOT(KA)*GSP(NI)-PA(KA)*HSP(NI)+ 
    1      PB(L)*GPP(NI)+(QTOT(II)-PTOT(L)-PTOT(KA))*GP2(NI)
    2      -0.5D0*(QA(II)-PA(L)-PA(KA))*(GPP(NI)-GP2(NI))
                                                                       
     F(S,P)                                                            
                                                                       
  80    F(M)=F(M)+2.D0*PTOT(M)*HSP(NI)-PA(M)*(HSP(NI)+GSP(NI))
                                                                       
     F(P,P*)                                                           
                                                                       
        IMINUS=IB-1                                                    
        DO 90 J=IPLUS,IMINUS                                           
           IC=J+1                                                      
        DO 90 L=IC,IB                                                  
           M=(L*(L-1))/2+J                                             
 90     F(M)=F(M)+PTOT(M)*(GPP(NI)-GP2(NI))
    +      -0.5D0*PA  (M)*(GPP(NI)+GP2(NI))                 
 100 CONTINUE                                                          

     RETURN
     END