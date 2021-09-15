
     SUBROUTINE LOCAL(C,MDIM,NOCC,EIG) 
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)              
     INCLUDE 'SIZES/NOLIST'
     DIMENSION C(MDIM,MDIM), EIG(MAXORB)
     COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
    1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
    +                NCLOSE,NOPEN
********************************************************************** 
                                                                       
   LOCALISATION SUBROUTINE                                             
 ON INPUT                                                              
        C = EIGENVECTORS IN AN MDIM*MDIM MATRIX                        
        NOCC = NUMBER OF FILLED LEVELS                                    
        NORBS = NUMBER OF ORBITALS                                         
        NUMAT = NUMBER OF ATOMS                                       
        NLAST   = INTEGER ARRAY OF ATOM ORBITAL COUNTERS                
        NFIRST   = INTEGER ARRAY OF ATOM ORBITAL COUNTERS                
                                                                       
       SUBROUTINE MAXIMISES <PSI>**4                                   
       REFERENCE:
       A New Rapid Method for Orbital Localisation, P.G. Perkins and
       J.J.P. Stewart, J.C.S. Faraday (II) 77, 000, (1981).
                                                                       
********************************************************************** 
     PARAMETER (MDUMY=(NUMATM*(NUMATM+1))/2)
     COMMON /SCRACH/ COLD(MAXORB,MAXORB), XDUMY(MDUMY)
     DIMENSION EIG1(MAXORB),PSI1(MAXORB),PSI2(MAXORB),
    +          CII(MAXORB),XIIM(MAXORB),REFEIG(MAXORB),IEL(10)
     CHARACTER*2 ELEMNT(99)
     LOGICAL LOC(MAXORB),LOCI                                             
     DATA ELEMNT/'H','He',
    2 'Li','Be','B','C','N','O','F','Ne',
    3 'Na','Mg','Al','Si','P','S','Cl','Ar',
    4 'K','Ca','Sc','Ti','V','Cr','Mn','Fe','Co','Ni','Cu',
    4 'Zn','Ga','Ge','As','Se','Br','Kr',
    5 'Rb','Sr','Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
    5 'Cd','In','Sn','Sb','Te','I','Xe',
    6 'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy',
    6 'Ho','Er','Tm','Yb','Lu','Hf','Ta','W','Re','Os','Ir','Pt',
    6 'Au','Hg','Tl','Pb','Bi','Po','At','Rn',
    7 'Fr','Ra','Ac','Th','Pa','U','Np','Pu','Am','Cm','Bk','Cf','XX'/
     NITER=1                                                           
     CONV=3.0                                                          
     A1=0.0                                                            
     A2=0.0                                                            
     DEL=1.20                                                          
     RANDOM=0.4321D0                                                   
     DO 50 I=1,NORBS                                                       
     DO 50 J=1,NOCC                                                       
 50  COLD(I,J)=C(I,J)                                                  
     DO 62 I=1,NOCC                                                       
     LOC(I)=.FALSE.                                                    
 62  REFEIG(I)=EIG(I)                                                  
 12  CONTINUE                                                          
     SUMOLD=0.D0                                                       
 38  ITER=1                                                            
 41  CONTINUE                                                          
     SUM=0.D0                                                          
     ITER=ITER+1                                                       
      WRITE(6,131) ITER,(LOC(I),I=1,NOCC)                                 
 131 FORMAT(I4,20L3)                                                   
     DO 10 I=1,NOCC                                                       
     LOCI=.TRUE.                                                       
     IF(LOC(I)) GOTO 10                                                
     DO 123 J=1,NOCC                                                      
     IF(J.EQ.I) GOTO 123                                               
     XIJJJ=0.D0                                                        
     XJIII=RANDOM                                                      
     DO 11 K=1,NORBS                                                       
     PSI1(K)=C(K,I)                                                    
  11 PSI2(K)=C(K,J)                                                    
 NOW FOLLOWS THE RATE-DETERMINING STEP FOR THE CALCULATION             
     DO 16 K1=1,NUMAT                                                 
     KL=NFIRST(K1)                                                       
     KU=NLAST(K1)                                                       
     DIJ=0.D0                                                          
     DII=0.D0                                                          
     DJJ=0.D0                                                          
     DO 15 K=KL,KU                                                     
     DIJ=DIJ+PSI1(K)*PSI2(K)                                           
     DII=DII+PSI1(K)*PSI1(K)                                           
     DJJ=DJJ+PSI2(K)*PSI2(K)                                           
     X=1.D0                                                            
 15  CONTINUE                                                          
     CII(K1)=DII                                                       
     XIJJJ=XIJJJ+DIJ*DJJ                                               
     XJIII=XJIII+DIJ*DII                                               
 16  CONTINUE                                                          
     SA=CONV*(XJIII-XIJJJ)                                             
     IF(LOCI) LOCI=(ABS(SA).LT.0.001D0)                                
     IF(ABS(SA).GT.0.6D0)SA=0.6D0                                     
     SUM=SUM+ABS(SA)                                                  
     CA=SQRT(1.D0-SA*SA)                                              
 66  FORMAT(I4,10F10.5)                                                
     DO 14 K=1,NORBS                                                       
     C(K,I)=CA*PSI1(K)+SA*PSI2(K)                                      
  14 C(K,J)=-SA*PSI1(K)+CA*PSI2(K)                                     
 77  FORMAT(2I4,2F10.5)                                                
 123 CONTINUE                                                          
     LOC(I)=LOCI                                                       
     XIIII=0.D0                                                        
     DO 39 J=1,NUMAT                                                  
 39  XIIII=XIIII+CII(J)**2                                             
     XIIM(I)=XIIII                                                     
 10  CONTINUE                                                          
 17  FORMAT(20I4)                                                      
     RANDOM=0.D0                                                       
 1       FORMAT(F16.10,9F12.3)                                         
     IPR=0                                                             
     SUM1=0.0                                                          
     DO 108 I=1,NOCC                                                      
     DO 108 J=1,NUMAT                                                 
     IL=NFIRST(J)                                                        
     IU=NLAST(J)                                                        
     X=0.0                                                             
     DO 102 K=IL,IU                                                    
 102 X=X+C(K,I)**2                                                     
 108 SUM1=SUM1+X*X                                                     
     A3=SUM1                                                           
     IF(A3-A2.LT.A2-A1)DEL=1.0/DEL                                     
     IF(A3.LT.A2)CONV=CONV*0.5                                         
     IF(ITER.GT.80)CONV=0.8                                            
     A1=A2                                                             
     A2=A3                                                             
     CONV=CONV*DEL                                                     
      SUMOLD=SUM1                                                      
     IF(SUM.GT.NOCC*1.D-6.AND.ITER.LT.100)GOTO 41                         
      WRITE(6,67)ITER,CONV,SUM1                                        
 67  FORMAT(/10X,'NUMBER OF ITERATIONS =',I4                           
    *,'  CONVERGANCE PARAMETER =',                                     
    +F10.6,/10X,'LOCALISATION VALUE =',F14.9,/)                         
     WRITE(6,68)                                                       
 68  FORMAT(3X,'NUMBER OF CENTERS',20X,'% COMPOSITION OF ORBITALS'//)  
     DO 51 I=1,NOCC                                                       
     SUM=0.D0                                                          
     DO 58 J=1,NOCC                                                       
     CO=0.D0                                                           
     DO 52 K=1,NORBS                                                       
 52  CO=CO+COLD(K,J)*C(K,I)                                            
 58  SUM=SUM+CO*CO*EIG(J)                                              
 51  EIG1(I)=SUM                                                       
     DO 53 I=1,NOCC                                                       
     X=100.D0                                                          
     DO 56 J=I,NOCC                                                       
     IF (X.LT.EIG1(J))  GOTO  56                                       
     X=EIG1(J)                                                         
     I1=J                                                              
 56  CONTINUE                                                          
     EIG(I)=EIG1(I1)                                                   
     X=EIG1(I1)                                                        
     EIG1(I1)=EIG1(I)                                                  
     EIG1(I)=X                                                         
     DO 54 J=1,NORBS                                                       
     X=C(J,I1)                                                         
     C(J,I1)=C(J,I)                                                    
 54  C(J,I)=X                                                          
 53  CONTINUE                                                          
     DO 42 I=1,NOCC                                                       
     X=0.D0                                                            
     DO 43 K1=1,NUMAT                                                 
     KL=NFIRST(K1)                                                       
     KU=NLAST(K1)                                                       
     DII=0.D0                                                          
     DO 44 K=KL,KU                                                     
 44  DII=DII+C(K,I)**2                                                 
     X=X+DII*DII                                                       
 43  PSI1(K1)=DII*100.D0                                               
     X=1/X                                                             
     DO 70 II=1,NUMAT                                                 
     SUM=0.D0                                                          
     DO 71 J=1,NUMAT                                                  
     IF(PSI1(J).LT.SUM) GOTO 71                                        
     SUM=PSI1(J)                                                       
     K=J                                                               
 71  CONTINUE                                                          
     PSI1(K)=0.D0                                                      
     CII(II)=SUM                                                       
     IEL(II)=K                                                         
     IF(SUM.LT.1.D0) GOTO 72                                           
 70  CONTINUE                                                          
 72  CONTINUE                                                          
     II=II-1                                                           
     WRITE(6,45)X,(ELEMNT(NAT(IEL(K))),IEL(K),CII(K),K=1,II)            
 45  FORMAT(F10.4,2(5(3X,A2,I3,F6.2),/10X))                            
 42  CONTINUE                                                          
 100 FORMAT(//20X,20H LOCALISED ORBITALS   ,//)                        
     WRITE(6,100)                                                      
     CALL MATOUT(C,EIG,NOCC,NORBS,MDIM)
   3 FORMAT(10F12.6)                                                   
     DO 63 I=1,NOCC                                                       
     EIG(I)=REFEIG(I)                                                  
     DO 63 J=1,NORBS                                                       
 63  C(J,I)=COLD(J,I)                                                  
     RETURN                                                            
     END                                                               