
     SUBROUTINE REPP(NI,NJ,RIJ,RI,CORE)
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
     DIMENSION RI(22),CORE(4,2)
     COMMON /MULTIP/ DD(54),QQ(54),ADD(54,3)
     COMMON /CORE/ TORE(54)
***********************************************************************
                                                                       
  REPP CALCULATES THE TWO-ELECTRON REPULSION INTEGRALS AND THE         
       NUCLEAR ATTRACTION INTEGRALS.                                   
                                                                       
     ON INPUT RIJ     = INTERATOMIC DISTANCE                           
              NI      = ATOM NUMBER OF FIRST ATOM                      
              NJ      = ATOM NUMBER OF SECOND ATOM                     
    (REF)     ADD     = ARRAY OF GAMMA, OR TWO-ELECTRON ONE-CENTER,    
                        INTEGRALS.                                     
    (REF)     TORE    = ARRAY OF NUCLEAR CHARGES OF THE ELEMENTS       
    (REF)     DD      = ARRAY OF DIPOLE CHARGE SEPARATIONS             
    (REF)     QQ      = ARRAY OF QUADRUPOLE CHARGE SEPARATIONS         

     THE COMMON BLOCKS ARE INITIALISED IN BLOCK-DATA, AND NEVER CHANGED.
                                                                       
    ON OUTPUT RI      = ARRAY OF TWO-ELECTRON REPULSION INTEGRALS      
              CORE    = 4 X 2 ARRAY OF ELECTRON-CORE ATTRACTION        
                        INTEGRALS                                      
                                                                       
***********************************************************************
     R=RIJ/0.529167D00                                                 
     PP=2.0D00                                                         
     P2=4.0D00                                                         
     P3=8.0D00                                                         
     P4=16.0D00                                                        
                                                                       
 *** THIS ROUTINE COMPUTES THE TWO-CENTRE REPULSION INTEGRALS AND THE  
 *** NUCLEAR ATTRACTION INTEGRALS.                                     
 *** THE TWO-CENTRE REPULSION INTEGRALS (OVER LOCAL COORDINATES) ARE   
 *** STORED AS FOLLOWS (WHERE P-SIGMA = O,  AND P-PI = P AND P* )      
     (SS/SS)=1,   (SO/SS)=2,   (OO/SS)=3,   (PP/SS)=4,   (SS/OS)=5,    
     (SO/SO)=6,   (SP/SP)=7,   (OO/SO)=8,   (PP/SO)=9,   (PO/SP)=10,   
     (SS/OO)=11,  (SS/PP)=12,  (SO/OO)=13,  (SO/PP)=14,  (SP/OP)=15,   
     (OO/OO)=16,  (PP/OO)=17,  (OO/PP)=18,  (PP/PP)=19,  (PO/PO)=20,   
     (PP/P*P*)=21,   (P*P/P*P)=22.                                     
 *** THE STORAGE OF THE NUCLEAR ATTRACTION INTEGRALS  CORE(KL/IJ) IS   
     (SS/)=1,   (SO/)=2,   (OO/)=3,   (PP/)=4                          
     WHERE IJ=1 IF THE ORBITALS CENTRED ON ATOM I,  =2 IF ON ATOM J.   
 *** NI AND NJ ARE THE ATOMIC NUMBERS OF THE TWO ELEMENTS.             
                                                                       
     DO 10 I=1,22                                                      
  10 RI(I)=0.0D00                                                       
     DO 20 I=1,4                                                       
     CORE(I,1)=0.0D00                                                  
  20 CORE(I,2)=0.0D00                                                  
                                                                       
     ATOMIC UNITS ARE USED IN THE CALCULATION                          
     DEFINE CHARGE SEPARATIONS.                                        
                                                                       
     DA=DD(NI)                                                         
     DB=DD(NJ)                                                         
     QA=QQ(NI)                                                         
     QB=QQ(NJ)                                                         
     TD = 2.D00                                                        
     OD = 1.D00                                                        
     FD = 4.D00                                                        
                                                                       
     HYDROGEN - HYDROGEN                                               
                                                                       
     AEE=0.25D00*(OD/ADD(NI,1)+OD/ADD(NJ,1))**2                        
     EE=OD/SQRT(R**2+AEE)                                              
     RI(1)=EE*27.21D00                                                  
     CORE(1,1)=TORE(NJ)*RI(1)                                           
     CORE(1,2)=TORE(NI)*RI(1)                                           
     IF (NI.LT.3.AND.NJ.LT.3) RETURN                                   
     IF (NI.LT.3) GO TO 30                                             
                                                                       
     HEAVY ATOM - HYDROGEN                                             
                                                                       
     ADE=0.25D00*(OD/ADD(NI,2)+OD/ADD(NJ,1))**2                        
     AQE=0.25D00*(OD/ADD(NI,3)+OD/ADD(NJ,1))**2                        
     DZE=-OD/SQRT((R+DA)**2+ADE)+OD/SQRT((R-DA)**2+ADE)                
     QZZE=OD/SQRT((R-TD*QA)**2+AQE)-TD/SQRT(R**2+AQE)+OD/              
    1SQRT((R+TD*QA)**2+AQE)                                            
     QXXE=TD/SQRT(R**2+FD*QA**2+AQE)-TD/SQRT(R**2+AQE)                 
     DZE=DZE/PP                                                        
     QXXE=QXXE/P2                                                      
     QZZE=QZZE/P2                                                      
     RI(2)=-DZE                                                         
     RI(3)=EE+QZZE                                                      
     RI(4)=EE+QXXE                                                      
     IF (NJ.LT.3) GO TO 40                                             
                                                                       
     HYDROGEN - HEAVY ATOM                                             
                                                                       
  30 CONTINUE                                                          
     AED=0.25D00*(OD/ADD(NI,1)+OD/ADD(NJ,2))**2                        
     AEQ=0.25D00*(OD/ADD(NI,1)+OD/ADD(NJ,3))**2                        
     EDZ=-OD/SQRT((R-DB)**2+AED)+OD/SQRT((R+DB)**2+AED)                
     EQZZ=OD/SQRT((R-TD*QB)**2+AEQ)-TD/SQRT(R**2+AEQ)+OD/              
    1SQRT((R+TD*QB)**2+AEQ)                                            
     EQXX=TD/SQRT(R**2+FD*QB**2+AEQ)-TD/SQRT(R**2+AEQ)                 
     EDZ=EDZ/PP                                                        
     EQXX=EQXX/P2                                                      
     EQZZ=EQZZ/P2                                                      
     RI(5)=-EDZ                                                         
     RI(11)=EE+EQZZ                                                     
     RI(12)=EE+EQXX                                                     
     IF (NI.LT.3) GO TO 40                                             
                                                                       
     HEAVY ATOM - HEAVY ATOM                                           
     CAUTION. ADD REPLACES ADD(1,1) IN /MULTIP/ AND MUST BE RESET.     
                                                                       
     ADD(1,1)=0.25D00*(OD/ADD(NI,2)+OD/ADD(NJ,2))**2                   
     ADQ=0.25D00*(OD/ADD(NI,2)+OD/ADD(NJ,3))**2                        
     AQD=0.25D00*(OD/ADD(NI,3)+OD/ADD(NJ,2))**2                        
     AQQ=0.25D00*(OD/ADD(NI,3)+OD/ADD(NJ,3))**2                        
     DXDX=TD/SQRT(R**2+(DA-DB)**2+ADD(1,1))                            
    1-TD/SQRT(R**2+(DA+DB)**2+ADD(1,1))                                
     DZDZ=OD/SQRT((R+DA-DB)**2+ADD(1,1))                               
    1+OD/SQRT((R-DA+DB)**2+ADD(1,1))-OD/SQRT((                         
    1R-DA-DB)**2+ADD(1,1))-OD/SQRT((R+DA+DB)**2+ADD(1,1))              
     DZQXX=-TD/SQRT((R+DA)**2+FD*QB**2+ADQ)+TD/SQRT((R-DA)**2          
    1+FD*QB**2+                                                        
    1ADQ)+TD/SQRT((R+DA)**2+ADQ)-TD/SQRT((R-DA)**2+ADQ)                
     QXXDZ=-TD/SQRT((R-DB)**2+FD*QA**2+AQD)+TD/SQRT((R+DB)**2          
    1+FD*QA**2+                                                        
    1AQD)+TD/SQRT((R-DB)**2+AQD)-TD/SQRT((R+DB)**2+AQD)                
     DZQZZ=-OD/SQRT((R+DA-TD*QB)**2+ADQ)+OD/SQRT((R-DA-TD*             
    1QB)**2+ADQ)-OD/SQRT((R+DA+TD*QB)**2+ADQ)+OD/SQRT((R-DA+TD*QB)     
    1**2+ADQ)-TD/SQRT((R-DA)**2+ADQ)+TD/SQRT((R+DA)**2+ADQ)            
     QZZDZ=-OD/SQRT((R+TD*QA-DB)**2+AQD)+OD/SQRT((R+TD*QA+             
    1DB)**2+AQD)-OD/SQRT((R-TD*QA-DB)**2+AQD)+OD/SQRT((R-2.D           
    100*QA+DB)**2+AQD)+TD/SQRT((R-DB)**2+AQD)-TD/SQRT((R+DB)**2        
    1+AQD)                                                             
     QXXQXX=TD/SQRT(R**2+FD*(QA-QB)**2+AQQ)+TD/SQRT(R**2+FD*(QA+QB)**2+
    1AQQ)-FD/SQRT(R**2+FD*QA**2+AQQ)-FD/SQRT(R**2+FD*QB**2+AQQ)+FD/SQRT
    2(R**2+AQQ)                                                        
     QXXQYY=FD/SQRT(R**2+FD*QA**2+FD*QB**2+AQQ)-FD/SQRT(R**2+FD*QA**2+A
    1QQ)-FD/SQRT(R**2+FD*QB**2+AQQ)+FD/SQRT(R**2+AQQ)                  
     QXXQZZ=TD/SQRT((R-TD*QB)**2+FD*QA**2+AQQ)+TD/SQRT((R+TD*QB)**2+FD*
    1QA**2+AQQ)-TD/SQRT((R-TD*QB)**2+AQQ)-TD/SQRT((R+TD*QB)**2+AQQ)-FD/
    2SQRT(R**2+FD*QA**2+AQQ)+FD/SQRT(R**2+AQQ)                         
     QZZQXX=TD/SQRT((R+TD*QA)**2+FD*QB**2+AQQ)+TD/SQRT((R-TD*QA)**2+FD*
    1QB**2+AQQ)-TD/SQRT((R+TD*QA)**2+AQQ)-TD/SQRT((R-TD*QA)**2+AQQ)-FD/
    2SQRT(R**2+FD*QB**2+AQQ)+FD/SQRT(R**2+AQQ)                         
     QZZQZZ=OD/SQRT((R+TD*QA-TD*QB)**2+AQQ)+OD/SQRT((R+TD*QA+TD*QB)**2+
    1AQQ)+OD/SQRT((R-TD*QA-TD*QB)**2+AQQ)+OD/SQRT((R-TD*QA+TD*QB)**2+AQ
    2Q)-TD/SQRT((R-TD*QA)**2+AQQ)-TD/SQRT((R+TD*QA)**2+AQQ)-TD/SQRT((R-
    3TD*QB)**2+AQQ)-TD/SQRT((R+TD*QB)**2+AQQ)+FD/SQRT(R**2+AQQ)        
     DXQXZ=-TD/SQRT((R-QB)**2+(DA-QB)**2+ADQ)+TD/SQRT((R+QB)**2+(DA-QB)
    1**2+ADQ)+TD/SQRT((R-QB)**2+(DA+QB)**2+ADQ)-TD/SQRT((R+QB)**2+(DA+Q
    2B)**2+ADQ)                                                        
     QXZDX=-TD/SQRT((R+QA)**2+(QA-DB)**2+AQD)+TD/SQRT((R-QA)**2+(QA-DB)
    1**2+AQD)+TD/SQRT((R+QA)**2+(QA+DB)**2+AQD)-TD/SQRT((R-QA)**2+(QA+D
    2B)**2+AQD)                                                        
     QXYQXY=FD/SQRT(R**2+TD*(QA-QB)**2+AQQ)+FD/SQRT(R**2+TD*(QA+QB)**2+
    1AQQ)-8.D00/SQRT(R**2+TD*(QA**2+QB**2)+AQQ)                        
     QXZQXZ=TD/SQRT((R+QA-QB)**2+(QA-QB)**2+AQQ)-TD/SQRT((R+QA+QB)**2+(
    1QA-QB)**2+AQQ)-TD/SQRT((R-QA-QB)**2+(QA-QB)**2+AQQ)+TD/SQRT((R-QA+
    2QB)**2+(QA-QB)**2+AQQ)-TD/SQRT((R+QA-QB)**2+(QA+QB)**2+AQQ)+TD/SQR
    3T((R+QA+QB)**2+(QA+QB)**2+AQQ)+TD/SQRT((R-QA-QB)**2+(QA+QB)**2+AQQ
    4)-TD/SQRT((R-QA+QB)**2+(QA+QB)**2+AQQ)                            
     DXDX=DXDX/P2                                                      
     DZDZ=DZDZ/P2                                                      
     DZQXX=DZQXX/P3                                                    
     QXXDZ=QXXDZ/P3                                                    
     DZQZZ=DZQZZ/P3                                                    
     QZZDZ=QZZDZ/P3                                                    
     DXQXZ=DXQXZ/P3                                                    
     QXZDX=QXZDX/P3                                                    
     QXXQXX=QXXQXX/P4                                                  
     QXXQYY=QXXQYY/P4                                                  
     QXXQZZ=QXXQZZ/P4                                                  
     QZZQXX=QZZQXX/P4                                                  
     QZZQZZ=QZZQZZ/P4                                                  
     QXZQXZ=QXZQXZ/P4                                                  
     QXYQXY=QXYQXY/P4                                                  
     RI(6)=DZDZ                                                         
     RI(7)=DXDX                                                         
     RI(8)=-EDZ-QZZDZ                                                   
     RI(9)=-EDZ-QXXDZ                                                   
     RI(10)=-QXZDX                                                      
     RI(13)=-DZE-DZQZZ                                                  
     RI(14)=-DZE-DZQXX                                                  
     RI(15)=-DXQXZ                                                      
     RI(16)=EE+EQZZ+QZZE+QZZQZZ                                         
     RI(17)=EE+EQZZ+QXXE+QXXQZZ                                         
     RI(18)=EE+EQXX+QZZE+QZZQXX                                         
     RI(19)=EE+EQXX+QXXE+QXXQXX                                         
     RI(20)=QXZQXZ                                                      
     RI(21)=EE+EQXX+QXXE+QXXQYY                                         
     RI(22)=QXYQXY                                                      
     ADD(1,1)=ADD(1,2)                                                 
  40 CONTINUE                                                          
                                                                       
     CONVERT INTO EV.                                                  
                                                                       
     DO 50 I=2,22                                                      
  50 RI(I)=RI(I)*27.21D00                                                
                                                                       
     CALCULATE CORE-ELECTRON ATTRACTIONS.                              
                                                                       
     CORE(2,1)=TORE(NJ)*RI(2)                                           
     CORE(3,1)=TORE(NJ)*RI(3)                                           
     CORE(4,1)=TORE(NJ)*RI(4)                                           
     CORE(2,2)=TORE(NI)*RI(5)                                           
     CORE(3,2)=TORE(NI)*RI(11)                                          
     CORE(4,2)=TORE(NI)*RI(12)                                          
     RETURN                                                            
                                                                       
     END                                                               