
     FUNCTION SPCG(C1,C2,C3,C4,W)                        
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
     INCLUDE 'SIZES/NOLIST'
     DIMENSION C1(*),C2(*),C3(*),C4(*),W(*)
     COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
    +                NLAST(NUMATM),NDUMY(6)
     COMMON /TWOELE/ GSS(54),GSP(54),GPP(54),GP2(54),HSP(54)
     LOGICAL CIYES                                                     
********************************************************************
                                                                       
     SPCG CALCULATES THE REPULSION BETWEEN ELECTRON 1 IN MOLECULAR     
     ORBITALS C1 AND C2 AND ELECTRON 2 IN M.O.S C3 AND C4 FOR THE 
     VALENCE SP SHELL AT AN MNDO OR MINDO/3 LEVEL.
                                                                       
                            USAGE                                      
      XJ=SPCG(C(1,I),C(1,J),C(1,K),C(1,L))                             
  OR, XJ=<I(1),J(1)/K(2),L(2)>

    ON INPUT C1    THE FIRST COLUMN MOLECULAR ORBITAL OF ELECTRON ONE.
             C2        SECOND                                           
             C3        FIRST                                      TWO.
             C4        SECOND                                           

   ON OUTPUT SPCG   =   <C1(1)*C2(1)/C3(2)*C4(2)>
*********************************************************************
     COMMON /KEYWRD/ KEYWRD
     CHARACTER*80 KEYWRD
     DATA ITYPE /1/
 90  CONTINUE
     GOTO (100,200,300) ITYPE
 100 CONTINUE
     IF(INDEX(KEYWRD,'MINDO3') .NE. 0) THEN
     ITYPE=2
     ELSE
     ITYPE=3
     ENDIF
     GOTO 90
                           ******************
                           *      MNDO      *
                           *     OPTION     *
                           ******************
300  SPCG=0.0D00                                                       
     KK=0                                                              
     DO 9 II=1,NUMAT                                                  
     IA=NFIRST(II)                                                     
     IB=NLAST(II)                                                      
     IMINUS=II-1                                                       
     DO 10 JJ=1,IMINUS                                                 
     JA=NFIRST(JJ)                                                     
     JB=NLAST(JJ)                                                      
     DO 11 I=IA,IB                                                     
     DO 11 J=IA,I                                                      
     DO 11 K=JA,JB                                                     
     DO 11 L=JA,K                                                      
     KK=KK+1                                                           
     WINT=W(KK)                                                        
     SPCG=SPCG+WINT*(C1(I)*C2(J)*C3(K)*C4(L)                           
    . + C1(K)*C2(L)*C3(I)*C4(J))                                       
     IF(I.NE.J) SPCG=SPCG+WINT*(C1(J)*C2(I)*C3(K)*C4(L)                
    . + C1(K)*C2(L)*C3(J)*C4(I))                                       
     IF(K.NE.L) SPCG=SPCG+WINT*(C1(I)*C2(J)*C3(L)*C4(K)                
    . + C1(L)*C2(K)*C3(I)*C4(J))                                       
     IF((I.NE.J).AND.(K.NE.L))SPCG=SPCG+WINT*(C1(J)*C2(I)*C3(L)*C4(K)  
    . +C1(L)*C2(K)*C3(J)*C4(I))                                        
  11 CONTINUE                                                          
  10 CONTINUE                                                          
   9 CONTINUE                                                          
     GOTO 301
                           ******************
                           *     MINDO/3    *
                           *     OPTION     *
                           ******************
 200 CONTINUE
     SPCG=0.D0
     KR=0
     DO 210 II=1,NUMAT
     IA=NFIRST(II)
     IB=NLAST(II)
     IM1=II-1
     DO 220 JJ=1,IM1
     KR=KR+1
     ELREP=W(KR)
     JA=NFIRST(JJ)
     JB=NLAST(JJ)
     DO 240 I=IA,IB                                                  
     DO 240 K=JA,JB                                                  
 240 SPCG=SPCG+ELREP*(C1(I)*C2(I)*C3(K)*C4(K)+
    +                 C1(K)*C2(K)*C3(I)*C4(I))
 220 CONTINUE
 210 CONTINUE
 301 CONTINUE
     ATEMP=SPCG                                                       
     IS1=0                                                             
     DO 3 I1=1,NUMAT                                                  
     IS1=IS1+1                                                         
     IZN=NAT(I1)                                                     
                                                                       
      (SS/SS)                                                          
                                                                       
     SPCG=SPCG+C1(IS1)*C2(IS1)*C3(IS1)*C4(IS1)*GSS(IZN)                
     IF(IZN.LT.3) GO TO 3                                              
     IS=IS1                                                            
     IS1=IS1+1                                                         
      IX=IS1                                                           
     IS1=IS1+1                                                         
      IY=IS1                                                           
     IS1=IS1+1                                                         
      IZ=IS1                                                           
     SPCG=SPCG+GPP(IZN)*                                               
    1                       (                                          
                                                                       
      (PP/PP) FOR P=X,Y,Z                                              
                                                                       
    2   C1(IX)*C2(IX)*C3(IX)*C4(IX)+                                   
    2   C1(IY)*C2(IY)*C3(IY)*C4(IY)+                                   
    3   C1(IZ)*C2(IZ)*C3(IZ)*C4(IZ)                                    
    4                       )                                          
     SPCG=SPCG+GSP(IZN)*                                               
    1                       (                                          
                                                                       
      (SS/PP)+(PP/SS) FOR P=X,Y,Z                                      
                                                                       
    1   C1(IS)*C2(IS)*C3(IX)*C4(IX)+                                   
    2   C1(IS)*C2(IS)*C3(IY)*C4(IY)+                                   
    3   C1(IS)*C2(IS)*C3(IZ)*C4(IZ)+                                   
    4   C1(IX)*C2(IX)*C3(IS)*C4(IS)+                                   
    5   C1(IY)*C2(IY)*C3(IS)*C4(IS)+                                   
    6   C1(IZ)*C2(IZ)*C3(IS)*C4(IS)                                    
    7                       )                                          
     SPCG=SPCG+GP2(IZN)*                                               
    1                       (                                          
                                                                       
      (PP/P,P,)+(P,P,/PP) FOR P.NE.P,=X,Y,Z                            
                                                                       
    1   C1(IX)*C2(IX)*C3(IY)*C4(IY)+                                   
    2   C1(IX)*C2(IX)*C3(IZ)*C4(IZ)+                                   
    3   C1(IY)*C2(IY)*C3(IZ)*C4(IZ)+                                   
    4   C1(IY)*C2(IY)*C3(IX)*C4(IX)+                                   
    5   C1(IZ)*C2(IZ)*C3(IX)*C4(IX)+                                   
    6   C1(IZ)*C2(IZ)*C3(IY)*C4(IY)                                    
    7                       )                                          
     TEMP1=HSP(IZN)                                                    
     DO 4 J1=IX,IZ                                                     
     SPCG=SPCG+TEMP1*                                                  
    1                       (                                          
                                                                       
      (SP/SP)+(SP/PS)+(PS/SP)+(PS/PS) FOR P=X,Y,Z                      
                                                                       
    2   C1(IS)*C2(J1)*C3(J1)*C4(IS)+                                   
    2   C1(IS)*C2(J1)*C3(IS)*C4(J1)+                                   
    3   C1(J1)*C2(IS)*C3(IS)*C4(J1)+                                   
    4   C1(J1)*C2(IS)*C3(J1)*C4(IS)                                    
    5                       )                                          
   4 CONTINUE                                                          
     TEMP1=0.5D0*(GPP(IZN)-GP2(IZN))
     DO 5 J1=IX,IZ                                                     
     DO 6 K1=IX,IZ                                                     
     IF(J1.EQ.K1) GO TO 6                                              
     SPCG=SPCG+TEMP1*                                                  
    1                       (                                          
                                                                       
      (PP,/PP,)+(PP,/P,P)+(P,P/PP,)+(P,P/P,P) FOR P.NE.P,=X,Y,Z        
                                                                       
    2   C1(J1)*C2(K1)*C3(J1)*C4(K1)+                                   
    3   C1(J1)*C2(K1)*C3(K1)*C4(J1)                                    
    4                       )                                          
   6 CONTINUE                                                          
   5 CONTINUE                                                          
   3 CONTINUE                                                          
     RETURN                                                            
     END                                                               