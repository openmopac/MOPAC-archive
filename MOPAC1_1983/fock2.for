
     SUBROUTINE FOCK2(F,PTOT,P,W,NUMAT,NFIRST,NLAST)
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
     DIMENSION F(*), PTOT(*), W(*), NFIRST(*), NLAST(*), P(*)
***********************************************************************
                                                                       
 FOCK2 FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE FOCK     
 MATRIX
 ON INPUT  PTOT = TOTAL DENSITY MATRIX.
           P    = ALPHA OR BETA DENSITY MATRIX.
           W    = TWO-ELECTRON INTEGRAL MATRIX.
  
  ON OUTPUT F   = PARTIAL FOCK MATRIX
***********************************************************************
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
 300 KK=0                                                              
     DO 61 II=1,NUMAT                                                 
        IA=NFIRST(II)                                                  
        IB=NLAST(II)                                                   
        IMINUS=II-1                                                    
        DO 60 JJ=1,IMINUS                                              
           JA=NFIRST(JJ)                                               
           JB=NLAST(JJ)                                                
        DO 60 I=IA,IB                                                  
           KA=(I*(I-1))/2                                              
        DO 60 J=IA,I                                                   
           KB=(J*(J-1))/2                                              
           IJ=KA+J                                                     
           AA=2.0D00                                                   
           IF (I.EQ.J) AA=1.0D00                                       
        DO 60 K=JA,JB                                                  
           KC=(K*(K-1))/2                                              
           IK=KA+K                                                     
           JK=KB+K                                                     
        DO 60 L=JA,K                                                   
           IL=KA+L                                                     
           JL=KB+L                                                     
           KL=KC+L                                                     
           BB=2.0D00                                                   
           IF (K.EQ.L) BB=1.0D00                                       
           KK=KK+1                                                     
           A=W(KK)                                                     
                                                                       
     A  IS THE REPULSION INTEGRAL (I,J/K,L) WHERE ORBITALS I AND J ARE 
     ON ATOM II, AND ORBITALS K AND L ARE ON ATOM JJ.                  
     AA AND BB ARE CORRECTION FACTORS SINCE                            
     (I,J/K,L)=(J,I/K,L)=(I,J/L,K)=(J,I/L,K)                           
     IJ IS THE LOCATION OF THE MATRIX ELEMENTS BETWEEN ATOMIC ORBITALS 
     I AND J.  SIMILARLY FOR IK ETC.                                   
                                                                       
 THIS FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE FOCK     
 MATRIX.  THE CODE HERE IS HARD TO FOLLOW, AND IMPOSSIBLE TO MODIFY!,  
 BUT IT WORKS,                                                         
           F(IJ)=F(IJ)+BB*A*PTOT(KL) 
           F(KL)=F(KL)+AA*A*PTOT(IJ)                                      
           A=A*AA*BB*0.25D0
           F(IK)=F(IK)-A*P(JL)                                         
           F(IL)=F(IL)-A*P(JK)                                         
           F(JK)=F(JK)-A*P(IL)                                         
           F(JL)=F(JL)-A*P(IK)                                         
  60    CONTINUE                                                       
  61    CONTINUE

     RETURN
 200 KR=0
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
     KA=(I*(I-1))/2               
     KK=KA+I                               
     DO 240 K=JA,JB                                                  
     LL=(K*(K+1))/2
     IK=KA+K                                                     
     F(KK)=F(KK)+PTOT(LL)*ELREP
     F(LL)=F(LL)+PTOT(KK)*ELREP
240  F(IK)=F(IK)-P(IK)*ELREP
 220 CONTINUE
 210 CONTINUE
     RETURN      
     END