
     FUNCTION HELECT(N,P,H,F)                                 
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
     DIMENSION P(*), H(*), F(*)                               
***********************************************************************
                                                                       
    SUBROUTINE CALCULATES THE ELECTRONIC ENERGY OF THE SYSTEM IN EV.   
                                                                       
    ON ENTRY N = NUMBER OF ATOMIC ORBITALS.                            
             P = DENSITY MATRIX, PACKED, LOWER TRIANGLE.               
             H = ONE-ELECTRON MATRIX, PACKED, LOWER TRIANGLE.          
             F = TWO-ELECTRON MATRIX, PACKED, LOWER TRIANGLE.          
    ON EXIT 
        HELECT = ELECTRONIC ENERGY.                                    

    NO ARGUMENTS ARE CHANGED.                       
                                                                       
***********************************************************************
     ED=0.0D00                                                         
     EE=0.0D00                                                         
     K=0                                                               
     NN=N+1                                                            
     DO 20 I=2,NN                                                      
        K=K+1                                                          
        JJ=I-1                                                         
        ED=ED+P(K)*(H(K)+F(K))                                         
        IF (I.EQ.NN) GO TO 20                                          
        DO 10 J=1,JJ                                                   
           K=K+1                                                       
  10    EE=EE+P(K)*(H(K)+F(K))                                         
  20 CONTINUE                                                          
     EE=EE+.5D00*ED                                                    
     HELECT=EE
     RETURN                                                            
                                                                       
     END                                                               