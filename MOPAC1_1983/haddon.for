
     SUBROUTINE HADDON (W,L,M,LOC,A)                                   
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
     DIMENSION A(3,50)                                                 
********************************************************************** 
                                                                       
   HADDON CALCULATES THE VALUE OF A SYMMETRY-DEPENDENT VARIABLE        
                                                                       
  ON INPUT: M   = NUMBER SPECIFYING THE SYMMETRY OPERATION             
            LOC = ADDRESS OF REFERENCE ATOM                            
            A   = ARRAY OF INTERNAL COORDINATES                        
  ON OUTPUT W   = VALUE OF DEPENDENT FUNCTION                          
            L   = 1 (FOR BOND LENGTH), 2 (ANGLE), OR 3 (DIHEDRAL)      
********************************************************************** 
     PI = 3.1415926536D00                                              
     IF (M.GT.14) GO TO 160                                            
     I=LOC                                                             
     GO TO                                                             
    +(1,2,3,10,20,30,40,50,60,70,80,90,100,110,130,140,150,160), M     
   3 W=A(3,I)                                                          
     GO TO 120                                                         
  10 W=(PI/2.0D00)-A(3,I)                                              
     GO TO 120                                                         
  20 W=(PI/2.0D00)+A(3,I)                                              
     GO TO 120                                                         
  30 W=(2.0D00*PI/3.0D00)-A(3,I)                                       
     GO TO 120                                                         
  40 W=(2.0D00*PI/3.0D00)+A(3,I)                                       
     GO TO 120                                                         
  50 W=(PI)-A(3,I)                                                     
     GO TO 120                                                         
  60 W=(PI)+A(3,I)                                                     
     GO TO 120                                                         
  70 W=(4.0D00*PI/3.0D00)-A(3,I)                                       
     GO TO 120                                                         
  80 W=(4.0D00*PI/3.0D00)+A(3,I)                                       
     GO TO 120                                                         
  90 W=(3.0D00*PI/2.0D00)-A(3,I)                                       
     GO TO 120                                                         
 100 W=(3.0D00*PI/2.0D00)+A(3,I)                                       
     GO TO 120                                                         
 110 W=-A(3,I)                                                         
 120 L=3                                                               
     RETURN                                                            
   1 L=1                                                               
     W=A(1,I)                                                          
     RETURN                                                            
 130 L=1                                                               
     W=A(1,I)/2.0D00                                                   
     RETURN                                                            
   2 L=2                                                               
     W=A(2,I)                                                          
     RETURN                                                            
 140 L=2                                                               
     W=A(2,I)/2.0D00                                                   
     RETURN                                                            
 150 L=2                                                               
     W=PI-A(2,I)                                                       
     RETURN                                                            
 160 CALL DEPVAR (A,I,W,L)                                             
     RETURN                                                            
                                                                       
     END                                                               