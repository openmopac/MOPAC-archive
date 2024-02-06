
     SUBROUTINE DEPVAR (A,I,W,L)                                       
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
***********************************************************************
                                                                       
  IN SUBROUTINE HADDON WHEN M, THE SYMMETRY OPERATION, IS 18 DEPVAR IS 
  CALLED. DEPVAR SHOULD THEN CONTAIN A USER-WRITTEN SYMMETRY OPERATION.
  SEE HADDON TO GET THE IDEA ON HOW TO WRITE DEPVAR.                   
                                                                       
 ON INPUT:                                                             
           A = ARRAY OF INTERNAL COORDINATES                           
           I = ADDRESS OF REFERENCE ATOM                               
 ON OUTPUT:                                                            
           L = 1 (IF A BOND-LENGTH IS THE DEPENDENT FUNCTION)          
             = 2 (IF AN ANGLE IS THE DEPENDENT FUNCTION)               
             = 3 (IF A DIHEDRAL ANGLE IS THE DEPENDENT FUNCTION)       
           W = VALUE OF THE FUNCTION                                   
                                                                       
  NOTE:  IT IS THE WRITER'S RESPONSIBILITY TO MAKE CERTAIN THAT THE    
         SUBROUTINE DOES NOT CONTAIN ANY ERRORS!                       
***********************************************************************
     RETURN                                                            
     END                                                               