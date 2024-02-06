
     SUBROUTINE SYMTRY
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)                     
     INCLUDE 'SIZES/NOLIST'
     COMMON /GMETRY/ GEO(3,NUMATM)
     COMMON /GEOSYM/ NDEP, LOCPAR(200), IDEPFN(200), LOCDEP(200)
********************************************************************** 
                                                                       
  SYMTRY COMPUTES THE BOND LENGTHS AND ANGLES THAT ARE FUNCTIONS OF    
         OTHER BOND LENGTHS AND ANGLES.                                
                                                                       
 ON INPUT GEO     = KNOWN INTERNAL COORDINATES                         
          NDEP    = NUMBER OF DEPENDENCY FUNCTIONS.
          IDEPFN  = ARRAY OF DEPENDENCY FUNCTIONS.
          LOCDEP  = ARRAY OF LABELS OF DEPENDENT ATOMS.
          LOCPAR  = ARRAY OF LABELS OF REFERENCE ATOMS.
                                                                       
  ON OUTPUT THE ARRAY "GEO" IS FILLED                                    
***********************************************************************
                                                                       
     NOW COMPUTE THE DEPENDENT PARAMETERS.                             
                                                                       
     DO 30 I=1,NDEP                                                    
        CALL HADDON (VALUE,LOCN,IDEPFN(I),LOCPAR(I),GEO)
        J=LOCDEP(I)                                                    
  30 GEO(LOCN,J)=VALUE                                                   
     RETURN
     END