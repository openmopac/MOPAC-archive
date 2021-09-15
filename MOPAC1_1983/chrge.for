
     SUBROUTINE CHRGE(P,Q)
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                 
     INCLUDE 'SIZES/NOLIST'
     DIMENSION P(*),Q(*)
     COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
    1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
    +                NCLOSE,NOPEN
***********************************************************************
                                                                       
      CHRGE STORES IN Q THE TOTAL ELECTRON DENSITIES ON THE ATOMS      
                                                                       
      ON INPUT P      = DENSITY MATRIX                                 

      ON OUTPUT Q     = ATOM ELECTRON DENSITIES                        
                                                                       
***********************************************************************
     K=0                                                               
     DO 1 I=1,NUMAT                                                    
         IA=NFIRST(I)                                                      
         IB=NLAST(I)                                                       
         Q(I)=0.D0                                                         
         DO 1 J=IA,IB                                                      
             K=K+J                                                             
   1         Q(I)=Q(I)+P(K)                                                    
     RETURN                                                            
     END                                                               