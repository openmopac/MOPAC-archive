      SUBROUTINE CHRGE(P,Q) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
      DIMENSION P(*),Q(*) 
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
C*********************************************************************** 
C 
C      CHRGE STORES IN Q THE TOTAL ELECTRON DENSITIES ON THE ATOMS 
C 
C      ON INPUT P      = DENSITY MATRIX 
C 
C      ON OUTPUT Q     = ATOM ELECTRON DENSITIES 
C 
C*********************************************************************** 
       SAVE
      K=0 
      DO 10 I=1,NUMAT 
         IA=NFIRST(I) 
         IB=NLAST(I) 
         Q(I)=0.D0 
         DO 10 J=IA,IB 
            K=K+J 
   10 Q(I)=Q(I)+P(K) 
      RETURN 
      END 
