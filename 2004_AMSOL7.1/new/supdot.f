      SUBROUTINE SUPDOT(S,H,G,N,IG) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      EXTERNAL SDOT                                                     GL0492
C     (S)=(H)*(G) WITH  H  IN PACKED FORM (CANONICAL ORDER). 
C     IG IS THE INCREMENT FOR THE VECTOR G. 
      DIMENSION S(*),H(*),G(*) 
C     CRAY-1 VERSION... BUT POORLY VECTORIZED. 
      K=1 
      L=1 
      DO 10 I=1,N 
C     S(I)=SDOT(I,H(K),1,G,IG,I) 
      S(I)=SDOT(I,H(K),1,G,IG)                                          GCL0393
      IF(I.GT.1) THEN 
         L=L+IG 
         CALL SAXPY(I-1,G(L),H(K),1,S,1) 
      ENDIF 
   10 K=K+I 
      RETURN 
      END 
