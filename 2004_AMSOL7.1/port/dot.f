      DOUBLE PRECISION FUNCTION DOT(X,Y,N) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      EXTERNAL SDOT                                                     GL0492
      DIMENSION X(*), Y(*) 
C     DOT =    DOT PRODUCT OF X AND Y, LENGHT N. 
C     CRAY VERSION 
      DOT=SDOT(N,X,1,Y,1) 
      RETURN 
      END 
