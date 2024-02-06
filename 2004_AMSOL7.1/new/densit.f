      SUBROUTINE DENSIT( C,MDIM, NORBS,NDUBL, NSINGL, FRACT, P) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
      DIMENSION P(*), C(MDIM,*) 
C*********************************************************************** 
C 
C   DENSIT COMPUTES THE DENSITY MATRIX GIVEN THE EIGENVECTOR MATRIX, AND 
C          INFORMATION ABOUT THE M.O. OCCUPANCY. 
C 
C  INPUT:  C     = SQUARE EIGENVECTOR MATRIX, C IS OF SIZE MDIM BY MDIM 
C                  AND THE EIGENVECTORS ARE STORED IN THE TOP LEFT-HAND 
C                  CORNER. 
C          NORBS = NUMBER OF ORBITALS 
C                  (MUST BE EQUAL TO MDIM IN THE CRAY VERSION) 
C          NDUBL = LABEL OF THE LAST DOUBLY-OCCUPIED M.O ( =0 IF UHF) 
C          NSINGL= LABEL OF THE LAST OCCUPIED M.O 
C 
C   ON EXIT: P   = DENSITY MATRIX, PACKED, CANONICAL 
C 
C*********************************************************************** 
C 
C SET UP LIMITS FOR SUMS 
C  NL1 = BEGINING OF ONE ELECTRON SUM 
C  NU1 = END OF SAME 
C  NL2 = BEGINING OF TWO ELECTRON SUM 
C  NU2 = END OF SAME 
C 
C    SCALAR VERSION 
C     L=0 
C     DO 40 I=1,NORBS 
C        DO 30 J=1,I 
C           L=L+1 
C           SUM2=0.D0 
C           DO 10 K=1,NDUBL 
C  10       SUM2=SUM2+C(I,K)*C(J,K) 
C           SUM1=0.D0 
C           DO 20 K=NDUBL+1,NSINGL 
C  20       SUM1=SUM1+C(I,K)*C(J,K) 
C  30    P(L)=(SUM2*2.D0+SUM1*FRACT) 
C  40 CONTINUE 
C 
C     CRAY VERSION 
C     COMMON /SCRACH/ PSYM(MORB2),B(MORB2) 
C
C     /SCRACH/ HAS BEEN CONVERTED TO /SCRCHR/ AND /SCRCHI/ FOR AMSOL    
      COMMON /SCRCHR/ PSYM(MORB2),B(MORB2),                             GCL0393
     *                DUM3(6*MAXPAR**2+1-2*MORB2)                       GCL0393
C
       SAVE
      L=0 
      DO 20 J=1,NORBS 
      DO 10 I=1,NDUBL 
      L=L+1 
   10 B(L)=C(J,I)*2.D0 
      DO 20 I=NDUBL+1,NSINGL 
      L=L+1 
   20 B(L)=C(J,I)*FRACT 
      CALL MXM (C,NORBS,B,NSINGL,PSYM,NORBS) 
      L=0 
      DO 30 I=1,NORBS 
      DO 30 J=I,I+(I-1)*NORBS,NORBS 
      L=L+1 
   30 P(L)=PSYM(J) 
      RETURN 
      END 
