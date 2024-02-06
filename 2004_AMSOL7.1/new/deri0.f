      SUBROUTINE DERI0 (C,CT,E,N) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
       INCLUDE 'SIZES.i'
      PARAMETER (MFBWO=9*MAXPAR)                                        3GL3092
C 
C     COMPUTE THE DIAGONAL DOMINANT PART OF THE SUPER-MATRIX AND 
C     DEFINE THE SCALAR COEFFICIENTS APPLIED ON EACH ROW OF THE 
C     SUPER LINEAR SYSTEM IN ORDER TO REDUCE THE EIGENVALUE SPECTRUM OF 
C     THE ELECTRONIC HESSIAN, 
C     THUS SPEEDING CONVERGENCE OF RELAXATION PROCESS IN 'DERI2'. 
C  INPUT 
C     C(N,N)           : M.O. COEFFICIENTS. 
C     E(N)             : EIGENVALUES OF FOCK MATRIX. 
C     N                : NUMBER OF M.O. 
C     NBO(3)           : OCCUPANCY BOUNDARIES. 
C     FRACT            : PARTIAL OCCUPANCY OF 'OPEN' SHELLS. 
C  OUTPUT 
C     CT               : TRANSPOSE OF C (OPEN,VIRTUAL PACKED). 
C  OUTPUT IN /SCRAH1/ 
C     SCALAR(MINEAR)   : SCALE APPLIED ON EACH COLUMN AND ROW OF THE 
C                        SYMMETRIC SUPER SYSTEM. 
C     DIAG  (MINEAR)   : DIAGONAL ELEMENTS * SCALAR. 
C 
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
     3       /SCRAH1/ SCALAR(MPACK/2),DIAG(MPACK/2),FRACT, FBWO(MFBWO), 3GL3092
     4                NBO(3), IDUM                                      3GL3092
      DIMENSION C(N,N),E(N),CT(*) 
       SAVE
      SHIFT=2.36D0 
C 
C     DOMINANT DIAGONAL PART OF THE SUPER-MATRIX. 
C     ------------------------------------------- 
      L=1 
      IF(NBO(2).GT.0 .AND. NBO(1).GT.0) THEN 
C        OPEN-CLOSED 
         CONST=1.D0/(2.D0-FRACT) 
         DO 10 J=1,NBO(1) 
         DO 10 I=NBO(1)+1,NOPEN 
         DIAG(L)=(E(I)-E(J))*CONST 
   10    L=L+1 
      ENDIF 
      IF(NBO(3).GT.0 .AND. NBO(1).GT.0) THEN 
C        VIRTUAL-CLOSED 
         DO 20 J=1,NBO(1) 
         DO 20 I=NOPEN+1,N 
         DIAG(L)=(E(I)-E(J))*0.5D0 
   20    L=L+1 
      ENDIF 
      IF(NBO(3).NE.0 .AND. NBO(2).NE.0) THEN 
C        VIRTUAL-OPEN 
         CONST=1.D0/FRACT 
         DO 30 J=NBO(1)+1,NOPEN 
         DO 30 I=NOPEN+1,N 
         DIAG(L)=(E(I)-E(J))*CONST 
   30    L=L+1 
      ENDIF 
C 
C     TAKE SCALE FACTORS AS (SHIFT-DIAG)**(-0.5) . 
C     ------------------------------------------ 
      DO 40 I=1,L-1 
      SCALAR(I)=SQRT(1.D0/MAX(0.3D0*DIAG(I),DIAG(I)-SHIFT)) 
   40 DIAG(I)=DIAG(I)*SCALAR(I) 
C 
C     STORE CT = C' FOR FURTHER USE IN DERI1 AND DERI22 
C     ------------------------------------------------- 
C     CLOSED 
CCC   L=0 
CCC   DO 50 I=1,N 
CCC   DO 50 J=1,NBO(1) 
CCC   L=L+1 
CCC50 CT(L)=C(I,J) 
      L=N*NBO(1) 
C     OPEN 
      DO 60 I=1,N 
      DO 60 J=NBO(1)+1,NOPEN 
      L=L+1 
   60 CT(L)=C(I,J) 
C     VIRTUAL 
      DO 70 I=1,N 
      DO 70 J=NOPEN+1,N 
      L=L+1 
   70 CT(L)=C(I,J) 
      RETURN 
      END 
