      SUBROUTINE DERI23 (F,MINEAR,FD,NINEAR,E,FCI,CMO,EMO,N,NCI1,NCI2) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
       INCLUDE 'SIZES.i'
      PARAMETER (MFBWO=9*MAXPAR)                                        3GL3092
C  1) UNPACK THE C.I-ACTIVE M.O. DERIVATIVES IN M.O. BASIS, 
C     DIAGONAL BLOCKS        INCLUDED. 
C  2) EXTRACT THE FOCK EIGENVALUES RELAXATION OVER C.I-ACTIVE M.O. 
C   INPUT 
C     F(MINEAR)   : UNSCALED SOLUTIONS VECTOR IN M.O. BASIS, 
C                   OFF-DIAGONAL BLOCKS PACKED AS DEFINED IN 'DERI21'. 
C     FD(NINEAR)  : DIAGONAL BLOCKS OF NON-RELAXED FOCK MATRIX 
C                   AS DEFINED IN 'DERI1'. 
C     E(N)        : FOCK EIGENVALUES. 
C     FCI(NINEAR) : DIAGONAL BLOCKS OF RELAXATION OF THE FOCK MATRIX, 
C                   ORDERED AS FD. 
C     N           : NUMBER OF M.O 
C     NCI1,NCI2   : # OF LAST FROZEN CORE M.O , C.I-ACTIVE BAND LENGTH. 
C   OUTPUT 
C     CMO(N,NCI1+1,...,NCI1+NCI2): C.I-ACTIVE M.O DERIVATIVES 
C                                  IN M.O BASIS. 
C     EMO(  NCI1+1,...,NCI1+NCI2): C.I-ACTIVE FOCK EIGENVALUE RELAXATION 
C 
      COMMON /SCRAH1/ SCALAR(MPACK/2),DIAG(MPACK/2),FRACT,FBWO(MFBWO),  3GL3092
     1                NBO(3), IDUM                                      3GL3092
      DIMENSION F(*),FD(*),E(*),FCI(*),CMO(N,*),EMO(*) 
       SAVE
C 
      NOPEN  =NBO(1)+NBO(2) 
C 
C     PART 1. 
C     ------- 
C     COMPUTE AND UNPACK DIAGONAL BLOCKS, DIAGONAL TERMS        INCLUDED, 
C     ACCORDING TO CMO(I,J) = (FD(I,J)-FCI(I,J))/(E(I)-E(J)) 
C     AND TAKING   CMO(I,J)=0 IF E(I)=E(J) (THRESHOLD 1D-4 EV), 
C                             I.E WHEN M.O. DEGENERACY OCCURS. 
      L=1 
      NEND=0 
      DO 30 LOOP=1,3 
      NINIT=NEND+1 
      NEND =NEND+NBO(LOOP) 
      N1=MAX(NINIT,NCI1+1   ) 
      N2=MIN(NEND ,NCI1+NCI2) 
      IF(N2.LT.N1) GO TO 30 
      DO 20 I=N1,N2 
      IF(I.GT.NINIT) THEN 
         DO 10 J=NINIT,I-1 
         DIFFE=E(I)-E(J) 
         IF(ABS(DIFFE).GT.1.D-4) THEN 
            COM=(FD(L)-FCI(L))/DIFFE 
         ELSE 
            COM=0.D0 
         ENDIF 
         CMO(I,J)=-COM 
         CMO(J,I)= COM 
   10    L=L+1 
      ENDIF 
   20 CMO(I,I)= 0.D0 
   30 CONTINUE 
      NCOL=N2-NINIT+1 
      IF (NCOL.GT.0.AND.N2.LT.N) THEN 
         DO 40 J=NINIT,N2 
         DO 40 I=N2+1,N 
         DIFFE=E(I)-E(J) 
         IF(ABS(DIFFE).GT.1.D-4) THEN 
            COM=(FD(L)-FCI(L))/DIFFE 
         ELSE 
            COM=0.D0 
         ENDIF 
         CMO(I,J)=-COM 
         CMO(J,I)= COM 
   40    L=L+1 
      ENDIF 
C 
C     C.I-ACTIVE EIGENVALUES RELAXATION. 
      CALL SCOPY(NCI2,FCI(L),1,EMO(NCI1+1),1) 
C 
C     PART 2. 
C     ------- 
C     UNPACK THE ANTISYMMETRIC MATRIX F IN CMO, (OFF-DIAGONAL BLOCKS). 
C 
      L=1 
      IF(NBO(2).GT.0 .AND. NBO(1).GT.0) THEN 
C        OPEN-CLOSED 
         SCAL=1.D0/(2.D0-FRACT) 
         DO 50 J=1       ,NBO(1) 
CDIR$ IVDEP 
         DO 50 I=NBO(1)+1,NOPEN 
         COM=F(L)*SCAL 
         CMO(I,J)=-COM 
         CMO(J,I)= COM 
   50    L=L+1 
      ENDIF 
      IF(NBO(3).GT.0 .AND. NBO(1).GT.0) THEN 
C        VIRTUAL-CLOSED 
         SCAL=0.5D0 
         DO 60 J=1     ,NBO(1) 
CDIR$ IVDEP 
         DO 60 I=NOPEN+1,N 
         COM=F(L)*SCAL 
         CMO(I,J)=-COM 
         CMO(J,I)= COM 
   60    L=L+1 
      ENDIF 
      IF(NBO(3).NE.0 .AND. NBO(2).NE.0) THEN 
C        VIRTUAL-OPEN 
         SCAL=1.D0/FRACT 
         DO 70 J=NBO(1)+1,NOPEN 
CDIR$ IVDEP 
         DO 70 I=NOPEN+1  ,N 
         COM=F(L)*SCAL 
         CMO(I,J)=-COM 
         CMO(J,I)= COM 
   70    L=L+1 
      ENDIF 
      RETURN 
      END 
