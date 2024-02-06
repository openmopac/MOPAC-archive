      SUBROUTINE MAMLT2(F,P,N,FS,PS,FP,FPPF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GCL0493
C ----------------------------------------------------------------------
C     MAMULT (VECTOR VERSION) IS DEDICATED TO ROUTINE 'PULAY'.
C  INPUT
C     F        : FOCK MATRIX, PACKED CANONICAL
C     P        : DENSITY MATRIX, PACKED CANONICAL
C     N        : NUMBER OF ORBITALS
C     FS,PS,FP : WORK ARRAYS OF SIZE N*N
C  OUTPUT
C     FPPF     : COMMUTATOR FP-PF, LOWER TRIANGLE, PACKED CANONICAL
C ----------------------------------------------------------------------
C     SUBROUTINE RENAMED DJG0295
      DIMENSION F(*),P(*),FS(N,N),PS(N,N),FP(N,N),FPPF(*)
      IROUTE=1
      GOTO 10
      ENTRY FPMPF (F,P,N,FS,PS,FP,FPPF)
C ----------------------------------------------------------------------
C     FPMPF (VECTOR VERSION) IS DEDICATED TO ROUTINE 'ITER'.
C  INPUT
C     F        : FOCK MATRIX, PACKED CANONICAL
C     P        : DENSITY MATRIX, PACKED CANONICAL
C     N        : NUMBER OF ORBITALS
C     FS,PS,FP : WORK ARRAYS OF SIZE N*N
C  OUTPUT
C     FPPF(1)  : LACK OF SELF-CONSISTENCY MEASURED BY THE NORM OF THE
C                COMMUTATOR (FP-PF) / THE NORM OF THE FOCK MATRIX.
C ----------------------------------------------------------------------
      IROUTE=2
   10 CALL UNCANO (F,N,FS,N)
      CALL UNCANO (P,N,PS,N)
      CALL MXM (FS,N,PS,N,FP,N)
      FPPF(1)=0.D0
      L=1
      DO 30 I=2,N
      DO 20 J=1,I-1
      L=L+1
   20 FPPF(L)=FP(I,J)-FP(J,I)
      L=L+1
   30 FPPF(L)=0.D0
      IF (IROUTE.EQ.2) THEN
         PL=SQRT(DOT(FPPF,FPPF,L)/DOT(F,F,L))
         FPPF(1)=PL
      ENDIF
      RETURN
      END
