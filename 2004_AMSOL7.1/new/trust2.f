C======================================================================+
      SUBROUTINE TRUST2 (N,X,F,G,P,H,V,B,RADIUS,PAS,MODE,PRED1,PRED2)   1201BL04
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               1201BL04
      INCLUDE 'SIZES.i'
      INCLUDE 'FFILES.i'                                                DJG0496
      DIMENSION X(*),G(*),P(*),H(*),V(N,*),B(*)
C ---------------------------------------------------------------------+
C     STEP BY QUADRATIC OR TRUST REGION SCHEME; STEPSIZE CHOICE.
C  INPUT:
C     X, F, G   CURRENT COORDINATES, ENERGY & GRADIENT.
C     H(*)      CURRENT HESSIAN (APPROXIMATED OR EXACT).
C     RADIUS    TRUST RADIUS.
C     NEW       HESSIAN OBSOLESCENCE COUNTER.
C     LGRAD     .TRUE. IF COST FUNCTION IS GRADIENT NORM.
C  OUTPUT:
C     P(N)      SEARCH DIRECTION, NORMALIZED TO UNITY.
C     PAS       PRECONIZED STEPSIZE.
C     V(N,N)    HESSIAN EIGENVECTORS.
C     B(N)      HESSIAN EIGENVALUES.
C     NEW       HESSIAN OBSOLESCENCE COUNTER (UPDATED).
C     MODE      1= TRUST REGION; 2= NEWTON.
C     PRED1 & 2 QUADRATIC PREDICTION DELTA-COST=PRED1*PAS+PRED2*PAS**2.
C ---------------------------------------------------------------------+
      COMMON /SCRCHR/ A(MAXPAR),C(MAXPAR),D(MAXPAR)
     1               ,DUM(6*MAXPAR**2+1-3*MAXPAR)                       DL0397
     2       /OPTIMI/ IMP,IMP0
     3       /OPTMCI/ NEW,KMODE,KSTOP,LM(0:10),MINDEX
     4       /OPTMCL/ LGRAD
      LOGICAL LGRAD
      RHO2=RADIUS**2
C
C     HESSIAN: EIGENVECTORS V, EIGENVALUES B.
      CALL UNCANO (H,N,V,N)
      CALL DIAGIV (V,N,N,B,EPS1)                                        DL0397
      EPS1=MAX(EPS1,1D-10)
      MINDEX=0
      DO 10 I=1,N
   10 IF (B(I).LT.-EPS1) MINDEX=I
C     GRADIENT IN EIGENVECTOR BASIS SET: A.
      CALL MTXM (V,N,G,N,A,1)
C
C     NEWTON STEP, ANNIHILATED FROM ZERO EIGENVALUES: C (IN BASIS V).
      KNW=0
      DO 20 I=1,N
      IF (ABS(B(I)).LE.EPS1) THEN
         C(I)=0D0
         KNW=KNW+1
      ELSE IF (LGRAD) THEN
         C(I)=-A(I)/B(I)
      ELSE
C        (COST IS ENERGY): CRUDELY STABILIZED, BUT EFFICIENT...
         C(I)=-A(I)/ABS(B(I))
      ENDIF
   20 CONTINUE
      IF (LGRAD) THEN
C        SWITCH A & B INTO GRADIENT' EUCLIDEAN NORM TAYLOR EXPANSION.
         DO 30 I=1,N
         A(I)=A(I)*B(I)
   30    D(I)=B(I)**2
      ELSE
         CALL SCOPY (N,B,1,D,1)
      ENDIF
      PAS=SQRT(DOT(C,C,N))
C     NEWTON STEP ACCEPTED IF STEPSIZE < RADIUS.
      IF (PAS.LE.RADIUS) THEN
         MODE=2
         IF (MINDEX.NE.0.AND..NOT.LGRAD) NEW=NEW+MODE
         IF (IMP.GT.1) WRITE(JOUT,'(4X,''NEWTON STEPSIZE='',F9.5,
     .   I5,'' MODE(S) ANNIHILATED.'')') PAS,KNW
         GOTO 100
      ENDIF
      PASNW=PAS
C
C     TRUST REGION STEP: MINIMIZE ENERGY ON BOUNDARY HYPERSPHERE.
C     LAGRANGE MULTIPLIER: AMBDA. BRACKET LAMBDA: AMBDAI, AMBDAS.
      MODE=1
c      RINV=1D0/RADIUS                                                  1128BL04 
c      RHO2=RADIUS**2                                                   1128BL04
c      RN=SQRT(DBLE(N))*RINV                                            1128BL04
c      AA=SQRT(DOT(A,A,N))*RINV                                         1128BL04
c      AMBDAI=MIN(-2D-13,D(1)-AA)                                       1128BL04
c      AMBDAS=MIN(-1D-13,D(N)-AA)                                       1128BL04
c      DO 40 I=1,N                                                      1128BL04
c      AMBDAI=MIN(AMBDAI,D(I)-ABS(A(I))*RN)                             1128BL04
c   40 AMBDAS=MIN(AMBDAS,D(I)-ABS(A(I))*RINV)                           1128BL04
c      AMBDAI=MIN(AMBDAI,AMBDAS-1D-13)                                  1128BL04
      DDMIN=-1D-13                                                      1128BL04
      DO 40 I=1,N                                                       1128BL04
   40 DDMIN=MIN(DDMIN,D(I))                                             1128BL04
      AMBDAS=MIN(-1D-13,DDMIN*(1D0+1D-13))                              1128BL04
      AMBDAI=MIN(2D0*AMBDAS,DDMIN-SQRT(DOT(A,A,N))/RADIUS)              1128BL04
C     SQUEEZE THE BRACKET BY LOGARITHMIC BISSECTION.
      ITER=0
   50 ITER=ITER+1
      AMBDA=-SQRT(AMBDAI*AMBDAS)
      F0=0D0
      DO 51 I=1,N
      C(I)=A(I)/(AMBDA-D(I))
   51 F0=F0+C(I)**2
      IF (F0.LT.RHO2) THEN
         AMBDAI=AMBDA
      ELSE IF (F0.GT.RHO2) THEN
         AMBDAS=AMBDA
      ELSE
         GOTO 60
      ENDIF
      IF (ABS(AMBDAS-AMBDAI).GT.MAX(1D-13,ABS(AMBDA)*1D-10)
     ..AND.ITER.LT.100) GOTO 50
   60 PAS=SQRT(F0)
      IF (IMP.GT.1) WRITE(JOUT,'(4X,''NEWTON STEPSIZE='',F9.5,
     .'' REJECTED.'',3X,''LAMBDA='',F14.4)') PASNW,AMBDA
C
C     EXPRESS THE STEP IN X BASIS: P.
  100 PNORM=1D0/PAS
      CALL DSCAL (N,PNORM,C,1)                                          DJG0496
      PRED1=0D0
      PRED2=0D0
      DO 110 I=1,N
      PRED1=PRED1+A(I)*C(I)
  110 PRED2=PRED2+D(I)*C(I)**2
      IF (LGRAD) THEN
         PRED1=2D0*PRED1
      ELSE
         PRED2=0.5D0*PRED2
      ENDIF
      CALL MXM (V,N,C,N,P,1)
      END
