C======================================================================+
      SUBROUTINE TRUST4 (N,F,X,G,V,FAIL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      DIMENSION X(*),G(*),V(N,*)
      LOGICAL FAIL
C ---------------------------------------------------------------------+
C     CALLED BY "TRUSTG". SET OR REVISE A HESSIAN MATRIX.
C  INPUT:
C     X(N),G(N) CURRENT COORDINATES AND GRADIENT.
C     H         CURRENT HESSIAN'S ESTIMATE.
C     B,V       ASSOCIATED EIGENVALUES AND VECTORS.
C     FAIL      .TRUE. IF SCF DIVERGENCE OCCURED.
C     KMODE     MINIMUM NUMBER OF MODES TO BE REVISED.
C  OUTPUT
C     H         NEW HESSIAN IN CANONICAL ORDER.
C     F,X,G     BEST POINT FOUND WHILE REVISING HESSIAN.
C     KMODE     NUMBER OF MODES ACTUALLY REVISED.
C ---------------------------------------------------------------------+
      COMMON /OPTMCR/ H(MAXHES),HV(MAXPAR**2),B(MAXPAR)
     1               ,XOLD(MAXPAR),GOLD(MAXPAR),POLD(MAXPAR),P(MAXPAR)
     2               ,RADIUS,RHOI,RHOS,GNORM,STEP(0:10),COST(0:10)
     3               ,FOST(0:10),GOST(MAXPAR,0:10)
     4               ,HNEW(MAXHES),BNEW(MAXPAR),VNEW(MAXPAR**2)
     5               ,FBEST,XBEST(MAXPAR),GBEST(MAXPAR)
     6       /OPTMCI/ NEW,KMODE,KSTOP,LM(0:10),MINDEX
     7       /OPTMCL/ LGRAD
      COMMON /SCRCHR/ WORK(MAXPAR**2),DUM(5*MAXPAR**2+1)                DL0397
     1       /PRECI / SCFCV,SCFTOL,EG(3),ESTIM(3),PMAX(3),KTYP(MAXPAR)
     2       /OPTIMI/ IMP,IMP0
      GRMS()=SQRT(DOT(G,G,N)/DBLE(N))
      DO 10 I=1,N
      XOLD(I)=X(I)
      XBEST(I)=X(I)
      GOLD(I)=G(I)
   10 GBEST(I)=G(I)
      FOLD=F
      FBEST=F
      RMSOLD=GRMS()
C
C     REVISE AT LEAST NCUT LOWEST MODES.
      NCUT=MAX(N/3,KMODE)
      KMODE=0
      DO 60 J=1,N
      IF (ABS(B(J)).LT.1D-15) B(J)=1D-15
      SLOPE=0D0
      ERR=0D0
      DO 30 I=1,N
      SLOPE=GOLD(I)*V(I,J)+SLOPE
   30 ERR=(V(I,J)*EG(KTYP(I)))**2+ERR
      SLOPE=-B(J)*SLOPE
      PAS=SQRT(ERR)/(ABS(B(J))*2.0D-3)
      PAS=SIGN(MAX(1D-3,MIN(PAS,6D-2)),SLOPE)
      DO 40 I=1,N
   40 X(I)=XOLD(I)+PAS*V(I,J)
      CALL COMPFG (X,F,FAIL,G,.TRUE.)
      IF (FAIL) CALL COMPFG (X,F,FAIL,G,.TRUE.)
      IF (FAIL) GOTO 60
      KMODE=KMODE+1
C     POWELL'S UPDATE FOUND BEST IN CONTEXT.
      CALL TRUST1 (N,XOLD,GOLD,X,G,H,0D0,NEW,2)
      RR=GRMS()
      IF (RR.LT.RMSOLD) THEN
         CALL SCOPY (N,X,1,XBEST,1)
         CALL SCOPY (N,G,1,GBEST,1)
         FBEST=F
         RMSOLD=RR
      ENDIF
      CALL HQRII (H,N,N,BNEW,VNEW)
C     STOPPING CRITERIA ("AND" CONDITIONS):
C     1- EIGENVALUES STABILIZED (50%, RELATIVE).
C     2- AT LEAST KMODE WEAKEST MODES REVISED.
C     3- EACH UNSTABLE MODE CHECKED BEFORE TO LEAVE.
      CRIT=0D0
      DO 50 I=1,N
   50 CRIT=MAX(CRIT,ABS(BNEW(I)-B(I))/ABS(B(I)))
      CALL SCOPY (N,BNEW,1,B,1)
      DO 51 I=J+1,N
      IF (B(I).LT.0D0) GOTO 60
   51 CONTINUE
      IF (CRIT.LT.0.5D0.AND.KMODE.GE.NCUT) GOTO 70
   60 CONTINUE
C     RETURN BEST POINT FOUND (LOWEST GRADIENT NORM).
   70 CALL SCOPY (N,XBEST,1,X,1)
      CALL SCOPY (N,GBEST,1,G,1)
      F=FBEST
      MINDEX=0
      DO 80 I=1,N
   80 IF (B(I).LT.-1D-10) MINDEX=MINDEX+1
      END
