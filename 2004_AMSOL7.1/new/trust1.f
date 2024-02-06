C======================================================================+
      SUBROUTINE TRUST1 (N,XOLD,GOLD,X,G,H,GPREC,NEW,MODEL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      DIMENSION X(*),G(*),H(*),XOLD(*),GOLD(*)
      COMMON /SCRCHR / S(MAXPAR),V(MAXPAR),T(MAXPAR)
     1                ,DUM(6*MAXPAR**2+1-3*MAXPAR)                      DL0397
C ---------------------------------------------------------------------+
C     HESSIAN UPDATE BY BFGS OR POWELL SUPERLINEAR SCHEME.
C INPUT
C     XOLD(N), GOLD(N) OLD COORDINATES AND GRADIENT
C     X(N), G(N)       NEW COORDINATES AND GRADIENT
C     H(*)             OLD HESSIAN (CANONICAL ORDER).
C     GPREC            ESTIMATE OF ERROR ON GRADIENT NORM.
C     NEW              CURRENT OBSOLESCENCE COUNTER.
C     MODEL            1 FOR BFGS,  2 FOR POWELL.
C OUTPUT
C     H(*)             NEW HESSIAN (CANONICAL ORDER).
C     NEW              UPDATED OBSOLESCENCE COUNTER.
C ---------------------------------------------------------------------+
      DATA EPS /1D-30/
      DO 10 I=1,N
      V(I)=G(I)-GOLD(I)
   10 S(I)=X(I)-XOLD(I)
      SS=DOT(S,S,N)
      IF (SS.LT.EPS) RETURN
      CALL SUPDOT (T,H,S,N,1)
C     GAUSSIAN DAMPING FROM LACK OF ACCURACY ON GRADIENT.
      ATT=EXP(-GPREC/DOT(V,V,N))
      IF (ATT.LT.0.6D0) NEW=NEW+1
      IF (MODEL.EQ.1) THEN
C
C        BFGS (1970) SCHEME.
         VS=DOT(V,S,N)
         IF (ABS(VS).LT.EPS) RETURN
         TS=DOT(T,S,N)
         IF (ABS(TS).LT.EPS) RETURN
         VS=ATT/VS
         TS=ATT/TS
         K=0
         DO 20 I=1,N
         DO 20 J=1,I
         K=K+1
   20    H(K)=H(K)+V(I)*V(J)*VS-T(I)*T(J)*TS
      ELSE IF (MODEL.EQ.2) THEN
C
C        POWELL (1971) SUPERLINEAR SCHEME.
         SS=1D0/SS
         DO 30 I=1,N
   30    V(I)=V(I)-T(I)
         VS=DOT(V,S,N)*SS
         ATT=SS*ATT
         K=0
         DO 40 I=1,N
         DO 40 J=1,I
         K=K+1
   40    H(K)=H(K)+(V(I)*S(J)+V(J)*S(I)-VS*S(I)*S(J))*ATT
      ENDIF
      END
