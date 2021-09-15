      SUBROUTINE SET (S1,S2,NA,NB,RAB,NBOND,II)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /SETC/ A(7),B(7),SA,SB,FACTOR,ISP,IPS
C***********************************************************************
C
C     SET IS PART OF THE OVERLAP CALCULATION, CALLED BY OVERLP.
C         IT CALLS AINTGS AND BINTGS
C
C***********************************************************************
      IF (NA.GT.NB) GO TO 10
      ISP=1
      IPS=2
      SA=S1
      SB=S2
      GOTO 20
   10 ISP=2
      IPS=1
      SA=S2
      SB=S1
   20 J=II+2
      IF (II.GT.3) J=J-1
      ALPHA=0.5D00*RAB*(SA+SB)
      BETA=0.5D00*RAB*(SB-SA)
      JCALL=J-1
      CALL AINTGS (ALPHA,JCALL)
      CALL BINTGS (BETA,JCALL)
      RETURN
C
      END
