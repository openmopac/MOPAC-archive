      SUBROUTINE SET (S1,S2,NA,NB,RAB,NBOND,II,*)                       CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /HPUSED/ HPUSED                                            LF1010
      LOGICAL HPUSED                                                    LF1010
      COMMON /SETC/ A(7),B(7),SA,SB,FACTOR,ISP,IPS
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1010
      DIMENSION TEMP(20)
      DIMENSION FACT(17)
CSAV         SAVE                                                           GL0892
      DATA FACT/1.D0,2.D0,6.D0,24.D0,120.D0,720.D0,5040.D0,40320.D0,
     1362880.D0,3628800.D0,39916800.D0,479001600.D0,6227020800.D0,
     28.71782912D10,1.307674368D12,2.092278989D13,3.556874281D14/
C***********************************************************************
C
C     SET IS PART OF THE OVERLAP CALCULATION, CALLED BY OVERLP.
C
C***********************************************************************
      IF (NA.GT.NB) then
        ISP=2
        IPS=1
        SA=S2
        SB=S1
      else
        ISP=1
        IPS=2
        SA=S1
        SB=S2
      endif
      J=II+2
      IF (II.GT.3) J=J-1
      IF (PMODS(1)) THEN                                                PZ1010
C        Recall that Hp hydrogen atoms are assigned atomic #1 when calling this subroutine
C        from DIAT2, whereas everywhere else they are assigned atomic #9.
         IF (NA.EQ.1.AND.NB.EQ.1) THEN                                  PZ1010
            SA=PMODVL(5)                                                PZ1010
            SB=PMODVL(5)                                                PZ1010
         ENDIF                                                          PZ1010
         IF (NA.EQ.8.AND.NB.EQ.8) THEN                                  PZ1010
            SA=PMODVL(6)                                                PZ1010
            SB=PMODVL(6)                                                PZ1010
         ENDIF                                                          PZ1010
      ENDIF                                                             PZ1010
      ALPHA=0.5D00*RAB*(SA+SB)
      BETA=0.5D00*RAB*(SB-SA)
      JCALL=J-1
      c = exp(-alpha)
      a(1) = c/alpha
      do 500 i=1,jcall
      a(i+1) = (a(i)*i+c)/alpha
500   continue
c------------------
c---- call BINTGS(beta,jcall)
c------------------
      ABSX = ABS(beta)
      IF (ABSX.GT.3.D00) GO TO 40
      IF (ABSX.LE.2.D00) GO TO 10
      IF (jcall.LE.10) GO TO 40
      LAST=15
      GO TO 60
   10 IF (ABSX.LE.1.D00) GO TO 20
      IF (jcall.LE.7) GO TO 40
      LAST=12
      GO TO 60
   20 IF (ABSX.LE.0.5D00) GO TO 30
      IF (jcall.le.5) GO TO 40
      LAST=7
      GO TO 60
   30 IF (ABSX.LE.1.D-6) GOTO 90
      LAST=6
      GO TO 60
   40 EXPX=EXP(beta)
      EXPMX=1.D00/EXPX
      xinv = 1.0/beta
      b(1) = (expx - expmx)*xinv
      zk = 1.0
      DO 50 I=1,jcall
      zk = -1.0*zk
      b(i+1) = (i*b(i) + zk*expx - expmx)*xinv
50    continue
      return
   60 DO 80 I=0,jcall
          zk = -beta
          y = 2.0*mod(i+1,2)/(i+1)
          do 70 m=1,last
          y = y + 2.0*zk*mod(m+i+1,2)/(fact(m)*(m+i+1))
          zk = -beta*zk
70        continue
   80 B(I+1)=Y
      return
   90 DO 100 I=0,jcall
  100 B(I+1)=(2*MOD(I+1,2))/(I+1.D0)
c--------------
      RETURN
C
 9999 RETURN 1                                                          CSTP
      END
