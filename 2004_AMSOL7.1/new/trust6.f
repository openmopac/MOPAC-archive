C======================================================================+
      SUBROUTINE TRUST6 (N,FLAG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'FFILES.i'                                                DJG0496
C ---------------------------------------------------------------------+
C     TRUST REGION METHODS. SAVE/RESTART DEDICATED ROUTINE.
C                                                       DL JANUARY 1996
C  INPUT
C      N   :  NUMBER OF OPTIMIZED VARIABLES.
C      FLAG:  .TRUE. FOR A RESTART, .FALSE. FOR A SAVE.
C ---------------------------------------------------------------------+
      COMMON /OPTIMI/ IMP,IMP0
     1       /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
     2       /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     3                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     4                NCLOSE,NOPEN,NDUMY
     5       /ERRFN / ERRFN(MAXPAR)
      COMMON /OPTMCR/ H(MAXHES),V(MAXPAR**2),B(MAXPAR)
     1               ,XOLD(MAXPAR),GOLD(MAXPAR),POLD(MAXPAR),Q(MAXPAR)
     2               ,RADIUS,RHOI,RHOS,GNORM,STEP(0:10),COST(0:10)
     3               ,FOST(0:10),GOST(MAXPAR,0:10)
     4               ,HNEW(MAXHES),BNEW(MAXPAR),VNEW(MAXPAR**2)
     5               ,FBEST,XBEST(MAXPAR),GBEST(MAXPAR)
     6       /OPTMCI/ NEW,KMODE,KSTOP,LM(0:10),MINDEX
     7       /OPTMCL/ LGRAD
      LOGICAL LGRAD,FLAG
      OPEN(UNIT=9,STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(UNIT=10,STATUS='UNKNOWN',FORM='UNFORMATTED')
      REWIND 9
      REWIND 10
      IR=9
      LINEAR=N*(N+1)/2
      LL=N*N
      NINEAR=NORBS*(NORBS+1)/2
      IF (FLAG) GOTO 100
C
C     SAVE
C
      WRITE(JOUT,'(//10X,''- - - - - - - TIME UP - - - - - - -''/)')
      WRITE(JOUT,'(/10X,'' - THE CALCULATION IS BEING DUMPED TO DISK'',
     ./10X,''RESTART IT USING THE MAGIC WORD "RESTART"'')')
C     TRUST DATA
      WRITE(IR)(H(I),I=1,LINEAR),(V(I),I=1,LL),(B(I),I=1,N)
      WRITE(IR)(XOLD(I),I=1,N),(GOLD(I),I=1,N),(POLD(I),I=1,N),
     .(Q(I),I=1,N),RADIUS,RHOI,RHOS
      WRITE(IR)NEW,KMODE,KSTOP,MINDEX
C     AMSOL STUFF
      WRITE(IR)(ERRFN(I),I=1,N)
      CLOSE (IR)
C     DENSITY MATRICES
      WRITE(10)(PA(I),I=1,NINEAR)
      IF(NALPHA.NE.0)WRITE(10)(PB(I),I=1,NINEAR)
      CLOSE (10)
      STOP
C
C     RESTART
C
  100 WRITE(JOUT,'(//10X,'' RESTORING DATA FROM DISK''/)')
C     TRUST DATA
      READ (IR)(H(I),I=1,LINEAR),(V(I),I=1,LL),(B(I),I=1,N)
      READ (IR)(XOLD(I),I=1,N),(GOLD(I),I=1,N),(POLD(I),I=1,N),
     .(Q(I),I=1,N),RADIUS,RHOI,RHOS
      READ (IR)NEW,KMODE,KSTOP,MINDEX
C     AMSOL STUFF
      READ (IR)(ERRFN(I),I=1,N)
      CLOSE (IR)
C     DENSITY MATRICES WILL BE READ IN ROUTINE "ITER".
      END
