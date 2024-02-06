      SUBROUTINE RABIOT (X,F,N,BARAT) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C 
C----------------------------------------------------------------------- 
C 
C     CHAIN ... INSERTION OF A POINT IN THE CHAIN 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / ISTAB,IJUMP,IROTH,NVAR,ITEG,IBRCH,               3GL3092
     1                 INDI(NCHAIN,2),NP1,ITE,KK,JVOIS,ITEST,MM,        3GL3092
     2                 ITETOT,NBID,LIMIT,IS,NLR(2),NT                   3GL3092
      COMMON /OPTMCL / FAIL,IBID,FLAG,FLACHN(NCHAIN+1)                  GCL0892
      COMMON /OPTMCR / H(MAXHES),GNEW(MAXPAR),XNEW(MAXPAR),ENEW,        3GL3092
     1                GOLD(MAXPAR),XOLD(MAXPAR),EOLD,QDER,DMAX,         3GL3092
     2                COSDIR(MAXPAR,2),TRANS1(MAXPAR),HAUT(MAXPAR+2),   3GL3092
     3                DIR(MAXPAR),EIGEN(MAXPAR),R(2),EPS1,COSTET,Q,D,   3GL3092
     4                D1,D2,D3,D4,D5,D6,D7,D8,D9,CORREL,PIGI,GIGI,      3GL3092
     5                XVAR(MAXPAR,MAXPAR),GVAR(MAXPAR,MAXPAR),          3GL3092 
     6                HVEC(MAXPAR,4),THR1,THR2,COORD(MAXPAR+1,NCHAIN),  3GL3092 
     7                XOUT(MAXPAR),FOUT,G(MAXPAR),EPS,DELTAE,           3GL3092 
     8                RDUMY(MAXPAR*MAXPAR)                              3GL3092
C     COMMON /OPTIM/ IMP,IMP0,LEC,IPRT,H(MAXHES),GNEW(MAXPAR) 
C    .              ,XNEW(MAXPAR),ENEW,GOLD(MAXPAR),XOLD(MAXPAR),EOLD 
C    .              ,QDER,DMAX,COSDIR(MAXPAR,2),TRANS1(MAXPAR) 
C    .              ,HAUT(MAXPAR+2),DIR(MAXPAR),EIGEN(MAXPAR),R(2),EPS1 
C    .              ,COSTET,Q,D,D1,D2,D3,D4,D5,D6,D7,D8,D9,CORREL,PIGI 
C    .              ,GIGI,XVAR(MAXPAR,MAXPAR),GVAR(MAXPAR,MAXPAR) 
C    .              ,HVEC(MAXPAR,4),THR1,THR2,COORD(MAXPAR+1,NCHAIN) 
C    .              ,XOUT(MAXPAR),FOUT,G(MAXPAR),EPS,DELTAE 
C    .              ,ISTAB,IJUMP,IROTH,NVAR,ITEG,IBRCH,INDI(NCHAIN,2) 
C    .              ,NP1,ITE,KK,JVOIS,ITEST,MM,ITETOT,NBID 
C    .              ,LIMIT,IS,NLR(2),NT,FAIL,IBID,FLAG,FLACHN(NCHAIN) 
      LOGICAL FAIL,IBID,FLAG,FLACHN 
      DIMENSION IT1(2),X(1)
      CHARACTER*6 BARAT(2)                                              GCL0393
       SAVE
      NLR(JVOIS)=NLR(JVOIS)+1 
      IF (NLR(1)+NLR(2).GT.NT) GO TO 120 
      DO 110 JJ=2,NT 
      IF (FLACHN(JJ)) THEN 
         INDI(NLR(JVOIS),JVOIS)=JJ 
         FLACHN(JJ)=.FALSE. 
         DO 100 I=1,N 
  100    COORD(I,JJ) = X(I) 
         COORD (NP1,JJ) = F 
         IF(IMP.GT.0) WRITE(JOUT,1010) BARAT(JVOIS),F 
         RETURN 
      ENDIF 
  110 CONTINUE 
  120 WRITE (JOUT,1000) NLR,NT 
      FAIL=.TRUE. 
      RETURN 
 1000 FORMAT ('0NB OF POINTS ON THE LEFT :',I3,/ 
     $        ' NB OF POINTS ON THE RIGHT :',I3,/ 
     $        ' STOP : THE CHAIN IS TOO LONG >',I3) 
 1010 FORMAT(' INSERTION ON THE',A6,'  ENERGY=',1PD13.5) 
C 
C----------------------------------------------------------------------- 
      ENTRY PTHAUT 
C     CHAIN ... SEARCH FOR THE HIGHEST POINT AND UPDATE ASSOCIATED 
C     INDEXES .ISTAB=1 MEANS THAT THE HIGHEST POINT REMAIN AT THE SAME 
C     INDEX , THUS ALLOWING QUADRATIC DIRECTION . 
C     THE HIGHEST POINT COORDINATES ARE RETURNED IN HAUT AND XOUT 
C 
      EREF = HAUT (NP1) 
      DO 220 J=1,2 
      IT1(J) = 0 
      KKFIN = NLR(J) 
      IF(KKFIN.LE.1) GO TO 220 
      DO 210 K=KKFIN,2,-1 
      IF (COORD(NP1,INDI(K,J)).LE.EREF) GO TO 210 
      EREF = COORD(NP1,INDI(K,J)) 
      IT1(J) = K 
  210 CONTINUE 
  220 CONTINUE 
      IF(IT1(1).NE.0.OR.IT1(2).NE.0) GO TO 225 
      ISTAB=1 
      DO 221 I=1,NP1 
  221 XOUT(I)=HAUT(I) 
      RETURN 
C 
C     SHIFTING MUST BE DONE 
  225 J=2 
      ISTAB=0 
      IF (IT1(2).EQ.0) J=1 
      IT = IT1(J) 
      K=3-J 
      NLR(K) = NLR(K)+1 
      JL = INDI(IT,J) 
      INDI(NLR(K),K)=JL 
      DO 230 I=1,NP1 
      XOUT(I)=COORD(I,JL) 
      COORD(I,JL) = HAUT(I) 
  230 HAUT(I)=XOUT(I) 
      IF (IT.EQ.NLR(J)) GO TO 250 
      DO 240 I=NLR(J),IT+1,-1 
      NLR(K)=NLR(K)+1 
  240 INDI(NLR(K),K)=INDI(I,J) 
  250 NLR(J)=IT-1 
      RETURN 
C 
C----------------------------------------------------------------------- 
      ENTRY VOISIN 
C     CHAIN ... DIRECTIONAL COSINES OF THE NEIGHBOURS AND DISTANCES 
C 
      DO 320 J=1,2 
      K = INDI(NLR(J),J) 
      DO 310 I=1,NBID 
  310 COSDIR(I,J) = HAUT(I) - COORD(I,K) 
      R(J)=SQRT(DOT(COSDIR(1,J),COSDIR(1,J),NBID)) 
      IF(R(J).GT.DMAX*1.D-6) GO TO 315 
      WRITE(JOUT,1020) J,R(J) 
      R(J)=DMAX*1.D-6 
  315 DO 320 I=1,NBID 
  320 COSDIR(I,J) = COSDIR(I,J)/R(J) 
      RETURN 
 1020 FORMAT(' WARNING : CONTIGUOUS LINK',I2,' OF ',1PD11.3,'LENGTH ') 
      END 
