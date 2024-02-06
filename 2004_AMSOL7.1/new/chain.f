      SUBROUTINE CHAIN (IND,XX,FF,GG,NN) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
C---------- 
C     TRANSITION STATE BY THE CHAIN METHOD ... MAIN SUBROUTINE . 
C---------- 
C     REQUIRED SUBROUTINES : 
C        COMPFG (X,F,FAIL,G,ASKG) : ENERGY (F) AT POINT (X)... 
C                                   AND GRADIENT (G) IF ASKG=.TRUE. 
C        COL ,ECRIT,QUADRI,RABIOT : CHAIN METHOD. 
C        DOT,SUPDOT,DIAGIV : MATHEMATICAL PACKAGE. 
C---------- 
C     THE COMMON/OPTIM/        INCLUDES THE WHOLE DATA REQUIRED. 
C     NOTE...THIS STRUCTURE ALLOWS TO OVERLAY THIS BRANCH (COL,QUADRI, 
C     RABIOT,DIAGIV,ECRIT,DOT,SUPDOT) WITH THOSE OF THE ENERGY AND 
C     GRADIENT COMPUTATION (COMPFG) 
C     MOREOVER,ONLY THIS SUBROUTINE MUST BE MODIFIED FOR IMPLEMENTATION 
C     OF THE ALGORITHM IN AN OTHER PACKAGE . 
C---------- 
C 
      DIMENSION XX(1),GG(1) 
      COMMON /PRECI / SCFCV,SCFTOL,BIDPRE(9),KTYP(MAXPAR) 
      COMMON /MESAGE/ IFLEP, ISCF                                       DJG0995
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / ISTAB,IJUMP,IROTH,NVAR,ITEG,IBRCH,               3GL3092
     1                 INDI(NCHAIN,2),NP1,ITE,KK,JVOIS,ITEST,MM,        3GL3092
     2                 ITETOT,N,LIMIT,IS,NLR(2),NT                      3GL3092
      COMMON /OPTMCL / FAIL,IBID,FLAG,FLACHN(NCHAIN+1)                  GCL0892
      COMMON /OPTMCR / H(MAXHES),GNEW(MAXPAR),XNEW(MAXPAR),ENEW,        3GL3092
     1                GOLD(MAXPAR),XOLD(MAXPAR),EOLD,QDER,DMAX,         3GL3092
     2                COSDIR(MAXPAR,2),TRANS1(MAXPAR),HAUT(MAXPAR+2),   3GL3092
     3                DIR(MAXPAR),EIGEN(MAXPAR),R(2),EPS1,COSTET,Q,D,   3GL3092
     4                D1,D2,D3,D4,D5,D6,D7,D8,D9,CORREL,PIGI,GIGI,      3GL3092
     5                XVAR(MAXPAR,MAXPAR),GVAR(MAXPAR,MAXPAR),          3GL3092 
     6                HVEC(MAXPAR,4),THR1,THR2,COORD(MAXPAR+1,NCHAIN),  3GL3092 
     7                X(MAXPAR),F,G(MAXPAR),EPS,DELTAE,                 3GL3092 
     8                RDUMY(MAXPAR*MAXPAR)                              3GL3092
      COMMON /SCRCHR/ GEO(3,NUMATM),DUMY(NUMATM),                       3GL3092
     1                RDUMMY(6*MAXPAR**2+1-4*NUMATM)                    GCL0393
      COMMON /SCRCHI/ IDUM1(NUMATM), IDUM2(3,NUMATM)                    3GL3092
C
      COMMON /GEOKST/ NATOMS, LDUM(4*NUMATM)                            3GL3092 
C     COMMON /GEOVAR/ NDUMM,LOC(2,MAXPAR) 
      COMMON /GEOVAR/ XPARAM(MAXPAR),NDUMM,LOC(2,MAXPAR),IDUMY          3GL3092
C     COMMON /TIME  / TIME0 
      COMMON /TIMECM  / TIME0                                           3GL3092
      LOGICAL FAIL,FAIL2,IBID,FLAG,FLACHN,SHOWT 
       SAVE
C 
C     STANDARD PARAMETERS AND INITIALIZATION 
      NT=NCHAIN 
      COSTET=30.D0 
      DMAX=0.3D0 
      N=NN 
C     'LEN1&2' ARE THE LENGTH OF /OPTIM/ TO BE SAVED WHEN TIME UP. 
C     (IN INTEGER*4 UNIT,TOTAL LENGTH=MAXPAR*LENGT1+LENGT2) 
C     ACTUAL TOTAL LENGTH=26+NCHAIN*3    + 
C     2*( 29+2*MAXPAR*MAXPAR+MAXHES+16*MAXPAR+NCHAIN*(MAXPAR+1) ) 
C     LEN1=2*(MAXPAR*2+16+NCHAIN) 
C     LEN2=NCHAIN*3+26 +2*(29+MAXHES+NCHAIN) 
C
C  LEN1, LEN2, AND LEN3 HAVE BEEN REDEFINED TO REPRESENT THE LENGTH OF THE 
C  COMMON BLOCKS OPTMCR, OPTMCI, AND OPTMCL IN SVOPTS (NEW VERSION OF SAVOPT) 
C
      LEN1= MAXHES+MAXPAR*(2*MAXPAR+NCHAIN+16)+29+NCHAIN                GL0492
      LEN2= 2*NCHAIN+19                                                 GL0492
      LEN3= NCHAIN+3                                                    GL0492
C
      SHOWT=ITIMES .NE. 0                                               DJG0995
C     TLEFT=3600. 
      TLEFT=TDEF                                                        GL0492
      TIME1 =TIME0 
      IF (ITLIMI.NE.0) TLEFT=FITLIM                                     DJG0995
      WRITE(JOUT,110) TLEFT 
      IF (IRESTA .NE. 0) THEN                                           DJG0995
         CALL SVOPTS(LEN1,LEN2,LEN3,.TRUE.)                             GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         GO TO 5 
      ENDIF 
      DO 1 I=1,N 
    1 X(I)=XX(I) 
      INDI(1,1)=1 
      INDI(1,2)=NT 
      NP1=N+1 
C     COORDINATES AND ENERGY OF LEFT AND RIGHT MINIMA 
      DO 4 K=1,2 
      JPOS=INDI(1,K) 
C     READ COORDINATES (ANGSTROM AND DEGREES) 
      CALL GETGEO(JDAT,IDUM1,GEO,IDUM2,IDUM1,IDUM1,IDUM1,DUMY 
     1           ,NATOMS,IBID) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      DO 2 I=1,N 
    2 COORD(I,JPOS)=GEO(LOC(2,I),LOC(1,I)) 
C     CONVERT IN ANGSTROM AND RADIAN 
      DO 3 I=1,N 
      IF(KTYP(I).GT.1) COORD(I,JPOS)=COORD(I,JPOS)/57.29577951D0 
    3 CONTINUE 
C     CALCULATE THE ENERGY OF THIS MINIMA 
      CALL COMPFG (COORD(1,JPOS),COORD(NP1,JPOS),FAIL,G,.FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
    4 FAIL=.TRUE. 
      IS=0 
C 
C     STANDARD THRESHOLDS ON CONVERGENCE AND ACCURACY 
C 
    5 DELTAE=MAX(1.D2*SCFCV,0.1D0) 
      EPS=5.D0 
      LIMIT=30+3*N 
      IF(IPRECI .NE. 0) THEN                                            DJG0995
         EPS=EPS*0.1D0 
         LIMIT=LIMIT*10 
      ENDIF 
      IF (ICYCLE.NE.0) LIMIT=IICYCL                                     DJG0995
      IF (IGNORM.NE.0) EPS=ABS(FIGNOR)                                  DJG0995
C 
C     ITERATIONS 
C 
   10 CALL COL 
C     NOTE ... IF FAIL=.T. THEN SCF TO BE INITIALISED WITH STANDARD 
C                 DIAGONAL DENSITY MATRICES (CF SUBROUTINE 'ITER'). 
C              FAIL=.T. ON COMPFG RETURN IIF ACTUAL DIVERGENCE. 
C              PLEASE, LOOK AT THE DISPATCHING ACCORDING TO 'IS' IN COL. 
      GO TO (11,11,13,21,20,30,30,21,30),IS 
   11 FAIL=.TRUE. 
      GO TO 21 
   13 FAIL=ISTAB.NE.1 
      CALL PORCPU (TIME2)                                               GL0492
      TIME=TIME2-TIME1 
      TIME1=TIME2 
   20 SCFTOL=1.D0 
      IF(SHOWT)WRITE(JOUT,100)TIME,TIME2-TIME0 
      CALL COMPFG(X,F,FAIL,G,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF(TLEFT-2.5D0*TIME.LT.TIME2-TIME0) THEN 
         CALL SVOPTS(LEN1,LEN2,LEN3,.FALSE.)                            GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         FAIL=.TRUE. 
         GO TO 30 
      ELSE 
         GO TO 10 
      ENDIF 
   21 SCFTOL=1.D2 
      CALL COMPFG (X,F,FAIL,G,.FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      GO TO 10 
C     TERMINATION 
   30 DO 31 I=1,N 
      XX(I)=X(I) 
   31 GG(I)=G(I) 
      SCFTOL=1.D0 
      FAIL2=.FALSE. 
      CALL COMPFG(X,FF,FAIL2,G,.FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IND=0 
      IFLEP=11                                                          DJG0995
      IF(FAIL.OR.FAIL2) THEN 
         IND=1 
         IFLEP=12                                                       DJG0995 
      ENDIF 
      RETURN 
  100 FORMAT(' ELAPSED TIME IN CHAIN METHOD =',F9.3,'  INTEGRAL =',F10.3 
     .      ,' SECOND') 
  110 FORMAT(' TOTAL TIME ALLOWED FOR THIS RUN :',F10.2,' SECONDS') 
      END 
