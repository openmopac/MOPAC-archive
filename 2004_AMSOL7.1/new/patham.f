      SUBROUTINE PATHAM (IND,X,F,G,N) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1295
C
C   This routine is the PATH routine in the original AMPAC 2.1
C--------------- 
C     MAIN ROUTINE TO FOLLOW A REACTION PATH (EG A -GRADIENT TRAJECTORY) 
C     STARTING (OR NOT) FROM A SADDLE. 
C--------------- 
C     REQUIRED SUBROUTINES : 
C        PATH1 : SELF STARTING MIXED INTEGRATOR 
C        PATH2 : INTERPOLATION DEDICATED TO PATH1 
C        DOT,SUPDOT,SCOPY,MXM,DIAGIV : MATHEMATICAL PACKAGE 
C        READVT: READ A VECTOR OF DATA 
C        SAVOPT: SAVE/RESTART ROUTINE (THIS ROUTINE HAS BEEN REFORMULATED AND
C                REMOVED)                                               GL0492
C        SVOPTS: SAVE/RESTART ROUTINE 
C--------------- 
C     THE COMMON/OPTIM/        INCLUDES THE WHOLE DATA REQUIRED... 
C     NOTE...THIS STRUCTURE ALLOWS TO OVERLAY THIS BRANCH (PATH1,PATH2, 
C     DOT,SUPDOT,DIAGIV) WITH THOSE OF THE ENERGY AND GRADIENT (COMPFG) 
C     MOREOVER,ONLY THIS SUBROUTINE MUST BE MODIFIED FOR IMPLEMENTATION 
C     OF THE ALGORITHM IN ANOTHER PACKAGE . 
C--------------- 
C 
      COMMON /PRECI / SCFCV,SCFTOL,EG(3),ESTIM(3),PMAX(3),KTYP(MAXPAR) 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / ICALL,ITERAT,IS,IPRIN,ITE,NBOUND,NBOULO,         DJG0995
     1                 IMETH, IDUMMY(19 + 2*NCHAIN - 8)                 3GL3092
      COMMON /OPTMCL / KONV,HUPDAT,RESET,INTERP,LDUMMY(NCHAIN)          GCL0892
      COMMON /OPTMCR / HESSE(MAXHES),P(MAXPAR,MAXPAR),DY2(MAXPAR),      3GL3092
     1                 POND(MAXPAR),D(MAXPAR),XOLD(MAXPAR),             3GL3092
     2                 TVOLD(MAXPAR),Y1(MAXPAR,2),Y2(MAXPAR,2),         3GL3092
     3                 Y3(MAXPAR,2),C(MAXPAR,4),Y4(MAXPAR,2),           3GL3092
     4                 Y(MAXPAR,2),YE(MAXPAR),DX(MAXPAR),               3GL3092
     5                 HPREC,HMIN,H,HMAX,HTOT,DHTOT,RMS,EOLD,CRITE,     3GL3092
     6                 Y1NORM,Y2NORM,Y3NORM,ETOL,TOLE,SAVE(MAXPAR),     3GL3092
     7                 RDUMY(MAXPAR*(2*MAXPAR + NCHAIN + 16 - 22) +     3GL3092
     8                       29 + NCHAIN - 14)                          3GL3092
C     COMMON /OPTIM / IMP,IMP0,LEC,IPRT,HESSE(MAXHES),P(MAXPAR,MAXPAR) 
C    .               ,DY2(MAXPAR),POND(MAXPAR),D(MAXPAR),XOLD(MAXPAR) 
C    .               ,TVOLD(MAXPAR),Y1(MAXPAR,2),Y2(MAXPAR,2) 
C    .               ,Y3(MAXPAR,2),C(MAXPAR,4),Y4(MAXPAR,2) 
C    .               ,Y(MAXPAR,2),YE(MAXPAR),DX(MAXPAR) 
C    .               ,HPREC,HMIN,H,HMAX,HTOT,DHTOT,RMS,EOLD,CRITE 
C    .               ,Y1NORM,Y2NORM,Y3NORM,ETOL,TOLE 
C    .               ,ICALL,ITERAT,IS,IPRINT,ITE,NBOUND,NBOULO 
C    .               ,KONV,HUPDAT,RESET,INTERP 
C    .               ,IMETH,SAVE(MAXPAR) 
C     COMMON /TIME  / TIME0 
      COMMON /TIMECM  / TIME0                                           3GL3092
      COMMON /MESAGE/ IFLEP, ISCF                                       DJG0995
      DIMENSION X(N),G(N) 
      LOGICAL KONV,HUPDAT,RESET,INTERP,FAIL,REST,SHOWT,RESTO 
      LOGICAL LDUMMY                                                    3GL3092
      SAVE
C 
C     TLEFT=3600. 
      TLEFT=TDEF                                                        GL0492
      IF(ITLIMI.NE.0) TLEFT=FITLIM                                      DJG0995
      SHOWT=ITIMES.NE.0                                                 DJG0995
      REST= IRESTA.NE.0                                                 DJG0995
      RESTO=.FALSE. 
C     FOR SAVE/RESTART : EQUIVALENCED /OPTIM/ I1(MAXPAR,LEN1),I2(LEN2) 
C     LEN1=2*(MAXPAR+22) 
C     LEN2=2*(MAXHES+14) + 16 
C
C  LEN1, LEN2, AND LEN3 HAVE BEEN REDEFINED TO REPRESENT THE LENGTH OF THE 
C  COMMON BLOCKS OPTMCR, OPTMCI, AND OPTMCL IN SVOPTS (NEW VERSION OF SAVOPT) 
C
      LEN1=MAXHES+MAXPAR*(MAXPAR+22)+14                                 GL0492
      LEN2=8                                                            GL0492
      LEN3=4                                                            GL0492
      IF ( REST ) THEN 
         CALL SVOPTS(LEN1,LEN2,LEN3,.TRUE.)                             GL0492
         CALL SCOPY (N,SAVE,1,X,1) 
      ENDIF 
C    ... 
C     FIRST CALL TO GRADIENT, THUS DEFINING THE ACCURACY DATA IN /PRECI/ 
      CALL PORCPU (TIME)                                                GL0492
      CALL COMPFG(X,F,FAIL,G,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
C     ... 
C     READ OPTIONS AND TAKE DEFAULT VALUES 
C     ... 
      LIMIT=100 
      EPS=5.D0/SQRT(DBLE(N))                                            GCL0393
      HMIN=0.04D0 
      ETOL=SCFCV*1.D1 
      TOLE=F+10.D0 
      IF(IPRECI.NE.0) THEN                                              DJG0995
         EPS=EPS*0.1D0 
         LIMIT=LIMIT*10 
         HMIN=HMIN*0.1D0 
      ENDIF 
      HPREC=HMIN*0.50D0 
      HMAX=HMIN*1.D1 
C     OVERWRITE DEFAULT VALUES IF PROVIDED 
      IF(ICYCLE.NE.0) LIMIT=IICYCL                                      DJG0995
      IF(IPRINT.NE.0) IMP=IIPRIN                                        DJG0995
      IF(IGNORM.NE.0) EPS=ABS(FIGNOR)/SQRT(DBLE(N))                     DJG0995
      IF ( REST ) GO TO 40 
      KONV=.TRUE. 
      IF(IWEIGH.EQ.0) THEN                                              DJG0995
C     IF NO WEIGHT ARE PROVIDED,SET EACH WEIGHT=1. 
         DO 2 I=1,N 
    2    POND(I)=1.D0 
      ELSE 
         CALL READVT (JDAT,JOUT,POND,N) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      ENDIF 
      DO 3 I=1,N 
      J=KTYP(I) 
      KONV=POND(I).GT.0.D0.AND.KONV 
    3 C(I,1)=EG(J) 
      EPS=MAX(EPS,3.D0*SQRT(DOT(C,C,N)/DBLE(N)))                        GCL0393
      IF(ITV.EQ.0) THEN                                                 DJG0995
C        IF NO TRANSITION VECTOR IS PROVIDED, 
C        SELECT THE WEIGTHED STEEPEST DESCENT. 
         DO 4 I=1,N 
    4    YE(I)=-G(I)/POND(I) 
      ELSE 
         CALL READVT (JDAT,JOUT,YE,N) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      ENDIF 
      DO 5 I=1,N 
    5 YE(I)=YE(I)/POND(I) 
      TVNORM=1.D0/SQRT(DOT(YE,YE,N)) 
      DO 6 I=1,N 
      DY2(I)=3.D0*C(I,1)/POND(I) 
    6 YE(I)=YE(I)*TVNORM 
      IMETH=8 
      GO TO 30 
C 
C     ITERATION WITH RESTORE OF DENSITY MATRICES IF SCF DIVERGENCE 
C 
   20 CALL PORCPU (TIME)                                                GL0492
      CALL COMPFG(X,F,FAIL,G,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
   30 IF(FAIL) GO TO 50 
      RESTO=.FALSE. 
      CALL PORCPU (TFLY)                                                GL0492
      TIME=TFLY-TIME 
      IF(SHOWT) WRITE(JOUT,110) TIME,TFLY-TIME0 
      IF(TLEFT-2.D0*TIME.LT.TFLY-TIME0) THEN 
         WRITE(JOUT,120) TIME 
         CALL SCOPY (N,X,1,SAVE,1) 
         CALL SVOPTS(LEN1,LEN2,LEN3,.FALSE.)                            GL0492
         IMETH=21 
         GO TO 41 
      ENDIF 
   40 CALL PATH1   (X,F,G,N,EPS,LIMIT,IMETH,P) 
      IF(IMETH.LT.20) GO TO 20 
   41 IF(IMETH.EQ.20) THEN 
         IND=1 
         IFLEP=11                                                       DJG0995
      ELSE 
         IND=0 
         IFLEP=12                                                       DJG0995 
      ENDIF 
      RETURN 
   50 WRITE(JOUT,100) 
      IF (RESTO) THEN 
         IMETH=21 
         GO TO 41 
      ELSE 
         RESTO=.TRUE. 
         GO TO 20 
      ENDIF 
  100 FORMAT(' *** WARNING FROM ''PATH'' *** SCF PROBLEMS...'/ 
     .       ' RESTORE DENSITY MATRICES AND TRY AGAIN') 
  110 FORMAT(' ELAPSED TIME IN ''PATH'' =',F9.3,'   INTEGRAL=',F10.3, 
     .       ' SECOND') 
  120 FORMAT(' TYPICAL TIME FOR ONE CYCLE OF ''PATH''=',F9.3) 
      RETURN                                                            GDH0495
      END 
