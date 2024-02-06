      SUBROUTINE POWELL (METHOD,XVAR,EOPT,GVAR,NVAR) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
C-------------------- 
C     MAIN ROUTINE FOR GRADIENT NORM MINIMIZATION BY THE POWELL METHOD 
C-------------------- 
C     REQUIRED SUBROUTINE : 
C        POWEL1 :THE ORIGINAL POWELL ALGORITHM 
C        DOT,SUPDOT,DIAGIV : MATHEMATICAL PACKAGE 
C        SAVOPT :SAVE/RESTART ROUTINE (THIS SUBROUTINE HAS BEEN REFORMULATED AND
C                RENAMED)                                               GL0492 
C        SVOPTS :SAVE/RESTART ROUTINE 
C-------------------- 
C     THE COMMON/OPTIM/        INCLUDES THE WHOLE DATA REQUIRED. 
C     NOTE...THIS STRUCTURE ALLOWS TO OVERLAY THIS BRANCH (POWEL1,DOT, 
C     SUPDOT,DIAGIV) WITH THOSE OF THE ENERGY AND GRADIENT (COMPFG). 
C     MOREOVER,ONLY THIS SUBROUTINE MUST BE MODIFIED FOR IMPLEMENTATION 
C     IN ANOTHER PACKAGE. 
C-------------------- 
C 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / IWORK(14),ISAVE(1), IDUMMY(19 + 2*NCHAIN -15)    3GL3092 
      COMMON /OPTMCR / AJINV(MAXPAR,MAXPAR),A(MAXPAR,MAXPAR),           3GL3092
     1                 W(MAXPAR*5),B(MAXPAR,MAXPAR),                    3GL3092
     2                 VALU(MAXPAR),RWORK(18),SAVE(MAXPAR),             3GL3092
     3                 RDUMY(MAXHES + MAXPAR*(NCHAIN+9) + NCHAIN + 11)  3GL3092
C     COMMON /OPTIM / IMP,IMP0,LEC,IPRT,AJINV(MAXPAR,MAXPAR) 
C    .               ,A(MAXPAR,MAXPAR),W(MAXPAR*5),B(MAXPAR,MAXPAR) 
C    .               ,VALU(MAXPAR),RWORK(18),IWORK(14),SAVE(MAXPAR) 
C    .               ,ISAVE(1) 
      COMMON /PRECI / SCFCV,SCFTOL,EG(3),ESTIM(3),DUM(3),KDUM(MAXPAR) 
      COMMON /MESAGE/ IFLEP, ISCF                                       DJG0995
      COMMON /SCFOK / FAIL 
C     COMMON /TIME  / TIME0 
      COMMON /TIMECM  / TIME0                                           3GL3092
      DIMENSION XVAR(1),GVAR(1) 
      LOGICAL FAIL,REST,SHOWT,RESTO 
      SAVE
C 
C     TLEFT=3600. 
      TLEFT=TDEF                                                        GL0492
      IF (ITLIMI.NE.0) TLEFT=FITLIM                                     DJG0995
      SHOWT=ITIMES.NE.0                                                 DJG0995
      REST =IRESTA.NE.0                                                 DJG0995
      RESTO=.FALSE. 
C     FOR SAVE/RESTART : EQUIVALENCED /OPTIM/ I1(MAXPAR,LEN1),I2(LEN2) 
C     LEN1=2*(3*MAXPAR+7) 
C     LEN2=19 + 2*(18) 
C
C  LEN1, LEN2, AND LEN3 HAVE BEEN REDEFINED TO REPRESENT THE LENGTH OF THE 
C  COMMON BLOCKS OPTMCR, OPTMCI, AND OPTMCL IN SVOPTS (NEW VERSION OF SAVOPT) 
C
      LEN1=MAXPAR*(3*MAXPAR+7)+18                                       GL0492
      LEN2=15                                                           GL0492
      LEN3=0                                                            GL0492
      IF ( REST ) THEN 
         CALL SVOPTS(LEN1,LEN2,LEN3,.TRUE.)                             GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         CALL SCOPY (NVAR,SAVE,1,XVAR,1) 
         IS=ISAVE(1) 
      ENDIF 
C     FIRST CALL TO COMPFG THUS DEFINING ACCURACY EG(3) ON GRADIENT 
      CALL PORCPU (TIME)                                                GL0492
      CALL COMPFG(XVAR,EOPT,FAIL,GVAR,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      CALL PORCPU (TFLY)                                                GL0492
      TIME=TFLY-TIME 
      IF (IFLEP.NE.12) IFLEP=13                                         GDH0597
C     TAKE STANDARD OPTION    DSTEP  : STEP FOR HESSIAN 
C                             DMAX   : GREATEST STEP LENGTH IN SEARCHES 
C                             MAXFUN : MAXIMUM NUMBER OF GRADIENT CALLS 
C     DSTEP AND DMAX ARE ALSO INFLUENCING THE BEHAVIOR OF THE METHOD... 
C     PLEASE READ THE POWELL PAPER BEFORE HAZARDOUS MODIFICATIONS. 
C                     (REFERENCE IN 'POWEL1') 
      DSTEP=0.03D0 
      DMAX=0.3D0 
      MAXFUN=5*NVAR 
      EPS=1.D0 
      IF(IPRECI.NE.0) THEN                                              DJG0995
         EPS=EPS*0.1D0 
         DSTEP=DSTEP*0.5D0 
         MAXFUN=MAXFUN*2 
      ENDIF 
C     OR OVERREAD WITH SPECIFIED CRITERIA 
      IF(IPRINT.NE.0) IMP=IIPRIN                                        DJG0995
      IF(ICYCLE.NE.0) MAXFUN=IICYCL                                     DJG0995
      IF(IGNORM.NE.0) EPS=ABS(FIGNOR)/SQRT(DBLE(NVAR))                  DJG0995
      EPS=MAX(EPS,EG(1)*SQRT(DBLE(NVAR)))                               GCL0393
      ACC=NVAR*EPS**2 
      WRITE(JOUT,110) NVAR,MAXFUN,EPS 
      WRITE (JOUT,120) EG(1),DSTEP,DMAX 
C     CHECK DATA AND TIMING 
      WRITE(JOUT,170) TIME 
      CALL PORCPU (TFLY)                                                GL0492
      IF(TLEFT-2.D0*TIME.LT.TFLY-TIME0) THEN 
         RMS=SQRT(DOT(GVAR,GVAR,NVAR)/DBLE(NVAR))                       GCL0393
         IF(RMS.GE.EPS) THEN 
            TIMIN=TIME*3.D0 
            TIME=(NVAR+2)*TIME 
            WRITE(JOUT,180) TLEFT,TIME,TIMIN 
            FAIL=.TRUE. 
         ENDIF 
      ENDIF 
      IF (NVAR.LE.1) THEN 
         WRITE (JOUT,130) 
         FAIL=.TRUE. 
      ELSE IF (MAXFUN.LT.NVAR+2 .AND..NOT.REST) THEN 
         WRITE (JOUT,150) NVAR+2 
         FAIL=.TRUE. 
      ENDIF 
      IF (FAIL) THEN 
         WRITE(JOUT,190) 
          ISTOP=1                                                       GDH1095
          IWHERE=153                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF 
C     START OR RESTART THE RUN 
      IF ( REST ) THEN 
         GO TO 4 
      ELSE 
         CALL POWEL1 (A,B,AJINV,XVAR,GVAR,NVAR,EOPT,DSTEP,DMAX,ACC 
     .               ,MAXFUN,IS) 
      ENDIF 
C 
C     ITERATION WITH RESTORE OF DENSITY MATRICES IF SCF DIVERGENCE 
C 
    2 CALL PORCPU (TIME)                                                GL0492
      CALL COMPFG (XVAR,EOPT,FAIL,GVAR,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) GO TO 11 
      RESTO=.FALSE. 
      CALL PORCPU (TFLY)                                                GL0492
      TIME=TFLY-TIME 
      IF(SHOWT) WRITE(JOUT,100) TIME,TFLY-TIME0 
      IF(TLEFT-2.D0*TIME.LT.TFLY-TIME0) THEN 
         CALL SCOPY (NVAR,XVAR,1,SAVE,1) 
         ISAVE(1)=IS 
         CALL SVOPTS(LEN1,LEN2,LEN3,.FALSE.)                            GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         METHOD=1 
         IFLEP=12                                                       DJG0995
         RETURN 
      ENDIF 
    4 CALL POWEL2 (A,B,AJINV,XVAR,GVAR,NVAR,EOPT,DSTEP,DMAX,ACC,MAXFUN 
     .            ,IS) 
      IF (FAIL)    GO TO 10 
      IF (IS.NE.6) GO TO 2 
C 
C     TERMINATION OR ERROR 
C 
    5 CALL COMPFG (XVAR,EOPT,FAIL,GVAR,.FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      METHOD=0 
      IFLEP=11                                                          DJG0995
      IF (.NOT.FAIL) RETURN 
C     2-TIME SCF DIVERGENCE OR POWELL COMPLETION 
   10 WRITE(JOUT,140) IS 
      METHOD=1 
      IFLEP=12                                                          DJG0995
      IF (IS.EQ.6) RETURN 
      CALL POWEL3 (A,B,AJINV,XVAR,GVAR,NVAR,EOPT) 
      GO TO 5 
   11 WRITE(JOUT,160) 
      IF ( RESTO ) THEN 
         IS=6 
         GO TO 10 
      ELSE 
         RESTO=.TRUE. 
         GO TO 2 
      ENDIF 
  100 FORMAT(' ELAPSED TIME IN''POWELL''=',F9.3,'   INTEGRAL=',F10.3, 
     .       ' SECOND') 
  110 FORMAT(/' STATIONARY POINT RESEARCH ... NUMBER OF VARIABLES',I3/ 
     * 6X,'MAXIMUM NUMBER OF GRADIENT CALLS',I6/ 
     *' REQUIRED CONVERGENCE ON RMS GRADIENT',1PD9.1) 
  120 FORMAT(' ERROR ON DERIVATIVES:',1PD10.2,6X,'STEP FOR HESSIAN:', 
     * D10.2,'< ',D10.2) 
  130 FORMAT(' POWELL METHOD REQUIRE AT LEAST TWO VARIABLES ... STOP') 
  140 FORMAT(' S.C.F.OR POWELL DIVERGENCE IN A BRANCH IS=',I2,'  STOP') 
  150 FORMAT(' THE MAXIMUM NUMBER OF CYCLES MUST BE GREATER THAN',I4) 
  160 FORMAT(' *** WARNING FROM ''POWELL'' ***  SCF PROBLEMS...'/ 
     .       ' RESTORE DENSITY MATRICES AND TRY AGAIN') 
  170 FORMAT(' TYPICAL TIME FOR ONE CYCLE OF ''POWELL''=',F9.3) 
  180 FORMAT(' WARNING ... THE ALLOWED TIME (',F10.3,') IS TOO SMALL' 
     ./' FOR A SIGNIFICANT IMPROVEMENT TO BE OBTAINED BY ''POWELL'' .' 
     ./' A MINIMUM VALUE FOR THE ALLOWED TIME SHOULD BE :',F11.3, 
     ./' START AGAIN WITH AT LEAST T=',F10.3) 
  190 FORMAT(/' *****   RUN STOPPED AT THIS POINT.   *****'/) 
      END 
