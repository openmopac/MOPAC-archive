      SUBROUTINE POWSQ(XPARAM, NVAR, FUNCT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
       EXTERNAL SDOT                                                    GL0492
      DIMENSION XPARAM(*)
**********************************************************************
*
*   POWSQ OPTIMISES THE GEOMETRY BY MINIMISING THE GRADIENT NORM.
*         THUS BOTH GROUND AND TRANSITION STATE GEOMETRIES CAN BE
*         CALCULATED. IT IS ROUGHLY EQUIVALENT TO FLEPO, FLEPO MINIMISES
*         THE ENERGY, POWSQ MINIMISES THE GRADIENT NORM.
*
*  ON ENTRY XPARAM = VALUES OF PARAMETERS TO BE OPTIMISED.
*           NVAR   = NUMBER OF PARAMETERS TO BE OPTIMISED.
*
*  ON EXIT  XPARAM = OPTIMISED PARAMETERS.
*           FUNCT  = HEAT OF FORMATION IN KCALS.
*
**********************************************************************
C        *****  ROUTINE PERFORMS  A LEAST SQUARES MINIMIZATION  *****
C        *****  OF A FUNCTION WHICH IS A SUM OF SQUARES.        *****
C        *****  INITIALLY WRITTEN BY J.W. MCIVER JR. AT SUNY/   *****
C        *****  BUFFALO, SUMMER 1971.  REWRITTEN AND MODIFIED   *****
C        *****  BY A.K. AT SUNY BUFFALO AND THE UNIVERSITY OF   *****
C        *****  TEXAS.  DECEMBER 1973                           *****
C        HOUSE-KEEPING BY D.L. (DEWAR GROUP, JULY 1985)
C
      COMMON /LAST  / LAST
C     COMMON /TIME  / TIME0
      COMMON /TIMECM  / TIME0                                           3GL3092
      COMMON /GRADNT/ GRAD(MAXPAR),GNFIN
      COMMON /MESAGE/ IFLEP,ISCF                                        DJG0995
      COMMON /PRECI / SCFCV,SCFTOL,EG(3),DUM(6),KDUM(MAXPAR)
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / IDUM,LOOP, IDUMMY(19 + 2*NCHAIN -2)              3GL3092
      COMMON /OPTMCR / HESS(MAXPAR,MAXPAR),PMAT(MAXHES),                3GL3092
     1                 BMAT(MAXPAR,MAXPAR),GMIN1(MAXPAR),GMIN,          3GL3092
     2                 SIG(MAXPAR),XBEST(MAXPAR),GNEXT,AMIN,            3GL3092
     3                 ANEXT,PVEC(MAXPAR*MAXPAR),EIG(MAXPAR),P(MAXPAR), 3GL3092
     4                 Q(MAXPAR), WORK(MAXPAR),GNEXT1(MAXPAR),          3GL3092
     5                 RDUMY(MAXPAR*(NCHAIN + 8) + NCHAIN + 25)         3GL3092
C     COMMON /OPTIM / IMP,IMP0,LEC,IPRT,HESS(MAXPAR,MAXPAR),PMAT(MAXHES)
C    1               ,BMAT(MAXPAR,MAXPAR),GMIN1(MAXPAR),GMIN
C    2               ,IDUM,LOOP,SIG(MAXPAR),XBEST(MAXPAR),GNEXT,AMIN
C    3               ,ANEXT,PVEC(MAXPAR*MAXPAR),EIG(MAXPAR),P(MAXPAR)
C    4               ,Q(MAXPAR), WORK(MAXPAR),GNEXT1(MAXPAR)
      COMMON /AREACM/ NOPTI,TAREA,NINTEG
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      LOGICAL DEBUG, RESTRT, TIMES, FAIL, OK,TAREA
      CHARACTER SPACE*1, CHDOT*1, ZERO*1, NINE*1, CH*1
      SAVE
      DATA SPACE,CHDOT,ZERO,NINE /' ','.','0','9'/
C
C     LENGTH OF DATA TO BE SAVED OR RESTORED IN THE BEGINNING OF THE
C     COMMON /OPTIM/ I1(MAXPAR,LEN1),I2(LEN2) ... SEE 'SAVOPT'.
C     LEN1=4*MAXPAR+6
C     LEN2=2*MAXHES+8
C
C  LEN1, LEN2, AND LEN3 HAVE BEEN REDEFINED TO REPRESENT THE LENGTH OF THE
C  COMMON BLOCKS OPTMCR, OPTMCI, AND OPTMCL IN SVOPTS (NEW VERSION OF SAVOPT)
C
      LEN1=MAXHES+MAXPAR*(3*MAXPAR+8)+4                                 GL0492
      LEN2=2                                                            GL0492
      LEN3=0                                                            GL0492
      IF (ICONTR(11).EQ.1) THEN                                         GDH0195
         ICONTR(11)=2                                                   GDH0195
         RESTRT=(IRESTA.NE.0)                                           DJG0995
         CALL PORCPU (TIME1)                                            GL0492
         TIME2=TIME1
         TIMES=(ITIMES.NE.0)                                            DJG0995
C        TOTIME=3600
         TOTIME=TDEF                                                    GL0492
         IF(ITLIMI.NE.0) THEN                                           DJG0995
            TOTIME=FITLIM                                               DJG0995
            WRITE(JOUT,'(//10X,'' TIME FOR THIS STEP ='',F8.2)')TOTIME
         ENDIF
         STEP=0.02D0
         LAST=0
         LOOP=1
         XINC=0.00529167D0
         RHO2=1.D-8
         NCYCLE=9999
         IF(ICYCLE.NE.0) NCYCLE=IICYCL                                  DJG0995
         GNORM=1.D0
         IF(IPRECI.NE.0) GNORM=GNORM*0.1D0                              DJG0995
         IF(IGNORM.NE.0) GNORM=FIGNOR                                   DJG0995
         DEBUG = (IPOWSQ.NE.0)                                          DJG0995
         IF(IPRINT.NE.0)IMP=IIPRIN                                      DJG0995
         IF(RESTRT) THEN
C           RESTORE STORED DATA
            CALL SVOPTS(LEN1,LEN2,LEN3,.TRUE.)                          GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
            CALL SCOPY (NVAR,SIG,1,XPARAM,1)
            IF(LOOP .GT. 0) THEN
               WRITE(JOUT,'(//10X,'' RESTARTING AT POINT'',I3)')LOOP
            ELSE
               WRITE(JOUT,'(//10X,'' RESTARTING IN OPTIMISATION'',
     1         '' ROUTINES'')')
            ENDIF
         ENDIF
      ENDIF
C***********************************************************************
C     INITIALIZE : FIRST CALL TO COMPFG                                *
C***********************************************************************
      NVAR=ABS(NVAR)
      IF(DEBUG) THEN
         WRITE(JOUT,'('' ENTERING POWSQ. XPARAM :'')')
         WRITE(JOUT,'(8(F10.4))')(XPARAM(I),I=1,NVAR)
      ENDIF
      IF( .NOT. RESTRT) THEN
         CALL COMPFG(XPARAM,FUNCT,FAIL, GRAD, .TRUE.)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) THEN                                                    GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=25                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF                                                             GDH1095
         CALL SCOPY (NVAR,XPARAM,1,XBEST,1)
         IF(DEBUG) THEN
            WRITE(JOUT,'('' STARTING GRADIENTS'')')
            WRITE(JOUT,'(3X,8F9.4)')(GRAD(I),I=1,NVAR)
         ENDIF
         GMIN=SQRT(DOT(GRAD,GRAD,NVAR))
         CALL SCOPY (NVAR,GRAD,1,GMIN1,1)
      ENDIF
C***********************************************************************
C    NOW TO CALCULATE THE INITIAL HESSIAN MATRIX.                      *
C***********************************************************************
      IF(LOOP.LT.0) GOTO 110
      DO 40 ILOOP=LOOP,NVAR
         CALL PORCPU (TIME1)                                            GL0492
         XPARAM(ILOOP)=XPARAM(ILOOP) + XINC
         CALL COMPFG(XPARAM,FUNCT,FAIL, GRAD, .TRUE.)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) THEN                                                    GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=26                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF                                                             GDH1095
         IF(DEBUG.OR.IMP.GT.3)WRITE(JOUT,'(I3,12(8F9.4,/3X))')
     1    ILOOP,(GRAD(J),J=1,NVAR)
         XPARAM(ILOOP)=XPARAM(ILOOP) - XINC
         DO 30 J=1,NVAR
   30    HESS(ILOOP,J)=(GMIN1(J)-GRAD(J))/XINC
         CALL PORCPU (TIME2)                                            GL0492
         TSTEP=TIME2-TIME1
         IF(TIMES)WRITE(JOUT,'('' TIME FOR STEP:'',F8.2,'' LEFT'',F8.2)'
     1                 )TSTEP, TOTIME-TIME2+TIME0
         IF( TOTIME-TIME2+TIME0 .LT. TSTEP*2.D0) THEN
C           STORE RESULTS TO DATE.
            LOOP=ILOOP+1
            CALL SCOPY (NVAR,XPARAM,1,SIG,1)
            CALL SVOPTS(LEN1,LEN2,LEN3,.FALSE.)                         GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
          ISTOP=1                                                       GDH1095
          IWHERE=154                                                    GDH1095
          RETURN                                                        GDH1095
         ENDIF
   40 CONTINUE
C     CHECK THE INDEX (NUMBER OF NEGATIVE EIGENVALUES) OF THE HESSIAN.
      K=0
      DO 50 J=1,NVAR
      DO 50 I=1,NVAR
      K=K+1
   50 PVEC(K)=-0.5D0*(HESS(I,J)+HESS(J,I))
      CALL INVERT (PVEC,NVAR,JNDEX,WORK,SUM)                            DL0397
C     SCALE -HESSIAN MATRIX AND INITIALIZE B MATRIX
      IF( DEBUG.OR.IMP.GT.3) THEN
         WRITE(JOUT,'(//10X,''UN-NORMALISED HESSIAN MATRIX'')')
         DO 60 I=1,NVAR
   60    WRITE(JOUT,'(8F10.4)')(HESS(J,I),J=1,NVAR)
      ENDIF
      DO 80 I=1,NVAR
         SUM=1.D0/SQRT(SDOT(NVAR,HESS(I,1),MAXPAR,HESS(I,1),MAXPAR))
         DO 70 J=1,NVAR
            BMAT(I,J)=0.D0
   70    HESS(I,J) = HESS(I,J)*SUM
   80    BMAT(I,I)=SUM*2.D0
      IF( DEBUG.OR.IMP.GT.3) THEN
         WRITE(JOUT,'(//10X,''HESSIAN MATRIX'')')
         DO 90 I=1,NVAR
   90    WRITE(JOUT,'(8F10.4)')(HESS(J,I),J=1,NVAR)
      ENDIF
************************************************************************
*  THIS IS THE START OF THE BIG LOOP TO OPTIMISE THE GEOMETRY.         *
************************************************************************
      LOOP=-99
      TSTEP=TSTEP*4
C     DEFINE TOL2, THE CONVERGENCE THRESHOLD ON THE GRADIENT NORM.
      TOL2=GNORM*SQRT(DBLE(NVAR))                                       GCL0393
      SUM=(EG(1)+EG(2)+EG(3))*0.866D0
      TOL2=MAX(SUM,TOL2)
      WRITE(JOUT,100)TOL2,NCYCLE,GMIN,JNDEX
  100 FORMAT('0MINIMIZATION OF THE GRADIENT NORM BY ''SIGMA''...'
     ./' CONVERGENCE THRESHOLD ON THE GRADIENT NORM :',1P,D10.2
     ./' MAXIMUM NUMBER OF CYCLE :',0P,I4
     ./'     AT THE STARTING POINT THE GRADIENT NORM IS :',1P,D10.2
     ./'                           THE INDEX OF THE HESSIAN IS :',0P,I3)
C  VARIABLE IFAIL CHANGED TO IFAILA TO AVOID CONFLICT WITH KEYWORD FLAG DJG0995
  110 IFAILA=0                                                          DJG0995
      DO 500 LLOOP = 1,NCYCLE
      IF( TOTIME-TIME2+TIME0 .LT. TSTEP*2.D0) THEN
C        STORE RESULTS TO DATE.
         CALL SCOPY (NVAR,XPARAM,1,SIG,1)
         CALL SVOPTS(LEN1,LEN2,LEN3,.FALSE.)                            GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
          ISTOP=1                                                       GDH1095
          IWHERE=155                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
C     FORM-A- DAGGER-A- IN PA SLONG WITH -P-
      IJ=0
      DO 120 J=1,NVAR
      P(J)=SDOT(NVAR,HESS(J,1),MAXPAR,GMIN1,1)
      DO 120 I=1,J
      IJ=IJ+1
  120 PMAT(IJ) = SDOT(NVAR,HESS(I,1),MAXPAR,HESS(J,1),MAXPAR)
      IF(DEBUG.OR.IMP.GT.3) THEN
         WRITE(JOUT,'(/10X,''P MATRIX IN POWSQ'')')
         CALL VECPRT(PMAT,NVAR)
      ENDIF
      CALL HQRII(PMAT,NVAR,NVAR,EIG,PVEC)
C     FIND -Q- VECTOR AS FOLLOWS :
C     CHECK FOR ZERO EIGENVALUE,
      IF(EIG(1).LT.RHO2) THEN
C        TAKE -Q- VECTOR AS EIGENVECTOR OF ZERO EIGENVALUE.
         CALL SCOPY (NVAR,PVEC,1,Q,1)
      ELSE
C        FORM INVERSE BY BACK TRANSFORMING THE EIGENVECTORS,
         IJ=0
         DO 140 I=1,NVAR
         IK=I
         DO 130 K=1,NVAR
         WORK(K)=PVEC(IK)/EIG(K)
  130    IK=IK+NVAR
         DO 140 J=1,I
         IJ=IJ+1
  140    PMAT(IJ) = SDOT(NVAR,PVEC(J),NVAR,WORK,1)
C        FIND -Q- VECTOR.
         CALL SUPDOT (Q,PMAT,P,NVAR,1)
      ENDIF
C     FIND SEARCH DIRECTION
      DO 150 I=1,NVAR
  150 SIG(I) = SDOT(NVAR,Q,1,BMAT(I,1),MAXPAR)
C     DO A ONE DIMENSIONAL SEARCH
      IF (DEBUG.OR.IMP.GT.2) THEN
         WRITE(JOUT,'('' SEARCH VECTOR'')')
         WRITE(JOUT,'(8F10.5)')(SIG(I),I=1,NVAR)
      ENDIF
      CALL SEARCH(XPARAM, ALPHA, NVAR, GMIN, OK)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
C     SAVE THE BEST TRIAL POINT IN XBEST.
      IF (.NOT.OK) THEN
         IFAILA=IFAILA+1                                                DJG0995
         IF(IFAILA.GE.NVAR/2) THEN                                      DJG0995
           WRITE(JOUT,'('' NO IMPROVEMENT OF THE GRADIENT NORM AFTER''
     .                 ,I3,'' CYCLES ... STOP'')')IFAIL                 DJG0995
           IFLEP=12                                                     DJG0995
           GO TO 610
         ENDIF
      ELSE
         IFAILA=0                                                       DJG0995
         CALL SCOPY (NVAR,XPARAM,1,XBEST,1)
      ENDIF
C     CONVERGENCE CRITERIA ON GRADIENT NORM AND COMPONENTS.
      IF( NVAR .EQ. 1) GOTO 600
      IF(GMIN.LE.TOL2) THEN
         RMX = 0.0D0
         DO 160 K=1,NVAR
  160    RMX=MAX(ABS(GMIN1(K)),RMX)
         IF(RMX.LT.2.5D0*GNORM) GO TO 600
      ENDIF
C     TWO STEP ESTIMATION OF DERIVATIVES,USING GNEXT1 & GMIN1
      DO 170 K=1,NVAR
  170 WORK(K) = (GMIN1(K)-GNEXT1(K))/(AMIN-ANEXT)
      RMU =-DOT(WORK,GMIN1,NVAR)/(GMIN**2)
      CALL SAXPY (NVAR,RMU,GMIN1,1,WORK,1)
C     SCALE -WORK- AND -SIG-
      SK = 1.0D0/SQRT(DOT(WORK,WORK,NVAR))
      DO 180 K=1,NVAR
      SIG(K)  = SK*SIG(K)
  180 WORK(K) = SK*WORK(K)
C     FIND INDEX OF REPLACEMENT DIRECTION
      PMAX = ABS(P(1)*Q(1))
      ID=1
      DO 190 I=2,NVAR
         IF(ABS(P(I)*Q(I)).GT.PMAX) THEN
            PMAX = ABS(P(I)*Q(I))
            ID = I
         ENDIF
  190 CONTINUE
C     REPLACE APPROPRIATE DIRECTION, DERIVATIVE & STARTING POINT
      DO 200 K=1,NVAR
      HESS(ID,K) = -WORK(K)
  200 BMAT(K,ID) = SIG(K)/0.529167D0
      IF(DEBUG.OR.IMP.GT.0)WRITE(JOUT,'('' CYCLE'',I4,''   GRADIENT =''
     .,F8.2)')LLOOP,GMIN
      IF(DEBUG.OR.IMP.GT.1)WRITE(JOUT,'('' REPLACING DIRECTION'',I4)')ID
      TIME1=TIME2
      CALL PORCPU (TIME2)                                               GL0492
      TSTEP=TIME2-TIME1
      IF(TIMES)WRITE(JOUT,'('' TIME FOR STEP:'',F8.2,'' LEFT'',F8.2)')
     1TSTEP, TOTIME-TIME2+TIME0
  500 CONTINUE
C     MAXIMUM NUMBER OF CYCLES EXCEEDED.
C
      WRITE(JOUT,'('' MAXIMUM NUMBER OF CYCLES REACHED IN POWSQ ...''
     .,/'' START AGAIN FROM THE LAST POINT IF NECESSARY.'')')
      IFLEP=12                                                          DJG0995
      GO TO 610
C
C     CONVERGENCE ACHIEVED
C
  600 IFLEP=11                                                          DJG0995
  610 LAST=1
      IJ=0
      DO 620 I=1,NVAR
      DO 620 J=1,I
      IJ=IJ+1
  620 PMAT(IJ)=-0.5D0*(HESS(I,J)+HESS(J,I))
      NMAX=MIN(8,NVAR)
      CALL HQRII (PMAT,NVAR,NVAR,EIG,PVEC)
  630 FORMAT(' FIRST EIGENVALUES OF THE FINAL HESSIAN (ESTIMATE) :'
     .         /1P,8D10.2)
      WRITE(JOUT,630) (EIG(I),I=1,NMAX)
      IJ=0
      K=0
      DO 650 I=1,NVAR
      DO 640 J=1,NVAR
      K=K+1
  640 BMAT(J,I)=PVEC(K)**2
      DO 650 J=1,I
      IJ=IJ+1
  650 PMAT(IJ)=(HESS(I,J)-HESS(J,I))**2
      DO 660 I=1,NMAX
      CALL SUPDOT (WORK,PMAT,BMAT(1,I),NVAR,1)
  660 EIG(I)=SQRT(DOT(WORK,BMAT(1,I),NVAR))*0.5D0
      WRITE(JOUT,670) (EIG(I),I=1,NMAX)
  670 FORMAT(' WITH ERRORS DUE TO THE LACK OF SYMMETRY IN THE HESSIAN:'
     .         /1P,8D10.2)
      CALL SCOPY (NVAR,XBEST,1,XPARAM,1)
      CALL COMPFG(XPARAM,FUNCT,FAIL, GRAD, .TRUE.)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      GNFIN=SQRT(DOT(GRAD,GRAD,NVAR))
      WRITE(JOUT,'('' GRADIENT NORM AT THE FINAL POINT ='',F9.3)')GNFIN
      RETURN
      END
