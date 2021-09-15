      SUBROUTINE POWSQ(XPARAM, NVAR, FUNCT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION XPARAM(*)
      COMMON /MESAGE/ IFLEPO,ISCF
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
C
      COMMON /GEOVAR/ NDUM,LOC(2,MAXPAR), IDUMY, XARAM(MAXPAR)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /LAST  / LAST
      COMMON /KEYWRD/ KEYWRD
      COMMON /TIME  / TIME0
      COMMON /GEOSYM/ NDEP, LOCPAR(MAXPAR), IDEPFN(MAXPAR),
     1                 LOCDEP(MAXPAR)
      COMMON /GRADNT/ GRAD(MAXPAR),GNFINA
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /NUMCAL/ NUMCAL
      COMMON /SIGMA1/ GNEXT, AMIN, ANEXT
      COMMON /SIGMA2/ GNEXT1(MAXPAR), GMIN1(MAXPAR)
      COMMON /NLLCOM/ HESS,BMAT,PMAT
      COMMON /SCRACH/ PVEC
      DIMENSION HESS(MAXPAR,MAXPAR), BMAT(MAXPAR,MAXPAR)
      DIMENSION IPOW(9), SIG(MAXPAR),
     1          PMAT(MAXHES), E1(MAXPAR), E2(MAXPAR),
     2          P(MAXPAR), WORK(MAXPAR),
     3          PVEC(MAXPAR*MAXPAR), EIG(MAXPAR), Q(MAXPAR)
      DIMENSION ISWAP(3)
      LOGICAL DEBUG, RESTRT, TIMES, OKC, OKF, ROUGH, NOPRT, SCF1
      CHARACTER*80 KEYWRD
      CHARACTER SPACE*1, CHDOT*1, ZERO*1, NINE*1, CH*1
      DATA SPACE,CHDOT,ZERO,NINE /' ','.','0','9'/
      DATA  ICALCN /0/, ISWAP /2,3,1/
      IF(ICALCN.NE.NUMCAL) THEN
         ICALCN=NUMCAL
         RESTRT=(INDEX(KEYWRD,'RESTART') .NE. 0)
         SCF1=(INDEX(KEYWRD,'1SCF') .NE. 0)
         ROUGH=.FALSE.
         TIME1=SECOND()
         TIME2=TIME1
         TIMES=(INDEX(KEYWRD,'TIME') .NE. 0)
         TOTIME=3600
         I=INDEX(KEYWRD,' T=')
         IF(I.NE.0) THEN
            TIM=READA(KEYWRD,I)
            DO 10 J=I+3,80
               CH=KEYWRD(J:J)
               IF( CH .NE. CHDOT .AND. (CH .LT. ZERO .OR. CH .GT. NINE))
     1 THEN
                  IF( CH .EQ. 'M') TIM=TIM*60
                  GOTO 20
               ENDIF
   10       CONTINUE
   20       TOTIME=TIM
            WRITE(6,'(//10X,'' TIME FOR THIS STEP ='',F8.2)')TOTIME
         ENDIF
         STEP=0.02D0
         LAST=0
         ILOOP=1
         NAT3=NUMAT*3
         XINC=0.00529167D0
         RHO2=1.D-8
         TOL2=4.D-1
         IF(INDEX(KEYWRD,'PREC') .NE. 0) TOL2=1.D-2
         DEBUG = (INDEX(KEYWRD,'POWSQ') .NE. 0)
         IF(RESTRT) THEN
C
C   RESTORE STORED DATA
C
            IPOW(9)=0
            CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,ILOOP,BMAT,IPOW)
            DO 30 I=1,NVAR
               GRAD(I)=GMIN1(I)
   30       GNEXT1(I)=GMIN1(I)
            WRITE(6,'('' XPARAM'',6F10.6)')(XPARAM(I),I=1,NVAR)
            IF(ILOOP .GT. 0) THEN
C#               ILOOP=ILOOP+1
               WRITE(6,'(//10X,'' RESTARTING AT POINT'',I3)')ILOOP
            ELSE
               WRITE(6,'(//10X,''RESTARTING IN OPTIMISATION'',
     1         '' ROUTINES'')')
            ENDIF
         ENDIF
*
*   DEFINITIONS:   NVAR   = NUMBER OF GEOMETRIC VARIABLES = 3*NUMAT-6
*
      ENDIF
C#      ILOOP=MIN(1,NVAR)
C#      NOPRT=(NVAR.LT.0)
      NVAR=ABS(NVAR)
      IF(DEBUG) THEN
         WRITE(6,'('' XPARAM'')')
         WRITE(6,'(5(2I3,F10.4))')(LOC(1,I),LOC(2,I),XPARAM(I),I=1,NVAR)
      ENDIF
      IF( .NOT. RESTRT) THEN
         DO 40 I=1,NVAR
   40    GRAD(I)=0.D0
         CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.)
      ENDIF
      IF(DEBUG) THEN
         WRITE(6,'('' STARTING GRADIENTS'')')
         WRITE(6,'(3X,8F9.4)')(GRAD(I),I=1,NVAR)
      ENDIF
      GMIN=SQRT(DOT(GRAD,GRAD,NVAR))
      GLAST=GMIN
      DO 50 I=1,NVAR
         GNEXT1(I)=GRAD(I)
         GMIN1(I)=GNEXT1(I)
   50 CONTINUE
C
C    NOW TO CALCULATE THE HESSIAN MATRIX.
C
      IF(ILOOP.LT.0) GOTO 160
C
C   CHECK THAT HESSIAN HAS NOT ALREADY BEEN CALCULATED.
C
      DO 70 ILOOP=ILOOP,NVAR
         TIME1=SECOND()
         XPARAM(ILOOP)=XPARAM(ILOOP) + XINC
         CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.)
         IF(SCF1) GOTO 390
         IF(DEBUG)WRITE(6,'(I3,12(8F9.4,/3X))')
     1    ILOOP,(GRAD(IF),IF=1,NVAR)
         GRAD(ILOOP)=GRAD(ILOOP)+1.D-5
         XPARAM(ILOOP)=XPARAM(ILOOP) - XINC
         DO 60 J=1,NVAR
   60    HESS(ILOOP,J)=-(GRAD(J)-GNEXT1(J))/XINC
         TIME2=SECOND()
         TSTEP=TIME2-TIME1
         IF(TIMES)WRITE(6,'('' TIME FOR STEP:'',F8.2,'' LEFT'',F8.2)')
     1    TSTEP, TOTIME-TIME2+TIME0
         IF( TOTIME-TIME2+TIME0 .LT. TSTEP*2.D0) THEN
C
C  STORE RESULTS TO DATE.
C
            IPOW(9)=1
            I=ILOOP
            CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,I,BMAT,IPOW)
         ENDIF
   70 CONTINUE
C        *****  SCALE -HESSIAN- MATRIX                           *****
      IF( DEBUG) THEN
         WRITE(6,'(//10X,''UN-NORMALISED HESSIAN MATRIX'')')
         DO 80 I=1,NVAR
   80    WRITE(6,'(8F10.4)')(HESS(J,I),J=1,NVAR)
      ENDIF
      DO 100 I=1,NVAR
         SUM = 0.0D0
         DO 90 J=1,NVAR
   90    SUM = SUM+HESS(I,J)**2
  100 WORK(I) = 1.0D0/SQRT(SUM)
      DO 110 I=1,NVAR
         DO 110 J=1,NVAR
  110 HESS(I,J) = HESS(I,J)*WORK(I)
      IF( DEBUG) THEN
         WRITE(6,'(//10X,''HESSIAN MATRIX'')')
         DO 120 I=1,NVAR
  120    WRITE(6,'(8F10.4)')(HESS(J,I),J=1,NVAR)
      ENDIF
C        *****  INITIALIZE B MATIRX                        *****
      DO 140 I=1,NVAR
         DO 130 J=1,NVAR
  130    BMAT(I,J) = 0.0D0
  140 BMAT(I,I) = WORK(I)*2.D0
************************************************************************
*
*  THIS IS THE START OF THE BIG LOOP TO OPTIMISE THE GEOMETRY
*
************************************************************************
      ILOOP=-99
      TSTEP=TSTEP*4
  150 CONTINUE
      IF( TOTIME-TIME2+TIME0 .LT. TSTEP*2.D0) THEN
C
C  STORE RESULTS TO DATE.
C
         IPOW(9)=1
         I=ILOOP
         CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,I,BMAT,IPOW)
      ENDIF
  160 CONTINUE
C        *****  FORM-A- DAGGER-A- IN PA SLONG WITH -P-     *****
      IJ=0
      DO 180 J=1,NVAR
         DO 180 I=1,J
            IJ=IJ+1
            SUM = 0.0D0
            DO 170 K=1,NVAR
  170       SUM = SUM + HESS(I,K)*HESS(J,K)
  180 PMAT(IJ) = SUM
      DO 200 I=1,NVAR
         SUM = 0.0D0
         DO 190 K=1,NVAR
  190    SUM = SUM-HESS(I,K)*GMIN1(K)
  200 P(I) = -SUM
      L=0
      IF(DEBUG) THEN
         WRITE(6,'(/10X,''P MATRIX IN POWSQ'')')
         CALL VECPRT(PMAT,NVAR)
      ENDIF
      CALL HQRII(PMAT,NVAR,NVAR,EIG,PVEC)
C        *****  CHECK FOR ZERO EIGENVALUE                  *****
      IF(EIG(1).LT.RHO2) GO TO 260
      INDC = 2
C        *****  IF MATRIX IS NOT SINGULAR FORM INVERSE     *****
C        *****  BY BACK TRANSFORMING THE EIGENVECTORS      *****
      IJ=0
      DO 220 I=1,NVAR
         DO 220 J=1,I
            IJ=IJ+1
            SUM = 0.0D0
            DO 210 K=1,NVAR
  210       SUM = SUM+PVEC((K-1)*NVAR+J)*PVEC((K-1)*NVAR+I)/EIG(K)
  220 PMAT(IJ) = SUM
C        *****  FIND -Q- VECTOR                            *****
      L=0
      IL=L+1
      L=IL+I-1
      DO 250 I=1,NVAR
         SUM = 0.0D0
         DO 230 K=1,I
            IK=(I*(I-1))/2+K
  230    SUM = SUM+PMAT(IK)*P(K)
         IP1=I+1
         DO 240 K=IP1,NVAR
            IK=(K*(K-1))/2+I
  240    SUM=SUM+PMAT(IK)*P(K)
  250 Q(I) = SUM
      GO TO 280
  260 CONTINUE
C        *****  TAKE  -Q- VECTOR AS EIGENVECTOR OF ZERO     *****
C        *****  EIGENVALUE                                 *****
      DO 270 I=1,NVAR
  270 Q(I) = PVEC(I)
  280 CONTINUE
C        *****  FIND SEARCH DIRECTION                      *****
      DO 290 I=1,NVAR
         SIG(I) = 0.0D0
         DO 290 J=1,NVAR
  290 SIG(I) = SIG(I) + Q(J)*BMAT(I,J)
C        *****  DO A ONE DIMENSIONAL SEARCH                *****
      IF (DEBUG) THEN
         WRITE(6,'('' SEARCH VECTOR'')')
         WRITE(6,'(8F10.5)')(SIG(I),I=1,NVAR)
      ENDIF
      CALL SEARCH(XPARAM, ALPHA, SIG, NVAR, GMIN, OKC, OKF)
      IF( NVAR .EQ. 1) GOTO 390
C
C  FIRST WE ATTEMPT TO OPTIMISE GEOMETRY USING SEARCH.
C  IF THIS DOES NOT WORK, THEN SWITCH TO LINMIN, WHICH ALWAYS WORKS,
C  BUT IS TWICE AS SLOW AS SEARCH.
C
      ROUGH=  (   .NOT.  OKF)
      RMX = 0.0D0
      DO 300 K=1,NVAR
         RT = ABS(GMIN1(K))
         IF(RT.GT.RMX)RMX = RT
  300 CONTINUE
      IF(RMX.LT.TOL2) GO TO 390
C        *****  TWO STEP ESTIMATION OF DERIVATIVES         *****
      DO 310 K=1,NVAR
  310 E1(K) = (GMIN1(K)-GNEXT1(K))/(AMIN-ANEXT)
      RMU = DOT(E1,GMIN1,NVAR)/DOT(GMIN1,GMIN1,NVAR)
      DO 320 K=1,NVAR
  320 E2(K) = E1(K) - RMU*GMIN1(K)
C        *****  SCALE -E2- AND -SIG-                       *****
      SK = 1.0D0/SQRT(DOT(E2,E2,NVAR))
      DO 330 K=1,NVAR
  330 SIG(K) = SK*SIG(K)
      DO 340 K=1,NVAR
  340 E2(K) = SK*E2(K)
C        *****  FIND INDEX OF REPLACEMENT DIRECTION        *****
      PMAX = -1.0D+20
      DO 350 I=1,NVAR
         IF(ABS(P(I)*Q(I)).LE.PMAX) GO TO 350
         PMAX = ABS(P(I)*Q(I))
         ID = I
  350 CONTINUE
C        *****  REPLACE APPROPRIATE DIRECTION AND DERIVATIVE ***
      DO 360 K=1,NVAR
  360 HESS(ID,K) = -E2(K)
C        *****  REPLACE STARTING POINT                     *****
      DO 370 K=1,NVAR
  370 BMAT(K,ID) = SIG(K)/0.529167D0
      DO 380 K=1,NVAR
  380 GNEXT1(K) = GMIN1(K)
      IF( .NOT. NOPRT)WRITE(6,'('' GRADIENT ='',F13.6)')GMIN
      GLAST = GMIN
      IF( .NOT. NOPRT)WRITE(6,'(''  REPLACING DIRECTION '',I4)') ID
      INDC = 1
      TIME1=TIME2
      TIME2=SECOND()
      TSTEP=TIME2-TIME1
      IF(TIMES)WRITE(6,'('' TIME FOR STEP:'',F8.2,'' LEFT'',F8.2)')
     1TSTEP, TOTIME-TIME2+TIME0
      GO TO 150
  390 CONTINUE
      DO 400 I=1,NVAR
  400 GRAD(I)=0.D0
      LAST=1
      CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.)
      DO 410 I=1,NVAR
  410 GRAD(I)=GMIN1(I)
      GNFINA=SQRT(DOT(GRAD,GRAD,NVAR))
      IFLEPO=11
      RETURN
      END
