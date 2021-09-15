      SUBROUTINE POWSQ(XPARAM, NVAR, FUNCT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION XPARAM(*)
      COMMON /MESAGE/ IFLEPO,ISCF
**********************************************************************
*
*   POWSQ OPTIMIZES THE GEOMETRY BY MINIMISING THE GRADIENT NORM.
*         THUS BOTH GROUND AND TRANSITION STATE GEOMETRIES CAN BE
*         CALCULATED. IT IS ROUGHLY EQUIVALENT TO FLEPO, FLEPO MINIMIZES
*         THE ENERGY, POWSQ MINIMIZES THE GRADIENT NORM.
*
*  ON ENTRY XPARAM = VALUES OF PARAMETERS TO BE OPTIMIZED.
*           NVAR   = NUMBER OF PARAMETERS TO BE OPTIMIZED.
*
*  ON EXIT  XPARAM = OPTIMIZED PARAMETERS.
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
      COMMON /NUMSCF/ NSCF
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
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),
     1PMAT(MAXPAR*MAXPAR)
      COMMON /SCRACH/ PVEC
      DIMENSION IPOW(9), SIG(MAXPAR),
     1          E1(MAXPAR), E2(MAXPAR),
     2          P(MAXPAR), WORK(MAXPAR),
     3          PVEC(MAXPAR*MAXPAR), EIG(MAXPAR), Q(MAXPAR)
      DIMENSION ISWAP(3)
      LOGICAL DEBUG, RESTRT, TIMES, OKC, OKF, ROUGH, SCF1, RESFIL
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
         ICYC=0
         TIMES=(INDEX(KEYWRD,'TIME') .NE. 0)
         TOTIME=MAXTIM
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
         TLEFT=TOTIME
         TLAST=TOTIME
         TDUMP=MAXDMP
         I=INDEX(KEYWRD,' DUMP')
         IF(I.NE.0) THEN
            TDUMP=READA(KEYWRD,I)
            DO 30 J=I+7,80
               CH=KEYWRD(J:J)
               IF( CH .NE. CHDOT .AND. (CH .LT. ZERO .OR. CH .GT. NINE))
     1 THEN
                  IF( CH .EQ. 'M') TDUMP=TDUMP*60
                  GOTO 40
               ENDIF
   30       CONTINUE
   40       CONTINUE
         ENDIF
         RESFIL=.FALSE.
         STEP=0.02D0
         LAST=0
         ILOOP=1
         NAT3=NUMAT*3
         XINC=0.00529167D0
         RHO2=1.D-4
         TOL2=4.D-1
         IF(INDEX(KEYWRD,'PREC') .NE. 0) TOL2=1.D-2
         DEBUG = (INDEX(KEYWRD,'POWSQ') .NE. 0)
         IF(RESTRT) THEN
C
C   RESTORE STORED DATA
C
            IPOW(9)=0
            CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,ILOOP,BMAT,IPOW)
            NSCF=IPOW(8)
            DO 50 I=1,NVAR
               GRAD(I)=GMIN1(I)
   50       GNEXT1(I)=GMIN1(I)
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
      NVAR=ABS(NVAR)
      IF(DEBUG) THEN
         WRITE(6,'('' XPARAM'')')
         WRITE(6,'(5(2I3,F10.4))')(LOC(1,I),LOC(2,I),XPARAM(I),I=1,NVAR)
      ENDIF
      IF( .NOT. RESTRT) THEN
         DO 60 I=1,NVAR
   60    GRAD(I)=0.D0
         CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.)
      ENDIF
      IF(DEBUG) THEN
         WRITE(6,'('' STARTING GRADIENTS'')')
         WRITE(6,'(3X,8F9.4)')(GRAD(I),I=1,NVAR)
      ENDIF
      GMIN=SQRT(DOT(GRAD,GRAD,NVAR))
      GLAST=GMIN
      DO 70 I=1,NVAR
         GNEXT1(I)=GRAD(I)
         GMIN1(I)=GNEXT1(I)
   70 CONTINUE
C
C    NOW TO CALCULATE THE HESSIAN MATRIX.
C
      IF(ILOOP.LT.0) GOTO 180
C
C   CHECK THAT HESSIAN HAS NOT ALREADY BEEN CALCULATED.
C
      ILPR=ILOOP
      DO 90 ILOOP=ILPR,NVAR
         TIME1=SECOND()
         XPARAM(ILOOP)=XPARAM(ILOOP) + XINC
         CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.)
         IF(SCF1) GOTO 430
         IF(DEBUG)WRITE(6,'(I3,12(8F9.4,/3X))')
     1    ILOOP,(GRAD(IF),IF=1,NVAR)
         GRAD(ILOOP)=GRAD(ILOOP)+1.D-5
         XPARAM(ILOOP)=XPARAM(ILOOP) - XINC
         DO 80 J=1,NVAR
   80    HESS(ILOOP,J)=-(GRAD(J)-GNEXT1(J))/XINC
         TIME2=SECOND()
         TSTEP=TIME2-TIME1
         IF(TIMES)WRITE(6,'('' TIME FOR STEP:'',F8.2,'' LEFT'',F8.2)')
     1    TSTEP, TLEFT
         IF(TLAST-TLEFT.GT.TDUMP)THEN
            TLAST=TLEFT
            RESFIL=.TRUE.
            IPOW(9)=2
            I=ILOOP
            IPOW(8)=NSCF
            CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,I,BMAT,IPOW)
         ENDIF
         IF( TLEFT .LT. TSTEP*2.D0) THEN
C
C  STORE RESULTS TO DATE.
C
            IPOW(9)=1
            I=ILOOP
            IPOW(8)=NSCF
            CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,I,BMAT,IPOW)
            STOP
         ENDIF
   90 CONTINUE
C        *****  SCALE -HESSIAN- MATRIX                           *****
      IF( DEBUG) THEN
         WRITE(6,'(//10X,''UN-NORMALIZED HESSIAN MATRIX'')')
         DO 100 I=1,NVAR
  100    WRITE(6,'(8F10.4)')(HESS(J,I),J=1,NVAR)
      ENDIF
      DO 120 I=1,NVAR
         SUM = 0.0D0
         DO 110 J=1,NVAR
  110    SUM = SUM+HESS(I,J)**2
  120 WORK(I) = 1.0D0/SQRT(SUM)
      DO 130 I=1,NVAR
         DO 130 J=1,NVAR
  130 HESS(I,J) = HESS(I,J)*WORK(I)
      IF( DEBUG) THEN
         WRITE(6,'(//10X,''HESSIAN MATRIX'')')
         DO 140 I=1,NVAR
  140    WRITE(6,'(8F10.4)')(HESS(J,I),J=1,NVAR)
      ENDIF
C        *****  INITIALIZE B MATIRX                        *****
      DO 160 I=1,NVAR
         DO 150 J=1,NVAR
  150    BMAT(I,J) = 0.0D0
  160 BMAT(I,I) = WORK(I)*2.D0
************************************************************************
*
*  THIS IS THE START OF THE BIG LOOP TO OPTIMIZE THE GEOMETRY
*
************************************************************************
      ILOOP=-99
      TSTEP=TSTEP*4
  170 CONTINUE
      IF(TLAST-TLEFT.GT.TDUMP)THEN
         TLAST=TLEFT
         RESFIL=.TRUE.
         IPOW(9)=2
         I=ILOOP
         IPOW(8)=NSCF
         CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,I,BMAT,IPOW)
      ENDIF
      IF( TLEFT .LT. TSTEP*2.D0) THEN
C
C  STORE RESULTS TO DATE.
C
         IPOW(9)=1
         I=ILOOP
         IPOW(8)=NSCF
         CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,I,BMAT,IPOW)
         STOP
      ENDIF
  180 CONTINUE
C        *****  FORM-A- DAGGER-A- IN PA SLONG WITH -P-     *****
      IJ=0
      DO 200 J=1,NVAR
         DO 200 I=1,J
            IJ=IJ+1
            SUM = 0.0D0
            DO 190 K=1,NVAR
  190       SUM = SUM + HESS(I,K)*HESS(J,K)
  200 PMAT(IJ) = SUM
      DO 220 I=1,NVAR
         SUM = 0.0D0
         DO 210 K=1,NVAR
  210    SUM = SUM-HESS(I,K)*GMIN1(K)
  220 P(I) = -SUM
      L=0
      IF(DEBUG) THEN
         WRITE(6,'(/10X,''P MATRIX IN POWSQ'')')
         CALL VECPRT(PMAT,NVAR)
      ENDIF
      CALL HQRII(PMAT,NVAR,NVAR,EIG,PVEC)
C        *****  CHECK FOR ZERO EIGENVALUE                  *****
C#      WRITE(6,'(''  EIGS IN POWSQ:'')')
C#      WRITE(6,'(6F13.8)')(EIG(I),I=1,NVAR)
      IF(EIG(1).LT.RHO2) GO TO 280
      INDC = 2
C        *****  IF MATRIX IS NOT SINGULAR FORM INVERSE     *****
C        *****  BY BACK TRANSFORMING THE EIGENVECTORS      *****
      IJ=0
      DO 240 I=1,NVAR
         DO 240 J=1,I
            IJ=IJ+1
            SUM = 0.0D0
            DO 230 K=1,NVAR
  230       SUM = SUM+PVEC((K-1)*NVAR+J)*PVEC((K-1)*NVAR+I)/EIG(K)
  240 PMAT(IJ) = SUM
C        *****  FIND -Q- VECTOR                            *****
      L=0
      IL=L+1
      L=IL+I-1
      DO 270 I=1,NVAR
         SUM = 0.0D0
         DO 250 K=1,I
            IK=(I*(I-1))/2+K
  250    SUM = SUM+PMAT(IK)*P(K)
         IP1=I+1
         DO 260 K=IP1,NVAR
            IK=(K*(K-1))/2+I
  260    SUM=SUM+PMAT(IK)*P(K)
  270 Q(I) = SUM
      GO TO 300
  280 CONTINUE
C        *****  TAKE  -Q- VECTOR AS EIGENVECTOR OF ZERO     *****
C        *****  EIGENVALUE                                 *****
      DO 290 I=1,NVAR
  290 Q(I) = PVEC(I)
  300 CONTINUE
C        *****  FIND SEARCH DIRECTION                      *****
      DO 310 I=1,NVAR
         SIG(I) = 0.0D0
         DO 310 J=1,NVAR
  310 SIG(I) = SIG(I) + Q(J)*BMAT(I,J)
C        *****  DO A ONE DIMENSIONAL SEARCH                *****
      IF (DEBUG) THEN
         WRITE(6,'('' SEARCH VECTOR'')')
         WRITE(6,'(8F10.5)')(SIG(I),I=1,NVAR)
      ENDIF
      CALL SEARCH(XPARAM, ALPHA, SIG, NVAR, GMIN, OKC, OKF, FUNCT)
      IF( NVAR .EQ. 1) GOTO 430
C
C  FIRST WE ATTEMPT TO OPTIMIZE GEOMETRY USING SEARCH.
C  IF THIS DOES NOT WORK, THEN SWITCH TO LINMIN, WHICH ALWAYS WORKS,
C  BUT IS TWICE AS SLOW AS SEARCH.
C
      ROUGH=  (   .NOT.  OKF)
      RMX = 0.0D0
      DO 320 K=1,NVAR
         RT = ABS(GMIN1(K))
         IF(RT.GT.RMX)RMX = RT
  320 CONTINUE
      IF(RMX.LT.TOL2) GO TO 430
C        *****  TWO STEP ESTIMATION OF DERIVATIVES         *****
      DO 330 K=1,NVAR
  330 E1(K) = (GMIN1(K)-GNEXT1(K))/(AMIN-ANEXT)
      RMU = DOT(E1,GMIN1,NVAR)/DOT(GMIN1,GMIN1,NVAR)
      DO 340 K=1,NVAR
  340 E2(K) = E1(K) - RMU*GMIN1(K)
C        *****  SCALE -E2- AND -SIG-                       *****
      SK = 1.0D0/SQRT(DOT(E2,E2,NVAR))
      DO 350 K=1,NVAR
  350 SIG(K) = SK*SIG(K)
      DO 360 K=1,NVAR
  360 E2(K) = SK*E2(K)
C        *****  FIND INDEX OF REPLACEMENT DIRECTION        *****
      PMAX = -1.0D+20
      DO 370 I=1,NVAR
         IF(ABS(P(I)*Q(I)).LE.PMAX) GO TO 370
         PMAX = ABS(P(I)*Q(I))
         ID = I
  370 CONTINUE
C        *****  REPLACE APPROPRIATE DIRECTION AND DERIVATIVE ***
      DO 380 K=1,NVAR
  380 HESS(ID,K) = -E2(K)
C        *****  REPLACE STARTING POINT                     *****
      DO 390 K=1,NVAR
  390 BMAT(K,ID) = SIG(K)/0.529167D0
      DO 400 K=1,NVAR
  400 GNEXT1(K) = GMIN1(K)
      GLAST = GMIN
      INDC = 1
      TIME1=TIME2
      TIME2=SECOND()
      TLEFT=TOTIME-TIME2+TIME0
      TSTEP=TIME2-TIME1
      ICYC=ICYC+1
      IF(RESFIL)THEN
         WRITE(6,410)TLEFT,GMIN,FUNCT
  410    FORMAT('  RESTART FILE WRITTEN,  TIME LEFT:',F9.1,
     1' GRAD.:',F10.3,' HEAT:',G14.7)
         RESFIL=.FALSE.
      ELSE
         WRITE(6,420)ICYC,TSTEP,TLEFT,GMIN,FUNCT
  420    FORMAT(' CYCLE:',I5,' TIME:',F6.1,' TIME LEFT:',F9.1,
     1' GRAD.:',F10.3,' HEAT:',G14.7)
      ENDIF
      IF(TIMES)WRITE(6,'('' TIME FOR STEP:'',F8.2,'' LEFT'',F8.2)')
     1TSTEP, TLEFT
      GO TO 170
  430 CONTINUE
      DO 440 I=1,NVAR
  440 GRAD(I)=0.D0
      LAST=1
      CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.)
      DO 450 I=1,NVAR
  450 GRAD(I)=GMIN1(I)
      GNFINA=SQRT(DOT(GRAD,GRAD,NVAR))
      IFLEPO=11
      RETURN
      END
