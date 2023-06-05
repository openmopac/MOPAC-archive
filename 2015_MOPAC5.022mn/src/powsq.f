      SUBROUTINE POWSQ(XPARAM, NVAR, FUNCT,*)                           CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
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
      COMMON /GEOVAR/ XDUMMY(MAXPAR),NDUM,LOC(2,MAXPAR)                 IR0394
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /LAST  / LAST
      COMMON /KEYWRD/ KEYWRD
      COMMON /TIMER / TIME0
      COMMON /NUMSCF/ NSCF
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /GRADNT/ GRAD(MAXPAR),GNFINA
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
C Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
     3       /MOLKSR/ FRACT
      COMMON /NUMCAL/ NUMCAL
      COMMON /SIGMA1/ GNEXT, AMIN, ANEXT
      COMMON /SIGMA2/ GNEXT1(MAXPAR), GMIN1(MAXPAR)
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),
     1                PMAT(MAXPAR*MAXPAR)
      COMMON /SCRACH/ PVEC
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION IPOW(9), SIG(MAXPAR),
     1          E1(MAXPAR), E2(MAXPAR),
     2          P(MAXPAR), WORK(MAXPAR),
     3          PVEC(MAXPAR*MAXPAR), EIG(MAXPAR), Q(MAXPAR)
      DIMENSION ISWAP(3)
      LOGICAL DEBUG, RESTRT, TIMES, OKC, OKF, ROUGH, SCF1, RESFIL
      CHARACTER*160 KEYWRD
      CHARACTER SPACE*1, CHDOT*1, ZERO*1, NINE*1, CH*1
         SAVE                                                           GL0892
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
            DO 10 J=I+3,160
               CH=KEYWRD(J+1:J+1)
               IF( CH .EQ.' ') THEN
                  CH=KEYWRD(J:J)
                  IF( CH .EQ. 'M') TIM=TIM*60
                  IF( CH .EQ. 'H') TIM=TIM*3600
                  IF( CH .EQ. 'D') TIM=TIM*86400
                  GOTO 20
               ENDIF
   10       CONTINUE
   20       TOTIME=TIM
            IF(DOPRNT)WRITE(6,'(//10X,'' TIME FOR THIS STEP ='',F10.2,  CIO
     1'' SECONDS'')')TOTIME
         ENDIF
         TLEFT=TOTIME
         TLAST=TOTIME
         TDUMP=MAXDMP
         I=INDEX(KEYWRD,' DUMP')
         IF(I.NE.0) THEN
            TDUMP=READA(KEYWRD,I)
            DO 30 J=I+7,160
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
         IF(INDEX(KEYWRD,'GNORM') .NE. 0) THEN
            TOL2=READA(KEYWRD,INDEX(KEYWRD,'GNORM'))
            IF(TOL2.LT.0.001.AND.INDEX(KEYWRD,'LET').EQ.0)THEN
               IF (DOPRNT) WRITE(6,'(/,A)')                             CIO
     &                '  GNORM HAS BEEN SET TOO LOW, RESET TO 0.001'    CIO
               TOL2=0.001D0
            ENDIF
         ENDIF
         DEBUG = (INDEX(KEYWRD,'POWSQ') .NE. 0)
         IF(RESTRT) THEN
C
C   RESTORE STORED DATA
C
            IPOW(9)=0
            CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,ILOOP,BMAT,IPOW,*9999)   CSTP(call)
            NSCF=IPOW(8)
            DO 50 I=1,NVAR
               GRAD(I)=GMIN1(I)
   50       GNEXT1(I)=GMIN1(I)
            IF (DOPRNT) WRITE(6,'(                                      CIO
     &               '' XPARAM'',6F10.6)')(XPARAM(I),I=1,NVAR)          CIO
            IF(ILOOP .GT. 0) THEN
C#               ILOOP=ILOOP+1
               IF (DOPRNT) WRITE(6,'(//10X,                             CIO
     &               '' RESTARTING AT POINT'',I3)')ILOOP                CIO
            ELSE
               IF (DOPRNT) WRITE(6,'(//10X,                             CIO
     &               ''RESTARTING IN OPTIMIZATION ROUTINES'')')         CIO
            ENDIF
         ENDIF
*
*   DEFINITIONS:   NVAR   = NUMBER OF GEOMETRIC VARIABLES = 3*NUMAT-6
*
      ENDIF
      NVAR=ABS(NVAR)
      IF(DEBUG) THEN
         IF (DOPRNT) WRITE(6,'('' XPARAM'')')                           CIO
         IF (DOPRNT) WRITE(6,'(5(2I3,F10.4))')                          CIO
     &               (LOC(1,I),LOC(2,I),XPARAM(I),I=1,NVAR)             CIO
      ENDIF
      IF( .NOT. RESTRT) THEN
         DO 60 I=1,NVAR
   60    GRAD(I)=0.D0
         CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.,*9999)  CSTP(call)
      ENDIF
      IF(DEBUG) THEN
         IF (DOPRNT) WRITE(6,'('' STARTING GRADIENTS'')')               CIO
         IF (DOPRNT) WRITE(6,'(3X,8F9.4)')(GRAD(I),I=1,NVAR)            CIO
      ENDIF
      GMIN=SQRT(ddot(nvar,grad,1,grad,1))
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
         CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.,*9999)  CSTP(call)
         IF(SCF1) GOTO 430
         IF(DEBUG.AND.DOPRNT)WRITE(6,'(I3,12(8F9.4,/3X))')              CIO
     1    ILOOP,(GRAD(IF),IF=1,NVAR)
         GRAD(ILOOP)=GRAD(ILOOP)+1.D-5
         XPARAM(ILOOP)=XPARAM(ILOOP) - XINC
         DO 80 J=1,NVAR
   80    HESS(ILOOP,J)=-(GRAD(J)-GNEXT1(J))/XINC
         TIME2=SECOND()
         TSTEP=TIME2-TIME1
         IF(TIMES.AND.DOPRNT)WRITE(6,'('' TIME FOR STEP:'',F8.2,        CIO
     &                          '' LEFT'',F8.2)') TSTEP, TLEFT          CIO
         IF(TLAST-TLEFT.GT.TDUMP)THEN
            TLAST=TLEFT
            RESFIL=.TRUE.
            IPOW(9)=2
            I=ILOOP
            IPOW(8)=NSCF
            CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,I,BMAT,IPOW,*9999)        CSTP(call)
         ENDIF
         IF( TLEFT .LT. TSTEP*2.D0) THEN
C
C  STORE RESULTS TO DATE.
C
            IPOW(9)=1
            I=ILOOP
            IPOW(8)=NSCF
            CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,I,BMAT,IPOW,*9999)        CSTP(call)
      RETURN 1                                                           CSTP (stop)
         ENDIF
   90 CONTINUE
C        *****  SCALE -HESSIAN- MATRIX                           *****
      IF( DEBUG) THEN
         IF(DOPRNT)WRITE(6,'(//10X,''UN-NORMALIZED HESSIAN MATRIX'')')  CIO
         DO 100 I=1,NVAR
  100    IF (DOPRNT) WRITE(6,'(8F10.4)')(HESS(J,I),J=1,NVAR)            CIO
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
         IF (DOPRNT) WRITE(6,'(//10X,''HESSIAN MATRIX'')')              CIO
         DO 140 I=1,NVAR
  140    IF (DOPRNT) WRITE(6,'(8F10.4)')(HESS(J,I),J=1,NVAR)            CIO
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
         CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,I,BMAT,IPOW,*9999)           CSTP(call)
      ENDIF
      IF( TLEFT .LT. TSTEP*2.D0) THEN
C
C  STORE RESULTS TO DATE.
C
         IPOW(9)=1
         I=ILOOP
         IPOW(8)=NSCF
         CALL POWSAV(HESS,GMIN1,XPARAM,PMAT,I,BMAT,IPOW,*9999)           CSTP(call)
      RETURN 1                                                           CSTP (stop)
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
         IF (DOPRNT) WRITE(6,'(/10X,''P MATRIX IN POWSQ'')')            CIO
         CALL VECPRT(PMAT,NVAR,*9999)                                   CSTP(call)
      ENDIF
      CALL HQRII(PMAT,NVAR,NVAR,EIG,PVEC,*9999)                         CSTP(call)
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
         IF (DOPRNT) WRITE(6,'('' SEARCH VECTOR'')')                    CIO
         IF (DOPRNT) WRITE(6,'(8F10.5)')(SIG(I),I=1,NVAR)               CIO
      ENDIF
      CALL SEARCH(XPARAM, ALPHA, SIG, NVAR, GMIN, OKC, OKF, FUNCT,*9999) CSTP(call)
      IF( NVAR .EQ. 1) GOTO 430
C
C  FIRST WE ATTEMPT TO OPTIMIZE GEOMETRY USING SEARCH.
C  IF THIS DOES NOT WORK, THEN SWITCH TO LINMIN, WHICH ALWAYS WORKS,
C  BUT IS TWICE AS SLOW AS SEARCH.
C
      ROUGH=  (   .NOT.  OKF)
      rmx = abs(gmin1(idamax(nvar,gmin1,1)))
      IF(RMX.LT.TOL2) GO TO 430
C        *****  TWO STEP ESTIMATION OF DERIVATIVES         *****
      DO 330 K=1,NVAR
  330 E1(K) = (GMIN1(K)-GNEXT1(K))/(AMIN-ANEXT)
      RMU = ddot(nvar,E1,1,GMIN1,1)/ddot(nvar,gmin1,1,gmin1,1)
      DO 340 K=1,NVAR
  340 E2(K) = E1(K) - RMU*GMIN1(K)
C        *****  SCALE -E2- AND -SIG-                       *****
      SK = 1.0D0/SQRT(ddot(nvar,E2,1,E2,1))
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
         IF (DOPRNT) WRITE(6,410)TLEFT,GMIN,FUNCT                       CIO
  410    FORMAT('  RESTART FILE WRITTEN,  TIME LEFT:',F9.1,
     1' GRAD.:',F10.3,' HEAT:',G14.7)
         RESFIL=.FALSE.
      ELSE
         IF (DOPRNT) WRITE(6,420)ICYC,TSTEP,TLEFT,GMIN,FUNCT            CIO
  420    FORMAT(' CYCLE:',I5,' TIME:',F6.1,' TIME LEFT:',F9.1,
     1' GRAD.:',F10.3,' HEAT:',G14.7)
      ENDIF
      IF(TIMES.AND.DOPRNT) WRITE(6,'('' TIME FOR STEP:'',F8.2,          CIO
     &                           '' LEFT'',F8.2)') TSTEP, TLEFT         CIO
      GO TO 170
  430 CONTINUE
      DO 440 I=1,NVAR
  440 GRAD(I)=0.D0
      LAST=1
      CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.,*9999)    CSTP(call)
      DO 450 I=1,NVAR
  450 GRAD(I)=GMIN1(I)
      GNFINA=SQRT(ddot(nvar,grad,1,grad,1))
      IFLEPO=11
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY POWSQ_INIT                                                  CSAV
             ALPHA = 0.0D0                                              CSAV
                CH = ''                                                 CSAV
             CHDOT = '.'                                                CSAV
                 D = 0.0D0                                              CSAV
             DEBUG = .FALSE.                                            CSAV
                E1 = 0.0D0                                              CSAV
                E2 = 0.0D0                                              CSAV
               EIG = 0.0D0                                              CSAV
             GLAST = 0.0D0                                              CSAV
              GMIN = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
              ICYC = 0                                                  CSAV
                ID = 0                                                  CSAV
                IJ = 0                                                  CSAV
                IK = 0                                                  CSAV
                IL = 0                                                  CSAV
             ILOOP = 0                                                  CSAV
              ILPR = 0                                                  CSAV
              INDC = 0                                                  CSAV
               IP1 = 0                                                  CSAV
              IPOW = 0                                                  CSAV
             ISWAP(1) = 2                                               CSAV
             ISWAP(2) = 3                                               CSAV
             ISWAP(3) = 1                                               CSAV
                 J = 0                                                  CSAV
                 K = 0                                                  CSAV
                 L = 0                                                  CSAV
              NAT3 = 0                                                  CSAV
              NINE = '9'                                                CSAV
               OKC = .FALSE.                                            CSAV
               OKF = .FALSE.                                            CSAV
                 P = 0.0D0                                              CSAV
              PMAX = 0.0D0                                              CSAV
                 Q = 0.0D0                                              CSAV
            RESFIL = .FALSE.                                            CSAV
            RESTRT = .FALSE.                                            CSAV
              RHO2 = 0.0D0                                              CSAV
               RMU = 0.0D0                                              CSAV
               RMX = 0.0D0                                              CSAV
             ROUGH = .FALSE.                                            CSAV
              SCF1 = .FALSE.                                            CSAV
               SIG = 0.0D0                                              CSAV
                SK = 0.0D0                                              CSAV
             SPACE = ' '                                                CSAV
              STEP = 0.0D0                                              CSAV
               SUM = 0.0D0                                              CSAV
             TDUMP = 0.0D0                                              CSAV
               TIM = 0.0D0                                              CSAV
             TIME1 = 0.0D0                                              CSAV
             TIME2 = 0.0D0                                              CSAV
             TIMES = .FALSE.                                            CSAV
             TLAST = 0.0D0                                              CSAV
             TLEFT = 0.0D0                                              CSAV
              TOL2 = 0.0D0                                              CSAV
            TOTIME = 0.0D0                                              CSAV
             TSTEP = 0.0D0                                              CSAV
              WORK = 0.0D0                                              CSAV
              XINC = 0.0D0                                              CSAV
              ZERO = '0'                                                CSAV
      RETURN                                                            CSAV
      END
