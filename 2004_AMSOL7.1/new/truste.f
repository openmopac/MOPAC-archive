      SUBROUTINE TRUSTE (X,F,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'                                                   DJG0496
      INCLUDE 'FFILES.i'                                                 DJG0496
      DIMENSION X(*)
      SAVE
C  ---------------------------------------------------------------------+
C     ENERGY MINIMIZATION BY A TRUST REGION ALGORITHM.
C                                         DL NOVEMBER 95,  MAY 96.
C  INPUT
C      X(N)       COORDINATES OF STARTING POINT.
C  OUTPUT
C      X(N)       COORDINATES OF ENDING POINT.
C      F, G(N)    ASSOCIATED ENERGY AND GRADIENT.
C      N          NUMBER OF DEGREES OF FREEDOM
C  ---------------------------------------------------------------------+
      COMMON /OPTIMI/ IMP,IMP0
     2       /MESAGE/ IFLEP,ISCF                                         DJG0496
     3       /TIMECM/ TIME0
     4       /PRECI / SCFCV,SCFTOL,EG(3),ESTIM(3),PMAX(3),KTYP(MAXPAR)
     5       /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(3*NUMATM+4),NCLOSE,NOPEN
     6               ,IDUMMY
     7       /GEOKST/ NATOMS,LABEL(NUMATM),NABC(3*NUMATM)
     8       /LAST  / LAST
      COMMON /OPTMCR/ H(MAXHES),V(MAXPAR**2),B(MAXPAR)
     1               ,XOLD(MAXPAR),GOLD(MAXPAR),POLD(MAXPAR),P(MAXPAR)
     2               ,RADIUS,RHOI,RHOS,GNORM,STEP(0:10),COST(0:10)
     3               ,FOST(0:10),GOST(MAXPAR,0:10)
     4               ,HNEW(MAXHES),BNEW(MAXPAR),VNEW(MAXPAR**2)
     5               ,FBEST,XBEST(MAXPAR),GBEST(MAXPAR)
     6       /OPTMCI/ NEW,KMODE,KSTOP,LM(0:10),MINDEX
     7       /OPTMCL/ LGRAD
      COMMON /GRADNT/ G(MAXPAR),GNRM2
      CHARACTER*5   TMODE(0:2)
      LOGICAL FAIL,PRINT,RESTRT,PURSUE,DBG,LGRAD
      DATA TMODE /'     ','TRUST','QUADR'/
C
      RESTRT=IRESTA.NE.0                                                 DJG0496
      IF (RESTRT) THEN
         CALL TRUST6 (N,.TRUE.)
         CALL SCOPY (N,XOLD,1,X,1)
      ENDIF
      PRINT *, 'TRUSTE'
C     FIRST CALL TO COMPFG, DEFINING ACCURACIES.
      FAIL=.FALSE.
      SCFTOL=1D0
      CALL COMPFG (X,F,FAIL,G,.TRUE.)                                    DJG0696
      IF (N.EQ.0) THEN                                                   DJG0496
         IFLEP=10                                                        DJG0496
         RETURN                                                          DJG0496
      ENDIF                                                              DJG0496
      IF (FAIL) THEN
         IFLEP=9                                                         DJG0496
         RETURN
      ENDIF
C
C     OPTIONS FROM KEYWORDS AND/OR DEFAULTS.
C
C     PRINTOUT LEVEL
      IF (IPRINT.NE.0) IMP=IIPRIN                                        DJG0496
      IMP0=IMP
      PRINT=IMP.GT.0
      DBG=IMP.GT.3
C     MAXIMUM NUMBER OF CYCLES: ITEMAX.
      IF (ICYCLE.NE.0) THEN                                              DJG0496
         ITEMAX=IICYCL                                                   DJG0496
      ELSE
         ITEMAX=4*N+10
      ENDIF
C     GRADIENT' COMPONENT AND RMS NORM.
      IF (IGCOMP.NE.0) THEN                                              DJG0496
         GNORM=FIGCOM                                                    DJG0496
      ELSE
         GNORM=0.45D0
      ENDIF
C     ACCURACY ON GRADIENT NORM AND LOWER BOUND TO GNORM.
      GPREC=0D0
      DO 10 I=1,N
   10 GPREC=GPREC+EG(KTYP(I))**2
      GPREC=GPREC/DBLE(N)
C      GNORM=MAX(GNORM,3D0*SQRT(GPREC))
      IF (3.0D0*SQRT(GPREC).GT.GNORM) THEN                               DJG0496
         GNORM=3.0D0*SQRT(GPREC)                                         DJG0496
         WRITE(JOUT,15) GNORM                                            DJG0496
   15    FORMAT (/,5X,'GRADIENT COMPONENT TOLERENCE RESET TO LOWER',     DJG0496
     1           ' BOUND FOR THIS MOLECULE',/,5X,'GCOMP = ',F7.4)        DJG0496
      ENDIF                                                              DJG0496
C     TOLERANCE ON INCREASE IN ENERGY: ETOL.
      ETOL=MIN(0.1D0,SCFCV*1D2)
C     TIME ALLOWED
      IF (ITLIMI.NE.0) THEN                                              DJG0496
         TLEFT=FITLIM                                                    DJG0496
      ELSE
         TLEFT=3600D0
      ENDIF
C     WILL RESET THE HESSIAN AFTER NEWHES "DIFFICULTIES"
      NEWHES=NINT(MAX(25D0,45D0-0.20D0*DBLE(N)))
      RMSOLD=SQRT(DOT(G,G,N)/DBLE(N))
C     TRUST RADIUS AND BOUNDARIES: RHOI < RADIUS < RHOS.
      RHOI=1D-4
      RHOS=0.5D0
      IF (.NOT.RESTRT) RADIUS=MAX(0.12D0,MIN(RMSOLD*3.9D-3,RHOS))
      IF (PRINT) THEN
C        WRITE HEADER FOR TRUST
         WRITE(JOUT,'('' ENERGY MINIMIZATION, TRUST REGION SCHEME'',
     .   '' IN'',I4,'' VARIABLES.'')') N
         WRITE(JOUT,'('' VERSION 1   (1995)'')')
         WRITE(JOUT,'('' CYCLES LIMIT:'',I5,''  TOLERANCE ON '',
     .   ''GRADIENT COMPONENT:'',F6.3)') ITEMAX,GNORM
         WRITE(JOUT,'('' TRUST RADIUS "RHO" AND BOUNDARIES:''
     .   ,F8.5,'' <'',F8.5,'' < '',F8.5)') RHOI,RADIUS,RHOS
      ENDIF
      IF (I1SCF.EQ.0) WRITE(JOUT,37)                                     DJG0496
 37   FORMAT(/,8X,'CYC : GEOM. CYCLES   T : TIME (S)  ',                 DJG0496
     1      'GEO : CHANGE IN GEOMETRY'                                   DJG0496
     2      ,' (RU) ',/,8X,'GCOMP : LARGEST COMPONENT OF GRADIENT ',     DJG0496
     3      '(KCAL/RU)',/,8X,'HEAT : TOTAL ENERGY (KCAL/MOL)',/,         DJG0496
     4      8X,'1 RU = 1 ANGSTROM OR 1 RADIAN',/)                        DJG0496
C
C     INITIALIZE COUNTER, ETC.
      LGRAD=.FALSE.
      FOLD=F
      CALL SCOPY (N,X,1,XOLD,1)
      CALL SCOPY (N,G,1,GOLD,1)
      MINDEX=-1
      ITER=-1
      PAS=RADIUS
      IF (.NOT.RESTRT) THEN
C        SET UP FIRST HESSIAN FROM 2-BODY MODEL.
         NEW=0
         NUMAT3=NUMAT*3
C        ARGUMENT #5 = 1: DEF. POSITIVE.
         CALL TRUST5 (N,X,V,NUMAT3,1,H,B,VNEW)                            1203BL04
      ENDIF
      GNRM2=DOT(G,G,N)
      RMS=SQRT(GNRM2/DBLE(N))
      GMAX=ABS(G(ISAMAX(N,G,1)))                                          DJG1196
      IF (PRINT) THEN
         WRITE(JOUT,'(/'' CYCLE   HEAT OF FORM.    RMS-G    MAX-G '',
     .   '' STEPSIZE    RHO  MOVE  INDEX'')')
         WRITE(JOUT,'(I5,F16.6,1P,D10.2,D9.2,0P,F10.5,F7.5,A6,I5)')
     .   ITER+1,F,RMS,GMAX,0D0,RADIUS,TMODE(0),0
      ENDIF
C
C     ITERATIONS.
C     -----------
C
   20 ITER=ITER+1
      IF (ITER.GT.0) THEN
         GNRM2=DOT(G,G,N)
         RMS=SQRT(GNRM2/DBLE(N))
         GMAX=ABS(G(ISAMAX(N,G,1)))                                      DJG1196
         IF (PRINT) WRITE(JOUT,'(I5,F16.6,1P,D10.2,D9.2,0P,F10.5,F7.5,A6
     .   ,I5)') ITER,F,RMS,GMAX,PAS,RADIUS,TMODE(MODE),MINDEX
      ENDIF
      IF (IMP.GT.2) THEN
         WRITE(JOUT,'('' COORDINATES:'',8F8.4/(5X,9F8.4))')(X(I),I=1,N)
         WRITE(JOUT,'('' GRADIENT   :'',8F8.3/(5X,9F8.3))')(G(I),I=1,N)
      ENDIF
C     DO WE STOP HERE?
C     CHANGED TO AMSOL CONVERGENCE CRITERIA                              DJG0496
       IF (RMS.LT.GNORM.AND.GMAX.LT.GNORM) THEN
c     CALL CONVCK(GNORM,FFNCT1,FFNCT2,F,N,ICONFG)                        DJG0496
c     IF (ICONFG.EQ.0) THEN                                              DJG0496
c        WRITE(JOUT,25) GMAX, GNORM                                      DJG0496
25       FORMAT(/,5X,'ABSOLUTE VALUE OF LARGEST GRADIENT COMPONENT =',   DJG0496
     1          F9.5,/,5X,'IS LESS THAN CUTOFF =', 1F9.5,                DJG0496
     2         ' AND THE CALCULATED ENERGY',/,5X,'CHANGED LESS THAN ',   DJG0496
     3          '.1 KCAL/MOL FOR LAST GEOMETRY CHANGE.',/,/)             DJG0496
         IFLEP=10                                                        DJG0496
         LAST=1
         RETURN
      ELSE IF (ITER.GT.ITEMAX) THEN
         IFLEP=12                                                        DJG0496
         LAST=1
         RETURN
      ELSE IF (PAS.EQ.0D0.OR.FAIL) THEN
         F=FOLD
         CALL SCOPY (N,XOLD,1,X,1)
         CALL SCOPY (N,GOLD,1,G,1)
         IFLEP=9                                                         DJG0496
         LAST=1
         RETURN
      ENDIF
      CALL PORCPU(TIME)
      TIME=TIME-TIME0
      FACTOR=DBLE(ITER+3)/DBLE(ITER+1)
      WRITE (JOUT,27) ITER,TIME,PAS,GMAX, F                              DJG0496
27    FORMAT(' CYC:',I3,'    T:',F8.1,'    GEO: ',F8.6,'    GCOMP:'      DJG0496
     1       ,F8.3,'   HEAT:',F12.6)                                     DJG0496
      IF (NEW.GT.NEWHES-2) FACTOR=DBLE(ITER+5)/DBLE(ITER+1)
      IF (TIME*FACTOR.GE.TLEFT) THEN
         CALL TRUST6 (N,.FALSE.)
         RETURN
      ENDIF
C
C     UPDATE THE HESSIAN
      IF (NEW.GE.NEWHES) THEN
         IF (PRINT) WRITE(JOUT,'('' HESSIAN RESETED'')')
         CALL TRUST5 (N,X,V,NUMAT3,1,H,B,VNEW)                           1203BL04
         NEW=0
      ENDIF
C     UPDATE BY BFGS (FOUND MORE EFFICIENT THAN POWELL).
      CALL TRUST1 (N,XOLD,GOLD,X,G,H,GPREC,NEW,1)
C     STORE COORDINATES AND GRADIENT FOR FURTHER USE
      CALL SCOPY (N,X,1,XOLD,1)
      CALL SCOPY (N,G,1,GOLD,1)
      FOLD=F
C
C     CHOICE OF SEARCH DIRECTION "P" AND STEPSIZE "PAS".
      IBACK=0
   30 CALL TRUST2 (N,X,F,G,P,H,V,B,RADIUS,PAS,MODE,PRED1,PRED2)
C
C     X(NEW) = X(OLD)+PAS*P; PAS < RADIUS.
      RMSOLD=RMS
C     STEPSIZE NOT OPTIMIZED, UNLESS REPEATED FAILURE.
      CALL SAXPY (N,PAS,P,1,X,1)
      CALL COMPFG (X,F,FAIL,G,.TRUE.)
      IF (F.GE.FOLD+ETOL.OR.FAIL) THEN
         NEW=NEW+MODE
         IF (IBACK.LT.2) THEN
C           UPDATE HESSIAN, REDUCE RADIUS, TRY AGAIN.
            IBACK=IBACK+1
            IF (.NOT.FAIL) CALL TRUST1 (N,XOLD,GOLD,X,G,H,GPREC,NEW,1)
            F=FOLD
            CALL SCOPY (N,XOLD,1,X,1)
            CALL SCOPY (N,GOLD,1,G,1)
            RADIUS=MAX(RHOI,MIN(PAS*0.70D0,RHOS))
            GOTO 30
         ENDIF
C        ROUGH LINEAR SEARCH REQUIRED TO DECREASE THE ENERGY.
         PAS2=MAX(PAS*1D-1,5D-1*PRED1*PAS**2/(FOLD-F+PRED1*PAS))
         RADIUS=MAX(RHOI,MIN(PAS2*3D0,PAS*0.9D0))
         F=FOLD
         CALL SCOPY (N,XOLD,1,X,1)
         CALL SCOPY (N,GOLD,1,G,1)
         PAS=MIN(PAS2,RADIUS)
         CALL TRUST3 (DBG,N,X,F,PAS,SECU,.TRUE.,FAIL,G,PRED1)
         IF (PAS.EQ.0D0) THEN
C           GRADIENT ROUND-OFF DOMINATED. TRY OPPOSITE BEFORE TO DIE.
            CALL SCOPY (N,GOLD,1,G,1)
            CALL DSCAL (N,-1D0,P,1)                                      DJG0597
            PRED1=-PRED1
            PAS=MIN(PAS2,RADIUS)*0.2D0
            CALL TRUST3 (DBG,N,X,F,PAS,SECU,.TRUE.,FAIL,G,PRED1)
            IF (PAS.EQ.0D0) THEN
C              WE ARE DEAD.
               CALL SCOPY (N,GOLD,1,G,1)
               GOTO 20
            ENDIF
         ENDIF
         CALL COMPFG (X,F,FAIL,G,.TRUE.)
         IF (F.GE.FOLD+ETOL.OR.FAIL) THEN
            IF (.NOT.FAIL) CALL TRUST1 (N,XOLD,GOLD,X,G,H,GPREC,NEW,1)
C           BACK TO A STABLE STEPSIZE FOUND IN "TRUST3"
            CALL SAXPY (N,SECU,P,1,X,1)
            CALL COMPFG (X,F,FAIL,G,.TRUE.)
            PAS=PAS-SECU
         ENDIF
      ENDIF
C
C     UPDATE TRUST RADIUS & HESSIAN' OBSOLESCENCE COUNTER.
c      PRED=(PRED2*PAS+PRED1)*PAS
c      RATIO=(F-FOLD-1D-2)/(PRED-1D-2)
C     INCREMENT HESSIAN OBSOLESCENCE COUNTER IF:
C     A- GNORM INCREASES AND TRUST RADIUS IS SMALL.
C     B- POOR DECREASE OF GNORM WITH NEWTON STEP (80%).
cC     C- RATIO PREDICTED/OBSERVED IS POOR (1/2 ... 5/4).
      IF (RADIUS.LE.RHOI*5D0.AND.RMS.GT.RMSOLD) NEW=NEW+MODE
      IF (MODE.EQ.2.AND.RMS.GT.RMSOLD*0.80D0) NEW=NEW+MODE
c      IF (RATIO.LT.0.50D0.OR.RATIO.GT.1.25D0) THEN
c         NEW=NEW+MODE
cC        TRUST RADIUS REDUCED.
c         TAU=0.5D0
c      ELSE
C        TRUST RADIUS: SEARCH FOR ORTHOGONALITY OF P(I), P(I+1).
         TAU=-PRED1/(ABS(DOT(P,G,N)-PRED1)+1D-30)
         TAU=MAX(0.50D0,MIN(TAU,2.0D0))
c      ENDIF
      RADIUS=MAX(RHOI,MIN(PAS*TAU,RHOS))
      GOTO 20
      END

