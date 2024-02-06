C======================================================================+
      SUBROUTINE TRUSTG (X,F,G,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'                                                   DJG0496
      INCLUDE 'FFILES.i'                                                 DJG0496
      DIMENSION X(*),G(*)
      SAVE
C ---------------------------------------------------------------------+
C     GRADIENT MINIMIZATION BY A TRUST REGION ALGORITHM.
C                                         DL JANUARY 96,  MAY 96.
C  INPUT
C      X(N)       COORDINATES OF STARTING POINT.
C  OUTPUT
C      X(N)       COORDINATES OF ENDING POINT.
C      F, G(N)    ASSOCIATED ENERGY AND GRADIENT.
C      N          # OF VARIABLES                                         DJG0496
C ---------------------------------------------------------------------+
      COMMON /OPTIMI/ IMP,IMP0
     1       /KEYWRD/ KEYWRD
     2       /MESAGE/ IFLEP,ISCF                                         DJG0496
     3       /TIMECM/ TIME0
     4       /PRECI / SCFCV,SCFTOL,EG(3),ESTIM(3),PMAX(3),KTYP(MAXPAR)
     5       /SCRCHR/ GH(MAXPAR),DUM(6*MAXPAR**2+1-MAXPAR)               DL0397
     6       /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(3*NUMATM+4),NCLOSE,NOPEN
     7               ,IDUMMY
     8       /GEOKST/ NATOMS,LABEL(NUMATM),NABC(3*NUMATM)
      COMMON /OPTMCR/ H(MAXHES),V(MAXPAR**2),B(MAXPAR)
     1               ,XOLD(MAXPAR),GOLD(MAXPAR),POLD(MAXPAR),P(MAXPAR)
     2               ,RADIUS,RHOI,RHOS,GNORM,STEP(0:10),COST(0:10)
     3               ,FOST(0:10),GOST(MAXPAR,0:10)
     4               ,HNEW(MAXHES),BNEW(MAXPAR),VNEW(MAXPAR**2)
     5               ,FBEST,XBEST(MAXPAR),GBEST(MAXPAR)
     6       /OPTMCI/ NEW,KMODE,KSTOP,LM(0:10),MINDEX
     7       /OPTMCL/ LGRAD
      CHARACTER*480 KEYWRD
      CHARACTER*5   TMODE(0:2)
      LOGICAL FAIL,PRINT,RESTRT,PURSUE,DBG,LGRAD,NOOPT
      DATA TMODE /'     ','TRUST','QUADR'/
      GRMS()=SQRT(DOT(G,G,N)/DBLE(N))
C
      RESTRT=IRESTA.NE.0                                                 DJG0496
      IF (RESTRT) THEN
         CALL TRUST6 (N,.TRUE.)
         CALL SCOPY (N,XOLD,1,X,1)
      ENDIF
C     FIRST CALL TO COMPFG, DEFINING ACCURACIES.
      FAIL=.FALSE.
      SCFTOL=1D0
      PRINT *, 'TRUSTG'
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
C     GRADIENT' COMPONENT AND RMS NORM: GNORM
      IF (IGCOMP.NE.0) THEN                                              DJG0496
         GNORM=FIGCOM                                                    DJG0496
      ELSE
         GNORM=0.45D0
      ENDIF
C     ACCURACY ON GRADIENT NORM AND LOWER BOUND TO GNORM.
      GPREC=0D0
      DO 10 I=1,N
   10 GPREC=GPREC+EG(KTYP(I))**2
      GPREC=GPREC/DBLE(GNORM)
      GNORM=MAX(GNORM,3D0*SQRT(GPREC))
      KTOLE=MIN(N/3,10)
C     TIME ALLOWED.
      IF (ITLIMI.NE.0) THEN                                              DJG0496
         TLEFT=FITLIM                                                    DJG0496
      ELSE
         TLEFT=3600D0
      ENDIF
C     WILL RESET THE HESSIAN AFTER NEWHES "DIFFICULTIES"
      NEWHES=NINT(MAX(25D0,45D0-0.20D0*DBLE(N)))
      RMSOLD=GRMS()
C     TRUST RADIUS AND BOUNDARIES: RHOI < RADIUS < RHOS.
      RHOI=1D-4
      RHOS=0.5D0
      IF (.NOT.RESTRT) RADIUS=MAX(0.12D0,MIN(RMSOLD*3.9D-3,RHOS))
      IF (PRINT) THEN
C        WRITE HEADER FOR TRUSTG
         WRITE(JOUT,'('' GRADIENT MINIMIZATION, TRUST REGION SCHEME'',
     .   '' IN'',I4,'' VARIABLES.'')') N
         WRITE(JOUT,'('' VERSION 1   (1996)'')')
         WRITE(JOUT,'('' CYCLES LIMIT:'',I5,''  TOLERANCE ON '',
     .   ''GRADIENT COMPONENT:'',F6.3)') ITEMAX,GNORM
         WRITE(JOUT,'('' TRUST RADIUS "RHO" AND BOUNDARIES:''
     .   ,F8.5,'' <'',F8.5,'' < '',F8.5)') RHOI,RADIUS,RHOS
      ENDIF
C
C     INITIALIZE COUNTER, ETC
      KSTOP=0
      NUMAT3=NUMAT*3
      LGRAD=.TRUE.
      FOLD=F
      CALL SCOPY (N,X,1,XOLD,1)
      CALL SCOPY (N,G,1,GOLD,1)
      MINDEX=-1
      ITER=-1
      PAS=RADIUS
      RMS=GRMS()
      GMAX=ABS(G(ISMAX(N,G,1)))
C   ALWAYS TRY TO TAKE 1 GEOMETRY STEP, EVEN IF GRADIANTS ARE LOW        DJG0496
C   SO COMMENT OUT THIS BLOCK OF CODE FROM DLIOTARD                      DJG0496
C      IF (RMS.LT.GNORM.AND.GMAX.LT.GNORM) THEN
C         IFLEP=2                                                         DJG0496
C         RETURN
C      ENDIF
      IF (.NOT.RESTRT) THEN
C        SET UP FIRST HESSIAN FROM 2-BODY MODEL + REFINEMENTS
C        ARGUMENT #5 = 0: ALLOW UNSTABLE MODES.
         CALL TRUST5 (N,X,V,NUMAT3,0,H,B,VNEW)                           DL0496
         KMODE=0
C  DEBUGGER - COMMENT OUT THIS LINE
C         CALL TRUST4 (N,F,X,G,V,FAIL)
         IF (PRINT) WRITE(JOUT,'('' INITIAL HESSIAN FROM 2-BODY '',
     .   ''MODEL AND'',I4,'' MODES REVISED.'')') KMODE
         NEW=0
      ENDIF
      IF (PRINT) THEN
         WRITE(JOUT,'(/'' CYCLE   HEAT OF FORM.    RMS-G    MAX-G '',
     .   '' STEPSIZE    RHO  MOVE  INDEX'')')
         WRITE(JOUT,100) ITER+1,F,RMS,GMAX,0D0,RADIUS,TMODE(0),MINDEX
      ENDIF
      CALL SCOPY (N,0D0,0,P,1)
      IF (I1SCF.EQ.0) WRITE(JOUT,37)                                     DJG0496
 37   FORMAT(/,8X,'CYC : GEOM. CYCLES   T : TIME (S)  ',                 DJG0496
     1      'GEO : CHANGE IN GEOMETRY'                                   DJG0496
     2      ,' (RU) ',/,8X,'GCOMP : LARGEST COMPONENT OF GRADIENT ',     DJG0496
     3      '(KCAL/RU)',/,8X,'HEAT : TOTAL ENERGY (KCAL/MOL)',/,         DJG0496
     4      8X,'1 RU = 1 ANGSTROM OR 1 RADIAN',/)                        DJG0496
C
C     ITERATIONS.
C     -----------
C
   20 ITER=ITER+1
      GMAXOL=GMAX
      IF (ITER.GT.0) THEN
         RMS=GRMS()
         GMAX=ABS(G(ISMAX(N,G,1)))
         IF (RMS.LT.GNORM) KSTOP=KSTOP+1
         IF (PRINT) WRITE(JOUT,100) ITER,F,RMS,GMAX,PAS,RADIUS
     .   ,TMODE(MODE),MINDEX
      ENDIF
      IF (IMP.GT.2) THEN
         WRITE(JOUT,'('' COORDINATES:'',8F8.4/(5X,9F8.4))')(X(I),I=1,N)
         WRITE(JOUT,'('' GRADIENT   :'',8F8.3/(5X,9F8.3))')(G(I),I=1,N)
      ENDIF
C     DO WE STOP HERE?
      CALL CONVCK(GNORM,FFNCT1,FFNCT2,F,N,ICONFG)                        DJG0496
C     REDONE TO CONFORM WITH AMSOL CONVERGENCE CRITERIA                  DJG0496
C      IF (RMS.LT.GNORM.AND.(GMAX.LT.GNORM.OR.KSTOP.GT.KTOLE)) THEN
      IF (ICONFG.EQ.0) THEN                                              DJG0496
         IFLEP=11                                                        DJG0496
         LAST=1
         RETURN
      ELSE IF (ITER.GT.ITEMAX) THEN
         IFLEP=12                                                        DJG0496
         LAST=1
         RETURN
      ELSE IF (FAIL) THEN
         IFLEP=9                                                         DJG0496
         LAST=1
         RETURN
      ELSE IF (PAS.EQ.0D0.AND.KMODE.GT.N-2.AND.NEW.LT.0) THEN
         IF (PRINT) THEN
            WRITE(JOUT,'('' GRADIENT NOT IMPROVED AND '',
     .      ''HESSIAN NEARLY RESETED:'')')
            WRITE(JOUT,'('' LACK OF ACCURACY ON GRADIENT OR '',
     .      '' DISSOCIATIVE CHANNEL.'')')
         ENDIF
         IFLEP=9                                                         DJG0496
         RETURN
      ENDIF
      CALL PORCPU(TIME)
      TIME=TIME-TIME0
      FACTOR=DBLE(ITER+3)/DBLE(ITER+1)
      WRITE (JOUT,27) ITER,TIME,PAS,GMAX, F                              DJG0496
27    FORMAT(' CYC:',I3,'    T:',F8.1,'    GEO: ',F8.6,'    GCOMP:'      DJG0496
     1       ,F8.3,'   HEAT:',F12.6)                                     DJG0496
      IF (NEW.GE.NEWHES) FACTOR=DBLE(ITER+N)/DBLE(ITER+1)
      IF (TIME*FACTOR.GE.TLEFT) THEN
         CALL TRUST6 (N,.FALSE.)
         RETURN
      ENDIF
C
C     UPDATE THE HESSIAN
      IF (NEW.GE.NEWHES.OR.PAS.EQ.0D0) THEN
         CALL HQRII (H,N,N,B,V)
         KMODE1=MAX(1,N/3)
         KMODE=MIN(KMODE+KMODE1,N)
         IF (PAS.EQ.0D0) KMODE=N
         CALL TRUST4 (N,F,X,G,V,FAIL)
         IF (RADIUS.LT.RHOI*5D0) RADIUS=MIN(RADIUS*5D0,RHOS)
         IF (PRINT) WRITE(JOUT,'('' HESSIAN REVISED IN'',I4,'' MODES''
     .   )') KMODE
         RMS=GRMS()
         NEW=-(2*KMODE)/3
      ELSE
C        UPDATE BY BFGS (INDEX 0) OR POWELL (OTHERS).
C        IUPH= . . .       1                   2
         IUPH=2
         IF (MINDEX.EQ.0.AND.F.LE.FOLD) IUPH=1
         CALL TRUST1 (N,XOLD,GOLD,X,G,H,GPREC,NEW,IUPH)
      ENDIF
C     STORE COORDINATES AND GRADIENT FOR FURTHER USE
      CALL SCOPY (N,X,1,XOLD,1)
      CALL SCOPY (N,G,1,GOLD,1)
      FOLD=F
      RMSOLD=RMS
C
C     CHOICE OF SEARCH DIRECTION "P" AND STEPSIZE "PAS".
      IBACK=0
      CALL SCOPY (N,P,1,POLD,1)
   40 CALL TRUST2 (N,X,F,G,P,H,V,B,RADIUS,PAS,MODE,PRED1,PRED2)
C
C     X(NEW) = X(OLD)+PAS*P; PAS < RADIUS.
C     STEPSIZE NOT OPTIMIZED, UNLESS REPEATED FAILURE.
      CALL SAXPY (N,PAS,P,1,X,1)
      CALL COMPFG (X,F,FAIL,G,.TRUE.)
      RMS=GRMS()
      IF (RMS.GE.RMSOLD.OR.FAIL) THEN
         IF (IBACK.LT.6) THEN
C           UPDATE HESSIAN, TRY AGAIN.
            IBACK=IBACK+1
            IF (RMS.GT.GNORM) NEW=NEW+MODE
            IF (.NOT.FAIL) THEN
               IUPH=2
               IF (MINDEX.EQ.0.AND.F.LE.FOLD) IUPH=1
               CALL TRUST1 (N,XOLD,GOLD,X,G,H,GPREC,NEW,IUPH)
            ENDIF
            F=FOLD
            CALL SCOPY (N,XOLD,1,X,1)
            CALL SCOPY (N,GOLD,1,G,1)
            GOTO 40
         ENDIF
C        LINEAR SEARCH TO DECREASE THE GRADIENT.
         PAS2=MAX(PAS*3D-1,5D-1*PRED1*PAS**2/(RMSOLD-RMS+PRED1*PAS))
         RADIUS=MAX(RHOI,PAS*0.9D0)
         IBACK=0
   50    IF (RMS.GT.GNORM) THEN
            NEW=NEW+MODE
            NOOPT=DOT(P,POLD,N).GE.0D0
         ELSE
            NOOPT=.FALSE.
         ENDIF
         F=FOLD
         CALL SCOPY (N,XOLD,1,X,1)
         CALL SCOPY (N,GOLD,1,G,1)
         PAS=MIN(PAS2,RADIUS)
         CALL TRUST3 (DBG,N,X,F,PAS,SECU,NOOPT,FAIL,G,PRED1)
         IF (PAS.EQ.0D0) THEN
            IF (IBACK.GT.0) GOTO 20
C           HESSIAN POOR, TRY THE OPPOSITE WAY.
            IBACK=1
            CALL DSCAL (N,-1D0,P,1)                                      DJG0496
            PAS2=PAS2*0.2D0
            PRED1=-PRED1
            GOTO 50
         ENDIF
         CALL COMPFG (X,F,FAIL,G,.TRUE.)
         RMS=GRMS()
         IF (RMS.GE.RMSOLD.OR.FAIL) THEN
C           BACK TO A SAFE STEPSIZE FOUND IN "TRUST3"
            CALL SAXPY (N,SECU,P,1,X,1)
            PAS=PAS-SECU
            F=FOST(LM(0))
            CALL SCOPY (N,GOST(1,LM(0)),1,G,1)
            RMS=GRMS()
         ENDIF
      ENDIF
C
C     UPDATE TRUST RADIUS & HESSIAN' OBSOLESCENCE COUNTER.
C     OBSOLESCENCE INCREASED IF POOR DECREASE OF GNORM (90%).
      IF (RMS.GT.MAX(GNORM,RMSOLD*0.90D0)) THEN
         NEW=NEW+MODE
         IF (RADIUS.LE.RHOI*5D0) NEW=NEW+MODE
      ENDIF
C     TRUST RADIUS: STEPSIZE MINIMIZING RMS ALONG P.
      CALL SUPDOT (GH,H,G,N,1)
      TAU=-PRED1/(ABS(2D0*DOT(P,GH,N)-PRED1)+1D-30)
      IF (NEW.LT.0.AND.PAS.LT.RHOI*20D0) THEN
         TAU=MAX(0.50D0,MIN(TAU,4D0))
      ELSE
         TAU=MAX(0.50D0,MIN(TAU,2D0))
      ENDIF
      RADIUS=MAX(RHOI,MIN(PAS*TAU,RHOS))
      GOTO 20
  100 FORMAT (I5,F16.6,1P,D10.2,D9.2,0P,F10.5,F7.5,A6,I5)
      END


