      SUBROUTINE DERIV(GRAD,FAIL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
       PARAMETER (MFBWO=9*MAXPAR, MSIZE=NUMATM*(NUMATM-1)/2)            DJG0496
C----------------------------------------------------------------------
C
C    DERIV CALCULATES THE DERIVATIVES OF THE ENERGY WITH RESPECT TO THE
C          OPTIMIZED PARAMETERS XPARAM(NVAR).
C
C    IMPLEMENTATION OF ANALYTICAL FORMULATION FOR OPEN SHELL OR CI,
C                      VARIABLES FINITE DIFFERENCE METHODS,
C                      STATISTICAL ESTIMATE OF THE ERRORS,
C                   BY D. LIOTARD (MJS DEWAR GROUP, FEB-SEPTEMBER 1986)
C    REVISED BY DL, MAY 96  (STEPSIZE AND /HBODY2/).
C    REVISED BY DL, MARCH 97 (STEPSIZE & ANALYTICAL GRADIENT FOR SM5.).
C
C                * * * WHAT CAN BE DONE: * * *
C
C==> IF THE WAVE FUNCTION IS A RHF CLOSED SHELL DETERMINANT OR UHF,THE
C    DERIVATIVES OF THE CORE-CORE REPULSION,CORE HAMILTONIAN AND 2-EL
C    INTEGRALS ARE EVALUATED IN CARTESIAN BY A 1 OR 2 POINTS FINITE
C    DIFFERENCE FORMULA AND THE ENERGY GRADIENT IS COMPUTED USING
C    THE SCF-CONVERGED DENSITY MATRIX (SUBROUTINES DCART, DCART2).
C
C==> IF THE WAVE FUNCTION IS NOT SO SIMPLE, I.E. HALF-ELECTRON OR CI,
C    THE DERIVATIVES OF THE 1 AND 2-ELECTRON INTEGRALS IN A.O. BASIS
C    ARE EVALUATED IN CARTESIAN COORDINATES BY A 1 OR 2 POINTS FINITE
C    DIFFERENCE FORMULA AND STORED. THUS ONE GETS THE NON-RELAXED
C    (I.E. WITH FROZEN ELECTRONIC CLOUD) CONTRIBUTION TO THE FOCK
C    EIGENVALUES AND 2-ELECTRON INTEGRALS IN M.O. BASIS.
C    THE NON-RELAXED GRADIENT ISSUES FROM THE NON-RELAXED C.I. MATRIX
C    DERIVATIVE (SUBROUTINE DERI1).
C    THEN THE DERIVATIVES OF THE M.O. COEFFICIENTS ARE WORKED OUT
C    ITERATIVELY (OK FOR BOTH CLOSED SHELLS AND HALF-ELECTRON CASES)
C    AND STORED. THUS ONE GETS THE ELECTRONIC RELAXATION CONTRIBUTION TO
C    THE FOCK EIGENVALUES AND 2-ELECTRON INTEGRALS IN M.O. BASIS.
C    FINALLY THE RELAXATION CONTRIBUTION TO THE C.I. MATRIX DERIVATIVE
C    GIVES THE RELAXATION CONTRIBUTION TO THE GRADIENT (ROUTINE DERI2).
C
C    THE GRADIENT IS THEN BACK TRANSFORMED INTO THE INTERNAL COORDINATES
C    via A JACOBIAN MATRIX WORKED OUT BY A 1 OR 2 POINTS FINITE
C    DIFFERENCE FORMULA (SUBROUTINE JCARIN).
C
C==> IF THE KEYWORD 'DERINU' IS SWITCHED ON, THE GRADIENT IN INTERNAL
C    COORDINATES IS CALCULATED BY A 1 OR 2 POINTS FINITE DIFFERENCE
C    ON THE TOTAL ENERGY (SUBROUTINE DERIV).
C    THIS OPTION CAN BE USED AS A CHECK OF ACCURACY AND SPEED OF THE
C    TWO PREVIOUS OPTIONS AND ALLOWS FOR ANY CHANGE IN THE FORMALISM.
C    IT IS ALSO USED IF THE LACK OF CORE MEMORY INHIBIT THE COMPUTATION
C    OF THE DERIVATIVES OF THE M.O. COEFFICIENTS IN 'DERI2'.
C
C NOTE ... THE FINITE DIFFERENCE METHOD IS SELECTED ACCORDING TO THE
C          KEYWORDS 'FORWARD' OR ONE OF THOSE IMPLYING THE COMPUTATION
C          OF AN ACCURATE MOLECULAR HESSIAN.
C
C    THE MAIN ARRAYS IN DERIV ARE:
C        LOC    LOC(1,I),LOC(2,I) GIVE THE ADDRESS (ROW,COLUMN) OF THE
C               INTERNAL COORDINATE #I TO BE USED IN THE GRADIENT
C               CALCULATION.
C        GEO    HOLDS THE INTERNAL  COORDINATES.
C        COORD  HOLDS THE CARTESIAN COORDINATES.
C    INPUT
C        GRAD   NOT DEFINED.
C        FAIL   LOGICAL, NOT DEFINED.
C    EXIT
C        GRAD   GRADIENT VECTOR (KCAL/RD,ANGSTROM), LENGTH NVAR.
C        FAIL   .TRUE. IF GRADIENT ACCURACY NOT ASCERTAINED,
C               .FALSE. OTHERWISE.
C
C----------------------------------------------------------------------
      COMMON /GEOVAR/ DUMMY(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY            0330GL92
      COMMON / EULER/ TVEC(3,3), ID
     1       /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM)   0330GL92
     2               ,NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA
     3               ,NCLOSE,NOPEN,NDUMY                                0330GL92
     4       /MLKSTR/ FRACT                                             0330GL92
     5       /GEOKST/ NATOMS,LABELS(NUMATM)                             0330GL92
     6               ,NA(NUMATM),NB(NUMATM),NC(NUMATM)                  0330GL92
     7       /GEOSYM/ NDEP, IDUMYS(MAXPAR,3)                            0330GL92
     8       /GEOM  / GEO(3,NUMATM)                                     0330GL92
     9       /UCELL / L1L,L2L,L3L,L1U,L2U,L3U,KDUM(6)                   0330GL92
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
     1       /WMATRX/ WJ(N2ELEC),WK(N2ELEC*2),NUMBW,NBAND(NUMATM)
     2       /HMATRX/ H(MPACK)
     3       /ELECT / ELECT
     4       /ENUCLR/ ENUCLR
     5       /PRECI / SCFCV,SCFTOL,EG(3),ESTIM(3),PMAX(3),KTYP(MAXPAR)
     6       /MESAGE/ IFLEP, ISCF                                       DJG0995
     7       /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
     8       /OPTIMI / IMP,IMP0
     9       /SCRAH2/ BWO(NRELAX*MORB2+1)                               GCL0892
      COMMON /SCRAH1/ SCALAR(MPACK/2),DIAG(MPACK/2),FRACT2,FBWO(MFBWO),
     1                NBO(3),IDUM
     2       /CIDATA/ VECTCI(MAXCI),XXCI,NCI1,NCI2,NCI3,                DL0397
     3                NCIDUM((1+2*NMECI)*MAXCI)                         DL0397
     4       /LAST  / LAST
     5       /HBODY2/ RG(3,MSIZE),BJACOB(MAXPAR**2),COORD(3,NUMATM)
     6       /SCRCHR/ COLD(3,NUMATM*27),DXYZ(3,NUMATM*27),              0330GL92
     7                DUM3(6*MAXPAR**2+1-6*NUMATM*27)                   DL0397
     8       /AREACM/ NOPTI, TAREA,NINTEG                               GDH0993
     9       /ONESCM/ ICONTR(100)                                       GDH0195
      COMMON /MODLCM/ SMODEL                                            DJG0496
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL0397
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL0397
     2               ,DATLGR(3,NUMATM)                                  DL0397
     3               ,LGR,FLAGRD                                        DL0397
                      LOGICAL LGR,FLAGRD                                DL0397
      DIMENSION CHANGE(3),EMAX(3),LMAX(3),XPARAM(MAXPAR)                DL0397
      DIMENSION W(1),GRAD(*),CT(MORB2)
      LOGICAL DEBUG, TIMES, HALFE, FAST, CI, FAIL, FORWRD,              DJG0495
     .       CLSD,NUM,ADJUST,CIJRDY,TAREA,SMODEL                        DJG0496
      EQUIVALENCE (W,WJ)
      DATA NUMCAL /1/                                                   DJG0295
C
C     SELECT THE REQUIRED OPTION AND READ KEYWORDS
C     --------------------------------------------
C
      SAVE
      IF (ICONTR(21).EQ.1) THEN                                         GDH0195
         ICONTR(21)=2                                                   GDH0195
         ICALCN=0                                                       GDH0195
         NSLV0=0
         NSLV1=0                                                        DJG0496
         IF (SMODEL.AND..NOT.FLAGRD) NSLV1=1                            DL0397
         NSLV2=INOPOL+ISM50                                             GDH0597
         IF (NSLV1.GT.0.AND.NSLV2.EQ.0) NSLV0=1                         GDH0993
         DEBUG = (IDERIV.NE.0)                                          DJG0995
         TIMES = (ITIMES.NE.0)                                          DJG0995
         CI    = (ICI.NE.0)                                             DJG0995
         HALFE = (NOPEN.GT.NCLOSE)
         FORWRD = (IFORWR.NE.0)                                         DJG0995
         CLSD=.NOT.(HALFE.OR.CI)
      ENDIF                                                             GDH0195
   1  CONTINUE
      FAST=NSLV0.EQ.0 .AND.ICALCN.NE.-1                                 GDH0993
      IF (IDERIN.GT.0) FAST=.FALSE.                                     DL0397
      IF (.NOT.CLSD) THEN
C        ACTUAL SIZES FOR C.I. GRADIENT CALCULATION.
         NBO(1)=NCLOSE
         NBO(2)=NOPEN-NCLOSE
         NBO(3)=NORBS-NOPEN
         LINEAR=NORBS*(NORBS+1)/2
         MINEAR=NBO(2)*NBO(1)+NBO(3)*NOPEN
         NINEAR=NCI2
         NEND=0
         DO 2 LOOP=1,3
         NINIT=NEND+1
         NEND =NEND+NBO(LOOP)
         N1=MAX(0,NCI1-NINIT)
         N2=MIN(NEND ,NCI1+NCI2)-NINIT
         IF(N2.GT.N1) NINEAR=NINEAR+(N2*(N2+1)-N1*(N1+1))/2
    2    CONTINUE
         NCOL=N2+1
         N2=N2+NINIT
         IF (NCOL.GT.0.AND.N2.LT.NORBS) THEN
            NINEAR=NINEAR+NCOL*(NORBS-N2)
         ELSE IF (NINEAR.LE.0) THEN
            NINEAR=1
         ENDIF
         J=0
         NUMBW=0
         DO 3 I=1,NUMAT
         J=MAX(J,NBAND(I))
    3    NUMBW=NUMBW+MAX(0,NBAND(I))
         NUMBW=J*(NUMBW-J)+1
         LINF=NINEAR+MINEAR
         LINB=NINEAR+MINEAR*2
         FRACT2=FRACT
C        THE ANALYTICAL COMPUTATION OF THE GRADIENT OF A C.I. FUNCTION,
C        WITHOUT I/O ON A SCRATCH FILE, REQUIRES A LARGE CORE MEMORY
C        PROVIDED BY /SCRAH2/.
C        /SCRAH1/ CONTAINS DATA TO SPEED UP CONVERGENCE,
C        /SCRACH/ IS USED AS A WORK AREA OF SIZE ROUGHLY = MORB2.
C        UP TO 30% OF THE CPU TIME IS SAVED IF ONE CAN COMPUTE THE
C        RELAXATION OF MBAND GRADIENT COMPONENTS AT A TIME:
C        MBAND=9 GIVES USUALLY OPTIMUM RESULTS IN SPEED AND MBAND SHOULD
C        BE A MULTIPLE OF 3 FOR BEST ACCURACY IN CARTESIAN COORD.
C        TAKING INTO ACCOUNT THE LIMIT DUE TO FBW AND BAB IN DERI2,
C        MBAND SHOULD BE GIVEN BY (J IS THE LIMIT DUE TO 'OSINV' )
         J=60
         MBAND=MIN(9,J/3,MFBWO/MAX(NVAR,NINEAR),NVAR)
C        AND THE d<IJ!KL> TRANSFORMATION REQUIRE
         NIJKL=NORBS*NCI2*NCI2*(NCI2+1)/2
         NFREE=NRELAX*MORB2-NIJKL
C        THEREFORE /SCRAH2/ BEEING USED IN DERI2 AS:
C        //DIJKL(NIJKL),F(MBAND*LINF),B(MAXITE*LINB)
C        MAXITE CANNOT EXCEED
         MAXITE=(NFREE-MBAND*LINF)/LINB
C        ELSEWHERE, THE DIMENSION OF THE BASIS SET WHEN THE RELAXATION
C        IS CONVERGED IN 'DERI2' TURNS OUT TO BE ROUGHLY GIVEN BY:
C                      MAXITE = 5 + 2*MBAND ,
C        FOR CALCULATIONS WITH REASONABLE ACCURACY, AND SAFETY FACTOR
C        OF 30%        INCLUDED.
C        THUS THE FINAL VALUES OF MBAND (AS N*3) AND MAXITE BECOME
         IF (MAXITE.LT.5+2*MBAND) THEN
            MBAND= (NFREE-5*LINB)/(LINF+2*LINB)
            MBAND=(MBAND/3)*3
            MAXITE=(NFREE-MBAND*LINF)/LINB
         ENDIF
C        ELSEWHERE /SCRAH2/ IS USED IN DERI1 AS:
C        //DIJKL(NIJKL),F(MBAND*LINF),WORK(NUMBW+LINEAR*2)
C        THUS CHECKING SIZES ....
         NUM=MBAND.LT.1.OR.MAXITE.LT.7.OR.
     .       MINEAR.GT.MPACK/2.OR.NFREE.LT.NUMBW+LINEAR*2
         IF (NUM.AND.FAST) THEN
           WRITE(JOUT,'('' INSUFFICIENT SIZE IN COMMON /SCRAH2/ TO GET''
     .     ,'' THE GRADIENT BY ANALYTICAL METHOD''/
     .     ,'' THUS USE NUMERICAL PROCEDURE WITH LOWER ACCURACY...''
     .     ,'' AND HIGHER TIMING.'')')
           FAST=.FALSE.
         ELSE IF (ID.NE.0) THEN
           WRITE(JOUT,'('' THIS VERSION DOES NOT ALLOW N-DIMENSIONAL ''
     .               ,''POLYMER WITH C.I ... SORRY'')')
           ISTOP=1                                                      GDH1095
           IWHERE=3                                                     GDH1095
           RETURN                                                       GDH1095
         ELSE
C          DEFINE ADDRESSES (ENTRY POINTS) IN /SCRAH2/.
C          1) TO BE USED IN DERI2
           ADJUST=.FALSE.
           IPOSF =NIJKL +1
           IPOSFD=IPOSF +MINEAR*MBAND
           IPOSFC=IPOSFD+NINEAR*MBAND
           IPOSB =IPOSFC+NINEAR*MAXITE
           IPOSAB=IPOSB +MINEAR*MAXITE
           DO 8 IQ=IPOSF,IPOSFD-1
             BWO(IQ)=0.0D0
   8       CONTINUE
C          AND AB IS     MINEAR*MAXITE LONG.
C          2) TO BE USED IN DERI1
           IPOSW2=IPOSB
           IPOSH2=IPOSW2+NUMBW
           IPOSH3=IPOSH2+LINEAR
           MPQKL =NRELAX*MORB2-IPOSH2+1
         ENDIF
      ENDIF
C
C     CHANGE(I) IS THE STEP SIZE OF THE FINITE DIFFERENCE FORMULA.
C     CHANGE(1) FOR BOND LENGTH OR CARTESIAN, (2) ANGLE, (3) DIHEDRAL.
      ESTIM(1)=2000.D0
      ESTIM(2)=250.D0
      ESTIM(3)=150.D0
      PMAX(1)=0.01D0
      PMAX(2)=0.1D0
      PMAX(3)=0.12D0
      IF (ISCFCR.NE.0) THEN                                             DL0397
C         SCFCRT=MAX(FISCFC,1.D-11)                                     1201BL04
         SCFCRT=FISCFC                                                  1201BL04
      ELSE                                                              DL0397
         SCFCRT=0.000001D0                                              DL0397
         IF(IFORCE+IPOLAR+ILTRD+INEWTO.NE.0) SCFCRT=SCFCRT*0.1D0        DL0397
      ENDIF                                                             DL0397
C     NOTE THE SECURITY FACTOR OF 3:                                    DL0397
      SCFCR=SCFCRT*23.061D0 * 3D0                                       DL0397
      IF (.NOT.FAST) THEN
C
C           - - - PURE NUMERICAL METHODS - - -
C    A LARGE STEP IS NEEDED AS FULL SCF CALCULATIONS ARE NEEDED,
C    AND THE DIFFERENCE BETWEEN THE TOTAL ENERGY IS USED.
C    THE STEP CANNOT BE VERY LARGE, AS THE SECOND DERIVATIVES ARE
C    NOT ALWAYS SMALL (UP TO 2000 KCAL/ANGSTROM**2 ). GIVEN:            DL0397
C    - SCFCR,       STANDARD DEVIATION ON THE LACK OF SELF-CONSISTENCY  DL0397
C                   ON THE ELECTRONIC ENERGY,                           DL0397
C    - ESTIM(I),    A LARGE ESTIMATE FOR 2ND DERIVATIVES,               DL0397
C    - ESTIM(I)*12, A LARGE ESTIMATE FOR 3RD DERIVATIVES,               DL0397
C    CHANGE(I) IS ADJUSTED TO ROUGHLY MINIMIZE THE ERROR EG(I) ON       DL0397
C    THE GRADIENT, IN ACCORDANCE TO THE DIFFERENCE FORMULA IN USE.      DL0397
C
         DO 4 I=1,3
         IF (FORWRD.OR.IPOWEL.NE.0) THEN                                GDH0597
            NLOOP=1                                                     DL0397
            CHANGE(I)=MIN(2.D0*SQRT(SCFCR/ESTIM(I)),PMAX(I))            GDH0597
            EG(I)=2.D0*SCFCV/CHANGE(I)+ESTIM(I)*CHANGE(I)/2.D0          DL0397
            IF (.NOT.FORWRD) NLOOP=2                                    GDH0597
         ELSE                                                           DL0397
            NLOOP=2                                                     DL0397
            CHANGE(I)=MIN((0.25D0*SCFCR/ESTIM(I))**0.333D0,PMAX(I))     GDH0597
            EG(I)=SCFCV/CHANGE(I)+2D0*ESTIM(I)*CHANGE(I)**2             DL0397
         ENDIF                                                          DL0397
    4    CONTINUE
         IF (IFORCE+IXYZ.NE.0) THEN                                     DL0397
            DO 5 I=2,3
            CHANGE(I)=CHANGE(1)
    5       EG(I)=EG(1)
         ENDIF
      ELSE
C
C          - - - QUASI-ANALYTICAL BRANCHES - - -
C    A REASONABLE ESTIMATE OF THE ERROR ON THE GRADIENT IS GIVEN BY A
C    LEAST SQUARE FIT USING A LAW SIMILAR TO THOSE USED IN 'ITER':
            EG(1)=1.D1**(0.80355D0*LOG10(SCFCR)+0.45782D0)
C    AND ONES MUST ADD THE LOWER BOUND DUE TO ROUND-OFF :
            EG(1)=EG(1)+2.D-4
C
C    NOTE ... USING A 1-POINT FINITE DIFFERENCE FORMULA IN 'DCART', ONE
C             SAVES 1/3 OF THE CPU TIME, BUT THE ACCURACY IS BOUNDED
C             BELOW BY THE VALUE 0.05 KCAL/MOLE.
         IF (FORWRD) EG(1)=EG(1)+0.05D0                                 DJG0495
         IF(CLSD) THEN
C           WILL CALL 'DCART' WITH STEPSIZE 1D-4 (DEFINED IN DCART)
            STEP=1.D-4
         ELSE
C           WILL CALL 'DERI1' AND 'DERI2'. STEP SIZE AND CV THRESHOLD
C           NOTE ... THE ROUND-OFF ERROR IS 1.D-8 IN ROTATE...
            STEP=1.D-3
C           THE LAW RELATING SCFCRT AND EG(I) BEEING CHOSEN AS:
C  SCFCRT(EV)  :     1D-3      1D-5       1D-7       1D-9       1D-11
C  EG(I)(KCAL) :     1.0       0.1        0.01       0.001      0.0001
C           AND A LEAST SQUARE FIT OF EG(I) VS THROLD GIVING:
C           LOG(EG(I)) = 1.06129 * LOG(THROLD) + 0.17161
C           ONE GET THE FOLLOWING VALUE FOR THROLD, THE CONVERGENCE
C           CRITERION IN THE RELAXATION PROCEDURE (CF 'DERI2') :
            BAL=MAX(LOG(EG(1)),0.76585D0*LOG(SCFCR)+3.88582D0)
            THROLD=EXP(0.9422D0*BAL-0.1617D0)
C           WITH THE RESULTING OVERALL ERROR ON THE GRADIENT:
            EG(1)=EG(1)+EXP(1.06129D0*LOG(THROLD)+0.17161D0)
         ENDIF
         DO 7 I=1,3
    7    CHANGE(I)=STEP
         EG(2)=EG(1)
         EG(3)=EG(1)
      ENDIF
C
C----------------------------------------------------------------------+
C
C     RESTORE THE GEOMETRIC PARAMETERS AND INITIALIZE
C     -----------------------------------------------
C
   10 FAIL=.FALSE.
      IF(NVAR.EQ.0) RETURN
      IF(DEBUG)
     .WRITE(JOUT,'('' INTERNAL COORDINATES AT START OF DERIV''/
     .(F19.5,2F12.5))')((GEO(J,I),J=1,3),I=1,NATOMS)
      DO 20 I=1,NVAR
      GRAD(I)=0.D0
   20 XPARAM(I)=GEO(LOC(2,I),LOC(1,I))
      CALL PORCPU (TIME1)                                               GL0492
      IF(NDEP.NE.0) CALL SYMTRY
      CALL GMETRY(GEO,COORD)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      AA=ELECT+ENUCLR
C
C     CRUDE FINITE DIFFERENCE ON THE ENERGY
C     -------------------------------------
C
      IF (FAST) GO TO 200
C     1 OR 2 POINTS FINITE DIFFERENCE
  100 BAL=-1.D0
      DO 130 IBAL=1,NLOOP
      BAL=-BAL
C     LOOP ON THE VARIABLES ( ONE STEP=CHANGE(LOC(2,*))*BAL )
         DO 120 I=1,NVAR
         IF(FAIL) GO TO 140
         DO 110 J=1,NVAR
  110    GEO(LOC(2,J),LOC(1,J))=XPARAM(J)
         STEP=CHANGE(LOC(2,I))*BAL
         GEO(LOC(2,I),LOC(1,I))=XPARAM(I)+STEP
         IF(NDEP.NE.0) CALL SYMTRY
         CALL GMETRY(GEO,COORD)
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         CALL HCORE(COORD,H,W, WJ, WK,ENUCLR)
         CALL ITER(H,W, WJ, WK,EE,.FALSE.,.FALSE.)
         FAIL=ISCF.NE.1                                                 DJG0995
         EE=(EE+ENUCLR)
         TOTL=(EE-AA)*23.061D0/STEP
  120    GRAD(I)=(TOTL+GRAD(I))/IBAL
  130 CONTINUE
C     RESTORE GEOMETRICAL DATA, INTEGRALS AND M.O.
  140 DO 150 I=1,NVAR
  150 GEO(LOC(2,I),LOC(1,I))=XPARAM(I)
      IF(NDEP.NE.0) CALL SYMTRY
      CALL GMETRY(GEO,COORD)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      CALL HCORE(COORD,H,W, WJ, WK,ENUCLR)
      CALL ITER(H,W, WJ, WK,EE,.FALSE.,.FALSE.)
      FAIL=ISCF.NE.1.OR.FAIL                                            DJG0995
      GO TO 500
C
C     HALF-ELECTRON OR C.I.
C     ---------------------
C
  200 IF (.NOT.CLSD) THEN
C        SCALING ROW FACTORS TO SPEED CV OF RELAXATION PROCEDURE.
         CALL DERI0 (C,CT,EIGS,NORBS)
         NBSIZE=0
         NVAX=3*NUMAT
         ILAST=0
  210    IFIST=ILAST+1
         ILAST=MIN(NVAX,ILAST+MBAND)
C        NON-RELAXED CONTRIBUTION (FROZEN ELECTRONIC CLOUD) IN GRAD
C        AND NON-RLXED FOCK MATRICES IN BWO(IPOSF) & BWO(IPOSFD).
         CIJRDY=.FALSE.
         DO 220 I=IFIST,ILAST
         STEP=CHANGE(1)
  220    CALL DERI1 (C,CT,NORBS,COORD,STEP,I,CBETA,GRAD(I)
     .              ,BWO(IPOSW2),BWO(IPOSH2),BWO(IPOSH3)
     .              ,BWO(IPOSF +MINEAR*(I-IFIST)),MINEAR
     .              ,BWO(IPOSFD+NINEAR*(I-IFIST)),NINEAR
     .              ,BWO(IPOSH2),MPQKL,CIJRDY)
         IF (ISTOP.NE.0) RETURN                                         GDH1095
C        COMPUTE THE ELECTRONIC RELAXATION CONTRIBUTION.
  230    CALL DERI2 (C,CT,EIGS,NORBS,MINEAR,THROLD,BWO(IPOSF)
     .              ,BWO(IPOSFD),BWO(IPOSFC),NINEAR,ILAST-IFIST+1
     .              ,MAXITE,CBETA,BWO(IPOSB),NBSIZE,GRAD(IFIST)
     .              ,FAIL,BWO(IPOSAB),FBWO,MFBWO)
         IF (FAIL.AND.ILAST.EQ.IFIST) THEN
C           CONVERGENCE NOT ACHIEVED IN DERI2 (LACK OF CORE MEMORY...
C           OR ILL-BEHAVED SYSTEM). GO TO CRUDE FINITE DIFFERENCE
C           SECTION FOR THE REMAINING PART OF THE JOB ]]]
            FAIL=.FALSE.
            WRITE(JOUT,'('' REQUIRED ACCURACY NOT ACHIEVED IN DERI2''/
     .'' USE CRUDE FINITE DIFFERENCE METHOD IN GRADIENT COMPUTATION'')')
            ICALCN=-1
            GO TO 1
         ELSE IF(FAIL) THEN
C           DECREASE MBAND (BY 3) AND TRY AGAIN.
            ILAST =ILAST-3
            FAIL  =.FALSE.
            ADJUST=.TRUE.
            GO TO 230
         ELSE IF(ADJUST) THEN
C          REDUCE MBAND AND UPDATE ADDRESSES
C          1) TO BE USED IN DERI2,
           MBAND=MBAND-3
           MAXITE=(NFREE-MBAND*LINF)/LINB
           ADJUST=.FALSE.
           IPOSFD=IPOSF +MINEAR*MBAND
           IPOSFC=IPOSFD+NINEAR*MBAND
           IPOSB =IPOSFC+NINEAR*MAXITE
           IPOSAB=IPOSB +MINEAR*MAXITE
C          2) TO BE USED IN DERI1
           IPOSW2=IPOSB
           IPOSH2=IPOSW2+NUMBW
           IPOSH3=IPOSH2+LINEAR
           MPQKL =NRELAX*MORB2-IPOSH2+1
         ENDIF
         IF (ILAST.LT.NVAX) GO TO 210
C        GRADIENT AS BEEN COMPUTED IN 3*N CARTESIAN.
         IF (IFORCE.EQ.0) THEN                                          DJG0995
C           CONVERT IN INTERNAL
            CALL SCOPY (NVAX,GRAD,1,DXYZ,1)
            GO TO 300
         ELSE
            GO TO 500
         ENDIF
      ENDIF
C
C     RHF CLOSED SHELLS WITHOUT C.I. OR UHF
C     -------------------------------------
C
      IF (CLSD) THEN
C        GRADIENT IN 3*NATOM CARTESIAN
         CALL DCART (COORD,DXYZ)                                        DL0397
         IF (SMODEL.AND.INOPOL.EQ.0) CALL DCART2 (COORD,DXYZ)           DL0397
      ENDIF
  300 CONTINUE
C
C     JACOBIAN dCARTESIAN/dINTERNAL
C     -----------------------------
C     STORED IN BWO.
      IF (IXYZ.EQ.0) THEN                                               GDH0597
         CALL JCARIN (COORD,XPARAM,FORWRD,BJACOB,NCOL)                  DL0496
C        GRADIENT IN INTERNAL, STORED IN GRAD.                          DL0496
         CALL MXM (BJACOB,NVAR,DXYZ,NCOL,GRAD,1)                        DL0496
      ELSE                                                              GDH0597
C        GRADIENT IN CARTESIAN, STORED IN GRAD.                         DJG1296
         CALL SCOPY(NVAR,DXYZ,1,GRAD,1)                                 DJG1296
      ENDIF                                                             GDH0597
C
C     SOME PRINTOUT IF NEEDED
C     -----------------------
C
  500 IF (ICALCN.NE.NUMCAL) THEN
C        PRINTOUT CALCULUS CONDITIONS & STANDARD DEVIATIONS             DL0397
         IF (FAST) THEN                                                 DL0397
            WRITE(JOUT,'(                                               DL0397
     .      '' GRADIENT BY QUASI-ANALYTICAL FORMALISM:'')')             DL0397
            IF (SMODEL) WRITE(JOUT,'('' - ANALYTICAL DERIVATIVES OF'',  DL0397
     .      '' SOLVATION INTEGRALS'')')                                 DL0397
            IF (FORWRD) THEN                                            DL0397
               WRITE(JOUT,'(                                            DL0397
     .         '' - NUMERICAL (FORWARD DIFFERENCE) DERIVATION OF'',     DL0397
     .         '' ELECTRONIC INTEGRALS & JACOBIAN'')')                  DL0397
            ELSE                                                        DL0397
               WRITE(JOUT,'(                                            DL0397
     .         '' - NUMERICAL (CENTRAL DIFFERENCES) DERIVATION OF'',    DL0397
     .         '' ELECTRONIC INTEGRALS & JACOBIAN'')')                  DL0397
            ENDIF                                                       DL0397
            IF (IFORCE.EQ.0.AND.IXYZ.EQ.0) THEN                         DL0397
C              PROPAGATE STD DEV. FROM CARTESIAN TO INTERNAL COORDS     DL0397
               EG1=EG(1)                                                DL0397
               DO 501 I=1,3                                             DL0397
               EMAX(I)=ZERO                                             DL0397
               LMAX(I)=0                                                DL0397
  501          EG(I)=ZERO                                               DL0397
               DO 502 I=1,NVAR                                          DL0397
               CURR=SQRT(SDOT(NCOL,BJACOB(I),NVAR,BJACOB(I),NVAR))      DL0397
               LMAX(KTYP(I))=LMAX(KTYP(I))+1                            DL0397
               EMAX(KTYP(I))=MAX(EMAX(KTYP(I)),CURR)                    DL0397
  502          EG(KTYP(I))=EG(KTYP(I))+CURR                             DL0397
               DO 503 I=1,3                                             DL0397
               IF(LMAX(I).GT.0)EG(I)=5D-1*(EG(I)/DBLE(LMAX(I))+EMAX(I)) DL0397
  503          EG(I)=EG(I)*EG1                                          DL0397
            ENDIF                                                       DL0397
         ELSE                                                           DL0397
            IF (FORWRD) THEN                                            DL0397
               WRITE(JOUT,'(                                            DL0397
     .         '' GRADIENT BY FORWARD DIFFERENCE ON ENERGY:'')')        DL0397
            ELSE                                                        DL0397
               WRITE(JOUT,'(                                            DL0397
     .         '' GRADIENT BY CENTRAL DIFFERENCES ON ENERGY:'')')       DL0397
            ENDIF                                                       DL0397
            WRITE(JOUT,'(                                               DL0397
     .      '' STEP LENGTH FOR DIFFERENCES                '',3F11.8)')  DL0397
     .      (CHANGE(I),I=1,3)                                           DL0397
         ENDIF                                                          DL0397
         WRITE(JOUT,'(                                                  DL0397
     .   /'' STANDARD DEVIATION ON ENERGY (kcal)        '', F11.8       DL0397
     .   /''                       GRADIENT (kcal/RU)   '',3F11.8)')    DL0397
     .   SCFCV,(EG(I),I=1,3)                                            DL0397
         ICALCN = NUMCAL                                                DL0397
      ENDIF                                                             DL0397
C      DO 505 I=1,NVAR     
C      IHOLD2=ICALCN
C      ICALCN=-1
C      IF (ABS(GRAD(I)).GT.5.0D0) ICALCN=IHOLD2
C505   CONTINUE
      IF (DEBUG) THEN
         WRITE(JOUT,'('' GRADIENTS (kcal)'')')                          GL0492
         WRITE(JOUT,'(10F8.3)')(GRAD(I),I=1,NVAR)
      ENDIF
      IF (TIMES) THEN
         CALL PORCPU (TFLY)                                             GL0492
         WRITE(JOUT,'('' TIME FOR DERIVATIVES'',F12.6)')TFLY-TIME1
      ENDIF
C
C     C.I CORRECTION ON THE DENSITY MATRIX.
C     -------------------------------------
      IF (LAST.EQ.1.AND..NOT.CLSD) CALL MECIP (P,C,CBETA,NORBS,BWO,NCI2)
      END
