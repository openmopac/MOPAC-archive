      SUBROUTINE DERIV(GEO,GRAD,*)                                      CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION GRAD(*), GEO(3,*)
      PARAMETER (MFBWO=9*MAXPAR, MSIZE=NUMATM*(NUMATM-1)/2)             DJG0496
      COMMON / EULER/ TVEC(3,3), ID
      COMMON /GEOVAR/ XDUMMY(MAXPAR), NVAR, LOC(2,MAXPAR)               IR0394 
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GRAVEC/ COSINE
      COMMON /GEOSYM/ NDEP, IDUMYS(MAXPAR,3)
C    changed common path for portability  (IR)
      COMMON /PATHI / LATOM,LPARAM
      COMMON /PATHR / REACT(200)

      COMMON /SCRAH1/ SCALAR(MPACK/2),DIAG(MPACK/2),FRACT2,FBWO(MFBWO),
     1                NBO(3),IDUM
     2       /CIDATA/ VECTCI(MAXCI),XXCI,NCI1,NCI2,NCI3,                DL0397
     3                NCIDUM((1+2*NMECI)*MAXCI)                         DL0397
     4       /LAST  / LAST
     5       /HBODY2/ RG(3,MSIZE),BJACOB(MAXPAR**2),COORD(3,NUMATM)
     8       /SCRAH2/ BWO(NRELAX*MORB2+1)
     9       /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
C    7                 DUM3(6*MAXPAR**2+1-6*NUMATM*27)                   DL0397
c
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U
      COMMON /XYZGRA/ DXYZ(3,NUMATM*27)
      COMMON /ENUCLR/ ENUCLR
      COMMON /NUMCAL/ NUMCAL
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
C     COMMON /WMATRX/ WJ(N2ELEC), WK(N2ELEC),NUMBW,NBAND(NUMATM)
      COMMON /WMATRX/ WJ(N2ELEC), WK(N2ELEC*2),NUMBW,NBAND(NUMATM)
C     COMMON /DWMAT/  DWJ(N2ELEC),DWK(N2ELEC*2)                          JZ0315
      COMMON /HMATRX/ H(MPACK)
      COMMON /ATHEAT/ ATHEAT
C***********************************************************************
C
C    DERIV CALCULATES THE DERIVATIVES OF THE ENERGY WITH RESPECT TO THE
C          INTERNAL COORDINATES. THIS IS DONE BY FINITE DIFFERENCES.
C
C    THE MAIN ARRAYS IN DERIV ARE:
C        LOC    INTEGER ARRAY, LOC(1,I) CONTAINS THE ADDRESS OF THE ATOM
C               INTERNAL COORDINATE LOC(2,I) IS TO BE USED IN THE
C               DERIVATIVE CALCULATION.
C        GEO    ARRAY \GEO\ HOLDS THE INTERNAL COORDINATES.
C        GRAD   ON EXIT, CONTAINS THE DERIVATIVES
C
C***********************************************************************
      COMMON /KEYWRD / KEYWRD
      COMMON /ERRFN  / ERRFN(MAXPAR)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      include 'result.f'                                                LF1210
      CHARACTER*160 KEYWRD
C     DIMENSION CHANGE(3), COORD(3,NUMATM), COLD(3,NUMATM*27)
C    1,         XDERIV(3), XPARAM(MAXPAR), XJUC(3), W(N2ELEC)
      DIMENSION CHANGE(3),COLD(3,NUMATM*27),XDERIV(3),
     1          XPARAM(MAXPAR), XJUC(3), W(N2ELEC),CT(MORB2),EG(3)
      LOGICAL DEBUG, TIMES, HALFE, FAST, SCF1, CI, PRECIS,CLSD,FAIL
     1,       ADJUST,NUM,CIJRDY,TAREA
      EQUIVALENCE (W,WJ)
C     EQUIVALENCE (W,DWJ)
         SAVE                                                           GL0892
      DATA ICALCN /0/
      IXYZ = 0
      IXYZ = INDEX(KEYWRD,'XYZ')
      IF(ICALCN.NE.NUMCAL) THEN
         I = INDEX(KEYWRD,'PRESS')
         PRESS=0.D0
         IF(I.NE.0) PRESS=READA(KEYWRD,I)*1476.8992D0
         IDLO=NATOMS+1
         IF(LABELS(NATOMS) .EQ. 107) THEN
            IDLO=NATOMS
            IF(LABELS(NATOMS-1) .EQ. 107)THEN
               IDLO=NATOMS-1
               IF(LABELS(NATOMS-2) .EQ. 107)THEN
                  IDLO=NATOMS-2
               ENDIF
            ENDIF
         ENDIF
         GRLIM=0.01D0
         DEBUG = (INDEX(KEYWRD,'DERIV') .NE. 0)
         PRECIS= (INDEX(KEYWRD,'PRECIS') .NE. 0)
         TIMES = (INDEX(KEYWRD,'TIME') .NE. 0)
         CI    = (INDEX(KEYWRD,'C.I.') .NE. 0)
         SCF1  = (INDEX(KEYWRD,'1SCF') .NE. 0)
         ICALCN=NUMCAL
         IF(INDEX(KEYWRD,'RESTART') .EQ. 0) THEN
            DO 10 I=1,NVAR
   10       ERRFN(I)=0.D0
         ENDIF
         GRLIM=0.01D0
         IF(PRECIS)GRLIM=0.0001D0
         IF(INDEX(KEYWRD,'FULSCF') .GT.0) GRLIM=1.D9
         HALFE = (NOPEN.GT.NCLOSE .OR. CI)
         CLSD  = .NOT.(HALFE.OR.CI) 
         IDELTA=-7
*
*   IDELTA IS A MACHINE-PRECISION DEPENDANT INTEGER
*
         IF(HALFE.AND.PRECIS) IDELTA=-3
         IF(HALFE.AND..NOT.PRECIS) IDELTA=-3
         FAST=.TRUE.
         CHANGE(1)= 10.D0**IDELTA
         CHANGE(2)= 10.D0**IDELTA
         CHANGE(3)= 10.D0**IDELTA
C
C    CHANGE(I) IS THE STEP SIZE USED IN CALCULATING THE DERIVATIVES.
C    FOR "CARTESIAN" DERIVATIVES, CALCULATED USING DCART,AN
C    INFINITESIMAL STEP, HERE 0.000001, IS ACCEPTABLE. IN THE
C    HALF-ELECTRON METHOD A QUITE LARGE STEP IS NEEDED AS FULL SCF
C    CALCULATIONS ARE NEEDED, AND THE DIFFERENCE BETWEEN THE TOTAL
C    ENERGIES IS USED. THE STEP CANNOT BE VERY LARGE, AS THE SECOND
C    DERIVATIVE IN FLEPO IS CALCULATED FROM THE DIFFERENCES OF TWO
C    FIRST DERIVATIVES. CHANGE(1) IS FOR CHANGE IN BOND LENGTH,
C    (2) FOR ANGLE, AND (3) FOR DIHEDRAL.
C
         XDERIV(1)= 0.5D0/CHANGE(1)
         XDERIV(2)= 0.5D0/CHANGE(2)
         XDERIV(3)= 0.5D0/CHANGE(3)
      ENDIF
      GNORM=0.D0
      IF(NVAR.EQ.0) RETURN
      IF(DEBUG)THEN
         IF (DOPRNT) WRITE(6,'('' GEO AT START OF DERIV'')')            CIO
         IF (DOPRNT) WRITE(6,'(F19.5,2F12.5)')((GEO(J,I),J=1,3),I=1,    CIO
     &                     NATOMS)                                      CIO
      ENDIF
      DO 20 I=1,NVAR
         XPARAM(I)=GEO(LOC(2,I),LOC(1,I))
   20 GNORM=GNORM+GRAD(I)**2
      GNORM=DSQRT(GNORM)
C     FAST=(GNORM .GT. GRLIM .AND. .NOT. SCF1 .OR. .NOT. HALFE)
      FAST=(GNORM .GT. GRLIM .AND. .NOT. SCF1 )
C     FAST=.TRUE.
      TIME1=SECOND()
C Taken from AMSOL 7.1 and modified by J. Zheng 03/15
      IF(.NOT.CLSD) THEN
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
      IF (ISCFCR.NE.0) THEN                                             DL0397
C         SCFCRT=MAX(FISCFC,1.D-11)                                     1201BL04
         SCFCRT=FISCFC                                                  1201BL04
      ELSE                                                              DL0397
C        SCFCRT=0.000001D0                                              DL0397
         SCFCRT=1D-7      
         IF(IFORCE+IPOLAR+ILTRD+INEWTO.NE.0) SCFCRT=SCFCRT*0.1D0        DL0397
      ENDIF                                                             DL0397
!     NOTE THE SECURITY FACTOR OF 3:                                    DL0397
      SCFCR=SCFCRT*23.061D0 * 3D0                                       DL0397

      IF(NDEP.NE.0) CALL SYMTRY(*9999)                                   CSTP(call)
      CALL GMETRY(GEO,COORD,*9999)                                       CSTP(call)
      IF(FAST) GO TO 200
      IF( .NOT. FAST ) THEN
         IF(DEBUG.AND.DOPRNT)                                           CIO
     &                   WRITE(6,'('' DOING FULL SCF''''S IN DERIV'')') CIO
         CALL HCORE(COORD,H,W, WJ, WK, ENUCLR,*9999)                     CSTP(call)
         IF(NORBS*NELECS.GT.0)THEN
            CALL ITER(H, W, WJ, WK, AA,.TRUE.,.FALSE.,*9999)             CSTP(call)
         ELSE
            AA=0.D0
         ENDIF
         LINEAR=(NORBS*(NORBS+1))/2
         DO 30 I=1,LINEAR
   30    P(I)=PA(I)*2.D0
         AA=(AA+ENUCLR)
      ENDIF
C
C     HALF-ELECTRON OR C.I.
C     ---------------------
C
  200 IF (.NOT.CLSD) THEN
C    A REASONABLE ESTIMATE OF THE ERROR ON THE GRADIENT IS GIVEN BY A
C    LEAST SQUARE FIT USING A LAW SIMILAR TO THOSE USED IN 'ITER':
            EG(1)=1.D1**(0.80355D0*LOG10(SCFCR)+0.45782D0)
C    AND ONES MUST ADD THE LOWER BOUND DUE TO ROUND-OFF :
            EG(1)=EG(1)+2.D-4
C
C    NOTE ... USING A 1-POINT FINITE DIFFERENCE FORMULA IN 'DCART', ONE
C             SAVES 1/3 OF THE CPU TIME, BUT THE ACCURACY IS BOUNDED
C             BELOW BY THE VALUE 0.05 KCAL/MOLE.
C        IF (FORWRD) EG(1)=EG(1)+0.05D0                                 DJG0495
C        SCALING ROW FACTORS TO SPEED CV OF RELAXATION PROCEDURE.
C            SCFCRT=0.000001D0
C     NOTE THE SECURITY FACTOR OF 3:                                    DL0397
c            SCFCR=SCFCRT*23.061D0 * 3D0                                DL0397
C            STEP = 1D-3
             BAL=MAX(LOG(EG(1)),0.76585D0*LOG(SCFCR)+3.88582D0)
             THROLD=EXP(0.9422D0*BAL-0.1617D0)
             EG(1)=EG(1)+EXP(1.06129D0*LOG(THROLD)+0.17161D0)
C        write(6,*) 'SCFCRT, SCFCR, EG(1)', SCFCRT, SCFCR, EG(1)
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
C        WRITE(6,'('' GRADIENTS AFTER DERI1'')')
C 220    WRITE(6,'(10F8.3)')GRAD(I)
C        IF (ISTOP.NE.0) RETURN                                         GDH1095
C        COMPUTE THE ELECTRONIC RELAXATION CONTRIBUTION.
  230    CALL DERI2 (C,CT,EIGS,NORBS,MINEAR,THROLD,BWO(IPOSF)
     .              ,BWO(IPOSFD),BWO(IPOSFC),NINEAR,ILAST-IFIST+1
     .              ,MAXITE,CBETA,BWO(IPOSB),NBSIZE,GRAD(IFIST)
     .              ,FAIL,BWO(IPOSAB),FBWO,MFBWO)
C        WRITE(6,'('' GRADIENTS AFTER DERI2'')')
C        WRITE(6,'(10F8.3)')(GRAD(I),I =1, NVAR)
         IF (FAIL.AND.ILAST.EQ.IFIST) THEN
C           CONVERGENCE NOT ACHIEVED IN DERI2 (LACK OF CORE MEMORY...
C           OR ILL-BEHAVED SYSTEM). GO TO CRUDE FINITE DIFFERENCE
C           SECTION FOR THE REMAINING PART OF THE JOB ]]]
            FAIL=.FALSE.
            WRITE(JOUT,'('' REQUIRED ACCURACY NOT ACHIEVED IN DERI2''/
     .'' USE CRUDE FINITE DIFFERENCE METHOD IN GRADIENT COMPUTATION'')')
            ICALCN=-1
C           GO TO 1
            STOP 
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
C           IF (IXYZ.EQ.0) THEN                                         GDH0597
             CALL JCARIN (COORD,XPARAM,FORWRD,BJACOB,NCOL)              DL0496
C        GRADIENT IN INTERNAL, STORED IN GRAD.                          DL0496
             CALL MXM (BJACOB,NVAR,DXYZ,NCOL,GRAD,1)                    DL0496
C           ELSE                                                        GDH0597
C        GRADIENT IN CARTESIAN, STORED IN GRAD.                         DJG1296
C           CALL SCOPY(NVAR,DXYZ,1,GRAD,1)                              DJG1296
C           ENDIF                                                       GDH0597

            GO TO 500
         ELSE
C           GO TO 300
         ENDIF
      ENDIF

C     RHF CLOSED SHELLS WITHOUT C.I. OR UHF
C     -------------------------------------
C
      IF (CLSD) 
     1CALL DCART(COORD,DXYZ,*9999)                                       CSTP(call)
 300  CONTINUE
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Get gradient of total energy with respect to each Cartesian coordinate.
C 
C This includes some redundancy for 5 (for linear) or 6 (for nonlinear
C molecules) degrees of freedom which can be removed from the total of 3N
C Cartesian coordinates, of course.  The dimension of the Cartesian
C coordinate-type gradient here is thus 5 or 6 larger than required
C to completely describe the gradient of the system and as such is
C actually reducible.
C
C Factor of two in derivatives appears at end of DCART subroutine
C when converting from eV to kcal/mol (uses factor of 46.122 instead
C of 23.061).  Also occurs in line 223 below.
      grnorm=0.0d0
      do myct=1,numat+3
C         write(6,77) myct, dxyz(1,myct)
C         write(6,78) myct, dxyz(2,myct)
C         write(6,79) myct, dxyz(3,myct)
 77       format('Derivative of energy wrt atom #',I2,'''s x is ',F16.8)
 78       format('Derivative of energy wrt atom #',I2,'''s y is ',F16.8)
 79       format('Derivative of energy wrt atom #',I2,'''s z is ',F16.8)
c#        write(24,1111) 1,myct,dxyz(1,myct)
c#        write(24,1111) 2,myct,dxyz(2,myct)
c#        write(24,1111) 3,myct,dxyz(3,myct)
 1111  format('Increasing GNORM by square of DXYZ(',I1,',',I3'):',F16.8)
        grnorm=grnorm+(dxyz(1,myct)**2+dxyz(2,myct)**2+dxyz(3,myct)**2)
      enddo
      grnorm=dsqrt(grnorm)/2.0d0   ! Remove factor of two also.
      FGRADT=grnorm
c#      write(24,*) "Gradient norm (kcal/mol/angstrom) : ",grnorm
c#      write(6 ,*) "Gradient norm (kcal/mol/angstrom) : ",grnorm
      FGRPER=FGRADT/DBLE(3*NUMAT)
c#      write(24,*)"Gradient norm per Cartesian df (kcal/mol/angstrom): ",
c#     &      FGRPER
c#      write(6 ,*)"Gradient norm per Cartesian df (kcal/mol/angstrom): ",
c#     &      FGRPER
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF(NDEP.NE.0) CALL SYMTRY(*9999)                                   CSTP(call)
      CALL GMETRY(GEO,COORD,*9999)                                       CSTP(call)
      IJ=0
      DO 70 II=1,NUMAT
         DO 60 IL=L1L,L1U
            DO 60 JL=L2L,L2U
               DO 60 KL=L3L,L3U
                  DO 40 LL=1,3
   40             XJUC(LL)=COORD(LL,II)+TVEC(LL,1)*IL+TVEC(LL,2)*JL+TVEC
     1(LL,3)*KL
                  IJ=IJ+1
                  DO 50 KK=1,3
                     COLD(KK,IJ)=XJUC(KK)
   50             CONTINUE
   60    CONTINUE
   70 CONTINUE
      SUM11=1.D-9
      SUM22=1.D-9
      SUM12=1.D-9
      DO 150 I=1,NVAR
         K=LOC(1,I)
         L=LOC(2,I)
         XSTORE=XPARAM(I)
         DO 80 J=1,NVAR
   80    GEO(LOC(2,J),LOC(1,J))=XPARAM(J)
         GEO(L,K)=XSTORE-CHANGE(L)
         IF(NDEP.NE.0) CALL SYMTRY(*9999)                                CSTP(call)
         CALL GMETRY(GEO,COORD,*9999)                                    CSTP(call)
C#         CALL GEOUT
C
C    USE LOOKUP TABLE OF CARTESIAN DERIVATIVES TO WORK OUT INTERNAL
C    COORDINATE DERIVATIVE.
C
         TOTL=0.D0
         IJ=0
         DO 130 II=1,NUMAT
            IF(ID.EQ.0) THEN
               DO 90 LL=1,3
   90          TOTL=TOTL+DXYZ(LL,II)*(COORD(LL,II)-COLD(LL,II))
            ELSE
               DO 120 IL=L1L,L1U
                  DO 120 JL=L2L,L2U
                     DO 120 KL=L3L,L3U
                        DO 100 LL=1,3
  100                   XJUC(LL)=COORD(LL,II)+TVEC(LL,1)*IL+TVEC(LL,2)*J
     1L+TVEC(LL,3)*KL
                        IJ=IJ+1
                        TOTL=TOTL+DXYZ(1,IJ)*(XJUC(1)-COLD(1,IJ))
     &                           +DXYZ(2,IJ)*(XJUC(2)-COLD(2,IJ))
     &                           +DXYZ(3,IJ)*(XJUC(3)-COLD(3,IJ))
  120          CONTINUE
            ENDIF
  130    CONTINUE
         TOTL=TOTL*XDERIV(L)
C
C   IF NEEDED, CALCULATE "EXACT" DERIVATIVES.
C
         IF( .NOT. FAST ) THEN
            CALL HCORE(COORD,H,W, WJ, WK,ENUCLR,*9999)                   CSTP(call)
            IF(NORBS*NELECS.GT.0)THEN
               CALL ITER(H,W, WJ, WK,EE,.TRUE.,.FALSE.,*9999)            CSTP(call)
            ELSE
               EE=0.D0
            ENDIF
            DO 140 II=1,LINEAR
  140       P(II)=PA(II)*2.D0
            EE=(EE+ENUCLR)
            TOTL1=(AA-EE)*23.061D0*XDERIV(L)*2.D0
C#            WRITE(6,*)AA-EE
            ERRFN(I)=TOTL1-TOTL
         ENDIF
         GEO(L,K)=XSTORE
         SUM11=SUM11+GRAD(I)**2
         SUM22=SUM22+TOTL**2
         SUM12=SUM12+TOTL*GRAD(I)
         GRAD(I)=TOTL+ERRFN(I)
  150 CONTINUE
  500 CONTINUE
      IF(DEBUG.AND.DOPRNT) THEN                                         CIO
         WRITE(6,'('' GRADIENTS'')')
         WRITE(6,'(10F8.3)')(GRAD(I),I=1,NVAR)
         WRITE(6,'('' ERROR FUNCTION'')')
         WRITE(6,'(10F8.3)')(ERRFN(I),I=1,NVAR)
      ENDIF
      COSINE=SUM12/SQRT(SUM11*SUM22)
      IF(DEBUG.AND.DOPRNT)                                              CIO
     1WRITE(6,'('' COSINE OF SEARCH DIRECTION ='',F30.6)')COSINE        CIO
      IF( .NOT. FAST ) COSINE=1.D0
      IF(TIMES.AND.DOPRNT)                                              CIO
     1WRITE(6,'('' TIME FOR DERIVATIVES'',F12.6)')SECOND()-TIME1        CIO
C
C     C.I CORRECTION ON THE DENSITY MATRIX.
C     -------------------------------------
      IF (LAST.EQ.1.AND..NOT.CLSD) CALL MECIP (P,C,CBETA,NORBS,BWO,NCI2)
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY DERIV_INIT                                                  CSAV
                AA = 0.0D0                                              CSAV
            CHANGE = 0.0D0                                              CSAV
                CI = .FALSE.                                            CSAV
             DEBUG = .FALSE.                                            CSAV
                EE = 0.0D0                                              CSAV
              FAST = .FALSE.                                            CSAV
             GNORM = 0.0D0                                              CSAV
             GRLIM = 0.0D0                                              CSAV
             HALFE = .FALSE.                                            CSAV
                 I = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
                ID = 0                                                  CSAV
            IDELTA = 0                                                  CSAV
              IDLO = 0                                                  CSAV
                II = 0                                                  CSAV
                IJ = 0                                                  CSAV
                 K = 0                                                  CSAV
                KK = 0                                                  CSAV
                 L = 0                                                  CSAV
            LINEAR = 0                                                  CSAV
                LL = 0                                                  CSAV
                NA = 0                                                  CSAV
            NALPHA = 0                                                  CSAV
                NB = 0                                                  CSAV
             NBETA = 0                                                  CSAV
                NC = 0                                                  CSAV
            NCLOSE = 0                                                  CSAV
            NELECS = 0                                                  CSAV
             NLAST = 0                                                  CSAV
            NMIDLE = 0                                                  CSAV
             NOPEN = 0                                                  CSAV
             NORBS = 0                                                  CSAV
            PRECIS = .FALSE.                                            CSAV
             PRESS = 0.0D0                                              CSAV
              SCF1 = .FALSE.                                            CSAV
             SUM11 = 0.0D0                                              CSAV
             SUM12 = 0.0D0                                              CSAV
             SUM22 = 0.0D0                                              CSAV
             TIME1 = 0.0D0                                              CSAV
             TIMES = .FALSE.                                            CSAV
              TOTL = 0.0D0                                              CSAV
             TOTL1 = 0.0D0                                              CSAV
              XJUC = 0.0D0                                              CSAV
            XDERIV = 0.0D0                                              CSAV
             COORD = 0.0D0                                              CSAV
             COLD  = 0.0D0                                              CSAV
            XPARAM = 0.0D0                                              CSAV
            XSTORE = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
