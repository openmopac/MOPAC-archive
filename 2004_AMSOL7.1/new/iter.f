      SUBROUTINE ITER  (H, W, WJ, WK, EE, LGRAD, RESTOR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION  MECI                                            3GL2292
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'                                                  DJG0995
      INCLUDE 'FFILES.i'                                                GDH1095
      PARAMETER (MPULAY=(MORB2*(NRELAX-8)-98)/4)
      DIMENSION H(*), W(*), WJ(*), WK(*)
      COMMON /MLKSTR/ FRACT                                             3GL3092
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
     3       /MOLORB/ DUMMY(MAXORB),PDIAG(MAXORB)
     5       /NUMSCF/ NSCF,FROZEN
     6       /FOKMAT/ F(MPACK),FB(MPACK),FO(MPACK),FBO(MPACK)           JWS1094
     7       /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
     8       /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON /LAST  / LAST
     1       /MESAGE/ IFLEP, ISCF                                       DJG0995
     2       /ATHEAT/ ATHEAT
     3       /ENUCLR/ ENUCLR
     4       /CITERM/ XI,XJ,XK
     5       /TIMECM  / TIME0                                           3GL3092
     6       /PRECI / SCFCV,SCFTOL,DUM(9),KDUM(MAXPAR)
      COMMON /OPTIMI / IMP,IMP0
      COMMON /REPATH/ REACT(100),LATOM,LPARAM                           03GCL93
      COMMON /SCRAH2/ POLD(MPULAY),  POLD2(MPULAY),  POLD3(49)
     1               ,PBOLD(MPULAY), PBOLD2(MPULAY), PBOLD3(49)
     2               ,AR1(MORB2), AR2(MORB2), AR3(MORB2), AR4(MORB2)
     3               ,BR1(MORB2), BR2(MORB2), BR3(MORB2), BR4(MORB2),   3GL3092
     4                DUM5(NRELAX*MORB2+1-8*MORB2-4*MPULAY-2*49)        GCL0892
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),
     1                QEFF(NUMATM),DRVPOL(MPACK)                        DJG1094
      COMMON /AREACM/ NOPTI,TAREA,NINTEG                                GDH0793
      COMMON /CYCLCM/ PCMIN, NGEOM, NSOLPR, NSCFS, JPCNT                GDH0893
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      COMMON /MODLCM/ SMODEL                                            DJG0496
C
C    NOTE... /SCRAH2/ IS A WORK AREA REQUIRED BY THE VARIOUS CONVERGERS.
C                     IT IS ALSO USED IN THE ITERATIVE COMPUTATION OF
C                     THE DERIVATIVES OF THE M.O IN ROUTINE 'DERI2'.
C***********************************************************************
C
C                        ITER GENERATES A SCF FIELD
C     ON INPUT :
C              LGRAD=.T. IF THE GRADIENT IS TO BE COMPUTED, IMPLYING A
C                        LARGE NUMBER OF <IJ!KL> TO BE CALCULATED WHEN
C                        CALLING MECI (ANALYTICAL GRADIENT ONLY).
C                    .F. OTHERWISE.
C              RESTOR=.T. RESET THE DENSITY MATRICES WITH THE DIAGONAL
C                         APPROXIMATION AND ITERATE
C                     .F. START WITH THE PREVIOUS DENSITY MATRICES
C     ON OUTPUT :
C              ISCF=  2  IF NOT CONVERGED, 1 OTHERWISE                  DJG0995
C              EE      ELECTRONIC ENERGY (EV) INCLUDING C.I. CORRECTION
C
C THE MAIN ARRAYS USED IN ITER ARE:
C            P      ONLY EVER CONTAINS THE TOTAL DENSITY MATRIX
C            PA     ONLY EVER CONTAINS THE ALPHA DENSITY MATRIX
C            PB     ONLY EVER CONTAINS THE BETA DENSITY MATRIX
C            C      ONLY EVER CONTAINS THE EIGENVECTORS
C            H      ONLY EVER CONTAINS THE ONE-ELECTRON MATRIX
C            F      STARTS OFF CONTAINING THE ONE-ELECTRON MATRIX,
C                   AND IS USED TO HOLD THE FOCK MATRIX
C            W      ONLY EVER CONTAINS THE TWO-ELECTRON MATRIX
C
C THE MAIN INTEGERS CONSTANTS IN ITER ARE:
C
C            LINEAR SIZE OF PACKED TRIANGLE = NORBS*(NORBS+1)/2
C
C THE MAIN  VARIABLES ARE
C            NITER  NUMBER OF ITERATIONS EXECUTED
C            SELCON=SCFCRT*SCFTOL CONVERGENCE THRESHOLD (KCAL)
C
C  PRINCIPAL REFERENCES:
C
C   ON MNDO: "GROUND STATES OF MOLECULES. 38. THE MNDO METHOD.
C             APPROXIMATIONS AND PARAMETERS."
C             DEWAR, M.J.S., THIEL,W., J. AM. CHEM. SOC.,99,4899,(1977).
C   ON SHIFT: "UNCONDITIONAL CONVERGENCE IN SCF THEORY: A GENERAL LEVEL
C             SHIFT TECHNIQUE"
C             CARBO, R., HERNANDEZ, J.A., SANZ, F., CHEM. PHYS. LETT.,
C             47, 581, (1977)
C   ON HALF-ELECTRON: "MINDO/3 COMPARISON OF THE GENERALISED S.C.F.
C             COUPLING OPERATOR AND "HALF-ELECTRON" METHODS FOR
C             CALCULATING THE ENERGIES AND GEOMETRIES OF OPEN SHELL
C             SYSTEMS"
C             DEWAR, M.J.S., OLIVELLA, S., J.CHEM.SOC.FARAD.TRANS. 2,
C             75,829,(1979).
C   ON PULAY'S CONVERGER: "CONVERGENCE ACCELERATION OF ITERATIVE
C             SEQUENCES. THE CASE OF SCF ITERATION", PULAY, P.,
C             CHEM. PHYS. LETT, 73, 393, (1980).
C   ON PSEUDODIAGONALISATION: "FAST SEMIEMPIRICAL CALCULATIONS",
C             STEWART. J.J.P., CSASZAR, P., PULAY, P., J. COMP. CHEM.,
C             3, 227, (1982)
C
C***********************************************************************
      CHARACTER ABPRT(3)*5                                              DJG0995
      LOGICAL PRTFOK,PRTEIG,PRTDEN, DEBUG, TIMES, CI, SMODEL            DJG0496
     1       ,UHF, NEWDG, HALFE, FORCE, PRT1EL, PRTPL, LGRAD, RESTOR
     2       ,EXCITD, MINPRT, FRST, OKPULY, OKPULA, READY, PRTVEC
     3       ,CAMKIN, SOMCON, ALLCON, MAKEA, MAKEB, INCITR, FROZEN
     4       ,AQCHK,TAREA                                               CC8-91
      SAVE
      DATA    ABPRT/'     ','ALPHA',' BETA'/
      DATA ZERO,ONE /0.0D0, 1.0D0/                                      DL0397
C
C  INITIALIZE
C
C GDH0497      IF (NOPTI.NE.1) THEN
C GDH0497      CALL ITER2(H,W,WJ,WK,EE,LGRAD,RESTOR)
C GDH0497      ELSE
      NSTAR=1                                                           CC8-91
      AQCHK=SMODEL
      EOLD=1.D2
      CCT1=0.D0                                                         CC1290
      READY=.FALSE.
      IF (ICONTR(44).EQ.1) THEN                                         GDH0195
         ICONTR(44)=2                                                   GDH0195
         LINEAR=NORBS*(NORBS+1)/2
         IF (ICHMOD.GE.1) CALL SCOPY (LINEAR,ZERO,0,DRVPOL,1)           GDH0497
C
C    DEBUG KEY-WORDS WORKED OUT
C
         DEBUG=IITER.NE.0                                               DJG0995
         MINPRT=ISADDL+LATOM.EQ.0.OR.DEBUG                              DJG0995
         PRTEIG=IEIGS.NE.0                                              DJG0995
         PRTPL =IPLITE.NE.0                                             DJG0995
         IF(IDEBUG.NE. 0 ) THEN                                         DJG0995
            PRT1EL=I1ELEC.NE.0                                          DJG0995
            PRTDEN=IDENSI.NE.0                                          DJG0995
            PRTFOK=IFOCK.NE.0                                           DJG0995
            PRTVEC=IVECTO.NE.0                                          DJG0995
         ELSE
            PRT1EL=.FALSE.
            PRTDEN=.FALSE.
            PRTFOK=.FALSE.
            PRTVEC=.FALSE.
         ENDIF
C
C INITIALIZE SOME LOGICALS AND CONSTANTS
C
C         FROZEN=.FALSE.
         NEWDG =.FALSE.
         PL    =1.D0
         IFILLV=0                                                       DJG0995
         BSHIFT=0.D0
         ITRMAX = 200
         NMOS=0
         NCIS=0
         NA2EL=NCLOSE
         NSCF=0
         NA1EL=NALPHA+NOPEN
         NB2EL=0
         NB1EL=NBETA+NOPEN
C
C  USE KEY-WORDS TO ASSIGN VARIOUS CONSTANTS
C
         IF(ICI.NE.0) NMOS=IICI                                         DJG0995
         IF(IMICRO.NE.0) NCIS=IIMICR                                    DJG0995
         IF(IFILL.NE.0) IFILLV=-IIFILL                                  DJG0995
         IF(ISHIFT.NE.0) BSHIFT=-FISHIF                                 DJG0995
         IF(IITRY.NE.0) ITRMAX=IIITRY                                   DJG0995
         CAMKIN=(ICAMPK.NE.0)                                           DJG0995
         CI    =(IMICRO+ICI.NE.0)                                       DJG0995
         UHF   =(IUHF.NE.0)                                             DJG0995
         EXCITD=(IEXCIT.NE.0)                                           DJG0995
         TIMES =(ITIMES.NE.0)                                           DJG0995
         FORCE =(IFORCE.NE.0)                                           DJG0995
         OKPULA=(IPULAY.NE.0)                                           DJG0995
         ALLCON=(OKPULA.OR.(BSHIFT.NE.0.D0).OR.CAMKIN)
         SOMCON=ALLCON
C
C   SET UP C.I. PARAMETERS
C   NMOS IS NO. OF M.O.S USED IN C.I.
C   NCIS IS CHANGE IN SPIN, OR NUMBER OF STATES
C
         IF(NMOS.EQ.0) NMOS=NOPEN-NCLOSE
         IF(NCIS.EQ.0) THEN
            IF(ITRIPL+IQUART.NE.0)NCIS=1                                DJG0995
            IF(IQUINT+ISEXTE.NE.0)NCIS=2                                DJG0995
         ENDIF
         TRANS=0.1D0
         IF(IRESTA+IOLDEN.NE.0) THEN                                    DJG0995
            REWIND JDEN
            READ(JDEN)(PA(I),I=1,LINEAR)
            IF( UHF) THEN
               READ(JDEN)(PB(I),I=1,LINEAR)
               DO 10 I=1,LINEAR
   10          P(I)=PA(I)+PB(I)
            ELSE
               DO 20 I=1,LINEAR
   20          P(I)=PA(I)*2.D0
            ENDIF
            ELSEIF(IOLDMA.NE.0) THEN                                    DJG0995
              REWIND JDMT
              CALL VECRED(PA,NORBS,JDMT)                                GDH1095
              IF(UHF) THEN
                    CALL VECRED(PB,NORBS,JDMT)                          GDH1095
                    DO 18 I=1,LINEAR
   18               P(I)=PA(I)+PB(I)
              ELSE
                    DO 19 I=1,LINEAR
                    P(I)=PA(I)
   19               PA(I)=PA(I)/2.0D0
              ENDIF
         ELSE
            DO 30 I=1,LINEAR
            P(I)=0.D0
            PA(I)=0.D0
   30       PB(I)=0.D0
            W1=NA1EL/(NA1EL+1.D-6+NB1EL)
            W2=1.D0-W1
            IF(W1.LT.1.D-6)W1=0.5D0
            IF(W2.LT.1.D-6)W2=0.5D0
            RANDOM=1.0D0
            IF(UHF.AND.NA1EL.EQ.NB1EL) RANDOM=1.1D0
            J=0
            DO 40 I=1,NORBS
            J=J+I
            P(J)=PDIAG(I)
            PA(J)=P(J)*W1*RANDOM
            RANDOM=1.D0/RANDOM
   40       PB(J)=P(J)*W2*RANDOM
            CALL SCOPY (LINEAR,PB,1,PBOLD,1)
            CALL SCOPY (LINEAR,PA,1,POLD ,1)
         ENDIF
         HALFE=(NOPEN .NE. NCLOSE)
         IF( HALFE ) THEN
            IF(NOPEN-NCLOSE.EQ.1) THEN
               IHOMO=(NCLOSE-1)*NORBS+1
               ILUMO=IHOMO+NORBS
            ELSE
               IPART1=NCLOSE*NORBS+1
               IPART2=IPART1+NORBS
            ENDIF
         ENDIF
C
C   DETERMINE THE SELF-CONSISTENCY CRITERION
C
C        DEFAULT VALUE (EV)
         SCFCRT=0.000001D0                                              DJG0495
         IF(FORCE.OR.IPOLAR+ILTRD+INEWTO.NE.0) SCFCRT=SCFCRT*0.1D0      DJG0995
C        OR STATED BY THE USER
         IF(ISCFCR.NE.0) THEN                                           DJG0995
C            SCFCRT=MAX(FISCFC,1.D-11)                                  1201BL04
            SCFCRT=FISCFC                                               1201BL04
            WRITE(JOUT,'(/,'' SCF CONVERGENCE CRITERION ='',
     1            1PD7.1,'' eV'')')SCFCRT
         ELSE IF (DEBUG) THEN
            WRITE(JOUT,'(/,'' SCF CONVERGENCE CRITERION ='',1PD7.1
     1            ,'' eV'')')SCFCRT
         ENDIF
         LAST=0
         SCFTOL=1.D0
C
C   STATISTICAL ESTIMATE OF THE ERROR ON THE ELECTRONIC ENERGY
C
C        ROUNDING-OFF ERROR ("5D-13"ONLY IS MACHINE DEPENDANT)
         K=0
         SCFCV=0.D0
         DO 51 I=1,NORBS
         K=K+I
   51    SCFCV=SCFCV+H(K)**2
         SCFCV=23.061D0*SQRT(SCFCV/DBLE(NORBS))*5D-13
ccc      SCFCV=(34.5915D0*NELECS*SCFCV)*5.0D-13
C        NOW WE USE THE TWO FOLLOWING EMPIRICAL LAWS (LEAST SQUARE FIT)
C        => LOG10(SCFCV)= 0.7174 * LOG10(SCFCRT) + 0.0913
C                WITH COVARIANCE MATRIX :
C                                       .0082662800
C                                       .0413313999 .2397221193
C        => LOG10(SCFCV)= 1.0968 * LOG10(PLTEST) + 0.5742
C        WHERE SCFCV  IS AN ESTIMATE OF THE ENERGY DROP OUT (KCAL) ...
C                     ASSUMING THAT ROUNDOFF IS NEGLIGIBLE.
C                     ('converged' energy - true one) = SCFCV)
C              PLTEST IS THE CONVERGENCE THRESHOLD ON DIAGONAL ELEMENTS
C                     OF THE TOTAL DENSITY MATRIX.
C        THUS,GIVEN SCFCRT,ONE GETS A REASONABLE VALUE OF PLTEST AND
C        A VALUE FOR THE STANDARD DEVIATION OF THE ENERGY ,SCFCV,
C        (via covariance matrix)
C        FOR FURTHER USE IN GRADIENT AND HESSIAN EVALUATION.
         SCFLOG=LOG10(SCFCRT)
         EE=1.D1**(0.66358D0*SCFLOG-0.29045D0)
         SIGMA=EE*SQRT (
     .   (.0082662800D0*SCFLOG+0.0826627998D0)*SCFLOG+.2397221193D0 )
         IF(DEBUG) WRITE(JOUT,FMT='('' ROUNDING-OFF ERROR'',1PD8.1,
     .   '' kcal/mole''/                                                GL0492
     .   '' ENERGY DROPOUT'',D8.1,'' WITH STANDARD DEVIATION'',D8.1,
     .   '' kcal/mole'')')                                              GL0492
C    .   '' KCAL/MOLE''/
C    .   '' KCAL/MOLE'')')
     .   SCFCV,EE,SIGMA
         IF(EE.LT.3.D0*SCFCV) WRITE(JOUT,FMT='('' ==> THERE IS A RISK''
     .,'' OF INFINITE LOOPING WITH  SCFCRT ='',1PD8.1)')SCFCRT
         SCFCV=SIGMA+SCFCV+EE
C
C   END OF INITIALIZATION SECTION.
C
      END IF
C
C   RESTORE DENSITY MATRICES IF NEEDED
C
      IF (RESTOR) THEN
         IF(DEBUG)WRITE(JOUT,'(''RESTORE ALL DENSITY MATRICES'')')
         NEWDG=.FALSE.
         DO 52 I=1,LINEAR
         P (I)=0.D0
         PA(I)=0.D0
   52    PB(I)=0.D0
         RANDOM=1.D0
         IF(UHF.AND.NA1EL.EQ.NB1EL) RANDOM=1.1D0
         J=0
         DO 53 I=1,NORBS
         J=J+I
         P(J)=PDIAG(I)
         PA(J)=P(J)*W1*RANDOM
         RANDOM=1.D0/RANDOM
   53    PB(J)=P(J)*W2*RANDOM
         CALL SCOPY (LINEAR,PB,1,PBOLD,1)
         CALL SCOPY (LINEAR,PA,1,POLD ,1)
      ENDIF
C
C   THE FOLLOWING INITIALIZATION OPERATIONS DONE EVERY CALL TO ITER
C
      MAKEA=.TRUE.
      MAKEB=.TRUE.
      PL=1.D0
      IF(NEWDG) NEWDG=(ABS(BSHIFT).LT.0.001D0)
      IF(LAST.EQ.1) NEWDG=.FALSE.
C   SCFTOL IS MANAGED BY THE ACTIVATED OPTIMIZATION ROUTINE
C   SELCON AND PLTEST ARE COUPLED BY THE TWO PRECEEDING EMPIRICAL LAWS
      SELCON=SCFCRT*23.061D0
      IF(SCFTOL.GT.1.D0) SELCON=MIN(SELCON*SCFTOL,0.023D0)
      PLTEST=1.D1**(0.6050D0*LOG10(SELCON)-1.6129D0)
      IF(DEBUG)WRITE(JOUT,'(''  SELCON, SCFTOL, PLTEST'',3F16.9)')
     .                       SELCON, SCFTOL, PLTEST
      CALL PORCPU (TITER1)                                              GL0492
      IF(PRT1EL) THEN
        WRITE(JOUT,'(10X,''ONE-ELECTRON MATRIX AT ENTRANCE TO ITER'')')
        CALL VECPRT(H,NORBS)
      ENDIF
      IREDY=0
   60 NITER=0
      FRST=.TRUE.
      IF(CAMKIN) THEN
         MODEA=1
         MODEB=1
      ELSE
         MODEA=0
         MODEB=0
      ENDIF
**********************************************************************
*                                                                    *
*                                                                    *
*                START THE SCF LOOP HERE                             *
*                                                                    *
*                                                                    *
**********************************************************************
      INCITR=.TRUE.
   70 INCITR=(MODEA.NE.3.AND.MODEB.NE.3)
      NINDX=NITER/NSTAR+1
      IF(INCITR)NITER=NITER+1
      IF(INCITR)NSCFS=NSCFS+1                                           GDH0495
      OKPULY=OKPULA.AND.(PL.LT.2.D-2).AND.IREDY.GT.2
      IF(NITER.EQ.ITRMAX.AND..NOT.ALLCON) THEN
        IF(AQCHK.AND.NSTAR.LT.5) THEN                                   CC8-91
            NITER=0                                                     CC8-91
            NSTAR=NSTAR+1                                               CC8-91
            WRITE(JOUT,'(//,''SCF OSCILLATION -- NSTAR INDEX '',        GCL0393
     1                      ''INCREASED BY ONE'')')                     GCL0393
            GO TO 70                                                    CC8-91
        ELSE                                                            CC8-91
************************************************************************
*                                                                      *
*                   SWITCH ON ALL CONVERGERS                           *
*                                                                      *
************************************************************************
         WRITE(JOUT,'(//,'' ALL CONVERGERS ARE NOW FORCED ON'',/
     1          '' SHIFT=1000, PULAY ON, CAMP-KING ON'',/
     2          '' AND ITERATION COUNTER RESET'',//)')
         ALLCON=.TRUE.
         BSHIFT=-1000.2D0                                               GCL0393
         IREDY=-4
         EOLD=100.D0
         OKPULA=.TRUE.
         NEWDG=.FALSE.
         CAMKIN=(.NOT.HALFE)
         GOTO 60
        ENDIF
      ENDIF
************************************************************************
*                                                                      *
*                        MAKE THE ALPHA FOCK MATRIX                    *
*                                                                      *
************************************************************************
      IF(BSHIFT .NE. 0.D0) THEN
         L=0
         SHIFT=BSHIFT*(NITER+1.D0)**(-1.5D0)
         DO 90 I=1,NORBS
            DO 80 J=1,I
               L=L+1
   80       F(L)=H(L)+SHIFT*PA(L)
   90    F(L)=F(L)-SHIFT
      ELSE
         CALL SCOPY (LINEAR,H,1,F,1)
      ENDIF
  110 CALL FOCK2(F,P,PA,W, WJ, WK,NUMAT,NFIRST,NMIDLE,NLAST)
      CALL FOCK1(F,P,PA,PB)
      IF(.NOT.AQCHK) GO TO 982                                          CC8-91
      IF(INOPOL.NE.0) GO TO 982                                         GDH0597
      CALL BORNPL(NINDX,.TRUE.,.TRUE.,LGRAD)                            DL0397
      IF (ICHGM.GE.2) CALL SCOPY (LINEAR,F,1,FO,1)                      GDH0997
      IF (ICHMOD.GE.1) THEN                                             GDH0997
         CALL SAXPY (LINEAR,ONE,DRVPOL,1,F,1)                           DL0397
      ELSE                                                              DJG1094
         K=0                                                            CC1290
         DO 115 I=1,NUMAT                                               CC1290
         DO 115 J=NFIRST(I),NLAST(I)                                    CC1290
           K=K+J                                                        CC1290
  115      F(K)=F(K)+BP(I)                                              CC1290
       ENDIF                                                            DJG1094
************************************************************************
*                                                                      *
*                        MAKE THE BETA FOCK MATRIX                     *
*                                                                      *
************************************************************************
982   IF (UHF) THEN
         IF(SHIFT .NE. 0.D0) THEN
            L=0
            DO 130 I=1,NORBS
               DO 120 J=1,I
                  L=L+1
  120          FB(L)=H(L)+SHIFT*PB(L)
  130       FB(L)=FB(L)-SHIFT
         ELSE
            CALL SCOPY (LINEAR,H,1,FB,1)
         ENDIF
         CALL FOCK2(FB,P,PB,W, WJ, WK,NUMAT,NFIRST,NMIDLE,NLAST)
         CALL FOCK1(FB,P,PB,PA)
         IF(.NOT.AQCHK) GO TO 983                                       CC8-91
         IF(INOPOL.NE.0) GO TO 983                                      GDH0297
         IF (ICHGM.GE.2) CALL SCOPY (LINEAR,FB,1,FBO,1)                 GDH0997
         IF (ICHMOD.GE.1) THEN                                          GDH0997
            CALL SAXPY (LINEAR,ONE,DRVPOL,1,FB,1)                       DL0397
         ELSE                                                           DJG1094
            K=0                                                         CC1290
            DO 68 I=1,NUMAT                                             DJG1094
              DO 67 J=NFIRST(I),NLAST(I)                                DJG1094
                K=K+J                                                   DJG1094
                FB(K)=FB(K)+BP(I)                                       DJG1094
   67         CONTINUE                                                  DJG1094
   68       CONTINUE                                                    DJG1094
         ENDIF                                                          DJG1094
983   ENDIF
      IF (PRTFOK) THEN
         WRITE(JOUT,150)NITER
  150    FORMAT('   FOCK MATRIX ON ITERATION',I3)
         CALL VECPRT (F,NORBS)
      ENDIF
************************************************************************
*                                                                      *
*                        CALCULATE THE ENERGY IN KCAL/MOLE             *
*                                                                      *
************************************************************************
      IF (ICHGM.GE.2.AND.INOPOL.EQ.0) THEN                              GDH1197
         EE=HELECT(NORBS,PA,H,FO)                                       DL0397
      ELSE                                                              DL0397
         EE=HELECT(NORBS,PA,H,F)                                        DL0397
      ENDIF                                                             DL0397
      IF (UHF) THEN                                                     DL0397
         IF (ICHGM.GE.2.AND.INOPOL.EQ.0) THEN                           GDH1197
            EE=EE+HELECT(NORBS,PB,H,FBO)                                DL0397
         ELSE                                                           DL0397
            EE=EE+HELECT(NORBS,PB,H,FB)                                 DL0397
         ENDIF                                                          DL0397
      ELSE                                                              DL0397
         EE=EE*2.D0                                                     DL0397
      ENDIF                                                             DL0397
      EE=EE+CCT1                                                        DL0397
      ESCF=(EE+ENUCLR+SHIFT*(NOPEN-NCLOSE)*0.25D0)*23.061D0+ATHEAT
      IF (INCITR) THEN
         DIFF=ESCF-EOLD
         IF(PL.LT.PLTEST.AND.
     1   ABS(DIFF).LT.SELCON .AND. READY) THEN
************************************************************************
*                                                                      *
*          SELF-CONSISTENCY TEST, EXIT MODE FROM ITERATIONS            *
*                                                                      *
************************************************************************
            IF (ABS(SHIFT) .LT. 1.D-10) GOTO 240
            SHIFT=0.D0
            CALL SCOPY (LINEAR,H,1,F,1)
            MAKEA=.TRUE.
            MAKEB=.TRUE.
            GOTO 110
         ENDIF
         READY=(IREDY.GT.0.AND.ABS(DIFF).LT.SELCON*10.D0)
         IREDY=IREDY+1
      ENDIF
      IF(PRTPL) THEN
         IF(ABS(ESCF).GT.99999.D0) ESCF=99999.D0
         IF(ABS(DIFF).GT.9999.D0)DIFF=0.D0
         IF(INCITR)
     1   WRITE(JOUT,'('' ITERATION'',I3,'' PLS='',2E10.3,'' ENERGY  '',
     2   F14.7,'' DELTAE'',F13.7)')NITER,PL,PLB,ESCF,DIFF
      ENDIF
      IF(INCITR)EOLD=ESCF
************************************************************************
*                                                                      *
*                        INVOKE THE CAMP-KING CONVERGER                *
*                                                                      *
************************************************************************
      IF(NITER.GT.2 .AND. CAMKIN .AND. MAKEA)
     1CALL INTERP(NORBS,NA1EL,NORBS-NA1EL, MODEA, ESCF/23.061D0,
     2F, C, AR1, AR2, AR3, AR4, AR1)
      MAKEB=.FALSE.
      IF(MODEA.EQ.3)GOTO 180
      MAKEB=.TRUE.
      IF( NEWDG ) THEN
************************************************************************
*                                                                      *
*                        INVOKE PULAY'S CONVERGER                      *
*                                                                      *
************************************************************************
         IF(OKPULY.AND.MAKEA)
     1CALL PULAY(F,PA,NORBS,POLD,POLD2,POLD3,JALP,IALP,MPULAY,FRST,PL)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
************************************************************************
*                                                                      *
*           DIAGONALIZE THE ALPHA OR RHF SECULAR DETERMINANT           *
* WHERE POSSIBLE, USE THE PULAY-STEWART METHOD, OTHERWISE USE BEPPU'S  *
*                                                                      *
************************************************************************
         IF (HALFE.OR.CAMKIN) THEN
            CALL HQRII(F,NORBS,NORBS,EIGS,C)
         ELSE
            CALL DIAG(F,C,NA1EL,EIGS,NORBS,NORBS)
         ENDIF
      ELSE
         CALL HQRII(F,NORBS,NORBS,EIGS,C)
      END IF
      J=1
      IF(PRTVEC) THEN
         J=1
         IF(UHF)J=2
         WRITE(JOUT,'(//10X,A,
     1'' EIGENVECTORS AND EIGENVALUES ON ITERATION'',I3)')
     2   ABPRT(J),NITER
         CALL MATOUT(C,EIGS,NORBS,NORBS,NORBS)
      ELSE
         IF (PRTEIG) WRITE(JOUT,170)ABPRT(J),NITER,(EIGS(I),I=1,NORBS)
      ENDIF
  170 FORMAT(10X,A,'  EIGENVALUES ON ITERATION',I3,/10(6F13.6,/))
  180 IF(IFILLV.NE.0)CALL SWAP(C,NORBS,NORBS,NA2EL,IFILLV)              DJG0995
************************************************************************
*                                                                      *
*            CALCULATE THE ALPHA OR RHF DENSITY MATRIX                 *
*                                                                      *
************************************************************************
      IF(UHF)THEN
         CALL DENSIT( C,NORBS, NORBS, NA2EL,NA1EL, FRACT, PA)
      ELSE
         CALL DENSIT( C,NORBS, NORBS, NA2EL,NA1EL, FRACT, P)
      ENDIF
      IF(MODEA.NE.3.AND..NOT. (NEWDG.AND.OKPULY))
     1    CALL CNVG(P, POLD, POLD2, NORBS, NITER, PL)
************************************************************************
*                                                                      *
*                       UHF-SPECIFIC CODE                              *
*                                                                      *
************************************************************************
      IF( UHF )THEN
************************************************************************
*                                                                      *
*                        INVOKE THE CAMP-KING CONVERGER                *
*                                                                      *
************************************************************************
         IF(NITER.GT.2 .AND. CAMKIN .AND. MAKEB )
     1CALL INTERP(NORBS,NB1EL,NORBS-NB1EL, MODEB, ESCF/23.061D0,
     2FB, CBETA, BR1, BR2, BR3, BR4, BR1)
         MAKEA=.FALSE.
         IF(MODEB.EQ.3) GOTO 190
         MAKEA=.TRUE.
************************************************************************
*                                                                      *
*                        INVOKE PULAY'S CONVERGER                      *
*                                                                      *
************************************************************************
         IF( NEWDG.AND.OKPULY.AND.MAKEB) THEN
            CALL PULAY(FB,PB,NORBS,PBOLD,PBOLD2,
     1               PBOLD3,JBET,IBET,MPULAY,FRST,PLB)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
************************************************************************
*                                                                      *
*           DIAGONALIZE THE ALPHA OR RHF SECULAR DETERMINANT           *
* WHERE POSSIBLE, USE THE PULAY-STEWART METHOD, OTHERWISE USE BEPPU'S  *
*                                                                      *
************************************************************************
            CALL DIAG(FB,CBETA,NB1EL,EIGB,NORBS,NORBS)
         ELSE
            CALL HQRII(FB,NORBS,NORBS,EIGB,CBETA)
         END IF
         IF(PRTVEC) THEN
            WRITE(JOUT,'(//10X,A,'' EIGENVECTORS AND EIGENVALUES ON '',
     1''ITERATION'',I3)')ABPRT(3),NITER
            CALL MATOUT(CBETA,EIGB,NORBS,NORBS,NORBS)
         ELSE
            IF (PRTEIG) WRITE(JOUT,170)ABPRT(3),NITER,
     1                                 (EIGB(I),I=1,NORBS)
         ENDIF
************************************************************************
*                                                                      *
*                CALCULATE THE BETA DENSITY MATRIX                     *
*                                                                      *
************************************************************************
  190    CALL DENSIT( CBETA,NORBS, NORBS, NB2EL, NB1EL, FRACT, PB)
         IF( .NOT. (NEWDG.AND.OKPULY))
     1   CALL CNVG(PB, PBOLD, PBOLD2, NORBS, NITER, PLB)
      ENDIF
************************************************************************
*                                                                      *
*                   CALCULATE THE TOTAL DENSITY MATRIX                 *
*                                                                      *
************************************************************************
      IF(UHF) THEN
         DO 200 I=1,LINEAR
  200    P(I)=PA(I)+PB(I)
      ELSE
         DO 210 I=1,LINEAR
         PA(I)=P(I)*0.5D0
  210    PB(I)=PA(I)
      ENDIF
      IF(PRTDEN) THEN
         WRITE(JOUT,'('' DENSITY MATRIX ON ITERATION'',I4)')NITER
         CALL VECPRT (P,NORBS)
      END IF
      NEWDG=(PL.LT.TRANS .OR. NEWDG)
      IF (NITER .GT. ITRMAX) THEN
         IF(MINPRT)WRITE (JOUT,220)
  220    FORMAT (//10X,'""""""ITER : UNABLE TO ACHIEVE SELF-CONSISTENCE'
     1,/)
         WRITE (JOUT,230) DIFF,PL
  230    FORMAT (//,10X,'DELTAE= ',E12.4,5X,'DELTAP= ',E12.4,///)
         IFLEP=9                                                        DJG0995
         ISCF=2                                                         DJG0995
         CALL WRITES (TIME0,ESCF)                                       GL0492
         RETURN
      END IF
      GO TO 70
**********************************************************************
*                                                                    *
*                                                                    *
*                      END THE SCF LOOP HERE                         *
*                NOW CALCULATE THE ELECTRONIC ENERGY                 *
*                                                                    *
*                                                                    *
**********************************************************************
*          SELF-CONSISTENCE ACHEIVED.
*
**********************************************************************
  240 IF (ICHGM.GE.2.AND.INOPOL.EQ.0) THEN                              GDH0997
         EE=HELECT(NORBS,PA,H,FO)                                       DL0397
      ELSE                                                              DL0397
         EE=HELECT(NORBS,PA,H,F)                                        DL0397
      ENDIF                                                             DL0397
      IF (UHF) THEN                                                     DL0397
         IF (ICHGM.GE.2.AND.INOPOL.EQ.0) THEN                           GDH0997
            EE=EE+HELECT(NORBS,PB,H,FBO)                                DL0397
         ELSE                                                           DL0397
            EE=EE+HELECT(NORBS,PB,H,FB)                                 DL0397
         ENDIF                                                          DL0397
      ELSE                                                              DL0397
         EE=EE*2.D0 + SHIFT*(NOPEN-NCLOSE)*0.25D0                       DL0397
      ENDIF                                                             DL0397
      ISCF=1                                                            DJG0995
      EE=EE+CCT1                                                        CC1290
      IF( NSCF.EQ.0 .OR. ABS(SHIFT) .GT. 1.D-5 .OR. CI .OR. HALFE ) THEN
C
C  PUT F AND FB INTO POLD IN ORDER TO NOT DESTROY F AND FB
C  AND DO EXACT DIAGONALISATIONS
         CALL SCOPY (LINEAR,F,1,POLD,1)
         CALL HQRII(POLD,NORBS,NORBS,EIGS,C)
         IF (UHF) THEN
            CALL SCOPY (LINEAR,FB,1,POLD,1)
            CALL HQRII(POLD,NORBS,NORBS,EIGB,CBETA)
         ENDIF
         IF(CI.OR.HALFE)
     .   EE=EE+MECI(EIGS,C,CBETA,EIGB, NORBS,NMOS,NCIS,.FALSE.,LGRAD)   GCL0393
      ENDIF
C      IF(.NOT.FROZEN) THEN                                             DJG0495
       NSCF=NSCF + 1                                                    GDH0983
C      ENDIF                                                            DJG0495
      CALL PORCPU (TITER2)                                              GL0492
      IF(TIMES) WRITE(JOUT,'('' TIME FOR SCF CALCULATION'',F8.2,
     1''    INTEGRAL'',F8.2)')TITER2-TITER1,TITER2-TIME0
      IF(DEBUG)WRITE(JOUT,'('' NO. OF ITERATIONS ='',I3)')NITER
C     IF(SOMCON.OR.(ALLCON.AND.ABS(BSHIFT+1000.2).LT.0.01))THEN
      IF(SOMCON.OR.(ALLCON.AND.ABS(BSHIFT+1000.2D0).LT.0.01D0))THEN     GCL0393
         CAMKIN=.FALSE.
         ALLCON=.FALSE.
         SOMCON=.FALSE.
         NEWDG=.FALSE.
         BSHIFT=0.D0
         OKPULA=.FALSE.
      ENDIF
      IF(HALFE) BSHIFT=0.D0
C GDH0497      ENDIF
C     THIS IS A HACK TO UPDATE EIGENVECTORS AND EIGENVALUES             DJG1296
      IF (ITRUSE.EQ.1) CALL HQRII(F,NORBS,NORBS,EIGS,C)                 DJG1296
      RETURN
      END
