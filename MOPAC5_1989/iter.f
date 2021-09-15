      SUBROUTINE ITER  (H, W, WJ, WK, EE, FULSCF,RAND)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DOUBLE PRECISION MECI
      DIMENSION H(*), W(*), WJ(*), WK(*)
      COMMON /FOKMAT/ F(MPACK), FB(MPACK)
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON /GRADNT/ DUMY(MAXPAR),GNORM
      COMMON /LAST  / LAST
      COMMON /MESAGE/ IFLEPO,IITER
      COMMON /ATHEAT/ ATHEAT
      COMMON /ENUCLR/ ENUCLR
      COMMON /CITERM/ XI,XJ,XK
      COMMON /PATH  / LATOM,LPARAM,REACT(200)
      COMMON /NUMCAL/ NUMCAL
      COMMON /TIME  / TIME0
      LOGICAL FULSCF, RAND
      REAL WJ, WK
C***********************************************************************
C
C     ITER GENERATES A SCF FIELD AND RETURNS THE ENERGY IN "ENERGY"
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
C THE MAIN INTEGER VARIABLES ARE
C            NITER  NUMBER OF ITERATIONS EXECUTED
C
C  PRINCIPAL REFERENCES:
C
C   ON MNDO: "GROUND STATES OF MOLECULES. 38. THE MNDO METHOD.
C             APPROXIMATIONS AND PARAMETERS."
C             DEWAR, M.J.S., THIEL,W., J. AM. CHEM. SOC.,99,4899,(1977).
C   ON SHIFT: "THE DYNAMIC 'LEVEL SHIFT' METHOD FOR IMPROVING THE
C             CONVERGENCE OF THE SCF PROCEDURE", A. V. MITIN, J. COMP.
C             CHEM. 9, 107-110 (1988)
C   ON HALF-ELECTRON: "MINDO/3 COMPARISON OF THE GENERALIZED S.C.F.
C             COUPLING OPERATOR AND "HALF-ELECTRON" METHODS FOR
C             CALCULATING THE ENERGIES AND GEOMETRIES OF OPEN SHELL
C             SYSTEMS"
C             DEWAR, M.J.S., OLIVELLA, S., J. CHEM. SOC. FARA. II,
C             75,829,(1979).
C   ON PULAY'S CONVERGER: "CONVERGANCE ACCELERATION OF ITERATIVE
C             SEQUENCES. THE CASE OF SCF ITERATION", PULAY, P.,
C             CHEM. PHYS. LETT, 73, 393, (1980).
C   ON CNVG:  IT ENCORPORATES THE IMPROVED ITERATION SCHEME (IIS) BY
C             PIOTR BADZIAG & FRITZ SOLMS. ACCEPTED FOR PUBLISHING
C             IN COMPUTERS & CHEMISTRY
C   ON PSEUDODIAGONALISATION: "FAST SEMIEMPIRICAL CALCULATIONS",
C             STEWART. J.J.P., CSASZAR, P., PULAY, P., J. COMP. CHEM.,
C             3, 227, (1982)
C
C***********************************************************************
      DIMENSION POLD(MPACK), POLD2(MPACK), POLD3(MAXORB+400)
      DIMENSION PBOLD(MPACK), PBOLD2(MPACK), PBOLD3(MAXORB+400)
************************************************************************
*                                                                      *
*   IF, FOR ANY REASON, ITER FAILS TO GENERATE AN SCF, THEN UNCOMMENT  *
*   ALL LINES THAT BEGIN WITH A ''.  THE EFFECT OF DOING THIS IS TO  *
*   INCREASE THE STORAGE REQUIREMENT BY ABOUT 20% AND TO ALLOW PULAY'S *
*   AND CAMP AND KING'S CONVERGERS TO BE USED.                         *
*                                                                      *
************************************************************************
      DIMENSION  AR1(2*NPULAY), AR2(2*NPULAY), AR3(2*NPULAY),
     1 AR4(2*NPULAY)
      DIMENSION  BR1(2*NPULAY), BR2(2*NPULAY), BR3(2*NPULAY),
     1 BR4(2*NPULAY)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,
     2                NALPHA, NBETA, NCLOSE, NOPEN, NDUMY, FRACT
     3      /MOLORB/ DUMMY(MAXORB),PDIAG(MAXORB)
     4      /KEYWRD/ KEYWRD
     5      /NUMSCF/ NSCF
      CHARACTER KEYWRD*80, ABPRT(3)*5
      LOGICAL PRTFOK,PRTEIG,PRTDEN, DEBUG, PRTENG, TIMES, CI
     1,UHF, NEWDG, SCF1, HALFE, FORCE, PRT1EL,PRTPL, OKNEWD
     2,EXCITD, MINPRT, FRST, BFRST, OKPULY, READY, PRTVEC,
     3CAMKIN, ALLCON, MAKEA, MAKEB, INCITR, CAPPS
      DATA ICALCN/0/, DEBUG/.FALSE./, PRTFOK/.FALSE./
      DATA PRTEIG/.FALSE./,PRTDEN,PRTENG/.FALSE.,.FALSE./
      DATA PRT1EL/.FALSE./
      DATA ABPRT/'     ','ALPHA',' BETA'/
C
C  INITIALIZE
C
      IFILL=0
      IHOMO=MAX(1,NCLOSE+NALPHA)
      IHOMOB=MAX(1,NCLOSE+NBETA)
      EOLD=1.D2
      READY=.FALSE.
      IF (ICALCN.NE.NUMCAL) THEN
         CALL EPSETA(EPS,ETA)
C
C  ULTIMATE SCF CRITERION: HEAT OF FORMATION CONVERGED WITHIN A FACTOR
C  OF 10 OF THE LIMITING PRECISION OF THE COMPUTER
C
         EPS=23.061D0*EPS*2.D0
         SHIFT=0.D0
         ICALCN=NUMCAL
         SHFMAX=20.D0
         LINEAR=(NORBS*(NORBS+1))/2
C
C    DEBUG KEY-WORDS WORKED OUT
C
         DEBUG=( INDEX(KEYWRD,'DEBUG') .NE. 0 )
         MINPRT=(INDEX(KEYWRD,'SADDLE')+
     1      LATOM .EQ.0 .OR. DEBUG)
         PRTEIG=( INDEX(KEYWRD,'EIGS') .NE. 0 )
         PRTENG=( INDEX(KEYWRD,'ENERGY').NE.0 )
         PRTPL =( INDEX(KEYWRD,' PL ')  .NE.0 )
         PRT1EL=( INDEX(KEYWRD,'1ELEC') .NE.0 .AND. DEBUG)
         PRTDEN=( INDEX(KEYWRD,'DENS').NE.0 .AND. DEBUG)
         PRTFOK=( INDEX(KEYWRD,'FOCK') .NE. 0  .AND. DEBUG)
         PRTVEC=( INDEX(KEYWRD,'VECT') .NE. 0  .AND. DEBUG)
         DEBUG=( INDEX(KEYWRD,'ITER') .NE. 0 )
C
C INITIALIZE SOME LOGICALS AND CONSTANTS
C
         NEWDG =.FALSE.
         PL    =1.D0
         BSHIFT=-15.D0
         SHIFT=1.D0
*
* SCFCRT AND PLTEST ARE MACHINE-PRECISION DEPENDENT
*
         SCFCRT=1.D-6
         PLTEST=0.0001D0
         ITRMAX = 200
         NMOS=0
         NCIS=0
         NA2EL=NCLOSE
         NA1EL=NALPHA+NOPEN
         NB2EL=0
         NB1EL=NBETA+NOPEN
C
C  USE KEY-WORDS TO ASSIGN VARIOUS CONSTANTS
C
         IF(INDEX(KEYWRD,'C.I.').NE.0)
     1      NMOS=READA(KEYWRD,INDEX(KEYWRD,'C.I.')+5)
         IF(INDEX(KEYWRD,'MICROS').NE.0)
     1      NCIS=READA(KEYWRD,INDEX(KEYWRD,'MICROS'))
         IF(INDEX(KEYWRD,'FILL').NE.0)
     1      IFILL=-READA(KEYWRD,INDEX(KEYWRD,'FILL'))
         IF(INDEX(KEYWRD,'SHIFT').NE.0)
     1      BSHIFT=-READA(KEYWRD,INDEX(KEYWRD,'SHIFT'))
         IF(BSHIFT.NE.0)TEN=BSHIFT
         IF(INDEX(KEYWRD,'ITRY').NE.0)
     1      ITRMAX=READA(KEYWRD,INDEX(KEYWRD,'ITRY'))
         CAMKIN=(INDEX(KEYWRD,'KING')+INDEX(KEYWRD,'CAMP') .NE. 0)
         CI=(INDEX(KEYWRD,'MICROS')+INDEX(KEYWRD,'C.I.') .NE. 0)
         OKPULY=.FALSE.
         OKPULY=(INDEX(KEYWRD,'PULAY').NE.0)
         UHF=(INDEX(KEYWRD,'UHF') .NE. 0)
         SCF1=(INDEX(KEYWRD,'1SCF') .NE. 0)
         OKNEWD=ABS(BSHIFT) .LT. 0.001D0
         IF(CAMKIN.AND.ABS(BSHIFT).GT.1.D-5) BSHIFT=4.44D0
         EXCITD=(INDEX(KEYWRD,'EXCITED') .NE. 0)
         TIMES=(INDEX(KEYWRD,'TIMES') .NE. 0)
         FORCE=(INDEX(KEYWRD,'FORCE') .NE. 0)
         ALLCON=(OKPULY.OR.CAMKIN)
C
C   SET UP C.I. PARAMETERS
C   NMOS IS NO. OF M.O.S USED IN C.I.
C   NCIS IS CHANGE IN SPIN, OR NUMBER OF STATES
C
         IF(NMOS.EQ.0) NMOS=NOPEN-NCLOSE
         IF(NCIS.EQ.0) THEN
            IF(INDEX(KEYWRD,'TRIPLET')+INDEX(KEYWRD,'QUART').NE.0)NCIS=1
            IF(INDEX(KEYWRD,'QUINTET')+INDEX(KEYWRD,'SEXTE').NE.0)NCIS=2
         ENDIF
C
C   DO WE NEED A CAPPED ATOM CORRECTION?
C
         CAPPS=.FALSE.
         DO 10 I=1,NUMAT
   10    IF(NAT(I).EQ.102)CAPPS=.TRUE.
         IITER=1
         TRANS=0.1D0
         IF(INDEX(KEYWRD,'RESTART')+INDEX(KEYWRD,'OLDENS')
     1      .NE. 0) THEN
            IF(INDEX(KEYWRD,'OLDENS').NE.0)
     1   OPEN(UNIT=10,FILE='FOR010',STATUS='UNKNOWN',FORM='UNFORMATTED')
            REWIND 10
            READ(10)(PA(I),I=1,LINEAR)
            IF( UHF) THEN
               READ(10)(PB(I),I=1,LINEAR)
               DO 20 I=1,LINEAR
                  POLD(I)=PA(I)
                  PBOLD(I)=PB(I)
   20          P(I)=PA(I)+PB(I)
            ELSE
               DO 30 I=1,LINEAR
                  PB(I)=PA(I)
                  PBOLD(I)=PA(I)
                  POLD(I)=PA(I)
   30          P(I)=PA(I)*2.D0
            ENDIF
         ELSE
            NSCF=0
            DO 40 I=1,LINEAR
               P(I)=0.D0
               PA(I)=0.D0
   40       PB(I)=0.D0
            W1=NA1EL/(NA1EL+1.D-6+NB1EL)
            W2=1.D0-W1
            IF(W1.LT.1.D-6)W1=0.5D0
            IF(W2.LT.1.D-6)W2=0.5D0
            RANDOM=1.0D0
            IF(UHF.AND.NA1EL.EQ.NB1EL) RANDOM=1.1D0
            DO 50 I=1,NORBS
               J=(I*(I+1))/2
               P(J)=PDIAG(I)
               PA(J)=P(J)*W1*RANDOM
               RANDOM=1.D0/RANDOM
   50       PB(J)=P(J)*W2*RANDOM
            DO 60 I=1,LINEAR
               PBOLD(I)=PB(I)
   60       POLD(I)=PA(I)
         ENDIF
         HALFE=(NOPEN .NE. NCLOSE)
C
C   DETERMINE THE SELF-CONSISTENCY CRITERION
C
         IF( HALFE .OR.  INDEX(KEYWRD,'PREC') .NE. 0)
     3                               SCFCRT=SCFCRT*0.01D0
         IF( INDEX(KEYWRD,'POLAR') + INDEX(KEYWRD,'NLLSQ') +
     2 INDEX(KEYWRD,'SIGMA') .NE. 0) SCFCRT=SCFCRT*0.001D0
         IF(FORCE)                   SCFCRT=SCFCRT*0.0001D0
         SCFCRT=MAX(SCFCRT,1.D-12)
C
C  THE USER CAN STATE THE SCF CRITERION, IF DESIRED.
C
         I=INDEX(KEYWRD,'SCFCRT')
         IF(I.NE.0) THEN
            SCFCRT=READA(KEYWRD,I)
            WRITE(6,'(''  SCF CRITERION ='',G14.4)')SCFCRT
            IF(SCFCRT.LT.1.D-12)
     1 WRITE(6,'(//2X,'' THERE IS A RISK OF INFINITE LOOPING WITH'',
     2'' THE SCFCRT LESS THAN 1.D-12'')')
         ELSE
            IF(DEBUG)WRITE(6,'(''  SCF CRITERION ='',G14.4)')SCFCRT
         ENDIF
         LAST=0
C
C   END OF INITIALIZATION SECTION.
C
      ELSEIF(FORCE)THEN
C
C   RESET THE DENSITY MATRIX IF MECI HAS FORMED AN EXCITED STATE
C   SUM IS NOT USED
C
         SUM=MECI(EIGS,C,CBETA,EIGB, NORBS,NMOS,1,.FALSE.)
      ENDIF
C
C   INITIALIZATION OPERATIONS DONE EVERY TIME ITER IS CALLED
C
      MAKEA=.TRUE.
      MAKEB=.TRUE.
      IF(NEWDG) NEWDG=(ABS(BSHIFT).LT.0.001D0)
      IF(LAST.EQ.1) NEWDG=.FALSE.
      SELCON=SCFCRT*23.061
      IF(PLTEST.LT.SCFCRT) PLTEST=SCFCRT
      IF(NALPHA.NE.NBETA.OR..NOT.UHF)PLTEST=0.001D0
      IF(.NOT. FORCE .AND. .NOT. HALFE) THEN
         IF(GNORM.GT.5.D0) SELCON=SCFCRT*GNORM*0.2D0
         IF(GNORM.GT.200.D0) SELCON=SCFCRT*50.D0
      ENDIF
      IF(DEBUG)WRITE(6,'(''  SELCON, GNORM'',2G16.7)')SELCON,GNORM
      TITER1=SECOND()
      IF(PRT1EL) THEN
         WRITE(6,'(//10X,''ONE-ELECTRON MATRIX AT ENTRANCE TO ITER'')')
         CALL VECPRT(H,NORBS)
      ENDIF
      IREDY=1
   70 NITER=0
      TIME1=SECOND()
      FRST=.TRUE.
      IF(CAMKIN) THEN
         MODEA=1
         MODEB=1
      ELSE
         MODEA=0
         MODEB=0
      ENDIF
      BFRST=.TRUE.
**********************************************************************
*                                                                    *
*                                                                    *
*                START THE SCF LOOP HERE                             *
*                                                                    *
*                                                                    *
**********************************************************************
      INCITR=.TRUE.
   80 INCITR=(MODEA.NE.3.AND.MODEB.NE.3)
      IF(INCITR)NITER=NITER+1
      IF(NITER.GT.ITRMAX-10.AND..NOT.ALLCON) THEN
************************************************************************
*                                                                      *
*                   SWITCH ON ALL CONVERGERS                           *
*                                                                      *
************************************************************************
         WRITE(6,'(//,'' ALL CONVERGERS ARE NOW FORCED ON'',/
     1          '' SHIFT=10, PULAY ON, CAMP-KING ON'',/
     2          '' AND ITERATION COUNTER RESET'',//)')
         ALLCON=.TRUE.
         BSHIFT=4.44D0
         IREDY=-4
         EOLD=100.D0
         OKPULY=.TRUE.
         NEWDG=.FALSE.
         CAMKIN=(.NOT.HALFE)
         GOTO 70
      ENDIF
************************************************************************
*                                                                      *
*                        MAKE THE ALPHA FOCK MATRIX                    *
*                                                                      *
************************************************************************
      IF(ABS(SHIFT).GT.1.D-10.AND.BSHIFT .NE. 0.D0) THEN
         L=0
         IF(NITER.GT.1)THEN
            IF(NEWDG.AND..NOT.(HALFE.OR.CAMKIN))THEN
C
C  SHIFT WILL APPLY TO THE VIRTUAL ENERGY LEVELS USED IN THE
C  PSEUDODIAGONALIIZATION. IF DIFF IS -VE, GOOD, THEN LOWER THE
C  HOMO-LUMO GAP BY 0.1EV, OTHERWISE INCREASE IT.
               IF(DIFF.GT.0)THEN
               SHIFT=1.D0
C
C IF THE PSEUDODIAGONALIZATION APPROXIMATION -- THAT THE WAVEFUNCTION
C IS ALMOST STABLE -- IS INVALID, TURN OFF NEWDG
               IF(DIFF.GT.1)NEWDG=.FALSE.
               ELSE
               SHIFT=-0.1D0
               ENDIF
            ELSE
               SHIFT=TEN+EIGS(IHOMO+1)-EIGS(IHOMO)+SHIFT
            ENDIF
            IF(DIFF.GT.0.D0) THEN
               IF(SHIFT.GT.4.D0)SHFMAX=4.5D0
               IF(SHIFT.GT.SHFMAX)SHFMAX=MAX(SHFMAX-0.5D0,0.D0)
            ENDIF
C
C   IF SYSTEM GOES UNSTABLE, LIMIT SHIFT TO THE RANGE -INFINITY - SHFMAX
C   BUT IF SYSTEM IS STABLE, LIMIT SHIFT TO THE RANGE -INFINITY - +20
C
            SHIFT=MAX(-20.D0,MIN(SHFMAX,SHIFT))
            IF(ABS(SHIFT-SHFMAX).LT.1.D-5)SHFMAX=SHFMAX+0.01D0
C
C  THE CAMP-KING AND PULAY CONVERGES NEED A CONSTANT SHIFT.
C  IF THE SHIFT IS ALLOWED TO VARY, THESE CONVERGERS WILL NOT
C  WORK PROPERLY.
C
            IF(OKPULY.OR.ABS(BSHIFT-4.44D0).LT.1.D-5)THEN
            SHIFT=-8.D0
            IF(NEWDG) SHIFT=0
            ENDIF
            IF(UHF)THEN
               IF(NEWDG.AND..NOT.(HALFE.OR.CAMKIN))THEN
                  SHIFTB=TEN-TENOLD
               ELSE
                  SHIFTB=TEN+EIGS(IHOMOB+1)-EIGS(IHOMOB)+SHIFTB
               ENDIF
               IF(DIFF.GT.0.D0)SHIFTB=MIN(4.D0,SHIFTB)
               SHIFTB=MAX(-20.D0,MIN(SHFMAX,SHIFTB))
               IF(OKPULY.OR.ABS(BSHIFT-4.44D0).LT.1.D-5)THEN
               SHIFTB=-8.D0
               IF(NEWDG)SHIFT=0
               ENDIF
C#       WRITE(6,*)'SHIFT:',SHIFT,SHIFTB
               DO 90 I=IHOMOB+1,NORBS
   90          EIGB(I)=EIGB(I)+SHIFTB
            ELSE
C#       WRITE(6,*)'SHIFT:',SHIFT
            ENDIF
         ENDIF
         TENOLD=TEN
         DO 100 I=IHOMO+1,NORBS
  100    EIGS(I)=EIGS(I)+SHIFT
         DO 120 I=1,NORBS
            DO 110 J=1,I
               L=L+1
  110       F(L)=H(L)+SHIFT*PA(L)
  120    F(L)=F(L)-SHIFT
      ELSEIF (RAND.AND.LAST.EQ.0.AND.NITER.LT.2.AND.FULSCF)THEN
         RANDOM=0.001D0
         DO 130 I=1,LINEAR
            RANDOM=-RANDOM
  130    F(I)=H(I)+RANDOM
      ELSE
         DO 140 I=1,LINEAR
  140    F(I)=H(I)
      ENDIF
  150 CALL FOCK2(F,P,PA,W, WJ, WK,NUMAT,NFIRST,NMIDLE,NLAST)
      CALL FOCK1(F,P,PA,PB)
************************************************************************
*                                                                      *
*                        MAKE THE BETA FOCK MATRIX                     *
*                                                                      *
************************************************************************
      IF (UHF) THEN
         IF(SHIFTB .NE. 0.D0) THEN
            L=0
            DO 170 I=1,NORBS
               DO 160 J=1,I
                  L=L+1
  160          FB(L)=H(L)+SHIFTB*PB(L)
  170       FB(L)=FB(L)-SHIFTB
         ELSEIF (RAND.AND.LAST.EQ.0.AND.NITER.LT.2.AND.FULSCF)THEN
            RANDOM=0.001
            DO 180 I=1,LINEAR
               RANDOM=-RANDOM
  180       FB(I)=H(I)+RANDOM
         ELSE
            DO 190 I=1,LINEAR
  190       FB(I)=H(I)
         ENDIF
         CALL FOCK2(FB,P,PB,W, WJ, WK,NUMAT,NFIRST,NMIDLE,NLAST)
         CALL FOCK1(FB,P,PB,PA)
      ENDIF
      IF( .NOT. FULSCF) GOTO 290
      IF(PRTFOK) THEN
         WRITE(6,200)NITER
  200    FORMAT('   FOCK MATRIX ON ITERATION',I3)
         CALL VECPRT (F,NORBS)
      ENDIF
************************************************************************
*                                                                      *
*                        CALCULATE THE ENERGY IN KCAL/MOLE             *
*                                                                      *
************************************************************************
      EE=HELECT(NORBS,PA,H,F)
      IF(UHF)THEN
         EE=EE+HELECT(NORBS,PB,H,FB)
      ELSE
         EE=EE*2.D0
      ENDIF
      IF(CAPPS)EE=EE+CAPCOR(NAT,NFIRST,NLAST,NUMAT,P,H)
      SCORR=SHIFT*(NOPEN-NCLOSE)*23.061D0*0.25D0*(FRACT*(2.D0-FRACT))
      ESCF=(EE+ENUCLR)*23.061D0+ATHEAT+SCORR
      SELLIM=MAX(SELCON,EPS*MAX(ABS(EE),1.D0))
      IF(INCITR)THEN
         DIFF=ESCF-EOLD
         IF(DIFF.GT.0)THEN
            TEN=TEN-1.D0
         ELSE
            TEN=TEN*0.975D0+0.05D0
         ENDIF
C#         WRITE(6,'(2F12.6)')TEN,DIFF
         IF(NITER.GT.4.AND.(PL.EQ.0.D0.OR.PL.LT.PLTEST.AND.
     1   ABS(DIFF/MAX(1.D0,ABS(EE))).LT.SELLIM) .AND. READY) THEN
************************************************************************
*                                                                      *
*          SELF-CONSISTENCY TEST, EXIT MODE FROM ITERATIONS            *
*                                                                      *
************************************************************************
            IF (ABS(SHIFT) .LT. 1.D-10) GOTO 290
            SHIFT=0.D0
            SHIFTB=0.D0
            DO 210 I=1,LINEAR
  210       F(I)=H(I)
            MAKEA=.TRUE.
            MAKEB=.TRUE.
            GOTO 150
         ENDIF
        READY=(IREDY.GT.0.AND.(ABS(DIFF).LT.SELLIM*10.D0.OR.PL.EQ.0.D0))
        IREDY=IREDY+1
      ENDIF
      IF(PRTPL.OR.DEBUG.AND.NITER.GT.ITRMAX-20) THEN
         IF(ABS(ESCF).GT.99999.D0) ESCF=SIGN(9999.D0,ESCF)
         IF(ABS(DIFF).GT.9999.D0)DIFF=0.D0
         IF(INCITR)
     1    WRITE(6,'('' ITERATION'',I3,'' PLS='',2E10.3,'' ENERGY  '',
     2F14.7,'' DELTAE'',F13.7)')NITER,PL,PLB,ESCF,DIFF
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
      IF(MODEA.EQ.3)GOTO 230
      MAKEB=.TRUE.
      IF( NEWDG ) THEN
************************************************************************
*                                                                      *
*                        INVOKE PULAY'S CONVERGER                      *
*                                                                      *
************************************************************************
         IF(OKPULY.AND.MAKEA.AND.IREDY.GT.1)
     1CALL PULAY(F,PA,NORBS,POLD,POLD2,POLD3,JALP,IALP,MPACK,FRST,PL)
************************************************************************
*                                                                      *
*           DIAGONALIZE THE ALPHA OR RHF SECULAR DETERMINANT           *
* WHERE POSSIBLE, USE THE PULAY-STEWART METHOD, OTHERWISE USE BEPPU'S  *
*                                                                      *
************************************************************************
         IF (HALFE.OR.CAMKIN) THEN
            CALL RSP(F,NORBS,NORBS,EIGS,C)
         ELSE
            CALL DIAG (F,C,NA1EL,EIGS,NORBS,NORBS)
         ENDIF
      ELSE
         CALL RSP(F,NORBS,NORBS,EIGS,C)
      ENDIF
      J=1
      IF(PRTVEC) THEN
         J=1
         IF(UHF)J=2
         WRITE(6,'(//10X,A,
     1'' EIGENVECTORS AND EIGENVALUES ON ITERATION'',I3)')
     2   ABPRT(J),NITER
         CALL MATOUT(C,EIGS,NORBS,NORBS,NORBS)
      ELSE
         IF (PRTEIG) WRITE(6,220)ABPRT(J),NITER,(EIGS(I),I=1,NORBS)
      ENDIF
  220 FORMAT(10X,A,'  EIGENVALUES ON ITERATION',I3,/10(6G13.6,/))
  230 IF(IFILL.NE.0)CALL SWAP(C,NORBS,NORBS,NA2EL,IFILL)
************************************************************************
*                                                                      *
*            CALCULATE THE ALPHA OR RHF DENSITY MATRIX                 *
*                                                                      *
************************************************************************
      IF(UHF)THEN
         CALL DENSIT( C,NORBS, NORBS, NA2EL,NA1EL, FRACT, PA, 1)
         IF(MODEA.NE.3.AND..NOT. (NEWDG.AND.OKPULY))
     1    CALL CNVG(PA, POLD, POLD2, NORBS, NITER, PL)
      ELSE
         CALL DENSIT( C,NORBS, NORBS, NA2EL,NA1EL, FRACT, P, 1)
         IF(MODEA.NE.3.AND..NOT. (NEWDG.AND.OKPULY))
     1    CALL CNVG(P, POLD, POLD2, NORBS, NITER, PL)
      ENDIF
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
         IF(MODEB.EQ.3) GOTO 240
         MAKEA=.TRUE.
         IF( NEWDG ) THEN
************************************************************************
*                                                                      *
*                        INVOKE PULAY'S CONVERGER                      *
*                                                                      *
************************************************************************
            IF( OKPULY.AND.MAKEB.AND.IREDY.GT.1)
     1CALL PULAY(FB,PB,NORBS,PBOLD,PBOLD2,
     2PBOLD3,JBET,IBET,MPACK,BFRST,PLB)
************************************************************************
*                                                                      *
*           DIAGONALIZE THE ALPHA OR RHF SECULAR DETERMINANT           *
* WHERE POSSIBLE, USE THE PULAY-STEWART METHOD, OTHERWISE USE BEPPU'S  *
*                                                                      *
************************************************************************
            IF (HALFE.OR.CAMKIN) THEN
               CALL RSP(FB,NORBS,NORBS,EIGB,CBETA)
            ELSE
               CALL DIAG (FB,CBETA,NB1EL,EIGB,NORBS,NORBS)
            ENDIF
         ELSE
            CALL RSP(FB,NORBS,NORBS,EIGB,CBETA)
         ENDIF
         IF(PRTVEC) THEN
            WRITE(6,'(//10X,A,'' EIGENVECTORS AND EIGENVALUES ON '',
     1''ITERATION'',I3)')ABPRT(3),NITER
            CALL MATOUT(CBETA,EIGB,NORBS,NORBS,NORBS)
         ELSE
            IF (PRTEIG) WRITE(6,220)ABPRT(3),NITER,(EIGB(I),I=1,NORBS)
         ENDIF
************************************************************************
*                                                                      *
*                CALCULATE THE BETA DENSITY MATRIX                     *
*                                                                      *
************************************************************************
  240    CALL DENSIT( CBETA,NORBS, NORBS, NB2EL, NB1EL, FRACT, PB, 1)
         IF( .NOT. (NEWDG.AND.OKPULY))
     1CALL CNVG(PB, PBOLD, PBOLD2, NORBS, NITER, PLB)
      ENDIF
************************************************************************
*                                                                      *
*                   CALCULATE THE TOTAL DENSITY MATRIX                 *
*                                                                      *
************************************************************************
      IF(UHF) THEN
         DO 250 I=1,LINEAR
  250    P(I)=PA(I)+PB(I)
      ELSE
         DO 260 I=1,LINEAR
            PA(I)=P(I)*0.5D0
  260    PB(I)=PA(I)
      ENDIF
      IF(PRTDEN) THEN
         WRITE(6,'('' DENSITY MATRIX ON ITERATION'',I4)')NITER
         CALL VECPRT (P,NORBS)
      ENDIF
      OKNEWD=(PL.LT.SELLIM .OR. OKNEWD)
      NEWDG=(PL.LT.TRANS .AND. OKNEWD .OR. NEWDG)
      IF(PL.LT.TRANS*0.3333D0)OKNEWD=.TRUE.
      IF (NITER .GE. ITRMAX) THEN
         IF(DIFF.LT.1.D-3.AND.PL.LT.1.D-4.AND..NOT.FORCE)THEN
            WRITE(6,'('' """""""""""""""UNABLE TO ACHIEVE SELF-CONSISTEN
     1CE, JOB CONTINUING'')')
            GOTO 290
         ENDIF
         IF(MINPRT)WRITE (6,270)
  270    FORMAT (//10X,'"""""""""""""UNABLE TO ACHIEVE SELF-CONSISTENCE'
     1,/)
         WRITE (6,280) DIFF,PL
  280    FORMAT (//,10X,'DELTAE= ',E12.4,5X,'DELTAP= ',E12.4,///)
         IFLEPO=9
         IITER=2
         CALL WRITE (TIME0,ESCF)
         STOP
      ENDIF
      GO TO 80
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
  290 EE=HELECT(NORBS,PA,H,F)
      IF(UHF) THEN
         EE=EE+HELECT(NORBS,PB,H,FB)
      ELSE
         EE=EE*2.D0 +
     1SHIFT*(NOPEN-NCLOSE)*23.061D0*0.25D0*(FRACT*(2.D0-FRACT))
      ENDIF
      IF(CAPPS)EE=EE+CAPCOR(NAT,NFIRST,NLAST,NUMAT,P,H)
C
C   NORMALLY THE EIGENVALUES ARE INCORRECT BECAUSE THE 
C   PSEUDODIAGONALIZATION HAS BEEN USED.  IF THIS
C   IS THE LAST SCF, THEN DO AN EXACT DIAGONALIZATION
      IF( NSCF.EQ.0 .OR. LAST.EQ.1 .OR. CI .OR. HALFE ) THEN
C
C  PUT F AND FB INTO POLD IN ORDER TO NOT DESTROY F AND FB
C  AND DO EXACT DIAGONALISATIONS
         DO 300 I=1,LINEAR
  300    POLD(I)=F(I)
         CALL RSP(POLD,NORBS,NORBS,EIGS,C)
         IF(UHF) THEN
            DO 310 I=1,LINEAR
  310       POLD(I)=FB(I)
            CALL RSP(POLD,NORBS,NORBS,EIGB,CBETA)
         ENDIF
         DO 320 I=1,LINEAR
  320    POLD(I)=P(I)
         IF(CI.OR.HALFE) THEN
            SUM=MECI(EIGS,C,CBETA,EIGB, NORBS,NMOS,0,.FALSE.)
            EE=EE+SUM
            IF(PRTPL)THEN
               ESCF=(EE+ENUCLR)*23.061 +ATHEAT
               WRITE(6,'(27X,''AFTER MECI, ENERGY  '',F14.7)')ESCF
            ENDIF
         ENDIF
      ENDIF
      NSCF=NSCF+1
      TITER2=SECOND()
      IF(TIMES) WRITE(6,'('' TIME FOR SCF CALCULATION'',F8.2,
     1''    INTEGRAL'',F8.2)')TITER2-TITER1,TITER2-TIME0
      IF(DEBUG)WRITE(6,'('' NO. OF ITERATIONS ='',I6)')NITER
C            IF(FORCE)  SCFCRT=1.D-5
      IF(ALLCON.AND.ABS(BSHIFT-4.44D0).LT.1.D-7)THEN
         CAMKIN=.FALSE.
         ALLCON=.FALSE.
         NEWDG=.FALSE.
         BSHIFT=-10.D0
         OKPULY=.FALSE.
      ENDIF
      SHIFT=1.D0
      RETURN
C
      END
