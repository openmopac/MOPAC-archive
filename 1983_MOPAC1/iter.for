      SUBROUTINE ITER  (H, W, EE, FULSCF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES/NOLIST'
      PARAMETER (MPULAY=MPACK)
*
* MPULAY IS USED IN THE PULAY CONVERGER. IT SHOULD BE AS LARGE AS
*        CONVENIENT, PREFERABLY 10 TO 20 TIMES SIZE OF A NORMAL
*        DENSITY MATRIX. IF SPACE IS LIMITED, 2 TIMES MPACK WILL
*        SUFFICE.
*        
      DIMENSION H(*), W(*)
      COMMON /FOKMAT/ F(MPACK), FB(MPACK)
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON /GRADNT/ DUMY(MAXPAR),GNORM
      COMMON /LAST  / LAST
      COMMON /MESAGE/ IFLEPO,IITER
      COMMON /CITERM/ XI,XJ,XK
      COMMON /PATH  / LATOM,LPARAM,REACT(100)
      COMMON /NUMCAL/ NUMCAL
      COMMON /TIME  / TIME0
      LOGICAL FULSCF
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
C             DEWAR, M.J.S., THIEL,W., J. AM. CHEM. SOC., 99, 4899, (1977).
C   ON SHIFT: "UNCONDITIONAL CONVERGENCE IN SCF THEORY: A GENERAL LEVEL
C             SHIFT TECHNIQUE" 
C             CARBO, R., HERNANDEZ, J.A., SANZ, F., CHEM. PHYS. LETT.,
C             47, 581, (1977)
C   ON HALF-ELECTRON: "MINDO/3 COMPARISON OF THE GENERALISED S.C.F. COUPLING
C             OPERATOR AND "HALF-ELECTRON" METHODS FOR CALCULATING THE
C             ENERGIES AND GEOMETRIES OF OPEN SHELL SYSTEMS"
C             DEWAR, M.J.S., OLIVELLA, S., J. AM. CHEM. SOC., 75, 829, (1979).
C   ON PULAY'S CONVERGER: "CONVERGANCE ACCELERATION OF ITERATIVE SEQUENCES.
C             THE CASE OF SCF ITERATION", PULAY, P., CHEM. PHYS. LETT,
C             73, 393, (1980).
C   ON PSEUDODIAGONALISATION: "FAST SEMIEMPIRICAL CALCULATIONS",
C             STEWART. J.J.P., CSASZAR, P., PULAY, P., J. COMP. CHEM.,
C             3, 227, (1982)
C
C***********************************************************************
      DIMENSION POLD(MPULAY), POLD2(MPULAY), POLD3(400)
      DIMENSION PBOLD(MPULAY), PBOLD2(MPULAY), PBOLD3(400)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     +                NLAST(NUMATM), NORBS, NELECS,
     1                NALPHA, NBETA, NCLOSE, NOPEN
     2      /MOLORB/ DUMMY(MAXORB),PDIAG(MAXORB)
     3      /KEYWRD/ KEYWRD
     4      /NUMSCF/ NSCF
      CHARACTER KEYWRD*80, ABPRT(3)*5
      LOGICAL PRTFOK,PRTEIG,PRTDEN, DEBUG, PRTENG, TIMES, CI
     +,UHF, NEWDG, SCF1, HALFE, TRIPLT, FORCE, PRT1EL,PRTPL, OKNEWD
     1,EXCITD, MINPRT, FRST, BFRST, OKPULY, READY, PRTVEC
      DATA ICALCN/0/, DEBUG/.FALSE./, PRTFOK/.FALSE./
      DATA PRTEIG/.FALSE./,PRTDEN,PRTENG/.FALSE.,.FALSE./
      DATA PRT1EL/.FALSE./
      DATA ABPRT/'     ','ALPHA',' BETA'/
C
C  INITIALIZE
C
      NITER=0
      EEOLD=1.D8
      READY=.FALSE.
      IITER=1
            IF (ICALCN.NE.NUMCAL) THEN
            ICALCN=NUMCAL
C    SET UP INITITAL DENSITY MATRIX
            LINEAR=(NORBS*(NORBS+1))/2
            IF( INDEX(KEYWRD,'ITER') .NE. 0 ) DEBUG  = .TRUE.
            MINPRT=(INDEX(KEYWRD,'SADDLE')+
     +      LATOM .EQ.0 .OR. DEBUG)
                IF( INDEX(KEYWRD,'EIGS') .NE. 0 ) PRTEIG = .TRUE.
                IF( INDEX(KEYWRD,'ENERGY').NE.0 ) PRTENG = .TRUE.
                IF( INDEX(KEYWRD,' PL ')  .NE.0 ) PRTPL  = .TRUE.
            IF( INDEX(KEYWRD,'DEBUG').NE. 0 ) THEN
                IF( INDEX(KEYWRD,'1ELEC') .NE.0 ) PRT1EL = .TRUE.
                IF( INDEX(KEYWRD,'DENSITY').NE.0) PRTDEN = .TRUE.
                IF( INDEX(KEYWRD,'FOCK') .NE. 0 ) PRTFOK = .TRUE.
                IF( INDEX(KEYWRD,'VECT') .NE. 0 ) PRTVEC = .TRUE.
            END IF
            NEWDG=.FALSE.
            PL=1.D0
            I=INDEX(KEYWRD,'FILL=')
            IF(I.NE.0) IFILL=-READA(KEYWRD,I)
            I=INDEX(KEYWRD,'SHIFT')
            IF(I.NE.0) BSHIFT=-READA(KEYWRD,I)
            BSHIFT=0.D0
            OKNEWD=ABS(BSHIFT) .LT. 0.001D0
            CI=(INDEX(KEYWRD,'C.I.') .NE. 0)
            OKPULY=(INDEX(KEYWRD,'PULAY').NE.0) 
            UHF=(INDEX(KEYWRD,'UHF') .NE. 0)
            TRANS=0.1D0
          I=0
          IF(INDEX(KEYWRD,'RESTART')+INDEX(KEYWRD,'OLDENS')
     +      .NE. 0) THEN
              REWIND 10
              READ(10)(PA(I),I=1,LINEAR)
              IF(UHF)READ(10)(PB(I),I=1,LINEAR)
              I=1
          ENDIF
          IF(I.EQ.1.OR.INDEX(KEYWRD,'RESTART') .NE. 0) THEN
              IF( UHF) THEN
                  DO 11 I=1,LINEAR
  11                  P(I)=PA(I)+PB(I)
              ELSE
                  DO 10 I=1,LINEAR
  10                  P(I)=PA(I)*2.D0
              ENDIF
          ELSE
          IF(INDEX(KEYWRD,'PARAM') .EQ. 0)THEN
                  DO 12 I=1,LINEAR
                    P(I)=0.D0
                    PA(I)=0.D0
  12                PB(I)=0.D0
                    W1=NALPHA/(NALPHA+1.D-6+NBETA)
                    W2=NBETA/(NALPHA+1.D-6+NBETA)
                    IF(W1.LT.1.D-6)W1=0.5D0
                    IF(W2.LT.1.D-6)W2=0.5D0
                    RANDOM=1.0D0
                    IF(UHF .AND. NALPHA.EQ.NBETA) RANDOM=1.02D0
                  DO 14 I=1,NORBS
                    J=(I*(I+1))/2
                    P(J)=PDIAG(I)
                    PA(J)=P(J)*W1*RANDOM
                  RANDOM=1.D0/RANDOM
  14              PB(J)=P(J)*W2*RANDOM
                  DO 18 I=1,LINEAR
                    PBOLD(I)=PB(I)
  18                POLD(I)=PA(I)
             ENDIF
          ENDIF
            SCF1=(INDEX(KEYWRD,'1SCF') .NE. 0)
            IF( .NOT. SCF1) SCF1=(INDEX(KEYWRD,'PARAM') .NE. 0)
            HALFE=(NOPEN .NE. NCLOSE)
            TRIPLT=(INDEX(KEYWRD,'TRIPLET') .NE. 0)
            EXCITD=(INDEX(KEYWRD,'EXCITED') .NE. 0)
            IF( HALFE ) THEN
                IF(NOPEN-NCLOSE.EQ.1) THEN
                    IHOMO=(NCLOSE-1)*NORBS+1
                    ILUMO=IHOMO+NORBS
                    ELSE
                    IPART1=NCLOSE*NORBS+1
                    IPART2=IPART1+NORBS
                ENDIF
        ENDIF
            SCFCRT=1.D-7
            PLTEST=0.00001D0
            IF(HALFE)SCFCRT=SCFCRT*0.0001D0
            FORCE=(INDEX(KEYWRD,'FORCE') .NE. 0)
            IF( FORCE ) SCFCRT=SCFCRT*0.01D0
            IF(INDEX(KEYWRD,'PREC') .NE. 0)SCFCRT=SCFCRT*0.01D0
            IF(SCFCRT.LT.1.D-9)SCFCRT=1.D-9
            I=INDEX(KEYWRD,'SCFCRT')
            IF(I.NE.0) THEN
                SCFCRT=READA(KEYWRD,I)
                WRITE(6,'(''  SCF CRITERION ='',F13.9)')SCFCRT
                ELSE
                IF(DEBUG)WRITE(6,'(''  SCF CRITERION ='',F13.9)')SCFCRT
            ENDIF
            TIMES=(INDEX(KEYWRD,'TIMES') .NE. 0)
            NSCF=0
            ITRMAX = 200
            IF( UHF ) THEN
                NOCC = NALPHA
                NVIR = NORBS-NALPHA
                NOCCB= NBETA
                NVIRB= NORBS-NOCCB
                IF(DEBUG)WRITE(6,'('' NALPHA,NBETA'',2I6)')NALPHA,NBETA
            ELSE
                PLB=0.D0
                NOCC=NCLOSE
            ENDIF
      LAST=0
      END IF
      IF(NEWDG) NEWDG=(ABS(BSHIFT).LT.0.001D0)
      IF(LAST.EQ.1) NEWDG=.FALSE.
      SELCON=SCFCRT
      IF(.NOT. HALFE) THEN
      IF(GNORM.GT.5.D0) SELCON=SCFCRT*GNORM*0.2D0
      IF(GNORM.GT.200.D0) SELCON=SCFCRT*50.D0
      ENDIF
      IF(DEBUG)WRITE(6,'(''  SELCON, GNORM'',2F16.7)')SELCON,GNORM
      TITER1=SECOND()
      IF(PRT1EL) THEN
          WRITE(6,'(//10X,''ONE-ELECTRON MATRIX AT ENTRANCE TO ITER'')')
          CALL VECPRT(H,NORBS)
      ENDIF
      TIME1=SECOND()
      FRST=.TRUE.
      BFRST=.TRUE.
**********************************************************************
*                                                                    *
*                                                                    *
*                START THE SCF LOOP HERE                             *
*                                                                    *
*                                                                    *
**********************************************************************
   40 NITER=NITER+1
C
      IF(BSHIFT .NE. 0.D0) THEN
          L=0
        SHIFT=BSHIFT*(NITER+1.D0)**(-1.5D0)+2.D0*COS
          DO 49 I=1,NORBS
              DO 53 J=1,I
                  L=L+1
  53          F(L)=H(L)+SHIFT*PA(L)
  49      F(L)=F(L)-SHIFT
      ELSE
          DO 50 I=1,LINEAR
   50         F(I)=H(I)
      ENDIF
C
C *** CONSTRUCT THE F-MATRIX.
C
      IF (UHF) THEN
          CALL FOCK2(F,P,PA,W,NUMAT,NFIRST,NLAST)
          CALL FOCK1(F,P,PA,PB)
          IF(SHIFT .NE. 0.D0) THEN
              L=0
              DO 58 I=1,NORBS
                  DO 59 J=1,I
                      L=L+1
  59              FB(L)=H(L)+SHIFT*PB(L)
  58          FB(L)=FB(L)-SHIFT
          ELSE
              DO 57 I=1,LINEAR
   57             FB(I)=H(I)
          ENDIF
          CALL FOCK2(FB,P,PB,W,NUMAT,NFIRST,NLAST)
          CALL FOCK1(FB,P,PB,PA)
      ELSE
          CALL FOCK2(F,P,PA,W,NUMAT,NFIRST,NLAST)
          CALL FOCK1(F,P,PA,PA)
      ENDIF
      IF( .NOT. FULSCF) GOTO 63
      IF(PRTFOK) THEN
            WRITE(6,308)NITER
  308       FORMAT('   FOCK MATRIX ON ITERATION',I3)
            CALL VECPRT (F,NORBS)
      END IF
C
C
C *** DIAGONALISE THE F-MATRIX.
C
C#         IF(FRST)WRITE(6,'('' SWITCHING TO PULAY'')')
      EE=HELECT(NORBS,PA,H,F)
      IF(UHF)THEN
      SUM2=HELECT(NORBS,PB,H,FB)
C#      WRITE(6,'('' ELECTRONIC ENERGY:'',3E20.12)')EE,SUM2,EE+SUM2
      EE=EE+SUM2
      ELSE
C#      WRITE(6,'('' ELECTRONIC ENERGY:'', E22.14)')EE*2.D0
      ENDIF
      IF(PL.LT.PLTEST.AND.ABS(EE-EEOLD).LT.SELCON .AND. READY) GOTO 63
      READY=(ABS(EE-EEOLD).LT.SELCON*10.D0) 
      EEOLD=EE
      IF( NEWDG ) THEN
      IF(OKPULY)
     +CALL PULAY(F,PA,NORBS,POLD,POLD2,POLD3,JALP,IALP,MPULAY,FRST,PL)
*
*   TEST PL TO SEE IF SCF FIELD ACCEPTABLE. EXIT IF O.K. SO THAT 
*   DENSITY MATRICES ARE THOSE THAT GIVE RISE TO THE FOCK MATRICES, 
*   AND NOT VICE-VERSA.
*
C#      IF(PRTPL)
C#     +WRITE(6,'('' ITERATION'',I3,'' PLS='',2E12.4)')NITER,PL
C#      IF(PL.LT.SELCON) GOTO 63
            IF (HALFE) THEN
                CALL HQRII(F,NORBS,NORBS,EIGS,C)
            ELSE
                CALL DIAG (F,C,NOCC,EIGS,NORBS,NORBS)
            ENDIF
C#      WRITE(6,'('' ELECTRONIC ENERGY AFTER DIAG:'',F19.12)')
C#     +HELECT(NORBS,PA,H,F)
            ELSE
            CALL HQRII(F,NORBS,NORBS,EIGS,C)
      END IF
C
      IF(PRTVEC) THEN
      J=1
      IF(UHF)J=2
      WRITE(6,'(//10X,A,
     +'' EIGENVECTORS AND EIGNEVALUES ON ITERATION'',I3)')
     +   ABPRT(J),NITER
      CALL MATOUT(C,EIGS,NORBS,NORBS,NORBS)
      ELSE
      IF (PRTEIG) WRITE(6,311)ABPRT(J),NITER,(EIGS(I),I=1,NORBS)
      ENDIF
  311       FORMAT(10X,A,'  EIGENVALUES ON ITERATION',I3,/10(6F13.6,/))
C
C *** COMPUTE THE BOND-ORDERS
C
      IF( UHF ) THEN
          I=0
          CALL DENSTY( C,NORBS, NORBS, I, NALPHA, PA)
          IF(.NOT. (NEWDG.AND.OKPULY))
     +    CALL CNVG(PA, POLD, POLD2, NORBS, NITER, PL)
          IF (NEWDG.AND.OKPULY) THEN
          CALL PULAY(FB,PB,NORBS,PBOLD,PBOLD2,
     +               PBOLD3,JBET,IBET,MPULAY,BFRST,PLB)
                CALL DIAG (FB,CBETA,NOCCB,EIGB,NORBS,NORBS)
                ELSE
                CALL HQRII(FB,NORBS,NORBS,EIGB,CBETA)
          END IF
      IF(PRTVEC) THEN
      J=1
      IF(UHF) J=3
      WRITE(6,'(//10X,A,
     +'' EIGENVECTORS AND EIGNEVALUES ON ITERATION'',I3)')
     +   ABPRT(J),NITER
      CALL MATOUT(CBETA,EIGB,NORBS,NORBS,NORBS)
      ELSE
      IF (PRTEIG) WRITE(6,311)ABPRT(J),NITER,(EIGB(I),I=1,NORBS)
      ENDIF
          I=0
          CALL DENSTY( CBETA,NORBS, NORBS, I, NBETA, PB)
          IF( .NOT. (NEWDG.AND.OKPULY))
     +      CALL CNVG(PB, PBOLD, PBOLD2, NORBS, NITER, PLB)
          DO 60 I=1,LINEAR
  60           P(I)=PA(I)+PB(I)
      ELSE
          IF(IFILL.NE.0)CALL SWAP(C,NORBS,NORBS,NCLOSE,IFILL)
          CALL DENSTY( C,NORBS, NORBS, NCLOSE, NOPEN, P)
          IF(.NOT.(NEWDG.AND.OKPULY)) THEN
              CALL CNVG(P, POLD, POLD2, NORBS, NITER, PL)
          ENDIF
          DO 51 I=1,LINEAR
  51      PA(I)=P(I)*0.5D0
C#      WRITE(6,'('' ELECTRONIC ENERGY AFTER DENSITY:'',F19.12)')
C#     +HELECT(NORBS,PA,H,F)
      ENDIF
      IF(PRTDEN) THEN
            WRITE(6,'('' DENSITY MATRIX ON ITERATION'',I4)')NITER
            CALL VECPRT (P,NORBS)
      END IF
              IF(PRTPL)
     +        WRITE(6,'('' ITERATION'',I3,'' PLS='',2E12.4)')
     +        NITER,PL,PLB
      OKNEWD=(PL.LT.SELCON .OR. OKNEWD)
      NEWDG=(PL.LT.TRANS .AND. OKNEWD .OR. NEWDG)
      IF(PL.LT.TRANS*0.3333D0)THEN
      OKNEWD=.TRUE.
      ENDIF
C
      IF (NITER .GE. ITRMAX) THEN
            IF(MINPRT)WRITE (6,260)
  260 FORMAT (//10X,'"""""""""""""UNABLE TO ACHIEVE SELF-CONSISTENCE'
     1,/)
            WRITE (6,270) DELTA,PL
  270       FORMAT (//,10X,  8HDELTAE= ,F15.7,5X,  8HDELTAP= ,F15.7,///)
C#          IF(PL .GT. 0.1D0) THEN
              IFLEPO=9
              IITER=2
              CALL WRITE (TIME0,ESCF)
              STOP
C#          ENDIF
C#      GOTO 63
      END IF
      GO TO 40
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
  63  EE=HELECT(NORBS,PA,H,F)
C#      WRITE(6,'(''  EE AFTER SCF'',F19.11)')EE
      IF(UHF) THEN
          EE=EE+HELECT(NORBS,PB,H,FB)
          ELSE
          EE=EE*2.D0
          ENDIF
      IF( CI .OR. SCF1 .OR. HALFE ) THEN
C
C  PUT F AND FB INTO POLD IN ORDER TO NOT DESTROY F AND FB
C  AND DO EXACT DIAGONALISATIONS
            DO 65 I=1,LINEAR
  65        POLD(I)=F(I)
            CALL HQRII(POLD,NORBS,NORBS,EIGS,C)
            IF(UHF) THEN
                DO 66 I=1,LINEAR
  66            POLD(I)=FB(I)
                CALL HQRII(POLD,NORBS,NORBS,EIGB,CBETA)
            ENDIF
       ENDIF
       IF( HALFE ) THEN
          IF(NOPEN-NCLOSE.EQ.1) THEN
              XI=SPCG(C(ILUMO),C(ILUMO),C(ILUMO),C(ILUMO),W)
              EE=EE-0.25D0*XI
              ELSE
              XI=SPCG(C(IPART1),C(IPART1),C(IPART1),C(IPART1),W)
              XJ=SPCG(C(IPART2),C(IPART2),C(IPART2),C(IPART2),W)
              XK=SPCG(C(IPART1),C(IPART2),C(IPART2),C(IPART1),W)
              IF( TRIPLT )THEN
                EE=EE-0.25D0*(XI+XJ) -0.5D0 *XK
                ELSE              
*
*  C.I. MUST BE USED WITH BIRADICAL SINGLETS.
*
                  EE=EE-0.25D0*(XI+XJ) +1.5D0 *XK
                  EE=OPCI(EE,XI,XJ,XK)
                ENDIF
          ENDIF
          ELSE
          IF( CI ) EE=OPCI(EE,XI,XJ,XK)
C#      WRITE(6,'(/10X,''  EE AFTER C.I.:'',F19.12)')EE
      ENDIF
            NSCF=NSCF+1
            TITER2=SECOND()
            IF(TIMES) WRITE(6,'('' TIME FOR SCF CALCULATION'',F8.2,
     +''    INTEGRAL'',F8.2)')TITER2-TITER1,TITER2-TIME0
            IF(DEBUG)WRITE(6,'('' NO. OF ITERATIONS ='',I3)')NITER
C            IF(FORCE)  SCFCRT=1.D-5
      IF(HALFE) BSHIFT=0.D0
      RETURN
C
      END
