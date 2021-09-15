      FUNCTION MECI(EIGS,COEFF,COEFFS,EIGA,N,NMOS,IDUMMY,FINISH)
***********************************************************************
*
*                 PROGRAM MECI
*
*   A MULTI-ELECTRON CONFIGURATION INTERACTION CALCULATION
*
*   WRITTEN BY JAMES J. P. STEWART, AT THE
*              FRANK J. SEILER RESEARCH LABORATORY
*              USAFA, COLORADO SPRINGS, CO 80840
*
*              1985
*
***********************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES'
      DOUBLE PRECISION MECI
C
C   MATRICES FOR SEC.DET., VECTORS, AND EIGENVALUES.
C
      DIMENSION CIMAT(NMECI**4), CONF(NMECI**4),
     1 EIG(NMECI**2), DIAG(2*NMECI**3)
C
C   MATRICES TO HOLD ELECTRON CONFIGURATIONS
C
      DIMENSION MICROA(NMECI,2*NMECI**3), MICROB(NMECI,2*NMECI**3),
     1IOCCA1(NMECI), IOCCA2(NMECI), IOCCB1(NMECI),
     2IOCCB2(NMECI), NALPHA(NMECI**2)
C
C   MATRICES FOR PERMUTATION WORK
C
      DIMENSION NFA(2*NMECI), NPERMA(NMECI,6*NMECI),
     1NPERMB(NMECI,6*NMECI)
C
C   MATRICES FOR ONE AND TWO ELECTRON INTEGRALS, AND M.O.S
C
      DIMENSION EIGA(MAXORB), EIGS(MAXORB), RJKAA(NMECI,NMECI),
     1          RJKAB(NMECI,NMECI), COEFF(N,N),  COEFFS(N,N)
C
C   SPIN MATRICES
C
      DIMENSION SPIN(NMECI**2)
      LOGICAL DEBUG,  LARGE, PRNT, FIRST, LSPIN, LSPIN1, FINISH,
     1 FIRST1, BIGPRT, SING, DOUB, TRIP, QUAR, QUIN, SEXT, LAST1,
     2 PRNT2, FORCE, GEOOK
      CHARACTER KEYWRD*80, TSPIN(7)*8, LINE*80
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,
     2                NDUMMY(2), NCLOSE, NOPEN, NDUMY, FRACT
      COMMON /SPQR/ ISPQR(NMECI**2,NMECI),IS,I,K
C#      COMMON /LAST  / LAST
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /KEYWRD/ KEYWRD
      COMMON /BASEOC/ OCCA(NMECI)
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      DATA TSPIN/'SINGLET ','DOUBLET ','TRIPLET ','QUARTET ','QUINTET ',
     1'SEXTET  ','SEPTET  '/
      DATA FIRST/.TRUE./, FIRST1/.TRUE./
      IF(FIRST)THEN
         FIRST=.FALSE.
         GEOOK=(INDEX(KEYWRD,'GEO-OK').NE.0)
         FORCE=(INDEX(KEYWRD,'FORCE').NE.0)
         LAST1=(INDEX(KEYWRD,'1SCF').NE.0)
         LSPIN1=(INDEX(KEYWRD,'ESR').NE.0)
         MDIM=NMECI**2
         DEBUG=(INDEX(KEYWRD,'DEBUG').NE.0)
         PRNT2=(INDEX(KEYWRD,'MECI').NE.0)
         DEBUG=(DEBUG.AND.PRNT2)
         LARGE=(INDEX(KEYWRD,'LARGE').NE.0)
         LROOT=1
         IF(INDEX(KEYWRD,'EXCI').NE.0)LROOT=2
         I=INDEX(KEYWRD,'ROOT')
         IF(I.NE.0)LROOT=READA(KEYWRD,I)
C#         WRITE(6,'('' NCLOSE, NOPEN'',2I6)')NCLOSE,NOPEN
         J=MAX(MIN((NCLOSE+NOPEN+1)/2-(NMOS-1)/2,NORBS-NMOS+1),1)
         L=0
         DO 10 I=J,NCLOSE
            L=L+1
   10    OCCA(L)=1
         DO 20 I=NCLOSE+1,NOPEN
            L=L+1
   20    OCCA(L)=FRACT*0.5D0
         DO 30 I=NOPEN+1,J+NMOS-1
            L=L+1
   30    OCCA(L)=0.D0
C#         WRITE(6,'('' INITIAL ORBITAL OCCUPANCIES'')')
C#         WRITE(6,'(6F12.6)')(OCCA(L),L=1,NMOS)
         SING=(INDEX(KEYWRD,'SINGL')+
     1         INDEX(KEYWRD,'EXCI')+
     2         INDEX(KEYWRD,'BIRAD').NE.0)
         DOUB=(INDEX(KEYWRD,'DOUBL').NE.0)
         TRIP=(INDEX(KEYWRD,'TRIPL').NE.0)
         QUAR=(INDEX(KEYWRD,'QUART').NE.0)
         QUIN=(INDEX(KEYWRD,'QUINT').NE.0)
         SEXT=(INDEX(KEYWRD,'SEXTE').NE.0)
         SMULT=-.5D0
         IF(SING) SMULT=0.00D0
         IF(DOUB) SMULT=0.75D0
         IF(TRIP) SMULT=2.00D0
         IF(QUAR) SMULT=3.75D0
         IF(QUIN) SMULT=6.00D0
         IF(SEXT) SMULT=8.75D0
C#      WRITE(6,'('' ORBITAL COUNTERS, PER ATOM, FIRST LAST'')')
C#      WRITE(6,'(I40,I6)')(NFIRST(I),NLAST(I),I=1,NUMAT)
         X=0.D0
         DO 40 J=1,NMOS
   40    X=X+OCCA(J)
         XX=X+X
         NE=XX+0.5
         NELEC=(NELECS-NE+1)/2
         NLEFT=NORBS-NMOS-NELEC
      ENDIF
      PRNT=(DEBUG.OR.FINISH.AND.PRNT2)
      BIGPRT=(PRNT.AND.LARGE)
      LAST1=(LAST1.OR.FINISH)
C
C    TEST TO SEE IF THE SET OF ENERGY LEVELS USED IN MECI IS COMPLETE,
C    I.E., ALL COMPONENTS OF DEGENERATE IRREDUCIBLE REPRESENTATIONS
C    ARE USED.  IF NOT, THEN RESULTS WILL BE NONSENSE.  GIVE USERS A
C    CHANCE TO REALLY FOUL THINGS UP BY ALLOWING JOB TO CONTINUE IF
C    'GEO-OK' IS SPECIFIED.
C
      DO 60 I=1,NMOS
         IN=I+NELEC
         DO 50 J=1,NORBS
   50    COEFFS(J,I)=COEFF(J,IN)
   60 EIGA(I)=EIGS(IN)
      LSPIN=(LSPIN1.AND. FINISH)
      IF(BIGPRT)THEN
         WRITE(6,'(''  INITIAL EIGENVALUES'')')
         WRITE(6,'(5F12.6)')(EIGA(I),I=1,NMOS)
         WRITE(6,'(//10X,''NUMBER OF ELECTRONS IN C.I. ='',F5.1)')XX
      ENDIF
      IF(.NOT.GEOOK.AND.NELEC.GT.0)THEN
         IF(ABS(EIGS(NELEC+1)-EIGS(NELEC)).LT.1.D-1.OR.
     1ABS(EIGS(NELEC+1+NMOS)-EIGS(NELEC+NMOS)).LT.1.D-1)THEN
            WRITE(6,'(///10X,A)')'DEGENERATE ENERGY LEVELS DETECTED IN M
     1ECI'
            WRITE(6,'(10X,A)')'SOME OF THESE LEVELS WOULD BE TREATED BY
     1MECI,'
            WRITE(6,'(10X,A)')'WHILE OTHERS WOULD NOT.  THIS WOULD RESUL
     1T IN'
            WRITE(6,'(10X,A)')'NON-REPRODUCABLE ELECTRONIC ENERGIES.'
            WRITE(6,'(10X,A)')'  JOB STOPPED.  TO CONTINUE, SPECIFY "GEO
     1-OK"'
            STOP
         ENDIF
      ENDIF
      KALPHA=(NE+1)/2
      KBETA=NE-KALPHA
C
C   FOR DEBUGGING PURPOSES, UNCOMMENT THE FOLLOWING LINES
C   THEY LOCK THE PHASE-FACTOR
C
C#      DO 55 I=1,NMOS
C#      SUM=0.D0
C#      DO 56 J=1,NORBS
C#  56  IF(ABS(SUM).LT.ABS(COEFFS(J,I)))SUM=COEFFS(J,I)
C#      IF(SUM.LT.0.D0)THEN
C#      DO 57 J=1,NORBS
C#  57  COEFFS(J,I)=-COEFFS(J,I)
C#      ENDIF
C#  55  CONTINUE
C
C   FOR DEBUGGING PURPOSES, UNCOMMENT THE PRECEEDING LINES
C
      IF( BIGPRT ) THEN
         WRITE(6,'(//10X,''EIGENVECTORS'',/)')
         DO 70 I=1,NORBS
   70    WRITE(6,'(6F12.6)')(COEFFS(I,J),J=1,NMOS)
      ENDIF
      DO 80 I=1,NMOS
         DO 80 J=1,NMOS
            DO 80 K=1,NMOS
               DO 80 L=1,NMOS
   80 XY(I,J,K,L)=100.0D0
      NFA(2)=1
      NFA(1)=1
      DO 90 I=3,NMECI+1
   90 NFA(I)=NFA(I-1)*(I-1)
      DO 100 I=1,NMOS
         DO 100 J=1,NMOS
            DO 100 K=1,NMOS
               DO 100 L=1,NMOS
  100 XY(I,J,K,L)=100.0D0
      DO 110 I=1,NMOS
         I1=I
         DO 110 J=1,I
            J1=J
            DO 110 K=1,NMOS
               K1=K
               DO 110 L=1,K
                  L1=L
  110 CALL IJKL(I1,K1,J1,L1,X,COEFFS,NORBS)
      DO 120 I=1,NMOS
         DO 120 J=1,NMOS
            RJKAA(I,J)=XY(I,I,J,J)-XY(I,J,I,J)
  120 RJKAB(I,J)=XY(I,I,J,J)
      DO 140 I=1,NMOS
         X=0.0
         DO 130 J=1,NMOS
            X=X+(RJKAA(I,J)+RJKAB(I,J))*OCCA(J)
  130    CONTINUE
         EIGA(I)=EIGA(I)-X
C#      IF(ABS(OCCA(I)-0.5).LT.1.D-4)EIGA(I)=EIGA(I)+XY(I,I,I,I)*0.25D0
  140 CONTINUE
      IF(BIGPRT) THEN
         WRITE(6,150)
  150    FORMAT(/,5X,'EIGENVALUES AFTER REMOVAL OF INTER-ELECTRONIC INTE
     1RACTIONS',/)
         WRITE(6,'(6F12.6)')(EIGA(I),I=1,NMOS)
         WRITE(6,'(///10X,''TWO-ELECTRON J-INTEGRALS'',/)')
         DO 160 I1=1,NMOS
  160    WRITE(6,'(10F10.4)')(RJKAB(I1,J1),J1=1,NMOS)
         WRITE(6,'(///10X,''TWO-ELECTRON K-INTEGRALS'',/)')
         DO 170 I1=1,NMOS
  170    WRITE(6,'(10F10.4)')(RJKAB(I1,J1)-RJKAA(I1,J1),J1=1,NMOS)
      ENDIF
      NATOMS=NUMAT
      DO 180 I=1,NMOS
         DO 180 J=1,NMOS
            RJKAA(I,J)=RJKAA(I,J)*0.5D0
  180 CONTINUE
      IF(FIRST1) THEN
         I=INDEX(KEYWRD,'MICROS')
         IF(I.NE.0)THEN
            K=READA(KEYWRD,I)
            LAB=K
            IF(PRNT)WRITE(6,'(''    MICROSTATES READ IN'')')
            NTOT=XX+0.5
            OPEN(UNIT=5,FILE='FOR005',STATUS='OLD',BLANK='ZERO')
            REWIND 5
            DO 190 I=1,3
  190       READ(5,'(A)')LINE
            DO 200 I=1,1000
               READ(5,'(A)')LINE
  200       IF(INDEX(LINE,'MICRO').NE.0)GOTO 210
  210       DO 240 I=1,LAB
               READ(5,'(A)')LINE
               IZERO=MAX(0,MIN(INDEX(LINE,'0'),INDEX(LINE,'1'))-1)
               DO 220 J=1,NMOS
                  IF(LINE(J+IZERO:J+IZERO).NE.'1')
     1            LINE(J+IZERO:J+IZERO)='0'
                  IF(LINE(J+NMOS+IZERO:J+NMOS+IZERO).NE.'1')
     1            LINE(J+NMOS+IZERO:J+NMOS+IZERO)='0'
                  MICROA(J,I)=ICHAR(LINE(J+IZERO:J+IZERO))-
     1          ICHAR('0')
                  MICROB(J,I)=ICHAR(LINE(J+NMOS+IZERO:J+NMOS+IZERO))-
     1          ICHAR('0')
  220          CONTINUE
               IF(PRNT)WRITE(6,'(20I6)')(MICROA(J,I),J=1,NMOS),
     1        (MICROB(J,I),J=1,NMOS)
               K=0
               DO 230 J=1,NMOS
  230          K=K+MICROA(J,I)+MICROB(J,I)
               IF(K.NE.NTOT)THEN
                  NTOT=K
                  XX=K
                  WRITE(6,'(/,''NUMBER OF ELECTRONS IN C.I. REDEFINED TO
     1:'',I4,/)')K
               ENDIF
  240       CONTINUE
            FIRST1=.FALSE.
            GOTO 310
         ENDIF
         NUPP=KALPHA
         NDOWN=KBETA
         AMS=(NUPP-NDOWN)*0.5D0
         IF(PRNT)WRITE(6,250) AMS
  250    FORMAT(10X,'COMPONENT OF SPIN  = ',F4.1)
         IF(NUPP*NDOWN.GE.0) GOTO 270
         WRITE(6,260)
  260    FORMAT(/10X,28H IMPOSSIBLE VALUE OF DELTA S/)
         STOP
  270    LIMA=NFA(NMOS+1)/(NFA(NUPP+1)*NFA(NMOS-NUPP+1))
         LIMB=NFA(NMOS+1)/(NFA(NDOWN+1)*NFA(NMOS-NDOWN+1))
         LAB=LIMA*LIMB
         IF(PRNT)WRITE(6,280) LAB
  280    FORMAT(//10X,35H NO OF CONFIGURATIONS CONSIDERED = ,I4)
C#      IF(LAB.LT.101) GOTO 240
C#      WRITE(6,230)
C#  230 FORMAT(10X,24H TOO MANY CONFIGURATIONS/)
C#      GOTO 160
C#  240 CONTINUE
         CALL PERM(NPERMA, NUPP, NMOS, NMECI, LIMA)
         CALL PERM(NPERMB, NDOWN, NMOS, NMECI, LIMB)
         K=0
         DO 290 I=1,LIMA
            DO 290 J=1,LIMB
               K=K+1
               DO 290 L=1,NMOS
                  MICROA(L,K)=NPERMA(L,I)
  290    MICROB(L,K)=NPERMB(L,J)
  300    FORMAT(10I1)
  310    CONTINUE
         LIMA=LAB
         LIMB=LAB
      ENDIF
      GSE=0.0D0
      DO 320 I=1,NMOS
C#         IF(ABS(OCCA(I)-0.5).LT.0.01)GSE=GSE-0.25D0*XY(I,I,I,I)
         GSE=GSE+EIGA(I)*OCCA(I)*2.D0
         GSE=GSE+XY(I,I,I,I)*OCCA(I)*OCCA(I)
         DO 320 J=I+1,NMOS
  320 GSE=GSE+2.D0*(2.D0*XY(I,I,J,J) - XY(I,J,I,J))*OCCA(I)*OCCA(J)
C#    IF(PRNT)WRITE(6,'('' GROUND STATE ENERGY:'',F13.6,'' E.V.'')')GSE
C     ..........
      IF( PRNT )
     1WRITE(6,'(//10X,''CONFIGURATIONS CONSIDERED IN C.I.'',//)')
      J=0
      DO 330 I=1,LAB
         DIAG(I)=DIAGI(MICROA(1,I),MICROB(1,I),EIGA,XY,NMOS)-GSE
  330 CONTINUE
  340 CONTINUE
      IF(LAB.LE.MDIM) GOTO 380
      X=-100.D0
      DO 350 I=1,LAB
         IF(DIAG(I).GT.X)THEN
            X=DIAG(I)
            J=I
         ENDIF
  350 CONTINUE
      IF(J.NE.LAB) THEN
         DO 370 I=J,LAB
            I1=I+1
            DO 360 K=1,NMOS
               MICROA(K,I)=MICROA(K,I1)
  360       MICROB(K,I)=MICROB(K,I1)
  370    DIAG(I)=DIAG(I1)
      ENDIF
      LAB=LAB-1
      GOTO 340
  380 CONTINUE
C
C  MAIN LOOP TO FILL SECULAR DETERMINANT
C
      IK=0
      DO 430 I=1,LAB
         K=0
         DO 390 J=1,NMOS
            IOCCB1(J)=MICROB(J,I)
            IOCCA1(J)=MICROA(J,I)
  390    K=K+IOCCA1(J)
         NALPHA(I)=K
         IF(PRNT)  THEN
            WRITE(6,'(/10X,I4,6X,10I4)') I,(IOCCA1(K),K=1,NMOS)
            WRITE(6,'(20X,10I4)')(IOCCB1(K),K=1,NMOS)
         ENDIF
         IS=2
C
C   INNER LOOP TO FILL SECULAR DETERMINANT
C
         DO 420 K=1,I
            IK=IK+1
            CIMAT(IK)=I*1.D-15+J*.1D-17
            IX=0
            IY=0
            DO 400 J=1,NMOS
               IOCCB2(J)=MICROB(J,K)
               IOCCA2(J)=MICROA(J,K)
               IX=IX+ABS(IOCCA1(J)-IOCCA2(J))
  400       IY=IY+ABS(IOCCB1(J)-IOCCB2(J))
C
C                              CHECK IF MATRIX ELEMENT HAS TO BE ZERO
C
            IF(IX+IY.GT.4 .OR. NALPHA(I).NE.NALPHA(K))  GOTO 420
            IF(IX+IY.EQ.4) THEN
               IF(IX.EQ.0)THEN
                  CIMAT(IK)=BABBCD(IOCCA1, IOCCB1, IOCCA2, IOCCB2, NMOS)
               ELSEIF(IX.EQ.2)THEN
                  CIMAT(IK)=AABBCD(IOCCA1, IOCCB1, IOCCA2, IOCCB2, NMOS)
               ELSE
                  CIMAT(IK)=AABACD(IOCCA1, IOCCB1, IOCCA2, IOCCB2, NMOS)
               ENDIF
            ELSEIF(IX.EQ.2)THEN
               CIMAT(IK)=AABABC(IOCCA1, IOCCB1, IOCCA2, IOCCB2, NMOS)
            ELSEIF(IY.EQ.2)THEN
               CIMAT(IK)=BABBBC(IOCCA1, IOCCB1, IOCCA2, IOCCB2, NMOS)
            ELSE
               CIMAT(IK)=DIAG(I)
               X=0.0D0
               DO 410 J=1,NMOS
  410          X=X+IOCCA1(J)*IOCCB1(J)
               SPIN(I)=(-(XX-2*NALPHA(I))*(XX-2*NALPHA(I))+X*4.D0)
            ENDIF
  420    CONTINUE
         ISPQR(I,1)=IS-1
  430 CONTINUE
      IF(BIGPRT)THEN
         WRITE(6,'(//,'' C.I. MATRIX'')')
         CALL VECPRT(CIMAT,LAB)
      ELSE
         IF(PRNT)WRITE(6,'(//,'' DIAGONAL OF C.I. MATRIX'')')
         IF(PRNT)WRITE(6,'(5F13.6)')(CIMAT((I*(I+1))/2),I=1,LAB)
      ENDIF
      CALL RSP(CIMAT,LAB,LAB,EIG,CONF)
C
C   DECIDE WHICH ROOT TO EXTRACT
C
      KROOT=0
      IF(SMULT.LT.0.1D0)THEN
         MECI=EIG(LROOT)
         KROOT=LROOT
      ENDIF
      IF(BIGPRT)  THEN
         WRITE(6,'(//20X,''STATE VECTORS'',//)')
         CALL MATOUT(CONF,EIG,LAB,LAB,LAB)
      ENDIF
      IF(PRNT)THEN
         WRITE(6,440)
  440    FORMAT(///,' STATE ENERGIES '
     1,' EXPECTATION VALUE OF S**2  S FROM S**2=S(S+1)',//)
      ENDIF
      IROOT=0
      DO 480 I=1,LAB
         X=0.5D0*XX
         II=(I-1)*LAB
         DO 470 J=1,LAB
            JI=J+II
            X=X-CONF(JI)*CONF(JI)*SPIN(J)*0.25D0
            K=ISPQR(J,1)
            IF(K.EQ.1)  GOTO  460
            DO 450 K=2,K
               LI=ISPQR(J,K)+II
  450       X=X+CONF(JI)*CONF(LI)*2.D0
  460       CONTINUE
  470    CONTINUE
         Y=(-1.D0+SQRT(1.D0+4.D0*X))*0.5D0
         IF(ABS(SMULT-X).LT.0.01)THEN
            IROOT=IROOT+1
            IF(IROOT.EQ.LROOT) THEN
               IF(KROOT.EQ.0)KROOT=I
               MECI=EIG(I)
            ENDIF
         ENDIF
         J=Y*2.D0+1.5D0
  480 IF(PRNT)WRITE(6,490) I,EIG(I),TSPIN(J),X,Y
  490 FORMAT(I5,F12.6,3X,A8,F15.5,F10.5)
C#      M=0
C#      DO 440 I=1,NMOS
C#         WRITE(6,*)
C#         DO 440 J=1,NMOS
C#            WRITE(6,*)
C#            DO 440 K=1,NMOS
C#  440 WRITE(6,'(4I2,8F12.6)')I,J,K,M,(XY(I,J,K,L),L=1,NMOS)
      IF(FORCE.OR.LAST1)THEN
C
C   REFORM DENSITY MATRIX
C
         K=(KROOT-1)*LAB
         DO 510 I=1,NMOS
            SUM=0.D0
            DO 500 J=1,LAB
  500       SUM=SUM+(MICROA(I,J)+MICROB(I,J))*CONF(J+K)**2
  510    EIGA(I)=SUM-OCCA(I)*2.D0
         L=0
         DO 530 I=1,NORBS
            DO 530 J=1,I
               SUM=0.D0
               DO 520 K=1,NMOS
  520          SUM=SUM+EIGA(K)*COEFFS(I,K)*COEFFS(J,K)
               L=L+1
  530    P(L)=P(L)+SUM
      ENDIF
      MAXVEC=0
      IF(LSPIN)MAXVEC=MIN(4,LAB)
      IF(LSPIN.AND.(NE/2)*2.EQ.NE) THEN
         WRITE(6,'(''   ESR SPECIFIED FOR AN EVEN-ELECTRON SYSTEM'')')
      ENDIF
      DO 540 I=1,NMOS
         DO 540 J=1,NORBS
  540 COEFFS(J,I)=COEFFS(J,I)**2
      DO 610 IUJ=1,MAXVEC
         IOFSET=(IUJ-1)*LAB
         WRITE(6,'(//,''      MICROSTATE CONTRIBUTIONS TO '',
     1''STATE EIGENFUNCTION'',I3)')IUJ
         WRITE(6,'(5F13.6)')(CONF(I+IOFSET),I=1,LAB)
         DO 550 I=1,LAB
  550    CONF(I)=CONF(I+IOFSET)**2
C                                             SECOND VECTOR!
         DO 570 I=1,NMOS
            SUM=0.D0
            DO 560 J=1,LAB
  560       SUM=SUM+(MICROA(I,J)-MICROB(I,J))*CONF(J)
  570    EIGA(I)=SUM
         WRITE(6,'(/,''    SPIN DENSITIES FROM EACH M.O., ENERGY:''
     1,F7.3)')EIG(IUJ)
         WRITE(6,'(5F12.6)') (EIGA(I),I=1,NMOS)
         WRITE(6,*)
         WRITE(6,*)'     SPIN DENSITIES FROM EACH ATOMIC ORBITAL'
         WRITE(6,*)'                              S        PX        '//
     1'PY        PZ        TOTAL'
         DO 600 I=1,NATOMS
            IL=NFIRST(I)
            IU=NLAST(I)
            L=0
            SUMM=0.D0
            DO 590 K=IL,IU
               L=L+1
               SUM=0.D0
               DO 580 J=1,NMOS
  580          SUM=SUM+COEFFS(K,J)*EIGA(J)
               SUMM=SUMM+SUM
  590       EIGS(L)=SUM
            IF(L.EQ.4)THEN
               WRITE(6,'(''  ATOM'',I4,''    SPIN DENSITY  '',5F10.7)')
     1I,(EIGS(K),K=1,L),SUMM
            ELSE
               WRITE(6,'(''  ATOM'',I4,''    SPIN DENSITY  '',F10.7,30X,
     1F10.7)')I,EIGS(1),SUMM
            ENDIF
  600    CONTINUE
  610 CONTINUE
      RETURN
      END
