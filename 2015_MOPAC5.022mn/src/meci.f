      SUBROUTINE MECI(EIGS,COEFF,COEFFS,EIGA,RMECI,N,NMOS,IRESET,       CSTP
     &                FINISH,LGRAD,*)                                   CSTP

CSTP  Converted from function to subroutine to support STOP statement replacement.
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
*  REVISED BASED ON MECI.F FROM AMSOL CODE TO COMPUTE CI ANALYTICAL GRADIENT
*  BY JINGJING ZHENG, 2015
*
***********************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      INCLUDE 'SIZES.i'
      PARAMETER (MAXPRM=6*NMECI)                 
C
C
CSTP  DOUBLE PRECISION MECI
CSTP  Return value for MECI when previously a function now is stored in
CSTP  RMECI for return to calling routine.
      DOUBLE PRECISION RMECI                                            CSTP
C
C   MATRICES FOR SEC.DET., VECTORS, AND EIGENVALUES.
C   MOVED TO COMMON BLOCK
C     DIMENSION CIMAT(NMECI**4), CONF(NMECI**4),
C    1 EIG(NMECI**2), DIAG(2*NMECI**3)
C
C   MATRICES TO HOLD ELECTRON CONFIGURATIONS
C
C     DIMENSION MICROA(NMECI,2*NMECI**3), MICROB(NMECI,2*NMECI**3),
C    1IOCCA1(NMECI), IOCCA2(NMECI), IOCCB1(NMECI),
C    2IOCCB2(NMECI), NALPHA(NMECI**2)
      DIMENSION IOCCA1(NMECI),IOCCA2(NMECI),IOCCB1(NMECI),IOCCB2(NMECI)
C
C   MATRICES FOR PERMUTATION WORK
C
C     DIMENSION NFA(2*NMECI), NPERMA(NMECI,6*NMECI),
C    1NPERMB(NMECI,6*NMECI)
C
C   MATRICES FOR ONE AND TWO ELECTRON INTEGRALS, AND M.O.S
C
      DIMENSION EIGA(MAXORB), EIGS(MAXORB), RJKAA(NMECI,NMECI),
     1          RJKAB(NMECI,NMECI), COEFF(N,N),  COEFFS(N,N)
C
C   SPIN MATRICES
C
C     DIMENSION SPIN(NMECI**2)
      LOGICAL DEBUG,  LARGE, PRNT, LSPIN, LSPIN1, FINISH,
     1 BIGPRT, SING, DOUB, TRIP, QUAR, QUIN, SEXT, LAST1,
     2 PRNT2, FORCE, GEOOK
      CHARACTER KEYWRD*160, TSPIN(7)*8, LINE*80
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NDUMMY(2),NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /WMATRX/ WJ(N2ELEC),WK(N2ELEC*2),NWDUM(NUMATM+1)           JZ0315
      COMMON /DWMAT/  DWJ(N2ELEC),DWK(N2ELEC*2)                         JZ0315
C     COMMON /SPQR/ ISPQR(NMECI**2,NMECI),IS,I,K
      COMMON /SPQR/ ISPQR(MAXCI,NMECI),IS,I,K                           JZ0315
     1       /BASEOC/ OCCA(NMECI),NFA(NMECI+2)
     2       /CIDATA/ VECTCI(MAXCI),XX,NELEC,NCI2,LAB                   DL0397
     3               ,NALPHA(MAXCI)                                     DL0397
     4               ,MICROA(NMECI,MAXCI),MICROB(NMECI,MAXCI)           DL0397
      COMMON /SCRH1M/ CIMAT(MAXCI*(MAXCI+1)/2),CONF(MAXCI**2)           DL0397
     1               ,EIG(MAXCI),DIAG(MAXCI),SPIN(MAXCI)                DL0397
     2               ,NPERMA(NMECI,MAXPRM),NPERMB(NMECI,MAXPRM)         DL0397
     3       /SCRAH2/ DIJKL(NRELAX*MORB2+1)                             GCL0892
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /KEYWRD/ KEYWRD
C     COMMON /BASEOC/ OCCA(NMECI)
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      COMMON /NUMCAL/ NUMCAL
      COMMON /IOCM/ IREAD
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      LOGICAL LGRAD                                                     JZ0315
      DIMENSION W(1)
C     EQUIVALENCE (W,WJ)                                                JZ0315
      EQUIVALENCE (W,DWJ)                                               JZ0315
         SAVE                                                           GL0892
      DATA TSPIN/'SINGLET ','DOUBLET ','TRIPLET ','QUARTET ','QUINTET ',
     1'SEXTET  ','SEPTET  '/
      DATA ICALCN /0/, ICALC1 /0/
c      write(6,*) eigs(1),coeff(1,1),coeffs(1,1),eiga(1),rmeci,n,nmos,   debug
c     &           ireset,finish                                          debug
         IF (ICALCN .NE. NUMCAL) THEN
            ICALCN = NUMCAL
         GEOOK=(INDEX(KEYWRD,'GEO-OK').NE.0)
         FORCE=(INDEX(KEYWRD,'FORCE').NE.0)
         LAST1=(INDEX(KEYWRD,'1SCF').NE.0)
         LSPIN1=(INDEX(KEYWRD,'ESR').NE.0)
         MDIM=NMECI**2
         DEBUG=(INDEX(KEYWRD,'DEBUG').NE.0)
         PRNT2=(INDEX(KEYWRD,'MECI').NE.0)
         DEBUG=(DEBUG.AND.PRNT2)
         LARGE=(INDEX(KEYWRD,'LARGE').NE.0)
C C.I.=(m,n) keyword                                                    IR0494
         NDOUBL=99
         IF(INDEX(KEYWRD,'C.I.=(').NE.0)THEN
            NDOUBL=READA(KEYWRD,INDEX(KEYWRD,'C.I.=(')+7)
            NMOS=READA(KEYWRD,INDEX(KEYWRD,'C.I.=(')+5)
         ELSEIF (INDEX(KEYWRD,'C.I.=').NE.0)THEN
            NMOS=READA(KEYWRD,INDEX(KEYWRD,'C.I.=')+5)
         ELSE
            NMOS=NOPEN-NCLOSE
         ENDIF
C END C.I.=(m,n) keyword
         LROOT=1
         IF(INDEX(KEYWRD,'EXCI').NE.0)LROOT=2
         I=INDEX(KEYWRD,'ROOT')
         IF(I.NE.0)LROOT=READA(KEYWRD,I)
C C.I.=(m,n) keyword                                                    IR0494
         IF(NDOUBL.EQ.99)THEN
            J=MAX(MIN((NCLOSE+NOPEN+1)/2-(NMOS-1)/2,NORBS-NMOS+1),1)
         ELSE
            J=NCLOSE-NDOUBL+1
            IF(FRACT.GT.1.99D0)J=J+1
         ENDIF
            J=(NCLOSE+NOPEN+1)/2-(NMOS-1)/2
C         write(6,*) 'J ', J
C END C.I.=(m,n) keyword
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
         NCI2=NMOS
      ENDIF
C     write(6,*) 'NMOS,NCLOSE,NOPEN,FRACT',NMOS,NCLOSE,NOPEN,FRACT
C     write(6,*) 'NELEC =', NELEC,NELECS,NE,XX
C     stop
      IF(IRESET.EQ.1)THEN
         ONE=-1.D0
         GOTO 520
      ELSE
         ONE=1.D0
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
        IF (DOPRNT) THEN                                                CIO
         WRITE(6,'(''  INITIAL EIGENVALUES'')')
         WRITE(6,'(5F12.6)')(EIGA(I),I=1,NMOS)
         WRITE(6,'(//10X,''NUMBER OF ELECTRONS IN C.I. ='',F5.1)')XX
        ENDIF                                                           CIO
      ENDIF
      IF(.NOT.GEOOK.AND.NELEC.GT.0)THEN
         IF(ABS(EIGS(NELEC+1)-EIGS(NELEC)).LT.1.D-1.OR.
     1ABS(EIGS(NELEC+1+NMOS)-EIGS(NELEC+NMOS)).LT.1.D-1)THEN
           IF (DOPRNT) THEN                                             CIO
            WRITE(6,'(///10X,A)')'DEGENERATE ENERGY LEVELS DETECTED IN M
     1ECI'
            WRITE(6,'(10X,A)')'SOME OF THESE LEVELS WOULD BE TREATED BY'
     1//'MECI,'
            WRITE(6,'(10X,A)')'WHILE OTHERS WOULD NOT.  THIS WOULD RESUL
     1T IN'
            WRITE(6,'(10X,A)')'NON-REPRODUCIBLE ELECTRONIC ENERGIES.'
            WRITE(6,'(10X,A)')'  JOB STOPPED.  TO CONTINUE, SPECIFY "GEO
     1-OK"'
            ENDIF                                                       CIO
            RETURN 1                                                    CSTP (stop)
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
         IF (DOPRNT) WRITE(6,'(//10X,''EIGENVECTORS'',/)')              CIO
         DO 70 I=1,NORBS
   70    IF (DOPRNT) WRITE(6,'(6F12.6)')(COEFFS(I,J),J=1,NMOS)          CIO
      ENDIF
C     DO 80 I=1,NMOS
C        DO 80 J=1,NMOS
C           DO 80 K=1,NMOS
C              DO 80 L=1,NMOS
C  80 XY(I,J,K,L)=100.0D0
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
  110 CALL IJKL(I1,K1,J1,L1,X,COEFFS,NORBS,*9999)                        CSTP(call)
C
C IJKL call is replaced by the IJKL subroutine from AMSOL (renamed as IJKLD)
C
C
C     2-ELECTRONS INTEGRALS OVER C.I-ACTIVE M.O.
C     ------------------------------------------
C     COMPUTE ALSO <IJ!KL> IN /SCRAH2/ FOR SUBSEQUENT GRADIENT,
C     WITH I RUNNING OVER C.I-ACTIVE M.O IF LGRAD IS FALSE
C                         OR   ALL   M.O IF LGRAD IS TRUE,
C          J AND K>=L RUNNING OVER ALL C.I-ACTIVE M.O.
      MDIJKL=N*NMOS*NMOS*(NMOS+1)/2
      MPQKL =NRELAX*MORB2-MDIJKL
      CALL IJKLD (COEFF,N,NELEC+1,NMOS,W,DIJKL(MDIJKL+1),MPQKL
     .          ,XY,NMECI,DIJKL,.TRUE.)
C    .          ,NMECI,DIJKL,.TRUE.)
C     stop
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
         IF (DOPRNT) WRITE(6,150)                                       CIO
  150    FORMAT(/,5X,'EIGENVALUES AFTER REMOVAL OF INTER-ELECTRONIC INTE
     1RACTIONS',/)
         IF (DOPRNT) WRITE(6,'(6F12.6)')(EIGA(I),I=1,NMOS)              CIO
         IF (DOPRNT) WRITE(6,'(///10X,''TWO-ELECTRON J-INTEGRALS'',/)') CIO
         DO 160 I1=1,NMOS
  160    IF (DOPRNT) WRITE(6,'(10F10.4)')(RJKAB(I1,J1),J1=1,NMOS)       CIO
         IF (DOPRNT) WRITE(6,'(///10X,''TWO-ELECTRON K-INTEGRALS'',/)') CIO
         DO 170 I1=1,NMOS
  170    IF (DOPRNT) WRITE(6,'(10F10.4)')(RJKAB(I1,J1)-RJKAA(I1,J1),    CIO
     &                                    J1=1,NMOS)                    CIO
      ENDIF
      NATOMS=NUMAT
      DO 180 I=1,NMOS
         DO 180 J=1,NMOS
            RJKAA(I,J)=RJKAA(I,J)*0.5D0
  180 CONTINUE
         IF (ICALC1 .NE. NUMCAL) THEN
         ICALC1 = NUMCAL
         I=INDEX(KEYWRD,'MICROS')
         IF(I.NE.0)THEN
            K=READA(KEYWRD,I)
            LAB=K
            IF(DOPRNT.AND.PRNT)WRITE(6,'(''    MICROSTATES READ IN'')') CIO
            NTOT=XX+0.5
            OPEN(UNIT=IREAD,FILE='FOR004',STATUS='OLD',                 No CIO
     &                      BLANK='ZERO')                               CIO
            REWIND IREAD                                                No CIO
            DO 190 I=1,3
  190       READ(IREAD,'(A)')LINE                                       No CIO
            DO 200 I=1,1000
               IF(DOPRNT) READ(IREAD,'(A)')LINE                         CIO
  200       IF(INDEX(LINE,'MICRO').NE.0)GOTO 210
  210       DO 240 I=1,LAB
               IF(DOPRNT) READ(IREAD,'(A)')LINE                         CIO
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
               IF(DOPRNT.AND.PRNT) WRITE(6,'(20I6)')                    CIO
     &              (MICROA(J,I),J=1,NMOS), (MICROB(J,I),J=1,NMOS)      CIO
               K=0
               DO 230 J=1,NMOS
  230          K=K+MICROA(J,I)+MICROB(J,I)
               IF(K.NE.NTOT)THEN
                  NTOT=K
                  XX=K
                  IF (DOPRNT) WRITE(6,'(/,                              CIO
     &           ''NUMBER OF ELECTRONS IN C.I. REDEFINED TO:'',I4,/)')K CIO
               ENDIF
  240       CONTINUE
            GOTO 310
         ENDIF
         NUPP=KALPHA
         NDOWN=KBETA
         AMS=(NUPP-NDOWN)*0.5D0
         IF(DOPRNT.AND.PRNT)WRITE(6,250) AMS                            CIO
  250    FORMAT(10X,'COMPONENT OF SPIN  = ',F4.1)
         IF(NUPP*NDOWN.GE.0) GOTO 270
         IF (DOPRNT) WRITE(6,260)                                       CIO
  260    FORMAT(/10X,28H IMPOSSIBLE VALUE OF DELTA S/)
      RETURN 1                                                          CSTP (stop)
  270    LIMA=NFA(NMOS+1)/(NFA(NUPP+1)*NFA(NMOS-NUPP+1))
         LIMB=NFA(NMOS+1)/(NFA(NDOWN+1)*NFA(NMOS-NDOWN+1))
         LAB=LIMA*LIMB
         IF(PRNT.AND.DOPRNT)WRITE(6,280) LAB                            CIO
  280    FORMAT(//10X,35H NO OF CONFIGURATIONS CONSIDERED = ,I4)
C#      IF(LAB.LT.101) GOTO 240
C#      WRITE(6,230)
C#  230 FORMAT(10X,24H TOO MANY CONFIGURATIONS/)
C#      GOTO 160
C#  240 CONTINUE
         CALL PERM(NPERMA, NUPP, NMOS, NMECI, LIMA,*9999)               CSTP(call)
         CALL PERM(NPERMB, NDOWN, NMOS, NMECI, LIMB,*9999)              CSTP(call)
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
C
C     GROUND STATE (I.E VACUUM) ENERGY AND DIAGONAL ELEMENTS OF THE C.I
C     -----------------------------------------------------------------
C     CALL MECID (EIGS,GSE,EIGA,DIAG)

      IF( DOPRNT.AND.PRNT )
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
C   BEFORE STARTING, CHECK THAT THE ROOT WANTED CAN EXIST
C
      IF(LAB.LT.LROOT)THEN
        IF (DOPRNT) THEN                                                CIO
         WRITE(6,'(//10X,''C.I. IS OF SIZE LESS THAN ROOT SPECIFIED'')')
         WRITE(6,'(10X,''MODIFY SIZE OF C.I. OR ROOT NUMBER'')')
        ENDIF                                                           CIO
      RETURN 1                                                          CSTP (stop)
      ENDIF
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
         IF(DOPRNT.AND.PRNT)  THEN                                      CIO
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
         IF (DOPRNT) WRITE(6,'(//,'' C.I. MATRIX'')')                   CIO
         CALL VECPRT(CIMAT,-LAB,*9999)                                  CSTP(call)
      ELSE
         IF(DOPRNT.AND.PRNT)WRITE(6,'(//,'' DIAGONAL OF C.I. MATRIX'')')CIO
         IF(DOPRNT.AND.PRNT)WRITE(6,'(5F13.6)')(CIMAT((I*(I+1))/2),I=1, CIO
     &                              LAB)                                CIO
      ENDIF
      CALL RSP(CIMAT,LAB,LAB,EIG,CONF,*9999)                            CSTP(call)
C
C   DECIDE WHICH ROOT TO EXTRACT
C
C
C     DECIDE WHICH ROOT TO EXTRACT
      KROOT=0
      IF(SMULT.LT.0.1D0) KROOT=LROOT
C     IF(SMULT.LT.-0.1D0)THEN
C        KROOT=LROOT
C     ENDIF
C         MECI=EIG(LROOT)
c     RMECI=EIG(LROOT)                                                  CSTP
C     J=LAB*(KROOT-1)                                                   JZ0315
C     DO 435 I=1,LAB
c 435 VECTCI(I)=CONF(I+J)
      IF(PRNT)  THEN                                                    IR0594
         IF (DOPRNT) WRITE(6,'(//20X,''STATE VECTORS'',//)')            CIO
         CALL MATOUT(CONF,EIG,LAB,LAB,LAB,*9999)                        CSTP(call)
      ENDIF
      IF(PRNT)THEN
         IF (DOPRNT) WRITE(6,440)                                       CIO
  440    FORMAT(///,' STATE ENERGIES '
     1,' EXPECTATION VALUE OF S**2  S FROM S**2=S(S+1)',//)
      ENDIF
      IROOT=0
      DO 450 I=1,9
  450 CIMAT(I)=0.1D0
      DO 490 I=1,LAB
         X=0.5D0*XX
         II=(I-1)*LAB
         DO 480 J=1,LAB
            JI=J+II
            X=X-CONF(JI)*CONF(JI)*SPIN(J)*0.25D0
            K=ISPQR(J,1)
            IF(K.EQ.1)  GOTO  470
            DO 460 K=2,K
               LI=ISPQR(J,K)+II
  460       X=X+CONF(JI)*CONF(LI)*2.D0
  470       CONTINUE
  480    CONTINUE
         Y=(-1.D0+SQRT(1.D0+4.D0*X))*0.5D0
         IF(ABS(SMULT-X).LT.0.01)THEN
            IROOT=IROOT+1
            IF(IROOT.EQ.LROOT) THEN
               KROOT=I
C               MECI=EIG(I)
               RMECI=EIG(I)                                             CSTP
            ENDIF
         ENDIF
         J=Y*2.D0+1.5D0
         CIMAT(J)=CIMAT(J)+1
  490 IF(DOPRNT.AND.PRNT)WRITE(6,510) I,EIG(I),TSPIN(J),X,Y             CSTP
      RMECI=EIG(LROOT)                                                  CSTP
      J=LAB*(KROOT-1)                                                   JZ0315
      DO 435 I=1,LAB
  435 VECTCI(I)=CONF(I+J)
      IF(PRNT) THEN
         WRITE(JOUT,'(/'' EIGENVECTOR OF THE SELECTED STATE'',
     1                 '' (NUMBER'',I3,'')'')')KROOT
         WRITE(JOUT,'(10F8.4)')(VECTCI(I),I=1,LAB)
      ENDIF

      IF(KROOT.EQ.0)THEN
         IF (DOPRNT) THEN                                               CIO
         WRITE(6,'(//10X,''THE STATE REQUIRED IS NOT PRESENT IN THE'')')
         WRITE(6,'(10X,  ''    SET OF CONFIGURATIONS AVAILABLE'')')
         WRITE(6,'(/ 4X,''NUMBER OF STATES ACCESSIBLE USING CURRENT KEY-
     1WORDS'',/)')
         ENDIF                                                          CIO
         DO 500 I=1,7
  500    IF(DOPRNT.AND.CIMAT(I).GT.0.5D0)                               CIO
     1WRITE(6,'((24X,A8,I4))')TSPIN(I),NINT(CIMAT(I))
      RETURN 1                                                          CSTP (stop)
      ENDIF
  510 FORMAT(I5,F12.6,3X,A8,F15.5,F10.5)
C#      M=0
C#      DO 440 I=1,NMOS
C#         WRITE(6,*)
C#         DO 440 J=1,NMOS
C#            WRITE(6,*)
C#            DO 440 K=1,NMOS
C#  440 WRITE(6,'(4I2,8F12.6)')I,J,K,M,(XY(I,J,K,L),L=1,NMOS)
  520 CONTINUE
      IF(FORCE.OR.LAST1)THEN
C
C   REFORM DENSITY MATRIX
C
         K=(KROOT-1)*LAB
         DO 540 I=1,NMOS
            SUM=0.D0
            DO 530 J=1,LAB
  530       SUM=SUM+(MICROA(I,J)+MICROB(I,J))*CONF(J+K)**2
  540    EIGA(I)=SUM-OCCA(I)*2.D0
         L=0
         DO 560 I=1,NORBS
            DO 560 J=1,I
               SUM=0.D0
               DO 550 K=1,NMOS
  550          SUM=SUM+EIGA(K)*COEFFS(I,K)*COEFFS(J,K)
               L=L+1
  560    P(L)=P(L)+SUM*ONE
      ENDIF
      MAXVEC=0
      IF(LSPIN)MAXVEC=MIN(4,LAB)
      IF(DOPRNT.AND.LSPIN.AND.(NE/2)*2.EQ.NE) THEN                      CIO
         WRITE(6,'(''   ESR SPECIFIED FOR AN EVEN-ELECTRON SYSTEM'')')
      ENDIF
      DO 570 I=1,NMOS
         DO 570 J=1,NORBS
  570 COEFFS(J,I)=COEFFS(J,I)**2
      DO 640 IUJ=1,MAXVEC
         IOFSET=(IUJ-1)*LAB
         IF (DOPRNT) THEN                                               CIO
         WRITE(6,'(//,''      MICROSTATE CONTRIBUTIONS TO '',
     1''STATE EIGENFUNCTION'',I3)')IUJ
         WRITE(6,'(5F13.6)')(CONF(I+IOFSET),I=1,LAB)
         ENDIF                                                          CIO
         DO 580 I=1,LAB
  580    CONF(I)=CONF(I+IOFSET)**2
C                                             SECOND VECTOR!
         DO 600 I=1,NMOS
            SUM=0.D0
            DO 590 J=1,LAB
  590       SUM=SUM+(MICROA(I,J)-MICROB(I,J))*CONF(J)
  600    EIGA(I)=SUM
         IF (DOPRNT) THEN                                               CIO
         WRITE(6,'(/,''    SPIN DENSITIES FROM EACH M.O., ENERGY:''
     1,F7.3)')EIG(IUJ)
         WRITE(6,'(5F12.6)') (EIGA(I),I=1,NMOS)
         WRITE(6,*)
         WRITE(6,*)'     SPIN DENSITIES FROM EACH ATOMIC ORBITAL'
         WRITE(6,*)'                              S        PX        '//
     1'PY        PZ        TOTAL'
         ENDIF                                                          CIO
         DO 630 I=1,NATOMS
            IL=NFIRST(I)
            IU=NLAST(I)
            L=0
            SUMM=0.D0
            DO 620 K=IL,IU
               L=L+1
               SUM=0.D0
               DO 610 J=1,NMOS
  610          SUM=SUM+COEFFS(K,J)*EIGA(J)
               SUMM=SUMM+SUM
  620       EIGS(L)=SUM
            IF (DOPRNT) THEN                                            CIO
            IF(L.EQ.4)THEN
               WRITE(6,'(''  ATOM'',I4,''    SPIN DENSITY  '',5F10.7)')
     1I,(EIGS(K),K=1,L),SUMM
            ELSE
               WRITE(6,'(''  ATOM'',I4,''    SPIN DENSITY  '',F10.7,30X,
     1F10.7)')I,EIGS(1),SUMM
            ENDIF
            ENDIF                                                       CIO
  630    CONTINUE
  640 CONTINUE
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY MECI_INIT                                                   CSAV
               AMS = 0.0D0                                              CSAV
            BIGPRT = .FALSE.                                            CSAV
CSPD             CIMAT = 0.0D0                                              CSAV
CSPD              CONF = 0.0D0                                              CSAV
                 D = 0.0D0                                              CSAV
                D0 = 0.0D0                                              CSAV
             DEBUG = .FALSE.                                            CSAV
CSPD              DIAG = 0.0D0                                              CSAV
              DOUB = .FALSE.                                            CSAV
CSPD               EIG = 0.0D0                                              CSAV
             FORCE = .FALSE.                                            CSAV
             GEOOK = .FALSE.                                            CSAV
               GSE = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
                I1 = 0                                                  CSAV
            ICALC1 = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
                II = 0                                                  CSAV
                IK = 0                                                  CSAV
                IL = 0                                                  CSAV
                IN = 0                                                  CSAV
            IOCCA1 = 0                                                  CSAV
            IOCCA2 = 0                                                  CSAV
            IOCCB1 = 0                                                  CSAV
            IOCCB2 = 0                                                  CSAV
            IOFSET = 0                                                  CSAV
             IROOT = 0                                                  CSAV
                IS = 0                                                  CSAV
                IU = 0                                                  CSAV
               IUJ = 0                                                  CSAV
                IX = 0                                                  CSAV
                IY = 0                                                  CSAV
             IZERO = 0                                                  CSAV
                 J = 0                                                  CSAV
                J1 = 0                                                  CSAV
                JI = 0                                                  CSAV
                 K = 0                                                  CSAV
                K1 = 0                                                  CSAV
            KALPHA = 0                                                  CSAV
             KBETA = 0                                                  CSAV
             KROOT = 0                                                  CSAV
                 L = 0                                                  CSAV
                L1 = 0                                                  CSAV
               LAB = 0                                                  CSAV
             LARGE = .FALSE.                                            CSAV
             LAST1 = .FALSE.                                            CSAV
                LI = 0                                                  CSAV
              LIMA = 0                                                  CSAV
              LIMB = 0                                                  CSAV
              LINE = ""                                                 CSAV
             LROOT = 0                                                  CSAV
             LSPIN = .FALSE.                                            CSAV
            LSPIN1 = .FALSE.                                            CSAV
            MAXVEC = 0                                                  CSAV
              MDIM = 0                                                  CSAV
            MICROA = 0                                                  CSAV
            MICROB = 0                                                  CSAV
            NALPHA = 0                                                  CSAV
            NATOMS = 0                                                  CSAV
            NCLOSE = 0                                                  CSAV
            NDOUBL = 0                                                  CSAV
             NDOWN = 0                                                  CSAV
             NELEC = 0                                                  CSAV
               NFA = 0                                                  CSAV
             NLEFT = 0                                                  CSAV
             NOPEN = 0                                                  CSAV
            NPERMA = 0                                                  CSAV
            NPERMB = 0                                                  CSAV
              NTOT = 0                                                  CSAV
              NUPP = 0                                                  CSAV
               ONE = 0.0D0                                              CSAV
              PRNT = .FALSE.                                            CSAV
             PRNT2 = .FALSE.                                            CSAV
              QUAR = .FALSE.                                            CSAV
              QUIN = .FALSE.                                            CSAV
             RJKAA = 0.0D0                                              CSAV
             RJKAB = 0.0D0                                              CSAV
              SEXT = .FALSE.                                            CSAV
              SING = .FALSE.                                            CSAV
             SMULT = 0.0D0                                              CSAV
              SPIN = 0.0D0                                              CSAV
               SUM = 0.0D0                                              CSAV
              SUMM = 0.0D0                                              CSAV
              TRIP = .FALSE.                                            CSAV
                 X = 0.0D0                                              CSAV
                XX = 0.0D0                                              CSAV
                 Y = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
