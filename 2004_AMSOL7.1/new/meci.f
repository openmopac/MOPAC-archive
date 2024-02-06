      DOUBLE PRECISION FUNCTION MECI(EIGS,COEFF,WORK,EIGA,N,NMOS,
     1                               KDELTA,PRNT1,LGRAD)
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
*   REVISED BY D.LIOTARD (M.J.S. DEWAR GROUP, SEPTEMBER 1986) :
*   1)  <IJ!KL> CALCULATION,
*   2)  DENSITY MATRIX C.I.-CORRECTION,
*   3)  CONNECTION WITH ANALYTICAL GRADIENT CALCULATION.
*   4)  "CIS" CAPABILITY, ENLARGED LIMITS (NMECI, MAXCI)                DL0397
***********************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'KEYS.i'                                                  DJG0995
      INCLUDE 'FFILES.i'                                                GDH1095
      INCLUDE 'SIZES.i'
      PARAMETER (MAXPRM=6*NMECI)                                        DL0397
      DIMENSION EIGA(MAXORB), EIGS(MAXORB), COEFF(N,N), WORK(N,N)
      LOGICAL DEBUG, PRTVEC, PRNT1, PRNT, LSPIN, LSPIN1,
     1  PRNT2, SING, DOUB, TRIP, QUAR, QUIN, SEXT, LGRAD
      CHARACTER TSPIN(7)*8                                              DJG0995
      COMMON /MLKSTR/ FRACT                                             3GL3092
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NDUMMY(2),           3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
     3       /WMATRX/ WJ(N2ELEC),WK(N2ELEC*2),NWDUM(NUMATM+1)
     4       /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
     5       /LAST  / LAST
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /SPQR  / ISPQR(MAXCI,NMECI),ISDUM(3)                       DL0397
     1       /BASEOC/ OCCA(NMECI),NFA(NMECI+2)
     2       /CIDATA/ VECTCI(MAXCI),XX,NELEC,NCI2,LAB                   DL0397
     3               ,NALPHA(MAXCI)                                     DL0397
     4               ,MICROA(NMECI,MAXCI),MICROB(NMECI,MAXCI)           DL0397
     5       /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      COMMON /SCRH1M/ CIMAT(MAXCI*(MAXCI+1)/2),CONF(MAXCI**2)           DL0397
     1               ,EIG(MAXCI),DIAG(MAXCI),SPIN(MAXCI)                DL0397
     2               ,NPERMA(NMECI,MAXPRM),NPERMB(NMECI,MAXPRM)         DL0397
     3       /SCRAH2/ DIJKL(NRELAX*MORB2+1)                             GCL0892
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      LOGICAL MSGEN                                                     IR1294
      DIMENSION W(1)
      EQUIVALENCE (W,WJ)
       SAVE
      DATA TSPIN/'SINGLET ','DOUBLET ','TRIPLET ','QUARTET ','QUINTET ',
     1           'SEXTET  ','SEPTET  '/
C
C     DEBUG KEYWORDS AND INITIALIZE.                                    DJG0995
C     ----------------------------
      IF (ICONTR(9).EQ.1) THEN                                          GDH0195
         ICONTR(9)=2                                                    GDH0195
         MSGEN = .TRUE.                                                 IR1294
         LSPIN1=(IESR.NE.0)                                             DJG0995
         DEBUG=(IDEBUG.NE.0)                                            DJG0995
         PRNT2=(IMECI.NE.0)                                             DJG0995
         LROOT=1
         IF(IEXCIT.NE.0)LROOT=2                                         DJG0995
         IF(IROOT.NE.0)LROOT=IIROOT                                     DJG0995
         PRTVEC=(IVECTO.NE.0)                                           DJG0995
C        ENOUGH ROOM FOR CI-ACTIVE MOS?
         IF (NMOS.GT.NMECI) THEN
            WRITE(JOUT,'('' NO ROOM FOR CI-ACTIVE MOS IN CI. STOP.'')') DL0397
            ISTOP=1                                                     DL0397
c   dl... adjust the value of iwhere... How find easily the highest in use??
            IWHERE=138                                                  DL0397
            RETURN                                                      DL0397
         ENDIF                                                          DL0397
C        OCCUPANCY OF C.I-ACTIVE M.O
         J=(NCLOSE+NOPEN+1)/2-(NMOS-1)/2
         L=0
         DO 10 I=J,NCLOSE
            L=L+1
   10    OCCA(L)=1
         DO 20 I=NCLOSE+1,NOPEN
            L=L+1
   20    OCCA(L)=FRACT*0.5D0
         DO 30 I=NOPEN+1,J+NMOS
            L=L+1
   30    OCCA(L)=0.D0
C        REQUIRED MULTIPLICITY.
         SING=(ISINGL+IEXCIT+IBIRAD.NE.0)                               DJG0995
         DOUB=(IDOUBL.NE.0)                                             DJG0995
         TRIP=(ITRIPL.NE.0)                                             DJG0995
         QUAR=(IQUART.NE.0)                                             DJG0995
         QUIN=(IQUINT.NE.0)                                             DJG0995
         SEXT=(ISEXTE.NE.0)                                             DJG0995
         SMULT=-.5D0
         IF(SING) SMULT=0.00D0
         IF(DOUB) SMULT=0.75D0
         IF(TRIP) SMULT=2.00D0
         IF(QUAR) SMULT=3.75D0
         IF(QUIN) SMULT=6.00D0
         IF(SEXT) SMULT=8.75D0
C        XX IS THE NUMBER OF ELECTRONS IN C.I
         X=0.D0
         DO 40 J=1,NMOS
   40    X=X+OCCA(J)
         XX=X+X
         NE=XX+0.5
         NELEC=(NELECS-NE+1)/2
         NLEFT=NORBS-NMOS-NELEC
C        FILL TABLE OF FACTORIALS NUMBER.
         NFA(1)=1
         NFA(2)=1
         DO 50 I=2,NMECI+1
   50    NFA(I+1)=NFA(I)*I
C        NUMBER OF ACTIVE M.O
         NCI2=NMOS
      ENDIF
C
C     PRINTOUT LEVEL
      DEBUG=DEBUG.AND.PRNT2
      PRNT=PRNT1.AND.PRNT2
      LSPIN=LSPIN1.AND.PRNT1
      KALPHA=(NE+1)/2
      KBETA=NE-KALPHA
C
C     2-ELECTRONS INTEGRALS OVER C.I-ACTIVE M.O.
C     ------------------------------------------
C     COMPUTE ALSO <IJ!KL> IN /SCRAH2/ FOR SUBSEQUENT GRADIENT,
C     WITH I RUNNING OVER C.I-ACTIVE M.O IF LGRAD IS FALSE
C                         OR   ALL   M.O IF LGRAD IS TRUE,
C          J AND K>=L RUNNING OVER ALL C.I-ACTIVE M.O.
      MDIJKL=N*NMOS*NMOS*(NMOS+1)/2
      MPQKL =NRELAX*MORB2-MDIJKL
      CALL IJKL (COEFF,N,NELEC+1,NMOS,W,DIJKL(MDIJKL+1),MPQKL
     .          ,XY,NMECI,DIJKL,LGRAD)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
C
C     READ OR GENERATE MICRO-STATES.
C     -----------------------------
      IF(MSGEN) THEN                                                    IR1294
         IF(IMICRO.NE.0)THEN                                            DJG0995
            K=IIMICR                                                    DJG0995
            LAB=K
            IF(DEBUG)WRITE(JOUT,'(''    MICROSTATES READ IN'')')
            NTOT=XX+0.5
            DO 110 I=1,LAB
               READ(JDAT,'(20I1)')(MICROA(J,I),J=1,NMOS),(MICROB(J,I)
     .                           ,J=1,NMOS)
               IF(DEBUG)WRITE(JOUT,'(20I6)')(MICROA(J,I),J=1,NMOS),
     1    (MICROB(J,I),J=1,NMOS)
               K=0
               DO 100 J=1,NMOS
  100          K=K+MICROA(J,I)+MICROB(J,I)
               IF(K.NE.NTOT)THEN
                  NTOT=K
                  XX=K
                  WRITE(JOUT,'(/,'' NUMBER OF ELECTRONS IN C.I. REDEFINE
     .D TO :'',I4)')K
               ENDIF
  110       CONTINUE
            MSGEN = .FALSE.                                             IR1294
         ENDIF
      ENDIF
C
C     COMPUTE SPIN MULTIPLICITY AND CHECK.
C
      K=KDELTA
      IF(PRNT)WRITE(JOUT,'(/,'' DELTA S = '',I4)')K
      NUPP=KALPHA+K
      NDOWN=KBETA-K
      AMS=(NUPP-NDOWN)*0.5D0
      IF(PRNT)WRITE(JOUT,'(/,'' MS = '',F4.1)') AMS
      IF(NUPP*NDOWN.LT.0) THEN
         WRITE(JOUT,'(/,'' IMPOSSIBLE VALUE OF DELTA S ... STOP.'')')
          ISTOP=1                                                       GDH1095
          IWHERE=137                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (ICIS.EQ.1) THEN                                               DL0397
C        GENERATE MICROSTATES BY SINGLY-EXCITED RULE.                   DL0397
         IF ((NE/2)*2.NE.NE) THEN                                       DL0397
            WRITE(JOUT,'('' OPTION "CIS" NOT ALLOWED WITH ODD NUMBER '',DL0397
     .      ''OF ELECTRONS.'')')                                        DL0397
            ISTOP=1                                                     DL0397
            IWHERE=138                                                  DL0397
            RETURN                                                      DL0397
         ENDIF                                                          DL0397
         LIMA=NUPP*(NMOS-NUPP)                                          DL0397
         LIMB=NDOWN*(NMOS-NDOWN)                                        DL0397
         LAB=1+LIMA+LIMB                                                DL0397
         IF(PRNT.OR.LAB.GT.MAXCI)                                       DL0397
     .   WRITE(JOUT,'(/'' NO OF CONFIGURATIONS CONSIDERED ='',I4)') LAB DL0397
         IF (LAB.GT.MAXCI) THEN                                         DL0397
            WRITE(JOUT,'('' TOO MANY CONFIGURATIONS IN C.I. STOP...'')')DL0397
            ISTOP=1                                                     DL0397
            IWHERE=138                                                  DL0397
            RETURN                                                      DL0397
         ENDIF                                                          DL0397
         CALL CISGEN (NUPP,NDOWN,NMOS,MICROA,MICROB,NMECI,LAB)          DL0397
      ELSE
C        GENERATE MICROSTATES BY FULL CI RULE.                          DL0397
         LIMA=NFA(NMOS+1)/(NFA(NUPP+1)*NFA(NMOS-NUPP+1))
         LIMB=NFA(NMOS+1)/(NFA(NDOWN+1)*NFA(NMOS-NDOWN+1))
         IF (MAX(LIMA,LIMB).GT.MAXPRM) THEN                             DL0397
            WRITE(JOUT,'('' NO ROOM FOR FULL-CI. STOP...'')')           DL0397
            ISTOP=1                                                     DL0397
            IWHERE=138                                                  DL0397
            RETURN                                                      DL0397
         ENDIF                                                          DL0397
         LAB=LIMA*LIMB
         IF(PRNT.OR.LAB.GT.MAXCI)                                       DL0397
     .   WRITE(JOUT,'(/'' NO OF CONFIGURATIONS CONSIDERED ='',I4)') LAB DL0397
         IF (LAB.GT.MAXCI) THEN                                         DL0397
            WRITE(JOUT,'('' TOO MANY CONFIGURATIONS IN C.I. STOP...'')')DL0397
            ISTOP=1                                                     GDH1095
            IWHERE=138                                                  GDH1095
            RETURN                                                      GDH1095
         ENDIF
         CALL PERM (NPERMA,NUPP, NMOS,NMECI,LIMA)                       DL0397
         CALL PERM (NPERMB,NDOWN,NMOS,NMECI,LIMB)                       DL0397
         K=0
         DO 120 I=1,LIMA
         DO 120 J=1,LIMB
         K=K+1
         DO 120 L=1,NMOS
         MICROA(L,K)=NPERMA(L,I)
  120    MICROB(L,K)=NPERMB(L,J)
      ENDIF
C
C     GROUND STATE (I.E VACUUM) ENERGY AND DIAGONAL ELEMENTS OF THE C.I
C     -----------------------------------------------------------------
      CALL MECID (EIGS,GSE,EIGA,DIAG)
C
C     BUILD SPIN AND NUMBER OF ALPHA SPIN TABLES.
C     -------------------------------------------
      DO 190 I=1,LAB
      K=0
      X=0.D0
      DO 180 J=1,NMOS
      X=X+MICROA(J,I)*MICROB(J,I)
  180 K=K+MICROA(J,I)
      NALPHA(I)=K
  190 SPIN(I)=4.D0*X-(XX-2*NALPHA(I))**2
C
C     SOME PRINTOUT.
C     --------------
      IF (PRNT) THEN
         WRITE(JOUT,'(/,'' NUMBER OF ELECTRONS IN C.I. ='',F5.1)')XX
         IF (DEBUG) THEN
            WRITE(JOUT,'(/''  C.I-ACTIVE M.O COEFFICIENTS AND '',
     .      ''EIGENVALUES BEFORE & AFTER 2-ELECT REMOVAL'')')
            DO 200 J=NELEC+1,NELEC+NMOS
            WRITE(JOUT,'('' M.O'',I4,'' EIGENVALUE'',2F12.6)')
     .                   J,EIGS(J),EIGA(J-NELEC)
  200       WRITE(JOUT,'(8F10.6)')(COEFF(I,J),I=1,NORBS)
         ENDIF
         WRITE(JOUT,'(/,'' TWO-ELECTRON J-INTEGRALS OVER ACTIVE M.O'')')
         DO 210 I=1,NMOS
  210    WRITE(JOUT,'(10F8.4)')(XY(I,I,J,J),J=1,NMOS)
         WRITE(JOUT,'(/,'' TWO-ELECTRON K-INTEGRALS OVER ACTIVE M.O'')')
         DO 220 I=1,NMOS
  220    WRITE(JOUT,'(10F8.4)')(XY(I,J,I,J),J=1,NMOS)
         WRITE(JOUT,'(/,'' GROUND STATE ENERGY:'',F13.6,'' E.V.'')')GSE
         WRITE(JOUT,'(/,'' CONFIGURATIONS CONSIDERED IN C.I.''/
     1          '' M.O. NUMBER :'',30I3)')(I,I=NELEC+1,NELEC+NMOS)      DL0397
         DO 230 I=1,LAB
         WRITE(JOUT,'(/10X,I4,30I3)') I,(MICROA(K,I),K=1,NMOS)          DL0397
  230    WRITE(JOUT,'(14X,30I3)')(MICROB(K,I),K=1,NMOS)                 DL0397
      ENDIF
C
C  FILL SECULAR DETERMINANT
C  ------------------------
C
      CALL MECIH (DIAG,CIMAT)
C
C     PRINTOUT
      IF(DEBUG)THEN
         WRITE(JOUT,'(/,'' C.I. MATRIX'')')
         CALL VECPRT(CIMAT,LAB)
      ELSE
         IF(PRNT) THEN
            WRITE(JOUT,'(/,'' DIAGONAL OF C.I. MATRIX'')')
            WRITE(6,'(8F10.6)')(CIMAT((I*(I+1))/2),I=1,LAB)             DL0397
         ENDIF
      ENDIF
C
C     DIAGONALIZE THE C.I MATRIX
C     --------------------------
      LABCUT=MIN(LAB,MAX(LROOT,KROOT)+10)                               DL0397
      CALL HQRII(CIMAT,LAB,LABCUT,EIG,CONF)
C
C     DECIDE WHICH ROOT TO EXTRACT
      KROOT=0
      IF(SMULT.LT.0.1D0) KROOT=LROOT
      IF(PRNT.AND.PRTVEC)  THEN
         WRITE(JOUT,'(/,'' STATE EIGENVECTORS'')')
         CALL MATOUT(CONF,EIG,LABCUT,LAB,LAB)
      ENDIF
      IF(PRNT)WRITE(JOUT,'(/,'' STATE ENERGIES '',
     .           '' EXPECTATION VALUE OF S**2  S FROM S**2=S(S+1)'')')
C     VARIABLE IROOT CHANGED TO IROOTA TO NOT CONFLICT WITH KEYWORD FLAGDJG0995
      IROOTA=0                                                          DJG0995
      DO 270 I=1,LABCUT
         X=0.5D0*XX
         II=(I-1)*LAB
         DO 260 J=1,LAB
            JI=J+II
            X=X-CONF(JI)*CONF(JI)*SPIN(J)*0.25D0
            K=ISPQR(J,1)
            IF(K.EQ.1)  GOTO  250
            DO 240 K=2,K
               LI=ISPQR(J,K)+II
  240       X=X+CONF(JI)*CONF(LI)*2.D0
  250       CONTINUE
  260    CONTINUE
         Y=(-1.D0+SQRT(1.D0+4.D0*X))*0.5D0
         IF(ABS(SMULT-X).LT.0.01)THEN
            IROOTA=IROOTA+1                                             DJG0995
            IF(IROOTA.EQ.LROOT) KROOT=I                                 DJG0995
         ENDIF
         J=Y*2.D0+1.5D0
  270 IF(PRNT)WRITE(JOUT,'(F12.6,I5,3X,A8,2F8.2)') EIG(I),I,TSPIN(J),X,Y
C
C     SELECTED EIGENSTATE TOWARD OPTIMIZATION AND DERIVATIVES
C     -------------------------------------------------------
      MECI=EIG(KROOT)
      J=LAB*(KROOT-1)
      DO 280 I=1,LAB
  280 VECTCI(I)=CONF(I+J)
      IF(PRNT) THEN
         WRITE(JOUT,'(/'' EIGENVECTOR OF THE SELECTED STATE'',
     1                 '' (NUMBER'',I3,'')'')')KROOT
         WRITE(JOUT,'(10F8.4)')(VECTCI(I),I=1,LAB)
      ENDIF
C
C     BUILD AND PRINT SPIN DENSITY MATRIX AFTER C.I.
C     ----------------------------------------------
      IF (.NOT.LSPIN) RETURN
      MAXVEC=MIN(4,LAB)
      IF((NE/2)*2.EQ.NE) THEN
        WRITE(JOUT,'(/,'' ESR SPECIFIED FOR AN EVEN-ELECTRON SYSTEM'')')
      ENDIF
      DO 300 I=1,NMOS
         DO 300 J=1,NORBS
  300 WORK(J,I)=COEFF(J,NELEC+I)**2
      DO 370 IUJ=1,MAXVEC
         IOFSET=(IUJ-1)*LAB
         WRITE(JOUT,'(/,'' MICROSTATE CONTRIBUTIONS TO '',
     .                  ''STATE EIGENFUNCTION'',I3)')IUJ
         WRITE(JOUT,'(5F13.6)')(CONF(I+IOFSET),I=1,LAB)
         DO 310 I=1,LAB
  310    CONF(I)=CONF(I+IOFSET)**2
C                                             SECOND VECTOR]
         DO 330 I=1,NMOS
            SUM=0.D0
            DO 320 J=1,LAB
  320       SUM=SUM+(MICROA(I,J)-MICROB(I,J))*CONF(J)
  330    EIGA(I)=SUM
         WRITE(JOUT,'(/,'' SPIN DENSITIES FROM EACH M.O., ENERGY:''
     .               ,F7.3)')EIG(IUJ)
         WRITE(JOUT,'(5F12.6)') (EIGA(I),I=1,NMOS)
         WRITE(JOUT,*)'     SPIN DENSITIES FROM EACH ATOMIC ORBITAL'
         WRITE(JOUT,*)'    S        PX        PY        PZ        TOTAL'
         DO 360 I=1,NUMAT
            IL=NFIRST(I)
            IU=NLAST(I)
            L=0
            SUMM=0.D0
            DO 350 K=IL,IU
               L=L+1
               SUM=0.D0
               DO 340 J=1,NMOS
  340          SUM=SUM+WORK(K,J)*EIGA(J)
            SUMM=SUMM+SUM
  350       EIGS(L)=SUM
      IF(L.EQ.4)THEN
         WRITE(JOUT,'(''  ATOM'',I4,''    SPIN DENSITY  '',5F10.7)')
     .               I,(EIGS(K),K=1,L),SUMM
      ELSE
         WRITE(JOUT,'(''  ATOM'',I4,''    SPIN DENSITY  '',F10.7
     .               ,30X,F10.7)')I,EIGS(1),SUMM
      ENDIF
  360 CONTINUE
  370 CONTINUE
      END
