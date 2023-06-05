      SUBROUTINE PRTDRC(ESCF,DELTT,XPARAM,REF,EKIN,GTOT,ETOT,VELO0,NVAR
     &                  ,*)                                             CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      INCLUDE 'SIZES.i'
C
C
      DIMENSION XPARAM(*), VELO0(*), REF(*)
*********************************************************************
*
*    PRTDRC PREPARES TO PRINT THE GEOMETRY ETC. FOR POINTS IN A DRC
*    OR IRC
*    CALCULATION.
*    ON INPUT  ESCF   = HEAT OF FORMATION FOR THE CURRENT POINT
*              DELTT  = CHANGE IN TIME, PREVIOUS TO CURRENT POINT
*              XPARAM = CURRENT CARTESIAN GEOMETRY
*              EKIN   = CURRENT KINETIC ENERGY
*              GTOT   = TOTAL GRADIENT NORM IN IRC CALC'N.
*              VELO0  = CURRENT VELOCITY
*              NVAR   = NUMBER OF VARIABLES = 3 * NUMBER OF ATOMS.
*
********************************************************************
      COMMON /KEYWRD/ KEYWRD
C Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /DRCCOM/  MCOPRT(2,MAXPAR), NCOPRT, PARMAX
      COMMON /CORE  / CORE(120)
      COMMON /ATMASS/ ATMASS(NUMATM)
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /FMATRX/ ALLXYZ(3,MAXPAR),ALLVEL(3,MAXPAR),PARREF(MAXPAR),
     1XYZ3(3,MAXPAR),VEL3(3,MAXPAR), ALLGEO(3,MAXPAR), GEO3(3,MAXPAR),
     2 DUMMY(MAXHES-19*MAXPAR)
      COMMON /NUMCAL/ NUMCAL
      DIMENSION ESCF3(3),EKIN3(3), GTOT3(3), CHARGE(NUMATM), XOLD3(3),
     1GEO(3*NUMATM), VREF(MAXPAR), VREF0(MAXPAR), TSTEPS(100), ETOT3(3),
     2XTOT3(3)
      LOGICAL TURN, PARMAX, LDRC, GOTURN
      CHARACTER*160 KEYWRD
      CHARACTER*80  TEXT1*3, TEXT2*2,  COTYPE(3)*2
         SAVE                                                           GL0892
      DATA REFSCF/0.D0/
      DATA COTYPE/'BL','BA','DI'/
      DATA ICALCN /0/
C*TNT      IF (FIRST) THEN
      IF (ICALCN .NE. NUMCAL) THEN
         SQRT2=SQRT(2.D0)
         DO 10 I=1,NVAR
   10    PARREF(I)=XPARAM(I)
         ETOT=ESCF+EKIN
         TLAST=0.D0
         GOTURN=.FALSE.
         SUM=0.D0
         DO 20 I=1,NVAR
            SUM=SUM+VELO0(I)**2
            VREF0(I)=VELO0(I)
   20    VREF(I)=VELO0(I)
         IONE=1
         LDRC=(SUM.GT.1.D0)
C*TNT         FIRST=.FALSE.
         ICALCN = NUMCAL
         ILOOP=1
         OLDT=-100.D0
         TOLD1=0.0D0
C
C       DETERMINE TYPE OF PRINT: TIME, ENERGY OR GEOMETRY PRIORITY
C       OR PRINT ALL POINTS
C
         STEPT=0.D0
         STEPH=0.D0
         STEPX=0.D0
         IF(INDEX(KEYWRD,' T-PRIO').NE.0)THEN
            IF(INDEX(KEYWRD,' T-PRIORITY=').NE.0)THEN
               STEPT=READA(KEYWRD,INDEX(KEYWRD,'T-PRIO')+5)
            ELSE
               STEPT=0.1D0
            ENDIF
            TREF=-1.D-6
            IF (DOPRNT) WRITE(6,'(/,'' TIME PRIORITY, INTERVAL ='',     CIO
     &                           F4.1,'' FEMTOSECONDS'',/)')STEPT       CIO
         ELSEIF(INDEX(KEYWRD,' H-PRIO').NE.0)THEN
            IF(INDEX(KEYWRD,' H-PRIORITY=').NE.0)THEN
               STEPH=READA(KEYWRD,INDEX(KEYWRD,'H-PRIO')+5)
            ELSE
               STEPH=0.1D0
            ENDIF
            WRITE(6,'(/,'' KINETIC ENERGY PRIORITY, STEP ='',F5.2,      CIO
     &                   '' KCAL/MOLE'',/)')STEPH                       CIO
         ELSEIF(INDEX(KEYWRD,' X-PRIO').NE.0)THEN
            IF(INDEX(KEYWRD,' X-PRIORITY=').NE.0)THEN
               STEPX=READA(KEYWRD,INDEX(KEYWRD,'X-PRIO')+5)
            ELSE
               STEPX=0.05D0
            ENDIF
            IF (DOPRNT) WRITE(6,'(/,'' GEOMETRY PRIORITY, STEP ='',     CIO
     &                            F5.2,'' ANGSTROMS'',/)')STEPX         CIO
         ENDIF
         IF(INDEX(KEYWRD,' REST').NE.0.AND.INDEX(KEYWRD,'IRC=').EQ.0) TH
     1EN
            IF (DOPRNT) THEN                                            CIO
            READ(9,*)(PARREF(I),I=1,NVAR)
            READ(9,*)(VREF0(I),I=1,NVAR)
            READ(9,*)(VREF(I),I=1,NVAR)
            READ(9,*)(ALLGEO(3,I),I=1,NVAR)
            READ(9,*)(ALLGEO(2,I),I=1,NVAR)
            READ(9,*)(ALLGEO(1,I),I=1,NVAR)
            READ(9,*)(ALLVEL(3,I),I=1,NVAR)
            READ(9,*)(ALLVEL(2,I),I=1,NVAR)
            READ(9,*)(ALLVEL(1,I),I=1,NVAR)
            READ(9,*)(ALLXYZ(3,I),I=1,NVAR)
            READ(9,*)(ALLXYZ(2,I),I=1,NVAR)
            READ(9,*)(ALLXYZ(1,I),I=1,NVAR)
            READ(9,*)ILOOP,LDRC,IONE,ETOT1,ETOT0,ESCF1,ESCF0,EKIN1,
     &               EKIN0,TOLD2,TOLD1,GTOT1,GTOT0,XOLD2,XOLD1,XOLD0,
     &               TOTIME,JLOOP,ETOT,REFX
            ENDIF                                                       CIO
         ENDIF
      ENDIF
      IF(ESCF.LT.-1.D8) THEN
         IF (DOPRNT) THEN                                               CIO
         WRITE(9,*)(PARREF(I),I=1,NVAR)
         WRITE(9,*)(VREF0(I),I=1,NVAR)
         WRITE(9,*)(VREF(I),I=1,NVAR)
         WRITE(9,*)(ALLGEO(3,I),I=1,NVAR)
         WRITE(9,*)(ALLGEO(2,I),I=1,NVAR)
         WRITE(9,*)(ALLGEO(1,I),I=1,NVAR)
         WRITE(9,*)(ALLVEL(3,I),I=1,NVAR)
         WRITE(9,*)(ALLVEL(2,I),I=1,NVAR)
         WRITE(9,*)(ALLVEL(1,I),I=1,NVAR)
         WRITE(9,*)(ALLXYZ(3,I),I=1,NVAR)
         WRITE(9,*)(ALLXYZ(2,I),I=1,NVAR)
         WRITE(9,*)(ALLXYZ(1,I),I=1,NVAR)
         WRITE(9,*)ILOOP,LDRC,IONE,ETOT1,ETOT0,ESCF1,ESCF0,EKIN1,EKIN0,
     1TOLD2,TOLD1,GTOT1,GTOT0,XOLD2,XOLD1,XOLD0,TOTIME,JLOOP,ETOT,REFX
         ENDIF                                                          CIO
         RETURN
      ENDIF
      CALL CHRGE(P,CHARGE,*9999)                                         CSTP(call)
      DO 30 I=1,NUMAT
         L=NAT(I)
   30 CHARGE(I)=CORE(L) - CHARGE(I)
      DELTAT=DELTT*1.D15
      NA(2)=-1
      CALL XYZINT(XPARAM,NUMAT,NA,NB,NC,57.29577951D0,GEO,*9999)         CSTP(call)
      NA(1)=99
      IF(ILOOP.EQ.1)THEN
         ETOT1=ETOT0
         ETOT0=ETOT
         ESCF1=ESCF
         ESCF0=ESCF
         EKIN1=EKIN
         EKIN0=EKIN
         DO 40 J=1,3
            DO 40 I=1,NVAR
               ALLGEO(J,I)=GEO(I)
               ALLXYZ(J,I)=XPARAM(I)
   40    ALLVEL(J,I)=VELO0(I)
      ELSE
         DO 50 I=1,NVAR
            ALLGEO(3,I)=ALLGEO(2,I)
            ALLGEO(2,I)=ALLGEO(1,I)
            ALLGEO(1,I)=GEO(I)
            ALLXYZ(3,I)=ALLXYZ(2,I)
            ALLXYZ(2,I)=ALLXYZ(1,I)
            ALLXYZ(1,I)=XPARAM(I)
            ALLVEL(3,I)=ALLVEL(2,I)
            ALLVEL(2,I)=ALLVEL(1,I)
   50    ALLVEL(1,I)=VELO0(I)
      ENDIF
C
C  FORM QUADRATIC EXPRESSION FOR POSITION AND VELOCITY W.R.T. TIME.
C
      T1=MAX(TOLD2,0.02D0)
      T2=MAX(TOLD1,0.02D0)+T1
      DO 60 I=1,NVAR
      geo3(3,i) = (t2*(allgeo(2,i) - allgeo(3,i)) -
     &             t1*(allgeo(1,i) - allgeo(3,i)))/
     &             (t2*t1**2 - t1*t2**2)
      geo3(2,i) = (allgeo(2,i) - allgeo(3,i) - geo3(3,i)*t1**2)/t1
      geo3(1,i) = allgeo(3,i)
      xyz3(3,i) = (t2*(allxyz(2,i) - allxyz(3,i)) -
     &             t1*(allxyz(1,i) - allxyz(3,i)))/
     &             (t2*t1**2 - t1*t2**2)
      xyz3(2,i) = (allxyz(2,i) - allxyz(3,i) - xyz3(3,i)*t1**2)/t1
      xyz3(1,i) = allxyz(3,i)
      vel3(3,i) = (t2*(allvel(2,i) - allvel(3,i)) -
     &             t1*(allvel(1,i) - allvel(3,i)))/
     &             (t2*t1**2 - t1*t2**2)
      vel3(2,i) = (allvel(2,i) - allvel(3,i) - vel3(3,i)*t1**2)/t1
      vel3(1,i) = allvel(3,i)
   60 CONTINUE
      ETOT2=ETOT1
      ETOT1=ETOT0
      ETOT0=ETOT
      CALL QUADR(ETOT2,ETOT1,ETOT0,T1,T2,ETOT3(1),ETOT3(2),
     1ETOT3(3),*9999)                                                    CSTP(call)
      EKIN2=EKIN1
      EKIN1=EKIN0
      EKIN0=EKIN
      CALL QUADR(EKIN2,EKIN1,EKIN0,T1,T2,EKIN3(1),EKIN3(2),
     1EKIN3(3),*9999)                                                    CSTP(call)
      ESCF2=ESCF1
      ESCF1=ESCF0
      ESCF0=ESCF
      CALL QUADR(ESCF2,ESCF1,ESCF0,T1,T2,ESCF3(1),ESCF3(2),
     1ESCF3(3),*9999)                                                    CSTP(call)
      GTOT2=GTOT1
      GTOT1=GTOT0
      GTOT0=GTOT
      CALL QUADR(GTOT2,GTOT1,GTOT0,T1,T2,GTOT3(1),GTOT3(2),
     1GTOT3(3),*9999)                                                    CSTP(call)
      XTOT2=XTOT1
      XTOT1=XTOT0
      XOLD2=XOLD2+XOLD1
      XOLD1=XOLD0
C
C   CALCULATE CHANGE IN GEOMETRY
C
      XOLD0=0.D0
      L=0
      XTOT0=0.D0
      SUM1=0.D0
      DO 80 I=1,NUMAT
         SUM=0.D0
         DO 70 J=1,3
            L=L+1
            SUM1=SUM1+(ALLXYZ(1,L)-REF(L))**2*ATMASS(I)
   70    SUM=SUM+(ALLXYZ(2,L)-ALLXYZ(1,L))**2
   80 XOLD0=XOLD0+SQRT(SUM)
      XTOT0=XTOT0+SQRT(SUM1)/SQRT2
      CALL QUADR(XTOT2,XTOT1,XTOT0,T1,T2,
     1XTOT3(1),XTOT3(2),XTOT3(3),*9999)                                  CSTP(call)
      CALL QUADR(XOLD2,XOLD2+XOLD1,XOLD2+XOLD1+XOLD0,T1,T2,
     1XOLD3(1),XOLD3(2),XOLD3(3),*9999)                                  CSTP(call)
***********************************************************************
*   GO THROUGH THE CRITERIA FOR DECIDING WHETHER OR NOT TO PRINT THIS *
*   POINT.  IF YES, THEN ALSO CALCULATE THE EXACT POINT AS A FRACTION *
*   BETWEEN THE LAST POINT AND THE CURRENT POINT                      *
***********************************************************************
C   NFRACT IS THE NUMBER OF POINTS TO BE PRINTED IN THE CURRENT DOMAIN
***********************************************************************
      IF(ILOOP.LT.3) GOTO 170
      FRACT=-10
      NFRACT=1
      IF(STEPH.NE.0) THEN
C
C   CRITERION FOR PRINTING RESULTS  IS A CHANGE IN HEAT OF FORMATION =
C   -CHANGE IN KINETIC ENERGY
C
         IF(REFSCF.EQ.0.D0) THEN
            I=nint(ESCF2/STEPH)
            REFSCF=I*STEPH
         ENDIF
         DH=ABS(ESCF1-REFSCF)
         IF(DH.GT.STEPH)THEN
            STEPH=SIGN(STEPH,ESCF1-REFSCF)
          nfract = iabs(nint(dh/steph))
            CC=ESCF3(1)
            BB=ESCF3(2)
            AA=ESCF3(3)
************************************************
* PROGRAMMERS! - BE VERY CAREFUL IF YOU CHANGE *
* THIS FOLLOWING SECTION.  THERE IS NUMERICAL  *
* INSTABILITY IF ABS(BB/AA) IS VERY LARGE. NEAR*
* INFLECTION POINTS AA CHANGES SIGN.       JJPS*
************************************************
            IF(ABS(BB/AA).GT.30)THEN
C
C   USE LINEAR INTERPOLATION
C
               DO 90 I=1,NFRACT
   90          TSTEPS(I)=-(CC-(REFSCF+I*STEPH))/BB
            ELSE
C
C  USE QUADRATIC INTERPOLATION
C
               DO 100 I=1,NFRACT
                  C1=CC-(REFSCF+I*STEPH)
  100          TSTEPS(I)=(-BB+SIGN(SQRT(BB*BB-4.D0*(AA*C1)),BB))/(2.D0*A
     1A)
            ENDIF
            FRACT=-.1
            REFSCF=REFSCF+NFRACT*STEPH
         ENDIF
      ELSEIF(STEPT.NE.0.D0) THEN
C
C   CRITERION FOR PRINTING RESULTS IS A CHANGE IN TIME.
C
         IF(ABS(TOTIME+TOLD2-TREF).GT.STEPT)THEN
            FINCR=STEPT
            I=nint(TOTIME/STEPT)
            FRACT=I*STEPT-TOTIME
            I=nint((TOLD2+TOTIME)/STEPT)
            J=nint(TOTIME/STEPT)
            NFRACT=I-J+ IONE
            IONE=0
            DO 110 I=1,NFRACT
  110       TSTEPS(I)=FRACT+I*STEPT
            TREF=TREF+NFRACT*STEPT
         ENDIF
      ELSEIF(STEPX.NE.0.D0) THEN
C
C   CRITERION FOR PRINTING RESULTS IS A CHANGE IN GEOMETRY.
C
         IF(XOLD2+XOLD1-REFX.GT.STEPX)THEN
            NFRACT=nint((XOLD2+XOLD1-REFX)/STEPX)
            CC=XOLD3(1)
            BB=XOLD3(2)
            AA=XOLD3(3)
            IF(ABS(BB/AA).GT.30)THEN
C
C   USE LINEAR INTERPOLATION
C
               DO 120 I=1,NFRACT
  120          TSTEPS(I)=-(CC-(REFX+I*STEPX))/BB
            ELSE
C
C  USE QUADRATIC INTERPOLATION
C
               DO 130 I=1,NFRACT
                  C1=CC-(REFX+I*STEPX)
  130          TSTEPS(I)=(-BB+SIGN(SQRT(BB*BB-4.D0*(AA*C1)),BB))/(2.D0*A
     1A)
            ENDIF
            REFX=REFX+NFRACT*STEPX
            FRACT=-.1
         ENDIF
      ELSE
C
C   PRINT EVERY POINT.
C
         FRACT=0.0
      ENDIF
      IF(FRACT.LT.-9.D0)GOTO 170
      TURN=(TURN.OR.ABS(FRACT-1.D0).GT.1.D-6)
C
C  LOOP OVER ALL POINTS IN CURRENT DOMAIN
C
      IF(FRACT.EQ.0.D0.AND.NFRACT.EQ.1)THEN
         TEXT1=' '
         TEXT2=' '
         II=0
         CALL DRCOUT(XYZ3,GEO3,VEL3,NVAR,TOTIME,ESCF3,EKIN3,
     1GTOT3,ETOT3,XTOT3,ILOOP,CHARGE,FRACT,TEXT1,TEXT2,II,JLOOP,*9999)   CSTP(call)
         N=0
         DO 140 I=1,NCOPRT
            K=MCOPRT(1,I)
            J=MCOPRT(2,I)
            L=K*3-3+J
            IF(ABS(GEO3(3,L)).GT.1.D-20)FRACT=-GEO3(2,L)/(GEO3(3,L)*2.D0
     1)
            IF(FRACT.GT.0.D0.AND.FRACT.LT.TOLD2) THEN
               IF(GEO3(3,L).GT.0.D0)TEXT1='MIN'
               IF(GEO3(3,L).LT.0.D0)TEXT1='MAX'
               TEXT2=COTYPE(J)
               IF(N.EQ.0)THEN
                  N=N+1
                  IF (DOPRNT) WRITE(6,'(/,20(''****''))')               CIO
               ENDIF
               TIME=TOTIME+FRACT
               CALL DRCOUT(XYZ3,GEO3,VEL3,NVAR,TIME,ESCF3,EKIN3,
     1GTOT3,ETOT3,XTOT3,ILOOP,CHARGE,FRACT,TEXT1,TEXT2,K,JLOOP,*9999)    CSTP(call)
            ENDIF
  140    CONTINUE
         IF(N.NE.0.AND.DOPRNT) WRITE(6,'(/,20(''****''))')              CIO
         IF(ABS(ESCF3(3)).GT.1.D-20)FRACT=-ESCF3(2)/(ESCF3(3)*2.D0)
         IF(.NOT.GOTURN.AND.FRACT.GT.0.D0.AND.FRACT.LT.TOLD2*1.04D0
     1.AND. PARMAX) THEN
            GOTURN=.TRUE.
            TIME=FRACT+TOTIME
            IF(ESCF3(3).GT.0.D0) THEN
               TEXT1='MIN'
               IF(LDRC) THEN
                  SUM= ddot(nvar,VELO0,1,VREF ,1)**2/
     &                (ddot(nvar,VELO0,1,VELO0,1)*
     &                 ddot(nvar,VREF ,1,VREF ,1)+1.D-10)
                  SUM1=ddot(nvar,VELO0,1,VREF0,1)**2/
     &                (ddot(nvar,VELO0,1,VELO0,1)*
     &                 ddot(nvar,VREF0,1,VREF0,1)+1.D-10)
                  IF(SUM1.GT.0.1D0.AND.DOPRNT)                          CIO
     1WRITE(6,'(/,A,F8.5,A,F8.5,A,F8.1,A)')' COEF. OF V(0)
     2=',SUM1,'   LAST V(0)',SUM,'   HALF-LIFE =',
     3-0.6931472D0*TIME/LOG(SUM1),' FEMTOSECS'
               ENDIF
               IF (DOPRNT) WRITE(6,'(//,A,F11.3,A)')                    CIO
     &                 ' HALF-CYCLE TIME =',TIME-TLAST,' FEMTOSECONDS'  CIO
               TLAST=TIME
               DO 150 I=1,NVAR
  150          VREF(I)=VELO0(I)
            ENDIF
            IF(ESCF3(3).LT.0.D0)TEXT1='MAX'
            TEXT2=' '
            CALL DRCOUT(XYZ3,GEO3,VEL3,NVAR,TIME,ESCF3,EKIN3,
     1GTOT3,ETOT3,XTOT3,ILOOP,CHARGE,FRACT,TEXT1,TEXT2,0,JLOOP,*9999)    CSTP(call)
         ELSE
            GOTURN=.FALSE.
         ENDIF
      ELSE
         DO 160 I=1,NFRACT
            TIME=TOTIME+TSTEPS(I)
            TEXT1=' '
            TEXT2=' '
C#           WRITE(6,'(A,4F12.4)')' KINETIC ENERGY, POINT',EKIN3,TSTEPS(
            CALL DRCOUT(XYZ3,GEO3,VEL3,NVAR,TIME,ESCF3,EKIN3,
     &GTOT3,ETOT3,XTOT3,ILOOP,CHARGE,TSTEPS(I),TEXT1,TEXT2,0,JLOOP,     CSTP(call)
     & *9999)                                                           CSTP(call)
  160    CONTINUE
      ENDIF
  170 CONTINUE
C
C BUFFER TOTAL TIME TO 3 POINTS BACK!
C
      TOTIME=TOTIME+TOLD2
      TOLD2=TOLD1
      TOLD1=DELTAT
      ILOOP=ILOOP+1
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY PRTDRC_INIT                                                 CSAV
                 A = 0.0D0                                              CSAV
                AA = 0.0D0                                              CSAV
                BB = 0.0D0                                              CSAV
                C1 = 0.0D0                                              CSAV
                CC = 0.0D0                                              CSAV
            CHARGE = 0.0D0                                              CSAV
            DELTAT = 0.0D0                                              CSAV
                DH = 0.0D0                                              CSAV
                EN = 0.0D0                                              CSAV
             FRACT = 0.0D0                                              CSAV
               GEO = 0.0D0                                              CSAV
            GOTURN = .FALSE.                                            CSAV
             GTOT0 = 0.0D0                                              CSAV
             GTOT1 = 0.0D0                                              CSAV
             GTOT2 = 0.0D0                                              CSAV
             GTOT3 = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
                II = 0                                                  CSAV
             ILOOP = 0                                                  CSAV
              IONE = 0                                                  CSAV
                 J = 0                                                  CSAV
             JLOOP = 0                                                  CSAV
                 K = 0                                                  CSAV
                 L = 0                                                  CSAV
              LDRC = .FALSE.                                            CSAV
                 N = 0                                                  CSAV
            NCOPRT = 0                                                  CSAV
            NFRACT = 0                                                  CSAV
              OLDT = 0.0D0                                              CSAV
            PARMAX = .FALSE.                                            CSAV
            REFSCF = 0.0D0                                              CSAV
              REFX = 0.0D0                                              CSAV
             SQRT2 = 0.0D0                                              CSAV
             STEPH = 0.0D0                                              CSAV
             STEPT = 0.0D0                                              CSAV
             STEPX = 0.0D0                                              CSAV
               SUM = 0.0D0                                              CSAV
              SUM1 = 0.0D0                                              CSAV
                T1 = 0.0D0                                              CSAV
                T2 = 0.0D0                                              CSAV
             TEXT1 = ""                                                 CSAV
             TEXT2 = ""                                                 CSAV
              TIME = 0.0D0                                              CSAV
             TLAST = 0.0D0                                              CSAV
             TOLD1 = 0.0D0                                              CSAV
             TOLD2 = 0.0D0                                              CSAV
            TOTIME = 0.0D0                                              CSAV
              TREF = 0.0D0                                              CSAV
            TSTEPS = 0.0D0                                              CSAV
              TURN = .FALSE.                                            CSAV
              VREF = 0.0D0                                              CSAV
             VREF0 = 0.0D0                                              CSAV
             XOLD0 = 0.0D0                                              CSAV
             XOLD1 = 0.0D0                                              CSAV
             XOLD2 = 0.0D0                                              CSAV
             XOLD3 = 0.0D0                                              CSAV
             XTOT0 = 0.0D0                                              CSAV
             XTOT1 = 0.0D0                                              CSAV
             XTOT2 = 0.0D0                                              CSAV
             XTOT3 = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
