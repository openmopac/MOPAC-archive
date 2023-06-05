      SUBROUTINE DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP,*)      CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      include 'SIZES.i'
C
************************************************************************
      DIMENSION XPARAM(*), GD(*), XLAST(*), MDFP(9),XDFP(9)
**********************************************************************
*
* DFPSAV STORES AND RESTORES DATA USED IN THE D-F-P GEOMETRY
*        OPTIMIZATION.
*
*  ON INPUT TOTIME = TOTAL CPU TIME ELAPSED DURING THE CALCULATION.
*           XPARAM = CURRENT VALUE OF PARAMETERS.
*           GD     = OLD GRADIENT.
*           XLAST  = OLD VALUE OF PARAMETERS.
*           FUNCT1 = CURRENT VALUE OF HEAT OF FORMATION.
*           MDFP   = INTEGER CONSTANTS USED IN D-F-P.
*           XDFP   = REAL CONSTANTS USED IN D-F-P.
*           MDFP(9)= 1 FOR DUMP, 0 FOR RESTORE.
**********************************************************************
      COMMON /KEYWRD/ KEYWRD
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /GEOVAR/ XDUMMY(MAXPAR),NVAR,LOC(2,MAXPAR)                 IR0394
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /ALPARM/ ALPARM(3,MAXPAR),X0, X1, X2, ILOOP
      COMMON /REACTN/ STEP, GEOA(3,NUMATM), GEOVEC(3,NUMATM),CALCST
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /ELEMTS/ ELEMNT(120)
C    changed common path for portability  (IR)
      COMMON /PATHI / LATOM,LPARAM
      COMMON /PATHR / REACT(200)
C
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /FMATRX/ HESINV(MAXHES)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),
     1                     LOCDEP(MAXPAR)
      COMMON /ERRFN / ERRFN(MAXPAR)
      COMMON /NUMCAL/ NUMCAL
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION IEL1(3),Q(3), COORD(3,NUMATM)
      CHARACTER ELEMNT*2, KEYWRD*160,KOMENT*80, TITLE*80
      LOGICAL INTXYZ
             SAVE                                                           GL0892
      DATA ICALCN /0/
      IF(DOPRNT)OPEN(UNIT=9,FILE='FOR009',STATUS='UNKNOWN',             CIO
     &               FORM='UNFORMATTED')                                CIO
      IF(DOPRNT) REWIND 9                                               CIO
      IF(DOPRNT) OPEN(UNIT=10,FILE='FOR010',STATUS='UNKNOWN',           CIO
     &                FORM='UNFORMATTED')                               CIO
      IF(DOPRNT) REWIND 10                                              CIO
      DEGREE=57.29577951D0
      IR=9
      IF(MDFP(9) .NE. 0) THEN
         IF(MDFP(9) .EQ. 1) THEN
            IF (DOPRNT) WRITE(6,'(//10X,                                CIO
     &                   ''- - - - - - - TIME UP - - - - - - -'',//)')  CIO
            IF(INDEX(KEYWRD,'SADDLE') .NE. 0) THEN
               IF (DOPRNT)                                              CIO
     &         WRITE(6,'(//10X,'' NO RESTART EXISTS FOR SADDLE'',//     CIO
     &  10X,'' HERE IS A DATA-FILE FILES THAT MIGHT BE SUITABLE'',/     CIO
     &  10X,'' FOR RESTARTING THE CALCULATION'',///)')                  CIO
               IF (DOPRNT) WRITE(6,'(A)')KEYWRD,KOMENT,TITLE            CIO
               INTXYZ=(NA(1).EQ.0)
               DO 60 ILOOP=1,2
                  IF(INTXYZ)THEN
                     GEO(2,1)=0.D0
                     GEO(3,1)=0.D0
                     GEO(1,1)=0.D0
                     GEO(2,2)=0.D0
                     GEO(3,2)=0.D0
                     GEO(3,3)=0.D0
                     DO 10 I=1,NATOMS
                        DO 10 J=1,3
   10                COORD(J,I)=GEO(J,I)
                  ELSE
                     CALL XYZINT(GEO,NUMAT,NA,NB,NC,1.D0,COORD,*9999)    CSTP(call)
                  ENDIF
                  IVAR=1
                  NA(1)=0
                  DO 40 I=1,NATOMS
                     DO 20 J=1,3
   20                IEL1(J)=0
   30                CONTINUE
                     IF(LOC(1,IVAR).EQ.I) THEN
                        IEL1(LOC(2,IVAR))=1
                        IVAR=IVAR+1
                        GOTO 30
                     ENDIF
                     IF(I.LT.4) THEN
                        IEL1(3)=0
                        IF(I.LT.3) THEN
                           IEL1(2)=0
                           IF(I.LT.2) THEN
                              IEL1(1)=0
                           ENDIF
                        ENDIF
                     ENDIF
                     IF(I.EQ.LATOM)IEL1(LPARAM)=-1
                     Q(1)=COORD(1,I)
                     Q(2)=COORD(2,I)*DEGREE
                     Q(3)=COORD(3,I)*DEGREE
   40             IF (DOPRNT) WRITE(6,'(2X,A2,3(F12.6,I3),I4,2I3)')     CIO
     &    ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I)      CIO
                  I=0
                  X=0.D0
                  IF (DOPRNT) WRITE(6,'(I4,3(F12.6,I3),I4,2I3)')        CIO
     &                              I,X,I,X,I,X,I,I,I,I                 CIO
                  DO 50 I=1,NATOMS
                     DO 50 J=1,3
   50             GEO(J,I)=GEOA(J,I)
                  NA(1)=99
   60          CONTINUE
               IF (DOPRNT)                                              CIO
     &             WRITE(6,'(///10X,''CALCULATION TERMINATED HERE'')')  CIO
      RETURN 1                                                           CSTP (stop)
            ENDIF
            IF (DOPRNT) WRITE(6,                                        CIO
     &          '(//10X,'' - THE CALCULATION IS BEING DUMPED TO DISK'', CIO
     &          /10X,''   RESTART IT USING THE MAGIC WORD "RESTART"'')')CIO
            IF (DOPRNT) WRITE(6,'(//10X,                                CIO
     &         ''CURRENT VALUE OF HEAT OF FORMATION ='',F12.6)')FUNCT1  CIO
         ENDIF
         IF(MDFP(9) .EQ. 1)THEN
            IF(NA(1) .EQ. 99) THEN
C
C  CONVERT FROM CARTESIAN COORDINATES TO INTERNAL
C
               DO 70 I=1,NATOMS
                  DO 70 J=1,3
   70          COORD(J,I)=GEO(J,I)
               CALL XYZINT(COORD,NUMAT,NA,NB,NC,1.D0,GEO,*9999)          CSTP(call)
            ENDIF
            GEO(2,1)=0.D0
            GEO(3,1)=0.D0
            GEO(1,1)=0.D0
            GEO(2,2)=0.D0
            GEO(3,2)=0.D0
            GEO(3,3)=0.D0
            IVAR=1
            NA(1)=0
            IF (DOPRNT) WRITE(6,'(A)')KEYWRD,KOMENT,TITLE               CIO
            DO 100 I=1,NATOMS
               DO 80 J=1,3
   80          IEL1(J)=0
   90          CONTINUE
               IF(LOC(1,IVAR).EQ.I) THEN
                  IEL1(LOC(2,IVAR))=1
                  IVAR=IVAR+1
                  GOTO 90
               ENDIF
               IF(I.LT.4) THEN
                  IEL1(3)=0
                  IF(I.LT.3) THEN
                     IEL1(2)=0
                     IF(I.LT.2) THEN
                        IEL1(1)=0
                     ENDIF
                  ENDIF
               ENDIF
               IF(I.EQ.LATOM)IEL1(LPARAM)=-1
               Q(1)=GEO(1,I)
               Q(2)=GEO(2,I)*DEGREE
               Q(3)=GEO(3,I)*DEGREE
  100       IF (DOPRNT) WRITE(6,'(2X,A2,3(F12.6,I3),I4,2I3)')           CIO
     &         ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I) CIO
            I=0
            X=0.D0
            IF (DOPRNT) WRITE(6,'(I4,3(F12.6,I3),I4,2I3)')              CIO
     1I,X,I,X,I,X,I,I,I,I
            IF(NDEP.NE.0)THEN
               DO 110 I=1,NDEP
  110          IF (DOPRNT) WRITE(6,'(3(I4,'',''))')                     CIO
     &                                LOCPAR(I),IDEPFN(I),LOCDEP(I)     CIO
               IF (DOPRNT) WRITE(6,*)                                   CIO
            ENDIF
         ENDIF
         IF (DOPRNT) WRITE(IR)MDFP,XDFP,TOTIME,FUNCT1                   CIO
         IF (DOPRNT) WRITE(IR)(XPARAM(I),I=1,NVAR),(GD(I),I=1,NVAR)     CIO
         IF (DOPRNT) WRITE(IR)(XLAST(I),I=1,NVAR),(GRAD(I),I=1,NVAR)    CIO
         LINEAR=(NVAR*(NVAR+1))/2
         IF (DOPRNT) WRITE(IR)(HESINV(I),I=1,LINEAR)                    CIO
         LINEAR=(NORBS*(NORBS+1))/2
         IF (DOPRNT) WRITE(10)(PA(I),I=1,LINEAR)                        CIO
         IF((NALPHA.NE.0).AND.DOPRNT) WRITE(10)(PB(I),I=1,LINEAR)       CIO
         IF(LATOM .NE. 0) THEN
            IF (DOPRNT) WRITE(IR)((ALPARM(J,I),J=1,3),I=1,NVAR)         CIO
            IF (DOPRNT) WRITE(IR)ILOOP,X0, X1, X2                       CIO
         ENDIF
         IF (DOPRNT) WRITE(IR)(ERRFN(I),I=1,NVAR)                       CIO
         IF (DOPRNT) CLOSE (9)                                          CIO
         IF (DOPRNT) CLOSE (10)                                         CIO
      ELSE
         IF (ICALCN .NE. NUMCAL) THEN
           IF(DOPRNT) WRITE(6,'(//10X,'' RESTORING DATA FROM DISK''/)') CIO
         END IF
         IF(DOPRNT) READ(IR,END=130,ERR=130)MDFP,XDFP,TOTIME,FUNCT1     CIO
         IF (ICALCN .NE. NUMCAL) THEN
           IF (DOPRNT) WRITE(6,'(10X,''FUNCTION ='',F13.6//)')FUNCT1    CIO
         END IF
         IF(DOPRNT) READ(IR)(XPARAM(I),I=1,NVAR),(GD(I),I=1,NVAR)       CIO
         IF(DOPRNT) READ(IR)(XLAST(I),I=1,NVAR),(GRAD(I),I=1,NVAR)      CIO
         LINEAR=(NVAR*(NVAR+1))/2
         IF(DOPRNT) READ(IR)(HESINV(I),I=1,LINEAR)                      CIO
         LINEAR=(NORBS*(NORBS+1))/2
         IF(DOPRNT) READ(10)(PA(I),I=1,LINEAR)                          CIO
         IF((NALPHA.NE.0).AND.DOPRNT) READ(10)(PB(I),I=1,LINEAR)        CIO
         IF(LATOM.NE.0) THEN
            IF(DOPRNT) READ(IR)((ALPARM(J,I),J=1,3),I=1,NVAR)           CIO
            IF(DOPRNT) READ(IR)ILOOP,X0, X1, X2                         CIO
         ENDIF
         IF(DOPRNT) READ(IR)(ERRFN(I),I=1,NVAR)                         CIO
  120    ICALCN = NUMCAL
         RETURN
  130    IF(DOPRNT) WRITE(6,'(//10X,''NO RESTART FILE EXISTS!'')')      CIO
      RETURN 1                                                          CSTP (stop)
      ENDIF
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY DFPSAV_INIT                                                 CSAV
                 I = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
             COORD = 0.0D0                                              CSAV
              IEL1 = 0                                                  CSAV
                 Q = 0.0D0                                              CSAV
            INTXYZ = .FALSE.                                            CSAV
                IR = 0                                                  CSAV
              IVAR = 0                                                  CSAV
                 J = 0                                                  CSAV
            LINEAR = 0                                                  CSAV
                 X = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
