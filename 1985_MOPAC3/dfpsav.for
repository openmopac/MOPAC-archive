      SUBROUTINE DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION XPARAM(*), GD(*), XLAST(*), MDFP(9),XDFP(9)
**********************************************************************
*
* DFPSAV STORES AND RESTORES DATA USED IN THE D-F-P GEOMETRY
*        OPTIMISATION.
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
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, DUMY(MAXPAR)
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /ALPARM/ ALPARM(3,MAXPAR),X0, X1, X2, ILOOP
      COMMON /REACTN/ STEP, GEOA(3,NUMATM), GEOVEC(3,NUMATM),CALCST
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /PATH  / LATOM,LPARAM,REACT(100)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /FMATRX/ HESINV(MAXHES)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),
     1                     LOCDEP(MAXPAR)
      COMMON /ERRFN / ERRFN(MAXPAR)
      DIMENSION IEL1(3),Q(3), COORD(3,NUMATM)
      CHARACTER ELEMNT*2, KEYWRD*80,KOMENT*80, TITLE*80
      LOGICAL FIRST, INTXYZ
      DATA FIRST /.TRUE./
      OPEN(UNIT=9,FILE='FOR009',STATUS='UNKNOWN',FORM='UNFORMATTED')
      REWIND 9
      OPEN(UNIT=10,FILE='FOR010',STATUS='UNKNOWN',FORM='UNFORMATTED')
      REWIND 10
      DEGREE=57.29577951D0
      IR=9
      IF(MDFP(9) .EQ. 1) THEN
         WRITE(6,'(//10X,''- - - - - - - TIME UP - - - - - - -'',//)')
         IF(INDEX(KEYWRD,'SADDLE') .NE. 0) THEN
            WRITE(6,'(//10X,'' NO RESTART EXISTS FOR SADDLE'',//
     1  10X,'' HERE IS A DATA-FILE FILES THAT MIGHT BE SUITABLE'',/
     2  10X,'' FOR RESTARTING THE CALCULATION'',///)')
            WRITE(6,'(1X,A)')KEYWRD,KOMENT,TITLE
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
   10             COORD(J,I)=GEO(J,I)
               ELSE
                  CALL XYZINT(GEO,NUMAT,NA,NB,NC,1.D0,COORD)
               ENDIF
               IVAR=1
               NA(1)=0
               DO 40 I=1,NATOMS
                  DO 20 J=1,3
   20             IEL1(J)=0
   30             CONTINUE
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
   40          WRITE(6,'(2X,A2,3(F12.6,I3),I4,2I3)')
     1    ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I)
               I=0
               X=0.D0
               WRITE(6,'(I4,3(F12.6,I3),I4,2I3)')
     1    I,X,I,X,I,X,I,I,I,I
               DO 50 I=1,NATOMS
                  DO 50 J=1,3
   50          GEO(J,I)=GEOA(J,I)
               NA(1)=99
   60       CONTINUE
            WRITE(6,'(///10X,''CALCULATION TERMINATED HERE'')')
            STOP
         ENDIF
         WRITE(6,'(//10X,'' - THE CALCULATION IS BEING DUMPED TO DISK'',
     1  /10X,''   RESTART IT USING THE MAGIC WORD "RESTART"'')')
         WRITE(6,'(//10X,''CURRENT VALUE OF HEAT OF FORMATION =''
     1  ,F12.6)')FUNCT1
         IF(NA(1) .EQ. 99) THEN
C
C  CONVERT FROM CARTESIAN COORDINATES TO INTERNAL
C
            DO 70 I=1,NATOMS
               DO 70 J=1,3
   70       COORD(J,I)=GEO(J,I)
            CALL XYZINT(COORD,NUMAT,NA,NB,NC,1.D0,GEO)
         ENDIF
         GEO(2,1)=0.D0
         GEO(3,1)=0.D0
         GEO(1,1)=0.D0
         GEO(2,2)=0.D0
         GEO(3,2)=0.D0
         GEO(3,3)=0.D0
         IVAR=1
         NA(1)=0
         WRITE(6,'(A)')KEYWRD,KOMENT,TITLE
         DO 100 I=1,NATOMS
            DO 80 J=1,3
   80       IEL1(J)=0
   90       CONTINUE
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
  100    WRITE(6,'(2X,A2,3(F12.6,I3),I4,2I3)')
     1ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I)
         I=0
         X=0.D0
         WRITE(6,'(I4,3(F12.6,I3),I4,2I3)')
     1I,X,I,X,I,X,I,I,I,I
         IF(NDEP.NE.0)THEN
            DO 110 I=1,NDEP
  110       WRITE(6,'(3(I4,'',''))')LOCPAR(I),IDEPFN(I),LOCDEP(I)
            WRITE(6,*)
         ENDIF
         WRITE(IR)MDFP,XDFP,TOTIME,FUNCT1
         WRITE(IR)(XPARAM(I),I=1,NVAR),(GD(I),I=1,NVAR)
         WRITE(IR)(XLAST(I),I=1,NVAR),(GRAD(I),I=1,NVAR)
         LINEAR=(NVAR*(NVAR+1))/2
         WRITE(IR)(HESINV(I),I=1,LINEAR)
         LINEAR=(NORBS*(NORBS+1))/2
         WRITE(10)(PA(I),I=1,LINEAR)
         IF(NALPHA.NE.0)WRITE(10)(PB(I),I=1,LINEAR)
         IF(LATOM .NE. 0) THEN
            WRITE(IR)((ALPARM(J,I),J=1,3),I=1,NVAR)
            WRITE(IR)ILOOP,X0, X1, X2
         ENDIF
         WRITE(IR)(ERRFN(I),I=1,NVAR)
         STOP
      ELSE
         IF (FIRST) WRITE(6,'(//10X,'' RESTORING DATA FROM DISK''/)')
         READ(IR)MDFP,XDFP,TOTIME,FUNCT1
         IF (FIRST) WRITE(6,'(10X,''FUNCTION ='',F13.6//)')FUNCT1
         READ(IR)(XPARAM(I),I=1,NVAR),(GD(I),I=1,NVAR)
         READ(IR)(XLAST(I),I=1,NVAR),(GRAD(I),I=1,NVAR)
         LINEAR=(NVAR*(NVAR+1))/2
         READ(IR)(HESINV(I),I=1,LINEAR)
         LINEAR=(NORBS*(NORBS+1))/2
         READ(10)(PA(I),I=1,LINEAR)
         IF(NALPHA.NE.0)READ(10)(PB(I),I=1,LINEAR)
         IF(LATOM.NE.0) THEN
            READ(IR)((ALPARM(J,I),J=1,3),I=1,NVAR)
            READ(IR)ILOOP,X0, X1, X2
         ENDIF
         READ(IR)(ERRFN(I),I=1,NVAR)
  120    FIRST=.FALSE.
         RETURN
      ENDIF
      END
