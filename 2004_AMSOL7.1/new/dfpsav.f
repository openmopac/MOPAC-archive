      SUBROUTINE DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
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
*           MDFP(9)= 2 FOR TEMPSTORE,1 FOR DUMP, 0 FOR RESTORE.         GDH1194
********************************************************************** 
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM 
C     COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, DUMY(MAXPAR) 
      COMMON /GEOVAR/ DUMY(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY             3GL3092
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK) 
      COMMON /ALPARM/ ALPARM(3,MAXPAR),X0, X1, X2, ILOOP 
      COMMON /REACTN/ STEP, GEOA(3,NUMATM), GEOVEC(3,NUMATM),CALCST 
      COMMON /GEOM  / GEO(3,NUMATM) 
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM), 
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM) 
      COMMON /ELEMTS/ ELEMNT(107) 
      COMMON /REPATH/ REACT(100),LATOM,LPARAM                           03GCL93
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
C     COMMON /OPTIM / HESINV(MAXHES) 
      COMMON /OPTMCR / HESINV(MAXHES),                                  3GL3092
     1                 RDUMY(MAXPAR*(3*MAXPAR+NCHAIN+16) + 29 + NCHAIN) 3GL3092
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR), 
     1                     LOCDEP(MAXPAR) 
      COMMON /ERRFN / ERRFN(MAXPAR) 
      COMMON /ASOLCM/ GASENG                                            GDH1194
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      DIMENSION XPARAM(*), GD(*), XLAST(*), MDFP(9),XDFP(9) 
      DIMENSION IEL1(3),Q(3), COORD(3,NUMATM) 
      CHARACTER ELEMNT*2                                                DJG0995
      LOGICAL FIRST, INTXYZ , DEBUG, XKWFLG, CHEQIT                     DJG0995
       SAVE
      IF (ICONTR(1).EQ.1) THEN                                          GDH0195
         ICONTR(1)=2                                                    GDH0195
         FIRST=.TRUE.                                                   GDH0195
      ENDIF                                                             GDH0195
      INQUIRE(JRES,OPENED=CHEQIT)                                       GDH1095
      IF(.NOT.CHEQIT) OPEN(JRES,STATUS='UNKNOWN',FORM='UNFORMATTED')    GDH1095
      INQUIRE(JDEN,OPENED=CHEQIT)                                       GDH1095
      IF(.NOT.CHEQIT) OPEN(JDEN,STATUS='UNKNOWN',FORM='UNFORMATTED')    GDH1095
      DEBUG=IDFPSA.NE.0                                                 DJG0995
      REWIND JRES
      REWIND JDEN
      DEGREE=57.29577951D0 
      IF(MDFP(9) .GE. 1) THEN 
         IF (MDFP(9).EQ.1)                                              GDH1194
     1   WRITE(JOUT,
     .        '(//10X,''- - - - - - - TIME UP - - - - - - -'',//)') 
         IF((ISADDL .NE. 0).AND.(MDFP(9).EQ.1)) THEN                    DJG0995
            WRITE(JOUT,'(//10X,'' NO RESTART EXISTS FOR SADDLE'',// 
     1  10X,'' HERE IS A DATA-FILE FILES THAT MIGHT BE SUITABLE'',/ 
     2  10X,'' FOR RESTARTING THE CALCULATION'',///)') 
            XKWFLG=.FALSE.                                              DJG0995
            NTPRT=1                                                     GDH1195
            CALL ECHOWD(JOUT,NTPRT,XKWFLG)                              GDH1195
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
   40          WRITE(JOUT,'(2X,A2,3(F12.6,I3),I4,2I3)') 
     1    ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I) 
               I=0 
               X=0.D0 
               WRITE(JOUT,'(I4,3(F12.6,I3),I4,2I3)') 
     1    I,X,I,X,I,X,I,I,I,I 
               DO 50 I=1,NATOMS 
                  DO 50 J=1,3 
   50          GEO(J,I)=GEOA(J,I) 
               NA(1)=99 
   60       CONTINUE 
            WRITE(JOUT,'(///10X,''CALCULATION TERMINATED HERE'')') 
      ISTOP=1                                                           GDH1095
      IWHERE=4                                                          GDH1095
      RETURN                                                            GDH1095
         ENDIF 
         IF (MDFP(9).EQ.1) THEN                                         GDH1194
         WRITE(JOUT,
     .        '(//10X,'' - THE CALCULATION IS BEING DUMPED TO DISK'',
     1     /10X,''   RESTART IT USING THE MAGIC WORD "RESTART"'')')
         WRITE(JOUT,'(//10X,''CURRENT VALUE OF HEAT OF FORMATION =''
     1     ,F12.6)')FUNCT1
         ENDIF                                                          GDH1194
C
C    RESINP PRINTS OUT A .INP FILE                                      DJG0395
C    IRESTA=-1 ADDS RESTART TO THE KEYWORD LINE                         DJG0995
C                                                                       DJG0395
         IF (IRESTA.EQ.0) IRESTA=-1                                     DJG0995
         CALL RESINP(FUNCT)                                             DJG0995
         WRITE(JRES)MDFP,XDFP,TOTIME,FUNCT1 
         WRITE(JRES)(XPARAM(I),I=1,NVAR),(GD(I),I=1,NVAR) 
         WRITE(JRES)(XLAST(I),I=1,NVAR),(GRAD(I),I=1,NVAR) 
         LINEAR=(NVAR*(NVAR+1))/2 
         WRITE(JRES)(HESINV(I),I=1,LINEAR) 
         LINEAR=(NORBS*(NORBS+1))/2 
         WRITE(JDEN)(PA(I),I=1,LINEAR) 
         IF(NALPHA.NE.0)WRITE(JDEN)(PB(I),I=1,LINEAR) 
         IF(LATOM .NE. 0) THEN 
            WRITE(JRES)((ALPARM(J,I),J=1,3),I=1,NVAR) 
            WRITE(JRES)ILOOP,X0, X1, X2 
         ENDIF 
         WRITE(JRES)(ERRFN(I),I=1,NVAR) 
         IF(DEBUG)THEN
           CALL VECPRT(PA,NORBS )
         ENDIF
         IF (MDFP(9).EQ.1) THEN                                         GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=5                                                          GDH1095
      RETURN                                                            GDH1095
         ENDIF                                                          GDH1095
         RETURN
      ELSE 
         IF (FIRST) WRITE(JOUT
     .         ,'(//10X,'' RESTORING DATA FROM DISK''/)')
         READ(JRES)MDFP,XDFP,TOTIME,FUNCT1
         IF (FIRST) WRITE(JOUT,'(10X,''FUNCTION ='',F13.6//)')FUNCT1
         READ(JRES)(XPARAM(I),I=1,NVAR),(GD(I),I=1,NVAR) 
         READ(JRES)(XLAST(I),I=1,NVAR),(GRAD(I),I=1,NVAR) 
         LINEAR=(NVAR*(NVAR+1))/2 
         READ(JRES)(HESINV(I),I=1,LINEAR) 
         LINEAR=(NORBS*(NORBS+1))/2 
         READ(JDEN)(PA(I),I=1,LINEAR) 
         IF(NALPHA.NE.0)READ(JDEN)(PB(I),I=1,LINEAR) 
         IF(LATOM.NE.0) THEN 
            READ(JRES)((ALPARM(J,I),J=1,3),I=1,NVAR) 
            READ(JRES)ILOOP,X0, X1, X2 
         ENDIF 
         READ(JRES)(ERRFN(I),I=1,NVAR) 
         IF(FIRST.AND.DEBUG)THEN
           CALL VECPRT(PA,NORBS )
         ENDIF
  120    FIRST=.FALSE.
         RETURN 
      ENDIF 
      END 
