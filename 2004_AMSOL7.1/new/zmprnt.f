      SUBROUTINE ZMPRNT(IWRITE,FUNCT)                                   DJG0995
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
C***********************************************************************
C
C     THIS SUBROUTINE WRITES A Z-MATRIX TO A FILE
C
C     Q = ATOMIC COORDINATES (EITHER INTERNAL OR CARTESIAN
C     IEL = OPTIMIZATION FLAGS
C     QCHARG = PARTIAL ATOMIC CHARGES
C     IVSZ = # OF SHELLS TO ENCLOSE MOLECULE
C     ITYPE = SM1A ATOM TYPES
C     LOCPAR = SYMMETRY REFERNCE ATOM
C     LOCDEP = SYMMETRY DEPENDENT ATOM
C     IDEPFN = TYPE OF SYMMETRY DEPENDENCE
C
C     CREATED FROM THE SUBROUTINE RESINP, DJG0995
C
C***********************************************************************
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),               DJG0195
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)           DJG0195
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOVAR/ DUMY(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY             3GL3092
      COMMON /ASOLCM/ GASENG                                            GDH1194
      COMMON /REPATH/ REACT(100),LATOM,LPARAM                           03GCL93
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),
     1                     LOCDEP(MAXPAR)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /CMCOM/  ECMCG(NUMATM)                                     DJG0995
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)                    DJG0995
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /IVSZCM/ IVSZ(NUMATM), IVSZC                               DJG0995
      COMMON /CORE  / CORE(107)
      COMMON /MODLCM/ SMODEL                                            DJG0496
      COMMON /AREACM/ NOPTI, TAREA, NINTEG                              DJG0496
      CHARACTER ELEMNT*2                                                DJG0995
      LOGICAL XKWFLG, SMODEL, TAREA                                     DJG0496
      DIMENSION IEL1(3),Q(3),COORD(3,NUMATM),                           DJG0995
     1          QCHARG(NUMATM),QDENS(NUMATM)                            DJG0995
      SAVE
      XKWFLG=.FALSE.                                                    DJG0995
      NTPRT=1                                                           GDH1195
      CALL ECHOWD(IWRITE,NTPRT,XKWFLG)                                  GDH1195
      DEGREE=57.29577951D0
      NA1=NA(1)                                                         DJG0995
      IF(NA(1) .EQ. 99) THEN
C
C  CONVERT FROM CARTESIAN COORDINATES TO INTERNAL
C
         DO 70 I=1,NATOMS
            DO 70 J=1,3
   70    COORD(J,I)=GEO(J,I)
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
      IF (IEXTCM.NE.0) THEN                                             DJG0995
         INCREM=1
         CALL SCOPY(NUMAT,ECMCG,INCREM,QCHARG,INCREM)                   DJG0995
      ELSE IF (ICHMOD.EQ.1) THEN                                        DJG0995
         CALL CHGMP1(P,QCHARG)                                          DL0397
         DO 50 I=1,NUMAT                                                DJG0995
            QDENS(I)=CORE(NAT(I))-QCHARG(I)                             DL0397
50       CONTINUE                                                       DJG0995
      ELSE IF (ICHMOD.EQ.2) THEN                                        GDH0997
         CALL CHGMP2(P,QCHARG)                                          GDH0997
         DO 51 I=1,NUMAT                                                GDH0997
            QDENS(I)=CORE(NAT(I))-QCHARG(I)                             GDH0997
51       CONTINUE                                                       GDH0997
      ELSE                                                              GDH0997
         CALL CHRGE(P,QDENS)                                            GDH0997
         DO 60 I=1,NUMAT                                                DJG0995
            QCHARG(I)=CORE(NAT(I))-QDENS(I)                             DJG0995
60       CONTINUE                                                       DJG0995
      ENDIF                                                             DJG0995
      L=0                                                               DJG0995
      DO 100 I=1,NATOMS
         DO 80 J=1,3
   80    IEL1(J)=0
   90    CONTINUE
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
C        IF(IXYZ.EQ.0) THEN                                             DJG0995
C         IF(ICART.EQ.0) THEN                                            DJG1196
            Q(1)=GEO(1,I)
            Q(2)=GEO(2,I)*DEGREE
            Q(3)=GEO(3,I)*DEGREE
C         ELSE                                                           DJG0995
C            Q(1)=COORD(1,I)                                             DJG0995
C            Q(2)=COORD(2,I)                                             DJG0995
C            Q(3)=COORD(3,I)                                             DJG0995
C         ENDIF                                                          DJG0995
         IF(LABELS(I).LT.99)THEN                                        DJG0995
            L=L+1                                                       GDH0596
C           IF (IXYZ.EQ.1) THEN                                         GDH0496
C            IF (ICART.EQ.1) THEN                                        DJG1196
C            IF (ISM50.EQ.1) THEN                                        GDH0497
C               WRITE(IWRITE,'(2X,A2,3(F12.6,I5),I4,F13.4)')             GDH0497
C     1         ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),                  GDH0497
C     2         IVSZ(L),0.00D0                                           GDH0497
C            ELSE IF (SMODEL.AND.NINTEG.LT.2) THEN                       GDH0597
C               WRITE(IWRITE,'(2X,A2,3(F12.6,I5),I4,F13.4)')             GDH0496
C     1         ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),                  GDH0496
C     2         IVSZ(L),QCHARG(L)                                        GDH0496
C            ELSE                                                        GDH0496
C               WRITE(IWRITE,'(2X,A2,3(F12.6,I5),F13.4)')                GDH0496
C     1         ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),                  GDH0496
C     2         QCHARG(L)                                                GDH0496
C            ENDIF                                                       GDH0496
C            ELSE                                                        GDH0496
            IF (ISM50.EQ.1) THEN                                        GDH0497
               WRITE(IWRITE,'(2X,A2,3(F12.6,I5),I4,2I5,I4,F13.4)')      GDH0496
     1         ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I),GDH0496
     2         IVSZ(L),0.00D0                                           GDH0496
            ELSE IF (SMODEL.AND.NINTEG.LT.2) THEN                       GDH0497
               WRITE(IWRITE,'(2X,A2,3(F12.6,I5),I4,2I5,I4,F13.4)')      GDH0496
     1         ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I),GDH0496
     2         IVSZ(L),QCHARG(L)                                        GDH0496
            ELSE                                                        GDH0496
               WRITE(IWRITE,'(2X,A2,3(F12.6,I5),I4,2I5,F13.4)')         GDH0496
     1         ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I),GDH0496
     2         QCHARG(L)                                                DJG0995
            ENDIF                                                       DJG0995
C            ENDIF                                                       GDH0496
         ELSE                                                           DJG0995
            WRITE(IWRITE,'(2X,A2,3(F12.6,I5),I4,2I5,F13.4)')            DJG0995
     1        ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I)  DJG0995
         ENDIF                                                          DJG0995
  100 CONTINUE                                                          DJG0995
      NA(1)=NA1                                                         DJG0995
      I=0
      X=0.D0
      WRITE(IWRITE,'(I4,3(F12.6,I5),I4,2I5)')                           GDH1194
     1   I,X,I,X,I,X,I,I,I,I
      IF(NDEP.NE.0)THEN                                                 DJG0995
        DO 230 I=1,NDEP
          IF (I.NE.NDEP) THEN                                           DJG0995
             WRITE(IWRITE,'(3(I4,'',''))')LOCPAR(I),IDEPFN(I),LOCDEP(I) DJG0995
          ELSE                                                          DJG0995
             WRITE(IWRITE,'(3(I4,'','')/)')LOCPAR(I),IDEPFN(I),LOCDEP(I)DJG0995
          ENDIF                                                         DJG0995
230     CONTINUE
      ENDIF                                                             DJG0995
      IF(ISM1A.GT.0) THEN                                               GDH0597
      DO 152 IENV=1,NUMAT                                               GDH0493
152   WRITE(IWRITE,'(I2)')ITYPE(IENV)                                   GDH0893
      WRITE(IWRITE,'(/)')                                               GL0592
      ENDIF                                                             GDH0893
      WRITE(IWRITE,'(/)')                                               GL0592
      END                                                               DJG0395
