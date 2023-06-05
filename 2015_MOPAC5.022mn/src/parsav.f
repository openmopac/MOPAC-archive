      SUBROUTINE PARSAV(MODE,N,M,*)                                     CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
**********************************************************************
*
*   PARSAV SAVES AND RESTORES DATA USED IN NLLSQ GRADIENT MINIMIZATION.
*
*    IF MODE IS 0 DATA ARE RESTORED, IF 1 THEN SAVED.
*
**********************************************************************
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /ALPARM/ ALPARM(3,MAXPAR),X0, X1, X2, ILOOP
      COMMON /ELEMTS/ ELEMNT(120)
      COMMON /KEYWRD/ KEYWRD
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),
     1                     LOCDEP(MAXPAR)
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOM  / GEO(3,NUMATM)
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /NLLCOM/ DDDUM(6),EFSLST(MAXPAR),Q(MAXPAR,MAXPAR),
     1                R(MAXPAR,MAXPAR),XLAST(MAXPAR),
     2                IIIUM(7),IDUMY(2*MAXPAR*MAXPAR-19-MAXPAR*4)
      COMMON /GEOVAR/ XDUMMY(MAXPAR),NVAR,LOC(2,MAXPAR)                 IR0394
      COMMON /LOCVAR/ LOCVAR(2,MAXPAR)
      COMMON /VALVAR/ VALVAR(MAXPAR),NUMVAR
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION IEL1(3),QQ(3), COORD(3,NUMATM)
      CHARACTER ELEMNT*2, KEYWRD*160,KOMENT*80, TITLE*80
CSAV         SAVE                                                       GL0892
      LATOM=0
      LPARAM=0
      IF (DOPRNT) THEN                                                  CIO
       OPEN(UNIT=9,FILE='FOR009',STATUS='UNKNOWN',FORM='UNFORMATTED')
       REWIND 9
       OPEN(UNIT=10,FILE='FOR010',STATUS='UNKNOWN',FORM='UNFORMATTED')
       REWIND 10
      ENDIF                                                             CIO
      IF(MODE.NE.0) GOTO 10
*
*  MODE=0: RETRIEVE DATA FROM DISK.
*
      IF (DOPRNT) THEN                                                  CIO
        READ(9,END=70,ERR=70)IIIUM,DDDUM,EFSLST,N,(XLAST(I),I=1,N),M
        READ(9)((Q(J,I),J=1,M),I=1,M)
        READ(9)((R(J,I),J=1,N),I=1,N)
        READ(9)(VALVAR(I),I=1,N)
      ENDIF                                                             CIO
      RETURN
   10 CONTINUE
      IF(MODE.EQ.1)THEN
         IF (DOPRNT) THEN                                               CIO
         WRITE(6,'(//10X,'' **** TIME UP ****'')')
         WRITE(6,'(//10X,'' CURRENT VALUES OF GEOMETRIC VARIABLES'',//)'
     1)
         ENDIF                                                          CIO
         IF(NA(1) .EQ. 99) THEN
C
C  CONVERT FROM CARTESIAN COORDINATES TO INTERNAL
C
            DO 20 I=1,NATOMS
               DO 20 J=1,3
   20       COORD(J,I)=GEO(J,I)
            CALL XYZINT(COORD,NUMAT,NA,NB,NC,1.D0,GEO,*9999)             CSTP(call)
         ENDIF
         DEGREE=57.29577951D0
         GEO(2,1)=0.D0
         GEO(3,1)=0.D0
         GEO(1,1)=0.D0
         GEO(2,2)=0.D0
         GEO(3,2)=0.D0
         GEO(3,3)=0.D0
         IVAR=1
         NA(1)=0
         IF (DOPRNT) WRITE(6,'(A)')KEYWRD,KOMENT,TITLE                  CIO
         DO 50 I=1,NATOMS
            DO 30 J=1,3
   30       IEL1(J)=0
   40       CONTINUE
            IF(LOC(1,IVAR).EQ.I) THEN
               IEL1(LOC(2,IVAR))=1
               IVAR=IVAR+1
               GOTO 40
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
            QQ(1)=GEO(1,I)
            QQ(2)=GEO(2,I)*DEGREE
            QQ(3)=GEO(3,I)*DEGREE
   50    IF (DOPRNT) WRITE(6,'(2X,A2,3(F12.6,I3),I4,2I3)')              CIO
     1    ELEMNT(LABELS(I)),(QQ(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I)
         I=0
         X=0.D0
         IF (DOPRNT) WRITE(6,'(I4,3(F12.6,I3),I4,2I3)')                 CIO
     1    I,X,I,X,I,X,I,I,I,I
         IF(NDEP.NE.0)THEN
            DO 60 I=1,NDEP
   60       IF (DOPRNT) WRITE(6,'(3(I4,'',''))')LOCPAR(I),IDEPFN(I),    CIO
     &                                          LOCDEP(I)               CIO
            IF (DOPRNT) WRITE(6,*)                                      CIO
         ENDIF
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     1''TO RESTART CALCULATION USE THE KEYWORD "RESTART".'')')
      ENDIF
      IF(DOPRNT) THEN                                                   CIO
        WRITE(9)IIIUM,DDDUM,EFSLST,N,(XLAST(I),I=1,N),M
        WRITE(9)((Q(J,I),J=1,M),I=1,M)
        WRITE(9)((R(J,I),J=1,N),I=1,N)
        WRITE(9)(VALVAR(I),I=1,N)
      ENDIF                                                             CIO
C*****
C     The density matrix is required by ITER upon restart .
C
      LINEAR=(NORBS*(NORBS+1))/2
      IF (DOPRNT) WRITE(10)(PA(I),I=1,LINEAR)                           CIO
      IF(DOPRNT.AND.NALPHA.NE.0)WRITE(10)(PB(I),I=1,LINEAR)             CIO
C*****
      IF (DOPRNT) CLOSE(9)                                              CIO
      IF (DOPRNT) CLOSE(10)                                             CIO
      RETURN
   70 IF (DOPRNT) WRITE(6,'(//10X,''NO RESTART FILE EXISTS!'')')        CIO
      RETURN 1                                                          CSTP (stop)
 9999 RETURN 1                                                          CSTP
      END
