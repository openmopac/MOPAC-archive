      SUBROUTINE POWSAV(HESS, GRAD, XPARAM, PMAT, ILOOP, BMAT, IPOW,*)  CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION HESS(MAXPAR,*),GRAD(*),BMAT(MAXPAR,*),IPOW(9),
     1 XPARAM(*), PMAT(*)
**********************************************************************
*
* POWSAV STORES AND RESTORES DATA USED IN THE SIGMA GEOMETRY
*        OPTIMIZATION.
*
*  ON INPUT HESS   = HESSIAN MATRIX, PARTIAL OR WHOLE.
*           GRAD   = GRADIENTS.
*           XPARAM = CURRENT STATE OF PARAMETERS.
*           ILOOP  = INDEX OF HESSIAN, OR FLAG OF POINT REACHED SO-FAR.
*           BMAT   = "B" MATRIX!
*           IPOW   = INDICES AND FLAGS.
*           IPOW(9)= 0 FOR RESTORE, 1 FOR DUMP
*
**********************************************************************
      COMMON /GEOVAR/ XDUMMY(MAXPAR),NVAR,LOC(2,MAXPAR)                 IR0394
      COMMON /ELEMTS/ ELEMNT(120)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /LOCVAR/ LOCVAR(2,MAXPAR)
      COMMON /KEYWRD/ KEYWRD
      COMMON /VALVAR/ VALVAR(MAXPAR),NUMVAR
      DIMENSION IEL1(3),QQ(3), COORD(3,NUMATM)
      CHARACTER ELEMNT*2, KEYWRD*160,KOMENT*80, TITLE*80
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /ALPARM/ ALPARM(3,MAXPAR),X0, X1, X2, JLOOP
C Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
     3       /MOLKSR/ FRACT
C    changed common path for portability  (IR)
      COMMON /PATHI / LATOM,LPARAM
      COMMON /PATHR / REACT(200)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
CSAV         SAVE                                                           GL0892
      IF(DOPRNT) OPEN(UNIT=9,FILE='FOR009',STATUS='UNKNOWN',            CIO
     &                                     FORM='UNFORMATTED')          CIO
      IF(DOPRNT) REWIND 9                                               CIO
      IF(DOPRNT) OPEN(UNIT=10,FILE='FOR010',STATUS='UNKNOWN',           CIO
     &                                      FORM='UNFORMATTED')         CIO
      IF(DOPRNT) REWIND 10                                              CIO
      IR=9
      IF(IPOW(9) .NE. 0) THEN
         IF(IPOW(9) .EQ. 1) THEN
            IF (DOPRNT) THEN                                            CIO
            WRITE(6,'(//10X,''- - - - - - - TIME UP - - - - - - -'',//)'
     1)
            WRITE(6,'(//10X,'' - THE CALCULATION IS BEING DUMPED TO DISK
     1'',/10X,''   RESTART IT USING THE KEY-WORD "RESTART"'')')
            ENDIF                                                       CIO
            FUNCT1=SQRT(ddot(nvar,grad,1,grad,1))
            IF (DOPRNT) WRITE(6,'(//10X,                                CIO
     &              ''CURRENT VALUE OF GRADIENT NORM ='',F12.6)')FUNCT1 CIO
            DO 10 I=1,NVAR
               K=LOC(1,I)
               L=LOC(2,I)
   10       GEO(L,K)=XPARAM(I)
            IF(DOPRNT)WRITE(6,'(/10X,''CURRENT VALUE OF GEOMETRY'',/)') CIO
            DEGREE=57.29577951D0
            IF(NA(1) .EQ. 99) THEN
C
C  CONVERT FROM CARTESIAN COORDINATES TO INTERNAL
C
               DO 20 I=1,NATOMS
                  DO 20 J=1,3
   20          COORD(J,I)=GEO(J,I)
               CALL XYZINT(COORD,NUMAT,NA,NB,NC,1.D0,GEO,*9999)         CSTP(call)
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
            DO 50 I=1,NATOMS
               DO 30 J=1,3
   30          IEL1(J)=0
   40          CONTINUE
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
               IF(I.EQ.LATOM)IEL1(LPARAM)=-1
               QQ(1)=GEO(1,I)
               QQ(2)=GEO(2,I)*DEGREE
               QQ(3)=GEO(3,I)*DEGREE
   50       IF (DOPRNT) WRITE(6,'(2X,A2,3(F12.6,I3),I4,2I3)')           CIO
     1    ELEMNT(LABELS(I)),(QQ(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I)
            I=0
            X=0.D0
            IF (DOPRNT) WRITE(6,'(I4,3(F12.6,I3),I4,2I3)')              CIO
     1    I,X,I,X,I,X,I,I,I,I
            IF(DOPRNT.AND.NDEP.NE.0)THEN
               DO 60 I=1,NDEP
   60          IF (DOPRNT) WRITE(6,'(3(I4,'',''))')LOCPAR(I),IDEPFN(I),LOCDEP(I)
               IF (DOPRNT) WRITE(6,*)
            ENDIF
         ENDIF
         IF (DOPRNT) THEN                                               CIO
           WRITE(IR)IPOW,ILOOP
           WRITE(IR)(XPARAM(I),I=1,NVAR)
           WRITE(IR)(  GRAD(I),I=1,NVAR)
           WRITE(IR)((HESS(J,I),J=1,NVAR),I=1,NVAR)
           WRITE(IR)((BMAT(J,I),J=1,NVAR),I=1,NVAR)
         ENDIF                                                          CIO
         LINEAR=(NVAR*(NVAR+1))/2
         IF (DOPRNT) WRITE(IR)(PMAT(I),I=1,LINEAR)                      CIO
         LINEAR=(NORBS*(NORBS+1))/2
         IF (DOPRNT) WRITE(10)(PA(I),I=1,LINEAR)                        CIO
         IF(DOPRNT.AND.NALPHA.NE.0)WRITE(10)(PB(I),I=1,LINEAR)          CIO
         IF(LATOM .NE. 0) THEN
            IF (DOPRNT) WRITE(IR)((ALPARM(J,I),J=1,3),I=1,NVAR)         CIO
            IF (DOPRNT) WRITE(IR)JLOOP,X0, X1, X2                       CIO
         ENDIF
         IF(DOPRNT) CLOSE (9)                                           CIO
         IF(DOPRNT) CLOSE (10)                                          CIO
         RETURN
      ELSE
         IF(DOPRNT) WRITE(6,'(//10X,'' RESTORING DATA FROM DISK''/)')   CIO
         IF(DOPRNT) READ(IR)IPOW,ILOOP                                  CIO
         IF(DOPRNT) READ(IR)(XPARAM(I),I=1,NVAR)                        CIO
         IF(DOPRNT) READ(IR)(  GRAD(I),I=1,NVAR)                        CIO
         IF(DOPRNT) READ(IR)((HESS(J,I),J=1,NVAR),I=1,NVAR)             CIO
         IF(DOPRNT) READ(IR)((BMAT(J,I),J=1,NVAR),I=1,NVAR)             CIO
         FUNCT1=SQRT(ddot(nvar,grad,1,grad,1))
         IF(DOPRNT) WRITE(6,'(10X,''FUNCTION ='',F13.6//)')FUNCT1       CIO
         LINEAR=(NVAR*(NVAR+1))/2
         IF(DOPRNT) READ(IR)(PMAT(I),I=1,LINEAR)                        CIO
         LINEAR=(NORBS*(NORBS+1))/2
         IF(DOPRNT) READ(10)(PA(I),I=1,LINEAR)                          CIO
         IF(DOPRNT.AND.NALPHA.NE.0)READ(10)(PB(I),I=1,LINEAR)           CIO
         IF(LATOM.NE.0) THEN
            IF(DOPRNT) READ(IR)((ALPARM(J,I),J=1,3),I=1,NVAR)           CIO
            IF(DOPRNT) READ(IR)JLOOP,X0, X1, X2                         CIO
            ILOOP=ILOOP+1
         ENDIF
         ILOOP=ILOOP+1
         RETURN
      ENDIF
      RETURN
 9999 RETURN 1                                                          CSTP
      END
