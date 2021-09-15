      SUBROUTINE POWSAV(HESS, GRAD, XPARAM, PMAT, ILOOP, BMAT, IPOW)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION HESS(MAXPAR,*),GRAD(*),BMAT(MAXPAR,*),IPOW(9),
     1 XPARAM(*), PMAT(*)
**********************************************************************
*
* POWSAV STORES AND RESTORES DATA USED IN THE SIGMA GEOMETRY
*        OPTIMISATION.
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
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, DUMY(MAXPAR)
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),
     1                     LOCDEP(MAXPAR)
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /LOCVAR/ LOCVAR(2,MAXPAR)
      COMMON /KEYWRD/ KEYWRD
      COMMON /VALVAR/ VALVAR(MAXPAR),NUMVAR
      DIMENSION IEL1(3),QQ(3)
      CHARACTER ELEMNT*2, KEYWRD*80,KOMENT*80, TITLE*80
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /ALPARM/ ALPARM(3,MAXPAR),X0, X1, X2, JLOOP
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /PATH  / LATOM,LPARAM,REACT(100)
      OPEN(UNIT=9,FILE='FOR009',STATUS='UNKNOWN',FORM='UNFORMATTED')
      REWIND 9
      OPEN(UNIT=10,FILE='FOR010',STATUS='UNKNOWN',FORM='UNFORMATTED')
      REWIND 10
      IR=9
      IF(IPOW(9) .EQ. 1) THEN
         WRITE(6,'(//10X,''- - - - - - - TIME UP - - - - - - -'',//)')
         WRITE(6,'(//10X,'' - THE CALCULATION IS BEING DUMPED TO DISK''
     1,/10X,''   RESTART IT USING THE MAGIC WORD "RESTART"'')')
         FUNCT1=SQRT(DOT(GRAD,GRAD,NVAR))
         WRITE(6,'(//10X,''CURRENT VALUE OF GRADIENT NORM =''
     1  ,F12.6)')FUNCT1
         WRITE(6,'(/10X,''CURRENT VALUE OF GEOMETRY'',/)')
         DEGREE=57.29577951D0
         GEO(2,1)=0.D0
         GEO(3,1)=0.D0
         GEO(1,1)=0.D0
         GEO(2,2)=0.D0
         GEO(3,2)=0.D0
         GEO(3,3)=0.D0
         IVAR=1
         NA(1)=0
         WRITE(6,'(1X,A)')KEYWRD,KOMENT,TITLE
         DO 30 I=1,NATOMS
            DO 10 J=1,3
   10       IEL1(J)=0
   20       CONTINUE
            IF(LOC(1,IVAR).EQ.I) THEN
               IEL1(LOC(2,IVAR))=1
               IVAR=IVAR+1
               GOTO 20
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
   30    WRITE(6,'(2X,A2,3(F12.6,I3),I4,2I3)')
     1    ELEMNT(LABELS(I)),(QQ(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I)
         I=0
         X=0.D0
         WRITE(6,'(I4,3(F12.6,I3),I4,2I3)')
     1    I,X,I,X,I,X,I,I,I,I
         IF(NDEP.NE.0)THEN
            DO 40 I=1,NDEP
   40       WRITE(6,'(3(I4,'',''))')LOCPAR(I),IDEPFN(I),LOCDEP(I)
            WRITE(6,*)
         ENDIF
         WRITE(IR)IPOW,ILOOP
         WRITE(IR)(XPARAM(I),I=1,NVAR)
         WRITE(IR)(  GRAD(I),I=1,NVAR)
         WRITE(IR)((HESS(J,I),J=1,NVAR),I=1,NVAR)
         WRITE(IR)((BMAT(J,I),J=1,NVAR),I=1,NVAR)
         LINEAR=(NVAR*(NVAR+1))/2
         WRITE(IR)(PMAT(I),I=1,LINEAR)
         LINEAR=(NORBS*(NORBS+1))/2
         WRITE(10)(PA(I),I=1,LINEAR)
         IF(NALPHA.NE.0)WRITE(10)(PB(I),I=1,LINEAR)
         IF(LATOM .NE. 0) THEN
            WRITE(IR)((ALPARM(J,I),J=1,3),I=1,NVAR)
            WRITE(IR)JLOOP,X0, X1, X2
         ENDIF
         STOP
      ELSE
         WRITE(6,'(//10X,'' RESTORING DATA FROM DISK''/)')
         READ(IR)IPOW,ILOOP
         READ(IR)(XPARAM(I),I=1,NVAR)
         READ(IR)(  GRAD(I),I=1,NVAR)
         READ(IR)((HESS(J,I),J=1,NVAR),I=1,NVAR)
         READ(IR)((BMAT(J,I),J=1,NVAR),I=1,NVAR)
         FUNCT1=SQRT(DOT(GRAD,GRAD,NVAR))
         WRITE(6,'(10X,''FUNCTION ='',F13.6//)')FUNCT1
         LINEAR=(NVAR*(NVAR+1))/2
         READ(IR)(PMAT(I),I=1,LINEAR)
         LINEAR=(NORBS*(NORBS+1))/2
         READ(10)(PA(I),I=1,LINEAR)
         IF(NALPHA.NE.0)READ(10)(PB(I),I=1,LINEAR)
         IF(LATOM.NE.0) THEN
            READ(IR)((ALPARM(J,I),J=1,3),I=1,NVAR)
            READ(IR)JLOOP,X0, X1, X2
            ILOOP=ILOOP+1
         ENDIF
         ILOOP=ILOOP+1
         RETURN
      ENDIF
      END
