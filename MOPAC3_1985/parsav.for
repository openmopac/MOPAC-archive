      SUBROUTINE PARSAV(MODE,N,M)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
**********************************************************************
*
*   PARSAV SAVES AND RESTORES DATA USED IN NLLSQ GRADIENT MINIMIZATION.
*
*    IF MODE IS 0 DATA ARE RESTORED, IF 1 THEN SAVED.
*
**********************************************************************
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /ALPARM/ ALPARM(3,MAXPAR),X0, X1, X2, ILOOP
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /KEYWRD/ KEYWRD
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),
     1                     LOCDEP(MAXPAR)
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /NLLCOM/ DDDUM(6),EFSLST(MAXPAR),Q(MAXPAR,MAXPAR),
     1                R(MAXPAR,MAXPAR),XLAST(MAXPAR),
     2                IIIUM(7),IDUMY(2*MAXHES-19-MAXPAR*4)
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), JDUMY, DUMY(MAXPAR)
      COMMON /LOCVAR/ LOCVAR(2,MAXPAR)
      COMMON /VALVAR/ VALVAR(MAXPAR),NUMVAR
      DIMENSION IEL1(3),QQ(3)
      CHARACTER ELEMNT*2, KEYWRD*80,KOMENT*80, TITLE*80
      OPEN(UNIT=9,FILE='FOR009',STATUS='UNKNOWN',FORM='UNFORMATTED')
      REWIND 9
      OPEN(UNIT=10,FILE='FOR010',STATUS='UNKNOWN',FORM='UNFORMATTED')
      REWIND 10
      IF(MODE.EQ.1) GOTO 10
*
*  MODE=0: RETRIEVE DATA FROM DISK.
*
      READ(9)IIIUM,DDDUM,EFSLST,N,(XLAST(I),I=1,N),M
      READ(9)((Q(J,I),J=1,M),I=1,M)
      READ(9)((R(J,I),J=1,N),I=1,N)
      READ(9)(VALVAR(I),I=1,N)
      RETURN
   10 CONTINUE
      WRITE(6,'(//10X,'' **** TIME UP ****'')')
      WRITE(6,'(//10X,'' CURRENT VALUES OF GEOMETRIC VARIABLES'',//)')
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
      DO 40 I=1,NATOMS
         DO 20 J=1,3
   20    IEL1(J)=0
   30    CONTINUE
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
         QQ(1)=GEO(1,I)
         QQ(2)=GEO(2,I)*DEGREE
         QQ(3)=GEO(3,I)*DEGREE
   40 WRITE(6,'(2X,A2,3(F12.6,I3),I4,2I3)')
     1    ELEMNT(LABELS(I)),(QQ(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I)
      I=0
      X=0.D0
      WRITE(6,'(I4,3(F12.6,I3),I4,2I3)')
     1    I,X,I,X,I,X,I,I,I,I
      IF(NDEP.NE.0)THEN
         DO 50 I=1,NDEP
   50    WRITE(6,'(3(I4,'',''))')LOCPAR(I),IDEPFN(I),LOCDEP(I)
         WRITE(6,*)
      ENDIF
      WRITE(6,'(//10X,
     1''TO RESTART CALCULATION USE THE KEYWORD "RESTART".'')')
      WRITE(9)IIIUM,DDDUM,EFSLST,N,(XLAST(I),I=1,N),M
      WRITE(9)((Q(J,I),J=1,M),I=1,M)
      WRITE(9)((R(J,I),J=1,N),I=1,N)
      WRITE(9)(VALVAR(I),I=1,N)
C*****
C     The density matrix is required by ITER upon restart .
C
      LINEAR=(NORBS*(NORBS+1))/2
      WRITE(10)(PA(I),I=1,LINEAR)
      IF(NALPHA.NE.0)WRITE(10)(PB(I),I=1,LINEAR)
C*****
      RETURN
      END
