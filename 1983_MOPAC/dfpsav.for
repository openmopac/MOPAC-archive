      SUBROUTINE DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES/NOLIST'
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
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), DUMY(MAXPAR)
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /ALPARM/ ALPARM(3,MAXPAR),ILOOP,X0, X1, X2
      COMMON /REACTN/ STEP, GEOA(3,NUMATM), GEOVEC(3,NUMATM),CALCST
      COMMON /GMETRY/ GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     +                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /PATH  / LATOM,LPARAM,REACT(100)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     +                NLAST(NUMATM), NORBS, NELECS,
     1                NALPHA, NBETA, NCLOSE, NOPEN
      COMMON /FMATRX/ HESINV(MAXHES)
      COMMON /GEOSYM/ NDEP,LOCPAR(200),IDEPFN(200),LOCDEP(200)
      DIMENSION IEL1(3),Q(3)
      CHARACTER ELEMNT(99)*2, KEYWRD*80,KOMENT*80, TITLE*80
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      DATA ELEMNT/' H','He',
     2 'Li','Be',' B',' C',' N',' O',' F','Ne',
     3 'Na','Mg','Al','Si',' P',' S','Cl','Ar',
     4 ' K','Ca','Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu',
     4 'Zn','Ga','Ge','As','Se','Br','Kr',
     5 'Rb','Sr',' Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
     5 'Cd','In','Sn','Sb','Te',' I','Xe',
     6 'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy',
     6 'Ho','Er','Tm','Yb','Lu','Hf','Ta',' W','Re','Os','Ir','Pt',
     6 'Au','Hg','Tl','Pb','Bi','Po','At','Rn',
     7 'Fr','Ra','Ac','Th','Pa',' U','Np','Pu','Am','Cm','Bk','Cf','XX'/
      DEGREE=57.29577951D0
      IR=9
      REWIND 10
      REWIND IR
      IF(MDFP(9) .EQ. 1) THEN
        WRITE(6,'(//10X,''- - - - - - - TIME UP - - - - - - -'',//)')
        IF(INDEX(KEYWRD,'SADDLE') .NE. 0) THEN
        WRITE(6,'(//10X,'' NO RESTART EXISTS FOR SADDLE'',//
     +  10X,'' HERE IS A DATA-FILE FILES THAT MIGHT BE SUITABLE'',/
     +  10X,'' FOR RESTARTING THE CALCULATION'',///)')
      WRITE(6,'(1X,A)')KEYWRD,KOMENT,TITLE
      DO 44 ILOOP=1,2
      GEO(2,1)=0.D0
      GEO(3,1)=0.D0
      GEO(1,1)=0.D0
      GEO(2,2)=0.D0
      GEO(3,2)=0.D0
      GEO(3,3)=0.D0
      IVAR=1
      NA(1)=0
      DO 112 I=1,NATOMS
          DO 122 J=1,3
  122         IEL1(J)=0
  123         CONTINUE
          IF(LOC(1,IVAR).EQ.I) THEN
              IEL1(LOC(2,IVAR))=1
              IVAR=IVAR+1
              GOTO 123
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
  112     WRITE(6,'(2X,A2,3(F12.6,I3),I4,2I3)')
     +    ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I)
          I=0
          X=0.D0
          WRITE(6,'(I4,3(F12.6,I3),I4,2I3)')
     +    I,X,I,X,I,X,I,I,I,I
      DO 45 I=1,NATOMS
      DO 45 J=1,3
  45  GEO(J,I)=GEOA(J,I)
  44  CONTINUE
      WRITE(6,'(///10X,''CALCULATION TERMINATED HERE'')')
      STOP
      ENDIF
        WRITE(6,'(//10X,'' - THE CALCULATION IS BEING DUMPED TO DISK'',
     +  /10X,''   RESTART IT USING THE MAGIC WORD "RESTART"'')')
        WRITE(6,'(//10X,''CURRENT VALUE OF HEAT OF FORMATION =''
     +  ,F12.6)')FUNCT1
      GEO(2,1)=0.D0
      GEO(3,1)=0.D0
      GEO(1,1)=0.D0
      GEO(2,2)=0.D0
      GEO(3,2)=0.D0
      GEO(3,3)=0.D0
      IVAR=1
      NA(1)=0
      WRITE(6,'(A)')KEYWRD,KOMENT,TITLE
      DO 110 I=1,NATOMS
          DO 120 J=1,3
  120         IEL1(J)=0
  121         CONTINUE
          IF(LOC(1,IVAR).EQ.I) THEN
              IEL1(LOC(2,IVAR))=1
              IVAR=IVAR+1
              GOTO 121
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
  110     WRITE(6,'(2X,A2,3(F12.6,I3),I4,2I3)')
     +    ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I)
          I=0
          X=0.D0
          WRITE(6,'(I4,3(F12.6,I3),I4,2I3)')
     +    I,X,I,X,I,X,I,I,I,I
      IF(NDEP.NE.0)THEN
        DO 130 I=1,NDEP
  130   WRITE(6,'(3(I4,'',''))')LOCPAR(I),IDEPFN(I),LOCDEP(I)
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
        FIRST=.FALSE.
        RETURN
      ENDIF
      END
