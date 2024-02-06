      SUBROUTINE SVOPTS (LEN1,LEN2,LEN3,FLAG) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C     THIS ROUTINE ALLOWS SAVING/RESTARTING OF OPTIMIZATION ROUTINES: 
C     CHAIN,LTRD,PATH,POWELL 
C     THE DATA TO BE WRITEN/READ ON FILE 9 MUST BE IN /OPTIM/, 
C     ACCORDING TO THE FOLLOWING EQUIVALENCED RULE : 
C     COMMON /OPTIM / ISAVE1(MAXPAR,LEN1),ISAVE2(LEN2) 
C     THE DENSITY MATRICES ARE ALSO SAVED ON UNIT IDEN AS THEY WILL BE 
C     READ IN ROUTINE 'ITER' WHEN RESTARTING. 
C     ON INPUT FLAG = .T. FOR A RESTART (READ  ON FILE  9) 
C                     .F. FOR A SAVE    (WRITE ON FILES 9 & 10 ) 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / ISAVE(2*NCHAIN + 19)                             3GL3092
      COMMON /OPTMCL / LSAVE(NCHAIN + 4)                                GCL0892
      COMMON /OPTMCR / RSAVE(MAXHES + MAXPAR*(3*MAXPAR+NCHAIN+16) +     3GL3092
     1                       29 + NCHAIN)                               3GL3092
C     COMMON /OPTIM / ISAVE(MAXPAR,1) 
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK) 
C     COMMON /TIME  / TIME0 
      COMMON /TIMECM  / TIME0                                           3GL3092
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /ELEMTS/ ELEMNT(107) 
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR), 
     1                     LOCDEP(MAXPAR) 
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM), 
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM) 
      COMMON /GEOM  / GEO(3,NUMATM) 
      COMMON /GEOVAR/ DUMY(MAXPAR),NVAR,LOC(2,MAXPAR),JDUMY             GCL0393
      COMMON /REPATH/ REACT(100),LATOM,LPARAM                           03GCL93
      DIMENSION IEL1(3),QQ(3) 
      CHARACTER ELEMNT*2                                                DJG0995
      LOGICAL FLAG, XKWFLG                                              DJG0995
      LOGICAL LSAVE
      SAVE
      OPEN(JRES,STATUS='UNKNOWN',FORM='UNFORMATTED')                    GDH1095
      REWIND JRES                                                       GDH1095
      OPEN(JDEN,STATUS='UNKNOWN',FORM='UNFORMATTED')                    GDH1095
      REWIND JDEN                                                       GDH1095
C 
C     RESTART SECTION 
      IF ( FLAG ) THEN 
C  ALWAYS READ THE INFORMATION CONTAINED IN THE COMMON BLOCK OPTIMI
C  IF LEN1 > 0 THEN READ IN THE REALS (OPTMCR)
C  IF LEN2 > 0 THEN READ IN THE INTEGERS (OPTMCI)
C  IF LEN3 > 0 THEN READ IN THE LOGICALS (OPTMCL)
C
         READ(JRES,END=200) IMP, IMP0, JDAT, JOUT
         IF(LEN1.GT.0) READ(9,END=210)(RSAVE(I),I=1,LEN1)
         IF(LEN2.GT.0) READ(9,END=220)(ISAVE(I),I=1,LEN2) 
         IF(LEN3.GT.0) READ(9,END=230)(LSAVE(I),I=1,LEN3)
         RETURN 
      ENDIF 
C 
C     SAVE SECTION 
      CALL PORCPU (TFLY)                                                GL0492
      WRITE(JOUT,100) TFLY-TIME0 
      LINEAR=(NORBS*(NORBS+1))/2 
      WRITE(10)(PA(I),I=1,LINEAR) 
      IF(NALPHA.NE.0)WRITE(10)(PB(I),I=1,LINEAR) 
C
C  ALWAYS WRITE THE INFORMATION CONTAINED IN THE COMMON BLOCK OPTIMI
C  IF LEN1 > 0 THEN WRITE IN THE REALS (OPTMCR)
C  IF LEN2 > 0 THEN WRITE IN THE INTEGERS (OPTMCI)
C  IF LEN3 > 0 THEN WRITE IN THE LOGICALS (OPTMCL)
C
      WRITE(JRES) IMP, IMP0, JDAT, JOUT
      IF(LEN1.GT.0) WRITE(JRES)(RSAVE(I),I=1,LEN1) 
      IF(LEN2.GT.0) WRITE(JRES)(ISAVE(I),I=1,LEN2) 
      IF(LEN3.GT.0) WRITE(JRES)(LSAVE(I),I=1,LEN3) 
C 
C     PRINTOUT MINIMUM INFORMATION BEFORE TO DIE... 
      WRITE(JOUT,'(/10X,'' CURRENT VALUES OF GEOMETRIC VARIABLES'',/)') 
      DEGREE=57.29577951D0 
      GEO(2,1)=0.D0 
      GEO(3,1)=0.D0 
      GEO(1,1)=0.D0 
      GEO(2,2)=0.D0 
      GEO(3,2)=0.D0 
      GEO(3,3)=0.D0 
      IVAR=1 
      NA(1)=0 
      XKWFLG=.FALSE.                                                    DJG0995
      NTPRT=1                                                           GDH1195
      CALL ECHOWD(JOUT,NTPRT,XKWFLG)                                    GDH1195
      DO 60 I=1,NATOMS 
      DO 40 J=1,3 
   40 IEL1(J)=0 
   50 CONTINUE 
      IF(LOC(1,IVAR).EQ.I) THEN 
         IEL1(LOC(2,IVAR))=1 
         IVAR=IVAR+1 
         GOTO 50 
      ENDIF 
      IF(I.LT.4) THEN 
         IEL1(3)=0 
         IF(I.LT.3) THEN 
            IEL1(2)=0 
            IF(I.LT.2) IEL1(1)=0 
         ENDIF 
      ENDIF 
      IF(I.EQ.LATOM)IEL1(LPARAM)=-1 
      QQ(1)=GEO(1,I) 
      QQ(2)=GEO(2,I)*DEGREE 
      QQ(3)=GEO(3,I)*DEGREE 
   60 WRITE(JOUT,110) ELEMNT(LABELS(I)),(QQ(K),IEL1(K),K=1,3) 
     .               ,NA(I),NB(I),NC(I) 
      I=0 
      X=0.D0 
      WRITE(JOUT,120) I,X,I,X,I,X,I,I,I,I 
      IF(NDEP.NE.0)THEN 
         DO 70 I=1,NDEP 
   70    WRITE(JOUT,130) LOCPAR(I),IDEPFN(I),LOCDEP(I) 
         WRITE(JOUT,*) 
      ENDIF 
C     NOW THE RUN IS DEAD. 
          ISTOP=1                                                       GDH1095
          IWHERE=156                                                    GDH1095
          RETURN                                                        GDH1095
C 
C     ERROR SECTION 
  200 WRITE(JOUT,300)
          ISTOP=1                                                       GDH1095
          IWHERE=157                                                    GDH1095
          RETURN                                                        GDH1095
  210 WRITE(JOUT,310) I
          ISTOP=1                                                       GDH1095
          IWHERE=158                                                    GDH1095
          RETURN                                                        GDH1095
  220 WRITE(JOUT,320) I
          ISTOP=1                                                       GDH1095
          IWHERE=159                                                    GDH1095
          RETURN                                                        GDH1095
  230 WRITE(JOUT,330) I
C 
  100 FORMAT(//' * * * *** TIME IS UP]]] *** * * * ALL DATA SAVED ON UNI 
     .T 9 AND 10'/ 
     .' * * * ELAPSED TIME IN THIS RUN :',F10.2,' SECONDS'/ 
     .' * * * TO RESTART, ADD *ONLY* THE KEYWORD ''RESTART''.'//) 
  110 FORMAT(2X,A2,3(F12.6,I3),I4,2I3) 
  120 FORMAT(I4,   3(F12.6,I3),I4,2I3) 
  130 FORMAT(3(I4,',')) 
  300 FORMAT('UNEXPECTED END-OF-FILE 9 IN SVOPTS READING ELEMENTS ',
     1       'OF OPTIMI')
  310 FORMAT('UNEXPECTED END-OF-FILE 9 IN SVOPTS READING ELEMENTS ',
     1       'OF OPTMCR, ELEMENT',I5)
  320 FORMAT('UNEXPECTED END-OF-FILE 9 IN SVOPTS READING ELEMENTS ',
     1       'OF OPTMCI, ELEMENT',I5)
  330 FORMAT('UNEXPECTED END-OF-FILE 9 IN SVOPTS READING ELEMENTS ',
     1       'OF OPTMCL, ELEMENT',I5)
      RETURN
      END 
