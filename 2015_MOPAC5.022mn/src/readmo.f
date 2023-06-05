      SUBROUTINE READMO(*)                                              CSTP
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      INCLUDE 'SIZES.i'
C
C
C
C MODULE TO READ IN GEOMETRY FILE, OUTPUT IT TO THE USER,
C AND CHECK THE DATA TO SEE IF IT IS REASONABLE.
C EXIT IF NECESSARY.
C
C
C
C  ON EXIT NATOMS    = NUMBER OF ATOMS PLUS DUMMY ATOMS (IF ANY).
C          KEYWRD    = KEYWORDS TO CONTROL CALCULATION
C          KOMENT    = COMMENT CARD
C          TITLE     = TITLE CARD
C          LABELS    = ARRAY OF ATOMIC LABELS INCLUDING DUMMY ATOMS
C                      (LABELS HERE MEANS ATOMIC NUMBER OF THAT ATOM).
C          GEO       = ARRAY OF INTERNAL COORDINATES.
C          LOPT      = FLAGS FOR OPTIMIZATION OF MOLECULE
C          NA        = ARRAY OF LABELS OF ATOMS, BOND LENGTHS.
C          NB        = ARRAY OF LABELS OF ATOMS, BOND ANGLES.
C          NC        = ARRAY OF LABELS OF ATOMS, DIHEDRAL ANGLES.
C          LATOM     = LABEL OF ATOM OF REACTION COORDINATE.
C          LPARAM    = RC: 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL
C          REACT(200)= REACTION COORDINATE PARAMETERS
C          LOC(1,I)  = LABEL OF ATOM TO BE OPTIMIZED.
C          LOC(2,I)  = 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL.
C          NVAR      = NUMBER OF PARAMETERS TO BE OPTIMIZED.
C          XPARAM    = STARTING VALUE OF PARAMETERS TO BE OPTIMIZED.
C
************************************************************************
C *** INPUT THE TRIAL GEOMETRY  \IE.  KGEOM=0\
C   LABEL(I) = THE ATOMIC NUMBER OF ATOM\I\.
C            = 99, THEN THE I-TH ATOM IS A DUMMY ATOM USED ONLY TO
C              SIMPLIFY THE DEFINITION OF THE MOLECULAR GEOMETRY.
C   GEO(1,I) = THE INTERNUCLEAR SEPARATION \IN ANGSTROMS\ BETWEEN ATOMS
C              NA(I) AND (I).
C   GEO(2,I) = THE ANGLE NB(I):NA(I):(I) INPUT IN DEGREES; STORED IN
C              RADIANS.
C   GEO(3,I) = THE ANGLE BETWEEN THE VECTORS NC(I):NB(I) AND NA(I):(I)
C              INPUT IN DEGREES - STORED IN RADIANS.
C  LOPT(J,I) = -1 IF GEO(J,I) IS THE REACTION COORDINATE.
C            = +1 IF GEO(J,I) IS A PARAMETER TO BE OPTIMIZED
C            =  0 OTHERWISE.
C *** NOTE:    MUCH OF THIS DATA IS NOT INCLUDED FOR THE FIRST 3 ATOMS.
C     ATOM1  INPUT LABELS(1) ONLY.
C     ATOM2  INPUT LABELS(2) AND GEO(1,2) SEPARATION BETWEEN ATOMS 1+2
C     ATOM3  INPUT LABELS(3), GEO(1,3)    SEPARATION BETWEEN ATOMS 2+3
C              AND GEO(2,3)              ANGLE ATOM1 : ATOM2 : ATOM3
C
************************************************************************
C
      DIMENSION LOPT(3,NUMATM)
      CHARACTER*160 KEYWRD,NKEYWRD                                      LF1211
      CHARACTER*80  KOMENT,TITLE,LINE, BANNER
      CHARACTER KEYS(80)*1, SPACE*1, SPACE2*2, CH*1, CH2*2
      CHARACTER ELEMNT*2, IDATE*24
      COMMON /KEYWRD/ KEYWRD
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /GEOVAR/ XPARAM(MAXPAR), NVAR, LOC(2,MAXPAR)               IR0394
C    changed common path for portability  (IR)
      COMMON /PATHI / LATOM,LPARAM
      COMMON /PATHR / REACT(200)
      COMMON /MESH  / LATOM1, LPARA1, LATOM2, LPARA2
      COMMON /ELEMTS/ ELEMNT(120)
      COMMON /ISTOPE/ AMS(120)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOSYM/ NDEP, LOCPAR(MAXPAR), IDEPFN(MAXPAR),
     1                      LOCDEP(MAXPAR)
      COMMON /IOCM/ IREAD
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      COMMON /INTEXT/ KEYTXT, COMTXT, TLETXT, SPCTXT(80),               LF0610
     &                CINTXT, FINTXT,                                   LF0610
     &                LINTXT                                            LF0610
      CHARACTER*160 KEYTXT
      CHARACTER*80  COMTXT, TLETXT, SPCTXT
      INTEGER       CINTXT, FINTXT                                      LF0610
      LOGICAL       LINTXT                                              LF0610
      LOGICAL INT
      DIMENSION COORD(3,NUMATM),VALUE(40)
      EQUIVALENCE (KEYS(1),KEYWRD)
CSAV         SAVE                                                           GL0892
      DATA SPACE, SPACE2/' ','  '/
      DATA ITAB /9/                                                     LF0710
C
      IF (LINTXT) THEN                                                  LF0610
         KEYWRD=KEYTXT                                                  LF0610
         KOMENT=COMTXT                                                  LF0610
         TITLE =TLETXT                                                  LF0610
      ELSE                                                              LF0610
         READ(IREAD,'(A)')KEYWRD,KOMENT,TITLE
      ENDIF                                                             LF0610
      DO 5 II=1,160                                                     LF0710
    5 IF (KEYWRD(II:II).EQ.CHAR(ITAB)) KEYWRD(II:II)=' '                LF0710
      ILOWA = ICHAR('a')
      ILOWZ = ICHAR('z')
      ICAPA = ICHAR('A')
************************************************************************
      NKEYWRD=KEYWRD                                                    LF1211
      DO 10 I=1,160
         ILINE=ICHAR(KEYWRD(I:I))
         IF(ILINE.GE.ILOWA.AND.ILINE.LE.ILOWZ) THEN
            NKEYWRD(I:I)=CHAR(ILINE+ICAPA-ILOWA)
         IF(ILINE.EQ.ITAB) KEYWRD(I:I)=SPACE                            LF0710
         ENDIF
   10 CONTINUE
C     LF1211: Undo conversion to upper case for file names appearing after "EXTERNAL=".
      IPOS=INDEX(NKEYWRD,'EXTERNAL=')                                   LF1211
      IF (IPOS.NE.0) THEN                                               LF1211
         IPOS2=INDEX(NKEYWRD(IPOS:),' ')+IPOS                           LF1211
         NKEYWRD(IPOS:IPOS2-1)=KEYWRD(IPOS:IPOS2-1)                     LF1211
      ENDIF                                                             LF1211
      KEYWRD=NKEYWRD                                                    LF1211
************************************************************************
      IF(INDEX(KEYWRD,'ECHO').NE.0)THEN
         IF (.NOT.DOPRNT) GOTO 40                                       LF0610
         IF (LINTXT) THEN                                               LF0610
            WRITE(6,'(1X,A)') KEYTXT                                    LF0610
            WRITE(6,'(1X,A)') COMTXT                                    LF0610
            WRITE(6,'(1X,A)') TLETXT                                    LF0610
            DO 15 I=1,FINTXT                                            LF0610
   15       WRITE(6,'(1X,A)') SPCTXT(I)                                 LF0610
         ELSE                                                           LF0610
            DO 30 I=1,1000
               READ(IREAD,'(A)',END=40)KEYWRD
               DO 20 J=160,2,-1
   20          IF(KEYWRD(J:J).NE.' ')GOTO 30
               J=1
   30       IF (DOPRNT) WRITE(6,'(1X,A)')KEYWRD(1:J)                    CIO
         ENDIF                                                          LF0610
      ENDIF
   40 REWIND IREAD                                                      LF0510
      IF(DOPRNT.AND.INDEX(KEYWRD,'ECHO').NE.0) WRITE(6,'(''1'')')       CIO
      IF (LINTXT) THEN                                                  LF0610
         KEYWRD=KEYTXT                                                  LF0610
         KOMENT=COMTXT                                                  LF0610
         TITLE= TLETXT                                                  LF0610
         CINTXT=1                                                       LF0610
      ELSE                                                              LF0610
         READ(IREAD,'(A)')KEYWRD,KOMENT,TITLE
      ENDIF                                                             LF0610
************************************************************************
      NKEYWRD=KEYWRD                                                    LF1211
      DO 50 I=1,160
         ILINE=ICHAR(KEYWRD(I:I))
         IF(ILINE.GE.ILOWA.AND.ILINE.LE.ILOWZ) THEN
            NKEYWRD(I:I)=CHAR(ILINE+ICAPA-ILOWA)
         ENDIF
   50 CONTINUE
      IPOS=INDEX(NKEYWRD,'EXTERNAL=')                                   LF1211
      IF (IPOS.NE.0) THEN                                               LF1211
         IPOS2=INDEX(NKEYWRD(IPOS:),' ')+IPOS                           LF1211
         NKEYWRD(IPOS:IPOS2-1)=KEYWRD(IPOS:IPOS2-1)                     LF1211
      ENDIF                                                             LF1211
      KEYWRD=NKEYWRD                                                    LF1211
************************************************************************
      IF(KEYWRD(1:1) .NE. SPACE) THEN
         CH=KEYWRD(1:1)
         KEYWRD(1:1)=SPACE
         DO 60 I=2,78
            CH2=KEYWRD(I:I)
            KEYWRD(I:I)=CH
            CH=CH2
            IF(KEYWRD(I+1:I+2) .EQ. SPACE2) THEN
               KEYWRD(I+1:I+1)=CH
               GOTO 70
            ENDIF
   60    CONTINUE
         IF(DOPRNT.AND.KEYWRD(160:160) .NE. SPACE)                       CIO
     1WRITE(6,'(//10X,''WARNING-THE LAST KEYWORD MAY BE CORRUPTED'')')
         KEYWRD(160:160)=CH
   70    CONTINUE
      ENDIF
      IF(KOMENT(1:1) .NE. SPACE) THEN
         CH=KOMENT(1:1)
         KOMENT(1:1)=SPACE
         DO 80 I=2,78
            CH2=KOMENT(I:I)
            KOMENT(I:I)=CH
            CH=CH2
            IF(KOMENT(I+1:I+2) .EQ. SPACE2) THEN
               KOMENT(I+1:I+1)=CH
               GOTO 90
            ENDIF
   80    CONTINUE
         IF(DOPRNT.AND.KOMENT(80:80) .NE. SPACE)
     1WRITE(6,'(//1X,A)')'WARNING-THE LAST WORD IN FIRST COMMENT LINE'//
     2' MAY BE CORRUPTED'
         KOMENT(80:80)=CH
   90    CONTINUE
      ENDIF
      IF(TITLE(1:1) .NE. SPACE) THEN
         CH=TITLE(1:1)
         TITLE(1:1)=SPACE
         DO 100 I=2,78
            CH2=TITLE(I:I)
            TITLE(I:I)=CH
            CH=CH2
            IF(TITLE(I+1:I+2) .EQ. SPACE2) THEN
               TITLE(I+1:I+1)=CH
               GOTO 110
            ENDIF
  100    CONTINUE
         IF(DOPRNT.AND.TITLE(80:80) .NE. SPACE)                         CIO
     1WRITE(6,'(//1X,A)')'WARNING-THE LAST WORD IN SECOND COMMENT'//
     2' LINE MAY BE CORRUPTED'
         TITLE(80:80)=CH
  110    CONTINUE
      ENDIF
      CALL GETGEO(IREAD,LABELS,GEO,LOPT,NA,NB,NC,AMS,NATOMS,INT,*9999)  CSTP(call)
C
C
C OUTPUT FILE TO UNIT 6
C
C    WRITE HEADER
      IDATE=' '
      IF (DOPRNT) CALL DATE(IDATE,*9999)                                CSTP(call) / CIO
C          WRITE(6,'(//)')
      IF (DOPRNT) WRITE(6,'(1X,15(''*****''),''****'')')                CIO
C
C     CHANGE THE FOLLOWING LINE TO SUIT LOCAL ENVIRONMENT, IF DESIRED
C
      BANNER=' **  Welcome to MOPAC 5.022mn - A version of MOPAC '//    LF0309
     1'for Direct Molecular Dynamics  **'
           IF (DOPRNT) WRITE(6,'(A)')BANNER                             CIO
C
C    THE BANNER DOES NOT APPEAR ANYWHERE ELSE.
C
      IF (DOPRNT) WRITE(6,'(1X,79(''*''))')                             CIO
      LINE='   MNDO'
      IF(INDEX(KEYWRD,'MNDO-D') .NE. 0)  LINE=' MNDO-D'                 LF0310
      IF(INDEX(KEYWRD,'MINDO') .NE. 0)   LINE='MINDO/3'
      IF((INDEX(KEYWRD,'AM1')   .NE. 0) .AND.
     1   (INDEX(KEYWRD,'AM1-D') .EQ. 0)) LINE='    AM1'                 LF0509
      IF((INDEX(KEYWRD,'PM3')   .NE. 0) .AND.
     1   (INDEX(KEYWRD,'PM3-D') .EQ. 0)) LINE='    PM3'                 LF0309
      IF(INDEX(KEYWRD,'PM6')   .NE. 0)   LINE='    PM6'                 LF0709
      IF((INDEX(KEYWRD,'PM6G09-D').NE.0).OR.
     &   (INDEX(KEYWRD,'PM6-D').NE.0))   LINE='  PM6-D'                 LF0310
      IF(INDEX(KEYWRD,'RM1')   .NE. 0)   LINE='    RM1'
      IF(INDEX(KEYWRD,'RM1-D') .NE. 0)   LINE='  RM1-D'                 LF0310
      IF(INDEX(KEYWRD,'PDG')   .NE. 0)   LINE='    PDG'
      IF(INDEX(KEYWRD,'MDG')   .NE. 0)   LINE='    MDG'
      IF(INDEX(KEYWRD,'PM3-D') .NE. 0)   LINE='  PM3-D'                 LF0309
      IF(INDEX(KEYWRD,'AM1-D') .NE. 0)   LINE='  AM1-D'                 LF0509
      IF(INDEX(KEYWRD,'MNDO-D3').NE.0)   LINE='MNDO-D3'                 LF1211
      IF(INDEX(KEYWRD,'AM1-D3' ).NE.0)   LINE=' AM1-D3'                 LF1211
      IF(INDEX(KEYWRD,'PM3-D3' ).NE.0)   LINE=' PM3-D3'                 LF1211
      IF(INDEX(KEYWRD,'RM1-D3' ).NE.0)   LINE=' RM1-D3'                 LF1211
      IF(INDEX(KEYWRD,'PM6-D3' ).NE.0)   LINE=' PM6-D3'                 LF1211
      IF(INDEX(KEYWRD,'PMOV1'  ).NE.0)   LINE='  PMOv1'                 LF0614
      IF(INDEX(KEYWRD,'PMO2'   ).NE.0)   LINE='   PMO2'                 LF0614
      IF(INDEX(KEYWRD,'PMO2A'  ).NE.0)   LINE='  PMO2A'                 LF0614
      IF (DOPRNT) THEN                                                  CIO
      WRITE(6,'(/29X,A,'' CALCULATION RESULTS'',28X,///1X,15(''*****'')
     1,''****'' )')LINE(:7)
      WRITE(6,'('' *'',10X,''MOPAC:  VERSION '',F5.3,
     115X,''CALC''''D. '',A24)') VERSON, IDATE
      ENDIF                                                             CIO
C
C CHECK DATA
C
      DO 120 I=1,NATOMS
         IF (LABELS(I) .LE. 0 ) THEN
            IF (DOPRNT) WRITE(6,'('' ATOMIC NUMBER OF '',I3,'' ?'')')   CIO
     &                      LABELS(I)                                   CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
         IF (  NA(I).GE.I.OR. NB(I).GE.I.OR. NC(I).GE.I
     1  .OR. (NA(I).EQ.NB(I))   .AND. I.GT.1
     2  .OR. (NA(I).EQ.NC(I).OR.NB(I).EQ.NC(I))  .AND. I.GT.2
     3  .OR.  NA(I)*NB(I)*NC(I).EQ.0  .AND. I.GT.3) THEN
            IF (DOPRNT) WRITE(6,'('' ATOM NUMBER '',I3,                 CIO
     &                             '' IS ILLDEFINED'')') I              CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
  120 CONTINUE
C
C WRITE KEYWORDS BACK TO USER AS FEEDBACK
      CALL WRTKEY(KEYWRD,*9999)                                         CSTP(call)
      IF (DOPRNT) WRITE(6,'(1X,14(''*****''),''*'',I3.3,''BY'',I3.3)')  CIO
     &                      MAXHEV,MAXLIT                               CIO
C
C CONVERT ANGLES TO RADIANS
      DO 130 I=1,NATOMS
         DO 130 J=2,3
            GEO(J,I) = GEO(J,I) * 2.D0*ASIN(1.D0)/180.D0
  130 CONTINUE
C
C FILL IN GEO MATRIX IF NEEDED
      NDEP=0
      IF( INDEX(KEYWRD,'SYM') .NE. 0) CALL GETSYM(*9999)                 CSTP(call)
      IF(NDEP.NE.0) CALL SYMTRY(*9999)                                   CSTP(call)
C
C INITIALIZE FLAGS FOR OPTIMIZE AND PATH
      IFLAG = 0
      NVAR  = 0
      LATOM = 0
      NUMAT=0
      DO 160 I=1,NATOMS
         IF(LABELS(I).NE.99.AND.LABELS(I).NE.107)NUMAT=NUMAT+1
         DO 160 J=1,3
            IF (LOPT(J,I) ) 140, 160, 150
C    FLAG FOR PATH
  140       CONVRT=1.D0
            IF ( IFLAG .NE. 0 ) THEN
               IF(INDEX(KEYWRD,'STEP1').NE.0)THEN
                  LPARA1=LPARAM
                  LATOM1=LATOM
                  LPARA2=J
                  LATOM2=I
                  LATOM=0
                  IFLAG=0
                  GOTO 160
               ELSE
                  IF (DOPRNT) WRITE(6,                                  CIO
     &                 '('' ONLY ONE REACTION COORDINATE PERMITTED'')') CIO
      RETURN 1                                                          CSTP (stop)
               ENDIF
            ENDIF
            LATOM  = I
            LPARAM = J
            IF(J.GT.1) CONVRT=0.01745329252D00
            REACT(1)  = GEO(J,I)
            IREACT=1
            IFLAG = 1
            GO TO 160
C    FLAG FOR OPTIMIZE
  150       NVAR = NVAR + 1
            LOC(1,NVAR) = I
            LOC(2,NVAR) = J
            XPARAM(NVAR)   = GEO(J,I)
  160 CONTINUE
C READ IN PATH VALUES
      IF(IFLAG.EQ.0) GO TO 200
      IF(INDEX(KEYWRD,'NLLSQ').NE.0)THEN
         IF (DOPRNT) WRITE(6,'(A)')' NLLSQ USED WITH REACTION PATH; '// CIO
     1'THIS OPTION IS NOT ALLOWED'
      RETURN 1                                                          CSTP (stop)
      ENDIF
      IF(INDEX(KEYWRD,'SIGMA').NE.0)THEN
         IF (DOPRNT) WRITE(6,'(A)')' SIGMA USED WITH REACTION PATH; '// CIO
     1'THIS OPTION IS NOT ALLOWED'
      RETURN 1                                                          CSTP (stop)
      ENDIF
  170 CONTINUE                                                          LF0610
      IF (LINTXT) THEN                                                  LF0610
         IF (CINTXT.GT.FINTXT) GOTO 190                                 LF0610
         LINE=SPCTXT(CINTXT)                                            LF0610
         CINTXT=CINTXT+1                                                LF0610
      ELSE                                                              LF0610
         READ(IREAD,'(A)',END=190) LINE                                 LF0610
      ENDIF                                                             LF0610
      CALL NUCHAR(LINE,VALUE,NREACT,*9999)                              CSTP(call)
      DO 180 I=1,NREACT
         IJ=IREACT+I
         IF(IJ.GT.200)THEN
            IF (DOPRNT) WRITE(6,'(///,''    ONLY TWO HUNDRED POINTS''// CIO
     &                 '' ALLOWED IN REACTION'','' COORDINATE'')')      CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
         REACT(IJ)=VALUE(I)*CONVRT
         IF(ABS(REACT(IJ)-REACT(IJ-1)).LT.1.D-5)THEN
            DUM1 = REACT(IJ)/CONVRT
            DUM2 = REACT(IJ-1)/CONVRT
            IF (DOPRNT) WRITE(6,'(///,                                  CIO
     &      '' TWO ADJACENT POINTS ARE IDENTICAL:  '',F7.3,2X,F7.3,/,   CIO
     &      '' THIS IS NOT ALLOWED IN A PATH CALCULATION'')') DUM1,DUM2 CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
  180 CONTINUE
      IREACT=IREACT+NREACT
      GO TO 170
  190 CONTINUE
      DEGREE=1.D0
      IF(LPARAM.GT.1)DEGREE=90.D0/ASIN(1.D0)
      IF(IREACT.LE.1) THEN
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     &                 '' NO POINTS SUPPLIED FOR REACTION PATH'')')     CIO
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     &                 '' GEOMETRY AS READ IN IS AS FOLLOWS'')')        CIO
         CALL GEOUT(*9999)                                              CSTP(call)
      RETURN 1                                                          CSTP (stop)
      ELSE
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     &                 '' POINTS ON REACTION COORDINATE'')')            CIO
         IF (DOPRNT) WRITE(6,'(10X,8F8.2)')(REACT(I)*DEGREE,I=1,IREACT) CIO
      ENDIF
      IEND=IREACT+1
      REACT(IEND)=-1.D12
C
C OUTPUT GEOMETRY AS FEEDBACK
C
  200 IF (DOPRNT) WRITE(6,'(A)')KEYWRD,KOMENT,TITLE                     CIO
      CALL GEOUT(*9999)                                                 CSTP(call)
      CALL GMETRY(GEO,COORD,*9999)                                      CSTP(call)
      IF (INDEX(KEYWRD,'NOXYZ') .EQ. 0) THEN
         IF (DOPRNT) WRITE(6,'(//10X,''CARTESIAN COORDINATES '',/)')    CIO
         IF (DOPRNT) WRITE(6,'(4X,''NO.'',7X,''ATOM'',9X,''X'',         CIO
     1  9X,''Y'',9X,''Z'',/)')
         L=0
         DO 210 I=1,NATOMS
            IF(LABELS(I) .EQ. 99.OR.LABELS(I).EQ.107) GOTO 210
            L=L+1
            IF (DOPRNT) WRITE(6,'(I6,8X,A2,4X,3F10.4)')                 CIO
     1  L,ELEMNT(LABELS(I)),(COORD(J,L),J=1,3)
  210    CONTINUE
      ENDIF
      IF(   INDEX(KEYWRD,' XYZ') .NE. 0 )THEN
         IF( NVAR .NE. 0 .AND.
     1 INT.AND.(NDEP .NE. 0 .OR.  NVAR.LT.3*NUMAT-6)) THEN
            IF(DOPRNT.AND.NDEP.NE.0)
     1WRITE(6,'(//10X,'' INTERNAL COORDINATES READ IN, AND SYMMETRY''
     2,/10X,'' SPECIFIED, BUT CALCULATION TO BE RUN IN CARTESIAN ''
     3,''COORDINATES'')')
            IF(DOPRNT.AND.NVAR.LT.3*NUMAT-6)
     1WRITE(6,'(//10X,'' INTERNAL COORDINATES READ IN, AND'',
     2'' CALCULATION '',/10X,''TO BE RUN IN CARTESIAN COORDINATES, '',
     3/10X,''BUT NOT ALL COORDINATES MARKED FOR OPTIMIZATION'')')
            IF (DOPRNT) WRITE(6,'(//10X,                                CIO
     &   '' THIS INVOLVES A LOGICALLLY ABSURD CHOICE'',                 CIO
     &   /10X,'' SO THE CALCULATION IS TERMINATED AT THIS POINT'')')    CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
         SUMX=0.D0
         SUMY=0.D0
         SUMZ=0.D0
         DO 220 J=1,NUMAT
            SUMX=SUMX+COORD(1,J)
            SUMY=SUMY+COORD(2,J)
  220    SUMZ=SUMZ+COORD(3,J)
         SUMX=SUMX/NUMAT
         SUMY=SUMY/NUMAT
         SUMZ=SUMZ/NUMAT
         DO 230 J=1,NUMAT
            GEO(1,J)=COORD(1,J)-SUMX
            GEO(2,J)=COORD(2,J)-SUMY
  230    GEO(3,J)=COORD(3,J)-SUMZ
         NA(1)=99
         J=0
         MVAR=1
         NVAR=1
         DO 260 I=1,NATOMS
            IF(LABELS(I).NE.99)THEN
               J=J+1
  240          IF(LOC(1,MVAR) .EQ. I) THEN
                  XPARAM(NVAR)=GEO(LOC(2,NVAR),J)
                  LOC(1,NVAR)=J
                  MVAR=MVAR+1
                  NVAR=NVAR+1
                  GOTO 240
               ENDIF
               LABELS(J)=LABELS(I)
            ELSE
  250          IF(LOC(1,MVAR) .EQ. I)THEN
                  MVAR=MVAR+1
                  GOTO 250
               ENDIF
            ENDIF
  260    CONTINUE
         NVAR=NVAR-1
         NATOMS=NUMAT
      ELSE
         IF(NVAR.EQ.0) RETURN
         IF( .NOT. INT.AND.(NDEP .NE. 0 .OR.  NVAR.LT.3*NUMAT-6)) THEN
            IF(DOPRNT.AND.NDEP.NE.0)
     1WRITE(6,'(//10X,'' CARTESIAN COORDINATES READ IN, AND SYMMETRY''
     2,/10X,'' SPECIFIED, BUT CALCULATION TO BE RUN IN INTERNAL ''
     3,''COORDINATES'')')
            IF(DOPRNT.AND.NVAR.LT.3*NUMAT-6)
     1WRITE(6,'(//10X,'' CARTESIAN COORDINATES READ IN, AND'',
     2'' CALCULATION '',/10X,''TO BE RUN IN INTERNAL COORDINATES, '',
     3/10X,''BUT NOT ALL COORDINATES MARKED FOR OPTIMIZATION'')')
            IF (DOPRNT) THEN                                            CIO
            WRITE(6,'(//10X,''MOPAC, BY DEFAULT, USES INTERNAL COORDINAT
     1ES'',/10X,''TO SPECIFY CARTESIAN COORDINATES USE KEY-WORD :XYZ:'')
     2')
            WRITE(6,'(10X,''YOUR CURRENT CHOICE OF KEY-WORDS INVOLVES A
     1'',''LOGICALLLY '',
     2/10X,''ABSURD CHOICE SO THE CALCULATION IS TERMINATED AT THIS ''
     3,''POINT'')')
            ENDIF                                                       CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
      ENDIF
      RETURN
 9999 RETURN 1                                                          CSTP
      END
