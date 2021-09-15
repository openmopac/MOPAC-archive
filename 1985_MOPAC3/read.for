      SUBROUTINE READ
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      INCLUDE 'SIZES'
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
C          LABELS    = ARRAY OF ATOMIC LABELS INCLUDING DUMMY ATOMS.
C          GEO       = ARRAY OF INTERNAL COORDINATES.
C          LOPT      = FLAGS FOR OPTIMIZATION OF MOLECULE
C          NA        = ARRAY OF LABELS OF ATOMS, BOND LENGTHS.
C          NB        = ARRAY OF LABELS OF ATOMS, BOND ANGLES.
C          NC        = ARRAY OF LABELS OF ATOMS, DIHEDRAL ANGLES.
C          LATOM     = LABEL OF ATOM OF REACTION COORDINATE.
C          LPARAM    = RC: 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL
C          REACT(100)= REACTION COORDINATE PARAMETERS
C          LOC(1,I)  = LABEL OF ATOM TO BE OPTIMISED.
C          LOC(2,I)  = 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL.
C          NVAR      = NUMBER OF PARAMETERS TO BE OPTIMISED.
C          XPARAM    = STARTING VALUE OF PARAMETERS TO BE OPTIMISED.
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
C            = +1 IF GEO(J,I) IS A PARAMETER TO BE OPTIMISED
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
      CHARACTER*80 KEYWRD,KOMENT,TITLE,LINE
      CHARACTER KEYS(80)*1, SPACE*1, SPACE2*2, CH*1, CH2*2
      COMMON /KEYWRD/ KEYWRD
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /GEOVAR/ NVAR, LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
      COMMON /PATH  / LATOM,LPARAM,REACT(100)
      COMMON /MESH  / LATOM1, LPARA1, LATOM2, LPARA2
      COMMON /ISTOPE/ AMS(107)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOSYM/ NDEP, LOCPAR(MAXPAR), IDEPFN(MAXPAR),
     1                      LOCDEP(MAXPAR)
      LOGICAL NOCHEK, INT
      DIMENSION IZOK(107), COORD(3,NUMATM),VALUE(40)
      EQUIVALENCE (KEYS(1),KEYWRD)
************************************************************************
*     PERIODIC TABLE OF THE ELEMENTS, A '1' MEANS THAT THE ELEMENT IS
*     ALLOWED AT THE MNDO LEVEL.
*GROUP1 2     'F' SHELL                 'D' SHELL           3 4 5 6 7 8
*
      DATA IZOK/
     11,                                                              0,
     21,1,                                                  1,1,1,1,1,0,
     31,0,                                                  1,1,1,1,1,0,
     41,0,                             0,0,0,1,0,0,0,0,0,0, 0,1,0,0,1,0,
     50,0,                             0,0,0,0,0,0,0,0,0,0, 0,1,0,0,1,0,
     60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,1, 0,1,0,0,0,0,
     70,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0, 1,1,1,1,1/
*
* 99=DUMMY ATOM, 103-106 ARE SPARKLES
************************************************************************
      DATA SPACE, SPACE2/' ','  '/
C
      READ(5,'(A)')KEYWRD,KOMENT,TITLE
      ILOWA = ICHAR('a')
      ILOWZ = ICHAR('z')
      ICAPA = ICHAR('A')
************************************************************************
      DO 10 I=1,80
         ILINE=ICHAR(KEYWRD(I:I))
         IF(ILINE.GE.ILOWA.AND.ILINE.LE.ILOWZ) THEN
            KEYWRD(I:I)=CHAR(ILINE+ICAPA-ILOWA)
         ENDIF
   10 CONTINUE
************************************************************************
      IF(INDEX(KEYWRD,'ECHO').NE.0)THEN
         REWIND 5
         DO 30 I=1,1000
            READ(5,'(A)',END=40)KEYWRD
            DO 20 J=80,2,-1
   20       IF(KEYWRD(J:J).NE.' ')GOTO 30
            J=1
   30    WRITE(6,'(1X,A)')KEYWRD(1:J)
      ENDIF
   40 REWIND 5
      IF(INDEX(KEYWRD,'ECHO').NE.0)WRITE(6,'(''1'')')
      READ(5,'(A)')KEYWRD,KOMENT,TITLE
************************************************************************
      DO 50 I=1,80
         ILINE=ICHAR(KEYWRD(I:I))
         IF(ILINE.GE.ILOWA.AND.ILINE.LE.ILOWZ) THEN
            KEYWRD(I:I)=CHAR(ILINE+ICAPA-ILOWA)
         ENDIF
   50 CONTINUE
************************************************************************
      IF(KEYWRD(1:1) .NE. SPACE) THEN
         CH=KEYWRD(1:1)
         KEYWRD(1:1)=SPACE
         DO 60 I=2,80
            CH2=KEYWRD(I:I)
            KEYWRD(I:I)=CH
            CH=CH2
            IF(KEYWRD(I:I+1) .EQ. SPACE2) GOTO 70
   60    CONTINUE
   70    CONTINUE
      ENDIF
      NOCHEK=(INDEX(KEYWRD,'EXTER').NE.0)
      CALL GETGEO(5,LABELS,GEO,LOPT,NA,NB,NC,AMS,NATOMS,INT)
C
C
C OUTPUT FILE TO UNIT 6
C
C    WRITE HEADER
      WRITE(6,'(1X,16(''*****''))')
      WRITE(6,'('' *** FRANK J SEILER RES. LAB., U.S. '',
     +''AIR FORCE ACADEMY, COLO. SPGS., CO. 80840 ***'')')
      IF(INDEX(KEYWRD,'MINDO') .NE. 0) THEN
         WRITE(6,'(1X,16(''*****'')//29X,''MINDO/3 CALCULATION RESULT
     1S'',      28X,///1X,16(''*****'') )')
      ELSEIF(INDEX(KEYWRD,'AM1') .NE. 0) THEN
         WRITE(6,'(1X,16(''*****'')//29X,''AM1 CALCULATION RESULTS'',
     1      28X,///1X,16(''*****'') )')
      ELSE
         WRITE(6,'(1X,16(''*****'')//29X,''MNDO CALCULATION RESULTS''
     1,      28X,///1X,16(''*****'') )')
      ENDIF
      WRITE(6,'('' *'',20X,''VERSION '',F5.2)')VERSON
C
C CHECK DATA
C
      DO 80 I=1,NATOMS
         IF(.NOT. NOCHEK) THEN
            IF (IZOK(LABELS(I)) .EQ. 0 ) THEN
               WRITE(6,'('' ATOMIC NUMBER '',I3,'' IS NOT AVAILABLE '',
     1        ''IN MNDO'')') LABELS(I)
               CALL EXIT
            END IF
         ENDIF
         IF (LABELS(I) .LE. 0 ) THEN
            WRITE(6,'('' ATOMIC NUMBER OF '',I3,'' ?'')') LABELS(I)
            CALL EXIT
         END IF
         IF (  NA(I).GE.I.OR. NB(I).GE.I.OR. NC(I).GE.I
     1  .OR. (NA(I).EQ.NB(I))   .AND. I.GT.1
     2  .OR. (NA(I).EQ.NC(I).OR.NB(I).EQ.NC(I))  .AND. I.GT.2
     3    ) THEN
            WRITE(6,'('' ATOM NUMBER '',I3,'' IS ILLDEFINED'')') I
            CALL EXIT
         END IF
   80 CONTINUE
C
C WRITE KEYWORDS BACK TO USER AS FEEDBACK
      CALL WRTKEY(KEYWRD)
      WRITE(6,'(1X,80(''*''))')
C
C CONVERT ANGLES TO RADIANS
      DO 90 I=1,NATOMS
         DO 90 J=2,3
            GEO(J,I) = GEO(J,I) * 0.01745329252D00
   90 CONTINUE
C
C FILL IN GEO MATRIX IF NEEDED
      NDEP=0
      IF( INDEX(KEYWRD,'SYM') .NE. 0) CALL GETSYM
      IF(NDEP.NE.0) CALL SYMTRY
C
C INITIALIZE FLAGS FOR OPTIMIZE AND PATH
      IFLAG = 0
      NVAR  = 0
      LATOM = 0
      NUMAT=0
      DO 120 I=1,NATOMS
         IF(LABELS(I).NE.99.AND.LABELS(I).NE.107)NUMAT=NUMAT+1
         DO 120 J=1,3
            IF (LOPT(J,I) ) 100, 120, 110
C    FLAG FOR PATH
  100       CONVRT=1.D0
            IF ( IFLAG .NE. 0 ) THEN
               IF(INDEX(KEYWRD,'STEP1').NE.0)THEN
                  LPARA1=LPARAM
                  LATOM1=LATOM
                  LPARA2=J
                  LATOM2=I
                  LATOM=0
                  IFLAG=0
                  GOTO 120
               ELSE
                  WRITE(6,'('' ONLY ONE REACTION COORDINATE PERMITTED'')
     1')
                  STOP
               ENDIF
            END IF
            LATOM  = I
            LPARAM = J
            IF(J.GT.1) CONVRT=0.01745329252D00
            REACT(1)  = GEO(J,I)
            IREACT=1
            IFLAG = 1
            GO TO 120
C    FLAG FOR OPTIMIZE
  110       NVAR = NVAR + 1
            LOC(1,NVAR) = I
            LOC(2,NVAR) = J
            XPARAM(NVAR)   = GEO(J,I)
  120 CONTINUE
C READ IN PATH VALUES
      IF(IFLAG.EQ.0) GO TO 160
  130 READ(5,'(A)',END=150) LINE
      CALL NUCHAR(LINE,VALUE,NREACT)
      DO 140 I=1,NREACT
         IJ=IREACT+I
         IF(IJ.GT.100)THEN
            WRITE(6,'(///,''    ONLY ONE HUNDRED POINTS ALLOWED IN REACT
     1ION'','' COORDINATE'')')
            STOP
         ENDIF
  140 REACT(IJ)=VALUE(I)*CONVRT
      IREACT=IREACT+NREACT
      GO TO 130
  150 CONTINUE
      DEGREE=1.D0
      IF(LPARAM.GT.1)DEGREE=57.29578D0
      IF(IREACT.LE.1) THEN
         WRITE(6,'(//10X,'' NO POINTS SUPPLIED FOR REACTION PATH'')')
         WRITE(6,'(//10X,'' GEOMETRY AS READ IN IS AS FOLLOWS'')')
         CALL GEOUT
         STOP
      ELSE
         WRITE(6,'(//10X,'' POINTS ON REACTION COORDINATE'')')
         WRITE(6,'(10X,8F8.2)')(REACT(I)*DEGREE,I=1,IREACT)
      ENDIF
      IEND=IREACT+1
      REACT(IEND)=-1.D12
C
C OUTPUT GEOMETRY AS FEEDBACK
C
  160 WRITE(6,'(1X,A)')KEYWRD,KOMENT,TITLE
      CALL GEOUT
      IF (INDEX(KEYWRD,'PARAM')+INDEX(KEYWRD,'NOXYZ') .EQ. 0) THEN
         CALL GMETRY(GEO,COORD)
         WRITE(6,'(//10X,''CARTESIAN COORDINATES '',/)')
         WRITE(6,'(4X,''NO.'',7X,''ATOM'',9X,''X'',
     1  9X,''Y'',9X,''Z'',/)')
         L=0
         DO 170 I=1,NATOMS
            IF(LABELS(I) .EQ. 99.OR.LABELS(I).EQ.107) GOTO 170
            L=L+1
            WRITE(6,'(I6,7X,I3,4X,3F10.4)')
     1  L,LABELS(I),(COORD(J,L),J=1,3)
  170    CONTINUE
      END IF
      IF(   INDEX(KEYWRD,' XYZ') .NE.0)THEN
         IF( INT.AND.(NDEP .NE. 0 .OR.  NVAR.LT.3*NUMAT-6)) THEN
            IF(NDEP.NE.0)
     1WRITE(6,'(//10X,'' INTERNAL COORDINATES READ IN, AND SYMMETRY''
     2,/10X,'' SPECIFIED, BUT CALCULATION TO BE RUN IN CARTESIAN ''
     3,''COORDINATES'')')
            IF(NVAR.LT.3*NUMAT-6)
     1WRITE(6,'(//10X,'' INTERNAL COORDINATES READ IN, AND'',
     2'' CALCULATION '',/10X,''TO BE RUN IN CARTESIAN COORDINATES, '',
     3/10X,''BUT NOT ALL COORDINATES MARKED FOR OPTIMISATION'')')
            WRITE(6,'(//10X,'' THIS INVOLVES A LOGICALLLY ABSURD CHOICE'
     1',/10X,'' SO THE CALCULATION IS TERMINATED AT THIS POINT'')')
            STOP
         ENDIF
         SUMX=0.D0
         SUMY=0.D0
         SUMZ=0.D0
         DO 180 J=1,NUMAT
            SUMX=SUMX+COORD(1,J)
            SUMY=SUMY+COORD(2,J)
  180    SUMZ=SUMZ+COORD(3,J)
         SUMX=SUMX/NUMAT
         SUMY=SUMY/NUMAT
         SUMZ=SUMZ/NUMAT
         DO 190 J=1,NUMAT
            GEO(1,J)=COORD(1,J)-SUMX
            GEO(2,J)=COORD(2,J)-SUMY
  190    GEO(3,J)=COORD(3,J)-SUMZ
         NA(1)=99
         J=0
         NVAR=1
         DO 210 I=1,NATOMS
            IF(LABELS(I).NE.99)THEN
               J=J+1
  200          IF(LOC(1,NVAR) .EQ. I) THEN
                  XPARAM(NVAR)=GEO(LOC(2,NVAR),J)
C#                      LOC(2,NVAR)=K
                  LOC(1,NVAR)=J
                  NVAR=NVAR+1
                  GOTO 200
               ENDIF
               LABELS(J)=LABELS(I)
            ENDIF
  210    CONTINUE
         NVAR=NVAR-1
         NATOMS=NUMAT
      ELSE
         IF( .NOT. INT.AND.(NDEP .NE. 0 .OR.  NVAR.LT.3*NUMAT-6)) THEN
            IF(NDEP.NE.0)
     1WRITE(6,'(//10X,'' CARTESIAN COORDINATES READ IN, AND SYMMETRY''
     2,/10X,'' SPECIFIED, BUT CALCULATION TO BE RUN IN INTERNAL ''
     3,''COORDINATES'')')
            IF(NVAR.LT.3*NUMAT-6)
     1WRITE(6,'(//10X,'' CARTESIAN COORDINATES READ IN, AND'',
     2'' CALCULATION '',/10X,''TO BE RUN IN INTERNAL COORDINATES, '',
     3/10X,''BUT NOT ALL COORDINATES MARKED FOR OPTIMISATION'')')
            WRITE(6,'(//10X,''MOPAC, BY DEFAULT, USES INTERNAL COORDINAT
     1ES'',/10X,''TO SPECIFY CARTESIAN COORDINATES USE KEY-WORD :XYZ:'',
     2/10X,''YOUR CURRENT CHOICE OF KEY-WORDS INVOLVES A LOGICALLLY '',
     3/10X,''ABSURD CHOICE SO THE CALCULATION IS TERMINATED AT THIS ''
     4,''POINT'')')
            STOP
         ENDIF
      ENDIF
      RETURN
      END
