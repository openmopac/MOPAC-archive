      SUBROUTINE GETGEO(IREAD,LABELS,GEO,LOPT,NA,NB,NC,AMS,NATOMS,INT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION GEO(3,*),NA(*),NB(*),NC(*),AMS(*), LOPT(3,*)
     1,LABELS(*)
      LOGICAL INT
************************************************************************
*
*   GETGEO READS IN THE GEOMETRY. THE ELEMENT IS SPECIFIED BY IT'S
*          CHEMICAL SYMBOL, OR, OPTIONALLY, BY IT'S ATOMIC NUMBER.
*
*  ON INPUT   IREAD  = CHANNEL NUMBER FOR READ, NORMALLY 5
*             AMS    = DEFAULT ATOMIC MASSES.
*
* ON OUTPUT LABELS = ATOMIC NUMBERS OF ALL ATOMS, INCLUDING DUMMIES.
*           GEO    = INTERNAL COORDINATES, IN ANGSTROMS, AND DEGREES.
*           LOPT   = INTEGER ARRAY, A '1' MEANS OPTIMIZE THIS PARAMETER,
*                    '0' MEANS DO NOT OPTIMIZE, AND A '-1' LABELS THE
*                    REACTION COORDINATE.
*           NA     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT)
*           NB     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT)
*           NC     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT)
*           ATMASS = ATOMIC MASSES OF ATOMS.
************************************************************************
      COMMON /ATMASS/ ATMASS(NUMATM)
      COMMON /KEYWRD/ KEYWRD
      DIMENSION ISTART(40), XYZ(3,NUMATM)
      LOGICAL LEADSP, IRCDRC
      CHARACTER*80 KEYWRD
      CHARACTER ELEMNT(107)*2, LINE*80, SPACE*1, NINE*1,ZERO*1,
     1TAB*1, COMMA*1, STRING*80, ELE*2, TURN*1
      DATA (ELEMNT(I),I=1,107)/'H','HE',
     1 'LI','BE','B','C','N','O','F','NE',
     2 'NA','MG','AL','SI','P','S','CL','AR',
     3 'K','CA','SC','TI','V','CR','MN','FE','CO','NI','CU',
     4 'ZN','GA','GE','AS','SE','BR','KR',
     5 'RB','SR','Y','ZR','NB','MO','TC','RU','RH','PD','AG',
     6 'CD','IN','SN','SB','TE','I','XE',
     7 'CS','BA','LA','CE','PR','ND','PM','SM','EU','GD','TB','DY',
     8 'HO','ER','TM','YB','LU','HF','TA','W','RE','OS','IR','PT',
     9 'AU','HG','TL','PB','BI','PO','AT','RN',
     1 'FR','RA','AC','TH','PA','U','NP','PU','AM','CM','BK','CF','XX',
     2 'FM','MD','CB','++','+','--','-','TV'/
      DATA COMMA,SPACE,NINE,ZERO/',',' ','9','0'/
      TAB=CHAR(9)
      IRCDRC=(INDEX(KEYWRD,'IRC')+INDEX(KEYWRD,'DRC') .NE.0)
      ILOWA = ICHAR('a')
      ILOWZ = ICHAR('z')
      ICAPA = ICHAR('A')
      NATOMS=0
      NUMAT=0
   10 READ(IREAD,'(A)',END=90,ERR=110)LINE
      IF(LINE.EQ.' ') GO TO 90
      IF(NATOMS.GT.NUMATM)THEN
         WRITE(6,'(//10X,''****  MAX. NUMBER OF ATOMS ALLOWED:'',I4)')
     1NUMATM
         STOP
      ENDIF
      NATOMS=NATOMS+1
*   CLEAN THE INPUT DATA
************************************************************************
      DO 20 I=1,80
         ILINE=ICHAR(LINE(I:I))
         IF(ILINE.GE.ILOWA.AND.ILINE.LE.ILOWZ) THEN
            LINE(I:I)=CHAR(ILINE+ICAPA-ILOWA)
         ENDIF
   20 CONTINUE
************************************************************************
      DO 30 I=1,80
   30 IF(LINE(I:I).LT.SPACE .OR.
     1       LINE(I:I).EQ.COMMA .OR.
     2       LINE(I:I).GT.CHAR(125))LINE(I:I)=SPACE
*
*   INITIALIZE ISTART TO INTERPRET BLANKS AS ZERO'S
      DO 40 I=1,10
   40 ISTART(I)=80
* FIND INITIAL DIGIT OF ALL NUMBERS, CHECK FOR LEADING SPACES FOLLOWED
*     BY A CHARACTER AND STORE IN ISTART
      LEADSP=.TRUE.
      NVALUE=0
      DO 50 I=1,80
         IF (LEADSP.AND.LINE(I:I).NE.SPACE) THEN
            NVALUE=NVALUE+1
            ISTART(NVALUE)=I
         END IF
         LEADSP=(LINE(I:I).EQ.SPACE)
   50 CONTINUE
*
* ESTABLISH THE ELEMENT'S NAME AND ISOTOPE, CHECK FOR ERRORS OR E.O.DATA
*
      WEIGHT=0.D0
      STRING=LINE(ISTART(1):ISTART(2)-1)
      IF( STRING(1:1) .GE. ZERO .AND. STRING(1:1) .LE. NINE) THEN
*  ATOMIC NUMBER USED: NO ISOTOPE ALLOWED
         LABEL=READA(STRING,1)
         IF (LABEL.EQ.0) GO TO 80
         IF (LABEL.LT.0.OR.LABEL.GT.107) THEN
            WRITE(6,'(''  ILLEGAL ATOMIC NUMBER'')')
            GO TO 120
         END IF
         GO TO 70
      END IF
*  ATOMIC SYMBOL USED
      REAL=ABS(READA(STRING,1))
      IF (REAL.LT.1.D-15) THEN
*   NO ISOTOPE
         ELE=STRING(1:2)
      ELSE
         WEIGHT=REAL
         IF( STRING(2:2) .GE. ZERO .AND. STRING(2:2) .LE. NINE) THEN
            ELE=STRING(1:1)
         ELSE
            ELE=STRING(1:2)
         END IF
      END IF
*   CHECK FOR ERROR IN ATOMIC SYMBOL
      IF(ELE(1:1).EQ.'-'.AND.ELE(2:2).NE.'-')ELE(2:2)=' '
      DO 60 I=1,107
         IF(ELE.EQ.ELEMNT(I)) THEN
            LABEL=I
            GO TO 70
         END IF
   60 CONTINUE
      WRITE(6,'(''  UNRECOGNIZED ELEMENT NAME: ('',A,'')'')')ELE
      GOTO 120
*
* ALL O.K.
*
   70 IF (LABEL.NE.99) NUMAT=NUMAT+1
      IF(WEIGHT.NE.0.D0)THEN
         WRITE(6,'('' FOR ATOM'',I4,''  ISOTOPIC MASS:''
     1    ,F15.5)')NATOMS, WEIGHT
         ATMASS(NUMAT)=WEIGHT
      ELSE
         IF(LABEL .NE. 99)  ATMASS(NUMAT)=AMS(LABEL)
      ENDIF
      LABELS(NATOMS)   =LABEL
      GEO(1,NATOMS)    =READA(LINE,ISTART(2))
      GEO(2,NATOMS)    =READA(LINE,ISTART(4))
      GEO(3,NATOMS)    =READA(LINE,ISTART(6))
      IF(IRCDRC)THEN
         TURN=LINE(ISTART(3):ISTART(3))
         IF(TURN.EQ.'T')THEN
            LOPT(1,NATOMS)=1
            IF(NATOMS.EQ.1)WRITE(6,'(A)')' IN DRC MONITOR POTENTIAL ENER
     1GY'//' TURNING POINTS'
         ELSE
            LOPT(1,NATOMS)=0
         ENDIF
         TURN=LINE(ISTART(5):ISTART(5))
         IF(TURN.EQ.'T')THEN
            LOPT(2,NATOMS)=1
         ELSE
            LOPT(2,NATOMS)=0
         ENDIF
         TURN=LINE(ISTART(7):ISTART(7))
         IF(TURN.EQ.'T')THEN
            LOPT(3,NATOMS)=1
         ELSE
            LOPT(3,NATOMS)=0
         ENDIF
      ELSE
         LOPT(1,NATOMS)   =READA(LINE,ISTART(3))
         LOPT(2,NATOMS)   =READA(LINE,ISTART(5))
         LOPT(3,NATOMS)   =READA(LINE,ISTART(7))
      ENDIF
      NA(NATOMS)       =READA(LINE,ISTART(8))
      NB(NATOMS)       =READA(LINE,ISTART(9))
      NC(NATOMS)       =READA(LINE,ISTART(10))
      GOTO 10
*
* ALL DATA READ IN, CLEAN UP AND RETURN
*
   80 NATOMS=NATOMS-1
   90 NA(2)=1
      IF(NATOMS.GT.3)THEN
         INT=(NA(4).NE.0)
      ELSE
         IF(GEO(2,3).LT.10.AND.NATOMS.EQ.3)
     1WRITE(6,'(//10X,'' WARNING: INTERNAL COORDINATES ARE ASSUMED -'',/
     210X,'' FOR THREE-ATOM SYSTEMS '',//)')
         INT=.TRUE.
      ENDIF
      IF(  .NOT. INT ) THEN
         DO 100 I=1,NATOMS
            DO 100 J=1,3
  100    XYZ(J,I)=GEO(J,I)
         DEGREE=90.D0/ASIN(1.D0)
         CALL XYZINT(XYZ,NATOMS,NA,NB,NC,DEGREE,GEO)
      ELSEIF (.NOT.IRCDRC) THEN
         IF(LOPT(1,1)+LOPT(2,1)+LOPT(3,1)+LOPT(2,2)+LOPT(3,2)+
     1        LOPT(3,3) .GT. 0)THEN
            LOPT(1,1)=0
            LOPT(2,1)=0
            LOPT(3,1)=0
            LOPT(2,2)=0
            LOPT(3,2)=0
            LOPT(3,3)=0
            WRITE(6,'(//10X,'' AN UNOPTIMIZABLE GEOMETRIC PARAMETER HAS'
     1',/10X,'' BEEN MARKED FOR OPTIMIZATION. THIS IS A NON-FATAL ''
     2,''ERROR'')')
         ENDIF
      ENDIF
      IF(NA(3).EQ.0) THEN
         NB(3)=1
         NA(3)=2
      ENDIF
      RETURN
* ERROR CONDITIONS
  110 IF(IREAD.EQ.5) THEN
         WRITE(6,'( '' ERROR DURING READ AT ATOM NUMBER '', I3 )')NATOMS
      ELSE
         NATOMS=0
         RETURN
      ENDIF
  120 J=NATOMS-1
      WRITE(6,'('' DATA CURRENTLY READ IN ARE '')')
      DO 130 K=1,J
  130 WRITE(6,140)LABELS(K),(GEO(J,K),LOPT(J,K),J=1,3),NA(K),NB(K),NC(K)
  140 FORMAT(I4,2X,3(F10.5,2X,I2,2X),3(I2,1X))
      STOP
      END
