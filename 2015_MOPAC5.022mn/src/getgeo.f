      SUBROUTINE GETGEO(IREAD,LABELS,GEO,LOPT,NA,NB,NC,AMS,NATOMS,INT,*) CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
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
      COMMON /PATHR / REACT(3,66), DUMM1,DUMM2                          IR0394
      COMMON /ATMASS/ ATMASS(NUMATM)
      COMMON /KEYWRD/ KEYWRD
      COMMON /HPUSED/ HPUSED                                            LF0210
      COMMON /ELEMTS/ ELEM(120)                                         LF0210
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      COMMON /INTEXT/ KEYTXT, COMTXT, TLETXT, SPCTXT(80),               LF0610
     &                CINTXT, FINTXT,                                   LF0610
     &                LINTXT                                            LF0610
      include 'method.f'
      CHARACTER*160 KEYTXT
      CHARACTER*80  COMTXT, TLETXT, SPCTXT
      INTEGER       CINTXT, FINTXT                                      LF0610
      LOGICAL       LINTXT                                              LF0610
      CHARACTER ELEM*2                                                  LF0210
      DIMENSION ISTART(40), XYZ(3,NUMATM), VALUE(4)
      LOGICAL LEADSP, IRCDRC
      CHARACTER*160 KEYWRD,NKEYWRD                                      LF1211
      CHARACTER ELEMNT(120)*2, LINE*160, SPACE*1, NINE*1,ZERO*1,
     1TAB*1, COMMA*1, STRING*80, ELE*2, TURN*1
      LOGICAL HPUSED                                                    LF0210
CSAV         SAVE                                                           GL0892
C     LF0210: Expanded the number of atom-type symbols in ELEMNT 
C             to 120 from 107.  Added element 108, 'Hp'.
      DATA (ELEMNT(I),I=1,120)/'H','HE',
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
     2 'FM','MD','CB','++','+','--','-','TV','HP','','','','','','','',
     3 '','','','',''/
      DATA COMMA,SPACE,NINE,ZERO/',',' ','9','0'/
      DATA ITAB /9/                                                     LF0710
C     LF0210: Adding a new element "Hp" for hydrogen with p orbitals as element
C     108.  When used all fluorines are treated as these type of
C     hydrogen atoms and so fluorine cannot be used simultaneous with
C     Hp atoms.  If Hp atoms specified HPUSED flag is set to true.
      HPUSED=.FALSE.                                                    LF0210
      TAB=CHAR(9)
      IRCDRC=(INDEX(KEYWRD,'IRC').NE.0.OR.INDEX(KEYWRD,'DRC').NE.0)
      ILOWA = ICHAR('a')
      ILOWZ = ICHAR('z')
      ICAPA = ICHAR('A')
      NATOMS=0
      NUMAT=0
      IF (LINTXT) CINTXT=1                                              LF0610
   10 CONTINUE                                                          LF0610
      IF (LINTXT) THEN                                                  LF0610
         IF (CINTXT.GT.FINTXT) GOTO 90                                  LF0610
         IF (CINTXT.GT.80) THEN                                         LF0912
            IF (DOPRNT) WRITE(6,'(A)')                                  LF0912
     &         ' TOO MANY GEOMETRY SPECIFICATION LINES IN INPUT FILE.'  LF0912
            RETURN 1                                                    LF0912
         ENDIF                                                          LF0912
         LINE=SPCTXT(CINTXT)                                            LF0610
         CINTXT=CINTXT+1                                                LF0610
      ELSE                                                              LF0610
         READ(IREAD,'(A)',END=90,ERR=180)LINE
      ENDIF                                                             LF0610
      IF(LINE.EQ.' ') GO TO 90
      NATOMS=NATOMS+1
      IF(NATOMS.GT.NUMATM)THEN
         IF (DOPRNT)                                                    CIO
     &    WRITE(6,'(//10X,''****  MAX. NUMBER OF ATOMS ALLOWED:'',I4)') CIO
     &    NUMATM                                                        CIO
      RETURN 1                                                           CSTP (stop)
      ENDIF
*   CLEAN THE INPUT DATA
************************************************************************
      NKEYWRD=KEYWRD                                                    LF1211
      DO 20 I=1,160
         ILINE=ICHAR(LINE(I:I))
         IF(ILINE.GE.ILOWA.AND.ILINE.LE.ILOWZ) THEN
            LINE(I:I)=CHAR(ILINE+ICAPA-ILOWA)
         IF(ILINE.EQ.ITAB) KEYWRD(I:I)=SPACE                            LF0710
         ENDIF
   20 CONTINUE
      IPOS=INDEX(NKEYWRD,'EXTERNAL=')                                   LF1211
      IF (IPOS.NE.0) THEN                                               LF1211
         IPOS2=INDEX(NKEYWRD(IPOS:),' ')                                LF1211
         NKEYWRD(IPOS:IPOS2-1)=KEYWRD(IPOS:IPOS2-1)                     LF1211
      ENDIF                                                             LF1211
      KEYWRD=NKEYWRD                                                    LF1211
************************************************************************
      ICOMMA=ICHAR(COMMA)
      ITAB=ICHAR(TAB)
      DO 30 I=1,160
      KHAR=ICHAR(LINE(I:I))
      IF(KHAR.EQ.ICOMMA.OR.KHAR.EQ.ITAB)LINE(I:I)=SPACE
  30  CONTINUE
*
*   INITIALIZE ISTART TO INTERPRET BLANKS AS ZERO'S
      DO 40 I=1,10
   40 ISTART(I)=160
* FIND INITIAL DIGIT OF ALL NUMBERS, CHECK FOR LEADING SPACES FOLLOWED
*     BY A CHARACTER AND STORE IN ISTART
      LEADSP=.TRUE.
      NVALUE=0
      DO 50 I=1,160
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
C        LF0210: Highest atomic number if now 108.
         IF (LABEL.LT.0.OR.LABEL.GT.108) THEN
            IF (DOPRNT) WRITE(6,'(''  ILLEGAL ATOMIC NUMBER'')')        CIO
            GO TO 190
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
      DO 60 I=1,120
         IF(ELE.EQ.ELEMNT(I)) THEN
            LABEL=I
            GO TO 70
         END IF
   60 CONTINUE
      IF (DOPRNT)                                                       CIO
     & WRITE(6,'(''  UNRECOGNIZED ELEMENT NAME: ('',A,'')'')')ELE       CIO
      GOTO 190
*
* ALL O.K.
*
   70 IF (LABEL.NE.99) NUMAT=NUMAT+1
C
C     LF0210: Check if p orbital-containing hydrogens are specified.
      IF (LABEL.EQ.108) THEN
         LABEL=9
C        When HPUSED flag is TRUE the element fluorine is treated 
C        as hydrogen with p orbitals.
         HPUSED=.TRUE.
         ELEMNT(9)='Hp'
         ELEM(9)='Hp'
      ENDIF
C
C     LF1010: Check if PMOv1 method is being used.  If so, convert H to
C             Hp atoms.  This also covers the case of PMOv1a too (keyword "PMOV1A" contains "PMOV1").
C     LF0113/LF0614: Check if the PMO2 or PMO2A method is being used.  If so, convert H
C             to Hp atoms.
C     Be careful, sometimes keywords will fall after EXTERNAL= in the input deck.
      LPMOV1= (KEYWRD(1:5).EQ.'PMOV1'.OR.(INDEX(KEYWRD,' PMOV1').NE.0))
      IF (KEYWRD(1:4).EQ.'PMO2') THEN                                   LF0614
        IF (KEYWRD(1:5).EQ.'PMO2A'.OR.INDEX(KEYWRD,' PMO2A').NE.0) THEN LF0614
          LPMO2A=.TRUE.                                                 LF0614
        ELSE                                                            LF0614
          LPMO2=.TRUE.                                                  LF0614
        ENDIF                                                           LF0614
      ENDIF                                                             LF0614
      IF(LABEL.EQ.1.AND.(LPMOV1.OR.LPMO2.OR.LPMO2A)) THEN               LF0614
         LABEL=9
         ELEMNT(9)='Hp'
         ELEM(9)='Hp'
         HPUSED=.TRUE.                                                  LF0113
      ENDIF
C
C     LF1010: Abandon calculation if PMOv1 method is being used with any
C             atoms other than hydrogen and oxygen.
      IF(LABEL.LT.99.AND.LABEL.NE.8.AND.LABEL.NE.9.AND.LPMOV1) THEN
         IF (DOPRNT) WRITE(6,'(A)') 
     &     '   CANNOT USE PMOv1 WITH ANY ELEMENTS OTHER THAN HYDROGEN'//
     &     ' AND OXYGEN.'
         RETURN 1
      ENDIF
C
C     LF0113: Abandon calculation if PMO2 method is being used with any
C             atoms other than hydrogen, carbon, and oxygen.
      IF(LABEL.LT.99.AND.LABEL.NE.8.AND.LABEL.NE.9.AND.LABEL.NE.6.AND.
     &   LPMO2) THEN
         IF (DOPRNT) WRITE(6,'(A)') 
     &     '   CANNOT USE PMO2 WITH ANY ELEMENTS OTHER THAN HYDROGEN'//
     &     ', CARBON, AND OXYGEN.'
         RETURN 1
      ENDIF
C
C
C     LF0614: Abandon calculation if PMO2a method is being used with any
C             atoms other than hydrogen, carbon, nitrogen, sulfur, and oxygen.
      IF(LABEL.LT.99.AND.LABEL.NE.6.AND.LABEL.NE.7.AND.LABEL.NE.8.AND.
     &   LABEL.NE.9.AND.LABEL.NE.16.AND.
     &   LPMO2A) THEN
         IF (DOPRNT) WRITE(6,'(A)') 
     &     '   CANNOT USE PMO2A WITH ANY ELEMENTS OTHER THAN HYDROGEN'//
     &     ', CARBON, NITROGEN, OXYGEN, AND SULFUR.'
         RETURN 1
      ENDIF
C
C ----

C
      IF(WEIGHT.NE.0.D0)THEN
         IF (DOPRNT) WRITE(6,'('' FOR ATOM'',I4,''  ISOTOPIC MASS:''    CIO
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
            IF(NATOMS.EQ.1)WRITE(6,'(A)')                               CIO
     &       ' IN DRC MONITOR POTENTIAL ENERGY'//' TURNING POINTS'      CIO
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
         IF(DOPRNT.AND.(GEO(2,3).LT.10.AND.NATOMS.EQ.3))                CIO
     1WRITE(6,'(//10X,'' WARNING: INTERNAL COORDINATES ARE ASSUMED -'',/
     210X,'' FOR THREE-ATOM SYSTEMS '',//)')
         INT=.TRUE.
      ENDIF
C
C     READ IN VELOCITY VECTOR, IF PRESENT
C
      IF(INDEX(KEYWRD,'VELO').GT.0)THEN
         IF(INT)THEN
            IF (DOPRNT) WRITE(6,'(A)')                                  CIO
     &  ' COORDINATES MUST BE CARTESIAN WHEN VELOCITY VECTOR IS USED.'  CIO
         RETURN 1                                                        CSTP (stop)
         ENDIF
C#      WRITE(6,'(/10X,A)')'INITIAL VELOCITY VECTOR FOR DRC'
         DO 110 I=1,NATOMS
            IF (LINTXT) THEN                                            LF0610
               LINE=SPCTXT(CINTXT)                                      LF0610
               CINTXT=CINTXT+1                                          LF0610
            ELSE                                                        LF0610
               READ(IREAD,'(A)') LINE                                   LF0610
            ENDIF                                                       LF0610
            CALL NUCHAR(LINE,VALUE,NDMY,*9999)                           CSTP(call)
            IF(NDMY.NE.3)THEN
               IF (DOPRNT) WRITE(6,'(/10X,A)')                          CIO
     1'  THERE MUST BE EXACTLY THREE VELOCITY DATA PER LINE'
      RETURN 1                                                           CSTP (stop)
            ENDIF
            DO 100 J=1,3
  100       REACT(J,I+2)=VALUE(J)
C#      WRITE(6,'(2X,A2,2X,3F13.5)')ELEMNT(LABELS(I)),(VALUE(J),J=1,3)
  110    CONTINUE
         DO 120 I=1,3
            DO 120 J=1,2
  120    REACT(I,J)=GEO(I,J+1)-GEO(I,1)
C
C  NOW TO ROTATE VELOCITY VECTOR TO SUIT INTERNAL COORDINATE DEFINITION
C
C
C   ROTATE AROUND THE 1-2 X-AXIS TO AS TO ELIMINATE REACT(3,2)
C   (PUT ATOM 2 IN X-Y PLANE)
         SA=REACT(3,1)/SQRT(REACT(2,1)**2+REACT(3,1)**2+1.D-20)
         CA=SIGN(SQRT(1.D0-SA**2),REACT(2,1))
C#      LABELS(NATOMS+1)=1
C#      LABELS(NATOMS+2)=1
C#      WRITE(6,*)' FIRST ROTATION, ABOUT 1-2 X-AXIS'
         DO 130 I=1,NATOMS+2
            TEMP1= REACT(2,I)*CA+REACT(3,I)*SA
            TEMP2=-REACT(2,I)*SA+REACT(3,I)*CA
            REACT(2,I)=TEMP1
            REACT(3,I)=TEMP2
C#      WRITE(6,'(2X,A2,2X,3F13.5)')ELEMNT(LABELS(I)),(REACT(J,I),J=1,3)
  130    CONTINUE
C   ROTATE AROUND THE 1-2 Z-AXIS TO AS TO ELIMINATE REACT(2,2)
C   (PUT ATOM 2 ON X AXIS)
         CA=REACT(1,1)/SQRT(REACT(2,1)**2+REACT(1,1)**2+1.D-20)
         SA=SIGN(SQRT(1.D0-CA**2),REACT(2,1))
C#      WRITE(6,*)' SECOND ROTATION, ABOUT 1-2 Z-AXIS'
         DO 140 I=1,NATOMS+2
            TEMP1= REACT(1,I)*CA+REACT(2,I)*SA
            TEMP2=-REACT(1,I)*SA+REACT(2,I)*CA
            REACT(1,I)=TEMP1
            REACT(2,I)=TEMP2
C#      WRITE(6,'(2X,A2,2X,3F13.5)')ELEMNT(LABELS(I)),(REACT(J,I),J=1,3)
  140    CONTINUE
C   ROTATE AROUND THE 2-3 X-AXIS TO AS TO ELIMINATE REACT(3,3)
C   (PUT ATOM 3 ON X-Y PLANE)
         SA=REACT(3,2)/SQRT(REACT(2,2)**2+REACT(3,2)**2+1.D-20)
         CA=SIGN(SQRT(1.D0-SA**2),REACT(2,2))
C#      WRITE(6,*)' THIRD ROTATION, ABOUT 2-3 X-AXIS'
         DO 150 I=1,NATOMS+2
            TEMP1= REACT(2,I)*CA+REACT(3,I)*SA
            TEMP2=-REACT(2,I)*SA+REACT(3,I)*CA
            REACT(2,I)=TEMP1
            REACT(3,I)=TEMP2
C#      WRITE(6,'(2X,A2,2X,3F13.5)')ELEMNT(LABELS(I)),(REACT(J,I),J=1,3)
  150    CONTINUE
C
C  STRIP OFF FIRST TWO COORDINATES; THESE WERE THE COORDINATE AXIS
C  DEFINITIONS
C
         DO 160 I=1,NATOMS
            DO 160 J=1,3
  160    REACT(J,I)=REACT(J,I+2)
      ENDIF
      IF(  .NOT. INT ) THEN
         DO 170 I=1,NATOMS
            DO 170 J=1,3
  170    XYZ(J,I)=GEO(J,I)
         DEGREE=90.D0/ASIN(1.D0)
         CALL XYZINT(XYZ,NATOMS,NA,NB,NC,DEGREE,GEO,*9999)              CSTP(call)
         IF(ABS(GEO(2,3)-180.D0).LT.1.D-4.OR.ABS(GEO(2,3)).LT.1.D-4)
     1THEN
            IF (DOPRNT) WRITE(6,'(A)')                                  CIO
     &         ' DUE TO PROGRAM BUG, THE FIRST THREE ATOMS'//           CIO
     &         ' MUST NOT LIE IN A STRAIGHT LINE.'                      CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
      ELSEIF (.NOT.IRCDRC) THEN
         IF(LOPT(1,1)+LOPT(2,1)+LOPT(3,1)+LOPT(2,2)+LOPT(3,2)+
     1        LOPT(3,3) .GT. 0)THEN
            LOPT(1,1)=0
            LOPT(2,1)=0
            LOPT(3,1)=0
            LOPT(2,2)=0
            LOPT(3,2)=0
            LOPT(3,3)=0
            IF (DOPRNT) THEN                                            CIO
            WRITE(6,'(//10X,'' AN UNOPTIMIZABLE GEOMETRIC PARAMETER HAS'
     1',/10X,'' BEEN MARKED FOR OPTIMIZATION. THIS IS A NON-FATAL ''
     2,''ERROR'')')
            ENDIF                                                       CIO
         ENDIF
      ENDIF
      IF(NA(3).EQ.0) THEN
         NB(3)=1
         NA(3)=2
      ENDIF
      RETURN
* ERROR CONDITIONS
  180 IF(IREAD.EQ.5) THEN
         IF (DOPRNT) WRITE(6,                                           CIO
     &      '( '' ERROR DURING READ AT ATOM NUMBER '', I3 )')NATOMS     CIO
      ELSE
         NATOMS=0
         RETURN
      ENDIF
  190 J=NATOMS-1
      IF (DOPRNT) WRITE(6,'('' DATA CURRENTLY READ IN ARE '')')         CIO
      DO 200 K=1,J
  200 IF (DOPRNT) WRITE(6,210)LABELS(K),(GEO(JJ,K),LOPT(JJ,K),JJ=1,3),  CIO
     1NA(K),NB(K),NC(K)
  210 FORMAT(I4,2X,3(F10.5,2X,I2,2X),3(I2,1X))
      RETURN 1                                                          CSTP (stop)
 9999 RETURN 1                                                          CSTP
      END
