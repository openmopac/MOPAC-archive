      SUBROUTINE GETGEO(IREAD,LABELS,GEO,LOPT,NA,NB,NC,AMS,NATOMS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION GEO(3,*),NA(*),NB(*),NC(*),AMS(*), LOPT(3,*)
     +,LABELS(*)
*****************************************************************************
*
*   GETGEO READS IN THE GEOMETRY. THE ELEMENT IS SPECIFIED BY IT'S CHEMICAL
*          SYMBOL, OR, OPTIONALLY, BY IT'S ATOMIC NUMBER.
*
*  ON INPUT   IREAD  = CHANNEL NUMBER FOR READ, NORMALLY 5
*             AMS    = DEFAULT ATOMIC MASSES.
*
*  ON OUTPUT  LABELS = ATOMIC NUMBERS OF ALL ATOMS, INCLUDING DUMMIES.
*             GEO    = INTERNAL COORDINATES, IN ANGSTROMS, AND DEGREES.
*             LOPT   = INTEGER ARRAY, A '1' MEANS OPTIMISE THIS PARAMETER,
*                      '0' MEANS DO NOT OPTIMISE, AND A '-1' LABELS THE
*                      REACTION COORDINATE.
*             NA     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT)
*             NB     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT)
*             NC     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT)
*             ATMASS = ATOMIC MASSES OF ATOMS.
*****************************************************************************
      COMMON /ATMASS/ ATMASS(NUMATM)
      DIMENSION ISTART(40)
      CHARACTER ELEMNT(99)*2, LINE*80, SPACE*1, NINE*1,ZERO*1,
     +TAB*1, COMMA*1, UPCASE*80, STRING*80, ELE*2
      DATA (ELEMNT(I),I=1,99)/'H','HE',
     2 'LI','BE','B','C','N','O','F','NE',
     3 'NA','MG','AL','SI','P','S','CL','AR',
     4 'K','CA','SC','TI','V','CR','MN','FE','CO','NI','CU',
     4 'ZN','GA','GE','AS','SE','BR','KR',
     5 'RB','SR','Y','ZR','NB','MO','TC','RU','RH','PD','AG',
     5 'CD','IN','SN','SB','TE','I','XE',
     6 'CS','BA','LA','CE','PR','ND','PM','SM','EU','GD','TB','DY',
     6 'HO','ER','TM','YB','LU','HF','TA','W','RE','OS','IR','PT',
     6 'AU','HG','TL','PB','BI','PO','AT','RN',
     7 'FR','RA','AC','TH','PA','U','NP','PU','AM','CM','BK','CF','XX'/
      DATA COMMA,TAB,SPACE,NINE,ZERO/',','	',' ','9','0'/
      NATOMS=0
      NUMAT=0
  10  READ(IREAD,'(A)',END=31,ERR=32)LINE
      IF(LINE.EQ.' ') GO TO 31
      IF(NATOMS.GT.NUMATM)THEN
      WRITE(6,'(//10X,''****  MAX. NUMBER OF ATOMS ALLOWED:'',I4)')
     +NUMATM
      STOP
      ENDIF
      NATOMS=NATOMS+1
*   CLEAN THE INPUT DATA
*****************************************************************************
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*  UPCASE IS MACHINE DEPENDENT CODE!!!! 
*  IF YOUR MACHINE DOES NOT HAVE LOWER CASE, REMOVE THE FOLLOWING LINE
*      LINE=UPCASE(LINE) 
       I=STR$UPCASE(LINE,LINE)
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*****************************************************************************
      DO 9 I=1,80
9       IF(LINE(I:I).EQ.TAB.OR.LINE(I:I).EQ.COMMA)LINE(I:I)=SPACE
*
*   INITIALIZE ISTART TO INTERPRET BLANKS AS ZERO'S
      DO 11 I=1,10
11       ISTART(I)=80  
* FIND INITIAL DIGIT OF ALL NUMBERS, CHECK FOR LEADING SPACES FOLLOWED
*     BY A CHARACTER AND STORE IN ISTART
      LEADSP=.TRUE.
      NVALUE=0
      DO 12 I=1,80
         IF (LEADSP.AND.LINE(I:I).NE.SPACE) THEN
           NVALUE=NVALUE+1
           ISTART(NVALUE)=I
         END IF
         LEADSP=(LINE(I:I).EQ.SPACE)
12    CONTINUE
*
* ESTABLISH THE ELEMENT'S NAME AND ISOTOPE, CHECK FOR ERRORS OR E.O.DATA
*
      WEIGHT=0.D0
      STRING=LINE(ISTART(1):ISTART(2)-1)
      IF( STRING(1:1) .GE. ZERO .AND. STRING(1:1) .LE. NINE) THEN
*  ATOMIC NUMBER USED: NO ISOTOPE ALLOWED
         LABEL=READA(STRING,1)
         IF (LABEL.EQ.0) GO TO 30
         IF (LABEL.LT.0.OR.LABEL.GT.99) THEN
           WRITE(6,'(''  ILLEGAL ATOMIC NUMBER'')')
           GO TO 33
         END IF
         GO TO 20
      END IF
*  ATOMIC SYMBOL USED
      REAL=READA(STRING,1)
      IF (REAL.LT..005) THEN
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
      DO 17 I=1,99
        IF(ELE.EQ.ELEMNT(I)) THEN
          LABEL=I
          GO TO 20
        END IF
17    CONTINUE
      WRITE(6,'(''  UNRECOGNIZED ELEMENT NAME:  <'',A,''>'')')ELE
      GOTO 33
*
* ALL O.K.
*
20    IF (LABEL.NE.99) NUMAT=NUMAT+1
      IF(WEIGHT.NE.0.D0)THEN
          WRITE(6,'('' FOR ATOM'',I4,''  ISOTOPIC MASS:''
     +    ,F12.5)')NATOMS, WEIGHT
          ATMASS(NUMAT)=WEIGHT
      ELSE
          IF(LABEL .NE. 99)  ATMASS(NUMAT)=AMS(LABEL)
      ENDIF      
      LABELS(NATOMS)   =LABEL
      GEO(1,NATOMS)    =READA(LINE,ISTART(2))
      LOPT(1,NATOMS)   =READA(LINE,ISTART(3))
      GEO(2,NATOMS)    =READA(LINE,ISTART(4))
      LOPT(2,NATOMS)   =READA(LINE,ISTART(5))
      GEO(3,NATOMS)    =READA(LINE,ISTART(6))
      LOPT(3,NATOMS)   =READA(LINE,ISTART(7))
      NA(NATOMS)       =READA(LINE,ISTART(8))
      NB(NATOMS)       =READA(LINE,ISTART(9))
      NC(NATOMS)       =READA(LINE,ISTART(10))
      GOTO 10
*
* ALL DATA READ IN, CLEAN UP AND RETURN
*
30      NATOMS=NATOMS-1
31      NA(2)=1
        IF(LOPT(1,1)+LOPT(2,1)+LOPT(3,1)+LOPT(2,2)+LOPT(3,2)+
     +     LOPT(3,3) .GT. 0)THEN
        LOPT(1,1)=0
        LOPT(2,1)=0
        LOPT(3,1)=0
        LOPT(2,2)=0
        LOPT(3,2)=0
        LOPT(3,3)=0
        WRITE(6,'(//10X,'' AN UNOPTIMIZABLE GEOMETRIC PARAMETER HAS''
     +,/10X,'' BEEN MARKED FOR OPTIMIZATION. THIS IS A NON-FATAL ''
     +,''ERROR'')')
      ENDIF
        IF(NA(3).EQ.0) THEN
            NB(3)=1
            NA(3)=2
        ENDIF
        RETURN
* ERROR CONDITIONS
  32  IF(IREAD.EQ.5) THEN
        WRITE(6,'( '' ERROR DURING READ AT ATOM NUMBER '', I3 )')NATOMS
      ELSE
        NATOMS=0
        RETURN
      ENDIF
  33  J=NATOMS-1
      WRITE(6,'('' DATA CURRENTLY READ IN ARE '')')
      DO 36 K=1,J
  36   WRITE(6,42)LABELS(K),(GEO(J,K),LOPT(J,K),J=1,3),NA(K),NB(K),NC(K)
  42   FORMAT(I4,2X,3(F10.5,2X,I2,2X),3(I2,1X))
      CALL EXIT
      END
