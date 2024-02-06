      SUBROUTINE KEYFLG
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'KEYS.i'
      INCLUDE 'FFILES.i'                                                GDH1095
      INCLUDE 'SIZES.i'                                                 GDH1196
C******************************************************************************
C
C      THIS SUBROUTINE CREATED BY DJG 9/95.  ALL KEYWRODS SHOULD BE
C      CHECKED FOR IN THIS SUBROUTINE AND NOT IN OTHER PARTS OF THE
C      PROGRAM.   COMMON BLOCK NAMES INVOLVING THESE KEYWORDS SHOULD
C      END IN 'WD' FOR WORD.
C
C      ALL KEYWORD FLAGS SHOULD BE LOGICAL VARIABLES
C
C      ALL KEYWORDS SHOULD BE SEARCHED FOR THE FULL
C      STRING.
C
C      GASENG CONTAINS THE GAS PHASE ENERGY
C      TITLE IS THE Z-MATRIX TITLE LINE
C      KOMENT IS THE Z-MATRIX COMMENT LINE
C      WORD IS THE KEYWORD STRING
C
C******************************************************************************
      COMMON /ASOLCM/ GASENG
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),               GDH1196
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)           GDH1196
      CHARACTER WORD*15, KEY*80, KOMENT*80, TITLE*80,KEY2*80            GDH1196
      CHARACTER SPACE*1, TEMPKEY*80                                     BJL1003
C
C     FILE JSCR IS A SCRATCH FILE THAT CONTAINS ALL THE KEYWORDS        GDH1195
C     BOTH FROM THE .DAT AND .XKW FILE.  IT DISAPPEARS WHEN THE JOB
C     TERMINATES (STATUS='SCRATCH').
C
      CALL KFINIT                                                       GDH1/96
      SPACE=' '
      OPEN(UNIT=JSCR,STATUS='UNKNOWN')                                  GDH0196
      REWIND(JSCR)                                                      GDH1195
      NFILE=JDAT                                                        GDH1095
      DO 15 I=1,NUMATM                                                  GDH1196
15       ITYPE(I)=0.D0                                                  GDH1196
10    FORMAT(A80)
20    FORMAT(A8)
      KEY(1:40)='                                        '              GDH1195
      KEY(40:80)='                                        '             GDH1195
      READ(JDAT,10,END=400) KEY                                         GDH1195
C
C     CONVERT LOWER CASE TO UPPER CASE
C
      I=0                                                               GDH1195
60    CALL CAPCNV(KEY,NUMUD,80)                                         GDH1195
      IF (KEY(1:4).NE.'ATOM') WRITE(JSCR,10) KEY                        GDH1196
C
C   KFLAG = 0 IF IN BETWEEN WORDS
C         = 1 IF IN THE MIDDLE OF A WORD
C         =-1 IF JUST FINISHED A WORD
C
      I=I+1
      KFLAG=0
100   N=1
      WORD='               '
      KFLAG=0
110   CONTINUE
C
C     NON-VALUE KEYWORDS END WITH A SPACE
C
      IF (KEY(I:I).EQ.SPACE) GOTO 120
C
C     KEYWORDS WITH VALUES END IN '=' OR '('
C
      IF (KEY(I:I).NE.'='.AND.KEY(I:I).NE.'(') THEN
C
C        ADD CHARACTER ONTO CURRENT KEYWORD STRING
C
         WORD(N:N)=KEY(I:I)
         KFLAG=1
C
C        IF AT THE END OF A LINE AND IN A KEYWORD, PRINT WARNING
C
         IF (I.EQ.80) WRITE(JOUT,8000) WORD                             GDH1095
         N=N+1
         I=I+1
      ELSE
         IF (I.EQ.80) THEN
C
C        MUST BE AT END OF VALUE KEYWORD AND AT END OF LINE, SO NO
C        VALUE HAS BEEN ENTERED.  END WITH ERROR MESSAGE
C
            WRITE(JOUT,9000) WORD                                       GDH1095
            ISTOP=1                                                     GDH1095
            IWHERE=83                                                   GDH1095
            RETURN                                                      GDH1095
         ENDIF
C
C        END OF VALUE KEYWORD STRING, SO CHECK VALIDITY AND READ VALUE
C
         ILENKW=N-1                                                     GDH0496
         CALL FINDKW(KEY,WORD,I,ILENKW)                                 GDH0496
         IF (ISTOP.NE.0) RETURN                                         GDH1095
C
C        INCREASE LINE COUNTER TO SKIP KEYWORD VALUE AND RESET FLAGS
C
         J=INDEX(KEY(I:80),SPACE)
         I=I+J
         KFLAG=-1
      ENDIF
C
C     END OF KEYWORD STRING, BEGIN PROCESS AGAIN
C
      IF (KFLAG.EQ.-1.AND.I.LE.80) GOTO 100
C
C     IN THE MIDDLE OF KEYWORD STRING, KEEP GOING
C
      IF (I.LE.80) GOTO 110
C
C     AT END OF LINE, MUST BE THE END OF THE KEYWORD, SEE IF IT IS VALID
C
      IF (KFLAG.NE.-1) THEN                                             GDH0496
         ILENKW=N-1                                                     GDH0496
         CALL FINDKY(WORD,I,ILENKW)                                     GDH0496
      ENDIF                                                             GDH0496
      GOTO 200
C
C     SPACE FOUND, SO MUST BE AT END OF KEYWORD STRING
C     CHECK KEYWORD VALIDITY
C
120   IF (KFLAG.EQ.1) THEN
         ILENKW=N-1                                                     GDH0496
         CALL FINDKY(WORD,I,ILENKW)                                     GDH0496
         KFLAG=-1
      ENDIF
      I=I+1
C
C     END OF KEYWORD STRING, BEGIN PROCESS AGAIN
C
      IF (KFLAG.EQ.-1.AND.I.LE.80) GOTO 100
C
C     IN THE MIDDLE OF KEYWORD STRING, KEEP GOING
C
      IF (I.LE.80) GOTO 110
200   CONTINUE
C
C     READ NEW LINE FROM FILE IF LINE DOES NOT BEGIN WITH '&' THEN
C     KEYWORD SECTION IS OVER.
C
      KEY(1:40)='                                        '              GDH1195
      KEY(40:80)='                                        '             GDH1195
      READ (NFILE,10,END=400) KEY
      TEMPKEY(1:80)=KEY(1:80)                                           BJL1003
      CALL CAPCNV(KEY(1:4),NUMUD,4)                                     GDH1196
      IF(KEY(1:1).NE.'&'.AND.KEY(1:4).NE.'ATOM'.AND.KEY(1:4).NE.'TYPE'. GDH1196
     .   AND.NFILE.EQ.JDAT) GOTO 300                                    GDH1196
      IF (KEY(1:1).EQ.'&') THEN
         I=1
      ELSE
         I=0
      ENDIF
      IF (KEY(1:4).EQ.'TYPE') THEN                                      GDH1196
C                                                                       GDH1196
C   MUST HAVE ATOM LINE BEFORE TYPE LINE.  NO ATOM LINE                 GDH1196
C   FOUND.  END WITH ERROR MESSAGE (SM5.0 & SM1A)                       GDH1196
C                                                                       GDH1196
            WRITE(JOUT,9100)                                            GDH1196
            ISTOP=1                                                     GDH1196
            IWHERE=172                                                  GDH1196
            RETURN                                                      GDH1196
      ENDIF                                                             GDH1196
C                                                                       GDH1196
C       READ IN ATOM/CHEMICAL ENVIRONMENT TYPE SURFACE CORRECTION TERM  GDH1196
C       IDENTIFIERS                                                     GDH1196
C                                                                       GDH1196
      IF (KEY(1:4).EQ.'ATOM') THEN                                      GDH1196
         I=80                                                           GDH1196
         WRITE(JSCR,10) KEY                                             GDH1196
         READ (NFILE,10,END=400) KEY2                                   GDH1196
         CALL CAPCNV(KEY2,NUMUD,80)                                     GDH1196
         WRITE(JSCR,10) KEY2                                            GDH1196
         IF (KEY2(1:4).NE.'TYPE') THEN                                  GDH1196
C                                                                       GDH1196
C   MUST HAVE A TYPE LINE AFTER THE ATOM LINE.  NO TYPE LINE            GDH1196
C   FOUND.  END WITH ERROR MESSAGE                                      GDH1196
C                                                                       GDH1196
            WRITE(JOUT,9200)                                            GDH1196
            ISTOP=1                                                     GDH1196
            IWHERE=173                                                  GDH1196
            RETURN                                                      GDH1196
         ENDIF                                                          GDH1196
         I50F=0                                                         GDH1196
         I50C1=5                                                        GDH1196
         I50C2=5                                                        GDH1196
         DO WHILE (I50F.EQ.0)                                           GDH1196
         DO WHILE ((KEY(I50C1:I50C1).EQ.SPACE.OR.KEY(I50C1:I50C1)       GDH1196
     .             .EQ.',').AND.I50C1.LE.80)                            GDH1196
            I50C1=I50C1+1                                               GDH1196
         ENDDO                                                          GDH1196
         DO WHILE ((KEY2(I50C2:I50C2).EQ.SPACE.OR.KEY2(I50C2:I50C2)     GDH1196
     .             .EQ.',').AND.I50C2.LE.80)                            GDH1196
            I50C2=I50C2+1                                               GDH1196
         ENDDO                                                          GDH1196
         IF (I50C1.LE.80.AND.I50C2.LE.80) THEN                          GDH1196
            I50PL=READIF(KEY,I50C1)                                     GDH1196
            I50VL=READIF(KEY2,I50C2)                                    GDH1196
            ITYPE(I50PL)=I50VL                                          GDH1196
            DO WHILE (KEY(I50C1:I50C1).NE.SPACE.AND.I50C1.LE.80)        GDH1196
               I50C1=I50C1+1                                            GDH1196
            ENDDO                                                       GDH1196
            DO WHILE (KEY(I50C2:I50C2).NE.SPACE.AND.I50C2.LE.80)        GDH1196
               I50C2=I50C2+1                                            GDH1196
            ENDDO                                                       GDH1196
         ELSE IF (I50C1.LE.80.AND.I50C2.GT.80) THEN                     GDH1196
C                                                                       GDH1196
C   YOU HAVE MORE ATOMS DEFINED FOR TYPES THAN HAVE BEEN GIVEN          GDH1196
C   TYPES.  END WITH ERROR MESSAGE                                      GDH1196
C                                                                       GDH1196
            WRITE(JOUT,9300)                                            GDH1196
            ISTOP=1                                                     GDH1196
            IWHERE=174                                                  GDH1196
            RETURN                                                      GDH1196
         ELSE IF (I50C1.GE.80) THEN                                     GDH1196
            I50F=1                                                      GDH1196
         ENDIF                                                          GDH1196
         ENDDO                                                          GDH1196
      GOTO 200                                                          GDH0897
      ELSE                                                              GDH1196
      GOTO 60                                                           GDH1196
      ENDIF                                                             GDH1196
300   CONTINUE
C
C     END OF .DAT KEYWORDS.  IF XKW WAS USED, THEN READ FROM XKW FILE
C     PUT STRING 'XKW FILE' IN AS A SPACER IN SCRATCH FILE TO DISTINGUISH
C     BETWEEN .DAT KEYWORDS AND .XKW KEYWORDS (FOR ECHOING PURPOSES)
C
      IF (IXKW.NE.0) THEN
         WRITE(JSCR,20) 'XKW FILE'                                      GDH1095
         NFILE=JXKW
         REWIND NFILE
         GOTO 200
      ENDIF
400   CONTINUE
      NFILE=JDAT                                                        GDH1095
C      BACKSPACE(NFILE)
C  BACKSPACE not defined for direct access                              BJL1003

C
C     READ IN TITLE AND COMMENT LINES
C
C      READ(NFILE,10) TITLE                                             BJL1003
      TITLE(1:80)=TEMPKEY(1:80)                                         BJL1003
      READ(NFILE,10) KOMENT
C
      IF (ITRUES.GE.1.AND.IHFCAL.EQ.0.AND.IHF.LT.1) IHFCAL=1            GDH1/96
      IF (ITRUES.EQ.1.AND.IHF.GE.1) THEN                                GDH1/96
             GASENG=FIHF                                                GDH1195
      ELSE IF (ITRUES.EQ.2.AND.IHF.EQ.0) THEN                           GDH1/96
             ICR=1                                                      GDH1/96
      ELSE IF (ITRUES.EQ.0.AND.IHF.GE.1) THEN                           GDH1/96
             IHF=-2                                                     GDH1/96
      ENDIF                                                             GDH1/96
8000  FORMAT(/,/,1X,'POSSIBLE KEYWORD TRUNCATION AT END-OF-LINE ---> ',
     1       A15,/,/)
9000  FORMAT(/,1X,'KEYWORD TRUNCATION AT END-OF-LINE ---> ',A15)
9100  FORMAT(/,1X,'AN ATOM LINE MUST PROCEED A TYPE LINE (SM1A or'      GDH1196
     .        ,' SM5.0)', /,' SEE MANUAL')                              GDH1196
9200  FORMAT(/,1X,'A TYPE LINE MUST FOLLOW AN ATOM LINE (SM1A or'       GDH1196
     .        ,' SM5.0)', /,' SEE MANUAL')                              GDH1196
9300  FORMAT(/,1X,'THE NUMBER OF PROVIDED ATOMS REQUIRING TYPES '       GDH1196
     .        ,'AND THE NUMBER OF PROVIDED TYPES DO NOT MATCH',         GDH1196
     .         /,'SEE MANUAL')                                          GDH1196
      RETURN
      END
