      LOGICAL FUNCTION WORD(KEYWRD,TESTWD)
      CHARACTER KEYWRD*160, TESTWD*(*)                                      LF0610
CSAV         SAVE                                                           GL0892
      WORD=.FALSE.
   10 J=INDEX(KEYWRD,TESTWD)
      IF(J.NE.0)THEN
   20    IF(KEYWRD(J:J).NE.' ')GOTO 30
         J=J+1
         IF (J.GT.LEN(KEYWRD)) RETURN                                   LF0311/LF0112
         GOTO 20
   30    WORD=.TRUE.
         DO 60 K=J,160
            IF(KEYWRD(K:K).EQ.'='.OR.KEYWRD(K:K).EQ.' ') THEN
C
C     CHECK FOR ATTACHED '=' SIGN
C
               J=K
               IF(KEYWRD(J:J).EQ.'=')GOTO 50
C
C     CHECK FOR SEPARATED '=' SIGN
C
               DO 40 J=K+1,160
                  IF(KEYWRD(J:J).EQ.'=') GOTO 50
   40          IF(KEYWRD(J:J).NE.' ')GOTO 10
C
C    THERE IS NO '=' SIGN ASSOCIATED WITH THIS KEYWORD
C
               GOTO 10
   50          KEYWRD(J:J)=' '
C
C   THERE MUST BE A NUMBER AFTER THE '=' SIGN, SOMEWHERE
C
               GOTO 20
            ENDIF
   60    KEYWRD(K:K)=' '
      ENDIF
      RETURN
      END
