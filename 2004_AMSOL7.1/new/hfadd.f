      SUBROUTINE HFADD(CHARS1,NCHR1)                                    GDH1195
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GDH1195
      INCLUDE 'KEYS.i'                                                  GDH1195
      INCLUDE 'FFILES.i'                                                GDH1195
      CHARACTER*80 CHARS1                                               GDH1195
      CHARACTER HOLD*30,HOLD2*15,HLDCHR*20                              GDH1/96
      IF (IHF.EQ.(-3).OR.IHF.EQ.(-5)) THEN                              GDH1195
      HOLD(1:30)='                               '                      GDH0196
      HOLD2(1:15)='               '                                     GDH1195
      IF (I1SCF.EQ.1) THEN                                              GDH0196
         HOLD(1:8)=' HF1SCF='                                           GDH0196
         ICNT=9                                                         GDH0196
      ELSE                                                              GDH0196
         HOLD(1:7)=' HFOPT='                                            GDH0196
         ICNT=8                                                         GDH0196
      ENDIF                                                             GDH0196
      WRITE(HOLD2,'(F15.6)')FIHF                                        GDH1195
      NSPA=ICHAR(' ')                                                   GDH1195
      DO 10 J=1,15                                                      GDH1195
         IF (ICHAR(HOLD2(J:J)).NE.NSPA) THEN                            GDH1195
            HOLD(ICNT:ICNT)=HOLD2(J:J)                                  GDH1195
            ICNT=ICNT+1                                                 GDH1195
         ENDIF                                                          GDH1195
  10  CONTINUE                                                          GDH1195
      ENDIF                                                             GDH1195
      NFND=1                                                            GDH1195
      IF (IHF.EQ.(-3).OR.IHF.EQ.(-4)) THEN                              GDH1195
         IF (IHFCAL.EQ.2) THEN                                          GDH1/96
            HLDCHR='HFCALC=OPT'                                         GDH1/96
            NLGTH=10                                                    GDH1/96
         ELSE IF (IHFCAL.EQ.3) THEN                                     GDH1/96
            HLDCHR='HFCALC=1SCF'                                        GDH1/96
            NLGTH=11                                                    GDH1/96
         ENDIF                                                          GDH1/96
         CALL KWRM(CHARS1,NCHR1,HLDCHR,NLGTH,INUMB)                     GDH1/96
         IF (IHF.EQ.(-3).AND.INUMB.EQ.1) THEN                           GDH1/96
               IHF=-5                                                   GDH1195
         ELSE IF (IHF.EQ.(-4).AND.INUMB.EQ.1) THEN                      GDH1/96
               IHF=-6                                                   GDH1195
         ENDIF                                                          GDH1/96
      ENDIF                                                             GDH1195
      IF (IHF.EQ.(-3).OR.IHF.EQ.(-5)) THEN                              GDH1195
      NROOM=0                                                           GDH1195
      NPOS=-1                                                           GDH1195
      DO 80 J=1,NCHR1                                                   GDH1195
      IF (ICHAR(CHARS1(J:J)).NE.NSPA) THEN                              GDH1195
         NROOM=0                                                        GDH1195
      ELSE                                                              GDH1195
         NROOM=NROOM+1                                                  GDH1195
         IF (NROOM.GE.(ICNT+1)) THEN                                    GDH1195
            NPOS=J-NROOM+1                                              GDH1195
            DO 120 K=NPOS,NPOS+ICNT-1                                   GDH1195
120            CHARS1(K:K)=HOLD(K+1-NPOS:K+1-NPOS)                      GDH1195
               IF (IHF.EQ.(-3)) THEN                                    GDH1195
                  IHF=-4                                                GDH1195
               ELSE IF (IHF.EQ.(-5)) THEN                               GDH1195
                  IHF=-6                                                GDH1195
               ENDIF                                                    GDH1195
            GOTO 90                                                     GDH1195
         ENDIF                                                          GDH1195
      ENDIF                                                             GDH1195
 80   CONTINUE                                                          GDH1195
 90   CONTINUE                                                          GDH1195
      ENDIF                                                             GDH1195
      RETURN                                                            GDH1195
      END                                                               GDH1195
