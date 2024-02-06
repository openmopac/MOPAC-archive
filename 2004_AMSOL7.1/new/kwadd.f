      SUBROUTINE KWADD(CHARS1,NCHR1,HLDCHR,NLGTH,INUMB)                 GDH1195
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GDH1195
      INCLUDE 'KEYS.i'                                                  GDH1195
      INCLUDE 'FFILES.i'                                                GDH1195
      CHARACTER*80 CHARS1                                               GDH1195
      CHARACTER*20 HLDCHR                                               GDH1195
      NSPA=ICHAR(' ')                                                   GDH1195
      ICNT=NLGTH+2                                                      GDH1195
      NROOM=0                                                           GDH1195
      NPOS=-1                                                           GDH1195
      DO 80 J=1,NCHR1                                                   GDH1195
      IF (ICHAR(CHARS1(J:J)).NE.NSPA) THEN                              GDH1195
         NROOM=0                                                        GDH1195
      ELSE                                                              GDH1195
         NROOM=NROOM+1                                                  GDH1195
         IF (NROOM.GE.ICNT) THEN                                        GDH1195
            NPOS=J-NROOM+2                                              GDH1195
            DO 120 K=NPOS,NPOS+NLGTH-1                                  GDH1195
               LPOS=K+1-NPOS                                            GDH1195
120            CHARS1(K:K)=HLDCHR(LPOS:LPOS)                            GDH1195
               INUMB=-2                                                 GDH1195
            GOTO 90                                                     GDH1195
         ENDIF                                                          GDH1195
      ENDIF                                                             GDH1195
 80   CONTINUE                                                          GDH1195
 90   CONTINUE                                                          GDH1195
      RETURN                                                            GDH1195
      END                                                               GDH1195
