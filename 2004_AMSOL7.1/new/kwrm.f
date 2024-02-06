      SUBROUTINE KWRM(CHARS1,NCHR1,HLDCHR,NLGTH,INUMB)                  GDH1/96
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GDH1/96
      INCLUDE 'KEYS.i'                                                  GDH1/96
      INCLUDE 'FFILES.i'                                                GDH1/96
      CHARACTER*80 CHARS1                                               GDH1/96
      CHARACTER*20 HLDCHR                                               GDH1/96
      CHARACTER*20 HLDCHR2                                              GDH1/96
      HLDCHR2='                    '                                    GDH1/96
      INUMB=0                                                           GDH1/96
      IWHR=INDEX(CHARS1(1:NCHR1),HLDCHR(1:NLGTH))                       GDH1/96
      IF (IWHR.NE.0) THEN                                               GDH1/96
         CHARS1(IWHR:IWHR+NLGTH-1)=HLDCHR2(1:NLGTH)                     GDH1/96
         INUMB=1                                                        GDH1/96
      ENDIF                                                             GDH1/96
      RETURN                                                            GDH1/96
      END                                                               GDH1/96
