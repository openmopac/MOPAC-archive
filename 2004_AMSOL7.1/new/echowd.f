      SUBROUTINE ECHOWD(NFILE,NTPRT,XKWFLG)                             GDH1195
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GDH1195
      INCLUDE 'KEYS.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C******************************************************************************
C
C    THIS SUBROUTINE CREATED BY DJG 9/95 TO ECHO THE KEYWORDS, TITLE AND
C    COMMENT LINE TO A FILE (NFILE)
C
C    IF XKWFLG=.TRUE. THEN ALSO ECHO THE KEYWORDS FROM THE XKW FILE
C
C******************************************************************************
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /ONESCM/ ICONTR(100)                                       GDH1195
      COMMON /CONVCM/ JCONV                                             GDH1195
      CHARACTER LINE*80, KOMENT*80, TITLE*80
      CHARACTER*800 HOLDER
      CHARACTER*20 HLDCHR                                               GDH1195
      LOGICAL XKWFLG, FIRST                                             GDH1195
      FIRST=.FALSE.                                                     GDH1195
      IF (ICONTR(60).EQ.1) THEN                                         GDH1195
         ICONTR(60)=0                                                   GDH1195
         NSZZ=1                                                         GDH1195
         FIRST=.TRUE.                                                   GDH1195
      ENDIF                                                             GDH1195
      K=1                                                               GDH1195
      KEYSCR=JSCR
      REWIND(KEYSCR)                                                    GDH1195
10    FORMAT(A80)
12    FORMAT(1X,A80)                                                    BJL1003
15    FORMAT(A28)
20    READ(KEYSCR,10,END=30) LINE
      IF (NFILE.EQ.JINP.AND.ICART.EQ.1) THEN                            GDH0796
         IPOSN=INDEX(LINE,"CART")                                       GDH0796
         IF (IPOSN.NE.0) THEN                                           GDH1297
            LINE(IPOSN:IPOSN+4)="    "                                  GDH1297
            ICART=0                                                     GDH1297
            IERASE=1                                                    PDW1199
            REWIND(KEYSCR)
            WRITE(KEYSCR,10) LINE                                       BJL1003
         ENDIF                                                          GDH1297
      ENDIF                                                             GDH0796
      IF (LINE(1:8).EQ.'XKW FILE'.AND..NOT.XKWFLG) GOTO 30
      IF (LINE(1:8).EQ.'XKW FILE') THEN
         GOTO 60                                         
      ELSE
         IF (NTPRT.EQ.0) WRITE(NFILE,10) LINE                           BJL1003
         IF (ISTDM.EQ.-1) THEN                                          GDH1195
            HLDCHR(1:4)='STDM'                                          GDH1195
            CALL KWADD(LINE,80,HLDCHR,4,ISTDM)                          GDH1195
         ENDIF                                                          GDH1195
         IF (IRESTA.EQ.-1) THEN                                         GDH1195
            HLDCHR(1:7)='RESTART'                                       GDH1195
            CALL KWADD(LINE,80,HLDCHR,7,IRESTA)                         GDH1195
         ENDIF                                                          GDH1195
         IF (IHF.LT.-2.AND.IHF.GT.-6.AND.(JCONV.EQ.1.OR.I1SCF.EQ.1))    GDH0196
     .      CALL HFADD(LINE,80)                                         GDH1195
         HOLDER(K:K+79)=LINE                                            GDH1195
         IF (NTPRT.EQ.1) WRITE(NFILE,10) HOLDER(K:K+79)                 GDH1195
            K=K+80                                                      GDH1195
         IF (NSZZ.LT.K-10)   NSZZ=NSZZ+80                               GDH1195
      ENDIF
      GOTO 20
30    CONTINUE
      IF ((IHF.LT.-2.AND.IHF.GT.-6).OR.IRESTA.EQ.-1.OR.ISTDM.EQ.-1) THENGDH1195
         IF (NSZZ.GT.K+10) THEN
            LINE=HOLDER(K:K+79)                                         GDH1195
         ELSE                                                           GDH1195
      LINE='&                                                           GDH1195
     .                    '                                             GDH1195
         ENDIF                                                          GDH1195
         IF (ISTDM.EQ.-1) THEN                                          GDH1195
            HLDCHR(1:4)='STDM'                                          GDH1195
            CALL KWADD(LINE,80,HLDCHR,4,ISTDM)                          GDH1195
         ENDIF                                                          GDH1195
         IF (IRESTA.EQ.-1) THEN                                         GDH1195
            HLDCHR(1:7)='RESTART'                                       GDH1195
            CALL KWADD(LINE,80,HLDCHR,7,IRESTA)                         GDH1195
         ENDIF                                                          GDH1195
         IF (IHF.LT.-2.AND.IHF.GT.-6.AND.(JCONV.EQ.1.OR.I1SCF.EQ.1))    GDH0196
     .      CALL HFADD(LINE,80)                                         GDH0196
         IF (NTPRT.EQ.1) WRITE(NFILE,10) LINE                           BJL1003
         HOLDER(K:K+79)=LINE                                            GDH1195
         K=K+80                                                         GDH1195
         IF (NSZZ.LT.K-10) NSZZ=NSZZ+80                                 GDH1195
      ENDIF                                                             GDH1195
      IF (NTPRT.EQ.1.AND.NSZZ.GT.K+10) THEN                             GDH1195
         WRITE(NFILE,10) HOLDER(K:K+79)                                 GDH1195
         K=K+80                                                         GDH1195
      ENDIF                                                             GDH1195
      WRITE(NFILE,12) TITLE                                             BJL1003
      WRITE(NFILE,12) KOMENT                                            BJL1003
      GOTO 90                                                           GDH1195
60    WRITE(NFILE,15) '********* XKW FILE *********'                    GDH1195
80    READ(KEYSCR,10,END=90) LINE                                       GDH1195
      WRITE(NFILE,12) LINE                                              BJL1003
      GOTO 80                                                           GDH1195
90    CONTINUE                                                          GDH1195
      RETURN
      END
