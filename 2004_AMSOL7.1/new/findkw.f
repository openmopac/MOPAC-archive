      SUBROUTINE FINDKW(KEY,WORD,IND,ILENKW)                            GDH0496
C******************************************************************************
C
C      THIS SUBROUTINE CREATED BY DJG 9/95 TO FIND IF VALID
C      KEYWORDS THAT HAVE ARGUMENTS WERE USED AND TO SET
C      THE KEYWORD FLAGS AND VARIABLES
C
C      WORD = KEYWORD STRING PARSED FROM KEYWORD LINE
C      IND = CURRENT POSITION ON KEYWORD LINE
C      NFLAG = 0 IF STRING NOT A KEYWORD, 1 IF IT IS
C
C      KEYWORDS THAT BEGIN WITH A NUMBER ARE SEARCHED FOR IN TWO SPOTS
C      TO ALLOW FOR THE FACT THAT ON SOME MACHINES THE ASCII CODES
C      COME IN A DIFFERENT ORDER WHEN COMPARING LETTERS AND NUMBERS.
C      THEY ARE SEARCHED FOR AS LESS THAN A AND GREATER THAN Z
C
C      SHORT KEYWORDS THAT ARE IDENTICAL TO THE FIRST LETTERS OF A LONGER
C      KEYWORD (I.E. XXX & XXXYZ) SHOULD BE SEARCHED FOR AFTER THE LONGER
C      KEYWORD IF THEY ARE BOTH A VALUE OR NON-VALUE KEYWORD.
C
C      KEYWORD FLAGS ARE INTEGERS THAT BEGIN WITH 'I' AND ARE FOLLOWED
C      BY THE FIRST 5 LETTERS OF THE KEYWORD (OR FEWER IF THE KEYWORD
C      IS SMALLER).  EXCEPTIONS SHOULD BE LISTED IN THE INCLUDE FILE
C      'KEYS.i'
C
C      KEYWORD VALUE VARIABLES BEGIN WITH 'F', 'I', OR 'C' TO INDICATE
C      WHETHER THE VALUE IS A FLOATING POINT, INTEGER OR CHARACTER.
C      THE NEXT FIVE CHARACTERS ARE THE FIRST FIVE CHARACTERS OF THE
C      KEYWORD FLAG (INCLUDING THE INITIAL CHARACTER 'I').  EXCEPTIONS
C      SHOULD BE LISTED IN THE INCLUDE FILE 'KEYS.i'
C
C******************************************************************************

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'KEYS.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C
      COMMON /OPTIMI / IMP,IMP0
      COMMON /ONESCM/ ICONTR(100)
      CHARACTER WORD*15, L1*1, L2*2, KEY*80, CH*1
C
      IF (ICONTR(45).EQ.1) THEN
         ICONTR(45)=2
      ENDIF
      K=ILENKW
      L1=WORD(1:1)
      L2=WORD(2:2)
      NFLAG=0
      IF (L1.LT.'N') THEN
         IF (L1.LT.'G') THEN
            IF (L1.LT.'D') THEN
               IF (L1.LT.'B') THEN
                  IF (L1.NE.'A') THEN
C ######
                     NFLAG=0
                  ELSE
C AAAAAA
                     NFLAG=1                                            DJG0396
                     IF (WORD(1:K).EQ.'ALPHA') THEN                     DJG0396
                        IALPHA=1                                        DJG0396
                        FIALPH=READIF(KEY,IND)                          DJG0396
                     ELSE                                               DJG0396
                        NFLAG=0
                     ENDIF                                              DJG0396
                  ENDIF
               ELSE
                  IF (L1.EQ.'B') THEN
C BBBBBB
                     NFLAG=1
                     IF (WORD(1:K).EQ.'BETA') THEN                      DJG0396
                        IBETA=1                                         DJG0396
                        FIBETA=READIF(KEY,IND)                          DJG0396
                     ELSE IF (WORD(1:K).EQ.'BAR') THEN                  DJG0396
                        IBAR=1
                        FIBAR=READIF(KEY,IND)
                     ELSE
                        NFLAG=0
                     ENDIF
                  ELSE
C CCCCCCC
                     NFLAG=1
                     IF (WORD(1:K).EQ.'CHARGE') THEN
                        ICHARG=1
                        IICHAR=READIF(KEY,IND)
                     ELSE IF (WORD(1:K).EQ.'CYCLES') THEN
                        ICYCLE=1
                        IICYCL=READIF(KEY,IND)
                     ELSE IF (WORD(1:K).EQ.'C.I.') THEN
                        ICI=1
                        IICI=READIF(KEY,IND)
                     ELSE
                        NFLAG=0
                     ENDIF
                  ENDIF
               ENDIF
            ELSE IF (L1.EQ.'D') THEN
C DDDDDD
               NFLAG=1
               IF (WORD(1:K).EQ.'DIELEC') THEN
                  IDIELE=1
                  FIDIEL=READIF(KEY,IND)
               ELSE IF (WORD(1:K).EQ.'DDMIN') THEN
                  IDDMIN=1
                  FIDDMI=READIF(KEY,IND)
               ELSE IF (WORD(1:K).EQ.'DDMAX') THEN
                  IDDMAX=1
                  FIDDMA=READIF(KEY,IND)
               ELSE IF (WORD(1:K).EQ.'DMAX') THEN
                  IDMAX=1
                  FIDMAX=READIF(KEY,IND)
               ELSE IF (WORD(1:K).EQ.'DRC') THEN
                  IDRC=2
                  FIDRC=READIF(KEY,IND)
               ELSE IF (WORD(1:K).EQ.'DEPVAR') THEN
                  IDEPVA=1
                  FIDEPV=READIF(KEY,IND)
               ELSE
                  NFLAG=0
               ENDIF
            ELSE
               IF (L1.EQ.'E') THEN
C EEEEEE
                  NFLAG=1
                  IF (WORD(1:K).EQ.'EXTERNAL') THEN
                     IEXTER=1
                     J=INDEX(KEY(IND+1:),' ')+IND
                     CIEXTE=KEY(IND+1:J)
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE
C FFFFFF
                  NFLAG=1
                  IF (WORD(1:K).EQ.'FILL') THEN
                     IFILL=1
                     IIFILL=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'FACARB') THEN
                     IFACAR=1
                     FFACAR=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'FEHALO') THEN
                     IFEHAL=1
                     FFEHAL=READIF(KEY,IND)
                  ELSE
                     NFLAG=0
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            IF (L1.LT.'K') THEN
               IF (L1.LT.'I') THEN
                  IF (L1.EQ.'G') THEN
C GGGGGG
                     NFLAG=1
                     IF (WORD(1:K).EQ.'GCOMP') THEN                     GDH1/96
                        IGCOMP=1
                        FIGCOM=READIF(KEY,IND)
                     ELSE IF (WORD(1:K).EQ.'GAMMA') THEN                DJG0396
                        IGAMMA=1                                        DJG0396
                        FIGAMM=READIF(KEY,IND)                          DJG0396
                     ELSE IF (WORD(1:K).EQ.'GNORM') THEN
                        IGNORM=1
                        FIGNOR=READIF(KEY,IND)
                     ELSE                                               GDH1/96
                        NFLAG=0
                     ENDIF
                  ELSE
C HHHHHH
                     NFLAG=1
                     IF (WORD(1:K).EQ.'HESS') THEN
                        IHESS=1
                        IIHESS=READIF(KEY,IND)
                     ELSE IF (WORD(1:K).EQ.'HFCALC') THEN               GDH1/96
                          IF (KEY(IND+1:IND+3).EQ.'OPT') THEN           GDH1/96
                              IHFCAL=2                                  GDH1/96
                          ELSE IF (KEY(IND+1:IND+4).EQ.'1SCF') THEN     GDH1/96
                              IHFCAL=3                                  GDH1/96
                          ENDIF                                         GDH1/96
                     ELSE IF (WORD(1:K).EQ.'HFOPT') THEN                GDH1/96
                           IHF=1                                        GDH1/96
                           FIHF=READIF(KEY,IND)                         GDH1/96
                     ELSE IF (WORD(1:K).EQ.'HF1SCF') THEN               GDH1/96
                           IF (IHF.EQ.1) THEN                           GDH1/96
                              IHF=-7                                    GDH1/96
                           ELSE                                         GDH1/96
                              IHF=2                                     GDH1/96
                              FIHF=READIF(KEY,IND)                      GDH1/96
                           ENDIF                                        GDH1/96
                     ELSE IF (WORD(1:K).EQ.'H-PRIO') THEN
                        IHPRIO=2
                        IIHPRI=READIF(KEY,IND)
                     ELSE
                        NFLAG=0
                     ENDIF
                  ENDIF
               ELSE
                  IF (L1.EQ.'I') THEN
C IIIIII
                     NFLAG=1
                     IF (WORD(1:K).EQ.'IOFR') THEN                      DJG0396
                        IIOFR=1                                         DJG0396
                        FIIOFR=READIF(KEY,IND)                          DJG0396
                     ELSE IF (WORD(1:K).EQ.'ITRY') THEN                 DJG0396
                        IITRY=1
                        IIITRY=READIF(KEY,IND)
                     ELSE IF (WORD(1:K).EQ.'IUPD') THEN
                        IIUPD=1
                        IIIUPD=READIF(KEY,IND)
                     ELSE
                        NFLAG=0
                     ENDIF
                  ELSE
C JJJJJJ
                     NFLAG=0
                  ENDIF
               ENDIF
            ELSE IF (L1.EQ.'K') THEN
C KKKKKK
                  NFLAG=1
                  IF (WORD(1:K).EQ.'KICK') THEN
                     IKICK=2
                     IIKICK=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'KINETIC') THEN
                     IKINET=1
                     FIKINE=READIF(KEY,IND)
                  ELSE
                     NFLAG=0
                  ENDIF
            ELSE
               IF (L1.EQ.'L') THEN
C LLLLLL
                  NFLAG=0
               ELSE
C MMMMMM
                  NFLAG=1
                  IF (WORD(1:K).EQ.'MSURFT') THEN
                     IMSURF=1
                     FIMSUR=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'MODE') THEN
                     IMODE=1
                     IIMODE=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'MICROS') THEN
                     IMICRO=1
                     IIMICR=READIF(KEY,IND)
                  ELSE
                     NFLAG=0
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ELSE
         IF (L1.LT.'T') THEN
            IF (L1.LT.'Q') THEN
               IF (L1.EQ.'N') THEN
C NNNNNN
                  NFLAG=1
                  IF (WORD(1:K).EQ.'NDIVCD') THEN
                     INDIVC=1
                     IIDIVC=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'NDIVEP') THEN
                     INDIVE=1
                     IIDIVE=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'NDOTCD') THEN
                     INDOTC=1
                     IIDOTC=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'NDOTEP') THEN
                     INDOTE=1
                     IIDOTE=READIF(KEY,IND)
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE IF (L1.EQ.'O') THEN
C OOOOOO
                  NFLAG=1
                  IF (WORD(1:K).EQ.'OMIN') THEN
                     IOMIN=1
                     FIOMIN=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'OPEN') THEN
                     IOPEN=1
                     IIOPE1=READIF(KEY,IND)
                     IND2=INDEX(KEY(IND:),',') + IND
                     IIOPE2=READIF(KEY,IND2)
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE
C PPPPPP
                  NFLAG=1
                  IF (WORD(1:K).EQ.'PRINT') THEN
                     IPRINT=1
                     IIPRIN=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'PARSET') THEN
C                    PARSET AND SOLVNT ARE THE SAME KEYWORD
                     ISOLVN=1
                     IND2=IND+1
                     CALL READCH(KEY,IND2,CISOLV)
                  ELSE
                     NFLAG=0
                  ENDIF
               ENDIF
            ELSE
               IF (L1.EQ.'Q') THEN
C QQQQQQ
                  NFLAG=0
               ELSE IF (L1.EQ.'R') THEN
C RRRRRR
                  NFLAG=1
                  IF (WORD(1:K).EQ.'RECALC') THEN
                     IRECAL=1
                     IIRECA=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'ROT') THEN
                     IROT=1
                     IIROT=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'ROOT') THEN
                     IROOT=1
                     IIROOT=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'RAD') THEN
                     IRAD1=1
                     IIRAD=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'RMIN') THEN
                     IRMIN=1
                     FIRMIN=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'RMAX') THEN
                     IRMAX=1
                     FIRMAX=READIF(KEY,IND)
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE
C SSSSSS
                  NFLAG=1
                  IF (WORD(1:K).EQ.'SCFCRT') THEN
                     ISCFCR=1
                     FISCFC=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'SOLVNT') THEN
                     ISOLVN=1
                     IND2=IND+1
                     CALL READCH(KEY,IND2,CISOLV)
                  ELSE IF (WORD(1:K).EQ.'SVCDRD') THEN
                     ISVCDR=1
                     FISVCD=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'SVCSRD') THEN
                     ISVCSR=1
                     FISVCS=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'SHIFT') THEN
                     ISHIFT=1
                     FISHIF=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'STEP1') THEN
                     ISTEP1=1
                     FISTE1=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'STEP2') THEN
                     ISTEP2=1
                     FISTE2=READIF(KEY,IND)
                  ELSE
                     NFLAG=0
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            IF (L1.LT.'W') THEN
               IF (L1.EQ.'T') THEN
C TTTTTT
                  NFLAG=1
                  IF (WORD(1:K).EQ.'TLIMIT') THEN
                     ITLIMI=1
                     FITLIM=READIF(KEY,IND)
                     I=IND+1
                     MFLAG=0
100                  CH=KEY(IND:IND)
                     IF (CH.EQ.'M') MFLAG=1
                     I=I+1
                     IF (CH.NE.' '.AND.I.LE.80) GOTO 100
                     IF (MFLAG.EQ.1) FITLIM=FITLIM*60
                  ELSE IF (WORD(1:K).EQ.'THERMO') THEN
                     ITHERM=2
                     IITHE1=READIF(KEY,IND)
                     INDEND=INDEX(KEY(IND:),')')+ind
                     IND2=INDEX(KEY(IND:INDEND),',')
                     IF (IND2.NE.0) THEN
                        ITHERM=3
                        IND2=IND2+IND
                        IITHE2=READIF(KEY,IND2)
                     ENDIF
                     IND2=IND2+1
                     IND3=INDEX(KEY(IND2:INDEND),',')
                     IF (IND3.NE.0) THEN
                        ITHERM=4
                        IND3=IND3+IND2-1
                        IITHE3=READIF(KEY,IND3)
                     ENDIF
                  ELSE IF (WORD(1:K).EQ.'TDUMP') THEN
                     ITDUMP=1
                     FITDUM=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'TEXPN') THEN
                     ITEXPN=1
                     FITEXP=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'TONE') THEN
                     ITONE=1
                     FITONE=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'TRANS') THEN
                     ITRANS=1
                     IITRAN=READIF(KEY,IND)
                  ELSE IF (WORD(1:K).EQ.'T-PRIO') THEN
                     ITPRIO=2
                     FITPRI=READIF(KEY,IND)
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE IF (L1.EQ.'U') THEN
C UUUUUU
                  NFLAG=0
               ELSE
C VVVVVV
                  NFLAG=0
               ENDIF
            ELSE
               IF (L1.LT.'Y') THEN
                  IF (L1.EQ.'W') THEN
C WWWWWW
                     NFLAG=0
                  ELSE
C XXXXXX
                     NFLAG=1
                     IF (WORD(1:K).EQ.'X-PRIO') THEN
                        IXPRIO=2
                        FIXPRI=READIF(KEY,IND)
                     ELSE
                        NFLAG=0
                     ENDIF
                     NFLAG=0
                  ENDIF
               ELSE
                  IF (L1.EQ.'Y') THEN
C YYYYYY
                     NFLAG=0
                  ELSE IF (L1.EQ.'Z') THEN
C ZZZZZZ
                     NFLAG=0
                  ELSE
C ######
                     NFLAG=0
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF (NFLAG.EQ.0) THEN
         WRITE(JOUT,9000) WORD
            ISTOP=1                                                     GDH1095
            IWHERE=50                                                   GDH1095
            RETURN                                                      GDH1095
      ENDIF
9000   FORMAT(/,1X,'THERE IS AN UNRECOGNIZED STRING IN THE KEYWORD '
     1       ,'SECTION ---> ',A15)
      RETURN
      END
