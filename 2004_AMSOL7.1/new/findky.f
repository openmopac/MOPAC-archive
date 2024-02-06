      SUBROUTINE FINDKY(WORD,IND,ILENKW)                                GDH0695
C******************************************************************************
C
C      THIS SUBROUTINE CREATED BY DJG 9/95 TO FIND IF VALID
C      KEYWORDS WERE USED AND TO SET THE KEYWORD FLAGS.  THIS
C      SUBROUTINE HANDLES KEYWORDS THAT DO NOT HAVE VALUES
C      ASSOCIATED WITH THEM ON THE KEYWORD LINE.
C
C      WORD = KEYWORD STRING PARSED FROM KEYWORD LINE
C      IND = CURRENT POSITION ON KEYWORD LINE
C      NFLAG = 0 IF STRING NOT A KEYWORD, 1 IF IT IS
C
C      KEYWORDS THAT BEGIN WITH A NUMBER ARE SEARCHED FOR IN TWO SPOTS
C      TO ALLOW FOR THE FACT THAT ON SOME MACHINES THE ASCII CODES
C      COME IN A DIFFERENT ORDER WHEN COMPARING LETTERS AND NUMBERS.
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
C      KEYWORDS THAT HAVE VALUES ASSOCIATED WITH THEM (I.E. SCFCRT=1.0)
C      ARE HANDLED BY THE SUBROUTINE FINDKW
C
C******************************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'KEYS.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C
      COMMON /OPTIMI / IMP,IMP0
      CHARACTER WORD*15, L1*1, L2*2
C
      K=ILENKW                                                          GDH0496
      L1=WORD(1:1)
      L2=WORD(2:2)
      NFLAG=0
      IF (L1.LT.'N') THEN
         IF (L1.LT.'G') THEN
            IF (L1.LT.'D') THEN
               IF (L1.LT.'B') THEN
                  IF (L1.NE.'A') THEN
C ######
                     NFLAG=1
                     IF (WORD(1:K).EQ.'1SCF') THEN
                        I1SCF=1
                     ELSE IF (WORD(1:K).EQ.'1ELEC') THEN
                        I1ELEC=1
                     ELSE IF (WORD(1:K).EQ.'0SCF') THEN
                        I0SCF=1
                     ELSE
                        NFLAG=0
                     ENDIF
                  ELSE
C AAAAAA
                     NFLAG=1
                     IF (WORD(1:K).EQ.'AM1') THEN
                        IAM1=1
                     ELSE IF (WORD(1:K).EQ.'AREAS') THEN
                        IAREAS=1
                     ELSE IF (WORD(1:K).EQ.'ASA') THEN
                        IASA=1
                     ELSE
                        NFLAG=0
                     ENDIF
                  ENDIF
               ELSE
C BBBBBB
                  IF (L1.EQ.'B') THEN
                     NFLAG=1
                     IF (WORD(1:K).EQ.'BFGS') THEN
                        IBFGS=1
                     ELSE IF (WORD(1:K).EQ.'BIRAD') THEN
                        IBIRAD=1
                     ELSE IF (WORD(1:K).EQ.'BONDS') THEN
                        IBONDS=1
                     ELSE
                        NFLAG=0
                     ENDIF
                  ELSE
C CCCCCC
                     NFLAG=1
                     IF (WORD(1:K).EQ.'CM1') THEN
                        ICM1=1
                     ELSE IF (WORD(1:K).EQ.'CM2') THEN                  GDH0997
                        ICM2=1                                          GDH0997
                     ELSE IF (WORD(1:K).EQ.'CM3') THEN                  !JT0802
                        ICM3=1                                          !JT0802
                     ELSE IF (WORD(1:2).EQ.'CS') THEN
                        IF (WORD(3:K).EQ.'1') THEN
                           ICS1=1
C GDH0597                        ELSE IF (WORD(3:K).EQ.'2') THEN
C GDH0597                           ICS2=1
C GDH0597                        ELSE IF (WORD(3:K).EQ.'3') THEN
C GDH0597                           ICS3=1
                        ELSE
                           NFLAG=0
                        ENDIF
                     ELSE IF (WORD(1:K).EQ.'COMPFG') THEN
                        ICOMPF=1
                     ELSE IF (WORD(1:K).EQ.'CART') THEN
                        ICART=1
                     ELSE IF (WORD(1:K).EQ.'CHAIN') THEN
                        ICHAIN=1
                     ELSE IF (WORD(1:K).EQ.'CAMPKING') THEN
                        ICAMPK=1
                     ELSE IF (WORD(1:K).EQ.'CIS') THEN                            DL0397
                        ICIS=1                                                    DL0397
                     ELSE
                        NFLAG=0
                     ENDIF
                  ENDIF
               ENDIF
            ELSE IF (L1.EQ.'D') THEN
C DDDDDD
                  NFLAG=1
               IF (L2.EQ.'E') THEN
                  IF (WORD(1:K).EQ.'DEV') THEN
                     IDEV=1
                  ELSE IF (WORD(1:K).EQ.'DERINU') THEN
                     IDERIN=1
                  ELSE IF (WORD(1:K).EQ.'DEBUGPULAY') THEN
                     IDEBUP=1
                  ELSE IF (WORD(1:K).EQ.'DEBUG') THEN
                     IDEBUG=1
                  ELSE IF (WORD(1:K).EQ.'DENMAT') THEN
                     IDENMA=1
                  ELSE IF (WORD(1:K).EQ.'DENOUT') THEN
                     IDENOU=1
                  ELSE IF (WORD(1:K).EQ.'DENSITY') THEN
                     IDENSI=1
                  ELSE IF (WORD(1:K).EQ.'DEP') THEN
                     IDEP=1
                  ELSE IF (WORD(1:K).EQ.'DERI1') THEN
                     IDERI1=1
                  ELSE IF (WORD(1:K).EQ.'DERI2') THEN
                     IDERI2=1
                  ELSE IF (WORD(1:K).EQ.'DERISA') THEN
                     IDERIS=1
                  ELSE IF (WORD(1:K).EQ.'DERIV') THEN
                     IDERIV=1
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE
                  IF (WORD(1:K).EQ.'DFP') THEN
                     IDFP=1
                  ELSE IF (WORD(1:K).EQ.'DOUBLET') THEN
                     IDOUBL=1
                  ELSE IF (WORD(1:K).EQ.'DOTS') THEN
                     IDOTS=1
                  ELSE IF (WORD(1:K).EQ.'DERIV') THEN
                     IDERIV=1
                  ELSE IF (WORD(1:K).EQ.'DCART') THEN
                     IDCART=1
                  ELSE IF (WORD(1:K).EQ.'DFORCE') THEN
                     IDFORC=1
                  ELSE IF (WORD(1:K).EQ.'DFPSAV') THEN
                     IDFPSA=1
                  ELSE IF (WORD(1:K).EQ.'DRC') THEN
                     IDRC=1
                  ELSE
                     NFLAG=0
                  ENDIF
               ENDIF
            ELSE
               IF (L1.EQ.'E') THEN
C EEEEEE
                  NFLAG=1
                  IF (L2.EQ.'X') THEN
                     IF (WORD(1:K).EQ.'EXTSM') THEN
                        IEXTSM=1
                     ELSE IF (WORD(1:K).EQ.'EXTCM') THEN
                        IEXTCM=1
                     ELSE IF (WORD(1:K).EQ.'EXTM') THEN
                        IEXTM=1
                     ELSE IF (WORD(1:K).EQ.'EXCITED') THEN
                        IEXCIT=1
                     ELSE
                        NFLAG=0
                     ENDIF
                  ELSE
                     IF (WORD(1:K).EQ.'EFOLLOW') THEN
                        IEFOLL=1
                     ELSE IF (WORD(1:K).EQ.'EIGS') THEN
                        IEIGS=1
                     ELSE IF (WORD(1:K).EQ.'ESR') THEN
                        IESR=1
                     ELSE IF (WORD(1:K).EQ.'ENPART') THEN
                        IENPAR=1
                     ELSE
                        NFLAG=0
                     ENDIF
                  ENDIF
               ELSE
C FFFFFF
                  NFLAG=1
                  IF (WORD(1:K).EQ.'FORCE') THEN
                     IFORCE=1
                  ELSE IF (WORD(1:K).EQ.'FOCK') THEN
                     IFOCK=1
                  ELSE IF (WORD(1:K).EQ.'FLEPO') THEN
                     IFLEPO=1
                  ELSE IF (WORD(1:K).EQ.'FORWRD') THEN
                     IFORWR=1
                  ELSE IF (WORD(1:K).EQ.'FMAT') THEN
                     IFMAT=1
                  ELSE IF (WORD(1:K).EQ.'FULSCF') THEN
                     IFULSC=1
                  ELSE IF (WORD(1:K).EQ.'FAIL') THEN
                     IFAIL=1
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
                     IF (WORD(1:K).EQ.'GAS') THEN                       GDH1/96
                        IGAS=1                                          GDH1/96
                     ELSE IF (WORD(1:K).EQ.'GEO-OK') THEN               GDH1/96
                        IGEOOK=1
                     ELSE IF (WORD(1:K).EQ.'GRADIENTS') THEN
                        IGRADI=1
                     ELSE IF (WORD(1:K).EQ.'GNMIN') THEN
                        IGNMIN=1
                     ELSE IF (WORD(1:K).EQ.'GEPOL') THEN
                        IGEPOL=1
                     ELSE IF (WORD(1:K).EQ.'GPGCDS') THEN               GDH1/96
                        IGPGCD=1                                        GDH1/96
                     ELSE IF (WORD(1:K).EQ.'GRAPH') THEN
                        IGRAPH=1
                     ELSE
                        NFLAG=0
                     ENDIF
                  ELSE
C HHHHHH
                     NFLAG=1
                     IF (WORD(1:K).EQ.'HCORE') THEN
                        IHCORE=1
                     ELSE IF (WORD(1:K).EQ.'H-PRIO') THEN
                        IHPRIO=1
                     ELSE IF (WORD(1:K).EQ.'HYPERFINE') THEN
                        IHYPER=1
                     ELSE
                        NFLAG=0
                     ENDIF
                  ENDIF
               ELSE
                  IF (L1.EQ.'I') THEN
C IIIIII
                     NFLAG=1
                     IF (WORD(1:K).EQ.'INPUT') THEN
                        IINPUT=1
                     ELSE IF (WORD(1:K).EQ.'ITER') THEN
                        IITER=1
                     ELSE IF (WORD(1:K).EQ.'ISOTOPE') THEN
                        IISOTO=1
                     ELSE IF (WORD(1:K).EQ.'INTERP') THEN
                        IINTER=1
                     ELSE
                        NFLAG=0
                     ENDIF
                  ELSE
C JJJJJJ
                     NFLAG=0
                  ENDIF
               ENDIF
            ELSE
               IF (L1.EQ.'K') THEN
C KKKKKK
                  NFLAG=1
                  IF (WORD(1:K).EQ.'KICK') THEN
                     IKICK=1
                     IIKICK=1
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE IF (L1.EQ.'L') THEN
C LLLLLL
                  NFLAG=1
                  IF (WORD(1:K).EQ.'LET') THEN
                     ILET=1
                  ELSE IF (WORD(1:K).EQ.'LTRD') THEN
                     ILTRD=1
                  ELSE IF (WORD(1:K).EQ.'LOCALIZE') THEN
                     ILOCAL=1
                  ELSE IF (WORD(1:K).EQ.'LINMIN') THEN
                     ILINMI=1
                  ELSE IF (WORD(1:K).EQ.'LOCMIN') THEN
                     ILOCMI=1
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE
C MMMMMM
                  NFLAG=1
                  IF (WORD(1:K).EQ.'MECI') THEN
                     IMECI=1
                  ELSE IF (WORD(1:K).EQ.'MINDO/3') THEN
                     IMINDO=1
                  ELSE IF (WORD(1:K).EQ.'MNDO') THEN
                     IMNDO=1
                  ELSE IF (WORD(1:K).EQ.'MNDOC') THEN
                     IMNDOC=1
                  ELSE IF (WORD(1:K).EQ.'MOLDAT') THEN
                     IMOLDA=1
                  ELSE IF (WORD(1:K).EQ.'MULLIK') THEN
                     IMULLI=1
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
                  IF (WORD(1:K).EQ.'NOPOL') THEN
                     INOPOL=1
                  ELSE IF (WORD(1:K).EQ.'NOCOGS') THEN
                     INOCOG=1
                  ELSE IF (WORD(1:K).EQ.'NOXYZ') THEN
                     INOXYZ=1
                  ELSE IF (WORD(1:K).EQ.'NOINTER') THEN
                     INOINT=1
                  ELSE IF (WORD(1:K).EQ.'NOPRINTGEOM') THEN
                     IPRGEO=0
                  ELSE IF (WORD(1:K).EQ.'NOPRINTCOUL') THEN
                     IPRCOU=0
                  ELSE IF (WORD(1:K).EQ.'NOPRINTRAD') THEN
                     IPRRAD=0
                  ELSE IF (WORD(1:K).EQ.'NOPRINTPOL') THEN
                     IPRPOL=0
                  ELSE IF (WORD(1:K).EQ.'NEWTON') THEN
                     INEWTO=1
                  ELSE IF (WORD(1:K).EQ.'NLLSQ') THEN
                     INLLSQ=1
                  ELSE IF (WORD(1:K).EQ.'NONR') THEN
                     INONR=1
                  ELSE IF (WORD(1:K).EQ.'NOINP') THEN
                     INOINP=1
                  ELSE IF (WORD(1:K).EQ.'NOFOC') THEN                   GDH0496
                     INOFOC=1                                           GDH0496
                  ELSE IF (WORD(1:K).EQ.'NOTRUES') THEN                 GDH0496
                     INOTRU=1                                           GDH0496
                  ELSE IF (WORD(1:K).EQ.'NOVOL') THEN
                     INOVOL=1
                  ELSE IF (WORD(1:K).EQ.'NOREF') THEN
                     INOREF=1
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE IF (L1.EQ.'O') THEN
C OOOOOO
                  NFLAG=1
                  IF (WORD(1:K).EQ.'OPT') THEN
                     IOPT=1
                  ELSE IF (WORD(1:K).EQ.'OLDENS') THEN
                     IOLDEN=1
                  ELSE IF (WORD(1:K).EQ.'OLDMAT') THEN
                     IOLDMA=1
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE
C PPPPPP
                  NFLAG=1
                  IF (WORD(1:K).EQ.'PM3') THEN
                     IPM3=1
                  ELSE IF (WORD(1:K).EQ.'PRECISE') THEN
                     IPRECI=1
                  ELSE IF (WORD(1:K).EQ.'POWELL') THEN
                     IPOWEL=1
                  ELSE IF (WORD(1:K).EQ.'PULAY') THEN
                     IPULAY=1
                  ELSE IF (WORD(1:K).EQ.'PATH') THEN
                     IPATH=1
                  ELSE IF (WORD(1:K).EQ.'PISIGM') THEN
                     IPISIG=1
                  ELSE IF (WORD(1:K).EQ.'PLITER') THEN
                     IPLITE=1
                  ELSE IF (WORD(1:K).EQ.'POLAR') THEN
                     IPOLAR=1
                  ELSE IF (WORD(1:K).EQ.'POWSQ') THEN
                     IPOWSQ=1
                  ELSE IF (WORD(1:K).EQ.'PRINTGEOM') THEN
                     IPRGEO=1
                  ELSE IF (WORD(1:K).EQ.'PRINTCOUL') THEN
                     IPRCOU=1
                  ELSE IF (WORD(1:K).EQ.'PRINTRAD') THEN
                     IPRRAD=1
                  ELSE IF (WORD(1:K).EQ.'PRINTPOL') THEN
                     IPRPOL=1

                  ELSE
                     NFLAG=0
                  ENDIF
               ENDIF
            ELSE
               IF (L1.EQ.'Q') THEN
C QQQQQQ
                  NFLAG=1
                  IF (WORD(1:K).EQ.'QUARTET') THEN
                     IQUART=1
                  ELSE IF (WORD(1:K).EQ.'QUINTET') THEN
                     IQUINT=1
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE IF (L1.EQ.'R') THEN
C RRRRRR
                  NFLAG=1
                  IF (WORD(1:K).EQ.'RESTART') THEN
                     IRESTA=1
                  ELSE IF (WORD(1:K).EQ.'RSCAL') THEN
                     IRSCAL=1
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE
                  NFLAG=1
                  IF (L2.EQ.'M') THEN
C SSSSSSS
                     IF (WORD(1:K).EQ.'SM5.4U') THEN
                        ISM5U=1                                         DJG0796
                     ELSE IF (WORD(1:K).EQ.'SM5.4A') THEN
                        ISM5A=1
                     ELSE IF (WORD(1:K).EQ.'SM5.2PDA') THEN             GDH0596
                        IS5A2P=1                                        GDH0596
                     ELSE IF (WORD(1:K).EQ.'SM5.2PDP') THEN             GDH0596
                        IS5P2P=1                                        GDH0596
                     ELSE IF (WORD(1:K).EQ.'SM5.4P') THEN
                        ISM5P=1
                     ELSE IF (WORD(1:K).EQ.'SM4') THEN
                        ISM4=1
                     ELSE IF (WORD(1:K).EQ.'SM2.1') THEN
                        ISM21=1
                     ELSE IF (WORD(1:K).EQ.'SM3.1') THEN
                        ISM31=1
                     ELSE IF (WORD(1:K).EQ.'SM2.2') THEN
                        ISM22=1
                     ELSE IF (WORD(1:K).EQ.'SM2') THEN
                        ISM2=1
                     ELSE IF (WORD(1:K).EQ.'SM3') THEN
                        ISM3=1
                     ELSE IF (WORD(1:K).EQ.'SM1A') THEN
                        ISM1A=1
                     ELSE IF (WORD(1:K).EQ.'SM1') THEN
                        ISM1=1
                     ELSE IF (WORD(1:K).EQ.'SM5.2R') THEN               GDH0997
                        ISM52R=1                                        GDH0997
                     ELSE IF (WORD(1:K).EQ.'SM5.2') THEN                PDW1099
                        ISM52=1                                         PDW1099
                     ELSE IF (WORD(1:K).EQ.'SM5.42R') THEN              GDH1197
                        ISM54R=1                                        GDH0997
                     ELSE IF (WORD(1:K).EQ.'SM5.42') THEN               PDW1199
                        ISM542=1                                        PDW1199
                     ELSE IF (WORD(1:K).EQ.'SM5.0R') THEN               GDH0297
                        ISM50=1                                         GDH0297
                     ELSE IF (WORD(1:K).EQ.'SM5.05R') THEN              GDH0297
                        ISM505=1                                        GDH0297
                        ISM50=1                                         GDH0297
                     ELSE IF (WORD(1:K).EQ."SM5.4PDU") THEN             GDH0696
                        IPDS5U=1                                        DJG0796
                     ELSE IF (WORD(1:K).EQ.'SM5.4PDA') THEN             GDH0696
                        IPDS5A=1                                        GDH0696
                     ELSE IF (WORD(1:K).EQ.'SM5.4PDP') THEN             GDH0696
                        IPDS5P=1                                        GDH0696
                     ELSE IF (WORD(1:K).EQ.'SM2.2PDA') THEN             GDH0696
                        ISM23=1                                         GDH0696
                     ELSE
                        NFLAG=0
                     ENDIF
                  ELSE
                     IF (WORD(1:K).EQ.'SYMMETRY') THEN
                        ISYMME=1
                     ELSE IF (WORD(1:K).EQ.'STDM') THEN
                        ISTDM=1
                     ELSE IF (WORD(1:K).EQ.'SINGLET') THEN
                        ISINGL=1
                     ELSE IF (WORD(1:K).EQ.'SPART') THEN
                        ISPART=1
                     ELSE IF (WORD(1:K).EQ.'SADDLE') THEN
                        ISADDL=1
                     ELSE IF (WORD(1:K).EQ.'SPIN') THEN
                        ISPIN=1
                     ELSE IF (WORD(1:K).EQ.'SIGMA') THEN
                        ISIGMA=1
                     ELSE IF (WORD(1:K).EQ.'SEXTET') THEN
                        ISEXTE=1
                     ELSE IF (WORD(1:K).EQ.'SEARCH') THEN
                        ISEARC=1
                     ELSE
                        NFLAG=0
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            IF (L1.LT.'W') THEN
               IF (L1.EQ.'T') THEN
C TTTTTT
                  NFLAG=1
                  IF (WORD(1:K).EQ.'TRUES') THEN
                     ITRUES=1                                           GDH1/96
                  ELSE IF (WORD(1:K).EQ.'TRUSTG') THEN                  DJG0496
                     ITRUSG=1                                           DJG0496
                  ELSE IF (WORD(1:K).EQ.'TRUSTE') THEN                  DJG0496
                     ITRUSE=1                                           DJG0496
                  ELSE IF (WORD(1:K).EQ.'TSTATE') THEN
                     ITSTAT=1
                  ELSE IF (WORD(1:K).EQ.'TRIPLET') THEN
                     ITRIPL=1
                  ELSE IF (WORD(1:K).EQ.'THERMO') THEN
                     ITHERM=1
                  ELSE IF (WORD(1:K).EQ.'TRANS') THEN
                     ITRANS=1
                  ELSE IF (WORD(1:K).EQ.'T.V.') THEN
                     ITV=1
                  ELSE IF (WORD(1:K).EQ.'T-PRIO') THEN
                     ITPRIO=1
                  ELSE IF (WORD(1:K).EQ.'TIMES') THEN
                     ITIMES=1
                  ELSE IF (WORD(1:K).EQ.'TGO') THEN                     GDH0597
                     ITGO=1                                             GDH0597
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE IF (L1.EQ.'U') THEN
C UUUUUU
                  NFLAG=1
                  IF (WORD(1:K).EQ.'UHF') THEN
                     IUHF=1
                  ELSE
                     NFLAG=0
                  ENDIF
               ELSE
C VVVVVV
                  NFLAG=1
                  IF (WORD(1:K).EQ.'VECTORS') THEN
                     IVECTO=1
                  ELSE IF (WORD(1:K).EQ.'VOLUME') THEN
                     IVOLUM=1
                  ELSE
                     NFLAG=0
                  ENDIF
               ENDIF
            ELSE
               IF (L1.LT.'Y') THEN
                  IF (L1.EQ.'W') THEN
C WWWWWW
                     NFLAG=1
                     IF (WORD(1:K).EQ.'WEIGHT') THEN
                        IWEIGH=1
                     ELSE
                        NFLAG=0
                     ENDIF
                  ELSE
C XXXXXX
                     NFLAG=1
                     IF (WORD(1:K).EQ.'XKW') THEN
                        IXKW=1
                     ELSE IF (WORD(1:K).EQ.'XYZ') THEN
                        IXYZ=1
                     ELSE IF (WORD(1:K).EQ.'X-PRIO') THEN
                        IXPRIO=1
                     ELSE
                        NFLAG=0
                     ENDIF
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
                     NFLAG=1
                     IF (WORD(1:K).EQ.'1SCF') THEN
                        I1SCF=1
                     ELSE IF (WORD(1:K).EQ.'1ELEC') THEN
                        I1ELEC=1
                     ELSE IF (WORD(1:K).EQ.'0SCF') THEN
                        I0SCF=1
                     ELSE
                        NFLAG=0
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF (NFLAG.EQ.0) THEN
         WRITE(JOUT,9000) WORD
      ISTOP=1                                                           GDH1095
      IWHERE=64                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF
9000  FORMAT(/,1X,'THERE IS AN UNRECOGNIZED STRING IN THE KEYWORD '
     1       ,'SECTION ---> ',A15)
      RETURN
      END
