      SUBROUTINE PRAREA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'KEYS.i'
      INCLUDE 'SIZES.i'
      INCLUDE 'FFILES.i'                                                GDH1095
        
C******************************************************************************
C
C  THIS SUBROUTINE PRINTS OUT THE SASA IN HANDY FORM FOR SM4 AND LATER
C  SOLVATION MODELS.
C
C  ATAR = ATOMIC SASA'S
C  AREAEL = SASA SUMMED BY ELEMENT
C  NATTYP = KEEPS TRACK OF WHICH ELEMENTS HAVE SASA
C  CSAREA = SASA ASSOCIATED WITH LARGE SOLVENT RADIUS
C  HBONDO = BOND-ORDER-DEPENDENT SASA FOR H TO EACH ELEMENT
C  SDEPA = SURFACE TENSION DEPENDENCE ON ALPHA
C  SDEPB = SURFACE TENSION DEPENDENCE ON BETA
C  SDEPN = SURFACE TENSION DEPENDENCE ON IOFR
C  HDEPA = H-ELEMENT SURFACE TENSION FOR ALPHA
C  HBEPB = H-ELEMENT SURFACE TENSION FOR BETA
C  HDEPN = H-ELEMENT SURFACE TENSION FOR IOFR
C
C******************************************************************************
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /HBONDA/ HBONDO(100),CBONDO,COBND5,CNBNDO,CBOND5,          GDH1196
     .                OOBNDO,ONBNDO,SSBNDO,OPBND5,CLCBND,BRCBND,        GDH1196
     .                FCBNDO,CNBOC2,TNCBC3,TNCBC2,HOHBND,HNNBND,        GDH1296
     .                SPBNDO,OSIBND                                     PDW0601
      COMMON /HEXSTM/ HRFN4(100),CRFNH4,CNRFN4,CORFN5,                  GDH0797
     X                CRFNH5,OORFN5,ONRFN4,SSRFN5,OPRFN5,               GDH0797
     1                FCRFN,CLCRFN,BRCRFN,CNSTC2,TNCSC3,TNCSC2,         GDH0797
     2                HOHST1,HNNST1, SPRFN5,OSIBD5                      PDW0901
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),   GDH0897
     1                HBCORC(100),QKNOT(100),QKONE(100)                 GDH0897
      COMMON /SOLVCM/ CELEID, SLVRAD, SLVRD2, CSSIGM
      COMMON /PROPCM/ SDEPT(10), SOLN,SOLA,SOLB,SOLG,SOLC,SOLH          GDH0897
      COMMON /PAREAS/ SDEPA(150),SDEPB(150),SDEPN(150),                 PDW0999
     X                HDEPA(150),HDEPB(150),HDEPN(150)                  PDW0999

      DIMENSION AREAEL(100),NATTYP(100)
      LOGICAL WATSRP
      CHARACTER ELEMNT*2
      INCLUDE 'PARAMS.i'
C
      WATSRP=CISOLV.EQ.'H2OSRP'                                         DJG0995
      SUB1=0.0D0                                                        GDH0897
      SUB1CS=0.0D0                                                      GDH0897
      SUB2=0.0D0                                                        GDH0897
      SUB3=0.0D0                                                        GDH0897
      SUB4=0.0D0                                                        GDH0897
      SUB1A=0.0D0                                                       PDW0999
      SUB1B=0.0D0                                                       PDW0999
      SUB1N=0.0D0                                                       PDW0999
      SUB2A=0.0D0                                                       PDW0999
      SUB2B=0.0D0                                                       PDW0999
      SUB2N=0.0D0                                                       PDW0999
      SUB3A=0.0D0                                                       PDW0999
      SUB3B=0.0D0                                                       PDW0999
      SUB3N=0.0D0                                                       PDW0999
      SUB4A=0.0D0                                                       PDW0999
      SUB4B=0.0D0                                                       PDW0999
      SUB4N=0.0D0                                                       PDW0999
C MUST ZERO OUT VALUES .... 
      DO 100 M=1,100                                                    DJG0995
         AREAEL(M)=0.0D0                                                DJG1094
         NATTYP(M)=0                                                    DJG1094
100   CONTINUE                                                          DJG0995
      DO 125 M=1,NUMAT                                                  DJG0995
         AREAEL(NAT(M))=AREAEL(NAT(M))+ATAR(M)                          DJG1094
         NATTYP(NAT(M))=1                                               DJG0195
125   CONTINUE                                                          DJG0995
      IF (IRAD.EQ.4.OR.IRAD.EQ.5.OR.IRAD.EQ.6.OR.IRAD.EQ.7.OR.IRAD.EQ.8 PDW1099
     .    .OR.IRAD.EQ.9.OR.IRAD.EQ.10.OR.IRAD.EQ.20.OR.IRAD.EQ.21) THEN PDW1099
      WRITE(JOUT,500)                                                   GDH0897
      WRITE(JOUT,600)                                                   PDW0999
      WRITE(JOUT,700) SLVRAD                                            PDW1199
      WRITE(JOUT,800) SLVRD2                                            PDW1199
      WRITE(JOUT,600)                                                   PDW1199
      WRITE(JOUT,1000)                                                  PDW0999
         DO 150 M=1,100                                                 DJG0995
            IF (NATTYP(M).NE.0) THEN                                    GDH0897
               WRITE(JOUT,1010) ELEMNT(M),AREAEL(M),SIGMA0(M),          GDH0897
     .         AREAEL(M)*SIGMA0(M)/1000.D0                              PDW0999
               SUB1=SUB1+AREAEL(M)*SIGMA0(M)/1000.D0                    GDH0897
               SUB1A=SUB1A+AREAEL(M)*SDEPA(M)/1000.D0                   PDW0999
               SUB1B=SUB1B+AREAEL(M)*SDEPB(M)/1000.D0                   PDW0999
               SUB1N=SUB1N+AREAEL(M)*SDEPN(M)/1000.D0                   PDW0999
               SUB1CS=SUB1CS+CSAREA(M)                                  GDH0897
            ENDIF                                                       GDH0897
150      CONTINUE                                                       DJG0995
         WRITE (JOUT,1065) SUB1                                         GDH0897
      ENDIF                                                             DJG1094
      WRITE(JOUT,600)                                                   PDW0999
      WRITE(JOUT,1070)                                                  GDH0797
      WRITE(JOUT,1080)                                                  PDW0999
      IF (IRAD.EQ.4.OR.IRAD.EQ.5.OR.IRAD.EQ.6.OR.IRAD.EQ.7.OR.IRAD.EQ.8 PDW1099
     .    .OR.IRAD.EQ.9.OR.IRAD.EQ.10.OR.IRAD.EQ.20.OR.IRAD.EQ.21) THEN PDW1099
         DO 225 M=1,100                                                 DJG0995
            IF (NATTYP(M).NE.0) THEN                                    GDH0897
               WRITE(JOUT,1090) "H ", ELEMNT(M),HBONDO(M),HRFN4(M),     PDW0999
     .         HBONDO(M)*HRFN4(M)/1000.D0                               GDH0897
               SUB2N=SUB2N+HBONDO(M)*HDEPN(M)/1000.D0                   PDW0999
               SUB2A=SUB2A+HBONDO(M)*HDEPA(M)/1000.D0                   PDW0999
               SUB2B=SUB2B+HBONDO(M)*HDEPB(M)/1000.D0                   PDW0999
               SUB2=SUB2+HBONDO(M)*HRFN4(M)/1000.D0                     GDH0897
            ENDIF                                                       GDH0897
225      CONTINUE                                                       DJG0995
         WRITE(JOUT,1090)"H ","N(2)",HNNBND,HNNST1,HNNBND*HNNST1/1000.D0PDW0999
         SUB3N=SUB3N+HNNBND*SDEPN(112)/1000.D0                          PDW0999
         SUB3A=SUB3A+HNNBND*SDEPA(112)/1000.D0                          PDW0999
         SUB3B=SUB3B+HNNBND*SDEPB(112)/1000.D0                          PDW0999
         SUB3=SUB3+HNNBND*HNNST1/1000.D0                                GDH0897
         WRITE(JOUT,1090)"H ","O(2)",HOHBND,HOHST1,HOHBND*HOHST1/1000.D0GDH0897
         SUB3N=SUB3N+HOHBND*SDEPN(113)/1000.D0                          PDW0999
         SUB3A=SUB3A+HOHBND*SDEPA(113)/1000.D0                          PDW0999
         SUB3B=SUB3B+HOHBND*SDEPB(113)/1000.D0                          PDW0999
         SUB3=SUB3+HOHBND*HOHST1/1000.D0                                GDH0897
         WRITE(JOUT,1090)"C ","C(1)",CBONDO,CRFNH4,CBONDO*CRFNH4/1000.D0GDH0897
         SUB3N=SUB3N+CBONDO*SDEPN(101)/1000.D0                          PDW0999
         SUB3A=SUB3A+CBONDO*SDEPA(101)/1000.D0                          PDW0999
         SUB3B=SUB3B+CBONDO*SDEPB(101)/1000.D0                          PDW0999
         SUB3=SUB3+CBONDO*CRFNH4/1000.D0                                GDH0897
         WRITE(JOUT,1090)"C ","C(2)",CBOND5,CRFNH5,CBOND5*CRFNH5/1000.D0GDH0897
         SUB3N=SUB3N+CBOND5*SDEPN(102)/1000.D0                          PDW0999
         SUB3A=SUB3A+CBOND5*SDEPA(102)/1000.D0                          PDW0999
         SUB3B=SUB3B+CBOND5*SDEPB(102)/1000.D0                          PDW0999
         SUB3=SUB3+CBOND5*CRFNH5/1000.D0                                GDH0897
         WRITE(JOUT,1090)"O ","C ",COBND5,CORFN5,COBND5*CORFN5/1000.D0  GDH0897
         SUB3N=SUB3N+COBND5*SDEPN(103)/1000.D0                          PDW0999
         SUB3A=SUB3A+COBND5*SDEPA(103)/1000.D0                          PDW0999
         SUB3B=SUB3B+COBND5*SDEPB(103)/1000.D0                          PDW0999
         SUB3=SUB3+COBND5*CORFN5/1000.D0                                GDH0897
         WRITE(JOUT,1090)"O ","O ",OOBNDO,OORFN5,OOBNDO*OORFN5/1000.D0  GDH0897
         SUB3N=SUB3N+OOBNDO*SDEPN(104)/1000.D0                          PDW0999
         SUB3A=SUB3A+OOBNDO*SDEPA(104)/1000.D0                          PDW0999
         SUB3B=SUB3B+OOBNDO*SDEPB(104)/1000.D0                          PDW0999
         SUB3=SUB3+OOBNDO*OORFN5/1000.D0                                GDH0897
         WRITE(JOUT,1090)"O ","Si",OSIBND,OSIBD5,OSIBND*OSIBD5/1000.D0  PDW0601
         SUB3N=SUB3N+OSIBND*SDEPN(108)/1000.D0                          PDW0601
         SUB3A=SUB3A+OSIBND*SDEPA(108)/1000.D0                          PDW0601
         SUB3B=SUB3B+OSIBND*SDEPB(108)/1000.D0                          PDW0601
         SUB3=SUB3+OSIBND*OSIBD5/1000.D0                                PDW0601
         WRITE(JOUT,1090)"C ","N ",CNBOC2,CNSTC2,CNBOC2*CNSTC2/1000.D0  GDH0897
         SUB3N=SUB3N+CNBOC2*SDEPN(110)/1000.D0                          PDW0999
         SUB3A=SUB3A+CNBOC2*SDEPA(110)/1000.D0                          PDW0999
         SUB3B=SUB3B+CNBOC2*SDEPB(110)/1000.D0                          PDW0999
         SUB3=SUB3+CNBOC2*CNSTC2/1000.D0                                GDH0897
         WRITE(JOUT,1090)"N ","C ",CNBNDO,CNRFN4,CNBNDO*CNRFN4/1000.D0  GDH0897
         SUB3N=SUB3N+CNBNDO*SDEPN(105)/1000.D0                          PDW0999
         SUB3A=SUB3A+CNBNDO*SDEPA(105)/1000.D0                          PDW0999
         SUB3B=SUB3B+CNBNDO*SDEPB(105)/1000.D0                          PDW0999
         SUB3=SUB3+CNBNDO*CNRFN4/1000.D0                                GDH0897
         WRITE(JOUT,1090)"N ","C(2)",TNCBC2,TNCSC2,TNCBC2*TNCSC2/1000.D0GDH0897
         SUB3N=SUB3N+TNCBC2*SDEPN(111)/1000.D0                          PDW0999
         SUB3A=SUB3A+TNCBC2*SDEPA(111)/1000.D0                          PDW0999
         SUB3B=SUB3B+TNCBC2*SDEPB(111)/1000.D0                          PDW0999
         SUB3=SUB3+TNCBC2*TNCSC2/1000.D0                                GDH0897
         WRITE(JOUT,1090)"N ","C(3)",TNCBC3,TNCSC3,TNCBC3*TNCSC3/1000.D0GDH0897
         SUB3N=SUB3N+TNCBC3*SDEPN(116)/1000.D0                          PDW0999
         SUB3A=SUB3A+TNCBC3*SDEPA(116)/1000.D0                          PDW0999
         SUB3B=SUB3B+TNCBC3*SDEPB(116)/1000.D0                          PDW0999
         SUB3=SUB3+TNCBC3*TNCSC3/1000.D0                                GDH0897
         WRITE(JOUT,1090)"O ","N ",ONBNDO,ONRFN4,ONBNDO*ONRFN4/1000.D0  GDH0897
         SUB3N=SUB3N+ONBNDO*SDEPN(106)/1000.D0                          PDW0999
         SUB3A=SUB3A+ONBNDO*SDEPA(106)/1000.D0                          PDW0999
         SUB3B=SUB3B+ONBNDO*SDEPB(106)/1000.D0                          PDW0999
         SUB3=SUB3+ONBNDO*ONRFN4/1000.D0                                GDH0897
         WRITE(JOUT,1090)"S ","S ",SSBNDO,SSRFN5,SSBNDO*SSRFN5/1000.D0  GDH0897
         SUB3N=SUB3N+SSBNDO*SDEPN(107)/1000.D0                          PDW0999
         SUB3A=SUB3A+SSBNDO*SDEPA(107)/1000.D0                          PDW0999
         SUB3B=SUB3B+SSBNDO*SDEPB(107)/1000.D0                          PDW0999
         SUB3=SUB3+SSBNDO*SSRFN5/1000.D0                                GDH0897
         WRITE(JOUT,1090)"O ","P ",OPBND0,OPRFN5,OPBND0*OPRFN5/1000.D0  GDH0897
         SUB3N=SUB3N+OPBND0*SDEPN(114)/1000.D0                          PDW0999
         SUB3A=SUB3A+OPNBD0*SDEPA(114)/1000.D0                          PDW0999
         SUB3B=SUB3B+OPNBD0*SDEPB(114)/1000.D0                          PDW0999
         SUB3=SUB3+OPBND0*OPRFN5/1000.D0                                GDH0897
         WRITE(JOUT,1090)"S ","P ",SPBNDO,SPRFN5,SPBNDO*SPRFN5/1000.D0  GDH0897
         SUB3N=SUB3N+SPBNDO*SDEPN(115)/1000.D0                          PDW0999
         SUB3A=SUB3A+SPBNDO*SDEPA(115)/1000.D0                          PDW0999
         SUB3B=SUB3B+SPBNDO*SDEPB(115)/1000.D0                          PDW0999
         SUB3=SUB3+SPBNDO*SPRFN5/1000.D0                                GDH0897
         WRITE(JOUT,1090)"Cl","C ",CLCBND,CLCRFN,CLCBND*CLCRFN/1000.D0  GDH0897
         SUB3N=SUB3N+CLCBND*SDEPN(117)/1000.D0                          PDW0999
         SUB3A=SUB3A+CLCBND*SDEPA(117)/1000.D0                          PDW0999
         SUB3B=SUB3B+CLCBND*SDEPB(117)/1000.D0                          PDW0999
         SUB3=SUB3+CLCBND*CLCRFN/1000.D0                                GDH0897
         WRITE(JOUT,1090)"Br","C ",BRCBND,BRCRFN,BRCBND*BRCRFN/1000.D0  GDH0897
         SUB3N=SUB3N+BRCBND*SDEPN(118)/1000.D0                          PDW0999
         SUB3A=SUB3A+BRCBND*SDEPA(118)/1000.D0                          PDW0999
         SUB3B=SUB3B+BRCBND*SDEPB(118)/1000.D0                          PDW0999
         SUB3=SUB3+BRCBND*BRCRFN/1000.D0                                GDH0897
         WRITE(JOUT,1065) SUB2+SUB3                                     PDW0999
         WRITE(JOUT,1220) SUB1+SUB2+SUB3
         WRITE(JOUT,600)                                                PDW0999
         WRITE(JOUT,1093)                                               PDW0999
         WRITE(JOUT,1200)                                               PDW1099
         DO 250 M=1,100                                                 DJG0995
            IF (NATTYP(M).NE.0) THEN                                    GDH0897
               WRITE(JOUT,1210) ELEMNT(M), CSAREA(M)                    PDW1099
            ENDIF                                                       GDH0897
 250     CONTINUE
         WRITE(JOUT,1097) SUB1CS 
         IF((NUMSLV.EQ.20)) THEN

         WRITE(JOUT,1095)                                               PDW0999
         WRITE (JOUT,1100)"IOFR     ",SUB1CS*SOLN        ,SDEPT(5),     GDH0897
     .                     (SUB1CS*SOLN/1000.D0)*SDEPT(5)               GDH0897
         SUB4=SUB4+(SUB1CS*SOLN/1000.D0)*SDEPT(5)                       GDH0897
         WRITE (JOUT,1100)"GAMMA    ",SUB1CS*SOLG        ,SDEPT(1),     GDH0897
     .                     (SUB1CS*SOLG/1000.D0)*SDEPT(1)               GDH0897
         SUB4=SUB4+(SUB1CS*SOLG/1000.D0)*SDEPT(1)                       GDH0897
         WRITE (JOUT,1100)"BETA**2  ",SUB1CS*SOLB*SOLB        ,SDEPT(2),GDH0897
     .                     (SUB1CS*SOLB*SOLB/1000.D0)*SDEPT(2)          GDH0897
         SUB4=SUB4+(SUB1CS*SOLB*SOLB/1000.D0)*SDEPT(2)                  GDH0897
         WRITE (JOUT,1100)"FACARB**2",SUB1CS*SOLC*SOLC        ,SDEPT(3),GDH0897
     .                     (SUB1CS*SOLC*SOLC/1000.D0)*SDEPT(3)          GDH0897
         SUB4=SUB4+(SUB1CS*SOLC*SOLC/1000.D0)*SDEPT(3)                  GDH0897
         WRITE (JOUT,1100)"FEHALO**2",SUB1CS*SOLH*SOLH        ,SDEPT(4),GDH0897
     .                     (SUB1CS*SOLH*SOLH/1000.D0)*SDEPT(4)          GDH0897
         SUB4=SUB4+(SUB1CS*SOLH*SOLH/1000.D0)*SDEPT(4)                  GDH0897
         WRITE (JOUT,1230) SUB4                                         PDW1199
         SUB4N=SOLN*SUB1CS/1000.D0*SDEPT(5)                             PDW0999
         WRITE(JOUT,600)                                                PDW0999
	 TOTN=SUB1N+SUB2N+SUB3N+SUB4N                                   PDW0999
         TOTA=SUB1A+SUB2A+SUB3A+SUB4A                                   PDW0999
	 TOTB=SUB1B+SUB2B+SUB3B+SUB4B                                   PDW0999
         WRITE(JOUT,1175)                                               PDW0999
         WRITE(JOUT,1177)                                               PDW0999
         WRITE(JOUT,1100) "IOFR",SOLN,TOTN,SOLN*TOTN
	 WRITE(JOUT,1100) "ALPHA",SOLA,TOTA,SOLA*TOTA
         WRITE(JOUT,1100) "BETA",SOLB,TOTB,SOLB*TOTB
         WRITE(JOUT,1100) "BETA**2",SOLB*SOLB,SDEPT(2)*SUB1CS/1000.D0,
     .         SOLB*SOLB*SDEPT(2)*SUB1CS/1000.D0 
	 WRITE(JOUT,1100) "GAMMA", SOLG, SUB1CS*SDEPT(1)/1000.D0,
     .         SOLG*SUB1CS*SDEPT(1)/1000.D0
         WRITE(JOUT,1100) "FACARB**2", SOLC, SUB1CS*SDEPT(3)/1000.D0,
     .         SOLC*SOLC*SDEPT(3)*SUB1CS/1000.D0
         WRITE(JOUT,1100) "FEHALO**2", SOLH, SUB1CS*SDEPT(4)/1000.D0,
     .         SOLH*SOLH*SDEPT(4)*SUB1CS/1000.D0

      ELSE IF (NUMSLV.NE.19) THEN
          SUB4 = ATLGAR*HEXLGS
          WRITE(JOUT,350)
          WRITE(JOUT,400) ATLGAR, 1000D0*SUB4/ATLGAR,SUB4
350   FORMAT(/,1X,T25,'CS Area',T35,'Sigma CS',T47,'LS Contribution',
     1      /,T25,'(Ang**2)',T34,'cal/(Ang**2)',T51,'(kcal)')
400   FORMAT(/,1X,'Specific CS parameters',T25,F7.2,T35,F7.2,T50,F7.2 ,
     1      /,1X,'for this solvent')
      ENDIF

      WRITE (JOUT,600)                                                  PDW0999
      WRITE (JOUT,1190)   SUB1+SUB2+SUB3+SUB4                           PDW0999
      WRITE (JOUT,600)                                                  PDW0999
      ENDIF                                                             DJG0995
      IF (IRAD.EQ.6.AND..NOT.WATSRP) THEN                               DJG0995
         WRITE(JOUT,1170) CBONDO                                        DJG0995
      ENDIF                                                             DJG1094
500   FORMAT(/,5X,'The following section contains information on ',     GDH0897
     .       ' G_CDS.')                                                 PDW1199
600   FORMAT(/,1X,'******')                                             PDW0999
700   FORMAT(/,1X,'Small-sphere (SS) solvent radius:',2X,F4.2)          PDW1199
800   FORMAT(1X,'Large-sphere (LS) solvent radius:',2X,F4.2)            PDW1199
1000  FORMAT(/,1X,'Single-subscript surface-tension-coefficient',       PDW1199
     1 ' contributions to G_CDS:',/,                                    PDW0999
     1/,2X,'Element        CD Area           Sigma           Total',    PDW0999
     1/,2X,'   Z                               k                   ',   PDW0999
     1/,2X,'              (Ang**2)       cal/(Ang**2)        (kcal)',   PDW0999
     1/)                                                                GDH0897
1010  FORMAT(5X,A2,9X,F10.3,5X,F10.3,5X,F10.3)                          PDW0999
1020  FORMAT(/,1X,'Single-subscript surface-tension-coefficient',       PDW1199
     1 ' contributions to G_CDS:',                                      PDW0999
     1/,2X,'Element        SS Area         Sigma         Total    ',    GDH0897
     1/,2X,'   Z                             k                    ',    GDH0897
     1/,2X,'              (Ang**2)     cal/(Ang**2)      (kcal)  ',/)   GDH0897
1065  FORMAT(/,1X,'Subtotal:',38X,F8.3)                                 GDH0897
1070  FORMAT(/,1X,'Double-subscript surface-tension-coefficient',       PDW1199
     1 ' contributions to G_CDS:')                                      PDW0999
1080  FORMAT(                                                           GDH0897
     1/,2X,'Element        COT*Area(k)      Sigma           Total ',    GDH0897
     1/,2X,"k   k'                            k,k'                ",    GDH0897
     1/,2X,"                (Ang**2)      cal/(Ang**2)      (kcal) ",/) GDH0897
1090  FORMAT(2X,A2,1X,A4,7X,F10.3,5X,F10.3,5X,F10.3)                    GDH0897
1093  FORMAT(/,1X,'Contribution to G_CDS from terms that do not depend',PDW1199
     1 ' on atomic number,'/,3X,'i.e., the ones that depend on the',    PDW1199
     2 ' large-sphere (LS) areas:')                                     PDW0999
1095  FORMAT(/,'   Solvent         SD *            Sigma      ',        PDW0999
     .       'LS Contribution',                                         GDH0897
     1/,"  Descriptor      LS Area                                    ",GDH0897
     1/,4X," (SD)         (Ang**2)     cal/(Ang**2)        (kcal) ",/)  GDH0897
1097  FORMAT(/,3X,'Total:',2X,F10.3,/)                                  PDW0999
1100  FORMAT(2X,A9,5X,F10.3,5X,F10.3,5X,F10.3)                          PDW0999
1170  FORMAT(/,'Carbon cubed bond order area =   ',F8.3)                DJG0995
1175  FORMAT(/,1X,'Complete CDS term broken down by solvent property:') PDW0999
1177  FORMAT(/,'   Solvent        Descriptor    Surface Energy',        PDW0999
     .  '   CDS Contribution',                                          PDW0999
     1/,2X,"Descriptor         Value      Sum over Atoms     SD*SE  ",  PDW0999
     1/,2X,"   (SD)                            (SE)          (kcal) ",  PDW0999
     1/,2X,"                                  (kcal)",/)                PDW1099
1190  FORMAT(/,1X,'G_CDS Total:',35X,F8.3)                              PDW0999
1200  FORMAT(/,2X,'Element     LS Area',                                PDW1099
     1/,5X,"Z       (Ang**2)",/)                                        PDW1099
1210  FORMAT(5X,A2,4X,F10.3)                                            PDW1099
1220  FORMAT(/,1X,'Total small-sphere (SS) G_CDS:',17X,F8.3)            PDW1199
1230  FORMAT(/,1X,'Total large-sphere (LS) G_CDS:',17X,F8.3)            PDW1199
      RETURN
      END
