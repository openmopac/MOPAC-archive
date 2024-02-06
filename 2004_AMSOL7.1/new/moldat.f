      SUBROUTINE MOLDAT 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
C ----------------------------------------------------------------------
C     SET UP MODEL HAMILTONIAN, ATOM & ORBITAL COUNTS,
C            ORBITAL OCCUPANCIES AND CHECK INPUT DATA.
C ----------------------------------------------------------------------
      COMMON /MLKSTR/ FRACT                                             3GL3092
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
     2       /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     3                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     4                NCLOSE,NOPEN,NDUMY                                3GL3092
     6       /NATORB/ NATORB(107)
     7       /CORE  / CORE(107)
     8       /BETAS / BETAS(107),BETAP(107),BETAD(107)
     9       /MOLORB/ USPD(MAXORB),PSPD(MAXORB)
      COMMON /WMATRX/ WDUMMY(N2ELEC*3),KDUMMY,NBAND(NUMATM)
     1       /VSIPS / VS(107),VP(107),VD(107)
     2       /ONELEC/ USS(107),UPP(107),UDD(107)
     3       /ATHEAT/ ATHEAT
     4       /POLVOL/ POLVOL(107)
     5       /MULTIP/ DD(107),QQ(107),AM(107),AD(107),AQ(107)
     6       /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107)
     7               ,GSD(107),GPD(107),GDD(107)
     8       /ALPHA / ALP(107)
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /GAUSS / FN1(107),FN2(107)
     1       /GEOM  / GEO(3,NUMATM)
     2       /MNDO  / USSM(107), UPPM(107), UDDM(107), ZSM(107),ZPM(107)
     3               ,ZDM(107), BETASM(107), BETAPM(107), BETADM(107)
     4               ,ALPM(107), EISOLM(107), DDM(107), QQM(107)
     5               ,AMM(107),ADM(107),AQM(107),GSSM(107),GSPM(107)
     6               ,GPPM(107),GP2M(107), HSPM(107), POLVOM(107)
     7       /IDEAS / GUESS1(107,10),GUESS2(107,10),GUESS3(107,10)
     8               ,NGUESS(107)
C    2       /SCRACH/ RXYZ(MPACK), XDUMY(3*MORB2-MPACK)
C
C     /SCRACH/ HAS BEEN CONVERTED TO /SCRCHR/ AND /SCRCHI/ FOR AMSOL    
      COMMON /SCRCHR/ RXYZ(MPACK), XDUMY(6*MAXPAR**2+1-MPACK)           GCL0393
C
      COMMON /MNDOC / USSMC(107), UPPMC(107), UDDMC(107)
     1               ,ZSMC(107), ZPMC(107), ZDMC(107), BETASC(107)
     2               ,BETAPC(107), BETADC(107), ALPMC(107), EISOLC(107)
     3               ,DDMC(107), QQMC(107), AMMC(107), ADMC(107)
     4               ,AQMC(107), GSSMC(107), GSPMC(107), GPPMC(107)
     5               ,GP2MC(107), HSPMC(107), POLVOC(107)
      COMMON /PM3   / USSPM3(107), UPPPM3(107), UDDPM3(107)
     1               ,ZSPM3(107),ZPPM3(107),ZDPM3(107), BETASP(107)
     2               ,BETAPP(107),BETADP(107),ALPPM3(107), EISOLP(107)
     3               ,DDPM3(107),QQPM3(107),AMPM3(107),ADPM3(107)
     4               ,AQPM3(107),GSSPM3(107),GSPPM3(107),GPPPM3(107)
     5               ,GP2PM3(107),HSPPM3(107),POLVOP(107)
     6       /IDEAP / GUESP1(107,10),GUESP2(107,10),GUESP3(107,10)
     7               ,NGUESP(107)
      COMMON /AM1PCM/ USSAM1(107), UPPAM1(107), UDDAM1(107),             IR1294
     1                ZSAM1(107), ZPAM1(107), ZDAM1(107),                IR1294
     2                BETASA(107),BETAPA(107), BETADA(107),ALPAM1(107),  IR1294
     3                EISOLA(107),DDAM1(107), QQAM1(107), AMAM1(107),    IR1294
     4                ADAM1(107), AQAM1(107),GSSAM1(107), GSPAM1(107),   IR1294
     5                GPPAM1(107),GP2AM1(107), HSPAM1(107),POLVOA(107)   IR1294
     6      /IDAACM / GUESA1(107,10),GUESA2(107,10),GUESA3(107,10),      IR1294
     7                NGUESA(107)                                        IR1294
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),               GDH0297
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)           GDH0297
C  COMMON BLOCKS FOR MINDO/3
      COMMON /ONELE3 /  USS3(18),UPP3(18)
     1       /ATOMI3 /  EISOL3(18),EHEAT3(18)
     2       /EXPON3 /  ZS3(18),ZP3(18)
C  END OF MINDO/3 COMMON BLOCKS
      COMMON /EXPONT/ ZS(107),ZP(107),ZD(107)
     1       /ATOMIC/ EISOL(107),EHEAT(107)
     2       /REFS/ REFER(107,5)
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           PDW1099
      CHARACTER*80 REFER
      CHARACTER*50 DTYPE(100)
      DIMENSION COORD(3,NUMATM),EXIST(107)
      LOGICAL DEBUG,UHF,EXCI,TRIP,MINDO3,BIRAD,PARAM,AM1,OPEN,PM3
     .       ,MNDOC,EXIST
      DATA DTYPE/'H with a charge of +1                             '   GDH0696
     2          ,'H with a charge of +1/2                           '   GDH0696
     3          ,'H with a charge of +1/3                           '   GDH0696
     4          ,'H with a charge of +1/4                           '   GDH0696
     5          ,'H with a charge of +1/5                           '   GDH0696
     6          ,'O with a charge of -1                             '   GDH0696
     7          ,'O with a charge of -1/2                           '   GDH0696
     8          ,'S with a charge of -1                             '   GDH0297
     4       ,92*'                                                  '/  GDH0696
      IF (ICONTR(10).EQ.1) THEN                                         GDH0195
         ICONTR(10)=2                                                   GDH0195
         PARAM=.FALSE.                                                  GDH0195
         DO 33 K=1,107                                                  GDH0195
   33     EXIST(K)=.FALSE.                                              GDH0195
      ENDIF                                                             GDH0195
      DEBUG = IMOLDA.NE.0                                               DJG0995
      MINDO3= IMINDO.NE.0                                               DJG0995
      UHF=IUHF.NE.0                                                     DJG0995
      AM1=IAM1.NE.0                                                     DJG0995
      PM3=IPM3.NE.0                                                     DJG0995
      MNDOC=IMNDOC.NE.0                                                 DJG0995
      KHARGE=0
      IF(ICHARG.NE.0) KHARGE=IICHAR                                     DJG0995
      NELECS=-KHARGE
      NDORBS=0
      ATHEAT=0.D0
      EAT=0.D0
      NUMAT=0
      IF (ISM50.EQ.0) THEN                                              GDH1196
      IF (AM1) THEN                                                      IR1294
C                                                                        IR1294
C      Switch in AM1 parameters                                          IR1294
C                                                                        IR1294
         MODEL=3
         DO 5 I=1,107                                                    IR1294
           NGUESS(I)=NGUESA(I)                                           IR1294
           DO 6  J=1,10                                                  IR1294
             GUESS1(I,J)=GUESA1(I,J)                                     IR1294
             GUESS2(I,J)=GUESA2(I,J)                                     IR1294
   6         GUESS3(I,J)=GUESA3(I,J)                                     IR1294
           POLVOL(I)=POLVOA(I)                                           IR1294
           ZS(I)=ZSAM1(I)                                                IR1294
           ZP(I)=ZPAM1(I)                                                IR1294
           ZD(I)=ZDAM1(I)                                                IR1294
           USS(I)=USSAM1(I)                                              IR1294
           UPP(I)=UPPAM1(I)                                              IR1294
           UDD(I)=UDDAM1(I)                                              IR1294
           BETAS(I)=BETASA(I)                                            IR1294
           BETAP(I)=BETAPA(I)                                            IR1294
           BETAD(I)=BETADA(I)                                            IR1294
           ALP(I)=ALPAM1(I)                                              IR1294
           EISOL(I)=EISOLA(I)                                            IR1294
           DD(I)=DDAM1(I)                                                IR1294
           QQ(I)=QQAM1(I)                                                IR1294
           AM(I)=AMAM1(I)                                                IR1294
           AD(I)=ADAM1(I)                                                IR1294
           AQ(I)=AQAM1(I)                                                IR1294
           GSS(I)=GSSAM1(I)                                              IR1294
           GPP(I)=GPPAM1(I)                                              IR1294
           GSP(I)=GSPAM1(I)                                              IR1294
           GP2(I)=GP2AM1(I)                                              IR1294
           HSP(I)=HSPAM1(I)                                              IR1294
   5     CONTINUE                                                        IR1294
      ENDIF                                                              IR1294
      IF (.NOT.(AM1.OR.PM3)) THEN
*
*        SWITCH IN MNDO/MNDOC PARAMETERS
*
         MODEL=1
         IF (MNDOC) MODEL=5
         DO 10 I=1,107
         IF(.NOT.MINDO3) POLVOL(I)=POLVOM(I)
         FN1(I)=0.D0
         IF (.NOT.MNDOC) THEN
*
*        SWITCH IN MNDO PARAMETERS
*
           ZS(I)=ZSM(I)
           ZP(I)=ZPM(I)
           ZD(I)=ZDM(I)
           USS(I)=USSM(I)
           UPP(I)=UPPM(I)
           UDD(I)=UDDM(I)
           BETAS(I)=BETASM(I)
           BETAP(I)=BETAPM(I)
           BETAD(I)=BETADM(I)
           ALP(I)=ALPM(I)
           EISOL(I)=EISOLM(I)
           DD(I)=DDM(I)
           QQ(I)=QQM(I)
           AM(I)=AMM(I)
           AD(I)=ADM(I)
           AQ(I)=AQM(I)
           GSS(I)=GSSM(I)
           GPP(I)=GPPM(I)
           GSP(I)=GSPM(I)
           GP2(I)=GP2M(I)
           HSP(I)=HSPM(I)
         ELSE
*
*        SWITCH IN MNDOC PARAMETERS
*
           ZS(I)=ZSMC(I)
           ZP(I)=ZPMC(I)
           ZD(I)=ZDMC(I)
           USS(I)=USSMC(I)
           UPP(I)=UPPMC(I)
           UDD(I)=UDDMC(I)
           BETAS(I)=BETASC(I)
           BETAP(I)=BETAPC(I)
           BETAD(I)=BETADC(I)
           ALP(I)=ALPMC(I)
           EISOL(I)=EISOLC(I)
           DD(I)=DDMC(I)
           QQ(I)=QQMC(I)
           AM(I)=AMMC(I)
           AD(I)=ADMC(I)
           AQ(I)=AQMC(I)
           GSS(I)=GSSMC(I)
           GPP(I)=GPPMC(I)
           GSP(I)=GSPMC(I)
           GP2(I)=GP2MC(I)
           HSP(I)=HSPMC(I)
         ENDIF
   10    CONTINUE
      ELSE IF (.NOT.AM1.AND.PM3) THEN
*
*        SWITCH IN PM3 PARAMETERS
*
         MODEL=4
         DO 12 I=1,107
         NGUESS(I)=NGUESP(I)
         DO 11 J=1,10
         GUESS1(I,J)=GUESP1(I,J)
         GUESS2(I,J)=GUESP2(I,J)
   11    GUESS3(I,J)=GUESP3(I,J)
         POLVOL(I)=POLVOP(I)
         FN1(I)=0.D0
         ZS(I)=ZSPM3(I)
         ZP(I)=ZPPM3(I)
         ZD(I)=ZDPM3(I)
         USS(I)=USSPM3(I)
         UPP(I)=UPPPM3(I)
         UDD(I)=UDDPM3(I)
         BETAS(I)=BETASP(I)
         BETAP(I)=BETAPP(I)
         BETAD(I)=BETADP(I)
         ALP(I)=ALPPM3(I)
         EISOL(I)=EISOLP(I)
         DD(I)=DDPM3(I)
         QQ(I)=QQPM3(I)
         AM(I)=AMPM3(I)
         AD(I)=ADPM3(I)
         AQ(I)=AQPM3(I)
         GSS(I)=GSSPM3(I)
         GPP(I)=GPPPM3(I)
         GSP(I)=GSPPM3(I)
         GP2(I)=GP2PM3(I)
         HSP(I)=HSPPM3(I)
   12    CONTINUE
      ENDIF
      IF (MINDO3) THEN
*
*        SWITCH IN MINDO/3 PARAMETERS
*
         MODEL=2
         DO 20 I=1,17
         IF(I.EQ.2.OR.I.EQ.10)GOTO 20
         USS(I)=USS3(I)
         UPP(I)=UPP3(I)
         EISOL(I)=EISOL3(I)
         EHEAT(I)=EHEAT3(I)
         ZS(I)=ZS3(I)
         ZP(I)=ZP3(I)
   20    CONTINUE
      ENDIF
      IF (USS(1).GT.-1.D0) THEN
         WRITE(JOUT,'('' THE HAMILTONIAN REQUESTED IS NOT AVAILABLE IN''
     1   ,'' THIS PROGRAM'')')
          ISTOP=1                                                       GDH1095
          IWHERE=138                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      ENDIF                                                             GDH1196
      IA=1
      IB=0
      NHEAVY=0
      DO 70 II=1,NATOMS
         IF(LABELS(II).EQ.99.OR.LABELS(II).EQ.107) GOTO 70
         EXIST(LABELS(II))=.TRUE.
         NUMAT=NUMAT+1
         NAT(NUMAT)=LABELS(II)
         NFIRST(NUMAT)=IA
         NI=NAT(NUMAT)
         ATHEAT=ATHEAT+EHEAT(NI)
         EAT   =EAT   +EISOL(NI)
         NELECS=NELECS+NINT(CORE(NI))
         IB=IA+NATORB(NI)-1
         NMIDLE(NUMAT)=IB
         IF(NATORB(NI).EQ.9)NDORBS=NDORBS+5
         IF(NATORB(NI).EQ.9)NMIDLE(NUMAT)=IA+3
         NLAST(NUMAT)=IB
         NBAND(NUMAT)=NATORB(NI)*(NATORB(NI)+1)/2
         USPD(IA)=USS(NI)
         IF(IA.EQ.IB) GOTO 60
         K=IA+1
         K1=IA+3
         DO 30 J=K,K1
   30    USPD(J)=UPP(NI)
         NHEAVY=NHEAVY+1
   40    IF(K1.EQ.IB)GOTO 60
         K=K1+1
         DO 50 J=K,IB
   50    USPD(J)=UDD(NI)
   60    CONTINUE
   70 IA=IB+1
      ATHEAT=ATHEAT-EAT*23.061D0
      NORBS=NLAST(NUMAT)
      IF(NORBS.GT.MAXORB)THEN
         WRITE(JOUT,'(10X,''**** MAX. NUMBER OF ORBITALS:'',I4,/
     1   10X,''NUMBER OF ORBITALS IN SYSTEM:'',I4)')
     2   MAXORB,NORBS
          ISTOP=1                                                       GDH1095
          IWHERE=139                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      NLIGHT=NUMAT-NHEAVY
      N2EL=50*NHEAVY*(NHEAVY-1)+10*NHEAVY*NLIGHT+(NLIGHT*(NLIGHT-1))/2
      IF(LABELS(NATOMS).EQ.107) N2EL=N2EL*2
      IF(N2EL.GT.N2ELEC)THEN
         WRITE(JOUT,'(10X,''MAX. NUMBER OF TWO-ELECTRON INTEGRALS:''
     1   ,I8/10X,''NUMBER OF TWO ELECTRON INTEGRALS IN SYSTEM:'',I8)')
     2   N2ELEC,N2EL
          ISTOP=1                                                       GDH1095
          IWHERE=140                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (ISM50.EQ.0) THEN                                              GDH1196
C
C   NOW TO CALCULATE THE NUMBER OF LEVELS OCCUPIED
      TRIP=(ITRIPL.NE.0)                                                DJG0995
      EXCI=(IEXCIT.NE.0)                                                DJG0995
      BIRAD=(EXCI.OR.IBIRAD.NE.0)                                       DJG0995
C
C NOW TO WORK OUT HOW MANY ELECTRONS ARE IN EACH TYPE OF SHELL
C
      NALPHA=0
      NBETA=0
      NCLOSE=0
      NOPEN=0
      IF (UHF) THEN
         FRACT=1.D0
         NBETA=NELECS/2
         IF (TRIP) THEN
            IF (NBETA*2 .NE. NELECS) THEN
               WRITE(JOUT,'(10X,''TRIPLET SPECIFIED WITH ODD NUMBER''
     1         ,'' OF ELECTRONS, CORRECT FAULT'')')
          ISTOP=1                                                       GDH1095
          IWHERE=141                                                    GDH1095
          RETURN                                                        GDH1095
            ELSE
               WRITE(JOUT,'('' Triplet state calculation'')')           GL0592
               NBETA=NBETA-1
            ENDIF
         ENDIF
         NALPHA=NELECS-NBETA
         WRITE(JOUT,'(/10X,''UHF calculation, no. of alpha electrons =''GL0592
     1   ,I3/27X,''no. of beta  electrons ='',I3)') NALPHA,NBETA        GL0592
      ELSE
C
C   NOW TO DETERMINE OPEN AND CLOSED SHELLS
C
         OPEN=.FALSE.
         IELEC=0
         ILEVEL=0
         IF( TRIP .OR. EXCI .OR. BIRAD ) THEN
            IF( (NELECS/2)*2 .NE. NELECS) THEN
               WRITE(JOUT,'(10X,''SYSTEM SPECIFIED WITH ODD NUMBER'',
     1         '' OF ELECTRONS, CORRECT FAULT'')')
          ISTOP=1                                                       GDH1095
          IWHERE=142                                                    GDH1095
          RETURN                                                        GDH1095
            ENDIF
            IF(BIRAD)WRITE(JOUT,'('' System is a biradical'')')         GL0592
            IF(TRIP )WRITE(JOUT,'('' Triplet state calculation'')')     GL0592
            IF(EXCI )WRITE(JOUT,'('' Excited state calculation'')')     GL0592
            IELEC=2
            ILEVEL=2
         ELSEIF((NELECS/2)*2.NE.NELECS) THEN
            IELEC=1
            ILEVEL=1
         ENDIF
         IF(IQUART.NE.0) THEN                                           DJG0995
            WRITE(JOUT,'('' Quartet state calculation'')')              GL0592
            IELEC=3
            ILEVEL=3
         ENDIF
         IF(IQUINT.NE.0) THEN                                           DJG0995
            WRITE(JOUT,'('' Quintet state calculation'')')              GL0592
            IELEC=4
            ILEVEL=4
         ENDIF
         IF(ISEXTE.NE.0) THEN                                           DJG0995
            WRITE(JOUT,'('' Sextet state calculation'')')               GL0592
            IELEC=5
            ILEVEL=5
         ENDIF
         IF (IOPEN.NE.0) THEN                                           DJG0995
            IELEC=IIOPE1                                                DJG0995
            ILEVEL=IIOPE2                                               DJG0995
C           CHECK LIMITING CASE.
            IF (IELEC.EQ.2*ILEVEL) THEN
               IELEC=0
               ILEVEL=0
            ENDIF
         ENDIF
         NCLOSE=NELECS/2
         NOPEN = NELECS-NCLOSE*2
         IF (IELEC.NE.0) THEN
            IF ((NELECS/2)*2.EQ.NELECS.NEQV.(IELEC/2)*2.EQ.IELEC) THEN
               WRITE(JOUT,'(10X,''IMPOSSIBLE NUMBER OF OPEN SHELL''
     1         ,'' ELECTRONS'')')
          ISTOP=1                                                       GDH1095
          IWHERE=143                                                    GDH1095
          RETURN                                                        GDH1095
            ENDIF
            NCLOSE=NCLOSE-IELEC/2
            NOPEN=ILEVEL
            FRACT=IELEC*1.D0/ILEVEL
            WRITE(JOUT,'(/,'' There are'',I3,'' doubly filled levels'') GL0592
     .      ')NCLOSE
         ENDIF
         IF(.NOT.PARAM) THEN
            WRITE(JOUT,'(/,10X,''RHF calculation, no. of '', 
     .            ''doubly occupied orbitals='',I3)')NCLOSE
            WRITE(JOUT,'(/)')
         ENDIF
         IF (NOPEN.NE.0) THEN
            IF (ABS(FRACT-1.D0).LT.1.D-4) THEN
               WRITE(JOUT,'(17X,''No. of singly occupied orbitals='',   GL0592
     .         I3)')NOPEN
            ELSE
               WRITE(JOUT,'(17X,''No. of levels with occupancy'',       GL0592
     .         F6.3,''  ='',I3)') FRACT,NOPEN
            ENDIF
            IF (FRACT.GE.2.D0.OR.FRACT.LE.0.D0) THEN
               WRITE(JOUT,'('' IMPOSSIBLE FRACTIONAL OCCUPANCY.'')')
          ISTOP=1                                                       GDH1095
          IWHERE=144                                                    GDH1095
          RETURN                                                        GDH1095
            ENDIF
         ENDIF
         NOPEN=NOPEN+NCLOSE
      ENDIF
      YY=DBLE(KHARGE)/(NORBS+1.D-10)
      L=0
      DO 100 I=1,NUMAT
         NI=NAT(I)
         XX=1.D0
         IF(NI.GT.2) XX=0.25D0
         W=CORE(NI)*XX-YY
         IA=NFIRST(I)
         IC=NMIDLE(I)
         IB=NLAST(I)
         DO 80 J=IA,IC
         L=L+1
   80    PSPD(L)=W
         DO 90 J=IC+1,IB
         L=L+1
   90    PSPD(L)=0.D0
  100 CONTINUE
C
C   WRITE OUT GAS-PHASE REFERENCES
C
      IF (INOREF.EQ.0) THEN                                             DJG0995
      WRITE (JOUT, 180)                                                 PDW1099
         DO 102 I=1,107
         IF (EXIST(I)) WRITE(JOUT,'(A)') REFER(I,MODEL)
  102    CONTINUE
C
C   WRITE OUT SOLVATION REFERENCES                                      PDW1099
C   I realize there are probably about 10 better ways of doing this than 
C   formatted write statements -- but I don't know how to do them. 
C
C   REFERENCE FOR CM 1 
C 
      IF (ICHMOD.EQ.1) THEN
      WRITE(JOUT,190)                                                   PDW1099
      WRITE(JOUT,380)                                                   PDW1099
      ENDIF
C
C   REFERENCE FOR CM2
C
      IF (ICHMOD.EQ.2) THEN
      WRITE(JOUT,200)
      WRITE(JOUT,220)                                                   PDW1099
      ENDIF
C
C --- REFERENCE FOR CM3 ---                                             !JT0802
C
      IF (ICHMOD.EQ.3) THEN                                             !JT0802
        WRITE(JOUT,201)                                                 !JT0802
        WRITE(JOUT,221)                                                 !JT0802
      END IF                                                            !JT0802
C     THE VALUE OF IRAD SPECIFIES THE TYPE OF CALCULATION               DJG1094
C                                                                       DJG1094
C     IRAD=0   AQUEOUS SM1-AM1                                          DJG1094
      IF (IRAD.EQ.0) THEN                                               PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,230)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=1   AQUEOUS SM1A-AM1                                         DJG1094
      IF (IRAD.EQ.1) THEN                                               PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,230)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=2   AQUEOUS SM2-AM1                                          DJG1094
      IF (IRAD.EQ.2) THEN                                               PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,240)                                                   PDW1099
      WRITE(JOUT,260)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=3   AQUEOUS SM3-PM3                                          DJG1094
      IF (IRAD.EQ.3) THEN                                               PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,250)                                                   PDW1099
      WRITE(JOUT,260)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=4   AQUEOUS AM1- or PM3-SM5.4U or AM1-SM5.4A or PM3-SM5.4P   DJG0796
      IF (IRAD.EQ.4) THEN                                               PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,310)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=5   AQUEOUS PD-SM5U PD-SM5A or PD-SM5P or                    DJG0796
C                      SM5/A2PD or SM5/P2PD                             GDH0596
      IF (IRAD.EQ.5) THEN                                               PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,350)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=6   ALKANE  SM4-AM1 OR PM3                                   DJG1094
      IF (IRAD.EQ.6) THEN                                               PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,270)                                                   PDW1099
      WRITE(JOUT,290)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=7   ORGANIC SM5.4 MODEL (/A OR /P)                           DJG1296
      IF (IRAD.EQ.7) THEN                                               PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,320)                                                   PDW1099
      WRITE(JOUT,330)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=8   CHLOROFORM,BENZENE, OR TOLUENE SM5.4 MODEL (/A OR /P)    DJG1296
      IF (IRAD.EQ.8) THEN                                               PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,320)                                                   PDW1099
      WRITE(JOUT,330)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=9   SM5.2R MODEL                                             GDH0897
      IF (IRAD.EQ.9) THEN                                               PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,370)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=10  SM5.42R MODEL                                            GDH1197
      IF (IRAD.EQ.10) THEN                                              PDW1099
         IF (ISM542.EQ.0) THEN                                          PDW1299 
            WRITE(JOUT,207)                                             PDW1299
            WRITE(JOUT,390)                                             PDW1299
            WRITE(JOUT,420)                                             PDW0901
         ENDIF                                                          PDW1299 
         IF (ISM542.EQ.1) THEN                                          PDW1199
         WRITE(JOUT,208)                                                PDW1299
         WRITE(JOUT,390)                                                PDW1299
         WRITE(JOUT,410)                                                PDW1299 
         WRITE(JOUT,420)                                                PDW0901
         ENDIF                                                          PDW1299 
      ENDIF                                                             PDW1099
C     IRAD=12  AQUEOUS SM2.1-AM1                                        DJG1094
      IF (IRAD.EQ.12) THEN                                              PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,240)                                                   PDW1099
      WRITE(JOUT,280)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=13  AQUEOUS SM3.1-PM3                                        DJG1094
      IF (IRAD.EQ.13) THEN                                              PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,250)                                                   PDW1099
      WRITE(JOUT,280)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=20  AQUEOUS SM5.0R                                           GDH0297
      IF (IRAD.EQ.20) THEN                                              PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,400)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=21  ORGANIC SM5.0R                                           GDH0797
      IF (IRAD.EQ.21) THEN                                              PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,360)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=22  AQUEOUS SM2.2-AM1                                        GDH0895
      IF (IRAD.EQ.22) THEN                                              PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,240)                                                   PDW1099
      WRITE(JOUT,300)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=23  AQUEOUS SM2.3-AM1                                        GDH0296
      IF (IRAD.EQ.23) THEN                                              PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,240)                                                   PDW1099
      ENDIF                                                             PDW1099
C     IRAD=33  AQUEOUS SM3.3-PM3                                        GDH0296
      IF (IRAD.EQ.33) THEN                                              PDW1099
      WRITE(JOUT,205)                                                   PDW1099
      WRITE(JOUT,250)                                                   PDW1099
      ENDIF                                                             PDW1099
      ENDIF                                                             GDH1196
      ENDIF
C
C   WRITE OUT THE INTERATOMIC DISTANCES
C
      CALL GMETRY(GEO,COORD)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      RMIN=100.D0
      L=0
      DO 110 I=1,NUMAT
      DO 110 J=1,I
      L=L+1
      RXYZ(L)=SQRT((COORD(1,I)-COORD(1,J))**2+
     .(COORD(2,I)-COORD(2,J))**2+(COORD(3,I)-COORD(3,J))**2)
      IF (RMIN.GT.RXYZ(L) .AND. I .NE. J .AND.
     1   (NAT(I).LT.103 .OR. NAT(J).LT.103)) THEN
         IMINR=I
         JMINR=J
         RMIN=RXYZ(L)
      ENDIF
  110 CONTINUE
      IF ((ICR.EQ.1).OR.(IPRGEO.EQ.1.OR.I1SCF.EQ.0)) THEN
      IF (INOINT.EQ.0) THEN                                             DJG0995
         WRITE(JOUT,'(/10X,''  Interatomic distances'')')
         CALL VECPRT(RXYZ,NUMAT)
      ENDIF
      ENDIF
      IF (RMIN.LT.0.8D0.AND.IGEOOK.EQ.0) THEN                           DJG0995
         WRITE(JOUT,120)IMINR,JMINR,RMIN
  120    FORMAT(' ATOMS',I3,' AND',I3,' ARE SEPARATED BY',F8.4,
     1   ' ANGSTROMS.'/8X,'TO CONTINUE CALCULATION SPECIFY "GEO-OK"')
          ISTOP=1                                                       GDH1095
          IWHERE=145                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF

      IF (ISM505.EQ.1) THEN                                             GDH0696
      WRITE(JOUT,'(/,''Environment specific atom types for aqueous '',  GDH0297
     1               ''solvation energy'',/)')                          GDH0297
      DO 157 I=1,NUMAT                                                  GDH0297
      IF (ITYPE(I).NE.0) THEN                                           GDH0297
      NTA2=NAT(I)                                                       GDH0297
      IF (NTA2.EQ.8) THEN                                               GDH0297
         ITPR=ITYPE(I)+5                                                GDH0297
      ELSEIF (NTA2.EQ.16) THEN                                          GDH0297
         ITPR=ITYPE(I)+7                                                GDH0297
      ELSEIF (NTA2.EQ.1) THEN                                           GDH0297
         ITPR=ITYPE(I)                                                  GDH0297
      ELSE                                                              GDH0297
            WRITE(JOUT,'(
     .      '' ONE OF THE ATOMS MIS-TYPED IN A SM5.05R'',
     .      '' CALCULATION.'')')                                        GDH0297
      ISTOP=1                                                           GDH1095
      IWHERE=62                                                         GDH1095
      ENDIF
      WRITE(JOUT,158)I,DTYPE(ITPR)                                      GDH0297
      ENDIF                                                             GDH0297
157   CONTINUE                                                          GDH0297
      ENDIF                                                             GDH0297
158   FORMAT(' Atom #',I3,' assigned as ',A50)                          GDH0297
      IF(.NOT. DEBUG) RETURN
      WRITE(JOUT,130)NUMAT,NORBS,NDORBS,NATOMS
  130 FORMAT(' NUMBER OF REAL ATOMS:',I4/'   NUMBER OF ORBITALS:',I4/
     .,' NUMBER OF D ORBITALS:',I4/,'   TOTAL NO. OF ATOMS:',I4)
      WRITE(JOUT,140)(USPD(I),I=1,NORBS)
  140 FORMAT(' ONE-ELECTRON DIAGONAL TERMS',10(/,10F8.3))
      WRITE(JOUT,150)(PSPD(I),I=1,NORBS)
  150 FORMAT(' INITIAL P FOR ALL ATOMIC ORBITALS',10(/,10F8.3))
  180 FORMAT(1X,'REFERENCES FOR PARAMETERS IN GAS-PHASE',               PDW1099
     .       ' HAMILTONIAN:',/)                                         PDW1099
  190 FORMAT(/,1X,'REFERENCE FOR CHARGE MODEL 1:')                      PDW1099
  200 FORMAT(/,1X,'REFERENCE FOR CHARGE MODEL 2:')                      PDW1099
  201 FORMAT(/,1X,'REFERENCES FOR CHARGE MODEL 3:')                     !JT0802
  205 FORMAT(/,1X,'REFERENCE(S) FOR CHOSEN SOLVATION MODEL:')           PDW1099
  207 FORMAT(/,1X,'REFERENCE FOR THE PARAMETERIZATION OF THE SM5.42R ', PDW1199
     .      'SOLVATION MODEL:')                                         PDW1199
  208 FORMAT(/,1X,'REFERENCE FOR THE PARAMETERIZATION OF THE SM5.42 ',  PDW1299
     .      'SOLVATION MODEL:')                                         PDW1299
  210 FORMAT(1X,A70,/)                                                  PDW1099
  220 FORMAT(/,1X,'J. Li, J. Xing, C. J. Cramer, and D. G. Truhlar, ',  PDW1099
     .      'J. Chem. Phys. 111 (1999) 885.')                           PDW1099
  221 FORMAT(/,1X,'P. Winget, J. D. Thompson, J. D. Xidos, C. J., ',    !JT0802
     .      'Cramer, and D. G. Truhlar,', /,
     .      1X,'J. Phys. Chem A, 2002,',                                !JT0802
     .      'submitted.',/,/,                                           !JT0802
     .      1X,'J. D. Thompson, C. J. Cramer, and D. G. Truhlar, ',     !JT0802
     .      'J. Comput. Chem., 2002,',/, 1X,'submitted.')               !JT0802
  230 FORMAT(/,1X,'C. J. Cramer, and D. G. Truhlar, J. ',               PDW1099
     .       'Am. Chem. Soc. 113, (1991), 8305, 9901(E).')              PDW1099
  240 FORMAT(/,1X,'C. J. Cramer and D. G. Truhlar, Science ',           PDW1099
     .       '(Washington DC) 256 (1992) 213-217.')                     PDW1099
  250 FORMAT(/,1X,'C. J. Cramer, and D. G. Truhlar, J. ',               PDW1099
     .       'Comput. Chem. 13 (1992) 1089-1097.')                      PDW1099
  260 FORMAT(/,1X,'C. J. Cramer, and D. G. Truhlar, J. ',               PDW1099
     .       'Comput.-Aided Mol. Des. 6 (1992) 629-666')                PDW1099
  270 FORMAT(/,1X,'D. J. Giesen, J. W. Storer, C. J. Cramer, ',         PDW1099
     .       'and D. G. Truhlar J. Am. Chem. Soc. '                     PDW1099
     1/,3X,"117 (1995) 1057-1068.")                                     PDW1099
  280 FORMAT(/,1X,'D. A. Liotard, G. D. Hawkins, G. C. Lynch, ',        PDW1099
     .      'C. J. Cramer, and D. G. Truhlar, '                         PDW1099
     1/,3X,"J. Comp. Chem. 16 (1995) 422-440")                          PDW1099
  290 FORMAT(/,1X,'D. J. Giesen, C. J. Cramer, and D. G. Truhlar, J. ', PDW1099
     .      ' Phys. Chem. 99 (1995) ',/,3X,'7137-7146.')                PDW1099
  300 FORMAT(/,1X,'G. D. Hawkins, C. J. Cramer, and D. G. Truhlar, ',   PDW1099
     .       'Chem. Phys. Lett. 246 (1995) ',/,3X,'122-129.')           PDW1099
  310 FORMAT(/,1X,'C. C. Chambers, G. D. Hawkins, C. J. Cramer, ',      PDW1099
     .      'and D. G. Truhlar, J. Phys.',/,3X,'Chem. 100 (1996)',      PDW1099
     .      ' 16385-16398.')                                            PDW1099
  320 FORMAT(/,1X,'D. J. Giesen, M. Z. Gu, C. J. Cramer, and D. G. ',   PDW1099
     .      'Truhlar, J. Org. Chem. 61',/,3X,'(1996) 8720-8721.')       PDW1099
  330 FORMAT(/,1X,'D. J. Giesen, D. G. Hawkins, D. A. Liotard, ',       PDW1099
     .      'C. J. Cramer, and D. G. Truhlar,  ',                       PDW1099
     1/,3X,"Theor. Chim. Acta, 98 (1997) 85-109")                       PDW1099
  340 FORMAT(/,1X,'D. J. Giesen, C. C. Chambers, C. J. Cramer, ',       PDW1099
     .       'and D. G. Truhlar, J. Phys. Chem. B ',                    PDW1099
     1/,3X,"101 (1997) 2061-2069.")                                     PDW1099
  350 FORMAT(/,1X,'G. D. Hawkins, C. J. Cramer, and D. G. Truhlar, J. ',PDW1099
     .       'Phys. Chem. 100 (1996) ',/,3X,'19824-19839.')             PDW1099
  360 FORMAT(/,1X,'G. D. Hawkins, D. A. Liotard, C. J. Cramer, ',       PDW1099
     .       'and D. G. Truhlar, J. Org. Chem. ',                       PDW1099
     1/3X,"63 (1998) 4305-4313.")                                       PDW1099
  370 FORMAT(/,1X,'G. D. Hawkins, C. J. Cramer, and D. G. Truhlar, ',   PDW1099
     .       'J. Phys. Chem. B',/,3X,'102 (1998) 3257-3271.')           PDW1099
  380 FORMAT(/,1X,'J. W. Storer, D. J. Giesen, C. J. Cramer, ',         PDW1099
     .       'and D. G. Truhlar, J. Comput.-Aided ',                    PDW1099
     1/,3X,"Mol. Des. 9 (1995) 87-110")                                 PDW1099
  390 FORMAT(/,1X,'J. Li, T. Zhu, G. D. Hawkins, P. Winget, ',          PDW1099
     .       'D. A. Liotard, C. J. Cramer, and '                        PDW1099
     1/,3X,"D. G. Truhlar, Theor. Chem. Acc. 103 (1999) 9-63")          PDW1299
  400 FORMAT(/,1X,'G. D. Hawkins, C. J. Cramer, and D. G. Truhlar, ',   PDW1099
     .       'J. Phys. Chem. B',/,3X,'101 (1997) 7147-7157.')           PDW1099
  410 FORMAT(/,1X,'Y.-Y. Chuang, M. L.  Radhakrishnan, P. L. Fast, ',   PDW1199
     .       'C. J. Cramer,',/,3X,'and D. G. Truhlar, J. Phys. Chem. ', PDW1199
     .       'A. 103 (1999) 4893.')                                     PDW1999
C 420 FORMAT(/,1X,'P. Winget, C. J. Cramer, and D. G. Truhlar, J.',     PDW0901
C    .       ' Phys. Chem. B. submitted.')                              PDW0901
  420 FORMAT(/,1X,'P. Winget, J. D. Thompson C. J. Cramer,',            !JT0102
     .       'and D. G. Truhlar, J. Phys. Chem. B. submitted.')         !JT0102

      RETURN
      END
