      SUBROUTINE WRTKEY                                                 DJG0995
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'KEYS.i'
       INCLUDE 'FFILES.i'                                               GDH1095
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTCOM/ LDER                                              GDH1093
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      LOGICAL AM1,MNDO,MINDO3,PM3,MNDOC
      LOGICAL LDER
      CHARACTER SPACE*1, DOT*1, ZERO*1, NINE*1, CH*1, SOLNAM*20         DJG0995
C ----------------------------------------------------------------------
C     WRITE HEADER.
C ----------------------------------------------------------------------
      DATA SPACE,DOT,ZERO,NINE /' ','.','0','9'/
C
C
C  RESTART
      IF (IRESTA.NE.0) WRITE(JOUT,240)                                  DJG0995
C
C  0 OR 1 SCF
      IF (I0SCF.NE.0) WRITE(JOUT,890)                                   DJG0995
      IF (I1SCF.NE.0) WRITE(JOUT,410)                                   DJG0995
C
C  2-D GRID
      IF (ISTEP1.NE.0) THEN                                             DJG0995
         WRITE(JOUT,970)
      ENDIF
C
C  OPTIMISATION SECTION
      IF (INEWTO.NE.0) THEN                                             DJG0995
         WRITE(JOUT,900)
      ENDIF
      IF (ILTRD.NE.0) THEN                                              DJG0995
         WRITE(JOUT,910)
      ENDIF
      IF (IPOWEL.NE.0) THEN                                             DJG0995
         WRITE(JOUT,920)
      ENDIF
      IF (IPATH.NE.0) THEN                                              DJG0995
         WRITE(JOUT,930)
         IF (ITV.NE.0) WRITE(JOUT,950)                                  DJG0995
         IF (IWEIGH.NE.0) WRITE(JOUT,960)                               DJG0995
      ENDIF
      IF (ICHAIN.NE.0) THEN                                             DJG0995
         WRITE(JOUT,940)
      ENDIF
      IF (IFORCE.NE.0) THEN                                             DJG0995
         WRITE(JOUT,430)
         IF (ILET.NE.0) WRITE(JOUT,620)                                 DJG0995
         IF (IISOTO.NE.0) WRITE(JOUT,480)                               DJG0995
      ELSE IF (IISOTO.NE.0.AND.(INEWTO+ILTRD).NE.0) THEN                DJG0955
         WRITE(JOUT,590)
      ENDIF
      IF (IFORCE+INEWTO+ILTRD.NE.0) THEN                                DJG0995
         IF (ITHERM.NE.0)THEN                                           DJG0995
            WRITE(JOUT,850)
            IF (IROT.NE.0) WRITE(JOUT,860) IIROT                        DJG0995
            IF (ITRANS.NE.0) WRITE(JOUT,580) IITRAN                     DJG0995
         ENDIF
      ENDIF
      IF (ISIGMA.NE.0) THEN                                             DJG0995
         WRITE(JOUT,550)
      ENDIF
      IF (INLLSQ.NE.0) THEN                                             DJG0995
         WRITE(JOUT,560)
      ENDIF
      IF (IDRC.NE.0) THEN                                               DJG0995
         WRITE(JOUT,140)
         IF (IDRC.EQ.2) WRITE(JOUT,150) FIDRC                           DJG0995
         IF (IKINET.NE.0) WRITE(JOUT,160) FIKINE                        DJG0995
      ENDIF
      IF ((IBFGS+IDFP).NE.0) THEN                                       DJG0995
         IF (IBFGS.NE.0) THEN                                           DJG0995
            WRITE(JOUT,980)                                             DJG0195
         ELSE                                                           IR0295
            WRITE(JOUT,985)                                             IR0295
         ENDIF                                                          IR0295
         IF (IFULSC.NE.0) WRITE(JOUT,1010)                              DJG0995
         IF (IGCOMP.NE.0) WRITE(JOUT,1130) FIGCOM                       DJG0995
      ENDIF                                                             DJG0195
      IF (ITRUSE.NE.0) THEN                                             DJG0496
         WRITE(JOUT,2170)                                               DJG0496
         IF (IGCOMP.NE.0) WRITE(JOUT,1130) FIGCOM                       DJG0496
      ENDIF                                                             DJG0496
      IF (ITRUSG.NE.0) THEN                                             DJG0496
         WRITE(JOUT,2180)                                               DJG0496
         IF (IGCOMP.NE.0) WRITE(JOUT,1130) FIGCOM                       DJG0496
      ENDIF                                                             DJG0496
      NUMOPT=ISTEP1+INEWTO+ILTRD+IPOWEL+IPATH+ICHAIN+IFORCE+ISIGMA+     DJG0995
     1       INLLSQ+IDRC+IDFP+IBFGS+ITRUSE+ITRUSG                       DJG0496
      IF (NUMOPT.EQ.0) THEN                                             DJG0995
C                                                                       IR1294
C       Start of EF section                                             IR1294
C                                                                       IR1294
         IF (IGCOMP.NE.0) WRITE(JOUT,1130) FIGCOM                       DJG0995
         IF (ITSTAT.NE.0) THEN                                          DJG0995
            WRITE(JOUT,1200)                                            DJG0195
         ELSE IF (IEFOLL.EQ.0) THEN                                     DJG0995
            WRITE(JOUT,1195)                                            DJG0195
         ELSE                                                           DJG0195
            WRITE(JOUT,1190)                                            DJG0195
         ENDIF                                                          DJG0195
         IF (IIUPD.NE.0) THEN                                           DJG0995
            IF (IIUPD.EQ.0) WRITE(JOUT,1210)                            DJG0995
            IF (IIUPD.EQ.1) WRITE(JOUT,1220)                            DJG0995
            IF (IIUPD.EQ.2) WRITE(JOUT,1230)                            DJG0995
         ENDIF                                                          IR1294
         IF (INONR.NE.0 )WRITE(JOUT,1240)                               DJG0995
         IF (IHESS.NE.0) THEN                                           DJG0995
            IF (IIHESS.EQ.0) WRITE(JOUT,1250)                           DJG0995
            IF (IIHESS.EQ.1) WRITE(JOUT,1260)                           DJG0995
            IF (IIHESS.EQ.2) WRITE(JOUT,1270)                           DJG0995
            IF (IIHESS.EQ.3) WRITE(JOUT,1280)                           DJG0995
            IF (IIHESS.EQ.5) WRITE(JOUT,1281)                           1203BL04
         ENDIF                                                          IR1294
         IF (IMODE.NE.0) WRITE(JOUT,1290) IIMODE                        DJG0995
         IF (IRSCAL.NE.0 )WRITE(JOUT,1300)                              DJG0995
         IF (IRECAL.NE.0) WRITE(JOUT,1310) IIRECA                       DJG0995
         IF (IDMAX.NE.0) WRITE(JOUT,1320)FIDMAX                         DJG0995
         IF (IOMIN.NE.0) WRITE(JOUT,1330)FIOMIN                         DJG0995
         IF (IRMIN.NE.0) WRITE(JOUT,1340)FIRMIN                         DJG0995
         IF (IRMAX.NE.0) WRITE(JOUT,1350)FIRMAX                         DJG0995
         IF (IDDMIN.NE.0) WRITE(JOUT,1360)FIDDMI                        DJG0995
         IF (IDDMAX.NE.0) WRITE(JOUT,1370)FIDDMA                        DJG0995
C                                                                       IR1294
C       END of EF section                                               IR1294
C                                                                       IR1294
      ENDIF
      IF (ICYCLE.NE.0) WRITE(JOUT,840) IICYCL                           DJG0995
      IF (IGNORM.NE.0) WRITE(JOUT,670) FIGNOR                           DJG0995
      IF (IPRINT.NE.0) WRITE(JOUT,650) IIPRIN                           DJG0995
      IF (IXYZ.NE.0) WRITE(JOUT,190)                                    DJG0995
      IF (ICART.NE.0) WRITE(JOUT,2160)                                  GDH0696
      IF (IGEOOK.NE.0) WRITE(JOUT,100)                                  DJG0995
      IF (IROOT.NE.0) WRITE(JOUT,570) IIROOT                            DJG0995
C
C  SYMMETRY CONDITION
      IF (ISYMME.NE.0) WRITE(JOUT,370)                                  DJG0995
C
C  INITIAL & FINAL PRINTOUT
      IF (INOINT.NE.0) WRITE(JOUT,470)                                  DJG0995
      IF (IPRGEO.NE.0) WRITE(JOUT,2310)                                 PDW1099
      IF (INOXYZ.NE.0) WRITE(JOUT,540)                                  DJG0995
      IF (IVECTO.NE.0) WRITE(JOUT,40)                                   DJG0995
      IF (IDENSI.NE.0) WRITE(JOUT,50)                                   DJG0995
      IF (ISPIN.NE.0) WRITE(JOUT,60)                                    DJG0995
      IF (IHYPER.NE.0) WRITE(JOUT,790)                                  DJG0995
      IF (ITIMES.NE.0) WRITE(JOUT,70)                                   DJG0995
      IF (IBONDS.NE.0) WRITE(JOUT,90)                                   DJG0995
      IF (IFOCK.NE.0) WRITE(JOUT,110)                                   DJG0995
      IF (I1ELEC.NE.0) WRITE(JOUT,120)                                  DJG0995
      IF (IESR.NE.0) WRITE(JOUT,130)                                    DJG0995
      IF (ILOCAL.NE.0) WRITE(JOUT,170)                                  DJG0995
      IF (IMULLI.NE.0) WRITE(JOUT,180)                                  DJG0995
      IF (IPISIG.NE.0) WRITE(JOUT,200)                                  DJG0995
      IF (IENPAR.NE.0) WRITE(JOUT,530)                                  DJG0995
      IF (IGRADI.NE.0) WRITE(JOUT,260)                                  DJG0995
      IF (IPOLAR.NE.0) WRITE(JOUT,220)                                  DJG0995
      IF (INOREF.NE.0) WRITE(JOUT,740)                                  DJG0995
      IF (IPRCOU.NE.0) WRITE(JOUT,2320)                                 PDW1199
      IF (IPRRAD.NE.0) WRITE(JOUT,2330)                                 PDW1199
      IF (IPRPOL.NE.0) WRITE(JOUT,2340)                                 PDW1199
C
C  NON-STANDARD OUTPUT (FILES)
      IF (IDENOU.NE.0) WRITE(JOUT,490)                                  DJG0995
C
C  DEBUG...
      IF (IDEBUG.NE.0) WRITE(JOUT,230)                                  DJG0995
      IF (LDER.AND.(IDERINU.NE.0)) WRITE(JOUT,1030)                     GDH0997
      IF (IDERIS.NE.0) WRITE(JOUT,1031)                                 DJG0995
      IF (IFORWR.NE.0) WRITE(JOUT,1120)                                 DJG0995
      IF (IFAIL.NE.0) WRITE(JOUT,1040)                                  DJG0995
C
C  ... AND DEDICATED PRINTOUT
      IF (IDEBUP.NE.0) WRITE(JOUT,750)                                  DJG0995
      IF (IFLEPO.NE.0) WRITE(JOUT,80)                                   DJG0995
      IF (ICOMPF.NE.0) WRITE(JOUT,630)                                  DJG0995
      IF (IDERIV.NE.0) WRITE(JOUT,640)                                  DJG0995
      IF (IDCART.NE.0) WRITE(JOUT,660)                                  DJG0995
      IF (IFMAT.NE.0) WRITE(JOUT,680)                                   DJG0995
      IF (IHCORE.NE.0) WRITE(JOUT,690)                                  DJG0995
      IF (IITER.NE.0) WRITE(JOUT,700)                                   DJG0995
      IF (ILINMI.NE.0) WRITE(JOUT,720)                                  DJG0995
      IF (INLLSQ.NE.0) WRITE(JOUT,730)                                  DJG0995
      IF (ILOCMI.NE.0) WRITE(JOUT,735)                                  DJG0995
      IF (IEIGS.NE.0) WRITE(JOUT,770)                                   DJG0995
      IF (IMOLDA.NE.0) WRITE(JOUT,780)                                  DJG0995
      IF (IPLITE.NE.0) WRITE(JOUT,810)                                  DJG0995
      IF (ISEARC.NE.0) WRITE(JOUT,820)                                  DJG0995
      IF (IPOWSQ.NE.0) WRITE(JOUT,825)                                  DJG0995
      IF (IDERI1.NE.0) WRITE(JOUT,990)                                  DJG0995
      IF (IDERI2.NE.0) WRITE(JOUT,1000)                                 DJG0995
      IF (IMECI.NE.0) WRITE(JOUT,1020)                                  DJG0995
C
C  ALLOWED TIME
      IF(ITLIMI.NE.0) WRITE(JOUT,400) FITLIM                            DJG0995
C
C  HALF ELECTRON AT SCF LEVEL
      IF(IOPEN.NE.0) WRITE(JOUT,390) IIOPE1, IIOPE2                     DJG0995
C
C  CONFIGURATION INTERACTION
      IF (ICI.NE.0) THEN                                                DL0397
         WRITE(JOUT,420) IICI                                           DJG0995
         IF (ICIS.NE.0) WRITE(JOUT,421)                                 DL0397
      ENDIF                                                             DL0397
      IF (IMICRO.NE.0) WRITE(JOUT,380)IIMICR                            DJG0995
C
C  CHARGE & MULTIPLICITY
      IF (ICHARG.NE.0) WRITE(JOUT,250) IICHAR                           DJG0995
      IF (ISINGL.NE.0) WRITE(JOUT,310)                                  DJG0995
      IF (IDOUBL.NE.0) WRITE(JOUT,320)                                  DJG0995
      IF (ITRIPL.NE.0) WRITE(JOUT,330)                                  DJG0995
      IF (IQUART.NE.0) WRITE(JOUT,340)                                  DJG0995
      IF (IQUINT.NE.0) WRITE(JOUT,350)                                  DJG0995
      IF (ISEXTE.NE.0) WRITE(JOUT,360)                                  DJG0995
C
      IF (IUHF.NE.0) WRITE(JOUT,270)                                    DJG0995
      IF (IBIRAD.NE.0) WRITE(JOUT,290)                                  DJG0995
      IF (IEXCIT.NE.0) WRITE(JOUT,300)                                  DJG0995
C
C  MODEL
      MINDO3= IMINDO.NE.0
      AM1   = IAM1.NE.0
      PM3   = IPM3.NE.0
      MNDOC = IMNDOC.NE.0
      MNDO  = .NOT.(MINDO3.OR.AM1.OR.PM3)
      IF (IMNDO.NE.0) MNDO = .TRUE.
      IF (MINDO3) THEN
         WRITE(JOUT,440)
      ENDIF
      IF (AM1   ) THEN
         WRITE(JOUT,450)
      ENDIF
      IF (PM3   ) THEN
         WRITE(JOUT,610)
      ENDIF
      IF (MNDOC ) THEN
         WRITE(JOUT,1090)
      ENDIF
C
C  DRIVING THE SCF
C
      IF (IPULAY.NE.0) WRITE(JOUT,710)
      IF (ICAMPK.NE.0) WRITE(JOUT,760)
      IF (ISHIFT.NE.0) WRITE(JOUT,500) FISHIF
      IF (IOLDEN.NE.0) WRITE(JOUT,510)
C      IF (ISCFCR.NE.0) WRITE(JOUT,520) MAX(1D-11,FISCFC)               1201BL04
      IF (ISCFCR.NE.0) WRITE(JOUT,520) FISCFC                           1201BL04
      IF (IITRY.NE.0) WRITE(JOUT,870) IIITRY                            DJG0995
      IF (IFILL.NE.0) WRITE(JOUT,830) IIFILL                            DJG0995
C
C     THE FOLLOWING SECTION WAS ADDED BY DJG BY TAKING LINES FROM READS
C     0995
C
      IF (IOPT.NE.0) WRITE(JOUT,1400)
      IF (INOPOL.NE.0) WRITE (JOUT,1410)
      IF (IDEV.NE.0) WRITE(JOUT,1420)
      IF (IEXTCM.EQ.0) THEN
         IF (IRAD.EQ.0) THEN
            WRITE(JOUT,1430)
            WRITE(JOUT,1780)
         ELSE IF (IRAD.EQ.1) THEN
            WRITE(JOUT,1440)
            WRITE(JOUT,1780)
         ELSE IF (IRAD.EQ.2) THEN                                       GDH1293
            WRITE(JOUT,1450)
            WRITE(JOUT,1780)
         ELSE IF (IRAD.EQ.3) THEN                                       GDH1293
            WRITE(JOUT,1460)
            WRITE(JOUT,1780)
         ELSE IF (IRAD.EQ.4.OR.IRAD.EQ.7.OR.IRAD.EQ.8) THEN             DJG1296
            IF(ISM5U.EQ.1)WRITE(JOUT,1470)                              DJG0796
            IF(ISM5A.EQ.1)WRITE(JOUT,1471)                              CCC0496
            IF(ISM5P.EQ.1)WRITE(JOUT,1472)                              CCC0496
            IF (IAM1.NE.0) THEN
               WRITE(JOUT,1760)
            ELSE IF (IPM3.NE.0) THEN
               WRITE(JOUT,1770)
            ENDIF
         ELSE IF (IRAD.EQ.9) THEN                                       GDH0997
            IF (ISM52.EQ.1) THEN                                        PDW1199 
            WRITE(JOUT,1476)
            ELSE
            WRITE(JOUT,1473)                                            GDH0997
            ENDIF
         ELSE IF (IRAD.EQ.10) THEN                                      GDH0997
            IF (ISM542.EQ.1) THEN
            WRITE(JOUT,1475)
            ELSE
            WRITE(JOUT,1474)                                            GDH0997
            ENDIF
            IF (IAM1.NE.0) THEN
               WRITE(JOUT,1761)
            ELSE IF (IPM3.NE.0) THEN
               WRITE(JOUT,1771)
            ENDIF
         ELSE IF (IRAD.EQ.5) THEN                                       GDH0496
            IF (IPDS5U.EQ.1) WRITE(JOUT,2130)                           DJG0796
            IF (IPDS5A.EQ.1) WRITE(JOUT,2140)                           GDH0496
            IF (IPDS5P.EQ.1) WRITE(JOUT,2150)                           GDH0496
            IF (IS5A2P.EQ.1) WRITE(JOUT,2190)                           GDH0496
            IF (IS5P2P.EQ.1) WRITE(JOUT,2200)                           GDH0496
            IF (IS5P2P+IS5A2P.EQ.0) THEN                                GDH0596
            IF (IAM1.NE.0) THEN                                         GDH0496
               WRITE(JOUT,1760)                                         GDH0496
            ELSE IF (IPM3.NE.0) THEN                                    GDH0496
               WRITE(JOUT,1770)
            ENDIF
            ENDIF                                                       GDH0596
         ELSE IF (IRAD.EQ.6) THEN                                       DJG1094
            WRITE(JOUT,1480)
            IF (IAM1.NE.0) THEN
               WRITE(JOUT,1760)
            ELSE IF (IPM3.NE.0) THEN
               WRITE(JOUT,1770)
            ENDIF
         ELSE IF (IRAD.EQ.12) THEN                                      GDH1293
            WRITE(JOUT,1490)
            WRITE(JOUT,1780)
         ELSE IF (IRAD.EQ.13) THEN                                      GDH1293
            WRITE(JOUT,1500)
            WRITE(JOUT,1780)
         ELSE IF (IRAD.EQ.22) THEN                                      GDH1293
            WRITE(JOUT,1491)
            WRITE(JOUT,1780)
         ELSE IF (IRAD.EQ.23) THEN                                      GDH1293
            WRITE(JOUT,1492)
            WRITE(JOUT,1780)
         ELSE IF (IRAD.EQ.33) THEN                                      GDH1293
            WRITE(JOUT,1493)
            WRITE(JOUT,1780)
         ELSE IF (ISM505.NE.0) THEN                                     GDH0497
            WRITE(JOUT,1554)                                            GDH0497
         ELSE IF (ISM50.NE.0) THEN                                      GDH0497
            WRITE(JOUT,1553)                                            GDH0497
         ENDIF                                                          GDH1293
      ELSE                                                              GDH0794
         IF (IRAD.EQ.0) THEN                                            GDH0794
            WRITE(JOUT,1510)
         ELSE IF (IRAD.EQ.1) THEN                                       GDH0794
            WRITE(JOUT,1520)
         ELSE IF (IRAD.EQ.2) THEN                                       GDH0794
            WRITE(JOUT,1530)
         ELSE IF (IRAD.EQ.3) THEN                                       GDH0794
            WRITE(JOUT,1540)
         ELSE IF (IRAD.EQ.4.OR.IRAD.EQ.7.OR.IRAD.EQ.8) THEN             DJG1296
            IF(ISM5A.EQ.1)WRITE(JOUT,1550)                              DJG0896
            IF(ISM5P.EQ.1)WRITE(JOUT,1551)                              DJG0896
            IF(ISM5U.EQ.1)WRITE(JOUT,1552)                              DJG0896
         ELSE IF (IRAD.EQ.5) THEN                                       GDH0696
            IF (IPDS5U.EQ.1) WRITE(JOUT,2131)                           DJG0796
            IF (IPDS5A.EQ.1) WRITE(JOUT,2141)                           GDH0496
            IF (IPDS5P.EQ.1) WRITE(JOUT,2151)                           GDH0496
            IF (IS5A2P.EQ.1) WRITE(JOUT,2191)                           GDH0496
            IF (IS5P2P.EQ.1) WRITE(JOUT,2201)                           GDH0496
         ELSE IF (IRAD.EQ.6) THEN                                       DJG1094
            WRITE(JOUT,1560)
         ELSE IF (IRAD.EQ.12) THEN                                      GDH0794
            WRITE(JOUT,1570)
         ELSE IF (IRAD.EQ.13) THEN                                      GDH0794
            WRITE(JOUT,1580)
         ENDIF                                                          GDH0794
      ENDIF                                                             GDH0794
      IF (IEXTSM.NE.0) WRITE(JOUT,1700)
      IF (IEXTCM.NE.0) WRITE(JOUT,1710)
      IF (ICM1.NE.0) THEN
         IF (IAM1.NE.0) THEN
            WRITE(JOUT,1720)
         ELSE IF (IPM3.NE.0) THEN
            WRITE(JOUT,1730)
         ENDIF
      ENDIF
      IF (ICM2.NE.0) THEN                                               GDH0997
         IF (IAM1.NE.0) THEN                                            GDH0997
            WRITE(JOUT,1721)                                            GDH0997
         ELSE IF (IPM3.NE.0) THEN                                       GDH0997
            WRITE(JOUT,1731)                                            GDH0997
         ENDIF                                                          GDH0997
      ENDIF                                                             GDH0997
      IF (ICM3.NE.0) THEN                                               !JT0802
         IF (IAM1.NE.0) THEN                                            !JT0802
            WRITE(JOUT,1722)                                            !JT0802
         ELSE IF (IPM3.NE.0) THEN                                       !JT0802
            WRITE(JOUT,1732)                                            !JT0802
         ENDIF                                                          !JT0802
      ENDIF                                                             !JT0802
      IF (IRAD.GE.0) THEN
         IF (ISOLVN.NE.0) THEN
            IF (IDIELE.EQ.0.OR.CISOLV.NE.'H2OSRP') THEN
               IF (CISOLV(1:5).EQ.'WATER') THEN                         DJG1294
                  SOLNAM='WATER'
               ELSE IF (CISOLV.EQ.'N5ANE') THEN
                  SOLNAM='PENTANE'                                      DJG1094
               ELSE IF (CISOLV.EQ.'N6ANE') THEN
                  SOLNAM='HEXANE'                                       DJG0195
               ELSE IF (CISOLV.EQ.'N7ANE') THEN
                  SOLNAM='HEPTANE'                                      DJG0195
               ELSE IF (CISOLV.EQ.'N8ANE') THEN
                  SOLNAM='OCTANE'                                       DJG0195
               ELSE IF (CISOLV.EQ.'N9ANE') THEN
                  SOLNAM='NONANE'                                       DJG0195
               ELSE IF (CISOLV.EQ.'N10ANE') THEN
                  SOLNAM='DECANE'                                       DJG0195
               ELSE IF (CISOLV.EQ.'N11ANE') THEN
                  SOLNAM='UNDECANE'                                     DJG0195
               ELSE IF (CISOLV.EQ.'N12ANE') THEN
                  SOLNAM='DODECANE'                                     DJG0195
               ELSE IF (CISOLV.EQ.'N14ANE') THEN
                  SOLNAM='TETRADECANE'                                  DJG0195
               ELSE IF (CISOLV.EQ.'N15ANE') THEN
                  SOLNAM='PENTADECANE'                                  DJG0195
               ELSE IF (CISOLV.EQ.'N16ANE') THEN
                  SOLNAM='HEXADECANE'                                   DJG0195
               ELSE IF (CISOLV.EQ.'2M4ANE') THEN
                  SOLNAM='2-METHYLBUTANE'                               DJG0195
               ELSE IF (CISOLV.EQ.'3M5ANE') THEN
                  SOLNAM='3-METHYLPENTANE'                              DJG0195
               ELSE IF (CISOLV.EQ.'ISOOCT') THEN
                  SOLNAM='ISOOCTANE'                                    DJG0195
               ELSE IF (CISOLV.EQ.'CYC6') THEN
                  SOLNAM='CYCLOHEXANE'                                  DJG0195
               ELSE IF (CISOLV.EQ.'MCYC6') THEN
                  SOLNAM='METHYLCYCLOHEXANE'                            DJG0195
               ELSE IF (CISOLV.EQ.'DECLIN') THEN
                  SOLNAM='DECALIN (MIXTURE OF cis AND trans)'           DJG0195
               ELSE IF (CISOLV.EQ.'GENALK'.OR.CISOLV.EQ.'GENORG') THEN  DJG0297
                  SOLNAM='USER-SPECIFIED'                               DJG0195
               ELSE IF (CISOLV.EQ.'CHCL3') THEN                         DJG0896
                  SOLNAM='CHLOROFORM'                                   DJG0896
               ELSE IF (CISOLV.EQ.'BENZENE') THEN                       DJG0297
                  SOLNAM='BENZENE'                                      DJG0297
               ELSE IF (CISOLV.EQ.'TOLUENE') THEN                       DJG0297
                  SOLNAM='TOLUENE'                                      DJG0297
               ENDIF
            ELSE IF (IRAD.EQ.7) THEN                                    DJG0796
               SOLNAM='ORGANIC'                                         DJG1297
               WRITE(JOUT,1755) SOLNAM                                  DJG0296
            ELSE
               SOLNAM='USER-SPECIFIED'
            ENDIF
            WRITE(JOUT,1740) SOLNAM
         ELSE
            WRITE(JOUT,1750)
         ENDIF                                                          DJG1094
      ENDIF
      IF (IDIELE.NE.0) WRITE(JOUT,1790) FIDIEL
      IF (ISVCDR.NE.0) WRITE(JOUT,1800) FISVCD
      IF (ISVCSR.NE.0) WRITE(JOUT,1810) FISVCS
      IF (IMSURF.NE.0) WRITE(JOUT,1820) FIMSUR
      IF (IIOFR.NE.0)  WRITE(JOUT,2250) FIIOFR                          DJG0796
      IF (IALPHA.NE.0) WRITE(JOUT,2260) FIALPH                          DJG0796
      IF (IBETA.NE.0)  WRITE(JOUT,2270) FIBETA                          DJG0796
      IF (IGAMMA.NE.0) WRITE(JOUT,2280) FIGAMM                          DJG0796
      IF (IFACAR.NE.0) WRITE(JOUT,2290) FFACAR                          TZ0898
      IF (IFEHAL.NE.0) WRITE(JOUT,2300) FFEHAL                          TZ0898
      IF (IRAD1.NE.0) THEN
         IF (IIRAD.EQ.0) THEN
            WRITE(JOUT,1830)
         ELSE
            WRITE(JOUT,1840)
         ENDIF
      ENDIF
      IF (IEXTM.NE.0) WRITE(JOUT,1850)
      IF (ISTDM.NE.0) WRITE(JOUT,1860)
      IF (IXKW.NE.0) WRITE(JOUT,1870)
      IF (INOCOG.NE.0) WRITE(JOUT,1880)
      IF (ICS1.NE.0) WRITE(JOUT,1890)
      IF (ICS2.NE.0) WRITE(JOUT,1900)
      IF (ICS3.NE.0) WRITE(JOUT,1910)
      IF (IKICK.EQ.1) WRITE(JOUT,1920)
      IF (IKICK.EQ.2) THEN
         IF (IIKICK.EQ.0) THEN
            WRITE(JOUT,1930)
         ELSE
            WRITE(JOUT,1940) IIKICK
         ENDIF
      ENDIF
      IF (IDOTS.NE.0) THEN
         WRITE(JOUT,1950)
         IF (INDOTC.NE.0) WRITE(JOUT,1960) IIDOTC
         IF (INDOTE.NE.0) WRITE(JOUT,1970) IIDOTE
      ELSE IF (IGEPOL.NE.0) THEN
         WRITE(JOUT,1980)
         IF (INDIVC.NE.0) WRITE(JOUT,1990) IIDIVC - 1
         IF (INDIVE.NE.0) WRITE(JOUT,2000) IIDIVE - 1
      ELSE IF (IASA.NE.0) THEN
         WRITE(JOUT,2010)
      ENDIF
C GDH0397      IF (INOVOL.NE.0) THEN
C         WRITE(JOUT,2020)
C      ELSE IF (IVOLUM.NE.0) THEN
C         WRITE(JOUT,2030)
C         IF (IGEPOL.EQ.0.AND.INDIVC.NE.0) WRITE(JOUT,2040) IIDIVC
C      ENDIF
      IF (INOINP.NE.0) THEN
         WRITE(JOUT,2050)
      ELSE IF (IINPUT.NE.0) THEN
         WRITE(JOUT,2060)
      ENDIF
      IF (IDENMA.NE.0) WRITE(JOUT,2070)
      IF (IOLDMA.NE.0) WRITE(JOUT,2080)
      IF (ITONE.NE.0) WRITE(JOUT,2090) FITONE
      IF (ITEXPN.NE.0) WRITE(JOUT,2100) FITEXP
      IF (ITRUES.NE.0) THEN                                             DJG0796
         WRITE(JOUT,2110)                                               DJG0796
         IF (IHFCAL.EQ.2) THEN                                          DJG0796
            WRITE(JOUT,2210)                                            DJG0796
         ELSE IF (IHFCAL.EQ.3) THEN                                     DJG0796
            WRITE(JOUT,2220)                                            DJG0796
         ELSE IF (IHF.EQ.1) THEN                                        DJG0796
            WRITE(JOUT,2230)                                            DJG0796
         ELSE IF (IHF.EQ.2) THEN                                        DJG0796
            WRITE(JOUT,2240)                                            DJG0796
         ENDIF                                                          DJG0796
      ENDIF                                                             DJG0796
      IF (IAREAS.NE.0) WRITE(JOUT,2120)
C
C     END OF NEW SECTION 0995 DJG
C
      WRITE(JOUT,'(1X,79(''*''))')
      RETURN
   40 FORMAT(' *  VECTORS  - FINAL EIGENVECTORS TO BE PRINTED')
   50 FORMAT(' *  DENSITY  - FINAL DENSITY MATRIX TO BE PRINTED')
   60 FORMAT(' *  SPIN     - FINAL UHF SPIN MATRIX TO BE PRINTED')
   70 FORMAT(' *  TIMES    - TIMES OF VARIOUS STAGES TO BE PRINTED')
   80 FORMAT(' *  FLEPO    - PRINT DETAILS OF GEOMETRY OPTIMISATION')
   90 FORMAT(' *  BONDS    - FINAL BOND-ORDER MATRIX TO BE PRINTED')
  100 FORMAT(' *  GEO-OK   - OVERRIDE INTERATOMIC DISTANCE CHECK')
  110 FORMAT(' *  FOCK     - LAST FOCK MATRIX TO BE PRINTED')
  120 FORMAT(' *  1ELECTRON- FINAL ONE-ELECTRON MATRIX TO BE PRINTED')
  130 FORMAT(' *  ESR      - RHF SPIN DENSITY CALCULATION REQUESTED')
  140 FORMAT(' *  DRC      - DYNAMIC REACTION COORDINATE CALCULATION')
  150 FORMAT(' *  DRC=     - HALF-LIFE FOR KINETIC ENERGY LOSS =',F9.2,
     1' * 10**(-14) SECONDS')
  160 FORMAT(' *  KINETIC= - ',F7.3,' KCAL KINETIC ENERGY ADDED TO DRC')
  170 FORMAT(' *  LOCALISE - LOCALISED ORBITALS TO BE PRINTED')
  180 FORMAT(' *  MULLIK   - THE MULLIKEN ANALYSIS TO BE PERFORMED')
  190 FORMAT(' *  XYZ     - CARTESIAN COORDINATES USED FOR',            GDH0496
     1' OPTIMIZATION')
  200 FORMAT(' *  PI      - BONDS MATRIX, SPLIT INTO SIGMA-PI-DELL',
     1' COMPONENTS, TO BE PRINTED')
  210 FORMAT(' *  CI-OK    - OVERRIDE MO DEGENERACY CHECK IN C.I.')
  220 FORMAT(' *  POLAR    - CALCULATE THE POLARIZATION VOLUMES')
  230 FORMAT(' *  DEBUG    - DEBUG OPTION TURNED ON')
  240 FORMAT(' *  RESTART  - CALCULATION RESTARTED')
  250 FORMAT(' *  CHARGE   - CHARGE ON SYSTEM=',I3)
  260 FORMAT(' *  GRADIENTS- ALL GRADIENTS TO BE PRINTED')
  270 FORMAT(' *  UHF      - UNRESTRICTED HARTREE-FOCK CALCULATION')
  280 FORMAT(' *  SINGLET  - STATE REQUIRED MUST BE A SINGLET')
  290 FORMAT(' *  BIRADICAL- SYSTEM HAS TWO UNPAIRED ELECTRONS')
  300 FORMAT(' *  EXCITED  - FIRST EXCITED STATE IS TO BE OPTIMISED')
  310 FORMAT(' *  SINGLET  - SPIN STATE DEFINED AS A SINGLET')
  320 FORMAT(' *  DOUBLET  - SPIN STATE DEFINED AS A DOUBLET')
  330 FORMAT(' *  TRIPLET  - SPIN STATE DEFINED AS A TRIPLET')
  340 FORMAT(' *  QUARTET  - SPIN STATE DEFINED AS A QUARTET')
  350 FORMAT(' *  QUINTET  - SPIN STATE DEFINED AS A QUINTET')
  360 FORMAT(' *  SEXTET   - SPIN STATE DEFINED AS A SEXTET')
  370 FORMAT(' *  SYMMETRY - SYMMETRY CONDITIONS TO BE IMPOSED')
  380 FORMAT(' *  MICROS=N -',I4,' MICROSTATES TO BE SUPPLIED FOR C.I.')
  390 FORMAT(' *  OPEN(N,N)- RHF WITH ',I2,' ELECTRONS IN',I2,' LEVELS')
  400 FORMAT(' *  TLIMIT=  - A TIME OF ',F8.0,' SECONDS REQUESTED')     DJG0995
  410 FORMAT(' *  1SCF     - SCF CALCULATION WITHOUT GEOMETRY ',
     .            'OPTIMIZATION ')
  420 FORMAT(' *  C.I.=N   -',I2,' M.O.S TO BE USED IN C.I.')
  421 FORMAT(' *  CIS      - CI RESTRICTED TO SINGLY EXCITED MS.')      DL0397
  430 FORMAT(' *  FORCE    - FORCE CALCULATION SPECIFIED')
  440 FORMAT(' *  MINDO/3  - THE MINDO/3 HAMILTONIAN TO BE USED')
  450 FORMAT(' *  AM1      - THE AM1 HAMILTONIAN TO BE USED')
  470 FORMAT(' *  NOINTER  - INTERATOMIC DISTANCES NOT TO BE PRINTED')
  480 FORMAT(' *  ISOTOPE  - FORCE MATRIX WRITTEN TO DISK (CHAN. 9 )')
  490 FORMAT(' *  DENOUT   - DENSITY MATRIX OUTPUT ON CHANNEL 10')
  500 FORMAT(' *  SHIFT    - A DAMPING FACTOR OF',F8.2,' DEFINED')
  510 FORMAT(' *  OLDENS   - INITIAL DENSITY MATRIX READ OF DISK')
  520 FORMAT(' *  SCFCRT   - DEFAULT SCF CRITERION REPLACED BY ',       GDH1093
     1       1PD7.1)
  530 FORMAT(' *  ENPART   - ENERGY TO BE PARTITIONED INTO COMPONENTS')
  540 FORMAT(' *  NOXYZ    - CARTESIAN COORDINATES NOT TO BE PRINTED')
  550 FORMAT(' *  SIGMA    - GRADIENTS TO BE MINIMIZED USING SIGMA.')
  560 FORMAT(' *  NLLSQ    - GRADIENTS TO BE MINIMISED USING NLLSQ.')
  570 FORMAT(' *  ROOT     - IN A C.I. CALCULATION, ROOT',I2,
     1                       ' TO BE OPTIMISED.')
  580 FORMAT(' *  TRANS=   -',I3,' LOWEST VIBRATIONS TO BE DELETED',
     1' FROM THERMO CALC.')
  590 FORMAT(' *  ISOTOPE  - *** OPTION AVAILABLE FROM "FORCE" ONLY')
  610 FORMAT(' *  PM3      - THE PM3 HAMILTONIAN TO BE USED')
  620 FORMAT(' *   LET     - DO FORCE CALCULATION IN SPITE OF POOR ',
     1'GRADIENT')
  630 FORMAT(' *  COMPFG   - PRINT HEAT OF FORMATION CALC''D IN COMPFG')
  640 FORMAT(' *  DERIV    - PRINT DETAILS OF DERIV')
  650 FORMAT(' *  PRINT    - PRINTOUT LEVEL IN OPTIMISATION =',I2)
  660 FORMAT(' *  DCART    - PRINT DETAILS OF DCART')
  670 FORMAT(' *  GNORM=   - OPTIMIZATION EXIT WHEN GRADIENT NORM BELOW'
     .                      ,F9.3)
  680 FORMAT(' *  FMAT     - PRINT DETAILS OF FMAT')
  690 FORMAT(' *  HCORE    - PRINT DETAILS OF HCORE')
  700 FORMAT(' *  ITER     - PRINT DETAILS OF ITER')
  710 FORMAT(' *  PULAY    - PULAY''S METHOD TO BE USED IN SCF')
  720 FORMAT(' *  LINMIN   - PRINT DETAILS OF FLEPO1')
  730 FORMAT(' *  LOCMIN   - PRINT DETAILS OF LOCMIN')
  735 FORMAT(' *  NLLSQ    - PRINT DETAILS OF NLLSQ1')                  DJG0995
  740 FORMAT(' *  NOREF    - DO NOT PRINT PARAMETER REFERENCES')
  750 FORMAT(' *  DEBUGPULAY-PRINT DETAILS OF PULAY')
  760 FORMAT(' *  CAMP,KING- THE CAMP-KING CONVERGER TO BE USED')
  770 FORMAT(' *  EIGS     - PRINT ALL EIGENVALUES IN ITER')
  780 FORMAT(' *  MOLDAT   - PRINT DETAILS OF MOLDAT')
  790 FORMAT(' *  HYPERFINE- HYPERFINE COUPLING CONSTANTS TO BE'
     1,' PRINTED')
  800 FORMAT(' *   IRC     - FOLLOW IRC, COMPUTING TRANSVERSE HESSIAN')
  810 FORMAT(' *   PL      - MONITOR CONVERGANCE IN DENSITY MATRIX')
  820 FORMAT(' *  SEARCH   - PRINT DETAILS OF SEARCH')
  825 FORMAT(' *  POWSQ    - PRINT DETAILS OF POWSQ1')
  830 FORMAT(' *  FILL=    - IN RHF CLOSED SHELL, FORCE M.O.',I3,' TO BE
     1 FILLED')
  840 FORMAT(' *  CYCLES=  - DO A MAXIMUM OF ',I4,' CYCLES IN OPTIMIZATI
     1ON')
  850 FORMAT(' *  THERMO   - THERMODYNAMIC QUANTITIES TO BE CALCULATED')
  860 FORMAT(' *  ROT      - SYMMETRY NUMBER OF',I3,' SPECIFIED')
  870 FORMAT(' *  ITRY=    - DO A MAXIMUM OF',I6,' ITERATIONS FOR SCF')
  880 FORMAT(' ******************* IMCOMPATIBLE OPTION REQUESTED******')
  890 FORMAT(' *  0SCF     - AFTER READING AND PRINTING DATA, STOP')
  900 FORMAT(' *  NEWTON   - MINIMIZE ENERGY USING FULL-NEWTON')
  910 FORMAT(' *  LTRD     - MINIMISE GRADIENT USING FULL-NEWTON')
  920 FORMAT(' *  POWELL   - MINIMISE GRADIENT USING POWELL METHOD')
  930 FORMAT(' *  PATH     - FOLLOW THE STEEPEST DESCENT PATH')
  940 FORMAT(' *  CHAIN    - TRANSITION STATE TO BE LOCATED')
  950 FORMAT(' *  T.V.     - TRANSITION VECTOR TO BE PROVIDED FOR PATH')
  960 FORMAT(' *  WEIGHT   - WEIGHT TO BE PROVIDED FOR PATH ')
  970 FORMAT(' *  STEP1    - 2-D GRID CALCULATION TO BE PERFORMED')
  980 FORMAT(' *  BFGS     - MINIMIZE ENERGY USING B-F-G-S METHOD ')    DJG0195
  985 FORMAT(' *  DFP      - MINIMIZE ENERGY USING D-F-P METHOD ')      IR0295
  990 FORMAT(' *  DERI1    - PRINT DETAILS OF DERI1')
 1000 FORMAT(' *  DERI2    - PRINT DETAILS OF DERI2')
 1010 FORMAT(' *  FULSCF   - WITH FULL SCF IN EACH LINEAR SEARCH')
 1020 FORMAT(' *  MECI     - PRINT DETAILED RESULTS IN C.I.')
 1030 FORMAT(' *  DERINU   - ALL DERIVATIVES BY NUMERICAL METHOD')
 1031 FORMAT(' *  DERISA   - ALL DERIVATIVES BY SEMI-ANALYTICAL METH',  GDH1093
     1       'OD (DEFAULT)')
 1040 FORMAT(' *  FAIL     - ALL SCF RESTARTED WITH DIAG. DENS.')
 1050 FORMAT(' *  C.I.(    - SPECIFIED RANGE FOR C.I-ACTIVE MOS')
 1060 FORMAT(' *  RECLAS(  - REORDER MOS BEFORE ENTERING C.I.')
 1070 FORMAT(' *  PROTO=',I2,' - CODE FOR PROTOTYPE MOS IN C.I.')
 1080 FORMAT(' *  PERTU=',I3, '- START OF PERTURBATIVE SORT IN C.I.')
 1090 FORMAT(' *  MNDOC    - THE MNDO-C.I. HAMILTONIAN TO BE USED')
 1120 FORMAT(' *  FORWRD   - CALCULATE NUMERICAL DERIVITIVES BY ',      DJG0495
     1       'FORWARD FINITE DIFFERENCES')                              DJG0495
 1130 FORMAT(' *  GCOMP    - THE GEOMETRY CONVERGENCE CRITERION IS SET',DJG0495
     1       ' TO ',F9.4)                                               DJG0495
C                                                                       IR1294
C       EF-specific FORMATs                                             IR1294
C                                                                       IR1294
 1190 FORMAT(' *  EFOLLOW  - USE EF ROUTINE FOR MINIMUM SEARCH',        DJG0195
     1            ' (DEFAULT)')                                         DJG0195
 1195 FORMAT(' *           - USE EF ROUTINE FOR MINIMUM SEARCH',        DJG0195
     1            ' (DEFAULT)')                                         DJG0195
 1200 FORMAT(' *  TSTATE   - USE EF ROUTINE FOR TS SEARCH')             DJG0195
 1210 FORMAT(' *  IUPD=0   - EF: HESSIAN WILL NOT BE UPDATED')          IR1294
 1220 FORMAT(' *  IUPD=1   - EF: HESSIAN UPDATED USING POWELL')         IR1294
 1230 FORMAT(' *  IUPD=2   - EF: HESSIAN UPDATED USING BFGS')           IR1294
 1240 FORMAT(' *  NONR     - EF: DO NOT USE NEWTON-RAPHSON STEP')       IR1294
 1250 FORMAT(' *  HESS=0   - EF: DIAGONAL HESSIAN AS INITIAL GUESS')    IR1294
 1260 FORMAT(' *  HESS=1   - EF: INITIAL HESSIAN CALCULATED USING',     DJG0495
     1       ' FORWARD FINITE DIFFERENCES')                             DJG0495
 1270 FORMAT(' *  HESS=2   - EF: INITIAL HESSIAN READ FROM DISK')       IR1294
 1280 FORMAT(' *  HESS=3   - EF: INITIAL HESSIAN CALCULATED USING',     DJG0495
     1       ' CENTRAL FINITE DIFFERENCES')                             DJG0495
 1281 FORMAT(' *  HESS=5   - EF: INITIAL HESSIAN IS LINDH')             1203BL04
 1290 FORMAT(' *  MODE=    - EF: FOLLOW HESSIAN MODE',I3,' TOWARD TS')  IR1294
 1300 FORMAT(' *  RSCAL    - EF: SCALE P-RFO STEP TO TRUST RADIUS')     IR1294
 1310 FORMAT(' *  RECALC=  - EF: HESSIAN RECALC EVERY',I4,' CYCLES ')   IR1294
 1320 FORMAT(' *  DMAX=    - EF: STARTING TRUST RADIUS',F7.3,' ANG/RAD')IR1294
 1330 FORMAT(' *  OMIN=    - EF: MINIMUM EIGENVECTOR OVERLAP ',F7.3)    IR1294
 1340 FORMAT(' *  RMIN=    - EF: MIN. CALC./PRED. ENERGY STEP ',F7.3)   IR1294
 1350 FORMAT(' *  RMAX=    - EF: MAX. CALC./PRED. ENERGY STEP ',F7.3)   IR1294
 1360 FORMAT(' *  DDMIN=   - EF: MINIMUM TRUST RADIUS',F9.7,' ANG/RAD') DJG0295
 1370 FORMAT(' *  DDMAX=   - EF: MAXIMUM TRUST RADIUS',F7.3,' ANG/RAD') IR1294
C                                                                       IR1294
C       End of  EF-specific FORMATs                                     IR1294
C                                                                       IR1294
 1400 FORMAT(1X,'*  OPT      - THE GEOMETRY WILL BE OPTIMIZED (DEFAU',  GDH1093
     1       'LT)')
 1410 FORMAT(1X,'*  NOPOL    - SOLVATION ENERGY TO BE CALCULATED FOR ', GL0492
     1       'GAS-PHASE WAVE FUNCTION')                                 GL0492
 1420 FORMAT(1X,'*  DEV      - DEVELOPER OPTIONS ARE ALLOWED')          GDH1293
 1430 FORMAT(1X,'*  SM1      - SM1 CALCULATIONS WILL BE PERFORMED')     GL0393
 1440 FORMAT(1X,'*  SM1A     - SM1a CALCULATIONS WILL BE PERFORMED')    GL0393
 1450 FORMAT(1X,'*  SM2      - SM2 CALCULATIONS WILL BE PERFORMED')     GL0393
 1460 FORMAT(1X,'*  SM3      - SM3 CALCULATIONS WILL BE PERFORMED')     GL0393
 1470 FORMAT(1X,'*  SM5.4U   - SM5.4/U CALCULATIONS WILL BE PERFORMED') GDH0696
 1471 FORMAT(1X,'*  SM5.4A   - SM5.4/A CALCULATIONS WILL BE PERFORMED') GDH0696
 1472 FORMAT(1X,'*  SM5.4P   - SM5.4/P CALCULATIONS WILL BE PERFORMED') GDH0696
 1473 FORMAT(1X,'*  SM5.2R   - SM5.2R CALCULATIONS WILL BE PERFORMED')  GDH0696
 1474 FORMAT(1X,'*  SM5.42R  - SM5.42R CALCULATIONS WILL BE PERFORMED') GDH1197
 1475 FORMAT(1X,'*  SM5.42   - SM5.42 CALCULATIONS WILL BE PERFORMED')  PDW1199
 1476 FORMAT(1X,'*  SM5.2    - SM5.2 CALCULATIONS WILL BE PERFORMED')   PDW1199
 1480 FORMAT(1X,'*  SM4      - SM4 CALCULATIONS WILL BE PERFORMED')     DJG1094
 1490 FORMAT(1X,'*  SM2.1    - SM2.1 CALCULATIONS WILL BE PERFORMED')   GDH1293
 1491 FORMAT(1X,'*  SM2.2    - SM2.2 CALCULATIONS WILL BE PERFORMED')   GDH1293
 1492 FORMAT(1X,'*  SM2.2PDA- SM2.2PD/A CALCULATIONS WILL BE PERFORMED')GDH0897
 1493 FORMAT(1X,'*  SM3.3    - SM3.3 CALCULATIONS WILL BE PERFORMED')   GDH1293
 1500 FORMAT(1X,'*  SM3.1    - SM3.1 CALCULATIONS WILL BE PERFORMED')   GDH1293
 1510 FORMAT(1X,'*  SM1      - SM1-like CALCULATIONS WILL BE ',         GDH0794
     .          'PERFORMED')                                            GDH0794
 1520 FORMAT(1X,'*  SM1A     - SM1a-like CALCULATIONS WILL BE ',        GDH0794
     .          'PERFORMED')                                            GDH0794
 1530 FORMAT(1X,'*  SM2      - SM2-like CALCULATIONS WILL BE ',         GDH0794
     .          'PERFORMED')                                            GDH0794
 1540 FORMAT(1X,'*  SM3      - SM3-like CALCULATIONS WILL BE ',         GDH0794
     .          'PERFORMED')                                            GDH0794
 1550 FORMAT(1X,'*  SM5.4A   - SM5.4/A-like CALCULATIONS WILL BE ',     GDH0696
     .          'PERFORMED')                                            GDH0696
 1551 FORMAT(1X,'*  SM5.4P   - SM5.4/P-like CALCULATIONS WILL BE ',     GDH0696
     .          'PERFORMED')                                            GDH0696
 1552 FORMAT(1X,'*  SM5.4U   - SM5.4/U-like CALCULATIONS WILL BE ',     GDH0696
     .          'PERFORMED')                                            GDH0696
 1553 FORMAT(1X,'*  SM5.0R   - SM5.0R CALCULATION WILL BE PERFORMED')   GDH0497
 1554 FORMAT(1X,'*  SM5.05R  - SM5.05R CALCULATION WILL BE PERFORMED')  GDH0497
 1560 FORMAT(1X,'*  SM4      - SM4-like CALCULATIONS WILL BE PERFORMED')DJG1094
 1570 FORMAT(1X,'*  SM2.1    - SM2.1-like CALCULATIONS WILL BE ',       GDH0794
     .          'PERFORMED')                                            GDH0794
 1580 FORMAT(1X,'*  SM3.1    - SM3.1-like CALCULATIONS WILL BE ',       GDH0794
     .          'PERFORMED')                                            GDH0794
 1700 FORMAT(1X,'*  EXTSM    - SOLVATION MODEL PARAMETERS WILL BE ',    GDH1293
     1       'INPUT')                                                   GDH1093
 1710 FORMAT(1X,'*  EXTCM    - EXTERNAL ATOMIC CHARGES WILL BE INPUT')  GDH0997
 1720 FORMAT(1X,'*  CM1      - CHARGE MODEL 1 WILL BE USED (CM1A)')     DJG0794
 1721 FORMAT(1X,'*  CM2      - CHARGE MODEL 2 WILL BE USED (CM2A)')     GDH0997
 1722 FORMAT(1X,'*  CM3      - CHARGE MODEL 3 WILL BE USED (CM3A)')     !JT0802
 1730 FORMAT(1X,'*  CM1      - CHARGE MODEL 1 WILL BE USED (CM1P)')     DJG0794
 1731 FORMAT(1X,'*  CM2      - CHARGE MODEL 2 WILL BE USED (CM2P)')     GDH0997
 1732 FORMAT(1X,'*  CM3      - CHARGE MODEL 3 WILL BE USED (CM3P)')     !JT0802
 1740 FORMAT(1X,'*           - THE SOLVENT IS ',A20)                    DJG1094
 1750 FORMAT(1X,'*           - THE SOLVENT IS WATER')                   DJG1094
 1755 FORMAT(1X,'*           - THE ',A20,' SOLVENT MODEL WILL BE USED') DJG1296
 1760 FORMAT(1X,'*           - CHARGE MODEL 1 WILL BE USED (CM1A)')     DJG0794
 1761 FORMAT(1X,'*           - CHARGE MODEL 2 WILL BE USED (CM2A)')     GDH0197
 1770 FORMAT(1X,'*           - CHARGE MODEL 1 WILL BE USED (CM1P)')     DJG0794
 1771 FORMAT(1X,'*           - CHARGE MODEL 2 WILL BE USED (CM2P)')     GDH0197
 1780 FORMAT(1X,'*           - MULLIKEN ANALYSIS WILL BE USED FOR ',    DJG1094
     1                         'ATOMIC PARTIAL CHARGES')                DJG1094
 1790 FORMAT(1X,'*  DIELEC   - THE SOLVENT DIELECTRIC CONSTANT IS ',    DJG1094
     1 F6.2)                                                            DJG1094
 1800 FORMAT(1X,'*  SVCDRD   - THE SOLVENT CD RADIUS IS ',F6.2,         DJG1094
     1          ' ANGSTROMS')                                           DJG1094
 1810 FORMAT(1X,'*  SVCSRD   - THE SOLVENT CS RADIUS IS ',F6.2,         DJG0195
     1          ' ANGSTROMS')                                           DJG1094
 1820 FORMAT(1X,'*  MSURFT   - THE CS SURFACE TENSION IS ',F6.2         DJG0195
     1          ,' CALORIES MOL^-1 ANGSTROM^-2')                        DJG0195
 1830 FORMAT(1X,'*  RAD=0    - THE FORCE RECTANGLE QUADRATURE ',        GDH0594
     1          'WILL BE USED')                                         GDH1293
 1840 FORMAT(1X,'*  RAD=1    - THE FORCE TRAPEZOID QUADRATURE ',        GDH0594
     1          'WILL BE USED')                                         GDH1293
 1850 FORMAT(1X,'*  EXTM     - FIXED M VALUES WILL BE INPUT')           GDH0494
 1860 FORMAT(1X,'*  STDM     - M VALUES IN DATA FILE WILL BE IGNORED')  GDH0494
 1870 FORMAT(1X,'*  XKW      - EXTRA KEYWORDS WILL BE READ FROM AN',    GDH0194
     1       ' EXTERNAL FILE')                                          GDH0194
 1880 FORMAT(1X,'*  NOCOGS   - THE C-O-G PARAMETERS ARE TURNED OFF')    DJG1094
 1890 FORMAT(1X,'*  CS1      - THE ORIGINAL CONVERGENCE STRATEGY',      GDH1093
     1       ' WILL BE USED (DEFAULT)')                                 GDH1093
 1900 FORMAT(1X,'*  CS2      - CONVERGENCE STRATEGY 2 WILL BE USED')    GDH1093
 1910 FORMAT(1X,'*  CS3      - CONVERGENCE STRATEGY 3 WILL BE USED')    GDH1093
 1920 FORMAT(1X,'*  KICK     - THE GEOMETRY WILL BE PERTURBED 1 TIME',/,DJG0395
     1 1X,'*           - IF GEOMETRY OPTIMIZATION ERRORS OCCUR')        DJG0395
 1930 FORMAT(1X,'*  KICK     - THE GEOMETRY WILL NOT BE PERTURBED',/,   DJG0395
     1 1X,'*           - IF GEOMETRY OPTIMIZATION ERRORS OCCUR')        DJG0395
 1940 FORMAT(1X,'*  KICK     - THE GEOMETRY WILL BE PERTURBED ',I2,/,   DJG0395
     1 1X,'*           - TIMES IF GEOMETRY OPTIMIZATION ERRORS OCCUR')  DJG0395
 1950 FORMAT(1X,'*  DOTS     - THE DOTS SURFACE AREA ALGORITHM WILL ',  GDH1093
     1       'BE USED')
 1960 FORMAT(1X,'*  NDOTCD   - UPPER BOUND ON NO. OF DOTS IN SURF. ',   GDH1093
     1       'TENSION SPHERES IS ',I6)                                  GDH1093
 1970 FORMAT(1X,'*  NDOTEP   - UPPER BOUND ON NO. OF DOTS FOR COULOMBIC'GDH1093
     1       ,' SPHERES IS    ',I6)                                     GDH1093
 1980 FORMAT(1X,'*  GEPOL    - THE GEPOL SURFACE AREA ALGORITHM WILL ', GDH1093
     1       'BE USED')
 1990 FORMAT(1X,'*  NDIVCD   - 60*(4^(',I1,')) TRIANGLES '              GDH1093
     1      ,'ON EACH SPHERE IN SURF. TEN. AREA CALCS.')                GDH1093
 2000 FORMAT(1X,'*  NDIVEP   - 60*(4^(',I1,')) TRIANGLES ON EACH SPHERE'GDH1093
     1      ,' IN COULOMBIC AREA CALCS.')                               GDH1093
 2010 FORMAT(1X,'*  ASA      - THE ANALYTICAL SURFACE AREA ALGORITHM ', GDH1093
     1       'WILL BE USED (DEFAULT)')
 2020 FORMAT(1X,'*  NOVOL    - THE VOLUME WILL NOT BE CALCULATED (DEF', GDH1093
     1       'AULT)')                                                   GDH1093
 2030 FORMAT(1X,'*  VOLUME   - THE VOLUME WILL BE CALCULATED WITH ',    GDH1093
     1       'THE GEPOL ALGORITHM')
 2040 FORMAT(1X,'*           - FOR THE VOLUME THE NO. OF TRIANGLE ',/,  GDH1093
     1    1X,'*             DIVISIONS IN GEPOL AREA CALC. IS ',I6)      DJG0495
 2050 FORMAT(1X,'*  NOINP    - A NEW INPUT FILE WILL NOT BE CREATE',    GDH1093
     1       'D (DEFAULT)')
 2060 FORMAT(1X,'*  INPUT    - A NEW INPUT FILE WILL BE CREATED WI',    GDH1093
     1       'TH THE OPTIMIZED GEOMETRY')
 2070 FORMAT(1X,'*  DENMAT   - FINAL DENSITY MATRIX WILL BE WRITTEN TO' DJG0295
     1          ,' ASCII FILE')                                         DJG0295
 2080 FORMAT(1X,'*  OLDMAT   - DENSITY MATRIX WILL BE READ FROM'        DJG0295
     1          ,' ASCII FILE')                                         DJG0295
 2090 FORMAT(1X,'*  TONE  (HALF-THICKNESS OF NUMERICAL INTEGRATION ',   GDH0194
     1          'SHELL 1) = ',F6.3)                                     GDH0194
 2100 FORMAT(1X,'*  TEXPN (GROWTH FACTOR OF NUMERICAL INTEGRATION ',    GL0393
     1          'SHELLS 1) = ',F6.3)                                    GL0393
 2110 FORMAT(1X,'*  TRUES    - TRUE SOLVATION FREE ENERGY CALCULATED')  GL0492
 2120 FORMAT(1X,'*  AREAS    - SOLVENT ACCESSIBLE SURFACE AREAS',       DJG0195
     1                       ' WILL BE PRINTED')                        DJG1094
 2131 FORMAT(' *  SM5.4PDU - SM5.4/PDU-like CALCULATIONS WILL BE ',     GDH0696
     .       'PERFORMED')                                               GDH0696
 2130 FORMAT(' *  SM5.4PDU - SM5.4/PDU CALCULATIONS WILL BE PERFORMED') DJG0496
 2141 FORMAT(' *  SM5.4PDA - SM5.4/PDA-like CALCULATIONS WILL BE ',     GDH0696
     .       'PERFORMED')                                               GDH0696
 2140 FORMAT(' *  SM5.4PDA - SM5.4/PDA CALCULATIONS WILL BE PERFORMED') GDH0496
 2150 FORMAT(' *  SM5.4PDP - SM5.4/PDP CALCULATIONS WILL BE PERFORMED') GDH0496
 2151 FORMAT(' *  SM5.4PDP - SM5.4/PDP-like CALCULATIONS WILL BE ',     GDH0696
     .       'PERFORMED')                                               GDH0696
 2160 FORMAT(' *  CART     - INPUT READ IN CARTESIAN COORDINATES')      GDH0496
 2170 FORMAT(' *  TRUSTE   - MINIMIZE ENERGY USING TRUST METHOD ')      DJG0496
 2180 FORMAT(' *  TRUSTG   - MINIMIZE GRADIENT USING TRUST METHOD ')    DJG0496
 2191 FORMAT(' *  SM5.2PDA - SM5.2/PDA-like CALCULATIONS WILL BE ',     GDH0696
     .       'PERFORMED')                                               GDH0696
 2190 FORMAT(' *  SM5.2PDA - SM5.2/PDA CALCULATIONS WILL BE PERFORMED') GDH0596
 2201 FORMAT(' *  SM5.2PDP - SM5.2/PDP-like CALCULATIONS WILL BE ',     GDH0696
     .       'PERFORMED')                                               GDH0696
 2200 FORMAT(' *  SM5.2PDP - SM5.2/PDP CALCULATIONS WILL BE PERFORMED') GDH0596
 2210 FORMAT(' *  HFCALC   - GAS PHASE OPTIMIZATION TO BE PERFORMED',   DJG0796
     .       ' FOR TRUES CALCULATION')                                  DJG0796
 2220 FORMAT(' *  HFCALC   - GAS PHASE SINGLE POINT TO BE PERFORMED',   DJG0796
     .       ' FOR TRUES CALCULATION')                                  DJG0796
 2230 FORMAT(' *  HFOPT    - OPTIMIZED GAS PHASE ENERGY PROVIDED',      DJG0796
     .       ' FOR TRUES CALCULATION')                                  DJG0796
 2240 FORMAT(' *  HF1SCF   - SINGLE POINT GAS PHASE ENERGY PROVIDED'    DJG0796
     .       ,' FOR TRUES CALCULATION')                                 DJG0796
 2250 FORMAT(' *  IOFR     - THE SOLVENT INDEX OF REFRACTION IS ',F6.4) DJG0796
 2260 FORMAT(' *  ALPHA    - THE SOLVENT ALPHA IS ',F4.2)               DJG0796
 2270 FORMAT(' *  BETA     - THE SOLVENT BETA IS ',F4.2)                DJG0796
 2280 FORMAT(' *  GAMMA    - THE SOLVENT MACROSCOPIC SURFACE TENSION ', DJG0796
     .       'IS ',/,' *',16X,F6.2,' CAL MOL^-1 ANGSTROM^-2')           PDW1199
 2290 FORMAT(' *  FACARB   - THE FRACTION OF AROMATIC CARBONS ',        TZ0898
     .       'IS ',F6.2)                                                TZ0898
 2300 FORMAT(' *  FEHALO   - THE FRACTION OF ELECTRONEGATIVE HALOGENS ',TZ0898
     .       'IS ',F6.2)                                                TZ0898
 2310 FORMAT(' *  PRINTGEOM - PRINT OUT ALL OF THE GEOMETRIES FOR',     PDW1099
     .       ' CALCULATIONS')                                           PDW1099
 2320 FORMAT(' *  PRINTCOUL - PRINT OUT THE COULOMB INTEGRALS')         PDW1199
 2330 FORMAT(' *  PRINTRAD  - PRINT OUT THE EFFECTIVE BORN RADII AND',  PDW1199
     .       ' EFFECTIVE DISTANCES')                                    PDW1199
 2340 FORMAT(' *  PRINTPOL  - PRINT OUT THE BORN POLARIZATION ENERGY',  PDW1199
     .        ' DECOMPOSITION')                                         PDW1199
      END
