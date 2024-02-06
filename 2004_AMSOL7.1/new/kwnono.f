      SUBROUTINE KWNONO
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'KEYS.i'
      INCLUDE 'FFILES.i'                                                GDH1095
      COMMON /OPTIMI / IMP,IMP0
      LOGICAL SOLSPC,NORGAN                                             DJG1296
      INTEGER AQCHK                                                     DJG0496
C******************************************************************************
C
C     KWNONO CHECKS FOR INCOMPATIBLE KEYWORD CHOICES
C
C     THIS SUBROUTINE WAS CREATED FROM PRE-EXISTING LINES IN READS
C     BY DJG.  THESE LINES WERE UPDATED TO INCLUDE THE NEW WAY
C     OF PARSING THE KEYWORDS.
C
C******************************************************************************
      AQCHK=ISM1+ISM2+ISM22+ISM23+ISM3+ISM4+ISM5U+ISM21+ISM31+ISM1A+    DJG1296
     1      IPDS5U+IPDS5A+IPDS5P+ISM5A+ISM5P+IS5A2P+IS5P2P+ISM50+       DJG1296
     2      ISM52R+ISM54R+ISM542+ISM52                                  PDW1099
      MFCHK=IOPT+I1SCF+ITRUES+IEXTCM                                    GDH0497
C     NOANAL=ISM1+ISM2+ISM22+ISM23+ISM3+ISM4+ISM21+ISM31+ISM1A+         PDW1199
C    1      ISM52+ISM542                                                PDW1199

C --- Analytic gradients for SM5.2 and SM5.42

      NOANAL=ISM1+ISM2+ISM22+ISM23+ISM3+ISM4+ISM21+ISM31+ISM1A          !JT0303
      IF (AQCHK.GT.1) THEN
          WRITE(JOUT,110) 
          ISTOP=1                                                       GDH1095
          IWHERE=84                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (INOPOL.GT.0.AND.ITRUES.EQ.1) THEN                             GDH0696
          WRITE(JOUT,120) 
          ISTOP=1                                                       GDH1095
          IWHERE=85                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (AQCHK.EQ.0.AND.ITRUES.NE.0) THEN
          WRITE(JOUT,130) 
          ISTOP=1                                                       GDH1095
          IWHERE=86                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (IVOLUM.GT.0) THEN                                             GDH0397
         WRITE(JOUT,710)                                                GDH0397
         ISTOP=1                                                        GDH0397
         IWHERE=173                                                     GDH0397
         RETURN                                                         GDH0397
      ENDIF                                                             GDH0397
      IF (IGEPOL.GT.0) THEN                                             GDH0397
         WRITE(JOUT,700)                                                GDH0397
         ISTOP=1                                                        GDH0397
         IWHERE=172                                                     GDH0397
         RETURN                                                         GDH0397
      ENDIF                                                             GDH0397
      NAREA=IDOTS+IGEPOL
      IF (NAREA.GT.1) THEN
          WRITE(JOUT,140) 
          ISTOP=1                                                       GDH1095
          IWHERE=87                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      NTYPE=INDIVE+INDOTE+INDOTC
      IF (NAREA.EQ.0.AND.NTYPE.GT.0) THEN
          WRITE(JOUT,150) 
          ISTOP=1                                                       GDH1095
          IWHERE=88                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF ((IGEPOL.EQ.0.AND.IVOLUM.EQ.0).AND.INDIVC.NE.0) THEN
          WRITE(JOUT,160) 
          ISTOP=1                                                       GDH1095
          IWHERE=89                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (IDOTS.NE.0.AND.(INDIVC.NE.0.OR.INDIVE.NE.0)) THEN
          WRITE(JOUT,170)
          ISTOP=1                                                       GDH1095
          IWHERE=90                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (IGEPOL.NE.0.AND.(INDOTC.NE.0.OR.INDOTE.NE.0)) THEN
          WRITE(JOUT,180)
          ISTOP=1                                                       GDH1095
          IWHERE=91                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (NAREA.NE.0.AND.IASA.NE.0) THEN 
          WRITE(JOUT,190)
          ISTOP=1                                                       GDH1095
          IWHERE=92                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      NSUM=ICS1+ICS2+ICS3
      IF (NSUM.GT.1) THEN 
          WRITE(JOUT,200)
          ISTOP=1                                                       GDH1095
          IWHERE=93                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      NSUM=IVOLUM+INOVOL
      IF (NSUM.GT.1) THEN 
          WRITE(JOUT,210)
          ISTOP=1                                                       GDH1095
          IWHERE=94                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      NSUM=IINPUT+INOINP
      IF (NSUM.GT.1) THEN 
          WRITE(JOUT,220)
          ISTOP=1                                                       GDH1095
          IWHERE=95                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (NOANAL.NE.0.AND.IDERIS.NE.0) THEN
          WRITE(JOUT,230)
          ISTOP=1                                                       GDH1095
          IWHERE=96                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (IDERIN.NE.0.AND.IDERIS.NE.0) THEN
          WRITE(JOUT,240)
          ISTOP=1                                                       GDH1095
          IWHERE=97                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      NSUM=IOPT+I1SCF                                                   GDH1197
      IF (NSUM.GT.1) THEN
          WRITE(JOUT,250)
          ISTOP=1                                                       GDH1095
          IWHERE=98                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (IDEV.EQ.0.AND.(ITONE.NE.0.OR.ITEXPN.NE.0)) THEN
          WRITE(JOUT,260)
          ISTOP=1                                                       GDH1095
          IWHERE=99                                                     GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (IDEV.EQ.0.AND.IEXTSM.NE.0) THEN
          WRITE(JOUT,270)
          ISTOP=1                                                       GDH1095
          IWHERE=100                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (ISM1A.NE.0.AND.IEXTSM.NE.0) THEN
          WRITE(JOUT,280)
          ISTOP=1                                                       GDH1095
          IWHERE=100                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (IDEV.EQ.0.AND.IEXTCM.NE.0) THEN
          WRITE(JOUT,290)
          ISTOP=1                                                       GDH1095
          IWHERE=101                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (IDEV.EQ.0.AND.IXKW.NE.0) THEN
          WRITE(JOUT,300)
          ISTOP=1                                                       GDH1095
          IWHERE=102                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IGEN=IPDS5U+ISM5U                                                 DJG1296
      IF (IDEV.EQ.0.AND.IGEN.NE.0) THEN                                 DJG0496
          WRITE(JOUT,650)                                               GDH0496
          ISTOP=1                                                       GDH0496
          IWHERE=171                                                    GDH0496
          RETURN                                                        GDH0496
      ENDIF
      IF (IEXTM.NE.0.AND.ISTDM.NE.0) THEN
          WRITE(JOUT,310)
          ISTOP=1                                                       GDH1095
          IWHERE=103                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (AQCHK.NE.0.AND.IAM1.EQ.0.AND.IPM3.EQ.0.AND.IDEV.EQ.0.AND.     GDH0497
     .    ISM50.EQ.0) THEN                                              GDH0497
          IF (ISM52R.EQ.0.OR.IMNDO.EQ.0) THEN                           GDH0997
          WRITE(JOUT,320)
          ISTOP=1                                                       GDH1095
          IWHERE=104                                                    GDH1095
          RETURN                                                        GDH1095
          ENDIF                                                         GDH0997
      ENDIF
      IF (AQCHK.NE.0.AND.ICM1.NE.0.AND.INOPOL.EQ.0) THEN                GDH1197
          WRITE(JOUT,330)
          ISTOP=1                                                       GDH1095
          IWHERE=105                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (AQCHK.NE.0.AND.ICM2.NE.0) THEN                                GDH0997
          WRITE(JOUT,331)                                               GDH0997
          ISTOP=1                                                       GDH0997
          IWHERE=105                                                    GDH0997
          RETURN                                                        GDH0997
      ENDIF
      IF (AQCHK.NE.0.AND.ICM3.NE.0) THEN                                !JT0802
         WRITE(JOUT,332)                                                !JT0802
         ISTOP=1                                                        !JT0802
         IWHERE=105                                                     !JT0802
         RETURN                                                         !JT0802
      END IF                                                            !JT0802
      IF (IDEV.EQ.0.AND.(ISVCDR.NE.0.OR.ISVCSR.NE.0)) THEN
          WRITE(JOUT,340)
          ISTOP=1                                                       GDH1095
          IWHERE=106                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      NORGAN=CISOLV.EQ.'GENORG'                                         DJG0297
      SOLSPC=CISOLV.EQ.'GENALK'.OR.CISOLV.EQ.'H2OSRP'.OR.NORGAN         DJG1296
      IF (IDEV.EQ.0.AND.IDIELE.NE.0.AND.(.NOT.SOLSPC)) THEN
          WRITE(JOUT,350)
          ISTOP=1                                                       GDH1095
          IWHERE=107                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (ICM1.NE.0.AND.IAM1.EQ.0.AND.IPM3.EQ.0) THEN
          WRITE(JOUT,360)
          ISTOP=1                                                       GDH1095
          IWHERE=108                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (ICM2.NE.0.AND.IAM1.EQ.0.AND.IPM3.EQ.0) THEN
          WRITE(JOUT,361)
          ISTOP=1                                                       GDH0997
          IWHERE=108                                                    GDH0997
          RETURN                                                        GDH0997
      ENDIF
      IF (ICM3.NE.0.AND.IAM1.EQ.0.AND.IPM3.EQ.0) THEN                   !JT0802
          WRITE(JOUT,362)                                               !JT0802
          ISTOP=1                                                       !JT0802
          IWHERE=108                                                    !JT0802
          RETURN                                                        !JT0802
      ENDIF
      IF (IDEV.EQ.0.AND.INOCOG.NE.0) THEN
          WRITE(JOUT,370)
          ISTOP=1                                                       GDH1095
          IWHERE=109                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (ISOLVN.NE.0.AND.(ISM4+ISM5U+ISM5A+ISM5P+IPDS5U+IPDS5A         DJG0796
     X    +IPDS5P+IS5A2P+IS5P2P+ISM50+ISM54R+ISM52R+ISM542
     X    +ISM52).EQ.0)  THEN                                           PDW1099
          WRITE(JOUT,380)
          ISTOP=1                                                       GDH1095
          IWHERE=110                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (ISOLVN.NE.0.AND.(ISM52R+ISM54R+ISM50+ISM4+ISM5A+
     X    ISM5P+ISM52+ISM542.EQ.0)                                      PDW1099
     .   .AND.(CISOLV.NE.'WATER ')) THEN                                GDH0997
          WRITE(JOUT,380)                                               DJG1296
          ISTOP=1                                                       DJG1296
          IWHERE=178                                                    DJG1296
          RETURN                                                        DJG1296
      ENDIF                                                             DJG1296
      IF (IDEV.EQ.0.AND.IMSURF.NE.0.AND.(.NOT.SOLSPC)) THEN
          WRITE(JOUT,390)
          ISTOP=1                                                       GDH1095
          IWHERE=111                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (((ISM4+ISM5U+ISM5A+ISM5P+IPDS5U+IPDS5A+IPDS5P+IS5P2P+IS5A2P   DJG0796
     X    +ISM52R+ISM54R).NE.0).                                        GDH0997
     X    AND.ISOLVN.EQ.0) THEN                                         GDH0496
          WRITE(JOUT,400)
          ISTOP=1                                                       GDH1095
          IWHERE=112                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      NSUM=ISM1+ISM1A+ISM2+ISM21+ISM22+ISM23+ISM5A+IS5A2P               DJG1296
      IF (NSUM.NE.0.AND.IAM1.EQ.0) THEN
          WRITE(JOUT,410)
          ISTOP=1                                                       GDH1095
          IWHERE=113                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      NSUM=ISM3+ISM31+ISM5P+IS5P2P                                      DJG1296
      IF (NSUM.NE.0.AND.IPM3.EQ.0) THEN
          WRITE(JOUT,420)
          ISTOP=1                                                       GDH1095
          IWHERE=114                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      NNOEF=INEWTO+IPOWEL+IBFGS+IDFP+ISIGMA+INLLSQ+ILTRD+ISADDL+ITRUSE+ DJG0496
     1      ITRUSG                                                      DJG0496
      IF (IPRECI.NE.0) THEN
         IF (NNOEF.EQ.0.) THEN
            WRITE(JOUT,430) 'EF'
          ISTOP=1                                                       GDH1095
          IWHERE=115                                                    GDH1095
          RETURN                                                        GDH1095
         ELSE IF (IBFGS.NE.0) THEN
            WRITE(JOUT,430) 'BFGS'
          ISTOP=1                                                       GDH1095
          IWHERE=116                                                    GDH1095
          RETURN                                                        GDH1095
         ELSE IF (IDFP.NE.0) THEN
            WRITE(JOUT,430) 'DFP'
          ISTOP=1                                                       GDH1095
          IWHERE=117                                                    GDH1095
          RETURN                                                        GDH1095
         ELSE IF (ITRUSE.NE.0) THEN                                     DJG0496
            WRITE(JOUT,430) 'TRUSTE'                                    DJG0496
            ISTOP=1                                                     DJG0496
            IWHERE=117                                                  DJG0496
            RETURN                                                      DJG0496
         ELSE IF (ITRUSG.NE.0) THEN                                     DJG0496
            WRITE(JOUT,430) 'TRUSTG'                                    DJG0496
            ISTOP=1                                                     DJG0496
            IWHERE=117                                                  DJG0496
            RETURN                                                      DJG0496
         ENDIF        
      ENDIF
      IF (IGNORM.NE.0) THEN
         IF (NNOEF.EQ.0.) THEN
            WRITE(JOUT,440) 'EF'
          ISTOP=1                                                       GDH1095
          IWHERE=118                                                    GDH1095
          RETURN                                                        GDH1095
         ELSE IF (IBFGS.NE.0) THEN
            WRITE(JOUT,440) 'BFGS'
          ISTOP=1                                                       GDH1095
          IWHERE=119                                                    GDH1095
          RETURN                                                        GDH1095
         ELSE IF (IDFP.NE.0) THEN
            WRITE(JOUT,440) 'DFP'
          ISTOP=1                                                       GDH1095
          IWHERE=120                                                    GDH1095
          RETURN                                                        GDH1095
         ELSE IF (ITRUSE.NE.0) THEN                                     DJG0496
            WRITE(JOUT,440) 'TRUSTE'                                    DJG0496
            ISTOP=1                                                     DJG0496
            IWHERE=120                                                  DJG0496
            RETURN                                                      DJG0496
         ELSE IF (ITRUSG.NE.0) THEN                                     DJG0496
            WRITE(JOUT,440) 'TRUSTG'                                    DJG0496
            ISTOP=1                                                     DJG0496
            IWHERE=120                                                  DJG0496
            RETURN                                                      DJG0496
         ENDIF
      ENDIF
      NSUM=IBIRAD+IEXCIT+ICI
      IF (IUHF.NE.0.AND.NSUM.NE.0) THEN
         WRITE(JOUT,450)
          ISTOP=1                                                       GDH1095
          IWHERE=121                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (IUHF.EQ.0.AND.IEXCIT.NE.0.AND.ITRIPL.NE.0) THEN
         WRITE(JOUT,460)
          ISTOP=1                                                       GDH1095
          IWHERE=122                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      NSUM=IAM1+IPM3+IMINDO+IMNDOC+IMNDO
      IF (NSUM.GT.1) THEN
         WRITE(JOUT,470)
          ISTOP=1                                                       GDH1095
          IWHERE=123                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF(AQCHK.NE.0.AND.IRAD1.NE.0) THEN
         NSUM=ISM1+ISM1A+ISM2+ISM3
         MSUM=ISM21+ISM31+ISM4+ISM5U+ISM5A+ISM5P                        DJG1296
         LSUM=ISM22+ISM23+IPDS5U+IPDS5A+IPDS5P+IS5P2P+IS5A2P            DJG0796
         IF (NSUM.NE.0.AND.IIRAD.NE.0.AND.IDEV.EQ.0) THEN
            WRITE(JOUT,490)
          ISTOP=1                                                       GDH1095
          IWHERE=125                                                    GDH1095
          RETURN                                                        GDH1095
         ENDIF
         IF (MSUM.NE.0.AND.IIRAD.NE.1.AND.IDEV.EQ.0) THEN 
            WRITE(JOUT,490)
          ISTOP=1                                                       GDH1095
          IWHERE=126                                                    GDH1095
          RETURN                                                        GDH1095
         ENDIF
         IF (LSUM.NE.0.AND.IIRAD.NE.2.AND.IDEV.EQ.0) THEN 
            WRITE(JOUT,490)
          ISTOP=1                                                       GDH1095
          IWHERE=127                                                    GDH1095
          RETURN                                                        GDH1095
         ENDIF
         IF (IIRAD.LT.0.OR.IIRAD.GT.3) THEN                             GDH1197
            WRITE(JOUT,500)
          ISTOP=1                                                       GDH1095
          IWHERE=128                                                    GDH1095
          RETURN                                                        GDH1095
         ENDIF
      ENDIF
      IF (MFCHK.NE.0) THEN                                              GDH0497
         IF (ISM505.NE.0) THEN                                          GDH0497
            ISTOP=1                                                     GDH0497
            IWHERE=174                                                  GDH0497
            RETURN                                                      GDH0497
         ELSE IF (ISM50.NE.0) THEN                                      GDH0497
            ISTOP=1                                                     GDH0497
            IWHERE=175                                                  GDH0497
            RETURN                                                      GDH0497
         ENDIF                                                          GDH0497
      ENDIF                                                             GDH0497
      NUMOPT=ISTEP1+INEWTO+ILTRD+IPOWEL+IPATH+ICHAIN+IFORCE+ISIGMA+
     1       INLLSQ+IDRC+IBFGS+IDFP+IEFOLL+ITSTAT+ITRUSE+ITRUSG         DJG0496
      MOKOPT=IDFP+IBFGS+IEFOLL+IPATH+IFORCE+ITRUSE+ITRUSG               DJG0496
      NOTOK=IDFP+IBFGS+IEFOLL+ITRUSE+ITRUSG                             DJG0496
      NOTOK2=IPATH+IFORCE
C
C     CAN'T HAVE MORE THAN 1 OPTIMIZER CHOSEN, UNLESS A PATH OR FORCE
C     CALCULATION, WHICH CAN BE COMBINED WITH EF, BFGS, OR DFP.  BUT
C     EF, BFGS AND DFP CAN NOT BE COMBINED WITH EACH OTHER.  WHEW!
C
      IF (NUMOPT.GT.1) THEN
         IF (MOKOPT.NE.NUMOPT) THEN
            WRITE(JOUT,520)
            ISTOP=1                                                     GDH1095
            IWHERE=130                                                  GDH1095
            RETURN                                                      GDH1095
         ELSE IF (NOTOK.GT.1.OR.NOTOK2.GT.1) THEN
            WRITE(JOUT,520)
            ISTOP=1                                                     GDH1095
            IWHERE=131                                                  GDH1095
            RETURN                                                      GDH1095
         ENDIF
      ENDIF
      IF (ITRUES.GT.1.AND.INOINP.NE.0) THEN
         WRITE(JOUT,530)
         ISTOP=1                                                        GDH1095
         IWHERE=132                                                     GDH1095
         RETURN                                                         GDH1095
      ENDIF
      IF (IHF.GE.1.AND.IHFCAL.GT.1) THEN                                GDH1/96
         WRITE(JOUT,570)                                                GDH1/96
         ISTOP=1                                                        GDH1/96
         IWHERE=166                                                     GDH1/96
         RETURN                                                         GDH1/96
      ENDIF                                                             GDH1/96
      IF (IHF.EQ.-7) THEN                                               GDH1/96
         WRITE(JOUT,580)                                                GDH1/96
         ISTOP=1                                                        GDH1/96
         IWHERE=167                                                     GDH1/96
         RETURN                                                         GDH1/96
      ENDIF                                                             GDH1/96
      IF (IGAS.EQ.1.AND.IGPGCD.EQ.1) THEN                               GDH1/96
         WRITE(JOUT,590)                                                GDH1/96
         ISTOP=1                                                        GDH1/96
         IWHERE=168                                                     GDH1/96
         RETURN                                                         GDH1/96
      ENDIF                                                             GDH1/96
      IF (IHFCAL.GE.2.AND.ITRUES.EQ.0) THEN                             GDH1/96
         WRITE(JOUT,600)                                                GDH1/96
         ISTOP=1                                                        GDH1/96
         IWHERE=169                                                     GDH1/96
         RETURN                                                         GDH1/96
      ENDIF                                                             GDH1/96
      IF (IGAS.EQ.1.AND.AQCHK.NE.0) THEN                                GDH1/96
         WRITE(JOUT,610)                                                GDH1/96
         ISTOP=1                                                        GDH1/96
         IWHERE=170                                                     GDH1/96
         RETURN                                                         GDH1/96
      ENDIF                                                             GDH1/96
      IF (IGPGCD.EQ.1.AND.AQCHK.EQ.0) THEN                              GDH1/96
         WRITE(JOUT,620)                                                GDH1/96
         ISTOP=1                                                        GDH1/96
         IWHERE=171                                                     GDH1/96
         RETURN                                                         GDH1/96
      ENDIF                                                             GDH1/96
      IF (I0SCF.EQ.1.AND.ITRUES.GE.1) THEN                              GDH1/96
         WRITE(JOUT,630)                                                GDH1/96
         ISTOP=1                                                        GDH1/96
         IWHERE=172                                                     GDH1/96
         RETURN                                                         GDH1/96
      ENDIF                                                             GDH1/96
      IF (INOPOL.EQ.1.AND.ITRUES.GE.1) THEN                             GDH1/96
         WRITE(JOUT,640)                                                GDH1/96
         ISTOP=1                                                        GDH1/96
         IWHERE=173                                                     GDH1/96
         RETURN                                                         GDH1/96
      ENDIF                                                             GDH1/96
      LSUM=ISM22+ISM23
      IF ((LSUM.NE.0.OR.IIRAD.EQ.2).AND.(ITEXPN.NE.0.OR.ITONE.NE.0.OR.
     1     IEXTM.NE.0)) THEN
         WRITE(JOUT,540)
          ISTOP=1                                                       GDH1095
          IWHERE=133                                                    GDH1095
          RETURN                                                        GDH1095
      ENDIF
      IF (NORGAN.AND.(IDIELE.EQ.0.OR.IIOFR.EQ.0.OR.IALPHA.EQ.0.OR.      DJG1296
     1   IBETA.EQ.0.OR.IGAMMA.EQ.0)) THEN                               DJG0496
         WRITE(JOUT,660)                                                DJG0496
         ISTOP=1                                                        DJG0496
         IWHERE=174                                                     DJG0496
         RETURN                                                         DJG0496
      ELSE IF (NORGAN.AND.(ISM50R.NE.0.OR.ISM52R.NE.0.OR.ISM54R.NE.0)   GDH0997
     .         .AND.(IFACAR.EQ.0.OR.IFEHAL.EQ.0)) THEN                  GDH0997
         WRITE(JOUT,661)                                                DJG0496
         ISTOP=1                                                        DJG0496
         IWHERE=174                                                     DJG0496
         RETURN                                                         DJG0496
      ENDIF                                                             GDH0997
      IF (.NOT.NORGAN.AND.(IIOFR.NE.0.OR.IALPHA.NE.0.OR.IBETA.NE.0.OR.  DJG1296
     1   IGAMMA.NE.0.OR.IFACAR.NE.0.OR.IFEHAL.NE.0)) THEN               GDH0797
         WRITE(JOUT,670)                                                DJG0496
         ISTOP=1                                                        DJG0496
         IWHERE=175                                                     DJG0496
         RETURN                                                         DJG0496
      ENDIF                                                             DJG0496
C    GOTTA MAKE SURE THAT NO ONE TRIES TO USE WATER WITH OSM5!!!        DJG0496
      IF (NORGAN.AND.(FIIOFR.GE.1.33D0.AND.FIIOFR.LE.1.335D0).AND.      DJG1296
     1    (FIALPH.EQ.0.82D0.OR.FIALPH.EQ.0.80).AND.(FIBETA.EQ.0.35D0.   DJG0496
     2     OR.FIBETA.EQ.0.40D0.OR.FIBETA.EQ.0.30D0).AND.(FIGAMM.GE.     DJG0496
     3     103.0D0.AND.FIGAMM.LE.104.0D0).AND.((FIDIEL.GT.78.0D0.AND.   DJG0496
     4     FIDIEL.LT.79.0D0).OR.FIDIEL.EQ.80.0D0)) THEN                 DJG0496
         WRITE(JOUT,680)                                                DJG0496
         IWHERE=176                                                     DJG0496
         RETURN                                                         DJG0496
      ENDIF                                                             DJG0496
C                                                                       DJG0796
C     WE ARE DISABLING THE XYZ KEYWORD FOR THE 6.0 RELEASE.  IT         DJG0796
C     SEEMS TO GIVE SPURRIOUS RESULTS                                   DJG0796
C                                                                       DJG0796
      IF (IXYZ.NE.0) THEN                                               DJG0796
         WRITE(JOUT,690)                                                DJG0796
         ISTOP=1                                                        GDH1297
         IWHERE=177                                                     DJG0796
         RETURN                                                         DJG0796
      ENDIF                                                             DJG0796
C
C     INTRODUCING THE SM5.2 AND SM5.42 KEYWORDS FOR OPTIMIZATION IN     PDW1099
C     SOLUTION.  THE SM5.2R AND SM5.42R MODELS MUST BE USED IN          PDW1099
C     CONJUNCTION WITH THE 1SCF KEYWORD                                 PDW1099
      IF (I1SCF.NE.1.AND.ISM52R.EQ.1) THEN                              PDW1099
         WRITE(JOUT,740)                                                PDW1099
         ISTOP=1                                                        PDW1099
         IWHERE=178                                                     PDW1099
         RETURN                                                         PDW1099
      ENDIF                                                             PDW1099
      IF (I1SCF.NE.1.AND.ISM54R.EQ.1) THEN                              PDW1099
         WRITE(JOUT,750)                                                PDW1099
         ISTOP=1                                                        PDW1099
         IWHERE=179                                                     PDW1099
         RETURN                                                         PDW1099
      ENDIF                                                             PDW1099
      IF (I1SCF.NE.0.AND.ISM52.EQ.1) THEN                               PDW1099
         WRITE(JOUT,760)                                                PDW1099
         ISTOP=1                                                        PDW1099
         IWHERE=180                                                     PDW1099
         RETURN                                                         PDW1099
      ENDIF                                                             PDW1099
      IF (I1SCF.NE.0.AND.ISM542.EQ.1) THEN                              PDW1099
         WRITE(JOUT,770)                                                PDW1099
         ISTOP=1                                                        PDW1099
         IWHERE=181                                                     PDW1099
         RETURN                                                         PDW1099
      ENDIF                                                             PDW1099
      RETURN
110   FORMAT(/,'WARNING: ONLY ONE SOLVATION MODEL CAN BE SPECIFIED')     
120   FORMAT(/,'WARNING: "TRUES" CANNOT BE USED WITH THE KEYWORD NOPOL')
130   FORMAT(/,'WARNING: "TRUES" CANNOT BE USED WITHOUT A SOLVATION ',  
     1          'MODEL')                                                
140   FORMAT(/,'WARNING: "GEPOL" CANNOT BE USED WITH THE KEYWORD ',
     1                   '"DOTS"')
150   FORMAT(/,'WARNING: THE KEYWORDS "NDOTCD","NDOTEP",',      
     1         'AND "NDIVEP" CANNOT BE USED WITHOUT',/,'THE',   
     2         ' KEYWORD "DOTS" OR "GEPOL".')                 
160   FORMAT(/,'WARNING: THE KEYWORD "NDIVCD" CANNOT BE USED WITHOUT',
     1         ' THE KEYWORD "GEPOL" ',/,'OR "VOLUME".')
170   FORMAT(/,'WARNING: "NDIVCD" AND "NDIVEP" CANNOT BE USED WITH ',
     1         'THE KEYWORD DOTS')                                      GDH0992
180   FORMAT(/,'WARNING: "NDOTCD" AND "NDOTEP" CANNOT BE USED WITH ',
     1         'THE KEYWORD GEPOL ',/,'(OR WITHOUT DOTS)')
190   FORMAT(/,'WARNING: THE KEYWORD "ASA" CANNOT BE USED WITH',
     1         ' THE KEYWORD "GEPOL" OR "DOTS".')                       GDH0793
200   FORMAT(/,'WARNING: THE KEYWORDS CS1, CS2, AND CS3 ARE MUTUALLY ',
     1         'EXCLUSIVE')
210   FORMAT(/,'WARNING: THE KEYWORD "NOVOL" CANNOT BE USED WITH',
     1         ' "VOLUME".')                                            GDH0893
220   FORMAT(/,'WARNING: THE KEYWORD "NOINP" CANNOT BE USED WITH',
     1         ' "INPUT".')                                             GDH0893
230   FORMAT(/,'WARNING: THE KEYWORD "DERISA" MAY BE USED WITH',        GDH0597
     1         ' ONLY WITH SM5.x SOLVATION MODELS.')                    GDH0597
240   FORMAT(/,'WARNING: THE KEYWORD "DERISA" CANNOT BE USED WITH', 
     1         ' "DERINU.')                                             GDH0893
250   FORMAT(/,'WARNING: ONLY ONE OF THE FOLLOWING KEYWORDS CAN BE',
     1         ' AT A TIME: ',/,'"OPT","1SCF"        .')                GDH1197
260   FORMAT(/,'WARNING: THE KEYWORDS "TONE" OR "TEXPN" CAN ONLY',
     1         ' BE USED WITH THE KEYWORD "DEV".')                      GDH0893
270   FORMAT(/,'WARNING: THE KEYWORD "EXTSM" CANNOT BE USED WITHOUT',
     1         ' THE KEYWORD "DEV".')                                   GDH1293
280   FORMAT(/,'WARNING: THE KEYWORD "EXTSM" CANNOT BE USED WITH',
     1         ' THE KEYWORD "SM1A".')                                  GDH1293
290   FORMAT(/,'WARNING: THE KEYWORD "EXTCM" CANNOT BE USED WITHOUT',
     1         ' THE KEYWORD "DEV".')                                   GDH1293
300   FORMAT(/,'WARNING: THE KEYWORD "XKW" CANNOT BE USED WITHOUT',
     1         ' THE KEYWORD "DEV".')                                   GDH0194
310   FORMAT(/,'WARNING: THE KEYWORD "EXTM" CANNOT BE USED WITH',
     1         ' THE KEYWORD "STDM".')                                  GDH0494
320   FORMAT(/,'WARNING: WITHOUT THE KEYWORD "DEV" THE SOLVATION MOD',
     1         'ELS CAN ONLY BE USED WITH',/,'THE HAMILTONIANS FOR '    GDH0997
     2         ,' WHICH THEY WERE PARAMETERIZED.')                      GDH0997
330   FORMAT(/,'WARNING: THE KEYWORD CM1 IS NOT COMPATIBLE WITH ', 
     1         'SOLVATION CALCULATIONS',/,'TO USE CHARGE ',             DJG1094
     2         'MODEL 1 IN A SOLVATION CALCULATION, YOU MUST SPECIFY',/,DJG1094
     3         'A SOLVATION MODEL WHICH INCLUDES CM1 CHARGES        ',/,DJG0496
     4         'IMPLICITLY                                          ',/)DJG0496
331   FORMAT(/,'WARNING: THE KEYWORD CM2 IS NOT COMPATIBLE WITH ', 
     1         'SOLVATION CALCULATIONS',/,'TO USE CHARGE ',             GDH0997
     2         'MODEL 2 IN A SOLVATION CALCULATION, YOU MUST SPECIFY',/,GDH0997
     3         'A SOLVATION MODEL WHICH INCLUDES CM2 CHARGES        ',/,GDH0997
     4         'IMPLICITLY                                          ',/)GDH0997
332   FORMAT(/,'WARNING: THE KEYWORD CM3 IS NOT COMPATIBLE WITH ',
     1         'SOLVATION CALCULATIONS',/,'CM3 IS NOT AVAILABLE ',     !JT0802
     2         'FOR SOLVATION MODEL CALCULATIONS YET.',/)              !JT0802
340   FORMAT(/,'WARNING: THE KEYWORDS "SVCDRD" AND "SVCSRD" CANNOT',    
     1         ' BE USED',/,'WITHOUT THE KEYWORD "DEV".') 
350   FORMAT(/,'WARNING: THE KEYWORD "DIELEC" CANNOT BE USED WITHOUT',
     1         ' THE KEYWORD "DEV".')                                   DJG1094
360   FORMAT(/,'WARNING: THE KEYWORD "CM1" CANNOT BE USED WITHOUT', 
     1         ' SPECIFYING EITHER AM1 OR PM3.')                        DJG1094
361   FORMAT(/,'WARNING: THE KEYWORD "CM2" CANNOT BE USED WITHOUT', 
     1         ' SPECIFYING EITHER AM1 OR PM3.')                        GDH0997
362   FORMAT(/,'WARNING: THE KEYWORD "CM3" CANNOT BE USED WITHOUT',
     1         ' SPECIFYING EITHER AM1 OR PM3.')                        !JT0802
370   FORMAT(/,'WARNING: THE KEYWORD "NOCOGS" CANNOT BE USED WITHOUT', 
     1         ' THE KEYWORD "DEV".')                                   DJG1094
380   FORMAT(/,'WARNING: THE KEYWORD "SOLVNT" CAN ONLY BE USED', 
     1    ' IN CONJUNCTION WITH THE',/,'SM4, SM5.4/A, SM5.4/P,',        GDH1197
     2    ' SM5.0R, SM5.2R, OR SM5.42R SOLVATION MODELS.')              GDH0997
390   FORMAT(/,'WARNING: THE KEYWORD "MSURFT" CANNOT BE USED WITHOUT', 
     1         ' THE KEYWORD "DEV".')                                   DJG0496
400   FORMAT(/,'WARNING: THE SOLVENT MUST BE SPECIFIED BY USING THE ', 
     1    'KEYWORD "SOLVNT="',/,'WHEN USING SM4 or SM5-like MODELS. ')  DJG0496
410   FORMAT(/,'WARNING: YOU HAVE CHOSEN A SOLVATION MODEL WHICH MUST ',DJG0896
     1         'BE USED WITH AM1')                                      DJG0896
420   FORMAT(/,'WARNING: YOU HAVE CHOSEN A SOLVATION MODEL WHICH MUST ',DJG0896
     1         'BE USED WITH PM3')                                      DJG0896
430   FORMAT(/,5X,'WARNING : THE KEYWORD PRECISE IS NO LONGER SUPPORTED'
     1     ,' FOR ',A6,/,10X,'THE FUNCTIONS OF PRECISE HAVE BEEN ',     DJG0496
     2     'TAKEN OVER AS FOLLOWS:',/,15X,'SCF CONVERGENCE CRITERION: ' DJG0495
     3     ,'KEYWORD SCFCRT=X',/,15X,'GEOMETRY CONVERGENCE CRITERION:'  DJG0495
     4     ,' KEYWORD GCOMP=X',/,15X,'CENTRAL FINITE DIFFERENCES FOR '  DJG0495
     5     ,'DERIVATIVES: DEFAULT',/,15X,'USE KEYWORD "FORWRD" FOR '    DJG0495
     6     ,'FORWARD FINITE DIFFERENCES',/)                             DJG0495
440   FORMAT(/,5X,'WARNING : THE KEYWORD GNORM IS NO LONGER SUPPORTED'
     1       ,' FOR ',A6,/,10X,'THE FUNCTION OF GNORM HAS BEEN REPLACED'DJG0496
     2       ,' BY GCOMP.',/,10X,'PLEASE SEE THE OPTIMIZATION SECTION ' DJG0495
     3       ,'OF THE MANUAL')                                          DJG0495
450   FORMAT(/,'WARNING : UHF CALCULATIONS CANNOT BE PERFORMED IN ',
     1         'CONJUNCTION WITH',/,'THE BIRADICAL, EXCITED, OR C.I.',
     2         ' KEYWORDS.')         
460   FORMAT(/,'WARNING : EXCITED TRIPLET CALCULATIONS CANNOT BE ',
     1         'PERFORMED WITHOUT "UHF".')
470   FORMAT(/,'WARNING : ONLY ONE HAMILTONIAN MAY BE USED') 
490   FORMAT(/,'WARNING: THE DEFAULT VALUE FOR "RAD" CANNOT BE', 
     1         ' CHANGED UNLESS THE KEYWORD "DEV" IS USED.')            GDH0893
500   FORMAT(/,'WARNING: THE VALUE FOR "RAD" MUST BE',                  GDH1197
     1         ' "RAD=0, 1, 2, or 3 ')                                  GDH1197
520   FORMAT(/,'WARNING: TOO MANY OPTIMIZERS CHOSEN!')                  DJG0995
530   FORMAT(/,'WARNING: "NOINP" CANNOT BE USED WITH "HFOPT=CALC" OR'   GDH0196
     1        ,' "HF1SCF=CALC"')                                        GDH0196
540   FORMAT(/,'WARNING: THE KEYWORDS "TEXPN", "TONE", AND "EXTM" '     GDH0995
     1        ,'ARE NOT COMPATIBLE WITH THE',/,'PAIRWISE DESCREENING '  GDH0995
     2        ,'APPROXIMATION (RAD=2 OR SM*.3 KEYWORDS).')              GDH0995
570   FORMAT(/,'WARNING: NEITHER "HFOPT" NOR "HF1SCF" CAN BE USED ',    GDH1/96
     .       'WITH "HFCALC"')                                           GDH1/96
580   FORMAT(/,'WARNING: "HFOPT" AND "HF1SCF" CANNOT BE USED ',         GDH1/96
     .       'TOGTHER')                                                 GDH1/96
590   FORMAT(/,'WARNING: "GAS" AND "GPGCDS" ARE ',                      GDH1/96
     .       'INCOMPATIBLE KEYWORDS')                                   GDH1/96
600   FORMAT(/,'WARNING: "HFCALC" CANNOT BE USED ',                     DJG0496
     .       'WITHOUT "TRUES"')                                         GDH1/96
610   FORMAT(/,'WARNING: THE KEYWORD "GAS" CANNOT BE USED WITH A ',     GDH1/96
     .       'SMx KEYWORD')                                             GDH1/96
620   FORMAT(/,'WARNING: THE KEYWORD "GPGCDSC" CANNOT BE USED WITHOUT', GDH1/96
     .       ' AN SMx KEYWORD')                                         GDH1/96
630   FORMAT(/,'WARNING: THE KEYWORD "0SCF" CANNOT BE USED WITH',       GDH1/96
     .       ' THE "TRUES" KEYWORD')                                    GDH1/96
640   FORMAT(/,'WARNING: THE KEYWORD "NOPOL" CANNOT BE USED WITH',      GDH1/96
     .       ' THE "TRUES" KEYWORD')                                    GDH1/96
650   FORMAT(/,'WARNING: THE KEYWORD "DEV" MUST BE USED WITH THE ',/,   DJG0496
     1       'GENERAL MODELS SM5.4U AND PD-SM5.4U')                     DJG1296
660   FORMAT(/,'WARNING: USE OF THE SM5 MODELS WITH ORGANIC SOLVENTS',/ GDH0997
     .       , 'REQUIRES THE KEYWORDS ',                                GDH0997
     1       '"IOFR", "ALPHA", "BETA", "GAMMA", AND "DIELEC"')          DJG0496
661   FORMAT(/,'WARNING: USE OF THE SM5.*R MODELS WITH ORGANIC ',       GDH0997
     .       'SOLVENTS ',/                                              GDH0997
     .       , 'REQUIRES THE KEYWORDS ',                                GDH0997
     1       '"FACARB" AND "FEHALO"')                                   DJG0496
670   FORMAT(/,'WARNING: USE OF "IOFR", "ALPHA", "BETA", "GAMMA", '     GDH0997
     1       ,'"FACARB", AND "FEHALO" IS',                              GDH0997
     1       /,' ALLOWED ONLY WITH THE GENORG MODEL')                   DJG0297
680   FORMAT(/,'WARNING: THE SOLVENT PROPERTIES ENTERED ARE ',          DJG0496
     1       'VERY CLOSE TO THOSE OF WATER,',/,'AND WATER SHOULD NOT',/,DJG0496
     2       ' BE TREATED USING THE OSM5 FORMALISM.')                   DJG0496
690   FORMAT(/,'THE KEYWORD XYZ HAS BEEN DISABLED IN THIS RELEASE OF ', DJG0796
     1       'AMSOL.',/,'FUTURE VERSIONS OF AMSOL MAY BE ENABLED TO ',  DJG0796
     2       'PERFORM CARTESIAN OPTIMIZATIONS.',/,'TO INPUT THE ',      DJG0796
     3       'GEOMETRY IN CARTESIAN COORDINATES, USE THE KEYWORD "CART"'DJG0796
     4       ,/,'ALL CALCULATIONS WILL BE PERFORMED USING INTERNAL ',   GDH1297
     5       'COORDINATES.')                                            DJG0796
700   FORMAT(/,'THE KEYWORD GEPOL IS NO LONGER SUPORTED')               GDH0397
710   FORMAT(/,'THE KEYWORD VOLUME IS NO LONGER SUPORTED')              GDH0397
720   FORMAT(/,'THE SM5.0R MODEL MAY NOT BE RUN WITH ANY TYPE OF '      GDH0497
     .       ,/,'GEOMETRY OR SCF OPTIMIZATION, SEE THE MANUAL OR ',     GDH0497
     .       /,'SM5.0R PAPER FOR FURTHER DETAILS.')                     GDH0497
730   FORMAT(/,'THE SM5.05R MODEL MAY NOT BE RUN WITH ANY TYPE OF '     GDH0497
     .       ,/,'GEOMETRY OR SCF OPTIMIZATION, SEE THE MANUAL OR ',     GDH0497
     .       /,'SM5.05R PAPER FOR FURTHER DETAILS.')                    GDH0497
740   FORMAT(/,'THE SM5.2R MODEL MAY NOT BE RUN WITH ANY TYPE OF ',     PDW1099
     1       'GEOMETRY OPTIMIZATION, SO YOU',/,'MUST USE THE 1SCF',     PDW1099
     2       ' KEYWORD.  PLEASE SEE THE MANUAL OR THE ',                PDW1099
     3       'SM5.2R PAPER FOR',/,'FURTHER DETAILS.',/,'IF YOU WANT ',  PDW1099
     4       'TO OPTIMIZE IN SOLUTION PLEASE USE USE THE SM5.2 ',       PDW1099
     5       'KEYWORD.')                                                PDW1099
750   FORMAT(/,'THE SM5.42R MODEL MAY NOT BE RUN WITH ANY TYPE OF ',    PDW1099
     1       'GEOMETRY OPTIMIZATION, SO YOU',/,'MUST USE THE 1SCF',     PDW1099
     2       ' KEYWORD.  PLEASE SEE THE MANUAL OR THE ',                PDW1099
     3       'SM5.42R PAPER FOR',/,'FURTHER DETAILS.',/,'IF YOU WANT ', PDW1099
     4       'TO OPTIMIZE IN SOLUTION PLEASE USE USE THE SM5.42 ',      PDW1099
     5       'KEYWORD.')                                                PDW1099
760   FORMAT(/,'THE SM5.2 MODEL MUST BE RUN WITH NUMERICAL ',           PDW1099
     1       'GEOMETRY OPTIMIZATION.',/,'SEE THE MANUAL ',              PDW1099
     2       'FOR FURTHER DETAILS.  IF YOU DO NOT WANT ',               PDW1099
     3       'TO OPTIMIZE IN SOLUTION PLEASE USE USE THE SM5.2R ',      PDW1099
     4       'KEYWORD.',/,'IT IS ALSO IMPORTANT TO NOTE THAT THE ',     PDW1099
     5       'SURFACE TENSIONS HAVE ONLY BEEN',/,'OPTIMIZED FOR',       PDW1099
     6       'GAS-PHASE GEOMETRIES.')                                   PDW1099
770   FORMAT(/,'THE SM5.42 MODEL MUST BE RUN WITH NUMERICAL ',          PDW1099
     1       'GEOMETRY OPTIMIZATION.',/,'SEE THE MANUAL ',              PDW1099
     2       'FOR FURTHER DETAILS.  IF YOU DO NOT WANT ',               PDW1099
     3       'TO OPTIMIZE IN',/,'SOLUTION PLEASE USE USE THE SM5.42R ', PDW1099
     4       'KEYWORD.',/,'IT IS ALSO IMPORTANT TO NOTE THAT THE ',     PDW1099
     5       'SURFACE TENSIONS HAVE ONLY BEEN',/,'OPTIMIZED FOR ',      PDW1099
     6       'GAS-PHASE GEOMETRIES.')                                   PDW1099
3000  FORMAT(/,I3)

      END
