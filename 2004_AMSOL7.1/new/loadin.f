      SUBROUTINE LOADIN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
         INCLUDE 'FFILES.i'                                             GDH1095
         INCLUDE 'KEYS.i'                                               GDH0496
C*****************************************************************************
C
C    THIS SUBROUTINE LOADS THE SOLVATION PARAMETERS INTO THE VARIABLES
C
C       SIGMA0 = SURFACE TENSIONS (SIGMA ZERO)
C       RKCDS = SURFACE TENSION RADII (Rk)
C       RHONOT = COULOMB RADII PART I "rho zero"
C       RHOONE = COULOMB RADII PART II "rho one"
C       QKNOT = PART OF COULOMB RADII CALC "q not"
C       QKONE = PART OF COULOMB RADII CALC "q one"
C       HBCORC = SURFACE TENSION CORRECTION FOR H-BONDS (SM2(.X) AND SM3(.X))
C       CRFNH4 = C-C BOND SURFACE TENSION
C
C    CREATED BY DJG 0995 FROM EXISTING LINES IN SMX1
C
C******************************************************************************
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),
     1                HBCORC(100),QKNOT(100),QKONE(100)
      COMMON /HEXSTM/ HRFN4(100),CRFNH4,CNRFN4,CORFN5,                  GDH0797
     X                CRFNH5,OORFN5,ONRFN4,SSRFN5,OPRFN5,               GDH0797
     1                FCRFN,CLCRFN,BRCRFN,CNSTC2,TNCSC3,TNCSC2,         GDH0797
     2                HOHST1,HNNST1, SPRFN5,OSIBD5                      PDW0901
      COMMON /PDCOM/  SMXPD(100), SPSRFT(10)
      COMMON /DKKCOM/ DKKG, DKKCH, DKKOO, DKKHH                         GDH0297
         INCLUDE 'PARAMS.i'
      IF (IRAD.EQ.0) THEN                                               GDH1093
         CALL SCOPY(100,SRFEN1,1,SIGMA0,1)                              DJG0995
         CALL SCOPY(100,W1,1,RKCDS,1)                                   DJG0995
         CALL SCOPY(100,PK01,1,RHONOT,1)                                DJG0995
         CALL SCOPY(100,PK11,1,RHOONE,1)                                DJG0995
         CALL SCOPY(100,QK01,1,QKNOT,1)                                 DJG0995
         CALL SCOPY(100,QK01A,1,QKONE,1)                                DJG0995
         DKKCH=DKKVAL(1)                                                GDH0596
         DKKG=DKKVAL(2)                                                 GDH0596
      ELSE IF (IRAD.EQ.1) THEN                                          GDH1093
         CALL SCOPY(100,SRFN1A,1,SIGMA0,1)                              DJG0995
         CALL SCOPY(100,W1,1,RKCDS,1)                                   DJG0995
         CALL SCOPY(100,PK01,1,RHONOT,1)                                DJG0995
         CALL SCOPY(100,PK11,1,RHOONE,1)                                DJG0995
         CALL SCOPY(100,QK01,1,QKNOT,1)                                 DJG0995
         CALL SCOPY(100,QK01A,1,QKONE,1)                                DJG0995
         DKKCH=DKKVAL(1)                                                GDH0596
         DKKG=DKKVAL(2)                                                 GDH0596
      ELSE IF (IRAD.EQ.2) THEN                                          GDH1093
         CALL SCOPY(100,SRFEN2,1,SIGMA0,1)                              DJG0995
         CALL SCOPY(100,W2,1,RKCDS,1)                                   DJG0995
         CALL SCOPY(100,PK02,1,RHONOT,1)                                DJG0995
         CALL SCOPY(100,PK12,1,RHOONE,1)                                DJG0995
         CALL SCOPY(100,QK02,1,QKNOT,1)                                 DJG0995
         CALL SCOPY(100,BK2,1,HBCORC,1)                                 DJG0995
         CALL SCOPY(100,QK02A,1,QKONE,1)                                DJG0995
         CALL SCOPY(10,SPST2,1,SPSRFT,1)                                GDH0296
         DKKCH=DKKVAL(1)                                                GDH0596
         DKKG=DKKVAL(2)                                                 GDH0596
      ELSE IF (IRAD.EQ.3) THEN                                          GDH1093
         CALL SCOPY(100,SRFEN3,1,SIGMA0,1)                              DJG0995
         CALL SCOPY(100,W3,1,RKCDS,1)                                   DJG0995
         CALL SCOPY(100,PK03,1,RHONOT,1)                                DJG0995
         CALL SCOPY(100,PK13,1,RHOONE,1)                                DJG0995
         CALL SCOPY(100,QK03,1,QKNOT,1)                                 DJG0995
         CALL SCOPY(100,BK3,1,HBCORC,1)                                 DJG0995
         CALL SCOPY(100,QK03A,1,QKONE,1)                                DJG0995
         CALL SCOPY(10,SPST3,1,SPSRFT,1)                                GDH0296
         DKKCH=DKKVAL(1)                                                GDH0596
         DKKG=DKKVAL(2)                                                 GDH0596
      ELSE IF (IRAD.EQ.4) THEN                                          CCC1295
         CALL SCOPY(100,W5,1,RKCDS,1)                                   CCC1295
         CALL SCOPY(100,PK05,1,RHONOT,1)                                CCC1295
         CALL SCOPY(100,PK15,1,RHOONE,1)                                CCC1295
         CALL SCOPY(100,QK05,1,QKNOT,1)                                 CCC1295
         CALL SCOPY(100,QK055,1,QKONE,1)                                CCC1295
         DKKCH=DKKVAL(3)                                                GDH0596
         DKKG=DKKVAL(4)                                                 GDH0596
         IF(ISM5U.EQ.1) THEN                                            DJG0496
            CALL SCOPY(100,SSTS5,1,SIGMA0,1)                            CCC0496
            CALL SCOPY(100,HRFN5,1,HRFN4,1)                             CCC0496
            CRFNH4=CCSRFT(4)                                            CCC0496
            CRFNH5=CCSRF5(4)                                            CCC0496
            CORFN5=OCSFT(4)                                             CCC0496
            CNRFN4=NCSFT(4)                                             CCC0496
            ONRFN4=ONSFT(4)                                             CCC0496
            OORFN5=OOSFT(4)                                             CCC0496
            SSRFN5=SSSFT(4)                                             CCC0496
         ELSEIF(ISM5A.EQ.1) THEN                                        CCC0496
            CALL SCOPY(100,SSTS5A,1,SIGMA0,1)                           CCC0496
            CALL SCOPY(100,HRFN5A,1,HRFN4,1)                            CCC0496
            CRFNH4=CCSRFT(5)                                            CCC0496
            CRFNH5=CCSRF5(5)                                            CCC0496
            CORFN5=OCSFT(5)                                             CCC0496
            CNRFN4=NCSFT(5)                                             CCC0496
            ONRFN4=ONSFT(5)                                             CCC0496
            OORFN5=OOSFT(5)                                             CCC0496
            SSRFN5=SSSFT(5)                                             CCC0496
         ELSEIF(ISM5P.EQ.1) THEN                                        CCC0496
            CALL SCOPY(100,SSTS5P,1,SIGMA0,1)                           CCC0496
            CALL SCOPY(100,HRFN5P,1,HRFN4,1)                            CCC0496
            CRFNH4=CCSRFT(7)                                            CCC0496
            CRFNH5=CCSRF5(7)                                            CCC0496
            CORFN5=OCSFT(7)                                             CCC0496
            CNRFN4=NCSFT(7)                                             CCC0496
            ONRFN4=ONSFT(7)                                             CCC0496
            OORFN5=OOSFT(7)                                             CCC0496
            SSRFN5=SSSFT(7)                                             CCC0496
         ENDIF                                                          CCC0496
      ELSE IF (IRAD.EQ.5) THEN                                          GDH0496
           IF (IPDS5U.EQ.1) THEN                                        DJG0796
              CALL SCOPY(100,W5,1,RKCDS,1)                              GDH0496
              CALL SCOPY(100,PK5PD,1,RHONOT,1)                          GDH0496
              CALL SCOPY(100,PK15PD,1,RHOONE,1)                         GDH0496
              CALL SCOPY(100,QK05,1,QKNOT,1)                            GDH0496
              CALL SCOPY(100,QK055,1,QKONE,1)                           GDH0496
              CALL SCOPY(100,SM5PD,1,SMXPD,1)                           GDH0496
              CALL SCOPY(100,STPD5,1,SIGMA0,1)                          GDH0496
              CALL SCOPY(100,HFPD5,1,HRFN4,1)                           GDH0496
              CRFNH4=CCSRFT(1)                                          GDH0496
              CRFNH5=CCSRF5(1)                                          GDH0496
              CORFN5=OCSFT(1)                                           GDH0496
              CNRFN4=NCSFT(1)                                           GDH0496
              ONRFN4=ONSFT(1)                                           GDH0496
              OORFN5=OOSFT(1)                                           GDH0496
              SSRFN5=SSSFT(1)                                           GDH0496
              DKKCH=DKKVAL(7)                                           GDH0596
              DKKG=DKKVAL(8)                                            GDH0596
           ELSE IF (IPDS5A.EQ.1) THEN                                   GDH0496
              CALL SCOPY(100,W5,1,RKCDS,1)                              GDH0496
              CALL SCOPY(100,PK5PD,1,RHONOT,1)                          GDH0496
              CALL SCOPY(100,PK15PD,1,RHOONE,1)                         GDH0496
              CALL SCOPY(100,QK05,1,QKNOT,1)                            GDH0496
              CALL SCOPY(100,QK055,1,QKONE,1)                           GDH0496
              CALL SCOPY(100,SM5PD,1,SMXPD,1)                           GDH0496
              CALL SCOPY(100,STPD5A,1,SIGMA0,1)                         GDH0496
              CALL SCOPY(100,HFPD5A,1,HRFN4,1)                          GDH0496
              CRFNH4=CCSRFT(2)                                          GDH0496
              CRFNH5=CCSRF5(2)                                          GDH0496
              CORFN5=OCSFT(2)                                           GDH0496
              CNRFN4=NCSFT(2)                                           GDH0496
              ONRFN4=ONSFT(2)                                           GDH0496
              OORFN5=OOSFT(2)                                           GDH0496
              SSRFN5=SSSFT(2)                                           GDH0496
              DKKCH=DKKVAL(7)                                           GDH0596
              DKKG=DKKVAL(8)                                            GDH0596
           ELSE IF (IPDS5P.EQ.1) THEN                                   GDH0496
              CALL SCOPY(100,W5,1,RKCDS,1)                              GDH0496
              CALL SCOPY(100,PK5PD,1,RHONOT,1)                          GDH0496
              CALL SCOPY(100,PK15PD,1,RHOONE,1)                         GDH0496
              CALL SCOPY(100,QK05,1,QKNOT,1)                            GDH0496
              CALL SCOPY(100,QK055,1,QKONE,1)                           GDH0496
              CALL SCOPY(100,SM5PD,1,SMXPD,1)                           GDH0496
              CALL SCOPY(100,STPD5P,1,SIGMA0,1)                         GDH0496
              CALL SCOPY(100,HFPD5P,1,HRFN4,1)                          GDH0496
              CRFNH4=CCSRFT(3)                                          GDH0496
              CRFNH5=CCSRF5(3)                                          GDH0496
              CORFN5=OCSFT(3)                                           GDH0496
              CNRFN4=NCSFT(3)                                           GDH0496
              ONRFN4=ONSFT(3)                                           GDH0496
              OORFN5=OOSFT(3)                                           GDH0496
              SSRFN5=SSSFT(3)                                           GDH0496
              DKKCH=DKKVAL(7)                                           GDH0596
              DKKG=DKKVAL(8)                                            GDH0596
           ELSE IF (IS5A2P.EQ.1) THEN                                   GDH0596
              CALL SCOPY(100,W5,1,RKCDS,1)                              GDH0596
              CALL SCOPY(100,PK5A2P,1,RHONOT,1)                         GDH0596
              CALL SCOPY(100,PK15PD,1,RHOONE,1)                         GDH0596
              CALL SCOPY(100,QK05,1,QKNOT,1)                            GDH0596
              CALL SCOPY(100,QK055,1,QKONE,1)                           GDH0596
              CALL SCOPY(100,SM5A2P,1,SMXPD,1)                          GDH0596
              CALL SCOPY(100,SPD52A,1,SIGMA0,1)                         GDH0596
              CALL SCOPY(100,HPD52A,1,HRFN4,1)                          GDH0596
              CRFNH4=CCSRFT(8)                                          GDH0596
              CRFNH5=CCSRF5(8)                                          GDH0596
              CORFN5=OCSFT(8)                                           GDH0596
              CNRFN4=NCSFT(8)                                           GDH0596
              ONRFN4=ONSFT(8)                                           GDH0596
              OPRFN5=OPSFT(8)                                           GDH0596
              OORFN5=OOSFT(8)                                           GDH0596
              SSRFN5=SSSFT(8)                                           GDH0596
              DKKCH=DKKVAL(9)                                           GDH0596
              DKKG=DKKVAL(10)                                           GDH0596
           ELSE IF (IS5P2P.EQ.1) THEN                                   GDH0596
              CALL SCOPY(100,W5,1,RKCDS,1)                              GDH0596
              CALL SCOPY(100,PK5P2P,1,RHONOT,1)                         GDH0596
              CALL SCOPY(100,PK15PD,1,RHOONE,1)                         GDH0596
              CALL SCOPY(100,QK05,1,QKNOT,1)                            GDH0596
              CALL SCOPY(100,QK055,1,QKONE,1)                           GDH0596
              CALL SCOPY(100,SM5P2P,1,SMXPD,1)                          GDH0596
              CALL SCOPY(100,SPD52P,1,SIGMA0,1)                         GDH0596
              CALL SCOPY(100,HPD52P,1,HRFN4,1)                          GDH0596
              CRFNH4=CCSRFT(9)                                          GDH0596
              CRFNH5=CCSRF5(9)                                          GDH0596
              CORFN5=OCSFT(9)                                           GDH0596
              CNRFN4=NCSFT(9)                                           GDH0596
              ONRFN4=ONSFT(9)                                           GDH0596
              OORFN5=OOSFT(9)                                           GDH0596
              SSRFN5=SSSFT(9)                                           GDH0596
              DKKCH=DKKVAL(11)                                          GDH0596
              DKKG=DKKVAL(12)                                           GDH0596
           ENDIF                                                        GDH0596
      ELSE IF (IRAD.EQ.6) THEN                                          DJG0995
         CALL SCOPY(100,SRFNHA,1,SIGMA0,1)                              DJG0995
         CALL SCOPY(100,WHA,1,RKCDS,1)                                  DJG0995
         CALL SCOPY(100,PK0HA,1,RHONOT,1)                               DJG0995
         CALL SCOPY(100,PK1HA,1,RHOONE,1)                               DJG0995
         CALL SCOPY(100,QK0HA,1,QKNOT,1)                                DJG0995
         CALL SCOPY(100,QK0HAA,1,QKONE,1)                               DJG0995
         CALL SCOPY(100,HRFNH4,1,HRFN4,1)                               DJG0995
         DKKCH=DKKVAL(1)                                                GDH0596
         DKKG=DKKVAL(2)                                                 GDH0596
         CRFNH4=CCSRFT(6)                                               DJG0995
      ELSE IF (IRAD.EQ.7) THEN                                          DJG0196
         CALL SCOPY(100,W5,1,RKCDS,1)                                   DJG0196
         CALL SCOPY(100,PK05,1,RHONOT,1)                                DJG0196
         CALL SCOPY(100,PK15,1,RHOONE,1)                                DJG0196
         CALL SCOPY(100,QK05,1,QKNOT,1)                                 DJG0196
         CALL SCOPY(100,QK055,1,QKONE,1)                                DJG0196
         CALL SCOPY(100,HRFNH5,1,HRFN4,1)                               DJG0196
         DKKCH=DKKVAL(3)                                                GDH0596
         DKKG=DKKVAL(4)                                                 GDH0596
         CALL OSM5SP                                                    DJG0396
      ELSE IF (IRAD.EQ.8) THEN                                          DJG0896
         CALL SCOPY(100,W5,1,RKCDS,1)                                   DJG0896
         CALL SCOPY(100,PK05,1,RHONOT,1)                                DJG0896
         CALL SCOPY(100,PK15,1,RHOONE,1)                                DJG0896
         CALL SCOPY(100,QK05,1,QKNOT,1)                                 DJG0896
         CALL SCOPY(100,QK055,1,QKONE,1)                                DJG0896
         DKKCH=DKKVAL(3)                                                DJG0896
         DKKG=DKKVAL(4)                                                 DJG0896
         IF(CISOLV.EQ.'CHCL3') THEN                                     DJG0297
            IF(ISM5A.EQ.1) THEN                                         DJG0297
               NVAL=12                                                  DJG0297
               CALL SCOPY(100,SSTS8A,1,SIGMA0,1)                        DJG0297
               CALL SCOPY(100,HRFN8A,1,HRFN4,1)                         DJG0297
            ELSEIF(ISM5P.EQ.1) THEN                                     DJG0297
               NVAL=13                                                  DJG0297
               CALL SCOPY(100,SSTS8P,1,SIGMA0,1)                        DJG0297
               CALL SCOPY(100,HRFN8P,1,HRFN4,1)                         DJG0297
            ENDIF                                                       DJG0297
         ELSEIF(CISOLV.EQ.'BENZENE') THEN                               DJG0297
            IF(ISM5A.EQ.1) THEN                                         DJG0297
               NVAL=14                                                  DJG0297
               CALL SCOPY(100,SSTB8A,1,SIGMA0,1)                        DJG0297
               CALL SCOPY(100,HRFB8A,1,HRFN4,1)                         DJG0297
            ELSEIF(ISM5P.EQ.1) THEN                                     DJG0297
               NVAL=15                                                  DJG0297
               CALL SCOPY(100,SSTB8P,1,SIGMA0,1)                        DJG0297
               CALL SCOPY(100,HRFB8P,1,HRFN4,1)                         DJG0297
            ENDIF                                                       DJG0297
         ELSEIF(CISOLV.EQ.'TOLUENE') THEN                               DJG0297
            IF(ISM5A.EQ.1) THEN                                         DJG0297
               NVAL=16                                                  DJG0297
               CALL SCOPY(100,SSTT8A,1,SIGMA0,1)                        DJG0297
               CALL SCOPY(100,HRFT8A,1,HRFN4,1)                         DJG0297
            ELSEIF(ISM5P.EQ.1) THEN                                     DJG0297
               NVAL=17                                                  DJG0297
               CALL SCOPY(100,SSTT8P,1,SIGMA0,1)                        DJG0297
               CALL SCOPY(100,HRFT8P,1,HRFN4,1)                         DJG0297
            ENDIF                                                       DJG0297
         ENDIF                                                          DJG0297
         CRFNH4=CCSRFT(NVAL)                                            DJG0297
         CRFNH5=CCSRF5(NVAL)                                            DJG0297
         CORFN5=OCSFT(NVAL)                                             DJG0297
         CNRFN4=NCSFT(NVAL)                                             DJG0297
         ONRFN4=ONSFT(NVAL)                                             DJG0297
         OORFN5=OOSFT(NVAL)                                             DJG0297
         SSRFN5=SSSFT(NVAL)                                             DJG0297
      ELSE IF (IRAD.EQ.9) THEN                                          GDH0997
         DKKCH=DKKVAL(3)                                                GDH0596
         DKKG=DKKVAL(4)                                                 GDH0596
         CALL SCOPY(100,W1,1,RKCDS,1)                                   GDH0997
         CALL SCOPY(100,PK06,1,RHONOT,1)                                GDH0997
C  The next three lines of parameters are not used in this method
C  and are therefore set to zero.
         CALL SCOPY(100,PK15PD,1,RHOONE,1)                              GDH0997
         CALL SCOPY(100,QK05,1,QKNOT,1)                                 GDH0997
         CALL SCOPY(100,QK055,1,QKONE,1)                                GDH0997
        IF (NUMSLV.EQ.20) THEN                                          GDH0997
         CALL OSM5SP                                                    GDH0997
        ELSE                                                            GDH0997
         IF(IMNDO.EQ.1) THEN                                            GDH0897
            CALL SCOPY(100,S052M,1,SIGMA0,1)                            GDH0997
            CALL SCOPY(100,HS052M,1,HRFN4,1)                            GDH0997
            CRFNH4=S052M(101)                                           GDH1296
            CRFNH5=S052M(102)                                           GDH1296
            CORFN5=S052M(103)                                           GDH1296
            CNRFN4=S052M(105)                                           GDH1296
            ONRFN4=S052M(106)                                           GDH1296
            OORFN5=S052M(104)                                           GDH1296
            SSRFN5=S052M(107)                                           GDH1296
            CNSTC2=S052M(110)                                           GDH1296
            TNCSC2=S052M(111)                                           GDH1296
            HNNST1=S052M(112)                                           GDH1296
            HOHST1=S052M(113)                                           GDH1296
            OPRFN5=S052M(114)                                           GDH0797
            SPRFN5=S052M(115)                                           GDH0797
            CSSIGM=0.0D0                                                GDH0997
         ELSEIF(IAM1.EQ.1) THEN                                         GDH0997
            CALL SCOPY(100,S052A,1,SIGMA0,1)                            GDH0997
            CALL SCOPY(100,HS052A,1,HRFN4,1)                            GDH0997
            CRFNH4=S052A(101)                                           GDH1296
            CRFNH5=S052A(102)                                           GDH1296
            CORFN5=S052A(103)                                           GDH1296
            CNRFN4=S052A(105)                                           GDH1296
            ONRFN4=S052A(106)                                           GDH1296
            OORFN5=S052A(104)                                           GDH1296
            SSRFN5=S052A(107)                                           GDH1296
            CNSTC2=S052A(110)                                           GDH1296
            TNCSC2=S052A(111)                                           GDH1296
            HNNST1=S052A(112)                                           GDH1296
            HOHST1=S052A(113)                                           GDH1296
            OPRFN5=S052A(114)                                           GDH0797
            SPRFN5=S052A(115)                                           GDH0797
            CSSIGM=0.0D0                                                GDH0997
         ELSEIF(IPM3.EQ.1) THEN                                         GDH0997
            CALL SCOPY(100,S052P,1,SIGMA0,1)                            GDH0997
            CALL SCOPY(100,HS052P,1,HRFN4,1)                            GDH0997
            CRFNH4=S052P(101)                                           GDH1296
            CRFNH5=S052P(102)                                           GDH1296
            CORFN5=S052P(103)                                           GDH1296
            CNRFN4=S052P(105)                                           GDH1296
            ONRFN4=S052P(106)                                           GDH1296
            OORFN5=S052P(104)                                           GDH1296
            SSRFN5=S052P(107)                                           GDH1296
            CNSTC2=S052P(110)                                           GDH1296
            TNCSC2=S052P(111)                                           GDH1296
            HNNST1=S052P(112)                                           GDH1296
            HOHST1=S052P(113)                                           GDH1296
            OPRFN5=S052P(114)                                           GDH0797
            SPRFN5=S052P(115)                                           GDH0797
            CSSIGM=0.0D0                                                GDH0997
         ENDIF                                                          GDH0997
        ENDIF                                                           GDH0997
      ELSE IF (IRAD.EQ.10) THEN                                         GDH0997
         DKKCH=4.2D0                                                    GDH0596
         DKKG=3.9D0                                                     GDH0596
         CALL SCOPY(100,W1,1,RKCDS,1)                                   GDH0997
         CALL SCOPY(100,PK07,1,RHONOT,1)                                GDH0997
C  The next three lines of parameters are not used in this method
C  and are therefore set to zero.
         CALL SCOPY(100,QK05,1,QKNOT,1)                                 GDH0997
         CALL SCOPY(100,QK055,1,QKONE,1)                                GDH0997
        IF (NUMSLV.EQ.20) THEN                                          GDH0997
         CALL OSM5SP                                                    GDH0997
        ELSE                                                            GDH0997
         IF(IAM1.EQ.1) THEN                                             GDH0997
            CALL SCOPY(100,S054A,1,SIGMA0,1)                            GDH0997
            CALL SCOPY(100,HS054A,1,HRFN4,1)                            GDH0997
            CRFNH4=S054A(101)                                           GDH1296
            CRFNH5=S054A(102)                                           GDH1296
            CORFN5=S054A(103)                                           GDH1296
            CNRFN4=S054A(105)                                           GDH1296
            ONRFN4=S054A(106)                                           GDH1296
            OORFN5=S054A(104)                                           GDH1296
            SSRFN5=S054A(107)                                           GDH1296
            OSIBD5=S054A(108)                                           PDW0901
            CNSTC2=S054A(110)                                           GDH1296
            TNCSC2=S054A(111)                                           GDH1296
            HNNST1=S054A(112)                                           GDH1296
            HOHST1=S054A(113)                                           GDH1296
            OPRFN5=S054A(114)                                           GDH0797
            SPRFN5=S054A(115)                                           GDH0797
            TNCSC3=S054A(116)                                           GDH1197
            CSSIGM=0.0D0                                                GDH0997
         ELSEIF(IPM3.EQ.1) THEN                                         GDH0997
            CALL SCOPY(100,S054P,1,SIGMA0,1)                            GDH0997
            CALL SCOPY(100,HS054P,1,HRFN4,1)                            GDH0997
            CRFNH4=S054P(101)                                           GDH1296
            CRFNH5=S054P(102)                                           GDH1296
            CORFN5=S054P(103)                                           GDH1296
            CNRFN4=S054P(105)                                           GDH1296
            ONRFN4=S054P(106)                                           GDH1296
            OORFN5=S054P(104)                                           GDH1296
            SSRFN5=S054P(107)                                           GDH1296
            OSIBD5=S054P(108)                                           PDW0901
            CNSTC2=S054P(110)                                           GDH1296
            TNCSC2=S054P(111)                                           GDH1296
            HNNST1=S054P(112)                                           GDH1296
            HOHST1=S054P(113)                                           GDH1296
            OPRFN5=S054P(114)                                           GDH0797
            SPRFN5=S054P(115)                                           GDH0797
            TNCSC3=S054P(116)                                           GDH1197
            CSSIGM=0.0D0                                                GDH0997
         ENDIF                                                          GDH0997
        ENDIF                                                           GDH0997
      ELSE IF (IRAD.EQ.12) THEN                                         GDH1093
         CALL SCOPY(100,SRFN12,1,SIGMA0,1)                              DJG0995
         CALL SCOPY(100,W12,1,RKCDS,1)                                  DJG0995
         CALL SCOPY(100,PK012,1,RHONOT,1)                               DJG0995
         CALL SCOPY(100,PK112,1,RHOONE,1)                               DJG0995
         CALL SCOPY(100,QK012,1,QKNOT,1)                                DJG0995
         CALL SCOPY(100,BK12,1,HBCORC,1)                                DJG0995
         CALL SCOPY(100,QK012A,1,QKONE,1)                               DJG0995
         CALL SCOPY(10,SPST2,1,SPSRFT,1)                                GDH0296
         DKKCH=DKKVAL(1)                                                GDH0596
         DKKG=DKKVAL(2)                                                 GDH0596
      ELSE IF (IRAD.EQ.13) THEN                                         GDH1093
         CALL SCOPY(100,SRFN13,1,SIGMA0,1)                              DJG0995
         CALL SCOPY(100,W13,1,RKCDS,1)                                  DJG0995
         CALL SCOPY(100,PK013,1,RHONOT,1)                               DJG0995
         CALL SCOPY(100,PK113,1,RHOONE,1)                               DJG0995
         CALL SCOPY(100,QK013,1,QKNOT,1)                                DJG0995
         CALL SCOPY(100,BK13,1,HBCORC,1)                                DJG0995
         CALL SCOPY(100,QK013A,1,QKONE,1)                               DJG0995
         CALL SCOPY(10,SPST3,1,SPSRFT,1)                                GDH0296
         DKKCH=DKKVAL(2)                                                GDH0596
         DKKG=DKKVAL(1)                                                 GDH0596
      ELSE IF (IRAD.EQ.20) THEN                                         GDH1296
              CALL SCOPY(100,W50,1,RKCDS,1)                             GDH1296
              CALL SCOPY(100,SPD50,1,SIGMA0,1)                          GDH1296
              CALL SCOPY(100,HRFN50,1,HRFN4,1)                          GDH1296
              CALL SCOPY(100,W50,1,RHONOT,1)                            GDH1296
              DKKCH=DKKVAL(3)                                           GDH1296
              DKKG=DKKVAL(4)                                            GDH1296
              DKKHH=DKKVAL(13)                                          GDH0297
              DKKOO=DKKVAL(14)                                          GDH0297
              CRFNH4=ADST50(1)                                          GDH1296
              CRFNH5=ADST50(2)                                          GDH1296
              CORFN5=ADST50(3)                                          GDH1296
              CNRFN4=ADST50(5)                                          GDH1296
              ONRFN4=ADST50(9)                                          GDH1296
              OORFN5=ADST50(4)                                          GDH1296
              SSRFN5=ADST50(13)                                         GDH1296
              FCRFN=ADST50(10)                                          GDH1296
              CLCRFN=ADST50(11)                                         GDH1296
              BRCRFN=ADST50(12)                                         GDH1296
              CNSTC2=ADST50(8)                                          GDH1296
              TNCSC3=ADST50(7)                                          GDH1296
              TNCSC2=ADST50(6)                                          GDH1296
C  The following values are loaded to make the output not have          GDH1296
C  undefined numbers BUT these are not necessary for this method        GDH1296
              CALL SCOPY(100,PK15PD,1,RHOONE,1)                         GDH1296
              CALL SCOPY(100,PK15PD,1,QKNOT,1)                          GDH1296
              CALL SCOPY(100,PK15PD,1,QKONE,1)                          GDH1296
              CALL SCOPY(100,PK15PD,1,SMXPD,1)                          GDH1296
      ELSE IF (IRAD.EQ.21) THEN                                         GDH0797
         CALL SCOPY(100,W50,1,RKCDS,1)                                  GDH0797
         DKKCH=DKKVAL(3)                                                GDH0797
         DKKG=DKKVAL(4)                                                 GDH0797
         CALL OSM5SP                                                    GDH0797
      ELSE IF (IRAD.EQ.22) THEN                                         GDH1093
         CALL SCOPY(100,SRFN22,1,SIGMA0,1)                              DJG0995
         CALL SCOPY(100,W12,1,RKCDS,1)                                  DJG0995
         CALL SCOPY(100,PK012,1,RHONOT,1)                               DJG0995
         CALL SCOPY(100,PK112,1,RHOONE,1)                               DJG0995
         CALL SCOPY(100,QK012,1,QKNOT,1)                                DJG0995
         CALL SCOPY(100,BK22,1,HBCORC,1)                                DJG0995
         CALL SCOPY(100,QK012A,1,QKONE,1)                               DJG0995
         CALL SCOPY(100,SM22PD,1,SMXPD,1)                               GDH0895
         CALL SCOPY(10,SPST2,1,SPSRFT,1)                                GDH0296
         DKKCH=DKKVAL(2)                                                GDH0596
         DKKG=DKKVAL(1)                                                 GDH0596
      ELSE IF (IRAD.EQ.23) THEN                                         GDH0496
         CALL SCOPY(100,SRFN23,1,SIGMA0,1)                              GDH0496
         CALL SCOPY(100,W12,1,RKCDS,1)                                  GDH0496
         CALL SCOPY(100,PK2PD,1,RHONOT,1)                               GDH0496
         CALL SCOPY(100,PK15PD,1,RHOONE,1)                              GDH0496
         CALL SCOPY(100,QK012,1,QKNOT,1)                                GDH0496
         CALL SCOPY(100,BK23,1,HBCORC,1)                                GDH0496
         CALL SCOPY(100,QK012A,1,QKONE,1)                               GDH0496
         CALL SCOPY(100,SM23PD,1,SMXPD,1)                               GDH0496
         CALL SCOPY(10,SPST23,1,SPSRFT,1)                               GDH0496
         DKKCH=DKKVAL(5)                                                GDH0596
         DKKG=DKKVAL(6)                                                 GDH0596
      ELSE                                                              GDH1093
         WRITE(JOUT,'('' UNIDENTIFIED "AMSOL" MODEL. ABORT.'')')        GDH1093
          ISTOP=1                                                       GDH1095
          IWHERE=134                                                    GDH1095
          CALL STPJOB                                                   DAL0303
      ENDIF                                                             GDH1293
      RETURN
      END
