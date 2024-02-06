CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   THIS FILE CONTAINS THE SOLVATION PARAMETERS FOR THE SMx MODELS IN AMSOL
C   AND THOSE PARAMETERS NEEDED FOR CM1
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DIMENSION W1(100),W2(100),W3(100),W5(100),W50(100)                GDH0696
      DIMENSION WHA(100),W10(100),W12(100),W13(100)                     DJG1094
      DIMENSION SRFEN1(100),SRFN1A(100),SRFEN2(100),SRFEN3(100)         DJG1094
      DIMENSION SRFNHA(100),SRFN11(100), SRFN50(100)                    GDH0696
      DIMENSION SSTS5(100),SSTS5A(100),SSTS5P(100)                      CCC0496
      DIMENSION SSTS8(100),SSTS8A(100),SSTS8P(100)                      DJG0896
      DIMENSION SSTB8A(100),SSTB8P(100),SSTT8A(100),SSTT8P(100)         DJG0297
      DIMENSION SRN11A(100),SRFN12(100),SRFN13(100)                     DJG1094
      DIMENSION BK2(100),BK3(100),BK12(100),BK13(100)                   DJG1094
      DIMENSION HRFN5(100),HRFNH4(100),HRFN5A(100),HRFN5P(100)          CCC0496
      DIMENSION HRFN8(100),HRFN8A(100),HRFN8P(100)                      DJG0896
      DIMENSION HRFB8A(100),HRFB8P(100),HRFT8A(100),HRFT8P(100)         DJG0297
      DIMENSION HRFN50(100)                                             GDH0696
      DIMENSION PK01(100),PK02(100),PK03(100),PK05(100)                 CCC1295
      DIMENSION PK0HA(100),PK011(100),PK012(100),PK013(100)             DJG1094
      DIMENSION PK11(100),PK12(100),PK13(100),PK15(100)                 CCC1295
      DIMENSION PK1HA(100),PK111(100),PK112(100),PK113(100)             DJG1094
      DIMENSION QK01(100),QK02(100),QK03(100),QK05(100)                 CCC1295
      DIMENSION QK0HA(100),QK011(100),QK012(100),QK013(100)             DJG1094
      DIMENSION QK01A(100),QK02A(100),QK03A(100),QK055(100)             CCC1295
      DIMENSION QK0HAA(100),QK011A(100),QK012A(100),QK013A(100)         DJG1094
      DIMENSION DIECON(100),SRADCD(100),SRADCS(100),CSSURF(100)         DJG1094
      DIMENSION SCALA1(100),OFSTA1(100),HOFFA1(100),OXOFF1(100)         DJG1094
      DIMENSION ANTOF1(100),SCALP1(100),OFSTP1(100),HOFFP1(100)         DJG1094
      DIMENSION ZHELP(100),CCSRFT(20),CCSRF5(20),OCSFT(20)              CCC1295
      DIMENSION NCSFT(20),ONSFT(20),OOSFT(20),SSSFT(20),OPSFT(20)       GDH0596
      DIMENSION SRFN22(100),BK22(100),BK23(100),BK33(100)               GDH0296
      DIMENSION SRFN23(100),SRFN33(100)                                 GDH0296
      DIMENSION SPST2(10), SPST3(10), SPST23(10)                        GDH0296
      DIMENSION SM22PD(100),SM23PD(100), SM5PD(100)                     GDH0496
      DIMENSION SM5P2P(100),SM5A2P(100)                                 GDH0596
      DIMENSION PK5PD(100), PK2PD(100), PK15PD(100)                     GDH0496
      DIMENSION PK5P2P(100), PK5A2P(100)                                GDH0596
      DIMENSION STPD5(100),STPD5A(100),STPD5P(100)                      GDH0496
      DIMENSION SPD52A(100),SPD52P(100),SPD50(100)                      GDH0596
      DIMENSION HFPD5(100),HFPD5A(100),HFPD5P(100)                      GDH0496
      DIMENSION HPD52A(100),HPD52P(100),DKKVAL(30)                      GDH0596
      DIMENSION SDEPNA(108),SDEPAA(108),SDEPBA(108)                     DJG0396
      DIMENSION HDEPNA(100),HDEPAA(100),HDEPBA(100)                     DJG0396
      DIMENSION SDEPNP(108),SDEPAP(108),SDEPBP(108)                     DJG0396
      DIMENSION HDEPNP(100),HDEPAP(100),HDEPBP(100)                     DJG0396
      DIMENSION SDEPNG(108),SDEPAG(108),SDEPBG(108)                     DJG0396
      DIMENSION HDEPNG(100),HDEPAG(100),HDEPBG(100)                     DJG0396
      DIMENSION ADST50(40), RHON50(100)                                 GDH0296
      DIMENSION NATCNV(100),RKKVAL(15,15),DRVAL(15)                     GDH0197
      DIMENSION RKKVL2(15,15),ENEGIT(100)                               GDH0197
      DIMENSION S50RNA(150),S50RAA(150),S50RBA(150),VBLANK(150)         GDH0797
      DIMENSION H50RNA(150),H50RAA(150),H50RBA(150),S50RTA(10)          GDH0797
C     SM5.2R MODELS                                                     GDH0997
      DIMENSION PK06(100)                                               GDH0797
      DIMENSION SDMPNA(150),SDMPAA(150),SDMPBA(150)                     GDH0396
      DIMENSION HDMPNA(150),HDMPAA(150),HDMPBA(150)                     GDH0396
      DIMENSION SDAPNA(150),SDAPAA(150),SDAPBA(150)                     GDH0797
      DIMENSION HDAPNA(150),HDAPAA(150),HDAPBA(150),SDAPTA(10)          GDH0797
      DIMENSION SDPPNA(150),SDPPAA(150),SDPPBA(150)                     GDH0797
      DIMENSION HDPPNA(150),HDPPAA(150),HDPPBA(150),SDPPTA(10)          GDH0797
      DIMENSION SDMPTA(10)                                              GDH0497
      DIMENSION HS052M(150), S052M(150)                                 GDH0297
      DIMENSION HS052A(150), S052A(150)                                 GDH0797
      DIMENSION HS052P(150), S052P(150)                                 GDH0797
C     SM5.42R MODELS                                                    GDH0198
      DIMENSION PK07(100),PK17(100)                                     GDH0797
      DIMENSION SDAPN4(150),SDAPA4(150),SDAPB4(150)                     GDH0797
      DIMENSION HDAPN4(150),HDAPA4(150),HDAPB4(150),SDAPT4(10)          GDH0797
      DIMENSION SDPPN4(150),SDPPA4(150),SDPPB4(150)                     GDH0797
      DIMENSION HDPPN4(150),HDPPA4(150),HDPPB4(150),SDPPT4(10)          GDH0797
      DIMENSION HS054A(150), S054A(150)                                 GDH0797
      DIMENSION HS054P(150), S054P(150)                                 GDH0797
C
C       VAN DER WAALS RADII A LA BONDI, JPC 68 (1964) 441.
C
      DATA W1/ 1.20D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0, 1.70D0, 1.55D0,
     1 1.52D0, 1.47D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0, 1.80D0,
     2 1.80D0, 1.75D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0, 1.40D0,
     3 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 1.85D0, 2.02D0, 0.00D0,
     4 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0, 2.10D0,
     5 2.06D0, 1.98D0, 2.60D0,46*0.D0/
C
C       EMPIRIC SURFACE TENSION RADII FOR SM2
C
      DATA W2/ 0.00D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0, 1.70D0, 1.90D0,
     1 1.80D0, 1.40D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0, 2.10D0,
     2 1.80D0, 2.00D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0, 1.40D0,
     3 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 2.00D0, 2.02D0, 0.00D0,
     4 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0, 2.10D0,
     5 2.06D0, 2.00D0, 2.60D0,46*0.D0/
C
C       EMPIRIC SURFACE TENSION RADII FOR SM3
C
      DATA W3/ 0.00D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0, 1.70D0, 1.90D0,
     1 1.80D0, 1.40D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0, 2.20D0,
     2 1.80D0, 2.00D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0, 1.40D0,
     3 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 2.00D0, 2.02D0,
     4 0.00D0, 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0,
     5 2.10D0, 2.06D0, 2.00D0, 2.60D0,46*0.D0/
C                                                                             
C       ORDERING                                                        REMOVE
C
C     DATA W2/ H.HHD0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, C.CCD0, N.NND0,  REMOVE
C    1 OxyOD0, F.FFD0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,  REMOVE
C    2 S.SSD0, Cl.0D0, 0.00D0, 0.00D0, 0.00D0, 7*0.D0, 0.00D0, 0.00D0,  REMOVE
C    3 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, Br.0D0, 0.00D0, 0.00D0,  REMOVE
C    4 0.00D0, 7*0.D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,  REMOVE
C    5 0.00D0, I.IID0, 0.00D0,46*0.D0/                                  REMOVE
C
C       EMPIRIC SURFACE TENSION RADII FOR SM5  --Bondi Radii            CCC1295
C                                                                       CCC0695
      DATA W5/ 1.20D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 1.70D0, 1.55D0,  CCC1295
     1 1.52D0, 1.47D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 1.80D0,  GDH0696
     2 1.80D0, 1.75D0, 0.00D0, 0.00D0, 0.00D0, 7*0.D0, 0.00D0, 0.00D0,  CCC0695
     3 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 1.85D0, 0.00D0, 0.00D0,  CCC0695
     4 0.00D0, 7*0.D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,  CCC0695
     5 0.00D0, 1.98D0, 0.00D0,46*0.D0/                                  CCC0695
C                                                                       GDH0297
C       EMPIRIC SURFACE TENSION RADII FOR SM5.0                         GDH0696
C                                                                       GDH0696
      DATA W50/ 1.20D0, 0.00D0, 0.00D0, 0.00D0,0.00D0, 1.70D0, 1.55D0,  GDH1196
     1 1.52D0, 1.47D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 1.80D0,  GDH1196
     2 1.80D0, 1.75D0, 0.00D0, 0.00D0, 0.00D0, 7*0.D0, 0.00D0, 0.00D0,  GDH1196
     3 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 1.85D0, 0.00D0, 0.00D0,  GDH1196
     4 0.00D0, 7*0.D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,  GDH1196
     5 0.00D0, 1.98D0, 0.00D0,46*0.D0/                                  GDH1196
C
C       EMPIRIC ION COULOMB RADII FOR SM5.05                            GDH0696
C                                                                       GDH0696
      DATA RHON50/ 2.25D0,0.00D0,0.00D0,0.00D0,0.00D0, 0.00D0, 0.00D0,  GDH1196
     1 1.70D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,  GDH1196
     2 2.07D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 7*0.D0, 0.00D0, 0.00D0,  GDH1196
     3 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,  GDH1196
     4 0.00D0, 7*0.D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,  GDH1196
     5 0.00D0, 0.00D0, 0.00D0,46*0.D0/                                  GDH1196
C                                                                       GDH0696
C   EMPIRIC SURFACE TENSION RADII FOR ALKANE SM4                        DJG1094
C                                                                       DJG1094
      DATA WHA/1.20D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0, 1.60D0, 1.55D0,  DJG1094
     1 1.52D0, 1.47D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0, 1.80D0,  DJG1094
     2 1.80D0, 1.75D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0, 1.40D0,  DJG1094
     3 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 1.85D0, 2.02D0, 0.00D0,  DJG1094
     4 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0, 2.10D0,  DJG1094
     5 2.06D0, 1.98D0, 2.60D0,46*0.D0/                                  DJG1094
C
C
C       VAN DER WAALS RADII A LA BONDI, JPC 68 (1964) 441.
C
      DATA W10/1.20D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0, 1.70D0, 1.55D0,
     1 1.52D0, 1.47D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0, 1.80D0,
     2 1.80D0, 1.75D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0, 1.40D0,
     3 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 1.85D0, 2.02D0, 0.00D0,
     4 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0, 2.10D0,
     5 2.06D0, 1.98D0, 2.60D0, 46*0.00D0/
C
C       EMPIRIC SURFACE TENSION RADII FOR SM2.1
C
      DATA W12/0.00D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0, 1.70D0, 1.90D0,
     1 1.80D0, 1.40D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0, 2.10D0,
     2 1.80D0, 2.00D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0, 1.40D0,
     3 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 2.00D0, 2.02D0, 0.00D0,
     4 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0, 2.10D0,
     5 2.06D0, 2.00D0, 2.60D0, 46*0.00D0/
C
C       EMPIRIC SURFACE TENSION RADII FOR SM3.1
C
      DATA W13/0.00D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0, 1.70D0, 1.90D0,
     1 1.80D0, 1.40D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0, 2.20D0,
     2 1.80D0, 2.00D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0, 1.40D0,
     3 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 2.00D0, 2.02D0,
     4 0.00D0, 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0,
     5 2.10D0, 2.06D0, 2.00D0, 2.60D0,46*0.D0/
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED WITH NO
C       ACCOUNT TAKEN OF CHEMICAL ENVIRONMENT (SM1)
C
      DATA SRFEN1/ 0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 14.95D0,
     1 -73.65D0, -52.74D0, 18.47D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     2   0.00D0,  10.67D0,-18.80D0, -2.14D0,  0.00D0,  0.00D0,  0.00D0,
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, -9.11D0,  0.00D0,
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     5  -8.21D0,   0.00D0, 46*0.D0/
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED WITH
C       ACCOUNT TAKEN OF CHEMICAL ENVIRONMENT (SM1A)
C
C       CURRENTLY: C(H), -nH, -oH, -sH,
C                  sp3/Amide-N, sp2/Arom/sp-N, sp3-O, sp2-O,
C                  F, S, Cl, Br, I, and P(H)  (total of 14 types)
C
       DATA SRFN1A/  4.15D0,  58.96D0, -23.39D0,  49.49D0, -368.97D0,
     1   -47.38D0,-109.70D0, -25.61D0,  21.17D0, -44.25D0,   -2.84D0,
     2    -8.93D0, -13.42D0,   3.95D0,  86*0.D0/
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR SM5.0 (For IONS)
C
C       CURRENTLY: H in NH4+, H in RNH3+, H in R2NH2+, H in R2NH+
C                  H in R3NH+, O in RCO2-, O in RCH2O- (7 types total)
C
       DATA SRFN50/ -453.89D0, -662.08D0, -1156.58D0, -1703.42D0,
     1   -2788.53D0, -584.91D0, -1269.91D0, 93*0.D0/
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM
C       EMPIRIC SURFACE TENSION TERMS (SM2)
C
      DATA SRFEN2/ 0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  3.36D0,
     1 -30.70D0, -25.01D0, 22.50D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     2   0.00D0,   3.90D0,-53.25D0, -2.28D0,  0.00D0,  0.00D0,  0.00D0,
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, -8.15D0,  0.00D0,
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     5 -15.18D0,   0.00D0, 46*0.D0/
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM
C       EMPIRIC SURFACE TENSION TERMS (SM3)
C
      DATA SRFEN3/ 0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  8.56D0,
     1 -42.20D0, -34.76D0, 20.02D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     2   0.00D0,   9.79D0,-53.00D0, -3.26D0,  0.00D0,  0.00D0,  0.00D0,
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, -4.61D0,  0.00D0,
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     5  -2.43D0,   0.00D0, 46*0.D0/
C   
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         CCC0695
C       EMPIRIC SURFACE TENSION TERMS (SM5.4U) WATER                    DJG0796
C                                                                       CCC0695
      DATA SSTS5/  20.0D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 62.35D0, CCC1295
     1  10.73D0, -89.15D0, 18.64D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0796
     2   0.00D0,  0.00D0, -40.17D0, -2.18D0,  0.00D0,  0.00D0,  0.00D0, CCC1295
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, -6.84D0,  0.00D0, CCC1295
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC1295
     5  -8.67D0,   0.00D0, 46*0.D0/                                     CCC1295
C                                                                       CCC1295
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         CCC0496
C       EMPIRIC SURFACE TENSION TERMS (SM5.4A) WATER                    DJG0796
C                                                                       CCC0496
      DATA SSTS5A/ 20.0D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 63.70D0, CCC0496
     1   3.74D0, -80.35D0, 18.89D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     2   0.00D0,  0.00D0, -43.28D0, -2.29D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, -6.84D0,  0.00D0, CCC0496
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     5  -7.96D0,   0.00D0, 46*0.D0/                                     CCC0496
C                                                                       CCC0496
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         CCC0496
C       EMPIRIC SURFACE TENSION TERMS (SM5.4P) WATER                    DJG0796
C                                                                       CCC0496
      DATA SSTS5P/ 20.0D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 59.37D0, CCC0496
     1  18.35D0, -97.46D0, 18.35D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     2   0.00D0,  0.00D0, -37.08D0, -1.91D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, -6.82D0,  0.00D0, CCC0496
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     5  -9.39D0,   0.00D0, 46*0.D0/                                     CCC0496
C   
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         DJG0896
C       EMPIRIC SURFACE TENSION TERMS (SM5.4U) CHLOROFORM               DJG0896
C                                                                       DJG0896
      DATA SSTS8/  20.0D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 62.35D0, DJG0896
     1  10.73D0, -89.15D0, 18.64D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0896
     2   0.00D0,  0.00D0, -40.17D0, -2.18D0,  0.00D0,  0.00D0,  0.00D0, DJG0896
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, -6.84D0,  0.00D0, DJG0896
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0896
     5  -8.67D0,   0.00D0, 46*0.D0/                                     DJG0896
C                                                                       DJG0996
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         DJG0996
C       EMPIRIC SURFACE TENSION TERMS (SM5.4A) CHLOROFORM               DJG0996
C                                                                       DJG0996
      DATA SSTS8A/ -75.23D0, 0.00D0,  0.00D0,  0.00D0,  0.00D0,-0.83D0, DJG0996
     1 -67.80D0,  -88.57D0,-46.09D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, DJG0996
     2   0.00D0,  0.00D0, -101.89D0,-68.54D0,  0.00D0,  0.00D0, 0.00D0, DJG0996
     3  10*0.D0,    0.00D0,  0.00D0,  0.00D0,  0.00D0,-72.84D0, 0.00D0, DJG0996
     4   0.00D0,    0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, DJG0996
     5 -77.29D0,    0.00D0, 46*0.D0/                                    DJG0996
C                                                                       DJG0996
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         DJG0996
C       EMPIRIC SURFACE TENSION TERMS (SM5.4P) CHLOROFORM               DJG0996
C                                                                       DJG0996
      DATA SSTS8P/-69.04D0, 0.00D0,  0.00D0,  0.00D0,  0.00D0, -1.95D0, DJG0996
     1 -48.74D0,  -94.77D0,-41.63D0, 0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0996
     2   0.00D0,   0.00D0, -92.88D0,-64.05D0, 0.00D0,  0.00D0,  0.00D0, DJG0996
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,-69.38D0,  0.00D0, DJG0996
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0996
     5  -73.54D0,  0.00D0, 46*0.D0/                                     DJG0996
C                                                                       DJG0297
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         DJG0297
C       EMPIRIC SURFACE TENSION TERMS (SM5.4A) BENZENE                  DJG0297
C                                                                       DJG0297
      DATA SSTB8A/ -78.10D0, 0.00D0,  0.00D0,  0.00D0,  0.00D0,-58.27D0,DJG0297
     1 -70.85D0,  -83.51D0,-63.30D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, DJG0297
     2   0.00D0,  0.00D0, -116.17D0,-86.30D0,  0.00D0,  0.00D0, 0.00D0, DJG0297
     3  10*0.D0,    0.00D0,  0.00D0,  0.00D0,  0.00D0,-91.72D0, 0.00D0, DJG0297
     4   0.00D0,    0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, DJG0297
     5 -96.77D0,    0.00D0, 46*0.D0/                                    DJG0297
C                                                                       DJG0297
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         DJG0297
C       EMPIRIC SURFACE TENSION TERMS (SM5.4P) BENZENE                  DJG0297
C                                                                       DJG0297
      DATA SSTB8P/-71.68D0, 0.00D0,  0.00D0,  0.00D0,  0.00D0, -55.68D0,DJG0297
     1 -54.20D0,  -89.37D0,-58.50D0, 0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     2   0.00D0,   0.00D0,-102.50D0,-81.38D0, 0.00D0,  0.00D0,  0.00D0, DJG0297
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,-88.39D0,  0.00D0, DJG0297
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     5  -92.91D0,  0.00D0, 46*0.D0/                                     DJG0297
C                                                                       DJG0297
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         DJG0297
C       EMPIRIC SURFACE TENSION TERMS (SM5.4A) BENZENE                  DJG0297
C                                                                       DJG0297
      DATA SSTT8A/ -77.84D0, 0.00D0,  0.00D0,  0.00D0,  0.00D0,-58.07D0,DJG0297
     1 -70.59D0,  -83.23D0,-63.08D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, DJG0297
     2   0.00D0,  0.00D0, -115.77D0,-86.01D0,  0.00D0,  0.00D0, 0.00D0, DJG0297
     3  10*0.D0,    0.00D0,  0.00D0,  0.00D0,  0.00D0,-91.41D0, 0.00D0, DJG0297
     4   0.00D0,    0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, DJG0297
     5 -96.45D0,    0.00D0, 46*0.D0/                                    DJG0297
C                                                                       DJG0297
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         DJG0297
C       EMPIRIC SURFACE TENSION TERMS (SM5.4P) BENZENE                  DJG0297
C                                                                       DJG0297
      DATA SSTT8P/-71.44D0, 0.00D0,  0.00D0,  0.00D0,  0.00D0, -55.50D0,DJG0297
     1 -53.99D0,  -89.07D0,-58.30D0, 0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     2   0.00D0,   0.00D0,-102.14D0,-81.11D0, 0.00D0,  0.00D0,  0.00D0, DJG0297
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,-88.09D0,  0.00D0, DJG0297
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     5  -92.60D0,  0.00D0, 46*0.D0/                                     DJG0297
C
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         GDH0496
C       EMPIRIC SURFACE TENSION TERMS (PD-SM5.4)                        GDH0496
C                                                                       GDH0496
      DATA STPD5/ 27.69D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 70.00D0, GDH0496
     1   8.23D0, -64.45D0, 17.51D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     2   0.00D0,  0.00D0, -45.88D0, -1.26D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, -5.65D0,  0.00D0, GDH0496
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     5  -8.51D0,   0.00D0, 46*0.D0/                                     GDH0496
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         GDH0496
C       EMPIRIC SURFACE TENSION TERMS (PD-SM5A)                         GDH0496
C                                                                       GDH0496
      DATA STPD5A/27.13D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 72.71D0, GDH0496
     1  -1.20D0, -53.89D0, 18.11D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     2   0.00D0,  0.00D0, -51.35D0, -1.41D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, -5.52D0,  0.00D0, GDH0496
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     5  -7.66D0,   0.00D0, 46*0.D0/                                     GDH0496
C                                                                       GDH0496
C                                                                       GDH0496
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         GDH0496
C       EMPIRIC SURFACE TENSION TERMS (PD-SM5P)                         GDH0496
C                                                                       GDH0496
      DATA STPD5P/25.95D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 65.68D0, GDH0496
     1  18.08D0, -75.53D0, 16.82D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     2   0.00D0,  0.00D0, -40.92D0, -1.08D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, -5.75D0,  0.00D0, GDH0496
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     5  -9.40D0,   0.00D0, 46*0.D0/                                     GDH0496
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         GDH0596
C       EMPIRIC SURFACE TENSION TERMS (SM5/A-C2-PD)                     GDH0596
C                                                                       GDH0596
      DATA SPD52A/26.54D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 40.15D0, GDH0596
     1 -32.74D0, -77.54D0, 16.17D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0596
     2   0.00D0, -37.36D0, -63.97D0, -4.51D0, 0.00D0,  0.00D0,  0.00D0, GDH0596
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,-10.78D0,  0.00D0, GDH0596
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0596
     5 -12.80D0,   0.00D0, 46*0.D0/                                     GDH0596
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         GDH0596
C       EMPIRIC SURFACE TENSION TERMS (SM5/P-C2-PD)                     GDH0596
C                                                                       GDH0596
      DATA SPD52P/26.65D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 48.75D0, GDH0596
     1  -26.87D0,-102.92D0, 17.36D0,  0.00D0, 0.00D0,  0.00D0,  0.00D0, GDH0596
     2   0.00D0,  -8.27D0, -60.26D0, -5.00D0, 0.00D0,  0.00D0,  0.00D0, GDH0596
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, -7.29D0,  0.00D0, GDH0596
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0596
     5  -9.45D0,   0.00D0, 46*0.D0/                                     GDH0596
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         GDH0596
C       EMPIRIC SURFACE TENSION TERMS (SM5.0)                           GDH0596
C                                                                       GDH0596
      DATA SPD50/ 98.22D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 89.21D0, GDH1196
     1 -277.00D0,-376.46D0, 29.85D0, 0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1196
     2   0.00D0,   0.00D0,-163.04D0, 73.52D0, 0.00D0,  0.00D0,  0.00D0, GDH1196
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, 70.20D0,  0.00D0, GDH1196
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1196
     5 -41.64D0 ,   0.00D0, 46*0.D0/                                    GDH1196
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         DJG1094
C       EMPIRIC SURFACE TENSION TERMS FOR SM4 ALKANES                   DJG1094
C                                                                       DJG1094
      DATA SRFNHA/ 0.00D0,-40.31D0,  0.00D0,  0.00D0,  0.00D0,-76.80D0, DJG1094
     1 -36.29D0, -39.18D0,-37.42D0,-41.55D0,  0.00D0,  0.00D0,  0.00D0, DJG1094
     2   0.00D0,   0.00D0,-59.77D0,-55.35D0,-47.66D0,  0.00D0,  0.00D0, DJG1094
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,-59.79D0,  0.00D0, DJG1094
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG1094
     5 -62.58D0,   0.00D0, 46*0.D0/                                     DJG1094
C
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED WITH NO
C       ACCOUNT TAKEN OF CHEMICAL ENVIRONMENT (SM1.1)
C
      DATA SRFN11/ 0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     1  14.95D0, -73.65D0,-52.74D0, 18.47D0,  0.00D0,  0.00D0,
     2   0.00D0,   0.00D0,  0.00D0, 10.67D0,-18.80D0, -2.14D0, 0.00D0,
     3   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,
     4   0.00D0,  -9.11D0,  0.00D0,  0.00D0,  0.00D0, 10*0.D0,
     5   0.00D0,   0.00D0,  0.00D0,  0.00D0, -8.21D0,  0.00D0,
     6  46*0.D0/
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED WITH
C       ACCOUNT TAKEN OF CHEMICAL ENVIRONMENT (SM1A.1)
C
C       CURRENTLY: C(H), -nH, -oH, -sH,
C                  sp3/Amide-N, sp2/Arom/sp-N, sp3-O, sp2-O,
C                  F, S, Cl, Br, I, and P(H)  (total of 14 types)
C
       DATA SRN11A/  4.15D0,  58.96D0,  -23.39D0,  49.49D0, -368.97D0,
     1 -47.38D0,  -109.70D0, -25.61D0,   21.17D0, -44.25D0,   -2.84D0,
     2  -8.93D0,   -13.42D0,   3.95D0, 86*0.00D0/
C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM
C       EMPIRIC SURFACE TENSION TERMS (SM2.1)
C
      DATA SRFN12/ 0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     1   3.35D0, -30.14D0,-24.97D0, 26.54D0,  0.00D0,  0.00D0,
     2   0.00D0,   0.00D0,  0.00D0,  3.97D0,-52.53D0, -1.72D0,
     3   0.00D0,   0.00D0,  0.00D0, 10*0.D0,  0.00D0,  0.00D0,
     4   0.00D0,   0.00D0, -7.26D0,  0.00D0,  0.00D0,  0.00D0,
     5  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,
     6 -14.53D0,   0.00D0, 46*0.D0/
C                                                                       GDH0895
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         GDH0895
C       EMPIRIC SURFACE TENSION TERMS (SM2.2)                           GDH0895
C                                                                       GDH0895
      DATA SRFN22/ 0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,          GDH0895
     1  13.04D0, -30.58D0,-25.95D0,  0.00D0,  0.00D0,  0.00D0,          GDH0895
     2   0.00D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,          GDH0895
     3   0.00D0,   0.00D0,  0.00D0, 10*0.D0,  0.00D0,  0.00D0,          GDH0895
     4   0.00D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,          GDH0895
     5  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,                   GDH0895
     6   0.00D0,   0.00D0, 46*0.D0/                                     GDH0895
C                                                                       GDH0496
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM         GDH0496
C       EMPIRIC SURFACE TENSION TERMS (PD-SM2)                          GDH0496
C                                                                       GDH0496
      DATA SRFN23/ 0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,          GDH0496
     1   5.08D0, -31.56D0,  8.94D0, 18.11D0,  0.00D0,  0.00D0,          GDH0496
     2   0.00D0,   0.00D0,  0.00D0,  3.67D0,-62.82D0, -2.85D0,          GDH0496
     3   0.00D0,   0.00D0,  0.00D0, 10*0.D0,  0.00D0,  0.00D0,          GDH0496
     4   0.00D0,   0.00D0, -9.24D0,  0.00D0,  0.00D0,  0.00D0,          GDH0496
     5  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,                   GDH0496
     6  -11.96D0,   0.00D0, 46*0.D0/                                    GDH0496


C
C       SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM
C       EMPIRIC SURFACE TENSION TERMS (SM3.1)
C
      DATA SRFN13/ 0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     1   4.45D0, -43.67D0,-36.43D0, 22.11D0,  0.00D0,  0.00D0,
     2   0.00D0,   0.00D0,  0.00D0,  5.39D0,-53.57D0, -2.67D0,
     3   0.00D0,   0.00D0,  0.00D0, 10*0.D0,  0.00D0,  0.00D0,
     4   0.00D0,   0.00D0, -4.76D0,  0.00D0,  0.00D0,  0.00D0,
     5  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,
     6   0.41D0,   0.00D0, 46*0.D0/
C
C       SIGMA CORRECTION B(H,k) PER BONDED HYDROGEN AS INDICATED
C       BY THE BOND ORDER MATRIX (SM2)
C
       DATA BK2/ 0.00D0, 0.00D0, 0.00D0,  0.00D0, 0.00D0,  3.24D0,
     1   -19.20D0,-37.28D0,  0.00D0, 0.00D0, 0.00D0,  0.00D0,
     2   0.00D0,  0.00D0,  0.00D0, 37.03D0,  0.00D0,  0.00D0,
     3   0.00D0,  0.00D0, 10*0.00D0, 0.00D0,  0.00D0,  0.00D0,
     4   0.00D0,  0.00D0 , 0.00D0, 0.00D0,  0.00D0, 10*0.00D0,
     5   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0 , 0.00D0,
     6   46*0.00D0/
C
C       SIGMA CORRECTION B(H,k) PER BONDED HYDROGEN AS INDICATED
C       BY THE BOND ORDER MATRIX (SM3)
C
       DATA BK3/ 0.00D0, 0.00D0, 0.00D0,  0.00D0, 0.00D0, -0.39D0,
     1   -24.25D0,-22.82D0,  0.00D0, 0.00D0, 0.00D0,  0.00D0,
     2   0.00D0,  0.00D0,  0.00D0, 36.40D0,  0.00D0,  0.00D0,
     3   0.00D0,  0.00D0, 10*0.00D0, 0.00D0,  0.00D0,  0.00D0,
     4   0.00D0,  0.00D0 , 0.00D0, 0.00D0,  0.00D0, 10*0.00D0,
     5   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0 , 0.00D0,
     6   46*0.00D0/
C
C
C       SIGMA CORRECTION B(H,k) PER BONDED HYDROGEN AS INDICATED
C       BY THE BOND ORDER MATRIX (SM2.1)
C
       DATA BK12/ 0.00D0, 0.00D0, 0.00D0,  0.00D0, 0.00D0,  3.24D0,     GDH1093
     1   -19.20D0,-37.28D0,  0.00D0, 0.00D0, 0.00D0,  0.00D0,           GDH1093
     2   0.00D0,  0.00D0,  0.00D0, 37.03D0,  0.00D0,  0.00D0,           GDH1093
     3   0.00D0,  0.00D0, 10*0.00D0, 0.00D0,  0.00D0,  0.00D0,          GDH1093
     4   0.00D0,  0.00D0 , 0.00D0, 0.00D0,  0.00D0, 10*0.00D0,          GDH1093
     5   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0 , 0.00D0,            GDH1093
     6   46*0.00D0/                                                     GDH1093
C
C       SIGMA CORRECTION B(H,k) PER BONDED HYDROGEN AS INDICATED
C       BY THE BOND ORDER MATRIX (SM3.1)
C
       DATA BK13/ 0.00D0, 0.00D0, 0.00D0,  0.00D0, 0.00D0,  2.70D0,     GDH1093
     1   -25.36D0,-20.78D0,  0.00D0, 0.00D0, 0.00D0,  0.00D0,           GDH1093
     2   0.00D0,  0.00D0,  0.00D0, 35.42D0,  0.00D0,  0.00D0,           GDH1093
     3   0.00D0,  0.00D0, 10*0.00D0, 0.00D0,  0.00D0,  0.00D0,          GDH1093
     4   0.00D0,  0.00D0 , 0.00D0, 0.00D0,  0.00D0, 10*0.00D0,          GDH1093
     5   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0 , 0.00D0,            GDH1093
     6   46*0.00D0/                                                     GDH1093
C                                                                       GDH0895
C                                                                       GDH0895
C       SIGMA CORRECTION B(H,k) PER BONDED HYDROGEN AS INDICATED        GDH0895
C       BY THE BOND ORDER MATRIX (SM2.2)                                GDH0895
C                                                                       GDH0895
       DATA BK22/ 0.00D0, 0.00D0, 0.00D0,  0.00D0, 0.00D0, -5.53D0,     GDH0895
     1   -16.59D0,-35.88D0,  0.00D0, 0.00D0, 0.00D0,  0.00D0,           GDH0895
     2   0.00D0,  0.00D0,  0.00D0, 37.03D0,  0.00D0,  0.00D0,           GDH0895
     3   0.00D0,  0.00D0, 10*0.00D0, 0.00D0,  0.00D0,  0.00D0,          GDH0895
     4   0.00D0,  0.00D0 , 0.00D0, 0.00D0,  0.00D0, 10*0.00D0,          GDH0895
     5   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0 , 0.00D0,            GDH0895
     6   46*0.00D0/                                                     GDH0895
C                                                                       GDH0296
C       SIGMA CORRECTION B(H,k) PER BONDED HYDROGEN AS INDICATED        GDH0895
C       BY THE BOND ORDER MATRIX (PD-SM2)                               GDH0895
C                                                                       GDH0496
      DATA BK23/ 0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,            GDH0496
     1   3.90D0,  -5.39D0,-59.00D0,  0.00D0,  0.00D0,  0.00D0,          GDH0496
     2   0.00D0,   0.00D0,  0.00D0,  0.00D0, 42.35D0,  0.00D0,          GDH0496
     3   0.00D0,   0.00D0,  0.00D0, 10*0.D0,  0.00D0,  0.00D0,          GDH0496
     4   0.00D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,          GDH0496
     5  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,                   GDH0496
     6   0.00D0,   0.00D0, 46*0.D0/                                     GDH0496
C
C                                                                       GDH0296
C                                                                       GDH0296
C       SPECIALIZED SURFACE TENSION CORRECTIONS SM2                     GDH0296
C       1 a SPECIALIZED CORRECTION FACTOR FOR NITROGEN                  GDH0296
C       2 b SPECIALIZED CORRECTION FACTOR FOR NITROGEN                  GDH0296
C       3 c SPECIALIZED CORRECTION FACTOR FOR NITROGEN                  GDH0296
C       4 w SPECIALIZED CORRECTION FACTOR FOR NITROGEN                  GDH0296
C       5 a SPECIALIZED CORRECTION FACTOR FOR OXYGEN                    GDH0296
C       6 b SPECIALIZED CORRECTION FACTOR FOR OXYGEN                    GDH0296
C       7 c SPECIALIZED CORRECTION FACTOR FOR OXYGEN                    GDH0296
C       8 w SPECIALIZED CORRECTION FACTOR FOR OXYGEN                    GDH0296
C       9 FACTOR FOR SS SM5-LIKE SURFACE TENSION FOR SS BONDS           GDH0296
C                                                                       GDH0296
      DATA SPST2/   -9.12D0,   1.35D0,  3.69D0,  1.50D0,                GDH0296
     1   -1.94D0,  0.46D0,  2.56D0, 1.0D0, 0.0D0, 0.0D0/                GDH0296
C                                                                       GDH0296
C       SPECIALIZED SURFACE TENSION CORRECTIONS SM3                     GDH0296
C       1 a SPECIALIZED CORRECTION FACTOR FOR NITROGEN                  GDH0296
C       2 b SPECIALIZED CORRECTION FACTOR FOR NITROGEN                  GDH0296
C       3 c SPECIALIZED CORRECTION FACTOR FOR NITROGEN                  GDH0296
C       4 w SPECIALIZED CORRECTION FACTOR FOR NITROGEN                  GDH0296
C       5 a SPECIALIZED CORRECTION FACTOR FOR OXYGEN                    GDH0296
C       6 b SPECIALIZED CORRECTION FACTOR FOR OXYGEN                    GDH0296
C       7 c SPECIALIZED CORRECTION FACTOR FOR OXYGEN                    GDH0296
C       8 w SPECIALIZED CORRECTION FACTOR FOR OXYGEN                    GDH0296
C       9 FACTOR FOR SS SM5-LIKE SURFACE TENSION FOR SS BONDS           GDH0296
C                                                                       GDH0296
      DATA SPST3/  -4.93D0,   1.00D0,  3.00D0,  0.50D0,                 GDH0296
     1   -2.62D0,  0.43D0, 2.66D0, 1.2D0,  0.00D0, 0.0D0/               GDH0296
C                                                                       GDH0296
C       SPECIALIZED SURFACE TENSION CORRECTIONS PD-SM2                  GDH0296
C       1 a SPECIALIZED CORRECTION FACTOR FOR NITROGEN                  GDH0296
C       2 b SPECIALIZED CORRECTION FACTOR FOR NITROGEN                  GDH0296
C       3 c SPECIALIZED CORRECTION FACTOR FOR NITROGEN                  GDH0296
C       4 w SPECIALIZED CORRECTION FACTOR FOR NITROGEN                  GDH0296
C       5 a SPECIALIZED CORRECTION FACTOR FOR OXYGEN                    GDH0296
C       6 b SPECIALIZED CORRECTION FACTOR FOR OXYGEN                    GDH0296
C       7 c SPECIALIZED CORRECTION FACTOR FOR OXYGEN                    GDH0296
C       8 w SPECIALIZED CORRECTION FACTOR FOR OXYGEN                    GDH0296
C       9 FACTOR FOR SS SM5-LIKE SURFACE TENSION FOR SS BONDS           GDH0296
C                                                                       GDH0296
      DATA SPST23/ -9.12D0,   1.35D0,  3.69D0,  1.50D0,                 GDH0496
     1   -1.32D0,  0.46D0,  2.56D0, 1.0D0, 35.89D0, 0.0D0/              GDH0496
C                                                                       CCC0695
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED CCC0695
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5.4U).  DEPENDANT ON BOND DJG0796
C       LENGTH TO ATOM X. SOLVENT=WATER                                 CCC0695
C                                                                       CCC1295
      DATA HRFN5/  0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, -20.98D0, CCC0496
     1 -56.50D0,  50.61D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0796
     2   0.00D0,   0.00D0, 39.18D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC1295
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC1295
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC1295
     5   0.00D0,   0.00D0, 46*0.D0/                                     CCC1295
C                                                                       CCC0496
C                                                                       CCC0496
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED CCC0496
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5.4A).  DEPENDANT ON BOND DJG0796
C       LENGTH TO ATOM X. SOLVENT=WATER                                 CCC0496
C                                                                       CCC0496
      DATA HRFN5A/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, -21.21D0, CCC0496
     1 -59.05D0,  38.66D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     2   0.00D0,   0.00D0, 50.22D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     5   0.00D0,   0.00D0, 46*0.D0/                                     CCC0496
C                                                                       CCC0496
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED CCC0496
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5.4P).  DEPENDANT ON BOND DJG0796
C       LENGTH TO ATOM X. SOLVENT=WATER                                 CCC0496
C                                                                       CCC0496
      DATA HRFN5P/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, -20.72D0, CCC0496
     1 -50.76D0,  61.54D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     2   0.00D0,   0.00D0, 28.15D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, CCC0496
     5   0.00D0,   0.00D0, 46*0.D0/                                     CCC0496
C                                                                       DJG0896
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED DJG0896
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5.4U).  DEPENDANT ON BOND DJG0896
C       LENGTH TO ATOM X. SOLVENT=CHLOROFORM                            DJG0896
C                                                                       DJG0896
      DATA HRFN8/  0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, -20.98D0, DJG0896
     1 -56.50D0,  50.61D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0896
     2   0.00D0,   0.00D0, 39.18D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0896
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0896
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0896
     5   0.00D0,   0.00D0, 46*0.D0/                                     DJG0896
C                                                                       DJG0896
C                                                                       DJG0896
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED DJG0896
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5.4A).  DEPENDANT ON BOND DJG0896
C       LENGTH TO ATOM X. SOLVENT=CHLOROFORM                            DJG0896
C                                                                       DJG0896
      DATA HRFN8A/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0,  16.38D0, DJG0996
     1  10.32D0,  83.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0996
     2   0.00D0,   0.00D0, 81.08D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0996
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0996
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0996
     5   0.00D0,   0.00D0, 46*0.D0/                                     DJG0996
C                                                                       DJG0896
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED DJG0896
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5.4P).  DEPENDANT ON BOND DJG0896
C       LENGTH TO ATOM X. SOLVENT=CHLOROFORM                            DJG0896
C                                                                       DJG0896
      DATA HRFN8P/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0,  14.14D0, DJG0996
     1  10.28D0,  92.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0996
     2   0.00D0,   0.00D0, 64.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0996
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0996
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0996
     5   0.00D0,   0.00D0, 46*0.D0/                                     DJG0996
C                                                                       DJG0297
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED DJG0297
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5.4A).  DEPENDANT ON BOND DJG0297
C       LENGTH TO ATOM X. SOLVENT=CHLOROFORM                            DJG0297
C                                                                       DJG0297
      DATA HRFB8A/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0,   1.14D0, DJG0297
     1 -14.51D0,  24.40D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     2   0.00D0,   0.00D0, 53.80D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     5   0.00D0,   0.00D0, 46*0.D0/                                     DJG0297
C                                                                       DJG0297
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED DJG0297
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5.4P).  DEPENDANT ON BOND DJG0297
C       LENGTH TO ATOM X. SOLVENT=CHLOROFORM                            DJG0297
C                                                                       DJG0297
      DATA HRFB8P/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0,  -1.20D0, DJG0297
     1 -15.28D0,  34.03D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     2   0.00D0,   0.00D0, 30.45D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     5   0.00D0,   0.00D0, 46*0.D0/                                     DJG0297
C                                                                       DJG0297
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED DJG0297
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5.4A).  DEPENDANT ON BOND DJG0297
C       LENGTH TO ATOM X. SOLVENT=CHLOROFORM                            DJG0297
C                                                                       DJG0297
      DATA HRFT8A/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0,   1.14D0, DJG0297
     1 -14.47D0,  24.30D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     2   0.00D0,   0.00D0, 53.62D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     5   0.00D0,   0.00D0, 46*0.D0/                                     DJG0297
C                                                                       DJG0297
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED DJG0297
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5.4P).  DEPENDANT ON BOND DJG0297
C       LENGTH TO ATOM X. SOLVENT=CHLOROFORM                            DJG0297
C                                                                       DJG0297
      DATA HRFT8P/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0,  -1.19D0, DJG0297
     1 -15.25D0,  33.90D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     2   0.00D0,   0.00D0, 30.33D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG0297
     5   0.00D0,   0.00D0, 46*0.D0/                                     DJG0297
C                                                                       DJG0896
C                                                                       GDH0496
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED GDH0496
C       FROM EMPIRIC SURFACE TENSION TERMS (PD-SM5). DEPENDANT ON BOND  GDH0496
C       LENGTH TO ATOM X. SOLVENT=WATER                                 GDH0496
C                                                                       GDH0496
      DATA HFPD5/ 0.00D0,   0.00D0,  0.00D0,  0.00D0, 0.00D0, -30.16D0, GDH0496
     1 -62.24D0, -18.88D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     2   0.00D0,   0.00D0, 37.63D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     5   0.00D0,   0.00D0, 46*0.D0/                                     GDH0496
C                                                                       GDH0496
C                                                                       GDH0496
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED GDH0496
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5.0). DEPENDANT ON BOND   GDH0496
C       LENGTH TO ATOM X. SOLVENT=WATER                                 GDH0496
C                                                                       GDH0496
      DATA HRFN50/0.00D0,   0.00D0,  0.00D0,  0.00D0, 0.00D0,-126.78D0, GDH1196
     1 -212.04D0,-248.86D0, 0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1196
     2   0.00D0,   0.00D0,171.42D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1196
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1196
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1196
     5   0.00D0,   0.00D0, 46*0.D0/                                     GDH1196
C                                                                       GDH0496
C                                                                       GDH0496
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED GDH0496
C       FROM EMPIRIC SURFACE TENSION TERMS (PD-SM5A). DEPENDANT ON BOND GDH0496
C       LENGTH TO ATOM X. SOLVENT=WATER                                 GDH0496
C                                                                       GDH0496
      DATA HFPD5A/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, -29.50D0, GDH0496
     1 -61.13D0, -35.12D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     2   0.00D0,   0.00D0, 53.50D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     5   0.00D0,   0.00D0, 46*0.D0/                                     GDH0496
C                                                                       GDH0496
C                                                                       GDH0496
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED GDH0496
C       FROM EMPIRIC SURFACE TENSION TERMS (PD-SM5P). DEPENDANT ON BOND GDH0496
C       LENGTH TO ATOM X. SOLVENT=WATER                                 GDH0496
C                                                                       GDH0496
      DATA HFPD5P/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, -27.71D0, GDH0496
     1 -59.59D0,  0.16D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0496
     2   0.00D0,   0.00D0, 25.69D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0496
     5   0.00D0,   0.00D0, 46*0.D0/                                     GDH0496
C                                                                       GDH0596
C                                                                       GDH0596
C                                                                       GDH0596
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED GDH0596
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5/A2PD).DEPENDANT ON BOND GDH0596
C       LENGTH TO ATOM X. SOLVENT=WATER                                 GDH0596
C                                                                       GDH0596
      DATA HPD52A/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, -25.51D0, GDH0596
     1 -82.74D0, -92.13D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0596
     2   0.00D0,   0.00D0, 56.87D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0596
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0596
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0596
     5   0.00D0,   0.00D0, 46*0.D0/                                     GDH0596
C                                                                       GDH0596
C                                                                       GDH0596
C                                                                       GDH0596
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED GDH0596
C       FROM EMPIRIC SURFACE TENSION TERMS (SM5/P2PD).DEPENDANT ON BOND GDH0596
C       LENGTH TO ATOM X. SOLVENT=WATER                                 GDH0596
C                                                                       GDH0596
      DATA HPD52P/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0, -28.30D0, GDH0596
     1 -76.00D0, -59.54D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0596
     2   0.00D0,   0.00D0, 52.44D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0596
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0596
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0596
     5   0.00D0,   0.00D0, 46*0.D0/                                     GDH0596
C                                                                       GDH0596
C
C                                                                       GDH0496
C       SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED DJG1094
C       FROM EMPIRIC SURFACE TENSION TERMS (SM4).  DEPENDANT ON BOND    DJG1094
C       ORDER TO ATOM X.  ALKANE                                        DJG1094
C                                                                       DJG1094
      DATA HRFNH4/ 0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,-50.21D0, DJG1094
     1 -56.29D0, -56.13D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG1094
     2   0.00D0,   0.00D0,-54.64D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG1094
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG1094
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, DJG1094
     5   0.00D0,   0.00D0, 46*0.D0/                                     DJG1094
C                                                                       GDH0895
C                                                                       GDH0895
C       PAIRWISE DESCREENING APPROXIMATION PARAMETERS FOR SM2.2         GDH0895
C       1. H-H,   2. H-C,   3. H-O,   4. C-H,   5. O-H,   6. C-ABH      GDH0496
C       7. 0-ABH, 8. H-N,   9. N-H,  10. N-ABH,11. H-S,  12. S-H        GDH0496
C      13. S-ABH,14. H-P,  15. P-H,  16. P-ABH,17. H-F,  18. F-H        GDH0496
C      19. H-Cl  20. Cl-H, 21. H-Br  22. Br-H, 23. H-I,  24. I-H        GDH0496
C      25. F-ABH,26.Cl-ABH,27.Br-ABH,28. I-ABH                          GDH0496
C                                                                       GDH0895
       DATA SM22PD/ 0.82D0, 0.82D0, 0.82D0, 0.70D0, 0.54D0, 0.70D0,     GDH0496
     2   0.54D0, 0.82D0,   0.66D0,   0.66D0,   0.00D0,   0.00D0,        GDH0496
     3   0.00D0, 0.00D0,   0.00D0,   0.00D0,   0.00D0,   0.00D0,        GDH0496
     4   0.00D0, 0.00D0,   0.00D0,   0.00D0,   0.00D0,   0.00D0,        GDH0496
     5   0.00D0, 0.00D0,   0.00D0,   0.00D0,   0.00D0,   0.00D0,        GDH0496
     6   70*0.D0/                                                       GDH0496
C                                                                       GDH0895
C       PAIRWISE DESCREENING APPR0XIMATION PARAMETERS FOR PD-SM2        GDH0895
C       INSERTED IN THE FORM: WHERE THE FIRST ATOM TYPE LISTED IS       GDH0496
C       THE ATOM WHICH IS DESCREENING AND THE SECOND IS THE ATOM        GDH0496
C       DESCREENED (ABH MEANS ALL ATOM TYPES EXCEPT H)                  GDH0496
C       1. H-H,   2. H-C,   3. H-O,   4. C-H,   5. O-H,   6. C-ABH      GDH0496
C       7. 0-ABH, 8. H-N,   9. N-H,  10. N-ABH,11. H-S,  12. S-H        GDH0496
C      13. S-ABH,14. H-P,  15. P-H,  16. P-ABH,17. H-F,  18. F-H        GDH0496
C      19. H-Cl  20. Cl-H, 21. H-Br  22. Br-H, 23. H-I,  24. I-H        GDH0496
C      25. F-ABH,26.Cl-ABH,27.Br-ABH,28. I-ABH                          GDH0496
       DATA SM23PD/0.50D0, 0.91D0, 0.50D0, 0.77D0, 0.91D0, 0.50D0,      GDH0496
     2   0.96D0, 0.96D0,   0.88D0,   0.50D0,   0.91D0,   1.10D0,        GDH0496
     3   1.10D0, 1.00D0,   0.64D0,   0.50D0,   0.97D0,   0.67D0,        GDH0496
     4   0.50D0, 1.00D0,   0.50D0,   0.87D0,   1.00D0,   0.96D0,        GDH0496
     5   1.00D0, 1.00D0,   1.00D0,   1.00D0,   0.00D0,   0.00D0,        GDH0496
     6   70*0.D0/                                                       GDH0496
C                                                                       GDH0895
C                                                                       GDH0895
C       PAIRWISE DESCREENING APPROXIMATION PARAMETERS FOR PD-SM5        GDH0895
C       PD-SM5A and PD-SM5P                                             GDH0496
C       INSERTED IN THE FORM: WHERE THE FIRST ATOM TYPE LISTED IS       GDH0496
C       THE ATOM WHICH IS DESCREENING AND THE SECOND IS THE ATOM        GDH0496
C       DESCREENED (ABH MEANS ALL ATOM TYPES EXCEPT H)                  GDH0496
C       1. H-H,   2. H-C,   3. H-O,   4. C-H,   5. O-H,   6. C-ABH      GDH0496
C       7. 0-ABH, 8. H-N,   9. N-H,  10. N-ABH,11. H-S,  12. S-H        GDH0496
C      13. S-ABH,14. H-P,  15. P-H,  16. P-ABH,17. H-F,  18. F-H        GDH0496
C      19. H-Cl  20. Cl-H, 21. H-Br  22. Br-H, 23. H-I,  24. I-H        GDH0496
C      25. F-ABH,26.Cl-ABH,27.Br-ABH,28. I-ABH                          GDH0496
       DATA SM5PD/0.84D0, 1.00D0, 0.50D0, 0.76D0, 0.50D0, 0.67D0,       GDH0496
     2   1.00D0, 0.81D0,   0.75D0,   0.52D0,   0.50D0,   0.78D0,        GDH0496
     3   1.00D0, 0.00D0,   0.00D0,   0.00D0,   0.50D0,   0.65D0,        GDH0496
     4   0.50D0, 0.76D0,   1.00D0,   0.59D0,   1.00D0,   0.63D0,        GDH0496
     5   1.00D0, 1.00D0,   0.86D0,   1.00D0,   0.00D0,   0.00D0,        GDH0496
     6   70*0.D0/                                                       GDH0496
C                                                                       GDH0895
C       PAIRWISE DESCREENING APPROXIMATION PARAMETERS FOR SM5/A2PD      GDH0895
C       INSERTED IN THE FORM: WHERE THE FIRST ATOM TYPE LISTED IS       GDH0596
C       THE ATOM WHICH IS DESCREENING AND THE SECOND IS THE ATOM        GDH0596
C       DESCREENED (ABH MEANS ALL ATOM TYPES EXCEPT H)                  GDH0596
C       1. H-H,   2. H-C,   3. H-O,   4. C-H,   5. O-H,   6. C-ABH      GDH0596
C       7. 0-ABH, 8. H-N,   9. N-H,  10. N-ABH,11. H-S,  12. S-H        GDH0596
C      13. S-ABH,14. H-P,  15. P-H,  16. P-ABH,17. H-F,  18. F-H        GDH0596
C      19. H-Cl  20. Cl-H, 21. H-Br  22. Br-H, 23. H-I,  24. I-H        GDH0596
C      25. F-ABH,26.Cl-ABH,27.Br-ABH,28. I-ABH                          GDH0596
       DATA SM5A2P/0.50D0, 1.00D0, 0.50D0, 0.77D0, 1.00D0, 0.59D0,      GDH0596
     2   1.00D0, 0.50D0,   1.00D0,   0.50D0,   0.50D0,   1.10D0,        GDH0596
     3   1.10D0, 1.00D0,   0.98D0,   0.50D0,   0.50D0,   0.66D0,        GDH0596
     4   0.50D0, 0.97D0,   0.50D0,   0.82D0,   0.50D0,   1.00D0,        GDH0596
     5   0.50D0, 1.00D0,   1.00D0,   1.00D0,   0.00D0,   0.00D0,        GDH0596
     6   70*0.D0/                                                       GDH0596
C                                                                       GDH0895
C       PAIRWISE DESCREENING APPROXIMATION PARAMETERS FOR SM5/P2PD      GDH0895
C       INSERTED IN THE FORM: WHERE THE FIRST ATOM TYPE LISTED IS       GDH0596
C       THE ATOM WHICH IS DESCREENING AND THE SECOND IS THE ATOM        GDH0596
C       DESCREENED (ABH MEANS ALL ATOM TYPES EXCEPT H)                  GDH0596
C       1. H-H,   2. H-C,   3. H-O,   4. C-H,   5. O-H,   6. C-ABH      GDH0596
C       7. 0-ABH, 8. H-N,   9. N-H,  10. N-ABH,11. H-S,  12. S-H        GDH0596
C      13. S-ABH,14. H-P,  15. P-H,  16. P-ABH,17. H-F,  18. F-H        GDH0596
C      19. H-Cl  20. Cl-H, 21. H-Br  22. Br-H, 23. H-I,  24. I-H        GDH0596
C      25. F-ABH,26.Cl-ABH,27.Br-ABH,28. I-ABH                          GDH0596
       DATA SM5P2P/0.71D0, 1.00D0, 0.50D0, 0.72D0, 0.50D0, 0.50D0,      GDH0596
     2   1.00D0, 1.00D0,   0.90D0,   0.50D0,   0.50D0,   1.10D0,        GDH0596
     3   1.10D0, 0.50D0,   0.61D0,   0.50D0,   0.50D0,   0.50D0,        GDH0596
     4   0.50D0, 0.50D0,   0.50D0,   0.50D0,   1.00D0,   0.50D0,        GDH0596
     5   0.50D0, 1.00D0,   1.00D0,   1.00D0,   0.00D0,   0.00D0,        GDH0596
     6   70*0.D0/                                                       GDH0596

C                                                                       GDH0895
C                                                                       GDH0895
C
C       COULOMB INTEGRAL BASES (PSEUDO RADII) FOR GENERATION OF BORN
C       RADII AND INTERACTIONS. (see below) Data are Pk(0) for SM'#'
C
      DATA PK01/ 0.57D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0, 1.68D0,
     1   1.40D0, 1.46D0, 1.37D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0,
     2   1.30D0, 1.30D0, 1.65D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0,
     3   1.40D0, 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 1.75D0, 2.02D0,
     4   0.00D0, 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0,
     5   2.10D0, 2.06D0, 1.88D0, 2.60D0,46*0.D0/
C
      DATA PK02/ 0.59D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0, 1.68D0,
     1   1.50D0, 1.46D0, 1.37D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0,
     2   1.30D0, 1.30D0, 1.65D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0,
     3   1.40D0, 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 1.75D0, 2.02D0,
     4   0.00D0, 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0,
     5   2.10D0, 2.06D0, 1.88D0, 2.60D0,46*0.D0/
C
      DATA PK03/ 0.59D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0, 1.58D0,
     1   1.60D0, 1.66D0, 1.37D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0,
     2   1.40D0, 1.20D0, 1.65D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0,
     3   1.40D0, 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 1.75D0, 2.02D0,
     4   0.00D0, 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0,
     5   2.10D0, 2.06D0, 1.88D0, 2.60D0,46*0.D0/
C                                                                       CCC0695
C    SM5 WATER rho(0)                                                   CCC1295
C                                                                       CCC1295
      DATA PK05/ 1.28D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 1.78D0,        CCC1295
     1  1.92D0, 1.60D0, 1.50D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, CCC1295
     2  2.40D0, 1.92D0, 2.13D0, 0.00D0, 0.00D0, 0.00D0, 7*0.D0, 0.00D0, CCC1295
     3  0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.31D0, 0.00D0, CCC1295
     4  0.00D0, 0.00D0, 7*0.D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, CCC1295
     5  0.00D0, 0.00D0, 2.66D0, 0.00D0,46*0.D0/                         CCC1295
C                                                                       CCC0695
C      ALKANE                                                           DJG1094
C                                                                       DJG1094
      DATA PK0HA/0.59D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0, 1.78D0,        DJG1094
     1   1.76D0, 1.45D0, 1.37D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.38D0,DJG1094
     2   1.40D0,1.391D0, 1.65D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0,DJG1094
     3   1.40D0, 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 1.75D0, 2.02D0,DJG1094
     4   0.00D0, 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.58D0,DJG1094
     5   2.10D0, 2.06D0, 1.88D0, 2.60D0,46*0.D0/                        DJG1094
C                                                                       GDH0496
C    PD-SM2 rho(0)                                                      GDH0496
C                                                                       GDH0496
      DATA PK2PD/ 1.16D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.07D0,       GDH0496
     1  1.96D0, 1.50D0, 1.51D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, GDH0496
     2  2.36D0, 2.23D0, 2.14D0, 0.00D0, 0.00D0, 0.00D0, 7*0.D0, 0.00D0, GDH0496
     3  0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.33D0, 0.00D0, GDH0496
     4  0.00D0, 0.00D0, 7*0.D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, GDH0496
     5  0.00D0, 0.00D0, 2.68D0, 0.00D0,46*0.D0/                         GDH0496
C
C                                                                       GDH0496
C    PD-SM5 rho(0)                                                      GDH0496
C                                                                       GDH0496
      DATA PK5PD/ 1.17D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 1.89D0,       GDH0496
     1  1.94D0, 1.66D0, 1.50D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, GDH0496
     2  0.00D0, 2.11D0, 2.14D0, 0.00D0, 0.00D0, 0.00D0, 7*0.D0, 0.00D0, GDH0496
     3  0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.30D0, 0.00D0, GDH0496
     4  0.00D0, 0.00D0, 7*0.D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, GDH0496
     5  0.00D0, 0.00D0, 2.67D0, 0.00D0,46*0.D0/                         GDH0596
C                                                                       GDH0596
C    SM5/A2PD rho(0)                                                    GDH0596
C                                                                       GDH0596
      DATA PK5A2P/1.17D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 1.99D0,       GDH0596
     1  1.79D0, 1.64D0, 1.50D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, GDH0596
     2  2.47D0, 2.34D0, 2.14D0, 0.00D0, 0.00D0, 0.00D0, 7*0.D0, 0.00D0, GDH0596
     3  0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.33D0, 0.00D0, GDH0596
     4  0.00D0, 0.00D0, 7*0.D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, GDH0596
     5  0.00D0, 0.00D0, 2.69D0, 0.00D0,46*0.D0/                         GDH0596
C                                                                       GDH0596
C    SM5/P2PD rho(0)                                                    GDH0596
C                                                                       GDH0596
      DATA PK5P2P/1.17D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 1.96D0,       GDH0596
     1  1.90D0, 1.67D0, 1.50D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, GDH0596
     2  2.19D0, 2.30D0, 2.15D0, 0.00D0, 0.00D0, 0.00D0, 7*0.D0, 0.00D0, GDH0596
     3  0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.31D0, 0.00D0, GDH0596
     4  0.00D0, 0.00D0, 7*0.D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, GDH0596
     5  0.00D0, 0.00D0, 2.67D0, 0.00D0,46*0.D0/                         GDH0596
C
C       COULOMB INTEGRAL BASES (PSEUDO RADII) FOR GENERATION OF BORN
C       RADII AND INTERACTIONS. (see below) Data are Pk(0) for SM'#'.1
C
      DATA PK011/0.57D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0, 1.68D0,        GDH1093
     1   1.40D0, 1.46D0, 1.37D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0,GDH1093
     2   1.30D0, 1.30D0, 1.65D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0,GDH1093
     3   1.40D0, 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 1.75D0, 2.02D0,GDH1093
     4   0.00D0, 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0,GDH1093
     5   2.10D0, 2.06D0, 1.88D0, 2.60D0,46*0.D0/                        GDH1093
C
      DATA PK012/0.59D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0,1.798D0,        GDH1093
     1   1.605D0,1.562D0,1.37D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0,GDH1093
     2   1.391D0,1.391D0,1.65D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0,GDH1093
     3   1.40D0, 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 1.75D0, 2.02D0,GDH1093
     4   0.00D0, 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0,GDH1093
     5   2.10D0, 2.06D0, 1.88D0, 2.60D0,46*0.D0/
C
      DATA PK013/0.59D0, 0.00D0, 1.82D0, 0.00D0, 0.00D0,1.691D0,        GDH1093
     1   1.712D0,1.776D0,1.37D0, 1.54D0, 2.27D0, 1.73D0, 2.50D0, 2.10D0,GDH1093
     2   1.498D0,1.284D0,1.65D0, 1.88D0, 2.75D0, 0.00D0, 7*0.D0, 1.63D0,GDH1093
     3   1.40D0, 1.39D0, 2.40D0, 2.10D0, 1.85D0, 1.90D0, 1.75D0, 2.02D0,GDH1093
     4   0.00D0, 0.00D0, 7*0.D0, 1.63D0, 1.72D0, 1.58D0, 2.50D0, 2.20D0,GDH1093
     5   2.10D0, 2.06D0, 1.88D0, 2.60D0,46*0.D0/
C
C       CHARGE ACCOUNTING ARCTANGENT PRE-FACTOR. THUS, EFFECTIVE COULOMB
C       RADIUS = Pk0n + Pk1n * f(ARCTAN(f(CHARGE)))
C
      DATA PK11/ 1.303D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     1  0.62D0,  -0.25D0, 0.181D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     2  0.00D0,   1.00D0,  0.80D0, 0.618D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.705D0,  0.00D0,  GDH1093
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     5 0.932D0,   0.00D0, 46*0.D0/
C
      DATA PK12/ 1.283D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     1  0.42D0,  -0.15D0, 0.145D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     2  0.00D0,   1.00D0,  0.80D0, 0.555D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.629D0,  0.00D0,  GDH1093
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     5 0.885D0,   0.00D0, 46*0.D0/
C
      DATA PK13/ 1.289D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.20D0,  GDH1093
     1  0.32D0,  -0.20D0, 0.149D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     2  0.00D0,   0.80D0,  0.90D0, 0.559D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.610D0,  0.00D0,  GDH1093
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     5 0.798D0,   0.00D0, 46*0.D0/
C
C    SM5-water rho(1)                                                   CCC1295
C
      DATA PK15/  -1.2D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     1  0.00D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     2  0.00D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     5  0.00D0,   0.00D0, 46*0.D0/                                      CCC1295
      DATA PK15PD/  100*0.D0/                                           GDH0496
C    ALKANE                                                             DJG1094
C                                                                       DJG1094
C                                                                       DJG1094
      DATA PK1HA/1.283D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     1  0.00D0,   0.80D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     2  0.00D0,   1.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     5  0.00D0,   0.00D0, 46*0.D0/                                      DJG1094
C
C       CHARGE ACCOUNTING ARCTANGENT PRE-FACTOR. THUS, EFFECTIVE COULOMB
C       RADIUS = Pk0n + Pk1n * f(ARCTAN(f(CHARGE))) SM*.1 Models
C
      DATA PK111/1.303D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     1  0.62D0,  -0.25D0, 0.181D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     2  0.00D0,   1.00D0,  0.80D0, 0.618D0,  0.00D0,  0.00D0 , 0.00D0,  GDH1093
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.705D0,  0.00D0,  GDH1093
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1093
     5 0.932D0,   0.00D0, 46*0.D0/
C
      DATA PK112/1.283D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,           GDH1093
     1  0.00D0,  0.449D0,-0.161D0, 0.145D0,  0.00D0,  0.00D0,           GDH1093
     2  0.00D0,   0.00D0,  0.00D0,  1.07D0, 0.856D0, 0.555D0,           GDH1093
     3  0.00D0,   0.00D0,  0.00D0, 10*0.D0,  0.00D0,  0.00D0,           GDH1093
     4  0.00D0,   0.00D0, 0.629D0,  0.00D0,  0.00D0,  0.00D0,           GDH1093
     5 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.885D0,  0.00D0,  GDH1093
     6 46*0.D0/
C
      DATA PK113/1.289D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,           GDH1093
     1  0.214D0, 0.342D0,-0.214D0, 0.149D0,  0.00D0,  0.00D0,           GDH1093
     2  0.00D0,   0.00D0,  0.00D0, 0.856D0, 0.963D0, 0.559D0,           GDH1093
     3  0.00D0,   0.00D0,  0.00D0, 10*0.D0,  0.00D0,  0.00D0,           GDH1093
     4  0.00D0,   0.00D0, 0.610D0,  0.00D0,  0.00D0,  0.00D0,           GDH1093
     5 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.798D0, 0.00D0,   GDH1093
     6 46*0.D0/
C
C    CHARGE REPRESENTING THE INFLECTION POINT ON THE COULOMB INTEGRAL
C    CURVE DESCRIBED AS AN ARCTANGENT FUNCTION Qk0 for SM'#'
C
      DATA QK01/ -0.30D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     1  0.40D0,   0.75D0, 0.700D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     2  0.00D0,   0.00D0,  0.70D0, 0.750D0,  0.00D0,  0.00D0,  0.00D0,
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.700D0,  0.00D0,
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     5 0.600D0,   0.00D0, 46*0.D0/
C
      DATA QK02/ -0.30D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     1  0.60D0,   0.75D0, 0.700D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     2  0.00D0,   0.00D0,  0.70D0, 0.750D0,  0.00D0,  0.00D0,  0.00D0,
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.700D0,  0.00D0,
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     5 0.600D0,   0.00D0, 46*0.D0/
C
      DATA QK03/ -0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.20D0,
     1  0.50D0,   0.25D0, 0.700D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     2  0.00D0,  -1.50D0,  0.70D0, 0.750D0,  0.00D0,  0.00D0,  0.00D0,
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.700D0,  0.00D0,
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,
     5  0.60D0,   0.00D0, 46*0.D0/
C            
C   SM5- water does not use these parameters...they should all be zero! CCC1295
C
      DATA QK05/  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     1   0.0D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     2  0.00D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     5  0.00D0,   0.00D0, 46*0.D0/                                      CCC1295
C                                                                       DJG1094
      DATA QK0HA/-0.30D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     1  0.00D0,   0.65D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     2  0.00D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     5  0.00D0,   0.00D0, 46*0.D0/                                      DJG1094
C
C    CHARGE REPRESENTING THE INFLECTION POINT ON THE COULOMB INTEGRAL
C    CURVE DESCRIBED AS AN ARCTANGENT FUNCTION Qk0 for SM'#'.1
C
      DATA QK011/-0.30D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,           GDH1293
     2   0.00D0,  0.40D0,  0.75D0, 0.700D0,  0.00D0,  0.00D0,           GDH1293
     3   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.70D0, 0.750D0,           GDH1293
     4   0.00D0,  0.00D0,  0.00D0, 10*0.D0,  0.00D0,  0.00D0,           GDH1293
     5   0.00D0,  0.00D0, 0.700D0,  0.00D0,  0.00D0,  0.00D0,           GDH1293
     6  10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.600D0, 0.00D0,
     7  46*0.D0/
C
      DATA QK012/-0.30D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1293
     1   0.60D0,  0.75D0, 0.700D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1293
     2   0.00D0,  0.00D0,  0.70D0, 0.750D0,  0.00D0,  0.00D0,  0.00D0,  GDH1293
     3  10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.700D0,  0.00D0,  GDH1293
     5   0.00D0,  0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1293
     6  0.600D0,  0.00D0, 46*0.D0/                                      GDH1293
C
      DATA QK013/-0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.20D0,  GDH1293
     1   0.50D0,  0.25D0, 0.700D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1293
     2   0.00D0, -1.50D0,  0.70D0, 0.750D0,  0.00D0,  0.00D0,  0.00D0,  GDH1293
     3  10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.700D0,  0.00D0,  GDH1293
     4   0.00D0,  0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH1293
     5  0.600D0,  0.00D0, 46*0.D0/
C
      DATA QK01A/ 0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  GDH0194
     1  0.10D0,   0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     2  0.00D0,   0.10D0,  0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  0.00D0,  GDH0194
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     5 0.100D0,   0.00D0, 46*0.D0/                                      GDH0194
C                                                                       GDH0194
      DATA QK02A/ 0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  GDH0194
     1  0.10D0,   0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     2  0.00D0,   0.10D0,  0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  0.00D0,  GDH0194
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     5 0.100D0,   0.00D0, 46*0.D0/                                      GDH0194
C
      DATA QK03A/ 0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  GDH0194
     1  0.10D0,   0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     2  0.00D0,   0.10D0,  0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  0.00D0,  GDH0194
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     5 0.100D0,   0.00D0, 46*0.D0/                                      GDH0194
C                                                                       GDH0194
C   SM5- water does not use these parameters...they should all be zero! CCC1295
C
      DATA QK055/ 0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     1  0.00D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     2  0.00D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  CCC1295
     5  0.00D0,   0.00D0, 46*0.D0/                                      CCC1295
C                                                                       DJG1094
      DATA QK0HAA/0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  DJG1094
     1  0.10D0,   0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     2  0.00D0,   0.10D0,  0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  0.00D0,  DJG1094
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  DJG1094
     5 0.100D0,   0.00D0, 46*0.D0/                                      DJG1094
C
C    CHARGE REPRESENTING THE STEEPNESS OF THE COULOMB INTEGRAL          GDH0194
C    CURVE DESCRIBED AS AN ARCTANGENT FUNCTION Qk0#A for SM'#'.1        GDH0194
C                                                                       GDH0194
      DATA QK011A/0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  GDH0194
     1  0.10D0,   0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     2  0.00D0,   0.10D0,  0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  0.00D0,  GDH0194
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     5 0.100D0,   0.00D0, 46*0.D0/                                      GDH0194
C                                                                       GDH0194
      DATA QK012A/0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  GDH0194
     1  0.10D0,   0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     2  0.00D0,   0.10D0,  0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  0.00D0,  GDH0194
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     5 0.100D0,   0.00D0, 46*0.D0/                                      GDH0194
C
      DATA QK013A/0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  GDH0194
     1  0.10D0,   0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     2  0.00D0,   0.10D0,  0.10D0,  0.10D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     3 10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.10D0,  0.00D0,  GDH0194
     4  0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  GDH0194
     5 0.100D0,   0.00D0, 46*0.D0/                                      GDH0194
C                                                                       DJG1094
C    FOR THE VARIABLES: DIECON, SRADCD, SRADCS AND CSSURF               DJG1094
C    DATA IS STORED IN ORDER BY SOLVENT AS FOLLOWS :                    DJG1094
C    WATER, PENTANE, HEXANE, HEPTANE, OCTANE, NONANE, DECANE, UNDECANE  DJG1094
C    DODECANE, TETRADECANE, PENTADECANE, HEXADECANE, 2-METHYLBUTANE     DJG1094
C    (ISOPENTANE), 3-METHYLPENTANE, 2,2,4-TRIMETHYLPENTANE (ISOOCTANE)  DJG1094
C    CYCLOHEXANE, METHYLCYCLOHEXANE, DECALIN, WATER (SM5), OSM5x,       DJG0496
C    CHLOROFORM (SM5.4/A), CHLOROFORM (SM5.4/P), BENZENE (SM5.4/A),     DJG0297
C    BENZENEE (SM5.4/P) TOLUENE (SM5.4/A), TOLUENE (SM5.4/P)            DJG0297
C                                                                       DJG1094
C    DIECON - SOLVENT DIELECTRIC CONSTANT AT 25 DEGREES CELCIUS         DJG1094
C                                                                       DJG1094
      DATA DIECON / 78.3D0, 1.84D0, 1.88D0, 1.91D0, 1.94D0, 1.96D0,     DJG1094
     1              1.98D0, 1.99D0, 2.01D0, 2.03D0, 2.03D0, 2.06D0,     DJG1094
     2              1.84D0, 1.88D0, 1.94D0, 2.02D0, 2.02D0, 2.20D0,     DJG0195
     3              78.3D0, 0.00D0, 4.71D0, 4.71D0, 2.27D0, 2.27D0,     DJG0297
     4              2.38D0, 2.38D0, 74*0.0D0/                           DJG0297
C                                                                       DJG1094
C    SRADCD - SOLVENT RADIUS FOR THE CD TERM (CDS TERM IN THE CASE OF   DJG1094
C             SM1, SM1A, SM2, SM3, SM5 water)  (ANGSTROMS)              DJG1094
C                                                                       DJG1094
      DATA SRADCD / 1.4D0, 17*2.0D0, 8*1.7D0, 74*0.0D0/                 DJG0896
C                                                                       DJG1094
C    SRADCS - SOLVENT RADIUS FOR THE CS TERM (NOT APPLICABLE TO SM1,    DJG1094
C             SM1A, SM2, SM3, SM5 water)  (ANGSTROMS)                   DJG1094
C                                                                       DJG1094
      DATA SRADCS / 0.00D0, 17*4.9D0, 0.0D0, 7*3.4D0, 74*0.0D0/         DJG0896
C                                                                       DJG1094
C    CSSURF - CS SURFACE TENSION FOR THE SOLVENT (CALORIES/(ANG^2*MOL)) DJG1094
C             NOT APPLICABLE TO SM1, SM1A, SM2, SM3, SM5 water          DJG1094
C                                                                       DJG1094
      DATA CSSURF/ 0.00D0, 16.70D0, 16.81D0, 16.90D0, 16.97D0, 17.03D0, DJG1094
     1            17.07D0, 17.11D0, 17.15D0, 17.21D0, 17.23D0, 17.25D0, DJG1094
     2            16.65D0, 16.80D0, 16.83D0, 17.14D0, 17.07D0, 16.97D0, DJG0195
     3             0.00D0, 17.25D0, 29.20D0, 26.74D0, 37.39D0, 34.94D0, DJG0297
     4            37.58D0, 35.11D0, 74*0.0D0/                           DJG0297
C                                                                       DJG0794
C    OFFSETS AND SCALE FACTORS FOR CM1                                  DJG0794
C                                                                       DJG0794
C    SCALE FACTORS FOR CM1A                                             DJG0794
C                                                                       DJG0794
      DATA SCALA1/  0.0D0, 0.0D0,  0.0D0,  0.0D0, 0.0D0, 0.0D0,0.3846D0,DJG0794
     1    0.0D0, 0.1468D0, 0.0D0,  0.0D0,  0.0D0, 0.0D0, 0.0D0,0.0000D0,DJG0794
     2-0.1311D0, 0.0405D0, 0.0D0,12*0.D0,  0.0D0, 0.0D0, 0.0D0,   0.0D0,DJG0794
     3 0.1761D0,    0.0D0, 0.0D0,  0.0D0,10*0.D0, 0.0D0, 0.0D0,   0.0D0,DJG0794
     4    0.0D0, 0.2380D0, 0.0D0,46*0.D0/                               DJG0794
C                                                                       DJG0794
C    OFFSETS FOR NON-HYDROGEN ATOMS IN CM1A                             DJG0794
C                                                                       DJG0794
      DATA OFSTA1/  0.0D0, 0.0D0,  0.0D0, 0.0D0, 0.0D0, 0.0D0,    0.0D0,DJG0794
     1-0.0283D0, 0.0399D0, 0.0D0,  0.0D0, 0.0D0, 0.0D0, 0.0D0,   0.00D0,DJG0794
     2-0.0956D0,-0.0276D0, 0.0D0,12*0.D0, 0.0D0, 0.0D0, 0.0D0,          DJG0794
     3    0.0D0,-0.0802D0, 0.0D0,  0.0D0, 0.0D0,10*0.D0,0.0D0,    0.0D0,DJG0794
     4    0.0D0,    0.0D0,-0.1819D0, 0.0D0, 46*0.0D0/                   DJG0794
C                                                                       DJG0794
C    OFFSETS FOR HYDROGEN ATOMS ATTACHED TO ELEMENT IN QUESTION FOR CM1ADJG0794
C                                                                       DJG0794
      DATA HOFFA1/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0850D0,  DJG0794
     1 0.1447D0, 0.000D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0640D0,         DJG0794
     2 0.0000D0, 0.0D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0,    DJG0794
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,DJG0794
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0/                                   DJG0794
C                                                                       DJG0794
C    OFFSETS FOR OXYGEN ATOMS ATTACHED TO ELEMENT IN QUESTION FOR CM1A  DJG0794
C                                                                       DJG0794
      DATA OXOFF1/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,     DJG0794
     1 0.0D0, 0.000D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,        DJG0794
     2 -0.0600D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,   DJG0794
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0794
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0/                                   DJG0794
C                                                                       DJG0794
C    OFFSETS FOR NITROGEN ATOMS ATTACHED TO ELEMENT IN QUESTION FOR CM1ADJG0794
C                                                                       DJG0794
      DATA ANTOF1/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,-0.0880D0, 0.0D0,  DJG0794
     1 -0.06300D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,     DJG0794
     2 0.0D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0794
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0794
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0/                                   DJG0794
C                                                                       DJG0794
C    SCALE FACTORS FOR CM1P                                             DJG0794
C                                                                       DJG0794
      DATA SCALP1/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,     DJG0794
     1 0.0D0, 0.3381D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,0.0000D0,     DJG0794
     2 -0.0834D0,-0.1080D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,DJG0794
     3 -0.0116D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,   DJG0794
     4 0.0D0, -0.3213D0, 0.0D0, 46*0.0D0/                               DJG0794
C                                                                       DJG0794
C    OFFSETS FOR NON-HYDROGEN ATOMS CM1P                                DJG0794
C                                                                       DJG0794
      DATA OFSTP1/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.0909D0, DJG0794
     1 -0.0449D0, 0.0148D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,   DJG0794
     2 -0.0848D0, -0.1168D0, 0.0D0,12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,DJG0794
     3 -0.0338D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,   DJG0794
     4 0.0D0, -0.0636D0, 0.0D0, 46*0.0D0/                               DJG0794
C                                                                       DJG0794
C    OFFSETS FOR HYDROGEN ATOMS ATTACHED TO ELEMENT IN QUESTION FOR CM1PDJG0794
C                                                                       DJG0794
      DATA HOFFP1/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.1854D0,  DJG0794
     1 0.1434D0,0.000D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,-0.1004D0,0.000D0,  DJG0794
     2 0.0D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0794
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0794
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0/                                   DJG0794
C
      DATA ZHELP/ 1.0D0, 2.0D0,1.0D0, 2.0D0, 3.0D0, 4.0D0, 5.0D0, 6.0D0,
     1 7.0D0, 8.0D0, 1.0D0, 2.0D0,    3.0D0, 4.0D0, 5.0D0, 6.0D0, 7.0D0,
     2 8.0D0, 1.0D0, 2.0D0, 10*2.0D0, 3.0D0, 4.0D0, 5.0D0, 6.0D0, 7.0D0,
     3 8.0D0, 1.0D0, 2.0D0, 10*2.0D0, 3.0D0, 4.0D0, 5.0D0, 6.0D0, 7.0D0,
     4 8.0D0, 1.0D0, 2.0D0, 24*2.0D0, 3.0D0, 4.0D0, 5.0D0, 6.0D0, 7.0D0,
     5 8.0D0, 1.0D0, 2.0D0, 12*2.0D0/
C
C    FOR THE NEXT 7 COMMON BLOCKS, THE ORDER OF SURFACE TENSION         GDH0496
C    PARAMETERS FOLLOWS THIS LIST:                                      GDH0496
C                1st Parameter:  PD-SM5*                                GDH0496
C                2nd Parameter:  PD-SM5A                                GDH0496
C                3rd Parameter:  PD-SM5P                                GDH0496
C                4th Parameter:  AM1- or PM3-SM5.4U                     DJG0796
C                5th Parameter:  AM1-SM5.4A                             DJG0796
C                6th Parameter:  SM4                                    GDH0496
C                7th Parameter:  PM3-SM5.4P                             DJG0796
C                8th Parameter:  SM5/A2PD                               GDH0596
C                9th Parameter   SM5/P2PD                               GDH0596
C               10th Parameter:  SM5.0                                  GDH0696
C               11th Parameter:  SM5.4/U CHLOROFORM                     DJG0896
C               12th Parameter:  SM5.4/A CHLOROFORM                     DJG0896
C               13th Parameter:  SM5.4/P CHLOROFORM                     DJG0896
C               14th Parameter:  SM5.4/A BENZENE                        DJG0297
C               15th Parameter:  SM5.4/P BENZENE                        DJG0297
C               16th Parameter:  SM5.4/A TOLUENE                        DJG0297
C               17th Parameter:  SM5.4/P TOLUENE                        DJG0297
C
C    Note that OSM5 parameters are given elsewhere in this file.        DJG0796
C
C    SURFACE TENSIONS FOR CARBON BONDED TO CARBON. SEE ABOVE CCRFT      GDH0496
C    Order of parameters is listed above the CCSRFT data block          GDH0496
C
C                                                                       DJG0195
      DATA CCSRFT/-50.09D0,-46.71D0,-52.33D0,-40.96D0,                  GDH0596
     1            -36.19D0,1.19D0, -44.56D0, -21.39D0,                  GDH0596
     2           -33.64D0,-86.28D0, 0.0D0, -45.78D0, -50.70D0, -20.84D0,DJG0297
     3           -26.73D0, -20.77D0, -26.64D0, 3*0.0D0/                 DJG0297
C
      DATA CCSRF5/-17.42D0, -20.89D0, -12.75D0, -19.16D0, -23.21D0,     GDH0596
     1             0.00D0, -13.88D0, -10.87D0,-10.63D0, -48.86D0,       GDH0596
     2             0.0D0, -6.46D0,  1.24D0, 0.76D0, 9.80D0, 0.76D0,     DJG0297
     3             9.77, 3*0.0D0/                                       DJG0297
C
C    SURFACE TENSION FOR OXYGEN BONDED TO CARBON.                       GDH0496
C    Order of parameters is listed above the CCSRFT data block          GDH0496
C
      DATA OCSFT/106.54D0, 86.41D0, 127.64D0, 129.98D0, 112.90D0,       GDH0496
     1             0.00D0, 146.31D0, 84.90D0,128.43D0,128.66D0,         GDH0696
     2             0.0D0, 58.59D0, 81.68D0, 24.55D0, 45.51D0, 24.27D0,  DJG0297
     3             45.37D0, 3*0.0D0/                                    DJG0297
C                                                                       CCC1295
C    SURFACE TENSION FOR NITROGEN BONDED TO CARBON.                     GDH0496
C    Order of parameters is listed above the CCSRFT data block          GDH0496
C
      DATA NCSFT/-33.7800D0,-26.560D0,-40.3600D0, -37.48D0,-33.15D0,    GDH0796
     1            0.00D0, -42.03D0, -25.600D0,-41.9100D0,-878.23D0,     GDH0696
     2             0.0D0, -16.92D0, -23.36D0, -7.64D0, -13.47D0,        DJG0297
     3            -7.61D0, -13.42D0, 3*0.0D0/                           DJG0297
C                                                                       CCC1295
C    SURFACE TENSION FOR OXYGEN BONDED TO NITROGEN.                     GDH0496
C    Order of parameters is listed above the CCSRFT data block          GDH0496
C
      DATA ONSFT/146.77D0, 129.91D0, 165.48D0, 174.52D0, 161.73D0,      GDH0796
     1             0.00D0, 187.05D0, 122.89D0,181.20D0,236.52D0,        GDH0696
     2             0.0D0, 64.23D0, 92.42D0, 21.93D0, 43.19D0, 21.89D0,  DJG0297
     3            43.08D0, 3*0.0D0/                                     DJG0297
C                                                                       CCC1295
C    SURFACE TENSION FOR OXYGEN BONDED TO OXYGEN.                       GDH0496
C    Order of parameters is listed above the CCSRFT data block          GDH0496
C
      DATA OOSFT/24.07D0, 32.46D0, 15.18D0, 31.81D0, 38.19D0,           GDH0496
     1             0.00D0, 25.16D0, 44.50D0,49.45D0, 41.35D0,           GDH0696
     2             0.0D0, 11.93D0, 2.88D0, 16.97D0, 9.98D0, 16.91D0,    DJG0297
     3             9.93D0, 3*0.0D0/                                     DJG0297
C                                                                       CCC1295
C    SURFACE TENSION FOR SULFUR BONDED TO SULFUR.                       GDH0496
C    Order of parameters is listed above the CCSRFT data block          GDH0496
C
      DATA SSSFT/  37.09D0, 37.31D0, 36.46D0, 37.28D0, 35.87D0,         GDH0496
     1             0.00D0, 38.09D0, 29.58D0,33.16D0, 28.40D0,           GDH0696
     2             0.0D0, 15.71D0, 16.23D0, 21.69D0, 20.23D0, 21.64D0,  DJG0297
     3            20.18D0, 3*0.0D0/                                     DJG0297
C
C    SURFACE TENSION FOR OXYGEN BONDED TO PHOSPHORUS.                   GDH0496
C    Order of parameters is listed above the CCSRFT data block          GDH0496
C
      DATA OPSFT/  0.000D0, 0.000D0, 0.000D0, 0.000D0, 0.000D0,         GDH0496
     1             0.00D0, 0.000D0, -48.06D0,0.000D0, 11.10D0,          GDH0696
     2             10*0.0D0/                                            GDH0596
C                                                                       GDH1196
C    SPECIAL SURFACE TENSIONS FOR THE SM5.0 MODEL                       GDH0197
C    These parameters appear in the order:                              GDH1296
C    C-C(1), C-C(2), C-O, O-O, N-C(1),N-C(2), N-C(3), C-N(1),           GDH1296
C    O-N, F-C, Cl-C, Br-C, S-S                                          GDH1296
      DATA ADST50/  -84.71D0, -50.91D0, 171.64D0, 152.57D0,             GDH1296
     1              -68.91D0,-529.66D0,   0.00D0,  78.99D0,             GDH1296
     2              363.37D0,   0.00D0,-223.18D0,-220.05D0,             GDH1296
     3              98.04D0 , 27*0.0D0/                                 GDH1296
C                                                                       GDH1296
C                                                                       CCC1295
C                                                                       DJG0396
C    FOR ATOMIC SURFACE TENSIONS:                                       DJG0396
C    INDEX OF REFRACTION DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT DJG0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, NCRFN5, ONRFN5, SSRFN5,   DJG0396
C    AND CSSIGM FOR OSM5.4A                                             DJG0796
C                                                                       DJG0396
      DATA SDEPNA/ -52.03D0,0.0D0,0.0D0,0.0D0,0.0D0,-39.79D0,-50.97D0,  DJG0996
     1 -55.63D0,-42.17D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0, 0.0D0,          DJG0996
     2 -79.01D0,-57.49D0,0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,   DJG0996
     3 -61.10D0, 0.0D0, 0.0D0,0.0D0,10*0.0D0, 0.0D0, 0.0D0, 0.0D0,      DJG0996
     4 0.0D0, -64.47D0, 0.0D0, 46*0.0D0, -13.89D0, 0.51D0, 15.58D0,     DJG0996
     5 12.90D0, -5.73D0, 8.81D0, 10.68D0, 23.56D0 /                     DJG0996
C                                                                       DJG0396
C    FOR ATOMIC SURFACE TENSIONS:                                       DJG0396
C    ALPHA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT               DJG0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, NCRFN5, ONRFN5, SSRFN5,   DJG0396
C    AND CSSIGM FOR OSM5.4A                                             DJG0796
C                                                                       DJG0396
      DATA SDEPAA/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,25.63D0,-23.10D0,  DJG0996
     1 -25.00D0,0.0D0,0.0D0,0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,          DJG0996
     2 -16.41D0,0.0D0,0.0D0,12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0996
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0996
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0, 0.0D0, 0.0D0, 42.38D0, 38.03D0,   DJG0996
     5 -23.01D0, 77.71D0, 0.0D0, 0.0D0/                                 DJG0996
C                                                                       DJG0396
C    FOR ATOMIC SURFACE TENSIONS:                                       DJG0396
C    BETA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT                DJG0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, NCRFN5, ONRFN5, SSRFN5,   DJG0396
C    AND CSSIGM FOR OSM5.4A                                             DJG0796
C                                                                       DJG0396
      DATA SDEPBA/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,10.41D0, 40.45D0,  DJG0996
     1 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,          DJG0996
     2 17.35D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,     DJG0996
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0996
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0, 0.0D0, 0.0D0, 8.28D0, -17.07D0,   DJG0996
     5 6.85D0, 62.15D0, 40.40D0, 0.0D0/                                 DJG0996
C                                                                       DJG0396
C    MACROSCOPIC SURFACE TENSION DEPENDENCE FOR OSM5 SOLVENTS (CSSIGM)  DJG0396
C    FOR OSM5.4A                                                        DJG0796
      DATA SDEPGA/ 7.55D0/                                              DJG0996
C                                                                       DJG0396
C    FOR HYDROGEN SURFACE TENSIONS:                                     DJG0396
C    INDEX OF REFRACTION DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT DJG0396
C    FOR OSM5.4A                                                        DJG0796
      DATA HDEPNA/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.76D0,-6.44D0,   DJG0996
     1 20.13D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,        DJG0996
     2 35.85D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,     DJG0996
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0996
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0/                                   DJG0996
C                                                                       DJG0396
C    FOR HYDROGEN SURFACE TENSIONS:                                     DJG0396
C    BETA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT                DJG0396
C    FOR OSM5.4A                                                        DJG0796
      DATA HDEPBA/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,-34.54D0,   DJG0996
     1 -41.58D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0996
     2 -0.06D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,     DJG0996
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0996
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0/                                   DJG0996
C                                                                       DJG0396
C    FOR ATOMIC SURFACE TENSIONS:                                       DJG0396
C    INDEX OF REFRACTION DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT DJG0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, NCRFN5, ONRFN5, SSRFN5,   DJG0396
C    AND CSSIGM FOR OSM5.4P                                             DJG0796
C                                                                       DJG0396
      DATA SDEPNP/ -47.75D0,0.0D0,0.0D0,0.0D0,0.0D0,-37.32D0,-40.56D0,  DJG0996
     1 -59.53D0,-38.97D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0, 0.0D0,          DJG0996
     2 -71.88D0,-54.22D0,0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,   DJG0996
     3 -58.88D0, 0.0D0, 0.0D0,0.0D0,10*0.0D0, 0.0D0, 0.0D0, 0.0D0,      DJG0996
     4 0.0D0, -61.89D0, 0.0D0, 46*0.0D0, -17.81D0, 6.53D0, 28.53D0,     DJG0996
     5 8.77D0, -9.46D0, 22.36D0, 10.71D0, 22.07D0 /                     DJG0996
C                                                                       DJG0396
C    FOR ATOMIC SURFACE TENSIONS:                                       DJG0396
C    ALPHA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT               DJG0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, NCRFN5, ONRFN5, SSRFN5,   DJG0396
C    AND CSSIGM FOR OSM5.4P                                             DJG0796
C                                                                       DJG0396
      DATA SDEPAP/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,19.75D0,-11.36D0,  DJG0996
     1 -31.41D0,0.0D0,0.0D0,0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,          DJG0996
     2 -31.08D0,0.0D0,0.0D0,12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0996
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0996
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0, 0.0D0, 0.0D0, 56.71D0, 35.93D0,   DJG0996
     5 -28.36D0, 64.47D0, 0.0D0, 0.0D0/                                 DJG0996
C                                                                       DJG0396
C    FOR ATOMIC SURFACE TENSIONS:                                       DJG0396
C    BETA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT                DJG0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, NCRFN5, ONRFN5, SSRFN5,   DJG0396
C    AND CSSIGM FOR OSM5.4P                                             DJG0796
C                                                                       DJG0396
      DATA SDEPBP/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 2.44D0, 47.75D0,  DJG0996
     1 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,          DJG0996
     2 38.63D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,     DJG0996
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0996
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0, 0.0D0, 0.0D0, 19.23D0, -22.79D0,  DJG0996
     5 5.21D0, 68.76D0, 29.70D0, 0.0D0/                                 DJG0996
C                                                                       DJG0396
C    MACROSCOPIC SURFACE TENSION DEPENDENCE FOR OSM5 SOLVENTS (CSSIGM)  DJG0396
C    FOR OSM5.4P                                                        DJG0796
      DATA SDEPGP/ 6.74D0/                                              DJG0996
C                                                                       DJG0396
C    FOR HYDROGEN SURFACE TENSIONS:                                     DJG0396
C    INDEX OF REFRACTION DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT DJG0396
C    FOR OSM5.4P                                                        DJG0796
      DATA HDEPNP/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,-0.80D0,-6.54D0,   DJG0996
     1 26.75D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,        DJG0996
     2 24.10D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,     DJG0996
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0996
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0/                                   DJG0996
C                                                                       DJG0396
C    FOR HYDROGEN SURFACE TENSIONS:                                     DJG0396
C    BETA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT                DJG0396
C    FOR OSM5.4P                                                        DJG0796
      DATA HDEPBP/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,-39.10D0,   DJG0996
     1 -43.73D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0996
     2 -40.91D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,    DJG0996
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0996
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0/                                   DJG0996
C                                                                       DJG0396
C    FOR ATOMIC SURFACE TENSIONS:                                       DJG0396
C    INDEX OF REFRACTION DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT DJG0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, NCRFN5, ONRFN5, SSRFN5,   DJG0396
C    AND CSSIGM FOR OSM5.4U                                             DJG0796
C                                                                       DJG0396
      DATA SDEPNG/ -48.99D0,0.0D0,0.0D0,0.0D0,0.0D0,-37.69D0,-40.85D0,  DJG0396
     1 -55.84D0,-40.16D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0, 0.0D0,          DJG0396
     2 -74.45D0,-55.52D0,0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,   DJG0396
     3 -59.70D0, 0.0D0, 0.0D0,0.0D0,10*0.0D0, 0.0D0, 0.0D0, 0.0D0,      DJG0396
     4 0.0D0, -62.80D0, 0.0D0, 46*0.0D0, -15.83D0, 3.19D0, 20.28D0,     DJG0396
     5 11.02D0, -198.14D0, 14.98D0, 10.57D0, 22.62D0 /                  DJG0396
C                                                                       DJG0396
C    FOR ATOMIC SURFACE TENSIONS:                                       DJG0396
C    ALPHA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT               DJG0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, NCRFN5, ONRFN5, SSRFN5,   DJG0396
C    AND CSSIGM FOR OSM5.4U                                             DJG0796
C                                                                       DJG0396
      DATA SDEPAG/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,22.60D0,-42.45D0,  DJG0396
     1 -28.53D0,0.0D0,0.0D0,0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,          DJG0396
     2 -26.12D0,0.0D0,0.0D0,12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0396
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0396
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0, 0.0D0, 0.0D0, 48.98D0, 38.24D0,   DJG0396
     5 -374.00D0, 75.41D0, 0.0D0, 0.0D0/                                DJG0396
C                                                                       DJG0396
C    FOR ATOMIC SURFACE TENSIONS:                                       DJG0396
C    BETA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT                DJG0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, NCRFN5, ONRFN5, SSRFN5,   DJG0396
C    AND CSSIGM FOR OSM5.4U                                             DJG0496
C                                                                       DJG0396
      DATA SDEPBG/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 6.41D0, 55.44D0,  DJG0396
     1 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,          DJG0396
     2 30.95D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,     DJG0396
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0396
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0, 0.0D0, 0.0D0, 13.21D0, -19.30D0,  DJG0396
     5 92.42D0, 63.20D0, 33.76D0, 0.0D0/                                DJG0396
C                                                                       DJG0396
C    MACROSCOPIC SURFACE TENSION DEPENDENCE FOR OSM5 SOLVENTS (CSSIGM)  DJG0396
C    FOR OSM5.4U                                                        DJG0796
      DATA SDEPGG/ 7.18D0/                                              DJG0396
C                                                                       DJG0396
C    FOR HYDROGEN SURFACE TENSIONS:                                     DJG0396
C    INDEX OF REFRACTION DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT DJG0396
C    FOR OSM5.4U                                                        DJG0796
      DATA HDEPNG/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,-0.96D0,-11.04D0,  DJG0396
     1 21.40D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,        DJG0396
     2 28.46D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,     DJG0396
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0396
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0/                                   DJG0396
C                                                                       DJG0396
C    FOR HYDROGEN SURFACE TENSIONS:                                     DJG0396
C    BETA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT                DJG0396
C    FOR OSM5.4U                                                        DJG0796
      DATA HDEPBG/ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,-41.52D0,   DJG0396
     1 -42.33D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0396
     2 -25.29D0, 0.0D0, 0.0D0, 12*0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,    DJG0396
     3 0.0D0, 0.0D0, 0.0D0, 0.0D0, 10*0.0D0, 0.0D0, 0.0D0, 0.0D0,       DJG0396
     4 0.0D0, 0.0D0, 0.0D0, 46*0.0D0/                                   DJG0396
C
C     The following common block sets the dkk values used in the        GDH0596
C     subroutine STLFAC.  The ordering is as follows:                   GDH0596
C     where ??? is used to refer to a model, it means that value is     GDH0596
C     for all models other than those elsewhere on the list.            GDH0596
C                 1st    Dkk for C-H, H-C cross term for ???            GDH0596
C                 2nd    General Dkk for ???                            GDH0596
C                 3st    Dkk for C-H, H-C cross term for SM5, SM5A, SM5P DH0596
C                 4nd    General Dkk for SM5, SM5A, SM5P                GDH0596
C                 5th    Dkk for C-H, H-C cross term for SM2/A2PD       GDH0596
C                 6th    General Dkk for SM2/A2PD                       GDH0596
C                 7th    Dkk for C-H, H-C cross term for SM5/A4PD, SM5/P4PD    
C                 8th    General Dkk for SM5/A4PD, SM5/P4PD             GDH0596
C                 9th    Dkk for C-H, H-C cross term for SM5/A2PD    
C                10th    General Dkk for SM5/A2PD                       GDH0596
C                11th    Dkk for C-H, H-C cross term for SM5/P2PD    
C                12th    General Dkk for SM5/P2PD                       GDH0596
C                13th    Dkk for H-H, H-H cross term for SM5.05         GDH0297
C                14th    Dkk for O-O, O-O cross term for SM5.05         GDH0297
      DATA DKKVAL/4.00D0, 4.00D0, 4.2D0, 4.0D0, 3.8D0, 4.0D0,           GDH0596
     1            4.5D0,  4.00D0, 4.0D0, 3.2D0, 4.1D0, 3.6D0,           GDH0596
     2            1.0D0,  3.00D0, 16*0.0D0/                             GDH0297
C    This array converts atomic number to supported atom types          GDH0197
C    used in the calculation of the geometry dependent functionals      GDH0197
C    in SM5.xR models                                                   GDH0197
C    Hydrogen - type 1                                                  GDH0197
C    Carbon   - type 2                                                  GDH0197
C    Nitrogen - type 3                                                  GDH0197
C    Oxygen   - type 4                                                  GDH0197
C    Fluorine - type 5                                                  GDH0197
C    Sulfur   - type 6                                                  PDW0601
C    Chlorine - type 7                                                  GDH0197
C    Bromine  - type 8                                                  GDH0197
C    Phos.    - type 9                                                  GDH0197
C    Iodine   - type 10                                                 GDH0797
C    Silicon  - type 11
      DATA NATCNV/1.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     2            2.00D0, 3.00D0, 4.00D0, 5.00D0, 0.00D0,               GDH0197
     3            0.00D0, 0.00D0, 0.00D0,11.00D0, 9.00D0,               GDH0197
     4            6.00D0, 7.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     5            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     6            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     7            0.00D0, 0.00D0, 0.00D0, 0.00D0, 8.00D0,               GDH0197
     8            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     1            0.00D0, 0.00D0,10.00D0, 0.00D0, 0.00D0,               GDH0197
     2            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     3            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     4            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     5            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     6            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     7            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     8            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0/               GDH0197
      DATA ((RKKVAL(J,I), I=1,15), J=1,15)                              GDH0197
C  Set for type 1
     .           /0.00D0, 1.55D0, 1.55D0, 1.55D0, 0.00D0,               GDH0197
     .            2.14D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 2
     .            1.55D0, 1.84D0, 1.84D0, 1.84D0, 1.84D0,               GDH0197
     .            2.20D0, 2.10D0, 2.30D0, 2.20D0, 2.60D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 3
     .            1.55D0, 1.84D0, 1.85D0, 1.50D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 4
     .            1.55D0, 1.84D0, 1.50D0, 2.75D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 2.10D0, 0.00D0,               GDH0197
     .            2.10D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 5
     .            0.00D0, 1.84D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 6
     .            2.14D0, 2.20D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            2.75D0, 0.00D0, 0.00D0, 2.50D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 7
     .            0.00D0, 2.10D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 8
     .            0.00D0, 2.30D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 9
     .            0.00D0, 2.20D0, 0.00D0, 2.10D0, 0.00D0,               GDH0197
     .            2.50D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 10
     .            0.00D0, 2.60D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 11
     .            0.00D0, 0.00D0, 0.00D0, 2.10D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 12
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 13
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 14
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 15
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0/               GDH0197
C
      DATA ((RKKVL2(J,I), I=1,15), J=1,15)                              GDH0197
C  Set for type 1
     .           /0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 2
     .            0.00D0, 1.27D0,1.225D0, 1.33D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 3
     .            0.00D0,1.225D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 4
     .            1.55D0, 1.33D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 5
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 6
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 7
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 8
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 9
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 10
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 11
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 12
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 13
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 14
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
C  Set for type 15
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0/               GDH0197
C
      DATA DRVAL/ 0.30D0, 0.10D0, 0.07D0, 0.065D0, 11*0.0D0/            GDH0198
      DATA ENEGIT/2.20D0, 0.00D0, 0.98D0, 1.57D0, 2.04D0,               GDH0197
     2            2.55D0, 3.04D0, 3.44D0, 3.98D0, 0.00D0,               GDH0197
     3            0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.19D0,               GDH0197
     4            2.58D0, 3.16D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     5            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     6            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     7            0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.96D0,               GDH0197
     8            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     1            0.00D0, 0.00D0, 2.66D0, 0.00D0, 0.00D0,               GDH0197
     2            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     3            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     4            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     5            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     6            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     7            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     8            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,               GDH0197
     .            0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0/               GDH0197
C                                                                       GDH0396
      DATA VBLANK/150*0.0D0/                                            GDH0597
C    FOR ATOMIC SURFACE TENSIONS:                                       GDH0396
C    PROPERTY DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT            GDH0396
C    AND THEN THESE START AT POSITION 100
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, CNRFN4, ONRFN5, SSRFN5,   GDH1197
C    BLANK, BLANK, CNSTC2, TNCSC2, HNNST1, HOHST1, OPRFN5,              GDH1197
C    SPRFN5, TNCSC3, CLCRFN, AND BRCRFN FOR SM5.0R                      GDH1197
C                                                                       GDH0396
C    !!!NOTICE!!! This order is DIFFERENT than in the OMNISOL blocks    GDH0197
      DATA S50RNA/43.22D0,          4*0.0D0, 45.12D0, -33.08D0,         GDH0198
     .  -116.87D0, -2.82D0,          6*0.0D0, -83.63D0, -18.06D0,       GDH0198
     .  17*0.0D0, -41.56D0, 17*0.0D0, -51.36D0,  47*0.0D0, -63.92D0,    GDH0198
     .  -6.13D0, 60.47D0, 17.54D0,-11.80D0, 108.56D0, 10.1D0, 2*0.0D0,  GDH0198
     .   -82.08D0, 1*0.0D0,-142.16D0, 129.24D0,3*0.0D0,  -38.76D0,      GDH0198
     .   0.85D0, 32*0.0D0/                                              GDH0198
C                                                                       GDH0198
      DATA S50RAA/0.00D0,        4*0.0D0,31.81D0,-169.60D0,-126.31D0,   GDH0198
     . 7*0.0D0,-76.37D0,   87*0.0D0,136.78D0,-53.66D0,38.20D0,          GDH0198
     . 3*0.0D0,161.37D0,       40*0.0D0  /                              GDH0198
C
      DATA S50RBA/ 0.00D0, 4*0.0D0,-41.37D0,20.75D0,                    GDH0198
     . 8*0.0D0,42.15D0, 86*0.0D0,-105.57D0,-13.92D0,3.79D0,-5.26D0,     GDH0198
     . 11*0.0D0, -62.93D0, 32*0.0D0 /                                   GDH0198
C
      DATA H50RNA/ 0.00D0,      4*0.0D0,-92.91D0, -94.47D0,-19.33D0,    GDH0198
     .    7*0.0D0,53.43D0,134*0.0D0 /                                   GDH0198
C
      DATA H50RAA/7*0.0D0,-192.63D0,142*0.0D0/                          GDH0198
C
      DATA H50RBA/0.00D0,       5*0.0D0,-164.33D0,-545.28D0, 142*0.0D0/ GDH0198
C
C    MACROSCOPIC SURFACE TENSION DEPENDENCE FOR OSM5 SOLVENTS (CSSIGM)  GDH0396
C    FOR SM5.0R                                                         GDH0796
C    GAMMA, BETA^2, Aromatic C^2, Electronegative Halogen^2, IOFR,      GDH0497
C                                                                       GDH0497
C    SURFACE TENSION COEFFICIENTS IN THE FOLLOWING COMMON BLOCK         GDH0497
C
      DATA S50RTA/ 0.1410D0, 16.68D0, -1.45D0, -12.30D0,                GDH0497
     . 6*0.0D0/                                                         GDH0497

C                                                                       GDH1296
C    SM5.2R rho(0)                                                      GDH1296
C                                                                       GDH1296
      DATA PK06/ 0.91D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 1.78D0,        GDH1296
     1  1.92D0, 1.60D0, 1.50D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, GDH1296
     2  2.40D0, 2.05D0, 2.13D0, 0.00D0, 0.00D0, 0.00D0, 7*0.D0, 0.00D0, GDH1296
     3  0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.31D0, 0.00D0, GDH1296
     4  0.00D0, 0.00D0, 7*0.D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, GDH1296
     5  0.00D0, 0.00D0, 2.66D0, 0.00D0,46*0.D0/                         GDH1296
C                                                                       GDH0396
C                                                                       GDH1296
C    SM5.42R rho(0)                                                      GDH1296
C                                                                       GDH1296
      DATA PK07/ 0.91D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 1.78D0,        GDH1296
C    1  1.92D0, 1.60D0, 1.50D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, GDH1296
     1  1.92D0, 1.60D0, 1.50D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.10D0, PDW0601
     2  2.40D0, 2.15D0, 2.13D0, 0.00D0, 0.00D0, 0.00D0, 7*0.D0, 0.00D0, GDH1296
     3  0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.31D0, 0.00D0, GDH1296
     4  0.00D0, 0.00D0, 7*0.D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, GDH1296
     5  0.00D0, 0.00D0, 2.66D0, 0.00D0,46*0.D0/                         GDH1296
C                                                                       GDH0396
C    FOR ATOMIC SURFACE TENSIONS:                                       GDH0396
C    INDEX OF REFRACTION DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT GDH0396
C    AND THEN THESE START AT POSITION 100
C    CCRFN4, CCRFN5, OCRFN5, OORFN5, CNRFN4, ONRFN5, SSRFN5,            GDH0396
C    BLANK, BLANK, CNSTC2, TNCSC2, HNNST1, HOHST1, OPRFN5, AND          GDH0797
C    SPRNF5 FOR SM5.2R/M                                                GDH0797
C                                                                       GDH0396
C     MNDO
      DATA SDMPNA/41.39D0, 4*0.0D0,47.12D0,-19.53D0,-66.62D0,           GDH0198
     . 11.29D0,5*0.0D0,294.15D0,-77.54D0,-29.56D0,17*0.0D0, -42.50D0,   GDH0597
     . 17*0.0D0,-52.03D0,47*0.D0,-64.18D0,16.89D0,42.50D0, 14.83D0,     GDH0597
     . -13.01D0,102.50D0,-0.27D0, 2*0.0D0, -60.77D0, 1*0.0D0,-152.04D0, GDH0597
     .  153.33D0, -29.37D0, 69.59D0, 35*0.0D0/                          GDH0797
C     AM1
      DATA SDAPNA/39.69D0, 4*0.0D0,74.59D0,-31.61D0,-47.20D0,           GDH0797
     .  3.97D0,5*0.0D0,399.49D0,-73.99D0,-35.21D0,17*0.0D0, -47.82D0,   GDH0797
     . 17*0.0D0,-54.85D0,47*0.D0,-68.15D0, 3.99D0,20.16D0,  7.19D0,     GDH0797
     . -10.87D0,76.85D0, 0.01D0,  2*0.0D0, -63.46D0, 1*0.0D0,-142.74D0, GDH0797
     .  172.52D0, -12.62D0, 278.73D0, 35*0.0D0/                         GDH0797
C     PM3
      DATA SDPPNA/40.75D0, 4*0.0D0,58.59D0,-31.36D0,-72.89D0,           GDH0198
     .  1.91D0,5*0.0D0,373.03D0,-75.01D0,-34.69D0,17*0.0D0,-46.18D0,    GDH0797
     . 17*0.0D0,-46.03D0,47*0.D0,-64.41D0,14.40D0,63.15D0,  7.57D0,     GDH0797
     . -15.18D0,131.78D0, 4.49D0, 2*0.0D0, -53.00D0, 1*0.0D0,-154.69D0, GDH0797
     .  157.32D0, -23.08D0, 129.51D0, 35*0.0D0/                         GDH0797
C                                                                       GDH0396
C    FOR ATOMIC SURFACE TENSIONS:                                       GDH0396
C    ALPHA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT               GDH0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, CNRFN4, ONRFN5, SSRFN5,   GDH0396
C    BLANK, BLANK, CNSTC2, TNCSC2, HNNST1, HOHST1, OPRFN5,              GDH0497
C     AND SPRFN5 FOR SM5.2R/M                                           GDH0796
C                                                                       GDH0396
C  MNDO                                                                 GDH0396
      DATA SDMPAA/0.00D0,        4*0.0D0,29.65D0,-165.99D0,-61.40D0,    GDH0597
     . 7*0.0D0,-60.07D0,   87*0.0D0,146.52D0,-62.52D0,86.06D0,          GDH0597
     . 3*0.0D0,218.33D0,       40*0.0D0  /                              GDH0597
C  AM1                                                                  GDH0797
      DATA SDAPAA/0.00D0,        4*0.0D0,36.30D0,-192.96D0,-66.98D0,    GDH0797
     . 7*0.0D0,-29.11D0,   87*0.0D0,153.66D0,-72.27D0,149.02D0,         GDH0797
     . 3*0.0D0,305.33D0,       40*0.0D0  /                              GDH0797
C  PM3                                                                  GDH0797
      DATA SDPPAA/0.00D0,        4*0.0D0,33.73D0,-179.29D0,-68.61D0,    GDH0797
     . 7*0.0D0,-41.03D0,   87*0.0D0,157.33D0,-72.73D0,105.13D0,         GDH0797
     . 3*0.0D0,269.16D0,       40*0.0D0  /                              GDH0797
C                                                                       GDH0396
C    FOR ATOMIC SURFACE TENSIONS:                                       GDH0396
C    BETA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT                GDH0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, CNRFN4, ONRFN5, SSRFN5,   GDH0396
C    BLANK, BLANK, CNSTC2, TNCSC2, HNNST1, HOHST1, OPRFN5,              GDH0497
C     AND SPRFN5 FOR SM5.2R/M                                           GDH0796
C                                                                       GDH0396
C MNDO                                                                  GDH0396
      DATA SDMPBA/ 0.00D0, 4*0.0D0,-23.44D0,36.92D0,                    GDH0597
     . 8*0.0D0,40.27D0, 86*0.0D0,-32.88D0,8.66D0,3.61D0,81.05D0,        GDH0597
     . 44*0.0D0 /                                                       GDH0597
C AM1                                                                   GDH0396
      DATA SDAPBA/ 0.00D0, 4*0.0D0,  8.88D0,37.73D0,                    GDH0597
     . 8*0.0D0,29.83D0, 86*0.0D0,-16.56D0,6.25D0,-3.25D0,69.91D0,       GDH0597
     . 44*0.0D0 /                                                       GDH0597
C PM3                                                                   GDH0396
      DATA SDPPBA/ 0.00D0, 4*0.0D0, -6.55D0,30.01D0,                    GDH0597
     . 8*0.0D0,34.65D0, 86*0.0D0,  2.78D0,-9.38D0,-1.17D0,98.79D0,      GDH0597
     . 44*0.0D0 /                                                       GDH0597
C                                                                       GDH0396
C    MACROSCOPIC SURFACE TENSION DEPENDENCE FOR OSM5 SOLVENTS (CSSIGM)  GDH0396
C    FOR SM5.2R/M                                                       GDH0796
C    GAMMA, BETA^2, Aromatic C^2, Electronegative Halogen^2             GDH0497
C    SURFACE TENSION COEFFICIENTS IN THE FOLLOWING COMMON BLOCK         GDH0497
      DATA SDMPTA/ 0.2424D0, 10.99D0, -2.75D0, -10.07D0,                GDH0497
     . 6*0.0D0/                                                         GDH0497
C    AM1                                                                GDH0797
      DATA SDAPTA/ 0.3024D0,  2.47D0, -3.69D0,  -8.39D0,                GDH0797
     . 6*0.0D0/                                                         GDH0797
C    PM3                                                                GDH0797
      DATA SDPPTA/ 0.2706D0,  6.35D0, -3.27D0,  -9.19D0,                GDH0797
     . 6*0.0D0/                                                         GDH0797
C                                                                       GDH0396
C    FOR HYDROGEN SURFACE TENSIONS:                                     GDH0396
C    INDEX OF REFRACTION DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT GDH0396
C    FOR SM5.2R/M                                                       GDH0796
C MNDO
      DATA HDMPNA/ 0.00D0,        4*0.0D0,-97.57D0,-102.16D0,-66.24D0,  GDH0597
     .    7*0.0D0,38.37D0,134*0.0D0 /                                   GDH0597
C AM1
      DATA HDAPNA/ 0.00D0,       4*0.0D0,-113.40D0, -95.52D0,-89.82D0,  GDH0597
     .    7*0.0D0,34.13D0,134*0.0D0 /                                   GDH0597
C PM3
      DATA HDPPNA/ 0.00D0,       4*0.0D0,-104.97D0, -95.35D0,-51.43D0,  GDH0597
     .    7*0.0D0,29.99D0,134*0.0D0 /                                   GDH0597
C                                                                       GDH0396
C    FOR HYDROGEN SURFACE TENSIONS:                                     GDH0396
C    ALPHA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT               GDH0396
C    FOR SM5.2R/M                                                       GDH0796
      DATA HDMPAA/7*0.0D0,-330.88D0,142*0.0D0/                          GDH0497
C AM1
      DATA HDAPAA/7*0.0D0,-341.94D0,142*0.0D0/                          GDH0797
C PM3
      DATA HDPPAA/7*0.0D0,-329.45D0,142*0.0D0/                          GDH0797
C                                                                       GDH0396
C    FOR HYDROGEN SURFACE TENSIONS:                                     GDH0396
C    BETA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT                GDH0396
C    FOR SM5.2R/M                                                       GDH0796
      DATA HDMPBA/0.00D0,       5*0.0D0,-158.03D0,-472.23D0, 142*0.0D0/ GDH0497
C AM1
      DATA HDAPBA/0.00D0,       5*0.0D0,-128.35D0,-420.35D0, 142*0.0D0/ GDH0797
C PM3
      DATA HDPPBA/0.00D0,       5*0.0D0,-134.64D0,-444.54D0, 142*0.0D0/ GDH0797
C                                                                       GDH0396
C                                                                       GDH1296
C    SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED    GDH0297
C    FROM EMPIRIC SURFACE TENSION TERMS (SM5.2R/M).DEPENDANT ON BOND    GDH0297
C    LENGTH TO ATOM X. SOLVENT=WATER                                    GDH0297
C    FOR ATOMIC SURFACE TENSIONS:                                       GDH0396
C    INDEX OF REFRACTION DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT GDH0396
C    SM5.2R/M                                                           GDH0796
C                                                                       GDH0297
C MNDO
      DATA HS052M/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0,-127.61D0, GDH0297
     1-240.72D0,-412.19D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0297
     2   0.00D0,   0.00D0, 19.84D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0297
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0297
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0297
     5   0.00D0,   0.00D0, 46*0.D0, 50*0.D0/                            GDH0198
C AM1
      DATA HS052A/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0,-158.54D0, GDH0797
     1-232.98D0,-465.86D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0797
     2   0.00D0,   0.00D0, -0.40D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0797
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0797
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0797
     5   0.00D0,   0.00D0, 46*0.D0, 50*0.D0/                            GDH0797
C PM3
      DATA HS052P/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0,-141.97D0, GDH0797
     1-234.59D0,-384.85D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0797
     2   0.00D0,   0.00D0,-30.98D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0797
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0797
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH0797
     5   0.00D0,   0.00D0, 46*0.D0, 50*0.D0/                            GDH0797
C                                                                       GDH0797
C
C    SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM            GDH0596
C    EMPIRIC SURFACE TENSION TERMS (SM5.2R/M)                           GDH0596
C    AND THEN THESE START AT POSITION 100
C    CCRFN4, CCRFN5, OCRFN5, OORFN5, CNRFN4, ONRFN5, SSRFN5,            GDH0396
C    BLANK, BLANK, CNSTC2, TNCSC2, HNNST1, HOHST1, OPRFN5,              GDH0497
C     AND SPRFN5 FOR SM5.2R/M                                           GDH0796
C                                                                       GDH0596
C MNDO
      DATA S052M/98.86D0, 4*0.0D0, 66.41D0, -193.07D0, -237.31D0,       GDH0597
     1 54.45D0, 5*0.0D0, -73.62D0, -101.8D0, 4.60D0,17*0.0D0,           GDH0797
     . -13.88D0, 17*0.0D0,                                              GDH0797
     2 -35.76D0, 47*0.0D0, -63.08D0, 29.16D0, 158.83D0, 152.86D0,       GDH0597
     3 -71.45D0, 382D0, 35.02D0, 2*0.0D0, 122.14D0, -383.81D0,          GDH0597
     4 -134.82D0, 320.76D0, 71.84D0, 237.29D0, 35*0.0D0 /               GDH0597
C AM1
      DATA S052A/98.81D0, 4*0.0D0,121.09D0, -206.82D0, -193.93D0,       GDH0797
     1 44.69D0, 5*0.0D0, -84.43D0,  -87.330D0,-4.00D0,17*0.0D0,         GDH0797
     . -26.68D0, 17*0.0D0,                                              GDH0797
     2 -38.43D0, 47*0.0D0, -61.48D0, 16.10D0, 120.71D0, 142.17D0,       GDH0797
     3 -68.86D0, 342.64D0, 40.62D0, 2*0.0D0, 136.46D0, -447.64D0,       GDH0797
     4 -113.59D0, 392.05D0, 133.65D0, 624.85D0, 35*0.0D0 /              GDH0797
C PM3
      DATA S052P/98.97D0, 4*0.0D0, 90.33D0, -206.45D0, -246.03D0,       GDH0797
     1 39.00D0, 5*0.0D0, -37.24D0, -82.730D0, -6.02D0,17*0.0D0,         GDH0797
     . -21.47D0, 17*0.0D0,                                              GDH0797
     2 -10.01D0, 47*0.0D0, -59.89D0, 31.28D0, 215.68D0, 129.32D0,       GDH0797
     3 -77.01D0, 422.77D0, 35.26D0, 2*0.0D0, 142.06D0, -330.58D0,       GDH0797
     4 -136.37D0, 341.53D0, 102.83D0, 342.94D0, 35*0.0D0 /              GDH0797
C                                                                       GDH0396
C    FOR ATOMIC SURFACE TENSIONS:                                       GDH0396
C    INDEX OF REFRACTION DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT GDH0396
C    AND THEN THESE START AT POSITION 100
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, CNRFN4, ONRFN5, SSRFN5,   GDH1197
C    BLANK, BLANK, CNSTC2, TNCSC2, HNNST1, HOHST1, OPRFN5,              GDH1197
C    SPRFN5, TNCSC3, CLCRFN, AND BRCRFN FOR SM5.42R                     GDH1197
C                                                                       GDH0396
C    SDAPN4(14), i.e., for Si changed from -118.8 to -118.5 for AM1     !JT1201
C    SDAPN4(14), i.e., for Si changed from -126.8 to -126.6 for PM3     !JT1201
C     AM1
      DATA SDAPN4/39.40D0, 4*0.0D0, 70.440, 42.07D0, -24.56D0, 1.57D0,  GDH0198
     .  4*0.0D0,-118.5D0,-29.90D0,-74.94D0,-36.15D0,17*0.0D0,-47.41D0,  PDW0601
     .  17*0.0D0, -50.90D0, 47*0.0D0, -72.83D0, 5.41D0, 45.06D0,        GDH0198
     .  -30.49D0,-15.34D0, 36.18D0, 16.77D0, 2*0.0D0,  -69.860,1*0.0D0, GDH0198
     .  -207.45D0, 226.66D0, 54.90D0, 282.36D0,-14.41D0, 34*0.0D0/      GDH0198
C     PM3
      DATA SDPPN4/39.47D0, 4*0.0D0, 64.86D0, 42.88D0,-41.08D0, -1.37D0, GDH1197
     . 4*0.0D0,-126.6D0,-137.51D0,-74.75D0,-35.41D0,17*0.0D0,-47.89D0,  PDW0601
     . 17*0.0D0, -50.79D0, 47*0.0D0, -71.03D0, 9.79D0, 76.79D0,         GDH1197
     . -34.44D0,-14.58D0, 89.27D0, 11.95D0, 2*0.0D0,  -77.47D0,1*0.0D0, GDH1197
     . -197.85D0, 209.43D0,47.66D0, 172.91D0, -2.28D0, 34*0.0D0/        GDH1197
C                                                                       GDH0396
C    FOR ATOMIC SURFACE TENSIONS:                                       GDH0396
C    ALPHA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT               GDH0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, CNRFN4, ONRFN5, SSRFN5,   GDH1197
C    OSIBD5, BLANK, CNSTC2, TNCSC2, HNNST1, HOHST1, OPRFN5,              GDH1197
C    SPRFN5, TNCSC3, CLCRFN, AND BRCRFN FOR SM5.42R                     GDH1197
C                                                                       GDH0396
C    SDAPA4(14), i.e., Si for AM1 has changed from 173.1 to 173.7       !JT1201 
C    SDAPA4(108),  i.e., O-Si for AM1 has been removed                  !JT0102
C    SDPPA4(14), i.e., Si for PM3 has changed from 152.4 to 152.1       !JT1201 
C    SDPPA4(108), i.e., O-Si for PM3  has been removed                  !JT0102
C  AM1                                                                  GDH0797
      DATA SDAPA4/5*0.0D0, 28.13D0, -70.23D0,-4.37D0,5*0.0D0,173.7D0,   PDW0601
     . 1*0.0D0,-55.10D0,87*0.0D0, 100.15D0, -66.82D0,-12.23D0, 0.0D0,   PDW0601
     .    0.0D0,0.0D0,149.73D0, 40*0.0D0/                               GDH1197
C  PM3                                                                  GDH0797
      DATA SDPPA4/0.00D0,        4*0.0D0,27.68D0, -75.43D0,-12.18D0,    GDH0797
     . 5*0.0D0,152.1D0,1*0.0D0,-51.10D0,87*0.0D0,110.31D0,-69.00D0,     PDW0601
     . 25.23D0,0.0D0,   0.0D0,0.0D0,154.20D0,       40*0.0D0  /         PDW0601
C                                                                       GDH0396
C    FOR ATOMIC SURFACE TENSIONS:                                       GDH0396
C    BETA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT                GDH0396
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, CNRFN4, ONRFN5, SSRFN5,   GDH1197
C    OSIBD5, BLANK, CNSTC2, TNCSC2, HNNST1, HOHST1, OPRFN5,             PDW0901
C    SPRFN5, TNCSC3, CLCRFN, AND BRCRFN FOR SM5.42R                     GDH1197
C                                                                       GDH0396
C    SDAPB4(14), i.e., Si for AM1 has changed from 163.4 to 161.1       !JT1201
C    SDPPB4(14), i.e., Si for PM3 has not changed                       !JT1201
C AM1                                                                   GDH0396
      DATA SDAPB4/  5*0.0D0, 11.40D0,115.66D0,6*0.0D0,161.1D0,1*0.0D0,  PDW0601
     . 64.39D0,86*0.0D0,74.93D0, -48.56D0, -0.59D0, 101.68D0, 44*0.0D0/ PDW0601
C PM3                                                                   GDH0396
      DATA SDPPB4/ 0.00D0, 4*0.0D0,  7.59D0,115.23D0,6*0.0D0,149.0D0,   PDW0601
     . 1*0.0D0,58.10D0, 86*0.0D0,100.40D0,-67.36D0,-1.44D0,133.40D0,    PDW0601
     . 44*0.0D0 /                                                       GDH1197
C                                                                       GDH0396
C    MACROSCOPIC SURFACE TENSION DEPENDENCE FOR OSM5 SOLVENTS (CSSIGM)  GDH0396
C    FOR SM5.42R                                                        GDH0198
C    GAMMA, BETA^2, Aromatic C^2, Electronegative Halogen^2             GDH0497
C    SURFACE TENSION COEFFICIENTS IN THE FOLLOWING COMMON BLOCK         GDH0497
C    AM1                                                                GDH0797
      DATA SDAPT4/ 0.3871D0,-0.37D0, -4.61D0,  -6.98D0,                 GDH0797
     . 6*0.0D0/                                                         GDH0797
C    PM3                                                                GDH0797
      DATA SDPPT4/ 0.3755D0,  0.52D0, -4.54D0,  -7.21D0,                GDH0797
     . 6*0.0D0/                                                         GDH0797
C                                                                       GDH0396
C    FOR HYDROGEN SURFACE TENSIONS:                                     GDH0396
C    INDEX OF REFRACTION DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT GDH0396
C    FOR SM5.42R                                                        GDH1197
C AM1
      DATA HDAPN4/ 0.00D0,       4*0.0D0,-112.19D0, -95.42D0,-10.73D0,  GDH1197
     .    7*0.0D0,37.67D0,134*0.0D0 /                                   GDH1197
C PM3
      DATA HDPPN4/ 0.00D0,       4*0.0D0,-108.95D0,-105.76D0, 17.26D0,  GDH1197
     .    7*0.0D0,36.92D0,134*0.0D0 /                                   GDH0597
C                                                                       GDH0396
C    FOR HYDROGEN SURFACE TENSIONS:                                     GDH0396
C    ALPHA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT               GDH0396
C    FOR SM5.42R                                                        GDH1197
C AM1
      DATA HDAPA4/7*0.0D0,-325.77D0,142*0.0D0/                          GDH1197
C PM3
      DATA HDPPA4/7*0.0D0,-308.95D0,142*0.0D0/                          GDH0797
C                                                                       GDH0396
C    FOR HYDROGEN SURFACE TENSIONS:                                     GDH0396
C    BETA DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT                GDH0396
C    FOR SM5.42R                                                        GDH1197
C AM1
      DATA HDAPB4/0.00D0,       5*0.0D0,-142.81D0,-263.31D0, 142*0.0D0/ GDH1197
C PM3
      DATA HDPPB4/0.00D0,       5*0.0D0,-147.61D0,-267.50D0, 142*0.0D0/ GDH1197
C                                                                       GDH0396
C    WATER                                                              GDH1296
C    SURFACE AREA DEPENDENT CORRECTION TERMS FOR HYDROGEN DETERMINED    GDH0297
C    FROM EMPIRIC SURFACE TENSION TERMS (SM5.2R/M).DEPENDANT ON BOND    GDH0297
C    LENGTH TO ATOM X. SOLVENT=WATER                                    GDH0297
C    FOR ATOMIC SURFACE TENSIONS:                                       GDH0396
C    INDEX OF REFRACTION DEPENDENCE FOR OSM5 SOLVENTS LISTED BY ELEMENT GDH0396
C    SM5.4R                                                             GDH0796
C                                                                       GDH0297
C AM1
      DATA HS054A/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0,-151.17D0, GDH1197
     1-270.26D0,-243.75D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1197
     2   0.00D0,   0.00D0,-34.36D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1197
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1197
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1197
     5   0.00D0,   0.00D0, 46*0.D0, 50*0.D0/                            GDH1197
C PM3
      DATA HS054P/ 0.00D0,  0.00D0,  0.00D0,  0.00D0, 0.00D0,-145.69D0, GDH1197
     1-285.67D0,-176.86D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1197
     2   0.00D0,   0.00D0,-78.18D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1197
     3  10*0.D0,   0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1197
     4   0.00D0,   0.00D0, 10*0.D0,  0.00D0,  0.00D0,  0.00D0,  0.00D0, GDH1197
     5   0.00D0,   0.00D0, 46*0.D0, 50*0.D0/                            GDH1197
C                                                                       GDH1197
C    WATER
C    SURFACE AREA DEPENDENT CORRECTION TERMS DETERMINED FROM            GDH0596
C    EMPIRIC SURFACE TENSION TERMS (SM5.2R/M)                           GDH0596
C    AND THEN THESE START AT POSITION 100
C    AND THEN CCRFN4, CCRFN5, OCRFN5, OORFN5, CNRFN4, ONRFN5, SSRFN5,   GDH1197
C    OSIBD5, BLANK, CNSTC2, TNCSC2, HNNST1, HOHST1, OPRFN5,             PDW0901
C    SPRFN5, TNCSC3, CLCRFN, AND BRCRFN FOR SM5.42R                     GDH1197
C                                                                       GDH0596
C AM1
C     DATA S054A/    99.480D0,    4*0.0D0,    113.37D0,    37.02D0,     GDH1197
C    .   -148.98D0,    44.970D0,    5*0.0D0,   -40.880D0,   -56.490D0,  GDH1197
C    .   -2.7200D0,   17*0.0D0,   -20.370D0,   17*0.0D0,   -18.820D0,   GDH1197
C    .   47*0.0D0,   -67.680D0,    16.100D0,    231.29D0,    41.820D0,  GDH1197
C    .   -69.62D0, 240.69D0,    49.210D0,    2*0.0D0,    35.020D0,      GDH1197
C    .   -238.94D0,-253.16D0, 534.56D0,149.34D0, 503.51D0,              GDH1197
C    .   -3.55D0,   34*0.0D0/                                           GDH1197
C     S054A(108), i.e., O-Si for AM1 has been removed                   !JT0102
      DATA S054A/    99.480D0,    4*0.0D0,    113.37D0,    37.02D0,     PDW0601
     .   -148.98D0,    44.970D0,4*0.0D0,68.6D0,-40.880D0, -56.490D0,    PDW0601
     .   -2.7200D0,   17*0.0D0,   -20.370D0,   17*0.0D0,   -18.820D0,   PDW0601 
     .   47*0.0D0,   -67.680D0,    16.100D0,    231.29D0,    41.820D0,  PDW0601
     .   -69.62D0, 240.69D0,  49.210D0,    0.0D0, 0.0D0, 35.020D0,      PDW0601
     .   -238.94D0,-253.16D0, 534.56D0,149.34D0, 503.51D0,              PDW0601
     .   -3.55D0,   34*0.0D0/                                      
C PM3
C     DATA S054P/99.32D0, 4*0.0D0, 103.78D0,35.42D0, -184.68D0,         GDH1197
C    . 39.23D0, 5*0.0D0, -63.31D0, -48.17D0, -0.74D0,17*0.0D0,-20.03D0, GDH1197
C    . 17*0.0D0, -18.03D0, 47*0.0D0, -66.43D0, 21.86D0,306.30D0, 23.9D0,GDH1197
C    .  -71.16D0, 370.94D0, 23.54D0, 2*0.0D0, 27.40D0, -336.91D0,       GDH1197
C    . -241.71D0, 489.79D0, 101.92D0, 284.09D0, 21.21D0, 34*0.0D0 /     GDH1197
C
C     S054P(108), i.e., O-Si for PM3 has been removed                   !JT0102
C
      DATA S054P/99.32D0, 4*0.0D0, 103.78D0,35.2D0, -184.68D0,          PDW0601
     . 39.23D0, 4*0.0D0, 35.2D0, -63.31D0, -48.17D0, -0.74D0,17*0.0D0,  PDW0601
     . -20.03D0, 17*0.0D0, -18.03D0, 47*0.0D0, -66.43D0, 21.86D0,       PDW0601
     . 306.30D0, 23.9D0, -71.16D0, 370.94D0, 23.54D0,    0.0D0, 0.0D0,  PDW0601
     . 27.40D0, -336.91D0, -241.71D0, 489.79D0, 101.92D0, 284.09D0,     PDW0601
     . 21.21D0, 34*0.0D0 /                                              PDW0601
