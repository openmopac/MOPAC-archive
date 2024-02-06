      SUBROUTINE OSM5SP
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
         INCLUDE 'KEYS.i'
C*****************************************************************************
C
C    THIS SUBROUTINE CALCULATES OSM5 SURFACE TENSIONS FROM THE
C       INDEX OF REFRACTION (FIIOFR)
C       ALPHA (FIALPH)
C       BETA (FIBETA)
C       MACROSCOPIC SURFACE TENSION (FIGAMM)
C
C       FIIOFR, FIALPH, FIBETA AND FIGAMM MUST BE INPUT BY THE USER
C       WITH THE KEYWORDS IOFR, ALPHA, BETA AND GAMMA RESPECTIVELY
C
C    AND THE ASSOCIATED SOLVENT PROPERTY DEPENDENCE PARAMETERS
C       SOLN, SOLA, SOLB, AND SOLG
C    
C       SIGMA0 = SURFACE TENSIONS (SIGMA ZERO)
C       HRFN4  = H-X SURFACE TENSIONS (SIGMA H-X)
C       CRFNH4 = C-C BOND SURFACE TENSION
C       CRFNH5 = C-C BOND SURFACE TENSION
C       OCFNH5 = O-C BOND SURFACE TENSION
C       OOFNH5 = O-O BOND SURFACE TENSION
C       CNFNH5 = N-C BOND SURFACE TENSION
C       ONFNH5 = O-N BOND SURFACE TENSION
C       SSFNH5 = S-S BOND SURFACE TENSION
C       OSIBD5 = O-Si BOND SURFACE TENSION
C       CCSIGM = CS SURFACE TENSION
C
C    CREATED BY DJG 0396 
C
C******************************************************************************
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),
     1                HBCORC(100),QKNOT(100),QKONE(100),SMXPD(100)
      COMMON /HEXSTM/ HRFN4(100),CRFNH4,CNRFN4,CORFN5,                  GDH0797
     X                CRFNH5,OORFN5,ONRFN4,SSRFN5,OPRFN5,               GDH0797
     1                FCRFN,CLCRFN,BRCRFN,CNSTC2,TNCSC3,TNCSC2,         GDH0797
     2                HOHST1,HNNST1, SPRFN5,OSIBD5                      PDW0901
      COMMON /SOLVCM/ CELEID, SLVRAD, SLVRD2, CSSIGM  
      COMMON /PROPCM/ SDEPT(10), SOLN,SOLA,SOLB,SOLG,SOLC,SOLH          GDH0897
      COMMON /PAREAS/ SDEPA(150),SDEPB(150),SDEPN(150),                 PDW0999
     X                HDEPA(150),HDEPB(150),HDEPN(150)                  PDW0999

      INCLUDE 'PARAMS.i'
      SOLN=FIIOFR
      SOLA=FIALPH
      SOLB=FIBETA
      SOLG=FIGAMM                                                       GDH0797
      SOLC=FFACAR                                                       GDH0797
      SOLH=FFEHAL                                                       GDH0797
      IF (ISM5A.EQ.1) THEN                                              DJG1296
         CALL SCOPY(108,SDEPNA,1,SDEPN,1)
         CALL SCOPY(108,SDEPAA,1,SDEPA,1)
         CALL SCOPY(108,SDEPBA,1,SDEPB,1)
         CALL SCOPY(100,HDEPNA,1,HDEPN,1)
         CALL SCOPY(150,VBLANK,1,HDEPA,1)                               GDH0797
         CALL SCOPY(100,HDEPBA,1,HDEPB,1)                               DJG0996
         DO 120 IJ=109,150                                              GDH0897
            SDEPN(IJ)=0.0D0                                             GDH0897
            SDEPA(IJ)=0.0D0                                             GDH0897
            SDEPB(IJ)=0.0D0                                             GDH0897
120      CONTINUE                                                       GDH0897
         DO 125 IJ=101,150                                              GDH0897
            HDEPN(IJ)=0.0D0                                             GDH0897
            HDEPB(IJ)=0.0D0                                             GDH0897
125      CONTINUE                                                       GDH0897
         DO 130 IJ=1,10                                                 GDH0897
            SDEPT(IJ)=0.0D0                                             GDH0897
130      CONTINUE                                                       GDH0897
         SDEPT(5)=SDEPN(108)                                            GDH0797
         SDEPT(1)=SDEPGA/100.00D0                                       GDH0797
      ELSE IF (ISM5P.EQ.1) THEN                                         DJG1296
         CALL SCOPY(108,SDEPNP,1,SDEPN,1)
         CALL SCOPY(108,SDEPAP,1,SDEPA,1)
         CALL SCOPY(108,SDEPBP,1,SDEPB,1)
         CALL SCOPY(100,HDEPNP,1,HDEPN,1)
         CALL SCOPY(150,VBLANK,1,HDEPA,1)                               GDH0797
         CALL SCOPY(100,HDEPBP,1,HDEPB,1)                               DJG0996
         DO 121 IJ=109,150                                              GDH0897
            SDEPN(IJ)=0.0D0                                             GDH0897
            SDEPA(IJ)=0.0D0                                             GDH0897
            SDEPB(IJ)=0.0D0                                             GDH0897
121      CONTINUE                                                       GDH0897
         DO 126 IJ=101,150                                              GDH0897
            HDEPN(IJ)=0.0D0                                             GDH0897
            HDEPB(IJ)=0.0D0                                             GDH0897
126      CONTINUE                                                       GDH0897
         DO 131 IJ=1,10                                                 GDH0897
            SDEPT(IJ)=0.0D0                                             GDH0897
131      CONTINUE                                                       GDH0897
         SDEPT(5)=SDEPN(108)                                            GDH0797
         SDEPT(1)=SDEPGP/100.00D0                                       GDH0797
      ELSE IF (ISM50.EQ.1) THEN                                         GDH0797
         CALL SCOPY(150,S50RNA,1,SDEPN,1)                               GDH0797
         CALL SCOPY(150,S50RAA,1,SDEPA,1)                               GDH0797
         CALL SCOPY(150,S50RBA,1,SDEPB,1)                               GDH0797
         CALL SCOPY(150,H50RNA,1,HDEPN,1)                               GDH0797
         CALL SCOPY(150,H50RAA,1,HDEPA,1)                               GDH0797
         CALL SCOPY(150,H50RBA,1,HDEPB,1)                               GDH0797
         CALL SCOPY(10,S50RTA,1,SDEPT,1)                                GDH0797
      ELSE IF (ISM52R.EQ.1.OR.ISM52.EQ.1) THEN                          PDW1199
        IF (IMNDO.EQ.1) THEN                                            GDH0997
         CALL SCOPY(150,SDMPNA,1,SDEPN,1)                               GDH0997
         CALL SCOPY(150,SDMPAA,1,SDEPA,1)                               GDH0997
         CALL SCOPY(150,SDMPBA,1,SDEPB,1)                               GDH0997
         CALL SCOPY(150,HDMPNA,1,HDEPN,1)                               GDH0997
         CALL SCOPY(150,HDMPAA,1,HDEPA,1)                               GDH0997
         CALL SCOPY(150,HDMPBA,1,HDEPB,1)                               GDH0997
         CALL SCOPY(10,SDMPTA,1,SDEPT,1)                                GDH0997
        ELSE IF (IAM1.EQ.1) THEN                                        GDH0997
         CALL SCOPY(150,SDAPNA,1,SDEPN,1)                               GDH0997
         CALL SCOPY(150,SDAPAA,1,SDEPA,1)                               GDH0997
         CALL SCOPY(150,SDAPBA,1,SDEPB,1)                               GDH0997
         CALL SCOPY(150,HDAPNA,1,HDEPN,1)                               GDH0997
         CALL SCOPY(150,HDAPAA,1,HDEPA,1)                               GDH0997
         CALL SCOPY(150,HDAPBA,1,HDEPB,1)                               GDH0997
         CALL SCOPY(10,SDAPTA,1,SDEPT,1)                                GDH0997
        ELSE IF (IPM3.EQ.1) THEN                                        GDH0997
         CALL SCOPY(150,SDPPNA,1,SDEPN,1)                               GDH0997
         CALL SCOPY(150,SDPPAA,1,SDEPA,1)                               GDH0997
         CALL SCOPY(150,SDPPBA,1,SDEPB,1)                               GDH0997
         CALL SCOPY(150,HDPPNA,1,HDEPN,1)                               GDH0997
         CALL SCOPY(150,HDPPAA,1,HDEPA,1)                               GDH0997
         CALL SCOPY(150,HDPPBA,1,HDEPB,1)                               GDH0997
         CALL SCOPY(10,SDPPTA,1,SDEPT,1)                                GDH0997
        ENDIF                                                           GDH0997
      ELSE IF ((ISM54R.EQ.1).OR.(ISM542.EQ.1)) THEN                     PDW1199 
        IF (IAM1.EQ.1) THEN                                             GDH0997
         CALL SCOPY(150,SDAPN4,1,SDEPN,1)                               GDH0997
         CALL SCOPY(150,SDAPA4,1,SDEPA,1)                               GDH0997
         CALL SCOPY(150,SDAPB4,1,SDEPB,1)                               GDH0997
         CALL SCOPY(150,HDAPN4,1,HDEPN,1)                               GDH0997
         CALL SCOPY(150,HDAPA4,1,HDEPA,1)                               GDH0997
         CALL SCOPY(150,HDAPB4,1,HDEPB,1)                               GDH0997
         CALL SCOPY(10,SDAPT4,1,SDEPT,1)                                GDH0997
        ELSE IF (IPM3.EQ.1) THEN                                        GDH0997
         CALL SCOPY(150,SDPPN4,1,SDEPN,1)                               GDH0997
         CALL SCOPY(150,SDPPA4,1,SDEPA,1)                               GDH0997
         CALL SCOPY(150,SDPPB4,1,SDEPB,1)                               GDH0997
         CALL SCOPY(150,HDPPN4,1,HDEPN,1)                               GDH0997
         CALL SCOPY(150,HDPPA4,1,HDEPA,1)                               GDH0997
         CALL SCOPY(150,HDPPB4,1,HDEPB,1)                               GDH0997
         CALL SCOPY(10,SDPPT4,1,SDEPT,1)                                GDH0997
        ENDIF                                                           GDH0997
      ELSE
         CALL SCOPY(108,SDEPNG,1,SDEPN,1)
         CALL SCOPY(108,SDEPAG,1,SDEPA,1)
         CALL SCOPY(108,SDEPBG,1,SDEPB,1)
         CALL SCOPY(100,HDEPNG,1,HDEPN,1)
         CALL SCOPY(150,VBLANK,1,HDEPA,1)                               GDH0797
         CALL SCOPY(100,HDEPBG,1,HDEPB,1)                               DJG0996
         DO 123 IJ=109,150                                              GDH0897
            SDEPN(IJ)=0.0D0                                             GDH0897
            SDEPA(IJ)=0.0D0                                             GDH0897
            SDEPB(IJ)=0.0D0                                             GDH0897
123      CONTINUE                                                       GDH0897
         DO 127 IJ=101,150                                              GDH0897
            HDEPN(IJ)=0.0D0                                             GDH0897
            HDEPB(IJ)=0.0D0                                             GDH0897
127      CONTINUE                                                       GDH0897
         DO 132 IJ=1,10                                                 GDH0897
            SDEPT(IJ)=0.0D0                                             GDH0897
132      CONTINUE                                                       GDH0897
         SDEPT(5)=SDEPN(108)                                            GDH0797
         SDEPT(1)=SDEPGG/100.00D0                                       GDH0797
      ENDIF
      DO 10 I=1,100
C        PLAIN ATOM SURFACE TENSIONS                                    GDH0797
         SIGMA0(I)=SDEPN(I)*SOLN+SDEPA(I)*SOLA+SDEPB(I)*SOLB
         HRFN4(I) =HDEPN(I)*SOLN+HDEPA(I)*SOLA+HDEPB(I)*SOLB            GDH0797
10    CONTINUE
C     C BONDED TO C(1)                                                  GDH0797
      CRFNH4=SDEPN(101)*SOLN+SDEPA(101)*SOLA+SDEPB(101)*SOLB
C     C BONDED TO C(2)                                                  GDH0797
      CRFNH5=SDEPN(102)*SOLN+SDEPA(102)*SOLA+SDEPB(102)*SOLB
C     O BONDED TO C                                                     GDH0797
      CORFN5=SDEPN(103)*SOLN+SDEPA(103)*SOLA+SDEPB(103)*SOLB
C     O BONDED TO O                                                     GDH0797
      OORFN5=SDEPN(104)*SOLN+SDEPA(104)*SOLA+SDEPB(104)*SOLB
C     N BONDED TO C(1)                                                  GDH0797
      CNRFN4=SDEPN(105)*SOLN+SDEPA(105)*SOLA+SDEPB(105)*SOLB
C     O BONDED TO N                                                     GDH0797
      ONRFN4=SDEPN(106)*SOLN+SDEPA(106)*SOLA+SDEPB(106)*SOLB
C     S BONDED TO S                                                     GDH0797
      SSRFN5=SDEPN(107)*SOLN+SDEPA(107)*SOLA+SDEPB(107)*SOLB
C
C                 THE FOLLOWING WERE ADDED AFTER SM5.4 MODELS
C
C     C BONDED TO N(1)                                                  GDH0797
      CNSTC2=SDEPN(110)*SOLN+SDEPA(110)*SOLA+SDEPB(110)*SOLB            GDH0797
C     N BONDED TO C(2)                                                  GDH0797
      TNCSC2=SDEPN(111)*SOLN+SDEPA(111)*SOLA+SDEPB(111)*SOLB            GDH0797
C     H BONDED TO N BONDED TO N                                         GDH0797
      HNNST1=SDEPN(112)*SOLN+SDEPA(112)*SOLA+SDEPB(112)*SOLB            GDH0797
C     H BONDED TO O BONDED TO H                                         GDH0797
      HOHST1=SDEPN(113)*SOLN+SDEPA(113)*SOLA+SDEPB(113)*SOLB            GDH0797
C     O BONDED TO Si                                                    PDW0901
      OSIBD5=SDEPN(108)*SOLN+SDEPA(108)*SOLA+SDEPB(108)*SOLB            PDW0901
C     O BONDED TO P                                                     GDH1197
      OPRFN5=SDEPN(114)*SOLN+SDEPA(114)*SOLA+SDEPB(114)*SOLB            GDH1197
C     S BONDED TO P                                                     GDH1197
      SPRFN5=SDEPN(115)*SOLN+SDEPA(115)*SOLA+SDEPB(115)*SOLB            GDH1197
C     N TRIPLE BONDED TO C                                              GDH1197
      TNCSC3=SDEPN(116)*SOLN+SDEPA(116)*SOLA+SDEPB(116)*SOLB            GDH1197
C     Cl BONDED TO C                                                    GDH0797
      CLCRFN=SDEPN(117)*SOLN+SDEPA(117)*SOLA+SDEPB(117)*SOLB            GDH0797
C     Br BONDED TO C                                                    GDH0797
      BRCRFN=SDEPN(118)*SOLN+SDEPA(118)*SOLA+SDEPB(118)*SOLB            GDH0797
C
C         CSSIGM=SDEPN(108)*SOLN+SDEPG*SOLG                              DJG1296
C     OVAREA
      CSSIGM=SDEPT(1)*SOLG+SDEPT(2)*SOLB*SOLB+SDEPT(3)*SOLC*SOLC        GDH0497
     .       +SDEPT(4)*SOLH*SOLH+SDEPT(5)*SOLN                          GDH0497
      RETURN
      END
