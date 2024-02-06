      SUBROUTINE ENPART(UHF,H,ALPHA,BETA,P,Q,COORD) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
      PARAMETER (NATMS2 = (NUMATM*(NUMATM+1))/2) 
      DIMENSION H(*), ALPHA(*), BETA(*), P(*), Q(*), COORD(3,*)
      LOGICAL UHF,MINDO3,AM1,IEQJ,KEQL,PM3
C ----------------------------------------------------------------------
C     ENERGY PARTITIONING OF UHF OR SCF CLOSED SHELL ENERGY (NO CI).
C     ROUTINE WRITTEN BY S.OLIVELLA, BARCELONA NOV. 1979,
C     WITHIN THE UMINDO/3 AND UMNDO SCHEME.
C INPUT UHF  = .TRUE. IF A U.H.F. CALCULATION.
C       H    = ONE-ELECTRON MATRIX.
C       ALPHA= ALPHA ELECTRON DENSITY.
C       BETA = BETA ELECTRON DENSITY.
C       P    = TOTAL ELECTRON DENSITY.
C       Q    = ATOM ELECTRON DENSITIES.
C NOTHING IS CHANGED ON EXIT.
C IMPLEMENTATION OF AM1(BORON)/PM3   D.L. JUNE 90.
C ----------------------------------------------------------------------
      COMMON /ONELEC/ USS(107),UPP(107),UDD(107)
     1       /CORE  / CORE(107)
     2       /IDEAS / FN1(107,10),FN2(107,10),FN3(107,10),NFN(107)
     3       /ALPHA3/ ALP3(153)
     4       /TWOEL3/ F03(107)
     5       /ALPHA / ALP(107)
     6       /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107)
     7               ,GSD(107),GPD(107),GDD(107)
C    8       /OPTIM / IMP,IMP0,LEC,IPRT
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
     3       /WMATRX/ W(N2ELEC*3),KDUMMY,NBAND(NUMATM)
C    5       /SCRACH/ EA(NUMATM,2),EAT(NUMATM),E(NATMS2,4),WAB(100)
C    6               ,RDUMMY(3*MORB2-NUMATM*3-4*NATMS2-100)
C
C     /SCRACH/ HAS BEEN CONVERTED TO /SCRCHR/ AND /SCRCHI/ FOR AMSOL    
      COMMON /SCRCHR/ EA(NUMATM,2),EAT(NUMATM),E(NATMS2,4),WAB(100),    3GL3092
     1                RDUMMY(6*MAXPAR**2+1-NUMATM*3-4*NATMS2-100)       GCL0393
C
      MINDO3=IMINDO.NE.0                                                DJG0995
C *** RECALCULATE THE DENSITY MATRICES IN THE UHF SCHEME
      LINEAR=NORBS*(NORBS+1)/2
      IF(.NOT.UHF) CALL SCOPY (LINEAR,ALPHA,1,BETA,1)                   GCL0393
C *** ONE-CENTRE ENERGIES
      K=0
      DO 30 I=1,NUMAT
      IA=NFIRST(I)
      IB=NLAST(I)
      NI=NAT(I)
      EA(I,1)=0.0
      DO 20 J=IA,IB
      K=K+J
      T=UPP(NI)
      IF(J.EQ.IA) T=USS(NI)
   20 EA(I,1)=EA(I,1)+P(K)*T
      ISS=IA*(IA+1)/2
      EA(I,2)=0.5*GSS(NI)*P(ISS)*P(ISS)
     .-0.5*GSS(NI)*(ALPHA(ISS)*ALPHA(ISS)+BETA(ISS)*BETA(ISS))
      IF(IA.EQ.IB) GO TO 30
      IA1=IA+1
      IA2=IA+2
      IXX=IA1*IA2/2
      IYY=IA2*IB/2
      IZZ=IB*(IB+1)/2
      IXY=IA1+IA2*IA1/2
      IXZ=IA1+IB*IA2/2
      IYZ=IA2+IB*IA2/2
      ISX=IA+IA1*IA/2
      ISY=IA+IA2*IA1/2
      ISZ=IA+IB*IA2/2
      SS1=P(IXX)*P(IXX)+P(IYY)*P(IYY)+P(IZZ)*P(IZZ)
      SS2=P(ISS)*(P(IXX)+P(IYY)+P(IZZ))
      SS3=P(IXX)*P(IYY)+P(IXX)*P(IZZ)+P(IYY)*P(IZZ)
      SS4=P(ISX)*P(ISX)+P(ISY)*P(ISY)+P(ISZ)*P(ISZ)
      SS5=P(IXY)*P(IXY)+P(IXZ)*P(IXZ)+P(IYZ)*P(IYZ)
      TT1=ALPHA(IXX)*ALPHA(IXX)+ALPHA(IYY)*ALPHA(IYY)
     1   +ALPHA(IZZ)*ALPHA(IZZ)+BETA(IXX)*BETA(IXX)
     2   +BETA(IYY)*BETA(IYY)+BETA(IZZ)*BETA(IZZ)
      TT2=ALPHA(ISS)*(ALPHA(IXX)+ALPHA(IYY)+ALPHA(IZZ))
     1   +BETA(ISS)*(BETA(IXX)+BETA(IYY)+BETA(IZZ))
      TT3=ALPHA(IXX)*ALPHA(IYY)+ALPHA(IXX)*ALPHA(IZZ)
     1   +ALPHA(IYY)*ALPHA(IZZ)+BETA(IXX)*BETA(IYY)
     2   +BETA(IXX)*BETA(IZZ)+BETA(IYY)*BETA(IZZ)
      TT4=ALPHA(ISX)*ALPHA(ISX)+ALPHA(ISY)*ALPHA(ISY)
     1   +ALPHA(ISZ)*ALPHA(ISZ)+BETA(ISX)*BETA(ISX)
     2   +BETA(ISY)*BETA(ISY)+BETA(ISZ)*BETA(ISZ)
      TT5=ALPHA(IXY)*ALPHA(IXY)+ALPHA(IXZ)*ALPHA(IXZ)
     1   +ALPHA(IYZ)*ALPHA(IYZ)+BETA(IXY)*BETA(IXY)
     2   +BETA(IXZ)*BETA(IXZ)+BETA(IYZ)*BETA(IYZ)
      EA(I,2)=EA(I,2)+0.5D0*GPP(NI)*SS1+GSP(NI)*SS2+GP2(NI)*SS3
     1   +HSP(NI)*SS4*2.0+0.5D0*(GPP(NI)-GP2(NI))*SS5*2.D0
     2   -0.5D0*GPP(NI)*TT1-GSP(NI)*TT4-GP2(NI)*TT5
     3   -HSP(NI)*(TT2+TT4)-0.5D0*(GPP(NI)-GP2(NI))*(TT3+TT5)
   30 CONTINUE
      AM1=(IAM1.NE.0)                                                   DJG0995
      PM3=(IPM3.NE.0)                                                   DJG0995
      IF (MINDO3) THEN
         WRITE(JOUT,'(/,10X,''TOTAL ENERGY PARTITIONING IN MINDO/3'')')
      ELSEIF ( AM1.OR.PM3 ) THEN
         WRITE(JOUT,'(/,10X,''TOTAL ENERGY PARTITIONING IN AM1-PM3'')')
      ELSE
         WRITE(JOUT,'(/,10X,''TOTAL ENERGY PARTITIONING IN MNDO'')')
      ENDIF
      WRITE(JOUT,'(/,'' ONE-CENTRE ENERGIES (EV)'')')
      K=1
      KL=MIN(NUMAT,7)
   40 WRITE(JOUT,50)(I,I=K,KL)
   50 FORMAT('  ATOM  ',7(I7,3X))
      WRITE(JOUT,60)(EA(I,1),I=K,KL)
   60 FORMAT('  EA U  ',7F10.3)
      WRITE(JOUT,70)(EA(I,2),I=K,KL)
   70 FORMAT('  EA E  ',7F10.3)
      DO 80 I=K,KL
   80 EAT(I)=EA(I,1)+EA(I,2)
      WRITE(JOUT,90)(EAT(I),I=K,KL)
   90 FORMAT('  TOTAL ',7F10.3)
      IF(NUMAT.LE.KL) GO TO 100
      K=KL+1
      KL=K+6
      GO TO 40
  100 EAU=0.0
      EAE=0.0
      DO 110 I=1,NUMAT
      NI=NAT(I)
      EAU=EAU+EA(I,1)
  110 EAE=EAE+EA(I,2)
      TONE=EAU+EAE
      EABE=0.0
      EABV=0.0
      EABN=0.0
      EABR=0.0
C *** TWO-CENTRE ENERGIES
C     RESONANCE TERMS
      N=1
      DO 130 II=2,NUMAT
      IA=NFIRST(II)
      IB=NLAST(II)
      DO 120 JJ=1,II-1
      N=N+1
      JA=NFIRST(JJ)
      JB=NLAST(JJ)
      E(N,1)=0.0
      DO 120 I=IA,IB
      KA=I*(I-1)/2
      DO 120 K=JA,JB
      IK=KA+K
  120 E(N,1)=E(N,1)+2.D0*P(IK)*H(IK)
  130 N=N+1
C
C     THE CODE THAT FOLLOWS APPLIES TO MNDO/AM1/PM3
C
      IF (.NOT.MINDO3) THEN
C        CORE ATTRACTION AND CORE REPULSION TERMS
         N=1
         NROW=0
         IPQRS=1
         DO 210 II=2,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
         NI=NAT(II)
         ISS=IA*(IA+1)/2
         IMINUS=II-1
         NROW=NROW+NBAND(IMINUS)
         NCOL=NBAND(II)
         NBAND2=0
         DO 200 JJ=1,IMINUS
         NBAND1=NBAND2+1
         NBAND2=NBAND2+NBAND(JJ)
         CALL WNONCA (WAB,W(IPQRS),NROW,NCOL,NBAND1,NBAND2)
         KK=0
         N=N+1
         JA=NFIRST(JJ)
         JB=NLAST(JJ)
         NJ=NAT(JJ)
         JSS=JA*(JA+1)/2
         KK=KK+1
         G=WAB(KK)*4.D0
         R=SQRT((COORD(1,II)-COORD(1,JJ))**2+(COORD(2,II)-COORD(2,JJ))**
     .        2+(COORD(3,II)-COORD(3,JJ))**2)
         SCALE=1.0+EXP(-ALP(NI)*R)+EXP(-ALP(NJ)*R)
         NT=NI+NJ
         IF(NT.LT.8.OR.NT.GT.9) GO TO 140
         IF(NI.EQ.7.OR.NI.EQ.8)SCALE=SCALE+(R-1D0)*(EXP(-ALP(NI)*R)
     .                                             +EXP(-ALP(NJ)*R))
  140    E(N,2)=CORE(NI)*CORE(NJ)*G*SCALE
         IF (AM1.OR.PM3 )THEN
C           FOR BORON, SELECT SOME SUBSET OF GAUSSIAN
            IF (NJ.EQ.5) THEN
               NNI=NJ
               NNJ=NI
            ELSE
               NNI=NI
               NNJ=NJ
            ENDIF
            IF (NNI.EQ.5) THEN
               IF (NNJ.EQ.1) THEN
C                 BORON-HYDROGEN
                  IGI1=4
                  IGI2=5
                ELSE IF (NNJ.EQ.6) THEN
C                 BORON-CARBON
                  IGI1=6
                  IGI2=7
                ELSE IF (NNJ.EQ. 9.OR.NNJ.EQ.17.OR.
     .                   NNJ.EQ.35.OR.NNJ.EQ.53) THEN
C                 BORON-HALOGEN
                  IGI1=8
                  IGI2=9
               ELSE
C                 BORON-OTHER
                  IGI1=1
                  IGI2=3
               ENDIF
            ELSE
C              NO BORON INVOLVED
               IGI1=1
               IGI2=NFN(NNI)
            ENDIF
            SCALE=0.D0
            DO 150 IG=IGI1,IGI2
  150       SCALE=SCALE+FN1(NNI,IG)*EXP(-FN2(NNI,IG)*(R-FN3(NNI,IG))**2)
            DO 151 IG=1,NFN(NNJ)
  151       SCALE=SCALE+FN1(NNJ,IG)*EXP(-FN2(NNJ,IG)*(R-FN3(NNJ,IG))**2)
            SCALE=SCALE*CORE(NNI)*CORE(NNJ)/R
            E(N,2)=E(N,2)+SCALE
         ENDIF
         E(N,3)=-(P(ISS)*CORE(NJ)+P(JSS)*CORE(NI))*G
         IF(NJ.LT.3) GO TO 170
         KINC=9
         JAP1=JA+1
         DO 160 K=JAP1,JB
         KC=K*(K-1)/2
         DO 160 L=JA,K
         KL=KC+L
         BB=2.D0
         IF(K.EQ.L) BB=1.D0
         KEQL=K.EQ.L
         KK=KK+1
         G=WAB(KK)*2.D0
         IF(KEQL) G=G*2.D0
  160    E(N,3)=E(N,3)-P(KL)*CORE(NI)*BB*G
         GO TO 180
  170    KINC=0
  180    IF(NI.LT.3) GO TO 200
         IAP1=IA+1
         DO 190 I=IAP1,IB
         KA=I*(I-1)/2
         DO 190 J=IA,I
         IJ=KA+J
         AA=2.D0
         IF(I.EQ.J) AA=1.D0
         IEQJ=I.EQ.J
         KK=KK+1
         G=WAB(KK)*2.D0
         IF (IEQJ) G=G*2.D0
         E(N,3)=E(N,3)-P(IJ)*CORE(NJ)*AA*G
  190    KK=KK+KINC
  200    CONTINUE
         IPQRS=IPQRS+NROW*NCOL
  210    N=N+1
C        COULOMB AND EXCHANGE TERMS
         N=1
         NROW=0
         IPQRS=1
         DO 230 II=2,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
         IMINUS=II-1
         NROW=NROW+NBAND(IMINUS)
         NCOL=NBAND(II)
         NBAND2=0
         DO 220 JJ=1,IMINUS
         NBAND1=NBAND2+1
         NBAND2=NBAND2+NBAND(JJ)
         CALL WNONCA (WAB,W(IPQRS),NROW,NCOL,NBAND1,NBAND2)
         KK=0
         JA=NFIRST(JJ)
         JB=NLAST(JJ)
         N=N+1
         E(N,4)=0.0
         DO 220 I=IA,IB
         KA=I*(I-1)/2
         DO 220 J=IA,I
         KB=J*(J-1)/2
         IJ=KA+J
         AA=2.D0
         IF(I.EQ.J) AA=1.D0
         IEQJ=I.EQ.J
         PIJ=P(IJ)
         DO 220 K=JA,JB
         KC=K*(K-1)/2
         IK=KA+K
         JK=KB+K
         DO 220 L=JA,K
         IL=KA+L
         JL=KB+L
         KL=KC+L
         BB=2.D0
         IF(K.EQ.L) BB=1.D0
         KEQL=K.EQ.L
         KK=KK+1
         G=WAB(KK)
         IF (IEQJ) G=G*2.D0
         IF (KEQL) G=G*2.D0
  220    E(N,4)=AA*BB*G*PIJ*P(KL)+E(N,4)
     .   -0.5D0*AA*BB*G*(ALPHA(IK)*ALPHA(JL)+ALPHA(IL)*ALPHA(JK)
     .   +BETA(IK)*BETA(JL)+BETA(IL)*BETA(JK))
         IPQRS=IPQRS+NROW*NCOL
  230    N=N+1
      ELSE
         N=1
         DO 290 I=2,NUMAT
         IA=NFIRST(I)
         IB=NLAST(I)
         NI=NAT(I)
         IMINUS=I-1
         DO 280 J=1,IMINUS
         N=N+1
         JA=NFIRST(J)
         JB=NLAST(J)
         NJ=NAT(J)
         RIJ=(COORD(1,I)-COORD(1,J))**2+(COORD(2,I)-COORD(2,J))**2
     .    +  (COORD(3,I)-COORD(3,J))**2
         GIJ=14.399D0/SQRT(RIJ+(7.1995D0/F03(NI)+7.1995D0/F03(NJ))**2)
         PAB2=0.0
         IJ=MAX(NI,NJ)
         NBOND=IJ*(IJ-1)/2+NI+NJ-IJ
         RIJ=SQRT(RIJ)
         IF(NBOND.EQ.22 .OR. NBOND.EQ.29) GO TO 240
         GO TO 250
  240    SCALE=ALP3(NBOND)*EXP(-RIJ)
         GO TO 260
  250    SCALE=EXP(-ALP3(NBOND)*RIJ)
  260    CONTINUE
         E(N,2)=CORE(NI)*CORE(NJ)*GIJ+
     1   ABS(CORE(NI)*CORE(NJ)*(14.399D0/RIJ-GIJ)*SCALE)
         E(N,3)=(-Q(I)*CORE(NJ)-Q(J)*CORE(NI))*GIJ
         E(N,4)=Q(I)*Q(J)*GIJ
         DO 270 K=IA,IB
         KK=K*(K-1)/2
         DO 270 L=JA,JB
         LK=KK+L
  270    PAB2=PAB2+ALPHA(LK)*ALPHA(LK)+BETA(LK)*BETA(LK)
  280    E(N,4)=E(N,4)-PAB2*GIJ
  290    N=N+1
      ENDIF
      XC=0.D0
      XD=0.D0
      WRITE(JOUT,300)
  300 FORMAT(/,' TWO-CENTRE ENERGIES (EV)'/3X,'E( A, B)',5X,
     11HR,9X,1HN,9X,1HV,9X,1HE,8X,5HTOTAL)
      DO 360 I=1,NUMAT
      NI=NAT(I)
      XA=0.0
      XB=0.0
      IP1=I+1
      DO 350 J=IP1,NUMAT
      NJ=NAT(J)
      IJ=I+J*(J-1)/2
      XT=E(IJ,1)+E(IJ,2)+E(IJ,3)+E(IJ,4)
      EABE=EABE+E(IJ,4)
      EABN=EABN+E(IJ,2)
      EABV=EABV+E(IJ,3)
      EABR=EABR+E(IJ,1)
      R=SQRT((COORD(1,I)-COORD(1,J))**2+(COORD(2,I)-COORD(2,J))**2
     1    +  (COORD(3,I)-COORD(3,J))**2)
      IF(R.GT.1.9D0) GO TO 310
      NT=NI+NJ
      IF(NT.EQ.2) GO TO 310
      XB=XB+XT
      GO TO 320
  310 XA=XA+XT
  320 CONTINUE
  330 WRITE(JOUT,340)I,J,(E(IJ,K),K=1,4),XT
  340 FORMAT(5X,I2,1X,I2,5F10.3)
  350 CONTINUE
      XC=XC+XA
      XD=XD+XB
  360 CONTINUE
      XT=XD+XC
      WRITE(JOUT,370)XD,XC
  370 FORMAT(' TOTAL SUM NEIGHBORING PAIR INTERACTIONS',F19.4/
     .' TOTAL SUM NONNEIGHBOR INTERACTIONS (R>1.9 A)',F14.4)
      ET=EAU+EAE+EABR+EABV+EABN+EABE
      WRITE(JOUT,380) TONE,XT
  380 FORMAT(' TOTAL SUM ONE-CENTRE ENERGIES',F29.4/
     1       ' TOTAL SUM TWO-CENTRE ENERGIES',F29.4)
      WRITE(JOUT,390)
  390 FORMAT(/,15X,'TOTAL SUMS OF ENERGY TERMS:')
      WRITE(JOUT,400) EAU,EAE,EABV,EABR,EABE,EABN
  400 FORMAT(' ONE-CENTRE CORE-ELECTRON ATTRACTION',8X,F15.4/
     1       ' ONE-CENTRE ELECTRON-ELECTRON REPULSION',5X,F15.4/
     2       ' TWO-CENTRE CORE-ELECTRON ATTRACTION',8X,F15.4/
     3       ' TWO-CENTRE CORE-ELECTRON RESONANCE',9X,F15.4/
     4       ' TWO-CENTRE ELECTRON-ELECTRON REPULSION',5X,F15.4/
     5       ' TWO-CENTRE CORE-CORE REPULSION',13X,F15.4)
      WRITE(JOUT,410) ET
  410 FORMAT(10X,'TOTAL ENERGY =',F35.4)
      IF(UHF) RETURN
      CALL SCOPY (LINEAR,P,1,ALPHA,1)                                   GCL0393
      RETURN
      END
