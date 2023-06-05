      SUBROUTINE ENPART(UHF,H,ALPHA,BETA,P,Q,COORD,*)                   CSTP
C----------------------------------------------------------*
C
C     NEW SUB. ENPART,  MODIFIED BY TSUNEO HIRANO 1986/6/3/
C
C---------------------------------------------------------*
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      PARAMETER (NATMS2 = (NUMATM*(NUMATM+1))/2)
      DIMENSION H(*), ALPHA(*), BETA(*), P(*), Q(*), COORD(3,*)
C--- DEFINED HERE, AND TO BE USED FOR ENPART-PRINT ONLY ---*
      DIMENSION EX(NATMS2,3), EMAT(NUMATM,NUMATM)
C--- END OF DIMENSION DEFINITION ----------------- BY TH --*
      LOGICAL UHF, MINDO3, AM1
      CHARACTER*160 KEYWRD
      CHARACTER*2 ENAME(NUMATM),ELEMNT
C***********************************************************************
C
C *** ENERGY PARTITIONING WITHIN THE UMINDO/3 AND UMNDO SCHEME
C     ROUTINE WRITTEN BY S.OLIVELLA, BARCELONA NOV. 1979.
C     EXTENDED TO AM1 AND PM3 BY JJPS.
C
C   ON INPUT UHF     = .TRUE. IF A U.H.F. CALCULATION.
C            H       = ONE-ELECTRON MATRIX.
C            ALPHA   = ALPHA ELECTRON DENSITY.
C            BETA    = BETA ELECTRON DENSITY.
C            P       = TOTAL ELECTRON DENSITY.
C            Q       = ATOM ELECTRON DENSITIES.
C
C    NOTHING IS CHANGED ON EXIT.
C
C@***        ENAME   = ELEMENT NAME OF ATOM, I.E. C, CL, SI ETC.
C@
C***********************************************************************
      COMMON /ONELEC/ USS(120), UPP(120), UDD(120)
      COMMON /CORE  / CORE(120)
      COMMON /IDEAS / FN1(120,10),FN2(120,10),FN3(120,10)
      COMMON /ALPHA3/ ALP3(153)
      COMMON /TWOEL3/ F03(120)
      COMMON /ALPHA / ALP(120)
      COMMON /TWOELE/ GSS(120),GSP(120),GPP(120),GP2(120),HSP(120)
     1                ,GSD(120),GPD(120),GDD(120)
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
C@
      COMMON/ELEMTS/ELEMNT(120)
C@
C     COMMON /WMATRX/ W(N2ELEC), DUMMY(N2ELEC),KDUMMY,NBAND(NUMATM)
      COMMON /WMATRX/ W(N2ELEC), DUMMY(N2ELEC*2),KDUMMY,NBAND(NUMATM)
      COMMON /KEYWRD/ KEYWRD
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      PARAMETER (MDUMY=MAXPAR*MAXPAR-NUMATM*3-NATMS2*4)
      COMMON /SCRACH/ EA(NUMATM,2),EAT(NUMATM), E(NATMS2,4),
     1XDUMY(MDUMY)
         SAVE                                                           GL0892
      MINDO3=(INDEX(KEYWRD,'MINDO').NE.0)
C@ --------------------------*
C@    ASSIGN ENAME(I)
      DO 10 I=1,NUMAT
         IA=NAT(I)
   10 ENAME(I)=ELEMNT(IA)
C@    CLEAR EMAT(I,I)
      DO 20 I=1,NUMAT
   20 EMAT(I,I)=0.0D0
C@ --------------------------*
C *** RECALCULATE THE DENSITY MATRICES IN THE UHF SCHEME
C
      LINEAR=NORBS*(NORBS+1)/2
      IF( .NOT. UHF) THEN
         DO 30 I=1,LINEAR
   30    BETA(I)=ALPHA(I)
      ENDIF
C
C *** ONE-CENTER ENERGIES
      K=0
      DO 50 I=1,NUMAT
         IA=NFIRST(I)
         IB=NLAST(I)
         NI=NAT(I)
         EA(I,1)=0.0D0
         DO 40 J=IA,IB
            K=K+J
            T=UPP(NI)
            IF(J.EQ.IA) T=USS(NI)
   40    EA(I,1)=EA(I,1)+P(K)*T
         ISS=(IA*(IA+1))/2
         EA(I,2)=0.5D0*GSS(NI)*P(ISS)*P(ISS)
     1  -0.5D0*GSS(NI)*(ALPHA(ISS)*ALPHA(ISS)+BETA(ISS)*BETA(ISS))
         IF(IA.EQ.IB) GO TO 50
         IA1=IA+1
         IA2=IA+2
         IXX=IA1*IA2/2
         IYY=IA2*IB/2
         IZZ=(IB*(IB+1))/2
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
     1+ALPHA(IZZ)*ALPHA(IZZ)+BETA(IXX)*BETA(IXX)
     2+BETA(IYY)*BETA(IYY)+BETA(IZZ)*BETA(IZZ)
         TT2=ALPHA(ISS)*(ALPHA(IXX)+ALPHA(IYY)+ALPHA(IZZ))
     1   +BETA(ISS)*(BETA(IXX)+BETA(IYY)+BETA(IZZ))
         TT3=ALPHA(IXX)*ALPHA(IYY)+ALPHA(IXX)*ALPHA(IZZ)
     1+ALPHA(IYY)*ALPHA(IZZ)+BETA(IXX)*BETA(IYY)
     2+BETA(IXX)*BETA(IZZ)+BETA(IYY)*BETA(IZZ)
         TT4=ALPHA(ISX)*ALPHA(ISX)+ALPHA(ISY)*ALPHA(ISY)
     1+ALPHA(ISZ)*ALPHA(ISZ)+BETA(ISX)*BETA(ISX)
     2+BETA(ISY)*BETA(ISY)+BETA(ISZ)*BETA(ISZ)
         TT5=ALPHA(IXY)*ALPHA(IXY)+ALPHA(IXZ)*ALPHA(IXZ)
     1+ALPHA(IYZ)*ALPHA(IYZ)+BETA(IXY)*BETA(IXY)
     2+BETA(IXZ)*BETA(IXZ)+BETA(IYZ)*BETA(IYZ)
         EA(I,2)=EA(I,2)+0.5D0*GPP(NI)*SS1+GSP(NI)*SS2
     1+GP2(NI)*SS3+HSP(NI)*SS4*2.0D0+0.5D0*(GPP(NI)-GP2(NI))*SS5*2.0D0
     2                -0.5D0*GPP(NI)*TT1-GSP(NI)*TT4-GP2(NI)*TT5-
     3        HSP(NI)*(TT2+TT4)-0.5D0*(GPP(NI)-GP2(NI))*(TT3+TT5)
   50 CONTINUE
      AM1=((INDEX(KEYWRD,'AM1').NE.0 .OR. INDEX(KEYWRD,'PM3').NE.0
     1 .OR. INDEX(KEYWRD,'RM1').NE.0)
     2 .AND. INDEX(KEYWRD,'PM3-D').EQ.0                                 LF0309
     3 .AND. INDEX(KEYWRD,'AM1-D').EQ.0)                                LF0509
      IF (DOPRNT) THEN                                                  CIO
      IF(MINDO3) THEN
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN MINDO/3'')')
      ELSEIF( (INDEX(KEYWRD,'PM3').NE.0)
     1        .AND. (INDEX(KEYWRD,'PM3-D').EQ.0) ) THEN                 LF0309
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN PM3'')')
      ELSEIF( INDEX(KEYWRD,'AM1').NE.0 ) THEN
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN AM1'')')
      ELSEIF( INDEX(KEYWRD,'RM1').NE.0 ) THEN
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN RM1'')')
      ELSEIF( INDEX(KEYWRD,'PDG').NE.0 ) THEN
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN PDG'')')
      ELSEIF( INDEX(KEYWRD,'MDG').NE.0 ) THEN
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN MDG'')')	 
      ELSEIF( INDEX(KEYWRD,'PM3-D').NE.0 ) THEN                         LF0309
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN PM3-D'')')    LF0309
      ELSEIF( INDEX(KEYWRD,'AM1-D').NE.0 ) THEN                         LF0509
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN AM1-D'')')    LF0509
      ELSEIF( INDEX(KEYWRD,'PM6').NE.0 ) THEN                           LF0809
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN PM6'')')      LF0809
      ELSE
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN MNDO'')')
      ENDIF
      ENDIF                                                             CIO
      KL=0
   60 K=KL+1
      KL=KL+10
      KL=MIN0(KL,NUMAT)
      DO 70 I=K,KL
   70 EAT(I)=EA(I,1)+EA(I,2)
      IF(NUMAT.GT.KL) GO TO 60
   80 EAU=0.0D0
      EAE=0.0D0
      DO 90 I=1,NUMAT
         EAU=EAU+EA(I,1)
   90 EAE=EAE+EA(I,2)
      TONE=EAU+EAE
C *** TWO-CENTER ENERGIES
C     RESONANCE (E(N,1)) TERMS
      N=1
      DO 110 II=2,NUMAT
         E(N,1)=0.0D0
         IA=NFIRST(II)
         IB=NLAST(II)
         IMINUS=II-1
         ONEII=1.D0
         IF(NAT(II).EQ.102)ONEII=0.D0
         DO 100 JJ=1,IMINUS
            N=N+1
            JA=NFIRST(JJ)
            JB=NLAST(JJ)
            ONEJJ=1.D0
            IF(NAT(JJ).EQ.102)ONEJJ=0.D0
            E(N,1)=0.0D0
            DO 100 I=IA,IB
               KA=(I*(I-1))/2
               DO 100 K=JA,JB
                  IK=KA+K
  100    E(N,1)=E(N,1)+2.0D0*P(IK)*H(IK)*ONEII*ONEJJ
  110 N=N+1
C
C     THE CODE THAT FOLLOWS APPLIES ONLY TO MNDO
C
      IF(.NOT.MINDO3) THEN
C     CORE-CORE REPULSION (E(N,2)) AND CORE-ELEC. ATTRACTION (E(N,3)).
         N=1
         KK=0
         DO 190 II=2,NUMAT
            E(N,2)=0.0D0
            E(N,3)=0.0D0
            IA=NFIRST(II)
            IB=NLAST(II)
            NI=NAT(II)
            ISS=(IA*(IA+1))/2
            IMINUS=II-1
            DO 180 JJ=1,IMINUS
               N=N+1
               JA=NFIRST(JJ)
               JB=NLAST(JJ)
               NJ=NAT(JJ)
               JSS=(JA*(JA+1))/2
               KK=KK+1
               G=W(KK)
               R=SQRT((COORD(1,II)-COORD(1,JJ))**2+(COORD(2,II)-COORD(2,
     1JJ))**2+  (COORD(3,II)-COORD(3,JJ))**2)
               SCALE=1.0D0+EXP(-ALP(NI)*R)+EXP(-ALP(NJ)*R)
               NT=NI+NJ
               IF(NT.LT.8.OR.NT.GT.9) GO TO 120
               IF(NI.EQ.7.OR.NI.EQ.8) SCALE=SCALE+(R-1.0D0)*EXP(-ALP(NI)
     1*R)
               IF(NJ.EQ.7.OR.NJ.EQ.8) SCALE=SCALE+(R-1.0D0)*EXP(-ALP(NJ)
     1*R)
  120          E(N,2)=CORE(NI)*CORE(NJ)*G*SCALE
               IF( AM1 )THEN
                  SCALE=0.0D0
                  DO 130 IG=1,10
                     IF(ABS(FN1(NI,IG)).GT.0.D0)
     1SCALE=SCALE +CORE(NI)*CORE(NJ)/R*
     2FN1(NI,IG)*EXP(-FN2(NI,IG)*(R-FN3(NI,IG))**2)
                     IF(ABS(FN1(NJ,IG)).GT.0.D0)
     1SCALE=SCALE +CORE(NI)*CORE(NJ)/R*
     2FN1(NJ,IG)*EXP(-FN2(NJ,IG)*(R-FN3(NJ,IG))**2)
  130             CONTINUE
                  E(N,2)=E(N,2)+SCALE
               ENDIF
               E(N,3)=-(P(ISS)*CORE(NJ)+P(JSS)*CORE(NI))*G
               IF(NJ.LT.3) GO TO 150
               KINC=9
               JAP1=JA+1
               DO 140 K=JAP1,JB
                  KC=(K*(K-1))/2
                  DO 140 L=JA,K
                     KL=KC+L
                     BB=2.0D0
                     IF(K.EQ.L) BB=1.0D0
                     KK=KK+1
  140          E(N,3)=E(N,3)-P(KL)*CORE(NI)*BB*W(KK)
               GO TO 160
  150          KINC=0
  160          IF(NI.LT.3) GO TO 180
               IAP1=IA+1
               DO 170 I=IAP1,IB
                  KA=(I*(I-1))/2
                  DO 170 J=IA,I
                     IJ=KA+J
                     AA=2.0D0
                     IF(I.EQ.J) AA=1.0D0
                     KK=KK+1
                     E(N,3)=E(N,3)-P(IJ)*CORE(NJ)*AA*W(KK)
  170          KK=KK+KINC
  180       CONTINUE
  190    N=N+1
C     COULOMB (E(N,4)) AND EXCHANGE (EX(N)) TERMS
         N=1
         KK=0
         DO 210 II=2,NUMAT
            E(N,4)=0.0D0
            EX(N,1)=0.0D0
            IA=NFIRST(II)
            IB=NLAST(II)
            IMINUS=II-1
            DO 200 JJ=1,IMINUS
               JA=NFIRST(JJ)
               JB=NLAST(JJ)
               N=N+1
               E(N,4)=0.0D0
               EX(N,1)=0.0D0
               DO 200 I=IA,IB
                  KA=(I*(I-1))/2
                  DO 200 J=IA,I
                     KB=(J*(J-1))/2
                     IJ=KA+J
                     AA=2.0D0
                     IF(I.EQ.J) AA=1.0D0
                     PIJ=P(IJ)
                     DO 200 K=JA,JB
                        KC=(K*(K-1))/2
                        IK=KA+K
                        JK=KB+K
                        DO 200 L=JA,K
                           IL=KA+L
                           JL=KB+L
                           KL=KC+L
                           BB=2.0D0
                           IF(K.EQ.L) BB=1.0D0
                           KK=KK+1
                           G=W(KK)
                           E(N,4)=E(N,4)+AA*BB*G*PIJ*P(KL)
  200       EX(N,1) = EX(N,1)
     1    -0.5D0*AA*BB*G*(ALPHA(IK)*ALPHA(JL)+ALPHA(IL)*ALPHA(JK)+
     2    BETA(IK)*BETA(JL)+BETA(IL)*BETA(JK))
  210    N=N+1
      ELSE
         N=1
         DO 270 I=2,NUMAT
            E(N,2)=0.0D0
            E(N,3)=0.0D0
            E(N,4)=0.0D0
            EX(N,1)=0.0D0
            IA=NFIRST(I)
            IB=NLAST(I)
            NI=NAT(I)
            IMINUS=I-1
            DO 260 J=1,IMINUS
               N=N+1
               JA=NFIRST(J)
               JB=NLAST(J)
               NJ=NAT(J)
               RIJ=(COORD(1,I)-COORD(1,J))**2+(COORD(2,I)-COORD(2,J))**2
     1+  (COORD(3,I)-COORD(3,J))**2
               GIJ=14.399D0/SQRT(RIJ+(7.1995D0/F03(NI)+7.1995D0/F03(NJ))
     1**2)
               PAB2=0.0D0
               IJ=MAX(NI,NJ)
               NBOND=(IJ*(IJ-1))/2+NI+NJ-IJ
               RIJ=SQRT(RIJ)
               IF(NBOND.EQ.22 .OR. NBOND .EQ. 29) GO TO 220
               GO TO 230
  220          SCALE=ALP3(NBOND)*EXP(-RIJ)
               GO TO 240
  230          SCALE=EXP(-ALP3(NBOND)*RIJ)
  240          CONTINUE
               E(N,2)=CORE(NI)*CORE(NJ)*GIJ+
     1     ABS(CORE(NI)*CORE(NJ)*(14.399D0/RIJ-GIJ)*SCALE)
               E(N,3)=(-Q(I)*CORE(NJ)-Q(J)*CORE(NI))*GIJ
               E(N,4)=Q(I)*Q(J)*GIJ
               DO 250 K=IA,IB
                  KK=(K*(K-1))/2
                  DO 250 L=JA,JB
                     LK=KK+L
  250          PAB2=PAB2+ALPHA(LK)*ALPHA(LK)+BETA(LK)*BETA(LK)
  260       EX(N,1) = -PAB2*GIJ
  270    N=N+1
      ENDIF
      NUMAT1=(NUMAT*(NUMAT+1))/2
      DO 280 I=1,4
  280 E(NUMAT1,I)=0.0D0
      DO 290 I=1,3
  290 EX(NUMAT1,I)=0.0D0
C@ --------------------------*
C-----PRINT OUT ONE AND TWO CENTER ENERGIES
C
C     E(I,1):     RESONANCE ENERGY
C     E(I,2):     NUCLEAR-NUCLEAR REPULSION ENERGY
C     E(I,3):     ELECTRON-NUCLEAR ATTRACTION ENERGY
C     E(I,4):     ELECTRON-ELECTRON REPULSION ENERGY
C     EX(I,1):    EXCHANGE  ENERGY
C     EX(I,2):    EXCHANGE + RESONANCE ENERGY
      IF (DOPRNT)                                                       CIO
     &    WRITE(6,'(//,''       ONE AND TWO CENTER ENERGIES (EV) '')')  CIO
C
      IF (DOPRNT) WRITE(6,'(/,''  [RESONANCE TERM] (EV)'')')            CIO
      CALL VECPRT(E,NUMAT,*9999)                                        CSTP(call)
C
      IF (DOPRNT) WRITE(6,'(/,''  [EXCHANGE TERM] (EV)'')')             CIO
      CALL VECPRT(EX,NUMAT,*9999)                                       CSTP(call)
C
      IF (DOPRNT) WRITE(6,'(/,''  [RESONANCE + EXCHANGE] (EV)'')')      CIO
      DO 300 N=1,NUMAT1
  300 EX(N,2) =E(N,1) + EX(N,1)
C
C   ADD IN MONOCENTRIC EXCHANGE AND COULOMBIC TERM
C
      DO 310 I=1,NUMAT
  310 EX((I*(I+1))/2,2)=EA(I,2)
      CALL VECPRT(EX(1,2),NUMAT,*9999)                                   CSTP(call)
C
      IF (DOPRNT)                                                        CIO
     &   WRITE(6,'(/,''  [ELECTRON - ELECTRON REPULSION] (EV)'')')       CIO
      CALL VECPRT(E(1,4),NUMAT,*9999)                                    CSTP(call)
C
      IF (DOPRNT)                                                        CIO
     &     WRITE(6,'(/,''  [ELECTRON-NUCLEAR ATTRACTION] (EV)'')')       CIO
      DO 320 I=1,NUMAT
  320 E((I*(I+1))/2,3)=EA(I,1)
      CALL VECPRT(E(1,3),NUMAT,*9999)                                    CSTP(call)
C
      IF (DOPRNT) WRITE(6,'(/,''  [NUCLEAR-NUCLEAR REPULSION] (EV)'')')  CIO
      CALL VECPRT(E(1,2),NUMAT,*9999)                                    CSTP(call)
C
      DO 330 N=1,NUMAT1
  330 EX(N,3) =E(N,4) + E(N,3) + E(N,2)
C     PRINT OUT OF TOTAL COULOMB TERM
      IF (DOPRNT)                                                        CIO
     &WRITE(6,'(/,''  [TOTAL COULOMB TERM (E-E, E-N, AND N-N)] (EV)'')') CIO
      CALL VECPRT(EX(1,3),NUMAT,*9999)                                   CSTP(call)
C     PRINT OUT OF TWO-CENTER SUM(OFF-DIAGONAL) +
C                  ONE-CENTER SUM(DIAGONAL).
      IF (DOPRNT)                                                       CIO
     &  WRITE(6,'(/,''  [TWO-CENTER SUM (OFF-DIAGONAL), AND  '',        CIO
     &  ''ONE-CENTER SUM (DIAGONAL)] (EV)'')')                          CIO
      DO 340 N=1,NUMAT1
  340 EX(N,3)=EX(N,3)+EX(N,2)
      CALL VECPRT(EX(1,3),NUMAT,*9999)                                  CSTP(call)
C@    TOTAL SUM
      EABR=0.0D0
      EABX=0.0D0
      EABEE=0.0D0
      EABEN=0.0D0
      EABNN=0.0D0
      DO 350 I=1,NUMAT
  350 E((I*(I+1))/2,3)=0.D0
      DO 360 I=1,NUMAT1
         EABR=EABR+E(I,1)
         EABX=EABX+EX(I,1)
         EABEE=EABEE+E(I,4)
         EABEN=EABEN+E(I,3)
         EABNN=EABNN+E(I,2)
  360 CONTINUE
      EABRX=EABR+EABX
      EABE=EABEE+EABEN+EABNN
      TTWO=EABRX+EABE
      ET=TONE+TTWO
C@ ***************************************************************
      IF (DOPRNT) THEN                                                  CIO
      WRITE(6,370)
  370 FORMAT(///,'***  SUMMARY OF ENERGY PARTITION  ***')
      WRITE(6,380)
  380 FORMAT(1H ,'---------------------------------------')
      WRITE(6,'(''     ONE-CENTER TERMS'')')
      WRITE(6,390) EAU
  390 FORMAT(/,' ELECTRON-NUCLEAR  (ONE-ELECTRON) ',F17.4,' EV')
      WRITE(6,400) EAE
  400 FORMAT(' ELECTRON-ELECTRON (TWO-ELECTRON) ',F17.4,' EV')
      WRITE(6,410) TONE
  410 FORMAT(/,' TOTAL OF ONE-CENTER TERMS        ',18X,F15.4,' EV')
      WRITE(6,380)
      WRITE(6,'(''     TWO-CENTER TERMS'')')
      WRITE(6,420) EABR
  420 FORMAT(/,' RESONANCE ENERGY',8X,F15.4,' EV')
      WRITE(6,430) EABX
  430 FORMAT(' EXCHANGE ENERGY ',8X,F15.4,' EV')
      WRITE(6,440) EABRX
  440 FORMAT(/,' EXCHANGE + RESONANCE ENERGY:       ',F15.4,' EV')
      WRITE(6,450) EABEE
  450 FORMAT(/,' ELECTRON-ELECTRON REPULSION',F12.4,' EV')
      WRITE(6,460) EABEN
  460 FORMAT(  ' ELECTRON-NUCLEAR ATTRACTION',F12.4,' EV')
      WRITE(6,470) EABNN
  470 FORMAT(  ' NUCLEAR-NUCLEAR REPULSION  ',F12.4,' EV')
      WRITE(6,480) EABE
  480 FORMAT(/,' TOTAL ELECTROSTATIC INTERACTION    ',F15.4,' EV',/)
      WRITE(6,490) TTWO
  490 FORMAT(' GRAND TOTAL OF TWO-CENTER TERMS   ',17X,F15.4,' EV')
      WRITE(6,380)
      WRITE(6,500) ET
  500 FORMAT(' ETOT (EONE + ETWO)   ',30X,F15.4,' EV'//)
      ENDIF                                                             CIO
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY ENPART_INIT                                                 CSAV
                AA = 0.0D0                                              CSAV
               AM1 = .FALSE.                                            CSAV
                BB = 0.0D0                                              CSAV
                 E = 0.0D0                                              CSAV
              EABE = 0.0D0                                              CSAV
             EABEE = 0.0D0                                              CSAV
             EABEN = 0.0D0                                              CSAV
             EABNN = 0.0D0                                              CSAV
              EABR = 0.0D0                                              CSAV
             EABRX = 0.0D0                                              CSAV
              EABX = 0.0D0                                              CSAV
               EAE = 0.0D0                                              CSAV
               EAT = 0.0D0                                              CSAV
               EAU = 0.0D0                                              CSAV
CSPD              EMAT = 0.0D0                                              CSAV
             ENAME = ""                                                 CSAV
                ET = 0.0D0                                              CSAV
CSPD                EX = 0.0D0                                              CSAV
                 G = 0.0D0                                              CSAV
               GIJ = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
                IA = 0                                                  CSAV
               IA1 = 0                                                  CSAV
               IA2 = 0                                                  CSAV
              IAP1 = 0                                                  CSAV
                IB = 0                                                  CSAV
                IG = 0                                                  CSAV
                II = 0                                                  CSAV
                IJ = 0                                                  CSAV
                IK = 0                                                  CSAV
                IL = 0                                                  CSAV
            IMINUS = 0                                                  CSAV
               ISS = 0                                                  CSAV
               ISX = 0                                                  CSAV
               ISY = 0                                                  CSAV
               ISZ = 0                                                  CSAV
               IXX = 0                                                  CSAV
               IXY = 0                                                  CSAV
               IXZ = 0                                                  CSAV
               IYY = 0                                                  CSAV
               IYZ = 0                                                  CSAV
               IZZ = 0                                                  CSAV
                 J = 0                                                  CSAV
                JA = 0                                                  CSAV
              JAP1 = 0                                                  CSAV
                JB = 0                                                  CSAV
                JJ = 0                                                  CSAV
                JK = 0                                                  CSAV
                JL = 0                                                  CSAV
               JSS = 0                                                  CSAV
                 K = 0                                                  CSAV
                KA = 0                                                  CSAV
                KB = 0                                                  CSAV
                KC = 0                                                  CSAV
              KINC = 0                                                  CSAV
                KK = 0                                                  CSAV
                KL = 0                                                  CSAV
                 L = 0                                                  CSAV
            LINEAR = 0                                                  CSAV
                LK = 0                                                  CSAV
            MINDO3 = .FALSE.                                            CSAV
                 N = 0                                                  CSAV
             NBOND = 0                                                  CSAV
                NI = 0                                                  CSAV
                NJ = 0                                                  CSAV
                NT = 0                                                  CSAV
            NUMAT1 = 0                                                  CSAV
             ONEII = 0.0D0                                              CSAV
             ONEJJ = 0.0D0                                              CSAV
              PAB2 = 0.0D0                                              CSAV
               PIJ = 0.0D0                                              CSAV
                 R = 0.0D0                                              CSAV
               RIJ = 0.0D0                                              CSAV
             SCALE = 0.0D0                                              CSAV
               SS1 = 0.0D0                                              CSAV
               SS2 = 0.0D0                                              CSAV
               SS3 = 0.0D0                                              CSAV
               SS4 = 0.0D0                                              CSAV
               SS5 = 0.0D0                                              CSAV
                 T = 0.0D0                                              CSAV
              TONE = 0.0D0                                              CSAV
               TT1 = 0.0D0                                              CSAV
               TT2 = 0.0D0                                              CSAV
               TT3 = 0.0D0                                              CSAV
               TT4 = 0.0D0                                              CSAV
               TT5 = 0.0D0                                              CSAV
              TTWO = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
