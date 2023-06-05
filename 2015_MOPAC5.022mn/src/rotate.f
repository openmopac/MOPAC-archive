      SUBROUTINE ROTATE (NI,NJ,XI,XJ,W,KR,E1B,E2A,ENUC,CUTOFF,GTERM,*)  CSTP
C***********************************************************************
C
C..IMPROVED SCALAR VERSION
C..WRITTEN BY ERNEST R. DAVIDSON, INDIANA UNIVERSITY.
C
C
C   ROTATE CALCULATES THE TWO-PARTICLE INTERACTIONS.
C
C   ON INPUT  NI     = ATOMIC NUMBER OF FIRST ATOM.
C             NJ     = ATOMIC NUMBER OF SECOND ATOM.
C             XI     = COORDINATE OF FIRST ATOM (ANGSTROMS).
C             XJ     = COORDINATE OF SECOND ATOM (ANGSTROMS).
C
C ON OUTPUT W      = ARRAY OF TWO-ELECTRON REPULSION INTEGRALS (in eV).
C           E1B,E2A= ARRAY OF ELECTRON-NUCLEAR ATTRACTION INTEGRALS,
C                    E1B = ELECTRON ON ATOM NI ATTRACTING NUCLEUS OF NJ.
C           ENUC   = NUCLEAR-NUCLEAR REPULSION TERM (in eV).
C
C
C *** THIS ROUTINE COMPUTES THE REPULSION AND NUCLEAR ATTRACTION
C     INTEGRALS OVER MOLECULAR-FRAME COORDINATES.  THE INTEGRALS OVER
C     LOCAL FRAME COORDINATES ARE EVALUATED BY SUBROUTINE REPP AND
C     STORED AS FOLLOWS (WHERE P-SIGMA = O,   AND P-PI = P AND P* )
C     IN RI
C     (SS/SS)=1,   (SO/SS)=2,   (OO/SS)=3,   (PP/SS)=4,   (SS/OS)=5,
C     (SO/SO)=6,   (SP/SP)=7,   (OO/SO)=8,   (PP/SO)=9,   (PO/SP)=10,
C     (SS/OO)=11,  (SS/PP)=12,  (SO/OO)=13,  (SO/PP)=14,  (SP/OP)=15,
C     (OO/OO)=16,  (PP/OO)=17,  (OO/PP)=18,  (PP/PP)=19,  (PO/PO)=20,
C     (PP/P*P*)=21,   (P*P/P*P)=22.
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*160 KEYWRD
      LOGICAL  SI,SJ
      include 'method.f'
      LOGICAL LOPEN                                                     LF0909
      LOGICAL HPUSED                                                    LF0210
      COMMON /DOPRNT/ DOPRNT                                            LF0312
      LOGICAL DOPRNT                                                    LF0312
      COMMON /NATORB/ NATORB(120)
      COMMON /TWOEL3/ F03(120)
      COMMON /ALPHA3/ ALP3(153)
      COMMON /ALPHA / ALP(120)
      COMMON /CORE  / TORE(120)
      COMMON /IDEAS / FN1(120,10),FN2(120,10),FN3(120,10)
      COMMON /ALPTM / ALPTM(30), EMUDTM(30)
      COMMON /ROTDUM/ CSS1,CSP1,CPPS1,CPPP1,CSS2,CSP2,CPPS2,CPPP2
      COMMON /ROTDU2/ X(3),Y(3),Z(3)
      COMMON /KEYWRD/ KEYWRD
      COMMON /W4G   / PREA(120), PREB(120), DA(120), DB(120)
      COMMON /PM3DSB/ ALPPMD,R0PMD(120),S6PMD,C6PMD(120),USSPMD(120),   LF0409
     &   UPPPMD(120),BETASD(120),BETAPD(120),ALPHAD(120),EISOLD(120)    LF0409
      COMMON /AM1DSB/ ALPAMD,R0AMD(120),S6AMD,C6AMD(120),USSAMD(120),   LF0509
     &   UPPAMD(120),BETASE(120),BETAPE(120),ALPHAE(120),EISOLE(120)    LF0509
      COMMON /MNDODS/ ALPMND,R0MND(120),S6MND,C6MND(120),USSMND(120),
     &   UPPMND(120),BETASF(120),BETAPF(120),ALPHAF(120),EISOLF(120),
     &   GSSMND(120),GSPMND(120),GPPMND(120),GP2MND(120),HSPMND(120),
     &   ZSMND(120),ZPMND(120)
      COMMON /RM1DSB/ ALPRMD,R0RMD(120),S6RMD,C6RMD(120),USSRMD(120),   LF0310
     &   UPPRMD(120),BETASI(120),BETAPI(120),ALPHAI(120),EISOLI(120)    LF0310
      COMMON /PM6DSB/ ALPP6D,R0P6D(120),S6P6D,C6P6D(120),USSP6D(120),   LF0310
     &   UPPP6D(120),BETASJ(120),BETAPJ(120),ALPHAJ(120),EISOLJ(120)    LF0310
      COMMON /PM6BLO/ USSPM6(120), UPPPM6(120), UDDPM6(120),ZSPM6(120), LF0709
     &ZPPM6(120), ZDPM6(120), BETASX(120), BETAPX(120), BETADX(120),    LF0709
     &GSSPM6(120),GSPPM6(120),GPPPM6(120),GP2PM6(120),HSPPM6(120),      LF0709
     &EISOLX(120), DDPM6(120), QQPM6(120), AMPM6(120), ADPM6(120),      LF0709
     &AQPM6(120), GAUSX1(120,10), GAUSX2(120,10), GAUSX3(120,10),       LF0709
     &ALPPM6(18,18), XPM6(18,18), ZSNPM6(18), ZPNPM6(18), ZDNPM6(18)    LF1009
      COMMON /HPUSED/ HPUSED                                            LF0210
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1010
      COMMON /IONPOT/ ATOMIP(120)                                       LF0310
C     COMMON /SCRTCH/ ANSCR(10000),IANSCP                               LF0610
      COMMON /SCRTCH/ ANSCR(5000000),IANSCP                               LF0610
      include 'corgen.f'   ! Common block declaration for /CORGEN/.     LF1010
      COMMON /SUMDISP/ DISPTOTAL, LDISPON                               LF0312
      LOGICAL LDISPON                                                   LF0412
      COMMON /TDAMPB/  CPAIR(16,16), DPAIR(16,16),                      LF0312
     &                 RPNUM, RPDEN, DPDEN, SPDEN,                      LF0312
     &                 USEDPR, USECPR                                   LF0312
      LOGICAL          USEDPR, USECPR                                   LF0312

      DIMENSION R0(120),C6(120)                                         LF0312
      DIMENSION XI(3),XJ(3),W(100),E1B(10),E2A(10)
      DIMENSION RI(22),CORE(4,2), BORON1(3,4), BORON2(3,4), BORON3(3,4)
      DIMENSION G(4),C(4)                                               LF1010
         SAVE                                                           GL0892

      EQUIVALENCE (CORE(1,1),CSS1)

      DATA BORON1/  0.182613D0,  0.118587D0, -0.073280D0,
     1              0.412253D0, -0.149917D0,  0.000000D0,
     2              0.261751D0,  0.050275D0,  0.000000D0,
     3              0.359244D0,  0.074729D0,  0.000000D0/
      DATA BORON2/  6.D0,  6.D0,  5.D0,
     1             10.D0,  6.D0,  0.D0,
     2              8.D0,  5.D0,  0.D0,
     3              9.D0,  9.D0,  0.D0/
      DATA BORON3/  0.727592D0,  1.466639D0,  1.570975D0,
     1              0.832586D0,  1.186220D0,  0.000000D0,
     2              1.063995D0,  1.936492D0,  0.000000D0,
     3              0.819351D0,  1.574414D0,  0.000000D0/
C


C ***********************************************************
C     FOR ANAYLTICAL GRADIENTS OPEN AND REWIND
C     A SCRATCH FILE THAT DELETES WHEN PROGRAM FINISHES
C ***********************************************************
      IF((.NOT.LOPEN).AND.ANALYT)THEN                                   LF0909
CIO         OPEN(UNIT=2,STATUS='SCRATCH',FORM='UNFORMATTED')
CIO         REWIND 2
         IANSCP=1                                                       LF0610 / CIO
         LOPEN=.TRUE.
      ENDIF
C


C ***********************************************************
C     BEGIN GETTING INTERNUCLEAR DIRECTED DISTANCES INFO
C ***********************************************************
      X(1)=XI(1)-XJ(1)
      X(2)=XI(2)-XJ(2)
      X(3)=XI(3)-XJ(3)
      RIJ=X(1)*X(1)+X(2)*X(2)+X(3)*X(3)

C ***********************************************************
C     CASE OF INTERNUCLEAR DISTANCE TOO SMALL (<0.2 ANGSTROMS)
C ***********************************************************
      IF (RIJ.LT.0.002D0) THEN
C
C     SMALL RIJ CASE
C
         DO 10 I=1,10
            E1B(I)=0.D0
            E2A(I)=0.D0
   10    CONTINUE
         W(KR)=0.D0
         ENUC=0.D0
         RETURN
      ENDIF


 
C ***********************************************************
C     MINDO CASE
C ***********************************************************
      IF (LMINDO) THEN
C
         SUM=14.399D0/DSQRT(RIJ+(7.1995D0/F03(NI)+7.1995D0/F03(NJ))**2)
         W(1)=SUM
         KR=KR+1
         DO 20 L=1,10
            E1B(L)=0.D0
            E2A(L)=0.D0
   20    CONTINUE
         E1B(1) = -SUM*TORE(NJ)
         E1B(3) = E1B(1)
         E1B(6) = E1B(1)
         E1B(10)= E1B(1)
         E2A(1) = -SUM*TORE(NI)
         E2A(3) = E2A(1)
         E2A(6) = E2A(1)
         E2A(10)= E2A(1)
         II = MAX(NI,NJ)
         NBOND = (II*(II-1))/2+NI+NJ-II
         RIJ = DSQRT(RIJ)
         IF(NBOND.LT.154) THEN
            IF(NBOND.EQ.22 .OR. NBOND .EQ. 29) THEN
C              NBOND = 22 IS C-H CASE
C              NBOND = 29 IS N-H CASE
               SCALE=ALP3(NBOND)*EXP(-RIJ)
            ELSE
C              NBOND < 154  IS NI < 18 AND NJ < 18 CASE
               SCALE=EXP(-ALP3(NBOND)*RIJ)
            ENDIF
         ELSE
C              NBOND > 154 INVOLVES NI OR NJ > 18
            SCALE = 0.D0                                                LF0110
            IF(NATORB(NI).EQ.0) SCALE=      EXP(-ALP(NI)*RIJ)
            IF(NATORB(NJ).EQ.0) SCALE=SCALE+EXP(-ALP(NI)*RIJ)
         ENDIF
         IF (ABS(TORE(NI)).GT.20.D0 .AND. ABS(TORE(NJ)).GT.20.D0) THEN
            ENUC=0.D0
         ELSE IF (RIJ.LT.1.D0 .AND. NATORB(NI)*NATORB(NJ).EQ.0) THEN
            ENUC=0.D0
         ELSE
            ENUC = TORE(NI)*TORE(NJ)*SUM
     &       + ABS(TORE(NI)*TORE(NJ)*(14.399D0/RIJ-SUM)*SCALE)
         ENDIF
         RETURN
      ENDIF


C ***********************************************************
C     MNDO, AM1, PM3, AND ALL OTHER SUCH CASES
C ***********************************************************

         RIJX = DSQRT(RIJ)
         RIJ = MIN(RIJX,CUTOFF)

C ***********************************************************
C     COMPUTE INTEGRALS IN DIATOMIC FRAME
C ***********************************************************

C *** THE REPULSION INTEGRALS OVER MOLECULAR FRAME (W) ARE STORED IN THE
C     ORDER IN WHICH THEY WILL LATER BE USED.  I.E.  (I,J/K,L) WHERE
C     J.LE.I  AND  L.LE.K     AND L VARIES MOST RAPIDLY AND I LEAST
C     RAPIDLY.  (ANTI-NORMAL COMPUTER STORAGE)
C

         CALL REPP(NI,NJ,RIJ,RI,CORE,*9999)                             CSTP
CIO         IF(ANALYT)WRITE(2)(RI(I),I=1,22)
         IF(ANALYT) THEN                                                LF0610 / CIO
           ANSCR(IANSCP:IANSCP+21)=RI(1:22)                             LF0610 / CIO
           IANSCP=IANSCP+22                                             LF0610 / CIO
         ENDIF                                                          LF0610 / CIO
C          .. WRITE THE TWO-ELECTRON INTEGRALS TO SCRATCH FILE

         GAM = RI(1)
C          .. THE (S S/S S) INTEGRAL TO BE USED LATER

 
         A=1.D0/RIJX
         X(1) = X(1)*A
         X(2) = X(2)*A
         X(3) = X(3)*A
         IF (ABS(X(3)).GT.0.99999999D0) THEN
            X(3) = SIGN(1.D0,X(3))
            Y(1) = 0.D0
            Y(2) = 1.D0
            Y(3) = 0.D0
            Z(1) = 1.D0
            Z(2) = 0.D0
            Z(3) = 0.D0
         ELSE
            Z(3)=SQRT(1.D0-X(3)*X(3))
            A=1.D0/Z(3)
            Y(1)=-A*X(2)*SIGN(1.D0,X(1))
            Y(2)=ABS(A*X(1))
            Y(3)=0.D0
            Z(1)=-A*X(1)*X(3)
            Z(2)=-A*X(2)*X(3)
         ENDIF
         SI = (NATORB(NI).GT.1)
         SJ = (NATORB(NJ).GT.1)
         IF ( SI .OR. SJ) THEN
C           .. AT LEAST ONE IS NON-HYDROGEN ..
            XX11 = X(1)*X(1)
            XX21 = X(2)*X(1)
            XX22 = X(2)*X(2)
            XX31 = X(3)*X(1)
            XX32 = X(3)*X(2)
            XX33 = X(3)*X(3)
            YY11 = Y(1)*Y(1)
            YY21 = Y(2)*Y(1)
            YY22 = Y(2)*Y(2)
            ZZ11 = Z(1)*Z(1)
            ZZ21 = Z(2)*Z(1)
            ZZ22 = Z(2)*Z(2)
            ZZ31 = Z(3)*Z(1)
            ZZ32 = Z(3)*Z(2)
            ZZ33 = Z(3)*Z(3)
            YYZZ11 = YY11+ZZ11
            YYZZ21 = YY21+ZZ21
            YYZZ22 = YY22+ZZ22
            XY11 = 2.D0*X(1)*Y(1)
            XY21 =      X(1)*Y(2)+X(2)*Y(1)
            XY22 = 2.D0*X(2)*Y(2)
            XY31 =      X(3)*Y(1)
            XY32 =      X(3)*Y(2)
            XZ11 = 2.D0*X(1)*Z(1)
            XZ21 =      X(1)*Z(2)+X(2)*Z(1)
            XZ22 = 2.D0*X(2)*Z(2)
            XZ31 =      X(1)*Z(3)+X(3)*Z(1)
            XZ32 =      X(2)*Z(3)+X(3)*Z(2)
            XZ33 = 2.D0*X(3)*Z(3)
            YZ11 = 2.D0*Y(1)*Z(1)
            YZ21 =      Y(1)*Z(2)+Y(2)*Z(1)
            YZ22 = 2.D0*Y(2)*Z(2)
            YZ31 =      Y(1)*Z(3)
            YZ32 =      Y(2)*Z(3)
         ENDIF


C     (S S/S S)
         W(1)=RI(1)
         KI = 1


         IF (SJ) THEN
C        .. ATOM 2 IS NON-HYDROGEN ..
C     (S S/PX S)
            W(2)=RI(5)*X(1)
C     (S S/PX PX)
            W(3)=RI(11)*XX11+RI(12)*YYZZ11
C     (S S/PY S)
            W(4)=RI(5)*X(2)
C     (S S/PY PX)
            W(5)=RI(11)*XX21+RI(12)*YYZZ21
C     (S S/PY PY)
            W(6)=RI(11)*XX22+RI(12)*YYZZ22
C     (S S/PZ S)
            W(7)=RI(5)*X(3)
C     (S S/PZ PX)
            W(8)=RI(11)*XX31+RI(12)*ZZ31
C     (S S/PZ PY)
            W(9)=RI(11)*XX32+RI(12)*ZZ32
C     (S S/PZ PZ)
            W(10)=RI(11)*XX33+RI(12)*ZZ33
            KI = 10
         ENDIF


         IF (SI) THEN
C          .. ATOM 1 IS NON-HYDROGEN ..
C     (PX S/S S)
            W(11)=RI(2)*X(1)


            IF (SJ) THEN
C             .. BOTH ATOMS ARE NON-HYDROGEN ..
C     (PX S/PX S)
               W(12)=RI(6)*XX11+RI(7)*YYZZ11
C     (PX S/PX PX)
               W(13)=X(1)*(RI(13)*XX11+RI(14)*YYZZ11)
     1           +RI(15)*(Y(1)*XY11+Z(1)*XZ11)
C     (PX S/PY S)
               W(14)=RI(6)*XX21+RI(7)*YYZZ21
C     (PX S/PY PX)
               W(15)=X(1)*(RI(13)*XX21+RI(14)*YYZZ21)
     1           +RI(15)*(Y(1)*XY21+Z(1)*XZ21)
C     (PX S/PY PY)
               W(16)=X(1)*(RI(13)*XX22+RI(14)*YYZZ22)
     1           +RI(15)*(Y(1)*XY22+Z(1)*XZ22)
C     (PX S/PZ S)
               W(17)=RI(6)*XX31+RI(7)*ZZ31
C     (PX S/PZ PX)
               W(18)=X(1)*(RI(13)*XX31+RI(14)*ZZ31)
     1           +RI(15)*(Y(1)*XY31+Z(1)*XZ31)
C     (PX S/PZ PY)
               W(19)=X(1)*(RI(13)*XX32+RI(14)*ZZ32)
     1           +RI(15)*(Y(1)*XY32+Z(1)*XZ32)
C     (PX S/PZ PZ)
               W(20)=X(1)*(RI(13)*XX33+RI(14)*ZZ33)
     1           +RI(15)*(          Z(1)*XZ33)
C     (PX PX/S S)
               W(21)=RI(3)*XX11+RI(4)*YYZZ11
C     (PX PX/PX S)
               W(22)=X(1)*(RI(8)*XX11+RI(9)*YYZZ11)
     1           +RI(10)*(Y(1)*XY11+Z(1)*XZ11)
C     (PX PX/PX PX)
               W(23) =
     1     (RI(16)*XX11+RI(17)*YYZZ11)*XX11+RI(18)*XX11*YYZZ11
     2     +RI(19)*(YY11*YY11+ZZ11*ZZ11)
     3     +RI(20)*(XY11*XY11+XZ11*XZ11)
     4     +RI(21)*(YY11*ZZ11+ZZ11*YY11)
     5     +RI(22)*YZ11*YZ11
C     (PX PX/PY S)
               W(24)=X(2)*(RI(8)*XX11+RI(9)*YYZZ11)
     1           +RI(10)*(Y(2)*XY11+Z(2)*XZ11)
C     (PX PX/PY PX)
               W(25) =
     1     (RI(16)*XX11+RI(17)*YYZZ11)*XX21+RI(18)*XX11*YYZZ21
     2     +RI(19)*(YY11*YY21+ZZ11*ZZ21)
     3     +RI(20)*(XY11*XY21+XZ11*XZ21)
     4     +RI(21)*(YY11*ZZ21+ZZ11*YY21)
     5     +RI(22)*YZ11*YZ21
C     (PX PX/PY PY)
               W(26) =
     1     (RI(16)*XX11+RI(17)*YYZZ11)*XX22+RI(18)*XX11*YYZZ22
     2     +RI(19)*(YY11*YY22+ZZ11*ZZ22)
     3     +RI(20)*(XY11*XY22+XZ11*XZ22)
     4     +RI(21)*(YY11*ZZ22+ZZ11*YY22)
     5     +RI(22)*YZ11*YZ22
C     (PX PX/PZ S)
               W(27)=X(3)*(RI(8)*XX11+RI(9)*YYZZ11)
     1           +RI(10)*(         +Z(3)*XZ11)
C     (PX PX/PZ PX)
               W(28) =
     1      (RI(16)*XX11+RI(17)*YYZZ11)*XX31
     2     +(RI(18)*XX11+RI(19)*ZZ11+RI(21)*YY11)*ZZ31
     3     +RI(20)*(XY11*XY31+XZ11*XZ31)
     4     +RI(22)*YZ11*YZ31
C     (PX PX/PZ PY)
               W(29) =
     1      (RI(16)*XX11+RI(17)*YYZZ11)*XX32
     2     +(RI(18)*XX11+RI(19)*ZZ11+RI(21)*YY11)*ZZ32
     3     +RI(20)*(XY11*XY32+XZ11*XZ32)
     4     +RI(22)*YZ11*YZ32
C     (PX PX/PZ PZ)
               W(30) =
     1      (RI(16)*XX11+RI(17)*YYZZ11)*XX33
     2     +(RI(18)*XX11+RI(19)*ZZ11+RI(21)*YY11)*ZZ33
     3     +RI(20)*XZ11*XZ33
C     (PY S/S S)
               W(31)=RI(2)*X(2)
C     (PY S/PX S)
               W(32)=RI(6)*XX21+RI(7)*YYZZ21
C     (PY S/PX PX)
               W(33)=X(2)*(RI(13)*XX11+RI(14)*YYZZ11)
     1           +RI(15)*(Y(2)*XY11+Z(2)*XZ11)
C     (PY S/PY S)
               W(34)=RI(6)*XX22+RI(7)*YYZZ22
C     (PY S/PY PX)
               W(35)=X(2)*(RI(13)*XX21+RI(14)*YYZZ21)
     1           +RI(15)*(Y(2)*XY21+Z(2)*XZ21)
C     (PY S/PY PY)
               W(36)=X(2)*(RI(13)*XX22+RI(14)*YYZZ22)
     1           +RI(15)*(Y(2)*XY22+Z(2)*XZ22)
C     (PY S/PZ S)
               W(37)=RI(6)*XX32+RI(7)*ZZ32
C     (PY S/PZ PX)
               W(38)=X(2)*(RI(13)*XX31+RI(14)*ZZ31)
     1           +RI(15)*(Y(2)*XY31+Z(2)*XZ31)
C     (PY S/PZ PY)
               W(39)=X(2)*(RI(13)*XX32+RI(14)*ZZ32)
     1           +RI(15)*(Y(2)*XY32+Z(2)*XZ32)
C     (PY S/PZ PZ)
               W(40)=X(2)*(RI(13)*XX33+RI(14)*ZZ33)
     1           +RI(15)*(         +Z(2)*XZ33)
C     (PY PX/S S)
               W(41)=RI(3)*XX21+RI(4)*YYZZ21
C     (PY PX/PX S)
               W(42)=X(1)*(RI(8)*XX21+RI(9)*YYZZ21)
     1           +RI(10)*(Y(1)*XY21+Z(1)*XZ21)
C     (PY PX/PX PX)
               W(43) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX11+RI(18)*XX21*YYZZ11
     2     +RI(19)*(YY21*YY11+ZZ21*ZZ11)
     3     +RI(20)*(XY21*XY11+XZ21*XZ11)
     4     +RI(21)*(YY21*ZZ11+ZZ21*YY11)
     5     +RI(22)*YZ21*YZ11
C     (PY PX/PY S)
               W(44)=X(2)*(RI(8)*XX21+RI(9)*YYZZ21)
     1           +RI(10)*(Y(2)*XY21+Z(2)*XZ21)
C     (PY PX/PY PX)
               W(45) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX21+RI(18)*XX21*YYZZ21
     2     +RI(19)*(YY21*YY21+ZZ21*ZZ21)
     3     +RI(20)*(XY21*XY21+XZ21*XZ21)
     4     +RI(21)*(YY21*ZZ21+ZZ21*YY21)
     5     +RI(22)*YZ21*YZ21
C     (PY PX/PY PY)
               W(46) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX22+RI(18)*XX21*YYZZ22
     2     +RI(19)*(YY21*YY22+ZZ21*ZZ22)
     3     +RI(20)*(XY21*XY22+XZ21*XZ22)
     4     +RI(21)*(YY21*ZZ22+ZZ21*YY22)
     5     +RI(22)*YZ21*YZ22
C     (PY PX/PZ S)
               W(47)=X(3)*(RI(8)*XX21+RI(9)*YYZZ21)
     1           +RI(10)*(         +Z(3)*XZ21)
C      (PY PX/PZ PX)
               W(48) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX31
     2     +(RI(18)*XX21+RI(19)*ZZ21+RI(21)*YY21)*ZZ31
     3     +RI(20)*(XY21*XY31+XZ21*XZ31)
     4     +RI(22)*YZ21*YZ31
C      (PY PX/PZ PY)
               W(49) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX32
     2     +(RI(18)*XX21+RI(19)*ZZ21+RI(21)*YY21)*ZZ32
     3     +RI(20)*(XY21*XY32+XZ21*XZ32)
     4     +RI(22)*YZ21*YZ32
C      (PY PX/PZ PZ)
               W(50) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX33
     2     +(RI(18)*XX21+RI(19)*ZZ21+RI(21)*YY21)*ZZ33
     3     +RI(20)*XZ21*XZ33
C     (PY PY/S S)
               W(51)=RI(3)*XX22+RI(4)*YYZZ22
C     (PY PY/PX S)
               W(52)=X(1)*(RI(8)*XX22+RI(9)*YYZZ22)
     1           +RI(10)*(Y(1)*XY22+Z(1)*XZ22)
C      (PY PY/PX PX)
               W(53) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX11+RI(18)*XX22*YYZZ11
     2     +RI(19)*(YY22*YY11+ZZ22*ZZ11)
     3     +RI(20)*(XY22*XY11+XZ22*XZ11)
     4     +RI(21)*(YY22*ZZ11+ZZ22*YY11)
     5     +RI(22)*YZ22*YZ11
C     (PY PY/PY S)
               W(54)=X(2)*(RI(8)*XX22+RI(9)*YYZZ22)
     1           +RI(10)*(Y(2)*XY22+Z(2)*XZ22)
C      (PY PY/PY PX)
               W(55) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX21+RI(18)*XX22*YYZZ21
     2     +RI(19)*(YY22*YY21+ZZ22*ZZ21)
     3     +RI(20)*(XY22*XY21+XZ22*XZ21)
     4     +RI(21)*(YY22*ZZ21+ZZ22*YY21)
     5     +RI(22)*YZ22*YZ21
C      (PY PY/PY PY)
               W(56) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX22+RI(18)*XX22*YYZZ22
     2     +RI(19)*(YY22*YY22+ZZ22*ZZ22)
     3     +RI(20)*(XY22*XY22+XZ22*XZ22)
     4     +RI(21)*(YY22*ZZ22+ZZ22*YY22)
     5     +RI(22)*YZ22*YZ22
C     (PY PY/PZ S)
               W(57)=X(3)*(RI(8)*XX22+RI(9)*YYZZ22)
     1           +RI(10)*(         +Z(3)*XZ22)
C      (PY PY/PZ PX)
               W(58) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX31
     2     +(RI(18)*XX22+RI(19)*ZZ22+RI(21)*YY22)*ZZ31
     3     +RI(20)*(XY22*XY31+XZ22*XZ31)
     4     +RI(22)*YZ22*YZ31
C      (PY PY/PZ PY)
               W(59) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX32
     2     +(RI(18)*XX22+RI(19)*ZZ22+RI(21)*YY22)*ZZ32
     3     +RI(20)*(XY22*XY32+XZ22*XZ32)
     4     +RI(22)*YZ22*YZ32
C      (PY PY/PZ PZ)
               W(60) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX33
     2     +(RI(18)*XX22+RI(19)*ZZ22+RI(21)*YY22)*ZZ33
     3     +RI(20)*XZ22*XZ33
C     (PZ S/SS)
               W(61)=RI(2)*X(3)
C     (PZ S/PX S)
               W(62)=RI(6)*XX31+RI(7)*ZZ31
C     (PZ S/PX PX)
               W(63)=X(3)*(RI(13)*XX11+RI(14)*YYZZ11)
     1           +RI(15)*(         +Z(3)*XZ11)
C     (PZ S/PY S)
               W(64)=RI(6)*XX32+RI(7)*ZZ32
C     (PZ S/PY PX)
               W(65)=X(3)*(RI(13)*XX21+RI(14)*YYZZ21)
     1           +RI(15)*(         +Z(3)*XZ21)
C     (PZ S/PY PY)
               W(66)=X(3)*(RI(13)*XX22+RI(14)*YYZZ22)
     1           +RI(15)*(         +Z(3)*XZ22)
C     (PZ S/PZ S)
               W(67)=RI(6)*XX33+RI(7)*ZZ33
C     (PZ S/PZ PX)
               W(68)=X(3)*(RI(13)*XX31+RI(14)*ZZ31)
     1           +RI(15)*(         +Z(3)*XZ31)
C     (PZ S/PZ PY)
               W(69)=X(3)*(RI(13)*XX32+RI(14)*ZZ32)
     1           +RI(15)*(         +Z(3)*XZ32)
C     (PZ S/PZ PZ)
               W(70)=X(3)*(RI(13)*XX33+RI(14)*ZZ33)
     1           +RI(15)*(         +Z(3)*XZ33)
C     (PZ PX/S S)
               W(71)=RI(3)*XX31+RI(4)*ZZ31
C     (PZ PX/PX S)
               W(72)=X(1)*(RI(8)*XX31+RI(9)*ZZ31)
     1           +RI(10)*(Y(1)*XY31+Z(1)*XZ31)
C      (PZ PX/PX PX)
               W(73) =
     1     (RI(16)*XX31+RI(17)*ZZ31)*XX11+RI(18)*XX31*YYZZ11
     2     +RI(19)*ZZ31*ZZ11
     3     +RI(20)*(XY31*XY11+XZ31*XZ11)
     4     +RI(21)*ZZ31*YY11
     5     +RI(22)*YZ31*YZ11
C     (PZ PX/PY S)
               W(74)=X(2)*(RI(8)*XX31+RI(9)*ZZ31)
     1           +RI(10)*(Y(2)*XY31+Z(2)*XZ31)
C      (PZ PX/PY PX)
               W(75) =
     1     (RI(16)*XX31+RI(17)*ZZ31)*XX21+RI(18)*XX31*YYZZ21
     2     +RI(19)*ZZ31*ZZ21
     3     +RI(20)*(XY31*XY21+XZ31*XZ21)
     4     +RI(21)*ZZ31*YY21
     5     +RI(22)*YZ31*YZ21
C      (PZ PX/PY PY)
               W(76) =
     1     (RI(16)*XX31+RI(17)*ZZ31)*XX22+RI(18)*XX31*YYZZ22
     2     +RI(19)*ZZ31*ZZ22
     3     +RI(20)*(XY31*XY22+XZ31*XZ22)
     4     +RI(21)*ZZ31*YY22
     5     +RI(22)*YZ31*YZ22
C     (PZ PX/PZ S)
               W(77)=X(3)*(RI(8)*XX31+RI(9)*ZZ31)
     1           +RI(10)*(         +Z(3)*XZ31)
C     (PZ PX/PZ PX)
               W(78) =
     1      (RI(16)*XX31+RI(17)*ZZ31)*XX31
     2     +(RI(18)*XX31+RI(19)*ZZ31)*ZZ31
     3     +RI(20)*(XY31*XY31+XZ31*XZ31)
     4     +RI(22)*YZ31*YZ31
C      (PZ PX/PZ PY)
               W(79) =
     1      (RI(16)*XX31+RI(17)*ZZ31)*XX32
     2     +(RI(18)*XX31+RI(19)*ZZ31)*ZZ32
     3     +RI(20)*(XY31*XY32+XZ31*XZ32)
     4     +RI(22)*YZ31*YZ32
C      (PZ PX/PZ PZ)
               W(80) =
     1      (RI(16)*XX31+RI(17)*ZZ31)*XX33
     2     +(RI(18)*XX31+RI(19)*ZZ31)*ZZ33
     3     +RI(20)*XZ31*XZ33
C     (PZ PY/S S)
               W(81)=RI(3)*XX32+RI(4)*ZZ32
C     (PZ PY/PX S)
               W(82)=X(1)*(RI(8)*XX32+RI(9)*ZZ32)
     1           +RI(10)*(Y(1)*XY32+Z(1)*XZ32)
C      (PZ PY/PX PX)
               W(83) =
     1     (RI(16)*XX32+RI(17)*ZZ32)*XX11+RI(18)*XX32*YYZZ11
     2     +RI(19)*ZZ32*ZZ11
     3     +RI(20)*(XY32*XY11+XZ32*XZ11)
     4     +RI(21)*ZZ32*YY11
     5     +RI(22)*YZ32*YZ11
C     (PZ PY/PY S)
               W(84)=X(2)*(RI(8)*XX32+RI(9)*ZZ32)
     1           +RI(10)*(Y(2)*XY32+Z(2)*XZ32)
C      (PZ PY/PY PX)
               W(85) =
     1     (RI(16)*XX32+RI(17)*ZZ32)*XX21+RI(18)*XX32*YYZZ21
     2     +RI(19)*ZZ32*ZZ21
     3     +RI(20)*(XY32*XY21+XZ32*XZ21)
     4     +RI(21)*ZZ32*YY21
     5     +RI(22)*YZ32*YZ21
C      (PZ PY/PY PY)
               W(86) =
     1     (RI(16)*XX32+RI(17)*ZZ32)*XX22+RI(18)*XX32*YYZZ22
     2     +RI(19)*ZZ32*ZZ22
     3     +RI(20)*(XY32*XY22+XZ32*XZ22)
     4     +RI(21)*ZZ32*YY22
     5     +RI(22)*YZ32*YZ22
C     (PZ PY/PZ S)
               W(87)=X(3)*(RI(8)*XX32+RI(9)*ZZ32)
     1           +RI(10)*(         +Z(3)*XZ32)
C      (PZ PY/PZ PX)
               W(88) =
     1      (RI(16)*XX32+RI(17)*ZZ32)*XX31
     2     +(RI(18)*XX32+RI(19)*ZZ32)*ZZ31
     3     +RI(20)*(XY32*XY31+XZ32*XZ31)
     4     +RI(22)*YZ32*YZ31
C      (PZ PY/PZ PY)
               W(89) =
     1      (RI(16)*XX32+RI(17)*ZZ32)*XX32
     2     +(RI(18)*XX32+RI(19)*ZZ32)*ZZ32
     3     +RI(20)*(XY32*XY32+XZ32*XZ32)
     4     +RI(22)*YZ32*YZ32
C       (PZ PY/PZ PZ)
               W(90) =
     1      (RI(16)*XX32+RI(17)*ZZ32)*XX33
     2     +(RI(18)*XX32+RI(19)*ZZ32)*ZZ33
     3     +RI(20)*XZ32*XZ33
C     (PZ PZ/S S)
               W(91)=RI(3)*XX33+RI(4)*ZZ33
C     (PZ PZ/PX S)
               W(92)=X(1)*(RI(8)*XX33+RI(9)*ZZ33)
     1           +RI(10)*(          Z(1)*XZ33)
C       (PZ PZ/PX PX)
               W(93) =
     1     (RI(16)*XX33+RI(17)*ZZ33)*XX11+RI(18)*XX33*YYZZ11
     2     +RI(19)*ZZ33*ZZ11
     3     +RI(20)*XZ33*XZ11
     4     +RI(21)*ZZ33*YY11
C     (PZ PZ/PY S)
               W(94)=X(2)*(RI(8)*XX33+RI(9)*ZZ33)
     1           +RI(10)*(         +Z(2)*XZ33)
C       (PZ PZ/PY PX)
               W(95) =
     1     (RI(16)*XX33+RI(17)*ZZ33)*XX21+RI(18)*XX33*YYZZ21
     2     +RI(19)*ZZ33*ZZ21
     3     +RI(20)*XZ33*XZ21
     4     +RI(21)*ZZ33*YY21
C       (PZ PZ/PY PY)
               W(96) =
     1     (RI(16)*XX33+RI(17)*ZZ33)*XX22+RI(18)*XX33*YYZZ22
     2     +RI(19)*ZZ33*ZZ22
     3     +RI(20)*XZ33*XZ22
     4     +RI(21)*ZZ33*YY22
C     (PZ PZ/PZ S)
               W(97)=X(3)*(RI(8)*XX33+RI(9)*ZZ33)
     1           +RI(10)*(         +Z(3)*XZ33)
C       (PZ PZ/PZ PX)
               W(98) =
     1      (RI(16)*XX33+RI(17)*ZZ33)*XX31
     2     +(RI(18)*XX33+RI(19)*ZZ33)*ZZ31
     3     +RI(20)*XZ33*XZ31
C       (PZ PZ/PZ PY)
               W(99) =
     1      (RI(16)*XX33+RI(17)*ZZ33)*XX32
     2     +(RI(18)*XX33+RI(19)*ZZ33)*ZZ32
     3     +RI(20)*XZ33*XZ32
C       (PZ PZ/PZ PZ)
               W(100) =
     1      (RI(16)*XX33+RI(17)*ZZ33)*XX33
     2     +(RI(18)*XX33+RI(19)*ZZ33)*ZZ33
     3     +RI(20)*XZ33*XZ33
               KI = 100


            ELSE
C             .. ATOM 1 IS NON-HYDROGEN, ATOM 2 IS HYDROGEN ..
C     (PX S/S S)
               W(2)=RI(2)*X(1)
C     (PX PX/S S)
               W(3)=RI(3)*XX11+RI(4)*YYZZ11
C     (PY S/S S)
               W(4)=RI(2)*X(2)
C     (PY PX/S S)
               W(5)=RI(3)*XX21+RI(4)*YYZZ21
C     (PY PY/S S)
               W(6)=RI(3)*XX22+RI(4)*YYZZ22
C     (PZ S/SS)
               W(7)=RI(2)*X(3)
C     (PZ PX/S S)
               W(8)=RI(3)*XX31+RI(4)*ZZ31
C     (PZ PY/S S)
               W(9)=RI(3)*XX32+RI(4)*ZZ32
C     (PZ PZ/S S)
               W(10)=RI(3)*XX33+RI(4)*ZZ33
               KI = 10
            END IF
         END IF


C
C *** NOW ROTATE THE NUCLEAR ATTRACTION INTEGRALS.
C *** THE STORAGE OF THE NUCLEAR ATTRACTION INTEGRALS  CORE(KL/IJ) IS
C     (SS/)=1,   (SO/)=2,   (OO/)=3,   (PP/)=4
C
         E1B(1)=-CSS1
         IF(NATORB(NI).EQ.4) THEN
            E1B(2) = -CSP1 *X(1)
            E1B(3) = -CPPS1*XX11-CPPP1*YYZZ11
            E1B(4) = -CSP1 *X(2)
            E1B(5) = -CPPS1*XX21-CPPP1*YYZZ21
            E1B(6) = -CPPS1*XX22-CPPP1*YYZZ22
            E1B(7) = -CSP1 *X(3)
            E1B(8) = -CPPS1*XX31-CPPP1*ZZ31
            E1B(9) = -CPPS1*XX32-CPPP1*ZZ32
            E1B(10)= -CPPS1*XX33-CPPP1*ZZ33
         END IF
         E2A(1)=-CSS2
         IF(NATORB(NJ).EQ.4) THEN
            E2A(2) = -CSP2 *X(1)
            E2A(3) = -CPPS2*XX11-CPPP2*YYZZ11
            E2A(4) = -CSP2 *X(2)
            E2A(5) = -CPPS2*XX21-CPPP2*YYZZ21
            E2A(6) = -CPPS2*XX22-CPPP2*YYZZ22
            E2A(7) = -CSP2 *X(3)
            E2A(8) = -CPPS2*XX31-CPPP2*ZZ31
            E2A(9) = -CPPS2*XX32-CPPP2*ZZ32
            E2A(10)= -CPPS2*XX33-CPPP2*ZZ33
         END IF
         IF(ABS(TORE(NI)).GT.20.D0.AND.ABS(TORE(NJ)).GT.20.D0) THEN
C FOR SPARKLE-SPARKLE INTERACTION:
            ENUC=0.D0
            RETURN
         ELSEIF (RIJ.LT.1.D0.AND.NATORB(NI)*NATORB(NJ).EQ.0) THEN
            ENUC=0.D0
            RETURN
         ENDIF
C FOR THE PM6 METHOD CALL THE SUBROUTINE FOR ITS CORE-CORE ENERGY CALCULATION.         
         IF (LPM6.OR.LPM6D) THEN                                        LF1009
           CALL PMSIX(NI,NJ,RIJ,GAM,ENUC,GTERM,*9999)                    ..     / CSTP
           IF(NATORB(NI)*NATORB(NJ).EQ.0)KI=0                            ..
           KR=KR+KI                                                      ..
           IF (LPM6) RETURN                                              ..
           GOTO 45                                                       ..
         ENDIF                                                          LF1009
C WHAT FOLLOWS IF FOR ALL OTHER METHODS OTHER THAN PM6
C START PUTTING TOGETHER SCALE VALUE:
         SCALE = EXP(-ALP(NI)*RIJ)+EXP(-ALP(NJ)*RIJ)
C HERE IS THE GENERALIZED CORE-CORE ENERGY CALCULATION PART FOR
C WHEN THE USER SPECIFIES "MOD5"
C HANDLE NEW MOD5 KEYWORD HERE.  REPLACING MOD3 CHANGES WITH THIS MORE
C GENERAL FORMULA.  NOTE THAT RIJ IS IN ANGSTROMS HERE.
         IF (PMODS(5)) THEN                                             LF0211
c#             write(6,*) "Doing PMOD #5."                                LF0211
c#             write(6,'(A,I2,A,I2,A)') "On atoms ",NI," and ",NJ,"."     LF0211
             SCALE = 0.0D0                                              LF0211
             SCALE = C0AB(NI,NJ)*EXP(-PRALP(NI,NJ)*RIJ)+                LF0211
     &               C1AB(NI,NJ)*EXP(-ALP(NI)*RIJ)+                     LF0211
     &               C2AB(NI,NJ)*EXP(-ALP(NJ)*RIJ)+                     LF0211
     &               C3AB(NI,NJ)*(RIJ)**(C3RPWR(NI,NJ))*                LF0211
     &                               EXP(-PR3ALP(NI,NJ)*RIJ)            LF0211
c#             write(6,*) "C0AB    = ",c0ab(ni,nj)                        LF0211
c#             write(6,*) "C1AB    = ",c1ab(ni,nj)                        LF0211
c#             write(6,*) "C2AB    = ",c2ab(ni,nj)                        LF0211
c#             write(6,*) "ALP(A)  = ",alp(ni)                            LF0211
c#             write(6,*) "ALP(B)  = ",alp(nj)                            LF0211
c#             write(6,*) "PRALP   = ",pralp(ni,nj)                       LF0211
c#             write(6,*) "C3AB    = ",c3ab(ni,nj)                        LF0211
c#             write(6,*) "C3RPWR  = ",c3rpwr(ni,nj)                      LF0211
c#             write(6,*) "PR3ALP  = ",pr3alp(ni,nj)                      LF0211
c#             write(6,*) "RIJ     = ",rij," angstroms"                   LF0211
c#             write(6,*) "W(1)    = ",w(1)                               LF0211
c#             write(6,*) "Scale   = ",scale                              LF0211
             GOTO 100                                                   LF0211
         ENDIF                                                          LF0211
C
C HERE IS THE "MOD3" MODIFICATION TO THE CORE-CORE ENERGY
         IF (PMODS(3)) THEN                                             LF0910
C          HANDLE PENG ZHANG'S MODIFICATIONS (IF .PMODS. IS TRUE)       LF0910
C              FOR Hp-Hp INTERACTION:                                   ZP0910
            IF (HPUSED.AND.(NI.EQ.9.AND.NJ.EQ.9)) THEN
c#         write (6,*) "PMODS section of ROTATE."                         debug LF
c#         write (6,*) "Using PMODVL(i) for i=1,2 of:"                    debug LF
c#         write (6,*) PMODVL(1), PMODVL(2)                               debug LF
c#         write (6,*) "RIJ here is ",RIJ                                 debug LF
c#         write (6,*) "NI, NJ:  ",NI,NJ                                  debug LF
c#         write (6,*) "XI: ",(XI(POS),POS=1,3)                           debug LF
c#         write (6,*) "XJ: ",(XJ(POS),POS=1,3)                           debug LF
c#         write (6,*) "SCALE = ",SCALE
               SCALE = EXP(-PMODVL(1)*RIJ)+EXP(-PMODVL(1)*RIJ)
c#         write (6,*) "SCALE = ",SCALE
            ENDIF
C              FOR O-O INTERACTION:
            IF (HPUSED.AND.(NI.EQ.8.AND.NJ.EQ.8)) THEN
c#         write (6,*) "PMODS section of ROTATE."                         debug LF
c#         write (6,*) "Using PMODVL(i) for i=1,2 of:"                    debug LF
c#         write (6,*) PMODVL(1), PMODVL(2)                               debug LF
c#         write (6,*) "RIJ here is ",RIJ                                 debug LF
c#         write (6,*) "NI, NJ:  ",NI,NJ                                  debug LF
c#         write (6,*) "XI: ",(XI(POS),POS=1,3)                           debug LF
c#         write (6,*) "XJ: ",(XJ(POS),POS=1,3)                           debug LF
c#         write (6,*) "SCALE = ",SCALE
c#               write(6,*) "M:  scale=",scale
c#               write(6,*) "    RIJ  =",rij
c#               write(6,*) "    PMODVL(2)=",pmodvl(2)
c#               write(6,*) "    pmodvl(2)-3.304=",pmodvl(2)-3.304d0
c#               write(6,*) "    -PMODVL(2)*RIJ=",(-pmodvl(2)*rij)
c#               write(6,*) "    exp(...) =",exp(-pmodvl(2)*rij)
               SCALE = EXP(-PMODVL(2)*RIJ)+EXP(-PMODVL(2)*RIJ)
c#               write(6,*) "N:  scale=",scale
c#         write (6,*) "SCALE = ",SCALE
            ENDIF                                                       ZP0910
         ENDIF                                                          LF0910
C FOR CHROMIUM-CHROMIUM INTERACTION:
         IF (NI.EQ.24.AND.NJ.EQ.24) THEN
            SCALE = EXP(-ALPTM(NI)*RIJ)+EXP(-ALPTM(NJ)*RIJ)
         ENDIF
C FOR O-H AND N-H PAIRS:
         NT=NI+NJ
C LF0210: If Hp atoms are used then all fluorines act as hydrogens.
         IF (HPUSED) THEN                                               HL0411
            IF (NJ.EQ.9) THEN                                           HL0411
               IF ((NI.EQ.7).OR.(NI.EQ.8)) THEN                         HL0411
                  SCALE=SCALE+(RIJ-1.D0)*EXP(-ALP(NI)*RIJ)              HL0411
               ENDIF                                                    HL0411
            ELSEIF (NI.EQ.9) THEN                                       HL0411
               IF ((NJ.EQ.7).OR.(NJ.EQ.8)) THEN                         HL0411
                  SCALE=SCALE+(RIJ-1.D0)*EXP(-ALP(NJ)*RIJ)              HL0411
               ENDIF                                                    HL0411
            ENDIF                                                       HL0411
         ENDIF                                                          HL0411
         IF((NT.EQ.8.OR.NT.EQ.9).OR.
     1        (HPUSED.AND.PMODS(1).AND.NI.EQ.8.AND.NJ.EQ.8)) THEN
            IF(NI.EQ.7.OR.NI.EQ.8) then
                                   SCALE=SCALE+(RIJ-1.D0)*EXP(-ALP(NI)*R
     1IJ)
            endif
            IF(NJ.EQ.7.OR.NJ.EQ.8) then
                                   SCALE=SCALE+(RIJ-1.D0)*EXP(-ALP(NJ)*R
     1IJ)
            endif
         ENDIF
C CONTINUE PUTTING TOGETHER ENUC AND SCALE VALUES:
 100     ENUC = TORE(NI)*TORE(NJ)*GAM
         SCALE=ABS(SCALE*ENUC)
C SPECIAL TERMS FOR AM1, PM3, AND RELATED METHODS IN NEXT SECTIONS:
         IF( AM1PM3 )THEN
C FOR PAIRS CONTAINING BORON:
         IF((LAM1.OR.LRM1.OR.LRM1D).AND.(NI.EQ.5.OR.NJ.EQ.5))THEN       LF0310
C
C   LOAD IN AM1 BORON GAUSSIANS
C
          NK=NI+NJ-5
C   NK IS THE ATOMIC NUMBER OF THE NON-BORON ATOM
          NL=1
          IF(NK.EQ.1)NL=2
          IF(NK.EQ.6)NL=3
          IF(NK.EQ.9.OR.NK.EQ.17.OR.NK.EQ.35.OR.NK.EQ.53)NL=4
          DO 25 I=1,3
          FN1(5,I)=BORON1(I,NL)
          FN2(5,I)=BORON2(I,NL)
  25      FN3(5,I)=BORON3(I,NL)
         ENDIF
C ADD IN AM1-TYPE GAUSSIAN TERMS FOR AM1, PM3, AND RELATED METHODS THAT USE THEM:
         IF(AM1PM3) THEN                                                LF0809
c#            write(6,*) "Using AM1-type gaussians:",fn1(6,1)
            GTERM=0.0D0                                                 LF0809
            DO 30 IG=1,10
               IF(ABS(FN1(NI,IG)).GT.0.D0) THEN
                  AX = FN2(NI,IG)*(RIJ-FN3(NI,IG))**2
                  IF(AX .LE. 25.D0) THEN
                     GTERM=GTERM +TORE(NI)*TORE(NJ)/RIJ*FN1(NI,IG)*EXP(-
     1AX)
                  ENDIF
               ENDIF
               IF(ABS(FN1(NJ,IG)).GT.0.D0) THEN
                  AX = FN2(NJ,IG)*(RIJ-FN3(NJ,IG))**2
                  IF(AX .LE. 25.D0) THEN
                     GTERM=GTERM +TORE(NI)*TORE(NJ)/RIJ*FN1(NJ,IG)*EXP(-
     1AX)
                  ENDIF
               ENDIF
   30       CONTINUE
         ENDIF
         ENDIF
C FOR PDDG-TYPE METHODS:
         IF (LPDG.OR.LMDG) THEN
C            ITYPE=2
C            WRITE(6,*) ITYPE,NI,PREA(NI), PREB(NI), DA(NI), DB(NI)
C            WRITE(6,*) ITYPE,NJ,PREA(NJ), PREB(NJ), DA(NJ), DB(NJ)
C
C THE PDDG FUNCTION HAS BEEN ADDED FOR THE PDDG METHODS
C
            QCORR = 0.0D0
            ZAF=TORE(NI)/(TORE(NI)+TORE(NJ))
            ZBF=TORE(NJ)/(TORE(NI)+TORE(NJ))
            QCORR =
     1 (ZAF*PREA(NI)+ZBF*PREA(NJ))*EXP(-10.0D0*(RIJ-DA(NI)-DA(NJ))**2)+
     2 (ZAF*PREA(NI)+ZBF*PREB(NJ))*EXP(-10.0D0*(RIJ-DA(NI)-DB(NJ))**2)+
     3 (ZAF*PREB(NI)+ZBF*PREA(NJ))*EXP(-10.0D0*(RIJ-DB(NI)-DA(NJ))**2)+
     4 (ZAF*PREB(NI)+ZBF*PREB(NJ))*EXP(-10.0D0*(RIJ-DB(NI)-DB(NJ))**2)
         ELSE
            QCORR = 0.0D0
         ENDIF 
                                                                              
 110     ENUC=ENUC+SCALE+QCORR+GTERM                                    LF0809
C#         write(6,*) "ENUC:   ",enuc                                     LF0211
C#         write(6,*) "SCALE:  ",scale                                    LF0211
C#         write(6,*) "QCORR:  ",qcorr                                    LF0211
C#         write(6,*) "GTERM:  ",gterm                                    LF0211

         IF(NATORB(NI)*NATORB(NJ).EQ.0)KI=0
         KR=KR+KI

C *************** END OF MAIN AM1,PM3,RM1,ETC. SECTIONS **********************

   45 continue                                                          LF0312
C
C ****************************************************************************
C THE "-D" DISPERSION TERMS ARE ADDED HERE FOR THE xxx-D METHODS        LF0312
C
      IF (DSPMET.AND..NOT.D3METH) THEN                                  LF0312
C
C     Be sure to treat "Hp" hydrogens as "H" hydrogens for dispersion.
      na=ni
      nb=nj
      if (ni.eq.9.and.hpused) na=1
      if (nj.eq.9.and.hpused) nb=1
C#      write(6,*) "check hpused=",hpused                                 debug
c#      write(6,*) "check element # na, nb first time: ",na,nb            debug
      
C     Use the correct dispersion parameters for the given method.
      if (LAM1D) then
         alpd=alpamd
         s6=s6amd
         do ii=1,120
           r0(ii)=r0amd(ii)
           c6(ii)=c6amd(ii)
         enddo
      elseif (LPM3D.or.LPMOV1.or.LPMO2.or.LPMO2A) then                  LF0113 / LF0614
         alpd=alppmd
         s6=s6pmd
         do ii=1,120
           r0(ii)=r0pmd(ii)
           c6(ii)=c6pmd(ii)
         enddo
      elseif (LRM1D) then
         alpd=alprmd
         s6=s6rmd
         do ii=1,120
           r0(ii)=r0rmd(ii)
           c6(ii)=c6rmd(ii)
         enddo
      elseif (LPM6D) then
         alpd=alpp6d
         s6=s6p6d
         do ii=1,120
           r0(ii)=r0p6d(ii)
           c6(ii)=c6p6d(ii)
         enddo
      else
         alpd=alpmnd
         s6=s6mnd
         do ii=1,120
           r0(ii)=r0mnd(ii)
           c6(ii)=c6mnd(ii)
         enddo
      endif
c#      write(6,*) "check rotate: r0(1)=",r0(1)                           debug
c#      write(6,*) "check rotate: r0(9)=",r0(9)                           debug

C#        write(6,*) "Doing dispersion for -D on ",NA,NB
        DCORR = 0.0D0                                                   LF0310

        IF (USEDPR) THEN                                                LF0312
          dij= DPAIR(NA,NB)                                               ..  
        ELSE                                                              ..  
          dij= r0(na) + r0(nb)                                            ..  
C#          write(6,*) "check element # na, nb: ",na,nb                   debug
C#          write(6,*) "check added r0(na)+r0(nb)=dij as ",               debug
C#     &                      r0(na),r0(nb),dij                           debug
        ENDIF                                                           LF0312

        IF (USECPR) THEN                                                LF0312
          ! Assume that units of specified pairwise C_6 parameters        ..
          ! are eV*Angstrom**6 already, unlike for monoatomic C_6         ..
          ! parameters which are in J*nm**6/mol (consistent with          ..
          ! Hillier's AM1-D and PM3-D version 1 "-D" dispersion paper).   ..
          c6ij= CPAIR(na,nb)                                              ..  
        ELSE                                                              ..  
          c6ij   = 2.0D0 * C6(NA)*C6(NB)/(C6(NA)+C6(NB))                  ..
          ! Convert from J*nm**6/mol to eV*Angstrom**6.                   ..
          c6ij   = c6ij   * 10.364264D0                                   ..
        ENDIF                                                           LF0312

c#        write(6,'(a,4f24.19)') "as doubles:",rpnum,rpden,dpden,spden     debug
c#        stop                                                             debug
                                                                         debug
        IF (LSDAMP) THEN                                                LF0312
          ! Stone's damping function.
          ! Convert atomic ionization potentials (eV) to atomic units.  LF0310
          BETAIJ=SQRT(2.D0*ATOMIP(NA)/27.211D0)+                        LF0310
     &           SQRT(2.D0*ATOMIP(NB)/27.211D0)                           ..
          T1=BETAIJ*RIJ/0.529177D0                                        ..
          T2=1.D0                                                         ..
          FDAMP=1.D0                                                      ..
          DO I=1,6                                                        ..
             T2 = T2*T1/DBLE(I)                                           ..
             FDAMP = FDAMP+T2                                             ..
          ENDDO                                                           ..
          FDAMP = 1.D0 - (FDAMP*EXP(-T1))                               LF0310
        ELSEIF (LHGDAMP) THEN                                           LF0312
          ! Head-Gordon's damping function.                             LF0312
          ! Ref: J. Chai and M. Head-Gordon, Phys. Chem. Chem. Phys., 2008, 10, 6615-6620.
          ! In paper: fdamp=1.d0/(1.d0+(alpd)*(rij/(r0(na)+r0(nb)))**(-12))     
          fdamp=1.d0/(1.d0+(alpd)*(rij/(dij))**(-12))                   LF0312
        ELSEIF (LTDAMP) THEN                                            LF0312
          ! Truhlar group's damping functions.                          LF0312 
          ! From keyword TDAMP={rpnum},{rpden},{dpden},{spden}.         LF0312
          fdamp=rij**rpnum/(rij**rpden+dij**dpden)**spden               LF0312
        ELSEIF (LDAMP5) THEN                                            LF0312
          ! An alternative form of damping similar to TDAMP=8,2,2,4     LF0312
          ! but it has a different functional form.                     LF0312
          fdamp=(rij**6)*(1.d0-exp(-(rij/dij)**8))                      LF0312
        ELSE                                                            LF0312 
          ! Default Hillier's -D version 1 damping function.            LF0312
          ! Ref: C. Morgado, M. A. Vincent, I. H. Hillier and X. Shan, Phys. Chem. Chem. Phys., 2007, 9, 448.
          ! In paper:  1.0D0/(1.0D0+EXP(-ALPD*(RIJ/(R0(NA)+R0(NB))-1.D0)))
          fdamp = 1.d0/(1.d0+EXP(-alpd*(rij/(dij)-1.d0)))               LF0310/LF0312
        ENDIF                                                           LF0312

        IF (LTDISP) THEN                                                LF0312
          ! Truhlar group's dispersion function.                        LF0312
          ! Question: use the same dispersion as Hillier -D or our new one with TDAMP damping?
          DCORR = (-c6ij/(rij**6))*fdamp                                LF0312
        ELSEIF (LHGDISP) THEN                                           LF0312
          ! Head-Gordon's dispersion function.                          LF0312
          ! Ref: J. Chai and M. Head-Gordon, Phys. Chem. Chem. Phys., 2008, 10, 6615-6620.
          ! Question: use the same dispersion as Hillier -D but with HG's damping?
          DCORR = (-c6ij/(rij**6))*fdamp                                LF0312
        ELSE                                                            LF0310
          ! Default Hillier's -D version 1 dispersion function.           ..
          ! Ref: C. Morgado, M. A. Vincent, I. H. Hillier and X. Shan, Phys. Chem. Chem. Phys., 2007, 9, 448.
          DCORR = (-S6)*FDAMP*C6IJ/(RIJ**6)                               ..
        ENDIF                                                           LF0312
 
        ! Sum up contributions to total dispersion energy (in eV still here).
c#        write(6,*) "na,nb=",na,nb
c#        write(6,*) "disp contribution=",dcorr
c#        write(6,*) "r0(na)=",r0(na)
c#        write(6,*) "r0(nb)=",r0(nb)
c#        write(6,*) "fdamp=",fdamp
c#        write(6,*) "dij=",dij
c#        write(6,*) "c6ij=",c6ij
        ENUC=ENUC+DCORR                                                   ..
      ENDIF                                                             LF0310
C                                                                       LF0312
c#      IF (DOPRNT) WRITE(6,*) "Pair's dispersion energy (eV)=",DCORR     LF0312
C     Here sum up all the dispersion energies from successive calls to ROTATE in kcal/mol.
      IF (LDISPON) DISPTOTAL=DISPTOTAL+DCORR*23.061D0                   LF0312/LF0412
C *** END OF SECTION FOR "-D" DISPERSION METHODS                        LF0310
C ****************************************************************************
C
C#      write(6,*) "ROTATE:"
C#      write(6,*) "GAM=",GAM
C#      write(6,*) "NI=",NI," NJ=",NJ
C#      write(6,*) "NA=",NA," NB=",NB
C#      write(6,*) "XI=",(XI(JJ),JJ=1,3)
C#      write(6,*) "XJ=",(XJ(JJ),JJ=1,3)
C#      write(6,*) "ENUC=",ENUC

c      LOGICAL  LMINDO,LMNDO,LAM1,LPM3,LRM1,LMDG,LPDG,LAM1D,LPM3D,LPM6,  LF0909
c     &         AM1PM3,ANALYT,                                           LF0909
c     &         LPM6G9,                                                  LF0110
c     &         LMNDOD,LSDAMP,LRM1D,LPM6D,DSPMET,                        LF0310
c     &         LPMOV1                                                   LF1010
c#      write(6,*) "LPM3 =",LPM3 
c#      write(6,*) "LPM3D=",LPM3D
c#      write(6,*) "LRM1 =",LRM1 
c#      write(6,*) "LRM1D=",LRM1D
c#      write(6,*) "LPM6 =",LPM6 
c#      write(6,*) "LPM6D=",LPM6D
c#      write(6,*) "AM1PM3=",AM1PM3
c#      write(6,*) "ANALYT=",ANALYT
c#      write(6,*) "LPMOV1=",LPMOV1
c#      write(6,*) "DSPMET=",DSPMET
c#      write(6,*) "LSDAMP=",LSDAMP
c#      write(6,*) "LMNDO=",LMNDO
c#      write(6,*) "LMNDOD=",LMNDOD
c#      write(6,*) "LAM1=",LAM1
c#      write(6,*) "LAM1D=",LAM1D
      RETURN
 9999 RETURN 1                                                          CSTP
      END
