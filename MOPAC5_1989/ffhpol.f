      SUBROUTINE FFHPOL (HEAT0,ATPOL,DIPVEC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*1 AXIS(3)
      CHARACTER*80 KEYWRD
      LOGICAL LARGE,POLDIP,DEBUG
      INCLUDE 'SIZES'
C***********************************************************************
C  SUBROUTINE FOR THE FINITE FIELD CALCULATION OF ELECTRIC RESPONSE
C  PROPERTIES (DIPOLE MOMENT, POLARIZABILITY, AND 1ST AND 2ND
C  HYPERPOLARIZABILITY.
C
C  HENRY A. KURTZ, DEPARTMENT OF CHEMISTRY
C                  MEMPHIS STATE UNIVERSITY
C                  MEMPHIS, TN   38152
C
C***********************************************************************
      COMMON /CORE  / CORE(107)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /COORD / COORD(3,NUMATM)
      COMMON /KEYWRD/ KEYWRD
      COMMON /FIELD / EFIELD(3)
      COMMON /EULER / TVEC(3,3),IDTVEC
C
C
C     DIPE4 AND DIPDP HOLD THE CALCULATED DIPOLE MOMENTS
C
C     APOLE4 AND APOLDP HOLD THE POLARIZABILITY TENSOR AS
C                                A PACKED ARRAY XX,XY,YY,XZ,YZ,ZZ
C
C     BETAE4 AND BETAEP HOLD THE FIRST HYPERPOLARIZABILITY
C                                1. XXX
C                                2. YYY     6. YXX
C                                3. ZZZ     7. YZZ
C                                4. XYY     8. ZXX
C                                5. XZZ     9. ZYY
C
      DIMENSION HEATE(3,2),DIPVEC(3),EIGS(3),VECTRS(9),
     1          DIPE4(3),APOLE4(6),BETAE4(9),GAMME4(6),
     2          DIPDP(3),APOLDP(6),BETADP(9),GAMMDP(6),
     3          DIP1P(3),DIP1M(3),DIP2P(3),DIP2M(3)
      DIMENSION IPTBD(6)
      DATA HEAT3M/0.0D0/,HEAT3P/0.D0/
      DATA IPTBD /5,7,4,9,6,8/
C Energy: a.u. to kcal/mole
      AUTOKC = 23.061D+00*27.2107D+00
C Length: a.u. to Angstrom
      AUTOA  = 0.529177D+00
C Dipole: a.u. to debye
      AUTODB = 2.541563D+00
C Electric Field: a.u. to volt/meter
      AUTOVM = 51.4257D+00
      NBDIP = 1
      NGDIP = 4
      NBCNT = 4
      NGCNT = 4
C
      DATA AXIS/'X','Y','Z'/
      LARGE = (INDEX(KEYWRD,'LARGE').NE.0)
      DEBUG = (INDEX(KEYWRD,'DEBUG').NE.0)
C
C  FIELD STRENGTH IN A.U.
C
      EFVAL=0.001D0
      IDIP=1
C#      READ (7,10) EFVAL,IDIP
   10 FORMAT (F10.5,I5)
      WRITE (6,20) EFVAL
   20 FORMAT (//' APPLIED ELECTRIC FIELD MAGNITUDE: ',F15.5)
      POLDIP = .FALSE.
      IF (IDIP.NE.0) POLDIP = .TRUE.
      SFE = 1.D00/EFVAL
      WRITE (6,30) 6.74834*ATPOL
   30 FORMAT (//' ATOMIC CONTRIBUTION TO THE POLARIZABILITY: ',F15.6)
C.......................................................................
C  CALCULATE THE POLARIZABILITY AND HYPERPOLARIZABILITIES ALONG
C  THE THREE PRINCIPLE AXES.  (THESE AXES DEPEND ON YOUR ARBITRARY
C  ORIENTATION AND MAY NOT BE THE TRUE PRINCIPLE AXES.)
C.......................................................................
      DO 160 ID = 1,3
         IF (DEBUG) THEN
            WRITE (6,40) AXIS(ID)
   40       FORMAT (//,' ****** ',A1,' DIRECTION *****',/)
         ENDIF
C
C ZERO THE FIELD
C
         DO 50 I = 1,3
            EFIELD(I) = 0.0D00
   50    CONTINUE
         HNUC = 0.0D00
         DO 60 I = 1,NUMAT
            HNUC = HNUC + EFVAL*GEO(ID,I)*CORE(NAT(I))*AUTOVM
   60    CONTINUE
         HNUC = HNUC*23.061D00
C +E(ID)
         EFIELD(ID) = EFVAL
         CALL COMPFG(GEO,.TRUE.,HEAT1P,.TRUE.,GRAD,.FALSE.)
         CALL DIPIND (DIP1P)
         DIIP = DIP1P(ID)
C -E(ID)
         EFIELD(ID) = -EFVAL
         CALL COMPFG(GEO,.TRUE.,HEAT1M,.TRUE.,GRAD,.FALSE.)
         CALL DIPIND (DIP1M)
         DIIM = DIP1M(ID)
C +2E(ID)
         EFIELD(ID) = 2.0D00*EFVAL
         CALL COMPFG(GEO,.TRUE.,HEAT2P,.TRUE.,GRAD,.FALSE.)
         CALL DIPIND (DIP2P)
C -2E(ID)
         EFIELD(ID) = -2.0D00*EFVAL
         CALL COMPFG(GEO,.TRUE.,HEAT2M,.TRUE.,GRAD,.FALSE.)
         CALL DIPIND (DIP2M)
C
C  CORRECT FOR ELECTRIC FIELD - NUCLEAR INTERACTIONS
C
         HEAT1P = HEAT1P + HNUC
         HEATE(ID,1) = HEAT1P
         HEAT1M = HEAT1M - HNUC
         HEATE(ID,2) = HEAT1M
         HEAT2P = HEAT2P + HNUC*2.D00
         HEAT2M = HEAT2M - HNUC*2.D00
C
         IF (DEBUG) THEN
            WRITE (6,70)
   70       FORMAT (' ENERGIES AT: ',5X,'F',21X,'2F',21X,'3F',/)
            WRITE (6,80) HEAT1P,HEAT2P,HEAT3P,HEAT1M,HEAT2M,HEAT3M
   80       FORMAT ('   + ',3(F20.10,3X),/,'   - ',3(F20.10,3X))
         ENDIF
C
C DIPOLE
C
         ETERM = (1.0D00/12.D00)*(HEAT2P - HEAT2M)
     1        - (2.0D00/3.0D00)*(HEAT1P - HEAT1M)
         DIPE4(ID) = ETERM*SFE/AUTOKC
C
C ALPHA
C
         IVL = (ID*(ID+1))/2
         ETERM = 2.5D00*HEAT0 - (4.D00/3.D00)*(HEAT1P + HEAT1M)
     1        + (1.D00/12.0D00)*(HEAT2P + HEAT2M)
         APOLE4(IVL) = ETERM*SFE*SFE/AUTOKC + ATPOL*6.74834
C
C BETA
C
         ETERM = (HEAT1P - HEAT1M) - 0.5D00*(HEAT2P - HEAT2M)
         BETAE4(ID) = ETERM*SFE*SFE*SFE/AUTOKC
C
C GAMMA
C
         ETERM = 4.0D00*(HEAT1P + HEAT1M) - (HEAT2P + HEAT2M)
     1        - 6.0D00*HEAT0
         GAMME4(ID) = ETERM*SFE*SFE*SFE*SFE/AUTOKC
C
C DIPOLE CALCULATIONS
C
         DMU = (2.0D00/3.0D00)*(DIP1P(ID) + DIP1M(ID))
     1      - (1.D00/6.0D00)*(DIP2P(ID) + DIP2M(ID))
         DIPDP(ID) = DMU/AUTODB
         AE = (2.0D00/3.0D00)*(DIP1P(ID) - DIP1M(ID))
     1     - (1.0D00/12.D00)*(DIP2P(ID) - DIP2M(ID))
         APOLDP(IVL) = AE*SFE/AUTODB
         BE = (1.D00/3.0D00)*(DIP2P(ID) + DIP2M(ID)
     1                     - DIP1P(ID) - DIP1M(ID))
         BETADP(ID) = BE*SFE*SFE/AUTODB
         GE = 0.5D00*(DIP2P(ID) - DIP2M(ID))
     1     - (DIP1P(ID) - DIP1M(ID))
         GAMMDP(ID) = GE*SFE*SFE*SFE/AUTODB
         DO 90 KD = 1,3
            IF (KD.LT.ID) THEN
               KVL = (ID*(ID-1))/2 + KD
               AKI = (2.0D00/3.0D00)*(DIP1P(KD) - DIP1M(KD))
     1         - (1.0D00/12.0D00)*(DIP2P(KD) - DIP2M(KD))
               APOLDP(KVL) = AKI*SFE/AUTODB
            ENDIF
            IF (KD.NE.ID) THEN
               BKII = (1.0D00/3.0D00)*(DIP2P(KD) + DIP2M(KD)
     1                           - DIP1P(KD) - DIP1M(KD))
               NBD = IPTBD(NBDIP)
               BETADP(NBD) = BKII*SFE*SFE/AUTODB
               NBDIP = NBDIP + 1
            ENDIF
   90    CONTINUE
C.......................................................................
C
C  NOW CALCULATE THE OFF AXIS RESULTS.
C
C.......................................................................
         IDM1 = ID - 1
         DO 150 JD = 1,IDM1
            HNUCJ = 0.0D00
            DO 100 I = 1,NUMAT
               HNUCJ = HNUCJ + EFVAL*GEO(JD,I)*CORE(NAT(I))*51.4257
  100       CONTINUE
            HNUCJ = HNUCJ*23.061
            DO 110 I = 1,3
               EFIELD(I) = 0.0D00
  110       CONTINUE
C
C DIAGONAL FIELDS WITH COMPONENTS EQUAL TO EFVAL
C
            EFIELD(ID) = EFVAL
            EFIELD(JD) = EFVAL
            CALL COMPFG(GEO,.TRUE.,HPP,.TRUE.,GRAD,.FALSE.)
            CALL DIPIND (DIP1P)
            DPP = DIP1P(ID)
            EFIELD(JD) = -EFVAL
            CALL COMPFG(GEO,.TRUE.,HPM,.TRUE.,GRAD,.FALSE.)
            CALL DIPIND (DIP1P)
            DPM = DIP1P(ID)
            EFIELD(ID) = -EFVAL
            CALL COMPFG(GEO,.TRUE.,HMM,.TRUE.,GRAD,.FALSE.)
            CALL DIPIND (DIP1P)
            DMM = DIP1P(ID)
            EFIELD(JD) = EFVAL
            CALL COMPFG(GEO,.TRUE.,HMP,.TRUE.,GRAD,.FALSE.)
            CALL DIPIND (DIP1P)
            DMP = DIP1P(ID)
            HPP = HPP + HNUC + HNUCJ
            HPM = HPM + HNUC - HNUCJ
            HMM = HMM - HNUC - HNUCJ
            HMP = HMP - HNUC + HNUCJ
            IF (DEBUG) THEN
               WRITE (6,120)
  120          FORMAT (/,' ',12X,'+,+',15X,'+,-',15X,'-,+',15X,'-,-')
               WRITE (6,130) HPP,HPM,HMP,HMM
  130          FORMAT ('  E ',4F15.6)
            ENDIF
C
C  DIAGONAL FIELDS WITH COMPONENTS EQUAL TO 2*EFVAL
C
            EFIELD(ID) = EFVAL*2.D00
            EFIELD(JD) = EFVAL*2.D00
            CALL COMPFG(GEO,.TRUE.,H2PP,.TRUE.,GRAD,.FALSE.)
            EFIELD(JD) = -EFVAL*2.D00
            CALL COMPFG(GEO,.TRUE.,H2PM,.TRUE.,GRAD,.FALSE.)
            EFIELD(ID) = -EFVAL*2.D00
            CALL COMPFG(GEO,.TRUE.,H2MM,.TRUE.,GRAD,.FALSE.)
            EFIELD(JD) = EFVAL*2.D00
            CALL COMPFG(GEO,.TRUE.,H2MP,.TRUE.,GRAD,.FALSE.)
            H2PP = H2PP + 2.0D00*(HNUC + HNUCJ)
            H2PM = H2PM + 2.0D00*(HNUC - HNUCJ)
            H2MM = H2MM - 2.0D00*(HNUC + HNUCJ)
            H2MP = H2MP - 2.0D00*(HNUC - HNUCJ)
            IF (DEBUG) THEN
               WRITE (6,140) H2PP,H2PM,H2MP,H2MM
  140          FORMAT (' 2E ',4F15.6)
            ENDIF
C
            ATERM = (1.0D00/48.0D00)*(H2PP - H2PM - H2MP + H2MM)
     1          - (1.0D00/3.0D00)*(HPP - HPM - HMP + HMM)
            AIJ = ATERM*SFE*SFE/AUTOKC
            IVL = (ID*(ID-1))/2 + JD
            APOLE4(IVL) = AIJ
            BTERM = 0.5D00*(HMM - HPP + HPM - HMP)
     1          + HEATE(JD,1) - HEATE(JD,2)
            BJII = BTERM*SFE*SFE*SFE/AUTOKC
            BETAE4(NBCNT) = BJII
            NBCNT = NBCNT + 1
            BTERM = 0.5D00*(HMM - HPP + HMP - HPM)
     1          + HEATE(ID,1) - HEATE(ID,2)
            BIJJ = BTERM*SFE*SFE*SFE/AUTOKC
            BETAE4(NBCNT) = BIJJ
            NBCNT = NBCNT + 1
C
            GTERM = -(HPP + HMM + HPM + HMP) - 4.0D00*HEAT0
     1           + 2.0D00*(HEATE(ID,1) + HEATE(ID,2))
     2           + 2.0D00*(HEATE(JD,1) + HEATE(JD,2))
            GIIJJ = GTERM*SFE*SFE*SFE*SFE/AUTOKC
            GAMME4(NGCNT) = GIIJJ
            GDIP = 0.5D00*(DPP - DMP + DPM - DMM) - (DIIP - DIIM)
            GAMMDP(NGCNT) = GDIP*SFE*SFE*SFE/AUTODB
            NGCNT = NGCNT + 1
  150    CONTINUE
C
  160 CONTINUE
C-----------------------------------------------------------------------
C  SUMMARIZE THE RESULTS
C-----------------------------------------------------------------------
      WRITE (6,170)
  170 FORMAT (//,' ',30('*'),' DIPOLE ',30('*'),//)
      DIPE4T = SQRT(DIPE4(1)*DIPE4(1) + DIPE4(2)*DIPE4(2)
     1              + DIPE4(3)*DIPE4(3))
      DIPE4D = DIPE4T*AUTODB
      DIPDPT = SQRT(DIPDP(1)*DIPDP(1) + DIPDP(2)*DIPDP(2)
     1              + DIPDP(3)*DIPDP(3))
      DIPDPD = DIPDPT*AUTODB
      WRITE (6,180)
  180 FORMAT (21X,'E4',13X,'DIP',/)
      WRITE (6,190) 'X',DIPE4(1),DIPDP(1)
      WRITE (6,190) 'Y',DIPE4(2),DIPDP(2)
      WRITE (6,190) 'X',DIPE4(3),DIPDP(3)
  190 FORMAT (5X,A1,7X,2F15.6)
      WRITE (6,200) DIPE4T,DIPDPT,
     1               DIPE4D,DIPDPD
  200 FORMAT (//' MAGNITUDE:  ',2F15.6,'  (A.U.)',/,
     1          ' ',12X,2F15.6,'  (DEBYE)')
C
C FIND EIGENVALUES AND EIGENVECTORS OF POLARIZATION MATRIX.
C
      WRITE (6,210)
  210 FORMAT (//,' ',30('*'),' POLARIZABILITY ',20('*'),//)
      WRITE (6,220)
  220 FORMAT (/' E4 POLARIZABILITY TENSOR:')
      I3 = 3
      CALL VECPRT (APOLE4,-I3)
      CALL RSP (APOLE4,I3,I3,EIGS,VECTRS)
      CALL MATOUT (VECTRS,EIGS,I3,I3,I3)
      AVGPE4 = (EIGS(1)+EIGS(2)+EIGS(3))/3.D0
      AVGA3 = AVGPE4*0.14818D00
      AVGESU = AVGPE4*0.296352D-24
      WRITE (6,230)
  230 FORMAT (/' DIP POLARIZABILITY TENSOR:')
      CALL VECPRT (APOLDP,-I3)
      CALL RSP (APOLDP,I3,I3,EIGS,VECTRS)
      CALL MATOUT (VECTRS,EIGS,I3,I3,I3)
      AVGPDP = (EIGS(1)+EIGS(2)+EIGS(3))/3.D0
      AVGA3D = AVGPDP*0.14818D00
      AVGESD = AVGPDP*0.296352D-24
      WRITE (6,240) AVGPE4,AVGPDP,AVGA3,AVGA3D,AVGESU,AVGESD
  240 FORMAT (//,' AVERAGE POLARIZABILITY:',8X,'E4',13X,'DIP',/,
     1           ' ',24X,2F15.6,'  A.U.',/,
     2           ' ',24X,2F15.6,'  ANG.**3',/,
     3           ' ',24X,2(1PD15.6),'  ESU')
C
C  CALCULATE "EXPERIMENTAL" HYPERPOLARIZABILITIES
C
C   8.65710D-33 is a.u. to e.s.u. conversion
      WRITE (6,250)
  250 FORMAT (//,' ',30('*'),' SECOND-ORDER ',25('*'),//)
      BX4 = 0.6D00*(BETAE4(1) + BETAE4(4) + BETAE4(6))
      BY4 = 0.6D00*(BETAE4(2) + BETAE4(5) + BETAE4(8))
      BZ4 = 0.6D00*(BETAE4(3) + BETAE4(7) + BETAE4(9))
      B4MU = (BX4*DIPE4(1) + BY4*DIPE4(2) + BZ4*DIPE4(3))/DIPE4T
      B4ESU = B4MU*8.65710D-03
      BXD = 0.6D00*(BETADP(1) + BETADP(4) + BETADP(6))
      BYD = 0.6D00*(BETADP(2) + BETADP(5) + BETADP(8))
      BZD = 0.6D00*(BETADP(3) + BETADP(7) + BETADP(9))
      BDMU = (BXD*DIPDP(1) + BYD*DIPDP(2) + BZD*DIPDP(3))/DIPDPT
      BDESU = BDMU*8.65710D-03
C
      WRITE (6,260)
  260 FORMAT ('  COMPONENT',12X,'E4',13X,'DIP',/)
      WRITE (6,270) 'XXX',BETAE4(1),BETADP(1)
      WRITE (6,270) 'XYY',BETAE4(4),BETADP(4)
      WRITE (6,270) 'XZZ',BETAE4(6),BETADP(6)
      WRITE (6,270) 'YYY',BETAE4(2),BETADP(2)
      WRITE (6,270) 'YXX',BETAE4(5),BETADP(5)
      WRITE (6,270) 'YZZ',BETAE4(8),BETADP(8)
      WRITE (6,270) 'ZZZ',BETAE4(3),BETADP(3)
      WRITE (6,270) 'ZXX',BETAE4(7),BETADP(7)
      WRITE (6,270) 'ZYY',BETAE4(9),BETADP(9)
  270 FORMAT (' ',5X,A4,5X,2F15.6)
      WRITE (6,280)
  280 FORMAT (/)
      WRITE (6,290) 'BX',BX4,BXD
      WRITE (6,290) 'BY',BY4,BYD
      WRITE (6,290) 'BZ',BZ4,BZD
  290 FORMAT (' ',6X,A2,6X,2F15.6)
      WRITE (6,280)
      WRITE (6,300) B4MU,BDMU,B4ESU,BDESU
  300 FORMAT (' ',4X,'B(AU)',5X,2F15.6,/,
     1        ' ',4X,'B(ESU)',4X,2F15.6,3X,'(X10-30)')
C
      WRITE (6,310)
  310 FORMAT (//' ',30('*'),' THIRD-ORDER ',25('*'),//)
      GAMVAL = (GAMME4(1) + GAMME4(2) + GAMME4(3))
      GAMVAL = GAMVAL + 2.0D00*(GAMME4(4) + GAMME4(5) + GAMME4(6))
      GAMVAL = GAMVAL/5.0D00
C  5.05116D-40 is the a.u. to e.s.u. conversion
      GAMESU = GAMVAL*5.05116D-04
      GAMDIP = (GAMMDP(1) + GAMMDP(2) + GAMMDP(3))
      GAMDIP = GAMDIP + 2.0D00*(GAMMDP(4) + GAMMDP(5) + GAMMDP(6))
      GAMDIP = GAMDIP/5.0D00
      GAMDES = GAMDIP*5.05116D-04
      WRITE (6,320)
  320 FORMAT (' ',17X,'E4',13X,'DIP',/)
      WRITE (6,330) 'XXXX',GAMME4(1),GAMMDP(1)
      WRITE (6,330) 'YYYY',GAMME4(2),GAMMDP(2)
      WRITE (6,330) 'ZZZZ',GAMME4(3),GAMMDP(3)
      WRITE (6,330) 'XXYY',GAMME4(4),GAMMDP(4)
      WRITE (6,330) 'XXZZ',GAMME4(5),GAMMDP(5)
      WRITE (6,330) 'YYZZ',GAMME4(6),GAMMDP(6)
  330 FORMAT (5X,A4,2F15.6)
      WRITE (6,340) GAMVAL,GAMDIP,GAMESU,GAMDES
  340 FORMAT (//' GAMMA = ',1PD15.6,1PD15.6,'  A.U.'/,
     1       ' ',8X,1PD15.6,1PD15.6,'  ESU (X10-36)')
C
      RETURN
      END
