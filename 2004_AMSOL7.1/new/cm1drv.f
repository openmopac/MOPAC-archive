      SUBROUTINE CM1DRV
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'
      LOGICAL AM1HAM
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),
     1                QEFF2(NUMATM),DRVPOL(MPACK)
      COMMON /CM1SUM/ B(NUMATM,NUMATM),QM(NUMATM),DELTA(NUMATM)         DL0397
     1               ,QC0(NUMATM),QC(NUMATM)                            DL0397
     2               ,QD0(NUMATM),FD(NUMATM,NUMATM),QD(NUMATM)          DL0397
      COMMON /SCRCHR/ DQ(NUMATM)                                        DL0397
      INCLUDE 'PARAMS.i'
C ---------------------------------------------------------------------+DL0397
C     CM1 MODELS: SOLVATION CONTRIBUTION TO FOCK OPERATOR.
C
C  ON INPUT
C             P : TOTAL DENSITY MATRIX (CANONICAL ORDER).
C     VECTOR QM : MULLIKEN NET CHARGES
C     MATRIX  B : B(I,I)="Bi"; B(I,J)=B(J,I)="-Bij"
C     VECTOR QC : QC0 + SPECIAL TERM FOR AM1 NC BOND
C     VECTOR QD : QD0 + DIAGONAL ELEMENTS OF B.FD (FD IS NON SYMMETRIC)
C                     + SPECIAL TERM FOR AM1 NC BOND
C     VECTOR DELTA(I) : QD(I) + QC(I).QM(I)
C     Q     : QM + B.DELTA     (STORED IN QEFF2)
C     FGB   : CELCOE/STILL's FACTOR "FGB"
C     BP    : FGB.Q
C
C  ON OUTPUT
C     DRVPOL (CANONICAL) TO BE ADDED TO GAZ PHASE FOCK MATRIX.
C
C     REWRITTEN IN MATRIX FORM BY DL, 0397:
C        SOLVATION ENERGY:  E = (-1/2).Q'.FGB.Q = (-1/2).BP'.Q
C        DERIVATIVE      : dE = -BP'.dQ
C        dQ = dQM + dB.DELTA + B.dDELTA
C        dDELTA = dQD + dQC.QM + QC.dQM
C        dQC = ZERO + DERIVATIVE OF SPECIAL NC AM1 TERMS
C        dQD = dB.FD (DIAGONAL TERMS) + DERIVATIVE OF SPECIAL NC AM1
C     WITH RESPECT TO A GIVEN P(a,b) = P(b,a),
C                             a ON ATOM A & b ON ATOM B:
C        A = B  ==>  dQM = ZERO IF a.NE.b
C                        = ZERO UNLESS dQM(A) = -1 FOR a = b
C                    dB  = ZERO
C        A.NE.B ==>  dQM = ZERO
C                    dB  = ZERO UNLESS dB(A,B) = dB(B,A) = -2.P(a,b)
C                                      dB(A,A) = dB(B,B) =  2.P(a,b)
C     NOTE: OFF-DIAGONAL FOCK ENTRIES ARE ONE HALF THE CORRESPONDING
C           DERIVATIVE OF THE SOLVATION ENERGY:
C           DRVPOL(a,b) = DRVPOL(b,a) = (1/2).dE
C ---------------------------------------------------------------------+DL0397
      DATA ZERO /0D0/                                                   DL0397
      AM1HAM=IAM1.NE.0                                                  DL0397
C     (CASES 3 & 4)                                                     DL0397
      LINEAR=NORBS*(NORBS+1)/2                                          DL0397
      CALL SCOPY (LINEAR,ZERO,0,DRVPOL,1)                               DL0397
C                                                                       DL0397
C     ONE-CENTRE P(MU,NU)                                               DL0397
C     -------------------  dQM = NON ZERO; dB = ZERO                    DL0397
C     (CASES 1 & 2)                                                     DL0397
      MUMU=0                                                            DL0397
      DO 100 NA=1,NUMAT                                                 DL0397
      DO 10 I=1,NUMAT                                                   DL0397
      DQ(I)=-QC(NA)*B(I,NA)                                             DL0397
   10 CONTINUE                                                          DL0397
      DQ(NA)=DQ(NA)-1D0                                                 DL0397
      DERE=-DOT(BP,DQ,NUMAT)                                            DL0397
      DO 100 MU=NFIRST(NA),NLAST(NA)                                    DL0397
      MUMU=MUMU+MU                                                      DL0397
      DRVPOL(MUMU)=DERE                                                 DL0397
  100 CONTINUE                                                          DL0397
C                                                                       DL0397
C     TWO-CENTRE P(MU,NU)                                               DL0397
C     -------------------  dQM = ZERO; dB = NON ZERO                    DL0397
C     (CASES 5, 6, 7)                                                   DL0397
      DO 300 NA=2,NUMAT                                                 DL0397
      DO 300 NB=1,NA-1                                                  DL0397
C     P(MU,NU) KEPT FACTORIZED, WILL BE DISTRIBUTED LATER ON.           DL0397
C     USUAL LAWS GIVE dQC = ZERO                                        DL0397
C                     dQD = DIAGONAL OF (dB.FD)                         DL0397
C     NON ZERO dQD: dQD(A)=-2.FD(NB,NA) & dQD(B)=-2.FD(NA,NB)           DL0397
C     dQ = B.dQD, HALVED                                                DL0397
      DO 200 I=1,NUMAT                                                  DL0397
      DQ(I)=-B(I,NA)*FD(NB,NA)-B(I,NB)*FD(NA,NB)                        DL0397
  200 CONTINUE                                                          DL0397
C     dQ = dQ + dB.DELTA, HALVED                                        DL0397
      DQ(NA)=DQ(NA)+DELTA(NA)-DELTA(NB)                                 DL0397
      DQ(NB)=DQ(NB)-DELTA(NA)+DELTA(NB)                                 DL0397
      IF (AM1HAM) THEN                                                  DL0397
C        SPECIAL TERMS FOR N--C BOND                                    DL0397
C        SCALE N--C & OFFSET N--C TOWARDS dDELTA, THEN DQ, HALVED:      DL0397
         IF (NAT(NA).EQ.7.AND.NAT(NB).EQ.6) THEN                        DL0397
            DBHAT=5D0*(1D0-TANH(1D1*B(NB,NA)+2.3D1)**2)                 GDH0597
            DDELTA=DBHAT*(ANTOF1(NAT(NB))-SCALA1(NAT(NA))*QM(NA))       DL0397
            CALL SAXPY (NUMAT,DDELTA,B(1,NA),1,DQ,1)                    DL0397
         ELSE IF (NAT(NA).EQ.6.AND.NAT(NB).EQ.7) THEN                   DL0397
            DBHAT=5D0*(1D0-TANH(1D1*B(NB,NA)+2.3D1)**2)                 GDH0597
            DDELTA=DBHAT*(ANTOF1(NAT(NA))-SCALA1(NAT(NB))*QM(NB))       DL0397
            CALL SAXPY (NUMAT,DDELTA,B(1,NB),1,DQ,1)                    DL0397
         ENDIF                                                          DL0397
      ENDIF                                                             DL0397
C     DISTRIBUTE INTO DRVPOL, FACTOR P(MU,NU) INCLUDED.                 DL0397
      DERE=-DOT(BP,DQ,NUMAT)                                            DL0397
      DO 300 MU=NFIRST(NA),NLAST(NA)                                    DL0397
      MUMU=MU*(MU-1)/2                                                  DL0397
      DO 300 NU=NFIRST(NB),NLAST(NB)                                    DL0397
      DRVPOL(MUMU+NU)=P(MUMU+NU)*DERE                                   DL0397
  300 CONTINUE                                                          DL0397
      END
