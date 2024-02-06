      SUBROUTINE CHGMP1 (P,Q)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'
      DIMENSION P(*),Q(*)
      LOGICAL AM1HAM,PM3HAM
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /CM1SUM/ B(NUMATM,NUMATM),QM(NUMATM),DELTA(NUMATM)         DL0397
     1               ,QC0(NUMATM),QC(NUMATM)                            DL0397
     2               ,QD0(NUMATM),FD(NUMATM,NUMATM),QD(NUMATM)          DL0397
      COMMON /ONESCM/ ICONTR(100)                                       DL0397
      INCLUDE 'PARAMS.i'
C ---------------------------------------------------------------------+DL0397
C     MAP MULLIKEN NET CHARGES TO CM1 NET CHARGES.
C
C     ON INPUT  P  = TOTAL DENSITY MATRIX (CANONICAL ORDER).
C     ON OUTPUT Q  = CM1 NET CHARGES.
C
C     REWRITTEN IN MATRIX FORM BY DL, 0397:
C     VECTOR QM : MULLIKEN NET CHARGES
C     MATRIX  B : B(I,I)="Bi"; B(I,J)=B(J,I)="-Bij"
C     VECTOR QC : QC0 + SPECIAL TERM FOR AM1 NC BOND
C     VECTOR QD : QD0 + DIAGONAL ELEMENTS OF B.FD (FD IS NON SYMMETRIC)
C                     + SPECIAL TERM FOR AM1 NC BOND
C     VECTOR DELTA(I) = QD(I) + QC(I).QM(I)
C     VECTOR        Q = QM + B.DELTA
C
C ---------------------------------------------------------------------+DL0397
      DATA ZERO /0D0/                                                   DL0397
      PM3HAM=IPM3.NE.0                                                  DL0397
      AM1HAM=IAM1.NE.0                                                  DL0397
      IF (ICONTR(46).EQ.1) THEN                                         DL0397
         ICONTR(46)=2                                                   DL0397
C                                                                       DL0397
C        FILL QC0, QD0, FD WITH RELEVANT PARAMETERS                     DL0397
C                                                                       DL0397
         IF (AM1HAM) THEN                                               DL0397
            DO 19 I=1,NUMAT                                             DL0397
            NI=NAT(I)                                                   DL0397
C           B AT THE POWER ZERO:                                        DL0397
            QC0(I)=SCALA1(NI)                                           DL0397
            QD0(I)=OFSTA1(NI)                                           DL0397
C           B AT THE POWER ONE (SPECIAL NC BOND TERMS EXCLUDED):        DL0397
            CALL SCOPY (NUMAT,ZERO,0,FD(1,I),1)                         DL0397
            IF (NI.EQ.1) THEN                                           DL0397
               DO 10 J=1,NUMAT                                          DL0397
               IF (J.EQ.I) GOTO 10                                      DL0397
C              OFFSET H--SOMETHING                                      DL0397
               FD(J,I)=-HOFFA1(NAT(J))                                  DL0397
   10          CONTINUE                                                 DL0397
            ELSE IF (NI.EQ.7) THEN                                      DL0397
               DO 11 J=1,NUMAT                                          DL0397
               IF (J.EQ.I) GOTO 11                                      DL0397
               IF (NAT(J).NE.6) THEN                                    DL0397
C                 OFFSET N--SOMETHING.NE.CARBON                         DL0397
                  FD(J,I)=-ANTOF1(NAT(J))                               DL0397
               ENDIF                                                    DL0397
   11          CONTINUE                                                 DL0397
            ELSE IF (NI.EQ.8) THEN                                      DL0397
               DO 12 J=1,NUMAT                                          DL0397
               IF (J.EQ.I) GOTO 12                                      DL0397
C              OFFSET O--SOMETHING                                      DL0397
               FD(J,I)=-OXOFF1(NAT(J))                                  DL0397
   12          CONTINUE                                                 DL0397
            ELSE                                                        DL0397
C              add other elements here, labels 13, 14, etc              DL0397
            ENDIF                                                       DL0397
   19       CONTINUE                                                    DL0397
         ELSE IF (PM3HAM) THEN                                          DL0397
            DO 29 I=1,NUMAT                                             DL0397
            NI=NAT(I)                                                   DL0397
C           B AT THE POWER ZERO:                                        DL0397
            QC0(I)=SCALP1(NI)                                           DL0397
            QD0(I)=OFSTP1(NI)                                           DL0397
C           B AT THE POWER ONE:                                         DL0397
            CALL SCOPY (NUMAT,ZERO,0,FD(1,I),1)                         DL0397
            IF (NI.EQ.1) THEN                                           DL0397
               DO 20 J=1,NUMAT                                          DL0397
               IF (J.EQ.I) GOTO 20                                      DL0397
C              OFFSET H--SOMETHING                                      DL0397
               FD(J,I)=-HOFFP1(NAT(J))                                  DL0397
   20          CONTINUE                                                 DL0397
            ELSE                                                        DL0397
C              add other elements here, labels 21, 22, etc.             DL0397
            ENDIF                                                       DL0397
   29       CONTINUE                                                    DL0397
         ELSE                                                           DL0397
C           add other hamiltonians here, labels 3x, 4x, etc.            DL0397
         ENDIF                                                          DL0397
C        NOTE: SPECIAL TERMS FOR NC BOND (AM1)                          DL0397
C              TAKEN INTO ACCOUNT IN LOOPS (210, 220) SEE BELOW.        DL0397
      ENDIF                                                             DL0397
C                                                                       DL0397
C     BUILD MULLIKEN NET CHARGES QM & MATRIX B.                         DL0397
C                                                                       DL0397
      K=0                                                               DL0397
      DO 101 I=1,NUMAT                                                  DL0397
      QM(I)=ZERO                                                        DL0397
      CALL SCOPY (NUMAT,ZERO,0,B(1,I),1)                                DL0397
      DO 101 IAO=NFIRST(I),NLAST(I)                                     DL0397
      IAOC=IAO*(IAO-1)/2                                                DL0397
      DO 100 J=1,I-1                                                    DL0397
      DO 100 JAO=NFIRST(J),NLAST(J)                                     DL0397
      B(J,I)=B(J,I)-P(IAOC+JAO)**2                                      DL0397
  100 CONTINUE                                                          DL0397
      K=K+IAO                                                           DL0397
      QM(I)=QM(I)+P(K)                                                  DL0397
  101 CONTINUE                                                          DL0397
      DO 103 I=1,NUMAT                                                  DL0397
C     SYMMETRISE B & FILL DIAGONAL ENTRIES.                             DL0397
      DO 102 J=1,I-1                                                    DL0397
      B(I,J)=B(J,I)                                                     DL0397
      B(I,I)=B(I,I)-B(J,I)                                              DL0397
      B(J,J)=B(J,J)-B(J,I)                                              DL0397
  102 CONTINUE                                                          DL0397
C     NET MULLIKEN CHARGES QM                                           DL0397
      QM(I)=ZHELP(NAT(I))-QM(I)                                         DL0397
  103 CONTINUE                                                          DL0397
C                                                                       DL0397
C     CALCULATE VECTORS QC (SCALE) & QD (OFFSET).                       DL0397
C                                                                       DL0397
C     USUAL LAWS ARE  QC = QC0                                          DL0397
C                     QD = QD0 + DIAGONAL OF (B.FD).                    DL0397
      DO 200 I=1,NUMAT                                                  DL0397
      QC(I)=QC0(I)                                                      DL0397
      QD(I)=QD0(I)                                                      DL0397
      DO 200 J=1,NUMAT                                                  DL0397
      QD(I)=QD(I)+B(I,J)*FD(J,I)                                        DL0397
  200 CONTINUE                                                          DL0397
      IF (AM1HAM) THEN                                                  DL0397
C        SPECIAL TERMS FOR N--C BOND                                    DL0397
         DO 220 I=1,NUMAT                                               DL0397
         IF (NAT(I).EQ.7) THEN                                          DL0397
            DO 210 J=1,NUMAT                                            DL0397
            IF (NAT(J).EQ.6) THEN                                       DL0397
C              NOTE: B(J,I) HOLDS "-Bkk'"; TANH(-x) = -TANH(x)          DL0397
               BHAT=0.5D0*(1D0-TANH(1D1*B(J,I)+2.3D1))                  DL0397
C              SCALE N--C                                               DL0397
               QC(I)=QC(I)-SCALA1(NAT(I))*BHAT                          DL0397
C              OFFSET N--C                                              DL0397
               QD(I)=QD(I)+ANTOF1(NAT(J))*BHAT                          DL0397
            ENDIF                                                       DL0397
  210       CONTINUE                                                    DL0397
         ENDIF                                                          DL0397
  220    CONTINUE                                                       DL0397
      ENDIF                                                             DL0397
C                                                                       DL0397
C     VECTORS DELTA AND Q FOLLOW.                                       DL0397
C                                                                       DL0397
      DO 300 I=1,NUMAT                                                  DL0397
      DELTA(I)=QC(I)*QM(I)+QD(I)                                        DL0397
  300 CONTINUE                                                          DL0397
C     Q = QM + B.DELTA; B IS SYMMETRIC.                                 DL0397
      DO 310 I=1,NUMAT                                                  DL0397
      Q(I)=DOT(B(1,I),DELTA,NUMAT)+QM(I)                                DL0397
  310 CONTINUE                                                          DL0397
      END
