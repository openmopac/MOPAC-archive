      SUBROUTINE FOCK2(F, PTOT, P, W, WJ, WK, NUMAT, NFIRST,
     1NMIDLE, NLAST,*)                                                  CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION F(*), PTOT(*), WJ(*), WK(*), NFIRST(*), NMIDLE(*),
     1          NLAST(*), P(*), W(*)
C***********************************************************************
C
C THIS IS THE NEW VERSION OF FOCK2 RECEIVED FROM J.J.P.STEWART ON 9JUN89
C 11JUN89 JJPS MOD - "DO 240 I=IA,IC" CHANGED TO "DO 240 I=IA,IB"
C
C FOCK2 FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE FOCK
C MATRIX
C ON INPUT  PTOT = TOTAL DENSITY MATRIX.
C           P    = ALPHA OR BETA DENSITY MATRIX.
C           W    = TWO-ELECTRON INTEGRAL MATRIX.
C
C  ON OUTPUT F   = PARTIAL FOCK MATRIX
C***********************************************************************
      COMMON /EULER / TVEC(3,3), ID
      COMMON /KEYWRD/ KEYWRD
      DIMENSION IFACT(MAXORB),
     1I1FACT(MAXORB), JINDEX(256), KINDEX(256), IJPERM(10), LLPERM(10),
     2PK(16), PJA(16), PJB(16), GJA(4,4), GJB(4,4), MMPERM(10),
     3GK(16), IKPERM(16), JPERMA(16), JPERMB(16), P2(MAXORB,MAXORB),
     4F2(MAXORB,MAXORB), PTOT2(MAXORB,MAXORB), JJNDEX(256)
      CHARACTER*160 KEYWRD
      LOGICAL LID
         SAVE                                                           GL0892
      DATA ITYPE /1/
   10 CONTINUE
      GOTO (20,330,50) ITYPE
   20 CONTINUE
C
C   SET UP ARRAY OF LOWER HALF TRIANGLE INDICES (PASCAL'S TRIANGLE)
C
      DO 30 I=1,MAXORB
         IFACT(I)=(I*(I-1))/2
   30 I1FACT(I)=IFACT(I)+I
C
C   SET UP GATHER-SCATTER TYPE ARRAYS FOR USE WITH TWO-ELECTRON
C   INTEGRALS.  JINDEX ARE THE INDICES OF THE J-INTEGRALS FOR ATOM I
C   INTEGRALS.  JJNDEX ARE THE INDICES OF THE J-INTEGRALS FOR ATOM J
C               KINDEX ARE THE INDICES OF THE K-INTEGRALS
C
      M=0
      DO 40 I=1,4
         DO 40 J=1,4
            IJ=MIN(I,J)
            JI=I+J-IJ
            DO 40 K=1,4
               IK=MIN(I,K)
               KI=I+K-IK
               DO 40 L=1,4
                  M=M+1
                  KL=MIN(K,L)
                  LK=K+L-KL
                  JL=MIN(J,L)
                  LJ=J+L-JL
                  KINDEX(M)= IFACT(LJ) +JL + 10*( IFACT(KI) +IK) -10
   40 JINDEX(M)=(IFACT(JI) + IJ)*10 + IFACT(LK) + KL - 10
      L=0
      DO 41 I=1,4
      I1=(I-1)*4
      DO 41 J=1,I
      I1=I1+1
      L=L+1
      IJPERM(L)=I1
      MMPERM(L)=IJPERM(L)-16
      LLPERM(L)=(I1-1)*16
  41  CONTINUE
      L=0
      DO 42 I=1,10
      M=MMPERM(I)
      L=LLPERM(I)
      DO 42 K=1,16
      L=L+1
      M=M+16
  42  JJNDEX(L)=JINDEX(M)
      LID=(ID.EQ.0)
      IONE=1
      IF(ID.NE.0)IONE=0
      IF(INDEX(KEYWRD,'MINDO') .NE. 0) THEN
         ITYPE=2
      ELSE
         ITYPE=3
      ENDIF
      GOTO 10
   50 KK=0
      L=0
      DO 51 I=1,NUMAT
      IA=NFIRST(I)
      IB=NLAST(I)
      M=0
      DO 141 J=IA,IB
      DO 141 K=IA,IB
      M=M+1
      JK=MIN(J,K)
      KJ=K+J-JK
      JK=JK+(KJ*(KJ-1))/2
      PTOT2(I,M)=PTOT(JK)
 141  CONTINUE
  51  CONTINUE
      DO 320 II=1,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
         IMINUS=II-IONE
         DO 310 JJ=1,IMINUS
            JA=NFIRST(JJ)
            JB=NLAST(JJ)
            JC=NMIDLE(JJ)
            IF(LID) THEN
               DREP=W(KK+1)
               IF(IB-IA.GE.3.AND.JB-JA.GE.3)THEN
C
C                         HEAVY-ATOM  - HEAVY-ATOM
C
C   EXTRACT COULOMB TERMS
C
                  DO 311 I=1,16
                  PJA(I)=PTOT2(II,I)
  311             PJB(I)=PTOT2(JJ,I)
C
C  COULOMB TERMS
C
               CALL JAB(IA,JA,LLPERM,JINDEX, JJNDEX, PJA,PJB,W(KK+1),F,  CSTP(call)
     & *9999)                                                           CSTP(call)
C
C  EXCHANGE TERMS
C
C
C  EXTRACT INTERSECTION OF ATOMS II AND JJ IN THE SPIN DENSITY MATRIX
C
                  L=0
                  DO 110 I=IA,IB
                     I1=IFACT(I)+JA
                     DO 110 J=I1,I1+3
                        L=L+1
  110             PK(L)=P(J)
                  CALL KAB(IA,JA, PK, W(KK+1), KINDEX, F,*9999)          CSTP(call)
                  KK=KK+100
               ELSEIF(IB-IA.GE.3)THEN
C
C                         LIGHT-ATOM  - HEAVY-ATOM
C
C
C   COULOMB TERMS
C
                  SUMDIA=0.D0
                  SUMOFF=0.D0
                  LL=I1FACT(JA)
                  K=0
                  DO 211 I=0,3
                     J1=IFACT(IA+I)+IA-1
                     DO 201 J=0,I-1
                        K=K+1
                        J1=J1+1
                        F(J1)=F(J1)+PTOT(LL)*W(KK+K)
  201                SUMOFF=SUMOFF+PTOT(J1)*W(KK+K)
                     J1=J1+1
                     K=K+1
                     F(J1)=F(J1)+PTOT(LL)*W(KK+K)
  211             SUMDIA=SUMDIA+PTOT(J1)*W(KK+K)
                  F(LL)=F(LL)+SUMOFF*2.D0+SUMDIA
C
C  EXCHANGE TERMS
C
C
C  EXTRACT INTERSECTION OF ATOMS II AND JJ IN THE SPIN DENSITY MATRIX
C
                  K=0
                  DO 231 I=IA,IB
                  I1=IFACT(I)+JA
                     SUM=0.D0
                     DO 221 J=IA,IB
                     K=K+1
                        J1=IFACT(J)+JA
  221                SUM=SUM+P(J1)*W(KK+JINDEX(K))
  231             F(I1)=F(I1)-SUM
                  KK=KK+10
               ELSEIF(JB-JA.GE.3)THEN
C
C                         HEAVY-ATOM - LIGHT-ATOM
C
C
C   COULOMB TERMS
C
                  SUMDIA=0.D0
                  SUMOFF=0.D0
                  LL=I1FACT(IA)
                  K=0
                  DO 210 I=0,3
                     J1=IFACT(JA+I)+JA-1
                     DO 200 J=0,I-1
                        K=K+1
                        J1=J1+1
                        F(J1)=F(J1)+PTOT(LL)*W(KK+K)
  200                SUMOFF=SUMOFF+PTOT(J1)*W(KK+K)
                     J1=J1+1
                     K=K+1
                     F(J1)=F(J1)+PTOT(LL)*W(KK+K)
  210             SUMDIA=SUMDIA+PTOT(J1)*W(KK+K)
                  F(LL)=F(LL)+SUMOFF*2.D0+SUMDIA
C
C  EXCHANGE TERMS
C
C
C  EXTRACT INTERSECTION OF ATOMS II AND JJ IN THE SPIN DENSITY MATRIX
C
                  K=IFACT(IA)+JA
                  J=0
                  DO 230 I=K,K+3
                     SUM=0.D0
                     DO 220 L=K,K+3
                        J=J+1
  220                SUM=SUM+P(L)*W(KK+JINDEX(J))
  230             F(I)=F(I)-SUM
                  KK=KK+10
               ELSE
C
C                         LIGHT-ATOM - LIGHT-ATOM
C
               I1=I1FACT(IA)
               J1=I1FACT(JA)
               IJ=I1+JA-IA
               F(I1)=F(I1)+PTOT(J1)*W(KK+1)
               F(J1)=F(J1)+PTOT(I1)*W(KK+1)
               F(IJ)=F(IJ)-P   (IJ)*W(KK+1)
               KK=KK+1
               ENDIF
            ELSE
               DREP=WJ(KK+1)
               DO 240 I=IA,IB
                  KA=IFACT(I)
                  DO 240 J=IA,I
                     KB=IFACT(J)
                     IJ=KA+J
                     AA=2.0D00
                     IF (I.EQ.J) AA=1.0D00
                     DO 240 K=JA,JC
                        KC=IFACT(K)
                        IF(I.GE.K) THEN
                           IK=KA+K
                        ELSE
                           IK=0
                        ENDIF
                        IF(J.GE.K) THEN
                           JK=KB+K
                        ELSE
                           JK=0
                        ENDIF
                        DO 240 L=JA,K
                           IF(I.GE.L) THEN
                              IL=KA+L
                           ELSE
                              IL=0
                           ENDIF
                           IF(J.GE.L) THEN
                              JL=KB+L
                           ELSE
                              JL=0
                           ENDIF
                           KL=KC+L
                           BB=2.0D00
                           IF (K.EQ.L) BB=1.0D00
                           KK=KK+1
                           AJ=WJ(KK)
                           AK=WK(KK)
C
C     A  IS THE REPULSION INTEGRAL (I,J/K,L) WHERE ORBITALS I AND J ARE
C     ON ATOM II, AND ORBITALS K AND L ARE ON ATOM JJ.
C     AA AND BB ARE CORRECTION FACTORS SINCE
C     (I,J/K,L)=(J,I/K,L)=(I,J/L,K)=(J,I/L,K)
C     IJ IS THE LOCATION OF THE MATRIX ELEMENTS BETWEEN ATOMIC ORBITALS
C     I AND J.  SIMILARLY FOR IK ETC.
C
C THIS FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE FOCK
C MATRIX.  THE CODE HERE IS HARD TO FOLLOW, AND IMPOSSIBLE TO MODIFY!,
C BUT IT WORKS,
                           IF(KL.LE.IJ)THEN
                              IF(I.EQ.K.AND.AA+BB.LT.2.1D0)THEN
                                 BB=BB*0.5D0
                                 AA=AA*0.5D0
                                 F(IJ)=F(IJ)+BB*AJ*PTOT(KL)
                                 F(KL)=F(KL)+AA*AJ*PTOT(IJ)
                              ELSE
                                 F(IJ)=F(IJ)+BB*AJ*PTOT(KL)
                                 F(KL)=F(KL)+AA*AJ*PTOT(IJ)
                                 A=AK*AA*BB*0.25D0
                                 F(IK)=F(IK)-A*P(JL)
                                 F(IL)=F(IL)-A*P(JK)
                                 F(JK)=F(JK)-A*P(IL)
                                 F(JL)=F(JL)-A*P(IK)
                              ENDIF
                           ENDIF
  240          CONTINUE
            ENDIF
  310    CONTINUE
  320 CONTINUE
C
      RETURN
  330 KR=0
      DO 360 II=1,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
         IM1=II-IONE
         DO 350 JJ=1,IM1
            KR=KR+1
            IF(LID)THEN
               ELREP=W(KR)
               ELEXC=ELREP
            ELSE
               ELREP=WJ(KR)
               ELEXC=WK(KR)
            ENDIF
            JA=NFIRST(JJ)
            JB=NLAST(JJ)
            DO 340 I=IA,IB
               KA=IFACT(I)
               KK=KA+I
               DO 340 K=JA,JB
                  LL=I1FACT(K)
                  IK=KA+K
                  F(KK)=F(KK)+PTOT(LL)*ELREP
                  F(LL)=F(LL)+PTOT(KK)*ELREP
  340       F(IK)=F(IK)-P(IK)*ELEXC
  350    CONTINUE
  360 CONTINUE
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY FOCK2_INIT                                                  CSAV
                 A = 0.0D0                                              CSAV
                AA = 0.0D0                                              CSAV
                AJ = 0.0D0                                              CSAV
                AK = 0.0D0                                              CSAV
                BB = 0.0D0                                              CSAV
              DREP = 0.0D0                                              CSAV
             ELEXC = 0.0D0                                              CSAV
             ELREP = 0.0D0                                              CSAV
                GK = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
                I1 = 0                                                  CSAV
            I1FACT = 0                                                  CSAV
                IA = 0                                                  CSAV
                IB = 0                                                  CSAV
                ID = 0                                                  CSAV
             IFACT = 0                                                  CSAV
                II = 0                                                  CSAV
                IJ = 0                                                  CSAV
            IJPERM = 0                                                  CSAV
                IK = 0                                                  CSAV
            IKPERM = 0                                                  CSAV
                IL = 0                                                  CSAV
               IM1 = 0                                                  CSAV
            IMINUS = 0                                                  CSAV
              IONE = 0                                                  CSAV
             ITYPE = 1                                                  CSAV
                 J = 0                                                  CSAV
                J1 = 0                                                  CSAV
                JA = 0                                                  CSAV
                JB = 0                                                  CSAV
                JC = 0                                                  CSAV
                JI = 0                                                  CSAV
            JINDEX = 0                                                  CSAV
                JJ = 0                                                  CSAV
            JJNDEX = 0                                                  CSAV
                JK = 0                                                  CSAV
                JL = 0                                                  CSAV
            JPERMA = 0                                                  CSAV
            JPERMB = 0                                                  CSAV
                 K = 0                                                  CSAV
                KA = 0                                                  CSAV
                KB = 0                                                  CSAV
                KC = 0                                                  CSAV
                KI = 0                                                  CSAV
            KINDEX = 0                                                  CSAV
                KJ = 0                                                  CSAV
                KK = 0                                                  CSAV
                KL = 0                                                  CSAV
                KR = 0                                                  CSAV
                 L = 0                                                  CSAV
               LID = .FALSE.                                            CSAV
                LJ = 0                                                  CSAV
                LK = 0                                                  CSAV
                LL = 0                                                  CSAV
            LLPERM = 0                                                  CSAV
                 M = 0                                                  CSAV
            MMPERM = 0                                                  CSAV
               PJA = 0.0D0                                              CSAV
               PJB = 0.0D0                                              CSAV
                PK = 0.0D0                                              CSAV
CSAV     To save time deactivated reinitialization of PTOT2 as it appears not
CSAV     to require clearing before reuse in subsequent MOPAC runs.
CSAV             PTOT2 = 0.0D0                                              CSAV
               SUM = 0.0D0                                              CSAV
            SUMDIA = 0.0D0                                              CSAV
            SUMOFF = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
