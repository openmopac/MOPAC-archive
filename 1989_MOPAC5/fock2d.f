      SUBROUTINE FOCK2D(F,PTOT, P, W, WJ, WK, NUMAT, NFIRST,
     1NMIDLE, NLAST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*), PTOT(*), WJ(*), WK(*), NFIRST(*), NMIDLE(*),
     1          NLAST(*), P(*), W(*)
C***********************************************************************
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
      DIMENSION SPPOP(2), DPOP(2), IFACT(20), I1FACT(20)
      LOGICAL LID
      CHARACTER*80 KEYWRD
      DATA ITYPE /1/
   10 CONTINUE
      GOTO (20,170,40) ITYPE
   20 CONTINUE
C
C   SET UP ARRAY OF (I*(I-1))/2
C
      DO 30 I=1,20
         IFACT(I)=(I*(I-1))/2
   30 I1FACT(I)=IFACT(I)+I
      LID=(ID.EQ.0)
      IONE=1
      IF(INDEX(KEYWRD,'MINDO') .NE. 0) THEN
         ITYPE=2
      ELSE
         ITYPE=3
      ENDIF
      GOTO 10
   40 KK=0
      NORBS=NLAST(NUMAT)
      LINEA1=(NORBS*(NORBS+1))/2 + 1
      P(LINEA1)=0.D0
      DO 160 II=1,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
         IC=NMIDLE(II)
         SUM=0.D0
         DO 50 I=IA,IC
   50    SUM=SUM+PTOT(I1FACT(I))
         SPPOP(II)=SUM
         SUM=0.D0
         DO 60 I=IC+1,IB
   60    SUM=SUM+PTOT(I1FACT(I))
         DPOP(II)=SUM
         IMINUS=II-IONE
         DO 150 JJ=1,IMINUS
            JA=NFIRST(JJ)
            JB=NLAST(JJ)
            JC=NMIDLE(JJ)
            DREP=WJ(KK+1)
            IF(LID) THEN
               DREP=W(KK+1)
               DO 70 I=IA,IC
                  KA=IFACT(I)
                  DO 70 J=IA,I
                     KB=IFACT(J)
                     IJ=KA+J
                     AA=2.0D00
                     IF (I.EQ.J) AA=1.0D00
                     DO 70 K=JA,JC
                        KC=IFACT(K)
                        IK=KA+K
                        JK=KB+K
                        DO 70 L=JA,K
                           IL=KA+L
                           JL=KB+L
                           KL=KC+L
                           BB=2.0D00
                           IF (K.EQ.L) BB=1.0D00
                           KK=KK+1
                           A=W(KK)
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
                           F(IJ)=F(IJ)+BB*A*PTOT(KL)
                           F(KL)=F(KL)+AA*A*PTOT(IJ)
                           A=A*AA*BB*0.25D0
                           F(IK)=F(IK)-A*P(JL)
                           F(IL)=F(IL)-A*P(JK)
                           F(JK)=F(JK)-A*P(IL)
                           F(JL)=F(JL)-A*P(IK)
   70          CONTINUE
            ELSE
               DO 80 I=IA,IC
                  KA=IFACT(I)
                  DO 80 J=IA,I
                     KB=IFACT(J)
                     IJ=KA+J
                     AA=2.0D00
                     IF (I.EQ.J) AA=1.0D00
                     DO 80 K=JA,JC
                        KC=IFACT(K)
                        IF(I.GE.K) THEN
                           IK=KA+K
                        ELSE
                           IK=LINEA1
                        ENDIF
                        IF(J.GE.K) THEN
                           JK=KB+K
                        ELSE
                           JK=LINEA1
                        ENDIF
                        DO 80 L=JA,K
                           IF(I.GE.L) THEN
                              IL=KA+L
                           ELSE
                              IL=LINEA1
                           ENDIF
                           IF(J.GE.L) THEN
                              JL=KB+L
                           ELSE
                              JL=LINEA1
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
   80          CONTINUE
            ENDIF
C
C   D-ORBITAL CORRECTION
C
            DO 90 I=IC+1,IB
               KA=IFACT(I)
               DO 90 J=JA,JB
                  IJ=KA+J
C
C   ATOM J (S, P, AND D (IF PRESENT)) EXCHANGE WITH ATOM I (D ONLY)
C
   90       F(IJ)=F(IJ)-0.5D0*DREP*P(IJ)
            DO 100 I=IA,IC
               KA=IFACT(I)
               DO 100 J=JC+1,JB
                  IJ=KA+J
C
C    ATOM J (D(IF PRESENT)) EXCHANGE WITH ATOM I (S AND P ONLY)
C
  100       F(IJ)=F(IJ)-0.5D0*DREP*P(IJ)
C
C                      THE COULOMB REPULSION TERMS.
C
C     FIRST, ATOM J (S, P AND D SHELLS) BEING REPELLED BY ATOM I(DSHELL)
C
            DO 110 J=JA,JB
               J2=I1FACT(J)
  110       F(J2)=F(J2)+DREP*DPOP(II)
C
C     ATOM J (D SHELL) BEING REPELLED BY ATOM I (S AND P SHELLS)
C
            DO 120 J=JC+1,JB
               J2=I1FACT(J)
  120       F(J2)=F(J2)+DREP*SPPOP(II)
C
C     ATOM I (S, P AND D SHELLS) BEING REPELLED BY ATOM J (D SHELL)
C
            DO 130 I=IA,IB
               I2=I1FACT(I)
  130       F(I2)=F(I2)+DREP*DPOP(JJ)
C
C    ATOM I (D SHELL) BEING REPELLED BY ATOM J (S AND P SHELLS)
C
            DO 140 I=IC+1,IB
               I2=I1FACT(I)
  140       F(I2)=F(I2)+DREP*SPPOP(JJ)
  150    CONTINUE
  160 CONTINUE
C
      RETURN
  170 KR=0
      DO 200 II=1,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
         IM1=II-IONE
         DO 190 JJ=1,IM1
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
            DO 180 I=IA,IB
               KA=IFACT(I)
               KK=KA+I
               DO 180 K=JA,JB
                  LL=I1FACT(K)
                  IK=KA+K
                  F(KK)=F(KK)+PTOT(LL)*ELREP
                  F(LL)=F(LL)+PTOT(KK)*ELREP
  180       F(IK)=F(IK)-P(IK)*ELEXC
  190    CONTINUE
  200 CONTINUE
      RETURN
      END
