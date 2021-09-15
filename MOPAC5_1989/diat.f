      SUBROUTINE DIAT(NI,NJ,XI,XJ,DI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*
*   DIAT CALCULATES THE DI-ATOMIC OVERLAP INTEGRALS BETWEEN ATOMS
*        CENTERED AT XI AND XJ.
*
*   ON INPUT NI  = ATOMIC NUMBER OF THE FIRST ATOM.
*            NJ  = ATOMIC NUMBER OF THE SECOND ATOM.
*            XI  = CARTESIAN COORDINATES OF THE FIRST ATOM.
*            XJ  = CARTESIAN COORDINATES OF THE SECOND ATOM.
*
*  ON OUTPUT DI  = DIATOMIC OVERLAP, IN A 9 * 9 MATRIX. LAYOUT OF
*                  ATOMIC ORBITALS IN DI IS
*                  1   2   3   4   5            6     7       8     9
*                  S   PX  PY  PZ  D(X**2-Y**2) D(XZ) D(Z**2) D(YZ)D(XY)
*
*   LIMITATIONS:  IN THIS FORMULATION, NI AND NJ MUST BE LESS THAN 107
*         EXPONENTS ARE ASSUMED TO BE PRESENT IN COMMON BLOCK EXPONT.
*
************************************************************************
      COMMON /KEYWRD/KEYWRD
      CHARACTER*80 KEYWRD
      INTEGER A,PQ2,B,PQ1,AA,BB
      LOGICAL FIRST, ANALYT
      COMMON /EXPONT/ EMUS(107),EMUP(107),EMUD(107)
      DIMENSION DI(9,9),S(3,3,3),UL1(3),UL2(3),C(3,5,5),NPQ(107)
     1          ,XI(3),XJ(3), SLIN(27), IVAL(3,5)
     2, C1(3,5), C2(3,5), C3(3,5), C4(3,5), C5(3,5)
     3, S1(3,3), S2(3,3), S3(3,3)
      EQUIVALENCE(SLIN(1),S(1,1,1))
      EQUIVALENCE (C1(1,1),C(1,1,1)), (C2(1,1),C(1,1,2)),
     1            (C3(1,1),C(1,1,3)), (C4(1,1),C(1,1,4)),
     2            (C5(1,1),C(1,1,5)), (S1(1,1),S(1,1,1)),
     3            (S2(1,1),S(1,1,2)), (S3(1,1),S(1,1,3))
      DATA NPQ/1,0, 2,2,2,2,2,2,2,0, 0,3,3,3,3,3,3,0, 0,4,4,4,4,4,4,4,
     14,4,4,4,4,4,4,4,4,0, 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
     2,32*6,15*0,3,5*0/
      DATA IVAL/1,0,9,1,3,8,1,4,7,1,2,6,0,0,5/
      DATA FIRST /.TRUE./
      ANALYT=(INDEX(KEYWRD,'ANALYT').NE.0)
      X1=XI(1)
      X2=XJ(1)
      Y1=XI(2)
      Y2=XJ(2)
      Z1=XI(3)
      Z2=XJ(3)
      PQ1=NPQ(NI)
      PQ2=NPQ(NJ)
      DO 20 I=1,9
         DO 10 J=1,9
            DI(I,J)=0.0D0
   10    CONTINUE
   20 CONTINUE
      CALL COE(X1,Y1,Z1,X2,Y2,Z2,PQ1,PQ2,C,R)
      IF(PQ1.EQ.0.OR.PQ2.EQ.0.OR.R.GE.10.D0) RETURN
      IF(R.LT.0.001)THEN
         RETURN
      ENDIF
      IA=MIN(PQ1,3)
      IB=MIN(PQ2,3)
      A=IA-1
      B=IB-1
      IF(ANALYT)THEN
         CALL GOVER(NI,NJ,XI,XJ,R,DI)
C#      WRITE(6,*)' OVERLAP FROM GOVER'
C#      WRITE(6,'(4F15.10)')SG
         RETURN
      ENDIF
      IF(NI.LT.18.AND.NJ.LT.18) THEN
         CALL DIAT2(NI,EMUS(NI),EMUP(NI),R,NJ,EMUS(NJ),EMUP(NJ),S)
      ELSE
         UL1(1)=EMUS(NI)
         UL2(1)=EMUS(NJ)
         UL1(2)=EMUP(NI)
         UL2(2)=EMUP(NJ)
         UL1(3)=MAX(EMUD(NI),0.3D0)
         UL2(3)=MAX(EMUD(NJ),0.3D0)
         DO 30 I=1,27
   30    SLIN(I)=0.0D0
         NEWK=MIN(A,B)
         NK1=NEWK+1
         DO 40 I=1,IA
            ISS=I
            IB=B+1
            DO 40 J=1,IB
               JSS=J
               DO 40 K=1,NK1
                  IF(K.GT.I.OR.K.GT.J) GOTO 40
                  KSS=K
                  S(I,J,K)=SS(PQ1,PQ2,ISS,JSS,KSS,UL1(I),UL2(J),R,FIRST)
   40    CONTINUE
      ENDIF
      DO 50 I=1,IA
         KMIN=4-I
         KMAX=2+I
         DO 50 J=1,IB
            IF(J.EQ.2)THEN
               AA=-1
               BB=1
            ELSE
               AA=1
               IF(J.EQ.3) THEN
                  BB=-1
               ELSE
                  BB=1
               ENDIF
            ENDIF
            LMIN=4-J
            LMAX=2+J
            DO 50 K=KMIN,KMAX
               DO 50 L=LMIN,LMAX
                  II=IVAL(I,K)
                  JJ=IVAL(J,L)
                  DI(II,JJ)=S1(I,J)*C3(I,K)*C3(J,L)*AA+
     1(C4(I,K)*C4(J,L)+C2(I,K)*C2(J,L))*BB*S2(I,J)+(C5(I,K)*C5(J,L)
     2+C1(I,K)*C1(J,L))*S3(I,J)
   50 CONTINUE
C#      WRITE(6,*)' OVERLAP FROM DIAT2'
C#      DO 12 I=1,4
C#  12  WRITE(6,'(4F15.10)')(DI(J,I),J=1,4)
      RETURN
      END
