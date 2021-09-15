      FUNCTION SPCG(C1,C2,C3,C4,W,WJ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION C1(*),C2(*),C3(*),C4(*),W(*), WJ(*)
      REAL WJ
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107)
     1                ,GSD(107),GPD(107),GDD(107)
      COMMON /EULER / TVEC(3,3), ID
C********************************************************************
C
C     SPCG CALCULATES THE REPULSION BETWEEN ELECTRON 1 IN MOLECULAR
C     ORBITALS C1 AND C2 AND ELECTRON 2 IN M.O.S C3 AND C4 FOR THE
C     VALENCE SP SHELL AT AN MNDO OR MINDO/3 LEVEL.
C
C                            USAGE
C      XJ=SPCG(C(1,I),C(1,J),C(1,K),C(1,L))
C  OR, XJ=<I(1),J(1)/K(2),L(2)>
C
C    ON INPUT C1    THE FIRST COLUMN MOLECULAR ORBITAL OF ELECTRON ONE.
C             C2        SECOND
C             C3        FIRST                                      TWO.
C             C4        SECOND
C
C   ON OUTPUT SPCG   =   <C1(1)*C2(1)/C3(2)*C4(2)>
C*********************************************************************
      COMMON /KEYWRD/ KEYWRD
      CHARACTER*80 KEYWRD
      LOGICAL LID
      DATA ITYPE /1/
   10 CONTINUE
      GOTO (20,70,30) ITYPE
   20 CONTINUE
      LID=(ID.EQ.0)
      IF(INDEX(KEYWRD,'MINDO') .NE. 0) THEN
         ITYPE=2
      ELSE
         ITYPE=3
      ENDIF
      GOTO 10
*                           ******************
*                           *      MNDO      *
*                           *     OPTION     *
*                           ******************
   30 SPCG=0.0D00
      KK=0
      DO 60 II=1,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
         IMINUS=II-1
         DO 50 JJ=1,IMINUS
            JA=NFIRST(JJ)
            JB=NLAST(JJ)
            DO 40 I=IA,IB
               DO 40 J=IA,I
                  DO 40 K=JA,JB
                     DO 40 L=JA,K
                        KK=KK+1
                        IF(LID) THEN
                           WINT=W(KK)
                        ELSE
                           WINT=WJ(KK)
                        ENDIF
                        SPCG=SPCG+WINT*(C1(I)*C2(J)*C3(K)*C4(L)
     1 + C1(K)*C2(L)*C3(I)*C4(J))
                        IF(I.NE.J)
     1SPCG=SPCG+WINT*(C1(J)*C2(I)*C3(K)*C4(L)
     2 + C1(K)*C2(L)*C3(J)*C4(I))
                        IF(K.NE.L) SPCG=SPCG+WINT*(C1(I)*C2(J)*C3(L)*C4(
     1K) + C1(L)*C2(K)*C3(I)*C4(J))
                        IF((I.NE.J).AND.(K.NE.L))SPCG=SPCG+WINT*(C1(J)*C
     12(I)*C3(L)*C4(K) +C1(L)*C2(K)*C3(J)*C4(I))
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
      GOTO 110
*                           ******************
*                           *     MINDO/3    *
*                           *     OPTION     *
*                           ******************
   70 CONTINUE
      SPCG=0.D0
      KR=0
      DO 100 II=1,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
         IM1=II-1
         DO 90 JJ=1,IM1
            KR=KR+1
            IF(LID) THEN
               ELREP=W(KR)
            ELSE
               ELREP=WJ(KR)
            ENDIF
            JA=NFIRST(JJ)
            JB=NLAST(JJ)
            DO 80 I=IA,IB
               DO 80 K=JA,JB
   80       SPCG=SPCG+ELREP*(C1(I)*C2(I)*C3(K)*C4(K)+
     1                                 C1(K)*C2(K)*C3(I)*C4(I))
   90    CONTINUE
  100 CONTINUE
  110 CONTINUE
      ATEMP=SPCG
      IS1=0
      DO 150 I1=1,NUMAT
         IS1=IS1+1
         IZN=NAT(I1)
C
C      (SS/SS)
C
         SPCG=SPCG+C1(IS1)*C2(IS1)*C3(IS1)*C4(IS1)*GSS(IZN)
         IF(IZN.LT.3) GO TO 150
         IS=IS1
         IS1=IS1+1
         IX=IS1
         IS1=IS1+1
         IY=IS1
         IS1=IS1+1
         IZ=IS1
         SPCG=SPCG+GPP(IZN)*
     1                       (
C
C      (PP/PP) FOR P=X,Y,Z
C
     2           C1(IX)*C2(IX)*C3(IX)*C4(IX)+
     2           C1(IY)*C2(IY)*C3(IY)*C4(IY)+
     3           C1(IZ)*C2(IZ)*C3(IZ)*C4(IZ)
     4                       )
         SPCG=SPCG+GSP(IZN)*
     1                       (
C
C      (SS/PP)+(PP/SS) FOR P=X,Y,Z
C
     1           C1(IS)*C2(IS)*C3(IX)*C4(IX)+
     2           C1(IS)*C2(IS)*C3(IY)*C4(IY)+
     3           C1(IS)*C2(IS)*C3(IZ)*C4(IZ)+
     4           C1(IX)*C2(IX)*C3(IS)*C4(IS)+
     5           C1(IY)*C2(IY)*C3(IS)*C4(IS)+
     6           C1(IZ)*C2(IZ)*C3(IS)*C4(IS)
     7                       )
         SPCG=SPCG+GP2(IZN)*
     1                       (
C
C      (PP/P,P,)+(P,P,/PP) FOR P.NE.P,=X,Y,Z
C
     1           C1(IX)*C2(IX)*C3(IY)*C4(IY)+
     2           C1(IX)*C2(IX)*C3(IZ)*C4(IZ)+
     3           C1(IY)*C2(IY)*C3(IZ)*C4(IZ)+
     4           C1(IY)*C2(IY)*C3(IX)*C4(IX)+
     5           C1(IZ)*C2(IZ)*C3(IX)*C4(IX)+
     6           C1(IZ)*C2(IZ)*C3(IY)*C4(IY)
     7                       )
         TEMP1=HSP(IZN)
         DO 120 J1=IX,IZ
            SPCG=SPCG+TEMP1*
     1                       (
C
C      (SP/SP)+(SP/PS)+(PS/SP)+(PS/PS) FOR P=X,Y,Z
C
     2           C1(IS)*C2(J1)*C3(J1)*C4(IS)+
     2           C1(IS)*C2(J1)*C3(IS)*C4(J1)+
     3           C1(J1)*C2(IS)*C3(IS)*C4(J1)+
     4           C1(J1)*C2(IS)*C3(J1)*C4(IS)
     5                       )
  120    CONTINUE
         TEMP1=0.5D0*(GPP(IZN)-GP2(IZN))
         DO 140 J1=IX,IZ
            DO 130 K1=IX,IZ
               IF(J1.EQ.K1) GO TO 130
               SPCG=SPCG+TEMP1*
     1                       (
C
C      (PP,/PP,)+(PP,/P,P)+(P,P/PP,)+(P,P/P,P) FOR P.NE.P,=X,Y,Z
C
     2           C1(J1)*C2(K1)*C3(J1)*C4(K1)+
     3           C1(J1)*C2(K1)*C3(K1)*C4(J1)
     4                       )
  130       CONTINUE
  140    CONTINUE
  150 CONTINUE
      RETURN
      END
