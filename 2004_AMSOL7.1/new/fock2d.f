      SUBROUTINE FOCK2D (F,PTOT, P, W, WJ, WK, NUMAT, NFIRST 
     1                  ,NMIDLE, NLAST) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      INCLUDE 'KEYS.i'                                                  DJG0995
C*********************************************************************** 
C 
C FOCK2D FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE FOCK 
C MATRIX 
C ON INPUT  PTOT = TOTAL DENSITY MATRIX. 
C           P    = ALPHA OR BETA DENSITY MATRIX. 
C           W    = TWO-ELECTRON INTEGRAL MATRIX. 
C 
C  ON OUTPUT F   = PARTIAL FOCK MATRIX 
C*********************************************************************** 
      COMMON /EULER / TVEC(3,3), ID 
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      DIMENSION F(*), PTOT(*), WJ(*), WK(*), NFIRST(*), NMIDLE(*), 
     1          NLAST(*), P(*), W(*) 
      DIMENSION IFACT(300), I1FACT(300) 
      LOGICAL LID,IEQJ 
      SAVE
      IF (ICONTR(32).EQ.1) THEN                                         GDH0195
         ICONTR(32)=2                                                   GDH0195
         ITYPE=1                                                        GDH0195
      ENDIF                                                             GDH0195
   10 GOTO (20,100,40) ITYPE 
C 
C   SET UP ARRAY OF (I*(I-1))/2 
C 
   20 DO 30 I=1,300 
         IFACT(I)=(I*(I-1))/2 
   30 I1FACT(I)=IFACT(I)+I 
      LID=(ID.EQ.0) 
         IONE=1 
      IF(IMINDO.NE.0) THEN                                              DJG0995
         ITYPE=2 
      ELSE 
         ITYPE=3 
      ENDIF 
      GOTO 10 
   40 KK=0 
      IF (.NOT.LID) THEN 
         NORBS=NLAST(NUMAT) 
         LINEA1=NORBS*(NORBS+1)/2+1 
         P(LINEA1)=0.D0 
      ENDIF 
      DO 90 II=1,NUMAT 
         IA=NFIRST(II) 
         IB=NLAST(II) 
         IC=NMIDLE(II) 
            IMINUS=II-IONE 
            DO 90 JJ=1,IMINUS 
               JA=NFIRST(JJ) 
               JB=NLAST(JJ) 
               JC=NMIDLE(JJ) 
               IF(LID) THEN 
                  DO 70 I=IA,IC 
                     KA=IFACT(I) 
                     DO 70 J=IA,I 
                        KB=IFACT(J) 
                        IJ=KA+J 
                        IEQJ=I.EQ.J 
                        PTOIJ2=PTOT(IJ)*2.D0 
                        FIJ=0.D0 
                           DO 60 K=JA,JC 
                              KC=IFACT(K) 
                              IK=KA+K 
                              JK=KB+K 
                              DO 50 L=JA,K-1 
                                 IL=KA+L 
                                 JL=KB+L 
                                 KL=KC+L 
                                 KK=KK+1 
                                 A=W(KK) 
C 
C     A  IS THE REPULSION INTEGRAL (I,J/K,L) WHERE ORBITALS I AND J ARE 
C     ON ATOM II, AND ORBITALS K AND L ARE ON ATOM JJ. 
C     IJ IS THE LOCATION OF THE MATRIX ELEMENTS BETWEEN ATOMIC ORBITALS 
C     I AND J.  SIMILARLY FOR IK ETC. 
C 
C THIS FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE DIATOMIC 
C FOCK MATRIX FOR MOLECULE. 
                                 FIJ=FIJ+A*PTOT(KL) 
                                 F(KL)=F(KL)+A*PTOIJ2 
                                 F(IK)=F(IK)-A*P(JL) 
                                 F(IL)=F(IL)-A*P(JK) 
                                 F(JK)=F(JK)-A*P(IL) 
                                 F(JL)=F(JL)-A*P(IK) 
   50                         CONTINUE 
                              KK=KK+1 
                              A=W(KK) 
                              KL=KC+K 
                              FIJ=FIJ+A*PTOT(KL) 
                              A=A+A 
                              F(KL)=F(KL)+A*PTOIJ2 
                              F(IK)=F(IK)-A*P(JK) 
                              F(JK)=F(JK)-A*P(IK) 
   60                      CONTINUE 
                           IF (IEQJ) THEN 
                              F(IJ)=F(IJ)+4.D0*FIJ 
                           ELSE 
                              F(IJ)=F(IJ)+2.D0*FIJ 
                           ENDIF 
   70             CONTINUE 
               ELSE 
                  DO 80 I=IA,IC 
                     KA=IFACT(I) 
                     DO 80 J=IA,I 
                        KB=IFACT(J) 
                        IJ=KA+J 
                        IEQJ=I.EQ.J 
                        PTOIJ2=PTOT(IJ)*2.D0 
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
                              KK=KK+1 
                              AJ=WJ(KK) 
                              AK=WK(KK) 
C 
C     A  IS THE REPULSION INTEGRAL (I,J/K,L) WHERE ORBITALS I AND J ARE 
C     ON ATOM II, AND ORBITALS K AND L ARE ON ATOM JJ. 
C     IJ IS THE LOCATION OF THE MATRIX ELEMENTS BETWEEN ATOMIC ORBITALS 
C     I AND J.  SIMILARLY FOR IK ETC. 
C 
C THIS FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE DIATOMIC 
C FOCK MATRIX FOR POLYMERS. 
                              IF (KL.EQ.IJ) THEN 
                                 F(IJ)=F(IJ)+2.D0*AJ*PTOIJ2 
                              ELSE IF (KL.LT.IJ) THEN 
                                 AJKL=2.D0*AJ*PTOT(KL) 
                                 IF (IEQJ) AJKL=AJKL+AJKL 
                                 F(IJ)=F(IJ)+AJKL 
                                 AJIJ=AJ*PTOIJ2 
                                 IF (K.EQ.L) AJIJ=AJIJ+AJIJ 
                                 F(KL)=F(KL)+AJIJ 
                                 F(IK)=F(IK)-AK*P(JL) 
                                 F(IL)=F(IL)-AK*P(JK) 
                                 F(JK)=F(JK)-AK*P(IL) 
                                 F(JL)=F(JL)-AK*P(IK) 
                              ENDIF 
   80             CONTINUE 
               ENDIF 
   90 CONTINUE 
C 
      RETURN 
  100 KR=0 
      DO 130 II=1,NUMAT 
         IA=NFIRST(II) 
         IB=NLAST(II) 
         IM1=II-IONE 
         DO 120 JJ=1,IM1 
            KR=KR+1 
            IF(LID)THEN 
               ELREP=4.D0*W(KR) 
               ELEXC=ELREP 
            ELSE 
               ELREP=4.D0*WJ(KR) 
               ELEXC=4.D0*WK(KR) 
            ENDIF 
            JA=NFIRST(JJ) 
            JB=NLAST(JJ) 
            DO 110 I=IA,IB 
               KA=IFACT(I) 
               KK=KA+I 
               DO 110 K=JA,JB 
                  LL=I1FACT(K) 
                  IK=KA+K 
                  F(KK)=F(KK)+PTOT(LL)*ELREP 
                  F(LL)=F(LL)+PTOT(KK)*ELREP 
  110       F(IK)=F(IK)-P(IK)*ELEXC 
  120    CONTINUE 
  130 CONTINUE 
      RETURN 
      END 
