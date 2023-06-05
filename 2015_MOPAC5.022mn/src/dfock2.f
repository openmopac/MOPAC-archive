      SUBROUTINE DFOCK2 (F,PTOT,P,W,NUMAT,NFIRST,NMIDLE,NLAST,NATI) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
C     INCLUDE 'KEYS.i'                                                  DJG0995
C 
C     DFOCK2 ADDS THE 2-ELECTRON 2-CENTER REPULSION CONTRIB. TO THE FOCK 
C     MATRIX DERIVATIVE WITHIN * MNDO * OR * MINDO * FORMALISMS. 
C  INPUT 
C     F    : 1-ELECTRON CONTRIBUTIONS DERIVATIVES. 
C     PTOT : TOTAL DENSITY MATRIX. 
C     P    : ALPHA OR BETA DENSITY MATRIX. = 0.5 * PTOT 
C     W    : NON VANISHING TWO-ELECTRON INTEGRAL DERIVATIVES 
C            (ORDERED AS DEFINED IN DHCORE). 
C     NATI : # OF THE ATOM SUPPORTING THE VARYING CARTESIAN COORDINATE. 
C  OUPUT 
C     F    : FOCK MATRIX DERIVATIVE WITH RESPECT TO THE CART. COORD. 
C 
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      DIMENSION F(*),PTOT(*),NFIRST(*),NMIDLE(*),NLAST(*),P(*),W(*) 
      DIMENSION IFACT(MAXORB),I1FACT(MAXORB) 
      LOGICAL SWAP,IEQJ 
       SAVE
      IF (ICONTR(31).EQ.1) THEN                                         GDH0195
         ICONTR(31)=2                                                   GDH0195
         ITYPE=1                                                        GDH0195
      ENDIF                                                             GDH0195
   10 GOTO (20,80,40) ITYPE 
C 
C     INITIALISATION (DONE ONLY ONCE) 
C 
C     SET UP ARRAY OF (I*(I-1))/2 AND (I*(I+1))/2 
   20 DO 30 I=1,NLAST(NUMAT) 
      IFACT(I)=(I*(I-1))/2 
   30 I1FACT(I)=IFACT(I)+I 
      IONE=1 
      IF(IMINDO.NE.0) THEN                                              DJG0995
         ITYPE=2 
      ELSE 
         ITYPE=3 
      ENDIF 
      GOTO 10 
C 
C     * MNDO * OR * AM1 * 
C       ----        --- 
C 
   40 KK=0 
      II=NATI 
      IA=NFIRST(II) 
      IC=NLAST(II) 
         DO 70 I=IA,IC 
            KA=IFACT(I) 
            DO 70 J=IA,I 
            KB=IFACT(J) 
            IJ=KA+J 
            IEQJ=I.EQ.J 
            PTOIJ2=2.D0*PTOT(IJ) 
            DO 70 JJ=1,NUMAT 
               IF (II.EQ.JJ) GO TO 70 
               SWAP=JJ.GT.II 
               JA=NFIRST(JJ) 
               JC=NLAST(JJ) 
               FIJ=0.D0 
               DO 60 K=JA,JC 
                  KC=IFACT(K) 
                  IF (SWAP) THEN 
                     IK=KC+I 
                     JK=KC+J 
                  ELSE 
                     IK=KA+K 
                     JK=KB+K 
                  ENDIF 
                  DO 50 L=JA,K-1 
                     IF (SWAP) THEN 
                        KD=IFACT(L) 
                        IL=KD+I 
                        JL=KD+J 
                     ELSE 
                        IL=KA+L 
                        JL=KB+L 
                     ENDIF 
                     KL=KC+L 
                     KK=KK+1 
                     A=W(KK) 
C THIS FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE FOCK 
C MATRIX DERIVATIVE FOR MOLECULE. 
                     FIJ=FIJ+A*PTOT(KL) 
                     F(KL)=F(KL)+A*PTOIJ2 
                     F(IK)=F(IK)-A*P(JL) 
                     F(IL)=F(IL)-A*P(JK) 
                     F(JK)=F(JK)-A*P(IL) 
                     F(JL)=F(JL)-A*P(IK) 
   50             CONTINUE 
                  KK=KK+1 
                  A=W(KK) 
                  KL=KC+L 
                  FIJ=FIJ+A*PTOT(KL) 
                  A=A+A 
                  F(KL)=F(KL)+A*PTOIJ2 
                  F(IK)=F(IK)-A*P(JK) 
                  F(JK)=F(JK)-A*P(IK) 
   60          CONTINUE 
               IF (IEQJ) THEN 
                  F(IJ)=F(IJ)+4.D0*FIJ 
               ELSE 
                  F(IJ)=F(IJ)+2.D0*FIJ 
               ENDIF 
   70 CONTINUE 
C 
      RETURN 
C 
C     * MINDO * 
C       ----- 
C 
   80 KR=0 
      II=NATI 
      IA=NFIRST(II) 
      IB=NLAST(II) 
      DO 100 JJ=1,NUMAT 
         IF (JJ.EQ.II) GO TO 100 
         KR=KR+1 
         ELREP=4.D0*W(KR) 
         JA=NFIRST(JJ) 
         JB=NLAST(JJ) 
         DO 90 I=IA,IB 
            KA=IFACT(I) 
            KK=KA+I 
            DO 90 K=JA,JB 
               LL=I1FACT(K) 
               IF (JA.LT.IA) THEN 
                  IK=KA+K 
               ELSE 
                  IK=LL+I 
               ENDIF 
               F(KK)=F(KK)+PTOT(LL)*ELREP 
               F(LL)=F(LL)+PTOT(KK)*ELREP 
   90    F(IK)=F(IK)-P(IK)*ELREP 
  100 CONTINUE 
      RETURN 
      END 
