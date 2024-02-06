      SUBROUTINE FOCK2 (F, PTOT, P, W, WJ, WK, NUMAT, NFIRST 
     1                 ,NMIDLE, NLAST) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
C*********************************************************************** 
C 
C FOCK2 FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE FOCK 
C MATRIX WITHIN * MNDO * OR * MINDO * FORMALISMS. 
C ON INPUT  PTOT = TOTAL DENSITY MATRIX. 
C           P    = ALPHA OR BETA DENSITY MATRIX. 
C           W    = TWO-ELECTRON INTEGRAL MATRIX. 
C 
C  ON OUTPUT F   = PARTIAL FOCK MATRIX 
C*********************************************************************** 
      COMMON /EULER / TVEC(3,3), ID 
      COMMON /WMATRX/ WDUM(N2ELEC*3),NUMBW,NBAND(NUMATM) 
C     COMMON /SCRACH/ FJINT(10*NUMATM),PIJ(10*NUMATM),FKL(10*NUMATM) 
C    1               ,F12(2,MAXORB),P12(2,MAXORB) 
C
C     /SCRACH/ HAS BEEN CONVERTED TO /SCRCHR/ AND /SCRCHI/ FOR AMSOL    
      COMMON /SCRCHR/ FJINT(10*NUMATM),PIJ(10*NUMATM),FKL(10*NUMATM),   3GL3092 
     1                F12(2,MAXORB),P12(2,MAXORB),                      3GL3092 
     2                DUM3(6*MAXPAR**2+1-30*NUMATM-4*MAXORB)            GCL0393
C
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      DIMENSION F(*), PTOT(*), WJ(*), WK(*), NFIRST(*), NMIDLE(*), 
     1          NLAST(*), P(*), W(*) 
      DIMENSION IFACT(MAXORB),I1FACT(MAXORB) 
      LOGICAL LID,IEQJ,MINDO 
      SAVE
      IF (ICONTR(5).EQ.1) THEN                                          GDH0195
C 
C        INITIALISATION (DONE ONLY ONCE) 
C 
         MINDO=IMINDO.NE.0                                              DJG0995 
C        SET UP ARRAY OF (I*(I-1))/2 AND (I*(I+1))/2 
         DO 10 I=1,MAXORB 
         IFACT(I)=(I*(I-1))/2 
   10    I1FACT(I)=IFACT(I)+I 
         LID=ID.EQ.0 
         IF(.NOT.LID) THEN 
            LINEA1=NLAST(NUMAT)*(NLAST(NUMAT)+1)/2 + 1 
            P(LINEA1)=0.D0 
            IONE=0 
         ELSE 
            IONE=1 
         ENDIF 
         ICONTR(5)=2                                                    GDH0195
      ENDIF 
      IF (MINDO) GO TO 100 
C 
C     * MNDO * OR * AM1 * 
C       ----        --- 
C 
      IF (LID) THEN 
C 
C        MOLECULE 
C        -------- 
         IPQRS=1 
         KPQRS=1 
         JIJ=0 
         DO 15 I=1,NLAST(1) 
         DO 15 J=1,I 
         JIJ=JIJ+1 
         FJINT(JIJ)=0.D0 
   15    PIJ(JIJ)=PTOT(JIJ)*2.D0 
         DO 75 II=2,NUMAT 
            IF (NBAND(II).LE.0) GO TO 75 
            IA=NFIRST(II) 
            IC=NLAST(II) 
C           J-CONTRIBUTIONS IN FJINT 
            IJOLD=JIJ+1 
            DO 20 I=IA,IC 
            DO 20 J=IA,I 
            JIJ=JIJ+1 
   20       PIJ(JIJ)=PTOT(IFACT(I)+J)*2.D0 
            NROW=IJOLD-1 
            NCOL=NBAND(II) 
            CALL MXM (PIJ,1,W(IPQRS),NROW,FJINT(IJOLD),NCOL) 
            CALL MXM (W(IPQRS),NROW,PIJ(IJOLD),NCOL,FKL,1) 
            IPQRS=IPQRS+NROW*NCOL 
            DO 25 I=1,NROW 
   25       FJINT(I)=FJINT(I)+FKL(I) 
C           K-CONTRIBUTIONS IN F 
            DO 70 I=IA,IC 
               KA=IFACT(I) 
               IF (I.GT.IA) THEN 
                  DO 30 K=1,IA-1 
   30             P12(1,K)=P(KA+K)*2.D0 
                  DO 55 J=IA,I-1 
                     KB=IFACT(J) 
                     DO 35 K=1,IA-1 
   35                P12(2,K)=P(KB+K)*2.D0 
                     DO 50 JJ=1,II-1 
                        JA=NFIRST(JJ) 
                        JJLEN=NLAST(JJ)-JA+1 
                        IF (JJLEN.GT.1) THEN 
                           CALL MXM (P12(1,JA),2,WK(KPQRS),JJLEN 
     .                              ,F12(1,JA),JJLEN) 
                           KPQRS=KPQRS+JJLEN*JJLEN 
                        ELSE IF (JJLEN.EQ.1) THEN 
                           DO 40 K=1,2 
   40                      F12(K,JA)=WK(KPQRS)*P12(K,JA) 
                           KPQRS=KPQRS+1 
                        ENDIF 
   50                CONTINUE 
CDIR$ IVDEP 
                     DO 55 K=1,IA-1 
                     F(KA+K)=F(KA+K)+F12(2,K) 
   55             F(KB+K)   =F(KB+K)+F12(1,K) 
               ENDIF 
               DO 60 JJ=1,II-1 
                  JA=NFIRST(JJ) 
                  JJLEN=NLAST(JJ)-JA+1 
                  IF (JJLEN.GT.1) THEN 
                     CALL MXM (P(KA+JA),1,WK(KPQRS),JJLEN,F12(JA,1) 
     .                        ,JJLEN) 
                     KPQRS=KPQRS+JJLEN*JJLEN 
                  ELSE IF (JJLEN.EQ.1) THEN 
                     F12(JA,1)=WK(KPQRS)*P(KA+JA) 
                     KPQRS=KPQRS+1 
                  ENDIF 
   60          CONTINUE 
               CALL SAXPY (IA-1,4.D0,F12,1,F(KA+1),1) 
   70       CONTINUE 
   75    CONTINUE 
C        MERGE J-CONTRIBUTIONS IN F 
         JIJ=0 
         DO 85 II=1,NUMAT 
         IA=NFIRST(II) 
         IC=NLAST(II) 
         DO 85 I=IA,IC 
         DO 80 J=IA,I-1 
         JIJ=JIJ+1 
   80    F(IFACT(I)+J)=F(IFACT(I)+J)+FJINT(JIJ) 
         JIJ=JIJ+1 
   85    F(I1FACT(I))=F(I1FACT(I))+FJINT(JIJ)*2.D0 
      ELSE 
C 
C        POLYMER 
C        ------- 
         KK=0 
         DO 90 II=1,NUMAT 
            IA=NFIRST(II) 
            IB=NLAST(II) 
            DO 90 I=IA,IC 
                KA=IFACT(I) 
                DO 90 J=IA,I 
                   KB=IFACT(J) 
                   IJ=KA+J 
                   IEQJ=I.EQ.J 
                   PTOIJ2=PTOT(IJ)*2.D0 
                   DO 90 JJ=1,II 
                      JA=NFIRST(JJ) 
                      JC=NLAST(JJ) 
                      DO 90 K=JA,JC 
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
                         DO 90 L=JA,K 
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
   90    CONTINUE 
      ENDIF 
      RETURN 
C 
C     * MINDO * 
C       ----- 
C 
  100 KR=0 
      DO 130 II=1,NUMAT 
         IA=NFIRST(II) 
         IB=NLAST(II) 
         DO 120 JJ=1,II-IONE 
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
