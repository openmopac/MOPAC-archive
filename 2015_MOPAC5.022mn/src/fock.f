      SUBROUTINE FOCK (F, P, N) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
C      INCLUDE 'KEYS.i'                                                 DJG0995
C*********************************************************************** 
C 
C FOCK  FORMS THE 2-ELECTRON PART OF THE FOCK MATRIX FOR MOLECULES 
C       IN RHF CASE WITHIN * MNDO (AM1) * OR * MINDO * FORMALISMS. 
C  INPUT    P(N,N) = TOTAL DENSITY MATRIX. 
C           N      = NUMBER OF A.O. 
C 
C  OUTPUT   F(N,N) = 2-ELECTRON CONTRIBUTION TO THE FOCK MATRIX. 
C*********************************************************************** 
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN                                      3GL3092
     3       /TWOELE/ GSS(120),GSP(120),GPP(120),GP2(120),HSP(120) 
     4               ,GSD(120),GPD(120),GDD(120) 
     5       /DWMAT/  DWJ(N2ELEC),DWK(N2ELEC*2)                         JZ0315
     6       /WMATRX/ WJ(N2ELEC),WK(N2ELEC*2),NUMBW,NBAND(NUMATM) 
C    5       /WMATRX/ WJ(N2ELEC),WK(N2ELEC),NUMBW,NBAND(NUMATM) 
C    6       /SCRACH/ PDUM(MORB2),FJINT(10*NUMATM),PIJ(10*NUMATM) 
C    7               ,FKL(10*NUMATM),F12(2,MAXORB),P12(2,MAXORB) 
C
C     /SCRACH/ HAS BEEN CONVERTED TO /SCRCHR/ AND /SCRCHI/ FOR AMSOL    
      COMMON /SCRCHR/ PDUM(MORB2),FJINT(10*NUMATM),PIJ(10*NUMATM),      3GL3092
     1                FKL(10*NUMATM),F12(2,MAXORB),P12(2,MAXORB),       3GL3092 
     2                DUM3(6*MAXPAR**2+1-MORB2-30*NUMATM-4*MAXORB)      GCL0393
C
      DIMENSION F(N,*), P(N,*) 
       SAVE
      DO 10 I=1,N*N 
   10 F(I,1)=0.D0 
C     DO 10 I = 1, N
C     DO 10 J = 1, N
C  10 F(I,J) = 0D0 
      IF(IMINDO.NE.0) GO TO 90                                          DJG0995
C 
C     * MNDO * OR * AM1 * 
C       ----        --- 
C 
      KPQRS=1 
      IPQRS=1 
      IJ=0 
      DO 20 I=1,NLAST(1) 
      DO 20 J=1,I 
      IJ=IJ+1 
      FJINT(IJ)=0.D0 
   20 PIJ(IJ)=P(J,I)*2.D0 
      DO 85 II=2,NUMAT 
         IF(NBAND(II).LE.0) GO TO 85 
         IA=NFIRST(II) 
         IC=NLAST(II) 
C 
C     J-CONTRIBUTIONS IN FJINT 
C 
         IJOLD=IJ+1 
         DO 30 I=IA,IC 
         DO 30 J=IA,I 
         IJ=IJ+1 
   30    PIJ(IJ)=P(J,I)*2.D0 
         NROW=IJOLD-1 
         NCOL=NBAND(II) 
C        CALL MXM (PIJ,1,WJ(IPQRS),NROW,FJINT(IJOLD),NCOL) 
C        CALL MXM (WJ(IPQRS),NROW,PIJ(IJOLD),NCOL,FKL,1) 
c        write(6,*) 'DWJ ', DWJ(IPQRS), IPQRS
         CALL MXM (PIJ,1,DWJ(IPQRS),NROW,FJINT(IJOLD),NCOL) 
         CALL MXM (DWJ(IPQRS),NROW,PIJ(IJOLD),NCOL,FKL,1) 
         IPQRS=IPQRS+NROW*NCOL 
         DO 40 I=1,NROW 
   40    FJINT(I)=FJINT(I)+FKL(I) 
C 
C     K-CONTRIBUTIONS IN F 
C 
         DO 84 I=IA,IC 
         IF (I.GT.IA) THEN 
            DO 50 K=1,IA 
   50       P12(1,K)=P(I,K) 
            DO 70 J=IA,I-1 
            DO 55 K=1,IA 
   55       P12(2,K)=P(J,K) 
            DO 65 JJ=1,II-1 
               JA=NFIRST(JJ) 
               JJLEN=NLAST(JJ)-JA+1 
               IF (JJLEN.GT.1) THEN 
c                 write(6,*) 'WK ', DWK(KPQRS), KPQRS
C                 CALL MXM (P12(1,JA),2,WK(KPQRS),JJLEN,F12(1,JA),JJLEN) 
                  CALL MXM(P12(1,JA),2,DWK(KPQRS),JJLEN,F12(1,JA),JJLEN) 
                  KPQRS=KPQRS+JJLEN*JJLEN 
               ELSE IF (JJLEN.EQ.1) THEN 
                  DO 60 K=1,2 
C  60             F12(K,JA)=WK(KPQRS)*P12(K,JA) 
   60             F12(K,JA)=DWK(KPQRS)*P12(K,JA) 
                  KPQRS=KPQRS+1 
               ENDIF 
   65       CONTINUE 
CDIR$ IVDEP 
            DO 70 K=1,IA-1 
            F(I,K)=F(I,K)+F12(2,K) 
   70       F(J,K)=F(J,K)+F12(1,K) 
         ENDIF 
         DO 80 JJ=1,II-1 
            JA=NFIRST(JJ) 
            JJLEN=NLAST(JJ)-JA+1 
            IF (JJLEN.GT.1) THEN 
C              CALL MXM (P(JA,I),1,WK(KPQRS),JJLEN,F12(JA,1),JJLEN) 
               CALL MXM (P(JA,I),1,DWK(KPQRS),JJLEN,F12(JA,1),JJLEN) 
               KPQRS=KPQRS+JJLEN*JJLEN 
            ELSE IF (JJLEN.EQ.1) THEN 
C              F12(JA,1)=WK(KPQRS)*P(JA,I) 
               F12(JA,1)=DWK(KPQRS)*P(JA,I) 
               KPQRS=KPQRS+1 
            ENDIF 
   80    CONTINUE 
         CALL SAXPY (IA-1,2.D0,F12,1,F(I,1),NORBS) 
   84    CONTINUE 
   85 CONTINUE 
C 
C     INSERT J-CONTRIBUTIONS IN F 
C 
      IJ=0 
      DO 86 II=1,NUMAT 
      IA=NFIRST(II) 
      IC=NLAST(II) 
      DO 86 I=IA,IC 
      DO 86 J=IA,I 
      IJ=IJ+1 
   86 F(I,J)=FJINT(IJ) 
C 
      GO TO 200 
C 
C     * MINDO * 
C       ----- 
C 
   90 KR=0 
      DO 120 II=2,NUMAT 
         IA=NFIRST(II) 
         IB=NLAST(II) 
         DO 110 JJ=1,II-1 
            KR=KR+1 
C           ELREP=2.D0*WJ(KR) 
            ELREP=2.D0*DWJ(KR) 
            JA=NFIRST(JJ) 
            JB=NLAST(JJ) 
            DO 100 I=IA,IB 
               DO 100 K=JA,JB 
                  F(I,I)=F(I,I)+P(K,K)*ELREP 
                  F(K,K)=F(K,K)+P(I,I)*ELREP 
  100       F(I,K)=F(I,K)-P(I,K)*ELREP 
  110    CONTINUE 
  120 CONTINUE 
C ********************************************************************* 
C 
C *** COMPUTE THE REMAINING CONTRIBUTIONS TO THE ONE-CENTRE ELEMENTS. 
C --- DUE TO ROUND-OFF CUMULATIVE EFFECTS, THIS SECTION IS BETTER 
C     TO BE DONE AFTER THE 2-CENTRE ONE. 
C           ..... NO "D" ORBITALS IN THIS VERSION ... 
C 
C ********************************************************************* 
  200 DO 250 II=1,NUMAT 
         IA=NFIRST(II) 
         IB=NMIDLE(II) 
         NI=NAT(II) 
C 
C     F(S,S) ... 1/2 
C 
         F(IA,IA)=F(IA,IA)+P(IA,IA)*GSS(NI)*0.25D0 
         IF (NI.LT.3) GO TO 250 
         PTPOP=0.D0 
         DO 210 I=IA+1,IB 
  210    PTPOP=PTPOP+P(I,I) 
         GSPHSP=GSP(NI)*0.5D0-HSP(NI)*0.25D0 
C 
C     F(S,S) ... 2/2 
C 
         F(IA,IA)=F(IA,IA)+PTPOP*GSPHSP 
         DO 220 J=IA+1,IB 
C 
C     F(P,P) 
C 
            F(J,J)=F(J,J)+P(IA,IA)*GSPHSP 
     1               +P(J,J)*GPP(NI)*0.25D0 
     2               +(PTPOP-P(J,J))*(0.625D0*GP2(NI)-0.125D0*GPP(NI)) 
C 
C     F(S,P) 
C 
  220    F(J,IA)=F(J,IA)+P(J,IA)*(1.5D0*HSP(NI)-0.5D0*GSP(NI)) 
C 
C     F(P,P*) 
C 
         DO 240 J=IA+1,IB-1 
            DO 230 L=J+1,IB 
  230       F(L,J)=F(L,J)+P(L,J)*(0.75D0*GPP(NI)-1.25D0*GP2(NI)) 
  240    CONTINUE 
  250 CONTINUE 
C 
C     SYMMETRIZE F 
C 
      DO 300 I=2,N 
CDIR$ IVDEP 
      DO 300 J=1,I-1 
  300 F(J,I)=F(I,J) 
      DO 310 I=1,NORBS*NORBS,NORBS+1 
  310 F(I,1)=F(I,1)*2.D0 
      RETURN 
      END 
