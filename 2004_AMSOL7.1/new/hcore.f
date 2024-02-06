      SUBROUTINE HCORE (COORD,H,W, WJ,WK,ENUCLR) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH0196
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
     3       /MOLORB/ USPD(MAXORB),DUMY(MAXORB) 
     5       /WMATRX/ WDUMMY(N2ELEC*3),KR,NBAND(NUMATM) 
      COMMON /EULER / TVEC(3,3), ID 
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
************************************************************************ 
C 
C   HCORE GENERATES THE ONE-ELECTRON MATRIX AND TWO ELECTRON INTEGRALS 
C         FOR A GIVEN MOLECULE WHOSE GEOMETRY IS DEFINED IN CARTESIAN 
C         COORDINATES. 
C 
C  ON INPUT  COORD   = COORDINATES OF THE MOLECULE. 
C 
C  ON OUTPUT  H      = ONE-ELECTRON MATRIX. 
C             W      = TWO-ELECTRON INTEGRALS. 
C             ENUCLR = NUCLEAR ENERGY 
************************************************************************ 
      DIMENSION COORD(3,*),H(*), WJ(*), WK(*), W(*) 
      DIMENSION E1B(10),E2A(10),DI(9,9), WJD(100), WKD(100) 
      LOGICAL DEBUG,MINDO 
       SAVE
      IF (ICONTR(6).EQ.1) THEN                                          GDH0195
         ICONTR(6)=2                                                    GDH0195
         IONE=1 
         CUTOFF=1.D10 
         IF(ID.NE.0)CUTOFF=60.D0 
         IF(ID.NE.0)IONE=0 
         DEBUG=IHCORE.NE.0                                              DJG0995 
         MINDO=IMINDO.NE.0                                              DJG0995 
      ENDIF 
      DO 10 I=1,(NORBS*(NORBS+1))/2 
   10 H(I)=0 
      ENUCLR=0.D0 
      KR=1 
      NROW=0 
      IPQRS=1 
      DO 110 I=1,NUMAT 
         IA=NFIRST(I) 
         IB=NLAST(I) 
         IC=NMIDLE(I) 
         NI=NAT(I) 
C 
C FIRST WE FILL THE DIAGONALS, AND OFF-DIAGONALS ON THE SAME ATOM 
C 
         DO 30 I1=IA,IB 
            I2=I1*(I1-1)/2+IA-1 
            DO 20 J1=IA,I1 
               I2=I2+1 
   20       H(I2)=0.D0 
   30    H(I2)=USPD(I1) 
         IM1=I-IONE 
         IF(IM1.GT.0) THEN 
            NROW=NROW+NBAND(IM1) 
            NCOL=NBAND(I) 
            NBAND2=0 
         ENDIF 
         DO 100 J=1,IM1 
            HALF=1.D0 
            IF(I.EQ.J)HALF=0.5D0 
            JA=NFIRST(J) 
            JB=NLAST(J) 
            JC=NMIDLE(J) 
            NJ=NAT(J) 
            CALL H1ELEC(NI,NJ,COORD(1,I),COORD(1,J),DI) 
C 
C   FILL THE ATOM-OTHER ATOM ONE-ELECTRON MATRIX<PSI(LAMBDA)!PSI(SIGMA)> 
C 
            I2=0 
            DO 40 I1=IA,IB 
               II=I1*(I1-1)/2+JA-1 
               I2=I2+1 
               J2=0 
               JJ=MIN(I1,JB) 
               DO 40 J1=JA,JJ 
                  II=II+1 
                  J2=J2+1 
   40       H(II)=H(II)+DI(I2,J2) 
C 
C   CALCULATE THE TWO-ELECTRON INTEGRALS, W; THE ELECTRON NUCLEAR TERMS 
C   E1B AND E2A; AND THE NUCLEAR-NUCLEAR TERM ENUC. 
C 
            KRO=KR 
            NBAND1=NBAND2+1 
            NBAND2=NBAND2+NBAND(J) 
            IF(ID.EQ.0) THEN 
               CALL ROTATE (NI,NJ,COORD(1,I),COORD(1,J), 
     1                      WJD,     KR,E1B,E2A,ENUC,CUTOFF) 
               IF(KR.LE.KRO) GO TO 50 
               IF(MINDO) THEN 
                  CALL SCOPY (KR-KRO,WJD,1,W(KRO),1) 
               ELSE 
                  CALL WCANON (WJD,W(IPQRS),NROW,NCOL,NBAND1,NBAND2) 
               ENDIF 
            ELSE 
               CALL SOLROT (NI,NJ,COORD(1,I),COORD(1,J), 
     1                      WJD, WKD,KR,E1B,E2A,ENUC,CUTOFF) 
               IF(KR.LE.KRO) GO TO 50 
               IF(MINDO) THEN 
                  CALL SCOPY (KR-KRO,WJD,1,WJ(KRO),1) 
                  CALL SCOPY (KR-KRO,WKD,1,WK(KRO),1) 
               ELSE 
                  CALL WCANON (WJD,WJ(IPQRS),NROW,NCOL,NBAND1,NBAND2) 
                  CALL WCANON (WKD,WK(IPQRS),NROW,NCOL,NBAND1,NBAND2) 
               ENDIF 
            ENDIF 
   50       ENUCLR = ENUCLR + ENUC 
C 
C   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM I. 
C 
            I2=0 
            DO 60 I1=IA,IC 
               II=I1*(I1-1)/2+IA-1 
               DO 60 J1=IA,I1 
                  II=II+1 
                  I2=I2+1 
   60       H(II)=H(II)+E1B(I2)*HALF 
            DO  70 I1=IC+1,IB 
               II=(I1*(I1+1))/2 
   70       H(II)=H(II)+E1B(1)*HALF 
C 
C   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM J. 
C 
            I2=0 
            DO 80 I1=JA,JC 
               II=I1*(I1-1)/2+JA-1 
               DO 80 J1=JA,I1 
                  II=II+1 
                  I2=I2+1 
   80       H(II)=H(II)+E2A(I2)*HALF 
            DO 90 I1=JC+1,JB 
               II=(I1*(I1+1))/2 
   90       H(II)=H(II)+E2A(1)*HALF 
  100    CONTINUE 
         IPQRS=IPQRS+NROW*NCOL 
  110 CONTINUE 
      IF (DEBUG) THEN 
         WRITE(JOUT,'(//10X,''ONE-ELECTRON MATRIX FROM HCORE'')') 
         CALL VECPRT(H,NORBS) 
         J=MIN(400,KR) 
         IF(ID.EQ.0) THEN 
            WRITE(JOUT,'(//10X,''TWO-ELECTRON MATRIX IN HCORE''/)') 
            WRITE(JOUT,120)(W(I),I=1,J) 
         ELSE 
            WRITE(JOUT,'(//10X,''TWO-ELECTRON J MATRIX IN HCORE''/)') 
            WRITE(JOUT,120)(WJ(I),I=1,J) 
            WRITE(JOUT,'(//10X,''TWO-ELECTRON K MATRIX IN HCORE''/)') 
            WRITE(JOUT,120)(WK(I),I=1,J) 
  120       FORMAT(10F8.4) 
         ENDIF 
      ENDIF 
      IF (ID.EQ.0) THEN 
C 
C        UNPACK (-0.5)*K  FOR FURTHER USE IN FOCK2, FOCK ETC 
C        --------------------------------------------------- 
         IPQRS=1 
         KPQRS=0 
         CONST=-0.5D0 
         DO 150 II=2,NUMAT 
         IA=NFIRST(II) 
         IC=NLAST(II) 
         DO 150 I=IA,IC 
         DO 150 J=IA,I 
         DO 150 JJ=1,II-1 
         JJLEN=MAX(0,NLAST(JJ)-NFIRST(JJ)+1) 
         DO 140 K=1,JJLEN 
         LK=(K-1)*JJLEN+1 
         KL=K 
         DO 130 L=1,K-1 
         WK(KPQRS+KL)=WJ(IPQRS)*CONST 
         WK(KPQRS+LK)=WJ(IPQRS)*CONST 
         IPQRS=IPQRS+1 
         LK=LK+1 
  130    KL=KL+JJLEN 
         WK(KPQRS+LK)=-WJ(IPQRS) 
  140    IPQRS=IPQRS+1 
  150    KPQRS=KPQRS+JJLEN*JJLEN 
      ENDIF 
      RETURN 
      END 
