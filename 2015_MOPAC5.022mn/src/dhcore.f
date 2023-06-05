      SUBROUTINE DHCORE (COORD,H,W,ENUCLR,NATI,NATX,STEP) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
C      INCLUDE 'KEYS.i'                                                 DJG0995
      DIMENSION COORD(3,*),H(*),W(*) 
C 
C  DHCORE GENERATES THE 1-ELECTRON  AND 2-ELECTRON INTEGRALS DERIVATIVES 
C         WITH RESPECT TO THE CARTESIAN COORDINATE COORD (NATX,NATI). 
C 
C  INPUT 
C      COORD     : CARTESIAN  COORDINATES OF THE MOLECULE. 
C      NATI,NATX : INDICES OF THE MOVING COORDINATE. 
C      STEP      : STEP SIZE OF THE 2-POINTS FINITE DIFFERENCE. 
C  OUTPUT 
C      H         : 1-ELECTRON INTEGRALS DERIVATIVES (PACKED CANONICAL). 
C      W         : 2-ELECTRON INTEGRALS DERIVATIVES (ORDERED AS REQUIRED 
C                             IN DFOCK2 AND DIJKL1). 
C      ENUCLR    : NUCLEAR ENERGY DERIVATIVE. 
C 
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN                                      3GL3092
     3       /MOLORB/ USPD(MAXORB),DUMY(MAXORB) 
     5       /WMATRX/ WDUMMY(N2ELEC*3),KR,NBAND(NUMATM) 
C    5       /WMATRX/ WDUMMY(N2ELEC*2),KR,NBAND(NUMATM) 
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      LOGICAL MINDO 
      DIMENSION E1B(10),DE1B(10),E2A(10),DE2A(10) 
     .         ,DI(9,9),DDI(9,9),WJD(100),DWJD(100) 
       SAVE
      IF (ICONTR(3).EQ.1) THEN                                          GDH0195
         ICONTR(3)=2                                                    GDH0195
         IONE=1 
         CUTOFF=1.D10 
         MINDO=IMINDO.NE.0                                              DJG0995 
      ENDIF                                                             GDH0195
      DO 10 I=1,(NORBS*(NORBS+1))/2 
   10 H(I)=0.0D0 
      ENUCLR=0.D0 
      KR=1 
      NROW=0 
      I=NATI 
      CSAVE=COORD(NATX,I) 
      IA=NFIRST(I) 
      IB=NLAST(I) 
      IC=NMIDLE(I) 
      NI=NAT(I) 
      NROW=-NBAND(I) 
      DO 20 J=1,NUMAT 
   20 NROW=NROW+NBAND(J) 
      NCOL=NBAND(I) 
      NBAND2=0 
      DO 110 J=1,NUMAT 
      IF (J.EQ.I) GO TO 110 
      JA=NFIRST(J) 
      JB=NLAST(J) 
      JC=NMIDLE(J) 
      NJ=NAT(J) 
      COORD(NATX,I)=CSAVE+STEP 
      CALL H1ELEC(NI,NJ,COORD(1,I),COORD(1,J),DI) 
      COORD(NATX,I)=CSAVE-STEP 
      CALL H1ELEC(NI,NJ,COORD(1,I),COORD(1,J),DDI) 
C 
C     FILL THE ATOM-OTHER ATOM ONE-ELECTRON MATRIX. 
C 
      I2=0 
      IF (IA.GT.JA) THEN 
         DO 30 I1=IA,IB 
         IJ=I1*(I1-1)/2+JA-1 
         I2=I2+1 
         J2=0 
         DO 30 J1=JA,JB 
         IJ=IJ+1 
         J2=J2+1 
   30    H(IJ)=H(IJ)+(DI(I2,J2)-DDI(I2,J2)) 
      ELSE 
         DO 40 I1=JA,JB 
         IJ=I1*(I1-1)/2+IA-1 
         I2=I2+1 
         J2=0 
         DO 40 J1=IA,IB 
         IJ=IJ+1 
         J2=J2+1 
   40    H(IJ)=H(IJ)+(DI(J2,I2)-DDI(J2,I2)) 
      ENDIF 
C 
C     CALCULATE THE TWO-ELECTRON INTEGRALS, W; THE ELECTRON NUCLEAR TERM 
C     E1B AND E2A; AND THE NUCLEAR-NUCLEAR TERM ENUC. 
C 
C     Variable GTERM is added to ROTATE                                  JZ0315
      KRO=KR 
      NBAND1=NBAND2+1 
      NBAND2=NBAND2+NBAND(J) 
      IF (MINDO) THEN 
         COORD(NATX,I)=CSAVE+STEP 
         CALL ROTATE1(NI,NJ,COORD(1,I),COORD(1,J) 
     .               ,WJD,KR,E1B,E2A,ENUC,CUTOFF,GTERM) 
         KR=KRO 
         COORD(NATX,I)=CSAVE-STEP 
         CALL ROTATE1(NI,NJ,COORD(1,I),COORD(1,J) 
     .               ,DWJD,KR,DE1B,DE2A,DENUC,CUTOFF,GTERM) 
         IF (KR.GT.KRO) THEN 
            DO 50 K=1,KR-KRO+1
   50       W(KRO+K-1)=WJD(K)-DWJD(K) 
         ENDIF 
      ELSE 
         COORD(NATX,I)=CSAVE+STEP 
         CALL ROTATE1(NI,NJ,COORD(1,I),COORD(1,J) 
     .               ,WJD,KR,E1B,E2A,ENUC,CUTOFF,GTERM) 
         KR=KRO 
         COORD(NATX,I)=CSAVE-STEP 
         CALL ROTATE1(NI,NJ,COORD(1,I),COORD(1,J) 
     .               ,DWJD,KR,DE1B,DE2A,DENUC,CUTOFF,GTERM) 
         IF (KR.GT.KRO) THEN 
            DO 60 K=1,KR-KRO 
C           write(6,*) 'WJD ',WJD(K),DWJD(K),KR,KRO
C           write(6,*) 'WJD ',WJD(K),K
   60       WJD(K)=WJD(K)-DWJD(K) 
            CALL WCANON (WJD,W,NROW,NCOL,NBAND1,NBAND2) 
         ENDIF 
      ENDIF 
      COORD(NATX,I)=CSAVE 
      ENUCLR = ENUCLR + ENUC-DENUC 
C 
C   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM I. 
C 
      I2=0 
      DO 70 I1=IA,IC 
      II=I1*(I1-1)/2+IA-1 
      DO 70 J1=IA,I1 
      II=II+1 
      I2=I2+1 
   70 H(II)=H(II)+E1B(I2)-DE1B(I2) 
C     CONTRIB D, CNDO. 
      DO 80 I1=IC+1,IB 
      II=(I1*(I1+1))/2 
   80 H(II)=H(II)+E1B(1)-DE1B(1) 
C 
C   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM J. 
C 
      I2=0 
      DO 90 I1=JA,JC 
      II=I1*(I1-1)/2+JA-1 
      DO 90 J1=JA,I1 
      II=II+1 
      I2=I2+1 
   90 H(II)=H(II)+E2A(I2)-DE2A(I2) 
C     CONTRIB D, CNDO. 
      DO 100 I1=JC+1,JB 
      II=(I1*(I1+1))/2 
  100 H(II)=H(II)+E2A(1)-DE2A(1) 
  110 CONTINUE 
      RETURN 
      END 

      SUBROUTINE WCANON (W,PQRS,NROW,NCOL,NBAND1,NBAND2)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C     * MNDO *  OR  * AM1 *  BUT NOT  * MINDO * 
C     FROM ONE BICENTRIC BLOCK OF 2-ELECTRON INTEGRALS 
C       TO CANONIC STORAGE. 
      DIMENSION W(*),PQRS(NROW,NCOL)
       SAVE
      K=1
      DO 10 J=1,NCOL
      DO 10 I=NBAND1,NBAND2
      PQRS(I,J)=W(K)
   10 K=K+1
      RETURN
      END

