      SUBROUTINE HCORE (COORD,H,W, WJ,WK,ENUCLR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION COORD(3,*),H(*), WJ(N2ELEC), WK(N2ELEC), W(N2ELEC)
      REAL WJ, WK
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
     3       /MOLORB/ USPD(MAXORB),DUMY(MAXORB)
     4       /KEYWRD/ KEYWRD
      COMMON /EULER / TVEC(3,3), ID
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
      CHARACTER*80 KEYWRD
      LOGICAL FIRST,DEBUG
      DIMENSION E1B(NUMATM),E2A(10),DI(9,9), WJD(100), WKD(100)
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
         IONE=1
         CUTOFF=1.D10
         IF(ID.NE.0)CUTOFF=60.D0
         IF(ID.NE.0)IONE=0
         FIRST=.FALSE.
         DEBUG=(INDEX(KEYWRD,'HCORE') .NE. 0)
      ENDIF
      DO 10 I=1,(NORBS*(NORBS+1))/2
   10 H(I)=0
      ENUCLR=0.D0
      KR=1
      DO 110 I=1,NUMAT
         E1B(I)=0.D0
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
         DO 100 J=1,IM1
            HALF=1.D0
            IF(I.EQ.J)HALF=0.5D0
            JA=NFIRST(J)
            JB=NLAST(J)
            JC=NMIDLE(J)
            NJ=NAT(J)
            CALL H1ELEC(NI,NJ,COORD(1,I),COORD(1,J),DI)
C
C   FILL THE ATOM-OTHER ATOM ONE-ELECTRON MATRIX<PSI(LAMBDA)|PSI(SIGMA)>
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
            IF(ID.EQ.0) THEN
               CALL ROTATE(NI,NJ,COORD(1,I),COORD(1,J),
     1 W(KR), KR,E1B,E2A,ENUC,CUTOFF)
            ELSE
               KRO=KR
               CALL SOLROT(NI,NJ,COORD(1,I),COORD(1,J),
     1                WJD, WKD,KR,E1B,E2A,ENUC,CUTOFF)
               JJ=0
               DO 50 II=KRO,KR-1
                  JJ=JJ+1
                  WJ(II)=WJD(JJ)
   50          WK(II)=WKD(JJ)
            ENDIF
            ENUCLR = ENUCLR + ENUC
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
  110 CONTINUE
      IF( .NOT. DEBUG) RETURN
      WRITE(6,'(//10X,''ONE-ELECTRON MATRIX FROM HCORE'')')
      CALL VECPRT(H,NORBS)
      J=MIN(400,KR)
      IF(ID.EQ.0) THEN
         WRITE(6,'(//10X,''TWO-ELECTRON MATRIX IN HCORE''/)')
         WRITE(6,120)(W(I),I=1,J)
      ELSE
         WRITE(6,'(//10X,''TWO-ELECTRON J MATRIX IN HCORE''/)')
         WRITE(6,120)(WJ(I),I=1,J)
         WRITE(6,'(//10X,''TWO-ELECTRON K MATRIX IN HCORE''/)')
         WRITE(6,120)(WK(I),I=1,J)
      ENDIF
  120 FORMAT(10F8.4)
      RETURN
      END
