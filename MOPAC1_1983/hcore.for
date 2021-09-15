      SUBROUTINE HCORE (COORD,H,W,ENUCLR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES/NOLIST'
      DIMENSION COORD(3,*),H(*),W(*)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     +                NCLOSE,NOPEN
     1       /MOLORB/ USPD(MAXORB),DUMY(MAXORB)
     2       /KEYWRD/ KEYWRD
C****************************************************************************
C
C   HCORE GENERATES THE ONE-ELECTRON MATRIX AND TWO ELECTRON INTEGRALS FOR A
C         GIVEN MOLECULE WHOSE GEOMETRY IS DEFINED IN CARTESIAN COORDINATES.
C
C  ON INPUT  COORD   = COORDINATES OF THE MOLECULE.
C
C  ON OUTPUT  H      = ONE-ELECTRON MATRIX.
C             W      = TWO-ELECTRON INTEGRALS.
C             ENUCLR = NUCLEAR ENERGY
C****************************************************************************
      CHARACTER*80 KEYWRD
      LOGICAL FIRST,DEBUG
      DIMENSION E1B(NUMATM),E2A(10),DI(9,9)
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
          FIRST=.FALSE.
          DEBUG=(INDEX(KEYWRD,'HCORE') .NE. 0)
      ENDIF
      ENUCLR=0.D0
      KR=1
      DO 1 I=1,NUMAT
        E1B(I)=0.D0
        IA=NFIRST(I)
        IB=NLAST(I)
        NI=NAT(I)
C
C FIRST WE FILL THE DIAGONALS, AND OFF-DIAGONALS ON THE SAME ATOM
C
        DO 2 I1=IA,IB
          I2=I1*(I1-1)/2+IA-1
          DO 3 J1=IA,I1
            I2=I2+1
   3        H(I2)=0.D0
   2      H(I2)=USPD(I1)
        IM1=I-1      
        DO 5 J=1,IM1
          JA=NFIRST(J)
          JB=NLAST(J)
          NJ=NAT(J)
          CALL H1ELEC(NI,NJ,COORD(1,I),COORD(1,J),DI)
C
C   FILL THE ATOM-OTHER ATOM ONE-ELECTRON MATRIX <PSI(LAMBDA)|PSI(SIGMA)>
C
          I2=0
          DO 6 I1=IA,IB
            II=I1*(I1-1)/2+JA-1
            I2=I2+1
            J2=0
            DO 6 J1=JA,JB
            II=II+1
            J2=J2+1
   6        H(II)=DI(I2,J2)
C
C   CALCULATE THE TWO-ELECTRON INTEGRALS, W; THE ELECTRON NUCLEAR TERMS
C   E1B AND E2A; AND THE NUCLEAR-NUCLEAR TERM ENUC.
C
          CALL ROTATE(NI,NJ,COORD(1,I),COORD(1,J),
     +                W(KR),KR,E1B,E2A,ENUC)
          ENUCLR = ENUCLR + ENUC
C
C   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM I.
C
          I2=0
          DO 7 I1=IA,IB
            II=I1*(I1-1)/2+IA-1
            DO 7 J1=IA,I1
              II=II+1
              I2=I2+1
   7          H(II)=H(II)+E1B(I2)
C
C   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM J.
C
          I2=0
          DO 8 I1=JA,JB
            II=I1*(I1-1)/2+JA-1
            DO 8 J1=JA,I1
              II=II+1
              I2=I2+1
   8      H(II)=H(II)+E2A(I2)
   5    CONTINUE
   1  CONTINUE
      IF( .NOT. DEBUG) RETURN
      WRITE(6,'(//10X,''ONE-ELECTRON MATRIX FROM HCORE'')')
      CALL VECPRT(H,NORBS)
      WRITE(6,'(//10X,''WHOLE OF TWO-ELECTRON MATRIX IN HCORE''/)')
      WRITE(6,111)(W(I),I=1,KR)
  111 FORMAT(10F8.4)
      RETURN
      END

