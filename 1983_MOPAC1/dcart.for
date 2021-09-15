      SUBROUTINE DCART (COORD,DXYZ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION COORD(3,*), DXYZ(3,*)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM), NFIRST(NUMATM), NMIDLE(NUMATM), 
     + NLAST(NUMATM), NORBS, NELECS, NALPHA, NBETA, NCLOSE, NOPEN
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
C***********************************************************************
C
C    DCART CALCULATES THE DERIVATIVES OF THE ENERGY WITH RESPECT TO THE
C          CARTESIAN COORDINATES. THIS IS DONE BY FINITE DIFFERENCES.
C
C    THE MAIN ARRAYS IN DCART ARE:
C        DXYZ   ON EXIT CONTAINS THE CARTESIAN DERIVATIVES.
C
C***********************************************************************
      COMMON /KEYWRD/ KEYWRD
      CHARACTER*80 KEYWRD
      DIMENSION PDI(36),PADI(36),PBDI(36),
     +CDI(3,2),NDI(2)
      LOGICAL DEBUG, FIRST, FORCE
      DATA CHNGE,CHNGE2 /1.D-6,5.D-7/
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
          DEBUG = (INDEX(KEYWRD,'DCART') .NE. 0)
          FORCE = (INDEX(KEYWRD,'PRECISE')+INDEX(KEYWRD,'FORCE') .NE. 0)
          FIRST = .FALSE.
      ENDIF
      DO 20 I=1,NUMAT
      DO 20 J=1,3
  20  DXYZ(J,I)=0.D0
      DO 1 II=2,NUMAT
          IM1=II-1
          IF=NFIRST(II)
          IL=NLAST(II)
          NDI(2)=NAT(II)
          DO 6 I=1,3
   6          CDI(I,2)=COORD(I,II)
      DO 1 JJ=1,IM1
C  FORM DIATOMIC MATRICES
          JF=NFIRST(JJ)
          JL=NLAST(JJ)
C   GET FIRST ATOM
          NDI(1)=NAT(JJ)
          DO 7 I=1,3
   7          CDI(I,1)=COORD(I,JJ)
          IJ=0
          DO 2 I=JF,JL
              K=I*(I-1)/2+JF-1
              DO 2 J=JF,I
                  IJ=IJ+1
                  K=K+1
          PADI(IJ)=PA(K)
          PBDI(IJ)=PB(K)
   2      PDI(IJ)=P(K)
C GET SECOND ATOM FIRST ATOM INTERSECTION
          DO 3 I=IF,IL
              L=I*(I-1)/2
              K=L+JF-1
              DO 4 J=JF,JL
                  IJ=IJ+1
                  K=K+1
                  PADI(IJ)=PA(K)
                  PBDI(IJ)=PB(K)
   4              PDI(IJ)=P(K)
              K=L+IF-1
              DO 5 L=IF,I
                  K=K+1
                  IJ=IJ+1
                  PADI(IJ)=PA(K)
                  PBDI(IJ)=PB(K)
   5              PDI(IJ)=P(K)
   3          CONTINUE
          IIJJ=IIJJ+1
          IF( .NOT. FORCE) THEN
          CDI(1,1)=CDI(1,1)+CHNGE2
          CDI(2,1)=CDI(2,1)+CHNGE2
          CDI(3,1)=CDI(3,1)+CHNGE2
          CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JL,IF,IL,
     +                 NCLOSE,NOPEN,NORBS,AA)
          ENDIF
          DO 8 K=1,3
          IF( FORCE )THEN
             CDI(K,2)=CDI(K,2)-CHNGE2
          CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JL,IF,IL,
     +                 NCLOSE,NOPEN,NORBS,AA)
              ENDIF
              CDI(K,2)=CDI(K,2)+CHNGE
              CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JL,IF,IL,
     +                 NCLOSE,NOPEN,NORBS,EE)
              CDI(K,2)=CDI(K,2)-CHNGE2
              DERIV=(AA-EE)*46.122D0/CHNGE
              DXYZ(K,II)=DXYZ(K,II)+DERIV
              DXYZ(K,JJ)=DXYZ(K,JJ)-DERIV
   8          CONTINUE
   1    CONTINUE
      IF (  .NOT. DEBUG) RETURN
      WRITE(6,'(//10X,''CARTESIAN COORDINATE DERIVATIVES'',//3X,
     +''ATOM  AT. NO.''5X,''X'',12X,''Y'',12X,''Z'',/)')
      WRITE(6,'(2I6,F13.6,2F13.6)')
     + (I,NAT(I),(DXYZ(J,I),J=1,3),I=1,NUMAT)
      RETURN
      END
      SUBROUTINE DHC (P,PA,PB,XI,NAT,IF,IL,JF,JL,NCLOSE,
     +NOPEN,NORBS,DENER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P(*), PA(*), PB(*)
      DIMENSION XI(3,*),NFIRST(2),NLAST(2),NAT(*)
C***********************************************************************
C
C  DHC CALCULATES THE ENERGY CONTRIBUTIONS FROM THOSE PAIRS OF ATOMS
C         THAT HAVE BEEN MOVED BY SUBROUTINE DERIV.
C
C***********************************************************************
      COMMON /KEYWRD/ KEYWRD
      COMMON /NUMCAL/ NUMCAL
      CHARACTER*80 KEYWRD
      LOGICAL UHF, HALFE, TRIPLT, DUBLET
      DIMENSION DCL(3), DIF(3), H(171), SHMAT(9,9), F(171),
     +          W(100), E1B(10), E2A(10)
      DATA ICALCN /0/
      IF( ICALCN.NE.NUMCAL) THEN
          ICALCN=NUMCAL
          UHF=(INDEX(KEYWRD,'UHF') .NE. 0)
          HALFE=(NCLOSE .NE. NOPEN)
          IF(HALFE) THEN
              IHOMO=(NCLOSE-1)*NORBS+1
              ILUMO=IHOMO+NORBS
          ENDIF
          DUBLET=(NOPEN-NCLOSE .EQ. 1)
          TRIPLT=(INDEX(KEYWRD,'TRIPLET') .NE. 0)
      ENDIF
      NFIRST(1)=1
      NLAST(1)=IL-IF+1
      NFIRST(2)=NLAST(1)+1
      NLAST(2)=NFIRST(2)+JL-JF
C      WRITE(6,'(4I4)')(NFIRST(I),NLAST(I),I=1,2)
      LINEAR=(NLAST(2)*(NLAST(2)+1))/2
      DO 10 I=1,LINEAR
          F(I)=0.D0
   10     H(I)=0.0D00
      JA=NFIRST(2)
      JB=NLAST(2)
      IA=NFIRST(1)
      IB=NLAST(1)
      JT=JB*(JB+1)/2
      J=2
      I=1
      NJ=NAT(2)
      NI=NAT(1)
      CALL H1ELEC(NI,NJ,XI(1,1),XI(1,2),SHMAT)
      J1=0
      DO 1 J=JA,JB
          JJ=J*(J-1)/2
          J1=J1+1
          I1=0
          DO 1 I=IA,IB
              JJ=JJ+1
              I1=I1+1
   1          H(JJ)=SHMAT(I1,J1)*2.D0
      CALL ROTATE (NJ,NI,XI(1,2),XI(1,1),W,KR,E2A,E1B,ENUCLR)
C
C    * ENUCLR IS SUMMED OVER CORE-CORE REPULSION INTEGRALS.
C
          I2=0
          DO 7 I1=IA,IB
              II=I1*(I1-1)/2+IA-1
              DO 7 J1=IA,I1
                  II=II+1
                  I2=I2+1
   7              H(II)=H(II)+E1B(I2)*2.D0
          I2=0
          DO 8 I1=JA,JB
              II=I1*(I1-1)/2+JA-1
              DO 8 J1=JA,I1
                  II=II+1
                  I2=I2+1
   8              H(II)=H(II)+E2A(I2)*2.D0
      CALL FOCK2(F,P,PA,W,2,NFIRST,NLAST)
      EE=HELECT(NLAST(2),PA,H,F)
      IF( UHF ) THEN
          DO 9 I=1,LINEAR
   9      F(I)=0.D0
          CALL FOCK2(F,P,PB,W,2,NFIRST,NLAST)
          EE=EE+HELECT(NLAST(2),PB,H,F)
        ELSE
          EE=EE*2.D0
      ENDIF
      DENER=EE+ENUCLR
      RETURN

C
      END

