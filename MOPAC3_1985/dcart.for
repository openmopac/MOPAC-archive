      SUBROUTINE DCART (COORD,DXYZ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION COORD(3,*), DXYZ(3,*)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
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
      COMMON /EULER / TVEC(3,3), ID
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U, K1L,K2L,K3L,K1U,K2U,K3U
      CHARACTER*80 KEYWRD
      DIMENSION PDI(171),PADI(171),PBDI(171),
     1CDI(3,2),NDI(2),LSTOR1(6), LSTOR2(6)
      LOGICAL DEBUG, FIRST, FORCE, MAKEP
      EQUIVALENCE (LSTOR1(1),L1L), (LSTOR2(1), K1L)
C#      DATA CHNGE,CHNGE2 /1.D-6,5.D-7/
      DATA CHNGE,CHNGE2 /1.D-4,5.D-5/
*
* CHNGE IS A MACHINE-PRECISION DEPENDENT CONSTANT
* CHNGE2=CHNGE/2
*
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
         DEBUG = (INDEX(KEYWRD,'DCART') .NE. 0)
         FORCE = (INDEX(KEYWRD,'PRECISE')+INDEX(KEYWRD,'FORCE') .NE. 0)
         FIRST = .FALSE.
      ENDIF
      NCELLS=(L1U-L1L+1)*(L2U-L2L+1)*(L3U-L3L+1)
      DO 10 I=1,6
         LSTOR2(I)=LSTOR1(I)
   10 LSTOR1(I)=0
      IOFSET=(NCELLS+1)/2
      NUMTOT=NUMAT*NCELLS
      DO 20 I=1,NUMTOT
         DO 20 J=1,3
   20 DXYZ(J,I)=0.D0
      DO 120 II=1,NUMAT
         III=NCELLS*(II-1)+IOFSET
         IM1=II
         IF=NFIRST(II)
         IM=NMIDLE(II)
         IL=NLAST(II)
         NDI(2)=NAT(II)
         DO 30 I=1,3
   30    CDI(I,2)=COORD(I,II)
         DO 120 JJ=1,IM1
            JJJ=NCELLS*(JJ-1)
C  FORM DIATOMIC MATRICES
            JF=NFIRST(JJ)
            JM=NMIDLE(JJ)
            JL=NLAST(JJ)
C   GET FIRST ATOM
            NDI(1)=NAT(JJ)
            MAKEP=.TRUE.
            DO 110 IK=K1L,K1U
               DO 110 JK=K2L,K2U
                  DO 110 KL=K3L,K3U
                     JJJ=JJJ+1
                     DO 40 L=1,3
   40                CDI(L,1)=COORD(L,JJ)+TVEC(L,1)*IK+TVEC(L,2)*JK+TVEC
     1(L,3)*KL
                     IF(.NOT. MAKEP) GOTO 90
                     MAKEP=.FALSE.
                     IJ=0
                     DO 50 I=JF,JL
                        K=I*(I-1)/2+JF-1
                        DO 50 J=JF,I
                           IJ=IJ+1
                           K=K+1
                           PADI(IJ)=PA(K)
                           PBDI(IJ)=PB(K)
   50                PDI(IJ)=P(K)
C GET SECOND ATOM FIRST ATOM INTERSECTION
                     DO 80 I=IF,IL
                        L=I*(I-1)/2
                        K=L+JF-1
                        DO 60 J=JF,JL
                           IJ=IJ+1
                           K=K+1
                           PADI(IJ)=PA(K)
                           PBDI(IJ)=PB(K)
   60                   PDI(IJ)=P(K)
                        K=L+IF-1
                        DO 70 L=IF,I
                           K=K+1
                           IJ=IJ+1
                           PADI(IJ)=PA(K)
                           PBDI(IJ)=PB(K)
   70                   PDI(IJ)=P(K)
   80                CONTINUE
   90                CONTINUE
                     IF(II.EQ.JJ) GOTO  110
                     IF( .NOT. FORCE) THEN
                        CDI(1,1)=CDI(1,1)+CHNGE2
                        CDI(2,1)=CDI(2,1)+CHNGE2
                        CDI(3,1)=CDI(3,1)+CHNGE2
                        CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,IM,IL
     1,                 NORBS,AA,CUC)
                     ENDIF
                     DO 100 K=1,3
                        IF( FORCE )THEN
                           CDI(K,2)=CDI(K,2)-CHNGE2
                           CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,IM
     1,IL,                 NORBS,AA,CUC)
                        ENDIF
                        CDI(K,2)=CDI(K,2)+CHNGE
                        CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,IM,IL
     1,                 NORBS,EE,CUC)
                        CDI(K,2)=CDI(K,2)-CHNGE2
                        IF( .NOT. FORCE) CDI(K,2)=CDI(K,2)-CHNGE2
                        DERIV=(AA-EE)*46.122D0/CHNGE
C#      WRITE(6,*)DERIV,II,JJ,K
                        DXYZ(K,III)=DXYZ(K,III)+DERIV
                        DXYZ(K,JJJ)=DXYZ(K,JJJ)-DERIV
  100                CONTINUE
C#      WRITE(6,*)' WHOLE OF DXYZ',III,JJJ,IK,JK,KL
C#      WRITE(6,'(3(3F17.5,/),/)')((DXYZ(J,I),J=1,3),I=1,18)
  110       CONTINUE
  120 CONTINUE
      DO 130 I=1,6
  130 LSTOR1(I)=LSTOR2(I)
      IF (  .NOT. DEBUG) RETURN
      WRITE(6,'(//10X,''CARTESIAN COORDINATE DERIVATIVES'',//3X,
     1''ATOM  AT. NO.'',5X,''X'',12X,''Y'',12X,''Z'',/)')
      WRITE(6,'(2I6,F13.6,2F13.6)')
     1 (I,NAT((I-1)/NCELLS+1),(DXYZ(J,I),J=1,3),I=1,NUMTOT)
      RETURN
      END
      SUBROUTINE DHC (P,PA,PB,XI,NAT,IF,IM,IL,JF,JM,JL,
     1NORBS,DENER,CUC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P(*), PA(*), PB(*)
      DIMENSION XI(3,*),NFIRST(2),NMIDLE(2),NLAST(2),NAT(*)
C***********************************************************************
C
C  DHC CALCULATES THE ENERGY CONTRIBUTIONS FROM THOSE PAIRS OF ATOMS
C         THAT HAVE BEEN MOVED BY SUBROUTINE DERIV.
C
C***********************************************************************
      COMMON /KEYWRD/ KEYWRD
     1       /ONELEC/ USS(107),UPP(107),UDD(107)
      COMMON /EULER / TVEC(3,3), ID
      COMMON /NUMCAL/ NUMCAL
      CHARACTER*80 KEYWRD
      LOGICAL UHF, CUC
      DIMENSION H(171), SHMAT(9,9), F(171),
     1          WJ(100), E1B(10), E2A(10), WK(100), W(100)
      DATA ICALCN /0/
      IF( ICALCN.NE.NUMCAL) THEN
         ICALCN=NUMCAL
         WLIM=4.D0
         IF(ID.EQ.0)WLIM=0.D0
         UHF=(INDEX(KEYWRD,'UHF') .NE. 0)
      ENDIF
      NFIRST(1)=1
      NMIDLE(1)=IM-IF+1
      NLAST(1)=IL-IF+1
      NFIRST(2)=NLAST(1)+1
      NMIDLE(2)=NFIRST(2)+JM-JF
      NLAST(2)=NFIRST(2)+JL-JF
      LINEAR=(NLAST(2)*(NLAST(2)+1))/2
      DO 10 I=1,LINEAR
         F(I)=0.D0
   10 H(I)=0.0D00
      DO 20 I=1,2
         NI=NAT(I)
         J=NFIRST(I)
         H((J*(J+1))/2)=USS(NI)
         H(((J+1)*(J+2))/2)=UPP(NI)
         H(((J+2)*(J+3))/2)=UPP(NI)
         H(((J+3)*(J+4))/2)=UPP(NI)
         H(((J+4)*(J+5))/2)=UDD(NI)
         H(((J+5)*(J+6))/2)=UDD(NI)
         H(((J+6)*(J+7))/2)=UDD(NI)
         H(((J+7)*(J+8))/2)=UDD(NI)
         H(((J+8)*(J+9))/2)=UDD(NI)
         H(((J+9)*(J+10))/2)=UDD(NI)
   20 CONTINUE
      DO 30 I=1,LINEAR
   30 F(I)=H(I)
      JA=NFIRST(2)
      JB=NLAST(2)
      JC=NMIDLE(2)
      IA=NFIRST(1)
      IB=NLAST(1)
      IC=NMIDLE(1)
      JT=JB*(JB+1)/2
      J=2
      I=1
      NJ=NAT(2)
      NI=NAT(1)
C#      WRITE(6,*)' BEFORE H1ELEC'
C#      WRITE(6,'(2I5,6F10.4)')NI, NJ, (XI(J,1),J=1,3), (XI(J,2),J=1,3)
      CALL H1ELEC(NI,NJ,XI(1,1),XI(1,2),SHMAT)
C#      WRITE(6,*)' SHMAT FROM H1ELEC'
C#      DO  66 I=1,4
C#  66  WRITE(6,'(4F12.6)')(SHMAT(J,I),J=1,4)
      J1=0
      DO 40 J=JA,JB
         JJ=J*(J-1)/2
         J1=J1+1
         I1=0
         DO 40 I=IA,IB
            JJ=JJ+1
            I1=I1+1
            H(JJ)=SHMAT(I1,J1)
   40 F(JJ)=SHMAT(I1,J1)
      KR=1
      IF(ID.EQ.0)THEN
         CALL ROTATE (NJ,NI,XI(1,2),XI(1,1),W(KR),KR,E2A,E1B,ENUCLR,100.
     1D0)
      ELSE
         CALL SOLROT (NJ,NI,XI(1,2),XI(1,1),WJ,WK,KR,E2A,E1B,ENUCLR,100.
     1D0)
      ENDIF
C#      WRITE(6,*)'   WJ  FROM SOLROT'
C#      WRITE(6,'(10F8.4)')(WJ(I),I=1,KR-1)
      IF(WJ(1).LT.WLIM)THEN
         DO 50 I=1,KR-1
   50    WK(I)=0.D0
      ENDIF
C#      WRITE(6,*)'   WK  FROM SOLROT'
C#      WRITE(6,'(10F8.4)')(WK(I),I=1,KR-1)
C
C    * ENUCLR IS SUMMED OVER CORE-CORE REPULSION INTEGRALS.
C
      I2=0
      DO 60 I1=IA,IC
         II=I1*(I1-1)/2+IA-1
         DO 60 J1=IA,I1
            II=II+1
            I2=I2+1
            H(II)=H(II)+E1B(I2)
   60 F(II)=F(II)+E1B(I2)
      DO  70 I1=IC+1,IB
         II=(I1*(I1+1))/2
         F(II)=F(II)+E1B(1)
   70 H(II)=H(II)+E1B(1)
      I2=0
      DO 80 I1=JA,JC
         II=I1*(I1-1)/2+JA-1
         DO 80 J1=JA,I1
            II=II+1
            I2=I2+1
            H(II)=H(II)+E2A(I2)
   80 F(II)=F(II)+E2A(I2)
      DO 90 I1=JC+1,JB
         II=(I1*(I1+1))/2
         F(II)=F(II)+E2A(1)
   90 H(II)=H(II)+E2A(1)
C#      CALL VECPRT(F,NLAST(2))
      CALL FOCK2D(F,P,PA,W, WJ, WK,2,NFIRST,NMIDLE,NLAST)
      EE=HELECT(NLAST(2),PA,H,F)
      IF( UHF ) THEN
         DO 100 I=1,LINEAR
  100    F(I)=H(I)
         CALL FOCK2D(F,P,PB,W, WJ, WK,2,NFIRST,NMIDLE,NLAST)
         EE=EE+HELECT(NLAST(2),PB,H,F)
      ELSE
         EE=EE*2.D0
      ENDIF
C#      CALL VECPRT(PA,NLAST(2))
C#      CALL VECPRT(F,NLAST(2))
      DENER=EE+ENUCLR
C#      WRITE(6,*)' EE ENUCLR DENER', EE, ENUCLR, DENER
      RETURN
C
      END
