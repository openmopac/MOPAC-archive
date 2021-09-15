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
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,20),NNHCO,ITYPE,USEMM
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U
      COMMON /DCARTC/ K1L,K2L,K3L,K1U,K2U,K3U
      CHARACTER*80 KEYWRD
      DIMENSION PDI(171),PADI(171),PBDI(171),
     1CDI(3,2),NDI(2),LSTOR1(6), LSTOR2(6), ENG(3)
      LOGICAL DEBUG, FIRST, FORCE, MAKEP, ANADER, USEMM
      EQUIVALENCE (LSTOR1(1),L1L), (LSTOR2(1), K1L)
      DATA CHNGE,CHNGE2 /1.D-4,5.D-5/
*
* CHNGE IS A MACHINE-PRECISION DEPENDENT CONSTANT
* CHNGE2=CHNGE/2
*
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
         ANADER= (INDEX(KEYWRD,'ANALYT') .NE. 0)
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
      IF(ANADER) REWIND 2
      KREP=0
      DO 130 II=1,NUMAT
         III=NCELLS*(II-1)+IOFSET
         IM1=II
         IF=NFIRST(II)
         IM=NMIDLE(II)
         IL=NLAST(II)
         NDI(2)=NAT(II)
         DO 30 I=1,3
   30    CDI(I,2)=COORD(I,II)
         DO 130 JJ=1,IM1
            JJJ=NCELLS*(JJ-1)
C  FORM DIATOMIC MATRICES
            JF=NFIRST(JJ)
            JM=NMIDLE(JJ)
            JL=NLAST(JJ)
C   GET FIRST ATOM
            NDI(1)=NAT(JJ)
            MAKEP=.TRUE.
            DO 120 IK=K1L,K1U
               DO 120 JK=K2L,K2U
                  DO 120 KL=K3L,K3U
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
                     IF(II.EQ.JJ) GOTO  120
                     IF(ANADER)THEN
                        CALL ANALYT(PDI,PADI,PBDI,CDI,NDI,JF,JL,IF,IL
     1,                 NORBS,ENG,KREP)
                        DO 100 K=1,3
                           DXYZ(K,III)=DXYZ(K,III)+ENG(K)
  100                   DXYZ(K,JJJ)=DXYZ(K,JJJ)-ENG(K)
                     ELSE
                        IF( .NOT. FORCE) THEN
                           CDI(1,1)=CDI(1,1)+CHNGE2
                           CDI(2,1)=CDI(2,1)+CHNGE2
                           CDI(3,1)=CDI(3,1)+CHNGE2
                           CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,IM
     1,IL,                 NORBS,AA)
                        ENDIF
                        DO 110 K=1,3
                           IF( FORCE )THEN
                              CDI(K,2)=CDI(K,2)-CHNGE2
                              CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF
     1,IM,IL,                 NORBS,AA)
                           ENDIF
                           CDI(K,2)=CDI(K,2)+CHNGE
                           CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,IM
     1,IL,                 NORBS,EE)
                           CDI(K,2)=CDI(K,2)-CHNGE2
                           IF( .NOT. FORCE) CDI(K,2)=CDI(K,2)-CHNGE2
                           DERIV=(AA-EE)*46.122D0/CHNGE
                           DXYZ(K,III)=DXYZ(K,III)+DERIV
                           DXYZ(K,JJJ)=DXYZ(K,JJJ)-DERIV
  110                   CONTINUE
                     ENDIF
  120       CONTINUE
  130 CONTINUE
      IF(USEMM)THEN
C
C   NOW ADD IN MOLECULAR-MECHANICS CORRECTION TO THE H-N-C=O TORSION
C
         DEL=1.D-5
         DO 160 I=1,NNHCO
            DO 150 J=1,4
               DO 140 K=1,3
                  COORD(K,NHCO(J,I))=COORD(K,NHCO(J,I))-DEL
                  CALL DIHED(COORD,NHCO(1,I),NHCO(2,I),NHCO(3,I),NHCO(4,
     1I),ANGLE)
                  REFH=HTYPE(ITYPE)*SIN(ANGLE)**2
                  COORD(K,NHCO(J,I))=COORD(K,NHCO(J,I))+DEL*2.D0
                  CALL DIHED(COORD,NHCO(1,I),NHCO(2,I),NHCO(3,I),NHCO(4,
     1I),ANGLE)
                  COORD(K,NHCO(J,I))=COORD(K,NHCO(J,I))-DEL
                  HEAT=HTYPE(ITYPE)*SIN(ANGLE)**2
                  SUM=(REFH-HEAT)/DEL
                  DXYZ(K,NHCO(J,I))=DXYZ(K,NHCO(J,I))+SUM
  140          CONTINUE
  150       CONTINUE
  160    CONTINUE
      ENDIF
      DO 170 I=1,6
  170 LSTOR1(I)=LSTOR2(I)
      IF (  .NOT. DEBUG) RETURN
      WRITE(6,'(//10X,''CARTESIAN COORDINATE DERIVATIVES'',//3X,
     1''ATOM  AT. NO.'',5X,''X'',12X,''Y'',12X,''Z'',/)')
      WRITE(6,'(2I6,F13.6,2F13.6)')
     1 (I,NAT((I-1)/NCELLS+1),(DXYZ(J,I),J=1,3),I=1,NUMTOT)
      IF (ANADER) REWIND 2
      RETURN
      END
