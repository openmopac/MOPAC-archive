      SUBROUTINE CHRGE(P,Q,*)                                           CSTP                                             
C FROM m507_unmod.f                                                     0909YC99
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION P(*),Q(*)
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /KEYWRD/ KEYWRD                                            0909YC99
      CHARACTER*160 KEYWRD                                               0909YC99
CSAV         SAVE                                                           GL0892
C***********************************************************************
C
C      CHRGE STORES IN Q THE TOTAL ELECTRON DENSITIES ON THE ATOMS
C
C      ON INPUT P      = DENSITY MATRIX
C
C      ON OUTPUT Q     = ATOM ELECTRON DENSITIES
C
C***********************************************************************
      IF(INDEX(KEYWRD,'CM2').NE.0 ) THEN                                0909YC99
         CALL CHGMP2(P,Q,*9999)                                         0909YC99 CSTP(call)
      ELSE                                                              0909YC99
      K=0
      DO 10 I=1,NUMAT
         IA=NFIRST(I)
         IB=NLAST(I)
         Q(I)=0.D0
         DO 10 J=IA,IB
            K=K+J
   10 Q(I)=Q(I)+P(K)
      ENDIF                                                             0909YC99
      RETURN
 9999 RETURN 1                                                          CSTP
      END
