      SUBROUTINE BONDS(P,*)                                             CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      PARAMETER (NATMS2=MAXPAR*MAXPAR-MAXORB*MAXORB)
      DIMENSION P(*),B(MAXORB,MAXORB)
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      LOGICAL PRINT
      COMMON /BONDCM/ BONDAB(NATMS2),PRINT
C*TNT      COMMON /SCRACH/ B(MAXORB,MAXORB), BONDAB(NATMS2)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
C***********************************************************************
C
C   CALCULATES, AND PRINTS, THE BOND INDICES AND VALENCIES OF ATOMS
C
C  FOR REFERENCE, SEE "BOND INDICES AND VALENCY", J. C. S. DALTON,
C  ARMSTRONG, D.R., PERKINS, P.G., AND STEWART, J.J.P., 838 (1973)
C
C   ON INPUT
C            P = DENSITY MATRIX, LOWER HALF TRIANGLE, PACKED.
C            P   IS NOT ALTERED BY BONDS.
C
C***********************************************************************
CSAV         SAVE                                                           GL0892
      IF(PRINT) THEN
        IF (DOPRNT) WRITE(6,10)                                         CIO
   10   FORMAT(//20X,'BOND ORDERS AND VALENCIES',//)
      END IF
      K=0
      DO 20 I=1,NORBS
         DO 20 J=1,I
            K=K+1
            B(I,J)=P(K)
   20 B(J,I)=P(K)
      IJ = 0
      DO 60 I=1,NUMAT
         L=NFIRST(I)
         LL=NLAST(I)
         DO 40 J=1,I
            IJ = IJ + 1
            K=NFIRST(J)
            KK=NLAST(J)
            X=0.0
            DO 30 IL=L,LL
               DO 30 IH=K,KK
   30       X=X+B(IL,IH)*B(IL,IH)
   40    BONDAB(IJ)=X
         X=-BONDAB(IJ)
         DO 50 J=L,LL
   50    X=X+2.D0*B(J,J)
         BONDAB(IJ)=X
   60 CONTINUE
      IF (PRINT) THEN
      CALL VECPRT( BONDAB, NUMAT,*9999)                                 CSTP(call)
      END IF
      RETURN
 9999 RETURN 1                                                          CSTP
      END
