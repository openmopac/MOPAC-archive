      SUBROUTINE BONDS(P)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      PARAMETER (NATMS2=MAXPAR*MAXPAR-MAXORB*MAXORB)
      DIMENSION P(*)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /SCRACH/ B(MAXORB,MAXORB), BONDAB(NATMS2)
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
      WRITE(6,10)
   10 FORMAT(//20X,'BOND ORDERS AND VALENCIES',//)
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
      CALL VECPRT( BONDAB, NUMAT)
      RETURN
      END
