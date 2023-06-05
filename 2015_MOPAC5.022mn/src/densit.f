      SUBROUTINE DENSIT( C,MDIM, NORBS,NDUBL, NSINGL, FRACT, P, MODE,*) CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P(*), C(MDIM,*)
CSAV         SAVE                                                           GL0892
C***********************************************************************
C
C   DENSIT COMPUTES THE DENSITY MATRIX GIVEN THE EIGENVECTOR MATRIX, AND
C          INFORMATION ABOUT THE M.O. OCCUPANCY.
C
C  INPUT:  C     = SQUARE EIGENVECTOR MATRIX, C IS OF SIZE MDIM BY MDIM
C                  AND THE EIGENVECTORS ARE STORED IN THE TOP LEFT-HAND
C                  CORNER.
C          NORBS = NUMBER OF ORBITALS
C          NDUBL = NUMBER OF DOUBLY-OCCUPIED M.O.S ( =0 IF UHF)
C          NSINGL= NUMBER OF SINGLY OR FRACTIONALLY OCCUPIED M.O.S.
C
C   ON EXIT: P   = DENSITY MATRIX
C
C***********************************************************************
C
C SET UP LIMITS FOR SUMS
C  NL1 = BEGINING OF ONE ELECTRON SUM
C  NU1 = END OF SAME
C  NL2 = BEGINING OF TWO ELECTRON SUM
C  NU2 = END OF SAME
C
      NORBS2=NORBS/2
      IF(NSINGL .GT. NORBS2 .AND. MODE .EQ. 1) THEN
C
C    TAKE POSITRON EQUIVALENT
C
         SIGN=-1.D0
         FRAC=2.D0-FRACT
         IF(NDUBL.EQ.0)THEN
            CONST=1.D0
            NL2=2
            NU2=0
            NL1=NSINGL+1
            NU1=NORBS
         ELSE
            CONST=2.D0
            NL2=NSINGL+1
            NU2=NORBS
            NL1=NDUBL+1
            NU1=NSINGL
         ENDIF
      ELSE
C
C    TAKE ELECTRON EQUIVALENT
C
         SIGN=1.D0
         FRAC=FRACT
         CONST=0.D0
         NL2=1
         NU2=NDUBL
         NL1=NDUBL+1
         NU1=NSINGL
      ENDIF
      l = 1
      do 40 i=1,norbs
      l = l + i - 1
           do 30 j=l,l+i-1
           p(j) = 0.0
  30       continue
c
           do 20 k=nl2,nu2
                do 10 j=l,l+i-1
                p(j) = p(j) +(2.0*c(i,k))*c(j-l+1,k)
  10            continue
  20       continue
c
           do 50 k=nl1,nu1
                do 60 j=l,l+i-1
                p(j) = p(j) + (frac*c(i,k))*c(j-l+1,k)
  60            continue
  50       continue
c
           if(nsingl.gt.norbs2) then
                do 70 j=l,l+i-1
                p(j) = - p(j)
  70            continue
           p(l+i-1) = const + p(l+i-1)
           endif
c
  40  continue
      RETURN
 9999 RETURN 1                                                          CSTP
      END
