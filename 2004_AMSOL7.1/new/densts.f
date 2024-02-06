      SUBROUTINE DENSTS( C,MDIM, NORBS,NDUBL, NSINGL, FRACT, P)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'                                                GDH1294
      DIMENSION P(*), C(MDIM,NORBS)                                     GDH1294
         SAVE                                                           GL0892
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
C    TAKE ELECTRON EQUIVALENT
C
         NL2=1
         HORBS=NORBS*0.5D0
         NU2=NDUBL
         NL1=NDUBL+1
         NU1=NSINGL
         CONST=0.D0
      L = 1
      DO 40 I=1,NORBS
      L = L + I - 1
           DO 30 J=L,L+I-1
           P(J) = 0.0
  30       CONTINUE
C
           DO 20 K=NL2,NU2
                DO 10 J=L,L+I-1
                P(J) = P(J) +(2.0*C(I,K))*C(J-L+1,K)
  10            CONTINUE
  20       CONTINUE
C
           DO 50 K=NL1,NU1
                DO 60 J=L,L+I-1
                P(J) = P(J) + (FRACT*C(I,K))*C(J-L+1,K)
  60            CONTINUE
  50       CONTINUE
C
           IF(NSINGL.LT.HORBS) THEN
                DO 70 J=L,L+I-1
                P(J) = - P(J)
  70            CONTINUE
           P(L+I-1) = CONST + P(L+I-1)
           ENDIF
C
  40  CONTINUE
      RETURN
      END
