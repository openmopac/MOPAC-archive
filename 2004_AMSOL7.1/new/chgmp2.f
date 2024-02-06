      SUBROUTINE CHGMP2 (P,Q)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'
      DIMENSION P(*),Q(*)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /CM1SUM/ B(NUMATM,NUMATM),QM(NUMATM),DELTA2(NUMATM)        GDH0997
     1               ,QC0(NUMATM),QC(NUMATM)                            GDH0997
     2               ,QD0(NUMATM),FD(NUMATM,NUMATM),QD(NUMATM)          GDH0997
      COMMON /ONESCM/ ICONTR(100)                                       GDH0997
C     COMMON /CKDD/CK(105,105),DD(105,105)                              GDH0997
      COMMON /CKDD/CK(105,105),DD(105,105),Bo                           !JT0802
      INCLUDE 'PARAMS.i'
C ---------------------------------------------------------------------+GDH0997
C     MAP MULLIKEN NET CHARGES TO CM2 NET CHARGES.
C
C     ON INPUT  P  = TOTAL DENSITY MATRIX (CANONICAL ORDER).
C     ON OUTPUT Q  = CM2 NET CHARGES.
C
C     VECTOR QM : MULLIKEN NET CHARGES
C     MATRIX  B : B(I,I)="Bi"; B(I,J)=B(J,I)="Bij"
C
C ---------------------------------------------------------------------+GDH0997
      DATA ZERO /0D0/                                                   GDH0997
C     BUILD MULLIKEN NET CHARGES QM & MATRIX B.                         GDH0997
C                                                                       GDH0997
      K=0                                                               GDH0997
      DO 101 I=1,NUMAT                                                  GDH0997
      QM(I)=ZERO                                                        GDH0997
      CALL SCOPY (NUMAT,ZERO,0,B(1,I),1)                                GDH0997
      DO 101 IAO=NFIRST(I),NLAST(I)                                     GDH0997
      IAOC=IAO*(IAO-1)/2                                                GDH0997
      DO 100 J=1,I-1                                                    GDH0997
      DO 100 JAO=NFIRST(J),NLAST(J)                                     GDH0997
      B(J,I)=B(J,I)+P(IAOC+JAO)**2                                      GDH0997
  100 CONTINUE                                                          GDH0997
      K=K+IAO                                                           GDH0997
      QM(I)=QM(I)+P(K)                                                  GDH0997
  101 CONTINUE                                                          GDH0997
      DO 103 I=1,NUMAT                                                  GDH0997
C     SYMMETRISE B & FILL DIAGONAL ENTRIES.                             GDH0997
      DO 102 J=1,I-1                                                    GDH0997
      B(I,J)=B(J,I)                                                     GDH0997
      B(I,I)=B(I,I)+B(J,I)                                              GDH0997
      B(J,J)=B(J,J)+B(J,I)                                              GDH0997
  102 CONTINUE                                                          GDH0997
C     NET MULLIKEN CHARGES QM                                           GDH0997
      QM(I)=ZHELP(NAT(I))-QM(I)                                         GDH0997
      Q(I)=QM(I)                                                        GDH0997
  103 CONTINUE                                                          GDH0997
      DO 10 I=1,105
      DO 10 J=1,105
      CK(I,J)=0.0
 10   DD(I,J)=0.0
      NSQ=NUMAT*(NUMAT+1)/2
      IF(IAM1.NE.0) GOTO 150
      IF(IPM3.NE.0) GOTO 200
      RETURN
 150  CONTINUE
C     AM1-mapping, H charge = 0.10
C
      CK(1,6)=-0.02
      CK(1,7)=0.207
      CK(1,8)=0.177
      CK(6,7)=0.008
      CK(7,8)=-0.197
      DD(6,7)=0.086
      CK(6,8)=0.026
      DD(6,8)=0.016
      DD(6,9)=0.019
      DD(6,17)=0.027
      DD(6,35)=0.081
      DD(6,53)=0.147
      CK(1,14)=-0.083
      CK(1,16)= 0.038
      CK(6,14)= 0.062
      CK(6,16)=-0.059
      DD(6,16)=0.171
      DD(1,15)=0.103
      DD(6,15)=-0.019
      DD(8,15)=0.088
      DD(9,15)=0.252
      DD(7,8)=0.134  
      DD(8,16)=0.00  
      DD(15,16)=-0.080
      GOTO 666
 200  CONTINUE
C
C
C     PM3-mapping, H charge = 0.10
C
      do i=1,105
      do j=1,105
      CK(I,J)=0.0
      DD(I,J)=0.0
      end do
      end do
      CK(1,6)=0.003
      CK(1,7)=0.274
      CK(1,8)=0.185
      CK(6,7)=-0.022
      DD(6,7)=0.156
      CK(6,8)=0.025
      DD(6,8)=0.016
      CK(6,9)=0.025
      DD(6,17)=0.117
      CK(6,35)=0.040
      CK(7,8)=-0.030
      CK(6,53)=-0.032
      CK(1,14)=-0.021
      CK(6,14)=-0.107
      CK(1,16)= 0.089
      CK(6,16)=-0.033
      DD(6,16)=0.112
      DD(7,8)=-0.043
      DD(8,16)=0.056
C
C     P-H, P-C, P-O, P=O
C     P-F
C
      DD(1,15)=0.253
      DD(6,15)=0.082
      DD(8,15)=0.181
      DD(9,15)=0.244
      DD(15,16)=-0.087
C
  666 CONTINUE

      DO i=1,105
      DO j=1,i
      CK(i,j)=-CK(j,i)
      DD(i,j)=-DD(j,i)
      end do
      end do
C
      DO 300 i=1,NUMAT
      IA=NAT(I)
      DO 300 J=1,i-1
      ij=I*(I-1)/2+j
      JA=NAT(J)
      C=CK(IA,JA)
      D=DD(IA,JA)
      B2=B(i,j)                                                         GDH1197
      DELTA=B2*(D+C*B2)
      Q(I)=Q(I)+DELTA
      Q(J)=Q(J)-DELTA
 300  CONTINUE
      RETURN
      END
