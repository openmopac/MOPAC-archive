      SUBROUTINE CHGMP3 (P,Q)
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
C     MAP CLASS II NET CHARGES TO CM3 NET CHARGES.                      !JT0802
C
C     CM3 FOR AM1 AND PM3 USES A NEW MAPPING FUNCTIONAL FOR N AND O
C     IN PARTICULAR, DELTA = B2*(D+C*exp(-(B2/Bo)**2)) FOR BOND ORDERS
C     BETWEEN N AND O
C
C     ON INPUT  P  = TOTAL DENSITY MATRIX (CANONICAL ORDER).
C     ON OUTPUT Q  = CM3 NET CHARGES.
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
C
C --- CM3 FOR AM1 ---
C
      CK(6,7) = 0.000D+00
      CK(6,8) = 0.029D+00
      CK(7,8) = 0.439D+00
      CK(8,14) = 0.201D+00
      CK(8,15) = 0.004D+00
      CK(15,16) = 0.341D+00
      DD(1,6) = -0.009D+00
      DD(1,7) = 0.200D+00
      DD(1,8) = 0.154D+00
      DD(1,14) = 0.231D+00
      DD(1,15) = 0.078D+00
      DD(1,16) = 0.072D+00
      DD(3,6) = 0.000D+00
      DD(3,7) = 0.000D+00
      DD(3,8) = 0.000D+00
      DD(3,16) = 0.000D+00
      DD(3,9) = 0.000D+00
      DD(3,17) = 0.000D+00
      DD(6,7) = 0.091D+00
      DD(6,8) = -0.012D+00
      DD(6,9) = 0.006D+00
      DD(6,14) = 0.163D+00
      DD(6,15) = 0.106D+00
      DD(6,16) = 0.103D+00
      DD(6,17) = 0.013D+00
      DD(6,35) = 0.073D+00
      DD(7,8) = -0.132D+00
      DD(7,15) = -0.067D+00
      DD(8,14) = 0.169D+00
      DD(8,15) = 0.033D+00
      DD(8,16) = 0.103D+00
      DD(9,14) = 0.228D+00
      DD(9,15) = 0.110D+00
      DD(14,17) = -0.171D+00
      DD(15,16) = -0.685D+00
      DD(15,17) = -0.252D+00
      Bo = 0.482D+00
      GOTO 666
 200  CONTINUE
C
C --- CM3 FOR PM3 ---
C
      do i=1,105
      do j=1,105
      CK(I,J)=0.0
      DD(I,J)=0.0
      end do
      end do
      CK(6,7) = -0.017D+00
      CK(6,8) = 0.010D+00
      CK(7,8) = 0.274D+00
      CK(8,14) = -0.044D+00
      CK(8,15) = -0.076D+00
      CK(15,16) = 0.245D+00
      DD(1,6) = 0.021D+00
      DD(1,7) = 0.243D+00
      DD(1,8) = 0.153D+00
      DD(1,14) = 0.191D+00
      DD(1,15) = 0.233D+00
      DD(1,16) = 0.118D+00
      DD(3,6) = 0.192D+00
      DD(3,7) = 0.403D+00
      DD(3,8) = 0.390D+00
      DD(3,16) = 0.268D+00
      DD(3,9) = 0.430D+00
      DD(3,17) = 0.117D+00
      DD(6,7) = 0.126D+00
      DD(6,8) = 0.018D+00
      DD(6,9) = 0.019D+00
      DD(6,14) = 0.069D+00
      DD(6,15) = 0.197D+00
      DD(6,16) = 0.076D+00
      DD(6,17) = 0.097D+00
      DD(6,35) = 0.028D+00
      DD(7,8) = -0.085D+00
      DD(7,15) = 0.018D+00
      DD(8,14) = 0.141D+00
      DD(8,15) = 0.231D+00
      DD(8,16) = 0.125D+00
      DD(9,14) = 0.099D+00
      DD(9,15) = 0.175D+00
      DD(14,17) = -0.180D+00
      DD(15,16) = -0.534D+00
      DD(15,17) = -0.394D+00
      Bo = 0.460D+00
      GOTO 666
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
      If ((IA .EQ. 7 .AND. JA .EQ. 8)  .OR.
     $    (IA .EQ. 8 .AND. JA .EQ. 7)) THEN
        DELTA=B2*(D+C*exp(-(B2/Bo)**2))
      ELSE
        DELTA=B2*(D+C*B2) 
      END IF
      Q(I)=Q(I)+DELTA
      Q(J)=Q(J)-DELTA
 300  CONTINUE
      RETURN
      END
