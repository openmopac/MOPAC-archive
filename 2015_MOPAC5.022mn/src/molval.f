      SUBROUTINE MOLVAL(C,NDUMMY,P,NOCC,RHFUHF,*)                       CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION C(NORBS,NORBS), P(*)
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION VAL(MAXORB)
CSAV         SAVE                                                           GL0892
      DO 40 I=1,NOCC
         SUM=0.D0
         DO 30 JJ=1,NUMAT
            JL=NFIRST(JJ)
            JU=NLAST(JJ)
            DO 30 J=JL,JU
               DO 30 KK=1,NUMAT
                  IF(KK.EQ.JJ) GOTO 20
                  KL=NFIRST(KK)
                  KU=NLAST(KK)
                  DO 10 K=KL,KU
                     L1=MAX(J,K)
                     L2=J+K-L1
                     L=(L1*(L1-1))/2+L2
                     SUM=SUM+C(J,I)*C(K,I)*P(L)
   10             CONTINUE
   20             CONTINUE
   30    CONTINUE
         VAL(I)=SUM*RHFUHF
   40 CONTINUE
      IF (DOPRNT) WRITE(6,'(10F8.4)')(VAL(I),I=1,NOCC)                  CIO
      RETURN
 9999 RETURN 1                                                          CSTP
      END
