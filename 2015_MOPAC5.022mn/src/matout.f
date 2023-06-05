      SUBROUTINE MATOUT (A,B,NC,NR,NDIM,*)                              CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION A(NDIM,NDIM), B(NDIM)
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /ELEMTS/ ELEMNT(120)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
C**********************************************************************
C
C      MATOUT PRINTS A SQUARE MATRIX OF EIGENVECTORS AND EIGENVALUES
C
C    ON INPUT A CONTAINS THE MATRIX TO BE PRINTED.
C             B CONTAINS THE EIGENVALUES.
C             NC NUMBER OF MOLECULAR ORBITALS TO BE PRINTED.
C             NR IS THE SIZE OF THE SQUARE ARRAY TO BE PRINTED.
C             NDIM IS THE ACTUAL SIZE OF THE SQUARE ARRAY "A".
C             NFIRST AND NLAST CONTAIN ATOM ORBITAL COUNTERS.
C             NAT = ARRAY OF ATOMIC NUMBERS OF ATOMS.
C
C
C***********************************************************************
      CHARACTER*2 ELEMNT, ATORBS(9), ITEXT(MAXORB), JTEXT(MAXORB)
      DIMENSION NATOM(MAXORB)
CSAV         SAVE                                                           GL0892
      DATA ATORBS/' S','PX','PY','PZ','X2','XZ','Z2','YZ','XY'/
      IF(NUMAT.EQ.0)GOTO 30
      IF(NLAST(NUMAT).NE.NR) GOTO 30
      DO 20 I=1,NUMAT
         JLO=NFIRST(I)
         JHI=NLAST(I)
         L=NAT(I)
         K=0
         DO 10 J=JLO,JHI
            K=K+1
            ITEXT(J)=ATORBS(K)
            JTEXT(J)=ELEMNT(L)
            NATOM(J)=I
   10    CONTINUE
   20 CONTINUE
      GOTO 50
   30 CONTINUE
      NR=ABS(NR)
      DO 40 I=1,NR
         ITEXT(I)='  '
         JTEXT(I)='  '
   40 NATOM(I)=I
   50 CONTINUE
      KA=1
      KC=6
   60 KB=MIN0(KC,NC)
      IF (DOPRNT) WRITE (6,100) (I,I=KA,KB)                             CIO
      IF(DOPRNT.AND.(B(1).NE.0.D0)) WRITE (6,110) (B(I),I=KA,KB)        CIO
      IF (DOPRNT) WRITE (6,120)                                         CIO
      LA=1
      LC=40
   70 LB=MIN0(LC,NR)
      DO 80 I=LA,LB
         IF(DOPRNT.AND.(ITEXT(I).EQ.' S')) WRITE(6,120)                 CIO
         IF (DOPRNT)                                                    CIO
     &     WRITE(6,130) ITEXT(I),JTEXT(I),NATOM(I),(A(I,J),J=KA,KB)     CIO
   80 CONTINUE
      IF (LB.EQ.NR) GO TO 90
      LA=LC+1
      LC=LC+40
      IF (DOPRNT) WRITE (6,140)                                         CIO
      GO TO 70
   90 IF (KB.EQ.NC) RETURN
      KA=KC+1
      KC=KC+6
      IF (DOPRNT.AND.(NR.GT.25)) WRITE (6,140)                          CIO
      GO TO 60
C
  100 FORMAT (////,3X,9H ROOT NO.,I5,9I12)
  110 FORMAT (/8X,10F12.5)
  120 FORMAT (2H  )
  130 FORMAT (2(1X,A2),I4,F10.5,10F12.5)
  140 FORMAT (1H1)
C
      RETURN
 9999 RETURN 1                                                          CSTP
      END
