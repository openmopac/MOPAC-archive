      SUBROUTINE MATOUT (A,B,NC,NR,NDIM) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
      DIMENSION A(NDIM,NDIM), B(NDIM) 
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /ELEMTS/ ELEMNT(107) 
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
       SAVE
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
   60 KB=MIN(KC,NC)                                                     GCL0393
      WRITE (JOUT,100) (I,I=KA,KB) 
      IF(B(1).NE.0.D0)WRITE (JOUT,110) (B(I),I=KA,KB) 
      WRITE (JOUT,120) 
      LA=1 
      LC=40 
   70 LB=MIN(LC,NR)                                                     GCL0393
      DO 80 I=LA,LB 
         IF(ITEXT(I).EQ.' S')WRITE(JOUT,120) 
         WRITE (JOUT,130) ITEXT(I),JTEXT(I),NATOM(I),(A(I,J),J=KA,KB) 
   80 CONTINUE 
      IF (LB.EQ.NR) GO TO 90 
      LA=LC+1 
      LC=LC+40 
      WRITE (JOUT,140) 
      GO TO 70 
   90 IF (KB.EQ.NC) RETURN 
      KA=KC+1 
      KC=KC+6 
      IF (NR.GT.25) WRITE (JOUT,140) 
      GO TO 60 
  100 FORMAT (////,3X,9H ROOT NO.,I5,9I12) 
  110 FORMAT (/8X,10F12.5) 
  120 FORMAT (2H  ) 
  130 FORMAT (1H ,2(1X,A2),I3,F10.5,10F12.5) 
  140 FORMAT (1H1) 
      END 
