      SUBROUTINE IJKL (C,N,N1,NA,W,PQKL,MPQKL,WIJKL,NMCI,DIJKL,LGRAD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C---------------------------------------------------------------------
C     2-ELECTRONS INTEGRALS IN M.O BASIS :
C                 WIJKL WITH I,J,K,L OVER C.I-ACTIVE M.O ,
C                 DIJKL WITH I       OVER     ALL    M.O ,
C                              J,K,L OVER C.I-ACTIVE M.O .
C
C   INPUT
C     C(N,N)     : M.O COEFFICIENTS IN A.O BASIS.
C     N1         : NUMBER OF THE FIRST ACTIVE M.O
C     NA         : NUMBER OF (CONSECUTIVE) ACTIVE M.O
C     W          : 2-CENTRE 2-ELECTRONS INTEGRALS  IN A.O. BASIS.
C     PQKL(MPQKL): SCRATCH ARRAY PROVIDED FOR THE <PQ!KL> FILE
C                  ISSUING FROM THE FIRST 2-INDICES TRANSFORMATION.
C     NMCI       : MAX. SIZE OF WIJKL.
C     LGRAD      : .TRUE. IF DIJKL IS TO BE FILLED,
C                  .FALSE. OTHERWISE.
C   OUTPUT
C     WIJKL(I,J,K,L)= < I(1),J(1) ! K(2),L(2) >
C                     WITH I,J,K,L      OVER C.I-ACTIVE M.O.
C
C     AND IF LGRAD IS .TRUE. :
C     DIJKL(I,J,KL )= < I(1),J(1) ! K(2),L(2) >
C                     WITH I            OVER    ALL     M.O
C                          J            OVER C.I-ACTIVE M.O.
C                          KL CANONICAL OVER C.I-ACTIVE M.O.
C     OTHERWISE DIJKL IS NOT MODIFIED BY THIS ROUTINE.
C   NOTE ... THIS ROUTINE USES 2-INDICES TRANSFORMATION.
C            (1-INDEX TRANSFORMATION IS USELESS WITHIN MNDO FORMALISM).
C     D.L. (DEWAR GROUP)  1986
C-----------------------------------------------------------------------
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
     3       /WMATRX/ WDUMMY(N2ELEC*3),KDUMMY,NBAND(NUMATM)             3GL3092
      COMMON /SCRCHR/ CIJ(NMECI*(NMECI+1)*NUMATM*5),                    3GL3092
     1                DUM3(6*MAXPAR**2+1-NMECI*(NMECI+1)*NUMATM*5)      GCL0393
C
      DIMENSION C(N,N),W(*),PQKL(*),DIJKL(N,NA,*)
     .         ,WIJKL(NMCI,NMCI,NMCI,NMCI)
      DIMENSION BUF((NMECI*(NMECI+1)/2)*(NMECI*(NMECI+1)/2+1)/2)
     .         ,INDX(NUMATM)
      LOGICAL LGRAD
       SAVE
C
C     LENGTH = NUMBER OF CANONICAL COUPLES OF 1-CENTER A.O.
C     INDX(I)= LOCATION OF THE FIRST COUPLE IJ BELONGING TO ATOM I.
      INDX(1)=1
      LENGTH =MAX(0,NBAND(1))
      DO 10 I=2,NUMAT
      INDX(I)=INDX(I-1)+MAX(0,NBAND(I-1))
   10 LENGTH =LENGTH   +MAX(0,NBAND(I))
C
C     DEFINE SIZES AND CHECK AVAILABLE CORE MEMORY.
      I=0
C     AVAILABLE SPACE IN /SCRACH/ ACCORDING TO DERIV,DERI1,DERI2.
      MSCRAH=MAX(NMECI*(NMECI+1)*NUMATM*5,6*MAXPAR**2+1)                DL0397
      IJBAND=MSCRAH/LENGTH
      KLBAND=NA*(NA+1)/2
C     CHECK PQKL(MPKL)
      IF (LENGTH*KLBAND .GT. MPQKL) THEN
         WRITE(JOUT,'('' AVAILABLE STORAGE ('',I10,'') TOO SMALL (''
     .             ,I10,'') IN COMMON /SCRAH2/'')')
     .              MPQKL,LENGTH*KLBAND
         I=1
      ENDIF
C     CHECK CIJ(LENGTH,KLBAND)
      IF (KLBAND.GT.IJBAND) THEN
         WRITE(JOUT,'('' AVAILABLE STORAGE ('',I10,'') TOO SMALL (''
     .             ,I10,'' ) IN COMMON /SCRACH/'')')
     .             MSCRAH,KLBAND*LENGTH
         I=1
      ENDIF
      IF(I.NE.0) THEN
         WRITE(JOUT,'(
     .   ''      ----- FATAL MESSAGE FROM ROUTINE IJKL -----''/
     .   '' REDUCE THE NUMBER OF C.I-ACTIVE MOLECULAR ORBITALS''/
     .   '' OR COMPILE WITH ADEQUATE VALUES OF THE PARAMETERS '',
     .   '' NRELAX, MAXPAR'')')                                         DL0397
         ISTOP=1                                                        GDH1095
         IWHERE=169                                                     GDH1095
         RETURN                                                         GDH1095
      ENDIF
C
C     FIRST 2-INDICES TRANSFORMATION OVER C.I-ACTIVE M.O.
C     ---------------------------------------------------
      IPQKL=1
      IPQ=1
      DO 50 K=1,NA
      MOK=N1-1+K
      DO 50 L=1,K
      MOL=N1-1+L
C     FORM CHARGE DISTRIBUTION VECTOR CIJ # KL.
      DO 40 IIA=1,NUMAT
      DO 20 IP=NFIRST(IIA),NLAST(IIA)
      DO 20 IQ=NFIRST(IIA),IP
      CIJ(IPQ)=C(IP,MOK)*C(IQ,MOL)+C(IP,MOL)*C(IQ,MOK)
   20 IPQ=IPQ+1
   40 CONTINUE
C     COMPUTE PQKL OVER C.I-ACTIVE M.O.
      CALL PQTKL (CIJ(IPQKL),INDX,NBAND,W,PQKL(IPQKL))
   50 IPQKL=IPQ
C
C     SECOND 2-INDICES TRANSFORMATION OVER C.I-ACTIVE M.O.
C     ----------------------------------------------------
C     FORM <IJ!KL> IN CANONICAL ORDER, STORED IN BUF.
      CALL MTXMC (CIJ,KLBAND,PQKL,LENGTH,BUF)
C     EXPAND BUF INTO WIJKL.
      NIJKL=1
      DO 60 I=1,NA
      DO 60 J=1,I
      DO 60 K=1,I
      IF(K.EQ.I) THEN
         LL=J
      ELSE
         LL=K
      ENDIF
      DO 60 L=1,LL
      VAL=BUF(NIJKL)
      NIJKL=NIJKL+1
      WIJKL(I,J,K,L)=VAL
      WIJKL(I,J,L,K)=VAL
      WIJKL(J,I,K,L)=VAL
      WIJKL(J,I,L,K)=VAL
      WIJKL(K,L,I,J)=VAL
      WIJKL(K,L,J,I)=VAL
      WIJKL(L,K,I,J)=VAL
   60 WIJKL(L,K,J,I)=VAL
C
      IF (.NOT.LGRAD) RETURN
C
C     DISPATCH WIJKL(I,J,K,L) AS DIAGONAL BLOCKS OF DIJKL(I,J,KL).
C     ------------------------------------------------------------
      DO 70 I=1,NA
      MOI=N1+I-1
      DO 70 J=1,NA
      KL=0
      DO 70 K=1,NA
      DO 70 L=1,K
      KL=KL+1
   70 DIJKL(MOI,J,KL)=WIJKL(I,J,K,L)
C
C     SECOND 2-INDICES TRANSFORMATION OVER NON ACTIVE/ C.I-ACTIVE M.O.
C     ----------------------------------------------------------------
C     INITIALIZE I1, LOWER BOUND OF THE POINTER I ON NON ACTIVE M.O.
      IF (N1.EQ.1.AND.N1+NA.GT.N) RETURN
      IF (N1.GT.1) THEN
         I1=1
      ELSE
         I1=N1+NA
      ENDIF
C     UPDATE     I2, UPPER BOUND OF THE POINTER I ON NON ACTIVE M.O.
   80 IF (I1.LT.N1) THEN
         I2=MIN(I1+IJBAND,N1)-1
      ELSE
         I2=MIN(I1+IJBAND-1,N)
      ENDIF
      IJBROD=I2-I1+1
C     WORK OUT DIJKL(I,J,KL) FOR I=I1, ... ,I2     NON ACTIVE
C                                J= 1, ... ,NA     C.I-ACTIVE
C                               KL= 1, ... ,KLBAND C.I-ACTIVE
      DO 120 J=1,NA
      MOJ=N1+J-1
      DO 110 I=I1,I2
      IPQ=I-I1+1
C     FORM SECOND CHARGE DISTRIBUTION INTO CIJ(IJBROD,LENGTH)
C     OVER I NON ACTIVE AND J C.I-ACTIVE.
      DO 110 IIA=1,NUMAT
C     SPARKLES ARE SKIPPED OUT (THANKS TO FORTRAN 77)
      DO 90 IP=NFIRST(IIA),NLAST(IIA)
      DO 90 IQ=NFIRST(IIA),IP
      CIJ(IPQ)=C(IP,  I)*C(IQ,MOJ)+C(IP,MOJ)*C(IQ,  I)
   90 IPQ=IPQ+IJBROD
  110 CONTINUE
      IPQKL=1
      DO 120 KL=1,KLBAND
      CALL MXM (CIJ,IJBROD,PQKL(IPQKL),LENGTH,DIJKL(I1,J,KL),1)
  120 IPQKL=IPQKL+LENGTH
C     UPDATE     I1, LOWER BOUND OF THE POINTER I ON NON ACTIVE M.O.
      IF (I2.EQ.N1-1) THEN
         I1=N1+NA
      ELSE
         I1=I2+1
      ENDIF
      IF (I1.LE.N) GO TO 80
      RETURN
      END
