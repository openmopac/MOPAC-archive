      SUBROUTINE DIJKL1 (C,N,N1,NA,W,PQKL,MPQKL,WIJKL,NMCI,NATI,CIJRDY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C-------------------------------------------------------------------
C     2-ELECTRONS INTEGRALS DERIVATIVES IN M.O BASIS :
C                 WIJKL WITH I,J,K,L OVER C.I-ACTIVE M.O ,
C
C   INPUT
C     C(N,N)     : M.O COEFFICIENTS IN A.O BASIS.
C     N1         : NUMBER OF THE FIRST ACTIVE M.O
C     NA         : NUMBER OF (CONSECUTIVE) ACTIVE M.O
C     W          : 2-CENTRE 2-ELECTRONS INTEGRALS DERIVATIVES IN A.O.
C     PQKL(MPQKL): SCRATCH ARRAY PROVIDED FOR THE <PQ!KL> FILE
C                  ISSUING FROM THE FIRST 2-INDICES TRANSFORMATION.
C     NMCI       : MAX. SIZE OF WIJKL.
C     NATI       : # OF THE MOVING ATOM.
C     CIJRDY     : .TRUE. IF THE CIJ MATRIX IS AVAILABLE IN /SCRACH/
C                  .FALSE. OTHERWISE (NEW BAND OR NEW ATOM, SEE DERIV)
C   OUTPUT
C     WIJKL(I,J,K,L)= < I(1),J(1) !d(1/R12)! K(2),L(2) >
C                     WITH I,J,K,L      OVER C.I-ACTIVE M.O.
C
C   NOTE ... THIS ROUTINE USES 2-INDICES TRANSFORMATION.
C            (1-INDEX TRANSFORMATION IS USELESS WITHIN MNDO FORMALISM).
C     D.L. (DEWAR GROUP) 1986
C-----------------------------------------------------------------------
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
     3       /WMATRX/ WDUMMY(N2ELEC*3),KDUMMY,NBAND(NUMATM)             3GL3092
      COMMON /SCRCHR/ CIJ(NMECI*(NMECI+1)*NUMATM*5),                    3GL3092
     1                DUM3(6*MAXPAR**2+1-NMECI*(NMECI+1)*NUMATM*5)      GCL0393
C
      DIMENSION C(N,N),W(*),PQKL(*)
     .         ,WIJKL(NMCI,NMCI,NMCI,NMCI)
      DIMENSION BUF((NMECI*(NMECI+1)/2)*(NMECI*(NMECI+1)/2+1)/2)
     .         ,INDX(NUMATM)
      LOGICAL CIJRDY
       SAVE
      DATA NATOLD /0/
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
     .   ''      ----- FATAL MESSAGE FROM ROUTINE DIJKL1 -----''/
     .   '' REDUCE THE NUMBER OF C.I-ACTIVE MOLECULAR ORBITALS''/
     .   '' OR COMPILE WITH ADEQUATE VALUES OF THE PARAMETERS '',
     .   '' NRELAX, MAXPAR'')')                                         DL0397
         ISTOP=1                                                        GDH1095
         IWHERE=168                                                     GDH1095
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
      IF (.NOT.CIJRDY) THEN
C        FORM CHARGE DISTRIBUTION MATRIX CIJ, NATI IN LAST POSITION.
         MOL=N1-1+L
         DO 30 IIA=1,NUMAT
         IF (IIA.EQ.NATI) GO TO 30
         DO 20 IP=NFIRST(IIA),NLAST(IIA)
         DO 20 IQ=NFIRST(IIA),IP
         CIJ(IPQ)=C(IP,MOK)*C(IQ,MOL)+C(IP,MOL)*C(IQ,MOK)
   20    IPQ=IPQ+1
   30    CONTINUE
         DO 40 IP=NFIRST(NATI),NLAST(NATI)
         DO 40 IQ=NFIRST(NATI),IP
         CIJ(IPQ)=C(IP,MOK)*C(IQ,MOL)+C(IP,MOL)*C(IQ,MOK)
   40    IPQ=IPQ+1
      ENDIF
C     COMPUTE PQKL FOR THE DISTRIBUTION KL.
      CALL DPQTKL (CIJ(IPQKL),NBAND,W,PQKL(IPQKL),NATI,LENGTH)
   50 IPQKL=IPQKL+LENGTH
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
      RETURN
      END
