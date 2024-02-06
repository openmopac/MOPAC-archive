      SUBROUTINE FINDS(P,XNEW,FNEW,N,ICURV,GRAD,NOPT,AK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH0196
C     ...
C     FIND S DIRECTION   GRADIENT OR NEWTON OR EIGENVECTORS
C                            AND ...
C     ESTABLISH SECOND ORDER PREVISION AND CONDITIONS :
C     USUALLY EUCLIDAN GRADIENT NORM DECREASE  (IN FIRST ORDER) ALONG
C     S DIRECTION FOR A POSITIVE STEP AK.
C     EXCEPTION : EIGENVECTOR DIRECTION.
C     ...
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / IPOLYN(10), ISTAT, NSOL, NPOINT, NPTMAX,         3GL3092
     1                 IESNO, NOIES, IDUMMY(19 + 2*NCHAIN - 16)         3GL3092
      COMMON /OPTMCL / INFLEX,IMPROV,FINAL, LDUMMY(NCHAIN + 4 - 3)      GCL0892
      COMMON /OPTMCR / HNEW(MAXHES),GNEW(MAXPAR),G(MAXPAR,10),          3GL3092
     1                 G0(MAXPAR),G1(MAXPAR),G2(MAXPAR),CURENT(MAXPAR), 3GL3092
     2                 X(10),EI(10),GGI(10),E(10),WW(10),GR(5),ER(4),   3GL3092
     3                 SOL(3),VAL(3),A(3,3),                            3GL3092
     4                 RDUMY(MAXPAR*(3*MAXPAR + NCHAIN + 16) + 29 +     3GL3092
     5                       NCHAIN - 15*MAXPAR - 74)                   3GL3092
C     COMMON /OPTIM/ IMP,IMP0,LEC,IPRT, HNEW(MAXHES),GNEW(MAXPAR)
C    .              ,G(MAXPAR,10),G0(MAXPAR),G1(MAXPAR),G2(MAXPAR)
C    .              ,CURENT(MAXPAR),X(10),EI(10),GGI(10),E(10)
C    .              ,WW(10),GR(5),ER(4),SOL(3),VAL(3),A(3,3)
C    .              ,IPOLYN(10),INFLEX,ISTAT ,NSOL,IMPROV,NPOINT,NPTMAX
C    .              ,IESNO,NOIES,FINAL
      COMMON /PRECI/ SCFCV,SCFTOL,EG(3),ESTIM(3),PMAX(3),KTYP(MAXPAR)
      COMMON /AREACM/ NOPTI,TAREA,NINTEG                                GDH0793
      DIMENSION XNEW(N),GRAD(N),P(N,N)                                  GCL0393
      CHARACTER*8 BARAT(3)                                              GCL0393
      LOGICAL INFLEX,IMPROV,FINAL,TAREA
      LOGICAL LDUMMY                                                    3GL3092
       SAVE
      DATA BARAT/'GRADIENT','NEWTON  ','EIGENVEC'/                      GCL0393
C
      IF (ICURV.GT.0.AND.NOPT.EQ.2) THEN
C
C        SELECT THE HIGHEST NEGATIVE EIGENVECTOR OF THE HESSIAN
         STII=GRAD(ICURV)
         DO 10 I=1,N
   10    GRAD(I)=P(I,ICURV)
         AK=DOT(GNEW,GRAD,N)
         IBARAT=3
         ISTAT=3
         IF (ABS(STII).LE.AK*AK) THEN                                   GCL0393
            AK=-1.D2*SCFCV/AK
         ELSE
            AK=-(SQRT(AK*AK-2.D2*SCFCV*STII)-AK)/STII
         ENDIF
      ELSE
         JCURV=MAX(1,ICURV)                                             GCL0393
         IF(MIN(ABS(GRAD(JCURV)),ABS(GRAD(MIN(N,ICURV+1)))).GT.1.D6     GCL0393
     .                            *EPS1) GO TO 30
C
C        GRADIENT OF THE EUCLIDIAN GRADIENT NORM
         IBARAT=1
         ISTAT=2
         CALL SUPDOT(GRAD,HNEW,GNEW,N,1)
         AK=DOT(GNEW,GRAD,N)/DOT(GRAD,GRAD,N)
      ENDIF
      DO 20 I=1,N
   20 GRAD(I)=AK*GRAD(I)
      INFLEX=.TRUE.
      GO TO 50
C
C     FULL NEWTON-RAPHSON
   30 IBARAT=2
      CALL INVRT1 (P,N,MMDUM,GRAD,EPS1)                                 DL0397
      DO 40 I=1,N
   40 GRAD(I)=-DOT(GNEW,P(1,I),N)
      INFLEX=.FALSE.
      ISTAT=2
   50 IF(NOPT.EQ.2) THEN
C        ENERGY MINIMIZATION ONLY  (NOPT=2) : ASSUME A FIRST ORDER
C        DECREASE OF ENERGY IN POSITIVE STEP DIRECTION
         ISTAT=3
         IF(DOT(GRAD,GNEW,N).GT.0.D0) THEN
            DO 60 I=1,N
   60       GRAD(I)=-GRAD(I)
         ENDIF
      ENDIF
C
C     OPTIMAL POSITIVE STEP
      AK=1.D0
      IF(IMP.GT.1) WRITE(JOUT,110) BARAT(IBARAT),GRAD
      DO 70 I=1,N
      G0(I)=GNEW(I)
   70 G2(I)=0.D0
      CALL SUPDOT(G1,HNEW,GRAD,N,1)
      CALL CARDAN(G0,GRAD,GR,ER,N,SOL,VAL,NSOL,IPOLYN(2))
C     0.1 < AK < 2.0 ... FOR SECURITY
      IF(IBARAT.NE.3) AK=MAX(0.1D0,MIN(SOL(1),2.D0))
C
C     STORE NEW GRADIENT FOR RETURNED VALUE
      CALL SCOPY (N,GRAD,1,HNEW,1)
      CALL SCOPY (N,GNEW,1,GRAD,1)
      RETURN
C
  110 FORMAT(/'      SELECTED DIRECTION ( ',A8,'    )',1P,4D10.2 /
     *      (10D10.2))
      END
