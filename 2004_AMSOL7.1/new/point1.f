      SUBROUTINE POINT1(P,XNEW,FNEW,N,EPS,ITERAT,LIMIT,GRAD) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C     ----- ANALYTICAL-NUMERICAL METHODS ----- 
C     ENERGY,GRADIENT AND SECOND DERIVATIVES AT CURRENT POINT XNEW , 
C     CONVERGENCE ON RMS GRADIENT : SQRT(<G!G>/N), 
C     LOCAL CURVATURE INFORMATION . 
C     S DIRECTION BY SECOND ORDER METHODS (MINIMUM OR STAT. POINT) , 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / M,M2,IROUTE,ITYP,ICURV,IOPT1,NOPT,               DJG0995
     1                 IDUMMY(19 + 2*NCHAIN - 7)                        3GL3092
      COMMON /OPTMCL / LFINAL,LBIS(2), LDUMMY(NCHAIN + 1)               GCL0892
      COMMON /OPTMCR / HNEW(MAXHES),GNEW(MAXPAR),                       3GL3092
     1                 PDUM(MAXPAR*MAXPAR),STEP(MAXPAR),SEUIL(2),       3GL3092 
     2                 ESTIM(3),PMIN(3),PMAX(3),EG(3),                  3GL3092 
     3                 EPS1,ERROR,SAVE,SAVE2,FCUR,FCUR2,V(MAXHES),      3GL3092
     4                 VEIG(MAXPAR),STI(MAXPAR),STIEG(MAXPAR),          3GL3092 
     5                 RDUMY(MAXPAR * (2*MAXPAR +  NCHAIN + 16 - 5) +   3GL3092
     6                       29 + NCHAIN - MAXHES - 20)                 3GL3092
C     COMMON /OPTIM/ IMP,IMP0,LEC,IPRT,HNEW(MAXHES),GNEW(MAXPAR) 
C    .              ,PDUM(MAXPAR*MAXPAR),STEP(MAXPAR),SEUIL(2) 
C    .              ,ESTIM(3),PMIN(3),PMAX(3),EG(3) 
C    .              ,EPS1,ERROR,SAVE,SAVE2,FCUR,FCUR2 
C    .              ,M,M2,IROUTE,ITYP,ICURV,IOPT,NOPT,LFINAL,LBIS(2) 
C    .              ,V(MAXHES),VEIG(MAXPAR),STI(MAXPAR),STIEG(MAXPAR) 
      COMMON /PRECI/ SCFCV,SCFTOL,EGDUM(9),KTYP(MAXPAR) 
C     COMMON /TIME / TIME0 
      COMMON /TIMECM / TIME0                                            3GL3092
      DIMENSION XNEW(N),GRAD(N),P(N,N) 
      LOGICAL LFINAL,BIS 
      LOGICAL LBIS, LDUMMY                                              3GL3092
      EQUIVALENCE (LBIS(1),BIS) 
       SAVE
C     T       : MINIMUM ALLOWED STEP 
C     X       : MAXIMUM ALLOWED STEP 
C     Y       : PREVIOUS STEP 
C     Z       : RELATIVE ERROR (OBSERVED) 
C     ZREF    : ZREF RELATIVE ERROR (WISHED) 
C     STEP CORRECTION FOR HESSIAN MATRIX EVALUATION ... 
      ADJUST(T,X,Y,Z,ZREF)=MAX(T,MIN(X,Y*MAX(0.20D0,1.2D0*Z/ZREF))) 
C 
C     ... 
C     ENERGY AND GRADIENT AT NEW CURRENT POINT XNEW 
C     ... 
      GG=DOT(GNEW,GNEW,N) 
      RMS=SQRT(GG/DBLE(N))                                              GCL0393
      LFINAL=ITERAT.GE.LIMIT .OR. RMS.LT.EPS 
      IF(LFINAL) IMP=MAX(IMP,3)                                         GCL0393
      IF(IMP.GT.0) THEN 
         CALL PORCPU (TFLY)                                             GL0492
         WRITE(JOUT,109) ITERAT,TFLY-TIME0 
         WRITE(JOUT,104) FNEW,GG,RMS,(GNEW(I),I=1,N) 
         WRITE(JOUT,100) XNEW 
      ENDIF 
C     ... 
C     SECOND DERIVATIVES (CORDE OR SECANT METHOD) 
C     ... 
      BIS=.FALSE. 
    1 ERROR=0.D0 
      IROUTE=1 
      M=0 
   10 M=M+1 
      IF(M.GT.N) GO TO 30 
      ITYP=KTYP(M) 
      SAVE=XNEW(M) 
   11 XNEW(M)=SAVE+STEP(M) 
      RETURN 
C     ( EVALUATION OF ENERGY AND GRADIENT AT POINT SAVE+STEP IF IOPT1=1 DJG0995
C        AND POINT SAVE-STEP IF IOPT1=2 )                               DJG0995
      ENTRY POINT2(P,XNEW,FNEW,N,EPS,ITERAT,LIMIT,GRAD) 
      IF(IOPT1.EQ.2) THEN                                               DJG0995
C        2-POINTS METHOD 
         DO 20 K=1,N 
   20    P(K,M)=(GRAD(K)-P(K,M))/(2.D0*STEP(M)) 
         SAUF=ABS(P(M,M))                                               GCL0393
         IF(SAUF.LT.5.D-2*ESTIM(ITYP)) GO TO 22 
         SAUF=EG(ITYP)/(STEP(M)*SAUF) 
         PAS=STEP(M) 
         IF(.NOT.BIS.AND.(SAUF.LT.0.1D0*SEUIL(2).OR.SAUF.GT.SEUIL(2))) 
     .   STEP(M)=ADJUST(PMIN(ITYP),STEP(M),PMAX(ITYP),SAUF,SEUIL(2)) 
      ELSE 
C        1-POINT METHOD 
         DO 21 K=1,N 
   21    P(K,M)=(GRAD(K)-GNEW(K))/STEP(M) 
         P(M,M)=-2.D0*(P(M,M)+3.D0*(GNEW(M)*STEP(M)+(FNEW-FCUR)) / 
     .                (STEP(M)**2)   ) 
         PDIAG=ABS(P(M,M))                                              GCL0393
         IF(PDIAG.LT.5.D-2*ESTIM(ITYP)) GO TO 22 
         SAUF=6.D0*(EG(ITYP)*STEP(M)+SCFCV)/(PDIAG*STEP(M)**2) 
         PAS=STEP(M) 
         IF(.NOT.BIS.AND.(SAUF.LT.0.1D0*SEUIL(1).OR.SAUF.GT.SEUIL(1))) 
     .      THEN 
            STEP(M)=3.D0*(EG(ITYP)+SQRT(EG(ITYP)**2+0.55D0*SEUIL(1) 
     .                 *SCFCV*PDIAG))/(0.8D0*SEUIL(1)*PDIAG) 
            STEP(M)=MAX(PMIN(ITYP),0.2D0*PAS,MIN(PMAX(ITYP),STEP(M))) 
         ENDIF 
      ENDIF 
C     HESSIAN STEP ADJUSTEMENT TO INSURE RELATIVE ERROR ON DIAGONAL 
      IF(PAS.NE.STEP(M)) GO TO 11 
      ERROR=MAX(ERROR,SAUF) 
   22 XNEW(M)=SAVE 
      GO TO 10 
   30 IF (N.GT.1) THEN 
C        PREPARE ERRORS COMPUTATION AND SYMMETRISE THE HESSIAN 
         DO 31 I=1,N 
         J=KTYP(I) 
   31    STIEG(I)=EG(J) 
         DO 32 I=1,N 
         STI(I)=STEP(I)**IOPT1                                          DJG0995
   32    STIEG(I)=STIEG(I)*STI(I) 
         DO 33 I=2,N 
         STII=STI(I) 
         I1=I-1 
CDIR$ IVDEP 
         DO 33 J=1,I1 
         STIJ=STII+STI(J) 
         P(J,I)=(P(I,J)*STII+P(J,I)*STI(J))/STIJ 
   33    P(I,J)=P(J,I) 
      ENDIF 
C     SAVE HESSIAN BEFORE DESTROYED BY DIAGONALIZATION ROUTINE 
      KI=0 
      DO 34 I=1,N 
      DO 34 J=1,I 
      KI=KI+1 
   34 HNEW(KI)=P(J,I) 
      IROUTE=2 
C     ... 
C     LOCAL CURVATURE INFORMATION 
C     ... 
C     DIAGONALIZE THE HESSIAN 
   40 CALL DIAGIV(P,N,N,GRAD,EPS1) 
C     COMPUTE THE INDEX (NUMBER OF NEGATIVE EIGENVALUES) 
      ICURV=0 
      DO 41 I=1,N 
      IF (GRAD(I).LE.-EPS1) ICURV=ICURV+1 
   41 CONTINUE 
   42 JCURV=MIN(N,ICURV+1)                                              GCL0393
      IF (ITERAT.GE.LIMIT) JCURV=N 
      IF(IMP.GT.0) THEN 
C        ... 
C        PRINTOUT ( PART 1/3 ) 
C        ... 
         WRITE(JOUT,102) ERROR 
         WRITE(JOUT,111) ICURV,EPS1,(GRAD(I),I=1,JCURV) 
      ENDIF 
C     ... 
C     STANDARD DEVIATION ON EIGENVALUES DUE TO GRADIENT ERRORS 
C     ... 
      DENER=SCFCV*DBLE(2-IOPT1)                                         DJG0995
      FACTOR=1.D0 
      IF (IOPT1.EQ.1) FACTOR=3.D0                                       DJG0995
      DO 44 I=1,N 
   44 VEIG(I)=FACTOR*(STIEG(I)+DENER/STEP(I))/STEP(I) 
      V(1)=VEIG(1) 
      FACTOR=DBLE(3-IOPT1)*2.D0                                         DJG0995
      IF (N.GT.1) THEN 
         K=1 
         DO 46 I=2,N 
         STIEGI=STIEG(I) 
         STPI=STEP(I) 
         STII=STI(I) 
         I1=I-1 
         DO 45 J=1,I1 
         K=K+1 
   45    V(K)=(STIEGI/STEP(J)+STIEG(J)/STPI)/(STII+STI(J)) 
         K=K+1 
   46    V(K)=VEIG(I) 
      ENDIF 
      DO 47 I=1,N 
   47 VEIG(I)=0.D0 
      K=0 
      DO 48 I=1,N 
      DO 48 J=1,I 
      K=K+1 
      VK2=V(K)*V(K) 
      DO 48 L=1,N 
   48 VEIG(L)=VEIG(L)+VK2*(P(I,L)*P(J,L))**2 
CDIR$ IVDEP 
      DO 49 L=1,N 
      VEIG(L)=FACTOR*SQRT(VEIG(L)) 
   49 V(L)=VEIG(L)/(ABS(GRAD(L))+EPS1)                                  GCL0393
C     PRINTOUT ( PART 2/3 ) 
      IF(IMP.GT.0) WRITE(JOUT,112) (VEIG(L),L=1,JCURV) 
C     ... 
C     PREPARE LARGER STEPS IF POOR ACCURACY ON EIGENVALUES 
C     ... 
      SAUF=0.D0 
      DO 50 I=1,N 
      VEIG(I)=STEP(I) 
   50 SAUF=MAX(SAUF,V(I)) 
      IF(BIS .OR. SAUF.LT.2.5D0*SEUIL(IOPT1)) GO TO 60                  DJG0995
      DO 51 I=1,N 
      STEP(I)=MIN(PMAX(KTYP(I)),STEP(I)*SAUF/SEUIL(IOPT1))              DJG0995
   51 BIS=BIS .OR. STEP(I).GT.VEIG(I) 
C     RESTART IF REQUIRED (LAST ITERATION ONLY) 
      IF(LFINAL.AND.BIS) GO TO 1 
C     ... 
C     PRINTOUT ( PART 3/3 ) 
C     ... 
   60 IF(IMP.LT.2) GO TO 70 
      KI=MIN(N,4)                                                       GCL0393
      WRITE(JOUT,101)(I,I=1,KI) 
      KI=0 
      DO 61 I=1,N 
      IK=KI+1 
      KI=KI+I 
   61 WRITE(JOUT,103) I,VEIG(I),(HNEW(J),J=IK,KI) 
      IF(IMP.LT.3) GO TO 70 
      WRITE(JOUT,106) 
      DO 62 I=1,JCURV 
   62 WRITE(JOUT,107) I,(P(J,I),J=1,N) 
C     ... 
   70 RETURN 
C     ... 
  100 FORMAT(' COORD',1P,6D12.4/(6X,6D12.4)) 
  101 FORMAT(/' MATRIX OF SECOND DERIVATIVES'/7X,'STEP',2X, 
     1 4(I6,6X),I6,' .....') 
  102 FORMAT(/' HESSIAN MATRIX : DIAGONAL TERM ERROR <',2PF9.3,' %') 
  103 FORMAT(1X,I3,1PD9.1,5D12.4/(13X,5D12.4)) 
  104 FORMAT(' ENERGY',1PD15.8,5X,'<G!G>',1PD15.8,5X 
     1 ,'RMS GRAD',D13.5/ 
     2 ' GRAD ',6D12.4/(6X,6D12.4)) 
  106 FORMAT(/' AND EIGENVECTORS :') 
  107 FORMAT(1X,I3,1P,7D10.2/(4X,7D10.2)) 
  109 FORMAT('1ITER',I4,5X,'ELAPSED TIME :',F8.2,' SECOND') 
  111 FORMAT(' NUMBER OF NEGATIVES EIGENVALUES',I5, 
     1 ' PRECISION:',1PD10.2/ 
     2 ' FIRST EIGENVALUES :',1P,6D10.2/(8D10.2) ) 
  112 FORMAT(' STANDARD DEVIATION:', 1P,6D10.2/(8D10.2)) 
      END 
