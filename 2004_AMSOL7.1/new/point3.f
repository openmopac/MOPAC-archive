      SUBROUTINE POINT3(P,XNEW,FNEW,N,EPS,ITERAT,LIMIT,GRAD) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C     ----- NUMERICAL-NUMERICAL METHODS ----- 
C     ENERGY,GRADIENT AND SECOND DERIVATIVES AT CURRENT POINT XNEW , 
C     CONVERGENCE ON RMS GRADIENT : SQRT(<G!G>/N), 
C     LOCAL CURVATURE INFORMATION . 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / M1,M2,IROUTE,ITYP,ICURV,IOPT1,NOPT,              DJG0995
     1                 IDUMMY(19 + 2*NCHAIN - 7)                        3GL3092
      COMMON /OPTMCL / LFINAL,LBIS(2), LDUMMY(NCHAIN + 1)               GCL0892
      COMMON /OPTMCR / HNEW(MAXHES),GNEW(MAXPAR),                       3GL3092
     1                 PDUM(MAXPAR*MAXPAR),STEP(MAXPAR),SEUIL(2),       3GL3092 
     2                 ESTIM(3),PMIN(3),PMAX(3),EG(3),                  3GL3092 
     3                 EPS1,ERROR,SAVE1,SAVE2,FCUR1,FCUR2,              3GL3092 
     4                 V(MAXPAR),VEIG(MAXPAR),                          3GL3092 
     5                 RDUMY(MAXPAR * (2*MAXPAR + NCHAIN + 12) +        3GL3092
     6                       29 + NCHAIN - 20)                          3GL3092
C     COMMON /OPTIM/ IMP,IMP0,LEC,IPRT,HNEW(MAXHES),GNEW(MAXPAR) 
C    .              ,PDUM(MAXPAR*MAXPAR),STEP(MAXPAR),SEUIL(2) 
C    .              ,ESTIM(3),PMIN(3),PMAX(3),EG(3) 
C    .              ,EPS1,ERROR,SAVE1,SAVE2,FCUR1,FCUR2 
C    .              ,M1,M2,IROUTE,ITYP,ICURV,IOPT,NOPT,LFINAL,LBIS(2) 
C    .              ,V(MAXPAR),VEIG(MAXPAR) 
C     COMMON /TIME / TIME0 
      COMMON /TIMECM / TIME0                                            3GL3092
      COMMON /PRECI/ SCFCV,SCFTOL,EGDUM(9),KTYP(MAXPAR) 
      DIMENSION XNEW(N),GRAD(N),P(N,N) 
      LOGICAL LFINAL,BIS 
      LOGICAL LBIS, LDUMMY                                              3GL3092
      EQUIVALENCE (LBIS(1),BIS) 
       SAVE
C     T       : MINIMUM ALLOWED STEP 
C     X       : MAXIMUM ALLOWED STEP 
C     Y       : PREVIOUS STEP 
C     Z       : RELATIVE ERROR (OBSERVED) 
C     ZREF    : RELATIVE ERROR (WISHED) 
C     STEP CORRECTION FOR HESSIAN MATRIX EVALUATION ... 
      ADJUST(T,X,Y,Z,ZREF)= 
     . MAX(T,MIN(X,Y*SQRT(MAX(0.01D0,1.2D0*Z/ZREF)))) 
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
C     SECOND DERIVATIVES (CORDE OR SECANT METHOD)-DIAGONAL TERMS 
C     ... 
      BIS=.FALSE. 
   10 ERROR=0.D0 
      IROUTE=1 
      M1=0 
   11 M1=M1+1 
      IF(M1.GT.N) GO TO 20 
      ITYP=KTYP(M1) 
      SAVE1=XNEW(M1) 
   12 RETURN 
C     ( EVALUATION OF ENERGY AT POINT SAVE+STEP 
C        AND POINT SAVE-STEP ) 
      ENTRY POINT4(P,XNEW,FNEW,N,EPS,ITERAT,LIMIT,GRAD) 
      P(M1,M1)=(FCUR1+FCUR2-2.D0*FNEW)/STEP(M1)**2 
      SAUF=ABS(P(M1,M1))                                                GCL0393
      IF (SAUF.LT.5.D-2*ESTIM(ITYP)) GO TO 13 
      SAUF=SCFCV*2.D0/(SAUF*STEP(M1)**2) 
C     HESSIAN STEP ADJUSTEMENT TO INSURE RELATIVE ERROR ON DIAGONAL 
      PAS=STEP(M1) 
      IF(.NOT.BIS.AND.(SAUF.LT..1D0*SEUIL(IOPT1).OR.SAUF.GT.SEUIL(IOPT1)DJG0995
     .)) STEP(M1)=ADJUST(PMIN(ITYP),PAS,PMAX(ITYP),SAUF,SEUIL(IOPT1))   DJG0995
      IF(PAS.NE.STEP(M1)) GO TO 12 
      ERROR=MAX(ERROR,SAUF) 
   13 GRAD(M1)=FCUR1 
      XNEW(M1)=SAVE1 
      GO TO 11 
C     ... 
C     OFF-DIAGONAL TERMS (CORDE OR SECANT METHOD) 
C     ... 
   20 IROUTE=2 
      IF(IOPT1.EQ.2) THEN                                               DJG0995
         DO 21 I=1,N 
   21    GRAD(I)=P(I,I)*STEP(I)**2 
      ENDIF 
      M1=1 
   22 M1=M1+1 
      IF(M1.GT.N) GO TO 30 
      SAVE1=XNEW(M1) 
      M2=0 
   23 M2=M2+1 
      IF(M2.GE.M1) GO TO 24 
      SAVE2=XNEW(M2) 
      RETURN 
C     ( EVALUATION OF ENERGY AT POINT SAVE+STEP AND POINT SAVE-STEP 
C     IF IOPT1=2 )                                                      DJG0995
      ENTRY POINT5 (P,XNEW,FNEW,N,EPS,ITERAT,LIMIT,GRAD) 
      IF(IOPT1.EQ.1) THEN                                               DJG0995
         P(M2,M1)=(FCUR1+FNEW-GRAD(M1)-GRAD(M2))/(STEP(M1)*STEP(M2)) 
      ELSE 
         P(M2,M1)=(FCUR1+FCUR2-FNEW*2.D0-GRAD(M1)-GRAD(M2)) 
     .            /(STEP(M1)*STEP(M2)*2.D0) 
      ENDIF 
      XNEW(M2)=SAVE2 
      GO TO 23 
   24 XNEW(M1)=SAVE1 
      GO TO 22 
C     ... 
C     SAVE THE HESSIAN BEFORE DESTROYED BY DIAGONALIZATION ROUTINE 
C     ... 
   30 KI=0 
      DO 31 I=1,N 
CDIR$ IVDEP 
      DO 31 J=1,I 
      KI=KI+1 
      P(I,J)=P(J,I) 
   31 HNEW(KI)=P(J,I) 
      IROUTE=3 
C     ... 
C     LOCAL CURVATURE INFORMATION 
C     ... 
C     DIAGONALIZATION 
      CALL DIAGIV(P,N,N,GRAD,EPS1) 
C     HESSIAN' INDEX (NUMBER OF NEGATIVE EIGENVALUES) 
      ICURV=0 
      DO 32 I=1,N 
      IF (GRAD(I).LT.-EPS1) ICURV=ICURV+1 
   32 CONTINUE 
      JCURV=MIN(N,ICURV+1)                                              GCL0393
      IF (ITERAT.GE.LIMIT) JCURV=N 
      IF(IMP.GT.0) THEN 
C        ... 
C        PRINTOUT ( PART 1/3 ) EIGENVALUES 
C        ... 
         WRITE(JOUT,102) ERROR 
         WRITE(JOUT,111) ICURV,EPS1,(GRAD(I),I=1,JCURV) 
      ENDIF 
C     ... 
C     STANDARD DEVIATION ON EIGENVALUES DUE TO ENERGIES ERRORS 
C     ... 
      DO 41 I=1,N 
      D1=0.D0 
      D2=0.D0 
      D3=0.D0 
      D4=0.D0 
      DO 40 J=1,N 
      SAUF=P(J,I)/STEP(J) 
      SAUF2=SAUF*SAUF 
      D1=D1+SAUF 
      D2=D2+SAUF2 
      D3=D3+SAUF*SAUF2 
   40 D4=D4+SAUF2*SAUF2 
      IF(IOPT1.EQ.1) THEN                                               DJG0995
         VEIG(I)=D4*4.00D0-D1*D3*4.00D0+D2*D2*2.00D0 
      ELSE 
         VEIG(I)=D4*7.50D0-D1*D3*8.00D0+D2*D2*2.25D0 
      ENDIF 
   41 CONTINUE 
CDIR$ IVDEP 
      DO 42 L=1,N 
      VEIG(L)=SCFCV*SQRT(ABS(VEIG(L)))                                  GCL0393
   42 V(L)=VEIG(L)/(ABS(GRAD(L))+EPS1)                                  GCL0393
C     PRINTOUT ( PART 2/3 ) STANDARD DEVIATION 
      IF(IMP.GT.0) WRITE(JOUT,112) (VEIG(L),L=1,JCURV) 
C     ... 
C     PREPARE LARGER STEPS IF POOR ACCURACY ON EIGENVALUES 
C     ... 
      SAUF=0.D0 
      DO 43 I=1,N 
      VEIG(I)=STEP(I) 
   43 SAUF=MAX(SAUF,V(I)) 
      IF(BIS .OR. SAUF.LT.2.5D0*SEUIL(IOPT1)) GO TO 50                  DJG0995
      DO 44 I=1,N 
      STEP(I)=MIN(PMAX(KTYP(I)),STEP(I)*SQRT(SAUF/SEUIL(IOPT1)))        DJG0995
   44 BIS=BIS .OR. STEP(I).GT.VEIG(I) 
C     RESTART IF REQUIRED (LAST ITERATION ONLY) 
      IF (LFINAL.AND.BIS) GO TO 10 
C     ... 
C     PRINTOUT ( PART 3/3 ) HESSIAN MATRIX AND EIGENVECTORS 
C     ... 
   50 IF(IMP.LT.2) GO TO 60 
      KI=MIN(N,6)                                                       GCL0393
      WRITE(JOUT,101)(I,I=1,KI) 
      KI=0 
      DO 51 I=1,N 
      IK=KI+1 
      KI=KI+I 
   51 WRITE(JOUT,103) I,VEIG(I),(HNEW(J),J=IK,KI) 
      IF(IMP.LT.3) GO TO 60 
      WRITE(JOUT,106) 
      DO 52 I=1,JCURV 
   52 WRITE(JOUT,107) I,(P(J,I),J=1,N) 
   60 RETURN 
  100 FORMAT(' COORD',1P,6D12.4/(6X,6D12.4)) 
  101 FORMAT(/' MATRIX OF SECOND DERIVATIVES'/7X,'STEP',2X, 
     * 5(I6,6X),I6,' .....') 
  102 FORMAT(/' HESSIAN MATRIX : DIAGONAL TERM ERROR <',2PF9.3,' %') 
  103 FORMAT(1X,I3,1PD9.1,5D12.4/(13X,5D12.4)) 
  104 FORMAT(' ENERGY',1PD15.8,5X,'<G!G>',1PD15.8,5X 
     1 ,'RMS GRAD',D13.5/ 
     2 ' GRAD ',6D12.4/(6X,6D12.4)) 
  106 FORMAT(/' AND EIGENVECTORS :') 
  107 FORMAT(1X,I3,1P,7D10.2/(4X,7D10.2)) 
  109 FORMAT('1ITER',I4,5X,'ELAPSED TIME :',F8.2,' SECOND') 
  111 FORMAT('NUMBER OF NEGATIVES EIGENVALUES',I5, 
     1 ' PRECISION:',1PD10.2/ 
     2 ' FIRST EIGENVALUES :',1P,6D10.2/(8D10.2) ) 
  112 FORMAT(' STANDARD DEVIATION:', 1P,6D10.2/(8D10.2)) 
      END 
