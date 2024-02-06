      SUBROUTINE STAT1(AK,N,XNEW,FNEW,EPS) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i' 
C 
C     IROUTE = 1 
C     OPTIMAL SEARCH ALONG THE OPTIMIZED S DIRECTION 
C     NEGATIVE STEP ARE NOT ALLOWED 
C 
C     IROUTE = 2 
C     OPTIMAL SEARCH ALONG THE S DIRECTION DEFINED BY NEWTON METHOD 
C     TRY TO AVOID DISAPOINTING STATIONARY POINT 
C     NEGATIVE STEP ARE ALLOWED 
C 
C     IROUTE = 3 
C     OPTIMAL SEARCH ALONG THE S DIRECTION DEFINED BY NEWTON METHOD 
C     ASSUME A BETTER ENERGY ( IT MUST GOES DOWN) 
C     NEGATIVE STEP ARE NOT ALLOWED 
C 
C     UP TO THREE DEGREE POLYNOMIAL INTERPOLATION IN ALL CASES 
C 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / IPOLYN(10),IROUTE,NSOL,NPOINT,NPTMAX,            3GL3092 
     1                 IESNO,NOIES, IDUMMY(19 + 2*NCHAIN - 16)          3GL3092
      COMMON /OPTMCL / INFLEX,IMPROV,FINAL,LOWER, LDUMMY(NCHAIN)        GCL0892
      COMMON /OPTMCR / HNEW(MAXHES),GNEW(MAXPAR),                       3GL3092 
     1                 G(MAXPAR,10),G0(MAXPAR),G1(MAXPAR),G2(MAXPAR),   3GL3092 
     2                 CURENT(MAXPAR),X(10),EI(10),GGI(10),E(10),       3GL3092 
     3                 GG(10),GR(5),ER(4),SOL(3),VAL(3),A(3,3),         3GL3092 
     4                 RDUMY(MAXPAR*(3*MAXPAR + NCHAIN + 1) +           3GL3092
     5                       29 + NCHAIN - 74)                          3GL3092
      COMMON/SCFOK/ FAIL 
      DIMENSION XNEW(1),GV(MAXPAR,3),S(1) 
      EQUIVALENCE (GV(1,1),G0(1)) , (S(1),HNEW(1)) 
      LOGICAL INFLEX,IMPROV,FAIL,FINAL,LOWER 
      LOGICAL LDUMMY                                                    3GL3092
      CHARACTER*4 NO, IES, INT, IESNOC, NOIESC                          GCL0393
       SAVE
      DATA NO,IES,INT /' NO ',' YES',' ?? '/                            GCL0393
C 
      LOWER=.FALSE. 
      FINAL=.FALSE. 
      NPTMAX=MAX(5,MIN(N-2,10))                                         GCL0393
C     INITIAL POINT 
      X(1)=0.D0 
      E(1)=FNEW 
      GG(1)=DOT(GNEW,GNEW,N) 
      ER(1)=FNEW 
      CALL SCOPY (N,GNEW,1,G(1,1),1) 
      NOIESC=INT                                                        GCL0393
      IESNOC=NO                                                         GCL0393
      IF(INFLEX) IESNOC=IES                                             GCL0393
C 
C     FIRST TRIAL POINT 
      NPOINT=2 
      CALL STATUS (XNEW,AK,N) 
      IF(.NOT.IMPROV) GO TO 40 
      RETURN 
      ENTRY STAT2(AK,N,XNEW,FNEW,EPS) 
      IF(FAIL) GO TO 50 
      GG(NPOINT)=DOT(G(1,NPOINT),G(1,NPOINT),N) 
C 
C     SECOND TRIAL POINT 
      NPOINT=3 
      X2=X(2)**2 
      DO 20 I=1,N 
   20 G2(I)=(G(I,2)-G0(I)-X(2)*G1(I))/X2 
      CALL CARDAN(GV,S,GR,ER,N,SOL,VAL,NSOL,IPOLYN(3)) 
C Former SELECT routine : renamed to avoid clash with the libc C library IR1294
      CALL AMSEL (IESNO,NOIESC,NPOINT,N,AK)                              IR1294
C      CALL SELECT (IESNO,NOIESC,NPOINT,N,AK)                            GCL0393
      CALL STATUS (XNEW,AK,N) 
      IF(.NOT.IMPROV) GO TO 40 
      RETURN 
      ENTRY STAT3(AK,N,XNEW,FNEW,EPS) 
      IF(FAIL) GO TO 50 
      GG(NPOINT)=DOT(G(1,NPOINT),G(1,NPOINT),N) 
C 
C     OTHERS TRIAL POINTS 
      NPOINT=NPOINT+1 
      A(3,3)= 1.D0/((X(NPOINT-1)-X(NPOINT-3))*(X(NPOINT-2)-X(NPOINT-3))) 
      A(3,2)=-1.D0/((X(NPOINT-1)-X(NPOINT-2))*(X(NPOINT-2)-X(NPOINT-3))) 
      A(3,1)= 1.D0/((X(NPOINT-1)-X(NPOINT-2))*(X(NPOINT-1)-X(NPOINT-3))) 
      A(2,3)=-A(3,3)*(X(NPOINT-1)+X(NPOINT-2)) 
      A(2,2)=-A(3,2)*(X(NPOINT-3)+X(NPOINT-1)) 
      A(2,1)=-A(3,1)*(X(NPOINT-2)+X(NPOINT-3)) 
      A(1,3)=A(3,3)*X(NPOINT-2)*X(NPOINT-1) 
      A(1,2)=A(3,2)*X(NPOINT-1)*X(NPOINT-3) 
      A(1,1)=A(3,1)*X(NPOINT-3)*X(NPOINT-2) 
      DO 30 I=1,3 
      DO 31 K=1,N 
   31 GV(K,I)=0.D0 
      DO 30 J=1,3 
      AIJ=A(I,J) 
CDIR$ IVDEP 
      DO 30 K=1,N 
   30 GV(K,I)=GV(K,I)+AIJ*G(K,NPOINT-J) 
      CALL CARDAN(GV,S,GR,ER,N,SOL,VAL,NSOL,IPOLYN(NPOINT)) 
      ER(1)=E(NPOINT-1)-((ER(4)*X(NPOINT-1)+ER(3))*X(NPOINT-1)+ER(2)) 
     1  *X(NPOINT-1) 
C Former SELECT routine : renamed to avoid clash with the libc C library IR1294
      CALL AMSEL(IESNO,NOIESC,NPOINT,N,AK)                               IR1294
C      CALL SELECT(IESNO,NOIESC,NPOINT,N,AK)                             GCL0393
      CALL STATUS (XNEW,AK,N) 
      IF(IMPROV) RETURN 
C 
C     FINAL STEP AND EDITION 
   40 FINAL=.TRUE. 
      IF(IMP.EQ.0) GO TO 47 
      WRITE(JOUT,105) IESNOC,NOIESC                                     GCL0393
      WRITE(JOUT,106) 
      WRITE(JOUT,102) X(1),E(1),GG(1) 
      IF(NPOINT.EQ.2) GO TO 44 
      NPO=NPOINT-1 
      DO 41 I=2,NPO 
   41 WRITE(JOUT,103) I,X(I),IPOLYN(I),GGI(I),EI(I),E(I),GG(I) 
   44 WRITE(JOUT,104)NPOINT,X(NPOINT),IPOLYN(NPOINT),GGI(NPOINT), 
     *EI(NPOINT) 
      WRITE(JOUT,107) 
C     ESTABLISH NEW CURRENT POINT XNEW 
   47 AK=E(2) 
      IF (IROUTE.NE.3) AK=GG(2) 
      J=2 
      IF (NPOINT.LT.4) GO TO 45 
      DO 43 I=3,NPO 
      IF (IROUTE.EQ.3) GO TO 46 
      IF (GG(I).GT.AK) GO TO 43 
      J=I 
      AK=GG(I) 
      GO TO 43 
   46 IF (E(I).GT.AK) GO TO 43 
      J=I 
      AK=E(I) 
   43 CONTINUE 
   45 AK=X(J) 
      DO 42 I=1,N 
      GNEW(I)=G(I,J) 
   42 XNEW(I)=XNEW(I)+AK*S(I) 
      FNEW=E(J) 
      RETURN 
C 
C     ABNORMAL TERMINATION : SCF DIVERGENCE 
   50 WRITE(JOUT,100) NPOINT 
      NPOINT=NPOINT-1 
      IF(NPOINT.GT.1) GO TO 40 
      WRITE(JOUT,108) 
          ISTOP=1                                                       GDH1095
          IWHERE=155                                                    GDH1095
          RETURN                                                        GDH1095
  100 FORMAT(' ***WARNING***  DIVERGENCE AT STEP',I3,' OF POLYNOMIAL 
     1INTERPOLATION') 
  102 FORMAT(' !   1 !',F8.4,'!',33X,'!',F15.9,1PD13.4,'!') 
  103 FORMAT(' !',I4,' !',F8.4,'!',I3,1PD14.4,0PF16.9,'!',F15.9,1PD13.4, 
     1 '!') 
  104 FORMAT(' !',I4,' !',F8.4,'!',I3,1PD14.4,0PF16.9,'!',28X,'!') 
  105 FORMAT(' ASK FOR <G!G> AFTER MAX:',A4,5X,'OBSERVED:',A4) 
  106 FORMAT (/1H ,79(1H-)/ 
     2 ' !POINT!  STEP  !',11X,'INTERPOLATED',10X,'!',11X,'OBSERVED',9X, 
     4 '!'/ 
     5' !',5X,'!',8X,'!DEGREE',4X,'<G!G>',8X,'ENERGY',4X,'!',5X,'ENERGY' 
     6 ,9X,'<G!G>',3X,'!') 
  107 FORMAT(1H ,79(1H-)) 
  108 FORMAT(' IMPERATIVE STOP') 
      END 
