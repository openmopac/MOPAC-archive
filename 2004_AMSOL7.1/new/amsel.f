      SUBROUTINE AMSEL (IESNO,NOIES,NPOINT,N,AK)                        IR1294
C Former SELECT routine : renamed to avoid clash with the libc C libraryIR1294
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
C     LTRD METHOD ... 
C     SELECT THE NEXT TRIAL STEP OF POLYNOMIAL INTERPOLATION 
C     IROUTE=2  STATIONNARY POINT RESEARCH BY NEWTON METHOD 
C     IROUTE=3  MINIMIZATION BY NEWTON METHOD 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / IPOLYN(10),IROUTE,NSOL,ICURV(1),                 3GL3092
     1                 IDUMMY(19 + 2*NCHAIN - 13)                       3GL3092
      COMMON /OPTMCL / INFLEX, IMPROV, LDUMMY(NCHAIN + 2)               GCL0892
      COMMON /OPTMCR / HNEW(MAXHES),GNEW(MAXPAR),                       3GL3092
     1                 G(MAXPAR,10),G0(MAXPAR),G1(MAXPAR),G2(MAXPAR),   3GL3092 
     2                 CURENT(MAXPAR),X(10),EI(10),GGI(10),E(10),       3GL3092 
     3                 GG(10),GR(5),ER(4),SOL(3),VAL(3),D(3,3),         3GL3092 
     4                 A(MAXPAR),O(2), B(3),C(3),F(3),                  3GL3092
     5                 RDUMY(MAXPAR*(3*MAXPAR+NCHAIN)+NCHAIN+29-85)     3GL3092
C     COMMON /OPTIM/ IMP,IMP0,LEC,IPRT,HNEW(MAXHES),GNEW(MAXPAR) 
C    .              ,G(MAXPAR,10),G0(MAXPAR),G1(MAXPAR),G2(MAXPAR) 
C    .              ,CURENT(MAXPAR),X(10),EI(10),GGI(10),E(10) 
C    .              ,GG(10),GR(5),ER(4),SOL(3),VAL(3),D(3,3) 
C    .              ,IPOLYN(10),INFLEX,IROUTE,NSOL,IMPROV,A(MAXPAR),O(2) 
C    .              ,B(3),C(3),F(3),ICURV(1) 
      DIMENSION S(1) 
      EQUIVALENCE (HNEW(1),S(1)) 
      LOGICAL INFLEX,IMPROV 
      LOGICAL LDUMMY                                                    3GL3092
      CHARACTER*4 NO, IES, NOIES                                        GCL0393
       SAVE
      DATA NO,IES /' NO ',' YES'/                                       GCL0393
      GO TO (10,10,30),IROUTE 
   10 NOIES=NO 
      IF(SOL(1).GT.0.D0) GO TO 11 
C 
C     LONG RANGE INVESTIGATION 
      AK=2.D0*X(NPOINT-1) 
      IF(INFLEX) RETURN 
C 
C     RESEARCH OF A MINIMUM OF EUCLIDIAN NORM OF GRADIENT IN A 
C       SECOND ORDER TAYLOR EXPANSION 
      NRANG=3 
      IF(NPOINT.EQ.3) NRANG=2 
      DO 1 I=1,NRANG 
      F(I)=SQRT(GG(NPOINT-I)) 
    1 B(I)=X(NPOINT-I) 
      IF(NRANG.EQ.2) GO TO 5 
C     SOLVE THE VAN DER MONDE EQUATIONS 
      D(1,1)= 1.D0/((B(3)-B(1))*(B(2)-B(1))) 
      D(2,1)=-1.D0/((B(2)-B(1))*(B(3)-B(2))) 
      D(3,1)= 1.D0/((B(3)-B(1))*(B(3)-B(2))) 
      D(1,2)=-D(1,1)*(B(2)+B(3)) 
      D(2,2)=-D(2,1)*(B(3)+B(1)) 
      D(3,2)=-D(3,1)*(B(1)+B(2)) 
      D(1,3)=D(1,1)*B(3)*B(2) 
      D(2,3)=D(2,1)*B(1)*B(3) 
      D(3,3)=D(3,1)*B(2)*B(1) 
      DO 2 I=1,3 
    2 C(I)=DOT(D(1,I),F,3) 
      IF(ABS(C(1)).LT.ABS(C(2))*1.D-3) GO TO 5                          GCL0393
      IF(C(1).GT.0.D0) GO TO 3 
             AK=MIN(5.D0*B(1) , 
     1 -( C(2)+SQRT(C(2)**2-4.D0*C(1)*C(3)))/(2.D0*C(1))       ) 
      RETURN 
    3        AK=MIN(-C(2)/(2.D0*C(1)),5.D0*B(1)) 
      IF(AK.GT.0.D0) RETURN 
    4        AK=B(1)/2.D0 
      RETURN 
    5 IF(F(1).GE.F(2)) GO TO 4 
             AK=MIN((B(1)*F(2)-B(2)*F(1))/(F(2)-F(1)),5.D0*B(1)) 
      RETURN 
C 
C     SELECT ONE OF THE CARDAN SOLUTIONS 
   11 IF(NSOL.EQ.3.AND.SOL(3).GT.0.D0) GO TO 12 
             AK=SOL(1) 
      IF(NSOL.EQ.3.AND.SOL(2).GT.0.D0) NOIES=IES 
      RETURN 
   12 IF(INFLEX) GO TO 13 
             AK=SOL(3) 
      RETURN 
   13        AK=SOL(1) 
      NOIES=IES 
      RETURN 
C 
C     RESEARCH OF A MINIMUM OF ENERGY IN A THIRD ORDER TAYLOR EXPANSION 
C     THIS FORMALISM IS AVAILABLE IF THE SLOPE AT ORIGIN IS NEGATIVE 
C     AND ONLY POSITIVE STEP ARE ALLOWED 
   30 NOIES=NO 
      ISOL=IPOLYN(NPOINT) 
      GO TO (35,32,31),ISOL 
   31 DELTA=ER(3)**2-3.D0*ER(2)*ER(4) 
      IF(DELTA.LT.0.D0) GO TO 35 
      AK=(SQRT(DELTA)-ER(3))/(3.D0*ER(4)) 
      GO TO 33 
   32 AK=-ER(2)/(2.D0*ER(3)) 
   33 IF(AK.GT.0.D0) GO TO 34 
      IF(E(NPOINT-1).LT.E(1)) GO TO 35 
      AK=X(NPOINT-1)/3.D0 
   34 IF(NSOL.EQ.3.AND.SOL(2).GT.0.D0.AND.SOL(2).LE.AK) NOIES=IES 
      RETURN 
   35 AK=X(NPOINT-1)*2.D0 
      GO TO 34 
      END 
