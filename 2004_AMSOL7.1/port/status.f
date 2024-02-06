      SUBROUTINE STATUS(XNEW,AK,N) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
C     LTRD METHOD         POLYNOMIAL EXTRAPOLATION PART 
C     ENERGY AND GRADIENT (INTERPOLATED AND OBSERVED) 
C     AT POINT (CURENT) = (XNEW) + AK * (S) 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / IPOLYN(10),IROUTE,NSOL,NPOINT,NPTMAX,            3GL3092 
     1                 IESNO,NOIES, IDUMMY(19 + 2*NCHAIN - 16)          3GL3092
      COMMON /OPTMCL / INFLEX,IMPROV,FINAL,LOWER, LDUMMY(NCHAIN)        GCL0892
      COMMON /OPTMCR / HNEW(MAXHES),GNEW(MAXPAR),                       3GL3092
     1                 G(MAXPAR,10),G0(MAXPAR),G1(MAXPAR),G2(MAXPAR),   3GL3092 
     2                 CURENT(MAXPAR),X(10),EI(10),GGI(10),E(10),       3GL3092 
     3                 GG(10),GR(5),ER(4),SOL(3),VAL(3),BIDON(3,3),     3GL3092 
     4                 RDUMY(MAXPAR*(3*MAXPAR + NCHAIN + 1) +           3GL3092
     5                       29 + NCHAIN - 74)                          3GL3092
C     COMMON /OPTIM/ IMP,IMP0,LEC,IPRT,HNEW(MAXHES),GNEW(MAXPAR) 
C    .              ,G(MAXPAR,10),G0(MAXPAR),G1(MAXPAR),G2(MAXPAR) 
C    .              ,CURENT(MAXPAR),X(10),EI(10),GGI(10),E(10) 
C    .              ,GG(10),GR(5),ER(4),SOL(3),VAL(3),BIDON(3,3) 
C    .              ,IPOLYN(10),INFLEX,IROUTE,NSOL,IMPROV,NPOINT,NPTMAX 
C    .              ,IESNO,NOIES,FINAL,LOWER 
      DIMENSION XNEW(1),S(1) 
      LOGICAL INFLEX,IMPROV,FINAL,LOWER 
      LOGICAL LDUMMY                                                    3GL3092
      EQUIVALENCE(HNEW(1),S(1)) 
       SAVE
      DATA XEPSI/1.D-5/ 
      NPMIN=NPOINT-1 
      IF (LOWER) GO TO 1 
      IF((IROUTE.EQ.3.AND.E(NPMIN).LT.E(1)).OR. 
     .   (IROUTE.NE.3.AND.GG(NPMIN).LT.GG(1))) LOWER=.TRUE. 
    1 AK=MAX(AK,10.D0**(-NPMIN)) 
      X  (NPOINT)=AK 
      EI(NPOINT)=((ER(4)*AK+ER(3))*AK+ER(2))*AK+ER(1) 
      GGI(NPOINT)=(((GR(5)*AK+GR(4))*AK+GR(3))*AK+GR(2))*AK+GR(1) 
      IF (NPOINT.GE.10) GO TO 10 
      IF (.NOT.LOWER) GO TO 20 
      IF (NPOINT.GT.NPTMAX) GO TO 10 
      DO 2 I=1,NPMIN 
      IF(ABS(AK-X  (I)).LT.XEPSI*10.D0**(NPOINT/2)) GO TO 10            GCL0393
    2 CONTINUE 
   20 IMPROV=.TRUE. 
      DO 3 I=1,N 
    3 CURENT(I)=XNEW(I)+AK*S(I) 
C     AT THIS POINT A CALL TO 'COMPFG' IS MADE IN MAIN ROUTINE 'STAT' 
      RETURN 
   10 IMPROV=.FALSE. 
      RETURN 
      END 
