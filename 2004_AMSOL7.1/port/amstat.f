      SUBROUTINE AMSTAT(AK,N,XNEW,FNEW,EPS)                             IR1294
C Former STAT routine : renamed to avoid clash with the libc C library  IR1294
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
C..... 
C     LTRD METHOD      MAIN ROUTINE FOR POLYNOMIAL EXTRAPOLATION, 
C                      MUST NOT BE OVERLAYED WITH 'COMPFG'. 
C..... 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / IPOLYN(10), IROUTE, NSOL, NPOINT, NPTMAX,        3GL3092
     1                 IESNO, NOIES, IDUMMY(19 + 2*NCHAIN - 16)         3GL3092
      COMMON /OPTMCL / INFLEX,IMPROV,FINAL, LDUMMY(NCHAIN + 1)          GCL0892
      COMMON /OPTMCR / HNEW(MAXHES),GNEW(MAXPAR),                       3GL3092
     1                 G(MAXPAR,10),G0(MAXPAR),G1(MAXPAR),G2(MAXPAR),   3GL3092 
     2                 CURENT(MAXPAR),X(10),EI(10),GGI(10),E(10),       3GL3092 
     3                 GG(10),GR(5),ER(4),SOL(3),VAL(3),A(3,3),         3GL3092 
     4                 RDUMY(MAXPAR*(3*MAXPAR + NCHAIN + 1) +           3GL3092
     5                       29 + NCHAIN - 74)                          3GL3092
C     COMMON /OPTIM/ IMP,IMP0,LEC,IPRT,HNEW(MAXHES),GNEW(MAXPAR) 
C    .              ,G(MAXPAR,10),G0(MAXPAR),G1(MAXPAR),G2(MAXPAR) 
C    .              ,CURENT(MAXPAR),X(10),EI(10),GGI(10),E(10) 
C    .              ,GG(10),GR(5),ER(4),SOL(3),VAL(3),A(3,3) 
C    .              ,IPOLYN(10),INFLEX,IROUTE,NSOL,IMPROV,NPOINT,NPTMAX 
C    .              ,IESNO,NOIES,FINAL 
      DIMENSION XNEW(1),S(1) 
      EQUIVALENCE(S(1),HNEW(1)) 
      LOGICAL INFLEX,IMPROV,FINAL 
      LOGICAL LDUMMY, FAIL                                              GCL0393
       SAVE
      CALL STAT1 (AK,N,XNEW,FNEW,EPS) 
      IF(FINAL) RETURN 
      CALL COMPFG (CURENT,E(NPOINT),FAIL,G(1,NPOINT),.TRUE.) 
      CALL STAT2 (AK,N,XNEW,FNEW,EPS) 
    1 IF(FINAL) RETURN 
      CALL COMPFG (CURENT,E(NPOINT),FAIL,G(1,NPOINT),.TRUE.) 
      CALL STAT3 (AK,N,XNEW,FNEW,EPS) 
      GO TO 1 
      END 
