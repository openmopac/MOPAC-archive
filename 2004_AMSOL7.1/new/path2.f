      SUBROUTINE PATH2 (Y1,Y2,N) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
C     THIRD ORDER INTERPOLATION BETWEEN Y1 AND Y2 (HALF STEP SIZE) 
C     H =STEP SIZE BETWEEN Y1 AND Y2 , Y2 POSTERIOR AT Y1. 
C     RESULTS IN Y VECTORS (Y AND DY/DH ). 
      COMMON /OPTMCI / ICALL,ITERAT,IS,IPRIN,ITE,NBOUND,NBOULO,         DJG0995
     1                 IDUMMY(19 + 2*NCHAIN - 7)                        3GL3092 
      COMMON /OPTMCL / KONV,HUPDAT,RESET,INTERP, LDUMMY(NCHAIN)         GCL0892
      COMMON /OPTMCR / HESSE(MAXHES),P(MAXPAR,MAXPAR),DY2(MAXPAR),      3GL3092
     1                 POND(MAXPAR),D(MAXPAR),XOLD(MAXPAR),             3GL3092
     2                 TVOLD(MAXPAR),Y1Y2Y3(MAXPAR,6),C(MAXPAR,4),      3GL3092
     3                 Y4(MAXPAR,2),Y(MAXPAR,2),YE(MAXPAR),DX(MAXPAR),  3GL3092
     4                 HPREC,HMIN,H,HMAX,HTOT,DHTOT,RMS,EOLD,CRITE,     3GL3092
     5                 Y1NORM,Y2NORM,Y3NORM,ETOL,TOLE,                  3GL3092
     6                 RDUMY(MAXPAR * (2*MAXPAR + NCHAIN + 16 - 21) +   3GL3092
     7                       29 + NCHAIN - 14)                          3GL3092
C     COMMON /OPTIM/ IMP,IMP0,LEC,IPRT,HESSE(MAXHES),P(MAXPAR,MAXPAR) 
C    .              ,DY2(MAXPAR),POND(MAXPAR),D(MAXPAR),XOLD(MAXPAR) 
C    .              ,TVOLD(MAXPAR),Y1Y2Y3(MAXPAR,6),C(MAXPAR,4) 
C    .              ,Y4(MAXPAR,2),Y(MAXPAR,2),YE(MAXPAR),DX(MAXPAR) 
C    .              ,HPREC,HMIN,H,HMAX,HTOT,DHTOT,RMS,EOLD,CRITE 
C    .              ,Y1NORM,Y2NORM,Y3NORM,ETOL,TOLE 
C    .              ,ICALL,ITERAT,IS,IPRINT,ITE,NBOUND,NBOULO 
C    .              ,KONV,HUPDAT,RESET,INTERP 
      DIMENSION Y1(MAXPAR,2),Y2(MAXPAR,2) 
      LOGICAL KONV,HUPDAT,RESET,INTERP 
      LOGICAL LDUMMY                                                    3GL3092
      SAVE
      IF(NBOULO.EQ.0) GO TO 20 
      DO 10 I=1,NBOULO 
      Y(I,1)=0.5D0*(Y1(I,1)+Y2(I,1))+H*(Y1(I,2)-Y2(I,2))*0.125D0 
   10 Y(I,2)=1.5D0*(Y2(I,1)-Y1(I,1))/H-(Y1(I,2)+Y2(I,2))*0.250D0 
      IF(NBOUND.GT.N) RETURN 
   20 HH=H*0.5D0 
      DO 30 I=NBOUND,N 
      EXPD=EXP(-D(I)*HH)                                                GCL0393
      AO=Y1(I,2)+D(I)*Y1(I,1) 
      C1=Y2(I,1)-Y1(I,1)-Y1(I,2)*C(I,1) 
      C2=Y2(I,2)+D(I)*Y2(I,1)-AO 
      DELTA=1.D0/(0.5D0*C(I,2)-C(I,3)) 
      A1=(0.5D0*C1-C(I,3)*C2)*DELTA 
      A2=(C(I,2)*C2-C1)*DELTA 
      CO=(1.D0-EXPD)/D(I) 
      C1=(0.5D0-CO/H)/D(I) 
      C2=(0.125D0-C1/H)/D(I) 
      Y(I,1)=Y1(I,1)*EXPD+AO*CO+A1*C1+A2*C2 
   30 Y(I,2)=-D(I)*Y(I,1)+AO+0.5D0*A1+0.125D0*A2 
      RETURN 
      END 
