      SUBROUTINE CARDAN (G,DI,A,AP,NDIM,S,F,N,IDEG) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
C     EXTREMA OF GRADIENT NORM IN THIRD ORDER TAYLOR EXPANSION 
      DIMENSION G(MAXPAR,3),A(5),AP(4),S(3),F(3),DI(*) 
       SAVE
      DATA TWOPI /6.283185307179586476925286766558 D0/ 
      DATA EPS /1.D-6/ ,EPS1,EPS2 /1.D-07,1.D-13 / 
      DERI(Y)=((4.D0*A5*Y+3.D0*A4)*Y+2.D0*A3)*Y+A2 
C 
C 
C     COMPUTE COEFFICIENTS OF<G!G> AND ... 
C     THOSE OF ENERGY PREDICTED IN !DI> DIRECTION. 
      A1=     DOT(G(1,1),G(1,1),NDIM) 
      A2=2.D0*DOT(G(1,1),G(1,2),NDIM) 
      A3=     DOT(G(1,2),G(1,2),NDIM) + DOT(G(1,1),G(1,3),NDIM)*2.D0 
      A4=2.D0*DOT(G(1,2),G(1,3),NDIM) 
      A5=     DOT(G(1,3),G(1,3),NDIM) 
      A(1)=A1 
      A(2)=A2 
      A(3)=A3 
      A(4)=A4 
      A(5)=A5 
C     AP(1) MUST BE PREVIOUSLY DEFINED ELSEWHERE 
      AP(2)=DOT(DI,G(1,1),NDIM) 
      AP(3)=DOT(DI,G(1,2),NDIM)/2.D0 
      AP(4)=DOT(DI,G(1,3),NDIM)/3.D0 
C 
C     SOLVE THE THIRD DEGREE PROBLEM 
      TR=1.D0/3.D0 
      IF(ABS(A5).LT.EPS2  ) GO TO 30                                    GCL0393
      IDEG=3 
      C=4.D0*A5 
      FLEX=-A4/C 
      P=(3.D0*A4*FLEX+2.D0*A3)/C 
      Q=DERI(FLEX)/C 
      IF(ABS(P).LT.EPS1) GO TO 42                                       GCL0393
      E=SQRT(ABS(P)/3.D0)                                               GCL0393
      T=-Q/(2.D0*(E**3)) 
      IF(P.GT.0.D0.OR.ABS(T).GT.1.D0) GO TO 43                          GCL0393
      B=ACOS(T) 
      DO 40 I=1,3 
   40 S(I)=2.D0*E*COS((B+TWOPI*DBLE(I-2))/3.D0)+FLEX                    GCL0393
      N=3 
C     CLASS THE ROOTS IN DESCENDING ORDER 
      DO 41 I=1,2 
      SS=S(I) 
      K=I+1 
      DO 41 J=K,3 
      IF(SS.GE.S(J)) GO TO 41 
      SS=S(J) 
      S(J)=S(I) 
      S(I)=SS 
   41 CONTINUE 
      GO TO 10 
   42 S(1)=FLEX 
      IF(ABS(Q).GT.EPS2) S(1)=S(1)+SIGN(ABS(Q)**TR,-Q)                  GCL0393
      GO TO 44 
   43 Q=Q/2.D0 
      T=(P**3)/27.D0+Q**2 
      IF(T.LT.EPS2) GO TO 42 
      T=SQRT(T) 
      S(1)=FLEX 
      P=T-Q 
      IF(ABS(P).GT.EPS2) S(1)=S(1)+SIGN(ABS(P)**TR,P)                   GCL0393
      P=T+Q 
      IF(ABS(P).GT.EPS2) S(1)=S(1)-SIGN(ABS(P)**TR,P)                   GCL0393
   44 N=1 
      GO TO 10 
   30 IF(ABS(A3).LT.EPS2  ) GO TO 20                                    GCL0393
      IDEG=2 
      N=1 
      S(1)=-A2/(2.D0*A3) 
      GO TO 10 
   20 IDEG=1 
      N=1 
      S(1)=-A1/A2 
   10 DO 11 I=1,N 
      IF(ABS(S(I)).LT.EPS ) S(I)=0.D0                                   GCL0393
   11 F(I)=(((A(5)*S(I)+A(4))*S(I)+A(3))*S(I)+A(2))*S(I)+A(1) 
      RETURN 
      END 
