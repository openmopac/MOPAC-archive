      SUBROUTINE SET (S1,S2,NA,NB,RAB,NBOND,II)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      COMMON /SETC/ A(7),B(7),SA,SB,FACTOR,ISP,IPS 
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
C*********************************************************************** 
C 
C     SET IS PART OF THE OVERLAP CALCULATION, CALLED BY DIAT2. 
C         IT BUILD THE "A" AND "B" INTEGRALS. 
C 
C*********************************************************************** 
      DIMENSION FACT(17),WORK(17),COEF(24) 
      SAVE
C     INITIALIZE (DONE ONLY ONCE) 
      IF (ICONTR(40).EQ.1) THEN                                         GDH0195
         ICONTR(40)=2                                                   GDH0195
         FACT(1)=1.D0 
         Y=1.D0 
         DO 10 I=2,17 
         Y=Y*DBLE(I)                                                    GCL0393
   10    FACT(I)=1.D0/Y 
CDIR$ IVDEP 
         DO 20 I=1,23,2 
         COEF(I)=2.D0/DBLE(I)                                           GCL0393
   20    COEF(I+1)=0.D0 
      ENDIF 
C 
      IF (NA.LE.NB) THEN 
         ISP=1 
         IPS=2 
         SA=S1 
         SB=S2 
      ELSE 
         ISP=2 
         IPS=1 
         SA=S2 
         SB=S1 
      ENDIF 
      IF (II.GT.3) THEN 
         K=II 
      ELSE 
         K=II+1 
      ENDIF 
C 
C     "A" INTEGRALS 
C 
      X=0.5D0*RAB*(SA+SB) 
      Y=1.D0/X 
      EXPMX=EXP(-X) 
      A(1)=EXPMX*Y 
      DO 30 I=1,K 
   30 A(I+1)=(A(I)*DBLE(I)+EXPMX)*Y                                     GCL0393
C 
C     "B" INTEGRALS 
C 
      X=0.5D0*RAB*(SB-SA) 
      ABSX = ABS(X) 
      IF (ABSX.GT.3.D0) THEN 
         EXPX=EXP(X) 
         EXPMX=1.D0/EXPX 
         Y=1.D0/X 
         B(1)=(EXPX-EXPMX)*Y 
         DO 40 I=1,K 
         EXPX=-EXPX 
   40    B(I+1)=(DBLE(I)*B(I)+EXPX-EXPMX)*Y                             GCL0393
      ELSE 
         DO 50 I=1,K+1 
   50    B(I)=COEF(I) 
         IF (ABSX.LE.1.D-6) RETURN 
         LAST=5+4*ABSX 
         DO 60 M=1,LAST 
   60    WORK(M)=FACT(M)*(-X)**M 
         DO 70 I=1,K+1 
         J=MOD(I,2)+1 
         DO 70 M=J,LAST,2 
   70    B(I)=WORK(M)*COEF(M+I)+B(I) 
      ENDIF 
      RETURN 
      END 
