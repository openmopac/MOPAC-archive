      SUBROUTINE BFN(X,B) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
C********************************************************************** 
C 
C     BFN FORMS THE "B" INTEGRALS FOR THE OVERLAP CALCULATION. 
C 
C********************************************************************** 
      DIMENSION B(13),FACT(17),COEF(30),WORK(17) 
       SAVE
C     INITIALIZE (DONE ONLY ONCE) 
      IF (ICONTR(30).EQ.1) THEN                                         GDH0195
         ICONTR(30)=2                                                   GDH0195
         FACT(1)=1.D0 
         Y=1.D0 
         DO 10 I=2,17 
         Y=Y*DBLE(I)                                                    GCL0393
   10    FACT(I)=1.D0/Y 
CDIR$ IVDEP 
         DO 20 I=1,29,2 
         COEF(I)=2.D0/DBLE(I)                                           GCL0393
   20    COEF(I+1)=0.D0 
      ENDIF 
      ABSX = ABS(X) 
      IF (ABSX.GT.3.D0) THEN 
         EXPX=EXP(X) 
         EXPMX=1.D0/EXPX 
         Y=1.D0/X 
         B(1)=(EXPX-EXPMX)*Y 
         DO 30 I=1,12 
         EXPX=-EXPX 
   30    B(I+1)=(DBLE(I)*B(I)+EXPX-EXPMX)*Y                             GCL0393
      ELSE 
         DO 40 I=1,13 
   40    B(I)=COEF(I) 
         IF (ABSX.LE.1.D-6) RETURN 
         LAST=5+4*ABSX 
         DO 50 M=1,LAST 
   50    WORK(M)=FACT(M)*(-X)**M 
         DO 60 I=1,13 
         J=MOD(I,2)+1 
         DO 60 M=J,LAST,2 
   60    B(I)=WORK(M)*COEF(M+I)+B(I) 
      ENDIF 
      RETURN 
      END 
