      SUBROUTINE DIAT2(NA,ESA,EPA,R12,NB,ESB,EPB,S) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
C*********************************************************************** 
C 
C DIAT2  CALCULATES OVERLAPS BETWEEN ATOMIC ORBITALS FOR PAIRS OF ATOMS 
C        IT CAN HANDLE THE ORBITALS 1S, 2S, 3S, 2P, AND 3P. 
C 
C*********************************************************************** 
      COMMON /SETC/ A(7),B(7),SA,SB,FACTOR,ISP,IPS 
      COMMON /ONESCM/ ICONTR(100)                                       DJG0295
      DIMENSION S(3,3,3) 
      DIMENSION INMB(17),III(78) 
       SAVE
      DATA INMB/1,0,2,2,3,4,5,6,7,0,8,8,8,9,10,11,12/ 
      IF (ICONTR(18).EQ.1) THEN                                         DJG0295
         ICONTR(18)=2                                                   DJG0295
         RT3=1.D0/SQRT(3.D0) 
      ENDIF 
C     NUMBERING CORRESPONDS TO BOND TYPE MATRIX GIVEN ABOVE 
C      THE CODE IS 
C 
C     III=1      FIRST - FIRST  ROW ELEMENTS 
C        =2      FIRST - SECOND 
C        =3      FIRST - THIRD 
C        =4      SECOND - SECOND 
C        =5      SECOND - THIRD 
C        =6      THIRD - THIRD 
      DATA III/1,2,4,   2,4,4,   2,4,4,4,   2,4,4,4,4, 
     1 2,4,4,4,4,4,   2,4,4,4,4,4,4,   3,5,5,5,5,5,5,6, 
     2 3,5,5,5,5,5,5,6,6,   3,5,5,5,5,5,5,6,6,6,   3,5,5,5,5,5,5,6,6,6,6 
     3, 3,5,5,5,5,5,5,6,6,6,6,6/ 
C 
C      ASSIGN BOND NUMBER 
C 
      JMAX=MAX(INMB(NA),INMB(NB))                                       GCL0393
      JMIN=MIN(INMB(NA),INMB(NB))                                       GCL0393
      NBOND=(JMAX*(JMAX-1))/2+JMIN 
      II=III(NBOND) 
      DO 10 I=1,27 
   10 S(I,1,1)=0.D0 
      RAB=R12/0.529167D0 
      GOTO (20,30,40,50,60,70), II 
C 
C     ------------------------------------------------------------------ 
C *** THE ORDERING OF THE ELEMENTS WITHIN S IS 
C *** S(1,1,1)=(S(B)/S(A)) 
C *** S(1,2,1)=(P-SIGMA(B)/S(A)) 
C *** S(2,1,1)=(S(B)/P-SIGMA(A)) 
C *** S(2,2,1)=(P-SIGMA(B)/P-SIGMA(A)) 
C *** S(2,2,2)=(P-PI(B)/P-PI(A)) 
C     ------------------------------------------------------------------ 
C *** FIRST ROW - FIRST ROW OVERLAPS 
C 
   20 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II) 
      S(1,1,1)=.25D00*SQRT((SA*SB*RAB*RAB)**3)*(A(3)*B(1)-B(3)*A(1)) 
      RETURN 
C 
C *** FIRST ROW - SECOND ROW OVERLAPS 
C 
   30 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II) 
      W=SQRT((SA**3)*(SB**5))*(RAB**4)*0.125D00 
      S(1,1,1)=W*RT3*(A(4)*B(1)-B(4)*A(1)+A(3)*B(2)-B(3)*A(2)) 
      IF (NB.GT.1) THEN 
         CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II) 
      ELSE 
         CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II) 
      ENDIF 
      W=SQRT((SA**3)*(SB**5))*(RAB**4)*0.125D00 
      S(ISP,IPS,1)=W*(A(3)*B(1)-B(3)*A(1)+A(4)*B(2)-B(4)*A(2)) 
      RETURN 
C 
C *** FIRST ROW - THIRD ROW OVERLAPS 
C 
   40 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II) 
      W=SQRT((SA**3)*(SB**7)/7.5D00)*(RAB**5)*0.0625D00 
      S(1,1,1)=W*(A(5)*B(1)-B(5)*A(1)+ 
     12.D00*(A(4)*B(2)-B(4)*A(2)))*RT3 
      IF (NB.GT.1) THEN 
         CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II) 
      ELSE 
         CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II) 
      ENDIF 
      W=SQRT((SA**3)*(SB**7)/7.5D00)*(RAB**5)*0.0625D00 
      S(ISP,IPS,1)=W*(A(4)*(B(1)+B(3))-B(4)*(A(1)+A(3))+ 
     1B(2)*(A(3)+A(5))-A(2)*(B(3)+B(5))) 
      RETURN 
C 
C *** SECOND ROW - SECOND ROW OVERLAPS 
C 
   50 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II) 
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00 
      S(1,1,1)=W*(A(5)*B(1)+B(5)*A(1)-2.0D00*A(3)*B(3))/3.0D00 
      IF (NA.LE.NB) THEN 
         CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II) 
      ELSE 
         CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II) 
      ENDIF 
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00 
      D=A(4)*(B(1)-B(3))-A(2)*(B(3)-B(5)) 
      E=B(4)*(A(1)-A(3))-B(2)*(A(3)-A(5)) 
      S(ISP,IPS,1)=W*RT3*(D+E) 
      IF (NA.LE.NB) THEN 
         CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II) 
      ELSE 
         CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II) 
      ENDIF 
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00 
      D=A(4)*(B(1)-B(3))-A(2)*(B(3)-B(5)) 
      E=B(4)*(A(1)-A(3))-B(2)*(A(3)-A(5)) 
      S(IPS,ISP,1)=-W*RT3*(E-D) 
      CALL SET (EPA,EPB,NA,NB,RAB,NBOND,II) 
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00 
      S(2,2,1)=-W*(B(3)*(A(5)+A(1))-A(3)*(B(5)+B(1))) 
      S(2,2,2)=0.5D0*W*(A(5)*(B(1)-B(3))-B(5)*(A(1)-A(3)) 
     1-A(3)*B(1)+B(3)*A(1)) 
      RETURN 
C 
C *** SECOND ROW - THIRD ROW OVERLAPS 
C 
   60 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II) 
      W=SQRT((SA**5)*(SB**7)/7.5D00)*(RAB**6)*0.03125D00 
      S(1,1,1)=W*(A(6)*B(1)+A(5)*B(2)-2.D0*(A(4)*B(3)+ 
     1A(3)*B(4))+A(2)*B(5)+A(1)*B(6))/3.D0 
      IF (NA.LE.NB) THEN 
         CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II) 
      ELSE 
         CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II) 
      ENDIF 
      W=SQRT((SA**5)*(SB**7)/7.5D00)*(RAB**6)*0.03125D00 
      S(ISP,IPS,1)=W*RT3*(A(6)*B(2)+A(5)*B(1)-2.D0*(A(4)*B(4)+A(3)*B(3)) 
     1+A(2)*B(6)+A(1)*B(5)) 
      IF (NA.LE.NB) THEN 
         CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II) 
      ELSE 
         CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II) 
      ENDIF 
      W=SQRT((SA**5)*SB**7/7.5D00)*(RAB**6)*0.03125D00 
      S(IPS,ISP,1)=-W*RT3*(A(5)*(2.D0*B(3)-B(1))-B(5)*(2.D0*A(3)-A(1)) 
     1-A(2)*(B(6)-2.D0*B(4))+B(2)*(A(6)-2.D0*A(4))) 
      CALL SET (EPA,EPB,NA,NB,RAB,NBOND,II) 
      W=SQRT((SA**5)*SB**7/7.5D00)*(RAB**6)*0.03125D00 
      S(2,2,1)=-W*(B(4)*(A(1)+A(5))-A(4)*(B(1)+B(5)) 
     1+B(3)*(A(2)+A(6))-A(3)*(B(2)+B(6))) 
      S(2,2,2)=0.5D0*W*(A(6)*(B(1)-B(3))-B(6)*(A(1)- 
     1A(3))+A(5)*(B(2)-B(4))-B(5 
     2)*(A(2)-A(4))-A(4)*B(1)+B(4)*A(1)-A(3)*B(2)+B(3)*A(2)) 
      RETURN 
C 
C *** THIRD ROW - THIRD ROW OVERLAPS 
C 
   70 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II) 
      W=SQRT((SA*SB*RAB*RAB)**7)/480.D00 
      S(1,1,1)=W*(A(7)*B(1)-3.D00*(A(5)*B(3)-A(3)*B(5))-A(1)*B(7))/3.D00 
      IF (NA.LE.NB) THEN 
         CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II) 
      ELSE 
         CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II) 
      ENDIF 
      W=SQRT((SA*SB*RAB*RAB)**7)/480.D00 
      D=A(6)*(B(1)-B(3))-2.D00*A(4)*(B(3)-B(5))+A(2)*(B(5)-B(7)) 
      E=B(6)*(A(1)-A(3))-2.D00*B(4)*(A(3)-A(5))+B(2)*(A(5)-A(7)) 
      S(ISP,IPS,1)=W*RT3*(D-E) 
      IF (NA.LE.NB) THEN 
         CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II) 
      ELSE 
         CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II) 
      ENDIF 
      W=SQRT((SA*SB*RAB*RAB)**7)/480.D00 
      D=A(6)*(B(1)-B(3))-2.D00*A(4)*(B(3)-B(5))+A(2)*(B(5)-B(7)) 
      E=B(6)*(A(1)-A(3))-2.D00*B(4)*(A(3)-A(5))+B(2)*(A(5)-A(7)) 
      S(IPS,ISP,1)=-W*RT3*(-D-E) 
      CALL SET (EPA,EPB,NA,NB,RAB,NBOND,II) 
      W=SQRT((SA*SB*RAB*RAB)**7)/480.D00 
      S(2,2,1)=-W*(A(3)*(B(7)+2.D0*B(3))-A(5)*(B(1)+ 
     12.D0*B(5))-B(5)*A(1)+A(7)*B(3)) 
      S(2,2,2)=0.5D0*W*(A(7)*(B(1)-B(3))+B(7)*(A(1)- 
     1A(3))+A(5)*(B(5)-B(3)-B(1) 
     2)+B(5)*(A(5)-A(3)-A(1))+2.D00*A(3)*B(3)) 
      RETURN 
C 
      END 
