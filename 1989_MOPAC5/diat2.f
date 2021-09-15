      SUBROUTINE DIAT2(NA,ESA,EPA,R12,NB,ESB,EPB,S)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION S(3,3,3)
C***********************************************************************
C
C OVERLP CALCULATES OVERLAPS BETWEEN ATOMIC ORBITALS FOR PAIRS OF ATOMS
C        IT CAN HANDLE THE ORBITALS 1S, 2S, 3S, 2P, AND 3P.
C
C***********************************************************************
      COMMON /SETC/ A(7),B(7),SA,SB,FACTOR,ISP,IPS
      DIMENSION INMB(17),III(78)
      DATA INMB/1,0,2,2,3,4,5,6,7,0,8,8,8,9,10,11,12/
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
C      ASSIGNS BOND NUMBER
C
      JMAX=MAX0(INMB(NA),INMB(NB))
      JMIN=MIN0(INMB(NA),INMB(NB))
      NBOND=(JMAX*(JMAX-1))/2+JMIN
      II=III(NBOND)
      DO 10 I=1,3
         DO 10 J=1,3
            DO 10 K=1,3
   10 S(I,J,K)=0.D0
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
      S(1,1,1) = SQRT(1.D00/3.D00)
      S(1,1,1)=W*S(1,1,1)*(A(4)*B(1)-B(4)*A(1)+A(3)*B(2)-B(3)*A(2))
      IF (NA.GT.1) CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II)
      IF (NB.GT.1) CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA**3)*(SB**5))*(RAB**4)*0.125D00
      S(ISP,IPS,1)=W*(A(3)*B(1)-B(3)*A(1)+A(4)*B(2)-B(4)*A(2))
      RETURN
C
C *** FIRST ROW - THIRD ROW OVERLAPS
C
   40 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA**3)*(SB**7)/7.5D00)*(RAB**5)*0.0625D00
      SROOT3 = SQRT(3.D00)
      S(1,1,1)=W*(A(5)*B(1)-B(5)*A(1)+
     12.D00*(A(4)*B(2)-B(4)*A(2)))/SROOT3
      IF (NA.GT.1) CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II)
      IF (NB.GT.1) CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA**3)*(SB**7)/7.5D00)*(RAB**5)*0.0625D00
      S(ISP,IPS,1)=W*(A(4)*(B(1)+B(3))-B(4)*(A(1)+A(3))+
     1B(2)*(A(3)+A(5))-A(2)*(B(3)+B(5)))
      RETURN
C
C *** SECOND ROW - SECOND ROW OVERLAPS
C
   50 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00
      RT3=1.D00/SQRT(3.D00)
      S(1,1,1)=W*(A(5)*B(1)+B(5)*A(1)-2.0D00*A(3)*B(3))/3.0D00
      CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II)
      IF (NA.GT.NB) CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00
      D=A(4)*(B(1)-B(3))-A(2)*(B(3)-B(5))
      E=B(4)*(A(1)-A(3))-B(2)*(A(3)-A(5))
      S(ISP,IPS,1)=W*RT3*(D+E)
      CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II)
      IF (NA.GT.NB) CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00
      D=A(4)*(B(1)-B(3))-A(2)*(B(3)-B(5))
      E=B(4)*(A(1)-A(3))-B(2)*(A(3)-A(5))
      S(IPS,ISP,1)=-W*RT3*(E-D)
      CALL SET (EPA,EPB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00
      S(2,2,1)=-W*(B(3)*(A(5)+A(1))-A(3)*(B(5)+B(1)))
      HD = .5D00
      S(2,2,2)=HD*W*(A(5)*(B(1)-B(3))-B(5)*(A(1)-A(3))
     1-A(3)*B(1)+B(3)*A(1))
      RETURN
C
C *** SECOND ROW - THIRD ROW OVERLAPS
C
   60 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA**5)*(SB**7)/7.5D00)*(RAB**6)*0.03125D00
      RT3 = 1.D00 / SQRT(3.D00)
      TD = 2.D00
      S(1,1,1)=W*(A(6)*B(1)+A(5)*B(2)-TD*(A(4)*B(3)+
     1A(3)*B(4))+A(2)*B(5)+A(
     21)*B(6))/3.D00
      CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II)
      IF (NA.GT.NB) CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA**5)*(SB**7)/7.5D00)*(RAB**6)*0.03125D00
      TD = 2.D00
      S(ISP,IPS,1)=W*RT3*(A(6)*B(2)+A(5)*B(1)-TD*(A(4)*B(4)+A(3)*B(3))
     1+A(2)*B(6)+A(1)*B(5))
      CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II)
      IF (NA.GT.NB) CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA**5)*SB**7/7.5D00)*(RAB**6)*0.03125D00
      TD = 2.D00
      S(IPS,ISP,1)=-W*RT3*(A(5)*(TD*B(3)-B(1))-B(5)*(TD*A(3)-A(1))-A(2
     1)*(B(6)-TD*B(4))+B(2)*(A(6)-TD*A(4)))
      CALL SET (EPA,EPB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA**5)*SB**7/7.5D00)*(RAB**6)*0.03125D00
      S(2,2,1)=-W*(B(4)*(A(1)+A(5))-A(4)*(B(1)+B(5))
     1+B(3)*(A(2)+A(6))-A(3)*(B(2)+B(6)))
      HD = .5D00
      S(2,2,2)=HD*W*(A(6)*(B(1)-B(3))-B(6)*(A(1)-
     1A(3))+A(5)*(B(2)-B(4))-B(5
     2)*(A(2)-A(4))-A(4)*B(1)+B(4)*A(1)-A(3)*B(2)+B(3)*A(2))
      RETURN
C
C *** THIRD ROW - THIRD ROW OVERLAPS
C
   70 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA*SB*RAB*RAB)**7)/480.D00
      RT3 = 1.D00 / SQRT(3.D00)
      S(1,1,1)=W*(A(7)*B(1)-3.D00*(A(5)*B(3)-A(3)*B(5))-A(1)*B(7))/3.D00
      CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II)
      IF (NA.GT.NB) CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA*SB*RAB*RAB)**7)/480.D00
      D=A(6)*(B(1)-B(3))-2.D00*A(4)*(B(3)-B(5))+A(2)*(B(5)-B(7))
      E=B(6)*(A(1)-A(3))-2.D00*B(4)*(A(3)-A(5))+B(2)*(A(5)-A(7))
      S(ISP,IPS,1)=W*RT3*(D-E)
      CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II)
      IF (NA.GT.NB) CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA*SB*RAB*RAB)**7)/480.D00
      D=A(6)*(B(1)-B(3))-2.D00*A(4)*(B(3)-B(5))+A(2)*(B(5)-B(7))
      E=B(6)*(A(1)-A(3))-2.D00*B(4)*(A(3)-A(5))+B(2)*(A(5)-A(7))
      S(IPS,ISP,1)=-W*RT3*(-D-E)
      CALL SET (EPA,EPB,NA,NB,RAB,NBOND,II)
      W=SQRT((SA*SB*RAB*RAB)**7)/480.D00
      TD = 2.D00
      S(2,2,1)=-W*(A(3)*(B(7)+TD*B(3))-A(5)*(B(1)+
     1TD*B(5))-B(5)*A(1)+A(7)*B(3))
      HD = .5D00
      S(2,2,2)=HD*W*(A(7)*(B(1)-B(3))+B(7)*(A(1)-
     1A(3))+A(5)*(B(5)-B(3)-B(1)
     2)+B(5)*(A(5)-A(3)-A(1))+2.D00*A(3)*B(3))
      RETURN
C
      END
