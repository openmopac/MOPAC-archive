      SUBROUTINE COE(X1,Y1,Z1,X2,Y2,Z2,PQ1,PQ2,C,R)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER PQ1,PQ2,PQ,CO
      DIMENSION C(75)
      XY=(X2-X1)**2+(Y2-Y1)**2
      R=SQRT(XY+(Z2-Z1)**2)
      XY=SQRT(XY)
      IF (XY.LT.1.D-10) GO TO 10
      CA=(X2-X1)/XY
      CB=(Z2-Z1)/R
      SA=(Y2-Y1)/XY
      SB=XY/R
      GO TO 50
   10 IF (Z2-Z1) 20,30,40
   20 CA=-1.D0
      CB=-1.D0
      SA=0.D0
      SB=0.D0
      GO TO 50
   30 CA=0.D0
      CB=0.D0
      SA=0.D0
      SB=0.D0
      GO TO 50
   40 CA=1.D0
      CB=1.D0
      SA=0.D0
      SB=0.D0
   50 CONTINUE
      CO=0
      DO 60 I=1,75
   60 C(I)=0.D0
      IF (PQ1.GT.PQ2) GO TO 70
      PQ=PQ2
      GO TO 80
   70 PQ=PQ1
   80 CONTINUE
      C(37)=1.D0
      IF (PQ.LT.2) GO TO 90
      C(56)=CA*CB
      C(41)=CA*SB
      C(26)=-SA
      C(53)=-SB
      C(38)=CB
      C(23)=0.D0
      C(50)=SA*CB
      C(35)=SA*SB
      C(20)=CA
      IF (PQ.LT.3) GO TO 90
      C2A=2*CA*CA-1.D0
      C2B=2*CB*CB-1.D0
      S2A=2*SA*CA
      S2B=2*SB*CB
      C(75)=C2A*CB*CB+0.5D0*C2A*SB*SB
      C(60)=0.5D0*C2A*S2B
      C(45)=0.8660254037841D0*C2A*SB*SB
      C(30)=-S2A*SB
      C(15)=-S2A*CB
      C(72)=-0.5D0*CA*S2B
      C(57)=CA*C2B
      C(42)=0.8660254037841D0*CA*S2B
      C(27)=-SA*CB
      C(12)=SA*SB
      C(69)=0.5773502691894D0*SB*SB*1.5D0
      C(54)=-0.8660254037841D0*S2B
      C(39)=CB*CB-0.5D0*SB*SB
      C(66)=-0.5D0*SA*S2B
      C(51)=SA*C2B
      C(36)=0.8660254037841D0*SA*S2B
      C(21)=CA*CB
      C(6)=-CA*SB
      C(63)=S2A*CB*CB+0.5D0*S2A*SB*SB
      C(48)=0.5D0*S2A*S2B
      C(33)=0.8660254037841D0*S2A*SB*SB
      C(18)=C2A*SB
      C(3)=C2A*CB
   90 CONTINUE
      RETURN
      END
