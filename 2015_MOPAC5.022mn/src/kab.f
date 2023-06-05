      SUBROUTINE KAB(IA,JA, PK, W, KINDEX, F,*)                         CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PK(*), W(*), F(*),KINDEX(256)
C    +, SUM(16)
CSAV         SAVE                                                           GL0892
C
C  FOR VECTOR MACHINES: REMOVE THE ARRAY SUM IN DIMENSION STMT
C                       UNCOMMENT LINES BETWEEN VECTOR CODE BOUNDARIES
C                       COMMENT LINES BETWEEN SCALAR CODE BOUNDARIES
C  FOR SCALAR MACHINES: ADD THE ARRAY SUM IN DIMENSION STMT
C                       UNCOMMENT LINES BETWEEN SCALAR CODE BOUNDARIES
C                       COMMENT LINES BETWEEN VECTOR CODE BOUNDARIES
C
C the following line is the start of the vector code
                  L=0
                  M=0
                  DO 130 J1=IA,IA+3
                  J=(J1*(J1-1))/2
                  DO 130 J2=JA,JA+3
                  M=M+1
                  J3=J+J2
                     SUM=0
                     DO 120 I=1,16
                        L=L+1
  120                SUM=SUM+PK(I)*W(KINDEX(L))
  130             F(J3)=F(J3)-SUM
C the previous line is the end of the vector code
C the following line is the start of the scalar code
C     SUM( 1)=
C    1+PK( 1)*W(  1)+PK( 2)*W(  2)+PK( 3)*W(  4)+PK( 4)*W(  7)
C    1+PK( 5)*W( 11)+PK( 6)*W( 12)+PK( 7)*W( 14)+PK( 8)*W( 17)
C    1+PK( 9)*W( 31)+PK(10)*W( 32)+PK(11)*W( 34)+PK(12)*W( 37)
C    1+PK(13)*W( 61)+PK(14)*W( 62)+PK(15)*W( 64)+PK(16)*W( 67)
C     SUM( 2)=
C    1+PK( 1)*W(  2)+PK( 2)*W(  3)+PK( 3)*W(  5)+PK( 4)*W(  8)
C    1+PK( 5)*W( 12)+PK( 6)*W( 13)+PK( 7)*W( 15)+PK( 8)*W( 18)
C    1+PK( 9)*W( 32)+PK(10)*W( 33)+PK(11)*W( 35)+PK(12)*W( 38)
C    1+PK(13)*W( 62)+PK(14)*W( 63)+PK(15)*W( 65)+PK(16)*W( 68)
C     SUM( 3)=
C    1+PK( 1)*W(  4)+PK( 2)*W(  5)+PK( 3)*W(  6)+PK( 4)*W(  9)
C    1+PK( 5)*W( 14)+PK( 6)*W( 15)+PK( 7)*W( 16)+PK( 8)*W( 19)
C    1+PK( 9)*W( 34)+PK(10)*W( 35)+PK(11)*W( 36)+PK(12)*W( 39)
C    1+PK(13)*W( 64)+PK(14)*W( 65)+PK(15)*W( 66)+PK(16)*W( 69)
C     SUM( 4)=
C    1+PK( 1)*W(  7)+PK( 2)*W(  8)+PK( 3)*W(  9)+PK( 4)*W( 10)
C    1+PK( 5)*W( 17)+PK( 6)*W( 18)+PK( 7)*W( 19)+PK( 8)*W( 20)
C    1+PK( 9)*W( 37)+PK(10)*W( 38)+PK(11)*W( 39)+PK(12)*W( 40)
C    1+PK(13)*W( 67)+PK(14)*W( 68)+PK(15)*W( 69)+PK(16)*W( 70)
C     SUM( 5)=
C    1+PK( 1)*W( 11)+PK( 2)*W( 12)+PK( 3)*W( 14)+PK( 4)*W( 17)
C    1+PK( 5)*W( 21)+PK( 6)*W( 22)+PK( 7)*W( 24)+PK( 8)*W( 27)
C    1+PK( 9)*W( 41)+PK(10)*W( 42)+PK(11)*W( 44)+PK(12)*W( 47)
C    1+PK(13)*W( 71)+PK(14)*W( 72)+PK(15)*W( 74)+PK(16)*W( 77)
C     SUM( 6)=
C    1+PK( 1)*W( 12)+PK( 2)*W( 13)+PK( 3)*W( 15)+PK( 4)*W( 18)
C    1+PK( 5)*W( 22)+PK( 6)*W( 23)+PK( 7)*W( 25)+PK( 8)*W( 28)
C    1+PK( 9)*W( 42)+PK(10)*W( 43)+PK(11)*W( 45)+PK(12)*W( 48)
C    1+PK(13)*W( 72)+PK(14)*W( 73)+PK(15)*W( 75)+PK(16)*W( 78)
C     SUM( 7)=
C    1+PK( 1)*W( 14)+PK( 2)*W( 15)+PK( 3)*W( 16)+PK( 4)*W( 19)
C    1+PK( 5)*W( 24)+PK( 6)*W( 25)+PK( 7)*W( 26)+PK( 8)*W( 29)
C    1+PK( 9)*W( 44)+PK(10)*W( 45)+PK(11)*W( 46)+PK(12)*W( 49)
C    1+PK(13)*W( 74)+PK(14)*W( 75)+PK(15)*W( 76)+PK(16)*W( 79)
C     SUM( 8)=
C    1+PK( 1)*W( 17)+PK( 2)*W( 18)+PK( 3)*W( 19)+PK( 4)*W( 20)
C    1+PK( 5)*W( 27)+PK( 6)*W( 28)+PK( 7)*W( 29)+PK( 8)*W( 30)
C    1+PK( 9)*W( 47)+PK(10)*W( 48)+PK(11)*W( 49)+PK(12)*W( 50)
C    1+PK(13)*W( 77)+PK(14)*W( 78)+PK(15)*W( 79)+PK(16)*W( 80)
C     SUM( 9)=
C    1+PK( 1)*W( 31)+PK( 2)*W( 32)+PK( 3)*W( 34)+PK( 4)*W( 37)
C    1+PK( 5)*W( 41)+PK( 6)*W( 42)+PK( 7)*W( 44)+PK( 8)*W( 47)
C    1+PK( 9)*W( 51)+PK(10)*W( 52)+PK(11)*W( 54)+PK(12)*W( 57)
C    1+PK(13)*W( 81)+PK(14)*W( 82)+PK(15)*W( 84)+PK(16)*W( 87)
C     SUM(10)=
C    1+PK( 1)*W( 32)+PK( 2)*W( 33)+PK( 3)*W( 35)+PK( 4)*W( 38)
C    1+PK( 5)*W( 42)+PK( 6)*W( 43)+PK( 7)*W( 45)+PK( 8)*W( 48)
C    1+PK( 9)*W( 52)+PK(10)*W( 53)+PK(11)*W( 55)+PK(12)*W( 58)
C    1+PK(13)*W( 82)+PK(14)*W( 83)+PK(15)*W( 85)+PK(16)*W( 88)
C     SUM(11)=
C    1+PK( 1)*W( 34)+PK( 2)*W( 35)+PK( 3)*W( 36)+PK( 4)*W( 39)
C    1+PK( 5)*W( 44)+PK( 6)*W( 45)+PK( 7)*W( 46)+PK( 8)*W( 49)
C    1+PK( 9)*W( 54)+PK(10)*W( 55)+PK(11)*W( 56)+PK(12)*W( 59)
C    1+PK(13)*W( 84)+PK(14)*W( 85)+PK(15)*W( 86)+PK(16)*W( 89)
C     SUM(12)=
C    1+PK( 1)*W( 37)+PK( 2)*W( 38)+PK( 3)*W( 39)+PK( 4)*W( 40)
C    1+PK( 5)*W( 47)+PK( 6)*W( 48)+PK( 7)*W( 49)+PK( 8)*W( 50)
C    1+PK( 9)*W( 57)+PK(10)*W( 58)+PK(11)*W( 59)+PK(12)*W( 60)
C    1+PK(13)*W( 87)+PK(14)*W( 88)+PK(15)*W( 89)+PK(16)*W( 90)
C     SUM(13)=
C    1+PK( 1)*W( 61)+PK( 2)*W( 62)+PK( 3)*W( 64)+PK( 4)*W( 67)
C    1+PK( 5)*W( 71)+PK( 6)*W( 72)+PK( 7)*W( 74)+PK( 8)*W( 77)
C    1+PK( 9)*W( 81)+PK(10)*W( 82)+PK(11)*W( 84)+PK(12)*W( 87)
C    1+PK(13)*W( 91)+PK(14)*W( 92)+PK(15)*W( 94)+PK(16)*W( 97)
C     SUM(14)=
C    1+PK( 1)*W( 62)+PK( 2)*W( 63)+PK( 3)*W( 65)+PK( 4)*W( 68)
C    1+PK( 5)*W( 72)+PK( 6)*W( 73)+PK( 7)*W( 75)+PK( 8)*W( 78)
C    1+PK( 9)*W( 82)+PK(10)*W( 83)+PK(11)*W( 85)+PK(12)*W( 88)
C    1+PK(13)*W( 92)+PK(14)*W( 93)+PK(15)*W( 95)+PK(16)*W( 98)
C     SUM(15)=
C    1+PK( 1)*W( 64)+PK( 2)*W( 65)+PK( 3)*W( 66)+PK( 4)*W( 69)
C    1+PK( 5)*W( 74)+PK( 6)*W( 75)+PK( 7)*W( 76)+PK( 8)*W( 79)
C    1+PK( 9)*W( 84)+PK(10)*W( 85)+PK(11)*W( 86)+PK(12)*W( 89)
C    1+PK(13)*W( 94)+PK(14)*W( 95)+PK(15)*W( 96)+PK(16)*W( 99)
C     SUM(16)=
C    1+PK( 1)*W( 67)+PK( 2)*W( 68)+PK( 3)*W( 69)+PK( 4)*W( 70)
C    1+PK( 5)*W( 77)+PK( 6)*W( 78)+PK( 7)*W( 79)+PK( 8)*W( 80)
C    1+PK( 9)*W( 87)+PK(10)*W( 88)+PK(11)*W( 89)+PK(12)*W( 90)
C    1+PK(13)*W( 97)+PK(14)*W( 98)+PK(15)*W( 99)+PK(16)*W(100)
C                  M=0
C                 DO 130 J1=IA,IA+3
C                 J=(J1*(J1-1))/2
C                 DO 130 J2=JA,JA+3
C                 M=M+1
C                 J3=J+J2
C 130             F(J3)=F(J3)-SUM(M)
C the previous line is the end of the scalar code
       RETURN
 9999 RETURN 1                                                          CSTP
      END
