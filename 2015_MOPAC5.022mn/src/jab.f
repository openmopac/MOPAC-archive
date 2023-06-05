      SUBROUTINE JAB(IA,JA,LLPERM,JINDEX, JJNDEX,PJA,PJB,W, F,*)        CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION LLPERM(10), PJA(16), PJB(16), W(*), F(*),
     +JINDEX(256), JJNDEX(256)
C    +, SUMA(10), SUMB(10)
CSAV         SAVE                                                           GL0892
C
C  FOR VECTOR MACHINES: REMOVE THE ARRAYS SUMA AND SUMB IN DIMENSION STM
C                       UNCOMMENT LINES BETWEEN VECTOR CODE BOUNDARIES
C                       COMMENT LINES BETWEEN SCALAR CODE BOUNDARIES
C  FOR SCALAR MACHINES: ADD THE ARRAYS SUMA AND SUMB IN DIMENSION STMT
C                       UNCOMMENT LINES BETWEEN SCALAR CODE BOUNDARIES
C                       COMMENT LINES BETWEEN VECTOR CODE BOUNDARIES
C
C the following line is the start of the vector code
                  I=0
                  DO 100 I5=1,4
                  IIA=IA+I5-1
                  IJA=JA+I5-1
                  IOFF=(IIA*(IIA-1))/2+IA-1
                  JOFF=(IJA*(IJA-1))/2+JA-1
                  DO 100 I6=1,I5
                  IOFF=IOFF+1
                  JOFF=JOFF+1
                        I=I+1
                        L=LLPERM(I)
                        SUMA=0
                        SUMB=0
                        DO 90 K=1,16
                           L=L+1
                           SUMB=SUMB+PJA(K)*W(JJNDEX(L))
   90                   SUMA=SUMA+PJB(K)*W(JINDEX(L))
                        F(IOFF)=F(IOFF)+SUMA
  100             F(JOFF)=F(JOFF)+SUMB
C the previous line is the end of the vector code
C the following line is the start of the scalar code
C     SUMA( 1)=
C    1+PJA( 1)*W(  1)+PJA( 2)*W( 11)+PJA( 3)*W( 31)+PJA( 4)*W( 61)
C    1+PJA( 5)*W( 11)+PJA( 6)*W( 21)+PJA( 7)*W( 41)+PJA( 8)*W( 71)
C    1+PJA( 9)*W( 31)+PJA(10)*W( 41)+PJA(11)*W( 51)+PJA(12)*W( 81)
C    1+PJA(13)*W( 61)+PJA(14)*W( 71)+PJA(15)*W( 81)+PJA(16)*W( 91)
C     SUMA( 2)=
C    1+PJA( 1)*W(  2)+PJA( 2)*W( 12)+PJA( 3)*W( 32)+PJA( 4)*W( 62)
C    1+PJA( 5)*W( 12)+PJA( 6)*W( 22)+PJA( 7)*W( 42)+PJA( 8)*W( 72)
C    1+PJA( 9)*W( 32)+PJA(10)*W( 42)+PJA(11)*W( 52)+PJA(12)*W( 82)
C    1+PJA(13)*W( 62)+PJA(14)*W( 72)+PJA(15)*W( 82)+PJA(16)*W( 92)
C     SUMA( 3)=
C    1+PJA( 1)*W(  3)+PJA( 2)*W( 13)+PJA( 3)*W( 33)+PJA( 4)*W( 63)
C    1+PJA( 5)*W( 13)+PJA( 6)*W( 23)+PJA( 7)*W( 43)+PJA( 8)*W( 73)
C    1+PJA( 9)*W( 33)+PJA(10)*W( 43)+PJA(11)*W( 53)+PJA(12)*W( 83)
C    1+PJA(13)*W( 63)+PJA(14)*W( 73)+PJA(15)*W( 83)+PJA(16)*W( 93)
C     SUMA( 4)=
C    1+PJA( 1)*W(  4)+PJA( 2)*W( 14)+PJA( 3)*W( 34)+PJA( 4)*W( 64)
C    1+PJA( 5)*W( 14)+PJA( 6)*W( 24)+PJA( 7)*W( 44)+PJA( 8)*W( 74)
C    1+PJA( 9)*W( 34)+PJA(10)*W( 44)+PJA(11)*W( 54)+PJA(12)*W( 84)
C    1+PJA(13)*W( 64)+PJA(14)*W( 74)+PJA(15)*W( 84)+PJA(16)*W( 94)
C     SUMA( 5)=
C    1+PJA( 1)*W(  5)+PJA( 2)*W( 15)+PJA( 3)*W( 35)+PJA( 4)*W( 65)
C    1+PJA( 5)*W( 15)+PJA( 6)*W( 25)+PJA( 7)*W( 45)+PJA( 8)*W( 75)
C    1+PJA( 9)*W( 35)+PJA(10)*W( 45)+PJA(11)*W( 55)+PJA(12)*W( 85)
C    1+PJA(13)*W( 65)+PJA(14)*W( 75)+PJA(15)*W( 85)+PJA(16)*W( 95)
C     SUMA( 6)=
C    1+PJA( 1)*W(  6)+PJA( 2)*W( 16)+PJA( 3)*W( 36)+PJA( 4)*W( 66)
C    1+PJA( 5)*W( 16)+PJA( 6)*W( 26)+PJA( 7)*W( 46)+PJA( 8)*W( 76)
C    1+PJA( 9)*W( 36)+PJA(10)*W( 46)+PJA(11)*W( 56)+PJA(12)*W( 86)
C    1+PJA(13)*W( 66)+PJA(14)*W( 76)+PJA(15)*W( 86)+PJA(16)*W( 96)
C     SUMA( 7)=
C    1+PJA( 1)*W(  7)+PJA( 2)*W( 17)+PJA( 3)*W( 37)+PJA( 4)*W( 67)
C    1+PJA( 5)*W( 17)+PJA( 6)*W( 27)+PJA( 7)*W( 47)+PJA( 8)*W( 77)
C    1+PJA( 9)*W( 37)+PJA(10)*W( 47)+PJA(11)*W( 57)+PJA(12)*W( 87)
C    1+PJA(13)*W( 67)+PJA(14)*W( 77)+PJA(15)*W( 87)+PJA(16)*W( 97)
C     SUMA( 8)=
C    1+PJA( 1)*W(  8)+PJA( 2)*W( 18)+PJA( 3)*W( 38)+PJA( 4)*W( 68)
C    1+PJA( 5)*W( 18)+PJA( 6)*W( 28)+PJA( 7)*W( 48)+PJA( 8)*W( 78)
C    1+PJA( 9)*W( 38)+PJA(10)*W( 48)+PJA(11)*W( 58)+PJA(12)*W( 88)
C    1+PJA(13)*W( 68)+PJA(14)*W( 78)+PJA(15)*W( 88)+PJA(16)*W( 98)
C     SUMA( 9)=
C    1+PJA( 1)*W(  9)+PJA( 2)*W( 19)+PJA( 3)*W( 39)+PJA( 4)*W( 69)
C    1+PJA( 5)*W( 19)+PJA( 6)*W( 29)+PJA( 7)*W( 49)+PJA( 8)*W( 79)
C    1+PJA( 9)*W( 39)+PJA(10)*W( 49)+PJA(11)*W( 59)+PJA(12)*W( 89)
C    1+PJA(13)*W( 69)+PJA(14)*W( 79)+PJA(15)*W( 89)+PJA(16)*W( 99)
C     SUMA(10)=
C    1+PJA( 1)*W( 10)+PJA( 2)*W( 20)+PJA( 3)*W( 40)+PJA( 4)*W( 70)
C    1+PJA( 5)*W( 20)+PJA( 6)*W( 30)+PJA( 7)*W( 50)+PJA( 8)*W( 80)
C    1+PJA( 9)*W( 40)+PJA(10)*W( 50)+PJA(11)*W( 60)+PJA(12)*W( 90)
C    1+PJA(13)*W( 70)+PJA(14)*W( 80)+PJA(15)*W( 90)+PJA(16)*W(100)
C     SUMB( 1)=
C    1+PJB( 1)*W(  1)+PJB( 2)*W(  2)+PJB( 3)*W(  4)+PJB( 4)*W(  7)
C    1+PJB( 5)*W(  2)+PJB( 6)*W(  3)+PJB( 7)*W(  5)+PJB( 8)*W(  8)
C    1+PJB( 9)*W(  4)+PJB(10)*W(  5)+PJB(11)*W(  6)+PJB(12)*W(  9)
C    1+PJB(13)*W(  7)+PJB(14)*W(  8)+PJB(15)*W(  9)+PJB(16)*W( 10)
C     SUMB( 2)=
C    1+PJB( 1)*W( 11)+PJB( 2)*W( 12)+PJB( 3)*W( 14)+PJB( 4)*W( 17)
C    1+PJB( 5)*W( 12)+PJB( 6)*W( 13)+PJB( 7)*W( 15)+PJB( 8)*W( 18)
C    1+PJB( 9)*W( 14)+PJB(10)*W( 15)+PJB(11)*W( 16)+PJB(12)*W( 19)
C    1+PJB(13)*W( 17)+PJB(14)*W( 18)+PJB(15)*W( 19)+PJB(16)*W( 20)
C     SUMB( 3)=
C    1+PJB( 1)*W( 21)+PJB( 2)*W( 22)+PJB( 3)*W( 24)+PJB( 4)*W( 27)
C    1+PJB( 5)*W( 22)+PJB( 6)*W( 23)+PJB( 7)*W( 25)+PJB( 8)*W( 28)
C    1+PJB( 9)*W( 24)+PJB(10)*W( 25)+PJB(11)*W( 26)+PJB(12)*W( 29)
C    1+PJB(13)*W( 27)+PJB(14)*W( 28)+PJB(15)*W( 29)+PJB(16)*W( 30)
C     SUMB( 4)=
C    1+PJB( 1)*W( 31)+PJB( 2)*W( 32)+PJB( 3)*W( 34)+PJB( 4)*W( 37)
C    1+PJB( 5)*W( 32)+PJB( 6)*W( 33)+PJB( 7)*W( 35)+PJB( 8)*W( 38)
C    1+PJB( 9)*W( 34)+PJB(10)*W( 35)+PJB(11)*W( 36)+PJB(12)*W( 39)
C    1+PJB(13)*W( 37)+PJB(14)*W( 38)+PJB(15)*W( 39)+PJB(16)*W( 40)
C     SUMB( 5)=
C    1+PJB( 1)*W( 41)+PJB( 2)*W( 42)+PJB( 3)*W( 44)+PJB( 4)*W( 47)
C    1+PJB( 5)*W( 42)+PJB( 6)*W( 43)+PJB( 7)*W( 45)+PJB( 8)*W( 48)
C    1+PJB( 9)*W( 44)+PJB(10)*W( 45)+PJB(11)*W( 46)+PJB(12)*W( 49)
C    1+PJB(13)*W( 47)+PJB(14)*W( 48)+PJB(15)*W( 49)+PJB(16)*W( 50)
C     SUMB( 6)=
C    1+PJB( 1)*W( 51)+PJB( 2)*W( 52)+PJB( 3)*W( 54)+PJB( 4)*W( 57)
C    1+PJB( 5)*W( 52)+PJB( 6)*W( 53)+PJB( 7)*W( 55)+PJB( 8)*W( 58)
C    1+PJB( 9)*W( 54)+PJB(10)*W( 55)+PJB(11)*W( 56)+PJB(12)*W( 59)
C    1+PJB(13)*W( 57)+PJB(14)*W( 58)+PJB(15)*W( 59)+PJB(16)*W( 60)
C     SUMB( 7)=
C    1+PJB( 1)*W( 61)+PJB( 2)*W( 62)+PJB( 3)*W( 64)+PJB( 4)*W( 67)
C    1+PJB( 5)*W( 62)+PJB( 6)*W( 63)+PJB( 7)*W( 65)+PJB( 8)*W( 68)
C    1+PJB( 9)*W( 64)+PJB(10)*W( 65)+PJB(11)*W( 66)+PJB(12)*W( 69)
C    1+PJB(13)*W( 67)+PJB(14)*W( 68)+PJB(15)*W( 69)+PJB(16)*W( 70)
C     SUMB( 8)=
C    1+PJB( 1)*W( 71)+PJB( 2)*W( 72)+PJB( 3)*W( 74)+PJB( 4)*W( 77)
C    1+PJB( 5)*W( 72)+PJB( 6)*W( 73)+PJB( 7)*W( 75)+PJB( 8)*W( 78)
C    1+PJB( 9)*W( 74)+PJB(10)*W( 75)+PJB(11)*W( 76)+PJB(12)*W( 79)
C    1+PJB(13)*W( 77)+PJB(14)*W( 78)+PJB(15)*W( 79)+PJB(16)*W( 80)
C     SUMB( 9)=
C    1+PJB( 1)*W( 81)+PJB( 2)*W( 82)+PJB( 3)*W( 84)+PJB( 4)*W( 87)
C    1+PJB( 5)*W( 82)+PJB( 6)*W( 83)+PJB( 7)*W( 85)+PJB( 8)*W( 88)
C    1+PJB( 9)*W( 84)+PJB(10)*W( 85)+PJB(11)*W( 86)+PJB(12)*W( 89)
C    1+PJB(13)*W( 87)+PJB(14)*W( 88)+PJB(15)*W( 89)+PJB(16)*W( 90)
C     SUMB(10)=
C    1+PJB( 1)*W( 91)+PJB( 2)*W( 92)+PJB( 3)*W( 94)+PJB( 4)*W( 97)
C    1+PJB( 5)*W( 92)+PJB( 6)*W( 93)+PJB( 7)*W( 95)+PJB( 8)*W( 98)
C    1+PJB( 9)*W( 94)+PJB(10)*W( 95)+PJB(11)*W( 96)+PJB(12)*W( 99)
C    1+PJB(13)*W( 97)+PJB(14)*W( 98)+PJB(15)*W( 99)+PJB(16)*W(100)
C                 I=0
C                 DO 100 I5=1,4
C                 IIA=IA+I5-1
C                 IJA=JA+I5-1
C                 IOFF=(IIA*(IIA-1))/2+IA-1
C                 JOFF=(IJA*(IJA-1))/2+JA-1
C                 DO 100 I6=1,I5
C                 IOFF=IOFF+1
C                 JOFF=JOFF+1
C                       I=I+1
C                       F(IOFF)=F(IOFF)+SUMB(I)
C 100             F(JOFF)=F(JOFF)+SUMA(I)
C the previous line is the end of the scalar code
      RETURN
 9999 RETURN 1                                                          CSTP
      END
