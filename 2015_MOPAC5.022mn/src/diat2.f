      SUBROUTINE DIAT2(NA,ESA,EPA,R12,NB,ESB,EPB,S,*)                   CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION S(3,3,3)
C***********************************************************************
C
C OVERLP CALCULATES OVERLAPS BETWEEN ATOMIC ORBITALS FOR PAIRS OF ATOMS
C        IT CAN HANDLE THE ORBITALS 1S, 2S, 3S, 2P, AND 3P.
C
C***********************************************************************
      COMMON /SETC/ A(7),B(7),SA,SB,FACTOR,ISP,IPS
      COMMON /HPUSED/ HPUSED                                            LF0410
      LOGICAL   HPUSED                                                  LF0410
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1010
      include 'corgen.f'   ! Declaration for /CORGEN/ common block.     LF0211
      INTEGER           baseindex, iel1, iel2                           LF0211
      DOUBLE PRECISION  kss, ksp, kps, kpp, bss, bsp, bps, bpp          LF0211
      DIMENSION INMB(120),III(91)                                       LF0410
CSAV         SAVE                                                           GL0892
C     LF0410: Reorganized INMB data into periodic table format.
C             Also expanded table to elements beyond n=3 row of periodic table.
C             Transition metals and f-block elements are ignored 
C             for now by setting INMB values to zero.
C     DATA INMB/1,0,2,2,3,4,5,6,7,0,8,8,8,9,10,11,12/
      DATA INMB/  1,                                              0,    LF0410
     2            2, 2,                        3,  4,  5,  6,  7, 0,    LF0410
     3            8, 8,                        8,  9, 10, 11, 12, 0,    LF0410
     4            8, 8,          10*0,         8,  9, 10, 11, 12, 0,    LF0410
     5            8, 8,          10*0,         8,  9, 10, 11, 12, 0,    LF0410
     6            8, 8,      14*0, 10*0,       8,  9, 10, 11, 12, 0,    LF0410
     7            8, 8,      14*0,  5*0,                                LF0410
     8            0, 0, 0,                                              LF0410
     9            0, 0, 0, 0, 0, 0, 0, 0, 0, 0/                         LF0410
C     NUMBERING CORRESPONDS TO BOND TYPE MATRIX GIVEN ABOVE
C      THE CODE IS
C
C      II=1      FIRST - FIRST  ROW ELEMENTS
C        =2      FIRST - SECOND
C        =3      FIRST - THIRD
C        =4      SECOND - SECOND
C        =5      SECOND - THIRD
C        =6      THIRD - THIRD
C        =7      Hp - FIRST
C        =8      Hp - SECOND
C        =9      Hp - Hp
C        =10     Hp - THIRD
C
C     DATA III/1,2,4,   2,4,4,   2,4,4,4,   2,4,4,4,4,
C    1 2,4,4,4,4,4,   2,4,4,4,4,4,4,   3,5,5,5,5,5,5,6,
C    2 3,5,5,5,5,5,5,6,6,   3,5,5,5,5,5,5,6,6,6,   3,5,5,5,5,5,5,6,6,6,6
C    3, 3,5,5,5,5,5,5,6,6,6,6,6/
C
C     LF0410: Reorganized the III data in lower triangular format.
C     LF0410: Added line for Hp atoms (INMB value=13) to III data table.
C                1  2  3  4  5  6  7  8  9  10 11 12 13
      DATA III/  1,                                                     1   H 
     &           2, 4,                                                  2   He
     &           2, 4, 4,                                               3   Li
     &           2, 4, 4, 4,                                            4   Be
     &           2, 4, 4, 4, 4,                                         5   B 
     &           2, 4, 4, 4, 4, 4,                                      6   C 
     &           2, 4, 4, 4, 4, 4, 4,                                   7   N 
     &           3, 5, 5, 5, 5, 5, 5, 6,                                8   O 
     &           3, 5, 5, 5, 5, 5, 5, 6, 6,                             9   F 
     &           3, 5, 5, 5, 5, 5, 5, 6, 6, 6,                          10  Ne
     &           3, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6,                       11  Na
     &           3, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6,                    12  Mg
     &           7, 8, 8, 8, 8, 8, 8,10,10,10,10,10, 9/                 13  Hp
C
      IF (HPUSED) INMB(9)=13                                            LF0410
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
C
C     For ordering of S matrix elements treat element 9 (Hp here) the
C     same as element 1 (hydrogen) which it is replacing.  Done to
C     maintain consistency between S matrix in subroutines DIAT2 and SET:
C
      NHPA=NA
      NHPB=NB
      IF (HPUSED) THEN                                                  LF0410
       IF (NA.EQ.9) NHPA=1                                              LF0410
       IF (NB.EQ.9) NHPB=1                                              LF0410
      ENDIF                                                             LF0410
C

c#           write(6,'(I2,A,I2,A,I4)') NA," and ",NB," go to ",10*(II+1)

      GOTO (20,30,40,50,60,70,80,90,100,110), II                        LF0410
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
   20 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      S(1,1,1)=.25D00*SQRT((SA*SB*RAB*RAB)**3)*(A(3)*B(1)-B(3)*A(1))
      RETURN
C
C *** FIRST ROW - SECOND ROW OVERLAPS
C
   30 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      W=SQRT((SA**3)*(SB**5))*(RAB**4)*0.125D00
      S(1,1,1) = SQRT(1.D00/3.D00)
      S(1,1,1)=W*S(1,1,1)*(A(4)*B(1)-B(4)*A(1)+A(3)*B(2)-B(3)*A(2))
      IF (NA.GT.1) CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II,*9999)           CSTP(call)
      IF (NB.GT.1) CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II,*9999)           CSTP(call)
      W=SQRT((SA**3)*(SB**5))*(RAB**4)*0.125D00
      S(ISP,IPS,1)=W*(A(3)*B(1)-B(3)*A(1)+A(4)*B(2)-B(4)*A(2))
      RETURN
C
C *** FIRST ROW - THIRD ROW OVERLAPS
C
   40 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      W=SQRT((SA**3)*(SB**7)/7.5D00)*(RAB**5)*0.0625D00
      SROOT3 = SQRT(3.D00)
      S(1,1,1)=W*(A(5)*B(1)-B(5)*A(1)+
     12.D00*(A(4)*B(2)-B(4)*A(2)))/SROOT3
      IF (NA.GT.1) CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II,*9999)           CSTP(call)
      IF (NB.GT.1) CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II,*9999)           CSTP(call)
      W=SQRT((SA**3)*(SB**7)/7.5D00)*(RAB**5)*0.0625D00
      S(ISP,IPS,1)=W*(A(4)*(B(1)+B(3))-B(4)*(A(1)+A(3))+
     1B(2)*(A(3)+A(5))-A(2)*(B(3)+B(5)))
      RETURN
C
C *** SECOND ROW - SECOND ROW OVERLAPS
C
   50 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00
      RT3=1.D00/SQRT(3.D00)
      S(1,1,1)=W*(A(5)*B(1)+B(5)*A(1)-2.0D00*A(3)*B(3))/3.0D00
      CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      IF (NA.GT.NB) CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II,*9999)          CSTP(call)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00
      D=A(4)*(B(1)-B(3))-A(2)*(B(3)-B(5))
      E=B(4)*(A(1)-A(3))-B(2)*(A(3)-A(5))
      S(ISP,IPS,1)=W*RT3*(D+E)
      CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      IF (NA.GT.NB) CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II,*9999)          CSTP(call)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00
      D=A(4)*(B(1)-B(3))-A(2)*(B(3)-B(5))
      E=B(4)*(A(1)-A(3))-B(2)*(A(3)-A(5))
      S(IPS,ISP,1)=-W*RT3*(E-D)

c#        write(6,'(A,2I6)') "IPS, ISP=",IPS,ISP
c#        write(6,'(A,F14.7)') "S(IPS,ISP,1)=",S(IPS,ISP,1)

      CALL SET (EPA,EPB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00
      S(2,2,1)=-W*(B(3)*(A(5)+A(1))-A(3)*(B(5)+B(1)))
      HD = .5D00
      S(2,2,2)=HD*W*(A(5)*(B(1)-B(3))-B(5)*(A(1)-A(3))
     1-A(3)*B(1)+B(3)*A(1))
      RETURN
C
C *** SECOND ROW - THIRD ROW OVERLAPS
C
   60 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      W=SQRT((SA**5)*(SB**7)/7.5D00)*(RAB**6)*0.03125D00
      RT3 = 1.D00 / SQRT(3.D00)
      TD = 2.D00
      S(1,1,1)=W*(A(6)*B(1)+A(5)*B(2)-TD*(A(4)*B(3)+
     1A(3)*B(4))+A(2)*B(5)+A(
     21)*B(6))/3.D00
      CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      IF (NA.GT.NB) CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II,*9999)          CSTP(call)
      W=SQRT((SA**5)*(SB**7)/7.5D00)*(RAB**6)*0.03125D00
      TD = 2.D00
      S(ISP,IPS,1)=W*RT3*(A(6)*B(2)+A(5)*B(1)-TD*(A(4)*B(4)+A(3)*B(3))
     1+A(2)*B(6)+A(1)*B(5))
      CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      IF (NA.GT.NB) CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II,*9999)          CSTP(call)
      W=SQRT((SA**5)*SB**7/7.5D00)*(RAB**6)*0.03125D00
      TD = 2.D00
      S(IPS,ISP,1)=-W*RT3*(A(5)*(TD*B(3)-B(1))-B(5)*(TD*A(3)-A(1))-A(2
     1)*(B(6)-TD*B(4))+B(2)*(A(6)-TD*A(4)))
      CALL SET (EPA,EPB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
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
   70 CALL SET (ESA,ESB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      W=SQRT((SA*SB*RAB*RAB)**7)/480.D00
      RT3 = 1.D00 / SQRT(3.D00)
      S(1,1,1)=W*(A(7)*B(1)-3.D00*(A(5)*B(3)-A(3)*B(5))-A(1)*B(7))/3.D00
      CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      IF (NA.GT.NB) CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II,*9999)          CSTP(call)
      W=SQRT((SA*SB*RAB*RAB)**7)/480.D00
      D=A(6)*(B(1)-B(3))-2.D00*A(4)*(B(3)-B(5))+A(2)*(B(5)-B(7))
      E=B(6)*(A(1)-A(3))-2.D00*B(4)*(A(3)-A(5))+B(2)*(A(5)-A(7))
      S(ISP,IPS,1)=W*RT3*(D-E)
      CALL SET (EPA,ESB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
      IF (NA.GT.NB) CALL SET (ESA,EPB,NA,NB,RAB,NBOND,II,*9999)          CSTP(call)
      W=SQRT((SA*SB*RAB*RAB)**7)/480.D00
      D=A(6)*(B(1)-B(3))-2.D00*A(4)*(B(3)-B(5))+A(2)*(B(5)-B(7))
      E=B(6)*(A(1)-A(3))-2.D00*B(4)*(A(3)-A(5))+B(2)*(A(5)-A(7))
      S(IPS,ISP,1)=-W*RT3*(-D-E)
      CALL SET (EPA,EPB,NA,NB,RAB,NBOND,II,*9999)                        CSTP(call)
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
C *** Hp ATOM - FIRST ROW OVERLAPS                                      LF0410
C
   80 CONTINUE                                                          LF0410
C
C         < 1s(Hp) | 1s(1st row) >
C         Mirrors block 20 above.
C
      CALL SET (ESA,ESB,NA,NB,RAB,NBOND,1,*9999)                        LF0410 CSTP(call)
      S(1,1,1)=.25D00*SQRT((SA*SB*RAB*RAB)**3)*(A(3)*B(1)-B(3)*A(1))
C
C         < 2p-sigma(Hp) | 1s(1st row) > 
C         Mirrors block 30 above but last SET argument is 2.
C
      IF (NA.EQ.9) CALL SET (EPA,ESB,NA,NB,RAB,NBOND,2,*9999)           LF0410 CSTP(call)
      IF (NB.EQ.9) CALL SET (ESA,EPB,NA,NB,RAB,NBOND,2,*9999)           LF0410 CSTP(call)
      W=SQRT((SA**3)*(SB**5))*(RAB**4)*0.125D00
      S(ISP,IPS,1)=W*(A(3)*B(1)-B(3)*A(1)+A(4)*B(2)-B(4)*A(2))

c#       write(6,'(A,F14.7)') "S(1,1,1)=",S(1,1,1)
c#       write(6,'(A,F14.7)') "S(2,1,1)=",S(2,1,1)
c#       write(6,'(A,F14.7)') "S(1,2,1)=",S(1,2,1)
c#       write(6,'(A,F14.7)') "S(2,2,1)=",S(2,2,1)
c#       write(6,'(A,F14.7)') "S(2,2,2)=",S(2,2,2)

      GOTO 2000
C
C *** Hp ATOM - SECOND ROW OVERLAPS                                     LF0410
C
   90 CONTINUE                                                          LF0410
C
C         < 1s(Hp) | 2s(2nd row) >
C         Mirrors that from block 30 above.
C
      CALL SET (ESA,ESB,NHPA,NHPB,RAB,NBOND,2,*9999)                    LF0410 CSTP(call)
      W=SQRT((SA**3)*(SB**5))*(RAB**4)*0.125D00
      S(1,1,1) = SQRT(1.D00/3.D00)
      S(1,1,1)=W*S(1,1,1)*(A(4)*B(1)-B(4)*A(1)+A(3)*B(2)-B(3)*A(2))
C
C         < 1s(Hp) | 2p-sigma(2nd row) >
C         Mirrors that from block 30 above.
C
      IF (NHPA.GT.1) CALL SET (EPA,ESB,NHPA,NHPB,RAB,NBOND,2,*9999)     LF0410 CSTP(call)
      IF (NHPB.GT.1) CALL SET (ESA,EPB,NHPA,NHPB,RAB,NBOND,2,*9999)     LF0410 CSTP(call)
      W=SQRT((SA**3)*(SB**5))*(RAB**4)*0.125D00
      S(ISP,IPS,1)=W*(A(3)*B(1)-B(3)*A(1)+A(4)*B(2)-B(4)*A(2))
c#        write(6,'(A,2I6)') "ISP, IPS=",ISP,IPS
c#        write(6,'(A,F14.7)') "S(ISP,IPS,1)=",S(ISP,IPS,1)

C
C         < 2p-sigma(Hp) | 2s(2nd row) >
C         Mirrors that from block 50 above.
C
      RT3=1.D00/SQRT(3.D00)
      CALL SET (EPA,ESB,NHPA,NHPB,RAB,NBOND,4,*9999)                    LF0410 CSTP(call)
      IF (NHPA.GT.NHPB) CALL SET (ESA,EPB,NHPA,NHPB,RAB,NBOND,4,*9999)  LF0410 CSTP(call)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00
      D=A(4)*(B(1)-B(3))-A(2)*(B(3)-B(5))
      E=B(4)*(A(1)-A(3))-B(2)*(A(3)-A(5))
      S(IPS,ISP,1)=-W*RT3*(E-D)
c#      write(6,*) "D,E,S:",D,E,S(IPS,ISP,1)

c#        write(6,'(A,2I6)') "IPS, ISP=",IPS,ISP
c#        write(6,'(A,F14.7)') "S(IPS,ISP,1)=",S(IPS,ISP,1)

C
C         < 2p-sigma(Hp) | 2p-sigma(2nd row) >
C         < 2p-pi(Hp) | 2p-pi(2nd row) >
C         Mirrors that from block 50 above.
C
      CALL SET (EPA,EPB,NHPA,NHPB,RAB,NBOND,4,*9999)                    LF0410 CSTP(call)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00
      S(2,2,1)=-W*(B(3)*(A(5)+A(1))-A(3)*(B(5)+B(1)))
      HD = 0.5D0
      S(2,2,2)=HD*W*(A(5)*(B(1)-B(3))-B(5)*(A(1)-A(3))
     &         -A(3)*B(1)+B(3)*A(1))

c#       write(6,'(A,F14.7)') "S(1,1,1)=",S(1,1,1)
c#       write(6,'(A,F14.7)') "S(2,1,1)=",S(2,1,1)
c#       write(6,'(A,F14.7)') "S(1,2,1)=",S(1,2,1)
c#       write(6,'(A,F14.7)') "S(2,2,1)=",S(2,2,1)
c#       write(6,'(A,F14.7)') "S(2,2,2)=",S(2,2,2)

      GOTO 2000
C
C *** Hp ATOM - Hp ATOM OVERLAPS                                        LF0410
C
  100 CONTINUE                                                          LF0410
C
C         < 1s(Hp) | 1s(Hp) >
C         Mirrors that from block 20 above.
C
      CALL SET (ESA,ESB,NHPA,NHPB,RAB,NBOND,1,*9999)                    LF0410 CSTP(call)
      S(1,1,1)=.25D00*SQRT((SA*SB*RAB*RAB)**3)*(A(3)*B(1)-B(3)*A(1))
C
C         < 1s(Hp) | 2p-sigma(Hp) >
C         < 2p-sigma(Hp) | 1s(Hp) >
C         Taken from block 30 above.
C         No need for dual IF statements calling SET subroutine.
C
      CALL SET (ESA,EPB,NHPA,NHPB,RAB,NBOND,2,*9999)                    LF0410 CSTP(call)
      W=SQRT((SA**3)*(SB**5))*(RAB**4)*0.125D00
      S(1,2,1)=W*(A(3)*B(1)-B(3)*A(1)+A(4)*B(2)-B(4)*A(2))
      S(2,1,1)=S(1,2,1)
C
C         < 2p-sigma(Hp) | 2p-sigma(Hp) >
C         < 2p-pi(Hp) | 2p-pi(Hp) >
C         Taken from block 50 above.
C
      CALL SET (EPA,EPB,NHPA,NHPB,RAB,NBOND,4,*9999)                    LF0410 CSTP(call)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D00
      S(2,2,1)=-W*(B(3)*(A(5)+A(1))-A(3)*(B(5)+B(1)))
      HD = 0.5D0
      S(2,2,2)=HD*W*(A(5)*(B(1)-B(3))-B(5)*(A(1)-A(3))
     &         -A(3)*B(1)+B(3)*A(1))

c#       write(6,'(A,F14.7)') "S(1,1,1)=",S(1,1,1)
c#       write(6,'(A,F14.7)') "S(2,1,1)=",S(2,1,1)
c#       write(6,'(A,F14.7)') "S(1,2,1)=",S(1,2,1)
c#       write(6,'(A,F14.7)') "S(2,2,1)=",S(2,2,1)
c#       write(6,'(A,F14.7)') "S(2,2,2)=",S(2,2,2)

      GOTO 2000
C
C *** Hp ATOM - THIRD ROW OVERLAPS                                      LF0410
C
  110 CONTINUE                                                          LF0410
C
C         < 1s(Hp) | 3s(3nd row) >
C         < 1s(Hp) | 3p-sigma(3rd row) >
C         Mirrors that from block 40 above.
C
      CALL SET (ESA,ESB,NHPA,NHPB,RAB,NBOND,3,*9999)                    LF0410 CSTP(call)
      W=SQRT((SA**3)*(SB**7)/7.5D00)*(RAB**5)*0.0625D00
      SROOT3 = SQRT(3.D00)
      S(1,1,1)=W*(A(5)*B(1)-B(5)*A(1)
     &          +2.D00*(A(4)*B(2)-B(4)*A(2)))/SROOT3
      IF (NHPA.GT.1) CALL SET (EPA,ESB,NHPA,NHPB,RAB,NBOND,3,*9999)     LF0410 CSTP(call)
      IF (NHPB.GT.1) CALL SET (ESA,EPB,NHPA,NHPB,RAB,NBOND,3,*9999)     LF0410 CSTP(call)
      W=SQRT((SA**3)*(SB**7)/7.5D00)*(RAB**5)*0.0625D00
      S(ISP,IPS,1)=W*(A(4)*(B(1)+B(3))-B(4)*(A(1)+A(3))
     &             +B(2)*(A(3)+A(5))-A(2)*(B(3)+B(5)))
C
C         < 2p-sigma(Hp) | 3s(3nd row) >
C         Mirrors that from block 60 above.
C
      RT3=1.D00/SQRT(3.D00)
      CALL SET (EPA,ESB,NHPA,NHPB,RAB,NBOND,5,*9999)                    LF0410 CSTP(call)
      IF (NHPA.GT.NHPB) CALL SET (ESA,EPB,NHPA,NHPB,RAB,NBOND,5,*9999)  LF0410 CSTP(call)
      W=SQRT((SA**5)*SB**7/7.5D00)*(RAB**6)*0.03125D00
      TD = 2.D00
      S(IPS,ISP,1)= -W*RT3*(A(5)*(TD*B(3)-B(1))-B(5)*(TD*A(3)-A(1))
     &              -A(2)*(B(6)-TD*B(4))+B(2)*(A(6)-TD*A(4)))
C
C         < 2p-sigma(Hp) | 3p-sigma(3nd row) >
C         < 2p-pi(Hp) | 3p-pi(3nd row) >
C         Mirrors that from block 60 above.
C
      CALL SET (EPA,EPB,NHPA,NHPB,RAB,NBOND,5,*9999)                    LF0410 CSTP(call)
      W=SQRT((SA**5)*SB**7/7.5D00)*(RAB**6)*0.03125D00
      S(2,2,1)= -W*(B(4)*(A(1)+A(5))-A(4)*(B(1)+B(5))
     &          +B(3)*(A(2)+A(6))-A(3)*(B(2)+B(6)))
      HD = 0.5D0
      S(2,2,2)=HD*W*(A(6)*(B(1)-B(3))-B(6)*(A(1)
     &         -A(3))+A(5)*(B(2)-B(4))-B(5)
     &         *(A(2)-A(4))-A(4)*B(1)+B(4)*A(1)-A(3)*B(2)+B(3)*A(2))

c#       write(6,'(A,F14.7)') "S(1,1,1)=",S(1,1,1)
c#       write(6,'(A,F14.7)') "S(2,1,1)=",S(2,1,1)
c#       write(6,'(A,F14.7)') "S(1,2,1)=",S(1,2,1)
c#       write(6,'(A,F14.7)') "S(2,2,1)=",S(2,2,1)
c#       write(6,'(A,F14.7)') "S(2,2,2)=",S(2,2,2)

      GOTO 2000
C


 2000 CONTINUE
C *** Handle the "MOD7" option if it is being used. (LF0211)
C     (SKIP OVER IF NOT USING THE MODIFICATION OPTION #6)
      IF (.NOT.PMODS(7)) GOTO 2100
           bss=prbeta(na,nb,1,1)                                        LF0811
           bpp=prbeta(na,nb,2,2)                                        LF0811
           bsp=prbeta(na,nb,1,2)                                        LF0811
           bps=prbeta(na,nb,2,1)                                        LF0811
           kss=kpair (na,nb,1,1)                                        LF0811
           kpp=kpair (na,nb,2,2)                                        LF0811
           ksp=kpair (na,nb,1,2)                                        LF0811
           kps=kpair (na,nb,2,1)                                        LF0811
c#           write(6,*) "KPR__ values for atom pair:",na,nb
c#           write(6,*) "Using Bss: ",bss
c#           write(6,*) "Using Bsp: ",bsp
c#           write(6,*) "Using Bps: ",bps
c#           write(6,*) "Using Bpp: ",bpp
c#           write(6,*) "Using Kss: ",kss
c#           write(6,*) "Using Ksp: ",ksp
c#           write(6,*) "Using Kps: ",kps
c#           write(6,*) "Using Kpp: ",kpp
c#           write(6,*) "s(1,1,1)=  ",s(1,1,1)
c#           write(6,*) "s(1,2,1)=  ",s(1,2,1)
c#           write(6,*) "s(2,1,1)=  ",s(2,1,1)
c#           write(6,*) "s(2,2,1)=  ",s(2,2,1)
c#           write(6,*) "s(2,2,2)=  ",s(2,2,2)
C       n.b.: RAB is in units of bohr now.
        S(1,1,1)=S(1,1,1)*bss*EXP(kss*RAB)  ! Apply MOD7 formula.
        S(1,2,1)=S(1,2,1)*bsp*EXP(ksp*RAB)
        S(2,1,1)=S(2,1,1)*bps*EXP(kps*RAB)
        S(2,2,1)=S(2,2,1)*bpp*EXP(kpp*RAB)
        S(2,2,2)=S(2,2,2)*bpp*EXP(kpp*RAB)
c#           write(6,*) "s(1,1,1)=  ",s(1,1,1)
c#           write(6,*) "s(1,2,1)=  ",s(1,2,1)
c#           write(6,*) "s(2,1,1)=  ",s(2,1,1)
c#           write(6,*) "s(2,2,1)=  ",s(2,2,1)
c#           write(6,*) "s(2,2,2)=  ",s(2,2,2)
      GOTO 230
C *** End of MOD7 section. (LF0211)

C *** Handle the "MOD1" option if it is being used for Hp-X
C     atom pairs.
C     SKIP OVER IF NOT USING THE MODIFICATION OPTION #1:
 2100 CONTINUE
      IF (.NOT.PMODS(1)) GOTO 230

      IF (NA.EQ.9.AND.NB.EQ.9) THEN                                     PZ0410
         S(2,1,1)=0.0D0     ! so beta_p(Hp)s(Hp')=0 for "MOD1"
         S(1,2,1)=0.0D0     ! so beta_s(Hp)p(Hp')=0 for "MOD1"
         S(2,2,1)=0.0D0     ! so beta_p(Hp)p(Hp')=0 for "MOD1" (both sigma p - on internuclear axis)
         S(2,2,2)=0.0D0     ! so beta_p(Hp)p(Hp')=0 for "MOD1" (both pi p - both orthogonal to internuclear axis)
      ENDIF
      
      DUMP1=PMODVL(3)                                                   PZ0410
      DUMP2=PMODVL(4)
C     Note that MOD6 modification only works with the MOD1 modification turned on.
      IF (PMODS(6)) THEN                                                ZP0111/LF0111
         RDEPBT=EXP(PMODVL(7)*RAB)                                      ZP0111/LF0111
         DUMP1=DUMP1*RDEPBT                                             ZP0111/LF0111
         DUMP2=DUMP2*RDEPBT                                             ZP0111/LF0111
      ENDIF                                                             ZP0111/LF0111
      IF(HPUSED.AND.NA.EQ.9.AND.NB.EQ.8) THEN
c#               write(6,*) "Hp-O atom pair in DIAT2:"
c#               write(6,*) "RAB=",RAB
c#               write(6,*) "DUMP1=",DUMP1
c#               write(6,*) "DUMP2=",DUMP2
         S(1,1,1)=S(1,1,1)          ! leave beta_s(Hp)s(O)
         S(1,2,1)=S(1,2,1)          ! leave beta_s(Hp)p(O)
         S(2,1,1)=S(2,1,1)*DUMP1    ! multiply MOD constant #3 into beta_p(Hp)s(O) for "MOD1"
         S(2,2,1)=S(2,2,1)*DUMP2    ! multiply MOD constant #4 into beta_p(Hp)p(O) for "MOD1" (both sigma p)
         S(2,2,2)=S(2,2,2)*DUMP2    ! multiply MOD constant #4 into beta_p(Hp)p(O) for "MOD1" (both pi p)
      ENDIF
      IF(HPUSED.AND.NA.EQ.8.AND.NB.EQ.9) THEN
c#               write(6,*) "O-Hp atom pair in DIAT2:"
c#               write(6,*) "RAB=",RAB
c#               write(6,*) "DUMP1=",DUMP1
c#               write(6,*) "DUMP2=",DUMP2
         S(1,1,1)=S(1,1,1)          ! leave beta_s(O)s(Hp)
         S(2,1,1)=S(2,1,1)          ! leave beta_s(O)s(Hp)
         S(1,2,1)=S(1,2,1)*DUMP1    ! multiply MOD constant #3 into beta_p(O)s(Hp) for "MOD1"
         S(2,2,1)=S(2,2,1)*DUMP2    ! multiply MOD constant #4 into beta_p(O)p(Hp) for "MOD1" (both sigma p)
         S(2,2,2)=S(2,2,2)*DUMP2    ! multiply MOD constant #4 into beta_p(O)p(Hp) for "MOD1" (both pi p)
      ENDIF

C#       write(6,'(A,F14.7)') "S(1,1,1)=",S(1,1,1)
C#       write(6,'(A,F14.7)') "S(2,1,1)=",S(2,1,1)
C#       write(6,'(A,F14.7)') "S(1,2,1)=",S(1,2,1)
C#       write(6,'(A,F14.7)') "S(2,2,1)=",S(2,2,1)
C#       write(6,'(A,F14.7)') "S(2,2,2)=",S(2,2,2)

C     Here use GA-modifiable parameters for the reduction factors of    LJF1110
C     the Hp(p)-C(s) and Hp(p)-C(p) overlap integrals:                  LJF1110
      DUMP1=PMODVL(3)                                                   PF0410
      DUMP2=PMODVL(4)
C     The MOD1 modification was abandoned when PMO was expanded beyond PMOv1 and began
C     including carbon, so the following MOD1 modifications are therefore obsolete:  (LF0412)
Cobs      IF(HPUSED.AND.NA.EQ.9.AND.NB.EQ.6) THEN
Cobs         S(1,1,1)=S(1,1,1)
Cobs         S(1,2,1)=S(1,2,1)
Cobs         S(2,1,1)=S(2,1,1)*DUMP1
Cobs         S(2,2,1)=S(2,2,1)*DUMP2
Cobs         S(2,2,2)=S(2,2,2)*DUMP2
Cobs      ENDIF
Cobs      IF(HPUSED.AND.NA.EQ.6.AND.NB.EQ.9) THEN
Cobs         S(1,1,1)=S(1,1,1)
Cobs         S(2,1,1)=S(2,1,1)
Cobs         S(1,2,1)=S(1,2,1)*DUMP1
Cobs         S(2,2,1)=S(2,2,1)*DUMP2
Cobs         S(2,2,2)=S(2,2,2)*DUMP2
Cobs      ENDIF

  230 CONTINUE

      RETURN
 9999 RETURN 1                                                          CSTP
      END
