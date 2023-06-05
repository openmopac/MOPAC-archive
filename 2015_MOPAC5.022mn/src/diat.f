      SUBROUTINE DIAT(NI,NJ,XI,XJ,DI,*)                                 CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*
*   DIAT CALCULATES THE DI-ATOMIC OVERLAP INTEGRALS BETWEEN ATOMS
*        CENTERED AT XI AND XJ.
*
*   ON INPUT NI  = ATOMIC NUMBER OF THE FIRST ATOM.
*            NJ  = ATOMIC NUMBER OF THE SECOND ATOM.
*            XI  = CARTESIAN COORDINATES OF THE FIRST ATOM.
*            XJ  = CARTESIAN COORDINATES OF THE SECOND ATOM.
*
*  ON OUTPUT DI  = DIATOMIC OVERLAP, IN A 9 * 9 MATRIX. LAYOUT OF
*                  ATOMIC ORBITALS IN DI IS
*                  1   2   3   4   5            6     7       8     9
*                  S   PX  PY  PZ  D(X**2-Y**2) D(XZ) D(Z**2) D(YZ)D(XY)
*
*   LIMITATIONS:  IN THIS FORMULATION, NI AND NJ MUST BE LESS THAN 107
*         EXPONENTS ARE ASSUMED TO BE PRESENT IN COMMON BLOCK EXPONT.
*
************************************************************************
      COMMON /KEYWRD/KEYWRD
      CHARACTER*160 KEYWRD
      INTEGER A,PQ2,B,PQ1,AA,BB
      LOGICAL ANALYT
      COMMON /EXPONT/ EMUS(120),EMUP(120),EMUD(120)
      DIMENSION DI(9,9),S(3,3,3),UL1(3),UL2(3),C(3,5,5),NPQ(120)
     1          ,XI(3),XJ(3), SLIN(27), IVAL(3,5)
     2, C1(3,5), C2(3,5), C3(3,5), C4(3,5), C5(3,5)
     3, S1(3,3), S2(3,3), S3(3,3)
      EQUIVALENCE(SLIN(1),S(1,1,1))
      EQUIVALENCE (C1(1,1),C(1,1,1)), (C2(1,1),C(1,1,2)),
     1            (C3(1,1),C(1,1,3)), (C4(1,1),C(1,1,4)),
     2            (C5(1,1),C(1,1,5)), (S1(1,1),S(1,1,1)),
     3            (S2(1,1),S(1,1,2)), (S3(1,1),S(1,1,3))
CSAV         SAVE                                                           GL0892
      DATA NPQ/1,0, 2,2,2,2,2,2,2,0, 0,3,3,3,3,3,3,0, 0,4,4,4,4,4,4,4,
     14,4,4,4,4,4,4,4,4,0, 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
     2,32*6,15*0,3,5*0
     3,13*0/                                                            LF0710
      DATA IVAL/1,0,9,1,3,8,1,4,7,1,2,6,0,0,5/
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1010
      include 'corgen.f'   ! Common block definition for /CORGEN/.      LF0211
      DOUBLE PRECISION KSS, KSP, KPS, KPP                               LF0211
      DOUBLE PRECISION BSS, BSP, BPS, BPP                               LF0211
      ANALYT= (INDEX(KEYWRD,'ANALYT').NE.0) 
      X1=XI(1)
      X2=XJ(1)
      Y1=XI(2)
      Y2=XJ(2)
      Z1=XI(3)
      Z2=XJ(3)
      PQ1=NPQ(NI)
      PQ2=NPQ(NJ)
      DO 20 I=1,9
         DO 10 J=1,9
            DI(I,J)=0.0D0
   10    CONTINUE
   20 CONTINUE
      CALL COE(X1,Y1,Z1,X2,Y2,Z2,PQ1,PQ2,C,R,*9999)                      CSTP(call)
      IF(PQ1.EQ.0.OR.PQ2.EQ.0.OR.R.GE.10.D0) RETURN
      IF(R.LT.0.001)THEN
         RETURN
      ENDIF
      IA=MIN(PQ1,3)
      IB=MIN(PQ2,3)
      A=IA-1
      B=IB-1
c      goto 888   ! Use Slater overlap instead of STO-6G with this GOTO statement. LF0111
c#      IF (PMODS(6)) goto 888  ! ... to use Slater overlap instead if desired.
      IF(ANALYT.OR.PMODS(1).OR.PMODS(7))THEN
C        GOVER changes the distance argument, so use a copy to send to it:
         RDIST=R   
c#         write(6,*) "Calling GOVER."
         CALL GOVER(NI,NJ,XI,XJ,RDIST,DI,*9999)                          CSTP(call)
c#         WRITE(6,*)' OVERLAP FROM GOVER (just after call)'
c#         DO 813 I=1,4
c#  813     WRITE(6,'(4F15.10)')(DI(J,I),J=1,4)
C        Handle MOD7 keyword modifications here:
         if (pmods(7)) then                                              LF0211
           bss=prbeta(ni,nj,1,1)                                        LF0811
           bpp=prbeta(ni,nj,2,2)                                        LF0811
           bsp=prbeta(ni,nj,1,2)                                        LF0811
           bps=prbeta(ni,nj,2,1)                                        LF0811
           kss=kpair (ni,nj,1,1)                                        LF0811
           kpp=kpair (ni,nj,2,2)                                        LF0811
           ksp=kpair (ni,nj,1,2)                                        LF0811
           kps=kpair (ni,nj,2,1)                                        LF0811
c#           write(6,*) "Using Bss: ",bss
c#           write(6,*) "Using Bsp: ",bsp
c#           write(6,*) "Using Bps: ",bps
c#           write(6,*) "Using Bpp: ",bpp
c#           write(6,*) "Using Kss: ",kss
c#           write(6,*) "Using Ksp: ",ksp
c#           write(6,*) "Using Kps: ",kps
c#           write(6,*) "Using Kpp: ",kpp
C          n.b.: R is in units of Angstroms here.
           DI(1,1)=DI(1,1)*bss*EXP(kss*R/0.529177d0)  ! Multiply in MOD7 changes: pairwise B factor and exponential factor using pairwise k's.
           DI(1,2)=DI(1,2)*bsp*EXP(ksp*R/0.529177d0)  ! Do for s/p orbital combinations.
           DI(1,3)=DI(1,3)*bsp*EXP(ksp*R/0.529177d0)
           DI(1,4)=DI(1,4)*bsp*EXP(ksp*R/0.529177d0)
           DI(2,1)=DI(2,1)*bps*EXP(kps*R/0.529177d0)  ! Do for p/s orbital combinations.
           DI(3,1)=DI(3,1)*bps*EXP(kps*R/0.529177d0)
           DI(4,1)=DI(4,1)*bps*EXP(kps*R/0.529177d0)
           DI(2,2)=DI(2,2)*bpp*EXP(kpp*R/0.529177d0)  ! Do for p/p orbital combinations.
           DI(2,3)=DI(2,3)*bpp*EXP(kpp*R/0.529177d0)
           DI(2,4)=DI(2,4)*bpp*EXP(kpp*R/0.529177d0)
           DI(3,2)=DI(3,2)*bpp*EXP(kpp*R/0.529177d0)
           DI(3,3)=DI(3,3)*bpp*EXP(kpp*R/0.529177d0)
           DI(3,4)=DI(3,4)*bpp*EXP(kpp*R/0.529177d0)
           DI(4,2)=DI(4,2)*bpp*EXP(kpp*R/0.529177d0)
           DI(4,3)=DI(4,3)*bpp*EXP(kpp*R/0.529177d0)
           DI(4,4)=DI(4,4)*bpp*EXP(kpp*R/0.529177d0)
           goto 800  ! Skip MOD1 and MOD6 code if doing MOD7.
         endif
C        End of MOD7 keyword modifications section.
C        Handle MOD1 keyword modifications next:
         IF (PMODS(1)) THEN
            IF (NI.EQ.9.AND.NJ.EQ.9) THEN
C              Keep only Hp(s)-Hp(s) interactions.
               DI(1,2)=0.0D0
               DI(1,3)=0.0D0
               DI(1,4)=0.0D0
               DI(2,1)=0.0D0
               DI(2,2)=0.0D0
               DI(2,3)=0.0D0
               DI(2,4)=0.0D0
               DI(3,1)=0.0D0
               DI(3,2)=0.0D0
               DI(3,3)=0.0D0
               DI(3,4)=0.0D0
               DI(4,1)=0.0D0
               DI(4,2)=0.0D0
               DI(4,3)=0.0D0
               DI(4,4)=0.0D0
            ELSEIF (NI.EQ.9.AND.NJ.EQ.6) THEN
C              Hp(s)-C(s) and Hp(s)-C(p) interactions not scaled.
C              Hp(p)-C(s) interactions scaled by PMODVL(3).
C              Hp(p)-C(p) interactions scaled by PMODVL(4).
               DI(2,1)=DI(2,1)*PMODVL(3)
               DI(2,2)=DI(2,2)*PMODVL(4)
               DI(2,3)=DI(2,3)*PMODVL(4)
               DI(2,4)=DI(2,4)*PMODVL(4)
               DI(3,1)=DI(3,1)*PMODVL(3)
               DI(3,2)=DI(3,2)*PMODVL(4)
               DI(3,3)=DI(3,3)*PMODVL(4)
               DI(3,4)=DI(3,4)*PMODVL(4)
               DI(4,1)=DI(4,1)*PMODVL(3)
               DI(4,2)=DI(4,2)*PMODVL(4)
               DI(4,3)=DI(4,3)*PMODVL(4)
               DI(4,4)=DI(4,4)*PMODVL(4)
            ELSEIF (NI.EQ.6.AND.NJ.EQ.9) THEN
C              C(s)-Hp(s) and C(p)-Hp(s) interactions not scaled.
C              C(s)-Hp(p) interactions scaled by PMODVL(3).
C              C(p)-Hp(p) interactions scaled by PMODVL(4).
               DI(1,2)=DI(1,2)*PMODVL(3)
               DI(1,3)=DI(1,3)*PMODVL(3)
               DI(1,4)=DI(1,4)*PMODVL(3)
               DI(2,2)=DI(2,2)*PMODVL(4)
               DI(2,3)=DI(2,3)*PMODVL(4)
               DI(2,4)=DI(2,4)*PMODVL(4)
               DI(3,2)=DI(3,2)*PMODVL(4)
               DI(3,3)=DI(3,3)*PMODVL(4)
               DI(3,4)=DI(3,4)*PMODVL(4)
               DI(4,2)=DI(4,2)*PMODVL(4)
               DI(4,3)=DI(4,3)*PMODVL(4)
               DI(4,4)=DI(4,4)*PMODVL(4)
            ELSEIF (NI.EQ.9.AND.NJ.EQ.8) THEN
C              Hp(s)-O(s) and Hp(s)-O(p) interactions not scaled.
C              Hp(p)-O(s) interactions scaled by PMODVL(3).
C              Hp(p)-O(p) interactions scaled by PMODVL(4).
               DUMP1=PMODVL(3)                                          ZP0410/LF0111
               DUMP2=PMODVL(4)                                          ZP0410/LF0111
               IF (PMODS(6)) THEN                                       ZP0111/LF0111
                  RDEPBT=       EXP(PMODVL(7)*R/0.529177D0)             ZP0111/LF0111
                  DUMP1=DUMP1*RDEPBT                                    ZP0111/LF0111
                  DUMP2=DUMP2*RDEPBT                                    ZP0111/LF0111
               ENDIF                                                    ZP0111/LF0111
               DI(2,1)=DI(2,1)*DUMP1
               DI(2,2)=DI(2,2)*DUMP2    
               DI(2,3)=DI(2,3)*DUMP2    
               DI(2,4)=DI(2,4)*DUMP2    
               DI(3,1)=DI(3,1)*DUMP1    
               DI(3,2)=DI(3,2)*DUMP2    
               DI(3,3)=DI(3,3)*DUMP2    
               DI(3,4)=DI(3,4)*DUMP2    
               DI(4,1)=DI(4,1)*DUMP1    
               DI(4,2)=DI(4,2)*DUMP2    
               DI(4,3)=DI(4,3)*DUMP2    
               DI(4,4)=DI(4,4)*DUMP2    
            ELSEIF (NI.EQ.8.AND.NJ.EQ.9) THEN
C              O(s)-Hp(s) and O(p)-Hp(s) interactions not scaled.
C              O(s)-Hp(p) interactions scaled by PMODVL(3).
C              O(p)-Hp(p) interactions scaled by PMODVL(4).
               DUMP1=PMODVL(3)                                          ZP0410/LF0111
               DUMP2=PMODVL(4)                                          ZP0410/LF0111
               IF (PMODS(6)) THEN                                       ZP0111/LF0111
                  RDEPBT=       EXP(PMODVL(7)*R/0.529177D0)             ZP0111/LF0111
                  DUMP1=DUMP1*RDEPBT                                    ZP0111/LF0111
                  DUMP2=DUMP2*RDEPBT                                    ZP0111/LF0111
               ENDIF                                                    ZP0111/LF0111
               DI(1,2)=DI(1,2)*DUMP1    
               DI(1,3)=DI(1,3)*DUMP1    
               DI(1,4)=DI(1,4)*DUMP1    
               DI(2,2)=DI(2,2)*DUMP2    
               DI(2,3)=DI(2,3)*DUMP2    
               DI(2,4)=DI(2,4)*DUMP2    
               DI(3,2)=DI(3,2)*DUMP2    
               DI(3,3)=DI(3,3)*DUMP2    
               DI(3,4)=DI(3,4)*DUMP2    
               DI(4,2)=DI(4,2)*DUMP2    
               DI(4,3)=DI(4,3)*DUMP2    
               DI(4,4)=DI(4,4)*DUMP2    
            ENDIF
         ENDIF
 800     CONTINUE
c#         write(6,*) "Called GOVER: (after mods stuff done to S matrix)"
c#         WRITE(6,'(A,I2,A,I2,A)') "ATOM INDICES ",NI,", ",NJ,"."
c#         WRITE(6,'(A,3F10.3)') "COORDINATES #1: ",(XI(IDX),IDX=1,3)
c#         WRITE(6,'(A,3F10.3)') "COORDINATES #2: ",(XJ(IDX),IDX=1,3)
c#         WRITE(6,*)' OVERLAP FROM GOVER'
c#         DO 13 I=1,4
c#  13     WRITE(6,'(4F15.10)')(DI(J,I),J=1,4)
         RETURN
      ENDIF
 888  CONTINUE 
      IF(NI.LT.18.AND.NJ.LT.18) THEN
         CALL DIAT2(NI,EMUS(NI),EMUP(NI),R,NJ,EMUS(NJ),EMUP(NJ),S,*9999) CSTP(call)
      ELSE
         UL1(1)=EMUS(NI)
         UL2(1)=EMUS(NJ)
         UL1(2)=EMUP(NI)
         UL2(2)=EMUP(NJ)
         UL1(3)=MAX(EMUD(NI),0.3D0)
         UL2(3)=MAX(EMUD(NJ),0.3D0)
         DO 30 I=1,27
   30    SLIN(I)=0.0D0
         NEWK=MIN(A,B)
         NK1=NEWK+1
         DO 40 I=1,IA
            ISS=I
            IB=B+1
            DO 40 J=1,IB
               JSS=J
               DO 40 K=1,NK1
                  IF(K.GT.I.OR.K.GT.J) GOTO 40
                  KSSTMP=K
                  S(I,J,K)=SS(PQ1,PQ2,ISS,JSS,KSSTMP,UL1(I),UL2(J),R)    IR0295
   40    CONTINUE
      ENDIF
      DO 50 I=1,IA
         KMIN=4-I
         KMAX=2+I
         DO 50 J=1,IB
            IF(J.EQ.2)THEN
               AA=-1
               BB=1
            ELSE
               AA=1
               IF(J.EQ.3) THEN
                  BB=-1
               ELSE
                  BB=1
               ENDIF
            ENDIF
            LMIN=4-J
            LMAX=2+J
            DO 50 K=KMIN,KMAX
               DO 50 L=LMIN,LMAX
                  II=IVAL(I,K)
                  JJ=IVAL(J,L)
                  DI(II,JJ)=S1(I,J)*C3(I,K)*C3(J,L)*AA+
     1(C4(I,K)*C4(J,L)+C2(I,K)*C2(J,L))*BB*S2(I,J)+(C5(I,K)*C5(J,L)
     2+C1(I,K)*C1(J,L))*S3(I,J)
   50 CONTINUE
c#      WRITE(6,'(A,I2,A,I2,A)') "ATOM INDICES ",NI,", ",NJ,"."              debug LF
c#      WRITE(6,'(A,3F10.3)') "COORDINATES #1: ",(XI(IDX),IDX=1,3)           debug LF
c#      WRITE(6,'(A,3F10.3)') "COORDINATES #2: ",(XJ(IDX),IDX=1,3)           debug LF
c#      WRITE(6,*)' OVERLAP FROM DIAT2'                                      debug LF
c#      DO 12 I=1,4                                                          debug LF
c#  12  WRITE(6,'(4F15.10)')(DI(J,I),J=1,4)                                  debug LF
c#      WRITE(6,*)' S OVERLAP FROM DIAT2'                                    debug LF
c#      DO 14 I=1,3                                                          debug LF
c#  14  WRITE(6,'(4F15.10)')(S(J,I,1),J=1,3)                                 debug LF
      RETURN
 9999 RETURN 1                                                          CSTP
      END
