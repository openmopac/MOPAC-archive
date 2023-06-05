      SUBROUTINE GOVER(NI,NJ,XI,XJ,R,SG,*)                              CSTP
************************************************************************
*                                                                      *
*   GOVER CALCULATES THE OVERLAP INTEGRALS USING A GAUSSIAN EXPANSION  *
*         STO-6G BY R.F. STEWART, J. CHEM. PHYS., 52 431-438, 1970     *
*                                                                      *
*         ON INPUT   NI   =  ATOMIC NUMBER OF FIRST ATOM               *
*                    NJ   =  ATOMIC NUMBER OF SECOND ATOM              *
*                    R    =  INTERATOMIC DISTANCE IN ANGSTROMS         *
*         ON EXIT    S    =  9X9 ARRAY OF OVERLAPS, IN ORDER S,PX,PY,  *
*                            PZ                                        *
*                                                                      *
************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      COMMON /NATYPE/ NZTYPE(120), MTYPE(30),LTYPE
      COMMON /TEMP/  C(60,6), Z(60,6)
      COMMON /TEMP2/ CCSPEC(10,6), ZZSPEC(10,6)                         LF1010
      COMMON /NATORB/ NATORB(120)
      include 'pmodsb.f'
      DIMENSION S(6,6), XI(3),XJ(3), SG(9,9)
CSAV         SAVE                                                           GL0892
      DATA NGAUSS/6/
C
C    FIND START AND END OF GAUSSIAN
C
c#      write(6,*) "GOVER:"
c#      write(6,*) "NI, NJ=",NI,NJ
c#      write(6,*) "R =    ",R
      IFA=NZTYPE(NI)*4-3
      IF(C(IFA+1,1).NE.0.D0)THEN
         ILA=IFA+3
      ELSE
         ILA=IFA
      ENDIF
      IFB=NZTYPE(NJ)*4-3
      IF(C(IFB+1,1).NE.0.D0)THEN
         ILB=IFB+3
      ELSE
         ILB=IFB
      ENDIF
C     LF1010: Find start and end of Gaussian for the case of Hp-Hp and O-O pairs
C     (only when the MOD1 option is being used).
C     Of course Hp and O atoms already have p orbitals and so the last index is
C     three greater than the first index value, but explicitly specifying it
C     is provided for clarity here.  The reason for this really is because the
C     subroutine will look at the second coefficient element for Hp and O atoms
C     but will not be looking in CCSPEC, rather it looks in the C array which is
C     not modified with the special exponents for Hp-Hp and O-O atom pairs.
      IF (PMODS(1)) THEN
         IF (NI.EQ.9.AND.NJ.EQ.9) THEN
            ILA=IFA+3
            ILB=IFB+3
         ELSEIF (NI.EQ.8.AND.NJ.EQ.8) THEN
            ILA=IFA+3
            ILB=IFB+3
         ENDIF
      ENDIF
C     End LF1010 modifications.
C
C  CONVERT R INTO AU
C
      R=R/0.529167D0
      R = R**2
      KA=0
      DO 80 I=IFA,ILA
         KA=KA+1
         NAT=KA-1
         KB=0
         DO 80 J=IFB,ILB
            KB=KB+1
            NBT=KB-1
C
C         DECIDE IS IT AN S-S, S-P, P-S, OR P-P OVERLAP
C
            IF(NAT.GT.0.AND.NBT.GT.0) THEN
C    P-P
               IS=4
               TOMB=(XI(NAT)-XJ(NAT))*(XI(NBT)
     &                -XJ(NBT))*3.5711928576D0
            ELSEIF(NAT.GT.0) THEN
C    P-S
               IS=3
               TOMB=(XI(NAT)-XJ(NAT))*1.88976D0
            ELSEIF(NBT.GT.0) THEN
C    S-P
               IS=2
               TOMB=(XI(NBT)-XJ(NBT))*1.88976D0
            ELSE
C    S-S
               IS=1
            ENDIF
            DO 60 K=1,NGAUSS
               DO 60 L=1,NGAUSS
                  S(K,L)=0.0D0
C LF1010:
                  IF (PMODS(1).AND.(NI.EQ.9.AND.NJ.EQ.9)) THEN
                    AMB=ZZSPEC(KA,K)+ZZSPEC(KB,L)
                    APB=ZZSPEC(KA,K)*ZZSPEC(KB,L)
                  ELSEIF (PMODS(1).AND.(NI.EQ.8.AND.NJ.EQ.8)) THEN
                    AMB=ZZSPEC(KA+4,K)+ZZSPEC(KB+4,L)
                    APB=ZZSPEC(KA+4,K)*ZZSPEC(KB+4,L)
                  ELSE
C End LF1010 changes.
                    AMB=Z(I,K)+Z(J,L)
                    APB=Z(I,K)*Z(J,L)
                  ENDIF
                  ADB=APB/AMB
C
C           CHECK IF OVERLAP IS NON-ZERO BEFORE STARTING
C
                  IF((ADB*R).LT.90.D0) THEN
                     ABN=1.0D0
                     GO TO(50,10,20,30),IS
   10                ABN=2.*TOMB*Z(I,K)*SQRT(Z(J,L))/AMB
C LF1010:
                    IF (PMODS(1).AND.NI.EQ.9.AND.NJ.EQ.9) THEN
                     ABN=2.*TOMB*ZZSPEC(KA,K)*SQRT(ZZSPEC(KB,L))/AMB
                    ELSEIF (PMODS(1).AND.NI.EQ.8.AND.NJ.EQ.8) THEN
                     ABN=2.*TOMB*ZZSPEC(KA+4,K)*SQRT(ZZSPEC(KB+4,L))/AMB
                    ENDIF
C End LF1010 changes.
                     GO TO 50
   20                ABN=-2.*TOMB*Z(J,L)*SQRT(Z(I,K))/AMB
C LF1010:
                   IF (PMODS(1).AND.NI.EQ.9.AND.NJ.EQ.9) THEN
                    ABN=-2.*TOMB*ZZSPEC(KB,L)*SQRT(ZZSPEC(KA,K))/AMB  
                   ELSEIF (PMODS(1).AND.NI.EQ.8.AND.NJ.EQ.8) THEN
                    ABN=-2.*TOMB*ZZSPEC(KB+4,L)*SQRT(ZZSPEC(KA+4,K))/AMB
                   ENDIF
C End LF1010 changes.
                     GO TO 50
   30                ABN=-ADB*TOMB
                     IF(NAT.EQ.NBT) ABN=ABN+0.5D0
   40                ABN=4.0D0*ABN*SQRT(APB)/AMB
   50                S(K,L)=SQRT((2.*SQRT(APB)/AMB)**3)*EXP(-ADB*R)*ABN
                  ENDIF
   60       CONTINUE
            SG(KA,KB)=0.0D0
            DO 70 K=1,NGAUSS
               DO 70 L=1,NGAUSS
                  SG(KA,KB)=SG(KA,KB)+S(K,L)*C(I,K)*C(J,L)
C LF1010:
C                  IF (PMODS(1)) THEN
C                     IF (NI.EQ.9.AND.NJ.EQ.9) THEN
C         SG(KA,KB)=SG(KA,KB)+S(K,L)*CCSPEC(KA,K)*CCSPEC(KB,L)*0.00D0
C                     ELSEIF (NI.EQ.8.AND.NJ.EQ.8) THEN
C         SG(KA,KB)=SG(KA,KB)+S(K,L)*CCSPEC(KA+4,K)*CCSPEC(KB+4,L)*0.00D0
C                     ELSE
C         SG(KA,KB)=SG(KA,KB)+S(K,L)*C(I,K)*C(J,L)
C                     ENDIF
C                  ELSE
C                     SG(KA,KB)=SG(KA,KB)+S(K,L)*C(I,K)*C(J,L)
C                  ENDIF
C End LF1010 changes.
   70       CONTINUE
   80 CONTINUE
c#      write(6,*) "S matrix from GOVER: ",NI,NJ,R                           debug LF
c#      write(6,'(3X,2X,9(8X,I2,5X))') (MYCT,MYCT=1,9)                    debug LF
c# 123  format(3X,I2,9(3X,F12.9))                                         debug LF
c#      do 124 myct=1,9                                                   debug LF
c# 124  write(6,123) myct,(s(myct,myct2),myct2=1,9)                       debug LF
      RETURN
 9999 RETURN 1                                                          CSTP
      END
