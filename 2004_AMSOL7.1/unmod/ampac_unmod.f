      DOUBLE PRECISION FUNCTION BABBCD(IOCCA1, IOCCB1, IOCCA2, 
     1                                 IOCCB2, NMOS) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION IOCCA1(NMOS), IOCCB1(NMOS), IOCCA2(NMOS), IOCCB2(NMOS) 
       INCLUDE 'SIZES.i'
*********************************************************************** 
* 
* BABBCD EVALUATES THE C.I. MATRIX ELEMENT FOR TWO MICROSTATES DIFFERING 
*       BY TWO BETA MOS. ONE MICROSTATE HAS BETA ELECTRONS IN 
*       M.O.S PSI(I) AND PSI(J) FOR WHICH THE OTHER MICROSTATE HAS 
*       ELECTRONS IN PSI(K) AND PSI(L) 
* 
*********************************************************************** 
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI) 
       SAVE
      IJ=0 
      DO 10 I=1,NMOS 
   10 IF(IOCCB1(I) .LT. IOCCB2(I)) GOTO 20 
   20 DO 30 J=I+1,NMOS 
         IF(IOCCB1(J) .LT. IOCCB2(J)) GOTO 40 
   30 IJ=IJ+IOCCA2(J)+IOCCB2(J) 
   40 IJ=IJ+IOCCA2(J) 
      DO 50 K=1,NMOS 
   50 IF(IOCCB1(K) .GT. IOCCB2(K)) GOTO 60 
   60 DO 70 L=K+1,NMOS 
         IF(IOCCB1(L) .GT. IOCCB2(L)) GOTO 80 
   70 IJ=IJ+IOCCA1(L)+IOCCB1(L) 
   80 IJ=IJ+IOCCA1(L) 
      IF((IJ/2)*2.EQ.IJ) THEN 
         ONE=1.D0 
      ELSE 
         ONE=-1.D0 
      ENDIF 
      BABBCD=(XY(I,K,J,L)-XY(I,L,J,K))*ONE 
      RETURN 
      END 
      SUBROUTINE BANGLE(XYZ,I,J,K,ANGLE) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION XYZ(3,*) 
********************************************************************* 
* 
* BANGLE CALCULATES THE ANGLE BETWEEN ATOMS I,J, AND K. THE 
*        CARTESIAN COORDINATES ARE IN XYZ. 
* 
********************************************************************* 
      SAVE
      D2IJ = (XYZ(1,I)-XYZ(1,J))**2+ 
     1       (XYZ(2,I)-XYZ(2,J))**2+ 
     2       (XYZ(3,I)-XYZ(3,J))**2 
      D2JK = (XYZ(1,J)-XYZ(1,K))**2+ 
     1       (XYZ(2,J)-XYZ(2,K))**2+ 
     2       (XYZ(3,J)-XYZ(3,K))**2 
      IF(D2IJ*D2JK.LT.1.D-30) THEN 
         ANGLE=0.D0 
      ELSE 
         D2IK = (XYZ(1,I)-XYZ(1,K))**2+ 
     1          (XYZ(2,I)-XYZ(2,K))**2+ 
     2          (XYZ(3,I)-XYZ(3,K))**2 
         ANGLE=ACOS( 
     .     MAX(-1.D0,MIN(0.5D0*(D2IJ+D2JK-D2IK)/SQRT(D2IJ*D2JK),1.D0)) ) 
      ENDIF 
      RETURN 
      END 
      SUBROUTINE CNVG(PNEW, P, P1,NORBS, NITER, PL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P1(*), P(*), PNEW(*)
      LOGICAL EXTRAP
C***********************************************************************
C
C  CNVG IS A TWO-POINT INTERPOLATION ROUTINE FOR SPEEDING CONVERGENCE
C       OF THE DENSITY MATRIX.
C
C ON OUTPUT P      = NEW DENSITY MATRIX
C           P1     = DIAGONAL OF OLD DENSITY MATRIX
C           PL     = LARGEST DIFFERENCE BETWEEN OLD AND NEW DENSITY
C                    MATRICES
C***********************************************************************
       SAVE
      PL=0.0D00
      FACA=0.0D00
      DAMP=1.D10
      IF(NITER.GT.3)DAMP=0.05D0
      FACB=0.0D00
      FAC=0.0D00
      II=MOD(NITER,3)
      EXTRAP=II.NE.0
      K=0
      DO 20 I=1,NORBS
         K=K+I
         A=PNEW(K)
         SA=ABS(A-P(K))
         IF (SA.GT.PL) PL=SA
         IF (EXTRAP) GO TO 10
         FACA=FACA+SA**2
         FACB=FACB+(A-2.D00*P(K)+P1(I))**2
   10    P1(I)=P(K)
   20 P(K)=A
      IF (FACB.LE.0.0D00) GO TO 30
      IF (FACA.LT.(100.D00*FACB)) FAC=SQRT(FACA/FACB)
   30 IE=0
      DO 50 I=1,NORBS
         II=I-1
         DO 40 J=1,II
            IE=IE+1
            A=PNEW(IE)
            P(IE)=A+FAC*(A-P(IE))
            PNEW(IE)=P(IE)
   40    CONTINUE
         IE=IE+1
         IF(ABS(P(IE)-P1(I)) .GT. DAMP) THEN
            P(IE)=P1(I)+SIGN(DAMP,P(IE)-P1(I))
         ELSE
            P(IE)=P(IE)+FAC*(P(IE)-P1(I))
         ENDIF
         P(IE)=MIN(2.D0,MAX(P(IE),0.D0))
   50 PNEW(IE)=P(IE)
      RETURN
      END
      SUBROUTINE COE(X1,Y1,Z1,X2,Y2,Z2,PQ1,PQ2,C,R) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      INTEGER PQ1,PQ2,PQ,CO 
      DIMENSION C(75) 
       SAVE
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
      SUBROUTINE DANG(A1,A2,B1,B2,RCOS) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
********************************************************************** 
* 
*    DANG  DETERMINES THE ANGLE BETWEEN THE POINTS (A1,A2), (0,0), 
*          AND (B1,B2).  THE RESULT IS PUT IN RCOS, BETWEEN -PI AND PI. 
*          USE TRIGONOMETRIC CONVENTION. 
* 
********************************************************************** 
      SAVE
      ANORM=SQRT(A1**2+A2**2) 
      BNORM=SQRT(B1**2+B2**2) 
      IF(ANORM.LT.1.D-30 .OR. BNORM.LT.1.D-30) THEN 
         RCOS=0.D0 
         ELSE 
         RCOS=ACOS(MAX(-1.D0,MIN((A1*B1+A2*B2)/(ANORM*BNORM),1.D0)) ) 
         IF(A1*B2.LT.A2*B1) RCOS=-RCOS 
      ENDIF 
      RETURN 
      END 
      DOUBLE PRECISION FUNCTION DIAGI(IALPHA,IBETA,EIGA,XY,NMOS) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
       INCLUDE 'SIZES.i'
      DIMENSION XY(NMECI,NMECI,NMECI,NMECI), EIGA(NMECI), 
     1IALPHA(NMOS), IBETA(NMOS) 
       SAVE
************************************************************************ 
* 
*  CALCULATES THE ENERGY OF A MICROSTATE DEFINED BY IALPHA AND IBETA 
* 
************************************************************************ 
      X=0.0D0 
      DO 20 I=1,NMOS 
         IF (IALPHA(I).NE.0)THEN 
            X=X+EIGA(I) 
            DO 10  J=1,NMOS 
               X=X+((XY(I,I,J,J)-XY(I,J,I,J))*IALPHA(J)*0.5D0 + 
     1        (XY(I,I,J,J)            )*IBETA(J)) 
   10       CONTINUE 
         ENDIF 
   20 CONTINUE 
      DO 40 I=1,NMOS 
         IF (IBETA(I).NE.0) THEN 
            X=X+EIGA(I) 
            DO 30 J=1,I 
   30       X=X+(XY(I,I,J,J)-XY(I,J,I,J))*IBETA(J) 
         ENDIF 
   40 CONTINUE 
      DIAGI=X 
      RETURN 
      END 
      SUBROUTINE DIAT(NI,NJ,XI,XJ,DI) 
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
      INTEGER A,PQ2,B,PQ1,AA,BB,YETA 
      COMMON /EXPONT/ EMUS(107),EMUP(107),EMUD(107) 
      COMMON /ALPTM / ALPTM(30), EMUDTM(30) 
      DIMENSION DI(9,9),S(3,3,3),UL1(3),UL2(3),C(3,5,5),NPQ(107) 
     1          ,XI(3),XJ(3), SLIN(27), IVAL(3,5) 
     2, C1(3,5), C2(3,5), C3(3,5), C4(3,5), C5(3,5) 
     3, S1(3,3), S2(3,3), S3(3,3) 
      EQUIVALENCE(SLIN(1),S(1,1,1)) 
      EQUIVALENCE (C1(1,1),C(1,1,1)), (C2(1,1),C(1,1,2)), 
     1            (C3(1,1),C(1,1,3)), (C4(1,1),C(1,1,4)), 
     2            (C5(1,1),C(1,1,5)), (S1(1,1),S(1,1,1)), 
     3            (S2(1,1),S(1,1,2)), (S3(1,1),S(1,1,3)) 
       SAVE
      DATA NPQ/1,0, 2,2,2,2,2,2,2,0, 3,3,3,3,3,3,3,0, 4,4,4,4,4,4,4,4, 
     14,4,4,4,4,4,4,4,4,0, 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5 
     1,32*6,21*0/ 
      DATA IVAL/1,0,9,1,3,8,1,4,7,1,2,6,0,0,5/ 
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
      CALL COE(X1,Y1,Z1,X2,Y2,Z2,PQ1,PQ2,C,R) 
      IF(PQ1.EQ.0.OR.PQ2.EQ.0.OR.R.GE.10.D0) RETURN 
      IF(R.LT.0.1)THEN 
         RETURN 
      ENDIF 
      IA=MIN(PQ1,3) 
      IB=MIN(PQ2,3) 
      A=IA-1 
      B=IB-1 
      IF(PQ1.LT.3.AND.PQ2.LT.3) THEN 
         CALL DIAT2(NI,EMUS(NI),EMUP(NI),R,NJ,EMUS(NJ),EMUP(NJ),S) 
      ELSE 
         UL1(1)=EMUS(NI) 
         UL2(1)=EMUS(NJ) 
         UL1(2)=EMUP(NI) 
         UL2(2)=EMUP(NJ) 
         UL1(3)=EMUD(NI) 
         UL2(3)=EMUD(NJ) 
C 
      IF (NI.EQ.24.AND.NJ.EQ.24) THEN 
         UL1(3)=EMUDTM(24) 
         UL2(3)=EMUDTM(24) 
      ENDIF 
C 
         DO 30 I=1,27 
   30    SLIN(I)=0.0D0 
         NEWK=MIN(A,B) 
         NK1=NEWK+1 
         YETA=PQ1+PQ2+3 
         DO 40 I=1,IA 
            ISS=I 
            IB=B+1 
            DO 40 J=1,IB 
               JSS=J 
               DO 40 K=1,NK1 
                  IF(K.GT.I.OR.K.GT.J) GOTO 40 
                  KSS=K 
                  S(I,J,K)=SS(PQ1,PQ2,ISS,JSS,KSS,UL1(I),UL2(J),R,YETA)  IR1294
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
      RETURN 
      END 
      SUBROUTINE DIHED(XYZ,I,J,K,L,ANGLE) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION XYZ(3,*) 
********************************************************************* 
* 
*      DIHED CALCULATES THE DIHEDRAL ANGLE BETWEEN ATOMS I, J, K, 
*            AND L.  THE CARTESIAN COORDINATES OF THESE ATOMS 
*            ARE IN ARRAY XYZ. 
* 
*     DIHED IS A MODIFIED VERSION OF A SUBROUTINE OF THE SAME NAME 
*           WHICH WAS WRITTEN BY DR. W. THEIL IN 1973. 
*           (USE CLOCKWISE CONVENTION) 
********************************************************************* 
      SAVE
      XI1=XYZ(1,I)-XYZ(1,K) 
      XJ1=XYZ(1,J)-XYZ(1,K) 
      XL1=XYZ(1,L)-XYZ(1,K) 
      YI1=XYZ(2,I)-XYZ(2,K) 
      YJ1=XYZ(2,J)-XYZ(2,K) 
      YL1=XYZ(2,L)-XYZ(2,K) 
      ZI1=XYZ(3,I)-XYZ(3,K) 
      ZJ1=XYZ(3,J)-XYZ(3,K) 
      ZL1=XYZ(3,L)-XYZ(3,K) 
C     ROTATE AROUND Z AXIS TO PUT KJ ALONG Y AXIS 
      DIST= SQRT(XJ1**2+YJ1**2+ZJ1**2) 
      IF(DIST.LT.1.D-30) THEN 
         ANGLE=0.D0 
         RETURN 
      ENDIF 
      COSA=MAX(-1.D0,MIN(ZJ1/DIST,1.D0)) 
      IF (1.D0-ABS(COSA).LT.1.D-30) THEN 
         XI2=XI1 
         XL2=XL1 
         YI2=YI1 
         YL2=YL1 
         COSTH=COSA 
         SINTH=0.D0 
      ELSE 
         YXDIST=DIST*SQRT(1.D0-COSA**2) 
         COSPH=YJ1/YXDIST 
         SINPH=XJ1/YXDIST 
         XI2=XI1*COSPH-YI1*SINPH 
         XJ2=XJ1*COSPH-YJ1*SINPH 
         XL2=XL1*COSPH-YL1*SINPH 
         YI2=XI1*SINPH+YI1*COSPH 
         YJ2=XJ1*SINPH+YJ1*COSPH 
         YL2=XL1*SINPH+YL1*COSPH 
         COSTH=COSA 
         SINTH=YJ2/DIST 
      ENDIF 
C     ROTATE KJ AROUND THE X AXIS SO KJ LIES ALONG THE Z AXIS 
      YI3=YI2*COSTH-ZI1*SINTH 
      YL3=YL2*COSTH-ZL1*SINTH 
      CALL DANG(XL2,YL3,XI2,YI3,ANGLE) 
      RETURN 
      END 
C*MODULE BLAS1   *DECK DSCAL
      SUBROUTINE  DSCAL(N,DA,DX,INCX)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DX(1)
C
C     SCALES A VECTOR BY A CONSTANT.
C           DX(I) = DA * DX(I)
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I + 1) = DA*DX(I + 1)
        DX(I + 2) = DA*DX(I + 2)
        DX(I + 3) = DA*DX(I + 3)
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE
      RETURN
      END
      SUBROUTINE EXCHNG (A,B,C,D,X,Y,T,Q,N) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION X(*), Y(*) 
C******************************************************************** 
C 
C THE CONTENTS OF A, C, T, AND X ARE STORED IN B, D, Q, AND Y] 
C 
C   THIS IS A DEDICATED ROUTINE, IT IS CALLED BY LINMIN AND LOCMIN ONLY. 
C 
C******************************************************************** 
       SAVE
      B=A 
      D=C 
      Q=T 
      DO 10 I=1,N 
   10 Y(I)=X(I) 
      RETURN 
C 
      END 
      DOUBLE PRECISION FUNCTION HELECT(N,P,H,F) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION P(*), H(*), F(*) 
       SAVE
C*********************************************************************** 
C 
C    SUBROUTINE CALCULATES THE ELECTRONIC ENERGY OF THE SYSTEM IN EV. 
C 
C    ON ENTRY N = NUMBER OF ATOMIC ORBITALS. 
C             P = DENSITY MATRIX, PACKED, LOWER TRIANGLE. 
C             H = ONE-ELECTRON MATRIX, PACKED, LOWER TRIANGLE. 
C             F = TWO-ELECTRON MATRIX, PACKED, LOWER TRIANGLE. 
C    ON EXIT 
C        HELECT = ELECTRONIC ENERGY. 
C 
C    NO ARGUMENTS ARE CHANGED. 
C 
C*********************************************************************** 
C     MODIFIED FOR OPTIMAL VECTORIZATION (D.L. JUNE 85) 
      LINEAR=N*(N+1)/2 
      HELECT=0.D0 
      K=0 
      DO 10 I=1,N 
      K=K+I 
   10 HELECT=HELECT+P(K)*(H(K)+F(K)) 
      HELECT=-0.5D0*HELECT 
      DO 20 I=1,LINEAR 
   20 HELECT=HELECT+P(I)*(H(I)+F(I)) 
      RETURN 
      END 
      SUBROUTINE MAMULT (F,P,N,FS,PS,FP,FPPF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     MAMULT (CRAY VERSION) IS DEDICATED TO ROUTINE 'PULAY'
C  INPUT
C     F        : FOCK MATRIX, PACKED CANONICAL
C     P        : DENSITY MATRIX, PACKED CANONICAL
C     N        : NUMBER OF ORBITALS
C     FS,PS,FP : WORK ARRAYS OF SIZE N*N
C  OUTPUT
C     FPPF     : COMMUTATOR FP-PF, LOWER TRIANGLE, PACKED CANONICAL
C
      DIMENSION F(*),P(*),FS(N,N),PS(N,N),FP(N,N),FPPF(*)
      SAVE
      L=0
      DO 10 I=1,N
CDIR$ IVDEP
      DO 10 J=1,I
      L=L+1
      FS(I,J)=F(L)
      FS(J,I)=F(L)
      PS(I,J)=P(L)
   10 PS(J,I)=P(L)
      CALL MXM (FS,N,PS,N,FP,N)
      L=0
      DO 20 I=1,N
      DO 20 J=1,I
      L=L+1
   20 FPPF(L)=FP(I,J)-FP(J,I)
      RETURN
      END
      SUBROUTINE MECID ( EIGS,GSE,EIGA,DIAG) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
       INCLUDE 'SIZES.i'
      DIMENSION EIGS(*),EIGA(*),DIAG(*) 
      COMMON /BASEOC/ OCCA(NMECI),KDUM(NMECI+2) 
     1       /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI) 
     2       /CIDATA/ VECTCI(MAXCI),XX,NELEC,NMOS,LAB                   DL0397
     3               ,NALPHA(MAXCI)                                     DL0397
     4               ,MICROA(NMECI,MAXCI),MICROB(NMECI,MAXCI)           DL0397
C 
C     EIGENVALUES AFTER REMOVAL OF 2-ELECT INTERACTION : EIGA 
C     GROUND STATE (VACUUM) ENERGY : GSE 
C     DIAGONAL ELEMENTS OF THE C.I.MATRIX : DIAG 
C 
       SAVE
      GSE=0.D0 
      DO 20 I=1,NMOS 
      X=0.0 
      DO 10 J=1,NMOS 
   10 X=X+(2.D0*XY(I,I,J,J)-XY(I,J,I,J))*OCCA(J) 
      EIGA(I)=EIGS(I+NELEC)-X 
      GSE=GSE+EIGA(I)*OCCA(I)*2.D0 
      GSE=GSE+XY(I,I,I,I)*OCCA(I)*OCCA(I) 
      DO 20 J=I+1,NMOS 
   20 GSE=GSE+2.D0*(2.D0*XY(I,I,J,J) - XY(I,J,I,J))*OCCA(I)*OCCA(J) 
C     DIAGONAL ELEMENTS OF C.I MATRIX 
      DO 30 I=1,LAB 
   30 DIAG(I)=DIAGI(MICROA(1,I),MICROB(1,I),EIGA,XY,NMOS)-GSE 
      RETURN 
      END 
      SUBROUTINE MECIH (DIAG,CIMAT) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
       INCLUDE 'SIZES.i'
      DIMENSION DIAG(*),CIMAT(*) 
C 
C     BUILD THE C.I. MATRIX 'CIMAT' IN PACKED CANONICAL FORM. 
C 
      COMMON /CIDATA/ VECTCI(MAXCI),XX,NELEC,NMOS,LAB                    DL0397
     1               ,NALPHA(MAXCI)                                      DL0397
     2               ,MICROA(NMECI,MAXCI),MICROB(NMECI,MAXCI)            DL0397
     3       /SPQR  / ISPQR(MAXCI,NMECI),IS,I,K                          DL0397
       SAVE
C 
      IK=0 
C 
C     OUTER LOOP TO FILL C.I. MATRIX. 
      DO 30 I=1,LAB 
         IS=2 
C 
C     INNER LOOP. 
         DO 20 K=1,I 
            IK=IK+1 
            CIMAT(IK)=0.D0 
            IX=0 
            IY=0 
            DO 10 J=1,NMOS 
            IX=IX+ABS(MICROA(J,I)-MICROA(J,K)) 
   10       IY=IY+ABS(MICROB(J,I)-MICROB(J,K)) 
C 
C                              CHECK IF MATRIX ELEMENT HAS TO BE ZERO 
C 
            IF(IX+IY.GT.4 .OR. NALPHA(I).NE.NALPHA(K)) GO TO 20 
            IF(IX+IY.EQ.4) THEN 
               IF(IX.EQ.0)THEN 
                  CIMAT(IK)=BABBCD(MICROA(1,I),MICROB(1,I) 
     .                            ,MICROA(1,K),MICROB(1,K),NMOS) 
               ELSE IF(IX.EQ.2) THEN 
                  CIMAT(IK)=AABBCD(MICROA(1,I),MICROB(1,I) 
     .                            ,MICROA(1,K),MICROB(1,K),NMOS) 
               ELSE 
                  CIMAT(IK)=AABACD(MICROA(1,I),MICROB(1,I) 
     .                            ,MICROA(1,K),MICROB(1,K),NMOS) 
               ENDIF 
            ELSE IF(IX.EQ.2) THEN 
               CIMAT(IK)=AABABC(MICROA(1,I),MICROB(1,I) 
     .                         ,MICROA(1,K),MICROB(1,K),NMOS) 
            ELSE IF(IY.EQ.2) THEN 
               CIMAT(IK)=BABBBC(MICROA(1,I),MICROB(1,I) 
     .                         ,MICROA(1,K),MICROB(1,K),NMOS) 
            ELSE 
               CIMAT(IK)=DIAG(I) 
            ENDIF 
   20    CONTINUE 
   30 ISPQR(I,1)=IS-1 
      RETURN 
      END 
      SUBROUTINE MTXMC (A,NAR,B,NBR,C) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     MATRIX PRODUCT C(NAR,NAR) = (A(NBR,NAR))' * B(NBR,NAR) 
C     A AND B RECTANGULAR , PACKED, 
C     C LOWER LEFT TRIANGLE ONLY, PACKED IN CANONICAL ORDER. 
      DIMENSION A(NBR,NAR),B(NBR,NAR),C(*) 
C  NOTE ... OPTIMUM VERSION ON CRAY-1. 
      L=1 
      DO 10 I=1,NAR 
      CALL MXM (A(1,I),1,B,NBR,C(L),I) 
   10 L=L+I 
      RETURN 
      END 
      SUBROUTINE MXMT (A,NAR,B,NBR,C,NCC) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     MATRIX PRODUCT C(NAR,NCC) = A(NAR,NBR) * (B(NCC,NBR))' 
C     ALL MATRICES RECTANGULAR , PACKED. 
C NOTE ... OPTIMUM VERSION ON CRAY-1. 
      DIMENSION A(NAR,NBR),B(NCC,NBR),C(NAR,NCC) 
      DO 10 I=1,NCC*NAR 
   10 C(I,1)=0.D0 
      DO 20 K=1,NBR 
      DO 20 J=1,NCC 
      DO 20 I=1,NAR 
   20 C(I,J)=C(I,J)+A(I,K)*B(J,K) 
      RETURN 
      END 
      SUBROUTINE NUCHAR(LINE,VALUE,NVALUE) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
************************************************************************ 
* 
*   NUCHAR  DETERMINS AND RETURNS THE REAL VALUES OF ALL NUMBERS 
*           FOUND IN 'LINE'. ALL CONNECTED SUBSTRINGS ARE ASSUMED 
*           TO CONTAIN NUMBERS 
*   ON ENTRY LINE    = CHARACTER STRING 
*   ON EXIT  VALUE   = ARRAY OF NVALUE REAL VALUES 
* 
************************************************************************ 
      DIMENSION VALUE(40),ISTART(40) 
      CHARACTER*80 LINE 
      CHARACTER*1 COMMA,SPACE 
      LOGICAL LEADSP 
      SAVE
      DATA COMMA,SPACE/',',' '/ 
* 
* CLEAN OUT COMMAS. (WARNING, TABS ARE NOT UNDERSTOOD IN EBCDIC) 
* 
      DO 10 I=1,80 
   10 IF(LINE(I:I).EQ.COMMA)LINE(I:I)=SPACE 
* 
* FIND INITIAL DIGIT OF ALL NUMBERS, CHECK FOR LEADING SPACES FOLLOWED 
*     BY A CHARACTER 
* 
      LEADSP=.TRUE. 
      NVALUE=0 
      DO 20 I=1,80 
         IF (LEADSP.AND.LINE(I:I).NE.SPACE) THEN 
            NVALUE=NVALUE+1 
            ISTART(NVALUE)=I 
         END IF 
         LEADSP=(LINE(I:I).EQ.SPACE) 
   20 CONTINUE 
* 
* FILL NUMBER ARRAY 
* 
      DO 30 I=1,NVALUE 
         VALUE(I)=READIF(LINE,ISTART(I)) 
   30 CONTINUE 
      RETURN 
      END 
      SUBROUTINE REPP(NI,NJ,RIJ,RI,CORE) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION RI(22),CORE(4,2) 
      COMMON /MULTIP/ DD(107),QQ(107),ADD(107,3) 
      COMMON /CORE/ TORE(107) 
      COMMON /NATORB/ NATORB(107) 
C*********************************************************************** 
C 
C  REPP CALCULATES THE TWO-ELECTRON REPULSION INTEGRALS AND THE 
C       NUCLEAR ATTRACTION INTEGRALS. 
C 
C     ON INPUT RIJ     = INTERATOMIC DISTANCE 
C              NI      = ATOM NUMBER OF FIRST ATOM 
C              NJ      = ATOM NUMBER OF SECOND ATOM 
C    (REF)     ADD     = ARRAY OF GAMMA, OR TWO-ELECTRON ONE-CENTER, 
C                        INTEGRALS. 
C    (REF)     TORE    = ARRAY OF NUCLEAR CHARGES OF THE ELEMENTS 
C    (REF)     DD      = ARRAY OF DIPOLE CHARGE SEPARATIONS 
C    (REF)     QQ      = ARRAY OF QUADRUPOLE CHARGE SEPARATIONS 
C 
C     THE COMMON BLOCKS ARE INITIALISED IN BLOCK-DATA, AND NEVER CHANGED 
C 
C    ON OUTPUT RI      = ARRAY OF TWO-ELECTRON REPULSION INTEGRALS 
C              CORE    = 4 X 2 ARRAY OF ELECTRON-CORE ATTRACTION 
C                        INTEGRALS 
C 
C*********************************************************************** 
      SAVE
      R=RIJ/0.529167D00 
      PP=2.0D00 
      P2=4.0D00 
      P3=8.0D00 
      P4=16.0D00 
C 
C *** THIS ROUTINE COMPUTES THE TWO-CENTRE REPULSION INTEGRALS AND THE 
C *** NUCLEAR ATTRACTION INTEGRALS. 
C *** THE TWO-CENTRE REPULSION INTEGRALS (OVER LOCAL COORDINATES) ARE 
C *** STORED AS FOLLOWS (WHERE P-SIGMA = O,  AND P-PI = P AND P* ) 
C     (SS/SS)=1,   (SO/SS)=2,   (OO/SS)=3,   (PP/SS)=4,   (SS/OS)=5, 
C     (SO/SO)=6,   (SP/SP)=7,   (OO/SO)=8,   (PP/SO)=9,   (PO/SP)=10, 
C     (SS/OO)=11,  (SS/PP)=12,  (SO/OO)=13,  (SO/PP)=14,  (SP/OP)=15, 
C     (OO/OO)=16,  (PP/OO)=17,  (OO/PP)=18,  (PP/PP)=19,  (PO/PO)=20, 
C     (PP/P*P*)=21,   (P*P/P*P)=22. 
C *** THE STORAGE OF THE NUCLEAR ATTRACTION INTEGRALS  CORE(KL/IJ) IS 
C     (SS/)=1,   (SO/)=2,   (OO/)=3,   (PP/)=4 
C     WHERE IJ=1 IF THE ORBITALS CENTRED ON ATOM I,  =2 IF ON ATOM J. 
C *** NI AND NJ ARE THE ATOMIC NUMBERS OF THE TWO ELEMENTS. 
C 
      DO 10 I=1,22 
   10 RI(I)=0.0D0 
      DO 20 I=1,8 
   20 CORE(I,1)=0.0D0 
C 
C     ATOMIC UNITS ARE USED IN THE CALCULATION 
C     DEFINE CHARGE SEPARATIONS. 
C 
      DA=DD(NI) 
      DB=DD(NJ) 
      QA=QQ(NI) 
      QB=QQ(NJ) 
      TD = 2.D00 
      OD = 1.D00 
      FD = 4.D00 
C 
C     HYDROGEN - HYDROGEN 
C 
      IF ((ABS(ADD(NI,1)).LE.1.0D-40).or.(ABS(ADD(NJ,1)).LE.1.0D-40))   BJL1003
     1 THEN                                                             BJL1003
           AEE=1.0D40                                                   BJL1003
       ELSE                                                             BJL1003
           AEE=0.25D00*(OD/ADD(NI,1)+OD/ADD(NJ,1))**2 
       ENDIF                                                            BJL1003



      EE=OD/SQRT(R**2+AEE) 
      RI(1)=EE*27.21D00 
      CORE(1,1)=-TORE(NJ)*RI(1) 
      CORE(1,2)=-TORE(NI)*RI(1) 
      IF (NATORB(NI).LT.3.AND.NATORB(NJ).LT.3) RETURN 
      IF (NATORB(NI).LT.3) GO TO 30 
C 
C     HEAVY ATOM - HYDROGEN 
C 
      ADE=0.25D00*(OD/ADD(NI,2)+OD/ADD(NJ,1))**2 
      AQE=0.25D00*(OD/ADD(NI,3)+OD/ADD(NJ,1))**2 
      DZE=-OD/SQRT((R+DA)**2+ADE)+OD/SQRT((R-DA)**2+ADE) 
      QZZE=OD/SQRT((R-TD*QA)**2+AQE)-TD/SQRT(R**2+AQE)+OD/ 
     1SQRT((R+TD*QA)**2+AQE) 
      QXXE=TD/SQRT(R**2+FD*QA**2+AQE)-TD/SQRT(R**2+AQE) 
      DZE=DZE/PP 
      QXXE=QXXE/P2 
      QZZE=QZZE/P2 
      RI(2)=-DZE 
      RI(3)=EE+QZZE 
      RI(4)=EE+QXXE 
      IF (NATORB(NJ).LT.3) GO TO 40 
C 
C     HYDROGEN - HEAVY ATOM 
C 
   30 CONTINUE 

      IF (ABS(ADD(NI,1)) .LE. 1.0D-40) THEN                             BJL1003
        AED=1.0D40                                                      BJL1003
        AEQ=1.0D40                                                      BJL1003
      ELSE                                                              BJL1003
        AED=0.25D00*(OD/ADD(NI,1)+OD/ADD(NJ,2))**2 
        AEQ=0.25D00*(OD/ADD(NI,1)+OD/ADD(NJ,3))**2 
      ENDIF                                                             BJL1003

      EDZ=-OD/SQRT((R-DB)**2+AED)+OD/SQRT((R+DB)**2+AED) 
      EQZZ=OD/SQRT((R-TD*QB)**2+AEQ)-TD/SQRT(R**2+AEQ)+OD/ 
     1SQRT((R+TD*QB)**2+AEQ) 
      EQXX=TD/SQRT(R**2+FD*QB**2+AEQ)-TD/SQRT(R**2+AEQ) 
      EDZ=EDZ/PP 
      EQXX=EQXX/P2 
      EQZZ=EQZZ/P2 
      RI(5)=-EDZ 
      RI(11)=EE+EQZZ 
      RI(12)=EE+EQXX 
      IF (NATORB(NI).LT.3) GO TO 40 
C 
C     HEAVY ATOM - HEAVY ATOM 
C     CAUTION. ADD REPLACES ADD(1,1) IN /MULTIP/ AND MUST BE RESET. 
C 
      ADD(1,1)=0.25D00*(OD/ADD(NI,2)+OD/ADD(NJ,2))**2 
      ADQ=0.25D00*(OD/ADD(NI,2)+OD/ADD(NJ,3))**2 
      AQD=0.25D00*(OD/ADD(NI,3)+OD/ADD(NJ,2))**2 
      AQQ=0.25D00*(OD/ADD(NI,3)+OD/ADD(NJ,3))**2 
      DXDX=TD/SQRT(R**2+(DA-DB)**2+ADD(1,1)) 
     1-TD/SQRT(R**2+(DA+DB)**2+ADD(1,1)) 
      DZDZ=OD/SQRT((R+DA-DB)**2+ADD(1,1)) 
     1+OD/SQRT((R-DA+DB)**2+ADD(1,1))-OD/SQRT(( 
     2R-DA-DB)**2+ADD(1,1))-OD/SQRT((R+DA+DB)**2+ADD(1,1)) 
      DZQXX=-TD/SQRT((R+DA)**2+FD*QB**2+ADQ)+TD/SQRT((R-DA)**2 
     1+FD*QB**2+ 
     2ADQ)+TD/SQRT((R+DA)**2+ADQ)-TD/SQRT((R-DA)**2+ADQ) 
      QXXDZ=-TD/SQRT((R-DB)**2+FD*QA**2+AQD)+TD/SQRT((R+DB)**2 
     1+FD*QA**2+ 
     2AQD)+TD/SQRT((R-DB)**2+AQD)-TD/SQRT((R+DB)**2+AQD) 
      DZQZZ=-OD/SQRT((R+DA-TD*QB)**2+ADQ)+OD/SQRT((R-DA-TD* 
     1QB)**2+ADQ)-OD/SQRT((R+DA+TD*QB)**2+ADQ)+OD/SQRT((R-DA+TD*QB) 
     2**2+ADQ)-TD/SQRT((R-DA)**2+ADQ)+TD/SQRT((R+DA)**2+ADQ) 
      QZZDZ=-OD/SQRT((R+TD*QA-DB)**2+AQD)+OD/SQRT((R+TD*QA+ 
     1DB)**2+AQD)-OD/SQRT((R-TD*QA-DB)**2+AQD)+OD/SQRT((R-2.D 
     200*QA+DB)**2+AQD)+TD/SQRT((R-DB)**2+AQD)-TD/SQRT((R+DB)**2 
     3+AQD) 
      QXXQXX=TD/SQRT(R**2+FD*(QA-QB)**2+AQQ)+TD/SQRT(R**2+FD*(QA+QB)**2+ 
     1AQQ)-FD/SQRT(R**2+FD*QA**2+AQQ)-FD/SQRT(R**2+FD*QB**2+AQQ)+FD/SQRT 
     2(R**2+AQQ) 
      QXXQYY=FD/SQRT(R**2+FD*QA**2+FD*QB**2+AQQ)-FD/SQRT(R**2+FD*QA**2+A 
     1QQ)-FD/SQRT(R**2+FD*QB**2+AQQ)+FD/SQRT(R**2+AQQ) 
      QXXQZZ=TD/SQRT((R-TD*QB)**2+FD*QA**2+AQQ)+TD/SQRT((R+TD*QB)**2+FD* 
     1QA**2+AQQ)-TD/SQRT((R-TD*QB)**2+AQQ)-TD/SQRT((R+TD*QB)**2+AQQ)-FD/ 
     2SQRT(R**2+FD*QA**2+AQQ)+FD/SQRT(R**2+AQQ) 
      QZZQXX=TD/SQRT((R+TD*QA)**2+FD*QB**2+AQQ)+TD/SQRT((R-TD*QA)**2+FD* 
     1QB**2+AQQ)-TD/SQRT((R+TD*QA)**2+AQQ)-TD/SQRT((R-TD*QA)**2+AQQ)-FD/ 
     2SQRT(R**2+FD*QB**2+AQQ)+FD/SQRT(R**2+AQQ) 
      QZZQZZ=OD/SQRT((R+TD*QA-TD*QB)**2+AQQ)+OD/SQRT((R+TD*QA+TD*QB)**2+ 
     1AQQ)+OD/SQRT((R-TD*QA-TD*QB)**2+AQQ)+OD/SQRT((R-TD*QA+TD*QB)**2+AQ 
     2Q)-TD/SQRT((R-TD*QA)**2+AQQ)-TD/SQRT((R+TD*QA)**2+AQQ)-TD/SQRT((R- 
     3TD*QB)**2+AQQ)-TD/SQRT((R+TD*QB)**2+AQQ)+FD/SQRT(R**2+AQQ) 
      DXQXZ=-TD/SQRT((R-QB)**2+(DA-QB)**2+ADQ)+TD/SQRT((R+QB)**2+(DA-QB) 
     1**2+ADQ)+TD/SQRT((R-QB)**2+(DA+QB)**2+ADQ)-TD/SQRT((R+QB)**2+(DA+Q 
     2B)**2+ADQ) 
      QXZDX=-TD/SQRT((R+QA)**2+(QA-DB)**2+AQD)+TD/SQRT((R-QA)**2+(QA-DB) 
     1**2+AQD)+TD/SQRT((R+QA)**2+(QA+DB)**2+AQD)-TD/SQRT((R-QA)**2+(QA+D 
     2B)**2+AQD) 
      QXYQXY=FD/SQRT(R**2+TD*(QA-QB)**2+AQQ)+FD/SQRT(R**2+TD*(QA+QB)**2+ 
     1AQQ)-8.D00/SQRT(R**2+TD*(QA**2+QB**2)+AQQ) 
      QXZQXZ=TD/SQRT((R+QA-QB)**2+(QA-QB)**2+AQQ)-TD/SQRT((R+QA+QB)**2+( 
     1QA-QB)**2+AQQ)-TD/SQRT((R-QA-QB)**2+(QA-QB)**2+AQQ)+TD/SQRT((R-QA+ 
     2QB)**2+(QA-QB)**2+AQQ)-TD/SQRT((R+QA-QB)**2+(QA+QB)**2+AQQ)+TD/SQR 
     3T((R+QA+QB)**2+(QA+QB)**2+AQQ)+TD/SQRT((R-QA-QB)**2+(QA+QB)**2+AQQ 
     4)-TD/SQRT((R-QA+QB)**2+(QA+QB)**2+AQQ) 
      DXDX=DXDX/P2 
      DZDZ=DZDZ/P2 
      DZQXX=DZQXX/P3 
      QXXDZ=QXXDZ/P3 
      DZQZZ=DZQZZ/P3 
      QZZDZ=QZZDZ/P3 
      DXQXZ=DXQXZ/P3 
      QXZDX=QXZDX/P3 
      QXXQXX=QXXQXX/P4 
      QXXQYY=QXXQYY/P4 
      QXXQZZ=QXXQZZ/P4 
      QZZQXX=QZZQXX/P4 
      QZZQZZ=QZZQZZ/P4 
      QXZQXZ=QXZQXZ/P4 
      QXYQXY=QXYQXY/P4 
      RI(6)=DZDZ 
      RI(7)=DXDX 
      RI(8)=-EDZ-QZZDZ 
      RI(9)=-EDZ-QXXDZ 
      RI(10)=-QXZDX 
      RI(13)=-DZE-DZQZZ 
      RI(14)=-DZE-DZQXX 
      RI(15)=-DXQXZ 
      RI(16)=EE+EQZZ+QZZE+QZZQZZ 
      RI(17)=EE+EQZZ+QXXE+QXXQZZ 
      RI(18)=EE+EQXX+QZZE+QZZQXX 
      RI(19)=EE+EQXX+QXXE+QXXQXX 
      RI(20)=QXZQXZ 
      RI(21)=EE+EQXX+QXXE+QXXQYY 
      RI(22)=0.5D0*(QXXQXX-QXXQYY) 
      ADD(1,1)=ADD(1,2) 
   40 CONTINUE 
C 
C     CONVERT INTO EV. 
C 
      DO 50 I=2,22 
   50 RI(I)=RI(I)*27.21D00 
C 
C     CALCULATE CORE-ELECTRON ATTRACTIONS. 
C 
      CORE(2,1)=-TORE(NJ)*RI(2) 
      CORE(3,1)=-TORE(NJ)*RI(3) 
      CORE(4,1)=-TORE(NJ)*RI(4) 
      CORE(2,2)=-TORE(NI)*RI(5) 
      CORE(3,2)=-TORE(NI)*RI(11) 
      CORE(4,2)=-TORE(NI)*RI(12) 
      RETURN 
      END 
      SUBROUTINE RLOCAL (COORD,NA,NB,NC,R) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
C     RLOCAL PROVIDES IN R (COLUMNWISE) THE LEFT-HANDED ORTHONORMALIZED 
C     'LOCAL' FRAME SPANNED BY THE ATOMS NA,NB,NC ACCORDING TO: 
C     NA       IS THE ORIGIN OF THE LOCAL FRAME 
C     NA-NB->  IS THE POSITIVE Z AXIS 
C     NC       LIES IN THE XZ PLANE WITH X POSITIVE 
C     THESE CONVENTIONS AGREE WITH THOSE ADOPTED IN BOTH 
C                            'XYZINT' AND 'INTCAR' ROUTINES. 
C     D.L.  (MJS DEWAR GROUP) JUNE 1986 
      DIMENSION COORD(3,*),R(3,3) 
      SAVE
C 
C..... THE FOLLOWING INSTRUCTIONS ARE USELESS IF 'RLOCAL' IS NOT CALLED 
C                                      FOR THE FIRST 3 ATOMS. 
CC     SPECIAL CASES FOR FIRST ATOMS 
C      IF(NA.EQ.0.OR.NB.EQ.0) THEN 
C         DO 1 I=1,9 
C    1    R(I,1)= 0.D0 
C         R(2,1)= 1.D0 
C         R(3,2)=-1.D0 
C         R(1,3)= 1.D0 
C         RETURN 
C      ENDIF 
C      IF(NC.EQ.0) THEN 
C         DO 2 I=1,9 
C    2    R(I,1)= 0.D0 
C         R(2,1)= 1.D0 
C         R(3,2)= 1.D0 
C         R(1,3)=-1.D0 
C         RETURN 
C      ENDIF 
C........... 
C     GENERAL CASE 
      DO 10 I=1,3 
      R(I,3)=COORD(I,NB)-COORD(I,NA) 
   10 R(I,1)=COORD(I,NC)-COORD(I,NB) 
      W=SQRT(DOT(R(1,3),R(1,3),3)) 
C     NORMALIZED Z DIRECTION R(I,3) 
      DO 20 I=1,3 
   20 R(I,3)=R(I,3)/W 
      W=DOT(R,R(1,3),3) 
      DO 30 I=1,3 
   30 R(I,1)=R(I,1)-W*R(I,3) 
      W=SQRT(DOT(R,R,3)) 
C     NORMALIZED X DIRECTION R(I,1) 
      DO 40 I=1,3 
   40 R(I,1)=R(I,1)/W 
C     NORMALIZED LEFT-HANDED Y DIRECTION R(I,2) 
      R(1,2)=R(2,1)*R(3,3)-R(3,1)*R(2,3) 
      R(2,2)=R(3,1)*R(1,3)-R(1,1)*R(3,3) 
      R(3,2)=R(1,1)*R(2,3)-R(2,1)*R(1,3) 
      RETURN 
      END 
      SUBROUTINE SCATER (N,A,INDEX,B)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     SCATTER B IN A ACCORDING TO INDEX
C     MIMIC ROUTINE ON CRAY (SCATTER) OR IBM (DSCTR)
      DIMENSION A(*),INDEX(*),B(*)
      DO 10 I=1,N
   10 A(INDEX(I))=B(I)
      RETURN
      END
      SUBROUTINE SCHMIB(U,N,NDIM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
C
C     SAME AS SCHMIDT BUT WORKS FROM RIGHT TO LEFT.
C
      DIMENSION U(NDIM,NDIM)
       SAVE
      DATA ZERO,SMALL,ONE/0.0D0,0.01D0,1.0D0/
      N1=N+1
      II=0
      DO 110 K=1,N
         K1=K-1
C
C     NORMALIZE KTH COLUMN VECTOR
C
         DOT = ZERO
         DO 10 I=1,N
   10    DOT=DOT+U(I,N1-K)*U(I,N1-K)
         IF(DOT.EQ.ZERO) GO TO 100
         SCALE=ONE/SQRT(DOT)
         DO 20 I=1,N
   20    U(I,N1-K)=SCALE*U(I,N1-K)
   30    IF(K1.EQ.0) GO TO 110
         NPASS=0
C
C     PROJECT OUT K-1 PREVIOUS ORTHONORMAL VECTORS FROM KTH VECTOR
C
   40    NPASS=NPASS+1
         DO 70 J=1,K1
            DOT=ZERO
            DO 50 I=1,N
   50       DOT=DOT+U(I,N1-J)*U(I,N1-K)
            DO 60 I=1,N
   60       U(I,N1-K)=U(I,N1-K)-DOT*U(I,N1-J)
   70    CONTINUE
C
C     SECOND NORMALIZATION (AFTER PROJECTION)
C     IF KTH VECTOR IS SMALL BUT NOT ZERO THEN NORMALIZE
C     AND PROJECT AGAIN TO CONTROL ROUND-OFF ERRORS.
C
         DOT=ZERO
         DO 80 I=1,N
   80    DOT=DOT+U(I,N1-K)*U(I,N1-K)
         IF(DOT.EQ.ZERO) GO TO 100
         IF(DOT.LT.SMALL.AND.NPASS.GT.2) GO TO 100
         SCALE=ONE/SQRT(DOT)
         DO 90 I=1,N
   90    U(I,N1-K)=SCALE*U(I,N1-K)
         IF(DOT.LT.SMALL) GO TO 40
         GO TO 110
C
C     REPLACE LINEARLY DEPENDENT KTH VECTOR BY A UNIT VECTOR.
C
  100    II=II+1
C     IF(II.GT.N) STOP
         U(II,N1-K)=ONE
         GO TO 30
  110 CONTINUE
      RETURN
      END
      SUBROUTINE SCHMIT(U,N,NDIM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
      DIMENSION U(NDIM,NDIM)
       SAVE
      DATA ZERO,SMALL,ONE/0.0D0,0.01D0,1.0D0/
      II=0
      DO 110 K=1,N
         K1=K-1
C
C     NORMALIZE KTH COLUMN VECTOR
C
         DOT = ZERO
         DO 10 I=1,N
   10    DOT=DOT+U(I,K)*U(I,K)
         IF(DOT.EQ.ZERO) GO TO 100
         SCALE=ONE/SQRT(DOT)
         DO 20 I=1,N
   20    U(I,K)=SCALE*U(I,K)
   30    IF(K1.EQ.0) GO TO 110
         NPASS=0
C
C     PROJECT OUT K-1 PREVIOUS ORTHONORMAL VECTORS FROM KTH VECTOR
C
   40    NPASS=NPASS+1
         DO 70 J=1,K1
            DOT=ZERO
            DO 50 I=1,N
   50       DOT=DOT+U(I,J)*U(I,K)
            DO 60 I=1,N
   60       U(I,K)=U(I,K)-DOT*U(I,J)
   70    CONTINUE
C
C     SECOND NORMALIZATION (AFTER PROJECTION)
C     IF KTH VECTOR IS SMALL BUT NOT ZERO THEN NORMALIZE
C     AND PROJECT AGAIN TO CONTROL ROUND-OFF ERRORS.
C
         DOT=ZERO
         DO 80 I=1,N
   80    DOT=DOT+U(I,K)*U(I,K)
         IF(DOT.EQ.ZERO) GO TO 100
         IF(DOT.LT.SMALL.AND.NPASS.GT.2) GO TO 100
         SCALE=ONE/SQRT(DOT)
         DO 90 I=1,N
   90    U(I,K)=SCALE*U(I,K)
         IF(DOT.LT.SMALL) GO TO 40
         GO TO 110
C
C     REPLACE LINEARLY DEPENDENT KTH VECTOR BY A UNIT VECTOR.
C
  100    II=II+1
C     IF(II.GT.N) STOP
         U(II,K)=ONE
         GO TO 30
  110 CONTINUE
      RETURN
      END
      SUBROUTINE SWAP(C,N,MDIM,NOCC,IFILL) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      INCLUDE 'SIZES.i'                                                 DL0397
      DIMENSION C(MDIM,MDIM),PSI(MAXORB),STDPSI(MAXORB)                 DL0397
      SAVE
C****************************************************************** 
C 
C        SWAP ENSURES THAT A NAMED MOLECULAR ORBITAL IFILL IS FILLED 
C ON INPUT 
C          C = EIGENVECTORS IN A MDIM*MDIM MATRIX 
C          N = NUMBER OF ORBITALS 
C          NOCC = NUMBER OF OCCUPIED ORBITALS 
C          IFILL = FILLED ORBITAL 
C****************************************************************** 
      IF(IFILL.GT.0) GOTO 20 
C 
C     WE NOW DEFINE THE FILLED ORBITAL 
C 
      IFILL=-IFILL 
      DO 10 I=1,N 
         STDPSI(I)=C(I,IFILL) 
   10 PSI(I)=C(I,IFILL) 
      RETURN 
   20 CONTINUE 
C 
C     FIRST FIND THE LOCATION OF IFILL 
C 
      SUM=0.D0 
      DO 30 I=1,N 
   30 SUM=SUM+PSI(I)*C(I,IFILL) 
      IF(ABS(SUM).GT.0.7071D0) GOTO 90 
C 
C     IFILL HAS MOVED] 
C 
      SUMMAX=0.D0 
      DO 50 IFILL=1,N 
         SUM=0.D0 
         DO 40 I=1,N 
   40    SUM=SUM+STDPSI(I)*C(I,IFILL) 
         SUM=ABS(SUM) 
         IF(SUM.GT.SUMMAX)JFILL=IFILL 
         IF(SUM.GT.SUMMAX)SUMMAX=SUM 
         IF(SUM.GT.0.7071D0) GOTO 90 
   50 CONTINUE 
      DO 70 IFILL=1,N 
         SUM=0.D0 
         DO 60 I=1,N 
   60    SUM=SUM+PSI(I)*C(I,IFILL) 
         SUM=ABS(SUM) 
         IF(SUM.GT.SUMMAX)JFILL=IFILL 
         IF(SUM.GT.SUMMAX)SUMMAX=SUM 
         IF(SUM.GT.0.7071D0) GOTO 90 
   70 CONTINUE 
   80 FORMAT(' SUM VERY SMALL, SUM =',F12.6,' JFILL=',I3) 
      IFILL=JFILL 
   90 CONTINUE 
C 
C    STORE THE NEW VECTOR IN PSI 
C 
C      DO 22 I=1,N 
C  22  PSI(I)=C(I,IFILL) 
C 
C    NOW CHECK TO SEE IF IFILL IS FILLED 
C 
      IF(IFILL.LE.NOCC) RETURN 
C 
C    ITS EMPTY, SO SWAP IT WITH THE HIGHEST FILLED 
C 
      DO 100 I=1,N 
         X=C(I,NOCC) 
         C(I,NOCC)=C(I,IFILL) 
         C(I,IFILL)=X 
  100 CONTINUE 
      RETURN 
      END 
      SUBROUTINE SYMTRY 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
      COMMON /GEOM  / GEO(3,NUMATM) 
      COMMON /GEOSYM/ NDEP, LOCPAR(MAXPAR), IDEPFN(MAXPAR), 
     1         LOCDEP(MAXPAR) 
C********************************************************************** 
C 
C  SYMTRY COMPUTES THE BOND LENGTHS AND ANGLES THAT ARE FUNCTIONS OF 
C         OTHER BOND LENGTHS AND ANGLES. 
C 
C ON INPUT GEO     = KNOWN INTERNAL COORDINATES 
C          NDEP    = NUMBER OF DEPENDENCY FUNCTIONS. 
C          IDEPFN  = ARRAY OF DEPENDENCY FUNCTIONS. 
C          LOCDEP  = ARRAY OF LABELS OF DEPENDENT ATOMS. 
C          LOCPAR  = ARRAY OF LABELS OF REFERENCE ATOMS. 
C 
C  ON OUTPUT THE ARRAY "GEO" IS FILLED 
C*********************************************************************** 
C 
C     NOW COMPUTE THE DEPENDENT PARAMETERS. 
C 
      SAVE
      DO 10 I=1,NDEP 
         CALL HADDON (VALUE,LOCN,IDEPFN(I),LOCPAR(I),GEO) 
         J=LOCDEP(I) 
   10 GEO(LOCN,J)=VALUE 
      RETURN 
      END 
      SUBROUTINE WCANON (W,PQRS,NROW,NCOL,NBAND1,NBAND2) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     * MNDO *  OR  * AM1 *  BUT NOT  * MINDO * 
C     FROM ONE BICENTRIC BLOCK OF 2-ELECTRON INTEGRALS 
C       TO CANONIC STORAGE. 
      DIMENSION W(*),PQRS(NROW,NCOL) 
       SAVE
      K=1 
      DO 10 J=1,NCOL 
      DO 10 I=NBAND1,NBAND2 
      PQRS(I,J)=W(K) 
   10 K=K+1 
      RETURN 
      END 
      SUBROUTINE WNONCA (W,PQRS,NROW,NCOL,NBAND1,NBAND2) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     * MNDO *  OR  * AM1 *  BUT NOT  * MINDO * 
C     FROM CANONIC STORAGE 
C       TO ONE BICENTRIC BLOCK OF 2-ELECTRON INTEGRALS. 
      DIMENSION W(*),PQRS(NROW,NCOL) 
       SAVE
      K=1 
      DO 20 J=1,NCOL 
      DO 20 I=NBAND1,NBAND2 
      W(K)=PQRS(I,J) 
   20 K=K+1 
      RETURN 
      END 
      SUBROUTINE XYZGEO(XYZ,NUMAT,NA,NB,NC,DEGREE,GEO) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION XYZ(3,*), NA(*), NB(*), NC(*), GEO(3,*) 
*********************************************************************** 
* 
*   XYZGEO CONVERTS COORDINATES FROM CARTESIAN TO INTERNAL. 
* 
*     ON INPUT XYZ  = ARRAY OF CARTESIAN COORDINATES 
*              NUMAT= NUMBER OF ATOMS 
*              NA   = NUMBERS OF ATOM TO WHICH ATOMS ARE RELATED 
*                     BY DISTANCE 
*              NB   = NUMBERS OF ATOM TO WHICH ATOMS ARE RELATED 
*                     BY ANGLE 
*              NC   = NUMBERS OF ATOM TO WHICH ATOMS ARE RELATED 
*                     BY DIHEDRAL 
* 
*    ON OUTPUT GEO  = INTERNAL COORDINATES IN ANGSTROMS, RADIANS, 
*                     AND RADIANS 
*                     ANGLE BETWEEN 0 AND PI, 
*                     DIHEDRAL BETWEEN -PI AND PI, ACCORDING TO THE 
*                     CLOCKWISE CONVENTION (ANTI TRIGONOMETRIC), 
*                     ( KLYNE & PRELOG,EXPERIENTIA 16, 521(1960) ) 
* 
*********************************************************************** 
      SAVE
      DO 10 I=2,NUMAT 
         J=NA(I) 
         K=NB(I) 
         L=NC(I) 
         IF(I.LT.3) GOTO 10 
         II=I 
         CALL BANGLE(XYZ,II,J,K,GEO(2,I)) 
         GEO(2,I)=GEO(2,I)*DEGREE 
         IF(I.LT.4) GOTO 10 
         CALL DIHED(XYZ,II,J,K,L,GEO(3,I)) 
         GEO(3,I)=GEO(3,I)*DEGREE 
   10 GEO(1,I)= SQRT((XYZ(1,I)-XYZ(1,J))**2+ 
     1                   (XYZ(2,I)-XYZ(2,J))**2+ 
     2                   (XYZ(3,I)-XYZ(3,J))**2) 
      GEO(1,1)=0.D0 
      GEO(2,1)=0.D0 
      GEO(3,1)=0.D0 
      GEO(2,2)=0.D0 
      GEO(3,2)=0.D0 
      GEO(3,3)=0.D0 
      RETURN 
      END 
