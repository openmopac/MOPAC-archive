C======================================================================+1128BL04
      SUBROUTINE TRUST5 (N,X,HVEXYZ,NUMAT3,IOPTN,HESSE,HVAL,WORK)       1128BL04
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               1128BL04
      INCLUDE 'SIZES.i'                                                 1128BL04
      INCLUDE 'KEYS.i'                                                  DJG0496
      PARAMETER (MSIZE=NUMATM*(NUMATM-1)/2, MXPAR2=MAXPAR**2)           1128BL04
      DIMENSION X(*),HVEXYZ(NUMAT3,*),HESSE(*),HVAL(*),WORK(*)          1128BL04
C ----------------------------------------------------------------------1128BL04
C     TRUST METHODS. MODEL HESSIAN A LA R. LINDH. CREATED 11/22/04, DL. 1128BL04
C  INPUT                                                                1128BL04
C     N:                 NUMBER OF OPTIMIZED PARAMETERS.                1128BL04
C     X(N):              OPTIMIZED PARAMETERS.                          1128BL04
C     NUMAT:             NUMBER OF NON-DUMMY ATOMS.                     1128BL04
C     NUMAT3:            NUMAT*3                                        1128BL04
C     COORD(3,NUMAT):    CARTESIAN COORDINATES.                         1128BL04
C     BJACOB(N,NUMAT3):  JACOBIAN MATRIX (SEE 'DERIV')                  1128BL04
C     HVEXYZ(NUMAT3,NUMAT3): WORK ARRAY (NOT LOCATED IN /SCRCHR/)       1128BL04
C     HVAL(NUMAT3)         : WORK ARRAY (NOT LOCATED IN /SCRCHR/)       1128BL04
C     WORK(NUMAT3**2)      : WORK ARRAY                                 1128BL04
C     IOPTN  :            OUTPUT OPTION, SEE BELOW.                     1128BL04
C  OUTPUT                                                               1128BL04
C     BJACOB(N,NUMAT3):  NOT MODIFIED.                                  1128BL04
C   * IOPTN= 0                                                          1128BL04
C     HESSE(*): 2-BODY HESSIAN, NOT NECESSARILY DEFINITE POSITIVE.      1128BL04
C   * IOPTI= 1                                                          1128BL04
C     HESSE(*): 2-BODY HESSIAN,     NECESSARILY DEFINITE POSITIVE.      1128BL04
C --------------------------------------------------------------------  1128BL04
      COMMON /HBODY2/ RG(3,MSIZE),BJACOB(MAXPAR**2),COORD(3,NUMATM)
     1       /SCRCHR/ WORK2(MAXPAR**2)
     2       /CYCLCM/ PCMIN,NGEOM,NSOLPR,NSCFS,JPCNT
     3       /GEOKST/ NATOMS,LABELS(NUMATM),NABC(3,NUMATM)
     4       /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     5                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     6                NCLOSE,NOPEN,NDUMY
     7       /PRECI / SCFCRT,SCFTOL,EG(3),ESTIM(3),PMAX(3),KTYP(MAXPAR)
      DIMENSION HESTIM(4),W(1)
C     PROPOSAL FROM R. LINDH & al.
      CALL HLINDH (COORD,NUMAT3,HVEXYZ)  
      IF (NABC(1,1).NE.99.OR.N.NE.NUMAT3) THEN
C        SET UP THE JACOBIAN MATRIX
         CALL JCARIN (COORD,X,.TRUE.,BJACOB,NCOL)
C        SET HESSIAN IN PARAMETER SPACE, VIA JACOBIAN BJACOB. 
         CALL MXM (BJACOB,N,HVEXYZ,NUMAT3,WORK,NUMAT3)
         CALL MXMT (WORK,N,BJACOB,NUMAT3,HVEXYZ,N)
      ENDIF
C     
C     RETURN THE HESSIAN ACCORDING TO THE OPTION "IOPTN".                DJG0496
C     
C     LOWER BOUNDS FOR DEF-POS HESSIAN EIGENVALUES:
C                  BOND   ANGLE  DIHE  CART
      DATA HESTIM /300D0, 150D0, 75D0, 200D0/
      IF (IOPTN.GE.1) THEN                                               DJG0496
C        DEF-POS HESSIAN REQUIRED.
C        EIGENVALUES AND VECTORS:
         CALL DIAGIV (HVEXYZ,N,N,HVAL,EPS1)                              DL0397 
         IF (NABC(1,1).EQ.99) THEN
C           CARTESIAN COORDS: HESTIM(4)
            DO 140 I=1,N
  140       HVAL(I)=MAX(HVAL(I),HESTIM(4))
         ELSE
C           INTERNAL COORDS: WEIGHTED HESTIM(1,2,3)
            DO 160 I=1,N
            VALOW=0D0
            DO 150 J=1,N
  150       VALOW=HESTIM(KTYP(J))*HVEXYZ((I-1)*N+J,1)**2+VALOW
  160       HVAL(I)=MAX(HVAL(I),VALOW)
         ENDIF
         DO 170 J=1,N
         DO 170 I=(J-1)*N+1,J*N
  170    WORK(I)=HVEXYZ(I,1)*HVAL(J)
         CALL MXMT (WORK,N,HVEXYZ,N,WORK2,N)
         CALL CANO (HESSE,N,WORK2,N)
      ELSE
         CALL CANO (HESSE,N,HVEXYZ,N)
      ENDIF
C     FINAL HESSIAN HAS BEEN STORED (CANONICAL) IN HESSE.
      END

C======================================================================+
      SUBROUTINE HLINDH (COORD,N3,HESSE)                                1128BL04
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               1128BL04
      INCLUDE 'SIZES.i'                                                 1128BL04
      DIMENSION COORD(3,*),HESSE(N3,*)                                  1128BL04
C ---------------------------------------------------------------------+
C     HESSIAN MODEL FUNCTION. FROM:                                     1128BL04
C     R.LINDH & AL., CHEM. PHYS. LETTERS, 241 (1995) 423                1128BL04
C     ADDED TRUNCATION AND SAVINGS (SIGNIFICANT FOR LARGE SYSTEMS).     1128BL04
C ---------------------------------------------------------------------+
      COMMON /SCRCHR/ R(NUMATM,NUMATM),UN(3,NUMATM,NUMATM)
     1       /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     2                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     3                NCLOSE,NOPEN,NDUMY
      DIMENSION SI(3),SJ(3),SM(3),C(3,4),CIJK(3)
      DIMENSION IROW(107)
      DATA IROW / 2*1,8*2,8*3,89*3 /
      DIMENSION RMOD(3,3),AMOD(3,3)
      DATA RMOD /1.35D0,2.10D0,2.53D0,
     1           2.10D0,2.87D0,3.40D0,
     2           2.53D0,3.40D0,3.40D0/
      DATA AMOD /1.0000D0,0.3949D0,0.3949D0,
     1           0.3949D0,0.2800D0,0.2800D0,
     2           0.3949D0,0.2800D0,0.2800D0/
      DATA RKR,RKF,RKT /0.45D0,0.15D0,0.005D0/ , EPS /1D-04/
      DATA A0 /0.529167D0/, EV /27.210D0/, EVCAL /23.061D0/
      DATA ZERO /0D0/, ONE /1D0/
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      SAVE TRUNC
      IF (FIRST) THEN
C        CONVERT MODEL PARAMETERS FROM A.U. INTO KCAL/MOL & ANGSTROMS.
         FCTR=ONE/A0**2
         DO I=1,9
         RMOD(I,1)=RMOD(I,1)*A0
         AMOD(I,1)=AMOD(I,1)*FCTR
         ENDDO
         FCTE=EV*EVCAL*FCTR
         RKR=RKR*FCTE
         RKF=RKF*FCTE
         RKT=RKT*FCTE
C        TRUNCATE WHEN EXP(R0**2-R**2) FALLS LOWER THAN 0.001*EXP(R0**2).
         TRUNC=5.91D0
         FIRST=.FALSE.
      ENDIF
      CALL SCOPY (N3**2,ZERO,0,HESSE,1)
C     INTERATOMIC UNIT VECTORS UN(3,I,J) AND DISTANCES R(I,J) 
      R(1,1)=ZERO
      UN(1,1,1)=ZERO
      UN(2,1,1)=ZERO
      UN(3,1,1)=ZERO
      DO 2 I=2,NUMAT
      DO 1 J=1,I-1
      UN(1,I,J)=COORD(1,J)-COORD(1,I) 
      UN(2,I,J)=COORD(2,J)-COORD(2,I) 
      UN(3,I,J)=COORD(3,J)-COORD(3,I)
      R(I,J)=SQRT(UN(1,I,J)**2+UN(2,I,J)**2+UN(3,I,J)**2)
      UN(1,I,J)=UN(1,I,J)/R(I,J)
      UN(2,I,J)=UN(2,I,J)/R(I,J)
      UN(3,I,J)=UN(3,I,J)/R(I,J)
      R(J,I)=R(I,J)
      UN(1,J,I)=-UN(1,I,J)
      UN(2,J,I)=-UN(2,I,J)
      UN(3,J,I)=-UN(3,I,J)
    1 CONTINUE
      R(I,I)=ZERO
      UN(1,I,I)=ZERO   
      UN(2,I,I)=ZERO
      UN(3,I,I)=ZERO
    2 CONTINUE
C
C     CONTRIBUTION FROM BONDS
C
      DO 11 K = 2,NUMAT
      KR=IROW(NAT(K))
      KH=3*(K-1)
      DO 10 L = 1,K-1
      LR=IROW(NAT(L))
      RKL2 = R(K,L)**2
      R0=RMOD(KR,LR)**2
      IF (RKL2.GT.TRUNC*R0) GOTO 10
      GMM=RKR*EXP(AMOD(KR,LR)*(R0-RKL2))
      HXX=GMM*UN(1,L,K)*UN(1,L,K)
      HXY=GMM*UN(1,L,K)*UN(2,L,K)
      HXZ=GMM*UN(1,L,K)*UN(3,L,K)
      HYY=GMM*UN(2,L,K)*UN(2,L,K)
      HYZ=GMM*UN(2,L,K)*UN(3,L,K)
      HZZ=GMM*UN(3,L,K)*UN(3,L,K)
      LH=3*(L-1)
      HESSE(1+KH,1+KH)=HESSE(1+KH,1+KH)+HXX
      HESSE(2+KH,1+KH)=HESSE(2+KH,1+KH)+HXY
      HESSE(2+KH,2+KH)=HESSE(2+KH,2+KH)+HYY
      HESSE(3+KH,1+KH)=HESSE(3+KH,1+KH)+HXZ
      HESSE(3+KH,2+KH)=HESSE(3+KH,2+KH)+HYZ
      HESSE(3+KH,3+KH)=HESSE(3+KH,3+KH)+HZZ
      HESSE(1+KH,1+LH)=HESSE(1+KH,1+LH)-HXX
      HESSE(1+KH,2+LH)=HESSE(1+KH,2+LH)-HXY
      HESSE(1+KH,3+LH)=HESSE(1+KH,3+LH)-HXZ
      HESSE(2+KH,1+LH)=HESSE(2+KH,1+LH)-HXY
      HESSE(2+KH,2+LH)=HESSE(2+KH,2+LH)-HYY
      HESSE(2+KH,3+LH)=HESSE(2+KH,3+LH)-HYZ
      HESSE(3+KH,1+LH)=HESSE(3+KH,1+LH)-HXZ
      HESSE(3+KH,2+LH)=HESSE(3+KH,2+LH)-HYZ
      HESSE(3+KH,3+LH)=HESSE(3+KH,3+LH)-HZZ
      HESSE(1+LH,1+LH)=HESSE(1+LH,1+LH)+HXX
      HESSE(2+LH,1+LH)=HESSE(2+LH,1+LH)+HXY
      HESSE(2+LH,2+LH)=HESSE(2+LH,2+LH)+HYY
      HESSE(3+LH,1+LH)=HESSE(3+LH,1+LH)+HXZ
      HESSE(3+LH,2+LH)=HESSE(3+LH,2+LH)+HYZ
      HESSE(3+LH,3+LH)=HESSE(3+LH,3+LH)+HZZ
 10   CONTINUE  
 11   CONTINUE
C
C     CONTRIBUTIONS FROM ANGLES
C
      DO 32 M =1,NUMAT
      MR=IROW(NAT(M))
      MH=3*(M-1)
      DO 31 I =1,NUMAT
      IF (I.EQ.M) GOTO 31
      IR=IROW(NAT(I))
      RMI=R(I,M)
      IF (RMI.LE.EPS) GOTO 31
      RMI2 = RMI**2
      R0MI=RMOD(MR,IR)**2
      IF (RMI2.GT.TRUNC*R0MI) GOTO 31
      AMI=AMOD(MR,IR)
      IH=3*(I-1)
      DO 30 J =1,I-1
      IF (J.EQ.M) GOTO 30
      JR=IROW(NAT(J))
      RMJ=R(J,M)
      IF (RMJ.LE.EPS) GOTO 30
      RMJ2 = RMJ**2
      R0MJ=RMOD(MR,JR)**2
      IF (RMJ2.GT.TRUNC*R0MJ) GOTO 30
      AMJ=AMOD(MR,JR)
C     CHECK ZERO OR PI ANGLE
      
      COSPHI=UN(1,I,M)*UN(1,J,M)+UN(2,I,M)*UN(2,J,M)+UN(3,I,M)*UN(3,J,M)
      SINPHI2=ONE-COSPHI**2
      IF (SINPHI2.LT.EPS) GOTO 30
      JH=3*(J-1)
      GIJ=RKF*EXP(AMI*(R0MI-RMI2)+AMJ*(R0MJ-RMJ2))
      SINPHI=SQRT(SINPHI2)
      SI(1)=(UN(1,M,I)*COSPHI-UN(1,M,J))/(RMI*SINPHI)
      SI(2)=(UN(2,M,I)*COSPHI-UN(2,M,J))/(RMI*SINPHI)
      SI(3)=(UN(3,M,I)*COSPHI-UN(3,M,J))/(RMI*SINPHI)
      SJ(1)=(COSPHI*UN(1,M,J)-UN(1,M,I))/(RMJ*SINPHI)
      SJ(2)=(COSPHI*UN(2,M,J)-UN(2,M,I))/(RMJ*SINPHI)
      SJ(3)=(COSPHI*UN(3,M,J)-UN(3,M,I))/(RMJ*SINPHI)
      SM(1)=-SI(1)-SJ(1)
      SM(2)=-SI(2)-SJ(2)
      SM(3)=-SI(3)-SJ(3)
      DO 22 IX=1,3
      DO 20 JX=1,3   
      IF (M.GT.I) THEN
         HESSE(IX+MH,JX+IH)=HESSE(IX+MH,JX+IH)+GIJ*SM(IX)*SI(JX)
      ELSE
         HESSE(IX+IH,JX+MH)=HESSE(IX+IH,JX+MH)+GIJ*SI(IX)*SM(JX)
      ENDIF
      IF (M.GT.J) THEN
         HESSE(IX+MH,JX+JH)=HESSE(IX+MH,JX+JH)+GIJ*SM(IX)*SJ(JX)
      ELSE
         HESSE(IX+JH,JX+MH)=HESSE(IX+JH,JX+MH)+GIJ*SJ(IX)*SM(JX)
      ENDIF
      IF (I.GT.J) THEN
         HESSE(IX+IH,JX+JH)=HESSE(IX+IH,JX+JH)+GIJ*SI(IX)*SJ(JX)
      ELSE
         HESSE(IX+JH,JX+IH)=HESSE(IX+JH,JX+IH)+GIJ*SJ(IX)*SI(JX)
      ENDIF
   20 CONTINUE
      DO 21 JX=1,IX
      HESSE(IX+IH,JX+IH)=HESSE(IX+IH,JX+IH)+GIJ*SI(IX)*SI(JX)
      HESSE(IX+MH,JX+MH)=HESSE(IX+MH,JX+MH)+GIJ*SM(IX)*SM(JX)
      HESSE(IX+JH,JX+JH)=HESSE(IX+JH,JX+JH)+GIJ*SJ(IX)*SJ(JX)
   21 CONTINUE
   22 CONTINUE
 30   CONTINUE
 31   CONTINUE
 32   CONTINUE
C
C     CONTRIBUTIONS FROM DIHEDRALS
C
      DO 53 J = 1,NUMAT
      JR=IROW(NAT(J))
      JH=3*(J-1)
      DO 52 K = 1,NUMAT
      IF (K.EQ.J) GOTO 52
      KR=IROW(NAT(K))
      RJK2=R(K,J)**2
      RJK0=RMOD(KR,JR)**2
      IF (RJK2.GT.RJK0*TRUNC) GOTO 52
      AJK=AMOD(JR,KR)
      KH=3*(K-1)
      DO 51 I = 1,NUMAT
      IF (I.EQ.J.OR.I.EQ.K) GOTO 51
      IR=IROW(NAT(I))
      RIJ2=R(I,J)**2
      RIJ0=RMOD(IR,JR)**2
      IF (RIJ2.GT.RIJ0*TRUNC) GOTO 51
      
      COSIJK=UN(1,I,J)*UN(1,K,J)+UN(2,I,J)*UN(2,K,J)+UN(3,I,J)*UN(3,K,J)
      SINIJK2=ONE-COSIJK**2
      IF (SINIJK2.LT.EPS) GOTO 51
      SRIJK2=ONE/(R(I,J)*SINIJK2)
C     COMPUTE THE WILSON-DECIUS-CROSS "B" MATRIX FOR TORSION (1/2).
      C(1,1)=(UN(2,I,J)*UN(3,J,K)-UN(3,I,J)*UN(2,J,K))*SRIJK2
      C(2,1)=(UN(3,I,J)*UN(1,J,K)-UN(1,I,J)*UN(3,J,K))*SRIJK2
      C(3,1)=(UN(1,I,J)*UN(2,J,K)-UN(2,I,J)*UN(1,J,K))*SRIJK2
      CIJK(1)=(R(K,J)-R(I,J)*COSIJK)*C(1,1)
      CIJK(2)=(R(K,J)-R(I,J)*COSIJK)*C(2,1)
      CIJK(3)=(R(K,J)-R(I,J)*COSIJK)*C(3,1)
      AIJ =AMOD(IR,JR)
      IJRANK=NUMAT*(J-1)+I
      IH=3*(I-1)
      DO 50 L = 1,NUMAT
      IF (L.EQ.I.OR.L.EQ.J.OR.L.EQ.K) GOTO 50
      IF (IJRANK.LE.NUMAT*(K-1)+L) GOTO 50
      LR=IROW(NAT(L))  
      RKL2=R(L,K)**2 
      RKL0=RMOD(LR,KR)**2
      IF (RKL2.GT.RKL0*TRUNC) GOTO 50
      
      COSJKL=UN(1,J,K)*UN(1,L,K)+UN(2,J,K)*UN(2,L,K)+UN(3,J,K)*UN(3,L,K)
      SINJKL2=ONE-COSJKL**2
      IF (SINJKL2.LT.EPS) GOTO 50
      SRJKL2=ONE/(R(L,K)*SINJKL2)
      AKL =AMOD(KR,LR)
      TIJ=RKT*EXP(AIJ*(RIJ0-RIJ2)+AJK*(RJK0-RJK2)+AKL*(RKL0-RKL2))
      LH=3*(L-1)
C     COMPUTE THE WILSON-DECIUS-CROSS "B" MATRIX FOR TORSION (2/2).
      DO 40 IX=1,3   
      IY=IX+1
      IF (IY.GT.3) IY=IY-3
      IZ=IY+1
      IF (IZ.GT.3) IZ=IZ-3
      C(IX,4)=(UN(IY,L,K)*UN(IZ,K,J)-UN(IZ,L,K)*UN(IY,K,J))*SRJKL2
      C(IX,2)=-(CIJK(IX)+R(L,K)*COSJKL*C(IX,4))/R(K,J)
      C(IX,3)=-C(IX,1)-C(IX,2)-C(IX,4)
   40 CONTINUE
      DO 43 IX=1,3
C     OFF DIAGONAL BLOCK
      DO 41 JX=1,3
      HESSE(IX+IH,JX+JH)=HESSE(IX+IH,JX+JH)+TIJ*C(IX,1)*C(JX,2)
      HESSE(IX+IH,JX+KH)=HESSE(IX+IH,JX+KH)+TIJ*C(IX,1)*C(JX,3)
      HESSE(IX+IH,JX+LH)=HESSE(IX+IH,JX+LH)+TIJ*C(IX,1)*C(JX,4)
      HESSE(IX+JH,JX+KH)=HESSE(IX+JH,JX+KH)+TIJ*C(IX,2)*C(JX,3)
      HESSE(IX+JH,JX+LH)=HESSE(IX+JH,JX+LH)+TIJ*C(IX,2)*C(JX,4)
      HESSE(IX+KH,JX+LH)=HESSE(IX+KH,JX+LH)+TIJ*C(IX,3)*C(JX,4)
   41 CONTINUE  
C     DIAGONAL BLOCK   
      DO 42 JX=1,IX
      HESSE(IX+IH,JX+IH)=HESSE(IX+IH,JX+IH)+TIJ*C(IX,1)*C(JX,1)
      HESSE(IX+JH,JX+JH)=HESSE(IX+JH,JX+JH)+TIJ*C(IX,2)*C(JX,2)
      HESSE(IX+KH,JX+KH)=HESSE(IX+KH,JX+KH)+TIJ*C(IX,3)*C(JX,3)
      HESSE(IX+LH,JX+LH)=HESSE(IX+LH,JX+LH)+TIJ*C(IX,4)*C(JX,4)
   42 CONTINUE
   43 CONTINUE
 50   CONTINUE
 51   CONTINUE
 52   CONTINUE
 53   CONTINUE
C     COLLECT TERMS AND SYMMETRIZE.
      DO 61 J=2,N3
      DO 60 I=1,J-1
      HESSE(I,J)=HESSE(I,J)+HESSE(J,I)
      HESSE(J,I)=HESSE(I,J)
   60 CONTINUE
   61 CONTINUE
      END

