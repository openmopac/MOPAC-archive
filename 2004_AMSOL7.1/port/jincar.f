      SUBROUTINE JINCAR (COORD,T,NCART,FAIL) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
C     BUILD THE JACOBIAN MATRIX T(I,J)=dinternal(i)/dcartesian(j) 
C     INTERNAL (ROW) ARE ORDERED AS PROVIDED BY THE INPUT DATA: 
C            ATOM 1     RHO(1)   THETA(2)    PHI(3) 
C            ATOM 2     RHO(4)   THETA(5)    PHI(6) AND SO ON 
C     CARTESIAN (COLUMN) ARE ORDERED AS FOLLOWS: 
C            ATOM 1      X (1)      Y (2)     Z (3) 
C            ATOM 2      X (4)      Y (5)     X (6) AND SO ON 
C     DUMMY ATOMS ARE ACCEPTED IF NOT ENGAGED IN THE DEFINITION OF A 
C     OPTIMIZED INTERNAL COORDINATE. 
C     THE JACOBIAN IS WORKED OUT ANALYTICALLY WITH RESPECT TO THE 
C     SPHERICAL COORDINATES IN THE LOCAL FRAME SPANNED BY NA,NB,NC 
C                             BUT 
C     ONE USES A TWO-POINT FINITE DIFFERENCE METHOD TO GET THE 
C     DERIVATIVES OF THE LOCAL FRAME WITH RESPECT TO THE CARTESIAN OF 
C     NA,NB,NC. 
C     SUCH A MIXED STRATEGY INSURES THE CONTINUITY FOR EITHER 
C     AN ANGLE THETA=0,180 OR A DIHEDRAL PHI=180,-180 
C     AND PROVIDES A GOOD ACCURACY WITH RESPECT TO THE HESSIAN ERROR. 
C     D.L. (MJS DEWAR GROUP) JUNE 1986 
C 
       INCLUDE 'SIZES.i'
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),NA(NUMATM),NB(NUMATM) 
     .               ,NC(NUMATM) 
     .       /GEOM  / GEO(3,NUMATM) 
C    .       /GEOVAR/ NVAR,LOC(2,MAXPAR) 
C    .       /SCRACH/ R(3,3),R1(3,3),R2(3,3),SJ(3,3),XD(3),SJR(3,3) 
C    .               ,FILA(3,3),FILB(3,3),LOCXYZ(NUMATM) 
      COMMON /GEOVAR/ XPARAM(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY           3GL3092
C
C     /SCRACH/ HAS BEEN CONVERTED TO /SCRCHR/ AND /SCRCHI/ FOR AMSOL    
      COMMON /SCRCHR/ R(3,3),R1(3,3),R2(3,3),SJ(3,3),XD(3),SJR(3,3),    GCL0892
     1                FILA(3,3),FILB(3,3), DUM3(6*MAXPAR**2-7*(3*3)-2)  GCL0393
      COMMON /SCRCHI/ LOCXYZ(NUMATM), IDUM3(4*NUMATM - NUMATM)          3GL3092
C
      DIMENSION COORD(3,*),T(MAXPAR,MAXPAR),SP(3,9) 
      EQUIVALENCE (SP(1,1),R(1,1)) 
      LOGICAL FLAGA,FLAGB,FAIL 
      SAVE
C 
C     STEP SIZE 'STEP' FOR FINITE DIFFERENCE IN CARTESIAN SPACE. 
      DATA STEP /1.D-6/ 
      STEP2=2.0D0*STEP 
C 
C     COLUMN (I.E. THE CARTESIANS) COUNTER, REMOVING DUMMY ATOMS 
      NCART=0 
      DO 1 I=1,NATOMS 
      IF (LABELS(I).GE.99) GO TO 1 
      LOCXYZ(I)=NCART 
      NCART=NCART+3 
    1 CONTINUE 
      FAIL=.FALSE. 
C 
C     ANALYTICAL EXPRESSIONS FOR FIRST ATOMS 
      NVAR1=1 
      IF (LOC(1,1).EQ.2) THEN 
C        BOND LENGTH 1-2 
         DO 10  I=1,9 
   10    SP(1,I)= 0.D0 
         SP(1,1)=-1.D0 
         SP(1,4)= 1.D0 
         NVAR1=NVAR1+1 
      ENDIF 
      IF (LOC(1,NVAR1).EQ.3) THEN 
         RHO   =GEO(1,3) 
         THETA =GEO(2,3) 
         SINT=SIN(THETA) 
         COST=COS(THETA) 
         IF (LOC(2,NVAR1).EQ.1) THEN 
C           BOND LENGTH 2-3 
            DO 11 I=1,9 
   11       SP(NVAR1,I)=0.D0 
            SP(NVAR1,4)= COST 
            SP(NVAR1,5)=-SINT 
            SP(NVAR1,7)=-COST 
            SP(NVAR1,8)= SINT 
            NVAR1=NVAR1+1 
         ENDIF 
      ENDIF 
      IF (LOC(1,NVAR1).EQ.3) THEN 
C        ANGLE 3-2-1 
         DO 12 I=1,9 
   12    SP(NVAR1,I)=0.D0 
         SP(NVAR1,2)=-1.D0/GEO(1,2) 
         SP(NVAR1,4)=-SINT/RHO 
         SP(NVAR1,7)=-SP(NVAR1,4) 
         SP(NVAR1,8)= COST/RHO 
         SP(NVAR1,5)=-SP(NVAR1,2)-SP(NVAR1,8) 
      ENDIF 
      IF (NVAR1.GT.1) THEN 
C        FILL FIRST ROWS OF THE JACOBIAN T 
         K=0 
         IF(LABELS(1).LT.99) THEN 
            DO 13 J=1,3 
            K=K+1 
            DO 13 I=1,NVAR1 
   13       T(I,K)=SP(I,J) 
         ENDIF 
         IF(LABELS(2).LT.99) THEN 
            DO 14 J=4,6 
            K=K+1 
            DO 14 I=1,NVAR1 
   14       T(I,K)=SP(I,J) 
         ENDIF 
         IF(LABELS(3).LT.99) THEN 
            DO 15 J=7,9 
            K=K+1 
            DO 15 I=1,NVAR1 
   15       T(I,K)=SP(I,J) 
         ENDIF 
         IF (NCART.EQ.K) RETURN 
         K=K+1 
         DO 16 I=1,NVAR1 
         DO 16 J=K,NCART 
   16    T(I,J)=0.D0 
         IF (NVAR1.EQ.NVAR) RETURN 
         NVAR1=NVAR1+1 
      ENDIF 
C 
C     MAIN LOOP ON THE ROW (I.E. THE INTERNAL VARIABLES) 
      NNOLD=0 
      DO 100 N=NVAR1,NVAR 
      NND=LOC(1,N) 
      NTYP=LOC(2,N) 
      IF (NND.NE.NNOLD) THEN 
         NNA=NA(NND) 
         NNB=NB(NND) 
         NNC=NC(NND) 
         NNOLD=NND 
C        FIND THE LOCAL FRAME: R 
         CALL RLOCAL (COORD,NNA,NNB,NNC,R) 
C        SPHERICAL COORDINATES OF D IN THE LOCAL FRAME 
         RHO  =GEO(1,NND) 
         THETA=GEO(2,NND) 
         PHI  =GEO(3,NND) 
         SINT=SIN(THETA) 
         COST=COS(THETA) 
         SINP=SIN(PHI) 
         COSP=COS(PHI) 
C        CARTESIAN COORDINATES OF D IN THE LOCAL FRAME: XD 
         W=RHO*SINT 
         XD(1)=W*COSP 
         XD(2)=W*SINP 
         XD(3)=RHO*COST 
C        JACOBIAN D(SPHERICAL)/D( LOCAL CARTESIAN): SJ 
         SJ(1,1)=SINT*COSP 
         SJ(1,2)=SINT*SINP 
         SJ(1,3)=COST 
         W=COST/RHO 
         SJ(2,1)=W*COSP 
         SJ(2,2)=W*SINP 
         SJ(2,3)=-SINT/RHO 
         IF(ABS(SINT).LT.1.D-30) THEN 
C           (SHOULD NEVER BE USED) 
            SJ(3,1)=0.D0 
            SJ(3,2)=0.D0 
         ELSE 
            W=1.D0/(RHO*SINT) 
            SJ(3,1)=-W*SINP 
            SJ(3,2)= W*COSP 
         ENDIF 
         SJ(3,3)=0.D0 
C        PRODUCT (SJR) = (SJ) * (R)' 
         DO 21 I=1,3 
         DO 21 J=1,3 
         W=0.D0 
         DO 20 K=1,3 
   20    W=W+SJ(I,K)*R(J,K) 
   21    SJR(I,J)=W 
         FLAGA=.FALSE. 
         FLAGB=.FALSE. 
      ENDIF 
C     FILL THE NON ZERO ELEMENTS OF THE ROW NUMBER N ... 
      DO 30 I=1,NCART 
   30 T(N,I)=0.D0 
C     ... DUE TO THE CARTESIAN OF ATOM D 
      IF(LABELS(NND).GE.99) THEN 
         FAIL=.TRUE. 
         RETURN 
      ENDIF 
      DO 40 I=1,3 
   40 T(N,LOCXYZ(NND)+I)=SJR(NTYP,I) 
C     ... DUE TO THE CARTESIAN OF ATOM A 
      IF(LABELS(NNA).GE.99) THEN 
         FAIL=.TRUE. 
         RETURN 
      ENDIF 
      DO 58 I=1,3 
C     BECAUSE OF TRANSLATION: 
      T(N,LOCXYZ(NNA)+I)=-SJR(NTYP,I) 
C     BECAUSE OF ROTATION (CENTRAL DIFFERENCES ON EULERIAN ANGLES): 
      IF(FLAGA) GO TO 58 
      COORD(I,NNA)=COORD(I,NNA)+STEP 
      CALL RLOCAL(COORD,NNA,NNB,NNC,R1) 
      COORD(I,NNA)=COORD(I,NNA)-STEP2 
      CALL RLOCAL(COORD,NNA,NNB,NNC,R2) 
      COORD(I,NNA)=COORD(I,NNA)+STEP 
      DO 50 J=1,9 
   50 R2(J,1)=(R1(J,1)-R2(J,1))/STEP2 
      DO 52 J=1,3 
      DO 52 K=1,3 
      W=0.D0 
      DO 51 L=1,3 
   51 W=W+SJR(J,L)*R2(L,K) 
   52 R1(J,K)=W 
      DO 54 J=1,3 
      W=0.D0 
      DO 53 K=1,3 
   53 W=W+R1(J,K)*XD(K) 
   54 FILA(J,I)=W 
   58 T(N,LOCXYZ(NNA)+I)=T(N,LOCXYZ(NNA)+I)-FILA(NTYP,I) 
      FLAGA=.TRUE. 
      IF(NTYP.NE.1) THEN 
C        ... DUE TO THE CARTESIAN OF ATOM B 
         IF(LABELS(NNB).GE.99) THEN 
            FAIL=.TRUE. 
            RETURN 
         ENDIF 
         DO 68 I=1,3 
C        BECAUSE OF ROTATION (CENTRAL DIFFERENCES ON EULERIAN ANGLES): 
         IF(FLAGB) GO TO 68 
         COORD(I,NNB)=COORD(I,NNB)+STEP 
         CALL RLOCAL(COORD,NNA,NNB,NNC,R1) 
         COORD(I,NNB)=COORD(I,NNB)-STEP2 
         CALL RLOCAL(COORD,NNA,NNB,NNC,R2) 
         COORD(I,NNB)=COORD(I,NNB)+STEP 
         DO 60 J=1,9 
   60    R2(J,1)=(R1(J,1)-R2(J,1))/STEP2 
         DO 62 J=2,3 
         DO 62 K=1,3 
         W=0.D0 
         DO 61 L=1,3 
   61    W=W+SJR(J,L)*R2(L,K) 
   62    R1(J,K)=W 
         DO 64 J=2,3 
         W=0.D0 
         DO 63 K=1,3 
   63    W=W+R1(J,K)*XD(K) 
   64    FILB(J,I)=W 
   68    T(N,LOCXYZ(NNB)+I)=-FILB(NTYP,I) 
         FLAGB=.TRUE. 
      ENDIF 
      IF(NTYP.EQ.3) THEN 
C        ... DUE TO THE CARTESIAN OF ATOM C 
         IF(LABELS(NNC).GE.99) THEN 
            FAIL=.TRUE. 
            RETURN 
         ENDIF 
         DO 78 I=1,3 
C        BECAUSE OF ROTATION (CENTRAL DIFFERENCES ON EULERIAN ANGLES): 
         COORD(I,NNC)=COORD(I,NNC)+STEP 
         CALL RLOCAL(COORD,NNA,NNB,NNC,R1) 
         COORD(I,NNC)=COORD(I,NNC)-STEP2 
         CALL RLOCAL(COORD,NNA,NNB,NNC,R2) 
         COORD(I,NNC)=COORD(I,NNC)+STEP 
         DO 70 J=1,9 
   70    R2(J,1)=(R1(J,1)-R2(J,1))/STEP2 
         DO 72 K=1,3 
         W=0.D0 
         DO 71 L=1,3 
   71    W=W+SJR(3,L)*R2(L,K) 
   72    R1(3,K)=W 
         W=0.D0 
         DO 73 K=1,3 
   73    W=W+R1(3,K)*XD(K) 
   78    T(N,LOCXYZ(NNC)+I)=-W 
      ENDIF 
  100 CONTINUE 
      RETURN 
      END 
