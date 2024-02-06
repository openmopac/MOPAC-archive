      SUBROUTINE MULLIK(C,UHF,H,NORBS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UHF
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
**********************************************************************
*      MULLIK DOES A MULLIKEN POPULATION ANALYSIS
*    INPUT     C      =  SQUARE ARRAY OF EIGENVECTORS.
*              H      =  PACKED ARRAY OF ONE-ELECTRON MATRIX
*    OUTPUT    IGPT      WRITTEN IF KEYWORD "GRAPH" ACTIVATED
*              OTHERWISE PRINT MULLIKEN POPULATION ON JOUT    
**********************************************************************
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBX, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /MLKSTR/ FRACT                                             3GL3092
      COMMON /BETAS / BETAS(107),BETAP(107),BETAD(107)                  DJG0995
     2       /GEOM  / GEO(3,NUMATM)                                     3GL3092
     3       /EXPONT/ ZS(107),ZP(107),ZD(107)                           3GL3092
     4       /SCRAH2/ STORE(MPACK),HS1(MPACK),VECS(MAXORB*MAXORB)       3GL3092
     5               ,EIGS(MAXORB),
     6                DUM5(NRELAX*MORB2+1-2*MPACK-MAXORB*(1+MAXORB))    GCL0892
C    4       /SCRACH/ WORK(MAXORB*MAXORB)                               3GL3092
C
C     /SCRACH/ HAS BEEN CONVERTED TO /SCRCHR/ AND /SCRCHI/ FOR AMSOL
      COMMON /SCRCHR/ WORK(MAXORB*MAXORB),                              3GL3092
     1                DUM3(6*MAXPAR**2+1-MAXORB*MAXORB)                 GCL0393
      COMMON /ELEMTS/ ELEMNT(107)                                       GDH1294
C
      CHARACTER*2 ELEMNT, ATORBS(9)                                     GDH1294
      INTEGER WORK7
      DIMENSION C(*), H(*)
      DIMENSION WORK2(MAXORB*MAXORB),WORK3(MAXORB*MAXORB)               GDH1294
      DIMENSION HS(MAXORB*MAXORB)                                       GDH1294
      DIMENSION WORK4(MAXORB*MAXORB), WORK5(MAXORB,MAXORB)              GDH1294
      DIMENSION WORK6(NUMATM,NUMATM),WORK7(MAXORB)                      GDH1294
      DIMENSION XYZ(3,NUMATM)
      DIMENSION IFACT(MAXORB)                                           3GL3092
      EQUIVALENCE (WORK(1),XYZ(1,1))
      SAVE
      DATA ATORBS/' S','PX','PY','PZ','X2','XZ','Z2','YZ','XY'/         GDH1294
      CALL SCOPY(MPACK,H,1,WORK,1)                                      GDH1194
      DO 10 I=1,NORBS
   10 IFACT(I)=(I*(I-1))/2
      IFACT(NORBS+1)=(NORBS*(NORBS+1))/2
C
C     CALCULATE THE OVERLAP MATRIX FROM H, BETAS, BETAP.
C     Then put the result in WORK.                                      DGT1294
C
      DO 50 I=1,NUMAT
         IA=NFIRST(I)
         IB=NLAST(I)
         BI=BETAS(NAT(I))
         DO 50 K=IA,IB
            II=IFACT(K)
            DO 30 J=1,I-1
               JA=NFIRST(J)
               JB=NLAST(J)
               BJ=BETAS(NAT(J))
               DO 20 JJ=JA,JB
                  WORK(II+JJ)=2.D0*WORK(II+JJ)/(BI+BJ)+ 1.D-14          GDH1194
C  THE  +1.D-14 IS TO PREVENT POSSIBLE ERRORS IN THE DIAGONALIZATION.
   20          BJ=BETAP(NAT(J))
   30       CONTINUE
            DO 40 JJ=IA,K
   40       WORK(II+JJ)=0.D0
   50 BI=BETAP(NAT(I))
      DO 60 I=1,NORBS
   60 WORK(IFACT(I+1))=1.D0
      CALL SCOPY (IFACT(NORBS+1),WORK,1,STORE,1)
C
C     WORK OUT RECIPROCAL OF SQRT(OVERLAP).  Put result in HS.          DGT1294
C
      CALL HQRII (WORK,NORBS,NORBS,EIGS,VECS)                           GDH0793
      K=0                                                               GDH1294
      DO 71 I=1,NORBS                                                   GDH1294
       EIGS(I)=1.D0/SQRT(ABS(EIGS(I)))                                  GDH1294
       DO 71 J=1,NORBS                                                  GDH1294
       K=K+1
       IF (J.EQ.I) THEN                                                 GDH1294
          WORK2(K)=EIGS(I)                                              GDH1294
       ELSE                                                             GDH1294
          WORK2(K)=0.D0                                                 GDH1294
       ENDIF                                                            GDH1294
   71  CONTINUE
      CALL MXT(VECS,WORK4,NORBS)                                        GDH1294
      CALL MXM (WORK2,NORBS,WORK4,NORBS,WORK3,NORBS)                    GDH1294
      CALL MXM (VECS,NORBS,WORK3,NORBS,HS,NORBS)                        GDH1294
      IF (IGRAPH.NE.0) THEN                                             DJG0995
*        WRITE ON UNIT 13 THE FOLLOWING DATA FOR GRAPHICS CALCULATION,
*        IN ORDER:
*           NUMBER OF ATOMS, ORBITAL, ELECTRONS
*           ALL ATOMIC COORDINATES
*           ORBITAL COUNTERS
*           ORBITAL EXPONENTS, S, P, AND D, AND ATOMIC NUMBERS
*           EIGENVECTORS (M.O.S NOT RE-NORMALISED)
*           INVERSE-SQUARE ROOT OF THE OVERLAP MATRIX.
         CALL GMETRY(GEO,XYZ)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         WRITE(JGPT)NUMAT,NORBS,NELECS,((XYZ(I,J),J=1,NUMAT),I=1,3)     GDH1095
         WRITE(JGPT)(NLAST(I),NFIRST(I),I=1,NUMAT)                      GDH1095
         WRITE(JGPT)(ZS(NAT(I)),I=1,NUMAT),(ZP(NAT(I)),I=1,NUMAT),      GDH1095
     1         (ZD(NAT(I)),I=1,NUMAT),(NAT(I),I=1,NUMAT)
         LINEAR=NORBS*NORBS
         WRITE(JGPT)(C (I),I=1,LINEAR)                                  GDH1095
         WRITE(JGPT)(HS(I),I=1,LINEAR)                                  GDH1095
      ENDIF
C
C PERFORM MULLIKEN ANALYSIS IF MULLIK KEYWORD IS SPECIFIED              GDH1294
C
C  CALCULATE MO COEFFICIENTS IN LOWDIN BASIS FROM MO COEFFICIENTS       DGT1294
C  OF NDO CALCULATIONS BY DIVIDING BY SQUARE ROOT OF OVERLAP MATRIX.    DGT1294
C  PUT RESULT IN VECS, OVERWRITING EIGENVECTORS OF OVERLAP MATRIX       GDH1294
C
      IF (IMULLI.NE.0) THEN                                             DJG0995
      CALL MXM (HS,NORBS,C,NORBS,VECS,NORBS)                            GDH1294
C  CALCULATE DENSITY MATRIX IN LOWDIN ORTHOGONALIZED BASIS FROM VECS.   DGT1294
C  PUT RESULT IN WORK.
C      CALL DENSIT(VECS,NORBS,NORBS,NCLOSE,NOPEN,FRACT,WORK)
      CALL DENSTS(VECS,NORBS,NORBS,NCLOSE,NOPEN,FRACT,WORK)             GDH1294
C  CALCULATE DENSITY (I,J) & OVERLAP (1,J) AS IN EQ.(2) OF R.S.         DGT1294
C  MULLIKEN, JCP 23, 1833(1955).  PUT THE RESULT IN WORK.               DGT1294
      DO 80 I=1,IFACT(NORBS+1)
   80 WORK(I)=WORK(I)*STORE(I)
C      DO 110 I=1,NORBS
C         SUM=0
C         DO 90 J=1,I
C   90    SUM=SUM+WORK(IFACT(I)+J)
C         DO 100 J=I+1,NORBS
C  100    SUM=SUM+WORK(IFACT(J)+I)
C  110 WORK(IFACT(I+1))=SUM
C  PRINT A MATRIX WHOSE DIAGONAL ELEMENTS ARE NET ATOMIC POPULATIONS    DGT1294
C  AND WHOSE OFF-DIAGONAL ELEMENTS ARE ONE HALF THE OVERLAP POPULATION  DGT1294
C  THE OVERLAP POPULATION, I.E., THE OVERLAP POPULATION OF BOND I-J     DGT1294
C  IS DIVIDED EQUALLY BETWEEN THE I,J AND J,I MATRIX ELEMENTS.          DGT1294
C  THE SUM OF ALL THE ELEMENTS OF THE MATRIX IS THEREFORE EQUAL TO THE  DGT1294
C  NUMBER OF ELECTRONS                                                  DGT1294
      CALL VECPRT(WORK,NORBS)
      CALL MXP(WORK,WORK5,NORBS)                                        GDH1294
      WRITE(JOUT,111)                                                   GDH1294
  111 FORMAT(/,'  Gross orbital populations:',/)                        GDH1294
         K4=0                                                           GDH1294
      DO 123 K=1,NUMAT                                                  GDH1294
         K1=NFIRST(K)                                                   GDH1294
         K2=NLAST(K)                                                    GDH1294
         K3=0                                                           GDH1294
      DO 112 I=K1,K2                                                    GDH1294
         SUM=0.D0                                                       GDH1294
         K3=K3+1                                                        GDH1294
         DO 114 J=1,NORBS                                               GDH1294
  114       SUM=SUM + WORK5(I,J)                                        GDH1294
      WRITE(JOUT,115) ATORBS(K3),ELEMNT(NAT(K)),K4+K3,SUM               GDH1294
  115 FORMAT (1X,A2,1X,A2,1X,I3,10F11.6)                                GDH1294
      WORK7(K4+K3)=K                                                    GDh1294
  112 CONTINUE                                                          GDH1294
      K4=K4+K3                                                          GDH1294
  123 CONTINUE                                                          GDH1294
      WRITE(JOUT,116)                                                   GDH1294
  116 FORMAT (/,'  Condensed to atoms (all electrons):')                GDH1294
      DO 124 K=1,NUMAT                                                  GDH1294
         K1=NFIRST(K)                                                   GDH1294
         K2=NLAST(K)                                                    GDH1294
      DO 126 I=K1,K2                                                    GDH1294
         SUM=0.D0                                                       GDH1294
         DO 127 J=1,NORBS                                               GDH1294
            WORK6(K,WORK7(J))=WORK5(I,J)+WORK6(K,WORK7(J))              GDH1294
  127    CONTINUE                                                       GDH1294
  126 CONTINUE                                                          GDH1294
  124 CONTINUE                                                          GDH1294
      K=0                                                               GDH1294
      DO 128 I=1,NUMAT                                                  GDH1294
        DO 128 J=1,I                                                    GDH1294
          K=K+1                                                         GDH1294
  128     STORE(K)=WORK6(I,J)                                           GDH1294
      CALL VECPRT(STORE,NUMAT)                                          GDH1294
      ENDIF                                                             GDH1294
      RETURN
      END
