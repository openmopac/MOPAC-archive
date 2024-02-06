      SUBROUTINE PQTKL (C34,INDX,NBAND,W,PQ34) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
      DIMENSION C34(*),INDX(*),NBAND(*),W(*),PQ34(*) 
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
     3       /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107) 
     4               ,GSD(107),GPD(107),GDD(107) 
C------------------------------------------------------------------ 
C 
C    PQTKL WORKS OUT  IN MNDO FORMALISM THE FIRST 2-INDICES TRANSFO. 
C          REQUIRED IN THE COMPUTATION OF 2-ELECTRONS REPULSION OVER M.O 
C  INPUT 
C     C34   : VECTOR OF THE CURRENT CHARGE DISTRIBUTION BETWEEN TWO M.O. 
C     INDX(I) : LOCATION IN C34 OF THE FIRST COUPLE OF A.O BELONGING TO 
C               ATOM # I. 
C     NBAND(I): NUMBER OF COUPLES OF A.O BELONGING TO ATOM # I. 
C     W     : 2-CENTRES 2-ELECTRONS INTEGRALS IN A.O. BASIS, STORED IN 
C             PACKED CANONICAL ORDER, SKIPPING OVER 1-CENTRE BLOCKS. 
C             (IN MINDO/3 OPTION THE SAME CANONICAL ORDER RUNS OVER 
C              THE ATOMS ONLY). 
C  OUTPUT 
C     PQ34(PQ) : <P(1),Q(1)!C3(2),C4(2)> WHERE P ,Q  ARE A.O. 
C                                          AND C3,C4 ARE M.O. 
C                P AND Q RUN IN CANONICAL ORDER OVER THE A.O BELONGING 
C                TO AN ATOM 'A' ONLY (BASIC ASSUMPTION OF MNDO SCHEME) 
C                AND 'A' RUNS OVER THE ATOMS OF THE SYSTEM. 
C     D.L. (DEWAR GROUP) 1986 
C---------------------------------------------------------------------- 
      DIMENSION LD(9),BUF(45),PTOT(NUMATM) 
      LOGICAL MINDO 
       SAVE
      DATA LD /0,2,5,9,14,20,27,35,44/ 
      MINDO=IMINDO.NE.0                                                 DJG0995
C     IJ    : POINTER OF CANONICAL PACKED LOCATION OF COUPLE IJ. 
C     KK    : POINTER OF SUPPORTING ATOM, SPARKLES SKIPPED OUT. 
C     IPQRS : CURRENT ENTRY POINT IN THE <PQ!RS> FILE. 
      KK=0 
      IPQRS=1 
      IJ=0 
      IJOLD=0 
C 
C     LOOP OVER OUTER ATOM A, SPARKLES EXCLUDED. 
C     ------------------------------------------ 
      DO 70 II=1,NUMAT 
      IA=NFIRST(II) 
      IB=NMIDLE(II) 
      IC=NLAST (II) 
      IF(IC.LT.IA) GO TO 70 
      KK=KK+1 
      LS=INDX(II) 
      IJ=IJ+NBAND(II) 
C 
C     PQ34(IJ) = <IJ!KL> * C34(KL)  , 1-CENTRE CONTRIBUTIONS. 
      IZN=NAT(II) 
C     BLOCK SS 
      PTOT(KK)=C34(LS) 
      PQ34(LS)=C34(LS)*GSS(IZN)*0.25D0 
      IF(IB.GT.IA) THEN 
C        BLOCK SP AND PP 
         HPP=0.5D0*(GPP(IZN)-GP2(IZN)) 
         LX=LS+LD(2) 
         LY=LS+LD(3) 
         LZ=LS+LD(4) 
         PP=C34(LX)+C34(LY)+C34(LZ) 
         PQ34(LS+1)=HSP(IZN)*C34(LS+1) 
         PQ34(LX  )=GPP(IZN)*C34(LX  )*0.25D0 
         PQ34(LS+3)=HSP(IZN)*C34(LS+3) 
         PQ34(LS+4)=HPP     *C34(LS+4) 
         PQ34(LY  )=GPP(IZN)*C34(LY  )*0.25D0 
         PQ34(LS+6)=HSP(IZN)*C34(LS+6) 
         PQ34(LS+7)=HPP     *C34(LS+7) 
         PQ34(LS+8)=HPP     *C34(LS+8) 
         PQ34(LZ  )=GPP(IZN)*C34(LZ  )*0.25D0 
         GSPSS=     GSP(IZN)*C34(LS  )*0.25D0 
         PQ34(LS)=PQ34(LS)+GSP(IZN)*PP*0.25D0 
         PQ34(LX)=PQ34(LX)+GP2(IZN)*(C34(LY)+C34(LZ))*0.25D0+GSPSS 
         PQ34(LY)=PQ34(LY)+GP2(IZN)*(C34(LZ)+C34(LX))*0.25D0+GSPSS 
         PQ34(LZ)=PQ34(LZ)+GP2(IZN)*(C34(LX)+C34(LY))*0.25D0+GSPSS 
         PTOT(KK)=PTOT(KK)+PP 
         IF(IC.GT.IB) THEN 
C           BLOCK SD, PD AND DD 
C           --- WAITING FOR 'D' PARAMETERS --- 
C               TAKE CARE : DIAGONAL ELEMENTS OF C34 ARE DOUBLED. 
         ENDIF 
      ENDIF 
      IF(KK.GT.1)THEN 
C 
C        LOOP OVER CHARGE DISTRIBUTION OF INNER ATOMS  B < A . 
C        ----------------------------------------------------- 
C        PQ34(IJ)=<IJ!KL>*C34(KL) 2-CENTRES CONTRIBUTIONS. 
C 
         IF (MINDO) THEN 
C           WE ARE IN MINDO/3 OPTION. 
            GMINDO=DOT(W(IPQRS),PTOT,KK-1) 
            DO 30 L=1,IC-IA+1 
   30       PQ34(LS+LD(L))=PQ34(LS+LD(L))+GMINDO 
            DO 50 JJ=1,II-1 
            JA=NFIRST(JJ) 
            JC=NLAST (JJ) 
            IF(JC.LT.JA) GO TO 50 
            GMINDO=W(IPQRS)*PTOT(KK) 
            DO 40 L=1,JC-JA+1 
   40       PQ34(INDX(JJ)+LD(L))=PQ34(INDX(JJ)+LD(L))+GMINDO 
            IPQRS=IPQRS+1 
   50       CONTINUE 
         ELSE 
C           WE ARE IN MNDO OR AM1 OPTION. 
            CALL MXM (C34,1,W(IPQRS),IJOLD,BUF,NBAND(II)) 
            CALL SAXPY (NBAND(II),1.D0,BUF,1,PQ34(LS),1) 
            DO 60 I=LS,IJ 
            CALL SAXPY (IJOLD,C34(I),W(IPQRS),1,PQ34,1) 
   60       IPQRS=IPQRS+IJOLD 
         ENDIF 
      ENDIF 
      IJOLD=IJ 
   70 CONTINUE 
      RETURN 
      END 
