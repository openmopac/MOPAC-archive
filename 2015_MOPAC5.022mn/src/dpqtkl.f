      SUBROUTINE DPQTKL (C34,NBAND,W,PQ34,NATI,LENGTH) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
C      INCLUDE 'KEYS.i'                                                 DJG0995
      DIMENSION C34(*),NBAND(*),W(*),PQ34(*) 
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN                                      3GL3092
C------------------------------------------------------------------ 
C    PQTKL WORKS OUT  IN MNDO OR MINDO FORMALISMS THE FIRST 2-INDICES 
C          TRANSFORMATION RUNNING OVER THE NON VANISHING 2-ELECTRONS 
C          INTEGRALS DERIVATIVES WITH RESPECT TO A CARTESIAN COORDINATE 
C          OF THE ATOM # NATI. 
C  INPUT 
C     C34     : CHARGE-DISTRIBUTION VECTOR BETWEEN TWO M.O. '3' & '4'. 
C     NBAND(I): NUMBER OF COUPLES OF A.O BELONGING TO ATOM # I. 
C     W     : 2-CENTRES 2-ELECTRONS INTEGRALS DERIVATIVES IN A.O. BASIS, 
C             STORED ACCORDING TO ROUTINE DHCORE. 
C     NATI  : # OF THE MOVING ATOM. 
C     LENGTH: LENGTH OF THE CHARGE-DISTRIBUTION VECTOR C34. 
C  OUTPUT 
C     PQ34(PQ) : <P(1),Q(1)!d(1/R12)!C3(2),C4(2)> WHERE P ,Q  ARE A.O. 
C                                                   AND C3,C4 ARE M.O. 
C                P AND Q RUN IN CANONICAL ORDER OVER THE A.O BELONGING 
C                TO AN ATOM 'A' ONLY (BASIC ASSUMPTION OF MNDO SCHEME) 
C                AND 'A' .NE. 'NATI' RUNS OVER THE ATOMS OF THE SYSTEM. 
C     D.L. (DEWAR GROUP) 1986 
C---------------------------------------------------------------------- 
      DIMENSION LD(9),PTOT(NUMATM) 
      LOGICAL MINDO 
       SAVE
      DATA LD /0,2,5,9,14,20,27,35,44/ 
C 
      MINDO=IMINDO.NE.0                                                 DJG0995
      LS=LENGTH-NBAND(NATI)+1 
      DO 10 I=1,LENGTH 
   10 PQ34(I)=0.D0 
      IF (MINDO) THEN 
C        WE ARE IN MINDO OPTION. 
C        ----------------------- 
C        PTOTNA = CHARGE ON ATOM NATI. 
         IA=NFIRST(NATI) 
         IC=NLAST(NATI) 
         PTOTNA=0.D0 
         DO 20 L=1,IC-IA+1 
   20    PTOTNA=PTOTNA+C34(LS+LD(L)) 
C        PTOT(KK) = CHARGE ON ATOM KK, SPARKLES AND NATI EXCLUDED. 
         KK=0 
         LL=1 
         DO 40 JJ=1,NUMAT 
         JA=NFIRST(JJ) 
         JC=NLAST (JJ) 
         IF (JC.LT.JA.OR.JJ.EQ.NATI) GO TO 40 
         KK=KK+1 
         PTOT(KK)=0.D0 
C        DISTRIBUTE CHARGE OF NATI. 
         GMINDO=W(KK)*PTOTNA 
         DO 30 L=1,JC-JA+1 
         PQ34(LL+LD(L))=PQ34(LL+LD(L))+GMINDO 
   30    PTOT(KK)=PTOT(KK)+C34(LL+LD(L)) 
         LL=LL+NBAND(JJ) 
   40    CONTINUE 
C        DISTRIBUTE CHARGES OF ATOMS B .NE. NATI. 
         GMINDO=DOT(W,PTOT,KK) 
         DO 50 L=1,IC-IA+1 
   50    PQ34(LS+LD(L))=PQ34(LS+LD(L))+GMINDO 
      ELSE 
C        WE ARE IN MNDO OR AM1 OPTION. 
C        ----------------------------- 
         IPQRS=1 
C        LOOP OVER CHARGE DISTRIBUTION OF ATOMS  B .NE. NATI . 
         IJOLD=LS-1 
         CALL MXM (C34,1,W,IJOLD,PQ34(LS),NBAND(NATI)) 
C        DISTRIBUTE THE CHARGE DISTRIBUTION OF NATI. 
         DO 60 I=LS,LENGTH 
         CALL SAXPY (IJOLD,C34(I),W(IPQRS),1,PQ34,1) 
   60    IPQRS=IPQRS+IJOLD 
      ENDIF 
      RETURN 
      END 
