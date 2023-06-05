      SUBROUTINE DHC (P,PA,PB,XI,NAT,IF,IM,IL,JF,JM,JL,
     1NORBS,DENER,*)                                                    CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION P(*), PA(*), PB(*)
      DIMENSION XI(3,*),NFIRST(2),NMIDLE(2),NLAST(2),NAT(*)
      dimension idiag(MAXORB),idiag1(MAXORB)
CGL0892  save idiag,idiag1
C***********************************************************************
C
C  DHC CALCULATES THE ENERGY CONTRIBUTIONS FROM THOSE PAIRS OF ATOMS
C         THAT HAVE BEEN MOVED BY SUBROUTINE DERIV.
C
C***********************************************************************
      COMMON /KEYWRD/ KEYWRD
     1       /ONELEC/ USS(120),UPP(120),UDD(120)
      COMMON /EULER / TVEC(3,3), ID
      COMMON /NUMCAL/ NUMCAL
      CHARACTER*160 KEYWRD
      LOGICAL UHF
      DIMENSION H(171), SHMAT(9,9), F(171),
     1          WJ(100), E1B(10), E2A(10), WK(100), W(100)
      COMMON /SUMDISP/ DISPTOTAL, LDISPON                               LF0412
      LOGICAL LDISPON                                                   LF0412
         SAVE                                                           GL0892
      DATA ICALCN /0/
      IF( ICALCN.NE.NUMCAL) THEN
         WLIM=4.D0
         IF(ID.EQ.0)WLIM=0.D0
         UHF=(INDEX(KEYWRD,'UHF') .NE. 0)
      ENDIF
      NFIRST(1)=1
      NMIDLE(1)=IM-IF+1
      NLAST(1)=IL-IF+1
      NFIRST(2)=NLAST(1)+1
      NMIDLE(2)=NFIRST(2)+JM-JF
      NLAST(2)=NFIRST(2)+JL-JF
      LINEAR=(NLAST(2)*(NLAST(2)+1))/2
      DO 10 I=1,LINEAR
         F(I)=0.D0
   10 H(I)=0.0D00
      if(ICALCN.NE.NUMCAL)  then
         do 11 i=1,maxorb
         idiag(i)=i*(i+1)/2
         idiag1(i)=i*(i-1)/2
11       continue
         ICALCN=NUMCAL
       endif
      JA=NFIRST(2)
      JB=NLAST(2)
      JC=NMIDLE(2)
      IA=NFIRST(1)
      IB=NLAST(1)
      IC=NMIDLE(1)
      JT=JB*(JB+1)/2
      J=2
      I=1
      NJ=NAT(2)
      NI=NAT(1)
      CALL H1ELEC(NI,NJ,XI(1,1),XI(1,2),SHMAT,*9999)                     CSTP(call)
      IF(NAT(1).EQ.102.OR.NAT(2).EQ.102) THEN
         K=(JB*(JB+1))/2
         DO 30 J=1,K
   30    H(J)=0.D0
      ELSE
         J1=0
         DO 40 J=JA,JB
            jj=idiag1(j)
            J1=J1+1
            I1=0
            DO 40 I=IA,IB
               JJ=JJ+1
               I1=I1+1
               H(JJ)=SHMAT(I1,J1)
               F(JJ)=SHMAT(I1,J1)
   40    CONTINUE
      ENDIF
      KR=1
      IF(ID.EQ.0)THEN
         LDISPON=.FALSE.                                                 LF0412
         CALL ROTATE (NJ,NI,XI(1,2),XI(1,1),W(KR),KR,E2A,E1B,ENUCLR,100.
     1D0,GTERM,*9999)                                                    CSTP(call)
      ELSE
         CALL SOLROT (NJ,NI,XI(1,2),XI(1,1),WJ,WK,KR,E2A,E1B,ENUCLR,100.
     1D0,*9999)                                                          CSTP(call)
      ENDIF
      IF(WJ(1).LT.WLIM)THEN
         DO 50 I=1,KR-1
   50    WK(I)=0.D0
      ENDIF
C
C    * ENUCLR IS SUMMED OVER CORE-CORE REPULSION INTEGRALS.
C
      I2=0
      DO 60 I1=IA,IC
         ii=idiag1(i1)+ia-1
         DO 60 J1=IA,I1
            II=II+1
            I2=I2+1
            H(II)=H(II)+E1B(I2)
   60 F(II)=F(II)+E1B(I2)
cdir$ ivdep
      DO  70 I1=IC+1,IB
         ii=idiag(i1)
         F(II)=F(II)+E1B(1)
   70 H(II)=H(II)+E1B(1)
      I2=0
      DO 80 I1=JA,JC
         ii=idiag1(i1)+ja-1
         DO 80 J1=JA,I1
            II=II+1
            I2=I2+1
            H(II)=H(II)+E2A(I2)
   80 F(II)=F(II)+E2A(I2)
cdir$ ivdep
      DO 90 I1=JC+1,JB
         ii=idiag(i1)
         F(II)=F(II)+E2A(1)
   90 H(II)=H(II)+E2A(1)
      CALL FOCK2D(F,P,PA,W, WJ, WK,2,NFIRST,NMIDLE,NLAST,*9999)          CSTP(call)
      EE=HELECT(NLAST(2),PA,H,F)
      IF( UHF ) THEN
         DO 100 I=1,LINEAR
  100    F(I)=H(I)
         CALL FOCK2D(F,P,PB,W, WJ, WK,2,NFIRST,NMIDLE,NLAST,*9999)       CSTP(call)
         EE=EE+HELECT(NLAST(2),PB,H,F)
      ELSE
         EE=EE*2.D0
      ENDIF
      DENER=EE+ENUCLR
      RETURN
C
 9999 RETURN 1                                                          CSTP
      ENTRY DHC_INIT                                                    CSAV
                D0 = 0.0D0                                              CSAV
                EE = 0.0D0                                              CSAV
            ENUCLR = 0.0D0                                              CSAV
             GTERM = 0.0D0                                              CSAV
        DO 8890 I=1, 10                                                 CSAV
               E1B(I)=0.0D0                                             CSAV
               E2A(I)=0.0D0                                             CSAV
 8890   CONTINUE                                                        CSAV
        DO 8891 I=1, 9                                                  CSAV
        DO 8891 J=1, 9                                                  CSAV
             SHMAT(I,J)=0.0D0                                           CSAV
 8891   CONTINUE                                                        CSAV
        DO 8892 I=1, 171                                                CSAV
                 H(I) = 0.0D0                                           CSAV
                 F(I) = 0.0D0                                           CSAV
 8892   CONTINUE                                                        CSAV
        DO 8893 I=1, 100                                                CSAV
                 W(I) = 0.0D0                                           CSAV
                WJ(I) = 0.0D0                                           CSAV
                WK(I) = 0.0D0                                           CSAV
 8893   CONTINUE                                                        CSAV
                 I = 0                                                  CSAV
                I1 = 0                                                  CSAV
                I2 = 0                                                  CSAV
                IA = 0                                                  CSAV
                IB = 0                                                  CSAV
                IC = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
        DO 8894 I=1, MAXORB                                             CSAV
             IDIAG(I)=0                                                 CSAV
            IDIAG1(I)=0                                                 CSAV
 8894   CONTINUE                                                        CSAV
                II = 0                                                  CSAV
                 J = 0                                                  CSAV
                J1 = 0                                                  CSAV
                JA = 0                                                  CSAV
                JB = 0                                                  CSAV
                JC = 0                                                  CSAV
                JJ = 0                                                  CSAV
                JT = 0                                                  CSAV
                 K = 0                                                  CSAV
                KR = 0                                                  CSAV
            LINEAR = 0                                                  CSAV
            NFIRST = 0                                                  CSAV
                NI = 0                                                  CSAV
                NJ = 0                                                  CSAV
             NLAST = 0                                                  CSAV
            NMIDLE = 0                                                  CSAV
               UHF = .FALSE.                                            CSAV
              WLIM = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
