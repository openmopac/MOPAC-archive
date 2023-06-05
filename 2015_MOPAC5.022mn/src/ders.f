      SUBROUTINE DERS(M,N,RR,DEL1,DEL2,DEL3,IS,IOL,NA,NB,KA,KB,*)       CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*    ON INPUT M    = INDEX OF FIRST ATOMIC ORBITAL                     *
*             N    = INDEX OF SECOND ATOMIC ORBITAL                    *
*             RR   = SQUARE OF INTERATOMIC DISTANCE (IN BOHR)          *
*             DEL1 = CATERSIAN DISTANCE IN DERIVATIVE DIRECTION        *
*             DEL2 = CARTESIAN DISTANCE IN M A.O.'S DIRECTION          *
*             DEL3 = CARTESIAN DISTANCE IN N A.O.'S DIRECTION          *
*             IS   = INDICATES TYPE OF A.O.-A.O. INTERACTION           *
*                  = 1 S/S, 2 S/P', 3 S/P, 4 P'/S, 5 P/S, 6 P/P',      *
*                    7 P'/P", 8 P'P', 9 P/P                            *
*             IOL  = INDEX FOR STORING DERIVATIVES IN DS               *
*             NA   = ATOMIC NUMBER OF FIRST ATOM                       *
*             NB   = ATOMIC NUMBER OF SECOND ATOM                      *
*             KA   = TYPE OF ORBITAL ON FIRST ATOM (1=S, 2-4=P)        *
*             KB   = TYPE OF ORBITAL ON SECOND ATOM (1=S, 2-4=P)       *
*                                                                      *
************************************************************************
      COMMON /DERIVS/ DS(16),DG(22),DR(100),TDX(3),TDY(3),TDZ(3)
      COMMON /TEMP/  CG(60,6),ZG(60,6)
      COMMON /TEMP2/ CCSPEC(10,6), ZZSPEC(10,6)                         LF1010
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1010
      DIMENSION SS(6,6)
CSAV         SAVE                                                           GL0892
      A0=0.529167D0
      DO 110 I=1,6
         DO 110 J=1,6
            SS(I,J)=0.0D0
            IF (PMODS(1)) THEN                                          LF1010
               IF (NA.EQ.9.AND.NB.EQ.9) THEN                            LF1010
                  APB=ZZSPEC(KA,I)*ZZSPEC(KB,J)                         LF1010
                  AMB=ZZSPEC(KA,I)+ZZSPEC(KB,J)                         LF1010
               ELSEIF (NA.EQ.8.AND.NB.EQ.8) THEN                        LF1010
                  APB=ZZSPEC(KA+4,I)*ZZSPEC(KB+4,J)                     LF1010
                  AMB=ZZSPEC(KA+4,I)+ZZSPEC(KB+4,J)                     LF1010
               ELSE
                  APB=ZG(M,I)*ZG(N,J)
                  AMB=ZG(M,I)+ZG(N,J)
               ENDIF                                                    LF1010
            ELSE                                                        LF1010
                  APB=ZG(M,I)*ZG(N,J)
                  AMB=ZG(M,I)+ZG(N,J)
            ENDIF                                                       LF1010
            ADB=APB/AMB
            ADR=MIN(ADB*RR,35.D0)
            GO TO (10,20,30,40,50,60,70,80,90),IS
   10       ABN=-2.0D0*ADB*DEL1/(A0**2)
            GO TO 100
   20       CONTINUE
            IF (PMODS(1)) THEN                                          LF1010
             IF (NA.EQ.9.AND.NB.EQ.9) THEN                              LF1010
             ABN=-4.D0*(ADB**2)*DEL1*DEL2/(SQRT(ZZSPEC(KB,J))*(A0**3))  LF1010
             ELSEIF (NA.EQ.8.AND.NB.EQ.8) THEN                          LF1010
             ABN=-4.D0*(ADB**2)*DEL1*DEL2/(SQRT(ZZSPEC(KB+4,J))*(A0**3))LF1010
             ELSE
              ABN=-4.0D0*(ADB**2)*DEL1*DEL2/(SQRT(ZG(N,J))*(A0**3))
             ENDIF                                                      LF1010
            ELSE                                                        LF1010
              ABN=-4.0D0*(ADB**2)*DEL1*DEL2/(SQRT(ZG(N,J))*(A0**3))
            ENDIF                                                       LF1010
            GO TO 100
   30       CONTINUE
            IF (PMODS(1)) THEN                                          LF1010
               IF (NA.EQ.9.AND.NB.EQ.9) THEN                            LF1010
                  ABN=(2.0D0*ADB/(SQRT(ZZSPEC(KB,J))*A0))*              LF1010
     1 (1.0D0-2.0D0*ADB*(DEL1**2)/(A0**2))                              LF1010
               ELSEIF (NA.EQ.8.AND.NB.EQ.8) THEN                        LF1010
                  ABN=(2.0D0*ADB/(SQRT(ZZSPEC(KB+4,J))*A0))*            LF1010
     1 (1.0D0-2.0D0*ADB*(DEL1**2)/(A0**2))                              LF1010
               ELSE
                  ABN=(2.0D0*ADB/(SQRT(ZG(N,J))*A0))*
     1 (1.0D0-2.0D0*ADB*(DEL1**2)/(A0**2))
               ENDIF                                                    LF1010
            ELSE                                                        LF1010
                  ABN=(2.0D0*ADB/(SQRT(ZG(N,J))*A0))*
     1 (1.0D0-2.0D0*ADB*(DEL1**2)/(A0**2))
            ENDIF                                                       LF1010
            GO TO 100
   40       CONTINUE
            IF (PMODS(1)) THEN                                          LF1010
             IF (NA.EQ.9.AND.NB.EQ.9) THEN                              LF1010
             ABN=4.0D0*(ADB**2)*DEL1*DEL2/(SQRT(ZZSPEC(KA,I))*(A0**3))  LF1010
             ELSEIF (NA.EQ.8.AND.NB.EQ.8) THEN                          LF1010
             ABN=4.0D0*(ADB**2)*DEL1*DEL2/(SQRT(ZZSPEC(KA+4,I))*(A0**3))LF1010
             ELSE
               ABN=4.0D0*(ADB**2)*DEL1*DEL2/(SQRT(ZG(M,I))*(A0**3))
             ENDIF                                                      LF1010
            ELSE                                                        LF1010
               ABN=4.0D0*(ADB**2)*DEL1*DEL2/(SQRT(ZG(M,I))*(A0**3))
            ENDIF                                                       LF1010
            GO TO 100
   50       CONTINUE
            IF (PMODS(1)) THEN                                          LF1010
               IF (NA.EQ.9.AND.NB.EQ.9) THEN                            LF1010
                  ABN=-(2.0D0*ADB/(SQRT(ZZSPEC(KA,I))*A0))*             LF1010
     1 (1.0D0-2.0D0*ADB*(DEL1**2)/(A0**2))                              LF1010
               ELSEIF (NA.EQ.8.AND.NB.EQ.8) THEN                        LF1010
                  ABN=-(2.0D0*ADB/(SQRT(ZZSPEC(KA+4,I))*A0))*           LF1010
     1 (1.0D0-2.0D0*ADB*(DEL1**2)/(A0**2))                              LF1010
               ELSE
                  ABN=-(2.0D0*ADB/(SQRT(ZG(M,I))*A0))*
     1 (1.0D0-2.0D0*ADB*(DEL1**2)/(A0**2))
               ENDIF                                                    LF1010
            ELSE                                                        LF1010
                  ABN=-(2.0D0*ADB/(SQRT(ZG(M,I))*A0))*
     1 (1.0D0-2.0D0*ADB*(DEL1**2)/(A0**2))
            ENDIF                                                       LF1010
            GO TO 100
   60       ABN=-(4.0D0*(ADB**2)*DEL2/(SQRT(APB)*(A0**2)))*
     1 (1.0D0-2.0D0*ADB*(DEL1**2)/(A0**2))
            GO TO 100
   70       ABN=8.0D0*(ADB**3)*DEL1*DEL2*DEL3/(SQRT(APB)*(A0**4))
            GO TO 100
   80       ABN=-(8.0D0*(ADB**2)*DEL1/(SQRT(APB)*(A0**2)))*
     1 (0.5D0-ADB*(DEL2**2)/(A0**2))
            GO TO 100
   90       ABN=-(8.0D0*(ADB**2)*DEL1/(SQRT(APB)*(A0**2)))*
     1 (1.5D0-ADB*(DEL1**2)/(A0**2))
  100       SS(I,J)=SQRT((2.0D0*SQRT(APB)/AMB)**3)*EXP(-ADR)*ABN
  110 CONTINUE
      DO 120 I=1,6
         DO 120 J=1,6
            IF (PMODS(1)) THEN                                          LF1010
               IF (NA.EQ.9.AND.NB.EQ.9) THEN                            LF1010
                  DS(IOL)=DS(IOL)+SS(I,J)*CCSPEC(KA,I)*CCSPEC(KA,J)     LF1010
               ELSEIF (NA.EQ.8.AND.NB.EQ.8) THEN                        LF1010
                  DS(IOL)=DS(IOL)+SS(I,J)*CCSPEC(KA+4,I)*CCSPEC(KA+4,J) LF1010
               ELSE
                  DS(IOL)=DS(IOL)+SS(I,J)*CG(M,I)*CG(N,J)
               ENDIF                                                    LF1010
            ELSE                                                        LF1010
               DS(IOL)=DS(IOL)+SS(I,J)*CG(M,I)*CG(N,J)
            ENDIF                                                       LF1010
  120 CONTINUE
C LF1110: Insert further code for MOD1 option used by PMO calculations.
      IF (PMODS(1)) THEN
        IF (NA.EQ.9.AND.NB.EQ.9.AND.(KA.NE.1.OR.KB.NE.1)) THEN
          DS(IOL)=0.0D0                   ! Hp(x)-Hp(x) pairs
        ELSEIF (NA.EQ.9.AND.NB.EQ.8) THEN
          IF (KA.NE.1) THEN
            IF (KB.EQ.1) THEN 
              DS(IOL)=DS(IOL)*PMODVL(3)   ! Hp(p)-O(s) orbital pair
            ELSE
              DS(IOL)=DS(IOL)*PMODVL(4)   ! Hp(p)-O(p) orbital pair
            ENDIF
          ENDIF
        ELSEIF (NA.EQ.8.AND.NB.EQ.9) THEN
          IF (KB.NE.1) THEN
            IF (KA.EQ.1) THEN
              DS(IOL)=DS(IOL)*PMODVL(3)   ! O(s)-Hp(p) orbital pair
            ELSE
              DS(IOL)=DS(IOL)*PMODVL(4)   ! O(p)-Hp(p) orbital pair
            ENDIF
          ENDIF 
        ELSEIF (NA.EQ.9.AND.NB.EQ.6) THEN
          IF (KA.NE.1) THEN
            IF (KB.EQ.1) THEN 
              DS(IOL)=DS(IOL)*PMODVL(3)   ! Hp(p)-C(s) orbital pair
            ELSE
              DS(IOL)=DS(IOL)*PMODVL(4)   ! Hp(p)-C(p) orbital pair
            ENDIF
          ENDIF
        ELSEIF (NA.EQ.6.AND.NB.EQ.9) THEN
          IF (KB.NE.1) THEN
            IF (KA.EQ.1) THEN
              DS(IOL)=DS(IOL)*PMODVL(3)   ! C(s)-Hp(p) orbital pair
            ELSE
              DS(IOL)=DS(IOL)*PMODVL(4)   ! C(p)-Hp(p) orbital pair
            ENDIF
          ENDIF 
        ENDIF
      ENDIF
C End LF1110 changes.
      RETURN
 9999 RETURN 1                                                          CSTP
      END
