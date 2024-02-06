      DOUBLE PRECISION FUNCTION AREAH(JFLAG,COORD,RA,IAT,
     1                                CORCK,DONE2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        INCLUDE 'SIZES.i'
        INCLUDE 'SIZES2.i'
      INCLUDE 'KEYS.i'                                                  DJG0995
      LOGICAL JFLAG,CORCK,DONE,ANLY,DONE2                               GDH0493
      INTEGER AIPTS,ATMHLD,ATMH                                         GDH0493
      INTEGER HSVCTC,HSVCTS,VQUADC,VQUADS                               GDH0493
      INTEGER HOLD,HOLDER,TYPE                                          GDH0493
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /HBORDS/ HBORD(NUMATM),HB(NUMATM)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),
     1                QEFF(NUMATM),DRVPOL(MPACK)                        DJG0994
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),               DJG0195
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)           DJG0195
      COMMON /SMOOTH/ RAL(NUMATM)
      COMMON /RADCOM/ SRFACI(NUMATM)                                    GDH0493
      COMMON /GEPCOM/ ISORTS,NDIV1,NDIV2,IPSUMC,IPSUMS                  GDH1192
      COMMON /ARACOM/ R(NPACK),R2(NPACK),ISAVE,DONE
     1               ,RAC(NUMATM),RAC2(NUMATM),DRAC(NUMATM,NUMATM)      DL0397
     2               ,RAS(NUMATM),RALG(NUMATM)
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM),RMAX(NUMATM)                  GDH0793
     1               ,URLIO(3,NUMATM,NUMATM)                            DL0397
       DIMENSION CORV(MXPT,3),CORVS(MXPT,3),VQUADS(MXPT)                GDH0493
       DIMENSION AIPTS(MXPT),VQUADC(MXPT)                               GDH0992
       DIMENSION ATMHLD(NUMATM),ANSV4(NUMATM)                           GDH0992
       DIMENSION VCOR(NUMATM,NUMATM,3),ANG(NUMATM,NUMATM,3)             GDH0493
       DIMENSION HSVCTS(MXPT,14),HSVCTC(MXPT,14),ANGV(NUMATM)           GDH0493
       DIMENSION ANSV(NUMATM),ANSV2(NUMATM),ANSV3(NUMATM)               GDH0493
       DIMENSION COORD(3,*),RA(NUMATM)
       DIMENSION HRPCAL(MXPTSV,IATMSV,IATMSV), HRPCL2(MXPT,NUMATM)      GDH0593
       DIMENSION MRPCAL(MXPT,NUMATM),ANGCK(NUMATM),IANGCK(NUMATM)       GDH0593
       SAVE
       IF (ISORTS.EQ.0) THEN                                            GDH0992
           NARCSC=NDIV2                                                 GDH1192
           NPTSC=2*NDIV2                                                GDH1192
           NARCSS=NDIV1                                                 GDH1192
           NPTSS=2*NDIV1                                                GDH1192
C GDH0597           IF (IDOTS.NE.0) THEN                                DJG0995
           CALL DOTMK(IPSUMC,NARCSC,NPTSC,CORV,VQUADC,HSVCTC)           GDH0992
           CALL DOTMK(IPSUMS,NARCSS,NPTSS,CORVS,VQUADS,HSVCTS)          GDH0992
C GDH0597           ELSE                                                GDH0493
C GDH0597           CALL GEPMK(CORV,VQUADC,HSVCTC,CORVS,VQUADS,HSVCTS)  GDH0992
C GDH0597           ENDIF                                               GDH0493
           HOLDER=MAX(IPSUMC,IPSUMS)                                    GDH0493
           DO 401 J=1,HOLDER                                            GDH0493
           DO 402 K=1,NUMAT                                             GDH0593
402        MRPCAL(J,K)=1                                                GDH0593
401        AIPTS(J)=1                                                   GDH0493
           ISORTS=ISORTS+1
       ENDIF
C   Determines which atoms are attached to the atom in question.
        JJ=0
        RA1=RA(IAT)
        RA2=RA1*RA1
        ANAREA=0.D0
        ANLY=.FALSE.
        DO 109 JJJ=1,NUMAT                                              GDH0992
          J=JJJ                                                         GDH1192
          ANGCK(J)=0.D0
          IANGCK(J)=0
           IF ((RA1.NE.0.D0).AND.(J.NE.IAT)) THEN                       GDH0992
              RADSQ=RA1+RA(J)                                           GDH0992
              DISTSQ=RLIO(IAT,J)                                        GDH0493
              IF ((DISTSQ.LT.RADSQ).AND.                                GDH0992
     &           ((DISTSQ+RA(J)).GT.RA1)) THEN                          GDH0992
                 IF (DISTSQ+RA1.LT.RA(J)) THEN                          GDH1093
                     AREAH=0.D0                                         GDH1093
                     RETURN                                             GDH1093
                 ENDIF                                                  GDH1093
                 JJ=JJ+1                                                GDH0992
                 ATMHLD(JJ)=J                                           GDH0992
              ENDIF                                                     GDH0992
           ENDIF                                                        GDH0992
109     CONTINUE                                                        GDH0992
C  Determines which dots are on the surface of the molecule
      IF (JFLAG) THEN                                                   GDH0992
         IPSUM=IPSUMC                                                   GDH0992
      ELSE                                                              GDH0992
         IPSUM=IPSUMS                                                   GDH0992
      ENDIF                                                             GDH0992
      DO 112 J2=1,JJ
           ATMH=ATMHLD(J2)
           ANLY=.TRUE.
           RDIST=RLIO(IAT,ATMH)
           RDIST2=RDIST*RDIST
           RA1A=RA(ATMH)
           RA2A=RA1A*RA1A
           K=1
      DO 113 J3=1,JJ
           IF (J3.NE.J2) THEN
           J5=ATMHLD(J3)
           IF (RLIO(ATMH,J5).LT.(RA(ATMH)+RA(J5))) ANLY=.FALSE.
           ENDIF
113   CONTINUE
      IF (ANLY) THEN
           COSTH=(RA2+RDIST2-RA2A)/(2.D0*RA1*RDIST)                     GDH0793
           ANAREA=ANAREA+2.D0*PI*(1-COSTH)                              GDH0793
        ELSE
c
C       CALCULATE INTERATOMIC DISTANCES
C
      IF (.NOT.DONE) THEN
      DONE=.TRUE.
      DO 15 I=1,NUMAT
        DO 15 J=1,I
          XX1=COORD(1,I)-COORD(1,J)
          XX2=COORD(2,I)-COORD(2,J)
          XX3=COORD(3,I)-COORD(3,J)
          DISTS=RLIO(I,J)
          IF (I.EQ.J) GOTO 15
          XD1=XX1/DISTS
          VCOR(J,I,1)=XD1                                               GDH0493
          XD2=XX2/DISTS
          VCOR(J,I,2)=XD2                                               GDH0493
          XD3=XX3/DISTS
          VCOR(J,I,3)=XD3                                               GDH0493
          VCOR(I,J,1)=-XD1                                              GDH0493
          VCOR(I,J,2)=-XD2                                              GDH0493
          VCOR(I,J,3)=-XD3                                              GDH0493
          VCORT=XD1
          VCORT2=XD2
          VCORT3=XD3
          IF (ABS(VCORT).GE.1.D0) THEN
             ANG(J,I,1)=0.D0
             ANG(I,J,1)=0.D0
          ELSE
             ANG(J,I,1)=ACOS(VCORT)
             IF (ANG(J,I,1).GT.(PD2)) ANG(J,I,1)=PI-ANG(J,I,1)
             ANG(I,J,1)=ANG(J,I,1)
          ENDIF
          IF (ABS(VCORT2).GE.1.D0) THEN
             ANG(J,I,2)=0.D0
             ANG(I,J,2)=0.D0
          ELSE
             ANG(J,I,2)=ACOS(VCORT2)
             IF (ANG(J,I,2).GT.(PD2)) ANG(J,I,2)=PI-ANG(J,I,2)
             ANG(I,J,2)=ANG(J,I,2)
          ENDIF
          IF (ABS(VCORT3).GE.1.D0) THEN
             ANG(J,I,3)=0.D0
             ANG(I,J,3)=0.D0
          ELSE
             ANG(J,I,3)=ACOS(VCORT3)
             IF (ANG(J,I,3).GT.(PD2)) ANG(J,I,3)=PI-ANG(J,I,3)
             ANG(I,J,3)=ANG(J,I,3)
          ENDIF
          IF ((IPSUMC.LE.MXPTSV).AND.(NUMAT.LE.IATMSV)) THEN
          DO 67 L=1,IPSUMC
              HRPHLD=CORV(L,1)*XD1+CORV(L,2)*XD2+CORV(L,3)*XD3
              HRPCAL(L,J,I)=HRPHLD
              HRPCAL(L,I,J)=-HRPHLD
67        CONTINUE
          ISAVE=1
          ENDIF
15     CONTINUE
      ENDIF
C
C     THIS CALCULATES TOTAL EXPOSED AREA OF SPHERE IAT.
C
        IF(.NOT.JFLAG) GO TO 910
C
C       SAVE ORIGINAL RA IN AHOLD AND ADJUST FIRST SPHERE TO COULOMB SURFACE
C
        IF (.NOT.DONE2) THEN
        DONE2=.TRUE.
        DO 63 K=1,NUMAT                                                 GDH0992
           IF (IAT.EQ.K) GOTO 63                                        GDH0493
           IF (IAT.GT.K) THEN                                           GDH0493
              IDIST=(IAT*(IAT-1))/2+K                                   GDH0493
           ELSE                                                         GDH0493
              IDIST=(K*(K-1))/2+IAT                                     GDH0493
           ENDIF                                                        GDH0493
           ANSV(K)=(RAC2(K))-R2(IDIST)                                  GDH0493
           ANSV2(K)=2.D0*R(IDIST)                                       GDH0493
63      CONTINUE
        IF (ISAVE.EQ.0) THEN                                            GDH0593
        DO 65 K=1,NUMAT                                                 GDH0593
           IF (K.EQ.IAT) GOTO 65
           XD1=VCOR(IAT,K,1)                                            GDH0593
           XD2=VCOR(IAT,K,2)                                            GDH0593
           XD3=VCOR(IAT,K,3)                                            GDH0593
           DO 69 J=1,IPSUMC                                             GDH0593
              HRPCL2(J,K)=CORV(J,1)*XD1+CORV(J,2)*XD2+CORV(J,3)*XD3     GDH0593
69         CONTINUE                                                     GDH0593
65      CONTINUE                                                        GDH0593
        ELSE                                                            GDH0593
        DO 71 K=1,NUMAT                                                 GDH0593
        IF (K.EQ.IAT) GOTO 71
        DO 66 J=1,IPSUMC                                                GDH0593
           HRPCL2(J,K)=HRPCAL(J,IAT,K)                                  GDH0593
66      CONTINUE                                                        GDH0593
71      CONTINUE                                                        GDH0593
        ENDIF                                                           GDH0593
        ENDIF
c
c       Point vector is ready - process it
c
C  Determines which dots are on the surface of the molecule
910     IF (JFLAG) THEN                                                 GDH0493
           ANSV3(ATMH)=ANSV(ATMH)-RA2                                   GDH0493
           ANSV4(ATMH)=ANSV2(ATMH)*RA1                                  GDH0493
        ELSE
           ANSV(ATMH)=RA2A-(RA2+RDIST2)                                 GDH0493
           ANSV2(ATMH)=2.D0*RA1*RDIST                                   GDH0493
        ENDIF
           HOLDER=0
           IF (JFLAG) THEN
              IF (IANGCK(ATMH).EQ.1) THEN
               DO 451 J=1,IPSUM                                         GDH0992
                  IF (MRPCAL(J,ATMH).EQ.0) THEN
                  DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &                        HRPCL2(J,ATMH))
                  IF (DISTSQ.GE.0.D0) THEN
                     AIPTS(J)=0
                  ELSE
                     MRPCAL(J,ATMH)=1
                  ENDIF
                  ENDIF
451            CONTINUE
              GOTO 68
             ELSE
              ANGV(ATMH)=ACOS((RA2+RDIST2-
     &                         RA2A)/(2.D0*RA1*RDIST))
              IF (ANGV(ATMH).LT.ANGCK(ATMH)) THEN
                 IANGCK(ATMH)=1
              ELSE
                 ANGCK(ATMH)=ANGV(ATMH)
              ENDIF
             ENDIF
           ELSE
              ANGV(ATMH)=ACOS((RA2+RDIST2-RA2A)/
     &                         (2.D0*RA1*RDIST))
           ENDIF
           VCORT=VCOR(IAT,ATMH,1)
           VCORT2=VCOR(IAT,ATMH,2)
           VCORT3=VCOR(IAT,ATMH,3)
           ANG1=ANG(IAT,ATMH,1)
           ANG2=ANG(IAT,ATMH,2)
           ANG3=ANG(IAT,ATMH,3)
           IF ((ANG1+ANGV(ATMH)).LT.PD2) THEN
               TYPE=1
               IF (VCORT.GT.0.D0) THEN
               HOLDER=9
               ELSE
               HOLDER=12
               ENDIF
           ELSE
           IF ((ANG2+ANGV(ATMH)).LT.PD2) THEN
               TYPE=1
               IF (VCORT2.GT.0.D0) THEN
               HOLDER=10
               ELSE
               HOLDER=13
               ENDIF
           ELSE
           IF (ANG3+ANGV(ATMH).LT.PD2) THEN
               TYPE=1
               IF (VCORT3.GT.0.D0) THEN
               HOLDER=11
               ELSE
               HOLDER=14
               ENDIF
           ELSE
           IF (-ANG1+ANGV(ATMH).GT.PD2) THEN
               TYPE=2
               IF (VCORT.GT.0.D0) THEN
               HOLDER=12
               ELSE
               HOLDER=9
               ENDIF
           ELSE
           IF (-ANG2+ANGV(ATMH).GT.PD2) THEN
               TYPE=2
               IF (VCORT2.GT.0.D0) THEN
               HOLDER=13
               ELSE
               HOLDER=10
               ENDIF
           ELSE
           IF (-ANG3+ANGV(ATMH).GT.PD2) THEN
               TYPE=2
               IF (VCORT3.GT.0.D0) THEN
               HOLDER=14
               ELSE
               HOLDER=11
               ENDIF
           ENDIF
           ENDIF
           ENDIF
           ENDIF
           ENDIF
           ENDIF
           IF (HOLDER.GT.0) THEN
           IF (TYPE.EQ.1) THEN
           IF (JFLAG) THEN
           IF (IANGCK(ATMH).EQ.1) THEN
           DO 450 J=1,IPSUM                                             GDH0992
              IF (HSVCTC(J,HOLDER).EQ.1) THEN
              DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &               HRPCL2(J,ATMH))                                    GDH0593
              IF (DISTSQ.GE.0.D0) THEN
                 AIPTS(J)=0
                 MRPCAL(J,ATMH)=0
              ENDIF
              ENDIF
450         CONTINUE
           ELSE
           DO 420 J=1,IPSUM                                             GDH0992
              IF (HSVCTC(J,HOLDER).EQ.1) THEN
              DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &               HRPCL2(J,ATMH))                                    GDH0593
              IF (DISTSQ.GE.0.D0) AIPTS(J)=0
              ENDIF
420         CONTINUE
           ENDIF
            ELSE
            DO 421 J=1,IPSUM                                            GDH0992
              IF (HSVCTS(J,HOLDER).EQ.1) THEN
              DISTSQ=ANSV(ATMH)+(ANSV2(ATMH)*(CORVS(J,1)*VCORT
     &                        +CORVS(J,2)*VCORT2+
     &                        CORVS(J,3)*VCORT3))                       GDH0493
              IF (DISTSQ.GE.0.D0) AIPTS(J)=0
              ENDIF
421         CONTINUE
           ENDIF
           ELSE
           IF (JFLAG) THEN
           IF (IANGCK(ATMH).EQ.1) THEN
           DO 452 J=1,IPSUM                                             GDH0992
              IF (HSVCTC(J,HOLDER).EQ.1) THEN
              DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &               HRPCL2(J,ATMH))                                    GDH0593
              IF (DISTSQ.GE.0.D0) THEN
                 AIPTS(J)=0
                 MRPCAL(J,ATMH)=0
              ENDIF
              ELSE
              AIPTS(J)=0
              MRPCAL(J,ATMH)=0
              ENDIF
452         CONTINUE
           ELSE
           DO 422 J=1,IPSUM                                             GDH0992
              IF (HSVCTC(J,HOLDER).EQ.1) THEN
              DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &               HRPCL2(J,ATMH))                                    GDH0593
              IF (DISTSQ.GE.0.D0) AIPTS(J)=0
              ELSE
              AIPTS(J)=0
              ENDIF
422         CONTINUE
           ENDIF
            ELSE
            DO 423 J=1,IPSUM                                            GDH0992
              IF (HSVCTS(J,HOLDER).EQ.1) THEN
              DISTSQ=ANSV(ATMH)+(ANSV2(ATMH)*(CORVS(J,1)*VCORT
     &                        +CORVS(J,2)*VCORT2+
     &                        CORVS(J,3)*VCORT3))                       GDH0493
              IF (DISTSQ.GE.0.D0) AIPTS(J)=0
              ELSE
              AIPTS(J)=0
              ENDIF
423         CONTINUE
           ENDIF
           ENDIF
           ELSE
           DO 412 I=1,3
           COUNTH=ANGV(ATMH)-ANG(IAT,ATMH,I)
           COUNTI=ANGV(ATMH)+ANG(IAT,ATMH,I)
           IF (COUNTH.LT.(-13.D0*PD64).AND.COUNTI.LT.
     &                   (30.D0*PD64)) THEN

              HOLDER=HOLDER - 1
           ELSE
              IF (COUNTH.LT.(9.D0*PD64).AND.COUNTI.LT.
     &                      (51.D0*PD64)) THEN
                 HOLDER=HOLDER + 1
              ELSE
              IF (COUNTH.GT.(13.D0*PD64).AND.COUNTI.GT.
     &                      (55.D0*PD64)) THEN
                 HOLDER=HOLDER + 6
              ELSE
                 HOLDER=HOLDER + 21
              ENDIF
              ENDIF
           ENDIF
412        CONTINUE
            IF (VCORT.GT.0.D0) THEN
               IF (VCORT2.GT.0.D0) THEN
                   IF (VCORT3.GT.0.D0) THEN
                      HOLD=1
                   ELSE
                      HOLD=6
                   ENDIF
               ELSE
                   IF (VCORT3.GT.0.D0) THEN
                      HOLD=4
                   ELSE
                      HOLD=7
                   ENDIF
               ENDIF
            ELSE
               IF (VCORT2.GT.0.D0) THEN
                   IF (VCORT3.GT.0.D0) THEN
                      HOLD=2
                   ELSE
                      HOLD=5
                   ENDIF
               ELSE
                   IF (VCORT3.GT.0.D0) THEN
                      HOLD=3
                   ELSE
                      HOLD=8
                   ENDIF
               ENDIF
            ENDIF
           IF (HOLDER.EQ.-3) THEN
           IF (JFLAG) THEN
           IF (IANGCK(ATMH).EQ.1) THEN
           DO 453 J=1,IPSUM                                             GDH0992
              IF (HOLD.EQ.VQUADC(J)) THEN
              DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &               HRPCL2(J,ATMH))                                    GDH0593
              IF (DISTSQ.GE.0.D0) THEN
                 AIPTS(J)=0
                 MRPCAL(J,ATMH)=0
              ENDIF
              ENDIF
453         CONTINUE
           ELSE
           DO 410 J=1,IPSUM                                             GDH0992
              IF (HOLD.EQ.VQUADC(J)) THEN
              DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &               HRPCL2(J,ATMH))                                    GDH0593
              IF (DISTSQ.GE.0.D0) AIPTS(J)=0
              ENDIF
410         CONTINUE
           ENDIF
            ELSE
            DO 411 J=1,IPSUM                                            GDH0992
              IF (HOLD.EQ.VQUADS(J)) THEN
              DISTSQ=ANSV(ATMH)+(ANSV2(ATMH)*(CORVS(J,1)*VCORT
     &                        +CORVS(J,2)*VCORT2+
     &                        CORVS(J,3)*VCORT3))                       GDH0493
              IF (DISTSQ.GE.0.D0) AIPTS(J)=0
              ENDIF
411         CONTINUE
           ENDIF
           ELSE
            IF (HOLDER.LE.3) THEN
            IF (JFLAG) THEN
            IF (IANGCK(ATMH).EQ.1) THEN
            DO 454 J=1,IPSUM                                            GDH0992
              IF (HSVCTC(J,HOLD).EQ.1) THEN
              DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &               HRPCL2(J,ATMH))                                    GDH0593
              IF (DISTSQ.GE.0.D0) THEN
                 AIPTS(J)=0
                 MRPCAL(J,ATMH)=0
              ENDIF
              ENDIF
454         CONTINUE
           ELSE
             DO 413 J=1,IPSUM                                           GDH0992
                IF (HSVCTC(J,HOLD).EQ.1) THEN
                DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &               HRPCL2(J,ATMH))                                    GDH0593
                IF (DISTSQ.GE.0.D0) AIPTS(J)=0
                ENDIF
413          CONTINUE
           ENDIF
            ELSE
             DO 414 J=1,IPSUM                                           GDH0992
              IF (HSVCTS(J,HOLD).EQ.1) THEN
                DISTSQ=ANSV(ATMH)+(ANSV2(ATMH)*(CORVS(J,1)*VCORT
     &                        +CORVS(J,2)*VCORT2+
     &                        CORVS(J,3)*VCORT3))                       GDH0493
              IF (DISTSQ.GE.0.D0) AIPTS(J)=0
              ENDIF
414          CONTINUE
            ENDIF
            ELSE
             IF (HOLDER.EQ.18) THEN
              IF (JFLAG) THEN
              IF (IANGCK(ATMH).EQ.1) THEN
              DO 455 J=1,IPSUM                                          GDH0992
              IF (HSVCTC(J,HOLD).EQ.0) THEN
              DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &               HRPCL2(J,ATMH))                                    GDH0593
              IF (DISTSQ.GE.0.D0) THEN
                 AIPTS(J)=0
                 MRPCAL(J,ATMH)=0
              ENDIF
              ELSE
              AIPTS(J)=0
              MRPCAL(J,ATMH)=0
              ENDIF
455         CONTINUE
            ELSE
               DO 415 J=1,IPSUM                                         GDH0992
                IF (HSVCTC(J,HOLD).EQ.0) THEN
                DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &               HRPCL2(J,ATMH))                                    GDH0593
                IF (DISTSQ.GE.0.D0) AIPTS(J)=0
                ELSE
                AIPTS(J)=0
                ENDIF
415            CONTINUE
             ENDIF
              ELSE
               DO 416 J=1,IPSUM                                         GDH0992
                IF (HSVCTS(J,HOLD).EQ.0) THEN
                 DISTSQ=ANSV(ATMH)+(ANSV2(ATMH)*(CORVS(J,1)*VCORT
     &                        +CORVS(J,2)*VCORT2+
     &                        CORVS(J,3)*VCORT3))                       GDH0493
                IF (DISTSQ.GE.0.D0) AIPTS(J)=0
                ELSE
                AIPTS(J)=0
                ENDIF
416            CONTINUE
              ENDIF
             ELSE
             IF (K.EQ.1.D0) THEN
              IF (JFLAG) THEN
              IF (IANGCK(ATMH).EQ.1) THEN
              DO 456 J=1,IPSUM                                          GDH0992
              DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &               HRPCL2(J,ATMH))                                    GDH0593
              IF (DISTSQ.GE.0.D0) THEN
                 AIPTS(J)=0
                 MRPCAL(J,ATMH)=0
              ENDIF
456           CONTINUE
              ELSE
               DO 417 J=1,IPSUM                                         GDH0992
                  DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &                        HRPCL2(J,ATMH))
                  IF (DISTSQ.GE.0.D0) AIPTS(J)=0
417            CONTINUE
              ENDIF
              ELSE
               DO 418 J=1,IPSUM                                         GDH0992
                DISTSQ=ANSV(ATMH)+(ANSV2(ATMH)*(CORVS(J,1)*VCORT
     &                        +CORVS(J,2)*VCORT2+
     &                        CORVS(J,3)*VCORT3))                       GDH0493
                IF (DISTSQ.GE.0.D0) AIPTS(J)=0
418            CONTINUE
              ENDIF
             ELSE
              IF (JFLAG) THEN
              IF (IANGCK(ATMH).EQ.1) THEN
              DO 457 J=1,IPSUM                                          GDH0992
              IF (AIPTS(J).NE.0) THEN
              DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &               HRPCL2(J,ATMH))                                    GDH0593
              IF (DISTSQ.GE.0.D0) THEN
                 AIPTS(J)=0
                 MRPCAL(J,ATMH)=0
              ENDIF
              ENDIF
457           CONTINUE
              ELSE
               DO 419 J=1,IPSUM                                         GDH0992
                IF (AIPTS(J).NE.0) THEN
                DISTSQ=ANSV3(ATMH)+(ANSV4(ATMH)*
     &                        HRPCL2(J,ATMH))
                IF (DISTSQ.GE.0.D0) AIPTS(J)=0
                ENDIF
419            CONTINUE
              ENDIF
              ELSE
               DO 430 J=1,IPSUM                                         GDH0992
                IF (AIPTS(J).NE.0) THEN
                DISTSQ=ANSV(ATMH)+(ANSV2(ATMH)*(CORVS(J,1)*VCORT
     &                        +CORVS(J,2)*VCORT2+
     &                        CORVS(J,3)*VCORT3))                       GDH0493
                IF (DISTSQ.GE.0.D0) AIPTS(J)=0
                ENDIF
430            CONTINUE
              ENDIF
             ENDIF
            ENDIF
           ENDIF
         ENDIF
        ENDIF
        K=2
68      CONTINUE
        ENDIF
112     CONTINUE
        IPTS=0
        DO 407 J=1,IPSUM
        IPTS=IPTS+AIPTS(J)
407     AIPTS(J)=1
        AREAH=((4.0D0*PI)*DBLE(IPTS)/DBLE(IPSUM))-
     &         ANAREA
        RETURN
        END
