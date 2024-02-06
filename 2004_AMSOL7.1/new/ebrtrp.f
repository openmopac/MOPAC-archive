      SUBROUTINE EBRTRP (COORD,BORN,NITER)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'SIZES2.i'
      INCLUDE 'KEYS.i'
      INCLUDE 'FFILES.i'                                                GDH0597
      PARAMETER (MT =2000)                                              DL0397
C ---------------------------------------------------------------------+
C     CALCULATE THE EFFECTIVE BORN RADII USING THE TRAPEZOIDAL METHOD.
C     USED FOR SMx MODELS BEGINNING WITH SM4 AND ALSO ALL SMx.1 MODELS.
C ON INPUT
C     RLIO  = INTERATOMIC DISTANCES
C     URLIO = UNIT VECTORS:
C             URLIO(i,j,k) = (COORD(i,k)-COORD(i,j))/RLIO(j,k)
C                          = dRLIO(j,k)/dCOORD(i,k)
C     RAC   = COULOMB RADII
C     DRAC  = DERIVATIVE: DRAC(i,j) = dRAC(j)/dRLIO(i,j)
C ON OUTPUT
C     BORN  = EFFECTIVE BORN RADII
C     DBORN = CARTESIAN DERIVATIVES OF BORN                             DL0397
C             DBORN(i,j,k) = dBORN(k)/dCOORD(i,j)                       DL0397
C
C     CREATED BY DJG 0995 FROM EXISTING LINES IN SMX1
C
C     REVISITED AND ADDED DERIVATIVES OF BORN: DBORN(3,NUMAT,NUMAT),    DL0397
C     FOR SMs MAKING USE OF ANALYTICAL EXPOSED AREA: TAREA=.TRUE.       DL0397
C     IMPLEMENTED EXACT d(AREA)/dr IN DERIVATIVES: DY from "DAREAL"     DL1097
C ---------------------------------------------------------------------+
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /ARACOM/ R(NPACK),R2(NPACK),ISAVE,DONE
     1               ,RAC(NUMATM),RAC2(NUMATM),DRAC(NUMATM,NUMATM)      DL0397
     2               ,RAS(NUMATM),RALG(NUMATM)
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM), RMAX(NUMATM)
     1               ,URLIO(3,NUMATM,NUMATM)                            DL0397
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL0397
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL0397
     2               ,DATLGR(3,NUMATM)                                  DL0397
     3               ,LGR,FLAGRD                                        DL0397
                      LOGICAL LGR,FLAGRD                                DL0397
      COMMON /SCRCHR/ BUF(3,NUMATM), WORK(6*MAXPAR**2+1-3*NUMATM)       DL0397
      COMMON /SCRAH1/ X(0:MT),DXI(0:MT),DXS(0:MT)                       DL0397
     .               ,Y(0:MT),DY(0:MT)                                  DL0397
     .               ,Z(0:MT),DZI(0:MT),DZS(0:MT)                       DL0397
     .               ,DAREA(3,0:NUMATM),NC(0:NUMATM)                    DL1097
      COMMON /IVSZCM/ IVSZ(NUMATM), IVSZC
      COMMON /ONESCM/ ICONTR(100)
      COMMON /AREACM/ NOPTI,TAREA,NINTEG
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),               GDH0297
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)           GDH0297
      DIMENSION BORN(*),COORD(3,*)
      LOGICAL CORCK,DONE2,DONE,TAREA,DOENP
      SAVE FOURPI,TEXPN,TINIT,ITPR,TMPM,TMPI                            DL0397
      DATA ZERO,ONE /0D0,1D0/                                           DL1097
      IF (ICONTR(52).EQ.1) THEN
         FOURPI=4.D0*PI
         TEXPN=1.20D0                                                   GDH0194
         TINIT=0.15D0                                                   GDH0194
         ITPR=0                                                         GDH0194
         IF (ITONE.NE.0) TINIT=FITONE                                   DJG0995
         IF (ITEXPN.NE.0) THEN                                          DJG0995
            TEXPN=FITEXP                                                DJG0995
            IF (TEXPN.EQ.ONE) ITPR=1                                    DL1097
         ENDIF                                                          GDH0194
         IF (ITPR.NE.1) THEN                                            GDH0194
            TMPM=LOG(TEXPN)                                             GDH0194
            TMPI=(TEXPN-ONE)/TINIT                                      DL1097
         ENDIF                                                          GDH0194
         ICONTR(52)=2
         DOENP=.TRUE.
      ENDIF
      DO 100 IAT=1,NUMAT
         IF (ISM50.GT.0) THEN                                           GDH0297
            IF (ITYPE(IAT).EQ.0) THEN                                   GDH0297
               BORN(IAT)= 0.00D0                                        GDH0297
               GOTO 100                                                 GDH0297
            ENDIF                                                       GDH0297
         ENDIF                                                          GDH0297
      IF (NITER.EQ.1) THEN                                              DJG0995
         CORCK=.FALSE.                                                  DJG0995
      ELSE                                                              DJG0995
         CORCK=.TRUE.                                                   DJG0995
      ENDIF                                                             DJG0995
      DONE2=.FALSE.                                                     DJG0995
C                                                                       DL0397
C     THE TRAPEZOIDAL INTEGRATION YIELDS INTEGRAL I = Y'.Z WITH         DL0397
C     EXPOSED AREA Y(i) CALCULATED AT ABSCISSAE X(i),                   DL0397
C     AND WEIGHTING FACTORS Z(i).                                       DL0397
C                                                                       DL0397
C     FOR SOME CARTESIAN VARIABLE "V", DERIVATIVE FOR THE FORMULA "I":  DL0397
C     dI = dY'.Z + Y'.dZ                                                DL0397
C     WITH, IN EINSTEIN'S CONVENTION (IMPLICIT SUM):                    DL0397
C     dY = dY/dV +                                                      DL0397
C         (dY/dX).((dX/dRI).(dRI/dV)+(dX/dR).(dR/dV)+(dX/dRS).(dRS/dV)) DL0397
C     dZ =(dZ/dX).((dX/dRI).(dRI/dV)+(dX/dR).(dR/dV)+(dX/dRS).(dRS/dV)) DL0397
C         NOTE: dZ/dV = 0                                               DL0397
C     THIS FORMALISM ACCOUNTS FOR THE DEPENDANCIES OF Xs IN:            DL0397
C         R  = RLIO(IAT,IUP)                                            DL0397
C         RI = RAC(IAT)                                                 DL0397
C         RP = RAC(IUP)                                                 DL0397
C     BUT NEGLECTS THE DEPENDANCIES OF Ys VS THE OTHER RAC(j) !         DL0397
C     RACs MAY DEPEND IN "V" (HN & HO BONDS). AVAILABLE:                DL0397
C     dRAC/dV = (dRAC/dRLIO).(dRLIO/dV)                                 DL0397
C                                                                       DL0397
C     STORAGES:                                                         DL0397
C     X  HOLDS  X         DXS    HOLDS   dX/dRS = dX/dR                 DL0397
C                         DXI      "     dX/dRI                         DL0397
C     Z    "    Z         DZS      "     dZ/dRS = dZ/dR                 DL1097
C                         DZI      "     dZ/dRI                         DL1097
C     RAC  "    RAC       DRAC     "     dRAC/dRLIO                     DL0397
C                         URLIO    "     dRLIO/dV                       DL0397
C     Y    "    Y         DAREA    "     dY/dV                          DL0397
C                         DY       "     dY/dX                          DL1097
C
C     SETUP THE INTERVAL OF INTEGRATION: RINF, RMAX.
C
      DO 10 I=1,NUMAT
      WORK(I)=RLIO(I,IAT)+RAC(I)                                        DJG0995
   10 CONTINUE                                                          DJG0995
      IF (LGR) CALL SCOPY (3*NUMAT,ZERO,0,DBORN(1,1,IAT),1)             DL1097
      RINF=RAC(IAT)                                                     DJG0995
      IUP=ISMAX(NUMAT,WORK,1)
      RSUP=WORK(IUP)
      IF (RSUP-RINF.LT.1D-3) THEN
C        ATOM No IAT ALREADY ENCLOSE THE SUPERMOLECULE.
         BORN(IAT)=FOURPI/RINF
         IF (LGR) THEN                                                  DL0397
            FACTI=-BORN(IAT)/RINF                                       DL1097
            FACTS= ZERO                                                 DL1097
         ENDIF                                                          DL0397
         GOTO 50                                                        DL0397
      ENDIF                                                             DL0397
C                                                                       DL0397
C     SETUP THE TRAPEZOIDAL ABSCISSAE:  X(0), ..., X(NT)                DL0397
C                           & WEIGTHS:  Z(0), ..., Z(NT)                DL0397
C     AND RELEVANT PARTIAL DERIVATIVES                                  DL0397
C                                                                       DL0397
      IF (IVSZC.EQ.1) THEN                                              GDH0494
C                                                                       GDH1197
C     Using external values for M shells in volume integral             GDH1197
C                                                                       GDH1197
         NT=IVSZ(IAT)                                                   GDH0294
         IF (ITPR.NE.1) THEN                                            GDH0494
C                                                                       GDH1197
C        TSIZE is the thickness of the first shell divided by           GDH1197
C        the expansion factor for the shell                             GDH1197
C        The next line was recoded according to the Liotard, Hawkins    GDH1197
C        paper J Comp Chem 16 (1995) 422.                               GDH1197
C
C            FACTOR=(ONE-TEXPN)/(TEXPN*(ONE-TEXPN)**NT)                  DL1097
            FACTOR=(ONE-TEXPN)/((ONE-TEXPN**NT)*TEXPN)                  GDH1197
            TSIZE=(RSUP-RINF)*FACTOR                                    DL0397
            IF (LGR) DTSIZE=FACTOR                                      DL0397
         ELSE                                                           GDH0494
            TSIZE=(RSUP-RINF)/NT                                        GDH0494
            IF (LGR) DTSIZE=ONE/NT                                      DL1097
         ENDIF                                                          GDH0494
      ELSE                                                              GDH0494
         IF (ITPR.NE.1) THEN                                            GDH0194
            TNT=LOG((RSUP-RINF)*TMPI+ONE)/TMPM                          DL1097
            NT=INT(TNT+ONE)                                             DL1097
            IF (LGR) DTNMBT=TMPI/(((RSUP-RINF)*TMPI+ONE)*TMPM)          DL1097
         ELSE                                                           GDH0194
            TNT=(RSUP-RINF)/TINIT                                       GDH0194
            NT=INT(TNT+ONE)                                             DL1097
            IF (LGR) DTNMBT=ONE/TINIT                                   DL1097
         ENDIF                                                          GDH0194
         IF (IVSZ(IAT).GT.NT) THEN                                      GDH0494
             NT=IVSZ(IAT)                                               GDH0494
         ELSE                                                           GDH0494
             IVSZ(IAT)=NT                                               GDH0194
         ENDIF                                                          GDH0494
         TSIZE=TNT*TINIT/(NT*TEXPN)                                     GDH0494
         IF (LGR) DTSIZE=DTNMBT*TINIT/(NT*TEXPN)                        DL0397
      ENDIF                                                             GDH0294
      IF (NT.GT.MT) THEN                                                DL0397
         WRITE(JOUT,'(I6,'' ABSCISSAE IN TRAPEZOIDAL INTEGRATION FOR'', DL0397
     .   '' ATOM'',I4,''. EXCEED LIMIT = '',I5)') NT,IAT,MT             DL0397
         STOP                                                           DL0397
      ENDIF                                                             DL0397
      X(0)=RINF                                                         DL0397
      Y(0)=LOG(X(0))                                                    DL0397
      Z(0)=ONE/X(0)                                                     DL1097
      IF (LGR) THEN                                                     DL0397
         DXS(0)=ZERO                                                    DL1097
         DXI(0)=ONE                                                     DL1097
         DZS(0)=-Z(0)/X(0)*DXS(0)                                       DL0397
         DZI(0)=-Z(0)/X(0)*DXI(0)                                       DL0397
      ENDIF                                                             DL0397
      DO 20 I=1,NT-1                                                    DL0397
      TSIZE=TEXPN*TSIZE                                                 DL0397::
      X(I)=X(I-1)+TSIZE                                                 DL0397
      Y(I)=LOG(X(I))                                                    DL0397
      Z(I)=(Y(I)-Y(I-1))/TSIZE                                          DL0397
      IF (LGR) THEN                                                     DL0397
         DTSIZE=TEXPN*DTSIZE                                            DL0397
         DXS(I)=DXS(I-1)+DTSIZE                                         DL0397
         DXI(I)=DXI(I-1)-DTSIZE                                         DL0397
         DZS(I)=(DXS(I)/X(I)-DXS(I-1)/X(I-1)-Z(I)*DTSIZE)/TSIZE         DL0397
         DZI(I)=(DXI(I)/X(I)-DXI(I-1)/X(I-1)+Z(I)*DTSIZE)/TSIZE         DL0397
      ENDIF                                                             DL0397
   20 CONTINUE                                                          DL0397
      X(NT)=RSUP                                                        DL0397
      Y(NT)=LOG(X(NT))                                                  DL0397
      Z(NT)=(Y(NT)-Y(NT-1))/(X(NT)-X(NT-1))                             DL0397
      IF (LGR) THEN                                                     DL0397
         DXS(NT)=ONE                                                    DL1097
         DXI(NT)=ZERO                                                   DL1097
         DZS(NT)=(DXS(NT)/X(NT)-DXS(NT-1)/X(NT-1)                       DL0397
     .   -Z(NT)*(DXS(NT)-DXS(NT-1)))/(X(NT)-X(NT-1))                    DL0397
         DZI(NT)=(DXI(NT)/X(NT)-DXI(NT-1)/X(NT-1)                       DL0397
     .   -Z(NT)*(DXI(NT)-DXI(NT-1)))/(X(NT)-X(NT-1))                    DL0397
      ENDIF                                                             DL0397
      DO 21 I=0,NT-1                                                    DL0397
      Z(I)=Z(I)-Z(I+1)                                                  DL0397
      IF (LGR) THEN                                                     DL0397
         DZS(I)=DZS(I)-DZS(I+1)                                         DL0397
         DZI(I)=DZI(I)-DZI(I+1)                                         DL0397
      ENDIF                                                             DL0397
   21 CONTINUE                                                          DL0397
C                                                                       DL0397
C     PERFORM THE TRAPEZOIDAL INTEGRATION AND CALCULATE DERIVATIVES.    DL0397
C                                                                       DL0397
C     Y(NT)=4.PI, A CONSTANT VALUE NO MATTER THE GEOMETRY               DL0397
      Y(NT)=FOURPI                                                      DL0397
      BORN(IAT)=FOURPI*Z(NT)                                            DL0397
      IF (LGR) DY(NT)=ZERO                                              DL1097
      DO 31 I=0,NT-1                                                    DL0397
      RAC(IAT)=X(I)                                                     DL0397
      IF (TAREA) THEN                                                   DL0397
         CALL DAREAL (RAC,NUMAT,Y(I),DAREA,NCROSS,NC,IAT,LGR            DL1097
     .               ,DY(I),LGR)                                        DL1097
         BORN(IAT)=BORN(IAT)+Y(I)*Z(I)                                  DL0397
         IF (LGR) THEN                                                  DL0397
C           ADD dY/dV                                                   DL0397
            DO 30 L=0,NCROSS                                            DL0397
            DBORN(1,NC(L),IAT)=DBORN(1,NC(L),IAT)+DAREA(1,L)*Z(I)       DL0397
            DBORN(2,NC(L),IAT)=DBORN(2,NC(L),IAT)+DAREA(2,L)*Z(I)       DL0397
            DBORN(3,NC(L),IAT)=DBORN(3,NC(L),IAT)+DAREA(3,L)*Z(I)       DL0397
   30       CONTINUE                                                    DL0397
         ENDIF                                                          DL0397
      ELSE                                                              DL0397
         BORN(IAT)=BORN(IAT)+AREAH(DOENP,COORD,RAC,IAT,CORCK,DONE2)*Z(I)DL0397
      ENDIF                                                             DL0397
   31 CONTINUE                                                          DL0397
      IF (LGR) THEN                                                     DL0397
C        FACTI = dI/dRI = Y'.(dZ/dX).(dX/dRI) + Z'.(dY/dX).(dX/dRI)     DL1097
C        FACTS = dI/dR  = dI/dRS (SIMILAR EXPRESSION)                   DL1097
         FACTI=ZERO                                                     DL1097
         FACTS=ZERO                                                     DL1097
         DO 40 I=0,NT                                                   DL1097
         FACTI=FACTI+Y(I)*DZI(I)+Z(I)*DY(I)*DXI(I)                      DL1097
   40    FACTS=FACTS+Y(I)*DZS(I)+Z(I)*DY(I)*DXS(I)                      DL1097
C        ADD (dI/dR).dRLIO(IAC,IUP)                                     DL1097
         X(1)=FACTS*URLIO(1,IUP,IAT)                                    DL1097
         X(2)=FACTS*URLIO(2,IUP,IAT)                                    DL1097
         X(3)=FACTS*URLIO(3,IUP,IAT)                                    DL1097
         DBORN(1,IUP,IAT)=DBORN(1,IUP,IAT)-X(1)                         DL1097
         DBORN(2,IUP,IAT)=DBORN(2,IUP,IAT)-X(2)                         DL1097
         DBORN(3,IUP,IAT)=DBORN(3,IUP,IAT)-X(3)                         DL1097
         DBORN(1,IAT,IAT)=DBORN(1,IAT,IAT)+X(1)                         DL1097
         DBORN(2,IAT,IAT)=DBORN(2,IAT,IAT)+X(2)                         DL1097
         DBORN(3,IAT,IAT)=DBORN(3,IAT,IAT)+X(3)                         DL1097
      ENDIF                                                             DL0397
C     RESET THE PROPER RADIUS FOR ATOM I                                DL0397
      RAC(IAT)=RINF                                                     DL0397
C     SET FINAL VALUE FOR BORN(IAT) AND DERIVATIVES:                    DL0397
   50 BORN(IAT)=FOURPI/BORN(IAT)                                        DL0397
      IF (LGR) THEN                                                     DL0397
C        ADD (dI/dRI).(dRI/dV) + (dI/dRS).(dRS/dV)                      DL0397
         DO 60 J=1,NUMAT                                                DL0397
         IF (DRAC(J,IAT).NE.ZERO) THEN                                  DL1097
            X(1)=FACTI*DRAC(J,IAT)*URLIO(1,J,IAT)                       DL1097
            X(2)=FACTI*DRAC(J,IAT)*URLIO(2,J,IAT)                       DL1097
            X(3)=FACTI*DRAC(J,IAT)*URLIO(3,J,IAT)                       DL1097
            DBORN(1,J,IAT)=DBORN(1,J,IAT)-X(1)                          DL1097
            DBORN(2,J,IAT)=DBORN(2,J,IAT)-X(2)                          DL1097
            DBORN(3,J,IAT)=DBORN(3,J,IAT)-X(3)                          DL1097
            DBORN(1,IAT,IAT)=DBORN(1,IAT,IAT)+X(1)                      DL1097
            DBORN(2,IAT,IAT)=DBORN(2,IAT,IAT)+X(2)                      DL1097
            DBORN(3,IAT,IAT)=DBORN(3,IAT,IAT)+X(3)                      DL1097
         ENDIF                                                          DL1097
         IF (DRAC(J,IUP).NE.ZERO) THEN                                  DL1097
            X(1)=FACTS*DRAC(J,IUP)*URLIO(1,J,IUP)                       DL1097
            X(2)=FACTS*DRAC(J,IUP)*URLIO(2,J,IUP)                       DL1097
            X(3)=FACTS*DRAC(J,IUP)*URLIO(3,J,IUP)                       DL1097
            DBORN(1,J,IAT)=DBORN(1,J,IAT)-X(1)                          DL1097
            DBORN(2,J,IAT)=DBORN(2,J,IAT)-X(2)                          DL1097
            DBORN(3,J,IAT)=DBORN(3,J,IAT)-X(3)                          DL1097
            DBORN(1,IUP,IAT)=DBORN(1,IUP,IAT)+X(1)                      DL1097
            DBORN(2,IUP,IAT)=DBORN(2,IUP,IAT)+X(2)                      DL1097
            DBORN(3,IUP,IAT)=DBORN(3,IUP,IAT)+X(3)                      DL1097
         ENDIF                                                          DL1097
   60    CONTINUE                                                       DL0397
C        ACCOUNT FOR FINAL INVERSION OF BORN:                           DL0397
         CALL DSCAL (3*NUMAT,-BORN(IAT)**2/FOURPI,DBORN(1,1,IAT),1)     DL0397
      ENDIF                                                             DL0397
  100 CONTINUE                                                          DL0397
      END
