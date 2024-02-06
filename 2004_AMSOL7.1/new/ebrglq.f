      SUBROUTINE EBRGLQ (COORD,BORN,NITER)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'SIZES2.i'
      INCLUDE 'KEYS.i'
      PARAMETER (MT=16)
C ---------------------------------------------------------------------+
C     CALCULATE THE EFFECTIVE BORN RADII BY A GAUSS-LEGENDRE QUADRATURE
C     IN THE METRIC "LOG(X)" + ANALYTICAL INTEGRATION OF THE TAIL.
C     SUITABLE FOR SMx MODELS BEGINNING WITH SM4
C                  AND ALSO ALL SMx.1 MODELS.
C ON INPUT
C     RLIO  = INTERATOMIC DISTANCES
C     URLIO = UNIT VECTORS:
C             URLIO(i,j,k) = (COORD(i,k)-COORD(i,j))/RLIO(j,k)
C                          = dRLIO(j,k)/dCOORD(i,k)
C     RAC   = COULOMB RADII
C     DRAC  = DERIVATIVE: DRAC(i,j) = dRAC(j)/dRLIO(i,j)
C ON OUTPUT
C     BORN  = EFFECTIVE BORN RADII
C     DBORN = CARTESIAN DERIVATIVES OF BORN                             DL1097
C             DBORN(i,j,k) = dBORN(k)/dCOORD(i,j)                       DL1097
C
C     CREATED BY DL 10/97, FOR SOLVATION MODELS USING "DAREAL".
C
C ---------------------------------------------------------------------+
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /ARACOM/ R(NPACK),R2(NPACK),ISAVE,DONE
     1               ,RAC(NUMATM),RAC2(NUMATM),DRAC(NUMATM,NUMATM)      DL1097
     2               ,RAS(NUMATM),RALG(NUMATM)
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM), RMAX(NUMATM)
     1               ,URLIO(3,NUMATM,NUMATM)                            DL1097
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL1097
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL1097
     2               ,DATLGR(3,NUMATM)                                  DL1097
     3               ,LGR,FLAGRD                                        DL1097
                      LOGICAL LGR,FLAGRD                                DL1097
      COMMON /SCRCHR/ BUF(3,NUMATM),W1(NUMATM)                          DL1097
     1               ,W2(6*MAXPAR**2+1-4*NUMATM)                        DL1097
      COMMON /SCRAH1/ X(0:MT),DXI(0:MT),DXS(0:MT),DR36(NUMATM)          DL1097
     1               ,Y(0:MT),DY(0:MT)                                  DL1097
     2               ,Z(0:MT),DZI(0:MT),DZS(0:MT)                       DL1097
     3               ,DAREA(3,0:NUMATM),NC(0:NUMATM)                    DL1097
      COMMON /ONESCM/ ICONTR(100)
      COMMON /IVSZCM/ IVSZ(NUMATM),IVSZC
      LOGICAL DONE,DOENP
      DIMENSION BORN(*),COORD(3,*)
      DIMENSION G(16,4:16),H(16,4:16)
      SAVE FOURPI,TEXPN,TONE                                            DL1097
      DATA ZERO,PT5,ONE /0D0,0.5D0,1D0/                                 DL1097
C
C     (TAKE A BREATH)...
C     GAUSS-LEGENDRE ROOTS & WEIGHTS FOR QUADRATURE IN INTERVAL (-1, 1) DL1097
C     DATA (G(I,2),I=1,2) /-0.577350269189626D0,0.577350269189626D0 /
C     DATA (H(I,2),I=1,2) /2*1D0 /
C     DATA (G(I,3),I=1,3) /-0.774596669241483D0,0D0,0.774596669241483D0/
C     DATA (H(I,3),I=1,3) /0.555555555555556D0,0.888888888888889D0,
C    .0.555555555555556D0/
      DATA (G(I,4),I=1,4) /-0.861136311594053D0,-0.339981043584856D0,
     .0.339981043584856D0,0.861136311594053D0/
      DATA (H(I,4),I=1,4) /0.347854845137454D0,0.652145154862546D0,
     .0.652145154862546D0,0.347854845137454D0/
      DATA (G(I,5),I=1,5) /-0.906179845938664D0,-0.538469310105683D0,
     .0D0,0.538469310105683D0,0.906179845938664D0/
      DATA (H(I,5),I=1,5) /0.236926885056189D0,0.478628670499366D0,
     .0.568888888888889D0,0.478628670499366D0,0.236926885056189D0/
      DATA (G(I,6),I=1,6) /-0.932469514203152D0,-0.661209386466265D0,
     .-0.238619186083197D0,0.238619186083197D0,0.661209386466265D0,
     .0.932469514203152D0/
      DATA (H(I,6),I=1,6) /0.171324492379170D0,0.360761573048139D0,
     .0.467913934572691D0,0.467913934572691D0,0.360761573048139D0,
     .0.171324492379170D0/
      DATA (G(I,7),I=1,7) /-0.949107912342759D0,-0.741531185599394D0,
     .-0.405845151377397D0,0D0,0.405845151377397D0,
     .0.741531185599394D0,0.949107912342759D0/
      DATA (H(I,7),I=1,7) /0.129484966168870D0,0.279705391489277D0,
     .0.381830050505119D0,0.417959183673469D0,0.381830050505119D0,
     .0.279705391489277D0,0.129484966168870D0/
      DATA (G(I,8),I=1,8) /-0.960289856497536D0,-0.796666477413627D0,
     .-0.525532409916329D0,-0.183434642495650D0,0.183434642495650D0,
     .0.525532409916329D0,0.796666477413627D0,0.960289856497536D0/
      DATA (H(I,8),I=1,8) /0.101228536290376D0,0.222381034453374D0,
     .0.313706645877887D0,0.362683783378362D0,0.362683783378362D0,
     .0.313706645877887D0,0.222381034453374D0,0.101228536290376D0/
      DATA (G(I,9),I=1,9) /-0.968160239507626D0,-0.836031107326636D0,
     .-0.613371432700590D0,-0.324253423403809D0,0D0,
     .0.324253423403809D0,0.613371432700590D0,0.836031107326636D0,
     .0.968160239507626D0/
      DATA (H(I,9),I=1,9) /0.081274388361574D0,0.180648160694857D0,
     .0.260610696402935D0,0.312347077040003D0,0.330239355001260D0,
     .0.312347077040003D0,0.260610696402935D0,0.180648160694857D0,
     .0.081274388361574D0/
      DATA (G(I,10),I=1,10) /-0.973906528517172D0,-0.865063366688985D0,
     .-0.679409568299024D0,-0.433395394129247D0,-0.148874338981631D0,
     .0.148874338981631D0,0.433395394129247D0,0.679409568299024D0,
     .0.865063366688985D0,0.973906528517172D0/
      DATA (H(I,10),I=1,10) /0.066671344308688D0,0.149451349150581D0,
     .0.219086362515982D0,0.269266719309996D0,0.295524224714753D0,
     .0.295524224714753D0,0.269266719309996D0,0.219086362515982D0,
     .0.149451349150581D0,0.066671344308688D0/
      DATA (G(I,11),I=1,11) /-0.978228658146057D0,-0.887062599768095D0,
     .-0.730152005574049D0,-0.519096129206812D0,-0.269543155952345D0,
     .0D0,0.269543155952345D0,0.519096129206812D0,
     .0.730152005574049D0,0.887062599768095D0,0.978228658146057D0/
      DATA (H(I,11),I=1,11) /0.055668567116174D0,0.125580369464905D0,
     .0.186290210927734D0,0.233193764591990D0,0.262804544510247D0,
     .0.272925086777901D0,0.262804544510247D0,0.233193764591990D0,
     .0.186290210927734D0,0.125580369464905D0,0.055668567116174D0/
      DATA (G(I,12),I=1,12) /-0.981560634246719D0,-0.904117256370475D0,
     .-0.769902674194305D0,-0.587317954286617D0,-0.367831498998180D0,
     .-0.125233408511469D0,0.125233408511469D0,0.367831498998180D0,
     .0.587317954286617D0,0.769902674194305D0,0.904117256370475D0,
     .0.981560634246719D0/
      DATA (H(I,12),I=1,12) /0.047175336386512D0,0.106939325995318D0,
     .0.160078328543346D0,0.203167426723066D0,0.233492536538355D0,
     .0.249147045813403D0,0.249147045813403D0,0.233492536538355D0,
     .0.203167426723066D0,0.160078328543346D0,0.106939325995318D0,
     .0.047175336386512D0/
      DATA (G(I,13),I=1,13) /-0.984183054718588D0,-0.917598399222978D0,
     .-0.801578090733310D0,-0.642349339440340D0,-0.448492751036447D0,
     .-0.230458315955135D0,0D0,0.230458315955135D0,
     .0.448492751036447D0,0.642349339440340D0,0.801578090733310D0,
     .0.917598399222978D0,0.984183054718588D0/
      DATA (H(I,13),I=1,13) /0.040484004765316D0,0.092121499837728D0,
     .0.138873510219787D0,0.178145980761946D0,0.207816047536889D0,
     .0.226283180262897D0,0.232551553230874D0,0.226283180262897D0,
     .0.207816047536889D0,0.178145980761946D0,0.138873510219787D0,
     .0.092121499837728D0,0.040484004765316D0/
      DATA (G(I,14),I=1,14) /-0.986283808696812D0,-0.928434883663574D0,
     .-0.827201315069765D0,-0.687292904811685D0,-0.515248636358154D0,
     .-0.319112368927890D0,-0.108054948707344D0,0.108054948707344D0,
     .0.319112368927890D0,0.515248636358154D0,0.687292904811685D0,
     .0.827201315069765D0,0.928434883663574D0,0.986283808696812D0/
      DATA (H(I,14),I=1,14) /0.035119460331752D0,0.080158087159760D0,
     .0.121518570687903D0,0.157203167158194D0,0.185538397477938D0,
     .0.205198463721296D0,0.215263853463158D0,0.215263853463158D0,
     .0.205198463721296D0,0.185538397477938D0,0.157203167158194D0,
     .0.121518570687903D0,0.080158087159760D0,0.035119460331752D0/
      DATA (G(I,15),I=1,15) /-0.987992518020485D0,-0.937273392400706D0,
     .-0.848206583410427D0,-0.724417731360170D0,-0.570972172608539D0,
     .-0.394151347077563D0,-0.201194093997435D0,0D0,
     .0.201194093997435D0,0.394151347077563D0,0.570972172608539D0,
     .0.724417731360170D0,0.848206583410427D0,0.937273392400706D0,
     .0.987992518020485D0/
      DATA (H(I,15),I=1,15) /0.030753241996117D0,0.070366047488108D0,
     .0.107159220467172D0,0.139570677926154D0,0.166269205816994D0,
     .0.186161000015562D0,0.198431485327112D0,0.202578241925561D0,
     .0.198431485327112D0,0.186161000015562D0,0.166269205816994D0,
     .0.139570677926154D0,0.107159220467172D0,0.070366047488108D0,
     .0.030753241996117D0/
      DATA (G(I,16),I=1,16) /-0.989400934991650D0,-0.944575023073233D0,
     .-0.865631202387832D0,-0.755404408355003D0,-0.617876244402644D0,
     .-0.458016777657227D0,-0.281603550779259D0,-0.095012509837637D0,
     .0.095012509837637D0,0.281603550779259D0,0.458016777657227D0,
     .0.617876244402644D0,0.755404408355003D0,0.865631202387832D0,
     .0.944575023073233D0,0.989400934991650D0/
      DATA (H(I,16),I=1,16) /0.027152459411754D0,0.062253523938648D0,
     .0.095158511682493D0,0.124628971255534D0,0.149595988816577D0,
     .0.169156519395003D0,0.182603415044924D0,0.189450610455068D0,
     .0.189450610455068D0,0.182603415044924D0,0.169156519395003D0,
     .0.149595988816577D0,0.124628971255534D0,0.095158511682493D0,
     .0.062253523938648D0,0.027152459411754D0/
C     (WHOOF)... TAKE CARE OF, THEY HAVE BEEN VALIDATED!
C
      IF (ICONTR(52).EQ.1) THEN                                         DL1097
         FOURPI=4.D0*PI                                                 DL1097
C        SET NT, THE NUMBER OF "ROOTS" IN THE QUADRATURE.               DL1097
C        NT INCREASES LINEARLY WITH MOLECULAR SIZE.                     DL1097
C        DEFAULT: NT FROM 10 TO 16, 16 BEING REACHED AT 30 ATOMS.       DL1097
C ...    sample of 31 molecules, from 2 to 76 atoms, gives errors (kcal)DL1097
C ...    mean signed= .00035, rms= .019, worst= .049 (9 atoms).         DL1097
         TONE=10D0                                                      DL1097
         TEXPN=0.20D0                                                   DL1097
C        POSSIBLE OVERWRITTE FROM DEVELOPER KEYWORDS:                   DL1097
         IF (ITONE.NE.0) TONE=FITONE                                    DL1097
         IF (ITEXPN.NE.0) TEXPN=FITEXP                                  DL1097
         NT=TONE+TEXPN*NUMAT                                            DL1097
C        NT.GE.4 (RELIABILITY) AND NT.LE.16 (LIMIT OF TABULATION).      DL1097
         NT=MAX(4,MIN(NT,16))                                           DL1097
C        THE "COST" IS CONSTANT, NO MATTER THE MOLECULAR CONFORMATION:  DL1097
         DO 1 I=1,NUMAT                                                 DL1097
    1    IVSZ(I)=NT                                                     DL1097
         DOENP=.TRUE.                                                   DL1097
         ICONTR(52)=2                                                   DL1097
      ENDIF                                                             DL1097
      DO 100 IAT=1,NUMAT                                                DL1097
      IF (LGR) CALL SCOPY (3*NUMAT,ZERO,0,DBORN(1,1,IAT),1)             DL1097
C                                                                       DL1097
C     THE QUADRATURE YIELDS INTEGRAL IN THE FORM I = Y'.Z WITH          DL1097
C     EXPOSED AREA Y(i) CALCULATED AT ABSCISSAE X(i),                   DL1097
C     AND WEIGHTING FACTORS Z(i).                                       DL1097
C                                                                       DL1097
C     FOR SOME CARTESIAN VARIABLE "V", DERIVATIVE FOR THE FORMULA "I":  DL1097
C     dI = dY'.Z + Y'.dZ                                                DL1097
C     WITH, IN EINSTEIN'S CONVENTION (IMPLICIT SUM):                    DL1097
C     dY = dY/dV +                                                      DL1097
C         (dY/dX).((dX/dRI).(dRI/dV)+(dX/dR).(dR/dV)+(dX/dRS).(dRS/dV)) DL1097
C     dZ =(dZ/dX).((dX/dRI).(dRI/dV)+(dX/dR).(dR/dV)+(dX/dRS).(dRS/dV)) DL1097
C         NOTE: dZ/dV = 0                                               DL1097
C     THIS FORMALISM ACCOUNTS FOR THE DEPENDANCIES OF Xs IN:            DL1097
C         R  = UPPER BOUND FOR GL QUADRATURE                            DL1097
C         RI = RAC(IAT) = LOWER BOUND FOR GL QUADRATURE                 DL1097
C         RS = RAC(IUP)                                                 DL1097
C     BUT NEGLECTS ANY DEPENDANCY OF Ys IN THE OTHER RAC(j)!            DL1097
C     RACs MAY DEPEND IN "V" (HN & HO BONDS WITH SM5.4). AVAILABLE:     DL1097
C     dRAC/dV = (dRAC/dRLIO).(dRLIO/dV)                                 DL1097
C                                                                       DL1097
C     THE UPPER BOUND FOR QUADRATURE SHOULD CORRESPOND TO THE ATOM MOST DL1097
C     REMOTE FROM IAT. THEY MAY HAVE SEVERAL, BY HAZARD OR SYMMETRY.    DL1097
C     THE UPPER BOUND MUST BE A SMOOTH FUNCTION OF THE MOLECULAR        DL1097
C     GEOMETRY FOR THE QUADRATURE FORMULA AND ITS GRADIENT TO BE SMOOTH.DL1097
C     CHOICE: RSUP = L36 NORM OF DISTANCES FROM CENTER OF INTEGRATION   DL1097
C     TO SPHERES CENTERED ON OTHER ATOMS IN THE MOLECULE.               DL1097
C                                                                       DL1097
C     STORAGES:                                                         DL1097
C     X  HOLDS  X         DXS    HOLDS   dX/dRS = dX/dR                 DL1097
C                         DXI      "     dX/dRI                         DL1097
C     Z    "    Z         DZS      "     dZ/dRS = dZ/dR                 DL1097
C                         DZI      "     dZ/dRI                         DL1097
C     RAC  "    RAC       DRAC     "     dRAC/dRLIO                     DL1097
C                         URLIO    "     dRLIO/dV                       DL1097
C     Y    "    Y         DAREA    "     dY/dV                          DL1097
C                         DY       "     dY/dX                          DL1097
C                         DR36     "     dR/dRLIO                       DL1097
C                                                                       DL1097
C     FIND THE INTERVAL OF INTEGRATION FOR QUADRATURE: RINF, RSUP.      DL1097
C                                                                       DL1097
      RINF=RAC(IAT)                                                     DL1097
      DO 10 I=1,NUMAT                                                   DL1097
      W1(I)=RLIO(I,IAT)+RAC(I)                                          DL1097
   10 CONTINUE                                                          DL1097
      TARGET=W1(ISMAX(NUMAT,W1,1))                                      DL1097
      RATIO=TARGET/RINF                                                 DL1097
      IF (RATIO.LT.1.00001D0) THEN                                      DL1097
C        ATOM No IAT ALREADY ENCLOSE THE SUPERMOLECULE.                 DL1097
         BORN(IAT)=FOURPI/RINF                                          DL1097
         IF (LGR) THEN                                                  DL1097
            FACTI=-BORN(IAT)/RINF                                       DL1097
            FACTS= ZERO                                                 DL1097
         ENDIF                                                          DL1097
         GOTO 50                                                        DL1097
      ENDIF                                                             DL1097
C     COMPUTE THE L36 NORM = (SUM OVER i OF R(i)**36)**1/36             DL1097
      SUM36=ZERO                                                        DL1097
      DO 11 I=1,NUMAT                                                   DL1097
      IF (I.EQ.IAT) THEN                                                DL1097
         W2(I)=ZERO                                                     DL1097
      ELSE                                                              DL1097
         W2(I)=W1(I)**35                                                DL1097
         SUM36=SUM36+W1(I)*W2(I)                                        DL1097
      ENDIF                                                             DL1097
   11 CONTINUE                                                          DL1097
      RSUPLN=LOG(SUM36)/DBLE(36)                                        DL1097
      RINFLN=LOG(RINF)                                                  DL1097
      RSUP=EXP(RSUPLN)                                                  DL1097
      ASCAL=(RSUPLN-RINFLN)*PT5                                         DL1097
      BSCAL=(RSUPLN+RINFLN)*PT5                                         DL1097
      IF (LGR) THEN                                                     DL1097
         DASCLS= PT5/RSUP                                               DL1097
         DASCLI=-PT5/RINF                                               DL1097
         DBSCLS= DASCLS                                                 DL1097
         DBSCLI=-DASCLI                                                 DL1097
         RATIO=RSUP/SUM36                                               DL1097
         DO 12 I=1,NUMAT                                                DL1097
   12    DR36(I)=RATIO*W2(I)                                            DL1097
      ENDIF                                                             DL1097
C                                                                       DL1097
C     SETUP THE QUADRATURE ABSCISSAE:  X(0), ..., X(NT)                 DL1097
C                          & WEIGTHS:  Z(0), ..., Z(NT)                 DL1097
C     AND RELEVANT PARTIAL DERIVATIVES DXS, DXI, DZS, DZI.              DL1097
C                                                                       DL1097
      DO 20 I=0,NT-1                                                    DL1097
      X(I)=EXP(G(I+1,NT)*ASCAL+BSCAL)                                   DL1097
      Z(I)=H(I+1,NT)*ASCAL/X(I)                                         DL1097
      IF (LGR) THEN                                                     DL1097
         DXS(I)=X(I)*(G(I+1,NT)*DASCLS+DBSCLS)                          DL1097
         DXI(I)=X(I)*(G(I+1,NT)*DASCLI+DBSCLI)                          DL1097
         DZS(I)=(H(I+1,NT)*DASCLS-Z(I)*DXS(I))/X(I)                     DL1097
         DZI(I)=(H(I+1,NT)*DASCLI-Z(I)*DXI(I))/X(I)                     DL1097
      ENDIF                                                             DL1097
   20 CONTINUE                                                          DL1097
      X(NT)=RSUP                                                        DL1097
      Z(NT)=ONE/RSUP                                                    DL1097
      IF (LGR) THEN                                                     DL1097
         DXS(NT)=ONE                                                    DL1097
         DXI(NT)=ZERO                                                   DL1097
         DZS(NT)=-Z(NT)/RSUP                                            DL1097
         DZI(NT)=ZERO                                                   DL1097
      ENDIF                                                             DL1097
C                                                                       DL1097
C     PERFORM THE GL QUADRATURE AND CALCULATE DERIVATIVES.              DL1097
C                                                                       DL1097
      Y(NT)=FOURPI                                                      DL1097
      BORN(IAT)=Y(NT)*Z(NT)                                             DL1097
      DO 31 I=0,NT-1                                                    DL1097
      RAC(IAT)=X(I)                                                     DL1097
      CALL DAREAL (RAC,NUMAT,Y(I),DAREA,NCROSS,NC,IAT,LGR               DL1097
     .            ,DY(I),LGR)                                           DL1097
      BORN(IAT)=BORN(IAT)+Y(I)*Z(I)                                     DL1097
      IF (LGR) THEN                                                     DL1097
C        ADD dY/dV TO CARTESIAN DERIVATIVES                             DL1097
         DO 30 L=0,NCROSS                                               DL1097
         DBORN(1,NC(L),IAT)=DBORN(1,NC(L),IAT)+DAREA(1,L)*Z(I)          DL1097
         DBORN(2,NC(L),IAT)=DBORN(2,NC(L),IAT)+DAREA(2,L)*Z(I)          DL1097
         DBORN(3,NC(L),IAT)=DBORN(3,NC(L),IAT)+DAREA(3,L)*Z(I)          DL1097
   30    CONTINUE                                                       DL1097
      ENDIF                                                             DL1097
   31 CONTINUE                                                          DL1097
      IF (LGR) THEN                                                     DL1097
C        FACTI = dI/dRI = Y'.(dZ/dX).(dX/dRI) + Z'.(dY/dX).(dX/dRI)     DL1097
C        FACTS = dI/dR  = dI/dRS                                        DL1097
C                       = Y'.(dZ/dX).(dX/dR)  + Z'.(dY/dX).(dX/dR)      DL1097
         DY(NT)=ZERO                                                    DL1097
         FACTI=ZERO                                                     DL1097
         FACTS=ZERO                                                     DL1097
         DO 40 I=0,NT                                                   DL1097
         FACTI=FACTI+Y(I)*DZI(I)+Z(I)*DY(I)*DXI(I)                      DL1097
   40    FACTS=FACTS+Y(I)*DZS(I)+Z(I)*DY(I)*DXS(I)                      DL1097
C        ADD (dI/dR).dRLIO(IAC,i)/dV) , i=1,NUMAT                       DL1097
         DO 41 I=1,NUMAT                                                DL1097
         FACTU=FACTS*DR36(I)                                            DL1097
         X(1)=FACTU*URLIO(1,I,IAT)                                      DL1097
         X(2)=FACTU*URLIO(2,I,IAT)                                      DL1097
         X(3)=FACTU*URLIO(3,I,IAT)                                      DL1097
         DBORN(1,I,IAT)=DBORN(1,I,IAT)-X(1)                             DL1097
         DBORN(2,I,IAT)=DBORN(2,I,IAT)-X(2)                             DL1097
         DBORN(3,I,IAT)=DBORN(3,I,IAT)-X(3)                             DL1097
         DBORN(1,IAT,IAT)=DBORN(1,IAT,IAT)+X(1)                         DL1097
         DBORN(2,IAT,IAT)=DBORN(2,IAT,IAT)+X(2)                         DL1097
   41    DBORN(3,IAT,IAT)=DBORN(3,IAT,IAT)+X(3)                         DL1097
      ENDIF                                                             DL1097
C     RESET THE INTRINSIC RADIUS FOR ATOM IAT                           DL1097
      RAC(IAT)=RINF                                                     DL1097
C     SET FINAL VALUE FOR BORN(IAT) AND DERIVATIVES:                    DL1097
   50 BORN(IAT)=FOURPI/BORN(IAT)                                        DL1097
      IF (LGR) THEN                                                     DL1097
C        ADD (dI/dRI).(dRI/dV) + (dI/dRS).(dRS/dV)                      DL1097
         DO 60 J=1,NUMAT                                                DL1097
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
         DO 60 I=1,NUMAT                                                DL1097
         IF (DRAC(J,I).NE.ZERO) THEN                                    DL1097
            FACTU=FACTS*DR36(I)*DRAC(J,I)                               DL1097
            X(1)=FACTU*URLIO(1,I,J)                                     DL1097
            X(2)=FACTU*URLIO(2,I,J)                                     DL1097
            X(3)=FACTU*URLIO(3,I,J)                                     DL1097
            DBORN(1,J,IAT)=DBORN(1,J,IAT)+X(1)                          DL1097
            DBORN(2,J,IAT)=DBORN(2,J,IAT)+X(2)                          DL1097
            DBORN(3,J,IAT)=DBORN(3,J,IAT)+X(3)                          DL1097
            DBORN(1,I,IAT)=DBORN(1,I,IAT)-X(1)                          DL1097
            DBORN(2,I,IAT)=DBORN(2,I,IAT)-X(2)                          DL1097
            DBORN(3,I,IAT)=DBORN(3,I,IAT)-X(3)                          DL1097
         ENDIF                                                          DL1097
   60    CONTINUE                                                       DL1097
C        ACCOUNT FOR FINAL INVERSION OF BORN:                           DL1097
         CALL DSCAL (3*NUMAT,-BORN(IAT)**2/FOURPI,DBORN(1,1,IAT),1)     DL1097
      ENDIF                                                             DL1097
  100 CONTINUE                                                          DL1097
      END
