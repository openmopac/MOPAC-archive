      SUBROUTINE WRITMO(TIME0,FUNCT,*)                                  CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      CHARACTER*160 KEYWRD
      CHARACTER*80  KOMENT,TITLE
CSTP  For conversion of subroutine MECI to a function its old return
CSTP  value now is put in RMECI.
      DOUBLE PRECISION RMECI                                            CSTP
C      DOUBLE PRECISION MECI
      COMMON /KEYWRD/ KEYWRD
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /ELEMTS/ ELEMNT(120)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /HMATRX/ H(MPACK)
      COMMON /FOKMAT/ F(MPACK), FB(MPACK)
      COMMON /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
C    changed common path for portability  (IR)
      COMMON /PATHI / LATOM,LPARAM
      COMMON /PATHR / REACT(200)
      COMMON /NUMSCF/ NSCF
C     COMMON /WMATRX/ WJ(N2ELEC), WK(N2ELEC),KDUMMY,NBAND(NUMATM)
      COMMON /WMATRX/ WJ(N2ELEC), WK(N2ELEC*2),KDUMMY,NBAND(NUMATM)
      COMMON /ATHEAT/ ATHEAT
      COMMON /CORE  / CORE(120)
      COMMON /SCRACH/ RXYZ(MPACK), XDUMY(MAXPAR**2-MPACK)
      COMMON /CIMATS/ ENGYCI(3),VECTCI(9),ECI(6)
      COMMON /MESAGE/ IFLEPO,IITER
      COMMON /ATMASS/ ATMASS(NUMATM)
      COMMON /ENUCLR/ ENUCLR
      COMMON /ELECT / ELECT
      COMMON /XYZGRA/ DXYZ(3,NUMATM*27)
      COMMON /GRADNT/ GRAD(MAXPAR), GNORM
C Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
     3       /MOLKSR/ FRACT
      COMMON /GEOVAR/ XPARAM(MAXPAR), NVAR, LOC(2,MAXPAR)               IR0394
      COMMON /NUMCAL/ NUMCAL
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      include 'result.f'                                                LF1210
      include 'method.f'                                                LF0312
      COMMON /SUMDISP/ DISPTOTAL, LDISPON                               LF0312
      LOGICAL LDISPON                                                   LF0412
************************************************************************
*
*   WRITE PRINTS OUT MOST OF THE RESULTS.
*         IT SHOULD NOT ALTER ANY PARAMETERS, SO THAT IT CAN BE CALLED
*         AT ANY CONVENIENT TIME.
*
************************************************************************
      DIMENSION Q(MAXORB), Q2(MAXORB), COORD(3,NUMATM)
     1,IEL1(120), NELEMT(120), IEL2(120)
      DIMENSION Q3(MAXORB)                                              0909YC99
      DIMENSION W(N2ELEC), DUMY(3)
      LOGICAL UHF, CI, SINGLT, TRIPLT, EXCITD, PRTGRA, XYZ
      CHARACTER TYPE(3)*11, IDATE*24, CALCN(2)*5, GTYPE*13, GRTYPE*14,
     1          FLEPO(16)*58, ITER(2)*58, NUMBRS(11)*1                  IR0494
      CHARACTER*2 ELEMNT, IELEMT(20), SPNTYP*7, CALTYP*7
      EQUIVALENCE (W,WJ)
         SAVE                                                           GL0892
      DATA TYPE/'BOND       ','ANGLE      ','DIHEDRAL   '/
      DATA CALCN /'     ','ALPHA'/
      DATA NUMBRS /'0','1','2','3','4','5','6','7','8','9',' '/
      DATA ICALCN /0/
      DATA FLEPO(1),FLEPO(2),FLEPO(3)/
     1' 1SCF WAS SPECIFIED, SO BFGS WAS NOT USED                 ',
     2' GRADIENTS WERE INITIALLY ACCEPTABLY SMALL                ',
     3' HERBERTS TEST WAS SATISFIED IN BFGS                      '/
      DATA FLEPO(4),FLEPO(5),FLEPO(6)/
     1' THE LINE MINIMIZATION FAILED TWICE IN A ROW.   TAKE CARE!',
     2' BFGS FAILED DUE TO COUNTS EXCEEDED. TAKE CARE!           ',
     3' PETERS TEST WAS SATISFIED IN BFGS OPTIMIZATION           '/
      DATA FLEPO(7),FLEPO(8),FLEPO(9)/
     1' THIS MESSAGE SHOULD NEVER APPEAR, CONSULT A PROGRAMMER!! ',
     2' GRADIENT TEST NOT PASSED, BUT FURTHER WORK NOT JUSTIFIED ',
     3' A FAILURE HAS OCCURRED, TREAT RESULTS WITH CAUTION!!     '/
      DATA FLEPO(10),FLEPO(11),FLEPO(12)/
     1' GEOMETRY OPTIMIZED USING NLLSQ. GRADIENT NORM MINIMIZED  ',
     2' GEOMETRY OPTIMIZED USING POWSQ. GRADIENT NORM MINIMIZED  ',
     3' CYCLES EXCEEDED, GRADIENT NOT FULLY MINIMIZED IN NLLSQ   '/
      DATA FLEPO(13),FLEPO(14),FLEPO(15)/                                IR0394
     1' 1SCF RUN AFTER RESTART.  GEOMETRY MIGHT NOT BE OPTIMIZED ',
     2' HEAT OF FORMATION MINIMIZED IN ONE LINE SEARCH           ',
     3' GEOMETRY OPTIMISED USING EIGENVECTOR FOLLOWING (EF).     '/
      DATA FLEPO(16)/
     1' EF-OPTIMIZED GEOMETRY.  NUMBER OF -VE ROOTS INCORRECT    '/
      DATA ITER/
     1' SCF FIELD WAS ACHIEVED                                   ',
     2'  ++++----**** FAILED TO ACHIEVE SCF. ****----++++        '/
C
C SUMMARY OF RESULTS (NOTE: THIS IS IN A SUBROUTINE SO IT
C          CAN BE USED BY THE PATH OPTION)
c      write(6,*) "NOPEN=",nopen,"   NCLOSE=",nclose                     debug
c      write(6,*) "NALPHA=",nalpha,"   NBETA=",nbeta                     debug
      PI=3.141592653589D0
      IDATE=' '
      IF(IFLEPO.EQ.0) IFLEPO=7
      IUHF=MIN(INDEX(KEYWRD,'UHF'),1)+1
      PRTGRA=(INDEX(KEYWRD,' GRAD').NE.0)
      LINEAR=(NORBS*(NORBS+1))/2
      XYZ=(INDEX(KEYWRD,' XYZ') .NE. 0)
      SINGLT=(INDEX(KEYWRD,'SINGLET') .NE. 0)
      TRIPLT=(INDEX(KEYWRD,'TRIPLET') .NE. 0)
      EXCITD=(INDEX(KEYWRD,'EXCITED') .NE. 0)
      SPNTYP='GROUND '
      IF(SINGLT) SPNTYP='SINGLET'
      IF(TRIPLT) SPNTYP='TRIPLET'
      IF(EXCITD) SPNTYP='EXCITED'
      CI=(INDEX(KEYWRD,'C.I.') .NE. 0)
      IF(INDEX(KEYWRD,'MINDO') .NE. 0) THEN
         CALTYP='MINDO/3'
      ELSEIF(INDEX(KEYWRD,'AM1-D') .NE. 0) THEN                         LF0509
         CALTYP='  AM1-D'                                               LF0509
      ELSEIF( (INDEX(KEYWRD,'AM1') .NE. 0) .AND.
     1        (INDEX(KEYWRD,'AM-1').EQ. 0)) THEN                        LF0509
         CALTYP='  AM1  '
      ELSEIF(INDEX(KEYWRD,'PM3-D') .NE. 0) THEN                         LF0309
         CALTYP='  PM3-D'                                               LF0309
      ELSEIF( (INDEX(KEYWRD,'PM3') .NE. 0) .AND. 
     1        (INDEX(KEYWRD,'PM3-D') .EQ. 0) ) THEN                     LF0309
         CALTYP='  PM3  '
      ELSEIF(INDEX(KEYWRD,'PDG') .NE. 0) THEN
         CALTYP='  PDG  '
      ELSEIF(INDEX(KEYWRD,'MDG') .NE. 0) THEN
         CALTYP='  MDG  '
      ELSEIF(INDEX(KEYWRD,'RM1') .NE. 0) THEN
         CALTYP='  RM1  '
      ELSEIF(INDEX(KEYWRD,'PM6') .NE. 0) THEN                           LF0909
         CALTYP='  PM6  '                                               LF0909
      ELSEIF(INDEX(KEYWRD,'PMOV1') .NE. 0) THEN                         LF0614
         CALTYP='  PMOv1'                                               LF0614
      ELSEIF(INDEX(KEYWRD,'PMO2') .NE. 0) THEN                          LF0614
         CALTYP='  PMO2 '                                               LF0614
      ELSEIF(INDEX(KEYWRD,'PMO2A') .NE. 0) THEN                         LF0614
         CALTYP='  PMO2a'                                               LF0614
      ELSEIF(INDEX(KEYWRD,'MNDO-D') .NE. 0) THEN                        LF0614
         CALTYP=' MNDO-D'                                               LF0614
      ELSE
         CALTYP=' MNDO  '
      ENDIF
      UHF=(IUHF.EQ.2)
      IF (DOPRNT) CALL DATE(IDATE)                                      CIO
      DEGREE=57.29577951D0
      IF(NA(1).EQ.99)THEN
         DEGREE=1.D0
         TYPE(1)='           '
         TYPE(2)='           '
         TYPE(3)='           '
      ENDIF
      GNORM=0.D0
      IF(NVAR.NE.0)GNORM=SQRT(ddot(nvar,grad,1,grad,1))
      IF (DOPRNT) THEN                                                  CIO
      WRITE(6,'(/,'' ----'',15(''-----''))')
      WRITE(6,'(A)')KEYWRD,KOMENT,TITLE                                 
      WRITE(6,'(//4X,A58)')FLEPO(IFLEPO)
      ENDIF                                                             CIO
      IITER=MAX(1,IITER)
      IF (DOPRNT) THEN                                                  CIO
      WRITE(6,'(4X,A58)')ITER(IITER)
      WRITE(6,'(//30X,A7,''  CALCULATION'')')CALTYP
      WRITE(6,'(55X,''VERSION '',F5.3)')VERSON                          JP1203
      WRITE(6,'(55X,A24)')IDATE
      ENDIF                                                             CIO
      IF(IITER.EQ.2)THEN
C
C   RESULTS ARE MEANINGLESS. DON'T PRINT ANYTHING!
C
         IF (DOPRNT) THEN                                                  CIO
         WRITE(6,'(//,'' FOR SOME REASON THE SCF CALCULATION FAILED.'',/
     1,'' THE RESULTS WOULD BE MEANINGLESS, SO WILL NOT BE PRINTED.'')')
         WRITE(6,'('' TRY TO FIND THE REASON FOR THE FAILURE BY USING ''
     1,''"PL".'',/,
     2'' CHECK YOUR GEOMETRY AND ALSO TRY USING SHIFT OR PULAY. '')')
         ENDIF                                                             CIO
         CALL GEOUT(*9999)                                               CSTP(call)
         RETURN 1                                                        IR0295
      ENDIF
      
      FNATMS=NUMAT                                                      LF0111
      FHEAT=FUNCT                                                       LF0710
      FATOMZ=FHEATS-FHEAT                                               LF1110
      FTOTAL=ELECT+ENUCLR                                               LF0710
      FEELEC=ELECT                                                      LF0710
      FECORE=ENUCLR                                                     LF0710
      FDISP=DISPTOTAL                                                   LF0312
c      FGRADT assigned in DERIV subroutine.                              LF1010
      IF (DOPRNT) THEN                                                  CIO
      WRITE(6,'(////10X,''FINAL HEAT OF FORMATION ='',F17.5,
     &      '' KCAL/MOL'')')FUNCT                                       LF0310
      IF (INDEX(KEYWRD,"PM3-D3").NE.0) THEN                             LF1211
      WRITE(6,'(10X,''(CAUTION USING HEAT OF FORMATION FOR PM3-D3.)'')')LF1211
      ELSEIF (INDEX(KEYWRD,"PM3-D").NE.0) THEN                          LF0409 / LF1211
      WRITE(6,'(10X,''(CAUTION USING HEAT OF FORMATION FOR PM3-D.)'')') LF0409
      ENDIF                                                             LF1211
      IF (INDEX(KEYWRD,"AM1-D3").NE.0) THEN                             LF1111
      WRITE(6,'(10X,''(CAUTION USING HEAT OF FORMATION FOR AM1-D3.)'')')LF1111
      ELSEIF (INDEX(KEYWRD,"AM1-D").NE.0) THEN                          LF0509 / LF1111
      WRITE(6,'(10X,''(CAUTION USING HEAT OF FORMATION FOR AM1-D.)'')') LF0509
      ENDIF                                                             LF1111
      IF (INDEX(KEYWRD,"MNDO-D3").NE.0) THEN                            LF1111
         WRITE(6,'(10X,                                                 LF1111
     &    ''(CAUTION USING HEAT OF FORMATION FOR MNDO-D3.)'')')         LF1111
      ELSEIF (INDEX(KEYWRD,"MNDO-D").NE.0) THEN                         LF0310 / LF1111
         WRITE(6,'(10X,                                                 LF0310 / LF1111
     &    ''(CAUTION USING HEAT OF FORMATION FOR MNDO-D.)'')')          LF0310 / LF1111
      ENDIF                                                             LF1111
      IF (INDEX(KEYWRD,"PM6-D3").NE.0) THEN                             LF1211
      WRITE(6,'(10X,''(CAUTION USING HEAT OF FORMATION FOR PM6-D3.)'')')LF1211
      ELSEIF ((INDEX(KEYWRD,"PM6-D").NE.0).OR.                          LF0310 / LF1211
     &   (INDEX(KEYWRD,"PM6G09-D").NE.0)) THEN                          LF0310 / LF1211
      WRITE(6,'(10X,''(CAUTION USING HEAT OF FORMATION FOR PM6-D.)'')') LF0310
      ENDIF                                                             LF1211
      IF (INDEX(KEYWRD,"RM1-D3").NE.0) THEN                             LF1211
      WRITE(6,'(10X,''(CAUTION USING HEAT OF FORMATION FOR RM1-D3.)'')')LF1211
      ELSEIF (INDEX(KEYWRD,"RM1-D").NE.0) THEN                          LF0310 / LF1211
      WRITE(6,'(10X,''(CAUTION USING HEAT OF FORMATION FOR RM1-D.)'')') LF0310
      ENDIF                                                             LF1211
      IF (LATOM.EQ.0) WRITE(6,'(/)')
      WRITE(6,'(    10X,''TOTAL ENERGY            ='',F17.9,'' EV''
     1,''  ='',F14.2,'' KCAL/MOL'')')ELECT+ENUCLR,                      LF0310
     2(ELECT+ENUCLR)*23.061D0                                           LF0310
      WRITE(6,'(    10X,''ELECTRONIC ENERGY       ='',F17.5,'' EV''
     1,''  ='',F14.2,'' KCAL/MOL'')')ELECT,(ELECT*23.061D0)             LF0310
      WRITE(6,'(    10X,''CORE-CORE REPULSION     ='',F17.5,'' EV''
     1,''  ='',F14.2,'' KCAL/MOL'')')ENUCLR,(ENUCLR*23.061D0)           LF0310
      IF (DSPMET)                                                       LF0312
     &WRITE(6,'(    10X,''DISPERSION ENERGY       ='',F17.5,'' EV''     LF0312
     &,''  ='',F20.8,'' KCAL/MOL'')')DISPTOTAL/23.061d0,DISPTOTAL       LF0312
      IF(LATOM.EQ.0) WRITE(6,'(1X)')
      ENDIF                                                             CIO
      PRTGRA=(PRTGRA .OR. GNORM .GT. 2.D0)
      IF(DOPRNT.AND.PRTGRA)                                             CIO
     1WRITE(6,'(    10X,''GRADIENT NORM           ='',F17.5)')GNORM
      IF(LATOM.NE.0) THEN
C
C   WE NEED TO CALCULATE THE REACTION COORDINATE GRADIENT.
C
         MVAR=NVAR
         LOC11=LOC(1,1)
         LOC21=LOC(2,1)
         NVAR=1
         LOC(1,1)=LATOM
         LOC(2,1)=LPARAM
         XREACT=GEO(LPARAM,LATOM)
         CALL DERIV(GEO,GCOORD,*9999)                                    CSTP(call)
         NVAR=MVAR
         LOC(1,1)=LOC11
         LOC(2,1)=LOC21
         GRTYPE=' KCAL/ANGSTROM'
         IF(LPARAM.EQ.1)THEN
            IF (DOPRNT)                                                 CIO
     &       WRITE(6,'(    10X,''FOR REACTION COORDINATE ='',F17.5,     CIO
     &         '' ANGSTROMS'')')XREACT
         ELSE
            IF(NA(1).NE.99)GRTYPE=' KCAL/RADIAN  '
            IF(DOPRNT) WRITE(6,'(    10X,''FOR REACTION COORDINATE ='', CIO
     &        F17.5,'' DEGREES'')')XREACT*DEGREE                        CIO
         ENDIF
         IF (DOPRNT) WRITE(6,'(    10X,''REACTION GRADIENT       ='',   CIO
     &      F17.5,A14)')GCOORD,GRTYPE                                   CIO
      ENDIF
      IF(NALPHA.GT.0)THEN
         EIONIS=-MAX(EIGS(NALPHA), EIGB(NBETA))
      ELSEIF(NELECS.EQ.1)THEN
         EIONIS=-EIGS(1)
      ELSEIF(NELECS.GT.1) THEN
         EIONIS=-MAX(EIGS(NCLOSE), EIGS(NOPEN))
      ELSE
         EIONIS=0.D0
      ENDIF
      NOPN=NOPEN-NCLOSE
C   CORRECTION TO I.P. OF DOUBLETS
      IF(NOPN.EQ.1)THEN
         I=NCLOSE*NORBS+1
         XIIII= SPCG(C(I),C(I),C(I),C(I),W,WJ)
         EIONIS=EIONIS+0.5D0*XIIII
      ENDIF
      FIONZN=EIONIS                                                     LF0710
      IF (DOPRNT)
     & WRITE(6,'(       10X,''IONIZATION POTENTIAL    ='',F17.5,
     &      " EV  (KOOPMANS'' THEOREM)")')
     &                EIONIS
      IF( UHF ) THEN
         IF (DOPRNT) WRITE(6,'(      10X,''NO. OF ALPHA ELECTRONS  ='',
     &                   I11)')NALPHA
         IF (DOPRNT) WRITE(6,'(      10X,''NO. OF BETA  ELECTRONS  ='',
     &                   I11)')NBETA
      ELSE
         IF (DOPRNT) WRITE(6,'(      10X,''NO. OF FILLED LEVELS    ='',
     &                   I11)')NCLOSE
         IF(NOPN.NE.0) THEN
            IF (DOPRNT) WRITE(6,'(   10X,''AND NO. OF OPEN LEVELS  ='',
     &                        I11)')NOPN
         ENDIF
      ENDIF
      SUMW=0
      DO 10 I=1,NUMAT
   10 SUMW=SUMW+ATMASS(I)
      IF(DOPRNT.AND.SUMW.GT.0.1D0)
     1WRITE(6,'(    10X,''MOLECULAR WEIGHT        ='',F11.3)')SUMW
      IF(LATOM.EQ.0.AND.DOPRNT) WRITE(6,'(/)')
      IF(DOPRNT) WRITE(6,'(10X,''SCF CALCULATIONS  =   '',I14 )') NSCF
      TIM=SECOND()-TIME0
      I=TIM*0.000001D0
      TIM=TIM-I*1000000
      CALL TIMOUT(6,TIM,*9999)
      IF( NDEP .NE. 0 )CALL SYMTRY(*9999)
      IF(NA(1).NE.99) THEN
         DO 50 J=1,NATOMS
            DO 50 I=1,3
               X=GEO(I,J)
               GOTO (40, 30, 20) I
   20          X=X - AINT(X/(2.D0*PI)+SIGN(0.4999D0,X)-0.0001D0)*PI*2.D0
               GEO(3,J)=X
               GOTO 40
   30          X=X - AINT(X/(2.D0*PI))*PI*2.D0
               IF(X.LT.0)X=X+PI*2.D0
               IF(X .GT. PI) THEN
                  GEO(3,J)=GEO(3,J)+PI
                  X=2.D0*PI-X
               ENDIF
               GEO(2,J)=X
   40          CONTINUE
   50    CONTINUE
      ENDIF
      DO 60 I=1,NVAR
   60 XPARAM(I)=GEO(LOC(2,I),LOC(1,I))
      CALL GMETRY(GEO,COORD,*9999)                                      CSTP(call)
      IF(PRTGRA.AND.DOPRNT)THEN                                         CIO
         WRITE(6,'(///7X,''FINAL  POINT  AND  DERIVATIVES'',/)')
         WRITE(6,'(''   PARAMETER     ATOM    TYPE  ''
     1    ,''          VALUE       GRADIENT'')')
      ENDIF
      SUM=0.5D0
      DO 70 I=1,NUMAT
   70 SUM=SUM+CORE(NAT(I))
      I=SUM
      KCHRGE=I-NCLOSE-NOPEN-NALPHA-NBETA
C
C    WRITE OUT THE GEOMETRIC VARIABLES
C
      IF(PRTGRA) THEN
         DO 80 I=1,NVAR
            J=LOC(2,I)
            K=LOC(1,I)
            L=LABELS(K)
            XI=XPARAM(I)
            IF(J.NE.1) XI=XI*DEGREE
            IF(J.EQ.1.OR.NA(1).EQ.99)THEN
               GTYPE='KCAL/ANGSTROM'
            ELSE
               GTYPE='KCAL/RADIAN  '
            ENDIF
   80    IF(DOPRNT)WRITE(6,'(I7,I11,1X,A2,4X,A11,F13.6,F13.6,2X,A13)')  CIO
     &                   I,K,ELEMNT(L),TYPE(J),XI,GRAD(I),GTYPE
      ENDIF
C
C     WRITE OUT THE GEOMETRY
C
      IF (DOPRNT) WRITE(6,'(///)')                                      CIO
      CALL GEOUT(*9999)                                                 CSTP(call)
      IF (INDEX(KEYWRD,'NOINTER') .EQ. 0) THEN
C
C   WRITE OUT THE INTERATOMIC DISTANCES
C
         L=0
         DO 90 I=1,NUMAT
            DO 90 J=1,I
               L=L+1
   90    RXYZ(L)=SQRT((COORD(1,I)-COORD(1,J))**2+
     1                         (COORD(2,I)-COORD(2,J))**2+
     2                         (COORD(3,I)-COORD(3,J))**2)
         IF (DOPRNT) WRITE(6,'(//10X,''  INTERATOMIC DISTANCES'')')     CIO
         CALL VECPRT(RXYZ,NUMAT,*9999)                                  CSTP(call)
         do 95 mycount=1,190                                            LJF
   95    FDIST(mycount)=RXYZ(mycount)                                   LJF
      ENDIF
      DO 100 I=1,NORBS
  100 IF(EIGS(I).LT.-999.D0.OR.EIGS(I).GT.1000.D0)EIGS(I)=0.D0
      DO 110 I=1,NORBS
  110 IF(EIGB(I).LT.-999.D0.OR.EIGB(I).GT.1000.D0)EIGS(I)=0.D0
      IF(NORBS.GT.0)THEN
         IF (INDEX(KEYWRD,'VECT') .NE. 0) THEN
            IF (DOPRNT) WRITE(6,'(//10X,A5,'' EIGENVECTORS  '')')       CIO
     &                          CALCN(IUHF)                             CIO
            CALL MATOUT (C,EIGS,NORBS,NORBS,NORBS,*9999)                CSTP(call)
            IF(UHF) THEN
               IF (DOPRNT) WRITE(6,'(//10X,'' BETA EIGENVECTORS  '')')  CIO
               CALL MATOUT (CBETA,EIGB,NORBS,NORBS,NORBS,*9999)         CSTP(call)
            ENDIF
         ELSE
            IF (DOPRNT) THEN                                            CIO
             WRITE(6,'(//10X,A5,''   EIGENVALUES'',/)')CALCN(IUHF)
             WRITE(6,'(8F10.5)')(EIGS(I),I=1,NORBS)
            ENDIF                                                       CIO
            IF(UHF) THEN
               IF (DOPRNT) THEN                                         CIO
                WRITE(6,'(//10X,'' BETA EIGENVALUES '')')
                WRITE(6,'(8F10.5)')(EIGB(I),I=1,NORBS)
               ENDIF                                                    CIO
            ENDIF
         ENDIF
      ENDIF
      IF (DOPRNT) WRITE(6,'(//13X,'' NET ATOMIC CHARGES AND DIPOLE '',  CIO
     1''CONTRIBUTIONS'')')                                              0909YC99
      IF (DOPRNT.AND.INDEX(KEYWRD,'CM2') .NE. 0)                        0909YC99 / CIO
     1     WRITE (6,'(25X,'' CALCULATED WITH CM2'')')                   0909YC99
      IF (DOPRNT) THEN                                                  CIO
        WRITE(6,*)                                                      0909YC99
        WRITE(6,'(8X,'' ATOM NO.   TYPE          CHARGE        ATOM''
     1  ,''  ELECTRON DENSITY'')')
        CALL CHRGE(P,Q,*9999)                                           CSTP(call)
        IF (INDEX(KEYWRD,'CM2').EQ.0) THEN                              0909YC99
          DO 120 I=1,NUMAT
            L=NAT(I)
            Q2(I)=CORE(L) - Q(I)
  120     WRITE(6,'(I12,9X,A2,4X,F13.4,F16.4)')                         CIO
     1                     I,ELEMNT(L),Q2(I),Q(I)
        ELSE                                                            0909YC99
          DO I=1,NUMAT                                                  0909YC99
            L=NAT(I)                                                    0909YC99
            Q2(I) = Q(I)                                                0909YC99
            Q3(I)=CORE(L) - Q(I)                                        0909YC99
          IF (DOPRNT) WRITE(6,'(I12,9X,A2,4X,F13.4,F16.4)')             0909YC99
     1                     I,ELEMNT(L),Q2(I),Q3(I)                      0909YC99
          ENDDO                                                         0909YC99
        ENDIF                                                           0909YC99
      ENDIF                                                             CIO
      DIP= DIPOLE(P,Q2,COORD,DUMY)
      IF (DOPRNT.AND.INDEX(KEYWRD,'NOXYZ') .EQ. 0) THEN                 CIO
         WRITE(6,'(//10X,''CARTESIAN COORDINATES '',/)')
         WRITE(6,'(4X,''NO.'',7X,''ATOM'',15X,''X'',
     1  9X,''Y'',9X,''Z'',/)')
         WRITE(6,'(I6,8X,A2,14X,3F10.4)')
     1  (I,ELEMNT(NAT(I)),(COORD(J,I),J=1,3),I=1,NUMAT)
      ENDIF
      IF(NORBS.GT.0) THEN
         IF (INDEX(KEYWRD,'FOCK') .NE. 0) THEN
            IF (DOPRNT) WRITE(6,'('' FOCK MATRIX IS '')')               CIO
            CALL VECPRT(F,NORBS,*9999)                                  CSTP(call)
         ENDIF
         IF (INDEX(KEYWRD,'DENSI') .NE. 0) THEN
            IF (DOPRNT) WRITE(6,'(//,20X,'' DENSITY MATRIX IS '')')     CIO
            CALL VECPRT(P,NORBS,*9999)                                  CSTP(call)
         ELSE
            IF (DOPRNT) THEN                                            CIO
            WRITE(6,'(//10X,''ATOMIC ORBITAL ELECTRON POPULATIONS'',/)')
            WRITE(6,'(8F10.5)')(P((I*(I+1))/2),I=1,NORBS)
            ENDIF                                                       CIO
         ENDIF
         IF(INDEX(KEYWRD,' PI') .NE. 0) THEN
            IF(DOPRNT)WRITE(6,'(//10X,''SIGMA-PI BOND-ORDER MATRIX'')') CIO
            CALL DENROT(*9999)                                          CSTP(call)
         ENDIF
         IF(UHF) THEN
            SZ=ABS(NALPHA-NBETA)*0.5D0
            SS2=SZ*SZ
            L=0
            DO 140 I=1,NORBS
               DO 130 J=1,I
                  L=L+1
                  PA(L)=PA(L)-PB(L)
  130          SS2=SS2+PA(L)**2
  140       SS2=SS2-0.5D0*PA(L)**2
            IF (DOPRNT) WRITE(6,'(//20X,''(SZ)    ='',F10.6)')SZ        CIO
            IF (DOPRNT) WRITE(6,'(  20X,''(S**2)  ='',F10.6)')SS2       CIO
            IF(INDEX(KEYWRD,'SPIN') .NE. 0) THEN
               IF (DOPRNT) WRITE(6,'(//10X,''SPIN DENSITY MATRIX'')')   CIO
               CALL VECPRT(PA,NORBS,*9999)                              CSTP(call)
            ELSE
               IF (DOPRNT) THEN                                         CIO
               WRITE(6,'(//10X,''ATOMIC ORBITAL SPIN POPULATIONS'',/)')
               WRITE(6,'(8F10.5)')(PA((I*(I+1))/2),I=1,NORBS)
               ENDIF                                                    CIO
            ENDIF
            IF(INDEX(KEYWRD,'HYPERFINE') .NE. 0) THEN
C
C  WORK OUT THE HYPERFINE COUPLING CONSTANTS.
C
               IF (DOPRNT) WRITE(6,'(//10X,                             CIO
     &            ''    HYPERFINE COUPLING COEFFICIENTS'',/)')          CIO
               J=(NALPHA-1)*NORBS
               DO 150 K=1,NUMAT
                  I=NFIRST(K)
C#          WRITE(6,'('' PA:'',F13.6,'' C('',I2,''+'',I3,''):'',
C#     +F13.5)')PA((I*(I+1))/2),I,J,C(I+J)
  150          Q(K)=PA((I*(I+1))/2)*0.3333333D0+C(I+J)**2*0.66666666D0
               IF (DOPRNT) WRITE(6,'(5(2X,A2,I2,F9.5,1X))')             CIO
     1    (ELEMNT(NAT(I)),I,Q(I),I=1,NUMAT)
            ENDIF
            DO 160 I=1,LINEAR
  160       PA(I)=P(I)-PB(I)
         ENDIF
         IF (INDEX(KEYWRD,'BONDS') .NE. 0) THEN
            IF(NBETA.EQ.0)THEN
               IF (DOPRNT) WRITE(6,'(/10X,                              CIO
     &                     ''BONDING CONTRIBUTION OF EACH M.O.'',/)')   CIO
               CALL MOLVAL(C,NORBS,P,NORBS,2.D0,*9999)                  CSTP(call)
            ELSE
               IF (DOPRNT) WRITE(6,'(/10X,                              CIO
     &                ''BONDING CONTRIBUTION OF EACH ALPHA M.O.'',/)')  CIO
               CALL MOLVAL(C,NORBS,P,NORBS,1.D0,*9999)                  CSTP(call)
               IF (DOPRNT) WRITE(6,'(/10X,                              CIO
     &         ''BONDING CONTRIBUTION OF EACH BETA  M.O.'',/)')         CIO
               CALL MOLVAL(C,NORBS,P,NORBS,1.D0,*9999)                  CSTP(call)
            ENDIF
            CALL BONDS(P,*9999)                                         CSTP(call)
         ENDIF
         I=NCLOSE+NALPHA
         IF (INDEX(KEYWRD,'LOCAL') .NE. 0) THEN
            CALL LOCAL(C,NORBS,I,EIGS,*9999)                            CSTP(call)
            IF(NBETA.NE.0)THEN
               IF (DOPRNT) WRITE(6,'(//10X,                             CIO
     &               '' LOCALIZED BETA MOLECULAR ORBITALS'')')          CIO
               CALL LOCAL(CBETA,NORBS,NBETA,EIGB,*9999)                 CSTP(call)
            ENDIF
         ENDIF
         IF (INDEX(KEYWRD,'1ELE') .NE. 0) THEN
            IF (DOPRNT) WRITE(6,'('' FINAL ONE-ELECTRON MATRIX '')')    CIO
            CALL VECPRT(H,NORBS,*9999)                                  CSTP(call)
         ENDIF
         IF(INDEX(KEYWRD,'ENPART') .NE. 0)
     1CALL ENPART(UHF,H,PA,PB,P,Q,COORD,*9999)                          CSTP(call)
      ENDIF
      DO 170 I=1,107
  170 NELEMT(I)=0
      DO 180 I=1,NUMAT
         IGO=NAT(I)
         IF (IGO.GT.120) GO TO 180
         NELEMT(IGO)=NELEMT(IGO)+1
  180 CONTINUE
      ICHFOR=0
      IF (NELEMT(6).EQ.0) GO TO 190
      ICHFOR=1
      IELEMT(1)=ELEMNT(6)
      NZS=NELEMT(6)
      IF (NZS.LT.10) THEN
         IF (NZS.EQ.1) THEN
            IEL1(1)=11
         ELSE
            IEL1(1)=NZS+1
         ENDIF
         IEL2(1)=11
      ELSE
         KFRST=NZS/10
         KSEC=NZS-(10*KFRST)
         IEL1(1)=KFRST+1
         IEL2(1)=KSEC+1
      ENDIF
  190 NELEMT(6)=0
      DO 200 I=1,107
         IF (NELEMT(I).EQ.0) GO TO 200
         ICHFOR=ICHFOR+1
         IELEMT(ICHFOR)=ELEMNT(I)
         NZS=NELEMT(I)
         IF (NZS.LT.10) THEN
            IF (NZS.EQ.1) THEN
               IEL1(ICHFOR)=11
            ELSE
               IEL1(ICHFOR)=NZS+1
            ENDIF
            IEL2(ICHFOR)=11
         ELSE
            KFRST=NZS/10
            KSEC=NZS-(10*KFRST)
            IEL1(ICHFOR)=KFRST+1
            IEL2(ICHFOR)=KSEC+1
         ENDIF
  200 CONTINUE
      IF(INDEX(KEYWRD,'DENOUT') .NE. 0) THEN
         OPEN(UNIT=10,FILE='FOR010',STATUS='UNKNOWN',                   No CIO
     &                   FORM='UNFORMATTED')                            No CIO
         REWIND 10                                                      No CIO
         WRITE(10)(PA(I),I=1,LINEAR)                                    No CIO
         WRITE(10)(PB(I),I=1,LINEAR)                                    No CIO           
         CLOSE (10)                                                     No CIO
      ENDIF
      IF((CI.OR.NOPEN.NE.NCLOSE.OR.INDEX(KEYWRD,'SIZE').NE.0)
     1.AND.(INDEX(KEYWRD,'MECI').NE.0.OR.INDEX(KEYWRD,'ESR').NE.0))THEN
C        CALL CIPARM                                                    JZ0315
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     1''MULTI-ELECTRON CONFIGURATION INTERACTION CALCULATION'',//)')
         NMOS=0
         NCIS=0
         IF(INDEX(KEYWRD,'C.I.=').NE.0)
     1      NMOS=READA(KEYWRD,INDEX(KEYWRD,'C.I.=')+5)
C
C   SET UP C.I. PARAMETERS
C   NMOS IS NO. OF M.O.S USED IN C.I.
C   NCIS IS CHANGE IN SPIN, OR NUMBER OF STATES
C
         IF(NMOS.EQ.0) NMOS=NOPEN-NCLOSE
         IF(NCIS.EQ.0) THEN
            IF(TRIPLT.OR.INDEX(KEYWRD,'QUAR').NE.0)NCIS=1
            IF(INDEX(KEYWRD,'QUIN').NE.0.OR.INDEX(KEYWRD,'SEXT').NE.0)
     1   NCIS=2
         ENDIF
         CALL MECI(EIGS,C,CBETA,EIGB,RMECI, NORBS,NMOS,NCIS,.TRUE.,
     1             .TRUE.)                                              CSTP CSTP(call)
         X=RMECI                                                        CSTP
CSTP     X=MECI(EIGS,C,CBETA,EIGB, NORBS,NMOS,NCIS, .TRUE.)
      ENDIF
      IF (INDEX(KEYWRD,'MULLIK').NE.0.OR.INDEX(KEYWRD,'GRAPH').NE.0)THEN
         IF (DOPRNT.AND.INDEX(KEYWRD,'MULLIK') .NE. 0)
     1   WRITE(6,'(/10X,'' MULLIKEN POPULATION ANALYSIS'')')
         IF (DOPRNT.AND.INDEX(KEYWRD,'GRAPH') .NE. 0)
     1   WRITE(6,'(/10X,'' DATA FOR GRAPH WRITTEN TO DISK'')')
         CALL MULLIK(C,CBETA,UHF,H,F,NORBS,P,RXYZ,*9999)                CSTP(call)
      ENDIF
C
C  NOTE THAT THE DENSITY, H AND F MATRICES ARE CORRUPTED BY A
C  CALL TO MULLIK.
      IF (ICALCN .NE. NUMCAL) THEN
         ICALCN = NUMCAL
         IF(DOPRNT) OPEN(UNIT=12,FILE='FOR012',STATUS='UNKNOWN')        CIO
         IF(DOPRNT) REWIND 12                                           CIO
      ENDIF
      IWRITE=12
      IF (DOPRNT) THEN                                                  CIO
         WRITE(IWRITE,'(//20X,'' SUMMARY OF '',A7,
     1'' CALCULATION'',/)')CALTYP
         WRITE(IWRITE,'(60X,''VERSION '',F5.3)')VERSON                  JP1203 
         WRITE (IWRITE,210) (IELEMT(I),NUMBRS(IEL1(I)),NUMBRS(IEL2(I))
     1,I=1,ICHFOR)
      ENDIF                                                             CIO
  210 FORMAT (//,1X,17(A2,A1,A1))
      IF (DOPRNT) WRITE(IWRITE,'(55X,A24)')IDATE                        CIO
      DO 220 IK=80,3,-1
  220 IF(KOMENT(IK:IK).NE.' ')GOTO 230
  230 IF (DOPRNT) WRITE(IWRITE,'(A)')KOMENT(:IK)                        CIO
      DO 240 IT=80,3,-1
  240 IF(TITLE(IT:IT).NE.' ')GOTO 250
  250 IF (DOPRNT) WRITE(IWRITE,'(A)')TITLE(:IT)                         CIO
      IF (DOPRNT) THEN                                                  CIO
        WRITE(IWRITE,'(//4X,A58)')FLEPO(IFLEPO)
        WRITE(IWRITE,'(4X,A58)')ITER(IITER)
        WRITE(IWRITE,'(//10X,''HEAT OF FORMATION       =''
     1,F17.6,'' KCAL'')')FUNCT
        WRITE(IWRITE,'(  10X,''ELECTRONIC ENERGY       =''
     1,F17.6,'' EV'')')ELECT
        WRITE(IWRITE,'(  10X,''CORE-CORE REPULSION     =''
     1,F17.6,'' EV'')')ENUCLR
      ENDIF                                                             CIO
      IF(PRTGRA.AND.DOPRNT)                                             CIO
     1WRITE(IWRITE,'(  10X,''GRADIENT NORM           =''
     2,F17.6)')GNORM
      IF(LATOM.NE.0) THEN
         GRTYPE=' KCAL/ANGSTROM'
         IF(LPARAM.EQ.1)THEN
            IF (DOPRNT) WRITE(IWRITE,                                   CIO
     &                   '(    10X,''FOR REACTION COORDINATE ='',F17.4, CIO
     &                   '' ANGSTROMS'')')XREACT                        CIO
         ELSE
            IF(NA(1).NE.99)GRTYPE=' KCAL/RADIAN  '
            IF (DOPRNT) WRITE(IWRITE,'(    10X,                         CIO
     &                   ''FOR REACTION COORDINATE ='',F17.4,           CIO
     &                   '' DEGREES'')')XREACT*DEGREE                   CIO
         ENDIF
         IF (DOPRNT) WRITE(IWRITE,'(    10X,                            CIO
     &        ''REACTION GRADIENT       ='',F17.6,A14)')GCOORD,GRTYPE   CIO
      ENDIF
      IF (DOPRNT) WRITE(IWRITE,'(  10X,''DIPOLE                  =''    CIO
     1,F16.5, '' DEBYE'')')DIP
      IF (DOPRNT) THEN                                                  CIO
        IF(UHF) THEN
         WRITE(IWRITE,'(  10X,''(SZ)                    ='',F17.6)')SZ
         WRITE(IWRITE,'(  10X,''(S**2)                  ='',F17.6)')SS2
         WRITE(IWRITE,'(  10X,''NO. OF ALPHA ELECTRONS  ='',I10)')NALPHA
         WRITE(IWRITE,'(  10X,''NO. OF BETA  ELECTRONS  ='',I10)')NBETA
        ELSE
         WRITE(IWRITE,'(  10X,''NO. OF FILLED LEVELS    ='',I10)')NCLOSE
         NOPN=NOPEN-NCLOSE
         IF(NOPN.NE.0)
     1WRITE(IWRITE,'(  10X,''AND NO. OF OPEN LEVELS  ='',I10)')NOPN
        ENDIF
      ENDIF                                                             CIO
      IF (DOPRNT) THEN                                                  CIO
        IF(CI)
     1  WRITE(IWRITE,'(  10X,''CONFIGURATION INTERACTION WAS USED'')')
        IF(KCHRGE.NE.0)
     1  WRITE(IWRITE,'(  10X,''CHARGE ON SYSTEM        ='',I10)')KCHRGE
        WRITE(IWRITE,'(  10X,''IONIZATION POTENTIAL    =''
     1     ,F17.6,'' EV'')')EIONIS
        WRITE(IWRITE,'(  10X,''MOLECULAR WEIGHT        ='',F14.3)')SUMW
        WRITE(IWRITE,'(  10X,''SCF CALCULATIONS        =''
     1     ,I10)') NSCF
      ENDIF                                                             CIO
      TIM=SECOND()-TIME0
      CALL TIMOUT(IWRITE,TIM,*9999)                                     CSTP(call)
      IF (DOPRNT) WRITE(IWRITE,'(//10X,                                 CIO
     &                ''FINAL GEOMETRY OBTAINED'',36X,''CHARGE'')')     CIO
      DO 260 I=160,3,-1
  260 IF(KEYWRD(I:I).NE.' ')GOTO 270
  270 IF (DOPRNT) WRITE(IWRITE,'(A)')KEYWRD(:I)                         CIO
      IF (DOPRNT) WRITE(IWRITE,'(A)')KOMENT(:IK)                        CIO
      IF (DOPRNT) WRITE(IWRITE,'(A)')TITLE(:IT)                         CIO
      NA1=NA(1)
      IF(XYZ) CALL XYZINT(GEO,NATOMS,NA,NB,NC,1.D0,COORD,*9999)         CSTP(call)
      DEGREE=57.29577951D0
      COORD(2,1)=0.D0
      COORD(3,1)=0.D0
      COORD(1,1)=0.D0
      COORD(2,2)=0.D0
      COORD(3,2)=0.D0
      COORD(3,3)=0.D0
      IVAR=1
      NA(1)=0
      L=0
      DO 300 I=1,NATOMS
         DO 280 J=1,3
            IF(.NOT.XYZ)COORD(J,I)=GEO(J,I)
  280    IEL1(J)=0
  290    CONTINUE
         IF(LOC(1,IVAR).EQ.I) THEN
            IEL1(LOC(2,IVAR))=1
            IVAR=IVAR+1
            GOTO 290
         ENDIF
         IF(I.LT.4) THEN
            IEL1(3)=0
            IF(I.LT.3) THEN
               IEL1(2)=0
               IF(I.LT.2) THEN
                  IEL1(1)=0
               ENDIF
            ENDIF
         ENDIF
         IF(I.EQ.LATOM)IEL1(LPARAM)=-1
         Q(1)=COORD(1,I)
         Q(2)=COORD(2,I)*DEGREE
         Q(3)=COORD(3,I)*DEGREE
         IF(LABELS(I).NE.107.AND.LABELS(I).NE.99)THEN
            L=L+1
            IF(DOPRNT)WRITE(IWRITE,'(1X,A2,3(F12.6,I3),I5,2I5,F13.4)')  CIO
     1    ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I),Q2(L)
         ELSE
            IF(DOPRNT)WRITE(IWRITE,'(1X,A2,3(F12.6,I3),I5,2I5,F13.4)')  CIO
     1    ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I)
         ENDIF
  300 CONTINUE
      NA(1)=NA1
      I=0
      X=0.D0
      IF (DOPRNT) WRITE(IWRITE,'(I3,3(F12.6,I3),I5,2I5)')               CIO
     1    I,X,I,X,I,X,I,I,I,I
      DO 310 I=1,NDEP
  310 IF (DOPRNT) WRITE(IWRITE,'(3(I4,'',''))')                         CIO
     &                               LOCPAR(I),IDEPFN(I),LOCDEP(I)      CIO
      IF (DOPRNT) WRITE(IWRITE,'(///)')                                 CIO
      NSCF=0
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY WRITMO_INIT                                                 CSAV
             CALCN(1) = '     '                                         CSAV
             CALCN(2) = 'ALPHA'                                         CSAV
            CALTYP = ""                                                 CSAV
                CI = .FALSE.                                            CSAV
             COORD = 0.0D0                                              CSAV
               DIP = 0.0D0                                              CSAV
              DUMY = 0.0D0                                              CSAV
               ECI = 0.0D0                                              CSAV
            EIONIS = 0.0D0                                              CSAV
            EXCITD = .FALSE.                                            CSAV
            GCOORD = 0.0D0                                              CSAV
            GRTYPE = ""                                                 CSAV
             GTYPE = ""                                                 CSAV
                 I = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
            ICHFOR = 0                                                  CSAV
             IDATE = ""                                                 CSAV
              IEL1 = 0                                                  CSAV
              IEL2 = 0                                                  CSAV
            IELEMT = ""                                                 CSAV
               IGO = 0                                                  CSAV
                IK = 0                                                  CSAV
                IT = 0                                                  CSAV
              IUHF = 0                                                  CSAV
              IVAR = 0                                                  CSAV
            IWRITE = 0                                                  CSAV
                 J = 0                                                  CSAV
                 K = 0                                                  CSAV
            KCHRGE = 0                                                  CSAV
             KFRST = 0                                                  CSAV
              KSEC = 0                                                  CSAV
                 L = 0                                                  CSAV
            LINEAR = 0                                                  CSAV
             LOC11 = 0                                                  CSAV
             LOC21 = 0                                                  CSAV
              MVAR = 0                                                  CSAV
               NA1 = 0                                                  CSAV
              NCIS = 0                                                  CSAV
            NELEMT = 0                                                  CSAV
              NMOS = 0                                                  CSAV
              NOPN = 0                                                  CSAV
               NZS = 0                                                  CSAV
                 Q = 0.0D0                                              CSAV
                Q2 = 0.0D0                                              CSAV
                Q3 = 0.0D0                                              CSAV
             RMECI = 0.0D0                                              CSAV
            SINGLT = .FALSE.                                            CSAV
            SPNTYP = ""                                                 CSAV
               SS2 = 0.0D0                                              CSAV
               SUM = 0.0D0                                              CSAV
              SUMW = 0.0D0                                              CSAV
                SZ = 0.0D0                                              CSAV
               TIM = 0.0D0                                              CSAV
            TRIPLT = .FALSE.                                            CSAV
              TYPE(1) = 'BOND       '                                   CSAV
              TYPE(2) = 'ANGLE      '                                   CSAV
              TYPE(3) = 'DIHEDRAL   '                                   CSAV
               UHF = .FALSE.                                            CSAV
            VECTCI = 0.0D0                                              CSAV
CSAV         For 200 heavy and 400 light atoms causes to overwrite approximately 5.7*10**6 
CSAV         double precision values in W() array (45 megabytes!).  Slows down testsuite by 0.10 seconds.
CSAV         Unnecessary to reset these before another MOPAC run it is believed so skip W() reinitialization.
C                 W = 0.0D0                                              CSAV
                 X = 0.0D0                                              CSAV
                XI = 0.0D0                                              CSAV
             XIIII = 0.0D0                                              CSAV
            XREACT = 0.0D0                                              CSAV
               XYZ = .FALSE.                                            CSAV
      RETURN                                                            CSAV
      END
