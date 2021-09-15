      SUBROUTINE WRITE(TIME0,FUNCT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES/NOLIST'
      DIMENSION LOPT(3,NUMATM)
      CHARACTER*80 KEYWRD,KOMENT,TITLE
      COMMON /KEYWRD/ KEYWRD
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /GMETRY/ GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     +                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /HMATRX/ H(MPACK)
      COMMON /FOKMAT/ F(MPACK), FB(MPACK)
      COMMON /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /GEOSYM/ NDEP, LOCPAR(200), IDEPFN(200), LOCDEP(200)
      COMMON /PATH  / LATOM,LPARAM,REACT(100)
      COMMON /NUMSCF/ NSCF
      COMMON /ATHEAT/ ATHEAT
      COMMON /CORE  / CORE(54)
      PARAMETER (MDUMY=MAXORB*MAXORB+(NUMATM*(NUMATM+1))/2-MPACK)
      COMMON /SCRACH/ RXYZ(MPACK), XDUMY(MDUMY)
      COMMON /CIMATS/ ENGYCI(3),VECTCI(9),ECI(6)
      COMMON /MESAGE/ IFLEPO,IITER
      COMMON /CITERM/ XIIII,XJJJJ,XIJJI
      COMMON /ENUCLR/ ENUCLR
      COMMON /ELECT / ELECT
      COMMON /XYZGRA/ DXYZ(3,NUMATM)
      COMMON /GRADNT/ GRAD(MAXPAR), GNORM
      COMMON /MOLKST/ NUMAT,NAT(NUMATM), NFIRST(NUMATM), NMIDLE(NUMATM),
     +  NLAST(NUMATM), NORBS, NELECS, NALPHA, NBETA, NCLOSE, NOPEN
      COMMON /GEOVAR/ NVAR, LOC(2,MAXPAR), XPARAM(MAXPAR)
*************************************************************************
*
*   WRITE PRINTS OUT MOST OF THE RESULTS.
*         IT SHOULD NOT ALTER ANY PARAMETERS, SO THAT IT CAN BE CALLED
*         AT ANY CONVENIENT TIME.
*
*************************************************************************
      DIMENSION Q(MAXORB), Q2(MAXORB), E(MAXORB), COORD(3,NUMATM)
     +,NELEMT(54),IEL1(54), IEL2(54)
      LOGICAL UHF, CI, SINGLT, TRIPLT, EXCITD, PRTGRA
      CHARACTER TYPE(3)*11, IDATE*9, CALCN(2)*5, GTYPE*13, GRTYPE*13,
     +          FLEPO(13)*58, ITER(2)*58
      CHARACTER*2 ELEMNT(99),ATORBS(9), IELEMT(20), SPNTYP*7, CALTYP*7
      DATA ATORBS/' S','PX','PY','PZ','X2','XZ','Z2','YZ','XY'/           
      DATA ELEMNT/' H','He',
     2 'Li','Be',' B',' C',' N',' O',' F','Ne',
     3 'Na','Mg','Al','Si',' P',' S','Cl','Ar',
     4 ' K','Ca','Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu',
     4 'Zn','Ga','Ge','As','Se','Br','Kr',
     5 'Rb','Sr',' Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
     5 'Cd','In','Sn','Sb','Te',' I','Xe',
     6 'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy',
     6 'Ho','Er','Tm','Yb','Lu','Hf','Ta',' W','Re','Os','Ir','Pt',
     6 'Au','Hg','Tl','Pb','Bi','Po','At','Rn',
     7 'Fr','Ra','Ac','Th','Pa',' U','Np','Pu','Am','Cm','Bk','Cf','XX'/
      DATA TYPE/'BOND       ','ANGLE      ','DIHEDRAL   '/
      DATA CALCN /'     ','ALPHA'/
      DATA FLEPO/
     +' 1SCF WAS SPECIFIED, SO FLETCHER-POWELL WAS NOT USED      ',
     1' GRADIENTS WERE INITIALLY ACCEPTABLY SMALL                ',
     2' HERBERTS TEST WAS SATISFIED IN FLETCHER-POWELL           ',
     3' THE LINE MINIMISATION FAILED TWICE IN A ROW.   TAKE CARE!',
     4' FLETCHER POWELL FAILED DUE TO COUNTS EXCEEDED. TAKE CARE!',
     5' PETERS TEST WAS SATISFIED IN FLETCHER-POWELL OPTIMISATION',
     6' THIS MESSAGE SHOULD NEVER APPEAR, CONSULT A PROGRAMMER!! ',
     7' GRADIENT TEST NOT PASSED, BUT FURTHER WORK NOT JUSTIFIED ',
     8' A FAILURE HAS OCCURRED, TREAT RESULTS WITH CAUTION!!     ',
     9' GEOMETRY OPTIMISED USING NLLSQ. GRADIENT NORM MINIMISED  ',
     A' GEOMETRY OPTIMISED USING POWSQ. GRADIENT NORM MINIMISED  ',
     B' CYCLES EXCEEDED, GRADIENT NOT FULLY MINIMIZED IN NLLSQ   ',
     C'                                                          '/
      DATA ITER/
     +' SCF FIELD WAS ACHIEVED                                   ',
     1'  <<<<----**** FAILED TO ACHIEVE SCF. ****---->>>>        '/
C
C SUMMARY OF RESULTS (NOTE: THIS IS IN A SUBROUTINE SO IT
C          CAN BE USED BY THE PATH OPTION)
      IF(IFLEPO.EQ.0) IFLEPO=7
      IUHF=MIN(INDEX(KEYWRD,'UHF'),1)+1
      PRTGRA=(INDEX(KEYWRD,'GRADIENTS').NE.0)
      LINEAR=(NORBS*(NORBS+1))/2
      SINGLT=(INDEX(KEYWRD,'SINGLET') .NE. 0)
      TRIPLT=(INDEX(KEYWRD,'TRIPLET') .NE. 0)
      EXCITD=(INDEX(KEYWRD,'EXCITED') .NE. 0)
                 SPNTYP='GROUND '
      IF(SINGLT) SPNTYP='SINGLET'
      IF(TRIPLT) SPNTYP='TRIPLET'
      IF(EXCITD) SPNTYP='EXCITED'
      CI=(INDEX(KEYWRD,'C.I.') .NE. 0)
      IF(INDEX(KEYWRD,'MINDO3') .NE. 0) THEN
          CALTYP='MINDO/3'
      ELSE
          CALTYP=' MNDO  '
      ENDIF
      UHF=(IUHF.EQ.2)
      CALL DATE(IDATE)
      DEGREE=57.29577951D0
      GNORM=0.D0
      IF(NVAR.NE.0)GNORM=SQRT(DOT(GRAD,GRAD,NVAR))
      WRITE(6,'(1X,A)')KEYWRD,KOMENT,TITLE
      WRITE(6,'(//4X,A58)')FLEPO(IFLEPO)
      WRITE(6,'(4X,A58)')ITER(IITER)
      WRITE(6,'(//30X,A7,''  CALCULATION'')')CALTYP
      WRITE(6,'(60X,''VERSION '',F5.2)')VERSON
      IF(IITER.EQ.2)THEN
C
C   RESULTS ARE MEANINGLESS. DON'T PRINT ANYTHING!
C
      WRITE(6,'(//,'' FOR SOME REASON THE SCF CALCULATION FAILED.'',/,
     +'' THE RESULTS WOULD BE MEANINGLESS, SO WILL NOT BE PRINTED.'',/,
     +'' TRY TO FIND THE REASON FOR THE FAILURE BY USING "PL".'',/,
     +'' CHECK YOUR GEOMETRY AND ALSO TRY USING SHIFT OR PULAY. '')')
      CALL GEOUT
      STOP
      ENDIF     
      WRITE(6,'(////10X,''FINAL HEAT OF FORMATION ='',F13.6,'' KCAL''
     +)')FUNCT
      IF(LATOM.EQ.0) WRITE(6,'(/)')
      WRITE(6,'(    10X,''ELECTRONIC ENERGY       ='',F13.6,'' EV''
     +)')ELECT
      WRITE(6,'(    10X,''CORE-CORE REPULSION     ='',F13.6,'' EV''
     +)')ENUCLR
      IF(LATOM.EQ.0) WRITE(6,'(1X)')
      PRTGRA=(PRTGRA .OR. GNORM .GT. 1.D0)
      IF(PRTGRA)
     +WRITE(6,'(    10X,''GRADIENT NORM           ='',F13.6)')GNORM
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
          CALL DERIV(GEO,GCOORD)
          NVAR=MVAR
          LOC(1,1)=LOC11
          LOC(2,1)=LOC21
          IF(LPARAM.EQ.1)THEN
              GRTYPE='KCAL/ANGSTROM'
              WRITE(6,'(    10X,''FOR REACTION COORDINATE ='',F13.4
     +        ,'' ANGSTROMS'')')XREACT
          ELSE
              GRTYPE='KCAL/RADIAN  '
              WRITE(6,'(    10X,''FOR REACTION COORDINATE ='',F13.4
     +        ,'' DEGREES'')')XREACT*DEGREE
          ENDIF
          WRITE(6,'(    10X,''REACTION GRADIENT       ='',F13.6,A13
     +    )')GCOORD,GRTYPE
      ENDIF
      IF(NALPHA.GT.0)THEN
          EIONIS=-MAX(EIGS(NALPHA), EIGB(NBETA))
      ELSE
          EIONIS=-MAX(EIGS(NCLOSE), EIGS(NOPEN))
      ENDIF
      WRITE(6,'(        10X,''IONISATION POTENTIAL    ='',F13.6)')EIONIS
      WRITE(6,'(60X,A9)')IDATE
      IF( UHF ) THEN
      WRITE(6,'(        10X,''NO. OF ALPHA ELECTRONS  ='',I6)')NALPHA
      WRITE(6,'(        10X,''NO. OF BETA  ELECTRONS  ='',I6)')NBETA
      ELSE
      WRITE(6,'(        10X,''NO. OF FILLED LEVELS    ='',I6)')NCLOSE
      NOPN=NOPEN-NCLOSE
      IF(NOPN.NE.0) THEN
      WRITE(6,'(        10X,''AND NO. OF OPEN LEVELS  ='',I6)')NOPN
      IF(NOPN.EQ.1) THEN
      WRITE(6,'( /10X,''HALF-ELECTRON CORRECTION FOR DOUBLET'',/
     +                  10X,'' =-0.25*<II/II>         ='',F13.6)')XIIII
      ELSE
      WRITE(6,'( /10X,''HALF-ELECTRON CORRECTION FOR '',A7,'' STATE''
     +    ,/)')SPNTYP
              IF( TRIPLT )THEN
      WRITE(6,'(  10X,''-0.25*(<II|II>+<JJ|JJ>)-0.5*<IJ|JI>'')')
                ELSE              
                CI=.TRUE.
      WRITE(6,'(  10X,''-0.25*(<II|II>+<JJ|JJ>)+1.5*<IJ|JI>'')')
                ENDIF
      WRITE(6,'( /30X''<II|II>='',F12.6,/ 30X,''<JJ|JJ>=''F12.6,/
     +30X,''<IJ|JI>='',F12.6)')XIIII,XJJJJ,XIJJI
      ENDIF
      ENDIF
      ENDIF
      IF( CI ) THEN
      WRITE(6,'(  10X,''CONFIGURATION INTERACTION WAS USED'')')
      WRITE(6,'(/10X,'' C.I. MATRIX'')')
      DO 44 I=1,6
  44  ECI(I)=ECI(I)*23.061D0
      DO 45 I=1,3
      ECI((I*(I+1))/2)=ECI((I*(I+1))/2)+ATHEAT+ENUCLR*23.061D0
  45  ENGYCI(I)=(ENGYCI(I)+ENUCLR)*23.061D0+ATHEAT
      CALL VECPRT(ECI,3)
      WRITE(6,'(//10X,''C.I. EIGENVALUES AND EIGENVECTORS'')')
      CALL MATOUT (VECTCI,ENGYCI,3,3,3)
      ENDIF
      IF(LATOM.EQ.0) WRITE(6,'(/)')
      WRITE(6,'(10X,''SCF CALCULATIONS  =   '',I5 )') NSCF
      TIM=SECOND()-TIME0
      WRITE(6,'(10X,''COMPUTATION TIME  ='',F9.2,'' SECONDS'')') TIM
      CALL GMETRY(GEO,COORD)
      IF(PRTGRA)THEN
          WRITE(6,'(///7X,''FINAL  POINT  AND  DERIVATIVES'',/)')
          WRITE(6,'(''   PARAMETER     ATOM    TYPE  ''
     +    ,''          VALUE       GRADIENT'')')
      ENDIF
      SUM=0.5D0
      DO 9 I=1,NUMAT
   9      SUM=SUM+CORE(NAT(I))
      I=SUM
      KCHRGE=I-NCLOSE-NOPEN-NALPHA-NBETA
C
C    WRITE OUT THE GEOMETRIC VARIABLES
C
      IF(PRTGRA) THEN
      DO 10 I=1,NVAR
      J=LOC(2,I)
      K=LOC(1,I)
      L=LABELS(K)
      XI=XPARAM(I)
      IF(J.NE.1) XI=XI*DEGREE
      IF(J.EQ.1)THEN
          GTYPE='KCAL/ANGSTROM'
      ELSE
          GTYPE='KCAL/RADIAN  '
      ENDIF
  10  WRITE(6,'(I7,I11,1X,A2,4X,A11,F13.6,F13.6,2X,A13)')
     +I,K,ELEMNT(L),TYPE(J),XI,GRAD(I),GTYPE
      ENDIF
C
C     WRITE OUT THE GEOMETRY
C
      WRITE(6,'(///)')
      CALL GEOUT
      IF (INDEX(KEYWRD,'NOINTER') .EQ. 0) THEN
C
C   WRITE OUT THE INTERATOMIC DISTANCES
C
      L=0
      DO 17 I=1,NUMAT
      DO 17 J=1,I
      L=L+1
  17  RXYZ(L)=SQRT((COORD(1,I)-COORD(1,J))**2+
     +             (COORD(2,I)-COORD(2,J))**2+
     1             (COORD(3,I)-COORD(3,J))**2)
      WRITE(6,'(//10X,''  INTERATOMIC DISTANCES'')')
      CALL VECPRT(RXYZ,NUMAT)
      ENDIF
      IF (INDEX(KEYWRD,'VECT') .NE. 0) THEN
        WRITE(6,'(//10X,A5,'' EIGENVECTORS  '')')CALCN(IUHF)
        CALL MATOUT (C,EIGS,NORBS,NORBS,NORBS)
        IF(UHF) THEN
            WRITE(6,'(//10X,'' BETA EIGENVECTORS  '')')
            CALL MATOUT (CBETA,EIGB,NORBS,NORBS,NORBS)
        ENDIF
        ELSE
      WRITE(6,'(//10X,A5,''   EIGENVALUES'',/)')CALCN(IUHF)
      WRITE(6,'(8F10.5)')(EIGS(I),I=1,NORBS)
        IF(UHF) THEN
            WRITE(6,'(//10X,'' BETA EIGENVALUES '')')
            WRITE(6,'(8F10.5)')(EIGB(I),I=1,NORBS)
        ENDIF
      END IF
      WRITE(6,'(//13X,'' NET ATOMIC CHARGES AND DIPOLE '',
     +''CONTRIBUTIONS'',/)')
      WRITE(6,'(8X,'' ATOM NO.   TYPE          CHARGE        ATOM''
     +,''  ELECTRON DENSITY'')')
      CALL CHRGE(P,Q)
      DO 20 I=1,NUMAT
      L=NAT(I)
      Q2(I)=CORE(L) - Q(I)
  20  WRITE(6,'(I12,9X,A2,4X,F13.4,F16.4)')
     +I,ELEMNT(L),Q2(I),Q(I)
      IF(KCHRGE.EQ.0) DIP= DIPOLE(P,Q2,COORD,DUMY)                 
      IF (INDEX(KEYWRD,'NOXYZ') .EQ. 0) THEN
        WRITE(6,'(//10X,''CARTESIAN COORDINATES '',/)')
        WRITE(6,'(4X,''NO.'',7X,''ATOM'',9X,''X'',
     +  9X,''Y'',9X,''Z'',/)')
        WRITE(6,'(I6,8X,A2,4X,3F10.4)')
     +  (I,ELEMNT(NAT(I)),(COORD(J,I),J=1,3),I=1,NUMAT)
        END IF
      IF (INDEX(KEYWRD,'FOCK') .NE. 0) THEN
        WRITE(6,'('' FOCK MATRIX IS '')')
        CALL VECPRT(F,NORBS)
        END IF
      IF (INDEX(KEYWRD,'DENS') .NE. 0) THEN
        WRITE(6,'('' DENSITY MATRIX IS '')')
        CALL VECPRT(P,NORBS)
        ELSE
        WRITE(6,'(//10X,''ATOMIC ORBITAL ELECTRON POPULATIONS'',/)')
        WRITE(6,'(8F10.5)')(P((I*(I+1))/2),I=1,NORBS)
        END IF
      IF(UHF) THEN
          SZ=ABS(NALPHA-NBETA)*0.5D0
          SS2=SZ*SZ
          L=0
          DO 13 I=1,NORBS
          DO 12 J=1,I
          L=L+1
          PA(L)=PB(L)-PA(L)
  12      SS2=SS2+PA(L)**2
  13      SS2=SS2-0.5D0*PA(L)**2
          WRITE(6,'(//20X,''<SZ>    ='',F10.6)')SZ
          WRITE(6,'(  20X,''<S**2>  ='',F10.6)')SS2
          IF(INDEX(KEYWRD,'SPIN') .NE. 0) THEN
              WRITE(6,'(//10X,''SPIN DENSITY MATRIX'')')
              CALL VECPRT(PA,NORBS)
              ELSE
              WRITE(6,'(//10X,''ATOMIC ORBITAL SPIN POPULATIONS'',/)')
              WRITE(6,'(8F10.5)')(PA((I*(I+1))/2),I=1,NORBS)
          ENDIF
          DO 11 I=1,LINEAR
  11      PA(I)=P(I)-PB(I)
      ENDIF
      IF (INDEX(KEYWRD,'BONDS') .NE. 0) THEN
        CALL BONDS(P)
        END IF
      IF (NCLOSE.NE.0.AND.INDEX(KEYWRD,'LOCAL') .NE. 0) THEN
        CALL LOCAL(C,NORBS,NCLOSE,EIGS)
        END IF
      IF (INDEX(KEYWRD,'1ELE') .NE. 0) THEN
        WRITE(6,'('' FINAL ONE-ELECTRON MATRIX '')')
        CALL VECPRT(H,NORBS)
        END IF
      IF(INDEX(KEYWRD,'ENPART') .NE. 0)
     +CALL ENPART(UHF,H,PA,PB,P,COORD)
      DO 30 I=1,54
  30  NELEMT(I)=0
      DO 40 I=1,NUMAT
         IGO=NAT(I)
         IF (IGO.GT.54) GO TO 40
         NELEMT(IGO)=NELEMT(IGO)+1
  40  CONTINUE
      ICHFOR=0
      IF (NELEMT(6).EQ.0) GO TO 60
      ICHFOR=1
      IELEMT(1)=ELEMNT(6)
      NZS=NELEMT(6)
      IF (NZS.GT.9) GO TO 50
      IF (NZS.EQ.1) IEL1(1)=32
      IF (NZS.GT.1) IEL1(1)=NZS+48
      IEL2(1)=32
      GO TO 60
  50  KFRST=NZS/10
      KSEC=NZS-(10*KFRST)
      IEL1(1)=KFRST+48
      IEL2(1)=KSEC+48
  60  NELEMT(6)=0
      DO 80 I=1,54
         IF (NELEMT(I).EQ.0) GO TO 80
         ICHFOR=ICHFOR+1
         IELEMT(ICHFOR)=ELEMNT(I)
         NZS=NELEMT(I)
         IF (NZS.GT.9) GO TO 70
         IF (NZS.EQ.1) IEL1(ICHFOR)=32
         IF (NZS.GT.1) IEL1(ICHFOR)=NZS+48
         IEL2(ICHFOR)=32
         GO TO 80
  70     CONTINUE
         KFRST=NZS/10
         KSEC=NZS-(10*KFRST)
         IEL1(ICHFOR)=KFRST+48
         IEL2(ICHFOR)=KSEC+48
  80  CONTINUE
      IF(INDEX(KEYWRD,'DENOUT') .NE. 0) THEN
      WRITE(10)(PA(I),I=1,LINEAR)
      IF(UHF)WRITE(10)(PB(I),I=1,LINEAR)
      ENDIF
      IWRITE=12
      WRITE(IWRITE,'(//20X,'' SUMMARY OF '',A7,
     +'' CALCULATION'',/)')CALTYP
      WRITE(IWRITE,'(60X,''VERSION '',F5.2)')VERSON
      WRITE (IWRITE,100) (IELEMT(I),IEL1(I),IEL2(I),I=1,ICHFOR)
  100 FORMAT (1H0,/,1X,17(A2,2A1))
      WRITE(IWRITE,'(60X,A9)')IDATE
      WRITE(IWRITE,'(1X,A)')KOMENT,TITLE
      WRITE(IWRITE,'(//4X,A58)')FLEPO(IFLEPO)
      WRITE(IWRITE,'(4X,A58)')ITER(IITER)
      WRITE(IWRITE,'(//10X,''HEAT OF FORMATION       =''
     +,F13.6,'' KCAL'')')FUNCT
      WRITE(IWRITE,'(  10X,''ELECTRONIC ENERGY       =''
     +,F13.6,'' EV'')')ELECT
      WRITE(IWRITE,'(  10X,''CORE-CORE REPULSION     =''
     +,F13.6,'' EV'')')ENUCLR
      IF(PRTGRA)
     +WRITE(IWRITE,'(  10X,''GRADIENT NORM           =''
     +,F13.6)')GNORM
      IF(LATOM.NE.0) THEN
          WRITE(IWRITE,'( 10X,''FOR REACTION COORDINATE ='',F13.6
     +    )')GEO(LPARAM,LATOM)
          WRITE(IWRITE,'( 10X,''REACTION GRADIENT       ='',F13.6,A13
     +    )')GCOORD,GRTYPE
      ENDIF
      IF(KCHRGE .EQ. 0)
     +WRITE(IWRITE,'(  10X,''DIPOLE                  =''
     +,F12.5, '' DEBYE'')')DIP
      IF(UHF) THEN
      WRITE(IWRITE,'(  10X,''<SZ>                    ='',F13.6)')SZ
      WRITE(IWRITE,'(  10X,''<S**2>                  ='',F13.6)')SS2
      WRITE(IWRITE,'(  10X,''NO. OF ALPHA ELECTRONS  ='',I6)')NALPHA
      WRITE(IWRITE,'(  10X,''NO. OF BETA  ELECTRONS  ='',I6)')NBETA
      ELSE
      WRITE(IWRITE,'(  10X,''NO. OF FILLED LEVELS    ='',I6)')NCLOSE
      NOPN=NOPEN-NCLOSE
      IF(NOPN.NE.0)
     +WRITE(IWRITE,'(  10X,''AND NO. OF OPEN LEVELS  ='',I6)')NOPN
      ENDIF
      IF(CI)
     +WRITE(IWRITE,'(  10X,''CONFIGURATION INTERACTION WAS USED'')')
      IF(KCHRGE.NE.0)
     +WRITE(IWRITE,'(  10X,''CHARGE ON SYSTEM        ='',I6)')KCHRGE
      WRITE(IWRITE,'(  10X,''IONISATION POTENTIAL    =''
     +,F13.6,'' EV'')')EIONIS
      WRITE(IWRITE,'(  10X,''SCF CALCULATIONS        =''
     +,I6 )') NSCF
      TIM=SECOND()-TIME0
      WRITE(IWRITE,'(  10X,''COMPUTATION TIME        =''
     +,F9.2,'' SECONDS'')') TIM
      IF (INDEX(KEYWRD,'NOXYZ') .EQ. 0) THEN
        WRITE(IWRITE,'(//10X,''CARTESIAN COORDINATES '',/)')
        WRITE(IWRITE,'(4X,''NO.'',7X,''ATOM'',15X,''X'',
     +  15X,''Y'',15X,''Z'',/)')
        WRITE(IWRITE,'(I6,8X,A2,4X,3F16.10)')
     +  (I,ELEMNT(NAT(I)),(COORD(J,I),J=1,3),I=1,NUMAT)
       END IF
      WRITE(IWRITE,'(//10X,''FINAL GEOMETRY OBTAINED'')')
      WRITE(IWRITE,'(1X,A)')KEYWRD,KOMENT,TITLE
      GEO(2,1)=0.D0
      GEO(3,1)=0.D0
      GEO(1,1)=0.D0
      GEO(2,2)=0.D0
      GEO(3,2)=0.D0
      GEO(3,3)=0.D0
      IVAR=1
      NA(1)=0
      DO 110 I=1,NATOMS
          DO 120 J=1,3
  120         IEL1(J)=0
  121         CONTINUE
          IF(LOC(1,IVAR).EQ.I) THEN
              IEL1(LOC(2,IVAR))=1
              IVAR=IVAR+1
              GOTO 121
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
      Q(1)=GEO(1,I)
      Q(2)=GEO(2,I)*DEGREE
      Q(3)=GEO(3,I)*DEGREE
  110     WRITE(IWRITE,'(2X,A2,3(F12.6,I3),I4,2I3)')
     +    ELEMNT(LABELS(I)),(Q(K),IEL1(K),K=1,3),NA(I),NB(I),NC(I)
          I=0
          X=0.D0
          WRITE(IWRITE,'(I4,3(F12.6,I3),I4,2I3)')
     +    I,X,I,X,I,X,I,I,I,I
      DO 130 I=1,NDEP
  130 WRITE(IWRITE,'(3(I4,'',''))')LOCPAR(I),IDEPFN(I),LOCDEP(I)
      WRITE(IWRITE,'(///)')
      NSCF=0
      RETURN
      END
