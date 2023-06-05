      SUBROUTINE FORCE(*)                                               CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      PARAMETER (MAXPA2=MAXPAR**2)
      COMMON /GEOVAR/ XDUMMY(MAXPAR),NVAR,LOC(2,MAXPAR)                 IR0394
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /FMATRX/ FMATRX(MAXHES)
      COMMON /KEYWRD/ KEYWRD
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      PARAMETER (IPADD=2*MORB2+2*MAXORB-MAXPAR-MAXPA2)
      COMMON /VECTOR/ CNORML(MAXPA2),FREQ(MAXPAR),DUMMY(IPADD)
      COMMON /ELEMTS/ ELEMNT(120)
      COMMON /LAST  / LAST
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /COORD / COORD(3,NUMATM)
***********************************************************************
*
*   FORCE CALCULATES THE FORCE CONSTANTS FOR THE MOLECULE, AND THE
*         VIBRATIONAL FREQUENCIES.  ISOTOPIC SUBSTITUTION IS ALLOWED.
*
***********************************************************************
      COMMON /EULER / TVEC(3,3), ID
      COMMON /SCRACH/ STORE(MAXPAR**2)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION XPARAM(MAXPAR), GR(3,NUMATM),
     1DELDIP(3,MAXPAR), TRDIP(3,MAXPAR),LOCOLD(2,MAXPAR)
     2,REDMAS(MAXPAR), SHIFT(6), DIPT(MAXPAR), TRAVEL(MAXPAR)
     3, ROT(3,3)
      CHARACTER KEYWRD*160, KEYS(80)*1, ELEMNT*2
      LOGICAL RESTRT, LINEAR, DEBUG, BARTEL, PRNT, LARGE
      EQUIVALENCE (GRAD(1), GR(1,1)), (KEYWRD,KEYS(1))
CSAV         SAVE                                                           GL0892
C
C TEST GEOMETRY TO SEE IF IT IS OPTIMIZED
      TIME2=-1.D9
      CALL GMETRY(GEO,COORD,*9999)                                       CSTP(call)
      NVAOLD=NVAR
      DO 10 I=1,NVAR
         LOCOLD(1,I)=LOC(1,I)
   10 LOCOLD(2,I)=LOC(2,I)
      NVAR=0
      NUMAT=0
      IF(LABELS(1) .NE. 99) NUMAT=1
      DO 30 I=2,NATOMS
         IF(LABELS(I).EQ.99.OR.LABELS(I).EQ.107) GOTO 30
         NUMAT=NUMAT+1
         IF(I.EQ.2)ILIM=1
         IF(I.EQ.3)ILIM=2
         IF(I.GT.3)ILIM=3
         DO 20 J=1,ILIM
            NVAR=NVAR+1
            LOC(1,NVAR)=I
            LOC(2,NVAR)=J
   20    XPARAM(NVAR)=GEO(J,I)
   30 CONTINUE
C
C   IF A RESTART, THEN TSCF AND TDER WILL BE FAULTY, THEREFORE SET TO -1
C
      TSCF=-1.D0
      TDER=-1.D0
      PRNT=(INDEX(KEYWRD,'RC=') .EQ. 0)
      DEBUG=(INDEX(KEYWRD,'DFORCE') .NE. 0)
      LARGE=(INDEX(KEYWRD,'LARGE') .NE. 0)
      BARTEL=((INDEX(KEYWRD,'NLLSQ') .NE. 0)                             IR1094
     1         .OR. (INDEX(KEYWRD,' EF') .NE. 0)
     2         .OR. (INDEX(KEYWRD,' TS') .NE. 0))
      RESTRT=(INDEX(KEYWRD,'RESTART') .NE. 0)
      TIME1=SECOND()
      IF (RESTRT) THEN
C
C   CHECK TO SEE IF CALCULATION IS IN NLLSQ OR FORCE.
C
         IF(BARTEL)GOTO 50
C
C   CALCULATION IS IN FORCE
C
         GOTO 70
      ENDIF
      CALL COMPFG( XPARAM, .TRUE., ESCF, .TRUE., GRAD, .FALSE.,*9999)    CSTP(call)
      IF(DOPRNT.AND.PRNT)WRITE(6,'(//10X,''HEAT OF FORMATION ='',F12.6,  CIO
     1'' KCALS/MOLE'')')ESCF                                             CIO
      TIME2=SECOND()
      TSCF=TIME2-TIME1
      CALL COMPFG( XPARAM, .TRUE., ESCF1, .FALSE., GRAD, .TRUE.,*9999)   CSTP(call)
      TIME3=SECOND()
      TDER=TIME3-TIME2
      IF(DOPRNT.AND.PRNT)                                                CIO
     &  WRITE(6,'(//10X,''INTERNAL COORDINATE DERIVATIVES'',//3X,        CIO
     &  ''ATOM  AT. NO.'',2X,''BOND'',9X,''ANGLE'',8X,''DIHEDRAL'',/)')  CIO
      L=0
      IU=0
      DO 40 I=1,NATOMS
         IF(LABELS(I).EQ.99) GOTO 40
         L=L+1
         IL=IU+1
         IF(I .EQ. 1) IU=IL-1
         IF(I .EQ. 2) IU=IL
         IF(I .EQ. 3) IU=IL+1
         IF(I .GT. 3) IU=IL+2
         IF(DOPRNT.AND.PRNT)WRITE(6,'(I6,4X,A2,F13.6,2F10.6)')          CIO
     1L,ELEMNT(LABELS(I)),(GRAD(J),J=IL,IU)
   40 CONTINUE
C   TEST SUM OF GRADIENTS
      I=INDEX(KEYWRD,' GNORM=')                                         IR1094
      IF (I .ne. 0) THEN                                                IR1094
         GNTOL=READA(KEYWRD, I)                                         IR1094
      ELSE                                                              IR1094
         GNTOL=10.0D0                                                   IR1094
      ENDIF                                                             IR1094
      GNORM=SQRT(ddot(nvar,grad,1,grad,1))
      IF(DOPRNT.AND.PRNT)                                               CIO
     &   WRITE(6,'(//10X,''GRADIENT NORM ='',F10.5)') GNORM             CIO
      IF(GNORM .LT. GNTOL) GOTO 60                                      IR1094
      IF(INDEX(KEYWRD,' LET ') .NE. 0) THEN
         IF (DOPRNT) WRITE(6,                                           CIO
     &      '(///1X,''** GRADIENT IS VERY LARGE, BUT SINCE "LET"'',     CIO
     &      '' IS USED, CALCULATION WILL CONTINUE'')')                  CIO
         GOTO 70
      ENDIF
      IF (DOPRNT) WRITE(6,                                              CIO
     &       '(///1X,''** GRADIENT IS TOO LARGE TO ALLOW '',            CIO
     &       ''FORCE MATRIX TO BE CALCULATED, (LIMIT=10) **'',//)')     CIO
   50 CONTINUE
      IF (DOPRNT)                                                       CIO
     &    WRITE(6,'(//10X,'' GEOMETRY WILL BE OPTIMIZED FIRST'')')      CIO
      IF(BARTEL) THEN
         IF( (INDEX(KEYWRD,' EF') + INDEX(KEYWRD,' TS')) .NE. 0) THEN   IR1094
             IF (DOPRNT)                                                CIO
     &         WRITE(6,'(15X,''USING EIGENVECTOR FOLLOWING'')')         IR1094 / CIO
             CALL EF (XPARAM,NVAR,ESCF,*9999)                           IR1094 CSTP(call)
         ELSE                                                           IR1094
             IF (DOPRNT) WRITE(6,'(15X,''USING NLLSQ'')')               CIO
             CALL NLLSQ(XPARAM,NVAR,*9999)                              CSTP(call)
         ENDIF
      ELSE
         IF (DOPRNT) WRITE(6,'(15X,''USING FLEPO'')')                   CIO
         CALL FLEPO(XPARAM,NVAR,ESCF,*9999)                             CSTP(call)
      ENDIF
      CALL COMPFG( XPARAM, .TRUE., ESCF, .TRUE., GRAD, .TRUE.,*9999)    CSTP(call)
      CALL WRITMO(TIME1,ESCF,*9999)                                     CSTP(call)
      IF (DOPRNT) WRITE(6,'(//10X,''GRADIENT NORM ='',F10.7)') GNORM    CIO
      CALL GMETRY(GEO,COORD,*9999)                                      CSTP(call)
   60 CONTINUE
C
C NOW TO CALCULATE THE FORCE MATRIX
C
C CHECK OUT SYMMETRY
   70 CONTINUE
C
C   NEED TO ENSURE THAT XYZINT WILL WORK CORRECTLY BEFORE CALL
C   TO DRC.
C
      L=0
      DO 80 I=1,NATOMS
         IF(LABELS(I).NE.99)THEN
            L=L+1
            LABELS(L)=LABELS(I)
         ENDIF
   80 CONTINUE
      NATOMS=NUMAT
      CALL XYZINT(COORD,NUMAT,NA,NB,NC,1.D0,GEO,*9999)                  CSTP(call)
      CALL GMETRY(GEO,COORD,*9999)                                      CSTP(call)
      IF(INDEX(KEYWRD,'THERMO').NE.0 .AND.GNORM.GT.1.D0) THEN
         IF (DOPRNT) WRITE(6,'(//30X,''**** WARNING ****'',//           CIO
     110X,'' GRADIENT IS VERY LARGE FOR A THERMO CALCULATION'',/
     210X,'' RESULTS ARE LIKELY TO BE INACCURATE IF THERE ARE'')')
         IF (DOPRNT) WRITE(6,'(10X,                                     CIO
     &     '' ANY LOW-LYING VIBRATIONS (LESS THAN ABOUT '',             CIO
     &     ''400CM-1)'')')                                              CIO
         IF (DOPRNT) WRITE(6,'(10X,                                     CIO
     &     '' GRADIENT NORM SHOULD BE LESS THAN ABOUT '',               CIO
     &     ''0.2 FOR THERMO'',/10X,'' TO GIVE ACCURATE RESULTS'')')     CIO
      ENDIF
      IF(TSCF.GT.0.D0) THEN
         IF (DOPRNT) WRITE(6,                                           CIO
     &      '(//10X,''TIME FOR SCF CALCULATION ='',F12.5)')TSCF         CIO
         IF (DOPRNT) WRITE(6,                                           CIO
     &      '(//10X,''TIME FOR DERIVATIVES     ='',F12.5)')TDER         CIO
      ENDIF
      IF(NDEP.GT.0) THEN
         IF (DOPRNT) WRITE(6,'(//10X,''SYMMETRY WAS SPECIFIED, BUT '',  CIO
     1''CANNOT BE USED HERE'')')
         NDEP=0
      ENDIF
      IF(PRNT)CALL AXIS(COORD,NUMAT,A,B,C,WTMOL,2,ROT,*9999)            CSTP(call)
      NVIB=3*NUMAT-6
      IF(ABS(C).LT.1.D-20)NVIB=NVIB+1
      IF(ID.NE.0)NVIB=3*NUMAT-3
      IF(DOPRNT.AND.PRNT) THEN                                          CIO
         WRITE(6,'(/9X,''ORIENTATION OF MOLECULE IN FORCE CALCULATION'')
     1')
         WRITE(6,'(/,4X,''NO.'',7X,''ATOM'',9X,''X'',
     19X,''Y'',9X,''Z'',/)')
      ENDIF
      L=0
      DO 90 I=1,NATOMS
         IF(LABELS(I) .EQ. 99) GOTO 90
         L=L+1
         IF(DOPRNT.AND.PRNT)WRITE(6,'(I6,7X,I3,4X,3F10.4)')             CIO
     1    L,LABELS(I),(COORD(J,L),J=1,3)
   90 CONTINUE
      CALL FMAT(FMATRX, NVIB, TSCF, TDER, DELDIP,ESCF,*9999)             CSTP(call)
C
C   THE FORCE MATRIX IS PRINTED AS AN ATOM-ATOM MATRIX RATHER THAN
C   AS A 3N*3N MATRIX, AS THE 3N MATRIX IS VERY CONFUSING!
C
      IJ=0
      IU=0
      DO 120 I=1,NUMAT
         IL=IU+1
         IU=IL+2
         IM1=I-1
         JU=0
         DO 110 J=1,IM1
            JL=JU+1
            JU=JL+2
            SUM=0.D0
            DO 100 II=IL,IU
               DO 100 JJ=JL,JU
  100       SUM=SUM+FMATRX((II*(II-1))/2+JJ)**2
            IJ=IJ+1
  110    STORE(IJ)=SQRT(SUM)
         IJ=IJ+1
  120 STORE(IJ)=SQRT(
     1FMATRX(((IL+0)*(IL+1))/2)**2+
     2FMATRX(((IL+1)*(IL+2))/2)**2+
     3FMATRX(((IL+2)*(IL+3))/2)**2+2.D0*(
     4FMATRX(((IL+1)*(IL+2))/2-1)**2+
     5FMATRX(((IL+2)*(IL+3))/2-2)**2+
     6FMATRX(((IL+2)*(IL+3))/2-1)**2))
      IF(DEBUG) THEN
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     &       '' FULL FORCE MATRIX, INVOKED BY "DFORCE"'')')             CIO
         I=-NVAR
         CALL VECPRT(FMATRX,I,*9999)                                    CSTP(call)
      ENDIF
      IF(PRNT)THEN
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     &       '' FORCE MATRIX IN MILLIDYNES/ANGSTROM'')')                CIO
         CALL VECPRT(STORE,NUMAT,*9999)                                 CSTP(call)
      ENDIF
      L=(NVAR*(NVAR+1))/2
      DO 130 I=1,L
  130 STORE(I)=FMATRX(I)
      IF(PRNT) CALL AXIS(COORD,NUMAT,A,B,C,SUM,0,ROT,*9999)             CSTP(call)
      IF(DOPRNT.AND.PRNT)WRITE(6,'(//10X,''HEAT OF FORMATION ='',F12.6, CIO
     1'' KCALS/MOLE'')')ESCF
      IF(LARGE)THEN
         CALL FRAME(STORE,NUMAT,0, SHIFT,*9999)                         CSTP(call)
         CALL RSP(STORE,NVAR,NVAR,FREQ,CNORML,*9999)                    CSTP(call)
         DO 140 I=NVIB+1,NVAR
            J=(FREQ(I)+50.D0)*0.01D0
  140    FREQ(I)=FREQ(I)-J*100
         IF(PRNT)THEN
            IF (DOPRNT) THEN                                            CIO
             WRITE(6,'(//10X,''TRIVIAL VIBRATIONS, SHOULD BE ZERO'')')
             WRITE(6,'(/, F9.4,''=TX'',F9.4,''=TY'',F9.4,''=TZ'',
     1             F9.4,''=RX'',F9.4,''=RY'',F9.4,''=RZ'')')
     2(FREQ(I),I=NVIB+1,NVAR)
             WRITE(6,'(//10X,''FORCE CONSTANTS IN MILLIDYNES/ANGSTROM''
     1,'' (= 10**5 DYNES/CM)'',/)')
             WRITE(6,'(8F10.5)')(FREQ(I),I=1,NVIB)
C CONVERT TO WEIGHTED FMAT
             WRITE(6,'(//10X,'' ASSOCIATED EIGENVECTORS'')')
            ENDIF                                                       CIO
            I=-NVAR
            CALL MATOUT(CNORML,FREQ,NVIB,I,NVAR,*9999)                  CSTP(call)
         ENDIF
      ENDIF
      CALL FREQCY(FMATRX,FREQ,CNORML,REDMAS,TRAVEL,.TRUE.,*9999)        CSTP(call)
C
C  CALCULATE ZERO POINT ENERGY
C
C
C  THESE CONSTANTS TAKEN FROM HANDBOOK OF CHEMISTRY AND PHYSICS 62ND ED.
C   N AVOGADRO'S NUMBER = 6.022045*10**23
C   H PLANCK'S CONSTANT = 6.626176*10**(-34)JHZ
C   C SPEED OF LIGHT    = 2.99792458*10**10 CM/SEC
C   CONST=0.5*N*H*C/(1000*4.184)
      CONST=1.4295718D-3
      SUM=0.D0
      DO 150 I=1,NVAR
  150 SUM=SUM+FREQ(I)
      SUM=SUM*CONST
      IF(DOPRNT.AND.PRNT)                                               CIO
     1WRITE(6,'(//10X,'' ZERO POINT ENERGY''
     2, F12.3,'' KILOCALORIES PER MOLE'')')SUM
      SUMM=0.D0
      DO 200 I=1,NVAR
         SUM1=1.D-20
         DO 160 J=1,NVAR
  160    SUM1=SUM1+CNORML(J+(I-1)*NVAR)**2
         SUM1=1.D0/SQRT(SUM1)
         DO 170 K=1,3
  170    GRAD(K)=0.D0
         DO 190 K=1,3
            SUM=0.D0
            DO 180 J=1,NVAR
  180       SUM=SUM+CNORML(J+(I-1)*NVAR)*DELDIP(K,J)
            SUMM=SUMM+ABS(SUM)
  190    TRDIP(K,I)=SUM*SUM1
         DIPT(I)=SQRT(TRDIP(1,I)**2+TRDIP(2,I)**2+TRDIP(3,I)**2)
  200 CONTINUE
      IF(DOPRNT.AND.PRNT)THEN                                           CIO
         WRITE(6,'(//3X,'' THE LAST'',I2,'' VIBRATIONS ARE THE'',
     1'' TRANSLATION AND ROTATION MODES'')')NVAR-NVIB
         WRITE(6,'(3X,'' THE FIRST THREE OF THESE BEING TRANSLATIONS'',
     1'' IN X, Y, AND Z, RESPECTIVELY'')')
      ENDIF
      IF(PRNT.AND.LARGE)THEN
         IF (DOPRNT) WRITE(6,                                           CIO
     &      '(//10X,'' FREQUENCIES, REDUCED MASSES AND '',              CIO
     &      ''VIBRATIONAL DIPOLES''/)')                                 CIO
         NTO6=NVAR/6
         NREM6=NVAR-NTO6*6
         IINC1=-5
         IF (NTO6.LT.1) GO TO 220
         DO 210 I=1,NTO6
            IF (DOPRNT) WRITE (6,'(/)')                                 CIO
            IINC1=IINC1+6
            IINC2=IINC1+5
            IF (DOPRNT) THEN                                            CIO
            WRITE (6,'(3X,''I'',10I10)') (J,J=IINC1,IINC2)
            WRITE (6,'('' FREQ(I)'',6F10.4,/)') (FREQ(J),J=IINC1,IINC2)
            WRITE (6,'('' MASS(I)'',6F10.5,/)') (REDMAS(J),J=IINC1,IINC2
     1)
            WRITE (6,'('' DIPX(I)'',6F10.5)') (TRDIP(1,J),J=IINC1,IINC2)
            WRITE (6,'('' DIPY(I)'',6F10.5)') (TRDIP(2,J),J=IINC1,IINC2)
            WRITE (6,'('' DIPZ(I)'',6F10.5,/)') (TRDIP(3,J),J=IINC1,IINC
     12)
            WRITE (6,'('' DIPT(I)'',6F10.5)')
     1   (DIPT(J),J=IINC1,IINC2)
            ENDIF                                                       CIO
  210    CONTINUE
  220    CONTINUE
         IF (NREM6.LT.1) GO TO 230
         IF (DOPRNT) WRITE (6,'(/)')                                    CIO
         IINC1=IINC1+6
         IINC2=IINC1+(NREM6-1)
         IF (DOPRNT) THEN                                               CIO
         WRITE (6,'(3X,''I'',10I10)') (J,J=IINC1,IINC2)
         WRITE (6,'('' FREQ(I)'',6F10.4)') (FREQ(J),J=IINC1,IINC2)
         WRITE (6,'(/,'' MASS(I)'',6F10.5)') (REDMAS(J),J=IINC1,IINC2)
         WRITE (6,'(/,'' DIPX(I)'',6F10.5)') (TRDIP(1,J),J=IINC1,IINC2)
         WRITE (6,'('' DIPY(I)'',6F10.5)') (TRDIP(2,J),J=IINC1,IINC2)
         WRITE (6,'('' DIPZ(I)'',6F10.5)') (TRDIP(3,J),J=IINC1,IINC2)
         WRITE (6,'(/,'' DIPT(I)'',6F10.5)')
     1   (DIPT(J),J=IINC1,IINC2)
         ENDIF                                                          CIO
  230    CONTINUE
      ENDIF
      IF (DOPRNT) WRITE(6,'(//10X,'' NORMAL COORDINATE ANALYSIS'')')    CIO
      I=-NVAR
      CALL MATOUT(CNORML,FREQ,NVAR,I,NVAR,*9999)                        CSTP(call)
C
C   CARRY OUT IRC IF REQUESTED.
C
      IF(INDEX(KEYWRD,'IRC').NE.0.OR.INDEX(KEYWRD,'DRC').NE.0)THEN
         DO 240 I=1,NVAR
            LOC(1,I)=0
  240    LOC(2,I)=0
         NVAR=NVAOLD
         DO 250 I=1,NVAR
            LOC(1,I)=LOCOLD(1,I)
  250    LOC(2,I)=LOCOLD(2,I)
         CALL XYZINT(COORD,NUMAT,NA,NB,NC,1.D0,GEO,*9999)               CSTP(call)
         LAST=1
         CALL DRC(CNORML,FREQ,*9999)                                    CSTP(call)
      RETURN 1                                                          CSTP (stop)
      ENDIF
      CALL FREQCY(FMATRX,FREQ,CNORML,DELDIP,DELDIP,.FALSE.,*9999)       CSTP(call)
      IF(DOPRNT)                                                        CIO
     &     WRITE(6,'(//10X,'' MASS-WEIGHTED COORDINATE ANALYSIS'')')    CIO
      CALL MATOUT(CNORML,FREQ,NVAR,I,NVAR,*9999)                        CSTP(call)
      CALL ANAVIB(COORD,FREQ,DIPT,NVAR,CNORML,STORE,
     1FMATRX,TRAVEL,REDMAS,*9999)                                       CSTP(call)
      IF(INDEX(KEYWRD,'THERMO').NE.0) THEN
         CALL GMETRY(GEO,COORD,*9999)                                   CSTP(call)
         I=INDEX(KEYWRD,' ROT')
         IF(I.NE.0) THEN
            SYM=READA(KEYWRD,I)
         ELSE
            SYM=1
         ENDIF
         LINEAR=(ABS(A*B*C) .LT. 1.D-10)
         I=INDEX(KEYWRD,' TRANS')
C
C   "I" IS GOING TO MARK THE BEGINNING OF THE GENUINE VIBRATIONS.
C
         IF(I.NE.0)THEN
            I=INDEX(KEYWRD,' TRANS=')
            IF(I.NE.0)THEN
               I=1+READA(KEYWRD,I)
               J=NVIB-I+1
               IF (DOPRNT) WRITE(6,'(//1X,                              CIO
     &                 ''THE LOWEST'',I3,'' VIBRATIONS ARE NOT'',/,     CIO
     &                 '' TO BE USED IN THE THERMO CALCULATION'')')I-1  CIO
            ELSE
               IF (DOPRNT)                                              CIO
     &              WRITE(6,'(//10X,''SYSTEM IS A TRANSITION STATE'')') CIO
               I=2
               J=NVIB-1
            ENDIF
         ELSE
            IF (DOPRNT) WRITE(6,'(//10X,''SYSTEM IS A GROUND STATE'')') CIO
            I=1
            J=NVIB
         ENDIF
         CALL THERMO(A,B,C,LINEAR,SYM,WTMOL,FREQ(I),J,ESCF,*9999)        CSTP(call)
      ENDIF
      RETURN
 9999 RETURN 1                                                          CSTP
      END
