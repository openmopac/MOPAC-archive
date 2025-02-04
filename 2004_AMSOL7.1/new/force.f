      SUBROUTINE FORCE 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
      PARAMETER (MORB22=MORB2*2, MAXOR2=MAXORB*2) 
C     COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR),IDUMY,DUMY(MAXPAR) 
      COMMON /GEOVAR/ DUMY(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY             3GL3092
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR), 
     1                LOCDEP(MAXPAR) 
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM), 
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM) 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCR / FMATRX(MAXHES),STORE(MAXPAR**2),                 3GL3092 
     1                RDUMY(MAXPAR*(2*MAXPAR+NCHAIN+16) + 29 + NCHAIN)  3GL3092
C     COMMON /OPTIM / IMP,IMP0,LEC,IPRT,FMATRX(MAXHES),STORE(MAXPAR**2) 
C     COMMON /TIME  / TIME0 
      COMMON /TIMECM  / TIME0                                           3GL3092
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM 
      COMMON /VECTOR/ CNORML(MORB22),FREQ(MAXOR2) 
      COMMON /GEOM  / GEO(3,NUMATM) 
      COMMON /COORD / COORD(3,NUMATM) 
      COMMON /EULER / TVEC(3,3), ID 
*********************************************************************** 
* 
*   FORCE CALCULATES THE FORCE CONSTANTS FOR THE MOLECULE, AND THE 
*         VIBRATIONAL FREQUENCIES.  ISOTOPIC SUBSTITUTION IS ALLOWED. 
* 
*********************************************************************** 
      DIMENSION XPARAM(MAXPAR), GR(3,NUMATM), DELDIP(3,MAXPAR), 
     1          TRDIP(3,MAXPAR), REDMAS(MAXPAR), SHIFT(6), ROT(3,3) 
      LOGICAL RESTRT, DIPOK, LINEAR, DEBUG, BARTEL, FAIL, BFGS          DJG0495 
      EQUIVALENCE (GRAD(1), GR(1,1))                                    DJG0995
       SAVE
C 
C TEST GEOMETRY TO SEE IF IT IS OPTIMISED 
      TIME2=-1.D9 
      CALL GMETRY(GEO,COORD) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      NVAR=0 
      NUMAT=0 
      IF(LABELS(1) .NE. 99) NUMAT=1 
      DO 20 I=2,NATOMS 
         IF(LABELS(I).EQ.99.OR.LABELS(I).EQ.107) GOTO 20 
         NUMAT=NUMAT+1 
         IF(I.EQ.2)ILIM=1 
         IF(I.EQ.3)ILIM=2 
         IF(I.GT.3)ILIM=3 
         DO 10 J=1,ILIM 
            NVAR=NVAR+1 
            LOC(1,NVAR)=I 
            LOC(2,NVAR)=J 
   10    XPARAM(NVAR)=GEO(J,I) 
   20 CONTINUE 
C 
C   IF A RESTART, THEN TSCF AND TDER WILL BE FAULTY, THEREFORE SET TO -1 
C 
      TSCF=-1.D0 
      TDER=-1.D0 
      DEBUG=(IDFORC.NE.0)                                               DJG0995 
      BARTEL=(INLLSQ.NE.0)                                              DJG0995
      BFGS=(IBFGS.NE.0.OR.IDFP.NE.0)                                    DJG0995
      RESTRT=(IRESTA.NE.0)                                              DJG0995
      CALL PORCPU (TIME1)                                               GL0492
      IF (RESTRT) THEN 
C 
C   CHECK TO SEE IF CALCULATION IS IN NLLSQ OR FORCE. 
C 
         IF(BARTEL)GOTO 40 
C 
C   CALCULATION IS IN FORCE 
C 
         GOTO 70 
      ENDIF 
      IF(IOLDEN.NE.0) THEN                                              DJG0995
       OPEN(JDEN,STATUS='UNKNOWN',FORM='UNFORMATTED')                   GDH1095
       REWIND JDEN                                                      GDH1095
      ENDIF
      CALL COMPFG( XPARAM,ESCF,FAIL,GRAD,.FALSE.) 
         IF (ISTOP.NE.0.) RETURN                                        GDH1095
         IF (FAIL) THEN                                                 GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=11                                                         GDH1095
      RETURN                                                            GDH1095
         ENDIF                                                          GDH1095
      WRITE(JOUT,'(//10X,''HEAT OF FORMATION ='',F12.6, 
     1'' KCALS/MOLE'')')ESCF 
      CALL PORCPU (TIME2)                                               GL0492
      TSCF=TIME2-TIME1 
      CALL COMPFG( XPARAM,ESCF,FAIL, GRAD, .TRUE.) 
         IF (ISTOP.NE.0.) RETURN                                        GDH1095
         IF (FAIL) THEN                                                 GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=12                                                         GDH1095
      RETURN                                                            GDH1095
         ENDIF                                                          GDH1095
      CALL PORCPU (TIME3)                                               GL0492
      TDER=TIME3-TIME2 
      WRITE(JOUT,'(//10X,''INTERNAL COORDINATE DERIVATIVES'',//3X, 
     1''ATOM  AT. NO.'',2X,''BOND'',9X,''ANGLE'',8X,''DIHEDRAL'',/)') 
      L=0 
      IU=0 
      DO 30 I=1,NATOMS 
         IF(LABELS(I).EQ.99) GOTO 30 
         L=L+1 
         IL=IU+1 
         IF(I .EQ. 1) IU=IL-1 
         IF(I .EQ. 2) IU=IL 
         IF(I .EQ. 3) IU=IL+1 
         IF(I .GT. 3) IU=IL+2 
         WRITE(JOUT,'(2I6,F13.6,2F10.6)') L,LABELS(I),(GRAD(J),J=IL,IU) 
   30 CONTINUE 
C   TEST SUM OF GRADIENTS 
      GNORM=SQRT(DOT(GRAD,GRAD,NVAR)) 
      WRITE(JOUT,'(//10X,''GRADIENT NORM ='',F10.5)') GNORM 
      IF(GNORM.LT.10.D0) GOTO 50 
      WRITE(JOUT,'(///1X,''** GRADIENT IS TOO LARGE TO ALLOW '', 
     1    ''FORCE MATRIX TO BE CALCULATED, (LIMIT=10) **'',//)') 
      IF(ILET.NE.0) GOTO 60                                             DJG0995
   40 CONTINUE 
      WRITE(JOUT,'(//10X,'' GEOMETRY WILL BE OPTIMISED FIRST'')') 
      IF(BARTEL) THEN 
         WRITE(JOUT,'(15X,''USING NLLSQ'')') 
         CALL NLLSQ(XPARAM,NVAR,ESCF,GRAD) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      ELSE IF (BFGS) THEN                                               DJG0495
         WRITE(JOUT,'(15X,''USING FLEPO'')') 
         CALL FLEPO(XPARAM,NVAR,ESCF) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         CALL COMPFG( XPARAM,ESCF,FAIL, GRAD, .TRUE.) 
         IF (ISTOP.NE.0.) RETURN                                        GDH1095
         IF (FAIL) THEN                                                 GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=13                                                         GDH1095
      RETURN                                                            GDH1095
         ENDIF                                                          GDH1095
      ELSE                                                              DJG0495
         WRITE(JOUT,'(15X,''USING EF'')')                               DJG0495
         CALL EF(XPARAM,NVAR,ESCF)                                      DJG0495
         IF (ISTOP.NE.0.) RETURN                                        GDH1095
         CALL COMPFG( XPARAM,ESCF,FAIL, GRAD, .TRUE.)                   DJG0495
         IF (ISTOP.NE.0.) RETURN                                        GDH1095
         IF (FAIL) THEN                                                 GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=68                                                         GDH1095
      RETURN                                                            GDH1095
         ENDIF                                                          GDH1095
      ENDIF                                                             DJG0495
      CALL WRITES(TIME1,ESCF)                                           GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      WRITE(JOUT,'(//10X,''GRADIENT NORM ='',F10.7)') GNORM 
      CALL GMETRY(GEO,COORD) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
   50 CONTINUE 
C 
C NOW TO CALCULATE THE FORCE MATRIX 
C 
C CHECK OUT SYMMETRY 
   60 CONTINUE 
      IF(ITHERM.NE.0.AND.GNORM.GT.1.D0) THEN                            DJG0995
         WRITE(JOUT,'(//30X,''**** WARNING ****'',// 
     110X,'' GRADIENT IS VERY LARGE FOR A THERMO CALCULATION'',/ 
     210X,'' RESULTS ARE LIKELY TO BE INACCURATE IF THERE ARE'')') 
         WRITE(JOUT,'(10X,'' ANY LOW-LYING VIBRATIONS (LESS THAN ABOUT ,GCL0393 
     1'',''400CM-1)'',/                                                 GCL0393
     210X,'' GRADIENT NORM SHOULD BE LESS THAN ABOUT 0.2 FOR THERMO'',// 
     310X,'' TO GIVE ACCURATE RESULTS'')') 
      ENDIF 
      IF(TSCF.GT.0.D0) THEN 
         WRITE(JOUT,'(//10X,''TIME FOR SCF CALCULATION ='',F8.2)')TSCF 
         WRITE(JOUT,'(//10X,''TIME FOR DERIVATIVES     ='',F8.2)')TDER 
      ENDIF 
   70 CONTINUE 
      IF(NDEP.GT.0) THEN 
         WRITE(JOUT,'(//10X,''SYMMETRY WAS SPECIFIED, BUT '', 
     1''CANNOT BE USED HERE'')') 
         NDEP=0 
      ENDIF 
      CALL AXIS(COORD,NUMAT,A,B,C,WTMOL,2,ROT) 
      WRITE(JOUT,'(/9X,''ORIENTATION OF MOLECULE IN FORCE CALCULATION'') 
     1') 
      WRITE(JOUT,'(/,4X,''NO.'',7X,''ATOM'',9X,''X'', 
     19X,''Y'',9X,''Z'',/)') 
      L=0 
      DO 80 I=1,NATOMS 
         IF(LABELS(I) .EQ. 99) GOTO 80 
         L=L+1 
         WRITE(JOUT,'(I6,7X,I3,4X,3F10.4)') 
     1    L,LABELS(I),(COORD(J,L),J=1,3) 
   80 CONTINUE 
      CALL FMAT(FMATRX, TSCF, TDER, DELDIP,ESCF) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
C 
C   THE FORCE MATRIX IS PRINTED AS AN ATOM-ATOM MATRIX RATHER THAN 
C   AS A 3N*3N MATRIX, AS THE 3N MATRIX IS VERY CONFUSING] 
C 
      IJ=0 
      IU=0 
      DO 110 I=1,NUMAT 
         IL=IU+1 
         IU=IL+2 
         IM1=I-1 
         JU=0 
         DO 100 J=1,IM1 
            JL=JU+1 
            JU=JL+2 
            SUM=0.D0 
            DO 90 II=IL,IU 
               DO 90 JJ=JL,JU 
   90       SUM=SUM+FMATRX((II*(II-1))/2+JJ)**2 
            IJ=IJ+1 
  100    STORE(IJ)=SQRT(SUM) 
         IJ=IJ+1 
  110 STORE(IJ)=SQRT( 
     1FMATRX(((IL+0)*(IL+1))/2)**2+ 
     2FMATRX(((IL+1)*(IL+2))/2)**2+ 
     3FMATRX(((IL+2)*(IL+3))/2)**2+2.D0*( 
     4FMATRX(((IL+1)*(IL+2))/2-1)**2+ 
     5FMATRX(((IL+2)*(IL+3))/2-2)**2+ 
     6FMATRX(((IL+2)*(IL+3))/2-1)**2)) 
      IF(DEBUG) THEN 
         WRITE(JOUT,'(//10X,'' FULL FORCE MATRIX, INVOKED BY "DFORCE"'') 
     1') 
         I=-NVAR 
         CALL VECPRT(FMATRX,I) 
      ENDIF 
      WRITE(JOUT,'(//10X,'' FORCE MATRIX IN MILLIDYNES/ANGSTROM'')') 
      CALL VECPRT(STORE,NUMAT) 
      L=(NVAR*(NVAR+1))/2 
      DO 120 I=1,L 
  120 STORE(I)=FMATRX(I) 
      CALL AXIS(COORD,NUMAT,A,B,C,SUM,0,ROT) 
      CALL FRAME(STORE,NUMAT,0, SHIFT) 
      CALL HQRII(STORE,NVAR,NVAR,FREQ,CNORML) 
      WRITE(JOUT,'(//10X,''HEAT OF FORMATION ='',F12.6, 
     1'' KCALS/MOLE'')')ESCF 
      NVIB=NVAR-6 
      IF(ABS(C).LT.1.D-20)NVIB=NVIB+1 
      IF(ID.NE.0)NVIB=NVAR-3 
      DO 130 I=NVIB+1,NVAR 
         J=(FREQ(I)+50.D0)*0.01D0 
  130 FREQ(I)=FREQ(I)-J*100 
      WRITE(JOUT,'(//10X,''TRIVIAL VIBRATIONS, SHOULD BE ZERO'')') 
      WRITE(JOUT,'(/, F9.4,''=TX'',F9.4,''=TY'',F9.4,''=TZ'', 
     1             F9.4,''=RX'',F9.4,''=RY'',F9.4,''=RZ'')') 
     2(FREQ(I),I=NVIB+1,NVAR) 
      WRITE(JOUT,'(//10X,''FORCE CONSTANTS IN MILLIDYNES/ANGSTROM'' 
     1,'' (= 10**5 DYNES/CM)'',/)') 
      WRITE(JOUT,'(8F10.5)')(FREQ(I),I=1,NVIB) 
C CONVERT TO WEIGHTED FMAT 
      WRITE(JOUT,'(//10X,'' ASSOCIATED EIGENVECTORS'')') 
      I=-NVAR 
      CALL MATOUT(CNORML,FREQ,NVIB,I,NVAR) 
      CALL FREQCY(FMATRX,FREQ,CNORML,REDMAS) 
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
      DO 140 I=1,NVAR 
  140 SUM=SUM+FREQ(I) 
      SUM=SUM*CONST 
      WRITE(JOUT,'(//10X,'' ZERO POINT ENERGY'' 
     1, F12.3,'' KILOCALORIES PER MOLE'')')SUM 
      SUMM=0.D0 
      DO 180 I=1,NVAR 
         DO 150 K=1,3 
  150    GRAD(K)=0.D0 
         DO 170 K=1,3 
            SUM=0.D0 
            DO 160 J=1,NVAR 
  160       SUM=SUM+CNORML(J+(I-1)*NVAR)*DELDIP(K,J) 
            SUMM=SUMM+ABS(SUM) 
  170    TRDIP(K,I)=SUM 
  180 CONTINUE 
      DIPOK  =  (SUMM.GT.0.1D0) 
      WRITE(JOUT,'(//3X,'' THE LAST'',I2,'' VIBRATIONS ARE THE'', 
     1'' TRANSLATION AND ROTATION MODES'')')NVAR-NVIB 
      WRITE(JOUT,'(3X,'' THE FIRST THREE OF THESE BEING TRANSLATIONS'', 
     1'' IN X, Y, AND Z, RESPECTIVELY'')') 
      IF(DIPOK) THEN 
         WRITE(JOUT,'(//10X,'' FREQUENCIES, REDUCED MASSES AND '', 
     1''VIBRATIONAL DIPOLES''/)') 
      ELSE 
         WRITE(JOUT,'(//10X,''      FREQUENCIES AND REDUCED MASSES ''/)' 
     1) 
      ENDIF 
      NTO6=NVAR/6 
      NREM6=NVAR-NTO6*6 
      IINC1=-5 
      IF (NTO6.LT.1) GO TO 200 
      DO 190 I=1,NTO6 
         WRITE (JOUT,'(/)') 
         IINC1=IINC1+6 
         IINC2=IINC1+5 
         WRITE (JOUT,'(3X,''I'',10I10)') (J,J=IINC1,IINC2) 
         WRITE (JOUT,'('' FREQ(I)'',6F10.4,/)')(FREQ(J),J=IINC1,IINC2) 
         WRITE (JOUT,'('' MASS(I)'',6F10.5,/)')(REDMAS(J),J=IINC1,IINC2) 
         IF(DIPOK) THEN 
            WRITE (JOUT,'('' DIPX(I)'',6F10.5)') (TRDIP(1,J),J=IINC1, 
     1IINC2) 
            WRITE (JOUT,'('' DIPY(I)'',6F10.5)') (TRDIP(2,J),J=IINC1, 
     1IINC2) 
            WRITE (JOUT,'('' DIPZ(I)'',6F10.5,/)') (TRDIP(3,J),J=IINC1, 
     1IINC2) 
            WRITE (JOUT,'('' DIPT(I)'',6F10.5)') 
     1   (SQRT(TRDIP(1,J)**2+TRDIP(2,J)**2+TRDIP(3,J)**2) 
     2   ,J=IINC1,IINC2) 
         ENDIF 
  190 CONTINUE 
  200 CONTINUE 
      IF (NREM6.LT.1) GO TO 210 
      WRITE (JOUT,'(/)') 
      IINC1=IINC1+6 
      IINC2=IINC1+(NREM6-1) 
      WRITE (JOUT,'(3X,''I'',10I10)') (J,J=IINC1,IINC2) 
      WRITE (JOUT,'('' FREQ(I)'',6F10.4)') (FREQ(J),J=IINC1,IINC2) 
      WRITE (JOUT,'(/,'' MASS(I)'',6F10.5)') (REDMAS(J),J=IINC1,IINC2) 
      IF(DIPOK) THEN 
         WRITE (JOUT,'(/,'' DIPX(I)'',6F10.5)') (TRDIP(1,J),J=IINC1, 
     1IINC2) 
         WRITE (JOUT,'('' DIPY(I)'',6F10.5)') (TRDIP(2,J),J=IINC1,IINC2) 
         WRITE (JOUT,'('' DIPZ(I)'',6F10.5)') (TRDIP(3,J),J=IINC1,IINC2) 
         WRITE (JOUT,'(/,'' DIPT(I)'',6F10.5)') 
     1   (SQRT(TRDIP(1,J)**2+TRDIP(2,J)**2+TRDIP(3,J)**2) 
     2   ,J=IINC1,IINC2) 
      ENDIF 
  210 CONTINUE 
      WRITE(JOUT,'(//10X,'' NORMAL VECTORS'')') 
      I=-NVAR 
      CALL MATOUT(CNORML,FREQ,NVAR,I,NVAR) 
      CALL ANAVIB(COORD,FREQ,NVAR,CNORML,FMATRX) 
      IF(ITHERM.NE.0) THEN                                              DJG0995
         CALL GMETRY(GEO,COORD) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         IF(IROT.NE.0) THEN                                             DJG0995
            SYM=IIROT                                                   DJG0995
         ELSE 
            SYM=1 
         ENDIF 
         LINEAR=(ABS(A*B*C) .LT. 1.D-10) 
C 
C   "I" IS GOING TO MARK THE BEGINNING OF THE GENUINE VIBRATIONS. 
C 
         IF(ITRANS.NE.0)THEN                                            DJG0995
            I=2 
            J=NVIB-1 
         ELSE 
            I=1 
            J=NVIB 
         ENDIF 
         WRITE(JOUT,305) J                                              DJG0295
 305     FORMAT(/,1X,'THERMODYNAMIC CALCULATIONS CALCULATED WITH ',I4,  DJG0295
     1         'VIBRATIONS')                                            DJG0295
         CALL THERMO(A,B,C,LINEAR,SYM,WTMOL,FREQ(I),J) 
      ENDIF 
      CALL PORCPU (TFLY)                                                GL0492
      WRITE(JOUT,'('' COMPUTATION TIME '',F10.2,'' SECONDS'')') 
     .         TFLY-TIME0 
      RETURN 
      END 
