      SUBROUTINE FLEPO(XPARAM,NVAR,FUNCT1) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'                                                  DJG0995
      INCLUDE 'FFILES.i'                                                GDH1095
      DIMENSION XPARAM(*) 
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(3*NUMATM+4),             3GL3092
     1                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /NUMSCF/ NSCF,FROZEN 
      COMMON /LAST  / LAST 
      COMMON /REPATH/ REACT(100),LATOM,LPARAM                           03GCL93
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM 
      COMMON /MESAGE/ IFLEP,ISCF                                        DJG0995
C     COMMON /TIME  / TIME0 
      COMMON /TIMECM  / TIME0                                           3GL3092
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCR / HESINV(MAXHES),XVAR(MAXPAR),GVAR(MAXPAR),        3GL3092
     1                 XD(MAXPAR),GD(MAXPAR),GLAST(MAXPAR),             3GL3092
     2                 XLAST(MAXPAR),GG(MAXPAR),PVECT(MAXPAR),          3GL3092
     3                 RDUMY(MAXPAR*(3*MAXPAR+NCHAIN+8) + NCHAIN + 29)  3GL3092
C     COMMON /OPTIM / IMP,IMP0,LEC,IPRT,HESINV(MAXHES),XVAR(MAXPAR) 
C    .               ,GVAR(MAXPAR),XD(MAXPAR),GD(MAXPAR),GLAST(MAXPAR) 
C    .               ,XLAST(MAXPAR),GG(MAXPAR),PVECT(MAXPAR) 
      COMMON /PRECI / SCFCRT,SCFTOL,DUM(9),KDUM(MAXPAR) 
      COMMON /GEOVAR/ DUMY(MAXPAR),NVAR1,LOC(2,MAXPAR),IDUMY            DJG0295
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      CHARACTER SPACE*1, CHDOT*1, ZERO*1, NINE*1, CH*1 
C 
C     * 
C     THIS SUBROUTINE ATTEMPTS TO MINIMIZE A REAL-VALUED FUNCTION OF 
C     THE N-COMPONENT REAL VECTOR XPARAM ACCORDING TO THE 
C     DAVIDON-FLETCHER-POWELL ALGORITHM (COMPUTER JOURNAL, VOL. 6, 
C     P. 163).  THE USER MUST SUPPLY THE SUBROUTINE 
C     COMPFG(XPARAM,FUNCT,FAIL,GRAD,LGRAD) 
C     WHICH COMPUTES FUNCTION VALUES  FUNCT AT GIVEN VALUES FOR THE 
C     VARIABLES XPARAM, AND THE GRADIENT GRAD IF LGRAD=.TRUE. 
C     THE .TRUE. VALUE IS RETURNED IN FAIL IF SCF NOT CONVERGED. 
C     THE MINIMIZATION PROCEEDS BY A SEQUENCE OF ONE-DIMENSIONAL 
C     MINIMIZATIONS.  THESE ARE CARRIED OUT WITHOUT GRADIENT COMPUTATION 
C     BY THE SUBROUTINE LINMIN, WHICH SOLVES THE SUBPROBLEM OF 
C     MINIMIZING THE FUNCTION FUNCT ALONG THE LINE XPARAM+ALPHA*PVECT, 
C     WHERE XPARAM 
C     IS THE VECTOR OF CURRENT VARIABLE VALUES,  ALPHA IS A SCALAR 
C     VARIABLE, AND  PVECT  IS A SEARCH-DIRECTION VECTOR PROVIDED BY THE 
C     DAVIDON-FLETCHER-POWELL ALGORITHM.  EACH ITERATION STEP CARRIED 
C     OUT BY FLEPO PROCEEDS BY LETTING LINMIN FIND A VALUE FOR ALPHA 
C     WHICH MINIMIZES  FUNCT  ALONG  XPARAM+ALPHA*PVECT, BY 
C     UPDATING THE VECTOR  XPARAM  BY THE AMOUNT ALPHA*PVECT, AND 
C     FINALLY BY GENERATING A NEW VECTOR  PVECT.  UNDER 
C     CERTAIN RESTRICTIONS (POWELL, J.INST.MATHS.APPLICS.(1971), 
C     V.7,21-36)  A SEQUENCE OF FUNCT VALUES CONVERGING TO SOME 
C     LOCAL MINIMUM VALUE AND A SEQUENCE OF 
C     XPARAM VECTORS CONVERGING TO THE CORRESPONDING MINIMUM POINT 
C     ARE PRODUCED. 
C
C                          CONVERGENCE TEST 
C 
C     GRADIENT COMPONENT TEST :                                         DJG0495
C        THE LARGEST COMPONENT OF THE GRADIENT IS LESS THAN TOLERG.     DJG0495
C                                                                       DJG0495
C         "GRADIENT COMPONENT TEST PASSED : GEOMETRY CONVERGED"         DJG0495
C
C 
C     AN UNSUCCESSFUL TERMINATION WILL TAKE PLACE AFTER 
C     COMPFG HAS BEEN CALLED MORE TIMES THAN THE USER-SUPPLIED VALUE 
C     OF MAXEND.  IN THIS CASE THE COMMENT 
C 
C                     "*** TERMINATION FROM TOO MANY COUNTS ***" 
C 
C     WILL BE PRINTED, AND FUNCT AND XPARAM WILL CONTAIN THE LAST 
C     FUNCTION VALUE CUM VARIABLE VALUES REACHED. 
C 
C     SIMILAR UNSUCCESSFUL TERMINATIONS WILL TAKE PLACE IF THE COSINE OF 
C     THE SEARCH DIRECTION TO GRADIENT VECTOR IS LESS THAN RST ON TWO 
C     CONSECUTIVE ITERATIONS. 
C 
C     THE DAVIDON-FLETCHER-POWELL ALGORITHM CHOOSES SEARCH DIRECTIONS 
C     ON THE BASIS OF LOCAL PROPERTIES OF THE FUNCTION.  A MATRIX  H, 
C     WHICH IN FLEPO IS PRESET WITH THE IDENTITY, IS MAINTAINED AND 
C     UPDATED AT EACH ITERATION STEP.  THE MATRIX DESCRIBES A LOCAL 
C     METRIC ON THE SURFACE OF FUNCTION VALUES ABOVE THE POINT XPARAM. 
C     THE SEARCH-DIRECTION VECTOR  PVECT  IS SIMPLY A TRANSFORMATION 
C     OF THE GRADIENT  GRAD  BY THE MATRIX H.  THE USER MAY THROW OUT  H 
C     AFTER EACH  NRST ITERATION STEPS (RESTARTING WITH THE IDENTITY) OR 
C     WHENEVER THE COSINE OF THE ANGLE BETWEEN  GRAD  AND PVECT BECOMES 
C     LESS THAN RST. THIS CAN BE SUPPRESSED ENTIRELY IF NRST .GT. MAXEND 
C     AND RST .LT. 0.0.   RESTARTING IS DISCUSSED MARGINALLY IN THE 
C     PAPER BY FLETCHER AND POWELL, BUT THERE ARE NO GOOD RULES ABOUT 
C     WHEN OR WHETHER THIS SHOULD BE DONE FOR ANY GIVEN FUNCTION. 
C 
      DIMENSION MDFP(9),XDFP(9) 
      LOGICAL OKF, OKC, PRINT,  TIME, RESTRT, MINPRT, SADDLE, GEOOK 
     1        ,RESET, FAIL, LGRAD, FROZEN, FIRST, BFGS                  DJG0995
     2        ,RSTFIL,HESSET                                            DJG0495 
C      LOGICAL FULSCF                                                    DJG0495
      EQUIVALENCE (MDFP(1),JCYC  ),(MDFP(2),JNRST),(MDFP(3),NCOUNT), 
     1            (MDFP(4),LNSTOP),(XDFP(1),ALPHA),(XDFP(2),COS   ), 
     2            (XDFP(3),PNORM ),(XDFP(4),YEAD ),(XDFP(5),DEL   ), 
     3            (XDFP(6),FREPF ),(XDFP(7),CYCMX),(XDFP(8),TOTIME) 
       SAVE
      DATA SPACE,CHDOT,ZERO,NINE /' ','.','0','9'/
      IF (ICONTR(23).EQ.1) THEN                                         GDH0195
         GEOOK=IGEOOK.NE.0                                              DJG0995
        FFNCT2=0.D0                                                     GDH1195
        FFNCT1=0.D0                                                     GDH1195
C                                                                       DJG0495
C      VARIABLES CHECK                                                  DJG0495
C                                                                       DJG0495
         IF (NUMAT.GT.2) THEN                                           DJG0495
           IF(NVAR.GT.(3*NUMAT-6)) THEN                                 DJG0495
             IF ((.NOT.GEOOK).AND.(NVAR.NE.(3*NUMAT))) THEN             DJG0495
               WRITE(JOUT,13)                                           DJG0495
13             FORMAT(/,5X,'REDUNDANT VARIABLES.  OPTIMIZATION ',       DJG0495
     1               'NOT RECOMMENDED',/,5X,'SPECIFY GEO-OK ',          DJG0495
     2               'IF YOU WANT TO CONTINUE',/)                       DJG0495
      ISTOP=1                                                           GDH1095
      IWHERE=65                                                         GDH1095
      RETURN                                                            GDH1095
             ENDIF                                                      DJG0495
           ELSE IF(NVAR.EQ.(3*NUMAT)) THEN                              DJG0495
             IF((IXYZ.EQ.0).AND.(.NOT.GEOOK)) THEN                      DJG0995
               WRITE(JOUT,14)                                           DJG0495
14             FORMAT(/,5X,'EXACTLY 3*N VARIABLES.',/,5X,               DJG0495
     1           'SPECIFY XYZ IF YOU ARE USING CARTESIAN COORDINATES',/,DJG0495
     2           5X,'IF YOU USE INTERNAL COORDINATES, YOU CAN SPECIFY ',DJG0495
     3           'GEO-OK,',/,5X,'BUT OPTIMIZATION IS NOT ',             DJG0495
     4           'RECOMMENDED',/)                                       DJG0495
      ISTOP=1                                                           GDH1095
      IWHERE=66                                                         GDH1095
      RETURN                                                            GDH1095
             ENDIF                                                      DJG0495
           ENDIF                                                        DJG0495
         ENDIF                                                          DJG0495
C   FLAG TO CHECK IF RESTART FILES HAVE BEEN WRITTEN                    DJG0495
         RSTFIL=.FALSE.                                                 DJG0495
C   THE FOLLOWING CONSTANTS ARE USED WITH THE "KICK" FUNCTION           DJG0495
         NUMVAR=0                                                       DJG0295
         KICK=1                                                         DJG0295
         KNUM=0                                                         DJG0295
         NFREDM=1                                                       DJG0295
         IF (IKICK.GT.1) KICK=IIKICK                                    DJG0995
C   END OF KICK CONSTANTS                                               DJG0495
         IF(IOLDEN.NE.0) THEN                                           DJG0995
          OPEN(JDEN,                                                    GDH1095
     $         STATUS='UNKNOWN',FORM='UNFORMATTED')
          REWIND JDEN                                                   GDH1095
         ENDIF
C   RST: IF COSINE OF ANGLE BETWEEN PVECT AND XPARAM<RST, RESET HESSIAN DJG0495
C   MAXEND: # OF CALLS TO COMPFG ALLOWED BEFORE DYING                   DJG0495
C   NRST: AFTER NRST CYCLES, RESET HESSIAN                              DJG0495
C   SFACT: IF SFACT*CYCMX TIME > TIME LEFT, DON'T START NEW CYCLE       DJG0495
C   TOLERG: THE MAXIMUM VALUE ALLOWED FOR A COMPONENT OF THE GRADIENT.  DJG0495
C   JRETRN: USED TO RESET HESSIAN BEFORE LINE MIN. ERROR                DJG0495
         TOLERG=0.45D0                                                  DJG0495
         IF (IGCOMP.NE.0) TOLERG=FIGCOM                                 DJG0995
         RST   = 0.05D0 
         MAXEND= 9999 
         IF (IRECAL.NE.0) THEN                                          DJG0995
           NRST=IIRECA                                                  DJG0995
         ELSE                                                           DJG0495
           NRST=MAXEND+1                                                DJG0495
         ENDIF                                                          DJG0495
         SFACT=1.5D0                                                    GCL0393
         JRETRN=0                                                       DJG0495
C   THE FOLLOWING CONSTANTS ARE USED IN CALCULATING THE HESSIAN         DJG0495
         TDEL  = 6.D0 
         PMSTE = 0.1D0 
         DELL  = 0.01D0 
         DEL=DELL 
C   TDP: TIME BETWEEN WRITING RESTART FILES                             DJG0495
         IF (ITDUMP.GT.0) THEN                                          DJG0995
             TDP=FITDUM                                                 DJG0995
         ELSE                                                           GDH1194
             TDP=1800                                                   GDH1194
         ENDIF                                                          GDH1194
         OKF    = .TRUE. 
         RESTRT = IRESTA.EQ.1                                           DJG0496
         SADDLE = ISADDL.NE.0                                           DJG0995
         MINPRT = IDEBUG.NE.0.AND.SADDLE                                DJG0995
         TIME   = ITIMES.NE.0                                           DJG0995
         BFGS   = IDFP.EQ.0                                             DJG0995
         PRINT  = (IFLEPO.NE.0).OR.(IIPRIN.GE.1)                        DJG0995
         TUSED=0.D0                                                     GDH1294
         FIRST=.FALSE.                                                  GDH1294
         TLEFT=TDEF                                                     GL0492
         CCN=NVAR 
         ROOTV=SQRT(CCN+1.D-5) 
C   FLAG TO SEE IF HESSIAN WAS RESET THAT TIME                          DJG0495
         HESSET=.FALSE.                                                 DJG0495
C   PERFORM LINMIN WITH GRADIENT COMPUTATION                            DJG0495
         IF(ITLIMI.NE.0) TLEFT=FITLIM                                   DJG0995 
         CALL PORCPU (TX2)                                              GL0492
         TLEFT=TLEFT-TX2+TIME0 
         IF (I1SCF.EQ.0) WRITE(JOUT,37)                                 DJG0196
 37      FORMAT(/,8X,'CYC : CYCLES   T : TIME (S)  ',                   DJG0495
     1         'GEO : CHANGE IN GEOMETRY'                               DJG0495
     2         ,' (RU) ',/,8X,'GCOMP : LARGEST COMPONENT OF GRADIENT ', DJG0495
     3         '(KCAL/RU)',/,8X,'HEAT : TOTAL ENERGY (KCAL/MOL)',/,     DJG0495
     4         8X,'1 RU = 1 ANGSTROM OR 1 RADIAN',/)                    DJG0495
      ENDIF
C 
C   THE FOLLOWING CONSTANTS SHOULD BE SET TO SOME ARBITARY LARGE VALUE. 
C 
      YEAD  = 1.D15 
      FREPF = 1.D15 
C 
C     AND FINALLY, THE FOLLOWING CONSTANTS ARE CALCULATED. 
C 
C   IHDIM: THE DIMENSION OF THE PACKED HESSIAN                          DJG0495
      IHDIM=(NVAR*(NVAR+1))/2 
      CNCADD=1.0D00/ROOTV 
      IF (CNCADD.GT.0.15D00) CNCADD=0.15D00 
C 
C     FIRST, WE INITIALISE THE VARIABLES. 
C 
C   JCYC: # OF CYCLES                                                   DJG0495
C   ALPHA: LENGTH OF STEP TO TAKE IN DIRECTION PVECT                    DJG0495
C   PNORM: NORM OF THE VECTOR PVECT                                     DJG0495
C   JNRST: # OF CYCLES SINCE LAST RESET OF HESSIAN                      DJG0495
C   CYCMX: LENGTH OF LONGEST CYCLE                                      DJG0495
C   COS: COSINE BETWEEN PVECT AND XPARAM                                DJG0495
C   TOTIME: TOTAL TIME IN ROUTINE                                       DJG0495
C   NCOUNT: # OF TIMES COMPFG IS CALLED                                 DJG0495
C   LIMCYC: LIMIT ON NUMBER OF CYCLES                                   DJG0495
C
      LIMCYC=100                                                        DJG0495
      IF (ICYCLE.NE.0) LIMCYC=IICYCL                                    DJG0995
      JCYC=0 
      LNSTOP=1 
      ALPHA = 1.0D00 
      PNORM=1.0D00 
      JNRST=0 
      CYCMX=0.D0 
      COS=0.0D00 
      TOTIME=0.D0 
      NCOUNT=1 
      FAIL=.FALSE. 
      IF( SADDLE) THEN 
* 
*   WE DON'T NEED HIGH PRECISION DURING A SADDLE-POINT CALCULATION. 
* 
         IF(NVAR.GT.0)GNORM=SQRT(DOT(GRAD,GRAD,NVAR))-3.D0 
         IF(GNORM.GT.10.D0)GNORM =10.D0 
         IF(GNORM.GT.1.D0) TOLERG=TOLERG*GNORM 
         WRITE(JOUT,'('' GRADIENT CRITERION IN FLEPO ='',F12.5)')TOLERG 
      ENDIF 
      IF (RESTRT .AND.ICONTR(23).EQ.1) THEN                             DJG0295
         MDFP(9)=0 
         CALL DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP) 
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         WRITE(JOUT,'(//10X,''TOTAL TIME USED SO FAR:'', 
     1    F13.2,'' SECONDS'')')TOTIME 
         IF(I1SCF.NE.0) THEN                                            DJG0995
            LGRAD= IGRADI.NE.0                                          DJG0995
            CALL COMPFG (XPARAM,FUNCT1,FAIL,GRAD,LGRAD) 
            IFLEP=1                                                     DJG0995
            RETURN 
         ENDIF 
      ELSE 
         TOTIME=0.D0 
C 
C CALCULATE THE VALUE OF THE FUNCTION -> FUNCT1, AND GRADIENTS -> GRAD. 
C NORMAL SET-UP OF FUNCT1 AND GRAD, DONE ONCE ONLY. 
C 
         LGRAD=I1SCF.EQ.0.OR.IGRADI.NE.0                                DJG0995
         CALL COMPFG (XPARAM,FUNCT1,FAIL,GRAD,LGRAD) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         WRITE(JOUT,33)                                                 DJG0495
 33      FORMAT(/,/)                                                    DJG0495
C GA-FRIENDLY : REPLACE THIS STOP WITH RETURN IF USING AMSOL AS SUBROUTINE
         IF(FAIL) THEN                                                  GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=66                                                         GDH1095
      RETURN                                                            GDH1095
         ENDIF                                                          GDH1095
         CALL SCOPY (NVAR,GRAD,1,GD,1) 
      ENDIF 
      IF (NVAR.NE.0) THEN 
         GNORM=SQRT(DOT(GRAD,GRAD,NVAR)) 
         CALL SCOPY (NVAR,GRAD,1,GLAST,1) 
         GLNORM=GNORM 
      ENDIF 
      IFLEP=1                                                           DJG0995
      IF(I1SCF.NE.0) RETURN                                             DJG0995
      IFLEP=2                                                           DJG0995
      CALL CONVCK(TOLERG,FFNCT1,FFNCT2,FUNCT1,NVAR,ICONFG)              GDH1195
      IF(ICONFG.EQ.0.OR.NVAR.EQ.0) THEN                                 DJG0495
         IF(RESTRT) 
     1     CALL COMPFG (XPARAM,FUNCT1,FAIL,GRAD,.TRUE.) 
         RETURN 
      ENDIF 
      CALL PORCPU (TX1)                                                 GL0492
      TLEFT=TLEFT-TX1+TX2 
C
C     * 
C     START OF EACH ITERATION CYCLE ... 
C     * 
C 
      RESET=.FALSE. 
      GOTO 60 
   40 CONTINUE 
      IF(COS .LT. RST) THEN 
         DO 50 I=1,NVAR 
   50    GD(I)=0.5D0 
      ENDIF 
   60 CONTINUE 
      GNORM=SQRT(DOT(GRAD,GRAD,NVAR)) 
      JCYC=JCYC+1 
      JNRST=JNRST+1 
      IF(RESTRT) THEN                                                   CC1-92
        RESTRT=.FALSE.                                                  CC1-92
        GOTO 70                                                         CC1-92
      ENDIF                                                             CC1-92
      IF (LNSTOP.NE.1 .AND. COS.GT.RST .AND. JNRST.LT.NRST) GOTO 160 
C 
C     * 
C     RESTART SECTION 
C     * 
C 
   70 CONTINUE 
   89 CONTINUE                                                          GL0492
      RESET=.TRUE. 
      DO 80 I=1,NVAR 
         XD(I)=XPARAM(I)-SIGN(DEL,GRAD(I)) 
   80 CONTINUE 
C 
C THIS CALL OF COMPFG IS USED TO CALCULATE THE SECOND-ORDER MATRIX IN H 
C IF THE NEW POINT HAPPENS TO IMPROVE THE RESULT, THEN IT IS KEPT. 
C OTHERWISE IT IS SCRAPPED, BUT STILL THE SECOND-ORDER MATRIX IS O.K. 
C 
      CALL COMPFG (XD,FUNCT2,FAIL,GD,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF(.NOT. GEOOK .AND. SQRT(DOT(GD,GD,NVAR))/GNORM.GT.10.D0         GCL0393 
     1.AND.GNORM.GT.20.D0.AND.JCYC.GT.2)THEN                            GCL0393
C 
C  THE GEOMETRY IS BADLY SPECIFIED IN THAT MINOR CHANGES IN INTERNAL 
C  COORDINATES LEAD TO LARGE CHANGES IN CARTESIAN COORDINATES, AND THESE 
C  LARGE CHANGES ARE BETWEEN PAIRS OF ATOMS THAT ARE CHEMICALLY BONDED 
C  TOGETHER. 
         WRITE(JOUT,'('' GRADIENTS OF OLD GEOMETRY, GNORM='',F13.6)') 
     .              GNORM 
         WRITE(JOUT,'(6F12.6)')(GRAD(I),I=1,NVAR) 
         GDNORM=SQRT(DOT(GD,GD,NVAR)) 
         WRITE(JOUT,'('' GRADIENTS OF NEW GEOMETRY, GNORM='',F13.6)') 
     .              GDNORM 
         WRITE(JOUT,'(6F12.6)')(GD(I),I=1,NVAR) 
C
         DEL = DEL/10.D0                                                GL0492
         THRESH = 5.D-5                                                 GL0492
         IF (DEL .GE. THRESH) THEN                                      GL0492
             GO TO 89                                                   GL0492
         ELSE                                                           GL0492
C
         WRITE(JOUT,'(///20X,''CALCULATION ABANDONED AT THIS POINT]'')') 
         WRITE(JOUT,'(//10X,'' SMALL CHANGES IN INTERNAL COORDINATES ARE 
     .   '',/10X,'' CAUSING A LARGE CHANGE IN THE DISTANCE BETWEEN'',/ 
     .   10X,'' CHEMICALLY-BOUND ATOMS. THE GEOMETRY OPTIMISATION'',/ 
     .   10X,'' PROCEDURE WOULD LIKELY PRODUCE INCORRECT RESULTS'')') 
         CALL GEOUT 
      ISTOP=1                                                           GDH1095
      IWHERE=67                                                         GDH1095
      RETURN                                                            GDH1095
C
         ENDIF                                                          GL0492
C
      ENDIF 
      IF (ICONTR(23).EQ.1) THEN                                         DJG0495
         WRITE(JOUT,83)                                                 DJG0495
   83    FORMAT(27X,'HESSIAN IS THE UNIT MATRIX',/)                     DJG0495
         ICONTR(23)=2                                                   DJG0495
      ELSE                                                              DJG0495
         WRITE(JOUT,85)                                                 DJG0495
   85    FORMAT(/,29X,'WRITING RESTART FILES',/)                        DJG0495
         MDFP(9)=2                                                      DJG0495
         RSTFIL=.TRUE.                                                  DJG0495
         CALL DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP)           DJG0495
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         WRITE(JOUT,87)                                                 DJG0495
   87    FORMAT(29X,'HESSIAN IS BEING RESET',/)                         DJG0495
         HESSET=.TRUE.                                                  DJG0495
      ENDIF                                                             DJG0495
   88 CONTINUE                                                          DJG0495
      NCOUNT=NCOUNT+1 
      DO 90 I=1,IHDIM 
   90 HESINV(I)=0.0D00 
      II=0 
      DO 120 I=1,NVAR 
         II=II+I 
         GGGGG=GRAD(I)-GD(I) 
         IF (ABS(GGGGG).LT.1.D-12) GO TO 100 
         GGD=ABS(GRAD(I)) 
         IF (FUNCT2.LT.FUNCT1) GGD=ABS(GD(I)) 
         HESINV(II)=SIGN(DEL,GRAD(I))/GGGGG 
         IF (HESINV(II).LT.0.0D00.AND.GGD.LT.1.D-12) GO TO 100 
         IF (HESINV(II).LT.0.0D00) HESINV(II)=(TDEL*DEL)/GGD 
         GO TO 110 
  100    HESINV(II)=0.01D00 
  110    CONTINUE 
         IF (GGD.LT.1.D-12) GGD=1.D-12 
         PMSTEP=ABS(PMSTE/GGD) 
         IF (HESINV(II).GT.PMSTEP) HESINV(II)=PMSTEP 
  120 CONTINUE 
      JNRST=0 
      IF(FUNCT2 .GE. FUNCT1) THEN 
         IF(PRINT)WRITE (JOUT,130) FUNCT1,FUNCT2 
  130    FORMAT (' FUNCTION VALUE=',F13.7, 
     1           '  WILL NOT BE REPLACED BY VALUE=',F13.7,/10X, 
     2           'CALCULATED BY RESTART PROCEDURE',/) 
      ELSE 
         IF( PRINT ) WRITE (JOUT,140) FUNCT1,FUNCT2 
  140    FORMAT (' FUNCTION VALUE=',F13.7, 
     1           ' IS BEING REPLACED BY VALUE=',F13.7,/,10X, 
     2           ' FOUND IN RESTART PROCEDURE',/,6X,'THE CORRESPONDING' 
     3           ,' X VALUES AND GRADIENTS ARE ALSO BEING REPLACED',/) 
         FUNCT1=FUNCT2 
         CALL SCOPY (NVAR,XD,1,XPARAM,1) 
         CALL SCOPY (NVAR,GD,1,GRAD  ,1) 
         GNORM=SQRT(DOT(GRAD,GRAD,NVAR)) 
      ENDIF 
      GO TO 200 
C 
C     * 
C     UPDATE VARIABLE-METRIC MATRIX 
C     * 
C 
  160 DO 170 I=1,NVAR 
      XVAR(I)=XPARAM(I)-XLAST(I) 
  170 GVAR(I)=GRAD(I)-GLAST(I) 
      CALL SUPDOT(GG,HESINV,GVAR,NVAR,1) 
      YHY=1.D0/DOT(GG,GVAR,NVAR)                                         IR0295
      SY =1.D0/DOT(XVAR,GVAR,NVAR)                                       IR0295
      K=0 
      DO 180 I=1,NVAR 
      XVARI=XVAR(I)*SY                                                   IR0295
      GGI=GG(I)*YHY                                                      IR0295
      DO 180 J=1,I 
      K=K+1 
  180 HESINV(K)=HESINV(K)+XVAR(J)*XVARI-GG(J)*GGI 
      IF (BFGS) THEN                                                     IR0295
         K=0                                                             IR0295
         DO 190 I=1,NVAR                                                 IR0295
         XVAR(I)=XVAR(I)*SY-GG(I)*YHY                                    IR0295
         XVARI=XVAR(I)/YHY                                               IR0295
         DO 190 J=1,I                                                    IR0295
         K=K+1                                                           IR0295
  190    HESINV(K)=HESINV(K)+XVAR(J)*XVARI                               IR0295
      ENDIF                                                              IR0295
C 
C     * 
C     ESTABLISH NEW SEARCH DIRECTION 
C     * 
  200 PNLAST=PNORM 
      CALL SUPDOT(PVECT,HESINV,GRAD,NVAR,1) 
      PNORM=SQRT(DOT(PVECT,PVECT,NVAR)) 
      DOTT=-DOT(PVECT,GRAD,NVAR) 
      DO 210 I=1,NVAR 
  210 PVECT(I)=-PVECT(I) 
      COS=-DOTT/(PNORM*GNORM) 
      IF (JNRST.EQ.0) GO TO 250 
      IF (COS.LE.CNCADD.AND.YEAD.GT.1.0D00) GO TO 230 
      IF (COS.LE.RST) GO TO 230 
      GO TO 250 
  230 PNORM=PNLAST 
      IF( PRINT )WRITE (JOUT,240) COS 
  240 FORMAT (//,5X, 'SINCE COS=',F9.3,5X,'THE PROGRAM WILL GO TO RE', 
     1'START SECTION',/) 
      GO TO 70 
  250 CONTINUE 
      IF ( PRINT ) WRITE (JOUT,260) JCYC,FUNCT1 
  260 FORMAT (1H , 'AT THE BEGINNING OF CYCLE',I5, '  THE FUNCTION VA 
     1LUE IS ',F13.6/, '  THE CURRENT POINT IS ...') 
      IF(PRINT)WRITE (JOUT,270) GNORM,COS 
  270 FORMAT ( '  GRADIENT NORM = ',F10.4/,'  ANGLE COSINE =',F10.4) 
      NTO6=NVAR/6 
      NREM6=NVAR-NTO6*6 
      IINC1=-5 
      IF (.NOT.HESSET) THEN                                             DJG0495
         WRITE (JOUT,275) JCYC-1,TOTIME,TX,GNORM, FUNCT1                GDH1195
275      FORMAT(' CYC:',I3,'    T:',F8.1,'    GEO: ',F8.6,'    GCOMP:'  DJG0495
     1          ,F8.3,'   HEAT:',F12.6)                                 DJG0495
      ELSE                                                              DJG0495
         JCYC=JCYC-1                                                    DJG0495
         HESSET=.FALSE.                                                 DJG0495
      ENDIF                                                             DJG0495
      IF (NTO6.LT.1.OR. .NOT. PRINT) GO TO 330 
      DO 320 I=1,NTO6 
         WRITE (JOUT,'(/)') 
         IINC1=IINC1+6 
         IINC2=IINC1+5 
         WRITE (JOUT,280) (J,J=IINC1,IINC2) 
         WRITE (JOUT,290) (XPARAM(J),J=IINC1,IINC2) 
         WRITE (JOUT,300) (GRAD(J),J=IINC1,IINC2) 
         WRITE (JOUT,310) (PVECT(J),J=IINC1,IINC2) 
  280    FORMAT (1H ,3X,  1HI,9X,I3,9(8X,I3)) 
  290    FORMAT (1H ,1X, 'XPARAM(I)',1X,F9.4,2X,9(F9.4,2X)) 
  300    FORMAT (1H ,1X, 'GRAD  (I)',F10.4,1X,9(F10.4,1X)) 
  310    FORMAT (1H ,1X, 'PVECT (I)',1X,F9.4,2X,9(F9.4,2X)) 
  320 CONTINUE 
  330 CONTINUE 
      IF (NREM6.LT.1.OR. .NOT. PRINT) GO TO 340 
      WRITE (JOUT,'(/)') 
      IINC1=IINC1+6 
      IINC2=IINC1+(NREM6-1) 
      WRITE (JOUT,280) (J,J=IINC1,IINC2) 
      WRITE (JOUT,290) (XPARAM(J),J=IINC1,IINC2) 
      WRITE (JOUT,300) (GRAD(J),J=IINC1,IINC2) 
      WRITE (JOUT,310) (PVECT(J),J=IINC1,IINC2) 
  340 CONTINUE 
      FI=FUNCT1 
      LNSTOP=0 
      ALPHA=ALPHA*PNLAST/PNORM 
      COSINE=DOT(GRAD,GLAST,NVAR)/(GLNORM*GNORM) 
      CALL SCOPY (NVAR,GRAD,  1,GLAST,1) 
      CALL SCOPY (NVAR,XPARAM,1,XLAST,1) 
      GLNORM=GNORM 
      IF (JNRST.EQ.0) ALPHA=1.0D00 
      YEAD=ABS(ALPHA*DOTT) 
      IF(PRINT)WRITE (JOUT,360) YEAD 
  360 FORMAT (1H , 13H -ALPHA.P.G =,F18.6,/) 
C 
C  ONE-DIMENSIONAL SEARCH ALONG THE DIRECTION PVECT. 
C 
C                VARIOUS ATTEMPT TO SAVE TIME : 
C 1)  SCFTOL OVERWRITE THE STANDARD SCF CV CRITERION. 
C 2)  (CLOSED SHELLS WITHOUT C.I. ONLY) CHECK THE ANGLE BETWEEN TWO 
C     SUCCESSIVE GRADIENTS AND DECIDE IF FULL SCF CALCULATION ARE TO 
C     BE DONE. (THRESHOLD = 0.8) 
      SCFTOL=1.D0 
      IF (JCYC.LT.3) SCFTOL=MIN(GNORM,1.D1) 
C     FROZEN SCF OPTION REMOVED -  LINE SEARCH PERFORMED WITH GRADIENT  DJG0495
C     COMPUTED
C      FROZEN=COSINE.LT.0.8D0 
C      IF(FULSCF.OR..NOT.OKF) FROZEN=.FALSE. 
      SMVAL=FUNCT1 
      BETA =ALPHA 
  380 CALL LINMIN(XPARAM,ALPHA,PVECT,NVAR,FUNCT1,OKF,OKC) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      NCOUNT=NCOUNT+1 
      IF ( .NOT. OKF) THEN 
         LNSTOP = 1 
         IF(MINPRT)WRITE (JOUT,'(/,20X, ''NO POINT LOWER IN ENERGY ''
     1      ,''THAN THE STARTING POINT COULD BE FOUND '', 
     2      ''IN THE LINE MINIMIZATION'')') 
         FUNCT1=SMVAL 
         ALPHA=BETA 
         CALL SCOPY (NVAR,GLAST,1,GRAD  ,1) 
         CALL SCOPY (NVAR,XLAST,1,XPARAM,1) 
C                                                                       DJG0495
C         IF (FROZEN.OR.SCFTOL.GT.1.D0) THEN                            DJG0495
         IF (SCFTOL.GT.1.D0) THEN                                       DJG0495
            SCFTOL=1.D0 
            GO TO 380 
         ENDIF 
         IF (JNRST.EQ.0)THEN 
C
C     TO TRY AND AVOID LINE MINIMIZATION ERRORS, WE ARE GOING TO
C     KICK THE GEOMETRY BY INCREASING A BOND LENGTH BY 0.15 ANGSTROMS.
C     ONLY BOND LENGTHS THAT ARE OPTIMIZED ARE INCREASED.  THE KICK
C     CAN BE PERFORMED ANY NUMBER OF TIMES, THE DEFAULT IS ONE TIME,
C     BUT THIS CAN BE CONTROLLED BY THE KEYWORD KICK=x.  IF NO
C     BOND LENGTH (NFREDM = 1) IS BEING OPTIMIZED, THEN AN ANGLE
C     WILL BE KICKED BY 5 DEGREES (NFREDM=2).  IF NO ANGLE IS OPTIMIZED
C     THEN A DIHEDRAL WILL BE KICKED BY 10 DEGREES (NFREDM=3).
C     EACH TIME THE GEOMETRY IS KICKED, A NEW DEGREE OF FREEDOM IS
C     CHOSEN, STARTING WITH THE FIRST DISTINCT OPTIMIZING DEGREE OF
C     FREEDOM, THEN THE SECOND...
C
  385       IF (KNUM.LT.KICK) THEN                                      DJG0295
               KNUM=KNUM+1                                              DJG0295
  390          NUMVAR=NUMVAR+1                                          DJG0295
               IF (NUMVAR.GT.NVAR) THEN                                 DJG0295
                  IF (KNUM.EQ.1) THEN                                   DJG0295
                     NFREDM=NFREDM+1                                    DJG0295
                     NUMVAR=1                                           DJG0295
                  ELSE                                                  DJG0295
                     NUMVAR=1                                           DJG0295
                  ENDIF                                                 DJG0295
               ENDIF                                                    DJG0295
               IF (LOC(2,NUMVAR).NE.NFREDM) GOTO 390                    DJG0295
               IF (NFREDM.EQ.1) THEN                                    DJG0295
                  XPARAM(NUMVAR)=XPARAM(NUMVAR)+0.15D0                  DJG0295
               ELSE IF (NFREDM.EQ.2) THEN                               DJG0295
                  XPARAM(NUMVAR)=XPARAM(NUMVAR)+5.0D0                   DJG0295
               ELSE                                                     DJG0295
                  XPARAM(NUMVAR)=XPARAM(NUMVAR)+10.0D0                  DJG0295
               ENDIF                                                    DJG0295
               FAIL=.FALSE.                                             DJG0295
               WRITE(JOUT,396)                                          DJG0295
  396          FORMAT(/,1X,'A LINE MINIMISATION ERROR HAS OCCURRED.  ', DJG0295
     1           'IN AN ATTEMPT TO CIRCUMVENT THIS,',/,1X,'THE GEOMETRY'DJG0295
     2                ,' WILL BE PERTURBED AND ANOTHER ATTEMPT MADE',/) DJG0495
               CALL COMPFG (XPARAM,FUNCT,FAIL,GRAD,.TRUE.)              DJG0295
      IF (ISTOP.NE.0) RETURN                                            GDH1095
               IF (FAIL) THEN                                           DJG0295
C                                                                       DJG0295
C     SCF DIDN'T CONVERGE USING OLD DENSITY MATRIX, START AGAIN WITH NEWDJG0295
C                                                                       DJG0295
                  WRITE(JOUT,386)                                       DJG0295
  386             FORMAT('SCF CONVERGENCE FAILED USING THE PRE-KICK ',  DJG0295
     1              'DENSITY MATRIX.  TRYING AGAIN USING A ',/,         DJG0295
     2              'NEW DIAGONAL MATRIX.')                             DJG0295
                  FAIL=.FALSE.                                          DJG0295
                  CALL COMPFG (XPARAM,FUNCT,FAIL,GRAD,.TRUE.)           DJG0295
      IF (ISTOP.NE.0) RETURN                                            GDH1095
                  IF (FAIL) THEN                                        DJG0295
                     WRITE(JOUT,387)                                    DJG0295
  387                FORMAT('SCF CONVERGENCE FAILED USING THE DIAGONAL' DJG0295
     1                 ,' DENSITY MATRIX.  KICK HAS FAILED.  PLEASE ',/,DJG0295
     2                 'RESTART WITH A NEW GEOMETRY.')                  DJG0295
      ISTOP=1                                                           GDH1095
      IWHERE=68                                                         GDH1095
      RETURN                                                            GDH1095
                  ENDIF                                                 DJG0295
               ENDIF                                                    DJG0295
               CALL SUPDOT(PVECT,HESINV,GRAD,NVAR,1)
               DO 2010 I=1,NVAR
 2010          PVECT(I)=-PVECT(I)
               COS=-DOTT/(PNORM*GNORM)
               ALPHA=1.0D0
               CALL LINMIN(XPARAM,ALPHA,PVECT,NVAR,FUNCT,OKF,OKC)       DJG0295
      IF (ISTOP.NE.0) RETURN                                            GDH1095
            ELSE                                                        DJG0295
               WRITE (JOUT,400) 
  400          FORMAT (1H ,//,20X, 'SINCE COS WAS JUST RESET,THE SEARCH'
     1           ,' IS BEING ENDED') 
C 
C           FLEPO IS ENDING BADLY. THIS IS IMMEDIATELY BEFORE THE RETURN 
C 
               LAST=1 
               WRITE(JOUT,402)                                          DJG0495
  402          FORMAT(/,29X,'HESSIAN IS BEING RESET',/)                 DJG0495
               WRITE(JOUT,405)                                          DJG0495
405            FORMAT(/,29X,'WRITING RESTART FILES',/)                  DJG0495
               CALL DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP)     DJG0495
               IF (ISTOP.NE.0) RETURN                                   GDH1095
               CALL COMPFG (XPARAM,FUNCT,FAIL,GRAD,.FALSE.) 
               IFLEP=4                                                  DJG0995
               TIME0=TIME0-TOTIME 
               RETURN 
            ENDIF                                                       DJG0295
            IF(PRINT)WRITE (JOUT,410) 
  410       FORMAT (1H ,20X, 'COS WILL BE RESET AND ANOTHER ' 
     1       ,'ATTEMPT MADE') 
            COS=0.0D00 
            IF (.NOT.OKF) GOTO 385                                      DJG0295
         ELSE                                                           DJG0295
            GO TO 570 
         ENDIF
      ENDIF 
C   WE WANT ACCURATE DERIVATIVES AT THIS POINT 
C 
C   LINMIN DOES NOT GENERATE ANY DERIVATIVES, THEREFORE COMPFG MUST BE 
C   CALLED TO END THE SEARCH 
C 
C     RESTORE TO STANDARD VALUE BEFORE COMPUTING THE GRADIENT 
C      FROZEN=.FALSE. 
      SCFTOL=1.D0 
      RESET=.FALSE. 
      CALL COMPFG (XPARAM,FUNCT1,FAIL,GRAD,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (.NOT. OKC .AND. MINPRT)WRITE (JOUT,430) JCYC 
  430 FORMAT ( 23H0LINMIN FAILED AT CYCLE,I5/,  1H0) 
      XN=SQRT(DOT(XPARAM,XPARAM,NVAR)) 
      TX=ABS(ALPHA*PNORM) 
      IF (XN.NE.0.0D00) TX=TX/XN 
      GCOMPL=0.0D0                                                      DJG0495
      DO 435 I=1,NVAR                                                   DJG0495
 435     IF (ABS(GRAD(I)).GE.GCOMPL) GCOMPL=ABS(GRAD(I))                GDH1195
      CALL CONVCK(TOLERG,FFNCT1,FFNCT2,FUNCT1,NVAR,ICONFG)              GDH1195
      IF (PRINT) WRITE (JOUT,450) GCOMPL,NCOUNT                         DJG0495
  450 FORMAT ('  TERMINATION TEST ...',/,10X,'LARGEST COMPONENT OF THE' DJG0495
     1        ,' GRADIENT = ',F9.4,/,10X,'NUMBER OF COUNTS = ',I5)      DJG0495
      IF (ICONFG.EQ.0) THEN                                             DJG0495
         LAST=1                                                         DJG0495
         WRITE(JOUT,455) GCOMPL,TOLERG                                  DJG0495
  455    FORMAT(/,5X,'ABSOLUTE VALUE OF LARGEST GRADIENT COMPONENT =',  DJG0495
     1          F9.5,/,5X,'IS LESS THAN CUTOFF =', 1F9.5,               GDH1195
     2         ' AND THE CALCULATED ENERGY',/,5X,'CHANGED LESS THAN ',  GDH1195
     3          '.1 KCAL/MOL FOR LAST GEOMETRY CHANGE.',/,/)            GDH1195
         CALL COMPFG (XPARAM,FUNCT,FAIL,GRAD,.FALSE.)                   DJG0495
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         IF (RSTFIL.OR.RESTRT) THEN                                     DJG0495
            WRITE(JOUT,458)                                             DJG0495
  458       FORMAT(/,29X,'WRITING RESTART FILES',/)                     DJG0495
            MDFP(9)=2                                                   DJG0495
            CALL DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP)        DJG0495
      IF (ISTOP.NE.0) RETURN                                            GDH1095
            IF (IRESTA.EQ.-1) IRESTA=0                                  DJG0995
            CALL RESINP(FUNCT1)                                         DJG0995
         ENDIF                                                          DJG0495
         IFLEP=3                                                        DJG0995
         TIME0=TIME0-TOTIME                                             DJG0495
         RETURN                                                         DJG0495
      ENDIF                                                             DJG0495
  460 IF (NCOUNT.GE.MAXEND) THEN 
         WRITE (JOUT,470) 
  470    FORMAT ( 33H0TERMINATION FROM TOO MANY COUNTS) 
         LAST=1 
         CALL COMPFG (XPARAM,FUNCT,FAIL,GRAD,.FALSE.) 
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         WRITE(JOUT,475)                                                DJG0495
  475    FORMAT(/,29X,'WRITING RESTART FILES',/)                        DJG0495
         MDFP(9)=2                                                      DJG0495
         CALL DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP)           DJG0495
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         IFLEP=5                                                        DJG0995
         TIME0=TIME0-TOTIME 
         RETURN 
      ENDIF 
C 
C   ALL TESTS HAVE FAILED, WE NEED TO DO ANOTHER CYCLE. 
C 
  570 CONTINUE 
      BSMVF=ABS(SMVAL-FUNCT1) 
      IF (BSMVF.GT.10.D00) COS = 0.0D00 
      DEL=0.002D00 
      IF (BSMVF.GT.1.0D00) DEL=DELL/2.0D00 
      IF (BSMVF.GT.5.0D00) DEL=DELL 
      CALL PORCPU (TX2)                                                 GL0492
      TCYCLE=TX2-TX1 
      TX1=TX2 
C 
C END OF ITERATION LOOP, EVERYTHING IS STILL O.K. SO GO TO 
C NEXT ITERATION, IF THERE IS ENOUGH TIME LEFT AND CYCLES               DJG0495
C 
      IF(TCYCLE.LT.100000.D0)CYCMX=MAX(CYCMX,TCYCLE) 
      TUSED=TUSED+TCYCLE                                                GDH1294
      TLEFT=TLEFT-TCYCLE 
      IF(TLEFT.LT.0)TLEFT=-0.1D0 
      IF(TCYCLE.GT.1.D5)TCYCLE=0.D0 
      IF(MINPRT.OR.TIME) WRITE(JOUT,580)JCYC,TCYCLE,TLEFT,GNORM,FUNCT1 
  580 FORMAT(' CYCLE:',I3,' TIME:',F7.2,' TIME LEFT:',F9.1, 
     1' GRAD.NORM:',F10.3,' HEAT:',G14.7) 
      IF (TUSED.GE.TDP) THEN                                            GDH1194
         TUSED=0.D0                                                     GDH1194
         MDFP(9)=2                                                      GDH1194
         GOTO 595                                                       GDH1194
      ENDIF                                                             GDH1194
  480 IF (JCYC.GE.LIMCYC) THEN                                          GDH1195
         WRITE(JOUT,485)                                                DJG0495
  485    FORMAT(/,8X,'CYC : CYCLES   T : TIME (S)  ',                   DJG0495
     1         'GEO : CHANGE IN GEOMETRY'                               DJG0495
     2         ,' (RU) ',/,8X,'GCOMP : LARGEST COMPONENT OF GRADIENT ', DJG0495
     3         '(KCAL/RU)',/,8X,'HEAT : TOTAL ENERGY (KCAL/MOL)',/,     DJG0495
     4         8X,'1 RU = 1 ANGSTROM OR 1 RADIAN',/)                    DJG0495
         WRITE (JOUT,490)                                               DJG0495
  490    FORMAT(/,30X,'CYCLE LIMIT REACHED')                            DJG0495
         LAST=1                                                         DJG9495
         CALL COMPFG (XPARAM,FUNCT,FAIL,GRAD,.FALSE.)                   DJG0495
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         WRITE(JOUT,495)                                                DJG0495
  495    FORMAT(/,29X,'WRITING RESTART FILES',/)                        DJG0495
         MDFP(9)=2                                                      DJG0495
         CALL DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP)           DJG0495
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         IFLEP=12                                                       DJG0995
         TIME0=TIME0-TOTIME                                             DJG0495
         RETURN                                                         DJG0495
      ENDIF                                                             DJG0495
  585 IF (TLEFT.GT.SFACT*CYCMX) GO TO 40                                GDH1194
      WRITE(JOUT,590) 
  590 FORMAT (20X, 42HTHERE IS NOT ENOUGH TIME FOR ANOTHER CYCLE,/,30X, 
     118HNOW GOING TO FINAL) 
      MDFP(9)=1 
  595 CALL PORCPU (TFLY)                                                GL0492
      TOTIME=TOTIME+TFLY-TIME0 
      WRITE(JOUT,600)
600   FORMAT(/,29X,'WRITING RESTART FILES',/)                           DJG0495
      CALL DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (MDFP(9).EQ.2) GOTO 585                                        GDH1194
      RETURN                                                            GDH0495
      END
