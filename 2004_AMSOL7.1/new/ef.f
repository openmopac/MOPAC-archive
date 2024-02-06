      SUBROUTINE EF(XPARAM, NVAR, FUNCT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'                                                  DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
      DIMENSION XPARAM(*)
**********************************************************************
*
*   EF IS A QUASI NEWTON RAPHSON OPTIMIZATION ROUTINE BASED ON
*      JACS SIMONS P-RFO ALGORITHM AS IMPLEMENTED BY JON BAKER
*      (J.COMP.CHEM. 7, 385). STEP SCALING TO KEEP LENGTH WITHIN
*      TRUST RADIUS IS TAKEN FROM CULOT ET AL. (THEO. CHIM. ACTA 82, 189)
*      THE TRUST RADIUS CAN BE UPDATED DYNAMICALLY ACCORDING TO FLETCHER.
*      SAFEGUARDS ON VALID STEP FOR TS SEARCHES BASED ON ACTUAL/PREDICTED
*      FUNCTION CHANGE AND CHANGE IN TS MODE ARE OWN MODIFICATIONS
*
*  ON ENTRY XPARAM = VALUES OF PARAMETERS TO BE OPTIMISED.
*           NVAR   = NUMBER OF PARAMETERS TO BE OPTIMISED.
*
*  ON EXIT  XPARAM = OPTIMISED PARAMETERS.
*           FUNCT  = HEAT OF FORMATION IN KCAL/MOL.
*
*  CURRENT VERSION IMPLEMENTING COMBINED NR, P-RFO AND QA ALGORITHM
*      TOGETHER WITH THRUST RADIUS UPDATE AND STEP REJECTION WAS
*      MADE OCTOBER 1992 BY F.JENSEN, ODENSE, DK
*      UPDATE OF CONTROL PARAMETERS WERE DONE JANUAR 1997
*
**********************************************************************
C
C      AMSOL COMMON BLOCKS
C
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
C
C GEOVAR VARIABLES CHANGED ORDER IN AMSOL
C     COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
C
C WARNING : XPARAM AND NVAR ARE PASSED BOTH THROUGH COMMON GEOVAR AND THE
C           ARGUMENT LIST WHICH IS STUPID AND DANGEROUS, SO I JUST CHANGE THE
C           THE LABELS TO XDUMMY AND NDUM IN THE COMMON BLOCK, FOR THE SAKE
C           OF SIMPLICITY .      IVAN 2/94
C
      COMMON /GEOVAR/ XDUMMY(MAXPAR),NDUM,LOC(2,MAXPAR),IDUMY 
      COMMON /GRADNT/ GRAD(MAXPAR),GNFINA
      COMMON /LAST  / LAST
      COMMON /MESAGE/ IFLEP, ISCF                                       DJG0995 
      COMMON /CYCLCM/ PCMIN, NGEOM, NSOLPR, NSCFS, JPCNT                GDH0495
C
C   THE COMMON BLOCK MOLKST HAS BEEN CONVERTED TO MLKSTI AND MLKSTR   
C         ONLY MLKSTI USED BY EF
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,       
     2                NCLOSE,NOPEN,NDUMY                              
C
      COMMON /ONESCM/ ICONTR(100)                                       DJG0295
      COMMON /TIMECM  / TIME0                                           DJG0195
C
C      EF-ONLY COMMON BLOCKS
C
      COMMON /TIMDMP/ TLEFT, TDP                                        DJG0395
      COMMON /SIGMA2/ GNEXT1(MAXPAR), GMIN1(MAXPAR)
      COMMON /ISTOPE/ AMS(107)
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),
     1                PMAT(MAXPAR,MAXPAR)                                IR0494
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,RMIN,RMAX,OMIN,XLAMD,XLAMD0,SKAL,
     $MODE,NSTEP,NEGREQ,IPRNT
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /TRCACM/ NCALF                                            DJG0995
C
      CHARACTER SPACE*1, CHDOT*1, ZIPPO*1, NINE*1, CH*1                 DJG0495
      DIMENSION IPOW(9), EIGVAL(MAXPAR),TVEC(MAXPAR),SVEC(MAXPAR),
     1FX(MAXPAR),HESSC(MAXHES),UC(MAXPAR**2),OLDFX(MAXPAR),
     1OLDEIG(MAXPAR),
     $OLDHSS(MAXPAR,MAXPAR),OLDU(MAXPAR,MAXPAR),OOLDF(MAXPAR)
      DIMENSION BB(MAXPAR,MAXPAR) 
      LOGICAL RESTRT,SCF1,FAIL
      LOGICAL LUPD,LTS,LRJK,LORJK,RRSCAL,DONR,LNOUPD
      LOGICAL ITS,RSTART                                                DJG0995
      EQUIVALENCE(IPOW(1),IHESS1)                                       DJG0995
      SAVE
      DATA  ZERO,ONE,TWO    /0.D0,1.D0,2.D0/                            DJG0295
      DATA TMONE /1.0D-1/, TMTWO/1.0D-2/, TMSIX/1.0D-06/
      DATA THREE/3.0D0/, FOUR/4.0D0/, 
     1PT25/0.25D0/, PT5/0.50D0/, PT75/0.75D0/
      DATA DEMIN/1.0D-1/, GMIN/5.0D0/
      DATA SPACE,CHDOT,ZIPPO,NINE /' ','.','0','9'/                     DJG0495
C
C      VARIABLES CHECK
C
C     WRITE(50,*)'In the subroutine EF'                                 CCCDEVL
C     CLOSE(50)                                                         CCCDEVL
      IF (NUMAT.GT.2) THEN                                              DJG0195
      IF(NVAR.GT.(3*NUMAT-6)) THEN
        IF (IGEOOK.EQ.0.AND.(NVAR.NE.(3*NUMAT))) THEN                   DJG0995
             WRITE(JOUT,13)                                             GDH1093
13           FORMAT(/,5X,'EF: REDUNDANT VARIABLES.  EIGENVECTOR ',      DJG0295
     1             'FOLLOWING NOT RECOMMENDED',/,5X,'SPECIFY GEO-OK ',  DJG0295
     2             'IF YOU WANT TO CONTINUE',/)                         DJG0295
      ISTOP=1                                                           GDH1095
      IWHERE=32                                                         GDH1095
      RETURN                                                            GDH1095
        ENDIF
      ELSEIF(NVAR.EQ.(3*NUMAT)) THEN
          ISXYZ=IXYZ                                                    DJG0995
          IF((ISXYZ.EQ.0).AND.(IGEOOK.EQ.0)) THEN                       DJG0995
           WRITE(JOUT,14) 
14         FORMAT(/,5X,'EF: EXACTLY 3*N VARIABLES.',/,5X,               DJG0295
     1           'SPECIFY XYZ IF YOU ARE USING CARTESIAN COORDINATES',/,DJG0295
     2           5X,'IF YOU USE INTERNAL COORDINATES, YOU CAN SPECIFY ',DJG0295
     3           'GEO-OK,',/,5X,'BUT EIGENVECTOR FOLLOWING IS NOT ',    DJG0295
     4           'RECOMMENDED',/)                                       DJG0295
      ISTOP=1                                                           GDH1095
      IWHERE=33                                                         GDH1095
      RETURN                                                            GDH1095
          ENDIF
      ENDIF
      ENDIF                                                             DJG0195
C
C     GET ALL INITIALIZATION DATA
C
      IF (ITDUMP.GT.0) THEN                                             DJG0995
          TDP=FITDUM                                                    DJG0995
      ELSE                                                              DJG0395
          TDP=1800                                                      DJG0395
      ENDIF                                                             DJG0395
      IHESS1=0                                                          DJG0995
      NSTEP=0
      CALL PORCPU (TT0)
C   FLAG TO CHECK IF RESTART FILES HAVE BEEN WRITTEN                    DJG0495
      RSTART=.FALSE.                                                    DJG0495
C   NHFLAG=1 IF HESSIAN SHOULD BE RECALCULATED ON FIRST CYCLE OF RESTARTDJG0495
      NHFLAG=0                                                          DJG0495
      IF(ITLIMI.NE.0) THEN                                              DJG0995
         TIM=FITLIM                                                     DJG0995
   21    TLEFT=TIM                                                      DJG0395
      ELSE                                                              DJG0395
         TLEFT=TDEF-TT0
      ENDIF                                                             DJG0395
      IF(ICONTR(27).EQ.1) THEN                                          DJG0295 
        NCALF=0                                                         DJG0995
        FFNCT2=0.D0                                                     GDH1195
        FFNCT1=0.D0                                                     GDH1195
        CALL EFSTR(XPARAM,FUNCT,IHESS1,NTIME,ILOOP,IGTHES,MXSTEP,IRECLC,DJG0995
     &              IUPD,DMAX,DDMAX,DDMIN,TOLERG,TOTIME,TIME1,TIME2,    DJG0495
     &              NVAR,SCF1,LUPD,RRSCAL,DONR)
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         ODMAX=DMAX                                                     DJG0295
C        INITIALIZE VARIABLES FOR KICK FUNCTION                         DJG0295
         DMAX1=DMAX                                                     DJG0295
         KNUM=0                                                         DJG0295
         NUMVAR=0                                                       DJG0295
         NFREDM=1                                                       DJG0295
         KICK=0                                                         DJG0495
         IF (IKICK.EQ.1) KICK=1                                         DJG0995
         IF (IKICK.EQ.2) KICK=IIKICK                                    DJG0995
         TUSED=0.0                                                      DJG0495
         ITS=ITSTAT.NE.0                                                DJG0995
      ENDIF                                                             DJG0295
      LTS=.FALSE.
      IF (NEGREQ.EQ.1) LTS=.TRUE.
      LORJK=.FALSE.
      IF (SCF1) THEN
         GNFINA=SQRT(DOT(GRAD,GRAD,NVAR))
         IFLEP=1                                                        DJG0995
         RETURN
      ENDIF
C
C     CHECK THAT GEOMETRY IS NOT ALREADY OPTIMIZED
C 
      CALL CONVCK(TOLERG,FFNCT1,FFNCT2,FUNCT,NVAR,ICONFG)               GDH1195
      IF (ICONFG.EQ.0) THEN                                             DJG0495
         IFLEP=2                                                        DJG0995
         LAST=1                                                         DJG0495
         IF (ICR.EQ.1) WRITE(JOUT,24)                                   DJG0995
24       FORMAT(16X,'NO STEPS WERE TAKEN IN GAS PHASE CALCULATION, SO', DJG0995
     1          /,12X,'RESTART FILES WILL NOT BE USED IN SOLVATION',    DJG0995
     2          ' CALCULATION')                                         DJG0995
         RETURN                                                         DJG0495
      ENDIF                                                             DJG0495
C
C     GET INITIAL HESSIAN. IF ILOOP IS .LE.0 THIS IS AN OPTIMIZATION RESTART 
C     AND HESSIAN SHOULD ALREADY BE AVAILABLE                                   
C
      IF (ICONTR(27).EQ.1) THEN                                         DJG0495
         WRITE(JOUT,37)                                                 DJG0495
 37   FORMAT(/,8X,'CYC : CYCLES   T : TIME (S)  ',                      DJG0495
     1       'GEO : CHANGE IN GEOMETRY'                                 DJG0495
     2       ,/,8X,'GCOMP : LARGEST COMPONENT OF GRADIENT ',            DJG0495
     3       '(KCAL/RU)',/,8X,'HEAT : TOTAL ENERGY (KCAL/MOL)',/,       DJG0495
     4       8X,'1 RU = 1 ANGSTROM OR 1 RADIAN',/)                      DJG0495
         IF (ILOOP.GT.0) CALL GETHES(XPARAM,IGTHES,NVAR,ILOOP,TOTIME)   DJG0495
      ENDIF                                                             DJG0495
      ICONTR(27)=2                                                      DJG0295
C
C     START OF MAIN LOOP
C     WE NOW HAVE GRADIENTS AND A HESSIAN. IF THIS IS THE FIRST
C     TIME THROUGH DON'T UPDATE THE HESSIAN. FOR LATER LOOPS ALSO
C     CHECK IF WE NEED TO RECALCULATE THE HESSIAN
C
      IFLEP=0                                                           DJG0995
      ITIME=0
   10 CONTINUE
C
C     STORE VARIOUS THINGS FOR POSSIBLY OMIN REJECTION
C
      LNOUPD=.FALSE.
      DO 30 I=1,NVAR
       OLDFX(I)=FX(I)
       OOLDF(I)=OLDF(I)
       OLDEIG(I)=EIGVAL(I)
       DO 20 J=1,NVAR
          OLDHSS(I,J)=HESS(I,J)
          OLDU(I,J)=U(I,J)
20     CONTINUE
30    CONTINUE
      IF (IHESS1.GE.IRECLC.AND.IFLEP.NE.3) THEN                         DJG0995
         ILOOP=1
         IHESS1=0                                                       DJG0995
         WRITE(JOUT,33)                                                 DJG0495
33       FORMAT(/,10X,'WRITING RESTART FILES')                          DJG0495
         IPOW(9)=2                                                      DJG0495
         RSTART=.TRUE.                                                  DJG0495
         CALL EFSAV(TSTEP,HESS,FUNCT,GRAD,XPARAM,PMAT,-NSTEP,NSTEP,BMAT DJG0495
     1             , IPOW,NHFLAG)                                       DJG0495
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         IF (IRESTA.EQ.0) IRESTA=-1                                     DJG0995
         CALL RESINP(FUNCT)                                             DJG0995
         CALL GETHES(XPARAM,IGTHES,NVAR,ILOOP,TOTIME)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      ENDIF
      IF (IHESS1.GT.0) CALL UPDHES(SVEC,TVEC,NVAR,IUPD)                 DJG0995
      IF(IPRNT.GE.2) CALL GEOUT
      IF(IPRNT.GE.2) THEN
         WRITE(JOUT,'('' XPARAM '')')
         WRITE(JOUT,'(5(2I3,F10.4))')(LOC(1,I),LOC(2,I),XPARAM(I),I=1,
     1NVAR)
         WRITE(JOUT,'('' GRADIENTS'')')
         WRITE(JOUT,'(3X,8F9.3)')(GRAD(I),I=1,NVAR)
      ENDIF
C
C        PRINT RESULTS IN CYCLE
      GNFINA=SQRT(DOT(GRAD,GRAD,NVAR))
      CALL PORCPU(TIME2)
      IF (ITIME.EQ.0) TIME1=TIME0
      TSTEP=TIME2-TIME1
      IF (TSTEP.LT.ZERO)TSTEP=ZERO
      TMDUMP=TMDUMP+TSTEP
      TLEFT=TLEFT-TSTEP
      IF (TMDUMP.GE.TDP.OR.TLEFT.LT.(TSTEP*TWO)) THEN                   DJG0395
         TMDUMP=0.D0                                                    DJG0395
         IPOW(9)=2                                                      DJG0395
         WRITE(JOUT,35)                                                 DJG0495
35       FORMAT(/,5X,'WRITING RESTART FILES',/)                         DJG0495
         RSTART=.TRUE.                                                  DJG0495
         CALL EFSAV(TSTEP,HESS,FUNCT,GRAD,XPARAM,PMAT,-NSTEP,NSTEP,BMAT DJG0395
     1             , IPOW,NHFLAG)                                       DJG0495
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         IF (IRESTA.EQ.0) IRESTA=-1                                     DJG0995
         CALL RESINP(FUNCT)                                             DJG0995
         IF (TLEFT.LT.(TSTEP*TWO)) THEN                                 DJG0395
            WRITE(JOUT,'(//22X,'' TIME IS RUNNING OUT!'')')             DJG0395
            WRITE(JOUT,     
     .              '(//10X,'' - THE CALCULATION IS BEING DUMPED TO'',  DJG0395
     1           '' DISK'',/10X,''   RESTART IT USING THE MAGIC WORD'', DJG0395
     2           '' "RESTART"'')')                                      DJG0395
            WRITE(JOUT,'(//10X,''CURRENT VALUE OF HEAT OF FORMATION ='' DJG0395
     1           ,F12.6)') FUNCT                                        DJG0395
         ENDIF                                                          DJG0395
      ENDIF                                                             DJG0395
      TIME1=TIME2
      ITIME=ITIME+1
      TUSED=TUSED+TSTEP                                                 DJG0495
      XN=SQRT(DOT(XPARAM,XPARAM,NVAR))                                  DJG0495
      TX=SQRT(ABS(DOT(D,D,NVAR)))                                       DJG0495
      IF (XN.NE.0.0D00) TX=TX/XN                                        DJG0495
      IF (TLEFT .LT. TSTEP*TWO) GOTO 280                                DJG0495
      WRITE(JOUT,40) NSTEP,TUSED,TX,GNFINA,FUNCT                        DJG0495
 40   FORMAT(' CYC:',I3,'  T:',F7.1,'  GEO: ',F8.6,                     BJL1003
     1       '  GCOMP:',F8.3,'  HEAT:',F12.6)                           DJG0495
      IHESS1=IHESS1+1                                                   DJG0995
      NSTEP=NSTEP+1
C
C        TEST FOR CONVERGENCE
C
      CALL CONVCK(TOLERG,FFNCT1,FFNCT2,FUNCT,NVAR,ICONFG)               GDH1195
      IF (ICONFG.EQ.0) GOTO 250                                         DJG0495
      OLDE  = FUNCT
      RMX=SQRT(DOT(GRAD,GRAD,NVAR))
      OLDGN = RMX
      DO 60 I=1,NVAR
         OLDF(I)=GRAD(I)
60    CONTINUE
C
C     IF THE OPTIMIZATION IS IN CARTESIAN COORDINATES, WE SHOULD REMOVE
C     TRANSLATION AND ROTATION MODES. 
C
      IF ( ISXYZ.NE.0 ) THEN
        WRITE(JOUT,62)
 62     FORMAT('OPTIMIZING USING CARTESIAN COORDINATES..')
      CALL PRJFC(HESS,XPARAM,NVAR)
      ENDIF
      IJ=0
      DO 80 I=1,NVAR
         DO 80 J=1,I
            IJ=IJ+1
            HESSC(IJ)=HESS(J,I)
   80 CONTINUE
      CALL HQRII(HESSC,NVAR,NVAR,EIGVAL,UC)
C
C IVAN : REMOVED, NOW HQRII SEEMS FAST. IF NOT, PUT LAPACK ROUTINE INSIDE
C      AN HQRII ENVELOPE. IT'S TIME TO SHIFT TO BLAS ! 
C
C      CALL RSP(HESSC,NVAR,NVAR,EIGVAL,UC)
C
      IJ=0
      DO 90 I=1,NVAR
       IF (ABS(EIGVAL(I)).LT.TMSIX) EIGVAL(I)=ZERO
         DO 90 J=1,NVAR
            IJ=IJ+1
            U(J,I)=UC(IJ)
   90 CONTINUE
      IF (IPRNT.GE.3) CALL PRTHES(EIGVAL,NVAR)
      IF (MXSTEP.EQ.0) NSTEP=0
      IF (MXSTEP.EQ.0) GOTO 280 
      NEG=0                    
      DO 100 I=1,NVAR         
         IF (EIGVAL(I) .LT. ZERO)NEG=NEG+1                                     
  100 CONTINUE               
      IF (IPRNT.GE.1)WRITE(JOUT,110)NEG,(EIGVAL(I),I=1,NEG)
  110 FORMAT(/,10X,'HESSIAN HAS',I3,' NEGATIVE EIGENVALUE(S)',6F7.1,/)
C
C     IF AN EIGENVALUE HAS BEEN ZERO OUT IT IS PROBABLY ONE OF THE T,R MODES
C     IN A CARTESIAN OPTIMIZATION. ZERO CORRESPONDING FX TO ALLOW FORMATION
C     OF STEP WITHOUT THESE CONTRIBUTIONS. A MORE SAFE CRITERIA FOR DECIDING
C     WHETHER THIS ACTUALLY IS A CARTESIAN OPTIMIZATION SHOULD BE PUT IN 
C     SOME DAY...
C
      DO 120 I=1,NVAR                                                           
         FX(I)=DOT(U(1,I),GRAD,NVAR)                                            
         IF (ABS(EIGVAL(I)).EQ.ZERO) FX(I)=ZERO
  120 CONTINUE                                                                  
      
C     FORM GEOMETRY STEP D
130   CALL FORMD(EIGVAL,FX,NVAR,DMAX,DDMIN,LTS,LRJK,LORJK,RRSCAL,DONR) 
      IF (LORJK .AND. .NOT.LNOUPD) LNOUPD=.TRUE.
      IF (ISTOP.NE.0) RETURN                                            GDH1095
C     IF LORJK IS TRUE, THEN TS MODE OVERLAP IS LESS THAN OMIN, REJECT PREV STEP
      IF (LORJK) THEN
       IF (IPRNT.GE.1)WRITE(JOUT,*)'      UNDOING PREVIOUS STEP',
     *                                 ' AND GOING FOR NEW STEP'
       DMAX=ODMAX
       DD=ODD
       OLDE=OOLDE
       DO 133 I=1,NVAR
          FX(I)=OLDFX(I)
          OLDF(I)=OOLDF(I)
          EIGVAL(I)=OLDEIG(I)
          DO 135 J=1,NVAR
             HESS(I,J)=OLDHSS(I,J)
             U(I,J)=OLDU(I,J)
135       CONTINUE 
133    CONTINUE
         DO 140 I=1,NVAR
            XPARAM(I)=XPARAM(I)-D(I)
            K=LOC(1,I)
            L=LOC(2,I)
            GEO(L,K)=XPARAM(I)
140      CONTINUE
         IF(NDEP.NE.0) CALL SYMTRY
       DMAX=MIN(DMAX,DD)/TWO
       DMAX=MAX(DMAX,DDMIN)
       ODMAX=DMAX
       ODD=DD
       NSTEP=NSTEP-1
         IF (DMAX.LT.DDMIN) GOTO 230
       IF(IPRNT.GE.1)WRITE(JOUT,220)DMAX
       GOTO 130
      ENDIF
C
C  FORM NEW TRIAL XPARAM AND GEO
C
      DO 150 I=1,NVAR
         XPARAM(I)=XPARAM(I)+D(I)
         K=LOC(1,I)
         L=LOC(2,I)
         GEO(L,K)=XPARAM(I)
  150 CONTINUE

      XN=SQRT(DOT(XPARAM,XPARAM,NVAR))
      TX=SQRT(ABS(DOT(D,D,NVAR)))
      IF (XN.NE.0.0D00) TX=TX/XN

      IF(NDEP.NE.0) CALL SYMTRY
C
C     COMPARE PREDICTED E-CHANGE WITH ACTUAL 
C
      DEPRE=ZERO
      IMODEV=1                                                          DJG0995
      IF (MODE.NE.0)IMODEV=MODE                                         DJG0995
      DO 160 I=1,NVAR
       XTMP=XLAMD
       IF (LTS .AND. I.EQ.IMODEV) XTMP=XLAMD0                           DJG0995
       IF (ABS(XTMP-EIGVAL(I)).LT.TMTWO) THEN
       SS=ZERO
       ELSE
       SS=SKAL*FX(I)/(XTMP-EIGVAL(I))
       ENDIF
       FRODO=SS*FX(I) + PT5*SS*SS*EIGVAL(I)
C        WRITE(JOUT,88)I,FX(I),SS,XTMP,EIGVAL(I),FRODO
       DEPRE=DEPRE+FRODO
160   CONTINUE
C88   FORMAT(I3,F10.3,F10.6,F10.3,4F10.6)
C
C     GET GRADIENT FOR NEW GEOMETRY 
C
      FAIL=.TRUE.
      CALL COMPFG(XPARAM, FUNCT, FAIL, GRAD, .TRUE.)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF(FAIL) GOTO 280 
      DEACT = FUNCT-OLDE
      RATIO = DEACT/DEPRE
      IF(IPRNT.GE.1)WRITE(JOUT,170)DEACT,DEPRE,RATIO       
  170 FORMAT(5X,'ACTUAL, PREDICTED ENERGY CHANGE, RATIO',2F10.3,F10.5)
C
C     POSSIBLY REJECT THE STEP IF THE RATIO BETWEEN ACTUAL AND 
C     PREDICTED CHANGE IN ENERGY IS OUTSIDE RMIN AND RMAX LIMITS
C     THE DEFAULT VALUE OF RMIN=0.0 FOR MINIMIZATIONS IS EQUIVALENT
C     TO NOT ALLOWING THE ENERGY TO RAISE.
C     DON'T WORRY IF THE ABSOLUTE CHANGES ARE SMALL ( < DEMIN)
C
      LRJK=.FALSE.
      IF ( (RATIO.LT.RMIN .OR. RATIO.GT.RMAX) .AND.
     $(ABS(DEPRE).GT.DEMIN .OR. ABS(DEACT).GT.DEMIN)) THEN
       DTMP = MIN(DMAX,DD)/TWO
       IF (DTMP.LE.DDMIN) DTMP=DDMIN
       IF (IPRNT.GE.1)WRITE(JOUT,180)DTMP
180   FORMAT(1X,'UNACCEPTABLE RATIO,',
     $          ' REJECTING STEP, REDUCING DMAX TO',F7.4)
       LRJK=.TRUE.
      ENDIF
C     IF THE TRUST RADIUS IS EQUAL TO DDMIN, CONTINUE ANYWAY
      IF (LRJK .AND. DMAX.LE.DDMIN) THEN
       IF (IPRNT.GE.1)WRITE(JOUT,190)
190   FORMAT(1X,'NEW TRUST RADIUS WOULD BE BELOW DDMIN,',
     $          ' ACCEPTING STEP ANYWAY')
       LRJK=.FALSE.
      ENDIF
      IF (LRJK) THEN
         DO 200 I=1,NVAR
            XPARAM(I)=XPARAM(I)-D(I)
            K=LOC(1,I)
            L=LOC(2,I)
            GEO(L,K)=XPARAM(I)
200      CONTINUE
         IF(NDEP.NE.0) CALL SYMTRY
         DMAX=DTMP
         IF (DMAX.LT.DDMIN) GOTO 230
       GOTO 130
      ENDIF
      IF(IPRNT.GE.1)WRITE(JOUT,210)DD
  210 FORMAT(5X,'STEPSIZE USED IS',F9.5)
      IF(IPRNT.GE.2) THEN
         WRITE(JOUT,'('' CALCULATED STEP'')')
         WRITE(JOUT,'(3X,8F9.5)')(D(I),I=1,NVAR)
      ENDIF
C
C     POSSIBLE USE DYNAMICAL TRUST RADIUS
C
      ODMAX=DMAX
      ODD=DD
      OOLDE=OLDE
      IF (LNOUPD) THEN
         LNOUPD=.FALSE.
         GOTO 230
      ENDIF
      IF (LUPD .AND. ( (RMX.GT.GMIN) .OR.
     $    (ABS(DEPRE).GT.DEMIN .OR. ABS(DEACT).GT.DEMIN) ) ) THEN
C
C     FLETCHER RECOMMEND DMAX=DMAX/4 AND DMAX=DMAX*2
C     THESE ARE ARE A LITTLE MORE CONSERVATIVE SINCE HESSIAN IS BEING UPDATED
C
      IF (LTS .AND. RATIO.LE.TMONE .OR. RATIO.GE.THREE)
     $    DMAX=MIN(DMAX,DD)/TWO
      IF (LTS .AND. RATIO.GE.PT75 .AND. RATIO.LE.(FOUR/THREE) 
     $                  .AND. DD.GT.(DMAX-TMSIX)) 
     $   DMAX=DMAX*SQRT(TWO)
C
C     ALLOW WIDER LIMITS FOR INCREASING TRUST RADIUS FOR MIN SEARCHES
C
      IF (.NOT.LTS .AND. RATIO.LE.TMONE)
     $    DMAX=MIN(DMAX,DD)/SQRT(TWO)
      IF (.NOT.LTS .AND. RATIO.GE.PT5 
     $                  .AND. DD.GT.(DMAX-TMSIX)) 
     $   DMAX=DMAX*SQRT(TWO)
C
C     BE BRAVE IF  0.90 < RATIO < 1.10 ...
C
      IF (ABS(RATIO-ONE).LT.TMONE) DMAX=DMAX*SQRT(TWO)
      DMAX=MAX(DMAX,DDMIN)
      DMAX=MIN(DMAX,DDMAX)
      ENDIF
C
C     ALLOW STEPSIZE UP TO 0.1 IN THE END-GAME WHERE CHANGES ARE LESS 
C     THAN DEMIN AND GRADIENT IS LESS THAN GMIN
C
      IF (LUPD .AND. RMX.LT.GMIN .AND.
     $   (ABS(DEPRE).LT.DEMIN .AND. ABS(DEACT).LT.DEMIN) )
     $    DMAX=MAX(DMAX,TMONE)
      IF(IPRNT.GE.1)WRITE(JOUT,220)DMAX
 220  FORMAT(5X,'NEW TRUST RADIUS = ',F7.5)                  
230   IF (DMAX.LT.DDMIN) THEN
 232     IF (KNUM.LT.KICK) THEN                                         DJG0495
            KNUM=KNUM+1                                                 DJG0295
 234        NUMVAR=NUMVAR+1                                             DJG0295
            IF (NUMVAR.GT.NVAR) THEN                                    DJG0295
               IF (KNUM.EQ.1) THEN                                      DJG0295
                  NFREDM=NFREDM+1                                       DJG0295
                  NUMVAR=1                                              DJG0295
               ELSE                                                     DJG0295
                  NUMVAR=1                                              DJG0295
               ENDIF                                                    DJG0295
            ENDIF                                                       DJG0295
            IF (LOC(2,NUMVAR).NE.NFREDM) GOTO 234                       DJG0295
            IF (NFREDM.EQ.1) THEN                                       DJG0295
               XPARAM(NUMVAR)=XPARAM(NUMVAR)+0.15D0                     DJG0295
            ELSE IF (NFREDM.EQ.2) THEN                                  DJG0295
               XPARAM(NUMVAR)=XPARAM(NUMVAR)+5.0D0                      DJG0295
            ELSE                                                        DJG0295
               XPARAM(NUMVAR)=XPARAM(NUMVAR)+10.0D0                     DJG0295
            ENDIF                                                       DJG0295
            WRITE(JOUT,236)                                             DJG0295
  236       FORMAT(/,/,1X,'TRUST RADIUS IS NOW TOO SMALL.  ',           DJG0295
     1        'IN AN ATTEMPT TO CIRCUMVENT THIS,',/,1X,'THE GEOMETRY'   DJG0295
     2        ,' WILL BE PERTURBED, THE TRUST RADIUS RESET, ',/,        DJG0295
     3        ' THE HESSIAN RECALCULATED, AND ANOTHER ATTEMPT MADE',/)  DJG0495
            DMAX=DMAX1                                                  DJG0295
            CALL GETHES(XPARAM,IGTHES,NVAR,ILOOP,TOTIME)                DJG0495
      IF (ISTOP.NE.0) RETURN                                            GDH1095
            WRITE(JOUT,2035)                                            DJG0495
2035        FORMAT(/,5X,'WRITING RESTART FILES',/)                      DJG0495
            IPOW(9)=2                                                   DJG0495
            RSTART=.TRUE.                                               DJG0495
            CALL EFSAV(TSTEP,HESS,FUNCT,GRAD,XPARAM,PMAT,-NSTEP,NSTEP,  DJG0495
     1                 BMAT,IPOW,NHFLAG)                                DJG0495
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         ELSE                                                           DJG0295
            WRITE(JOUT,2050)                                            DJG0495
2050        FORMAT(/,10X,'WRITING RESTART FILES',/)                     DJG0495
            IPOW(9)=2                                                   DJG0495
            RSTART=.TRUE.                                               DJG0495
            NHFLAG=0                                                    DJG0495
cfrj        NHFLAG=1                                                    DJG0495
            CALL EFSAV(TSTEP,HESS,FUNCT,GRAD,XPARAM,PMAT,-NSTEP,NSTEP,  DJG0495
     1                 BMAT,IPOW,NHFLAG)                                DJG0495
      IF (ISTOP.NE.0) RETURN                                            GDH1095
            IF (IRESTA.EQ.0) IRESTA=-1                                  DJG0995
            CALL RESINP(FUNCT)                                          DJG0995
            WRITE(JOUT,240)DDMIN
240         FORMAT(/,10X,'TRUST RADIUS NOW LESS THAN ',F7.5,            DJG0495
     1            ' OPTIMIZATION TERMINATING',/,10X,'GEOMETRY MAY NOT', DJG0495
     2            ' BE COMPLETELY OPTIMIZED',/,10X,'RESTART FILES HAVE',DJG0495
     3            ' BEEN PRINTED.',/,10X,'SEE OPTIMIZATION SECTION',    DJG0495
     4            ' OF THE MANUAL FOR FURTHER INFORMATION ON',/,10X,    DJG0495
     5            'IMPROVING OPTIMIZATIONS.' )                          DJG0495
            IFLEP=9                                                     DJG0995
            GOTO 270
         ENDIF                                                          DJG0295
      ENDIF
C
C     CHECK STEPS AND ENOUGH TIME FOR ANOTHER PASS
C
      IF (NSTEP.GE.MXSTEP) GOTO 280
C
C     RETURN FOR ANOTHER CYCLE
C
      GOTO 10                                                                  
C
C     ****** OPTIMIZATION TERMINATION ******
C
  250 CONTINUE
      GCMAX=ABS(GRAD(1))                                                DJG0495
      DO 255 I=2,NVAR                                                   DJG0495
         IF (ABS(GRAD(I)).GT.GCMAX) GCMAX=ABS(GRAD(I))                  DJG0495
  255 CONTINUE                                                          DJG0495
      WRITE(JOUT,260) GCMAX, TOLERG                                     DJG0495
  260 FORMAT(/,5X,'ABSOLUTE VALUE OF LARGEST GRADIENT COMPONENT =',     DJG0495
     1          F9.5,/,5X,'IS LESS THAN CUTOFF =', 1F9.5,               GDH1195
     2         ' AND THE CALCULATED ENERGY',/,5X,'CHANGED LESS THAN ',  GDH1195
     3          '.1 KCAL/MOL FOR LAST GEOMETRY CHANGE.',/,/)            GDH1195
C                                                                       DJG0495
C     IF RESTART FILES HAVE BEEN WRITTEN, OR FINISHING THE GAS-PHASE    DJG0995
C     PORTION OF A 'CALC' RUN -                                         DJG0995
C     REWRITE THE RESTART FILES WITH NEW GEOMETRY AND WITHOUT           DJG0495
C     KEYWORD 'RESTART'                                                 DJG0495
C                                                                       DJG0495
      IF (RSTART) THEN                                                  DJG0495
         IPOW(9)=2                                                      DJG0495
         WRITE(JOUT,265)                                                DJG0495
265      FORMAT(/,5X,'WRITING RESTART FILES',/)                         DJG0495
         CALL EFSAV(TSTEP,HESS,FUNCT,GRAD,XPARAM,PMAT,-NSTEP,NSTEP,     DJG0495
     1              BMAT,IPOW,NHFLAG)                                   DJG0495
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         IF (IRESTA.EQ.-1) IRESTA=0                                     DJG0995
         CALL RESINP(FUNCT)                                             DJG0995
      ELSE IF (ICR.EQ.1) THEN                                           DJG0995
         CALL EFCRES(TSTEP,FUNCT,GRAD,XPARAM,-NSTEP,NSTEP,IPOW,NHFLAG)  DJG0995
      ENDIF                                                             DJG0495
      IFLEP=3                                                           DJG0995
  270 LAST=1
      RETURN
C
C     ERROR HANDLING. DUMP RESULTS
C
  280 CONTINUE
      IF ( FAIL ) THEN
         WRITE(JOUT,281)
  281    FORMAT(/,10X,'EF: ENERGY NOT CONVERGED...SORRY !',/)           
      ENDIF
      IF (TLEFT .LT. TSTEP*TWO) THEN
         WRITE(JOUT,290)
  290    FORMAT(/,10X,'EF: NOT ENOUGH TIME FOR ANOTHER CYCLE',/)
         IFLEP=12                                                       DJG0995
      ENDIF
      IF (NSTEP.GE.MXSTEP) THEN
         WRITE(JOUT,300)
  300    FORMAT(/,10X,'EF: EXCESS NUMBER OF OPTIMIZATION CYCLES',/)
         IFLEP=12                                                       DJG0995
      ENDIF
      IPOW(9)=1
      CALL PORCPU(TT0)
      TT0=TT0-TIME0
      WRITE(JOUT,310)                                                   DJG0495
310   FORMAT(/,5X,'WRITING RESTART FILES',/)                            DJG0495
      CALL EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,-NSTEP,NSTEP,BMAT,IPOW,DJG0495
     1           NHFLAG)                                                DJG0495
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      WRITE(JOUT,'(//10X,'' - THE CALCULATION IS BEING DUMPED TO'',     DJG0395
     1     '' DISK'',/10X,''   RESTART IT USING THE MAGIC WORD'',       DJG0395
     2     '' "RESTART"'')')                                            DJG0395
      WRITE(JOUT,'(//10X,''CURRENT VALUE OF HEAT OF FORMATION =''       DJG0395
     1     ,F12.6)') FUNCT                                              DJG0395
      IF (IRESTA.EQ.0) IRESTA=-1                                        DJG0995
      CALL RESINP(FUNCT)                                                DJG0995
      IF (IFLEP.NE.12) IFLEP=13                                         GDH0597
      RETURN
      END
