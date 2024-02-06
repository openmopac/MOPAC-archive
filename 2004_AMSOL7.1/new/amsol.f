      SUBROUTINE AMSOL                                                  GDH1095
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
       INCLUDE 'SIZES2.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
      PARAMETER (MFBWO=9*MAXPAR)
************************************************************************
      COMMON /REPATH/ REACT(100),LATOM,LPARAM                           03GCL93
      COMMON /TIMECM  / TIME0                                           03/30/GL
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOVAR/ XPARAM(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY           03/30/GL
      COMMON /OPTIMI / IMP,IMP0                                         03/30/GL
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /ONESCM/ ICONTR(100)                                       GDH0895
      COMMON /CONVCM/ JCONV                                             GDH1195
      COMMON /QESTR/  SCOORD(3,NUMATM), ISPEC                           GDH0197
      COMMON /MODLCM/ SMODEL                                            GDH0597
      COMMON /PROPCM/ SDEPT(10), SOLN,SOLA,SOLB,SOLG,SOLC,SOLH          GDH0897
      LOGICAL LDUMMY                                                    GDH1095
      SAVE                                                              GDH1294
      ICR=0                                                             GDH0495
      OPEN(19,NAME='fort.19')                                           BJL1003
      OPEN(20,NAME='fort.20')                                           BJL1003
      PI=ACOS(-1.D0)
      PD2=PI*0.5D0
      PD64=PI*0.015625D0
C
C   CALCULATE THE OVERHEAD FOR CALLING THE CPU TIMIMG ROUTINE
C
      CALL PORCPU (T0)                                                  GL0492
      CALL PORCPU (T1)                                                  GL0492
      TOVERH = T1 - T0                                                  GL0492
C
      CALL PORCPU (TIME0)                                               GL0492
      TIME0 = TIME0 + TOVERH                                            GL0492
C                                                                       DJG0995
C  ICONTR IS A FLAG WHICH INDICATES WHETHER A SUBROUTINE HAS NEVER BEEN DJG0996
C  ENTERED (ICONTR=1) OR HAS BEEN ENTERED BEFORE (ICONTR=0)             DJG0996
C                                                                       DJG0996
C  CURRENT HIGHEST-USED VALUE OF ICONTR IS 62                           DL0397
C  ICONTR IS ALSO RESET IN THE SUBROUTINE PATHS (THREE SPOTS) BETWEEN   DJG0995
C  EACH REACTION COORDINATE CALCULATION                                 DJG0995
C  AND ALSO AT THE END OF WRITES IF A TRUES/CALC CALCULATION            DJG0995
C                                                                       DJG0995
      DO 3 I=1,100                                                      GDH0895
3        ICONTR(I)=1                                                    GDH0895


C READ AND CHECK INPUT FILE, EXIT IF NECESSARY.
C     WRITE INPUT FILE TO UNIT IPRT AS FEEDBACK TO USER
C
  1   CONTINUE                                                          GDH1095
      CALL READS                                                        GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF(I0SCF.NE.0) THEN                                               DJG0995
      RETURN                                                            GDH1095
      ENDIF                                                             GDH1095
C
C INITIALIZE CALCULATION AND WRITE CALCULATION INDEPENDENT INFO
C
      IF(IEXTER.NE.0) THEN                                              DJG0995
         CALL AM1
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      ELSE
         CALL MOLDAT
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      ENDIF

C
C CALCULATE IF 1SCF NOT REQUIRED
C
      IF(I1SCF.NE.0) THEN                                               DJG0995
         IF(IRESTA.EQ.0) THEN                                           DJG0995
C           DO A SINGLE CALCULATION ...
            NVAR=0
            IF(IGRADI.NE.0) THEN                                        DJG0995
C              WITH A SINGLE GRADIENT COMPUTATION
               DO 30 I=2,NATOMS
                  IF(LABELS(I).EQ.99) GOTO 30
                  IF(I.EQ.2)ILIM=1
                  IF(I.EQ.3)ILIM=2
                  IF(I.GT.3)ILIM=3
                  DO 20 J=1,ILIM
                     NVAR=NVAR+1
                     LOC(1,NVAR)=I
                     LOC(2,NVAR)=J
   20             XPARAM(NVAR)=GEO(J,I)
   30          CONTINUE
            ENDIF
            CALL FLEPO(XPARAM, NVAR, ESCF)                              DJG0195
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         ENDIF                                                          DJG0195
         GOTO 100                                                       DJG0195
      ENDIF                                                             DJG0195
C
C     SECTION TRAJECTORY
C     ------------------
      IF(IDRC.NE.0) THEN                                                DJG0995
C     FOLLOW THE DYNAMIC REACTION COORDINATE
         CALL DRC
         RETURN                                                         GDH1095
      ENDIF
      IF(IPATH.NE.0) THEN                                               DJG0995
C     FOLLOW THE STATIC REACTION PATH
         CALL PATHAM (IND,XPARAM,ESCF,GRAD,NVAR)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         GO TO 100
      ENDIF
C
C     SECTION TRANSITION STATE SEARCH
C     -------------------------------
      IF(ITSTAT.NE.0) THEN                                              DJG0995
         CALL EF (XPARAM,NVAR,ESCF)                                     IR1294
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         GO TO 100                                                      IR1294
      ENDIF                                                             IR1294
      IF(ISADDL.NE.0) THEN                                              DJG0995
C     USING THE 'SADDLE' METHOD
         CALL REACT1(ESCF)
         RETURN                                                         GDH1095
      ENDIF
      IF(ICHAIN.NE.0) THEN                                              DJG0995
C     USING THE 'CHAIN' METHOD
         CALL CHAIN (IND,XPARAM,ESCF,GRAD,NVAR)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         GO TO 100
      ENDIF
      IF (LATOM.NE.0) THEN
C     USING A REACTION COORDINATE
         CALL PATHS
         RETURN                                                         GDH1095
      ENDIF
C
C     SECTION 2-D GRID
C     ----------------
      IF(ISTEP1.NE.0) THEN                                              DJG0995
         CALL GRID
         RETURN                                                         GDH1095
      ENDIF
C
C     SECTION MINIMISATION OF THE GRADIENT NORM
C     -----------------------------------------
      IF(INLLSQ.NE.0) THEN                                              DJG0995
C     USING NONLINEAR LEAST SQUARE METHOD
         CALL NLLSQ(XPARAM, NVAR, ESCF, GRAD )
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         GOTO 100
      ENDIF
      IF(ISIGMA.NE.0) THEN                                              DJG0995
C     USING MAC IVER KOMORNICKI METHOD
         CALL POWSQ(XPARAM,NVAR,ESCF)
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         GO TO 100
      ENDIF
      IF(IPOWEL.NE.0) THEN                                              DJG0995
C     USING POWELL ALGORITHM
         CALL POWELL(IND,XPARAM,ESCF,GRAD,NVAR)
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         GO TO 100
      ENDIF
      IF(ITRUSG.NE.0) THEN                                              DJG0496
C        USING TRUST REGION ALGORITHM                                   DJG0496
         CALL TRUSTG (XPARAM,ESCF,GRAD,NVAR)                            DJG0496
         IF (ISTOP.NE.0) RETURN                                         DJG0496
         GO TO 100                                                      DJG0496
      ENDIF                                                             DJG0496
C
C     SECTION SECOND DERIVATIVES
C     --------------------------
      IF(IFORCE.NE.0) THEN                                              DJG0995
C     THERMODYNAMIC WITH THE 'FORCE' ALGORITHM
         CALL FORCE
         RETURN                                                         GDH1095
      ENDIF
      IF(ILTRD.NE.0) THEN                                               DJG0995
C     GRADIENT MINIMISATION USING THE FULL NEWTON METHOD
         IND=2
         CALL LTRD(IND,XPARAM,ESCF,GRAD,NVAR)
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         GO TO 100
      ENDIF
      IF(INEWTO.NE.0) THEN                                              DJG0995
C     ENERGY MINIMISATION USING THE FULL NEWTON METHOD
         IND=1
         CALL LTRD(IND,XPARAM,ESCF,GRAD,NVAR)
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         GO TO 100
      ENDIF
C
C     SECTION ENERGY MINIMIZATION OR SINGLE CALCULATION
C     -------------------------------------------------
      IF(ISM50.EQ.0) THEN                                               GDH0597
      IF(ITRUSE.NE.0) THEN                                              DJG0496
C        USING TRUST REGION ALGORITHM                                   DJG0496
         CALL TRUSTE (XPARAM,ESCF,NVAR)                                 DJG0496
         IF (ISTOP.NE.0) RETURN                                         DJG0496
         GO TO 100                                                      DJG0496
      ENDIF                                                             DJG0496
      IF(IBFGS.NE.0.OR.IDFP.NE.0) THEN                                  DJG0395
C        USING QUASI-NEWTON METHOD FOR MINIMIZATION                     DJG0195
         CALL FLEPO(XPARAM, NVAR, ESCF)                                 DJG0195
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         GO TO 100                                                      DJG0195
      ENDIF                                                             DJG0195
      CALL EF(XPARAM,NVAR,ESCF)                                         DJG0195
      ENDIF                                                             GDH0597
      IF (ISTOP.NE.0) RETURN                                            GDH1095
 100  CONTINUE                                                          DJG0395
      CALL WRITES(TIME0, ESCF)                                          DJG0395
      IF(ICR.EQ.1) THEN                                                 GDH0495
        ICR=2                                                           GDH0495
        IF (JCONV.EQ.1.OR.I1SCF.EQ.1) THEN                              GDH0196
           GOTO 1                                                       GDH1295
        ELSE                                                            GDH1295
           WRITE(JOUT,110)                                              GDH1295
           ISTOP=1                                                      GDH1295
           IWHERE=163                                                   GDH1295
           RETURN                                                       GDH1295
         ENDIF                                                          GDH1295
      ENDIF                                                             GDH0495
 110  FORMAT(/,'THE GEOMETRY OPTIMIZATION FAILED IN THE GAS PHASE.'     GDH1295
     .    ,/,'THE HFOPT=CALC CALCULATION CANNOT CONTINUE BECAUSE THE ', GDH0196
     .    /,'CALCULATED HEAT OF FORMATION IN THE GAS PHASE IS FOR',     GDH1295
     .    /,'A NON-OPTIMIZED GEOMETRY.')                                GDH1295
      IF(IPOLAR.NE.0) THEN                                              DJG0995
C        WITH POLARISATION COMPUTATION                                  DJG0395
         CALL POLAR                                                     DJG0395
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      ENDIF                                                             DJG0395
 200  CONTINUE                                                          DJG0495
      RETURN
      END
      BLOCK DATA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'PARAM.i'
      END
