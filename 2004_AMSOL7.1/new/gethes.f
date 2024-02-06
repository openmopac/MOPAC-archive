      SUBROUTINE GETHES(XPARAM,IGTHES,NVAR,ILOOP,TOTIME)                 1203BL04
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                1203BL04
      INCLUDE 'SIZES.i'                                                  1203BL04
      INCLUDE 'FFILES.i'                                                 GDH1095
      PARAMETER (MSIZE=NUMATM*(NUMATM-1)/2, MXPAR2=MAXPAR**2)
C     GET THE HESSIAN. DEPENDING ON IGTHES WE GET IT FROM :
C
C      0 : DIAGONAL MATRIX, DGHSX*I (DEFAULT FOR MIN-SEARCH)
C      1 : CALCULATE IT NUMERICALLY (DEFAULT FOR TS-SEARCH)
C      2 : READ IN FROM FTN009
C      3 : CALCULATE IT BY DOUBLE NUMERICAL DIFFERENTIATION
C      4 : READ IN FROM FTN009 (DURING RESTART, PARTLY OR WHOLE,
C          ALREADY DONE AT THIS POINT)
C      5 : MODEL HESSIAN A LA R. LINDH.                                  DL1104
C GEOVAR VARIABLES CHANGED ORDER IN AMSOL
C     COMMON /GEOVAR/ NDUM,LOC(2,MAXPAR), IDUMY, XARAM(MAXPAR)
C
      COMMON /GEOVAR/ XDUMMY(MAXPAR),NDUM,LOC(2,MAXPAR),IDUMY
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /LAST  / LAST
      COMMON /TIMECM  / TIME0                                            DJG0195
      COMMON /GRADNT/ GRAD(MAXPAR),GNFINA
      COMMON /HBODY2/ RG(3,MSIZE),BJACOB(MAXPAR**2),COORD(3,NUMATM)
      COMMON /SCRCHR/ WORK(MAXPAR**2)
C
C   THE COMMON BLOCK MOLKST HAS BEEN CONVERTED TO MLKSTI AND MLKSTR
C               ONLY MLKSTI USED BY EF
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /SIGMA2/ GNEXT1(MAXPAR), GMIN1(MAXPAR)
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),
     1                PMAT(MAXPAR,MAXPAR)                                IR0494
      COMMON /TIMDMP/ TLEFT, TDUMP
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,RMIN,RMAX,OMIN,XLAMD,XLAMD0,SKAL,
     $MODE,NSTEP,NEGREQ,IPRNT
C
      DIMENSION IPOW(9)
      DIMENSION XPARAM(*),TMP(150,150)
      LOGICAL RESTRT,SCF1,LDUM,LTRUE,LFALSE
      CHARACTER CHDOT*1,ZERO*1,NINE*1,CH*1
       SAVE
      DATA CHDOT,ZERO,NINE  /'.','0','9'/
      DATA  ZZERO,ONE,TWO    /0.D0,1.D0,2.D0/
C
      DATA DGHSS,DGHSA,DGHSD /1000.D0,500.D0,200.D0/
      DATA XINC /1.D-3/
C     DGHSX IS HESSIAN DIAGONAL FOR IGTHES=0 (STRETCHING, ANGLE,
C     DIHEDRAL).  THE VALUES SHOULD BE 'OPTIMUM' FOR CYCLOHEXANONE
C     XINC IS STEPSIZE FOR HESSIAN CALCULATION. TESTS SHOWS THAT IT  SHOULD
C     BE IN THE RANGE 10(-2) TO 10(-4). 10(-3) APPEARS TO BE
C     A REASONABLE COMPROMISE BETWEEN ACCURACY AND NUMERICAL PROBLEMS
      LTRUE=.TRUE.                                                       DJG0195
      LFALSE=.FALSE.                                                     DJG0195
C  DUMMY VARIABLE FOR EFSAV CALL                                         DJG0495
      NHFLAG=0                                                           DJG0495
      IF (IGTHES.EQ.0) THEN
         WRITE(JOUT,10)
   10    FORMAT(/,10X,'DIAGONAL MATRIX USED AS HESSIAN',/)               DJG0495
         DO 20 I=1,NVAR
            DO 20 J=1,NVAR
               HESS(I,J)=ZZERO
   20    CONTINUE
         IJ=1
         DO 30 J=1,NUMATM
            DO 30 I=1,3
               IF (LOC(2,IJ).EQ.I.AND.LOC(1,IJ).EQ.J)THEN
                  IF (I.EQ.1)HESS(IJ,IJ)=DGHSS
                  IF (I.EQ.2)HESS(IJ,IJ)=DGHSA
                  IF (I.EQ.3)HESS(IJ,IJ)=DGHSD
                  IJ=IJ+1
               ENDIF
   30    CONTINUE
         IJ=IJ-1
         IF(IJ.NE.NVAR)WRITE(JOUT,*)
     .            'ERROR IN IGTHES=0,IJ,NVAR',IJ,NVAR
      ENDIF
C
      IF (IGTHES.EQ.5) THEN                                              DL1104
         WRITE(JOUT,40)                                                  DL1104
   40    FORMAT(/,10X,'HESSIAN FROM MODEL A LA R. LINDH',/)              DL1104
         NUMAT3=3*NUMAT                                                  DL1104
         CALL HLINDH (COORD,NUMAT3,HESS)                                 DL1104
C        SET UP THE JACOBIAN MATRIX (NO MATTER "IDERIN").                DL1104
         CALL JCARIN (COORD,XPARAM,.TRUE.,BJACOB,NCOL)                   DL1104
C        SET HESSIAN IN PARAMETER SPACE, VIA JACOBIAN BJACOB.            DL1104
         CALL MXM (BJACOB,NVAR,HESS,NUMAT3,WORK,NUMAT3)                  DL1104
         DO 60 J=1,NVAR                                                  DL1104
         DO 60 I=1,J                                                     DL1104
         HESS(I,J)=0D0                                                   DL1104
         DO 50 K=1,NUMAT3                                                DL1104
         HESS(I,J)=HESS(I,J)+WORK(I+NVAR*(K-1))*BJACOB(J+NVAR*(K-1))     DL1104
   50    CONTINUE                                                        DL1104
         HESS(J,I)=HESS(I,J)                                             DL1104
   60    CONTINUE                                                        DL1104
      ENDIF                                                              DL1104
C
      IF (IGTHES.EQ.2) THEN
         WRITE(JOUT,100)
  100    FORMAT(/,10X,'HESSIAN READ FROM DISK',/)
         IPOW(9)=0
C
C        USE DUMMY ARRAY FOR CALL EXCEPT FOR HESSIAN
C        TEMPORARY SET NALPHA = 0, THEN WE CAN READ HESSIAN FROM RHF
C        RUN FOR USE IN SAY UHF RUNS
C        ALSO SAVE MODE, TO ALLOW FOLLOWING A DIFFERENT MODE THAN THE ONE
C        CURRENTLY ON RESTART FILE
C
         NXXX=NALPHA
       NALPHA=0
       MTMP=MODE
       CALL EFSAV(TDM,HESS,FDMY,GNEXT1,GMIN1,PMAT,IIDUM,J,BMAT,IPOW,     DJG0495
     1              NHFLAG)                                              DJG0495
      IF (ISTOP.NE.0) RETURN                                             GDH1095
       NALPHA=NXXX
       MODE=MTMP
       NSTEP=0
      ENDIF
      IF((IGTHES.EQ.1).OR.(IGTHES.EQ.3).OR.(IGTHES.EQ.4))THEN
C
C       IF IGTHES IS .EQ. 4, THEN THIS IS A HESSIAN RESTART.
C       USE GNEXT1 AND DUMMY FOR CALLS TO COMPFG DURING HESSIAN
C       CALCULATION
C
         IF (IGTHES.EQ.1)WRITE(JOUT,190)
  190    FORMAT(/,10X,'HESSIAN CALCULATED NUMERICALLY',/)
         IF (IGTHES.EQ.3)WRITE(JOUT,191)
  191    FORMAT(/,10X,'HESSIAN CALCULATED DOUBLE NUMERICALLY',/)
         CALL PORCPU(TIME1)
         TSTORE=TIME1
         DO 210 I=ILOOP,NVAR
            XPARAM(I)=XPARAM(I) + XINC
            CALL COMPFG(XPARAM, DUMMY, LTRUE, GNEXT1, .TRUE.)            DJG0195
      IF (ISTOP.NE.0) RETURN                                             GDH1095
            XPARAM(I)=XPARAM(I) - XINC
          IF (IGTHES.EQ.3) THEN
            XPARAM(I)=XPARAM(I) - XINC
            CALL COMPFG(XPARAM, DUMMY, LTRUE, GMIN1, .TRUE.)             DJG0195
      IF (ISTOP.NE.0) RETURN                                             GDH1095
            XPARAM(I)=XPARAM(I) + XINC
            DO 199 J=1,NVAR
  199       HESS(I,J)= (GNEXT1(J)-GMIN1(J))/(XINC+XINC)
          ELSE
            DO 200 J=1,NVAR
  200       HESS(I,J)= (GNEXT1(J)-GRAD(J))/XINC
          ENDIF
            CALL PORCPU(TIME2)
            TSTEP=TIME2-TIME1
            TLEFT=TLEFT-TSTEP
            TIME1=TIME2
            IF( TLEFT .LT. TSTEP*TWO) THEN
C
C  STORE PARTIAL HESSIAN PATRIX
C  STORE GRADIENTS FOR GEOMETRY AND ILOOP AS POSITIVE
               WRITE(JOUT,'(A)')' NOT ENOUGH TIME TO COMPLETE HESSIAN'
               WRITE(JOUT,
     .             '(A,I4)')' STOPPING IN HESSIAN AT COORDINATE:',I
               IPOW(9)=1
               CALL PORCPU(TT0)
               TT0=TT0-TIME0
               CALL EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,I,NSTEP,BMAT,
     1                    IPOW,NHFLAG)                                   DJG0495
      IF (ISTOP.NE.0) RETURN                                             GDH1095
               ISTOP=1                                                   GDH1095
               IWHERE=35                                                 GDH1095
               RETURN                                                    GDH1095
            ENDIF
  210    CONTINUE
C     FIX LAST ENTRY IN GEO ARRAY, THIS IS CURRENTLY AT VALUE-XINC
         K=LOC(1,NVAR)
         L=LOC(2,NVAR)
         GEO(L,K)=XPARAM(NVAR)
         IF(NDEP.NE.0) CALL SYMTRY
C        ADD ALL TIME USED BACK TO TLEFT, THIS WILL THEN BE SUBTRACTED
C        AGAIN IN MAIN EF ROUTINE
         CALL PORCPU(TIME2)
         TSTEP=TIME2-TSTORE
         TLEFT=TLEFT+TSTEP
      ENDIF
C
C     SYMMETRIZE HESSIAN
      DO 220 I=1,NVAR
C$DIR NO_RECURRENCE
         DO 220 J=1,I-1
            HESS(I,J)=(HESS(I,J)+HESS(J,I))/TWO
            HESS(J,I)=HESS(I,J)
  220 CONTINUE
      RETURN
      END

