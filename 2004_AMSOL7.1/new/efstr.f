      SUBROUTINE EFSTR(XPARAM,FUNCT,IHESS1,NTIME,ILOOP,IGTHES,MXSTEP,    DJG0995
     $IRECLC,IUPD,DMAX,DDMAX,DDMIN,TOLERG,TOTIME,TIME1,TIME2,NVAR,       DJG0495
     $SCF1,LUPD,RRSCAL,DONR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'                                                   DJG0995
      INCLUDE 'FFILES.i'                                                 GDH1095
      DIMENSION XPARAM(*)
C     SUBROUTINE MOVED FROM EF_PORT.F TO EF_MOD.F  4/11/95               DJG0495
      COMMON /GRADNT/ GRAD(MAXPAR),GNFINA
      COMMON /LAST  / LAST
      COMMON /ISTOPE/ AMS(107)
C
C   THE COMMON BLOCK MOLKST HAS BEEN CONVERTED TO MLKSTI AND MLKSTR
C       ONLY MLKSTI USED BY EF
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
C
      COMMON /TIMECM  / TIME0                                            DJG0195
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),
     1                PMAT(MAXPAR,MAXPAR)                                 IR0494
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,RMIN,RMAX,OMIN,XLAMD,XLAMD0,SKAL,
     $MODE,NSTEP,NEGREQ,IPRNT
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV            GDH0897
      COMMON /TRCACM/ NCALF                                              DJG0995
      DIMENSION IPOW(9)
      LOGICAL RESTRT,SCF1,LDUM,LUPD,RRSCAL,DONR,LTRUE,LFALSE             DJG0195
      CHARACTER CHDOT*1,ZERO*1,NINE*1,CH*1
       SAVE
      DATA CHDOT,ZERO,NINE   /'.','0','9'/
      DATA  ZZERO/0.D0/
C
C     GET ALL INITIALIZATION DATA
C
         LTRUE=.TRUE.                                                    DJG0195
         LFALSE=.FALSE.                                                  DJG0195
         NVAR=ABS(NVAR)
cfrj     lupd is a logical which determine whether the trust
c        radius should be allowed to change during the optimization
c        currently it is hardwired to .true., but a proper keyword
c        could be put in, for example a 'NOTRUPD' for disabling it
         lupd=.true.
c        old code had it to depend on IIIUPD, which in turn depends
c        on iupd being given as keyword. This is wrong.
c        LUPD=(IIIUPD.NE.0)                                              DJG0995
c
         RESTRT=(IRESTA.EQ.1.OR.NCALF.EQ.1)                              DJG0496
         SCF1=I1SCF.NE.0                                                 DJG0995
         NSTEP=0
         IHESS1=0                                                        DJG0995
         LAST=0
         NTIME=0
         ILOOP=1
         IF(ITSTAT.NE.0) THEN                                            DJG0995
            MODE=1
            IGTHES=1
            IUPD  =1
            NEGREQ=1
            RMIN=0.25D0
            RMAX=4.0D0
            OMIN=0.8D0
            DMAX=0.1D0
            DDMAX=0.3D0
         ELSE
            MODE=0
            IGTHES=0
            IUPD  =2
            NEGREQ=0
            RMIN=0.0D0
            RMAX=1.0D3
            DMAX=0.2D0
            DDMAX=0.5D0
         ENDIF
         RRSCAL=.FALSE.
         IF(IRSCAL.NE.0) RRSCAL=.TRUE.                                   DJG0995
         DONR=.TRUE.
         IF(INONR.NE.0) DONR=.FALSE.                                     DJG0995
         IPRNT=0
         IF(IPRINT.NE.0) IPRNT=IIPRIN                                    DJG0995
         IF(IPRNT.GT.5)IPRNT=5
         IF(IPRNT.LT.0)IPRNT=0
         MXSTEP=100
         IF(ICYCLE.NE.0) MXSTEP=IICYCL                                   DJG0995
         IF (I.NE.0 .AND. MXSTEP.EQ.0 .AND.IPRINT.EQ.0) IPRNT=3          DJG0995
         IRECLC=999999
         IF(IRECAL.NE.0) IRECLC=IIRECA                                   DJG0995
         IF(IIUPD.NE.0) IUPD=IIIUPD                                      DJG0995
         IF(IMODE.NE.0) MODE=IIMODE                                      DJG0995
         DDMIN=1.0D-2                                                    DJG0295
         IF(IDDMIN.NE.0) DDMIN=FIDDMI                                    DJG0995
         IF(IDMAX.NE.0) DMAX=FIDMAX                                      DJG0995
         IF(IDDMAX.NE.0) DDMAX=FIDDMA                                    DJG0995
         TOLERG=0.45D0                                                   DJG0495
         IF(IGCOMP.NE.0) TOLERG=FIGCOM                                   DJG0995
         IF(ILET.EQ.0.AND.TOLERG.LT.0.01D0)THEN                          DJG0995
            WRITE(JOUT,20)                                               DJG0495
20          FORMAT(/,/,15X,'GCOMP HAS BEEN SET TOO LOW, RESET TO',       DJG0495
     1             ' 0.01.',/,10X,'SPECIFY "LET" AS KEYWORD TO ALLOW',   DJG0495
     2             ' GCOMP LESS THAN 0.01',/,/)                          DJG0495
            TOLERG=0.01D0                                                DJG0495
         ENDIF
         IF(IHESS.NE.0) IGTHES=IIHESS                                    DJG0995
         IF(IRMIN.NE.0) RMIN=FIRMIN                                      DJG0995
         IF(IRMAX.NE.0) RMAX=FIRMAX                                      DJG0995
         IF(IOMIN.NE.0) OMIN=FIOMIN                                      DJG0995
         TIME1=TIME0
         TIME2=TIME1
C   DONE WITH ALL INITIALIZING STUFF.
C   CHECK THAT OPTIONS REQUESTED ARE RESONABLE
         IF(NVAR.GT.(3*NUMAT-6) .AND. NUMAT.GE.3)WRITE(JOUT,25)
   25    FORMAT(/,'*** WARNING! MORE VARIABLES THAN DEGREES OF FREEDOM',
     1/)
         IF((ITSTAT.NE.0).AND.(IUPD.EQ.2))THEN                           DJG0995
            WRITE(JOUT,*)' TS SEARCH AND BFGS UPDATE WILL NOT WORK'
      ISTOP=1                                                            GDH1095
      IWHERE=36                                                          GDH1095
      RETURN                                                             GDH1095
         ENDIF
         IF((ITSTAT.NE.0).AND.(IGTHES.EQ.0))THEN                         DJG0995
        WRITE(JOUT,*)' TS SEARCH REQUIRE BETTER THAN DIAGONAL HESSIAN'
      ISTOP=1                                                            GDH1095
      IWHERE=37                                                          GDH1095
      RETURN                                                             GDH1095
         ENDIF
         IF((IGTHES.LT.0).OR.(IGTHES.GT.5))THEN                          1203BL04
            WRITE(JOUT,*)' UNRECOGNIZED HESS OPTION',IGTHES              1203BL04
      ISTOP=1                                                            GDH1095
      IWHERE=38                                                          GDH1095
      RETURN                                                             GDH1095
         ENDIF
         IF((OMIN.LT.0.D0).OR.(OMIN.GT.1.D0))THEN
            WRITE(JOUT,*)' OMIN MUST BE BETWEEN 0 AND 1',OMIN
      ISTOP=1                                                            GDH1095
      IWHERE=38                                                          GDH1095
      RETURN                                                             GDH1095
         ENDIF
         IF (RESTRT) THEN
C
C   RESTORE DATA. I INDICATES (HESSIAN RESTART OR OPTIMIZATION
C   RESTART). IF I .GT. 0 THEN HESSIAN RESTART AND I IS LAST
C   STEP CALCULATED IN THE HESSIAN. IF I .LE. 0 THEN J (NSTEP)
C   IN AN OPTIMIZATION HAS BEEN DONE.
C
            IPOW(9)=0
            MTMP=MODE
            CALL EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,I,J,BMAT,IPOW,    DJG0495
     1                 NHFLAG)                                           DJG0495
            MODE=MTMP
            K=TT0/1000000.D0
            TIME0=TIME0-TT0+K*1000000.D0
            ILOOP=I
            IF (I .GT. 0) THEN
               IGTHES=4
               NSTEP=J
            WRITE(JOUT,'(10X,''RESTARTING HESSIAN AT POINT'',I4)')ILOOP
            IF(NSTEP.NE.0)WRITE(JOUT,'(10X,''IN OPTIMIZATION STEP'',I4)'
     1)NSTEP
            ELSE
               NSTEP=J
               IF (IRESTA.NE.0)                                          DJG0995
     1            WRITE(JOUT,
     .                    '(//10X,''RESTARTING OPTIMIZATION AT STEP'',   DJG0995
     2                  I4)') NSTEP                                      DJG0995
               DO 26 I=1,NVAR
   26          GRAD(I)=ZZERO
C
C IVAN : FEBRUARY 94
C   WARNING : MOPAC COMPFG IS DIFFERENT FROM AMSOL ONE !
C MOPAC :      CALL COMPFG(XPARAM, .TRUE. , FUNCT, .TRUE., GRAD, .TRUE.)
C
C  There is a problem with the storage of internal coordinates when the input file
C  was in cartesian coordinates.  This problem effects only optimization in
C  solution. i.e. SM5.4A, SM5.4P, SM5.42, etc..
C  This is not an elegant solution, just one that I got to work.
C  IERASE eq 1 if the keyword CART has been erased from the keyword line.
C  Paul Winget Nov. 15, 1999.
      IF (IERASE.NE.0) THEN                                              PDW1199
      XPARAM(1)=XPARAM(4)                                                PDW1199
      XPARAM(2)=XPARAM(7)                                                PDW1199
      XPARAM(3)=XPARAM(8)                                                PDW1199
      XPARAM(4)=XPARAM(10)                                               PDW1199
      DO 27 Q=5,42                                                       PDW1199
      W=Q+6                                                              PDW1199
   27 XPARAM(Q)=XPARAM(W)                                                PDW1199
      ENDIF                                                              PDW1199
C   End of loop to correct coordinate storage.
C
               CALL COMPFG(XPARAM, FUNCT, LTRUE, GRAD, .TRUE.)           DJG0195
      IF (ISTOP.NE.0) RETURN                                             GDH1095
            ENDIF
            IF (NHFLAG.EQ.1) THEN                                        DJG0495
               CALL GETHES(XPARAM,IGTHES,NVAR,ILOOP,TOTIME)              DJG0495
      IF (ISTOP.NE.0) RETURN                                             GDH1095
            ENDIF                                                        DJG0495
         ELSE
C   NOT A RESTART, WE NEED TO GET THE GRADIENTS
            DO 30 I=1,NVAR
   30       GRAD(I)=ZZERO
C
C IVAN : FEBRUARY 94
C   WARNING : MOPAC COMPFG IS DIFFERENT FROM AMSOL ONE !
C MOPAC :      CALL COMPFG(XPARAM, .TRUE. , FUNCT, .TRUE., GRAD, .TRUE.)
C
            CALL COMPFG(XPARAM,  FUNCT, LTRUE, GRAD, .TRUE.)             DJG0195
      IF (ISTOP.NE.0) RETURN                                             GDH1095
         ENDIF
      RETURN
      END


