      SUBROUTINE EF(XPARAM, NVAR, FUNCT,*)                              CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION LAMDA,LAMDA0
      INCLUDE 'SIZES.i'
      DIMENSION XPARAM(MAXPAR)
**********************************************************************
*
*   EF IS A QUASI NEWTON RAPHSON OPTIMIZATION ROUTINE BASED ON
*      Jacs Simons P-RFO algorithm as implemented by Jon Baker
*      (J.COMP.CHEM. 7, 385). Step scaling to keep length within
*      trust radius is taken from Culot et al. (Theo. Chim. Acta 82, 189)
*      The trust radius can be updated dynamically according to Fletcher.
*      Safeguards on valid step for TS searches based on actual/predicted
*      function change and change in TS mode are own modifications
*
*  ON ENTRY XPARAM = VALUES OF PARAMETERS TO BE OPTIMISED.
*           NVAR   = NUMBER OF PARAMETERS TO BE OPTIMISED.
*
*  ON EXIT  XPARAM = OPTIMISED PARAMETERS.
*           FUNCT  = HEAT OF FORMATION IN KCAL/MOL.
*
*  Current version implementing combined NR, P-RFO and QA algorithm
*      together with trust radius update and step rejection was
*      made October 1992 by F.Jensen, Odense, DK
*
**********************************************************************
C
      COMMON /MESAGE/ IFLEPO,ISCF
      COMMON /GEOVAR/ XDUMMY(MAXPAR), NDUM, LOC(2,MAXPAR)               IR0494
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /ISTOPE/ AMS(120)
      COMMON /LAST  / LAST
      COMMON /KEYWRD/ KEYWRD
      COMMON /TIMER / TIME0
      COMMON /GRADNT/ GRAD(MAXPAR),GNFINA
C Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
     3       /MOLKSR/ FRACT
      COMMON /NUMCAL/ NUMCAL
      COMMON /TIMDMP/ TLEFT, TDUMP
      COMMON /SIGMA2/ GNEXT1(MAXPAR), GMIN1(MAXPAR)
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),
     1                PMAT(MAXPAR,MAXPAR)                                IR0494
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,rmin,rmax,omin,xlamd,xlamd0,skal,
     $MODE,NSTEP,NEGREQ,IPRNT
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510

      DIMENSION IPOW(9), EIGVAL(MAXPAR),TVEC(MAXPAR),SVEC(MAXPAR),
     1FX(MAXPAR),HESSC(MAXHES),UC(MAXPAR**2),oldfx(maxpar),
     1oldeig(maxpar),
     $oldhss(maxpar,maxpar),oldu(maxpar,maxpar),ooldf(maxpar)
      DIMENSION BB(MAXPAR,MAXPAR) 

      LOGICAL RESTRT,SCF1,LIMSCF
      LOGICAL LUPD,lts,lrjk,lorjk,rrscal,donr,gnmin                      IR0794
      CHARACTER*160 KEYWRD
      EQUIVALENCE(IPOW(1),IHESS)
      DATA  ICALCN,ZERO,ONE,TWO    /0,0.D0,1.D0,2.D0/
      DATA tmone /1.0d-1/, TMTWO/1.0D-2/, TMSIX/1.0D-06/
      data three/3.0d0/, four/4.0d0/, 
     1pt25/0.25d0/, pt5/0.50d0/, pt75/0.75d0/
      data demin/2.0d-2/, gmin/5.0d0/
C
C       Variables check                                                 IR0494
c
      if(nvar.gt.(3*NUMAT-6)) then
        igeok=index(keywrd,' GEO-OK')
        if ((igeok.eq.0).and.(nvar.ne.(3*NUMAT))) then
             IF (DOPRNT) write(6,'(/,5x,"EF: Redundant variables. ",    CIO
     1       "Eigenvector Following NOT recommended",/,5x,
     2       "Specify GEO-OK if you want to continue",/)')
             stop
        endif
      elseif(nvar.eq.(3*NUMAT)) then
          isXYZ=index(keywrd,' XYZ')
          if((isXYZ.eq.0).and.(igeok.eq.0)) then
           IF(DOPRNT)write(6,'(/,5x,"EF: Exactly 3*N variables.",/,5x,  CIO
     1      "Specify XYZ if you are using CARTESIAN coordinates",/,5x,
     2      "If you use INTERNAL coordinate,  you can specify GEO-OK,",/,
     3      5x,"but Eigenvector Following is NOT recommended",/)')
           stop
          endif
      endif
C
C      Zero out some variables (for MD-type runs)                       IR0894
C
      do 5 i=1,MAXPAR
  5      EIGVAL(i)=0.0d0
      do 6 i=1,MAXPAR
  6      FX(i)=0.0d0
      odd=0.0d0
      odmax=0.0d0
      oolde=0.0d0
C
C     GET ALL INITIALIZATION DATA
C
      IF(ICALCN.NE.NUMCAL) 
     1CALL EFSTR(XPARAM,FUNCT,IHESS,NTIME,ILOOP,IGTHES,MXSTEP,IRECLC,    IR0494
     *           IUPD,DMAX,DDMAX,dmin,TOL2,TOTIME,nvar,SCF1,LUPD,ldump,
     *           rrscal,donr,gnmin,*9999)                                CSTP(call)
      lts=.false.
      if (negreq.eq.1) lts=.true.
      lorjk=.false.
c     osmin is smallest step for which a ts-mode overlap less than omin
c     will be rejected. for updated hessians there is little hope of
c     better overlap by reducing the step below 0.005. for exact hessian
c     the overlap should go toward one as the step become smaller, but
c     don't allow very small steps 
      osmin=0.005d0
      if(ireclc.eq.1)osmin=0.001d0
      IF (SCF1) THEN
         GNFINA=SQRT(DDOT(NVAR,GRAD,1,GRAD,1))                          IR0494
         IFLEPO=1
         RETURN
      ENDIF
C     CHECK THAT GEOMETRY IS NOT ALREADY OPTIMIZED
         RMX=SQRT(DDOT(NVAR,GRAD,1,GRAD,1))                             IR0494
         IF (RMX.LT.TOL2) THEN
            IFLEPO=2
            LAST=1
            RETURN
         ENDIF
C     GET INITIAL HESSIAN. IF ILOOP IS .LE.0 THIS IS AN OPTIMIZATION RESTART 
C     AND HESSIAN SHOULD ALREADY BE AVAILABLE                                   
      IF (ILOOP .GE. 0) CALL GETHES(XPARAM,IGTHES,NVAR,iloop,TOTIME,     IR0594 CSTP(call)
     & *9999)                                                           CSTP(call)

C     START OF MAIN LOOP
C     WE NOW HAVE GRADIENTS AND A HESSIAN. IF THIS IS THE FIRST
C     TIME THROUGH DON'T UPDATE THE HESSIAN. FOR LATER LOOPS ALSO
C     CHECK IF WE NEED TO RECALCULATE THE HESSIAN
      IFLEPO=0
      itime=0
   10 CONTINUE
c     store various things for possibly omin rejection
      do 30 i=1,nvar
         oldfx(i)=fx(i)
         ooldf(i)=oldf(i)
         oldeig(i)=eigval(i)
         do 20 j=1,nvar
         oldhss(i,j)=hess(i,j)
           oldu(i,j)=u(i,j)
20       continue
30    continue
      IF (IHESS.GE.IRECLC.AND.IFLEPO.NE.15) THEN
         ILOOP=1
         IHESS=0
         if (igthes.ne.3)IGTHES=1
         CALL GETHES(XPARAM,IGTHES,NVAR,iloop,TOTIME,*9999)              CSTP(call)
      ENDIF
         IF (IHESS.GT.0) CALL UPDHES(SVEC,TVEC,NVAR,IUPD,*9999)          CSTP(call)
         IF(IPRNT.GE.2) CALL GEOUT(*9999)                               IR0494 CSTP(call)
         IF(IPRNT.GE.2) THEN
            IF (DOPRNT) WRITE(6,'('' XPARAM '')')                       CIO
            IF (DOPRNT) WRITE(6,'(5(2I3,F10.4))')(LOC(1,I),LOC(2,I),    CIO
     1                                XPARAM(I),I=1,NVAR)
            IF (DOPRNT) WRITE(6,'('' GRADIENTS'')')                     CIO
            IF (DOPRNT) WRITE(6,'(3X,8F9.3)')(GRAD(I),I=1,NVAR)         CIO
         ENDIF
C
C        PRINT RESULTS IN CYCLE
         GNFINA=SQRT(DDOT(NVAR,GRAD,1,GRAD,1))                          IR0494
      TIME2=SECOND()
      if (itime.eq.0) time1=time0
      TSTEP=TIME2-TIME1
      IF (TSTEP.LT.ZERO)TSTEP=ZERO
      TLEFT=TOTIME-TIME2                                                IR0494
      TIME1=TIME2
      itime=itime+1
      IF (DOPRNT) WRITE(6,40) NSTEP+1, TSTEP, TLEFT, GNFINA, FUNCT      CIO
   40 FORMAT(' CYCLE:',I4,' TIME:',F7.2,' TIME LEFT:',F9.1,
     1       ' GRAD.:',F10.3,' HEAT:',G13.7)
      IF (TLEFT .LT. TSTEP*TWO) GOTO 280
      IHESS=IHESS+1
      NSTEP=NSTEP+1
C
C        TEST FOR CONVERGENCE
C
      RMX=SQRT(DDOT(NVAR,GRAD,1,GRAD,1))                                IR0494
      IF (RMX.LT.TOL2)GOTO 250
      OLDE  = FUNCT
      oldgn = rmx
      DO 60 I=1,NVAR
         OLDF(I)=GRAD(I)
60    CONTINUE
C
C     if the optimization is in cartesian coordinates, we should remove
C     translation and rotation modes. Possible problem if run is in
C     internal but with exactly 3*natoms variable (i.e. dummy atoms
C     are also optimized).
      isXYZ=index(keywrd,' XYZ')                                        LF0610 (else may not be defined yet)
      if ( isXYZ.ne.0 ) then                                            IR0494
        IF (DOPRNT)                                                     IR0494 / CIO
     &    write(6,'(10x,"Optimizing using CARTESIAN coordinates..")')   IR0494 / CIO
        call prjfc(hess,xparam,nvar,*9999)                               CSTP(call)
      endif
      IJ=0
      DO 80 I=1,NVAR
         DO 80 J=1,I
            IJ=IJ+1
            HESSC(IJ)=HESS(J,I)
   80 CONTINUE
c#      write(6,*) "EF calls RSP from 208."
      CALL RSP(HESSC,NVAR,NVAR,EIGVAL,UC,*9999)                          CSTP(call)
      IJ=0
      DO 90 I=1,NVAR
         IF (ABS(EIGVAL(I)).LT.TMSIX) EIGVAL(I)=ZERO
         DO 90 J=1,NVAR
            IJ=IJ+1
            U(J,I)=UC(IJ)
   90 CONTINUE
      IF (IPRNT.GE.3) CALL PRTHES(EIGVAL,NVAR,*9999)                     CSTP(call)
      IF (MXSTEP.EQ.0) nstep=0
      IF (MXSTEP.EQ.0) GOTO 280                                                 

      NEG=0                                                                     
      DO 100 I=1,NVAR                                                           
         IF (EIGVAL(I) .LT. ZERO)NEG=NEG+1                                     
  100 CONTINUE                                                                  
      IF (DOPRNT.AND.(IPRNT.GE.1))WRITE(6,110)NEG,(eigval(i),i=1,neg)   CIO
  110 FORMAT(/,10X,'HESSIAN HAS',I3,' NEGATIVE EIGENVALUE(S)',6f7.1,/)
c     if an eigenvalue has been zero out it is probably one of the T,R modes
c     in a cartesian optimization. zero corresponding fx to allow formation
c     of step without these contributions. a more safe criteria for deciding
c     whether this actually is a cartesian optimization should be put in 
c     some day...
      DO 120 I=1,NVAR                                                           
         FX(I)=DDOT(NVAR,U(1,I),1,GRAD,1)                               IR0494
         if (abs(eigval(i)).eq.zero) fx(i)=zero
  120 CONTINUE                                                                  
c     form geometry step d
130   CALL FORMD(EIGVAL,FX,NVAR,DMAX,OSMIN,lts,LRJK,lorjk,rrscal,donr)  
c     if lorjk is true, then ts mode overlap is less than omin, reject prev step
      if (lorjk) then
         if (DOPRNT.AND.(iprnt.ge.1))                                   CIO
     &                    write(6,*)'      Now undoing previous step'   CIO
         dmax=odmax
         dd=odd
         olde=oolde
         do 131 i=1,nvar
            fx(i)=oldfx(i)
                   oldf(i)=ooldf(i)
            eigval(i)=oldeig(i)
            do 132 j=1,nvar
               hess(i,j)=oldhss(i,j)
               u(i,j)=oldu(i,j)
132         CONTINUE
131      CONTINUE
         DO 140 I=1,NVAR
            XPARAM(I)=XPARAM(I)-D(I)
            K=LOC(1,I)
            L=LOC(2,I)
            GEO(L,K)=XPARAM(I)
140      CONTINUE
         IF(NDEP.NE.0) CALL SYMTRY(*9999)                                CSTP(call)
         dmax=min(dmax,dd)/two
         odmax=dmax
         odd=dd
         nstep=nstep-1
         if (dmax.lt.dmin) goto 230
      if (DOPRNT.AND.(iprnt.ge.1)) write(6,*)                           CIO
     1'      Finish undoing, now going for new step'
         goto 130
      endif
C
C  FORM NEW TRIAL XPARAM AND GEO
C
      DO 150 I=1,NVAR
         XPARAM(I)=XPARAM(I)+D(I)
         K=LOC(1,I)
         L=LOC(2,I)
         GEO(L,K)=XPARAM(I)
  150 CONTINUE
      IF(NDEP.NE.0) CALL SYMTRY(*9999)                                   CSTP(call)
C
C     COMPARE PREDICTED E-CHANGE WITH ACTUAL 
C
      depre=zero
      imode=1
      if (mode.ne.0)imode=mode
      do 160 i=1,nvar
         xtmp=xlamd
         if (lts .and. i.eq.imode) xtmp=xlamd0
         if (abs(xtmp-eigval(i)).lt.tmtwo) then
         ss=zero
         else
         ss=skal*fx(i)/(xtmp-eigval(i))
         endif
         frodo=ss*fx(i) + pt5*ss*ss*eigval(i)
c        write(6,88)i,fx(i),ss,xtmp,eigval(i),frodo
         depre=depre+frodo
160   continue
c88   format(i3,f10.3,f10.6,f10.3,4f10.6)
C
C     GET GRADIENT FOR NEW GEOMETRY 
C
      CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.,*9999)     CSTP(call)
      IF(GNMIN) GNTEST=SQRT(DDOT(NVAR,GRAD,1,GRAD,1))                   IR0494
      DEACT = FUNCT-OLDE
      RATIO = DEACT/DEPRE
      if(DOPRNT.AND.(iprnt.ge.1))WRITE(6,170)DEACT,DEPRE,RATIO          CIO
  170 FORMAT(5X,'ACTUAL, PREDICTED ENERGY CHANGE, RATIO',2F10.3,F10.5)
      lrjk=.false.
C     if this is a minimum search, don't allow the energy to raise
      if (.not.lts .and. funct.gt.olde) then
         if (DOPRNT.AND.(iprnt.ge.1))write(6,180)funct,min(dmax,dd)/two CIO
180      format(1x,'energy raises ',f10.4,' rejecting step, ',
     $             'reducing dmax to',f7.4)
         lrjk=.true.
      endif
      if (gnmin .and. gntest.gt.oldgn) then
         if(DOPRNT.AND.(iprnt.ge.1))write(6,181)gntest,min(dmax,dd)/two CIO
181      format(1x,'gradient norm raises ',f10.4,' rejecting step, ',
     $             'reducing dmax to',f7.4)
         lrjk=.true.
      endif
      if (lts .and. (ratio.lt.rmin .or. ratio.gt.rmax) .and.
     $(abs(depre).gt.demin .or. abs(deact).gt.demin)) then
         if(DOPRNT.AND.iprnt.ge.1)write(6,190)min(dmax,dd)/two          CIO
190   format(1x,'unacceptable ratio,',
     $          ' rejecting step, reducing dmax to',f7.4)
         lrjk=.true.
      endif
      if (lrjk) then
         DO 200 I=1,NVAR
            XPARAM(I)=XPARAM(I)-D(I)
            K=LOC(1,I)
            L=LOC(2,I)
            GEO(L,K)=XPARAM(I)
200      CONTINUE
         IF(NDEP.NE.0) CALL SYMTRY(*9999)                               CSTP(call)
         dmax=min(dmax,dd)/two
         if (dmax.lt.dmin) goto 230
         goto 130
      endif
      IF(DOPRNT.AND.(IPRNT.GE.1)) WRITE(6,210)DD                        CIO
  210 FORMAT(5X,'STEPSIZE USED IS',F9.5)
      IF(IPRNT.GE.2) THEN
         IF (DOPRNT) WRITE(6,'('' CALCULATED STEP'')')                  CIO
         IF (DOPRNT) WRITE(6,'(3X,8F9.5)')(D(I),I=1,NVAR)               CIO
      ENDIF
C
C     POSSIBLE USE DYNAMICAL TRUST RADIUS
      odmax=dmax
      odd=dd
      oolde=olde
      IF (LUPD .and. ( (RMX.gt.gmin) .or.
     $    (abs(depre).gt.demin .or. abs(deact).gt.demin) ) ) THEN
c     Fletcher recommend dmax=dmax/4 and dmax=dmax*2
c     these are are a little more conservative since hessian is being updated
c     don't reduce trust radius due to ratio for min searches
      if (lts .and. ratio.le.tmone .or. ratio.ge.three)
     $    dmax=min(dmax,dd)/two
      if (lts .and. ratio.ge.pt75 .and. ratio.le.(four/three) 
     $                  .and. dd.gt.(dmax-tmsix)) 
     $   dmax=dmax*sqrt(two)
c     allow wider limits for increasing trust radius for min searches
      if (.not.lts .and. ratio.ge.pt5 
     $                  .and. dd.gt.(dmax-tmsix)) 
     $   dmax=dmax*sqrt(two)
c     be brave if  0.90 < ratio < 1.10 ...
      if (abs(ratio-one).lt.tmone) dmax=dmax*sqrt(two)
      dmax=max(dmax,dmin-tmsix)
      dmax=min(dmax,ddmax)
      ENDIF
c     allow stepsize up to 0.1 in the end-game where changes are less 
c     than demin and gradient is less than gmin
      IF (LUPD .and. RMX.lt.gmin .and.
     $   (abs(depre).lt.demin .and. abs(deact).lt.demin) )
     $    dmax=max(dmax,tmone)
      if(DOPRNT.AND.(iprnt.ge.1)) WRITE(6,220) DMAX                     CIO
 220  FORMAT(5X,'CURRENT TRUST RADIUS = ',F7.5)                  
230   if (dmax.lt.dmin) then
         IF (DOPRNT) write(6,240)dmin                                   CIO
240      format(/,5x,'TRUST RADIUS NOW LESS THAN ',F7.5,' OPTIMIZATION',
     $   ' TERMINATING',/,5X,
     1' GEOMETRY MAY NOT BE COMPLETELY OPTIMIZED')
         goto 270
      endif

C     CHECK STEPS AND ENOUGH TIME FOR ANOTHER PASS
      if (nstep.ge.mxstep) goto 280
C     IN USER UNFRIENDLY ENVIROMENT, SAVE RESULTS EVERY 1 CPU HRS
      ITTEST=AINT((TIME2-TIME0)/TDUMP)
      IF (ITTEST.GT.NTIME) THEN
         LDUMP=1
         NTIME=MAX(ITTEST,(NTIME+1))
         IPOW(9)=2
         TT0=SECOND()-TIME0
         CALL EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,-NSTEP,NSTEP,BMAT,
     1              IPOW,*9999)                                          CSTP(call)
      ELSE
         LDUMP=0
      ENDIF
C     RETURN FOR ANOTHER CYCLE
      GOTO 10                                                                  
C
C     ****** OPTIMIZATION TERMINATION ******
C
  250 CONTINUE
      IF (DOPRNT) WRITE(6,260)RMX,TOL2                                  CIO
  260 FORMAT(/,5X,'RMS GRADIENT =',F9.5,'  IS LESS THAN CUTOFF =',
     1F9.5,//)
  270 IFLEPO=15
      LAST=1
C     SAVE HESSIAN ON FILE 9
      IPOW(9)=2
      TT0=SECOND()-TIME0
      CALL EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,-NSTEP,NSTEP,BMAT,
     1           IPOW,*9999)                                             CSTP(call)
C     CALL COMPFG TO CALCULATE ENERGY FOR FIXING MO-VECTOR BUG
      CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .FALSE.,*9999)    CSTP(call)
      RETURN
  280 CONTINUE
C     WE RAN OUT OF TIME or too many iterations. DUMP RESULTS
      IF (TLEFT .LT. TSTEP*TWO) THEN
         IF (DOPRNT) WRITE(6,290)                                       CIO
  290    FORMAT(/,5X,'NOT ENOUGH TIME FOR ANOTHER CYCLE')
      ENDIF
      IF (nstep.ge.mxstep) THEN
         IF (DOPRNT) WRITE(6,300)                                       CIO
  300    FORMAT(/,5X,'EXCESS NUMBER OF OPTIMIZATION CYCLES')
      ENDIF
      IPOW(9)=1
      TT0=SECOND()-TIME0
      CALL EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,-NSTEP,NSTEP,BMAT,
     1           IPOW,*9999)                                             CSTP(call)
      RETURN
 9999 RETURN 1                                                          CSTP
      END
