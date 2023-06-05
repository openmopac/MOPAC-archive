      SUBROUTINE GETHES(XPARAM,IGTHES,NVAR,iloop,TOTIME,*)              CSTP 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                       
      INCLUDE 'SIZES.i'
C     GET THE HESSIAN. DEPENDING ON IGTHES WE GET IT FROM :                     
C                                                                               
C      0 : DIAGONAL MATRIX, DGHSX*I (DEFAULT FOR MIN-SEARCH)                    
C      1 : CALCULATE IT NUMERICALLY (DEFAULT FOR TS-SEARCH)                     
C      2 : READ IN FROM FTN009                                                  
C      3 : CALCULATE IT BY DOUBLE NUMERICAL DIFFERENTIATION
C      4 : READ IN FROM FTN009 (DURING RESTART, PARTLY OR WHOLE,                
C          ALREADY DONE AT THIS POINT)                                          
      COMMON /GEOVAR/ XDUMMY(MAXPAR), NDUM, LOC(2,MAXPAR)               IR0494
      COMMON /GEOM  / GEO(3,NUMATM)                                             
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)         
      COMMON /LAST  / LAST                                                      
      COMMON /KEYWRD/ KEYWRD                                                    
      COMMON /TIMER/ TIME0                                                     
      COMMON /GRADNT/ GRAD(MAXPAR),GNFINA                                       
C Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
     3       /MOLKSR/ FRACT
      COMMON /NUMCAL/ NUMCAL                                                    
      COMMON /SIGMA2/ GNEXT1(MAXPAR), GMIN1(MAXPAR)                             
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),                  
     1                PMAT(MAXPAR,MAXPAR)                                IR0494
      COMMON /TIMDMP/ TLEFT, TDUMP
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,rmin,rmax,omin,xlamd,xlamd0,skal,
     $MODE,NSTEP,NEGREQ,IPRNT
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION IPOW(9), EIGVAL(MAXPAR),TVEC(MAXPAR),SVEC(MAXPAR),              
     *FX(MAXPAR),HESSC(MAXHES),UC(MAXPAR**2)                                    
      DIMENSION XPARAM(*),tmp(150,150)
      LOGICAL RESTRT,SCF1,LDUM 
      CHARACTER*160 KEYWRD                                               IR0494
      CHARACTER CHDOT*1,ZERO*1,NINE*1,CH*1
      DATA CHDOT,ZERO,NINE  /'.','0','9'/                     
      DATA  ICALCN,ZZERO,ONE,TWO    /0,0.D0,1.D0,2.D0/                          
C
      DATA DGHSS,DGHSA,DGHSD /1000.d0,500.d0,200.d0/          
      DATA XINC /1.d-3/          
C     DGHSX IS HESSIAN DIAGONAL FOR IGTHES=0 (STRETCHING, ANGLE,
C     DIHEDRAL).  THE VALUES SHOULD BE 'OPTIMUM' FOR CYCLOHEXANONE
C     XINC IS STEPSIZE FOR HESSIAN CALCULATION. TESTS SHOWS THAT IT SHOULD
C     BE IN THE RANGE 10(-2) TO 10(-4). 10(-3) APPEARS TO BE 
C     A REASONABLE COMPROMISE BETWEEN ACCURACY AND NUMERICAL PROBLEMS
      IF (IGTHES.EQ.0) THEN
         IF (DOPRNT) WRITE(6,60)                                        CIO
   60    FORMAT(/,10X,'DIAGONAL MATRIX USED AS START HESSIAN',/)
         DO 70 I=1,NVAR
            DO 70 J=1,NVAR
               HESS(I,J)=ZZERO
   70    CONTINUE
         IJ=1
         DO 80 J=1,NUMATM
            DO 80 I=1,3
               IF (LOC(2,IJ).EQ.I.AND.LOC(1,IJ).EQ.J)THEN
                  IF (I.EQ.1)HESS(IJ,IJ)=DGHSS
                  IF (I.EQ.2)HESS(IJ,IJ)=DGHSA
                  IF (I.EQ.3)HESS(IJ,IJ)=DGHSD
                  IJ=IJ+1
               ENDIF
   80    CONTINUE
         IJ=IJ-1
         IF(IJ.NE.NVAR)                                                 CIO
     &      WRITE(*,*)'ERROR IN IGTHES=0,IJ,NVAR',IJ,NVAR               CIO
      ENDIF
C
      IF (IGTHES.EQ.2) THEN
         IF (DOPRNT) WRITE(6,100)                                       CIO
  100    FORMAT(/,10X,'HESSIAN READ FROM DISK',/)
         IPOW(9)=0
C        USE DUMMY ARRAY FOR CALL EXCEPT FOR HESSIAN
C        TEMPORARY SET NALPHA = 0, THEN WE CAN READ HESSIAN FROM RHF
C        RUN FOR USE IN SAY UHF RUNS
C        ALSO SAVE MODE, TO ALLOW FOLLOWING A DIFFERENT MODE THAN THE ONE
C        CURRENTLY ON RESTART FILE
         nxxx=nalpha
         nalpha=0
         mtmp=mode
         CALL EFSAV(TDM,HESS,FDMY,GNEXT1,GMIN1,PMAT,IIDUM,J,BMAT,IPOW,  CSTP(call)
     & *9999)                                                           CSTP(call)
         nalpha=nxxx
         mode=mtmp
         nstep=0
      ENDIF
      IF((IGTHES.EQ.1).OR.(IGTHES.EQ.3).OR.(IGTHES.EQ.4))THEN
C       IF IGTHES IS .EQ. 4, THEN THIS IS A HESSIAN RESTART.
C       USE GNEXT1 AND DUMMY FOR CALLS TO COMPFG DURING HESSIAN
C       CALCULATION
         IF (DOPRNT.AND.(IGTHES.EQ.1)) WRITE(6,190)                     CIO
  190    FORMAT(/,10X,'HESSIAN CALCULATED NUMERICALLY',/)
         IF (DOPRNT.AND.(IGTHES.EQ.3)) WRITE(6,191)                     CIO
  191    FORMAT(/,10X,'HESSIAN CALCULATED DOUBLE NUMERICALLY',/)
            IF(DOPRNT.AND.(IPRNT.GE.5)) WRITE(6,'(I3,12(8F9.4,/3X))')   CIO
     1    0,(Grad(IF),IF=1,NVAR)
         TIME1=SECOND()
         TSTORE=TIME1
         DO 210 I=ILOOP,NVAR
            XPARAM(I)=XPARAM(I) + XINC
            CALL COMPFG(XPARAM, .TRUE., DUMMY, .TRUE., GNEXT1, .TRUE.,  CSTP(call)
     & *9999)                                                           CSTP(call)
            IF(DOPRNT.AND.(IPRNT.GE.5)) WRITE(6,'(I3,12(8F9.4,/3X))')   CIO
     1    I,(GNEXT1(IF),IF=1,NVAR)
            XPARAM(I)=XPARAM(I) - XINC
            if (igthes.eq.3) then
            XPARAM(I)=XPARAM(I) - XINC
            CALL COMPFG(XPARAM, .TRUE., DUMMY, .TRUE., GMIN1, .TRUE.,   CSTP(call)
     & *9999)                                                           CSTP(call)
            IF(DOPRNT.AND.(IPRNT.GE.5)) WRITE(6,'(I3,12(8F9.4,/3X))')   CIO
     1    -I,(GMIN1(IF),IF=1,NVAR)
            XPARAM(I)=XPARAM(I) + XINC
            DO 199 J=1,NVAR
  199       HESS(I,J)= (GNEXT1(J)-GMIN1(J))/(XINC+XINC)
            else
            DO 200 J=1,NVAR
  200       HESS(I,J)= (GNEXT1(J)-GRAD(J))/XINC
            endif
            TIME2=SECOND()
            TSTEP=TIME2-TIME1
            TLEFT=TOTIME-TIME2                                           IR0494
            TIME1=TIME2
            IF( TLEFT .LT. TSTEP*TWO) THEN
C
C  STORE PARTIAL HESSIAN PATRIX
C  STORE GRADIENTS FOR GEOMETRY AND ILOOP AS POSITIVE
               IF (DOPRNT) THEN                                         CIO
               WRITE(6,'(A)')' NOT ENOUGH TIME TO COMPLETE HESSIAN'
               WRITE(6,'(A,I4)')' STOPPING IN HESSIAN AT COORDINATE:',I
               ENDIF                                                    CIO
               IPOW(9)=1
               TT0=SECOND()-TIME0
               CALL EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,I,NSTEP,BMAT,
     1IPOW,*9999)                                                        CSTP(call)
      RETURN 1                                                           CSTP (stop)
            ENDIF
  210    CONTINUE
C     fix last entry in geo array, this is currently at value-xinc
         K=LOC(1,nvar)
         L=LOC(2,nvar)
         GEO(L,K)=XPARAM(nvar)
         IF(NDEP.NE.0) CALL SYMTRY(*9999)                                CSTP(call)
c        add all time used back to tleft, this will then be subtracted
c        again in main ef routine
         TIME2=SECOND()
         TSTEP=TIME2-TSTORE
         TLEFT=TOTIME-TIME2                                              IR0494
      ENDIF
C
C     SYMMETRIZE HESSIAN
      DO 220 I=1,NVAR
         DO 220 J=1,I-1
            HESS(I,J)=(HESS(I,J)+HESS(J,I))/TWO
            HESS(J,I)=HESS(I,J)
  220 CONTINUE
      RETURN
 9999 RETURN 1                                                          CSTP
      END
