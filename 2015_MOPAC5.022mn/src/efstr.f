      SUBROUTINE EFSTR(XPARAM,FUNCT,IHESS,NTIME,ILOOP,IGTHES,MXSTEP,
     $                 IRECLC,IUPD,DMAX,DDMAX,dmin,TOL2,TOTIME,nvar,
     $                 SCF1,LUPD,ldump,rrscal,donr,gnmin,*)              IR0494 / CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                       
      INCLUDE 'SIZES.i'
      DIMENSION XPARAM(*)                                                       
C                                                                               
      COMMON /ISTOPE/ AMS(120)                                                  
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
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),                  
     1                PMAT(MAXPAR,MAXPAR)                                IR0494
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,rmin,rmax,omin,xlamd,xlamd0,skal,
     $MODE,NSTEP,NEGREQ,IPRNT
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION IPOW(9)               
      LOGICAL RESTRT,SCF1,LDUM,LUPD,rrscal,donr,gnmin 
      CHARACTER*160 KEYWRD                                               IR0494
      CHARACTER CHDOT*1,ZERO*1,NINE*1,CH*1
      DATA CHDOT,ZERO,NINE   /'.','0','9'/                     
      DATA  ICALCN,ZZERO  /0,0.D0/                          
C     GET ALL INITIALIZATION DATA                                               
         NVAR=ABS(NVAR)
         LDUMP=0
         ICALCN=NUMCAL
         LUPD=(INDEX(KEYWRD,' IUPD=0') .EQ. 0)                          IR0494
         RESTRT=(INDEX(KEYWRD,'RESTART') .NE. 0)
         SCF1=(INDEX(KEYWRD,'1SCF') .NE. 0)
         NSTEP=0
         IHESS=0
         LAST=0
         NTIME=0
         ILOOP=1
         IMIN=INDEX(KEYWRD,' EF')
         IF(IMIN.NE.0) THEN
            MODE=0
            IGTHES=0
            IUPD  =2
            NEGREQ=0
            ddmax=0.5d0
         ENDIF
         ITS=INDEX(KEYWRD,' TS')
         IF(ITS.NE.0) THEN
            MODE=1
            IGTHES=1
            IUPD  =1
            NEGREQ=1
            rmin=0.0d0
            rmax=4.0d0
            omin=0.8d0
            ddmax=0.3d0
         ENDIF
         rrscal=.false.
         I=INDEX(KEYWRD,' RSCAL') 
         IF(I.NE.0) rrscal=.true.
         donr=.true.
         I=INDEX(KEYWRD,' NONR') 
         IF(I.NE.0) donr=.false.
         gnmin=.false.
         I=INDEX(KEYWRD,' GNMIN') 
         IF(I.NE.0) gnmin=.true.
         IPRNT=0
         IP=INDEX(KEYWRD,' PRNT=') 
         IF(IP.NE.0) IPRNT=READA(KEYWRD,IP) 
         IF(IPRNT.GT.5)IPRNT=5                                                  
         IF(IPRNT.LT.0)IPRNT=0                                                  
         MXSTEP=100                                                             
         I=INDEX(KEYWRD,' CYCLES=')                                             
         IF(I.NE.0) MXSTEP=READA(KEYWRD,I)                                      
         IF (I.NE.0 .AND. MXSTEP.EQ.0 .AND. IP.EQ.0) IPRNT=3
         IRECLC=999999
         I=INDEX(KEYWRD,' RECALC=')
         IF(I.NE.0) IRECLC=READA(KEYWRD,I)
         I=INDEX(KEYWRD,' IUPD=')
         IF(I.NE.0) IUPD=READA(KEYWRD,I)
         I=INDEX(KEYWRD,' MODE=')
         IF(I.NE.0) MODE=READA(KEYWRD,I)
         DMIN=1.0D-3
         I=INDEX(KEYWRD,' DDMIN=')
         IF(I.NE.0) DMIN=READA(KEYWRD,I)
         DMAX=0.2D0
         I=INDEX(KEYWRD,' DMAX=')
         IF(I.NE.0) DMAX=READA(KEYWRD,I)
         I=INDEX(KEYWRD,' DDMAX=')
         IF(I.NE.0) DDMAX=READA(KEYWRD,I)
         TOL2=1.D+0
         IF(INDEX(KEYWRD,' PREC') .NE. 0) TOL2=5.D-2
         I=INDEX(KEYWRD,' GNORM=')
         IF(I.NE.0) TOL2=READA(KEYWRD,I)
         IF(INDEX(KEYWRD,' LET').EQ.0.AND.TOL2.LT.0.01D0)THEN
            IF (DOPRNT) WRITE(6,'(/,A)')                                CIO
     &      '  GNORM HAS BEEN SET TOO LOW, RESET TO 0.01. '//           CIO
     &      ' SPECIFY LET AS KEYWORD TO ALLOW GNORM LESS THAN 0.01'     CIO
            TOL2=0.01D0
         ENDIF
         I=INDEX(KEYWRD,' HESS=')
         IF(I.NE.0) IGTHES=READA(KEYWRD,I)
         I=INDEX(KEYWRD,' RMIN=')
         IF(I.NE.0) RMIN=READA(KEYWRD,I)
         I=INDEX(KEYWRD,' RMAX=')
         IF(I.NE.0) RMAX=READA(KEYWRD,I)
         I=INDEX(KEYWRD,' OMIN=')
         IF(I.NE.0) OMIN=READA(KEYWRD,I)
C fix timing problem                                                     IR0494
         I=INDEX(KEYWRD,' T=')
         IF(I.NE.0) THEN
            TOTIME=READA(KEYWRD,I)
         ELSE
            TOTIME=MAXTIM
         ENDIF
C   DONE WITH ALL INITIALIZING STUFF.
C   CHECK THAT OPTIONS REQUESTED ARE RESONABLE
         IF((ITS.NE.0).AND.(IUPD.EQ.2))THEN
            IF (DOPRNT) WRITE(6,*)                                      CIO
     &                ' TS SEARCH AND BFGS UPDATE WILL NOT WORK'        CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
         IF((ITS.NE.0).AND.(IGTHES.EQ.0))THEN
            IF (DOPRNT) WRITE(6,*)                                      CIO
     &              ' TS SEARCH REQUIRE BETTER THAN DIAGONAL HESSIAN'   CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
         IF((IGTHES.LT.0).OR.(IGTHES.GT.3))THEN
            IF (DOPRNT) WRITE(6,*)' UNRECOGNIZED HESS OPTION',IGTHES    CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
         IF((OMIN.LT.0.d0).OR.(OMIN.GT.1.d0))THEN
            IF (DOPRNT) WRITE(6,*)' OMIN MUST BE BETWEEN 0 AND 1',OMIN  CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
         IF (RESTRT) THEN
C
C   RESTORE DATA. I INDICATES (HESSIAN RESTART OR OPTIMIZATION
C   RESTART). IF I .GT. 0 THEN HESSIAN RESTART AND I IS LAST
C   STEP CALCULATED IN THE HESSIAN. IF I .LE. 0 THEN J (NSTEP)
C   IN AN OPTIMIZATION HAS BEEN DONE.
C
            IPOW(9)=0
            mtmp=mode
            CALL EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,I,J,BMAT,IPOW,   CSTP(call)
     & *9999)                                                           CSTP(call)
            mode=mtmp
            ILOOP=I
            IF (I .GT. 0) THEN
               IGTHES=4
               NSTEP=J
               IF (DOPRNT)                                              CIO
     &         WRITE(6,'(10X,''RESTARTING HESSIAN AT POINT'',I4)')ILOOP CIO
               IF(DOPRNT.AND.(NSTEP.NE.0))                              CIO
     &              WRITE(6,'(10X,''IN OPTIMIZATION STEP'',I4)') NSTEP  CIO
            ELSE
               NSTEP=J
               IF (DOPRNT)                                               CIO
     &         WRITE(6,'(//10X,''RESTARTING OPTIMIZATION AT STEP'',I4)') CIO
     1NSTEP
               DO 26 I=1,NVAR
   26          GRAD(I)=ZZERO
               CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.,  CSTP(call)
     & *9999)                                                           CSTP(call)
            ENDIF
         ELSE
C   NOT A RESTART, WE NEED TO GET THE GRADIENTS
            DO 30 I=1,NVAR
   30       GRAD(I)=ZZERO
            CALL COMPFG(XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.,     CSTP(call)
     & *9999)                                                           CSTP(call)
         ENDIF
      return
 9999 RETURN 1                                                          CSTP
      end
