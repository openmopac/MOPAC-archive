      SUBROUTINE FORMD(EIGVAL,FX,NVAR,DMAX,
     1osmin,ts,lrjk,lorjk,rrscal,donr,*)                                CSTP
C     This version forms geometry step by either pure NR, P-RFO or QA
C     algorithm, under the condition that the steplength is less than dmax
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION LAMDA,lamda0
      INCLUDE 'SIZES.i'
      logical ts,rscal,frodo1,frodo2,lrjk,lorjk,rrscal,donr              IR0794
      DIMENSION EIGVAL(MAXPAR),FX(MAXPAR)                                       
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,rmin,rmax,omin,xlamd,xlamd0,skal,
     $MODE,NSTEP,NEGREQ,IPRNT
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DATA ZERO/0.0D0/, HALF/0.5D0/, TWO/2.0D+00/, TOLL/1.0D-8/         
      DATA STEP/5.0D-02/, TEN/1.0D+1/, ONE/1.0D+0/, BIG/1.0D+3/         
      DATA FOUR/4.0D+00/
      DATA TMTWO/1.0D-2/, TMSIX/1.0D-06/, SFIX/1.0D+01/, EPS/1.0D-12/ 
C                                                                       
      MAXIT=999                                                         
      NUMIT=0                                                           
      SKAL=ONE
      rscal=rrscal
      it=0
      jt=1
      if (ts) then
      IF(MODE.NE.0) THEN 
      CALL EFOVLP(dmax,osmin,NEWMOD,NVAR,lorjk,*9999)                    CSTP(call)
      if (lorjk) return
C                                                                               
C  ON RETURN FROM EFOVLP, NEWMOD IS THE TS MODE
C                                                                               
      IF(DOPRNT.AND.(NEWMOD.NE.MODE .and. iprnt.ge.1))                  CIO
     &         WRITE(6,1000) MODE,NEWMOD                                CIO
1000  FORMAT(5X,'WARNING! MODE SWITCHING. WAS FOLLOWING MODE ',I3,             
     $       ' NOW FOLLOWING MODE ',I3)                                         
      MODE=NEWMOD                                                               
      IT=MODE                                                                   
      ELSE
      IT=1
      ENDIF
      eigit=eigval(it)
      IF (IPRNT.GE.1) THEN                                                      
         IF (DOPRNT) WRITE(6,900)IT,EIGIT                               CIO
         IF (DOPRNT) WRITE(6,910)(U(I,IT),I=1,NVAR)                     CIO                    
900      FORMAT(/,5X,'TS MODE IS NUMBER',I3,' WITH EIGENVALUE',F9.1,/,          
     *5X,'AND COMPONENTS',/)                                                    
910      FORMAT(5X,8F9.4)                                                       
      ENDIF                                                                     
      endif
      if (it.eq.1) jt=2
      eone=eigval(jt)                                                    
      ssmin=max(abs(eone)*eps,(ten*eps))
      ssmax=max(big,abs(eone))
      ssmax=ssmax*big
      sstoll=toll
      d2max=dmax*dmax                                                   
c     write(6,*)'from formd, eone, ssmin, ssmax, sstoll',
c    $eone,ssmin,ssmax,sstoll
      
C  SOLVE ITERATIVELY FOR LAMDA                                          
C  INITIAL GAUSS FOR LAMDA IS ZERO EXCEPT NOTE THAT                     
C  LAMDA SHOULD BE LESS THAN EIGVAL(1)                                  
C  START BY BRACKETING ROOT, THEN HUNT IT DOWN WITH BRUTE FORCE BISECT. 
C                                                                       
         frodo1=.false.
         frodo2=.false.
         LAMDA=ZERO                                                     
         lamda0=zero
      if (ts .and. eigit.lt.zero .and. eone.ge.zero .and. donr) then
         if (iprnt.ge.1) then
         IF (DOPRNT)                                                    CIO
     &     write(6,*)' ts search, correct hessian, trying pure NR step' CIO
         endif
         goto 776
      endif
      if (.not.ts .and. eone.ge.zero .and. donr) then
         if (iprnt.ge.1) then
         IF (DOPRNT)                                                    CIO
     &    write(6,*)' min search, correct hessian, trying pure NR step' CIO
         endif
         goto 776
      endif
5     if (ts) then
         lamda0=eigval(it)+sqrt(eigval(it)**2+four*fx(it)**2)
         lamda0=lamda0*half
         if (DOPRNT.AND.(iprnt.ge.1)) WRITE(6,1030) LAMDA0              CIO
      endif
         SSTEP = STEP                                                          
         IF(EONE.LE.ZERO) LAMDA=EONE-SSTEP                              
         IF(EONE.GT.ZERO) SSTEP=EONE                                           
         BL = LAMDA - SSTEP                                             
         BU = LAMDA + SSTEP*HALF                                        
20       FL = ZERO                                                      
         FU = ZERO                                                      
         DO 30 I = 1,NVAR                                               
            if (i.eq.it) goto 30
            FL   = FL + (FX(I)*FX(I))/(BL-EIGVAL(I))                    
            FU   = FU + (FX(I)*FX(I))/(BU-EIGVAL(I))                    
30       CONTINUE                                                       
         FL = FL - BL                                                   
         FU = FU - BU                                                   
c        write(6,*)'bl,bu,fl,fu from brack'                             
c        write(6,668)bl,bu,fl,fu                                        
c668     format(6f20.15)
         IF (FL*FU .LT. ZERO) GOTO 40                                   
         BL = BL - (EONE-BL)                                            
         BU = BU + HALF*(EONE-BU)                                       
         IF (BL.LE.-SSMAX) then
            BL = -SSMAX
            frodo1=.true.
         endif
         IF (abs(eone-bu).le.ssmin) then
            BU = EONE-SSMIN           
            frodo2=.true.
         endif
         IF (frodo1.and.frodo2) THEN              
            IF (DOPRNT) THEN                                            CIO
             WRITE(6,*)'NUMERICAL PROBLEMS IN BRACKETING LAMDA',
     $                    EONE,BL,BU,FL,FU
             write(6,*)' going for fixed step size....'                       
            ENDIF                                                       CIO
            goto 450                                                           
         ENDIF                                                          
         GOTO 20                                                        
                                                                        
40       CONTINUE                                                       
         NCNT = 0                                                       
         XLAMDA = ZERO                                                  
50       CONTINUE                                                       
         FL = ZERO                                                      
         FU = ZERO                                                      
         FM = ZERO                                                      
         LAMDA = HALF*(BL+BU)                                                  
         DO 60 I = 1,NVAR                                               
            if (i.eq.it) goto 60
            FL   = FL + (FX(I)*FX(I))/(BL-EIGVAL(I))                    
            FU   = FU + (FX(I)*FX(I))/(BU-EIGVAL(I))                    
            FM   = FM + (FX(I)*FX(I))/(LAMDA-EIGVAL(I))                 
60       CONTINUE                                                       
         FL = FL - BL                                                   
         FU = FU - BU                                                   
         FM = FM - LAMDA                                                
c        write(6,*)'bl,bu,lamda,fl,fu,fm from search'                   
c        write(6,668)bl,bu,lamda,fl,fu,fm                               
         IF (ABS(XLAMDA-LAMDA).LT.sstoll) GOTO 776
         NCNT = NCNT + 1                                                
         IF (NCNT.GT.1000) THEN                                         
            IF(DOPRNT) WRITE(6,*)'TOO MANY ITERATIONS IN LAMDA BISECT', CIO
     $                    BL,BU,LAMDA,FL,FU
      RETURN 1                                                           CSTP (stop)

         ENDIF                                                          
         XLAMDA = LAMDA                                                 
         IF (FM*FU.LT.ZERO) BL = LAMDA                                  
         IF (FM*FL.LT.ZERO) BU = LAMDA                                  
         GOTO 50                                                        
C                                                                       
776   if (DOPRNT.AND.(iprnt.ge.1)) WRITE(6,1031) LAMDA                  CIO
C                                                                       
C  CALCULATE THE STEP                                                   
C                                                                       
      DO 310 I=1,NVAR                                                   
      D(I)=ZERO                                                         
310   CONTINUE                                                          
      DO 330 I=1,NVAR                                                   
      if (lamda.eq.zero .and. abs(eigval(i)).lt.tmtwo) then
      temp=zero
      else
      TEMP=FX(I)/(LAMDA-EIGVAL(I))                                      
      endif
      if (i.eq.it) then
      TEMP=FX(IT)/(LAMDA0-EIGVAL(IT)) 
      endif
      if (DOPRNT.AND.(iprnt.ge.5)) write(6,*)'formd, delta step',i,temp CIO
      DO 320 J=1,NVAR                                                   
      D(J)=D(J)+TEMP*U(J,I)                                             
320   CONTINUE                                                          
330   CONTINUE                                                          
      DD=SQRT(DDOT(NVAR,D,1,D,1))                                       IR0494
      if(DOPRNT.AND.(lamda.eq.zero .and.                                CIO
     &     lamda0.eq.zero .and.iprnt.ge.1)) write(6,777)dd              CIO
777   format(1x,'pure NR-step has length',f10.5)
      if(DOPRNT.AND.(lamda.ne.zero .and.                                CIO
     &     lamda0.ne.-lamda .and.iprnt.ge.1)) write(6,778)dd            CIO
778   format(1x,'P-RFO-step   has length',f10.5)
      if (dd.lt.(dmax+tmsix)) then
         xlamd=lamda
         xlamd0=lamda0
         return
      endif
      if (lamda.eq.zero .and. lamda0.eq.zero) goto 5
      if (rscal) then
         SKAL=DMAX/DD
         DO 160 I=1,NVAR
            D(I)=D(I)*SKAL
160      CONTINUE
         DD=SQRT(DDOT(NVAR,D,1,D,1))                                    IR0494
         IF(DOPRNT.AND.(IPRNT.GE.1)) WRITE(6,170)SKAL                   CIO
170      FORMAT(5X,'CALCULATED STEP SIZE TOO LARGE, SCALED WITH',F9.5)
         xlamd=lamda
         xlamd0=lamda0
         return
      endif

450      LAMDA=ZERO                                                     
         frodo1=.false.
         frodo2=.false.
         SSTEP = STEP                                                          
         IF(EONE.LE.ZERO) LAMDA=EONE-SSTEP                              
         if (ts .and. -eigit.lt.eone) lamda=-eigit-sstep
         IF(EONE.GT.ZERO) SSTEP=EONE                                           
         BL = LAMDA - SSTEP                                             
         BU = LAMDA + SSTEP*HALF                                        
520      FL = ZERO                                                      
         FU = ZERO                                                      
         DO 530 I = 1,NVAR                                              
            if (i.eq.it) goto 530
            FL   = FL + (FX(I)/(BL-EIGVAL(I)))**2                       
            FU   = FU + (FX(I)/(BU-EIGVAL(I)))**2                       
530      CONTINUE                                                       
         if (ts) then
            FL   = FL + (FX(IT)/(BL+EIGVAL(IT)))**2                       
            FU   = FU + (FX(IT)/(BU+EIGVAL(IT)))**2                       
         endif
         FL = FL - d2max                                                
         FU = FU - d2max                                                
c        write(6,*)'bl,bu,fl,fu from brack2'                            
c        write(6,668)bl,bu,fl,fu                                        
         IF (FL*FU .LT. ZERO) GOTO 540                                  
         BL = BL - (EONE-BL)                                            
         BU = BU + HALF*(EONE-BU)                                       
         IF (BL.LE.-SSMAX) then
            BL = -SSMAX
            frodo1=.true.
         endif
         IF (abs(eone-bu).le.ssmin) then
            BU = EONE-SSMIN           
            frodo2=.true.
         endif
         IF (frodo1.and.frodo2) THEN              
            IF (DOPRNT) THEN                                            CIO
            WRITE(6,*)'NUMERICAL PROBLEMS IN BRACKETING LAMDA',
     $                    EONE,BL,BU,FL,FU
            write(6,*)' going for fixed level shifted NR step...'
            ENDIF                                                       CIO
c           both lamda searches failed, go for fixed level shifted nr    
c           this is unlikely to produce anything useful, but maybe we're lucky
            lamda=eone-sfix                                                    
            lamda0=eigit+sfix
            rscal=.true.                                                
            goto 776                                                    
         ENDIF                                                          
         GOTO 520                                                       
                                                                        
540      CONTINUE                                                       
         NCNT = 0                                                       
         XLAMDA = ZERO                                                  
550      CONTINUE                                                       
         FL = ZERO                                                      
         FU = ZERO                                                      
         FM = ZERO                                                      
         LAMDA = HALF*(BL+BU)                                                  
         DO 560 I = 1,NVAR                                              
            if (i.eq.it) goto 560
            FL   = FL + (FX(I)/(BL-EIGVAL(I)))**2                       
            FU   = FU + (FX(I)/(BU-EIGVAL(I)))**2                       
            FM   = FM + (FX(I)/(LAMDA-EIGVAL(I)))**2                    
560      CONTINUE                                                       
         if (ts) then
            FL   = FL + (FX(IT)/(BL+EIGVAL(IT)))**2                       
            FU   = FU + (FX(IT)/(BU+EIGVAL(IT)))**2                       
            FM   = FM + (FX(IT)/(LAMDA+EIGVAL(IT)))**2                    
         endif
         FL = FL - d2max                                                
         FU = FU - d2max                                                
         FM = FM - d2max                                                
c        write(6,*)'bl,bu,lamda,fl,fu,fm from search2'                  
c        write(6,668)bl,bu,lamda,fl,fu,fm                               
         IF (ABS(XLAMDA-LAMDA).LT.sstoll) GOTO 570                        
         NCNT = NCNT + 1                                                
         IF (NCNT.GT.1000) THEN                                         
            IF (DOPRNT) WRITE(6,*)'TOO MANY ITERATIONS IN LAMDA BISECT', CIO
     $                    BL,BU,LAMDA,FL,FU
      RETURN 1                                                           CSTP (stop)

         ENDIF                                                          
         XLAMDA = LAMDA                                                 
         IF (FM*FU.LT.ZERO) BL = LAMDA                                  
         IF (FM*FL.LT.ZERO) BU = LAMDA                                  
         GOTO 550                                                       
C                                                                       
570      CONTINUE                                                       
         lamda0=-lamda
         rscal=.true.                                                   
         goto 776                                                       
C                                                                       
1030  FORMAT(1X,'lamda that maximizes along ts modes =   ',F15.5)       
1031  FORMAT(1X,'lamda that minimizes along all modes =  ',F15.5)       
      RETURN
 9999 RETURN 1                                                          CSTP
      END                                                               
