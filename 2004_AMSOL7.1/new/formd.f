      SUBROUTINE FORMD(EIGVAL,FX,NVAR,DMAX,
     1DDMIN,TS,LRJK,LORJK,RRSCAL,DONR)
C     THIS VERSION FORMS GEOMETRY STEP BY EITHER PURE NR, P-RFO OR QA
C     ALGORITHM, UNDER THE CONDITION THAT THE STEPLENGTH IS LESS THAN DMAX
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      INCLUDE 'SIZES.i'  
       INCLUDE 'FFILES.i'                                               GDH1095
      LOGICAL TS,RSCAL,FRODO1,FRODO2,LRJK,LORJK,RRSCAL,DONR
      DIMENSION EIGVAL(MAXPAR),FX(MAXPAR)                                       
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,RMIN,RMAX,OMIN,XLAMD,XLAMD0,SKAL,
     $MODE,NSTEP,NEGREQ,IPRNT
       SAVE
      DATA ZERO/0.0D0/, HALF/0.5D0/, TWO/2.0D+00/, TOLL/1.0D-8/         
      DATA STEP/5.0D-02/, TEN/1.0D+1/, ONE/1.0D+0/, BIG/1.0D+3/         
      DATA FOUR/4.0D+00/
      DATA TMTWO/1.0D-2/, TMSIX/1.0D-06/, SFIX/1.0D+01/, EPS/1.0D-12/ 
C                                                                       
      MAXIT=999                                                         
      NUMIT=0                                                           
      SKAL=ONE
      RSCAL=RRSCAL
      IT=0
      JT=1
      IF (TS) THEN
      IF(MODE.NE.0) THEN 
      CALL OVERLP(DMAX,DDMIN,NEWMOD,NVAR,LORJK) 
      IF (LORJK) RETURN
C                                                                               
C  ON RETURN FROM OVERLP, NEWMOD IS THE TS MODE
C                                                                               
      IF(NEWMOD.NE.MODE .AND. IPRNT.GE.1) WRITE(JOUT,1000) MODE,NEWMOD
1000  FORMAT(5X,'WARNING! MODE SWITCHING. WAS FOLLOWING MODE ',I3,             
     $       ' NOW FOLLOWING MODE ',I3)                                         
      MODE=NEWMOD                                                               
      IT=MODE                                                                   
      ELSE
      IT=1
      ENDIF
      EIGIT=EIGVAL(IT)
      IF (IPRNT.GE.1) THEN                                                      
         WRITE(JOUT,900)IT,EIGIT
         WRITE(JOUT,910)(U(I,IT),I=1,NVAR)                                     
900      FORMAT(/,5X,'TS MODE IS NUMBER',I3,' WITH EIGENVALUE',F9.1,/,          
     *5X,'AND COMPONENTS',/)                                                    
910      FORMAT(5X,8F9.4)                                                       
      ENDIF                                                                     
      ENDIF
      IF (IT.EQ.1) JT=2
      EONE=EIGVAL(JT)                                                    
      SSMIN=MAX(ABS(EONE)*EPS,(TEN*EPS))
      SSMAX=MAX(BIG,ABS(EONE))
      SSMAX=SSMAX*BIG
      SSTOLL=TOLL
      D2MAX=DMAX*DMAX                                                   
C     WRITE(JOUT,*)'FROM FORMD, EONE, SSMIN, SSMAX, SSTOLL',
C    $EONE,SSMIN,SSMAX,SSTOLL
      
C  SOLVE ITERATIVELY FOR ALAMDA                                          
C  INITIAL GUESS FOR ALAMDA IS ZERO EXCEPT NOTE THAT                     
C  ALAMDA SHOULD BE LESS THAN EIGVAL(1)                                  
C  START BY BRACKETING ROOT, THEN HUNT IT DOWN WITH BRUTE FORCE BISECT. 
C                                                                       
       FRODO1=.FALSE.
       FRODO2=.FALSE.
         ALAMDA=ZERO                                                    DJG0295
       ALAMD0=ZERO                                                      DJG0295
      IF (TS .AND. EIGIT.LT.ZERO .AND. EONE.GE.ZERO .AND. DONR) THEN
       IF (IPRNT.GE.1) THEN
       WRITE(JOUT,*)' TS SEARCH, CORRECT HESSIAN, TRYING PURE NR STEP'
       ENDIF
       GOTO 776
      ENDIF
      IF (.NOT.TS .AND. EONE.GE.ZERO .AND. DONR) THEN
       IF (IPRNT.GE.1) THEN
      WRITE(JOUT,*)' MIN SEARCH, CORRECT HESSIAN, TRYING PURE NR STEP'
       ENDIF
       GOTO 776
      ENDIF
5     IF (TS) THEN
       ALAMD0=EIGVAL(IT)+SQRT(EIGVAL(IT)**2+FOUR*FX(IT)**2)
       ALAMD0=ALAMD0*HALF
         IF (IPRNT.GE.1)WRITE(JOUT,1030) ALAMD0
      ENDIF
       SSTEP = STEP                                                          
         IF(EONE.LE.ZERO) ALAMDA=EONE-SSTEP                              
       IF(EONE.GT.ZERO) SSTEP=EONE                                           
         BL = ALAMDA - SSTEP                                             
         BU = ALAMDA + SSTEP*HALF                                        
20       FL = ZERO                                                      
         FU = ZERO                                                      
         DO 30 I = 1,NVAR                                               
          IF (I.EQ.IT) GOTO 30
            FL   = FL + (FX(I)*FX(I))/(BL-EIGVAL(I))                    
            FU   = FU + (FX(I)*FX(I))/(BU-EIGVAL(I))                    
30       CONTINUE                                                       
         FL = FL - BL                                                   
         FU = FU - BU                                                   
C        WRITE(JOUT,*)'BL,BU,FL,FU FROM BRACK'                             
C        WRITE(JOUT,668)BL,BU,FL,FU                                        
C668     FORMAT(6F20.15)
         IF (FL*FU .LT. ZERO) GOTO 40                                   
         BL = BL - (EONE-BL)                                            
         BU = BU + HALF*(EONE-BU)                                       
       IF (BL.LE.-SSMAX) THEN
          BL = -SSMAX
          FRODO1=.TRUE.
         ENDIF
       IF (ABS(EONE-BU).LE.SSMIN) THEN
          BU = EONE-SSMIN           
          FRODO2=.TRUE.
         ENDIF
         IF (FRODO1.AND.FRODO2) THEN              
            WRITE(JOUT,*)'NUMERICAL PROBLEMS IN BRACKETING LAMDA',
     $                    EONE,BL,BU,FL,FU
          WRITE(JOUT,*)' GOING FOR FIXED STEP SIZE....'                       
          GOTO 450                                                           
         ENDIF                                                          
         GOTO 20                                                        
                                                                        
40       CONTINUE                                                       
         NCNT = 0                                                       
         XLAMDA = ZERO                                                  
50       CONTINUE                                                       
         FL = ZERO                                                      
         FU = ZERO                                                      
         FM = ZERO                                                      
       ALAMDA = HALF*(BL+BU)                                                  
         DO 60 I = 1,NVAR                                               
          IF (I.EQ.IT) GOTO 60
            FL   = FL + (FX(I)*FX(I))/(BL-EIGVAL(I))                    
            FU   = FU + (FX(I)*FX(I))/(BU-EIGVAL(I))                    
            FM   = FM + (FX(I)*FX(I))/(ALAMDA-EIGVAL(I))                 
60       CONTINUE                                                       
         FL = FL - BL                                                   
         FU = FU - BU                                                   
         FM = FM - ALAMDA                                                
         IF (ABS(XLAMDA-ALAMDA).LT.SSTOLL) GOTO 776
         NCNT = NCNT + 1                                                
         IF (NCNT.GT.1000) THEN                                         
            WRITE(JOUT,*)'TOO MANY ITERATIONS IN LAMDA BISECT',
     $                    BL,BU,ALAMDA,FL,FU
      ISTOP=1                                                           GDH1095
      IWHERE=69                                                         GDH1095
      RETURN                                                            GDH1095
         ENDIF                                                          
         XLAMDA = ALAMDA                                                 
         IF (FM*FU.LT.ZERO) BL = ALAMDA                                  
         IF (FM*FL.LT.ZERO) BU = ALAMDA                                  
         GOTO 50                                                        
C                                                                       
776   IF (IPRNT.GE.1) WRITE(JOUT,1031) ALAMDA 
C                                                                       
C  CALCULATE THE STEP                                                   
C                                                                       
      DO 310 I=1,NVAR                                                   
      D(I)=ZERO                                                         
310   CONTINUE                                                          
      DO 330 I=1,NVAR                                                   
      IF (ALAMDA.EQ.ZERO .AND. ABS(EIGVAL(I)).LT.TMTWO) THEN
      TEMP=ZERO
      ELSE
      TEMP=FX(I)/(ALAMDA-EIGVAL(I))                                      
      ENDIF
      IF (I.EQ.IT) THEN
      TEMP=FX(IT)/(ALAMD0-EIGVAL(IT)) 
      ENDIF
      DO 320 J=1,NVAR                                                   
      D(J)=D(J)+TEMP*U(J,I)                                             
320   CONTINUE                                                          
330   CONTINUE                                                          
      DD=SQRT(DOT(D,D,NVAR))
      IF(ALAMDA.EQ.ZERO .AND. ALAMD0.EQ.ZERO .AND.IPRNT.GE.1)
     1 WRITE(JOUT,777)DD
777   FORMAT(1X,'PURE NR-STEP   HAS LENGTH',F10.5)
      IF(ALAMDA.NE.ZERO .AND. ALAMD0.NE.-ALAMDA .AND.IPRNT.GE.1) 
     1WRITE(JOUT,778)DD
778   FORMAT(1X,'P-RFO-STEP     HAS LENGTH',F10.5)
      IF(ALAMDA.NE.ZERO .AND. ALAMD0.EQ.-ALAMDA .AND.IPRNT.GE.1) 
     1WRITE(JOUT,779)DD
779   FORMAT(1X,'QA/TRIM-STEP   HAS LENGTH',F10.5)
      IF (DD.LT.(DMAX+TMSIX)) THEN
         XLAMD=ALAMDA
         XLAMD0=ALAMD0
         RETURN
      ENDIF
      IF (ALAMDA.EQ.ZERO .AND. ALAMD0.EQ.ZERO) GOTO 5
      IF (RSCAL) THEN
         SKAL=DMAX/DD
         DO 160 I=1,NVAR
            D(I)=D(I)*SKAL
160      CONTINUE
         DD=SQRT(DOT(D,D,NVAR))
         IF(IPRNT.GE.1)WRITE(JOUT,170)SKAL
170      FORMAT(5X,'CALCULATED STEP SIZE TOO LARGE, SCALED WITH',F9.5)
         XLAMD=ALAMDA
         XLAMD0=ALAMD0
       RETURN
      ENDIF

450     ALAMDA=ZERO                                                     
       FRODO1=.FALSE.
       FRODO2=.FALSE.
       SSTEP = STEP                                                          
         IF(EONE.LE.ZERO) ALAMDA=EONE-SSTEP                              
       IF (TS .AND. -EIGIT.LT.EONE) ALAMDA=-EIGIT-SSTEP
       IF(EONE.GT.ZERO) SSTEP=EONE                                           
         BL = ALAMDA - SSTEP                                             
         BU = ALAMDA + SSTEP*HALF                                        
520      FL = ZERO                                                      
         FU = ZERO                                                      
         DO 530 I = 1,NVAR                                              
          IF (I.EQ.IT) GOTO 530
            FL   = FL + (FX(I)/(BL-EIGVAL(I)))**2                       
            FU   = FU + (FX(I)/(BU-EIGVAL(I)))**2                       
530      CONTINUE                                                       
         IF (TS) THEN
            FL   = FL + (FX(IT)/(BL+EIGVAL(IT)))**2                       
            FU   = FU + (FX(IT)/(BU+EIGVAL(IT)))**2                       
       ENDIF
         FL = FL - D2MAX                                                
         FU = FU - D2MAX                                                
         IF (FL*FU .LT. ZERO) GOTO 540                                  
         BL = BL - (EONE-BL)                                            
         BU = BU + HALF*(EONE-BU)                                       
       IF (BL.LE.-SSMAX) THEN
          BL = -SSMAX
          FRODO1=.TRUE.
         ENDIF
       IF (ABS(EONE-BU).LE.SSMIN) THEN
          BU = EONE-SSMIN           
          FRODO2=.TRUE.
         ENDIF
         IF (FRODO1.AND.FRODO2) THEN              
            WRITE(JOUT,*)'NUMERICAL PROBLEMS IN BRACKETING LAMDA',
     $                    EONE,BL,BU,FL,FU
          WRITE(JOUT,*)' GOING FOR FIXED LEVEL SHIFTED NR STEP...'
C           BOTH LAMDA SEARCHES FAILED, GO FOR FIXED LEVEL SHIFTED NR    
C           THIS IS UNLIKELY TO PRODUCE ANYTHING USEFUL, BUT MAYBE WE'RE LUCKY
          ALAMDA=EONE-SFIX                                                    
          ALAMD0=EIGIT+SFIX
            RSCAL=.TRUE.                                                
            GOTO 776                                                    
         ENDIF                                                          
         GOTO 520                                                       
                                                                        
540      CONTINUE                                                       
         NCNT = 0                                                       
         XLAMDA = ZERO                                                  
550      CONTINUE                                                       
         FL = ZERO                                                      
         FU = ZERO                                                      
         FM = ZERO                                                      
       ALAMDA = HALF*(BL+BU)                                                  
         DO 560 I = 1,NVAR                                              
          IF (I.EQ.IT) GOTO 560
            FL   = FL + (FX(I)/(BL-EIGVAL(I)))**2                       
            FU   = FU + (FX(I)/(BU-EIGVAL(I)))**2                       
            FM   = FM + (FX(I)/(ALAMDA-EIGVAL(I)))**2                    
560      CONTINUE                                                       
         IF (TS) THEN
            FL   = FL + (FX(IT)/(BL+EIGVAL(IT)))**2                       
            FU   = FU + (FX(IT)/(BU+EIGVAL(IT)))**2                       
            FM   = FM + (FX(IT)/(ALAMDA+EIGVAL(IT)))**2                    
       ENDIF
         FL = FL - D2MAX                                                
         FU = FU - D2MAX                                                
         FM = FM - D2MAX                                                
         IF (ABS(XLAMDA-ALAMDA).LT.SSTOLL) GOTO 570                        
         NCNT = NCNT + 1                                                
         IF (NCNT.GT.1000) THEN                                         
            WRITE(JOUT,*)'TOO MANY ITERATIONS IN LAMDA BISECT',
     $                    BL,BU,ALAMDA,FL,FU
      ISTOP=1                                                           GDH1095
      IWHERE=70                                                         GDH1095
      RETURN                                                            GDH1095
         ENDIF                                                          
         XLAMDA = ALAMDA                                                 
         IF (FM*FU.LT.ZERO) BL = ALAMDA                                  
         IF (FM*FL.LT.ZERO) BU = ALAMDA                                  
         GOTO 550                                                       
C                                                                       
570      CONTINUE                                                       
         ALAMD0=-ALAMDA
         RSCAL=.TRUE.                                                   
         GOTO 776                                                       
C                                                                       
1030  FORMAT(1X,'LAMDA THAT MAXIMIZES ALONG TS MODES =   ',F15.5)       
1031  FORMAT(1X,'LAMDA THAT MINIMIZES ALONG ALL MODES =  ',F15.5)       
      END                                                               
