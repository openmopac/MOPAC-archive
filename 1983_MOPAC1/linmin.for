
     SUBROUTINE LINMIN(XPARAM,STEP,PVECT,NVAR,FUNCT,OKF,OKC)
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
     INCLUDE 'SIZES/NOLIST'
     DIMENSION XPARAM(NVAR),PVECT(NVAR)
     COMMON /GRAVEC/ COSINE
     COMMON /NUMCAL/ NUMCAL
*********************************************************************  
                                                                       
  LINMIN DOES A LINE MINIMISATION.
                                                                       
  ON INPUT:  XPARAM = STARTING COORDINATE OF SEARCH.
             STEP   = STEP SIZE FOR INITIATING SEARCH.
             PVECT  = DIRECTION OF SEARCH.
             NVAR   = NUMBER OF VARIABLES IN XPARAM.
             FUNCT  = INITIAL VALUE OF THE FUNCTION TO BE MINIMISED.
             ISOK   = NOT IMPORTANT.
             COSINE = COSINE OF ANGLE OF CURRENT AND PREVIOUS GRADIENT.
           
  ON OUTPUT: XPARAM = COORDINATE OF MINIMUM OF FUNCTI0N.
             STEP   = NEW STEP SIZE, USED IN NEXT CALL OF LINMIN.
             PVECT  = UNCHANGED, OR NEGATED, DEPENDING ON STEP.
             FUNCT  = FINAL, MINIMUM VALUE OF THE FUNCTION.
             OKF    = .TRUE. IF LINMIN IMPROVED FUNCT, .FALSE. OTHERWISE.
             OKC    = .TRUE. IF LINMIN FOUND THE MINIMUM, .FALSE. OTHERWISE.
                                                                       
********************************************************************** 
     COMMON /KEYWRD/ KEYWRD
     CHARACTER*80 KEYWRD
     DIMENSION PHI(3), VT(3)
     DIMENSION XSTOR(300)                                  
     INTEGER LEFT,RIGHT,CENTER
     LOGICAL PRINT,OKF,OKC, FULSCF, ASKFUL, UHF 
     DATA ICALCN /0/
     IF (ICALCN.NE.NUMCAL) THEN
         UHF=(INDEX(KEYWRD,'UHF') .NE. 0)
         ASKFUL=(INDEX(KEYWRD,'FULSCF') .NE. 0)
         DROP=0.002D0
         IF(INDEX(KEYWRD,'PREC') .NE. 0) DROP=DROP*0.01D0
         XMAXM  = 0.4D0
         I      = 2
         STEP   = 1.D0
         MAXLIN = 15
         XCRIT  = 0.0001D0
         IF(INDEX(KEYWRD,'FORCE') .NE. 0) THEN
             I=3
             XCRIT=0.00001D0
         ENDIF
         ANGLE=0.8D0
         IF(UHF) ANGLE=0.96D0
         COSINE=99.99D0

  ANGLE IS USED TO DECIDE IF P IS TO BE UPDATED AS CALCULATION
        PROCEEDS.

         YMAXST  = 0.4D0
         EPS=10**(-I)
         TEE=EPS                                                       
         PRINT=(INDEX(KEYWRD,'LINMIN') .NE. 0)
         ICALCN=NUMCAL
     END IF
     FULSCF=(ASKFUL.OR.COSINE.GT.ANGLE)
     IF(PRINT) WRITE(6,'(''  FULL SCF CALCULATIONS:'',3X,L1)')FULSCF
     XMAXM=0.D0
     DO 14 I=1,NVAR
         PABS=ABS(PVECT(I))   
 14      XMAXM=MAX(XMAXM,PABS)
     XMINM=XMAXM
     XMAXM=YMAXST/XMAXM
     FIN=FUNCT
     SSQLST=FUNCT
     IQUIT=0                                                             
     PHI(1)=FUNCT                                                          
     VT(1)=0.0D00                                                      
     VT(2)=STEP/4.0D00                                                    
     IF (VT(2).GT.XMAXM) VT(2)=XMAXM                                   
     FMAX=FUNCT
     FMIN=FUNCT
     STEP=VT(2)                                                           
     DO 10 I=1,NVAR                                                       
  10 XPARAM(I)=XPARAM(I)+STEP*PVECT(I)                                                  
     CALL COMPFG(XPARAM, .TRUE., PHI(2),FULSCF,GRAD,SIGMA)             
     IF(PHI(2).GT.FMAX) FMAX=PHI(2)
     IF(PHI(2).LT.FMIN) FMIN=PHI(2)
     CALL EXCHNG (PHI(2),SQSTOR,ENERGY,ESTOR,XPARAM,XSTOR,
    +STEP,ALFS,NVAR)                                                                 
     IF (PHI(1).LE.PHI(2)) GO TO 30                                    
     GO TO 40                                                          
  30 VT(3)=-VT(2)                                                      
     LEFT=3                                                            
     CENTER=1                                                          
     RIGHT=2                                                           
     GO TO 50                                                          
  40 VT(3)=2.0D00*VT(2)                                                
     LEFT=1                                                            
     CENTER=2                                                          
     RIGHT=3                                                           
  50 STLAST=VT(3)                                                       
     STEP=STLAST-STEP                                                         
     DO 60 I=1,NVAR                                                       
  60 XPARAM(I)=XPARAM(I)+STEP*PVECT(I)                                                  
     CALL COMPFG (XPARAM, .TRUE., FUNCT,FULSCF,GRAD,SIGMA)                 
     IF(FUNCT.GT.FMAX) FMAX=FUNCT
     IF(FUNCT.LT.FMIN) FMIN=FUNCT
     IF (FUNCT.LT.SQSTOR) CALL EXCHNG (FUNCT,SQSTOR,ENERGY,
    +ESTOR,XPARAM,XSTOR,STEP,ALFS,NVAR)                                                     
     IF (FUNCT.LT.FIN) IQUIT=1                                               
     PHI(3)=FUNCT                                                          
     IF (PRINT)WRITE (6,290) VT(1),PHI(1),VT(2),PHI(2),VT(3),PHI(3)
     OKC=.TRUE.
     DO 240 ICTR=3,MAXLIN
        ALPHA=VT(2)-VT(3)                                              
        BETA=VT(3)-VT(1)                                               
        GAMMA=VT(1)-VT(2)                                              
        ALPHA=-(PHI(1)*ALPHA+PHI(2)*BETA+PHI(3)*GAMMA)/(ALPHA*BETA*GAMM
    1   A)                                                             
        BETA=((PHI(1)-PHI(2))/GAMMA)-ALPHA*(VT(1)+VT(2))               
        IF (ALPHA) 100,100,130                                         
 100    IF (PHI(RIGHT).GT.PHI(LEFT)) GO TO 110                         
        STEP=3.0D00*VT(RIGHT)-2.0D00*VT(CENTER)                           
        GO TO 120                                                      
 110    STEP=3.0D00*VT(LEFT)-2.0D00*VT(CENTER)                            
 120    S=STEP-STLAST                                                      
        IF (ABS(S).GT.XMAXM) S=SIGN(XMAXM,S)*(1+0.01*(XMAXM/S))
        STEP=S+STLAST                                                      
        GO TO 140                                                      
 130    STEP=-BETA/(2.0D00*ALPHA)                                         
        S=STEP-STLAST                                                      
        XXM=2.0D00*XMAXM                                               
        IF (ABS(S).GT.XXM) S=SIGN(XXM,S)*(1+0.01*(XXM/S))
        STEP=S+STLAST                                                      
 140    CONTINUE                                                       
        IF (ICTR.LE.3) GO TO 150                                       
        AABS=ABS(S*XMINM)                                              
        IF (AABS.LT.XCRIT) GO TO 250                                   
 150    CONTINUE                                                       
        DO 160 I=1,NVAR                                                   
 160    XPARAM(I)=XPARAM(I)+S*PVECT(I)                                               
        FUNOLD=FUNCT
        CALL COMPFG (XPARAM, .TRUE., FUNCT,FULSCF,GRAD,SIGMA)
        IF(FUNCT.GT.FMAX) FMAX=FUNCT
        IF(FUNCT.LT.FMIN) FMIN=FUNCT
        IF (FUNCT.LT.SQSTOR) CALL EXCHNG (FUNCT,SQSTOR,ENERGY,ESTOR,
    +   XPARAM,XSTOR,STEP,ALFS,NVAR)                                               
        IF (FUNCT.LT.FIN) IQUIT=1                                            
        IF(ISKSCF.EQ.1)NCOUNT=NCOUNT+1                                 
        IF (PRINT) WRITE (6,300) VT(LEFT),PHI(LEFT),
    +                            VT(CENTER),PHI(CENTER),
    1                            VT(RIGHT),PHI(RIGHT),STEP,FUNCT                                              
                                                                       
 TEST TO EXIT FROM LINMIN IF NOT DROPPING IN VALUE OF FUNCTION FAST.              
                                                                       
        TINY = MAX((SSQLST-FMIN)*0.2D0 , DROP)      
        TINY = MIN(TINY,0.5D0)
        IF(PRINT) WRITE(6,'(''  TINY'',F12.6)')TINY
        IF(ABS(FUNOLD-FUNCT) .LT. TINY .AND. IQUIT .EQ. 1) GOTO 250
        IF ((ABS(STEP-STLAST).LE.EPS*ABS(STEP+STLAST)+TEE).
    +   AND.(IQUIT.EQ.1)) GO TO 250 
        STLAST=STEP                                                        
        IF ((STEP.GT.VT(RIGHT)).OR.(STEP.GT.VT(CENTER)
    +        .AND.FUNCT.LT.PHI(CENTER)).OR.(STEP.GT.VT(LEFT)
    1        .AND.STEP.LT.VT(CENTER).AND.FUNCT.GT.PHI(CENTER)))
    2         GOTO 200
        VT(RIGHT)=STEP
        PHI(RIGHT)=FUNCT                                                   
        GO TO 210                                                      
 200    VT(LEFT)=STEP                                                     
        PHI(LEFT)=FUNCT                                                    
 210    IF (VT(CENTER).LT.VT(RIGHT)) GO TO 220                         
        I=CENTER                                                       
        CENTER=RIGHT                                                   
        RIGHT=I                                                        
 220    IF (VT(LEFT).LT.VT(CENTER)) GO TO 230                          
        I=LEFT                                                         
        LEFT=CENTER                                                    
        CENTER=I                                                       
 230    IF (VT(CENTER).LT.VT(RIGHT)) GO TO 240                         
        I=CENTER                                                       
        CENTER=RIGHT                                                   
        RIGHT=I                                                        
 240 CONTINUE                                                          
     OKC=.FALSE.
 250 CONTINUE                                                          
     CALL EXCHNG (SQSTOR,FUNCT,ESTOR,ENERGY,XSTOR,XPARAM,
    +             ALFS,STEP,NVAR)    
     OKF = (FUNCT.LT.SSQLST)      
     IF (FUNCT.GE.SSQLST) RETURN                                           
     IF (STEP) 260,280,280                                                
 260 STEP=-STEP                                                              
     DO 270 I=1,NVAR                                                      
 270 PVECT(I)=-PVECT(I)                                                        
 280 CONTINUE                                                          
     RETURN                                                            
                                                                       
 290 FORMAT ( 11H ---QLINMN ,/5X, 10HLEFT   ...,2F17.8/5X, 10HCENTER ..
    1.,2F17.8/5X, 10HRIGHT  ...,2F17.8/,  2H ')                        
 300 FORMAT (5X, 10HLEFT   ...,2F17.8/5X, 10HCENTER ...,2F17.8/5X, 10HR
    1IGHT  ...,2F17.8/5X, 10HNEW    ...,2F17.8/,  2H ')                
                                                                       
     END                                                               