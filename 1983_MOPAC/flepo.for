
     SUBROUTINE FLEPO (XPARAM,NVAR,FUNCT1)
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
     INCLUDE 'SIZES/NOLIST'
     DIMENSION XPARAM(*)
     COMMON /KEYWRD/ KEYWRD
     COMMON /LAST  / LAST
     COMMON /PATH  / LATOM,LPARAM,REACT(100)
     COMMON /GRADNT/ GRAD(MAXPAR),GNORM
     COMMON /MESAGE/ IFLEPO,ISCF
     COMMON /TIME  / TIME0
     COMMON /FMATRX/ HESINV(MAXHES)
     COMMON /NUMCAL/ NUMCAL
     COMMON /GRAVEC/ COSINE
     CHARACTER*80 KEYWRD
                                                                       
     *                                                                 
     THIS SUBROUTINE ATTEMPTS TO MINIMIZE A REAL-VALUED FUNCTION OF    
     THE N-COMPONENT REAL VECTOR XPARAM ACCORDING TO THE                    
     DAVIDON-FLETCHER-POWELL ALGORITHM (COMPUTER JOURNAL, VOL. 6,      
     P. 163).  THE USER MUST SUPPLY THE SUBROUTINE   
     COMPFG(NVAR,XPARAM,FUNCT,GRAD,1)
     WHICH COMPUTES FUNCTION VALUES  FUNCT  AND GRADIENTS  GRAD  AT GIVEN     
     VALUES FOR THE VARIABLES XPARAM.  THE MINIMIZATION PROCEEDS BY A       
     SEQUENCE OF ONE-DIMENSIONAL MINIMIZATIONS.  THESE ARE CARRIED OUT 
     BY THE SUBROUTINE LINMIN, WHICH SOLVES THE SUBPROBLEM OF          
     MINIMIZING THE FUNCTION FUNCT ALONG THE LINE XPARAM+ALPHA*PVECT,
     WHERE XPARAM 
     IS THE VECTOR OF CURRENT VARIABLE VALUES,  ALPHA IS A SCALAR      
     VARIABLE, AND  PVECT  IS A SEARCH-DIRECTION VECTOR PROVIDED BY THE    
     DAVIDON-FLETCHER-POWELL ALGORITHM.  EACH ITERATION STEP CARRIED   
     OUT BY FLEPO PROCEEDS BY LETTING LINMIN FIND A VALUE FOR ALPHA    
     WHICH MINIMIZES  FUNCT  ALONG  XPARAM+ALPHA*PVECT, BY
     UPDATING THE VECTOR  XPARAM  BY THE AMOUNT ALPHA*PVECT, AND FINALLY BY 
     GENERATING A NEW VECTOR  PVECT.  UNDER CERTAIN RESTRICTIONS           
     (POWELL, J.INST.MATHS.APPLICS.(1971),V.7,21-36)  A SEQUENCE OF    
     FUNCT VALUES CONVERGING TO SOME LOCAL MINIMUM VALUE AND A SEQUENCE OF 
     XPARAM VECTORS CONVERGING TO THE CORRESPONDING MINIMUM POINT      
     ARE PRODUCED.
                          CONVERGENCE TESTS.

     HERBERTS TEST: THE ESTIMATED DISTANCE FROM THE CURRENT POINT
                    POINT TO THE MINIMUM IS LESS THAN TOLERA.
 
                    "HERBERTS TEST SATISFIED - GEOMETRY OPTIMISED"

     GRADIENT TEST: THE GRADIENT NORM HAS BECOME LESS THAN TOLERG
                    TIMES THE SQUARE ROOT OF THE NUMBER OF VARIABLES.

                    "TEST ON GRADIENT SATISFIED".

     XPARAM TEST:   THE RELATIVE CHANGE IN XPARAM, MEASURED BY ITS NORM,
                    OVER ANY TWO SUCCESSIVE ITERATION STEPS DROPS BELOW
                    TOLERX.

                    "TEST ON XPARAM SATISFIED".

     FUNCTION TEST: THE CALCULATED VALUE OF THE HEAT OF FORMATION 
                    BETWEEN ANY TWO CYCLES IS WITHIN TOLERF OF EACH OTHER.

                    "HEAT OF FORMATION TEST SATISFIED"

     FOR THE GRADIENT, FUNCTION, AND XPARAM TESTS A FURTHER CONDITION, 
     THAT NO INDIVIDUAL COMPONENT OF THE GRADIENT IS GREATER 
     THAN TOLERG, MUST BE SATISFIED, IN WHICH CASE THE 
     CALCULATION EXITS WITH THE MESSAGE

                     "PETERS TEST SATISFIED"

     AN UNSUCCESSFUL TERMINATION WILL TAKE PLACE AFTER       
     COMPFG HAS BEEN CALLED MORE TIMES THAN THE USER-SUPPLIED VALUE    
     OF MAXEND.  IN THIS CASE THE COMMENT                              

                     "*** TERMINATION FROM TOO MANY COUNTS ***"

     WILL BE PRINTED, AND FUNCT AND XPARAM WILL CONTAIN THE LAST FUNCTION    
     VALUE CUM VARIABLE VALUES REACHED.  

     SIMILAR UNSUCCESSFUL TERMINATIONS WILL TAKE PLACE IF THE COSINE OF
     THE SEARCH DIRECTION TO GRADIENT VECTOR IS LESS THAN RST ON TWO
     CONSECUTIVE ITERATIONS.

     THE DAVIDON-FLETCHER-POWELL ALGORITHM CHOOSES SEARCH DIRECTIONS   
     ON THE BASIS OF LOCAL PROPERTIES OF THE FUNCTION.  A MATRIX  H,   
     WHICH IN FLEPO IS PRESET WITH THE IDENTITY, IS MAINTAINED AND     
     UPDATED AT EACH ITERATION STEP.  THE MATRIX DESCRIBES A LOCAL     
     METRIC ON THE SURFACE OF FUNCTION VALUES ABOVE THE POINT XPARAM.       
     THE SEARCH-DIRECTION VECTOR  PVECT  IS SIMPLY A TRANSFORMATION        
     OF THE GRADIENT  GRAD  BY THE MATRIX H.  THE USER MAY THROW OUT  H   
     AFTER EACH  NRST ITERATION STEPS  (RESTARTING WITH THE IDENTITY)  
     OR WHENEVER THE COSINE OF THE ANGLE BETWEEN  GRAD  AND  PVECT  BECOMES   
     LESS THAN RST. THIS CAN BE SUPPRESSED ENTIRELY IF NRST .GT. MAXEND
     AND RST .LT. 0.0.   RESTARTING IS DISCUSSED MARGINALLY IN THE     
     PAPER BY FLETCHER AND POWELL, BUT THERE ARE NO GOOD RULES ABOUT   
     WHEN OR WHETHER THIS SHOULD BE DONE FOR ANY GIVEN FUNCTION.       
                                                                       
     DIMENSION XD(MAXPAR), GD(MAXPAR), GLAST(MAXPAR), MDFP(9),XDFP(9),
    +          XLAST(MAXPAR), GG(MAXPAR), PVECT(MAXPAR)
     LOGICAL OKF, OKC, PRINT,  TIME, RESTRT, MINPRT, SADDLE, GEOOK
    +        ,RESET
     EQUIVALENCE (MDFP(1),JCYC  ),(MDFP(2),JNRST),(MDFP(3),NCOUNT),
    +            (MDFP(4),LNSTOP),(XDFP(1),ALPHA),(XDFP(2),COS   ),
    1            (XDFP(3),PNORM ),(XDFP(4),YEAD ),(XDFP(5),DEL   ),
    2            (XDFP(6),FREPF ),(XDFP(7),CYCMX)
     DATA ICALCN /0/
     IF (ICALCN.NE.NUMCAL) THEN

   THE FOLLOWING CONSTANTS SHOULD BE SET BY THE USER.

     RST   = 0.05D0
     MAXEND= 9999
     TDEL  = 6.D0
     NRST  = 30
     SFACT=1.5
     PMSTE = 0.1D0
     DELL  = 0.01D0
     EINC  = 0.3D0
     IGG1  = 3
     DEL=DELL                                                          

    THESE CONSTANTS SHOULD BE SET BY THE PROGRAM.

     RESTRT   = (INDEX(KEYWRD,'RESTART').NE.0)
     GEOOK   = (INDEX(KEYWRD,'GEO-OK').NE.0)
     TIME   = (INDEX(KEYWRD,'TIME').NE.0)
     TLEFT=3600
     GNLIM=1.D0
     IF(INDEX(KEYWRD,'GNORM=').NE.0)
    + GNLIM=READA(KEYWRD,INDEX(KEYWRD,'GNORM='))
     IF(INDEX(KEYWRD,'PARA') .NE. 0) TLEFT=1.D12
     MINPRT=(LATOM+INDEX(KEYWRD,'SADDLE')+INDEX(KEYWRD,'PARA')+
    +INDEX(KEYWRD,'SADDLE') .EQ.0)
     SADDLE=(INDEX(KEYWRD,'SADDLE') .NE.0)
     IF( .NOT. MINPRT) MINPRT=(INDEX(KEYWRD,'DEBUG') .NE. 0)
     I=INDEX(KEYWRD,' T=')
     IF(I.NE.0) TLEFT=READA(KEYWRD,I)
     TX2=SECOND()
     TLEFT=TLEFT-TX2+TIME0
     PRINT  = (INDEX(KEYWRD,'FLEPO').NE.0)
     TOLERX = 0.0001D0
     EYEAD  = 0.0005D0
     TOLERF = 0.002D0
     TOLERG = 0.5D0
     TOLRG  = TOLERG
     IF (INDEX(KEYWRD,'FORCE') .NE. 0) THEN
         TOLERX = 0.00001D0
         TOLERF = 0.0002D0
         TOLERG = 0.05D0
         EYEAD  = 0.00005D0
     ENDIF
     IF(INDEX(KEYWRD,'PREC') .NE. 0) THEN
         TOLERX=TOLERX*0.01D0
         EYEAD=EYEAD*0.01D0
         TOLERF=TOLERF*0.01D0
         TOLERG=TOLERG*0.01D0
     ENDIF
     ENDIF

     THE FOLLOWING CONSTANTS SHOULD BE SET TO SOME ARBITARY LARGE VALUE.
                                                                       
     YEAD  = 1.D15
     FREPF = 1.D15                                                      

     AND FINALLY, THE FOLLOWING CONSTANTS ARE CALCULATED.

     IHDIM=(NVAR*(NVAR+1))/2                                                 
     CCN=DFLOAT(NVAR)                                                     
     ROOTV=SQRT(CCN+1.D-5)
     CNCADD=1.0D00/ROOTV                                           
     IF (CNCADD.GT.0.15D00) CNCADD=0.15D00                             

     FIRST, WE INITIALISE THE VARIABLES.

     JCYC=0                                                            
     LNSTOP=1                                                          
     IREPET=1                                                          
     ALPHA = 1.0D00                                                    
     PNORM=1.0D00                                                      
     JNRST=0
     CYCMX=0.D0
     COS=0.0D00      
     TOTIME=0.D0                                                  
     NCOUNT=1                                                          
     IF( SADDLE) THEN

   WE DON'T NEED HIGH PRECISION DURING A SADDLE-POINT CALCULATION.

         IF(NVAR.GT.0)GNORM=SQRT(DOT(GRAD,GRAD,NVAR))
         IF(GNORM.GT.1.D0) TOLERG=TOLRG*GNORM
     WRITE(6,'('' GRADIENT CRITERION IN FLEPO ='',F12.5)')TOLERG
     ENDIF
     IF (RESTRT .AND. ICALCN .NE. NUMCAL) THEN
         MDFP(9)=0
         CALL DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP)
     ELSE
                                                                       
 CALCULATE THE VALUE OF THE FUNCTION -> FUNCT1, AND GRADIENTS -> GRAD.
 NORMAL SET-UP OF FUNCT1 AND GRAD, DONE ONCE ONLY.                          
                                                                       
         CALL COMPFG (XPARAM, .TRUE., FUNCT1,.TRUE.,GRAD,.TRUE.)
         DO 641 I=1,NVAR
 641     GD(I)=GRAD(I)
     ENDIF
     ICALCN=NUMCAL
     IF(NVAR.NE.0)GNORM=SQRT(DOT(GRAD,GRAD,NVAR))
     IFLEPO=1
     IF(INDEX(KEYWRD,'1SCF') .NE. 0) RETURN
     IFLEPO=2
     IF(GNORM.LT.MAX(GNLIM,TOLERG).OR.NVAR.EQ.0) THEN
     IF(RESTRT)
    + CALL COMPFG (XPARAM, .TRUE., FUNCT1,.TRUE.,GRAD,.TRUE.)
     RETURN
     ENDIF
     TX1 =  SECOND()                                                 
     TLEFT=TLEFT-TX1+TX2
     *                                                                 
     START OF EACH ITERATION CYCLE ...                                 
     *                                                                 
                                                                       
     RESET=.FALSE.
     GOTO 191
 190 CONTINUE
     IF(COS .LT. RST) THEN
         DO 192 I=1,NVAR
 192     GD(I)=0.5D0
     ENDIF
 191 CONTINUE
     GNORM=SQRT(DOT(GRAD,GRAD,NVAR))
     JCYC=JCYC+1                                                       
     JNRST=JNRST+1                                                     
     IF (LNSTOP.NE.1 .AND. COS.GT.RST .AND. JNRST.LT.NRST) GOTO 300
                                                                       
     *                                                                 
     RESTART SECTION                                                   
     *                                                                 
                                                                       
 220 CONTINUE                                                          
     RESET=.TRUE.
     DO 230 I=1,NVAR                                                      
        XD(I)=XPARAM(I)-SIGN(DEL,GRAD(I))
 230 CONTINUE                                                          
                                                                       
 THIS CALL OF COMPFG IS USED TO CALCULATE THE SECOND-ORDER MATRIX IN H 
 IF THE NEW POINT HAPPENS TO IMPROVE THE RESULT, THEN IT IS KEPT.      
 OTHERWISE IT IS SCRAPPED, BUT STILL THE SECOND-ORDER MATRIX IS O.K.   
                                                                       
     CALL COMPFG (XD, .TRUE., FUNCT2,.TRUE.,GD,.TRUE.)               
     IF(.NOT. GEOOK .AND. SQRT(DOT(GD,GD,NVAR))/GNORM.GT.10.
    + AND.GNORM.GT.20)THEN

    THE GEOMETRY IS BADLY SPECIFIED IN THAT MINOR CHANGES IN INTERNAL
    COORDINATES LEAD TO LARGE CHANGES IN CARTESIAN COORDINATES, AND THESE
    LARGE CHANGES ARE BETWEEN PAIRS OF ATOMS THAT ARE CHEMICALLY BONDED
    TOGETHER.
     WRITE(6,'('' GRADIENTS OF OLD GEOMETRY, GNORM='',F13.6)')GNORM
     WRITE(6,'(6F12.6)')(GRAD(I),I=1,NVAR)
     GDNORM=SQRT(DOT(GD,GD,NVAR))
     WRITE(6,'('' GRADIENTS OF NEW GEOMETRY, GNORM='',F13.6)')GDNORM
     WRITE(6,'(6F12.6)')(GD(I),I=1,NVAR)
     WRITE(6,'(///20X,''CALCULATION ABANDONED AT THIS POINT!'')')
     WRITE(6,'(//10X,'' SMALL CHANGES IN INTERNAL COORDINATES ARE'',/
    +10X,'' CAUSING A LARGE CHANGE IN THE DISTANCE BETWEEN'',/
    110X,'' CHEMICALLY-BOUND ATOMS. THE GEOMETRY OPTIMISATION'',/
    210X,'' PROCEDURE WOULD LIKELY PRODUCE INCORRECT RESULTS'')')
     CALL GEOUT
     STOP
     ENDIF
     NCOUNT=NCOUNT+1                                                   
     DO 240 I=1,IHDIM                                                  
 240   HESINV(I)=0.0D00                                                      
     DO 270 I=1,NVAR                                                      
        II=I+NVAR*(I-1)-((I*(I-1))/2)                                     
        GGGGG=GRAD(I)-GD(I)                                               
        IF (ABS(GGGGG).LT.1.D-12) GO TO 250                            
        GGD=ABS(GRAD(I))                                                  
        IF (FUNCT2.LT.FUNCT1) GGD=ABS(GD(I))                               
        HESINV(II)=SIGN(DEL,GRAD(I))/GGGGG                                   
        IF (HESINV(II).LT.0.0D00.AND.GGD.LT.1.D-12) GO TO 250              
        IF (HESINV(II).LT.0.0D00) HESINV(II)=(TDEL*DEL)/GGD                    
        GO TO 260                                                      
 250    HESINV(II)=0.01D00                                                 
 260    CONTINUE                                                       
        IF (GGD.LT.1.D-12) GGD=1.D-12                                  
        PMSTEP=ABS(PMSTE/GGD)                                          
        IF (HESINV(II).GT.PMSTEP) HESINV(II)=PMSTEP                            
 270 CONTINUE                                                          
     JNRST=0                                                           
     IF(JCYC.LT.2) COSINE=1.D0
     IF(FUNCT2 .GE. FUNCT1) THEN
         IF(PRINT)WRITE (6,740) FUNCT1,FUNCT2
 740     FORMAT ('      FUNCTION VALUE=',F15.8,5X,                         
    1 'WILL NOT BE REPLACED BY VALUE=',F15.8,5X,                       
    1 'CALCULATED BY RESTART PROCEDURE',/)                             
         CALL COMPFG (XPARAM, .TRUE., FUNCT1,.TRUE.,GD,.FALSE.)               
         COSINE=1.D0
         ELSE
         IF( PRINT ) WRITE (6,750) FUNCT1,FUNCT2                           
 750     FORMAT ('      FUNCTION VALUE=',F15.8,5X,                         
    +'IS BEING REPLACED BY VALUE=',F15.8,5X,                           
    +'FOUND IN RESTART PROCEDURE',/,6X,                                
    +'THE CORRESPONDING X VALUES AND GRADIENTS ARE ALSO BEING REPLACED'
    +,/)                                                               
         FUNCT1=FUNCT2                                                         
         GNORM=0.0D00                                                      
         DO 280 I=1,NVAR                                                      
            XPARAM(I)=XD(I)                                                     
            GRAD(I)=GD(I)                                                     
 280     GNORM=GNORM+GRAD(I)**2                                               
         GNORM=SQRT(GNORM)                                                 
     ENDIF
     GO TO 340                                                         
                                                                       
     *                                                                 
     UPDATE VARIABLE-METRIC MATRIX                                     
     *                                                                 
                                                                       
 300 SY=0.0D00                                                         
     YHY=0.0D00                                                        
     DO 320 I=1,NVAR                                                      
        S=0.0D00                                                       
        DO 310 K=1,NVAR                                                   
           IK=I+NVAR*(K-1)-((K*(K-1))/2)                                  
           IF (K.GT.I) IK=K+NVAR*(I-1)-((I*(I-1))/2)                      
 310    S=S+HESINV(IK)*(GRAD(K)-GLAST(K))                                     
        GG(I)=S                                                        
        Y=GRAD(I)-GLAST(I)                                                
        YHY=YHY+GG(I)*Y                                                
 320 SY=SY+(XPARAM(I)-XLAST(I))*Y                                           
     DO 330 I=1,NVAR                                                      
        Y=XPARAM(I)-XLAST(I)                                                
     DO 330 K=I,NVAR                                                      
        IK=K+NVAR*(I-1)-((I*(I-1))/2)                                     
        HESINV(IK)=HESINV(IK)+Y*(XPARAM(K)-XLAST(K))
    +              /SY-GG(I)*GG(K)/YHY             
 330 CONTINUE                                                          
                                                                       
     *                                                                 
     ESTABLISH NEW SEARCH DIRECTION                                    
     *                                                                 
 340 PNLAST=PNORM                                                      
     PNORM=0.0D00                                                      
     DOTT=0.0D00                                                        
     DO 360 K=1,NVAR                                                      
        S=0.0D00                                                       
        DO 350 I=1,NVAR                                                   
           IK=I+NVAR*(K-1)-((K*(K-1))/2)                                  
           IF (K.GT.I) IK=K+NVAR*(I-1)-((I*(I-1))/2)                      
           S=S-HESINV(IK)*GRAD(I)                                             
 350    CONTINUE                                                       
        PVECT(K)=S                                                         
        PNORM=PNORM+PVECT(K)**2                                            
 360 DOTT=DOTT+PVECT(K)*GRAD(K)                                                 
     PNORM=SQRT(PNORM)                                                 
     COS=-DOTT/(PNORM*GNORM)                                            
     IF (JNRST.EQ.0) GO TO 380                                         
     IF (COS.LE.CNCADD.AND.YEAD.GT.1.0D00) GO TO 370                   
     IF (COS.LE.RST) GO TO 370                                       
     GO TO 380                                                         
 370 PNORM=PNLAST                                                      
     IF( PRINT )WRITE (6,760) COS                                
 760 FORMAT (//,5X, 10HSINCE COS=,F9.3,5X, 38HTHE PROGRAM WILL GO TO RE
    1START SECTION,/)                                                  
     GO TO 220                                                         
 380 CONTINUE                                                          
     IF ( PRINT ) WRITE (6,870) JCYC,FUNCT1                           
 870 FORMAT (1H , 25HAT THE BEGINNING OF CYCLE,I5, 24H  THE FUNCTION VA
    1LUE IS ,F13.6/, 26H  THE CURRENT POINT IS ...)                    
     IF(PRINT)WRITE (6,930) GNORM,COS                           
 930 FORMAT ( 18H  GRADIENT NORM = ,F10.4/, 17H  ANGLE COSINE = ,F10.4)
     NTO6=NVAR/6
     NREM6=NVAR-NTO6*6
     IINC1=-5
     IF (NTO6.LT.1.OR. .NOT. PRINT) GO TO 420
     DO 410 I=1,NTO6
        WRITE (6,'(/)')
        IINC1=IINC1+6
        IINC2=IINC1+5
        WRITE (6,890) (J,J=IINC1,IINC2)
        WRITE (6,900) (XPARAM(J),J=IINC1,IINC2)
        WRITE (6,910) (GRAD(J),J=IINC1,IINC2)
        WRITE (6,920) (PVECT(J),J=IINC1,IINC2)
 890 FORMAT (1H ,3X,  1HI,9X,I3,9(8X,I3))
 900 FORMAT (1H ,1X, 'XPARAM(I)',1X,F9.4,2X,9(F9.4,2X))
 910 FORMAT (1H ,1X, 'GRAD  (I)',F10.4,1X,9(F10.4,1X))
 920 FORMAT (1H ,1X, 'PVECT (I)',1X,F9.4,2X,9(F9.4,2X))
 410 CONTINUE
 420 CONTINUE
     IF (NREM6.LT.1.OR. .NOT. PRINT) GO TO 430
     WRITE (6,'(/)')
     IINC1=IINC1+6
     IINC2=IINC1+(NREM6-1)
     WRITE (6,890) (J,J=IINC1,IINC2)
     WRITE (6,900) (XPARAM(J),J=IINC1,IINC2)
     WRITE (6,910) (GRAD(J),J=IINC1,IINC2)
     WRITE (6,920) (PVECT(J),J=IINC1,IINC2)
 430 CONTINUE
     FI=FUNCT1                                                           
     LNSTOP=0                                                          
     ALPHA=ALPHA*PNLAST/PNORM                                          
     DO 440 I=1,NVAR                                                      
        GLAST(I)=GRAD(I)                                                  
 440 XLAST(I)=XPARAM(I)                                                     
     IF (JNRST.EQ.0) ALPHA=1.0D00                                      
     YEAD=ABS(ALPHA*DOTT)                                               
     IF(PRINT)WRITE (6,780) YEAD                                
 780 FORMAT (1H , 13H -ALPHA.P.G =,F18.6,/)                            
     IF (JNRST.NE.0.AND.YEAD.LT.EYEAD) THEN
         IF(MINPRT)WRITE (6,830)                                                     
 830     FORMAT(//,10X,'HERBERTS TEST SATISFIED - GEOMETRY OPTIMISED')
                                                                       
   FLEPO IS ENDING PROPERLY. THIS IS IMMEDIATELY BEFORE THE RETURN.    
                                                                       
         LAST=1
         CALL COMPFG (XPARAM, .TRUE., FUNCT,.TRUE.,GRAD,.FALSE.)
         IFLEPO=3
         RETURN                                                            
     ENDIF                                      
     SMVAL=FUNCT1                                                        
     CALL LINMIN(XPARAM,ALPHA,PVECT,NVAR,FUNCT1,OKF,OKC)
     NCOUNT=NCOUNT+1                                                   
     IF ( .NOT. OKF) THEN
         LNSTOP = 1
         IF(MINPRT)WRITE (6,800)                                     
 800     FORMAT (1H ,///,20X, 'NO POINT LOWER IN ENERGY ',
    +    'THAN THE STARTING POINT COULD BE FOUND ',
    1    'IN THE LINE MINIMIZATION')                   
         FUNCT1=SMVAL                                                        
         DO 480 I=1,NVAR                                                      
            GRAD(I)=GLAST(I)                                                  
            XPARAM(I)=XLAST(I)                                                  
 480     CONTINUE                                                          
         IF (JNRST.EQ.0)THEN
             WRITE (6,820)                                     
 820        FORMAT (1H ,//,20X, 'SINCE COS WAS JUST RESET,THE SEARCH ',
    +        'IS BEING ENDED')                                                          
                                                                       
             FLEPO IS ENDING BADLY. THIS IS IMMEDIATELY BEFORE THE RETURN.       
                                                                       
             LAST=1
             CALL COMPFG (XPARAM, .TRUE., FUNCT,.TRUE.,GRAD,.FALSE.)                 
             IFLEPO=4
             RETURN
         ENDIF
         IF(PRINT)WRITE (6,810)                                     
 810     FORMAT (1H ,20X, 'COS WILL BE RESET AND ANOTHER '
    +    ,'ATTEMPT MADE')   
         COS=0.0D00                                                        
         GO TO 670                                                         
     ENDIF
   WE WANT ACCURATE DERIVATIVES AT THIS POINT                          
                                                                       
   LINMIN DOES NOT GENERATE ANY DERIVATIVES, THEREFORE COMPFG MUST BE  
   CALLED TO END THE SEARCH                                            
                                                                       
          IF(RESET) THEN
          DO 642 J=1,NVAR
 642          GRAD(J)=0.D0
          RESET=.FALSE.
          ENDIF
     CALL COMPFG (XPARAM, .TRUE., FUNCT1,.TRUE.,GRAD,.TRUE.)                 
     IF (.NOT. OKC .AND. MINPRT)WRITE (6,940) JCYC
 940 FORMAT ( 23H0LINMIN FAILED AT CYCLE,I5/,  1H0)                    
     XN=0.0D00                                                         
     DO 510 K=1,NVAR                                                      
 510   XN=XN+XPARAM(K)**2                                                     
     XN=SQRT(XN)                                                       
     TX=ABS(ALPHA*PNORM)                                               
     IF (XN.NE.0.0D00) TX=TX/XN                                        
     TF=ABS(FI-FUNCT1)                                                   
#      IF (FUNCT1.NE.0.0D00) TF=TF/ABS(FUNCT1)                               
     IF (PRINT) WRITE (6,950) NCOUNT,TX,TF,GNORM 
 950 FORMAT ( 23H  TERMINATION TESTS ...,/, 24H     NUMBER OF COUNTS = 
    1,I5/, 28H     RELATIVE CHANGE IN X = ,F13.6/, 28H     RELATIVE CHA
    2NGE IN F = ,F13.6/, '     GRADIENT NORM        = ',F13.6,//)      
 530 IF (NCOUNT.GE.MAXEND) THEN
         WRITE (6,960)                                                     
 960     FORMAT ( 33H0TERMINATION FROM TOO MANY COUNTS)                    
         LAST=1
         CALL COMPFG (XPARAM, .TRUE., FUNCT,.TRUE.,GRAD,.FALSE.)                 
         IFLEPO=5
         RETURN                                                            
     ENDIF
     IF (TX.LE.TOLERX) THEN
         IF(MINPRT) WRITE (6,970)                                     
 970     FORMAT ( 20H0TEST ON X SATISFIED)                                 
         GO TO 610                                                         
     ENDIF
     IF (TF.LE.TOLERF) THEN
         IF(MINPRT) WRITE (6,980)                                     
 980     FORMAT (' HEAT OF FORMATION TEST SATISFIED') 
         GO TO 610                                                         
     ENDIF
     IF (GNORM.LE.TOLERG*ROOTV) THEN
         IF(MINPRT) WRITE (6,990)                                     
 990     FORMAT ( 27H0TEST ON GRADIENT SATISFIED)                          
         GOTO 610
     ENDIF
     GOTO 670
 610 DO 620 I=1,NVAR                                                      
       IF (ABS(GRAD(I)).GT.TOLERG)THEN
          IREPET=IREPET+1                                                   
          IF (IREPET.GT.1) GO TO 640                                        
          FREPF=FUNCT1                                                        
          COS=0.0D00                                                        
 640      CONTINUE                                                          
          IF(MINPRT) WRITE (6,1000)TOLERG
1000      FORMAT (20X,'HOWEVER, A COMPONENT OF GRADIENT IS ',
    +     'LARGER THAN',F6.2 ,/)        
          IF (ABS(FUNCT1-FREPF).GT.EINC) IREPET=0                             
          IF (IREPET.GT.IGG1) THEN
              WRITE (6,840)IGG1,EINC
 840 FORMAT (10X,' THERE HAVE BEEN',I2,' ATTEMPTS TO REDUCE THE ',
    +' GRADIENT.',/10X,' DURING THESE ATTEMPTS THE ENERGY DROPPED',
    +' BY LESS THAN',F4.1,' KCAL/MOLE',/
    +10X,' FURTHER CALCULATION IS NOT JUSTIFIED AT THIS TIME.',/
    +10X,' TO CONTINUE, START AGAIN WITH THE WORD "PRECISE"' ) 
              LAST=1
              CALL COMPFG (XPARAM, .TRUE., FUNCT,.TRUE.,GRAD,.FALSE.)                 
              IFLEPO=8
              RETURN                                                            
          ELSE
          GOTO 670
          ENDIF 
       ENDIF
 620 CONTINUE
     IF(MINPRT) WRITE (6,860)                                                     
 860 FORMAT ( 23H PETERS TEST SATISFIED ) 
     LAST=1
     CALL COMPFG (XPARAM, .TRUE., FUNCT,.TRUE.,GRAD,.FALSE.)                 
     IFLEPO=6
     RETURN                                                            

   ALL TESTS HAVE FAILED, WE NEED TO DO ANOTHER CYCLE.

 670 CONTINUE                                                          
     BSMVF=ABS(SMVAL-FUNCT1)                                             
     IF (BSMVF.GT.10.D00) COS = 0.0D00                                 
     DEL=0.002D00                                                      
     IF (BSMVF.GT.1.0D00) DEL=DELL/2.0D00                              
     IF (BSMVF.GT.5.0D00) DEL=DELL                                     
     TX2 = SECOND ()                                                
     TCYCLE=TX2-TX1                                                    
     TX1=TX2                                                           
                                                                       
 END OF ITERATION LOOP, EVERYTHING IS STILL O.K. SO GO TO              
 NEXT ITERATION, IF THERE IS ENOUGH TIME LEFT.                         
                                      
     IF(TCYCLE.LT.100000.D0)CYCMX=MAX(CYCMX,TCYCLE)
     TLEFT=TLEFT-TCYCLE
     IF(TLEFT.LT.0)TLEFT=-0.1D0
     IF(TCYCLE.GT.1.D5)TCYCLE=0.D0
     IF(MINPRT) WRITE(6,3141)JCYC,TCYCLE,TLEFT,GNORM,FUNCT1
3141 FORMAT(' CYCLE:',I3,' TIME:',F8.2,' TIME LEFT:',F9.2,
    +' GRADIENT:',F10.3,' HEAT:',F10.5)
     IF (TLEFT.GT.SFACT*CYCMX) GO TO 190                         
     WRITE(6,700)                                                      
 700 FORMAT (20X, 42HTHERE IS NOT ENOUGH TIME FOR ANOTHER CYCLE,/,30X, 
    118HNOW GOING TO FINAL)                                            
     MDFP(9)=1
     CALL DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP)
                                                                       
                                                                       
     END                                                               