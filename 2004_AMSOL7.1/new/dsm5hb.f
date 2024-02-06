      SUBROUTINE DSM5HB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'FFILES.i'
C***********************************************************************
C     DERIVATIVE OF SURFACE TENSIONS WHICH ARE LATER
C   MULTIPLIED TIMES SASA TO GET THE CDS TERM.
C
C***********************************************************************
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /HEXSTM/ HRFN4(100),CRFNH4,CNRFN4,CORFN5,                  GDH0797
     X                CRFNH5,OORFN5,ONRFN4,SSRFN5,OPRFN5,               GDH0797
     1                FCRFN,CLCRFN,BRCRFN,CNSTC2,TNCSC3,TNCSC2,         GDH0797
     2                HOHST1,HNNST1, SPRFN5,OSIBD5                      PDW0901
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),
     1                HBCORC(100),QKNOT(100),QKONE(100)
      COMMON /HBORDS/ HBORD(NUMATM), HB(NUMATM)
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /OPTIMI/ IMP,IMP0
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM),RMAX(NUMATM)
     1               ,URLIO(3,NUMATM,NUMATM)                            GDH0597
      SQRT3=SQRT(3.0D0)
      EXPONE=EXP(1.0D0)
      DO 950 IAT=1,NUMAT
         NATIAT=NAT(IAT)
         ARED3=ATAR(IAT)*1D-3
CCC This is the NEW sigma Hk term, based on a Tkk                       
CCC NOTE: Hydrogen only has this extra surface tension when bonded to   
CCC   Carbon, Oxygen, and Nitrogen.                                     
         HB(IAT)=0.0                                                    
         RTKK=0.0D0                                                     
         IF (NAT(IAT).EQ.1) THEN                                        
            DO 150 J=1,NUMAT                                            
               RTKK=0.0D0                                               
               IF (NAT(J).EQ.6) THEN                                      
                  RECCC=1.85D0                                          
                  DELTAR=0.3D0                                          
                  BCCC=1.0D0                                            
                  CUTOF1=RECCC+DELTAR                                   
                  IF (RLIO(IAT,J).LT.CUTOF1) THEN                         
         RTKK=EXP(1.0D0-BCCC/(1.0D0-(RLIO(IAT,J)-RECCC)/DELTAR))/EXPONE  BJL1003
                  ENDIF                                                 
                  HB(IAT)=HB(IAT)+RTKK*HRFN4(NAT(J))*ARED3              
               ELSE IF (NAT(J).EQ.7) THEN
                  RECCC=1.7D0                                           
                  DELTAR=0.3D0                                          
                  BCCC=1.0D0                                            
                  CUTOF1=RECCC+DELTAR                                   
                  IF (RLIO(IAT,J).LT.CUTOF1) THEN
                     RTKK=EXP(1.0D0-(BCCC/                              
     X                  (1.0D0-(RLIO(IAT,J)-RECCC)/DELTAR)))
     X                  /EXPONE
                  ENDIF                                                 
                  HB(IAT)=HB(IAT)+RTKK*HRFN4(NAT(J))*ARED3              
               ELSE IF (NAT(J).EQ.8) THEN                                  
                  RECCC=1.7D0                                           
                  DELTAR=0.3D0                                          
                  BCCC=1.0D0                                            
                  CUTOF1=RECCC+DELTAR                                   
                  IF (RLIO(IAT,J).LT.CUTOF1) THEN                         
                     RTKK=EXP(1.0D0-(BCCC/                              
     X                  (1.0D0-((RLIO(IAT,J)-RECCC)/DELTAR))))          
     X                  /EXPONE
                  ENDIF                                                 
                  HB(IAT)=HB(IAT)+RTKK*HRFN4(NAT(J))*ARED3              
               ELSE IF (NAT(J).EQ.16) THEN                                 
                  RECCC=2.14D0                                          
                  DELTAR=0.3D0                                          
                  BCCC=1.0D0                                            
                  CUTOF1=RECCC+DELTAR                                   
                  IF (RLIO(IAT,J).LT.CUTOF1) THEN                         
                     RTKK=EXP(1.0D0-(BCCC/                              
     X                  (1.0D0-((RLIO(IAT,J)-RECCC)/DELTAR))))          
     X                  /EXPONE
                  ENDIF                                                 
                  HB(IAT)=HB(IAT)+RTKK*HRFN4(NAT(J))*ARED3              
               ENDIF                                                    
150         CONTINUE                                                    
         ENDIF                                                          
CCC These are the NEW OC CDS terms. They are of the Tkk form.           
         IF (NAT(IAT).EQ.8) THEN                                        
                 RTKK=0.0D0                                             
            DO 200 J=1,NUMAT
              IF (NAT(J).EQ.6) THEN                                       
                 BCCC=1.0D0                                             
                 RECCC=1.33D0                                           
                 DELTAR=0.1D0                                           
                 CUTOF1=RECCC+DELTAR                                    
                 IF (RLIO(IAT,J).LT.CUTOF1) THEN                          
                     RTKK=RTKK+EXP(1.0D0-(BCCC/                         
     X                  (1.0D0-((RLIO(IAT,J)-RECCC)/DELTAR))))          
     X                  /EXPONE
                 ENDIF                                                  
              ENDIF                                                     
200         CONTINUE                                                    
            HB(IAT)=HB(IAT)+RTKK*CORFN5*ARED3                           
         ENDIF                                                          
CCC Add the Nitrogen bonded to Carbon Surface Tension terms             
CCC The terms are a sum over all N bonded to C of a COT times the       
C   sum of all other atoms attached to the C attached to the N raised   
C   to a power.  See the paper for more explicit details.               
         IF (NAT(IAT).EQ.7) THEN                                        
            BCCC=1.0D0                                                  
            RTKK=0.0D0                                                  
            RTKKS=0.0D0                                                 
            RECCC=1.84D0                                                
            RECCH=1.55D0                                                
            DELTAR=0.3D0                                                
            CUTOF1=RECCC+DELTAR                                         
            DO 250 J=1,NUMAT                                            
              IF (NAT(J).EQ.6) THEN                                       
                 IF (RLIO(IAT,J).LT.CUTOF1) THEN                          
                    RTKK=0.0D0                                          
                    RTKK=EXP(1.0D0-BCCC/
     X                 (1.0D0-(RLIO(IAT,J)-RECCC)/DELTAR))
     X              /EXPONE
                     RTKK3=0.0D0                                        
                     DO 251 K=1,NUMAT                                   
                       RTKK2=0.D0                                       
                       IF (K.NE.IAT.AND.K.NE.J) THEN                    
                       IF (NAT(K).EQ.1) THEN                            
                           CUTOF2=RECCH+DELTAR                          
                           RHLD=RECCH                                   
                       ELSE                                             
                           CUTOF2=RECCC+DELTAR                          
                           RHLD=RECCC                                   
                       ENDIF                                            
                       IF (RLIO(J,K).LT.CUTOF2) THEN                      
                        RTKK2=EXP(1.0D0-BCCC/
     .                  (1.0D0-(RLIO(J,K)-RHLD)/DELTAR))
     .                  /EXPONE
                      ENDIF                                             
                      ENDIF                                             
                        RTKK3=RTKK3+RTKK2                               
251                  CONTINUE                                           
                     RTKKS=RTKKS+RTKK*RTKK3*RTKK3                       
                 ENDIF                                                  
              ENDIF                                                     
250         CONTINUE                                                    
                    HB(IAT)=HB(IAT)+(RTKKS**1.3D0)*ARED3*CNRFN4         
         ENDIF                                                          
CCC These are the NEW O-N CDS terms. They are of the Tkk form.          
         IF (NAT(IAT).EQ.8) THEN                                        
            RTKK=0.0D0                                                  
            BCCC=1.0D0                                                  
            RECCC=1.50D0                                                
            DELTAR=0.3D0                                                
            CUTOF1=RECCC+DELTAR                                         
            DO 350 J=1,NUMAT                                            
              IF (NAT(J).EQ.7) THEN                                       
                 IF (RLIO(IAT,J).LT.CUTOF1) THEN                          
                     RTKK=RTKK+EXP(1.0D0-BCCC/
     X               (1.0D0-(RLIO(IAT,J)-RECCC)/DELTAR))
     X                  /EXPONE
                 ENDIF                                                  
              ENDIF                                                     
350         CONTINUE                                                    
            HB(IAT)=HB(IAT)+RTKK*ONRFN4*ARED3                           
         ENDIF                                                          
CCC These are the NEW O-P CDS terms. They are of the Tkk form.
         IF (NAT(IAT).EQ.8) THEN                                        
            RTKK=0.0D0                                                  
            BCCC=1.0D0                                                  
            RECCC=2.00D0                                                
            DELTAR=0.3D0                                                
            CUTOF1=RECCC+DELTAR                                         
            DO 360 J=1,NUMAT                                            
              IF (NAT(J).EQ.15) THEN
                 IF (RLIO(IAT,J).LT.CUTOF1) THEN
                     RTKK=RTKK+EXP(1.0D0-BCCC/
     X               (1.0D0-(RLIO(IAT,J)-RECCC)/DELTAR))
     X                  /EXPONE
                 ENDIF                                                  
              ENDIF                                                     
360         CONTINUE                                                    
            HB(IAT)=HB(IAT)+RTKK*OPRFN5*ARED3                           
         ENDIF                                                          
CC Add the Oxygen - Oxygen Surface Tension term                         
CCC   sigma(OO) surface tension.
         RTKK=0.0D0                                                     
         KCCAN=0                                                        
         IF (NAT(IAT).EQ.8) THEN                                        
            RECCC=2.75D0                                                
            DELTAR=0.3D0                                                
            BCCC=1.0D0                                                  
            DO 400 J=1,NUMAT                                            
               IF (J.EQ.IAT)GOTO 400                                     
               IF (NAT(J).EQ.8) THEN
                  CUTOF1=RECCC+DELTAR                                   
                  IF (RLIO(IAT,J).LT.CUTOF1) THEN
                     RTKK=RTKK+EXP(1.0D0-BCCC/
     X                  (1.0D0-(RLIO(IAT,J)-RECCC)/DELTAR))
     X                  /EXPONE
                     KCCAN=KCCAN+1                                      
                  ENDIF                                                 
               ENDIF                                                    
 400        CONTINUE                                                    
         ENDIF                                                          
         IF (KCCAN.GE.1.AND.RTKK.GT.0.00000001) THEN
            XRTKO=0.0D0                                                 
            SRTNO=-1.0D0*RTKK                                           
            REYYO=-0.4D0                                                
            DELTAO=0.4D0                                                
            BYYO=1.0D0                                                  
            CUTOF1=REYYO+DELTAO                                         
            IF (SRTNO.LT.CUTOF1) THEN
               XRTKO=EXP(1.0D0-BYYO/(1.0D0-(SRTNO-REYYO)/DELTAO))
     X            /EXPONE
            ENDIF                                                       
            HB(IAT)=HB(IAT)+XRTKO*OORFN5*ARED3                          
         ENDIF                                                          
CCC  These are the SM5 CC CDS terms. They are of the Tkk form.          
         IF (NAT(IAT).EQ.6) THEN                                        
            DO 500 KCCC=1,2                                             
               BCCC=1.0D0                                               
               RTKK=0.0D0                                               
               IF (KCCC.EQ.1) THEN
                  RECCC=1.84D0                                          
                  DELTAR=0.3D0                                          
               ELSE IF (KCCC.EQ.2) THEN
                  RECCC=1.27D0                                          
                  DELTAR=0.07D0                                         
               ENDIF                                                    
               CUTOF1=RECCC+DELTAR                                      
               DO 450 J=1,NUMAT                                         
                 IF (J.EQ.IAT) GOTO 450
                 IF (NAT(J).EQ.6) THEN
                    IF (RLIO(IAT,J).LT.CUTOF1) THEN
                        RTKK=RTKK+EXP(1.0D0-BCCC/
     X                     (1.0D0-(RLIO(IAT,J)-RECCC)/DELTAR))
     X                  /EXPONE
                    ENDIF                                               
                 ENDIF                                                  
450            CONTINUE                                                 
               IF (KCCC.EQ.1) THEN
                  HB(IAT)=HB(IAT)+RTKK*CRFNH4*ARED3                     
               ELSE IF (KCCC.EQ.2) THEN
                  HB(IAT)=HB(IAT)+RTKK*CRFNH5*ARED3                     
               ENDIF                                                    
500         CONTINUE                                                    
         ENDIF                                                          
CC Add the Sulfur - Sulfur Surface Tension term                         
CCC   sigma(SS) surface tension.                                        
         RTKK=0.0D0                                                     
         IF (NAT(IAT).EQ.16) THEN                                       
            RECCC=2.75D0                                                
            DELTAR=0.3D0                                                
            BCCC=1.0D0                                                  
            DO 550 J=1,NUMAT                                            
               IF (J.EQ.IAT) GOTO 550
               IF (NAT(J).EQ.16) THEN
                  CUTOF1=RECCC+DELTAR                                   
                  IF (RLIO(IAT,J).LT.CUTOF1) THEN
                     RTKK=EXP(1.0D0-BCCC/
     X                  (1.0D0-(RLIO(IAT,J)-RECCC)/DELTAR))
     X                  /EXPONE
                  ENDIF                                                 
                  HB(IAT)=HB(IAT)+RTKK*SSRFN5*ARED3                     
               ENDIF                                                    
550         CONTINUE                                                    
         ENDIF                                                          
950   CONTINUE
      END

