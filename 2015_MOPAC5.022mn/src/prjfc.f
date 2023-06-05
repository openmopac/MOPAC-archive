      SUBROUTINE PRJFC(F,xparam,nvar,*)                                 CSTP 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION SDET(2)                                                 LF0913 - for call to DGEDI only
C                                                                       
C  CALCULATES PROJECTED FORCE CONSTANT MATRIX (F).                      
C  THIS ROUTINE CAME ORIGINALLY FROM POLYRATE. IT IS USED BY PERMISSION
C  OF D. TRUHLAR. THE CURRENT VERSION IS LIFTED FROM GAMESS AND 
C  ADAPTED BY F.JENSEN, ODENSE, DK             
C  IF WE ARE AT A STATIONARY POINT (STPT=.T.), I.E. GNORM .LT. 10,      
C  THEN THE ROTATIONAL AND TRANSLATIONAL MODES ARE PROJECTED OUT        
C  AND THEIR FREQUENCIES BECOME IDENTICAL ZERO. IF NOT AT A STATIONARY  
C  POINT THEN THE MASS-WEIGHTED GRADIENT IS ALSO PROJECTED OUT AND      
C  THE CORRESPONDING FREQUENCY BECOME ZERO.                             
C ************************************************                      
C   X : MASS-WEIGHTED COORDINATE                                        
C   DX: NORMALIZED MASS-WEIGHTED GRADIENT VECTOR                        
C   F : MASS-WEIGHTED FORCE CONSTANT MATRIX                             
C   RM: INVERSION OF SQUARE ROOT OF MASS                                
C   P, COF: BUFFER                                                      
C                                                                       
      COMMON /ATMASS/ ATMASS(NUMATM)
      DIMENSION X(MAXPAR),RM(MAXPAR),F(MAXPAR,MAXPAR),                  
     *          P(MAXPAR,MAXPAR),COF(MAXPAR,MAXPAR)                     
      DIMENSION TENS(3,3,3),ROT(3,3),SCR(3,3),ISCR(6),CMASS(3)          
      dimension coord(3,numatm),dx(maxpar),xparam(maxpar)
      PARAMETER (ZERO=0.0d+00, ONE=1.0d+00, EPS=1.0d-14,                
     *           CUT5=1.0d-05, CUT8=1.0d-08)                            
C                                                                       
C TOTALLY ASYMMETRIC CARTESIAN TENSOR.                                  
      DATA TENS/ 0.0d+00,  0.0d+00,  0.0d+00,                           
     X           0.0d+00,  0.0d+00, -1.0d+00,                           
     X           0.0d+00,  1.0d+00,  0.0d+00,                           
     Y           0.0d+00,  0.0d+00,  1.0d+00,                           
     Y           0.0d+00,  0.0d+00,  0.0d+00,                           
     Y          -1.0d+00,  0.0d+00,  0.0d+00,                           
     Z           0.0d+00, -1.0d+00,  0.0d+00,                           
     Z           1.0d+00,  0.0d+00,  0.0d+00,                           
     Z           0.0d+00,  0.0d+00,  0.0d+00  /                         
C                                                                       
      natm=nvar/3                                                        
      nc1=nvar
      ij=1
      do 2 i=1,natm                                                    
         coord(1,i)=xparam(ij)
         coord(2,i)=xparam(ij+1)
         coord(3,i)=xparam(ij+2)
         ij=ij+3
2     continue                                                          
C     CALCULATE 1/SQRT(MASS)                                            
      L=0                                                               
      DO 3 I=1,NATM                                                     
         TMP=ONE/SQRT(ATMASS(I))                                        
         DO 3 J=1,3                                                     
            L=L+1                                                       
3           RM(L)=TMP                                                   
C     PREPARE GRADIENT                                                  
         DO 4 I=1,NC1                                                   
4        DX(I)=ZERO                                                     
C     FIND CMS AND CALCULATED MASS WEIGHTED COORDINATES                 
      totm=zero                                                         
      cmass(1)=zero                                                     
      cmass(2)=zero                                                     
      cmass(3)=zero                                                     
      DO 6 I=1,NATM                                                     
         TOTM=TOTM+ATMASS(I)                                            
         DO 6 J=1,3                                                     
            CMASS(J)=CMASS(J)+ATMASS(I)*COORD(J,I)                      
6     CONTINUE                                                          
      DO 7 J=1,3                                                        
7     CMASS(J)=CMASS(J)/TOTM                                            
      L=0                                                               
      DO 8 I=1,NATM                                                     
         DO 8 J=1,3                                                     
         TMP=SQRT(ATMASS(I))                                            
             L=L+1                                                      
             X(L)=TMP*(COORD(J,I)-CMASS(J))                             
8     CONTINUE                                                          
c     WRITE(6,9020)                                                     
c     CALL prsq(f,nc1,nc1,maxpar,1)                                          
c9020 FORMAT(/1X,'ENTER THE SUBROUTINE <PRJFC>'//                       
c    *        1X,'UNPROJECTED FORCE CONSTANT MATRIX (HARTREE/BOHR**2)') 
c     WRITE(6,*)' MASS-WEIGHTED COORDINATES AND CORRESPONDING GRADIENT' 
c     DO 9 I=1,NC1                                                      
c9       WRITE(6,*)X(I),DX(I)                                           
C                                                                       
C 2. COMPUTE INERTIA TENSOR.                                            
      DO 10 I=1,3                                                       
       DO 10 J=1,3                                                      
   10   ROT(I,J)=ZERO                                                   
      DO 20 I=1,NATM                                                    
       L=3*(I-1)+1                                                      
       ROT(1,1)=ROT(1,1)+X(L+1)**2+X(L+2)**2                            
       ROT(1,2)=ROT(1,2)-X(L)*X(L+1)                                    
       ROT(1,3)=ROT(1,3)-X(L)*X(L+2)                                    
       ROT(2,2)=ROT(2,2)+X(L)**2+X(L+2)**2                              
       ROT(2,3)=ROT(2,3)-X(L+1)*X(L+2)                                  
   20  ROT(3,3)=ROT(3,3)+X(L)**2+X(L+1)**2                              
      ROT(2,1)=ROT(1,2)                                                 
      ROT(3,1)=ROT(1,3)                                                 
      ROT(3,2)=ROT(2,3)                                                 
C                                                                       
CHECK THE INERTIA TENSOR.                                               
      CHK=ROT(1,1)*ROT(2,2)*ROT(3,3)                                    
      IF(ABS(CHK).GT.CUT8) GO TO 21                                     
c     WRITE(6,23)                                                       
c  23 FORMAT(/1X,'MATRIX OF INERTIA MOMENT')                            
c     CALL PRSQ(ROT,3,3,3,3)                                              
      IF(ABS(ROT(1,1)).GT.CUT8) GO TO 11                                
C X=0                                                                   
      IF(ABS(ROT(2,2)).GT.CUT8) GO TO 12                                
C X,Y=0                                                                 
      IF(ABS(ROT(3,3)).GT.CUT8) GO TO 13                                
      IF (DOPRNT) WRITE(6,14) ROT(1,1),ROT(2,2),ROT(3,3)                CIO
   14 FORMAT(1X,'EVERY DIAGONAL ELEMENTS ARE ZERO ?',3F20.10)           
      RETURN                                                            
C                                                                       
C* 1. X,Y=0 BUT Z.NE.0                                                  
   13 ROT(3,3)=ONE/ROT(3,3)                                             
      GO TO 22                                                          
C Y.NE.0                                                                
   12 IF(ABS(ROT(3,3)).GT.CUT8) GO TO 15                                
C* 2. X,Z=0 BUT Y.NE.0                                                  
      ROT(2,2)=ONE/ROT(2,2)                                             
      GO TO 22                                                          
C X.NE.0                                                                
   11 IF(ABS(ROT(2,2)).GT.CUT8) GO TO 16                                
      IF(ABS(ROT(3,3)).GT.CUT8) GO TO 17                                
C* 3. Y,Z=0 BUT X.NE.0                                                  
      ROT(1,1)=ONE/ROT(1,1)                                             
      GO TO 22                                                          
C* 4. X,Y.NE.0 BUT Z=0                                                  
   16 DET=ROT(1,1)*ROT(2,2)-ROT(1,2)*ROT(2,1)                           
      TRP=ROT(1,1)                                                      
      ROT(1,1)=ROT(2,2)/DET                                             
      ROT(2,2)=TRP/DET                                                  
      ROT(1,2)=-ROT(1,2)/DET                                            
      ROT(2,1)=-ROT(2,1)/DET                                            
      GO TO 22                                                          
C* 5. X,Z.NE.0 BUT Y=0                                                  
   17 DET=ROT(1,1)*ROT(3,3)-ROT(1,3)*ROT(3,1)                           
      TRP=ROT(1,1)                                                      
      ROT(1,1)=ROT(3,3)/DET                                             
      ROT(3,3)=TRP/DET                                                  
      ROT(1,3)=-ROT(1,3)/DET                                            
      ROT(3,1)=-ROT(3,1)/DET                                            
      GO TO 22                                                          
C* 6. Y,Z.NE.0 BUT X=0                                                  
   15 DET=ROT(3,3)*ROT(2,2)-ROT(3,2)*ROT(2,3)                           
      TRP=ROT(3,3)                                                      
      ROT(3,3)=ROT(2,2)/DET                                             
      ROT(2,2)=TRP/DET                                                  
      ROT(3,2)=-ROT(3,2)/DET                                            
      ROT(2,3)=-ROT(2,3)/DET                                            
      GO TO 22                                                          
   21 CONTINUE                                                          
C                                                                       
C.DEBUG.                                                                
c      CALL PRSQ(TENS(1,1,1),3,3,3,3)                                     
c      CALL PRSQ(TENS(1,1,2),3,3,3,3)                                     
c      CALL PRSQ(TENS(1,1,3),3,3,3,3)                                     
c      CALL PRSQ(ROT,3,3,3,3)                                             
C                                                                       
C 4. COMPUTE INVERSION MATRIX OF ROT.                                   
C     CALL MXLNEQ(ROT,3,3,DET,JRNK,EPS,SCR,+0)                          
C     IF(JRNK.LT.3) RETURN 1                                            CSTP (stop)
      INFO=0                                                            
      CALL DGEFA(ROT,3,3,ISCR,INFO,*9999)                                CSTP(call)
      IF(INFO.NE.0) RETURN 1                                            CSTP (stop)
      DET=ZERO                                                          
      CALL DGEDI(ROT,3,3,ISCR,SDET,SCR,1,*9999)                           CSTP(call) / LF0913 (replaced DET scalar with SDET array)
C                                                                       
   22 CONTINUE                                                          
c     WRITE (6,702)                                                     
c 702 FORMAT(/1X,'INVERSE MATRIX OF MOMENT OF INERTIA.')                
c     CALL PRSQ(ROT,3,3,3,3)                                              
C                                                                       
C 5. TOTAL MASS ---> TOTM.                                              
C                                                                       
C 6. COMPUTE P MATRIX                                                   
C    ----------------                                                   
      DO 100 IP=1,NATM                                                  
       INDX=3*(IP-1)                                                    
       DO 100 JP=1,IP                                                   
        JNDX=3*(JP-1)                                                   
        DO 70 IC=1,3                                                    
         JEND=3                                                         
         IF(JP.EQ.IP) JEND=IC                                           
         DO 70 JC=1,JEND                                                
          SUM=ZERO                                                      
          DO 50 IA=1,3                                                  
           DO 50 IB=1,3                                                 
            IF(TENS(IA,IB,IC).EQ.0) GO TO 50                            
            DO 30 JA=1,3                                                
             DO 30 JB=1,3                                               
              IF(TENS(JA,JB,JC).EQ.0) GO TO 30                          
              SUM=SUM+TENS(IA,IB,IC)*TENS(JA,JB,JC)*ROT(IA,JA)*         
     &                X(INDX+IB)*X(JNDX+JB)                             
   30         CONTINUE                                                  
   50       CONTINUE                                                    
          II=INDX+IC                                                    
          JJ=JNDX+JC                                                    
          P(II,JJ)=SUM+DX(II)*DX(JJ)                                    
          IF(IC.EQ.JC) P(II,JJ)=P(II,JJ)+ONE/(RM(II)*RM(JJ)*TOTM)       
   70     CONTINUE                                                      
  100   CONTINUE                                                        
C                                                                       
C 7. COMPUTE DELTA(I,J)-P(I,J)                                          
      DO 110 I=1,NC1                                                    
       DO 110 J=1,I                                                     
        P(I,J)=-P(I,J)                                                  
        IF(I.EQ.J) P(I,J) = ONE +P(I,J)                                 
  110   CONTINUE                                                        
C                                                                       
C 8. NEGLECT SMALLER VALUES THAN 10**-8.                                
      DO 120 I=1,NC1                                                    
       DO 120 J=1,I                                                     
        IF(ABS(P(I,J)).LT.CUT8) P(I,J)=ZERO                             
        P(J,I)=P(I,J)                                                   
  120   CONTINUE                                                        
C                                                                       
C.DEBUG.                                                                
c     WRITE(6,703)                                                      
c 703 FORMAT(/1X,'PROJECTION MATRIX')                                   
c     CALL PRSQ(P,NC1,NC1,NC1)                                          
c     CALL PRSQ(P,NC1,NC1,maxpar,3)                                       
C                                                                       
C 10. POST AND PREMULTIPLY F BY P.                                      
C     USE COF FOR SCRATCH.                                              
      DO 150 I=1,NC1                                                    
       DO 150 J=1,NC1                                                   
        SUM=ZERO                                                        
        DO 140 K=1,NC1                                                  
  140    SUM=SUM+F(I,K)*P(K,J)                                          
  150   COF(I,J)=SUM                                                    
C                                                                       
C 11. COMPUTE P*F*P.                                                    
      DO 200 I=1,NC1                                                    
       DO 200 J=1,NC1                                                   
        SUM=ZERO                                                        
        DO 190 K=1,NC1                                                  
  190    SUM=SUM+P(I,K)*COF(K,J)                                        
  200   F(I,J)=SUM                                                      
C                                                                       
c     WRITE(6,9030)                                                     
c     CALL prsq(f,nc1,nc1,maxpar,1)                                          
c9030 FORMAT(/1X,'LEAVE THE SUBROUTINE <PRJFC>'//                       
c    *        1X,'PROJECTED FORCE CONSTANT MATRIX (HARTREE/BOHR**2)')   
      RETURN                                                            
 9999 RETURN 1                                                          CSTP
      END                                                               
