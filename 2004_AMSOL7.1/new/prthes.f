      SUBROUTINE PRTHES(EIGVAL,NVAR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                       
      INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),
     1                PMAT(MAXPAR,MAXPAR)                                IR0494
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR), 
     $U(MAXPAR,MAXPAR),DD,RMIN,RMAX,OMIN,XLAMD,XLAMD0,SKAL,
     $MODE,NSTEP,NEGREQ,IPRNT
      DIMENSION EIGVAL(MAXPAR)
      SAVE
         IF (IPRNT.GE.4) THEN                                                   
         WRITE(JOUT,*)' '                                                    
         WRITE(JOUT,*)'              HESSIAN MATRIX'                        
         LOW=1                                                                  
         NUP=8                                                                  
540      NUP=MIN(NUP,NVAR)                                                      
         WRITE(JOUT,1000) (I,I=LOW,NUP)                    
         DO 550 I=1,NVAR                                                        
         WRITE(JOUT,1010) I,(HESS(I,J),J=LOW,NUP)           
550      CONTINUE                                                               
         NUP=NUP+8                                                              
         LOW=LOW+8                                                              
         IF(LOW.LE.NVAR) GOTO 540                                               
         ENDIF                                                                  
         WRITE(JOUT,*)' '                                                 
         WRITE(JOUT,*)'              HESSIAN EIGENVALUES AND -VECTORS'     
         LOW=1                                                                  
         NUP=8                                                                  
560      NUP=MIN(NUP,NVAR)                                                      
         WRITE(JOUT,1000) (I,I=LOW,NUP)                         
         WRITE(JOUT,1020) (EIGVAL(I),I=LOW,NUP)          
         DO 570 I=1,NVAR                                                        
         WRITE(JOUT,1030) I,(U(I,J),J=LOW,NUP)     
570      CONTINUE                                                               
         NUP=NUP+8                                                              
         LOW=LOW+8                                                              
         IF(LOW.LE.NVAR) GOTO 560                                               
1000     FORMAT(/,3X,8I9)                                                       
1010     FORMAT(1X,I3,8F9.1)                                                    
1020     FORMAT(/,4X,8F9.1,/)                                                   
1030     FORMAT(1X,I3,8F9.4)                                                    
      RETURN
      END
