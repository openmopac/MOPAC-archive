      SUBROUTINE PRTHES(EIGVAL,NVAR,*)                                  CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                       
      INCLUDE 'SIZES.i'
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),                  
     1                PMAT(MAXPAR,MAXPAR)                                IR0494
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),                        
     $U(MAXPAR,MAXPAR),DD,rmin,rmax,omin,xlamd,xlamd0,skal,
     $MODE,NSTEP,NEGREQ,IPRNT
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION EIGVAL(MAXPAR)
         IF (IPRNT.GE.4) THEN                                                   
         IF (DOPRNT) WRITE(6,*)' '                                      CIO
         IF (DOPRNT) WRITE(6,*)'              HESSIAN MATRIX'           CIO
         LOW=1                                                                  
         NUP=8                                                                  
540      NUP=MIN(NUP,NVAR)                                                      
         IF (DOPRNT) WRITE(6,1000) (I,I=LOW,NUP)                        CIO
         DO 550 I=1,NVAR                                                        
         IF (DOPRNT) WRITE(6,1010) I,(HESS(I,J),J=LOW,NUP)              CIO
550      CONTINUE                                                               
         NUP=NUP+8                                                              
         LOW=LOW+8                                                              
         IF(LOW.LE.NVAR) GOTO 540                                               
         ENDIF                                                                  
         IF (DOPRNT) WRITE(6,*)' '                                      CIO
         IF (DOPRNT) WRITE(6,*)                                         CIO
     &              '              HESSIAN EIGENVALUES AND -VECTORS'    CIO
         LOW=1                                                                  
         NUP=8                                                                  
560      NUP=MIN(NUP,NVAR)                                                      
         IF (DOPRNT) WRITE(6,1000) (I,I=LOW,NUP)                        CIO
         IF (DOPRNT) WRITE(6,1020) (EIGVAL(I),I=LOW,NUP)                CIO
         DO 570 I=1,NVAR                                                        
         IF (DOPRNT) WRITE(6,1030) I,(U(I,J),J=LOW,NUP)                 CIO
570      CONTINUE                                                               
         NUP=NUP+8                                                              
         LOW=LOW+8                                                              
         IF(LOW.LE.NVAR) GOTO 560                                               
1000     FORMAT(/,3X,8I9)                                                       
1010     FORMAT(1X,I3,8F9.1)                                                    
1020     FORMAT(/,4X,8F9.1,/)                                                   
1030     FORMAT(1X,I3,8F9.4)                                                    
      RETURN
 9999 RETURN 1                                                          CSTP
      END
