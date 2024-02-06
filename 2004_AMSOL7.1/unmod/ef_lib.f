      SUBROUTINE DGEDI(A,LDA,N,IPVT,DET,WORK,JOB)                       
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(LDA,1),DET(2),WORK(1),IPVT(1)                         
C                                                                       
C     DGEDI COMPUTES THE DETERMINANT AND INVERSE OF A MATRIX            
C     USING THE FACTORS COMPUTED BY DGECO OR DGEFA.                     
C                                                                       
C     ON ENTRY                                                          
C                                                                       
C        A       DOUBLE PRECISION(LDA, N)                               
C                THE OUTPUT FROM DGECO OR DGEFA.                        
C                                                                       
C        LDA     INTEGER                                                
C                THE LEADING DIMENSION OF THE ARRAY  A .                
C                                                                       
C        N       INTEGER                                                
C                THE ORDER OF THE MATRIX  A .                           
C                                                                       
C        IPVT    INTEGER(N)                                             
C                THE PIVOT VECTOR FROM DGECO OR DGEFA.                  
C                                                                       
C        WORK    DOUBLE PRECISION(N)                                    
C                WORK VECTOR.  CONTENTS DESTROYED.                      
C                                                                       
C        JOB     INTEGER                                                
C                = 11   BOTH DETERMINANT AND INVERSE.                   
C                = 01   INVERSE ONLY.                                   
C                = 10   DETERMINANT ONLY.                               
C                                                                       
C     ON RETURN                                                         
C                                                                       
C        A       INVERSE OF ORIGINAL MATRIX IF REQUESTED.               
C                OTHERWISE UNCHANGED.                                   
C                                                                       
C        DET     DOUBLE PRECISION(2)                                    
C                DETERMINANT OF ORIGINAL MATRIX IF REQUESTED.           
C                OTHERWISE NOT REFERENCED.                              
C                DETERMINANT = DET(1) * 10.0**DET(2)                    
C                WITH  1.0 .LE. ABS(DET(1)) .LT. 10.0                   
C                OR  DET(1) .EQ. 0.0 .                                  
C                                                                       
C     ERROR CONDITION                                                   
C                                                                       
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS     
C        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.           
C        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY      
C        AND IF DGECO HAS SET RCOND .GT. 0.0 OR DGEFA HAS SET           
C        INFO .EQ. 0 .                                                  
C                                                                       
C     LINPACK. THIS VERSION DATED 08/14/78 .                            
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     BLAS DAXPY,DSCAL,DSWAP                                            
C     FORTRAN ABS,MOD                                                   
C                                                                       
C     COMPUTE DETERMINANT                                               
C                                                                       
      IF (JOB/10 .EQ. 0) GO TO 70                                       
         DET(1) = 1.0D+00                                               
         DET(2) = 0.0D+00                                               
         TEN = 10.0D+00                                                 
         DO 50 I = 1, N                                                 
            IF (IPVT(I) .NE. I) DET(1) = -DET(1)                        
            DET(1) = A(I,I)*DET(1)                                      
C        ...EXIT                                                        
            IF (DET(1) .EQ. 0.0D+00) GO TO 60                           
   10       IF (ABS(DET(1)) .GE. 1.0D+00) GO TO 20                      
               DET(1) = TEN*DET(1)                                      
               DET(2) = DET(2) - 1.0D+00                                
            GO TO 10                                                    
   20       CONTINUE                                                    
   30       IF (ABS(DET(1)) .LT. TEN) GO TO 40                          
               DET(1) = DET(1)/TEN                                      
               DET(2) = DET(2) + 1.0D+00                                
            GO TO 30                                                    
   40       CONTINUE                                                    
   50    CONTINUE                                                       
   60    CONTINUE                                                       
   70 CONTINUE                                                          
C                                                                       
C     COMPUTE INVERSE(U)                                                
C                                                                       
      IF (MOD(JOB,10) .EQ. 0) GO TO 150                                 
         DO 100 K = 1, N                                                
            A(K,K) = 1.0D+00/A(K,K)                                     
            T = -A(K,K)                                                 
            CALL DSCAL(K-1,T,A(1,K),1)                                  
            KP1 = K + 1                                                 
            IF (N .LT. KP1) GO TO 90                                    
            DO 80 J = KP1, N                                            
               T = A(K,J)                                               
               A(K,J) = 0.0D+00                                         
               CALL SAXPY(K,T,A(1,K),1,A(1,J),1)                        
   80       CONTINUE                                                    
   90       CONTINUE                                                    
  100    CONTINUE                                                       
C                                                                       
C        FORM INVERSE(U)*INVERSE(L)                                     
C                                                                       
         NM1 = N - 1                                                    
         IF (NM1 .LT. 1) GO TO 140                                      
         DO 130 KB = 1, NM1                                             
            K = N - KB                                                  
            KP1 = K + 1                                                 
            DO 110 I = KP1, N                                           
               WORK(I) = A(I,K)                                         
               A(I,K) = 0.0D+00                                         
  110       CONTINUE                                                    
            DO 120 J = KP1, N                                           
               T = WORK(J)                                              
               CALL SAXPY(N,T,A(1,J),1,A(1,K),1)                        
  120       CONTINUE                                                    
            L = IPVT(K)                                                 
            IF (L .NE. K) CALL DSWAP(N,A(1,K),1,A(1,L),1)               
  130    CONTINUE                                                       
  140    CONTINUE                                                       
  150 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DGEFA(A,LDA,N,IPVT,INFO)                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(LDA,1),IPVT(1)                                        
C                                                                       
C     DGEFA FACTORS A DOUBLE PRECISION MATRIX BY GAUSSIAN ELIMINATION.  
C                                                                       
C     DGEFA IS USUALLY CALLED BY DGECO, BUT IT CAN BE CALLED            
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.          
C     (TIME FOR DGECO) = (1 + 9/N)*(TIME FOR DGEFA) .                   
C                                                                       
C     ON ENTRY                                                          
C                                                                       
C        A       DOUBLE PRECISION(LDA, N)                               
C                THE MATRIX TO BE FACTORED.                             
C                                                                       
C        LDA     INTEGER                                                
C                THE LEADING DIMENSION OF THE ARRAY  A .                
C                                                                       
C        N       INTEGER                                                
C                THE ORDER OF THE MATRIX  A .                           
C                                                                       
C     ON RETURN                                                         
C                                                                       
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS         
C                WHICH WERE USED TO OBTAIN IT.                          
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE       
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER          
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.       
C                                                                       
C        IPVT    INTEGER(N)                                             
C                AN INTEGER VECTOR OF PIVOT INDICES.                    
C                                                                       
C        INFO    INTEGER                                                
C                = 0  NORMAL VALUE.                                     
C                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR       
C                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES        
C                     INDICATE THAT DGESL OR DGEDI WILL DIVIDE BY ZERO  
C                     IF CALLED.  USE  RCOND  IN DGECO FOR A RELIABLE   
C                     INDICATION OF SINGULARITY.                        
C                                                                       
C     LINPACK. THIS VERSION DATED 08/14/78 .                            
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     BLAS DAXPY,DSCAL,IDAMAX                                           
C                                                                       
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING                        
C                                                                       
      INFO = 0                                                          
      NM1 = N - 1                                                       
      IF (NM1 .LT. 1) GO TO 70                                          
      DO 60 K = 1, NM1                                                  
         KP1 = K + 1                                                    
C                                                                       
C        FIND L = PIVOT INDEX                                           
C                                                                       
         L = ISAMAX(N-K+1,A(K,K),1) + K - 1                             
         IPVT(K) = L                                                    
C                                                                       
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED          
C                                                                       
         IF (A(L,K) .EQ. 0.0D+00) GO TO 40                              
C                                                                       
C           INTERCHANGE IF NECESSARY                                    
C                                                                       
            IF (L .EQ. K) GO TO 10                                      
               T = A(L,K)                                               
               A(L,K) = A(K,K)                                          
               A(K,K) = T                                               
   10       CONTINUE                                                    
C                                                                       
C           COMPUTE MULTIPLIERS                                         
C                                                                       
            T = -1.0D+00/A(K,K)                                         
            CALL DSCAL(N-K,T,A(K+1,K),1)                                
C                                                                       
C           ROW ELIMINATION WITH COLUMN INDEXING                        
C                                                                       
            DO 30 J = KP1, N                                            
               T = A(L,J)                                               
               IF (L .EQ. K) GO TO 20                                   
                  A(L,J) = A(K,J)                                       
                  A(K,J) = T                                            
   20          CONTINUE                                                 
               CALL SAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)                  
   30       CONTINUE                                                    
         GO TO 50                                                       
   40    CONTINUE                                                       
            INFO = K                                                    
   50    CONTINUE                                                       
   60 CONTINUE                                                          
   70 CONTINUE                                                          
      IPVT(N) = N                                                       
      IF (A(N,N) .EQ. 0.0D+00) INFO = N                                 
      RETURN                                                            
      END                                                               
      SUBROUTINE PRJFC(F,XPARAM,NVAR) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'FFILES.i'                                                GDH1095
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
      DIMENSION DETERM(2)                                               GDH0597
      DIMENSION COORD(3,NUMATM),DX(MAXPAR),XPARAM(MAXPAR)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, EPS=1.0D-14,                
     *           CUT5=1.0D-05, CUT8=1.0D-08)                            
      SAVE
C                                                                       
C TOTALLY ASYMMETRIC CARTESIAN TENSOR.                                  
      DATA TENS/ 0.0D+00,  0.0D+00,  0.0D+00,                           
     X           0.0D+00,  0.0D+00, -1.0D+00,                           
     X           0.0D+00,  1.0D+00,  0.0D+00,                           
     Y           0.0D+00,  0.0D+00,  1.0D+00,                           
     Y           0.0D+00,  0.0D+00,  0.0D+00,                           
     Y          -1.0D+00,  0.0D+00,  0.0D+00,                           
     Z           0.0D+00, -1.0D+00,  0.0D+00,                           
     Z           1.0D+00,  0.0D+00,  0.0D+00,                           
     Z           0.0D+00,  0.0D+00,  0.0D+00  /                         
C                                                                       
      NATM=NVAR/3                                                        
      NC1=NVAR
      IJ=1
      DO 2 I=1,NATM                                                    
       COORD(1,I)=XPARAM(IJ)
       COORD(2,I)=XPARAM(IJ+1)
       COORD(3,I)=XPARAM(IJ+2)
       IJ=IJ+3
2     CONTINUE                                                          
C                                                                       
C     CALCULATE 1/SQRT(MASS)                                            
C                                                                       
      L=0                                                               
      DO 3 I=1,NATM                                                     
         TMP=ONE/SQRT(ATMASS(I))                                        
         DO 3 J=1,3                                                     
            L=L+1                                                       
3           RM(L)=TMP                                                   
C                                                                       
C     PREPARE GRADIENT                                                  
C                                                                       
         DO 4 I=1,NC1                                                   
4        DX(I)=ZERO                                                     
C                                                                       
C     FIND CMS AND CALCULATED MASS WEIGHTED COORDINATES                 
C                                                                       
      TOTM=ZERO                                                         
      CMASS(1)=ZERO                                                     
      CMASS(2)=ZERO                                                     
      CMASS(3)=ZERO                                                     
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
C                                                                       
C 2. COMPUTE INERTIA TENSOR.                                            
C                                                                       
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
C     CHECK THE INERTIA TENSOR.                                               
C                                                                       
      CHK=ROT(1,1)*ROT(2,2)*ROT(3,3)                                    
      IF(ABS(CHK).GT.CUT8) GO TO 21                                     
      IF(ABS(ROT(1,1)).GT.CUT8) GO TO 11                                
      IF(ABS(ROT(2,2)).GT.CUT8) GO TO 12                                
      IF(ABS(ROT(3,3)).GT.CUT8) GO TO 13                                
      WRITE(JOUT,14) ROT(1,1),ROT(2,2),ROT(3,3)                            
   14 FORMAT(1X,'EVERY DIAGONAL ELEMENTS ARE ZERO ?',3F20.10)           
      RETURN                                                            
C                                                                       
C* 1. X,Y=0 BUT Z.NE.0                                                  
C                                                                       
   13 ROT(3,3)=ONE/ROT(3,3)                                             
      GO TO 22                                                          
C                                                                       
C Y.NE.0                                                                
C                                                                       
   12 IF(ABS(ROT(3,3)).GT.CUT8) GO TO 15                                
C                                                                       
C* 2. X,Z=0 BUT Y.NE.0                                                  
C                                                                       
      ROT(2,2)=ONE/ROT(2,2)                                             
      GO TO 22                                                          
C                                                                       
C X.NE.0                                                                
C                                                                       
   11 IF(ABS(ROT(2,2)).GT.CUT8) GO TO 16                                
      IF(ABS(ROT(3,3)).GT.CUT8) GO TO 17                                
C                                                                       
C* 3. Y,Z=0 BUT X.NE.0                                                  
C                                                                       
      ROT(1,1)=ONE/ROT(1,1)                                             
      GO TO 22                                                          
C                                                                       
C* 4. X,Y.NE.0 BUT Z=0                                                  
C                                                                       
   16 DET=ROT(1,1)*ROT(2,2)-ROT(1,2)*ROT(2,1)                           
      TRP=ROT(1,1)                                                      
      ROT(1,1)=ROT(2,2)/DET                                             
      ROT(2,2)=TRP/DET                                                  
      ROT(1,2)=-ROT(1,2)/DET                                            
      ROT(2,1)=-ROT(2,1)/DET                                            
      GO TO 22                                                          
C                                                                       
C* 5. X,Z.NE.0 BUT Y=0                                                  
C                                                                       
   17 DET=ROT(1,1)*ROT(3,3)-ROT(1,3)*ROT(3,1)                           
      TRP=ROT(1,1)                                                      
      ROT(1,1)=ROT(3,3)/DET                                             
      ROT(3,3)=TRP/DET                                                  
      ROT(1,3)=-ROT(1,3)/DET                                            
      ROT(3,1)=-ROT(3,1)/DET                                            
      GO TO 22                                                          
C                                                                       
C* 6. Y,Z.NE.0 BUT X=0                                                  
C                                                                       
   15 DET=ROT(3,3)*ROT(2,2)-ROT(3,2)*ROT(2,3)                           
      TRP=ROT(3,3)                                                      
      ROT(3,3)=ROT(2,2)/DET                                             
      ROT(2,2)=TRP/DET                                                  
      ROT(3,2)=-ROT(3,2)/DET                                            
      ROT(2,3)=-ROT(2,3)/DET                                            
      GO TO 22                                                          
   21 CONTINUE                                                          
C                                                                       
C 4. COMPUTE INVERSION MATRIX OF ROT.                                   
C                                                                       
      INFO=0                                                            
      CALL DGEFA(ROT,3,3,ISCR,INFO)                                     
      IF(INFO.NE.0) THEN                                                GDH1095
         ISTOP=1                                                        GDH1095
         IWHERE=165                                                     GDH1095
         RETURN                                                         GDH1095
      ENDIF                                                             GDH1095
      DETERM(1)=ZERO                                                    DJG0597
      DETERM(2)=ZERO                                                    DJG0597
      CALL DGEDI(ROT,3,3,ISCR,DETERM,SCR,1)                             DJG0597
C                                                                       
   22 CONTINUE                                                          
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
C                                                                       
      DO 110 I=1,NC1                                                    
       DO 110 J=1,I                                                     
        P(I,J)=-P(I,J)                                                  
        IF(I.EQ.J) P(I,J) = ONE +P(I,J)                                 
  110   CONTINUE                                                        
C                                                                       
C 8. NEGLECT SMALLER VALUES THAN 10**-8.                                
C                                                                       
      DO 120 I=1,NC1                                                    
       DO 120 J=1,I                                                     
        IF(ABS(P(I,J)).LT.CUT8) P(I,J)=ZERO                             
        P(J,I)=P(I,J)                                                   
  120   CONTINUE                                                        
C                                                                       
C 10. POST AND PREMULTIPLY F BY P.                                      
C     USE COF FOR SCRATCH.                                              
C                                                                       
      DO 150 I=1,NC1                                                    
       DO 150 J=1,NC1                                                   
        SUM=ZERO                                                        
        DO 140 K=1,NC1                                                  
  140    SUM=SUM+F(I,K)*P(K,J)                                          
  150   COF(I,J)=SUM                                                    
C                                                                       
C 11. COMPUTE P*F*P.                                                    
C                                                                       
      DO 200 I=1,NC1                                                    
       DO 200 J=1,NC1                                                   
        SUM=ZERO                                                        
        DO 190 K=1,NC1                                                  
  190    SUM=SUM+P(I,K)*COF(K,J)                                        
  200   F(I,J)=SUM                                                      
C                                                                       
      RETURN                                                            
      END                                                               
