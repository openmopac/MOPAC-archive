      SUBROUTINE OSINV (A,N,D) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      INCLUDE 'FFILES.i'                                                GDH1095
C     INVERT THE SYMMETRIC MATRIX SQUARE MATRIX A 
C   INPUT 
C     A : THE MATRIX TO BE INVERTED , PACKED 
C     N : THE DIMENSION 
C   OUTPUT 
C     A : THE INVERSE OF THE ORIGINAL A MATRIX 
C     N : NOT MODIFIED 
C     D = 0 IF ONE PIVOT IS LOWER THAN DLIMIT ; 1. OTHERWISE 
C 
C     CRAY-1 VERSION USING LINPACK LIBRARY 
C 
      DIMENSION A(N,N),B(60),C(60),LZ(60) 
      SAVE
      DATA DLIMIT /1.D-15/ 
      IF (N.GT.60) THEN 
         WRITE(JOUT,'('' A CALL TO OSINV WITH N='',I3, 
     .                '' OVERFLOWS ACTUAL DIMENSION 60 ... STOP'')')N 
         ISTOP=1                                                        GDH1095 
         IWHERE=166                                                     GDH1095
         RETURN                                                         GDH1095
      ENDIF 
      CALL SSIFA (A,N,N,LZ,INFO) 
      IF (INFO.NE.0) THEN 
         D=0.D0 
      ELSE 
         D=1.D0 
         CALL SSIDI (A,N,N,LZ,B,INERT,C,001) 
         DO 10 I=2,N 
CDIR$ IVDEP 
         DO 10 J=1,I-1 
   10    A(I,J)=A(J,I) 
      ENDIF 
      RETURN 
      END 
