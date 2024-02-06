      SUBROUTINE HQRII(A,N,M,E,V)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'FFILES.i'                                                GDH1095
      INCLUDE 'SIZES.i'
*************************************************************
* HQRII IS A DIAGONALISATION ROUTINE OPTIMALLY WRITTEN FOR CRAY-1.
*
* ON INPUT    A       = MATRIX TO BE DIAGONALISED (PACKED CANONICAL)
*             N       = SIZE OF MATRIX TO BE DIAGONALISED.
*             M       = NUMBER OF EIGENVECTORS NEEDED.
*             E       = ARRAY OF SIZE AT LEAST N
*             V       = ARRAY OF SIZE AT LEAST NMAX*M
*
* ON OUTPUT   E       = EIGENVALUES
*             V       = EIGENVECTORS IN ARRAY OF SIZE NMAX*M
************************************************************************
      COMMON /SCRCHR/ B(MAXSIZ**2),FV1(MAXSIZ),FV2(MAXSIZ)              DL0397
     1               ,DUM(6*MAXPAR**2+1-MAXSIZ*(MAXSIZ+2))              DL0397
      COMMON /AREACM/ NOPTI,TAREA,NINTEG                                GDH0793
C
      DIMENSION A(*), E(*), V(N,M)
      LOGICAL TAREA
C     USE EIGPACK LIBRARY: EITHER 'RS' OR 'RSP' ROUTINE.
C     RS BEING SIGNIFICANTLY FASTER THAN RSP, FIRST EXPAND A (CANONICAL)
C     IN B (BIDIM.) AND THEN CALL RS.
      SAVE
C GDH0497      IF (NOPTI.NE.1) THEN                                     GDH0793
C GDH0497         CALL HQRII2(A,N,M,E,V)                                GDH0793
C GDH0497      ELSE                                                     GDH0793
         K=0
         DO 10 I=1,N
         IJ=I
CDIR$ IVDEP
         DO 10 JI=N*(I-1)+1,N*(I-1)+I
         K=K+1
         B(IJ)=A(K)
         B(JI)=A(K)
   10    IJ=IJ+N
         CALL RS (N,N,B,E,1,V,FV1,FV2,IERR)
         IF (IERR.NE.0) THEN
            WRITE (JOUT,100) IERR
  100       FORMAT(' WARNING : ERROR CODE',I4,'  RETURNED FROM ',
     .             'RS CALLED BY HQRII')
         ENDIF
C GDH0497      ENDIF                                                    GDH0793
      END
