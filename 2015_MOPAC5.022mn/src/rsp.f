      SUBROUTINE RSP(A,N,MATZ,W,Z,*)                                    CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
C
C
      DIMENSION A(*),  W(N), Z(N,N)
*******************************************************************
*
*   EISPACK DIAGONALIZATION ROUTINES: TO FIND THE EIGENVALUES AND
*           EIGENVECTORS (IF DESIRED) OF A REAL SYMMETRIC PACKED MATRIX.
* ON INPUT-      N  IS THE ORDER OF THE MATRIX  A,
*                A  CONTAINS THE LOWER TRIANGLE OF THE REAL SYMMETRIC
*                   PACKED MATRIX STORED ROW-WISE,
*             MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF ONLY
*                   EIGENVALUES ARE DESIRED,  OTHERWISE IT IS SET TO
*                   ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND
*                   EIGENVECTORS.
* ON OUTPUT-     W  CONTAINS THE EIGENVALUES IN ASCENDING ORDER,
*                Z  CONTAINS THE EIGENVECTORS IF MATZ IS NOT ZERO,
*
*******************************************************************
* THIS SUBROUTINE WAS CHOSEN AS BEING THE MOST RELIABLE. (JJPS)
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
C FV and FV2 are scratch arrays which shouldo be dimensioned to the 
C to the order of the biggest diagonalizable matrix
C This is accomplished by the parameter MAXDMO ( Max Diag. Mat. order)   IR0494
      DIMENSION FV1(MAXDMO),FV2(MAXDMO)
CSAV        SAVE
C   Test that MAXDMO is big enough                                       IR0494
      MX=MAX(MAXORB,NMECI*NMECI)
      IF(MAXDMO.LT.MX) then
        IF (DOPRNT) THEN                                                CIO
          WRITE(6,'(3x,"*** MAXDMO should be at least",I5," ***")') MX
          WRITE(6,'("Please, change MAXDMO in SIZES.i and recompile")')
        ENDIF                                                           CIO
      RETURN 1                                                           CSTP (stop)
      ENDIF
      NV=(MAXDMO*(MAXDMO+1))/2                                           IR0494
      NM=N
C
20    call tred3(n,nv,a,w,fv1,fv2)
      IF (MATZ .NE. 0) GO TO 30
C     ********** FIND EIGENVALUES ONLY **********
      call tqlrat(n,w,fv2,ierr,*9999)                                    CSTP(call)
      GO TO 60
C     ********** FIND BOTH EIGENVALUES AND EIGENVECTORS **********
   30 DO 50    I = 1, N
C
         DO 40    J = 1, N
            Z(J,I)=0.0D0
   40    CONTINUE
C
         Z(I,I)=1.0D0
   50 CONTINUE
C
      call tql2(nm,n,w,fv1,z,ierr,*9999)                                 CSTP(call)
      IF (IERR .NE. 0) GO TO 70
      call trbak3(nm,n,nv,a,n,z,ierr,*9999)                             IR0494 CSTP(call)
   60 return
C     ********** LAST CARD OF RSP **********
   70 IF (DOPRNT) write(6,'( "Error",I3," in RSP" )') ierr              IR0494 / CIO
      RETURN
 9999 RETURN 1                                                          CSTP
      END
