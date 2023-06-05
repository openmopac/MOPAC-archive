      SUBROUTINE XERBLA ( SRNAME, INFO,*)                               CSTP
*     ..    Scalar Arguments ..
      INTEGER            INFO
      CHARACTER*6        SRNAME
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
*     ..
*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the Level 2 BLAS routines.
*
*  It is called by the Level 2 BLAS routines if an input parameter is
*  invalid.
*
*  Installers should consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Parameters
*  ==========
*
*  SRNAME - CHARACTER*6.
*           On entry, SRNAME specifies the name of the routine which
*           called XERBLA.
*
*  INFO   - INTEGER.
*           On entry, INFO specifies the position of the invalid
*           parameter in the parameter-list of the calling routine.
*
*
*  Auxiliary routine for Level 2 Blas.
*
*  Written on 20-July-1986.
*
*     .. Executable Statements ..
*
      IF (DOPRNT) WRITE (*,99999) SRNAME, INFO                          CIO
*
      RETURN 1                                                          CSTP (stop)
*
99999 FORMAT ( ' ** On entry to ', A6, ' parameter number ', I2,
     $         ' had an illegal value' )
*
*     End of XERBLA.
*
 9999 RETURN 1                                                          CSTP
      END
