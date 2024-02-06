C***********************************************************************
C  DATESV
C***********************************************************************
C
      SUBROUTINE DATESV (IO)
C
C   This is a machine-specific subprogram that writes the date and 
C   time to a common block /datecm/.   
C   This subprogram calls the C program SHDATE, which was 
C   written by J.G. at the University of Minnesota.  
C
C   Machine: IBM RS/6000
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      common /DATECM/ CDAT
C
      CHARACTER*80 CDAT
      CHARACTER*40 DATE
      DATA DATE /'                                        '/
C              
      STATUS = SHDATE(DATE)
C
      IF (STATUS .NE. -1) CDAT='Wall clock time and date at job'
     1                              //' start '//DATE
      IF (STATUS .EQ. -1) WRITE (IO, 1100)     
C
 1100 FORMAT(/,3X,'ERROR WITH THE ACQUIRING THE DATE AND TIME',/)
C
      RETURN
      END
C*****
