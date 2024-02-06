C***********************************************************************
C  DATESV
C***********************************************************************
C
      SUBROUTINE DATESV (IO)
C
C   This is a machine-specific subprogram that writes the date and 
C   time to a file linked to FORTRAN unit IO.
C
C   Machines: Cray-2, Cray X-MP, Cray Y-MP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /DATECM/ CDAT
      CHARACTER*80 CDAT
      CHARACTER*10 CDATE,CTIME
C                   
      CALL DATE(CDATE)
      CALL CLOCK(CTIME)
      CDAT='Wall clock date and time at beginning of job '//CDATE//CTIME
C
C
      RETURN
      END
C*****
