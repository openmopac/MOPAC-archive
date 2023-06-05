C***********************************************************************
C  DATE
C***********************************************************************
C
      SUBROUTINE DATE (IDATE,*)                                         CSTP
C
C   This is a machine-specific subprogram that writes the date and 
C   time to a file linked to FORTRAN unit IO.
C
C   Machine: Silicon Graphics Workstations
C
      CHARACTER IDATE*9
C                  
      CALL FDATE(IDATE)
C
      RETURN
 9999 RETURN 1                                                          CSTP
      END
