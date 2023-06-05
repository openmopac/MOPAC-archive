C-------------------------------------------------------------------------------
C		DATE (IBM RS/6000 version)                    
C 	return date and time in an ASCII string
C-------------------------------------------------------------------------------
      SUBROUTINE DATE(today,*)                                          CSTP
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
C
      CHARACTER*24 TODAY 
C              
      CALL FDATE(TODAY)
C
      RETURN
 9999 RETURN 1                                                          CSTP
      END
