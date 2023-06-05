C***********************************************************************
C  DATE
C***********************************************************************
C
      SUBROUTINE DATE (IDATE,*)                                         CSTP
C
C     VERSION FOR SUN WORKSTASIONS
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      CHARACTER*24 IDATE
C                   
      CALL FDATE(IDATE)
C
      RETURN
 9999 RETURN 1                                                          CSTP
      END
