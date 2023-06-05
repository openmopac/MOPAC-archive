C-------------------------------------------------------------------------------
C       DATE (HP version)
C       return date and time in an ASCII string
C-------------------------------------------------------------------------------
      SUBROUTINE DATE(today,*)                                          CSTP
c
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      integer*4 status
      character*24 today
      character*40 aux
c
      STATUS = SHDATE(aux)
      IF ((STATUS .EQ. -1).AND.DOPRNT) WRITE (IO, 1100)                 CIO
 1100 FORMAT(/,3X,'ERROR WITH THE ACQUIRING THE DATE AND TIME',/)
C
      today=aux(1:10) // aux(24:28) // aux(11:19)
c
      RETURN
 9999 RETURN 1                                                          CSTP
      END
