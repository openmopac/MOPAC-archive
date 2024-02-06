C
         SUBROUTINE PORCPU (T1)
C
C   This is a machine-specific subprogram that obtains elapsed CPU time
C   in seconds.
C
C   Machines: Silicon Graphics Workstation
C
         IMPLICIT DOUBLE PRECISION (A-H, O-Z)
         EXTERNAL ETIME
         REAL TIME(2)
C
         T0 = ETIME(TIME)
         T1 = TIME(1) + TIME(2)
C
         RETURN
         END
C*****
