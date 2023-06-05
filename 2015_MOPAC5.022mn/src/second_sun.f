         DOUBLE PRECISION FUNCTION SECOND ()
C
C   This is a machine-specific subprogram that obtains elapsed CPU time
C   in seconds.
C
C   Machines: Sun Workstation
C
         IMPLICIT DOUBLE PRECISION (A-H, O-Z)
         EXTERNAL ETIME
         REAL NTIME(2)
C
         TIME = ETIME(NTIME)
         SECOND = NTIME(1) + NTIME(2)
C
         RETURN
         END
