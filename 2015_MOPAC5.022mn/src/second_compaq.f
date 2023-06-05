         DOUBLE PRECISION FUNCTION SECOND ()
C
C   This is a machine-specific subprogram that obtains elapsed CPU time
C   in seconds.
C
C   Machine: Compaq
C
         IMPLICIT DOUBLE PRECISION (A-H, O-Z)
         EXTERNAL ETIME
         REAL NTIME(2), ETIME
C
         TIME = ETIME(NTIME)
         SECOND   = DBLE(NTIME(1)) + DBLE(NTIME(2))
         RETURN
         END
