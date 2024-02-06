C
         SUBROUTINE PORCPU (T1)
C
C   This is a machine-specific subprogram that obtains elapsed CPU time
C   in seconds.
C
C   Machine: IBM RS/6000
C
         IMPLICIT DOUBLE PRECISION (A-H, O-Z)
         INTEGER NTIME
C
         NTIME = MCLOCK()
C
C    Convert to a real number and adjust to seconds
C
         T1 = DBLE(NTIME)/100.D0
C
         RETURN
         END
C*****
