C    INCLUDE FILE FOR COMMON BLOCK /PMODSB/:

      COMMON /PMODSB/ PMODVL(200), PMODS(200)
      DOUBLE PRECISION PMODVL
      LOGICAL PMODS

C     These are parameters for various modifications to the usual
C     MNDO formalism.
C     The PMODS booleans are whether or not a certain modification
C     is to be used by the program.
C     The PMODVL values are the actual constants that are used
C     by the modifications and have specific meanings.
C
