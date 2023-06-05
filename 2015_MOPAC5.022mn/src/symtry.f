      SUBROUTINE SYMTRY(*)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOSYM/ NDEP, LOCPAR(MAXPAR), IDEPFN(MAXPAR),
     1         LOCDEP(MAXPAR)
CSAV         SAVE                                                           GL0892
C**********************************************************************
C
C  SYMTRY COMPUTES THE BOND LENGTHS AND ANGLES THAT ARE FUNCTIONS OF
C         OTHER BOND LENGTHS AND ANGLES.
C
C ON INPUT GEO     = KNOWN INTERNAL COORDINATES
C          NDEP    = NUMBER OF DEPENDENCY FUNCTIONS.
C          IDEPFN  = ARRAY OF DEPENDENCY FUNCTIONS.
C          LOCDEP  = ARRAY OF LABELS OF DEPENDENT ATOMS.
C          LOCPAR  = ARRAY OF LABELS OF REFERENCE ATOMS.
C
C  ON OUTPUT THE ARRAY "GEO" IS FILLED
C***********************************************************************
C
C     NOW COMPUTE THE DEPENDENT PARAMETERS.
C
      DO 10 I=1,NDEP
         CALL HADDON (VALUE,LOCN,IDEPFN(I),LOCPAR(I),GEO,*9999)          CSTP(call)
         J=LOCDEP(I)
   10 GEO(LOCN,J)=VALUE
      RETURN
 9999 RETURN 1                                                          CSTP
      END
