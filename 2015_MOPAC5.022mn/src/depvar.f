      SUBROUTINE DEPVAR (A,I,W,L,*)                                     CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(3,*)
C***********************************************************************
C
C  IN SUBROUTINE HADDON WHEN M, THE SYMMETRY OPERATION, IS 18 DEPVAR IS
C  CALLED. DEPVAR SHOULD THEN CONTAIN A USER-WRITTEN SYMMETRY OPERATION.
C  SEE HADDON TO GET THE IDEA ON HOW TO WRITE DEPVAR.
C
C ON INPUT:
C           A = ARRAY OF INTERNAL COORDINATES
C           I = ADDRESS OF REFERENCE ATOM
C ON OUTPUT:
C           L = 1 (IF A BOND-LENGTH IS THE DEPENDENT FUNCTION)
C             = 2 (IF AN ANGLE IS THE DEPENDENT FUNCTION)
C             = 3 (IF A DIHEDRAL ANGLE IS THE DEPENDENT FUNCTION)
C           W = VALUE OF THE FUNCTION
C
C  NOTE:  IT IS THE WRITER'S RESPONSIBILITY TO MAKE CERTAIN THAT THE
C         SUBROUTINE DOES NOT CONTAIN ANY ERRORS!
C***********************************************************************
      COMMON /KEYWRD/ KEYWRD
      COMMON /NUMCAL/ NUMCAL
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      CHARACTER*160 KEYWRD
         SAVE ICALCN, FACT                                              GL0892
      DATA ICALCN /0/
      IF (ICALCN .NE. NUMCAL) THEN
         ICALCN = NUMCAL
         FACT=READA(KEYWRD,INDEX(KEYWRD,'DEPVAR'))
         IF (DOPRNT) WRITE(6,'(''  UNIT CELL LENGTH ='',F14.7,          CIO
     1'' TIMES BOND LENGTH'')')FACT                                     CIO
      ENDIF
      W=A(1,I)*FACT
      L=1
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY DEPVAR_INIT                                                 CSAV
              FACT = 0.0D0                                              CSAV
            ICALCN = 0                                                  CSAV
      RETURN                                                            CSAV
      END
