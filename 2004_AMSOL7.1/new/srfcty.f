      SUBROUTINE SRFCTY(COORD,DOENP,CORCK)                              DJG0995
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        INCLUDE 'SIZES.i'
        INCLUDE 'SIZES2.i'
        INCLUDE 'KEYS.i'                                                DJG0995
C
C   THIS SUBROUTINE CONVERTED TO DOUBLE PRECISION 03/17/92 BY GCL
C
      LOGICAL DOENP,CORCK,TAREA,DONE,DONE2,ARATYP                       DJG0995
      COMMON /ONESCM/ ICONTR(100)                                       DJG0295
      DIMENSION BORN(NUMATM)                                            DJG0995
C
C ---------------------------------------------------------------------
C     SMx.1: FGB MATRIX ELEMENTS AND G-CDS CONTRIBUTION.
C     * ANALYTICAL COMPUTATION OF ACCESSIBLE SURFACE AREA: "AREA",
C     * INTEGRATION FORMULA FOR EFFECTIVE BORN RADII,
C     * SAVINGS.
C     (D.L., JAN 93)
C ---------------------------------------------------------------------
C
C     DOENP = TRUE --> DO ENP CALCULATION                               DJG0995
C     DOENP = FALSE --> DO CDS CALCULATION                              DJG0995
C
        SAVE
      IF (ICONTR(35).EQ.1) THEN                                         DJG0295
C                                                                       DJG0995
C        LOAD IN THE SOLVATION PARAMETERS                               DJG0995
C                                                                       DJG0995
C       SIGMA0 = SURFACE TENSIONS (SIGMA ZERO)                          DJG0995
C       RKCDS = SURFACE TENSION RADII (Rk)                              DJG0995
C       RHONOT = COULOMB RADII PART I "rho zero"                        DJG0995
C       RHOONE = COULOMB RADII PART II "rho one"                        DJG0995
C       QKNOT = PART OF COULOMB RADII CALC "q not"                      DJG0995
C       QKONE = PART OF COULOMB RADII CALC "q one"                      DJG0995
C       HBCORC = SURFACE TENSION CORRECTION FOR H-BONDS                 DJG0995
C                 (SM2(.X) AND SM3(.X))                                 DJG0995
C                                                                       DJG0995
C                                                                       DJG0995
         CALL LOADSP                                                    DJG0995
C                                                                       DJG0995
C        DETERMINE SASA RADII                                           DJG0995
C                                                                       DJG0995
         CALL SASARD                                                    DJG0995
         ICONTR(35)=2                                                   DJG0995
      ENDIF                                                             GDH0394
      IF (.NOT.DOENP) THEN                                              DJG0995
C                                                                       DJG0995
C        DETERMINE SOLVENT-ACCESSIBLE SURFACE AREAS                     DJG0995
C                                                                       DJG0995
         CALL SASADO(COORD,NITER)                                       DJG0995
      ELSE
C                                                                       DJG0995
C        DETERMINE COULOMB RADII                                        DJG0995
C                                                                       DJG0995
         CALL COULRD                                                    DJG0995
C                                                                       DJG0995
C        CALCULATE SMOOTHING FUNCTION                                   DJG0995
C                                                                       DJG0995
         CALL RACSMO                                                    DJG0995
C                                                                       DJG0995
C        CALCULATE EFFECTIVE BORN RADII                                 DJG0995
C                                                                       DJG0995
         CALL EBRREC(COORD,BORN,CORCK)                                  DJG0995
C
C        COMPUTE THE STILL FACTOR FGB AS A FUNCTION OF R AND ALPHA
C
         CALL STLFAC(BORN)                                              DJG0995
      ENDIF
      RETURN
      END
