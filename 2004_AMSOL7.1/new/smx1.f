      SUBROUTINE SMX1 (COORD,NITER,DOENP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GCL0493
      INCLUDE 'SIZES.i'                                                 GCL0493
      INCLUDE 'SIZES2.i'
      INCLUDE 'KEYS.i'                                                  DJG0995
C ---------------------------------------------------------------------
C     FGB MATRIX ELEMENTS OR G-CDS CONTRIBUTION.
C     * ANALYTICAL COMPUTATION OF ACCESSIBLE SURFACE AREA,
C     * INTEGRATION FORMULA FOR EFFECTIVE BORN RADII,
C       (D.L., JAN 93)
C     * PAIRWISE DESCREENING
C       (CCC about 95)
C     * ANALYTICAL GRADIENT (SM5.2, SM5.4, SM5.4PD  )                   DL0397
C       (D.L., MARCH 97)                                                DL0397
C  ON INPUT
C     DOENP = TRUE  --> DO ENP CALCULATION                              DJG0995
C     DOENP = FALSE --> DO CDS CALCULATION                              DJG0995
C ---------------------------------------------------------------------
      LOGICAL DOENP,TAREA                                               DL0397
      DIMENSION COORD(3,*),BORN(NUMATM)                                 DJG0995
      COMMON /ONESCM/ ICONTR(100)                                       DJG0295
      COMMON /AREACM/ NOPTI,TAREA,NINTEG
      COMMON /CYCLCM/ PCMIN, NGEOM, NSOLPR, NSCFS, JPCNT                GDH0893
      SAVE
      IF (ICONTR(25).EQ.1) THEN                                         DJG0295
         ICONTR(25)=2                                                   DJG0295
C        LOAD IN THE SOLVATION PARAMETERS                               DJG0995
C                                                                       DJG0995
C        SIGMA0 = SURFACE TENSIONS (SIGMA ZERO)                         DJG0995
C        RKCDS  = SURFACE TENSION RADII (Rk)                            DJG0995
C        RHONOT = COULOMB RADII PART I "rho zero"                       DJG0995
C        RHOONE = COULOMB RADII PART II "rho one"                       DJG0995
C        QKNOT  = PART OF COULOMB RADII CALC "q not"                    DJG0995
C        QKONE  = PART OF COULOMB RADII CALC "q one"                    DJG0995
C        HBCORC = SURFACE TENSION CORRECTION FOR H-BONDS                DJG0995
C                 (SM2(.X) AND SM3(.X))                                 DJG0995
C                                                                       DJG0995
         CALL LOADSP                                                    DJG0995
C        FILL SASA RADII (RAS & RALG)                                   DJG0995
         CALL SASARD                                                    DJG0995
      ENDIF                                                             GDH1293
C     ----------
      IF (DOENP) THEN                                                   DL0397
C        DETERMINE COULOMB RADII (RAC) & DERIVATIVES                    DL0397
         CALL COULRD                                                    DJG0995
C        DETERMINE EFFECTIVE BORN RADII (RADIAL QUADRATURE OR PD)       DJG0995
         IF (NINTEG.EQ.3) THEN                                          DAL0303
C           GAUSS-LEGENDRE QUADRATURE                                   DAL0303
            CALL EBRGLQ (COORD,BORN,NITER)                              DAL0303
         ELSE IF (NINTEG.EQ.2) THEN                                     DAL0303
C           PAIRWISE DESCREENING & DERIVATIVES
            CALL EBRPD (BORN)                                           DJG0995
         ELSE                                                           DJG0995
C           TRAPEZOIDAL QUADRATURE & DERIVATIVES
            CALL EBRTRP (COORD,BORN,NITER)                              DJG0995
         ENDIF                                                          DJG0995
C        COMPUTE THE STILL FACTOR AS A FUNCTION OF R AND ALPHA,
C        RETURN FGB = CELCOE/STILL'S FACTORS & DERIVATIVES.             DL0397
         CALL STLFAC (BORN)                                             DL0397
      ELSE                                                              DJG0995
C        DETERMINE SASA FOR CDS & DERIVATIVES                           DL0397
         CALL SASADO (COORD,NITER)                                      DL0397
      ENDIF
      END
