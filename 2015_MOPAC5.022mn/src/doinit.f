      SUBROUTINE DOINIT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

C     Calls the _INIT entry points of all functions/subroutines that still must have
C     SAVE statements used for their local variables.  Before every new MOPAC-MN
C     run within the greater optimization program, this subroutine must be called
C     to reinitialize the functions and subroutines that use the SAVE statement.
C     
C     Additionally, this subroutine will reset necessary named common block variables
C     that were overwritten since they were first defined in the BLOCK DATA section
C     of the program.  This is necessary because the BLOCK DATA section causes
C     assignment of the named common block variables ONLY ONCE at the start of program
C     execution and so manually resetting those variables which may have changed
C     is therefore necessary before the next time the MOPAC-MN subprogram is called.
C     
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C FIRST RESET LOCAL VARIABLES IN SUBUNITS WITH "SAVE" STATEMENT STILL ACTIVE.
        CALL cnvg_INIT
        CALL compfg_INIT
        CALL dcart_INIT
        CALL delri_INIT
        CALL depvar_INIT
        CALL deriv_INIT
        CALL dfpsav_INIT
        CALL dhc_INIT
        CALL dipind_INIT
        CALL dipole_INIT
        CALL enpart_INIT
        CALL flepo_INIT
        CALL fock2d_INIT
        CALL fock2_INIT
        CALL hcore_INIT
        CALL hqrii_INIT
        CALL interp_INIT
        CALL iter_INIT
        CALL linmin_INIT
        CALL local_INIT
        CALL locmin_INIT
        CALL meci_INIT
        CALL moldat_INIT
        CALL polar_INIT
        CALL powsq_INIT
        CALL prtdrc_INIT
        CALL pulay_INIT
        CALL react1_INIT
C        CALL rotate_INIT                                                LF0710
        CALL search_INIT
        CALL solrot_INIT
        CALL spcg_INIT
        CALL ss_INIT
        CALL update_INIT
        CALL writmo_INIT
        CALL wrtkey_INIT
        CALL xyzgeo_INIT
        CALL xyzint_INIT

C NEXT RESET COMMON BLOCK VARIABLES THAT MAY HAVE CHANGED DURING LAST RUN.

      CALL RESETD
      CALL RESETC

      RETURN
      END
