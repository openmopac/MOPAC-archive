  Building MOPAC on various platforms

All command scripts for building MOPAC on the various platforms are in this folder.
To avoid confusion, do NOT use command scripts in other folders or on other machines.

An attempt was made to merge the GPU and non-GPU command scripts,
after six hours work this was abandoned as being too difficult.
Do NOT try to merge the scripts - it's an exercise in futility.

Command scripts that call these command scripts from other platforms are stored on the other platforms.

Script Makefile_for_MOPAC_Windows should not be used - use the Intel Visual Studio instead, particularly when debugging might be necessary.

WINDOWS only, specific compilations:

*  ligand.F90 - For RELEASE mode, turn off optimization.  If ligand.F90 is optimized, a random error can occur.  For example, a file called test.mop might run but an identical file called THIS_IS_A_TEST.mop might not run.  The fault was traced to the block starting at line "attached = .false." and ending at the line "het = "HET""

*  readmo.F90 - for DEBUG and RELEASE mode, add pre-processor flag BITS32 or BITS64.

MAC and Linux only, specific compilations:

* add_hydrogen_atoms.F90 - For RELEASE mode, turn off optimization. 