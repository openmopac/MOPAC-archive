C       This file contains the FORTRAN FILE numbers for each of the possible
C  files created by the AMSOL program.
C       JDAT represents the tr#.dat file
C       JOUT represents the tr#.out file
C       JRES represents the tr#.res file
C       JDEN represents the tr#.den file
C       JARC represents the tr#.arc file
C       JGPT represents the tr#.gpt file
C       JDMT represents the tr#.dmt file
C       JINP represents the tr#.inp file
C       JXSM represents a file containing EXTSM data
C       JXKW represents a file containing XKW keywords
C       JDVP is a file used in development
C       JSCR is a scratch file used internally
C       The following common block holds the FORTRAN file numbers       
C  (unit numbers) for the possible output files during an AMSOL run
C  The FORTRAN numbers are reassigned to the standard file names at the
C  completion of a run by the run script.
C
      COMMON /FUCOM / JDAT,JOUT,JRES,JDEN,JARC,JGPT,JDMT,JINP,          GDH1095
     1                JXSM,JXKW,JDVP,JSCR                               GDH1095
C
C       The following common block is used to determine whether AMSOL
C       terminates successfully by completing the calculation, or abnormally
C       due to problems in the calculation or setup.  If the value of ISTOP 
C       is 0 then the job was successful, if the value of ISTOP is 1 
C       then the job was unsuccessful.  IWHERE is zero if the calculation
C       terminated successfully and is the number of the error location
C       if the calculation terminates abnormally.  The position of the
C       stop in the code can be found by using a command like
C               grep "IWHERE=XX" *.f
C       in the directory with the AMSOL source code, where XX is the
C       number placed at the bottom of a .out file for an unsuccessful
C       run.
C
      COMMON /QCOM  / ISTOP,IWHERE                                      GDH1095
