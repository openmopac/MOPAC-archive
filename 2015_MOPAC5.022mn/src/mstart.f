      PROGRAM MSTART
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER I
      CHARACTER*80    FNAME
      COMMON /DOPRNT/ DOPRNT
      LOGICAL DOPRNT
      COMMON /INTEXT/ KEYTXT, COMTXT, TLETXT, SPCTXT(80),
     &                CINTXT, FINTXT,
     &                LINTXT
      CHARACTER*160 KEYTXT
      CHARACTER*80  COMTXT, TLETXT, SPCTXT
      INTEGER       CINTXT, FINTXT
      LOGICAL       LINTXT
      include 'result.f'                                                LF1210

C ***********************************************************************
C
C  Luke Fiedler 07/2010:
C
C  The subroutine MSTART is responsible for setting up the next
C  MOPAC run.  Details as to how to set up this subroutine for
C  embedding MOPAC-MN in an optimization program or calculation
C  requiring repeated fresh MOPAC calculations.
C  
C
C  Common block variable of /DOPRNT/
C    DOPRNT    This boolean common block variable in /DOPRNT/ determines
C              whether or not MOPAC will do standard input/output for
C              all units (.TRUE.) or supress all such statements
C              (.FALSE.) that are not absolutely necessary for the type
C              of calculation being requested.
C
C  Common block variables of /INTEXT/
C              The virtual input file is the common block /INTEXT/.
C    KEYTXT    Input deck of keywords (first line of MOPAC input files).
C    COMTXT    Comment text (second line of MOPAC input files).
C    TLETXT    Title text (third line of MOPAC input files).
C    SPCTXT()  All lines of molecular and run specification in a
C              MOPAC input file from line #4 to the end.
C    CINTXT    Counter indicating which element of SPCTXT is next to
C              be read.
C    FINTXT    Final index of text in SPCTXT() array containing info.
C    LINTXT    Whether or not /INTEXT/ is used as the source of input
C              file data (=.TRUE.) or whether an external file is
C              used for input data (=.FALSE.).
C
C  Subroutine
C    DOINIT    Calling this subroutine reinitializes all program
C              common block variables and subroutine/function local
C              variables necessary for a fresh call to subroutine
C              MOPAC, that is, a new MOPAC run.
C
C  Subroutine 
C    MOPAC     The top-level of the calculation part of the program.
C              It takes two arguments: the first is the unit number
C              used for standard input (typically this is 5); the
C              second number is a pointer to a label in the calling
C              subroutine for alternate returns which occur in place
C              of the program completely terminating upon the
C              occurence of STOP statements in earlier versions of
C              MOPAC.
C
C  Subroutine
C    SETNTX    This subroutine sets all the virtual input file text and
C              values (other than LINTXT) for the common block /INTEXT/
C              which contains all of the keywords, comment lines, and
C              molecular/job specification information.
C
C
C ***********************************************************************

C     Whether or not to allow all input/output or to prevent all output and 
C     all unnecessary input operations.
      DOPRNT=.TRUE.

C     Whether to use text from a source within the program (LINTXT=.true.) or not.
      LINTXT=.false.

C     Set up internal text source for input into MOPAC subroutine (only need to 
C     use if LINTXT is true).
c      CALL SETNTX

C     Decide what type of program run to make here.
      CALL DOINIT
      CALL MOPAC(5,*9999)
      GOTO 9990


  10  CONTINUE
C     For running the test suite and ensuring all calculations are okay after 
C     doing one call to DOINIT.
C     This just resets MOPAC and does one call to the MOPAC subroutine.
C      CALL DOINIT
      CALL MOPAC(5 ,*9999)
      WRITE(6,'(A,F12.3,A)')" FINAL HEAT OF FORMATION = ",FHEAT," KCAL"
      WRITE(6,'(A,F12.3,A)')" TOTAL ENERGY            = ",FTOTAL," EV"
      WRITE(6,'(A,F12.3,A)')" ELECTRONIC ENERGY       = ",FEELEC," EV"
      WRITE(6,'(A,F12.3,A)')" CORE REPULSION ENERGY   = ",FECORE," EV"
      WRITE(6,'(A,F12.3,A)')" IONIZATION POTENTIAL    = ",FIONZN," EV"
      GOTO 9990     
 
C SKIP THE FOLLOWING SECTION (USED ONLY FOR TESTING PURPOSES - MAY BE
C OBSOLETED IN THE FUTURE).
  20  CONTINUE
C     For running the same file TEST.DAT multiple times in a row to ensure that DOINIT
C     cleared out and reinitialized all important common blocks and local
C     subunit variables with the 'save' attribute.
      OPEN(UNIT=20,FILE='TEST.DAT',STATUS='OLD')
      REWIND(UNIT=20)
      CALL DOINIT
      CALL MOPAC(20,*9999)
      CLOSE(UNIT=20)
      write(6,*)"first run done"

      DO 21 I=1,100,1
      OPEN(UNIT=20,FILE='test31.dat',STATUS='OLD')
      REWIND(UNIT=20)
      CALL DOINIT
      CALL MOPAC(20,*9999)
      CLOSE(UNIT=20)
      write(6,*)"second run done"   
   21 continue
      GOTO 9990     

   30 CONTINUE
C     For running most of the test suite files back-to-back to ensure
C     that reinitializing does not ruin program's calculational accuracy
C     and there is no "data forward contamination" between successive
C     runs of the MOPAC subroutine (that is, no carry-over between runs).
      DO 31 I=1,38
      IF (I.EQ.10.OR.I.EQ.16.OR.I.EQ.27.OR.(I.GE.32.AND.I.LE.34)
     &    .OR.I.EQ.7) 
     &    GOTO 31
      CALL DOINIT
      write(6,'(A,I2,A)') "Opening input file for test run ",I,"."
      WRITE(FNAME,'(A,I2.2,A)') "test",I,".dat"
      write(6,'(A,A,A)') "Opening input file for test run ",FNAME,"."
      OPEN(UNIT=30,FILE=FNAME)
      CALL MOPAC(30,*9999)
      CLOSE(UNIT=30)
      write(6,*) "Done with run of ",FNAME
   31 CONTINUE
      goto 9990




 9999 WRITE(6,'(A)') "A STOP statement was encountered and the call"//
     &               " stack unwound to MSTART."
 9990 CONTINUE
      END
