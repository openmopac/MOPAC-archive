$!                            MOPACCOM COMMAND FILE
$!
$!
$!    Each user of MOPAC should put in their LOGIN.COM file the
$!    line 
$!          "$ @ <MOPAC-directoryname>MOPACCOM",
$!
$!    <MOPAC-directoryname> being the name of the disk and directory 
$!    which holds all the MOPAC files.
$!
$!      The following definitions are useful -
$!
$DDAT:==DIR *.DAT
$DOUT:==DIR *.OUT/SIZE/DATE/SIN=YEST
$DARC:==DIR *.ARC
$DRES:==DIR *.RES
$DDEN:==DIR *.DEN
$! 
$!    TO THE INSTALLER OF MOPAC: THE FOLLOWING LINES SHOULD
$!    BE ALTERED TO SUIT LOCAL CONDITIONS
$! 
$!    (1) Substitute for the word MOPACDIRECTORY: the actual compound name
$!        of the directory to hold MOPAC, e.g. DBA0:[MOPAC] or assign
$!        MOPACDIRECTORY via a logical, thus $ASS DBA0:[MOPAC] MOPACDIRECTORY
$!
$!        The command MOPAC will cause a MOPAC job to be submitted to a queue.
$!
$ASS   DISK$USER:[STEWART.MOPAC] MOPACDIRECTORY
$MOPAC  :== @MOPACDIRECTORY:MOPAC          
$! 
$!        The SHUT command will instruct a batch job to stop at the first
$!        opportunity.
$!
$SHUT   :== @MOPACDIRECTORY:SHUT
$!
$!     (2) To install the MOPAC HELP library, use the following line.
$!         Some modification may be necessary to suit local conditions.
$!         The HELP library is very useful, particularly when the manual
$!         has been mis-laid.
$!
$ASSIGN MOPACDIRECTORY:MOPAC HLP$LIBRARY   
$!
$!
$  ASS MOPACDIRECTORY:DIMSIZES.DAT SIZES

