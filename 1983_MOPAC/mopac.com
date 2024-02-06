$! COMMAND FILE TO SUBMIT A MOPAC JOB TO A BATCH QUEUE. 
$! THIS COMMAND CAN BE USED 'MANUALLY' BY ENTERING THE INSTRUCTION
$!    "$ @MOPAC <filename> <queue> <priority>" or
$!    "$ @MOPAC" and be prompted for the other arguments,
$! A RECOMMENDED APPROACH WOULD BE TO INSERT INTO THE LOGIN COMMAND THE LINE
$!    "$ MOPAC :== @MOPACDIRECTORY:MOPAC" This would allow the command
$!
$!		"$MOPAC <filename>  <queue>  <priority>"   to be used
$!
$!	Fetch parameters
$!
$	IF P1.NES."" THEN GOTO H
$ G:
$	INQUIRE P1 "What file? "
$ H:
$	LEN = 'F$LEN(P1)'
$	DOT = 'F$LOC(".",P1)
$	IF LEN.EQ.DOT THEN GOTO CHECK
$		LEXT = LEN - DOT
$		EXT := 'F$EXT(DOT,LEXT,P1)'
$		P1 := 'F$EXT(0,DOT,P1)'
$CHECK:
$!    check to see if file is there, if not send error message
$	OPEN /ERR=NOFILE DUMMY 'P1'.DAT 	! see if data file is there
$	CLOSE DUMMY				! Yes
$	GOTO OKAY
$NOFILE:
$	WRITE SYS$OUTPUT -
	" error opening ''P1'.DAT"
$	P1 := ""
$	GOTO G
$OKAY:
$ C:
$	IF P2.NES."" THEN GOTO K
$ J:
$	INQUIRE P2 "What queue? (QUEUE1, QUEUE2, BATCH) "
$
$       IF P3.EQS."" THEN INQUIRE P3 "What priority? [5]"
$ K:
$		IF P2.EQS. "FLASH"	THEN GOTO M
$		IF P2.EQS. "FAST"	THEN GOTO M
$		IF P2.EQS. "BATCH"	THEN P2 := "SYS$BATCH"
$		IF P2.EQS. "SYS$BATCH"	THEN GOTO M
$		WRITE SYS$OUTPUT -
	" Queue ''P2'?"
$		GOTO J
$ M:
$!
$       IF P3.EQS."" THEN P3 := "5"
$!
$!	Submit
$!
$SUBMIT /NAME='P1' MOPACDIRECTORY:RMOPAC /QUEUE='P2' -
/PARA=("''P1'","''F$DIRECTORY()'")/PRIO='P3'
$EXIT
