$WRITE SYS$OUTPUT "  "
$WRITE SYS$OUTPUT "  "
$	OPEN /ERR=NODATA DUMMY 'P1'.DAT 	! see if DATA file is there
$       CLOSE DUMMY
$	OPEN /ERR=NOOUT DUMMY 'P1'.OUT 	! see if OUT file is there
$       CLOSE DUMMY
$WRITE SYS$OUTPUT "     Job called ''P1' is not running - This command"
$WRITE SYS$OUTPUT "     will only work for jobs actually running"
$GOTO OK
$NOOUT:
$COPY 'P1'.DAT 'P1'.TMP    ! This MUST be done in two lines.
$RENAME 'P1'.TMP 'P1'.END
$WRITE SYS$OUTPUT  "      Shutdown Command issued to Job called  ''P1'"
$GOTO  OK
$NODATA:
$WRITE SYS$OUTPUT "           File ''P1'.DAT not present in this Directory"
$OK:
$WRITE SYS$OUTPUT "  "
$WRITE SYS$OUTPUT "  "

