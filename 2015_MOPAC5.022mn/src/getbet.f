      SUBROUTINE GETBET( NATM1, NATM2, BETTYP, RGETBT,*)                CSTP
CSTP  Conversion from function to subroutine for handling STOP statements.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CSTP  Return variable for GETBET when a function is now RGETBT.
      DOUBLE PRECISION RGETBT                                           CSTP
      CHARACTER*5 BETTYP
* 
*  Get the value of the special SRP BETAs ( Ivan Rossi - April '94 )
*      WARNING: BETSP(x,y) is DIFFERENT from BETSP(y,x)
*
*   INPUT:
*     NATM1, NATM2 : Atomic numbers of the atom couple
*     BETTYP : Name of the Beta parameter type (UPPERCASE!)
*   OUTPUT:
*     Value of the SRP BETA requested
*
      INCLUDE 'SIZES.i'                                                
      COMMON /SRPI/ IBTPTR(120), NATPTR(MXATSP), NATSP
     1       /SRPR/ BETSS(MAXBET), BETSP(MXATSP,MXATSP), BETPP(MAXBET)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      COMMON /HPUSED/ HPUSED                                            LF0111
      LOGICAL HPUSED                                                    LF0111
c
      DIMENSION NATM(2)
C
        NATM(1)=NATM1
        NATM(2)=NATM2
C
C   INPUT Error checking
C
      do 10 j=1,2
        if (IBTPTR(NATM(j)) .eq. 0) then 
          IF (DOPRNT)                                                   CIO
     &     WRITE(6,'("GETBET: Atom type",I4," NOT present!")') NATM(j)  CIO
      RETURN 1                                                           CSTP (stop)
        endif
 10   continue
c
c  Calculate address of beta and set value
c
      IROW=IBTPTR(NATM(1))
      JCOL=IBTPTR(NATM(2))
      if( JCOL .gt. IROW ) then
        IROW=IBTPTR(NATM(2))
        jCOL=IBTPTR(NATM(1))
      ENDIF
      j=NATSP*(JCOL-1)+IROW -(JCOL*(JCOL-1))/2
      if( BETTYP .eq. 'BETPP') then
        RGETBT=BETPP(j)
      else if( BETTYP .eq. 'BETSP') then
        RGETBT=BETSP(IBTPTR(NATM1), IBTPTR(NATM2))
      else if( BETTYP .eq. 'BETSS') then
        RGETBT=BETSS(j)
      else
        IF (DOPRNT) write(6,'("GETBET: Wrong beta type",A5)') BETTYP    CIO
      RETURN 1                                                          CSTP (stop)
      endif
      return
 9999 RETURN 1                                                          CSTP
      END
