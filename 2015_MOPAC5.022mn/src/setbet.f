      SUBROUTINE SETBET( NATM1, NATM2, BETTYP, BETVAL,*)                CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*5 BETTYP
      DOUBLE PRECISION BETVAL
* 
*  Set the value of the special SRP BETAs ( Ivan Rossi - April '94 )
*      WARNING: BETSP(x,y) is DIFFERENT from BETSP(y,x)
*
*   INPUT:
*     NATM1, NATM2 : Atomic numbers of the atom couple
*     BETTYP : Name of the Beta parameter type (UPPERCASE!)
*     BETVAL : value of the BETA to set
*
      INCLUDE 'SIZES.i'                                                
      COMMON /SRPI/ IBTPTR(120), NATPTR(MXATSP), NATSP
     *       /SRPR/ BETSS(MAXBET), BETSP(MXATSP,MXATSP), BETPP(MAXBET)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),                            debug
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)                  debug
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      COMMON /HPUSED/ HPUSED                                            LF0111
      LOGICAL HPUSED                                                    LF0111
c
      DIMENSION NATM(2)
C
      include 'corgen.f'    ! Common block declaration for /CORGEN/.    LF0211
        NATM(1)=NATM1
        NATM(2)=NATM2
c#        write(6,*) "natm()=",natm(1),natm(2)
        if (natm(1).eq.108.and.hpused) natm(1)=9   ! LF
        if (natm(2).eq.108.and.hpused) natm(2)=9   ! LF
        if (natm(1).eq.108.and.hpused) natm1=9   ! LF
        if (natm(2).eq.108.and.hpused) natm2=9   ! LF
c#        write(6,*) "natm1=",natm1
c#        write(6,*) "natm2=",natm2
c#        write(6,*) "hpused=",hpused
c#        write(6,*) "iel1=",iel1
c#        write(6,*) "iel2=",iel2
C
C   INPUT Error checking
C
C#      WRITE(6,*) "NATM(1)=",NATM(1)
C#      WRITE(6,*) "NATM(2)=",NATM(2)
C#      WRITE(6,*) "IBTPTR(1)=",IBTPTR(1)
C#      WRITE(6,*) "IBTPTR(6)=",IBTPTR(6)
C#      WRITE(6,*) "IBTPTR(8)=",IBTPTR(8)
C#      WRITE(6,*) "IBTPTR(9)=",IBTPTR(9)
C#      WRITE(6,*) "IBTPTR(108)=",IBTPTR(108)
      do 10 j=1,2
        if (IBTPTR(NATM(j)) .eq. 0) then 
          IF (DOPRNT) WRITE(6,'(                                        CIO
     &            "SETBET: Atom type",I4," NOT present!")') NATM(j)     CIO
          RETURN 1                                                      CSTP (stop)
        endif
 10   continue
c
c  Calculate address of beta (packed lower symmetric matrix) and set value
c
     
      IROW=IBTPTR(NATM(1))
      JCOL=IBTPTR(NATM(2))
      if( JCOL .gt. IROW ) then
        IROW=IBTPTR(NATM(2))
        JCOL=IBTPTR(NATM(1))
      ENDIF
      j=NATSP*(JCOL-1)+IROW -(JCOL*(JCOL-1))/2
      if( BETTYP .eq. 'BETPP') then
        BETPP(j)=BETVAL
        prbeta(natm1,natm2,2,2)=betval                                  LF0911
        prbeta(natm2,natm1,2,2)=betval                                  LF0911
c#        WRITE(6,*) "Set PRBETA(",baseindex+3,") to ",BETVAL   !LF0211
      else if( BETTYP .eq. 'BETSP') then
           BETSP(IBTPTR(NATM1),IBTPTR(NATM2))=BETVAL
           prbeta(natm1,natm2,1,2)=betval                               LF0911
           prbeta(natm2,natm1,2,1)=betval                               LF0911
c#          WRITE(6,*) "Set PRBETA(",baseindex+1,") to ",BETVAL   !LF0211
c#          WRITE(6,*) "Set PRBETA(",baseindex+2,") to ",BETVAL   !LF0211
c#          WRITE(6,*) "Set PRBETA(",baseindex+1,") to ",BETVAL   !LF0211
c#          WRITE(6,*) "Set PRBETA(",baseindex+2,") to ",BETVAL   !LF0211
      else if( BETTYP .eq. 'BETSS') then
         BETSS(j)=BETVAL
         prbeta(natm1,natm2,1,1)=betval                                  LF0911
         prbeta(natm2,natm1,1,1)=betval                                  LF0911
c#        WRITE(6,*) "Set PRBETA(",baseindex+0,") to ",BETVAL   !LF0211
      else
        IF (DOPRNT) write(6,'("SETBET: Wrong beta type",A5)') BETTYP    CIO
        RETURN 1                                                        CSTP (stop)
      endif
      return
 9999 RETURN 1                                                          CSTP
      END
