C    BETSRP unit (Ivan Rossi - April 94)                               
C
C include in SIZES.i the two following paramrters:
C
C     MXATSP : MAXIMUM NUMBER OF atomic species in the system
C     MXSRPB : MAXIMUM NUMBER OF SRP special beta allowed
*
*     COMMON BLOCKS FOR SRP (Ivan Rossi - April 94)                    IR0494
*
*     COMMON /SRPI/ IBTPTR(120), NATPTR(MXATSP), NATSP
*    *       /SRPL/ ISSRP
*    *       /SRPR/ BETSS(MAXBET), BETSP(MXATSP,MXATSP), BETPP(MAXBET)
*     LOGICAL ISSRP
c
C     Explanations of variables used in INIBET, GETBET, SETBET, and EXTPAR:
C
C     NATSP      = Number of atomic species in system.
C     IBTPTR     = Array linking atomic # (index of array) to index
C                  for that element in the NATPTR array.  This new indexing
C                  is that used by the BETSS, BETSP, and BETPP arrays of
C                  the /SRPR/ common block pairwise beta values.
C     NATPTR     = Array of atomic #s covering all atomic species in system.
C                  Reverse look-up of the IBTPTR in that it goes from the
C                  indexing used by the BETSS, BETSP, and BETPP arrays to
C                  the atomic numbers.
C     BETSS      = Packed 1D array of X(s)-Y(s) beta parameters.
C     BETPP      = Packed 1D array of X(p)-Y(p) beta parameters.
C     BETSP      = 2D array of X(s)-Y(p) and X(p)-Y(s) beta parameters.
C

      SUBROUTINE INIBET(*)                                              CSTP
*
*  Initialize arrays and pointers for special SRP BETAs (Ivan Rossi- April '94)
*
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'                                                
      COMMON /SRPI/ IBTPTR(120), NATPTR(MXATSP), NATSP
     *       /SRPR/ BETSS(MAXBET), BETSP(MXATSP,MXATSP), BETPP(MAXBET)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
*
*     COMMON BLOCKS FOR STANDARD BETAs
*
      COMMON /BETAS / BETAS(120),BETAP(120),BETAD(120)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      COMMON /HPUSED/ HPUSED                                            LF0111
      LOGICAL HPUSED                                                    LF0111
c
c     Initialize pointers to RSP betas
c
      IF (HPUSED) BETAS(9)=BETAS(108)                                   LF0111
      IF (HPUSED) BETAP(9)=BETAP(108)                                   LF0111
      NATSP=0
      do 5 i=1,120                                                      LF0410
   5     IBTPTR(i)=0
      DO 10 i=1,NATOMS
        if (LABELS(i) .eq. 99 .or. LABELS(i) .eq.107) goto 10
        if (IBTPTR(LABELS(i)) .eq. 0) then
           NATSP=NATSP+1
           if (NATSP .gt. MXATSP) then
             IF (DOPRNT) THEN                                           CIO
             Write(6,'("***  The parameter MXATSP is too SMALL ! ***")')
             Write(6,'("*** Increase it in SIZES.i and recompile ***")')
             ENDIF                                                      CIO
             RETURN 1                                                   CSTP (stop)
           ENDIF
C#           WRITE(6,*) "INIBET: NATSP=",NATSP," I=",I," LABELS(I)=",
C#     &                LABELS(I)
           NATPTR(NATSP)=LABELS(I)
           IBTPTR(LABELS(i))=NATSP
        endif
 10     continue
        do 50 j=1,NATSP
          do 50 i=j,NATSP
          BSS= 0.5d0*(BETAS(NATPTR(i))+BETAS(NATPTR(j)))
          BPP= 0.5d0*(BETAP(NATPTR(i))+BETAP(NATPTR(j)))
          call SETBET( NATPTR(i), NATPTR(j), 'BETSS', BSS ,*9999)        CSTP(call)
          call SETBET( NATPTR(i), NATPTR(j), 'BETPP', BPP ,*9999)        CSTP(call)
 50     continue
        do 60 j=1,NATSP
          do 60 i=1,NATSP
          BSP= 0.5d0*(BETAS(NATPTR(i))+BETAP(NATPTR(j)))
          call SETBET( NATPTR(i), NATPTR(j), 'BETSP', BSP ,*9999)        CSTP(call)
 60     continue
      RETURN
 9999 RETURN 1                                                          CSTP
      END
