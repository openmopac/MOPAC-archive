      SUBROUTINE MOPAC(MYREAD,*)
C
C         Notice of Public Domain nature of MOPAC
C
C      'This computer program is a work of the United States
C       Government and as such is not subject to protection by
C       copyright (17 U.S.C. # 105.)  Any person who fraudulently
C       places a copyright notice or does any other act contrary
C       to the provisions of 17 U.S. Code 506(c) shall be subject
C       to the penalties provided therein.  This notice shall not
C       be altered or removed from this software and is to be on
C       all reproductions.'
C
C	mopac 5017 main: to use mopac outside of morate.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      COMMON /KEYWRD/ KEYWRD
      COMMON /GEOVAR/ XPARAM(MAXPAR), NVAR, LOC(2,MAXPAR)               IR0394
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
C Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
     3       /MOLKSR/ FRACT
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /MESAGE/ IFLEPO,ISCF
      COMMON /NUMCAL/ NUMCAL
      COMMON /TIMER / TIME0
C   Splitted common PATH for portability                                IR0394
      COMMON /PATHI / LATOM,LPARAM
      COMMON /PATHR / REACT(200)
      COMMON /COORD/ COORD(3,NUMATM)
c
C Morate unit input number: use this instead of 5 for compatibility     IR0494
      COMMON /IOCM/ IREAD
C
      COMMON /LAST  / LAST
      COMMON /ATOMIC/ EISOL(120),EHEAT(120)
      COMMON /MULTIP/ DD(120),QQ(120),AM(120),AD(120),AQ(120)
C
C     COMMON BLOCKS FOR SRP (Ivan Rossi - April 94)                    IR0494
C
      COMMON /SRPI/ IBTPTR(120), NATPTR(MXATSP), NATSP
     *       /SRPL/ ISSRP
     *       /SRPR/ BETSS(MAXBET), BETSP(MXATSP,MXATSP), BETPP(MAXBET)
      LOGICAL ISSRP
      CHARACTER*160 KEYWRD 
C
      COMMON /IONPOT/ ATOMIP(120)
C     The unit number to read in input data is given by IREAD for the
C     read of MOPAC's program execution.  Changing it for testing
C     purposes to a value passed in from the calling subroutine MSTART.
C      IREAD=5
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      COMMON /HPUSED/ HPUSED
      LOGICAL HPUSED
      LOGICAL DOPRNT                                                    LF0510
      COMMON /ONESCM/ ICONTR(100)                                       JZ0315
      IREAD=MYREAD
      NUMCAL=0
   10 NUMCAL=NUMCAL+1
C
      TIME0=SECOND()

C                                                                       DJG0995
C  ICONTR IS A FLAG WHICH INDICATES WHETHER A SUBROUTINE HAS NEVER BEEN DJG0996
C  ENTERED (ICONTR=1) OR HAS BEEN ENTERED BEFORE (ICONTR=0)             DJG0996
C                                                                       DJG0996
C  CURRENT HIGHEST-USED VALUE OF ICONTR IS 62                           DL0397
C  ICONTR IS ALSO RESET IN THE SUBROUTINE PATHS (THREE SPOTS) BETWEEN   DJG0995
C  EACH REACTION COORDINATE CALCULATION                                 DJG0995
C  AND ALSO AT THE END OF WRITES IF A TRUES/CALC CALCULATION            DJG0995
C                                                                       DJG0995
      DO  I=1,100                                                       GDH0895
        ICONTR(I)=1                                                     GDH0895
      ENDDO
C
C READ AND CHECK INPUT FILE, EXIT IF NECESSARY.
C     WRITE INPUT FILE TO UNIT 6 AS FEEDBACK TO USER
C
C      WRITE (6,*) '*** left for      READMO ***'                       CMESSAGE
   20 CALL READMO(*9999)                                                CSTP(call)
C      WRITE (6,*) '*** returned from READMO ***'                       CMESSAGE
      EMIN=0.D0
C#      CALL TIMER('AFTER READ')
      IF(NATOMS.EQ.0) GOTO 50
      IF(INDEX(KEYWRD,'AUTHOR') .NE. 0) THEN
        IF (DOPRNT) THEN                                                CIO
         WRITE(6,'(10X,'' MOPAC - A GENERAL MOLECULAR ORBITAL PACKAGE'',
     1/         ,10X,''   ORIGINAL VERSION WRITTEN IN 1983'')')
         WRITE(6,'(10X,''     BY JAMES J. P. STEWART AT THE'',/
     1         ,10X,''     UNIVERSITY OF TEXAS AT AUSTIN'',/
     2         ,10X,''          AUSTIN, TEXAS, 78712'')')
        ENDIF                                                           CIO
      ENDIF
C
C INITIALIZE CALCULATION AND WRITE CALCULATION INDEPENDENT INFO
C
      IF(INDEX(KEYWRD,'0SCF') .NE. 0) THEN
         IF (DOPRNT) WRITE(6,'(A)')' GEOMETRY IN MOPAC Z-MATRIX FORMAT' CIO
         CALL GEOUT(*9999)                                              CSTP(call)
         GOTO 50
      ENDIF
C
C     If the keyword EXTERNAL appears in the input, do top half
C     of the MOLDAT to initialize the parameters, and then call EXTPAR
C     to change the parameters specified in the EXTERNAL file. After that,
C     EXTPAR calls MOLDAT again to do the bottom half of MOLDAT.
C     If there is no EXTERNAL keyword specified, then just do the whole
C     MOLDAT once.                     Wei-Ping, Mar 4, 1993
C
      IF(INDEX(KEYWRD,'EXTERNAL') .NE. 0) THEN
         CALL MOLDAT(0,*9999)                                           0304WH93 CSTP(call)
         CALL EXTPAR(*9999)                                             IR0494 CSTP(call)
      ELSE
         CALL MOLDAT(2,*9999)                                           0304WH93 CSTP(call)
      ENDIF
      IF (INDEX(KEYWRD,'RESTART').EQ.0)THEN
         IF (INDEX(KEYWRD,'1SCF').NE.0) THEN
            IF(LATOM.NE.0)THEN
               IF (DOPRNT) THEN                                         CIO
               WRITE(6,'(//,10X,A)')'1SCF SPECIFIED WITH PATH.  THIS PAI
     1R OF'
               WRITE(6,'(   10X,A)')'OPTIONS IS NOT ALLOWED'
               ENDIF                                                    CIO
               GOTO 50
            ENDIF
            IFLEPO=1
            ISCF=1
            LAST=1
            I=INDEX(KEYWRD,'GRAD')
            DO 39 J=1,NVAR
  39        GRAD(J)=0.D0
            CALL COMPFG(XPARAM,.TRUE.,ESCF,.TRUE.,GRAD,I.NE.0,*9999)    CSTP(call)
            GOTO 40
         ENDIF
      ENDIF
C
C CALCULATE DYNAMIC REACTION COORDINATE.
C
C
      IF(INDEX(KEYWRD,'SADDLE') .NE. 0) THEN
         CALL REACT1(ESCF,*9999)                                        CSTP(call)
         GOTO 50
      ENDIF
      IF(INDEX(KEYWRD,'STEP1') .NE. 0) THEN
         CALL GRID(*9999)                                               CSTP(call)
         GOTO 50
      ENDIF
      IF (LATOM .NE. 0) THEN
C
C       DO PATH
C
         CALL PATHS(*9999)                                              CSTP(call)
         GOTO 50
      ENDIF
      IF (   INDEX(KEYWRD,'FORCE') .NE. 0
     1  .OR. INDEX(KEYWRD,'THERM') .NE. 0 ) THEN
C
C FORCE CALCULATION IF DESIRED
C
         CALL FORCE(*9999)                                              CSTP(call)
         GOTO 50
      ENDIF
      IF(INDEX(KEYWRD,' DRC') + INDEX(KEYWRD,' IRC') .NE. 0) THEN
C
C   IN THIS CONTEXT, "REACT" HOLDS INITIAL VELOCITY VECTOR COMPONENTS.
C
         CALL DRC(REACT,REACT,*9999)                                    CSTP(call)
         GOTO 50
      ENDIF
C
      IF(INDEX(KEYWRD,'NLLSQ') .NE. 0) THEN
         CALL NLLSQ(XPARAM, NVAR ,*9999)                                CSTP(call)
         CALL COMPFG(XPARAM,.TRUE.,ESCF,.TRUE.,GRAD,.TRUE.,*9999)       CSTP(call)
         GOTO 40
      ENDIF
C
      IF(INDEX(KEYWRD,'SIGMA') .NE. 0) THEN
         CALL POWSQ(XPARAM, NVAR, ESCF,*9999)                           CSTP(call)
         GOTO 40
      ENDIF
C
C  EF OPTIMIZATION
C
      IF(INDEX(KEYWRD,' EF').NE.0 .OR. INDEX(KEYWRD,' TS').NE.0) THEN
        CALL EF (XPARAM,NVAR,ESCF,*9999)                                CSTP(call)
        GOTO 40
      ENDIF
C
C ORDINARY GEOMETRY OPTIMIZATION
C
      CALL FLEPO(XPARAM, NVAR, ESCF,*9999)                              CSTP(call)
   40 LAST=1
      IF(IFLEPO.GE.0)CALL WRITMO(TIME0, ESCF,*9999)                     CSTP(call)
      IF(INDEX(KEYWRD,'POLAR') .NE. 0) THEN
         CALL POLAR(*9999)                                              CSTP(call)
      ENDIF
   50 TIM=SECOND()-TIME0
      IF (DOPRNT) WRITE(6,'(///,                                        CIO
     &        '' TOTAL CPU TIME: '',F16.2,'' SECONDS'')') TIM           CIO
      IF (DOPRNT) WRITE(6,'(/,'' == MOPAC DONE =='')')
C#      call debugs("final parameters shown:")

      RETURN
 9999 RETURN 1
      END
