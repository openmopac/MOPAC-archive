      PROGRAM MAIN
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
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /KEYWRD/ KEYWRD
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /NUMCAL/ NUMCAL
      COMMON /TIME  / TIME0
      COMMON /PATH  / LATOM,LPARAM,REACT(200)
      CHARACTER*80 KEYWRD
      OPEN(UNIT=5,FILE='FOR005',STATUS='OLD',BLANK='ZERO')
      REWIND 5
      OPEN(UNIT=6,FILE='FOR006',STATUS='UNKNOWN')
      REWIND 6
      NUMCAL=1
C
      TIME0=SECOND()
C
C READ AND CHECK INPUT FILE, EXIT IF NECESSARY.
C     WRITE INPUT FILE TO UNIT 6 AS FEEDBACK TO USER
C
   10 CALL READ
      IF(INDEX(KEYWRD,'AUTHOR') .NE. 0) THEN
         WRITE(6,'(10X,'' MOPAC - A GENERAL MOLECULAR ORBITAL PACKAGE'',
     1/         ,10X,''   ORIGINAL VERSION WRITTEN IN 1983'')')
         WRITE(6,'(10X,''     BY JAMES J. P. STEWART AT THE'',/
     1         ,10X,''     UNIVERSITY OF TEXAS AT AUSTIN'',/
     2         ,10X,''          AUSTIN, TEXAS, 78712'')')
      ENDIF
C
C INITIALIZE CALCULATION AND WRITE CALCULATION INDEPENDENT INFO
C
      IF(INDEX(KEYWRD,'0SCF') .NE. 0) GOTO 70
      IF(INDEX(KEYWRD,'EXTERNAL') .NE. 0) THEN
         CALL AM1
      ELSE
         CALL MOLDAT
      ENDIF
      IF (INDEX(KEYWRD,' XYZ') .NE. 0.AND.NVAR.NE.0) THEN
C
C   SET ALL OPTIMIZATION FLAGS TO 1.  IT IS POSSIBLE TO SPECIFY " XYZ"
C   AND HAVE 3N-6 FLAGS SET AND STILL NOT HAVE EVERY ATOM MARKED FOR
C   OPTIMIZATION.  THIS CAN HAPPEN IF DUMMY ATOMS ARE PRESENT
C   TO PREVENT THIS, WE EXPLICITELY SET ALL FLAGS.
C
         NVAR=0
         DO 30 I=1,NATOMS
C
C  I DON'T THINK THE FOLLOWING LOGICAL WILL EVER BE TRUE, BUT I'M NOT
C  TAKING IT OUT IN THIS VERSION, JUST IN CASE.
C
            IF(LABELS(I).EQ.99) GOTO 30
            DO 20 J=1,3
               NVAR=NVAR+1
               LOC(1,NVAR)=I
               LOC(2,NVAR)=J
   20       XPARAM(NVAR)=GEO(J,I)
   30    CONTINUE
      ENDIF
      IF (INDEX(KEYWRD,'RESTART').EQ.0)THEN
         IF (INDEX(KEYWRD,'1SCF') .NE. 0) THEN
            IF(LATOM.NE.0)THEN
               WRITE(6,'(//,10X,A)')'1SCF SPECIFIED WITH PATH.  THIS PAI
     1R OF'
               WRITE(6,'(   10X,A)')'OPTIONS IS NOT ALLOWED'
               GOTO 70
            ENDIF
            NVAR=0
            IF(INDEX(KEYWRD,'GRAD').NE.0) THEN
               NVAR=0
               DO 50 I=2,NATOMS
                  IF(LABELS(I).EQ.99) GOTO 50
                  IF(I.EQ.2)ILIM=1
                  IF(I.EQ.3)ILIM=2
                  IF(I.GT.3)ILIM=3
                  DO 40 J=1,ILIM
                     NVAR=NVAR+1
                     LOC(1,NVAR)=I
                     LOC(2,NVAR)=J
   40             XPARAM(NVAR)=GEO(J,I)
   50          CONTINUE
            ENDIF
         ENDIF
      ENDIF
C
C CALCULATE DYNAMIC REACTION COORDINATE.
C
C
      IF(INDEX(KEYWRD,'SADDLE') .NE. 0) THEN
         CALL REACT1(ESCF)
         GOTO 60
      ENDIF
      CLOSE (5)
      IF(INDEX(KEYWRD,'STEP1') .NE. 0) THEN
         CALL GRID
         GOTO 70
      ENDIF
      IF (LATOM .NE. 0) THEN
C
C       DO PATH
C
         CALL PATHS
         GOTO 70
      ENDIF
      IF (INDEX(KEYWRD,'FORCE') + INDEX(KEYWRD,'IRC=') .NE. 0 ) THEN
C
C FORCE CALCULATION IF DESIRED
C
         CALL FORCE
         GOTO 70
      ENDIF
      IF(INDEX(KEYWRD,' DRC') + INDEX(KEYWRD,' IRC') .NE. 0) THEN
C
C   IN THIS CONTEXT, "REACT" HOLDS INITIAL VELOCITY VECTOR COMPONENTS.
C
         CALL DRC(REACT,REACT)
         GOTO 70
      ENDIF
C
      IF(INDEX(KEYWRD,'NLLSQ') .NE. 0) THEN
         CALL NLLSQ(XPARAM, NVAR )
         CALL COMPFG(XPARAM,.TRUE.,ESCF,.TRUE.,GRAD,.TRUE.)
         GOTO 60
      ENDIF
C
      IF(INDEX(KEYWRD,'SIGMA') .NE. 0) THEN
         CALL POWSQ(XPARAM, NVAR, ESCF)
         GOTO 60
      ENDIF
C
C ORDINARY GEOMETRY OPTIMISATION
C
      CALL FLEPO(XPARAM, NVAR, ESCF)
   60 CALL WRITE(TIME0, ESCF)
      IF(INDEX(KEYWRD,'POLAR') .NE. 0) THEN
         CALL POLAR
      ENDIF
   70 TIM=SECOND()-TIME0
      WRITE(6,'(///,'' TOTAL CPU TIME: '',F16.2,'' SECONDS'')') TIM
      WRITE(6,'(/,'' == MOPAC DONE =='')')
      STOP
      END
