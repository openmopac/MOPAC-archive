      SUBROUTINE GETSYM 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
      COMMON /GEOSYM/ NDEP, LOCPAR(MAXPAR), IDEPFN(MAXPAR), 
     1                LOCDEP(MAXPAR) 
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
C*********************************************************************** 
C 
C   GETSYM READS IN THE SYMMETRY DEPENDENCE RELATIONSHIPS. 
C 
C   ON EXIT     NDEP    = NUMBER OF SYMMETRY RELATIONS. 
C               LOCPAR  = ARRAY OF REFERENCE FUNCTION INDICES. 
C               IDEPFN  = ARRAY OF REFERENCE ATOM LOCATIONS. 
C               LOCDEP  = ARRAY OF DEPENDENT ATOM LOCATIONS. 
C 
C*********************************************************************** 
C 
C     LOCDEP IS THE ATOM WHOSE COORDINATES DEPEND ON THE COORDINATES OF 
C     LOCPAR. 
C     LOCPAR IS THE ATOM WHOSE COORDINATES ARE USED TO CALCULATE THOSE 
C     OF LOCDEP 
C     IDEPFN POINTS TO THE PARTICULAR FUNCTION TO BE USED (SEE NDDO) 
C 
C*********************************************************************** 
      DIMENSION IVALUE(40),VALUE(40) 
      CHARACTER  TEXT(18)*60, LINE*80                                   DJG0995
       SAVE
      DATA TEXT/ 
     1' Bond length    is set equal to the reference bond length   ',   GL0592
     2' Bond angle     is set equal to the reference bond angle    ', 
     3' Dihedral angle is set equal to the reference dihedral angle', 
     4' Dihedral angle varies as  90 degrees - reference dihedral  ', 
     5' Dihedral angle varies as  90 degrees + reference dihedral  ', 
     6' Dihedral angle varies as 120 degrees - reference dihedral  ', 
     7' Dihedral angle varies as 120 degrees + reference dihedral  ', 
     8' Dihedral angle varies as 180 degrees - reference dihedral  ', 
     9' Dihedral angle varies as 180 degrees + reference dihedral  ', 
     1' Dihedral angle varies as 240 degrees - reference dihedral  ', 
     2' Dihedral angle varies as 240 degrees + reference dihedral  ', 
     3' Dihedral angle varies as 270 degrees - reference dihedral  ', 
     4' Dihedral angle varies as 270 degrees - reference dihedral  ', 
     5' Dihedral angle varies as - reference dihedral              ', 
     6' Bond length varies as half the reference bond length       ', 
     7' Bond angle varies as half the reference bond angle         ', 
     8' Bond angle varies as 180 degrees - reference bond angle    ', 
     9' THE USER HAS TO SUPPLY THIS FUNCTION IN DEPVAR             '/ 
C 
C TITLE OUTPUT 
      IF (ICR.EQ.2) THEN                                                GDH0195
         IRD=JINP                                                       GDH0195
      ELSE                                                              GDH0195
         IRD=JDAT                                                       GDH0195
      ENDIF                                                             GDH0195
      WRITE (JOUT,10) 
   10 FORMAT (/,5X,25HParameter dependence data//                       GL0592
     1'        Reference atom      Function no.    Dependent atom(s)')  GL0592
C 
C INPUT SYMMETRY : FUNCTION, REFERANCE PARAMETER, AND DEPENDENT ATOMS 
C 
      NDEP=0 
   20 READ(IRD,'(A)',END=70) LINE                                       IR1294
      CALL NUCHAR(LINE,VALUE,NVALUE) 
C   INTEGER VALUES 
      DO 30 I=1,NVALUE 
   30 IVALUE(I)=VALUE(I) 
C   FILL THE LOCDEP ARRAY 
      IF(NVALUE.EQ.0.OR.IVALUE(3).EQ.0) GO TO 70 
      DO 40 I=3,NVALUE 
         IF(IVALUE(I).EQ.0) GOTO 50 
         NDEP=NDEP+1 
         LOCDEP(NDEP)=IVALUE(I) 
         LOCPAR(NDEP)=IVALUE(1) 
         IDEPFN(NDEP)=IVALUE(2) 
   40 CONTINUE 
   50 LL=I-1 
      WRITE(JOUT,60)IVALUE(1),IVALUE(2),(IVALUE(J),J=3,LL) 
   60 FORMAT(I13,I19,I14,20I3) 
      GO TO 20 
C 
C CLEAN UP 
   70 CONTINUE 
      WRITE(JOUT,80) 
   80 FORMAT(/,10X,'   Descriptions of the function numbers in the ',   GDH1093
     1      'above table.')
      DO 120 J=1,18 
         DO 90 I=1,NDEP 
            IF(IDEPFN(I).EQ.J) GOTO 100 
   90    CONTINUE 
         GOTO 120 
  100    WRITE(JOUT,110)J,TEXT(J) 
  110    FORMAT(I4,5X,A) 
  120 CONTINUE 
      WRITE(JOUT,130)                                                   GDH1093
  130 FORMAT(/,10X,'      Initial Geometry',/)                          GDH1093
      RETURN 
      END 
