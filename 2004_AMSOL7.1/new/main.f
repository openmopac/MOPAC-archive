C     Main program of AMSOL
C     Copyright© 1992-1998 Regents of the University of Minnesota.
C     All Rights Reserved.
C
      PROGRAM MAIN                                                      GDH1095
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GDH1095
      INCLUDE 'FFILES.i'                                                GDH1095
      SAVE                                                              GDH1294
C
C     This section sets the FORTRAN UNIT numbers which will be used
C     for the various output and input files in the AMSOL program.
C     The default settings for these variables correspond to the
C     values used in the distributed run scripts.
      JDAT=5
      JOUT=6
      JRES=9
      JDEN=10
      JARC=12
      JGPT=13
      JDMT=15
      JINP=18
      JXSM=19
      JXKW=20
      JDVP=14
      JSCR=21
C
C     This section sets the variable that determines whether the
C     calculation was successful or not.  The largest IWHERE value is  GDH1195
C     175 (The first available is 176)                                 GDH0397
C
      ISTOP=0
      IWHERE=0
C
      CALL AMSOL                                                        GDH1095
C
      IF (ISTOP.NE.0) CALL STPJOB                                       DAL0303
      END
