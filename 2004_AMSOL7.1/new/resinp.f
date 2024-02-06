      SUBROUTINE RESINP(FUNCT)
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
       COMMON /ONESCM/ ICONTR(100)                                      DJG0995
C***********************************************************************
C
C     THIS SUBROUTINE CREATES A .INP FILE
C
C     CREATED FROM EXISTING CODE, DJG0395
C
C***********************************************************************
      SAVE
      IF (ICONTR(57).EQ.1) THEN                                         DJG0995
         NFILE=JINP                                                     GDH1195
         OPEN(UNIT=NFILE,STATUS='UNKNOWN')                              DJG0995
         ICONTR(57)=2                                                   DJG0995
      ENDIF                                                             DJG0995
      REWIND(NFILE)                                                     DJG0995 
      CALL ZMPRNT(NFILE,FUNCT)                                          DJG0995
      RETURN                                                            DJG0995
      END                                                               DJG0995
