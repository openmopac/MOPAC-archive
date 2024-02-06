      SUBROUTINE CISGEN (NUPP,NDOWN,NMOS,MICA,MICB,NMECI,LAB)           DL0397
C ---------------------------------------------------------------------+DL0397
C     CIS GENERATION OF MICROSTATES.                                    DL0397
C  ON INPUT                                                             DL0397
C     NUPP    : NUMBER OF ALPHA SPIN.                                   DL0397
C     NDOWN   : NUMBER OF BETA  SPIN.                                   DL0397
C     NMOS    : NUMBER OF CI-ACTIVE MOS.                                DL0397
C  ON OUTPUT                                                            DL0397
C     LAB          : NUMBER OF MICROSTATES GENERATED                    DL0397
C     MICA(NMOS,i) : LOCATION OF ALPHA SPINS FOR MICROSTATE i.          DL0397
C     MICB(NMOS,i) : LOCATION OF BETA  SPINS FOR MICROSTATE i.          DL0397
C ---------------------------------------------------------------------+DL0397
      DIMENSION MICA(NMECI,*),MICB(NMECI,*)                             DL0397
C     "GROUND" MICROSTATE (REFERENCE FOR SINGLE EXCITATION):            DL0397
      DO 10 K=1,NUPP                                                    DL0397
      MICA(K,1)=1                                                       DL0397
   10 CONTINUE                                                          DL0397
      DO 11 K=NUPP+1,NMOS                                               DL0397
      MICA(K,1)=0                                                       DL0397
   11 CONTINUE                                                          DL0397
      DO 12 K=1,NDOWN                                                   DL0397
      MICB(K,1)=1                                                       DL0397
   12 CONTINUE                                                          DL0397
      DO 13 K=NDOWN+1,NMOS                                              DL0397
      MICB(K,1)=0                                                       DL0397
   13 CONTINUE                                                          DL0397
      LAB=1                                                             DL0397
      IF (NUPP.EQ.NDOWN) THEN                                           DL0397
C        GENERATE SIMULTANEOUSLY ALPHA AND BETA SINGLE EXCITATIONS      DL0397
         DO 21 I=NUPP,1,-1                                              DL0397
         DO 21 J=NUPP+1,NMOS                                            DL0397
         LAB=LAB+2                                                      DL0397
         DO 20 K=1,NMOS                                                 DL0397
         MICA(K,LAB-1)=MICA(K,1)                                        DL0397
         MICB(K,LAB-1)=MICB(K,1)                                        DL0397
         MICA(K,LAB  )=MICA(K,1)                                        DL0397
   20    MICB(K,LAB  )=MICB(K,1)                                        DL0397
         MICA(I,LAB-1)=0                                                DL0397
         MICA(J,LAB-1)=1                                                DL0397
         MICB(I,LAB  )=0                                                DL0397
         MICB(J,LAB  )=1                                                DL0397
   21    CONTINUE                                                       DL0397
      ELSE                                                              DL0397
C        GENERATE ALPHA SINGLE EXCITATIONS                              DL0397
         DO 32 I=NUPP,1,-1                                              DL0397
         DO 31 J=NUPP+1,NMOS                                            DL0397
         LAB=LAB+1                                                      DL0397
         DO 30 K=1,NMOS                                                 DL0397
         MICA(K,LAB)=MICA(K,1)                                          DL0397
   30    MICB(K,LAB)=MICB(K,1)                                          DL0397
         MICA(I,LAB)=0                                                  DL0397
         MICA(J,LAB)=1                                                  DL0397
   31    CONTINUE                                                       DL0397
   32    CONTINUE                                                       DL0397
C        GENERATE BETA SINGLE EXCITATIONS                               DL0397
         DO 42 I=NDOWN,1,-1                                             DL0397
         DO 41 J=NDOWN+1,NMOS                                           DL0397
         LAB=LAB+1                                                      DL0397
         DO 40 K=1,NMOS                                                 DL0397
         MICA(K,LAB)=MICA(K,1)                                          DL0397
   40    MICB(K,LAB)=MICB(K,1)                                          DL0397
         MICB(I,LAB)=0                                                  DL0397
         MICB(J,LAB)=1                                                  DL0397
   41    CONTINUE                                                       DL0397
   42    CONTINUE                                                       DL0397
      ENDIF                                                             DL0397
      END                                                               DL0397
