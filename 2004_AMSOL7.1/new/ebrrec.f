      SUBROUTINE EBRREC(COORD,BORN,CORCK)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
         INCLUDE 'KEYS.i'
         INCLUDE 'SIZES.i'
         INCLUDE 'SIZES2.i'
C******************************************************************************
C
C   THIS SUBROUTINE CALCULATES THE EFFECTIVE BORN RADII BY THE FORCE
C   RECTANGLE QUADRATURE.
C
C   BORN = EFFECTIVE BORN RADII
C   IVSZ = FOR EACH ATOM, NUMBER OF SHELLS TO ENCLOSE ENTIRE MOLECULE
C   RAC = COULOMB RADII
C
C   CREATED BY DJG 0995 FROM EXISTING LINES IN SRFCTY
C
C   WARNING: ANALYTICAL DERIVATIVES FOR BORNPL NOT IMPLEMENTED HERE     DL0397
C******************************************************************************
      COMMON /ONESCM/ ICONTR(100)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM), RMAX(NUMATM)
     1               ,URLIO(3,NUMATM,NUMATM)                            DL0397
      COMMON /ARACOM/ R(NPACK),R2(NPACK),ISAVE,DONE
     1               ,RAC(NUMATM),RAC2(NUMATM),DRAC(NUMATM,NUMATM)      DL0397
     2               ,RAS(NUMATM),RALG(NUMATM)
      COMMON /IVSZCM/ IVSZ(NUMATM), IVSZC
      COMMON /AREACM/ NOPTI, TAREA, NINTEG
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      LOGICAL CORCK,DONE2,DONE,TAREA
      DIMENSION RA(NUMATM),BORN(*),COORD(3,*)
      IF (ICONTR(53).EQ.1) THEN
         ICONTR(53)=2
         PMAREA=1/(4.0D0*PI)                                            GDH0394
         IF(ITEXPN.EQ.0) THEN                                           DJG0995
            TEXPN=1.5D0
         ELSE
            TEXPN=FITEXP                                                DJG0995
         ENDIF
         IF(ITONE.EQ.0) THEN                                            DJG0995
            IF(IRAD.NE.0.OR.IRAD.NE.1) THEN                             DJG0995
               TINIT=0.01D0
            ELSE
               TINIT=0.05D0
            ENDIF
         ELSE
            TINIT=FITONE*.5                                             DJG0995
         ENDIF
      ENDIF
      TSTEP=TINIT
      IFLAG=0                                                           GDH0494
      IEND=0                                                            GDH0394
      DO 50 I=1,NUMAT
         RA(I)=RAC(I)
         BORN(I)=0.0D0
50    CONTINUE
      IF (.NOT.CORCK) THEN                                              DJG0995
C
C        CALCULATE INTERATOMIC DISTANCES
C
         ISAVE=0
         DONE=.FALSE.
         DO 100 I=1,NUMAT
            RMAX(I)=0.D0
            DO 100 J=1,I
               IF (RMAX(J).LT.RLIO(I,J)+RA(I))RMAX(J)=RLIO(I,J)+RA(I)   GDH0194
               IF (RMAX(I).LT.RLIO(I,J)+RA(J))RMAX(I)=RLIO(I,J)+RA(J)   GDH0194
100      CONTINUE
      ENDIF
      DO 300 IAT=1,NUMAT
C        IF(RA(IAT).EQ.0.D0) GO TO 300
C        Determines which atoms are attached to the atom in question.
C
C        SAVE ORIGINAL RA IN AHOLD AND ADJUST FIRST SPHERE TO COULOMB SURFACE
C
         AHOLD=RA(IAT)
         DONE2=.FALSE.
         NIVSZ=0                                                        GDH0194
         PRCNT=0.1D0                                                    GDH0194
C
C        IFLAG DETERMINES WHETHER THE CALCULATION IS FOR THE CENTER OF A
C        BORN SHELL OR THE INNER SURFACE. THE FINAL SHELL MUST ENTIRELY
C        ENCLOSE THE REMAINDER OF THE MOLECULE OR SUPERMOLECULE. IEND FLAGS
C        THIS CONDITION HAVING BEEN MET
C
         IF(IFLAG.EQ.1) GO TO 200
150      IF(RA(IAT).GE.RMAX(IAT)) THEN                                  GDH0194
            IF(TSTEP.EQ.TINIT) TSTEP=0.D0
            RA(IAT)=RA(IAT)+TSTEP                                       GDH0194
            GOTO 250                                                    GDH0194
         ENDIF                                                          GDH0194
         IFLAG=1
         RA(IAT)=RA(IAT)+TSTEP                                          GDH0194
         IF (TAREA) THEN
            AREA=AREAL(RA,NUMAT,IAT)                                    DL0397
         ELSE
            AREA=AREAH(DOENP,COORD,RA,IAT,CORCK,DONE2)                  DJG0995
         ENDIF
200      IFLAG=0
         PRCNT=AREA*PMAREA                                              GDH0394
         TDIFF=1.D0/(RA(IAT)-TSTEP)-
     1         1.D0/(RA(IAT)+TSTEP)
         BORN(IAT)=BORN(IAT)+PRCNT*TDIFF
         NIVSZ=NIVSZ+1                                                  GDH0194
         RA(IAT)=RA(IAT)+TSTEP
         TSTEP=TEXPN*TSTEP
         GOTO 150                                                       GDH0194
C
C        ADD THE FINAL SHELL'S CONTRIBUTION AND RESET ALL THE STEP VARIABLES
C        TO INITIAL VALUES. RESET THE PROPER RADIUS FOR ATOM I.
C
250      BORN(IAT)=BORN(IAT)+1.D0/RA(IAT)
         BORN(IAT)=1.0D0/BORN(IAT)
         IEND=0
         TSTEP=TINIT
         RA(IAT)=AHOLD
         IVSZ(IAT)=NIVSZ+1                                              GDH0194
300   CONTINUE
      RETURN
      END
