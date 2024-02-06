      SUBROUTINE SASADO (COORD,NITER)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'
C ---------------------------------------------------------------------+
C     CALCULATE THE SOLVENT-ACCESSIBLE SURFACE AREA FOR G-CDS
C ON INPUT
C     LGR    = TRUE IF DERIVATIVES TO BE CALCULATED                     DL0397
C ON OUTPUT
C     ATAR   = SASA ASSOCIATED WITH SMALL SOLVENT RADIUS BY ATOM
C     ATLGAR = TOTAL SASA ASSOCIATED WITH LARGE SOLVENT RADIUS
C     CSAREA = SASA (LARGE RADIUS) BY ELEMENT
C     DATAR  = CARTESIAN DERIVATIVES OF ATAR IIF LGR=.TRUE.
C
C     CREATED BY DJG 0995 FROM EXISTING LINES IN SMX1 AND SRFCTY
C     IIF AREAL IN USE, WITH FIXED RADII, ADDED SOME DERIVATIVES:       DL0397
C     CARTESIAN DERIVATIVES OF ATAR:                                    DL0397
C         DATAR(i,j,k) = dATAR(k)/dCOORD(i,j),                          DL0397
C     CARTESIAN DERIVATIVES OF ATLGAR:                                  DL0397
C         DATLGR(i,j)= dATLGAR/dCOORD(i,j),                             DL0397
C     NOTE: /SCRAH1/ LOCAL STORAGE, /DSOLVA/ GLOBAL STORAGE.            DL0397
C ---------------------------------------------------------------------+
      COMMON /ARACOM/ R(NPACK),R2(NPACK),ISAVE,DONE
     1               ,RAC(NUMATM),RAC2(NUMATM),DRAC(NUMATM,NUMATM)      DL0397
     2               ,RAS(NUMATM),RALG(NUMATM)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /AREACM/ NOPTI, TAREA, NINTEG
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /SCRAH1/ DAREA(3,0:NUMATM),NC(0:NUMATM)                    DL0397
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL0397
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL0397
     2               ,DATLGR(3,NUMATM)                                  DL0397
     3               ,LGR,FLAGRD                                        DL0397
                      LOGICAL LGR,FLAGRD                                DL0397
      LOGICAL DONE2, TAREA, CORCK, HAREA, DONE, DOENP
      DIMENSION COORD(3,*)
      DATA ZERO /0.0D0/
      DOENP=.FALSE.
      HAREA=ISM2+ISM3+ISM21+ISM31+ISM22+ISM23.EQ.0
      IF (NITER.EQ.1) THEN
         CORCK=.FALSE.
      ELSE
         CORCK=.TRUE.
      ENDIF
      DONE2=.FALSE.
      IF (LGR) THEN                                                     DL0397
C        ATAR DERIVATIVES                                               DL0397
         DO 10 K=1,NUMAT                                                DL0397
   10    CALL SCOPY (3*NUMAT,ZERO,0,DATAR(1,1,K),1)                     DL0397
      ENDIF                                                             DL0397
      DO 30 IAT=1,NUMAT
         RAS2=RAS(IAT)**2                                               DL0397
         IF (HAREA.OR.NAT(IAT).NE.1) THEN                               DJG0995
            IF (TAREA) THEN
               CALL DAREAL (RAS,NUMAT,ATAR(IAT),DAREA,NCROSS,NC,IAT     DL0397
     .                     ,LGR,DAREA,.FALSE.)                          DL1097
               ATAR(IAT)=ATAR(IAT)*RAS2                                 DL0397
               IF (LGR) THEN                                            DL0397
                  DO 20 L=0,NCROSS                                      DL0397
                  DATAR(1,NC(L),IAT)=DAREA(1,L)*RAS2                    DL0397
                  DATAR(2,NC(L),IAT)=DAREA(2,L)*RAS2                    DL0397
                  DATAR(3,NC(L),IAT)=DAREA(3,L)*RAS2                    DL0397
   20             CONTINUE                                              DL0397
               ENDIF                                                    DL0397
            ELSE
C              NO DERIVATIVE AVAILABLE HERE                             DL0397
               ATAR(IAT)=AREAH(DOENP,COORD,RAS,IAT,CORCK,DONE2)*RAS2    DJG0995
            ENDIF
         ENDIF
   30 CONTINUE
      IF (IRAD.EQ.6.OR.IRAD.EQ.7.OR.IRAD.EQ.8.OR.IRAD.EQ.21.OR.         GDH0997
     .    ((IRAD.EQ.9.OR.IRAD.EQ.10).AND.(NUMSLV.EQ.20))) THEN          GDH0997
         CALL SCOPY (100,ZERO,0,CSAREA,1)                               DL0397
         ATLGAR=0.0D0                                                   DJG1094
         IF (LGR) CALL SCOPY (3*NUMAT,ZERO,0,DATLGR,1)                  DL0397
         DO 50 I=1,NUMAT                                                DJG0995
            RALG2=RALG(I)**2                                            DL0397
            IF (TAREA) THEN                                             DJG0995
               CALL DAREAL (RALG,NUMAT,ATLG,DAREA,NCROSS,NC,I,LGR       DL1097
     .                     ,DAREA,.FALSE.)                              DL1097
               ATLG=ATLG*RALG2                                          DL0397
               IF (LGR) THEN                                            DL0397
                  DO 40 L=0,NCROSS                                      DL0397
                  DATLGR(1,NC(L))=DAREA(1,L)*RALG2+DATLGR(1,NC(L))      DL0397
                  DATLGR(2,NC(L))=DAREA(2,L)*RALG2+DATLGR(2,NC(L))      DL0397
                  DATLGR(3,NC(L))=DAREA(3,L)*RALG2+DATLGR(3,NC(L))      DL0397
   40             CONTINUE                                              DL0397
               ENDIF                                                    DL0397
            ELSE                                                        DJG0995
C              NO DERIVATIVE HERE                                       DL0397
               ATLG=AREAH(DOENP,COORD,RALG,I,CORCK,DONE2)*RALG2         DJG0995
            ENDIF                                                       DJG0995
            ATLGAR=ATLGAR+ATLG                                          DJG0995
            CSAREA(NAT(I))=CSAREA(NAT(I))+ATLG                          DJG0995
   50    CONTINUE                                                       DJG0995
      ENDIF
      END
