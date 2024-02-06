      SUBROUTINE CDSCAL
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'
C ---------------------------------------------------------------------+
C     SUM THE CDS TERMS, INCLUDING THE SPECIAL TERMS
C     CALCULATED IN SRFCAL AND STORED IN HB.
C  ON INPUT
C     SRFACI = SURFACE TENSIONS (INDEXED BY ATOM # IN Z-MATRIX)
C     ATAR = ATOMIC SASA'S
C     HB = H-BOND CORRECTIONS (MEANING DIFFERS BETWEEN MODELS)
C     DCDS = CARTESIAN DERIVATIVES OF THE SUM OF HBs (kcal/angstrom)    DL0397
C  ON OUTPUT
C     SRFACT = CDS ENERGY ATOM BY ATOM
C     SURFCT = CDS TOTAL ENERGY
C     DCDS = CARTESIAN DERIVATIVES OF SURFCT (kcal/angstrom)            DL0397
C
C     CREATED FROM EXISTING LINES IN BORNPL AND BRNPL2 BY DJG 0995
C     ANALYTICAL GRADIENT dCDS/dCARTESIAN FOR SM5.xx   BY  DL 0397
C ---------------------------------------------------------------------+
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /HBORDS/ HBORD(NUMATM), HB(NUMATM)
      COMMON /RADCOM/ SRFACI(NUMATM)
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL0397
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL0397
     2               ,DATLGR(3,NUMATM)                                  DL0397
     3               ,LGR,FLAGRD                                        DL0397
                      LOGICAL LGR,FLAGRD                                DL0397
      INCLUDE 'PARAMS.i'
C
C     COMPUTE SRFACT(NUMAT) AND SUM THEM IN SURFCT
C
      SURFCT=0.D0
      IF (ISM50.EQ.0.OR.(ISM50.EQ.1.AND.ICHARG.EQ.0)) THEN              GDH0696
         DO 20 IAT=1,NUMAT
         FACTOR=1D-3*SRFACI(IAT)                                        DL0397
         SRFACT(IAT)=FACTOR*ATAR(IAT)+HB(IAT)                           DL0397
         SURFCT=SURFCT+SRFACT(IAT)                                      DL0397
         IF (LGR) THEN                                                  DL0397
            DO 10 J=1,NUMAT                                             DL0397
            DCDS(1,J)=FACTOR*DATAR(1,J,IAT)+DCDS(1,J)                   DL0397
            DCDS(2,J)=FACTOR*DATAR(2,J,IAT)+DCDS(2,J)                   DL0397
   10       DCDS(3,J)=FACTOR*DATAR(3,J,IAT)+DCDS(3,J)                   DL0397
         ENDIF                                                          DL0397
   20    CONTINUE                                                       DJG0995
      ELSE
         DO 50 IAT=1,NUMAT                                              GDH0696
C         ICNT1=ITYPE(IAT)
C         IF (ICNT1.NE.0) THEN                                           GDH0696
C            FACTOR=1D-3*(SRFACI(IAT)+SRFN50(ICNT1))                     DL0397
C            SRFACT(IAT)=FACTOR*ATAR(IAT)+HB(IAT)                        DL0397
C            SURFCT=SURFCT+SRFACT(IAT)
C            IF (LGR) THEN                                               DL0397
C               DO 30 J=1,NUMAT                                          DL0397
C               DCDS(1,J)=FACTOR*DATAR(1,J,IAT)+DCDS(1,J)                DL0397
C               DCDS(2,J)=FACTOR*DATAR(2,J,IAT)+DCDS(2,J)                DL0397
C   30          DCDS(3,J)=FACTOR*DATAR(3,J,IAT)+DCDS(3,J)                DL0397
C            ENDIF                                                       DL0397
C         ELSE
            FACTOR=1D-3*SRFACI(IAT)                                     DL0397
            SRFACT(IAT)=FACTOR*ATAR(IAT)+HB(IAT)                        DL0397
            SURFCT=SURFCT+SRFACT(IAT)                                   DL0397
            IF (LGR) THEN                                               DL0397
               DO 40 J=1,NUMAT                                          DL0397
               DCDS(1,J)=FACTOR*DATAR(1,J,IAT)+DCDS(1,J)                DL0397
               DCDS(2,J)=FACTOR*DATAR(2,J,IAT)+DCDS(2,J)                DL0397
   40          DCDS(3,J)=FACTOR*DATAR(3,J,IAT)+DCDS(3,J)                DL0397
            ENDIF                                                       DL0397
C         ENDIF                                                          GDH0696
   50    CONTINUE                                                       DJG0995
      ENDIF
C
C     ADD THE LARGE RADIUS TERM TO SURFCT, THIS IS THE CDS ENERGY.
C
      IF (IRAD.EQ.6.OR.IRAD.EQ.7.OR.IRAD.EQ.8.OR.IRAD.EQ.21.OR          GDH0997
     .    .NUMSLV.EQ.20) THEN                                           GDH0997
         SURFCT=SURFCT+ATLGAR*HEXLGS                                    DJG0195
         IF (LGR) CALL SAXPY (3*NUMAT,HEXLGS,DATLGR,1,DCDS,1)           DL0397
      ENDIF                                                             DJG1094
      END
