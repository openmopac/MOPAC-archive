      SUBROUTINE BORNPL (NITER,LBORN,LGCDS,LGRAD)                       DL0397
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'KEYS.i'                                                  DJG0995
      PARAMETER (MSIZE=NUMATM*(NUMATM-1)/2)
C ---------------------------------------------------------------------+
C     MAIN ROUTINE FOR SOLVATION MODELS SMx.y IN ANY KIND.
C     (SEE ALSO ROUTINE "BRNPL2")
C
C  ON INPUT
C     NITER  = SCF ITERATION COUNTER
C     LBORN  = TRUE TO CALCULATE BORN POLARIZATION TERMS
C     LGCDS  = TRUE TO CALCULATE G-CDS TERMS
C     LGRAD  = TRUE TO CALCULATE ANALYTICAL MOLECULAR GRADIENTS.
C
C  ON OUTPUT
C     QEFF2  = EFFECTIVE NET ATOMIC CHARGES (MULLIKEN OR CM1 OR EXTCM)
C              OR SM5.05 CHARGES
C     FGB    = POLARIZATION MATRIX CELCOE/STILL'S FACTORS.
C     BP     = FGB.QEFF, CONTRIBUTION TO FOCK MATRIX (MULLIKEN CHARGES)
C     DRVPOL = CONTRIBUTION TO FOCK MATRIX (CM1 CHARGES)
C     CCT1   = SOLVATION ENERGY NOT COMING FROM FOCK MATRIX IN "ITER"
C
C     CONVERTED TO DOUBLE PRECISION 03/16/92 BY GCL
C     NEW ARCHITECTURE 09/95                 BY DJG
C     ANALYTICAL GRADIENTS 03/97             BY DL
C ---------------------------------------------------------------------+
      LOGICAL LGRAD,LEXTCM,FLGEXT                                       DL0397
      LOGICAL LBORN,LGCDS,TCHOSE,TAREA                                  DJG0995
      COMMON /SMOOTH/ RAL(NUMATM)                                       GDH1093
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /HBORDS/ HBORD(NUMATM),HB(NUMATM)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),          DJG0994
     1                QEFF2(NUMATM),DRVPOL(MPACK)                       DJG0994
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),               DJG0195
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)           DJG0195
      COMMON /GEPCOM/ ISORTS,NDIV1,NDIV2,IPSUMC,IPSUMS                  GDH1192
      COMMON /RADCOM/ SRFACI(NUMATM)
      COMMON /CYCLCM/ PCMIN, NGEOM, NSOLPR, NSCFS, JPCNT                GDH0893
      COMMON /AREACM/ NOPTI, TAREA, NINTEG                              GDH0893
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /CMCOM/  ECMCG(NUMATM)                                     GDH1293
      COMMON /HBONDA/ HBONDO(100),CBONDO,COBND5,CNBNDO,CBOND5,          GDH1196
     .                OOBNDO,ONBNDO,SSBNDO,OPBND5,CLCBND,BRCBND,        GDH1196
     .                FCBNDO,CNBOC2,TNCBC3,TNCBC2,HOHBND,HNNBND,        GDH1296
     .                SPBNDO,OSIBND                                     PDW0601
      COMMON /HEXSTM/ HRFN4(100),CRFNH4,CNRFN4,CORFN5,                  GDH0797
     X                CRFNH5,OORFN5,ONRFN4,SSRFN5,OPRFN5,               GDH0797
     1                FCRFN,CLCRFN,BRCRFN,CNSTC2,TNCSC3,TNCSC2,         GDH0797
     2                HOHST1,HNNST1, SPRFN5,OSIBD5                      PDW0901
      COMMON /SOLVCM/ CELEID, SLVRAD, SLVRD2, CSSIGM                    DJG1094
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),   DJG0995
     1                HBCORC(100),QKNOT(100),QKONE(100)                 DJG0995
      COMMON /HBODY2/ RG(3,MSIZE),BJACOB(MAXPAR**2),COORD(3,NUMATM)
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL0397
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL0397
     2               ,DATLGR(3,NUMATM)                                  DL0397
     3               ,LGR,FLAGRD                                        DL0397
                      LOGICAL LGR,FLAGRD                                DL0397
      SAVE
      DATA ZERO /0D0/                                                   DL0397
C
C     DECIDE ON AVAILABILITY FOR ANALYTICAL GRADIENT CALCULATION:       DL0397
C
      FLAGRD = TAREA                                                    DL0397
     .    .AND.NINTEG.GT.0                                              DL0397
     .    .AND.(IRAD.EQ.4.OR.IRAD.EQ.5.OR.IRAD.EQ.7.OR.IRAD.EQ.8        GDH0997
     .          .OR.IRAD.EQ.9.OR.IRAD.EQ.10)                            GDH0597
     .    .AND.IEXTCM.EQ.0                                              DL0397
C     DECIDE ON ACTUAL CALCULATION FOR ANALYTICAL GRADIENT:             DL0397
      LGR=     LGRAD.AND.NITER.EQ.1.AND.FLAGRD                          DL0397
C
C     EFFECTIVE ATOMIC NET CHARGES
C     FLGEXT=.FALSE. IGNORE EXTCM CHARGES                               DJG0995
C
      IQDEP=0                                                           GDH1296
      IQDEP=ISM1+ISM1A+ISM2+ISM21+ISM22+ISM3+ISM31+ISM23+ISM4           DJG1296
      IF (IEXTCM.NE.0) THEN                                             DJG0995
         FLGEXT=.TRUE.                                                  DJG0995
         CALL GETCHG(QEFF2,FLGEXT)                                      DJG0995
      ELSE IF (ISM505.GT.0) THEN                                        GDH0597
         DO 147 I=1,NUMAT                                               GDH0197
         IF (ITYPE(I).GT.0) THEN                                        GDH0197
            IF(NAT(I).EQ.1) THEN                                        GDH0197
               ITPER=ITYPE(I)                                           GDH0197
            ELSE IF(NAT(I).EQ.8.OR.NAT(I).EQ.16) THEN                   GDH0197
               ITPER=-ITYPE(I)                                          GDH0197
            ENDIF                                                       GDH0197
         QEFF2(I)=1.0D0/ITPER                                           GDH0197
         ELSE                                                           GDH0197
         QEFF2(I)=0.0D0                                                 GDH0197
         ENDIF                                                          GDH0197
147      CONTINUE                                                       GDH0197
      ELSE                                                              GDH0597
         FLGEXT=.FALSE.                                                 DJG0995
         CALL GETCHG(QEFF2,FLGEXT)                                      DJG0995
      ENDIF                                                             DJG0995
      CALL SCOPY (NUMAT,ZERO,0,BP,1)
      TCHOSE=.FALSE.                                                    GDH1293
      IF (LBORN) THEN                                                   GDH1293
C
C        SECTION BORN POLARIZATION TERMS, MAY NOT DEPEND IN QEFF.
C
         IF (I1SCF.NE.0) THEN                                           DJG0995
            IF (NITER.NE.1.AND.(DBLE(NITER)/4.0D0).NE.                  GDH1293
     *      INT((DBLE(NITER))/4.0D0)) GOTO 10                           GDH1293
         ELSE                                                           GDH1293
            IF (SQRT(DBLE(NITER)).NE.INT(SQRT(DBLE(NITER)))) GOTO 10    GDH1293
         ENDIF                                                          GDH1293
         IF (NITER.EQ.1) THEN                                           GDH1293
             CALL SCOPY (NUMAT,ZERO,0,RAL,1)
             CALL DBRNPL(COORD,GEO)                                     GDH1293
         ENDIF                                                          GDH1293
         IF (NITER.EQ.1.OR.IQDEP.EQ.1) THEN                             GDH1296
            NSOLPR=NSOLPR+1                                             GDH1293
            IF (NINTEG.GT.0) THEN                                       GDH1296
               CALL SMX1(COORD,NITER,.TRUE.)                            DL0397
            ELSE                                                        GDH1293
               CALL SRFCTY (COORD,.TRUE.,.FALSE.)                       DL0397
            ENDIF                                                       GDH1293
         ENDIF                                                          GDH1296
         TCHOSE=.TRUE.                                                  GDH1293
      ENDIF                                                             GDH1293
C
   10 IF (.NOT.TCHOSE.AND.NOPTI.EQ.1) GOTO 20                           DJG0995
C
      IF (LGCDS) THEN                                                   GDH1293
C
C        SECTION G-CDS TERMS, MAY NOT DEPEND IN QEFF.
C
         IF (NITER.EQ.1.OR.IQDEP.EQ.1) THEN                             GDH1296
            IF (NINTEG.GT.0) THEN                                       DJG0995
               CALL SMX1(COORD,NITER,.FALSE.)                           DL0397
            ELSE                                                        GDH1293
               CALL SRFCTY(COORD,.FALSE.,.FALSE.)                       GDH1293
            ENDIF                                                       GDH1293
C           CALCULATE SPECIAL SURFACE TENSION TERMS                     DJG0995
            CALL SRFCAL                                                 DL0397
C           SUM SURFACE TENSION TERMS INTO CDS ENERGY                   DJG0995
            CALL CDSCAL                                                 DJG0995
         ENDIF                                                          GDH1296
      ENDIF
C
   20 CONTINUE
C                                                                       DJG0995
C     CALCULATE BORN POLARIZATION TERM & CCT1 AS FUNCTIONS OF QEFF      DJG0995
C                                                                       DJG0995
      CALL CALCBP (QEFF2)                                               DJG0995
      END
