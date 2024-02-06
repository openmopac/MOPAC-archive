      SUBROUTINE DBRNPL(COORD,GEO)                                      GDH1293
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               GDH1293
        INCLUDE 'SIZES.i'                                               GDH1293
        INCLUDE 'SIZES2.i'                                              GDH1293
        INCLUDE 'FFILES.i'                                              DJG0196
        INCLUDE 'KEYS.i'                                                GDH0197
      LOGICAL DONE
C                                                                       DJG0996
C     THIS SUBROUTINE CALCULATES INTERATOMIC DISTANCES USED FOR BORNPL  DJG0996
C     AND ASSOCIATED UNIT VECTORS USED IN AREAL & DERIVATIVES.          DL0397
C                                                                       DJG0996
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /ARACOM/ R(NPACK),R2(NPACK),ISAVE,DONE
     1               ,RAC(NUMATM),RAC2(NUMATM),DRAC(NUMATM,NUMATM)      DL0397
     2               ,RAS(NUMATM),RALG(NUMATM)
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM),RMAX(NUMATM)                  GDH1293
     1               ,URLIO(3,NUMATM,NUMATM)                            DL0397
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),          GDH0197
     1                QEFF2(NUMATM),DRVPOL(MPACK)                       GDH0197
      COMMON /QESTR/  SCOORD(3,NUMATM), ISPEC                           GDH0197
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),               GDH0297
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)           GDH0297
      DIMENSION COORD(3,*),GEO(3,*)                                     GDH1293
      CALL GMETRY(GEO,COORD)
      IF (ISM50.NE.0) THEN                                              GDH0197
               XCOR=0.D0                                                GDH0197
               YCOR=0.D0                                                GDH0197
               ZCOR=0.D0                                                GDH0197
               DO 147 I=1,NUMAT                                         GDH0197
               SCOORD(1,I)=COORD(1,I)                                   GDH0197
               SCOORD(2,I)=COORD(2,I)                                   GDH0197
               SCOORD(3,I)=COORD(3,I)                                   GDH0197
147            CONTINUE                                                 GDH0197
      ENDIF                                                             GDH0197
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      R(1)=0.D0                                                         GDH1293
      R2(1)=0.D0                                                        GDH1293
      RLIO(1,1)=0.D0                                                    GDH1293
      URLIO(1,1,1)=0.D0                                                 DL0397
      URLIO(2,1,1)=0.D0                                                 DL0397
      URLIO(3,1,1)=0.D0                                                 DL0397
      IJ=1                                                              GDH1293
      DO 20 I=2,NUMAT                                                   GDH1293
        DO 10 J=1,I-1                                                   GDH1293
          IJ=IJ+1                                                       GDH1293
          XX1=COORD(1,I)-COORD(1,J)                                     GDH1293
          XX2=COORD(2,I)-COORD(2,J)                                     GDH1293
          XX3=COORD(3,I)-COORD(3,J)                                     GDH1293
          DIST=XX1*XX1+XX2*XX2+XX3*XX3                                  GDH1293
          R2(IJ)=DIST                                                   GDH1293
          DISTS=SQRT(DIST)                                              GDH1293
          R(IJ)=DISTS                                                   GDH1293
          RLIO(I,J)=DISTS                                               GDH1293
          RLIO(J,I)=DISTS                                               GDH1293
          URLIO(1,J,I)=XX1/DISTS                                        DL0397
          URLIO(2,J,I)=XX2/DISTS                                        DL0397
          URLIO(3,J,I)=XX3/DISTS                                        DL0397
          URLIO(1,I,J)=-URLIO(1,J,I)                                    DL0397
          URLIO(2,I,J)=-URLIO(2,J,I)                                    DL0397
          URLIO(3,I,J)=-URLIO(3,J,I)                                    DL0397
   10   CONTINUE                                                        GDH1293
        IJ=IJ+1                                                         GDH1293
        R(IJ)=0.D0                                                      GDH1293
        R2(IJ)=0.D0                                                     GDH1293
        RLIO(I,I)=0.D0                                                  GDH1293
        URLIO(1,I,I)=0.D0                                               DL0397
        URLIO(2,I,I)=0.D0                                               DL0397
        URLIO(3,I,I)=0.D0                                               DL0397
   20 CONTINUE                                                          GDH0495
      END                                                               GDH1293
