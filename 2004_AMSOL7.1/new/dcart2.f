      SUBROUTINE DCART2 (COORD,DXYZ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      DIMENSION COORD(3,*),DXYZ(3,*)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
     4       /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),
     5                QEFF(NUMATM),DRVPOL(MPACK)
     6       /SOLVCM/ CELEID,SLVRAD,SLVRD2,CSSIGM
     7       /SCRAH1/ AMAT(NPACK)                                       DL0397
     8               ,FBWO(MPACK/2+MPACK/2+1+9*MAXPAR-NPACK)            DL0397
     9               ,NBO(3),IDUM                                       DL0397
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL0397
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL0397
     2               ,DATLGR(3,NUMATM)                                  DL0397
     3               ,LGR,FLAGRD                                        DL0397
                      LOGICAL LGR,FLAGRD                                DL0397
C ---------------------------------------------------------------------+
C     CARTESIAN DERIVATIVES OF SOLVATION CONTRIBUTION FROM SCRF MODELS:
C     THAT IS SCF (RHF OR UHF) EXACTLY VARIATIONAL VS DENSITY MATRICES.
C  ON INPUT
C     COORD(3,NUMAT)     : CARTESIAN COORDINATES (ANGSTROMS).
C     DXYZ(3,NUMAT)      : GAZ PHASE FORMALISM DERIVATIVES (kcal/mol).
C     QEFF(NUMAT)        : NET ATOMIC CHARGES (SCF LEVEL),
C                          MULLIKEN OR CM1.
C     FGB                : CELCOE/Still's FACTORS  (eV).
C     DCDS(3,NUMAT)      : ANALYTICAL DERIVATIVES OF CDS (kcal/mol).
C     DFGB(NPACK,3,NUMAT): ANALYTICAL DERIVATIVES OF FGB (eV):
C                          DFGB(kl,i,j) = dFGB(kl)/dCOORD(i,j),
C                          kl = CANONICAL INDEX OF ATOMIC PAIR (k,l).
C  ON OUTPUT
C     DXYZ(3,NUMAT)      : FINAL CARTESIAN DERIVATIVES (kcal/mol).
C
C     CREATED BY DL, MARCH 1997.
C     NOTE... /SCRAH1/ USED AS LOCAL STORAGE AS DXYZ STORED IN /SCRCHR/
C ---------------------------------------------------------------------+
C                                                                       DL0397
C     CAVITY/DISPERSION ENERGY = SURFCT, DEPENDS ONLY FROM GEOMETRY;    DL0397
C     DERIVATIVES ALREADY STORED IN DCDS (kcal/mol).                    DL0397
C                                                                       DL0397
C     POLARISATION ENERGY EnP = -0.5 QEFF'.FGB.QEFF;                    DL0397
C     DERIVATIVE dEnP/dx = -0.5 QEFF'.dFGB/dx.QEFF                      DL0397
C                                                                       DL0397
C     SET AMAT(I,J) = -(23.061/2) QEFF(I).QEFF(J),                      DL0397
C     SO THAT dEnP/dx (kcal/mol) = Trace (AMAT.dFGB/dx).                DL0397
C                                                                       DL0397
      FACTOR=-23.061D0                                                  DL0397
      L=0                                                               DL0397
      DO 11 I=1,NUMAT                                                   DL0397
      DO 10 J=1,I-1                                                     DL0397
      L=L+1                                                             DL0397
   10 AMAT(L)=FACTOR*QEFF(I)*QEFF(J)                                    DL0397
      L=L+1                                                             DL0397
   11 AMAT(L)=0.5D0*FACTOR*QEFF(I)**2                                   DL0397
C                                                                       DL0397
C     ADD SOLVATION ENERGY DERIVATIVE TO DXYZ(3,NUMAT).                 DL0397
C                                                                       DL0397
      DO 20 J=1,NUMAT                                                   DL0397
      DXYZ(1,J)=DXYZ(1,J)+DCDS(1,J)+DOT(AMAT,DFGB(1,1,J),L)             DL0397
      DXYZ(2,J)=DXYZ(2,J)+DCDS(2,J)+DOT(AMAT,DFGB(1,2,J),L)             DL0397
      DXYZ(3,J)=DXYZ(3,J)+DCDS(3,J)+DOT(AMAT,DFGB(1,3,J),L)             DL0397
   20 CONTINUE                                                          DL0397
      END
