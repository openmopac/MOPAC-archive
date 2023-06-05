C  This include file is for the following common blocks:
C  /CORGEN/  :   For constants in new general core-core expression.
C  /BTPAIR/  :   For pairwise beta parameters and related in resonance integrals.
C

      INTEGER          MAXELM, NUMTYP, MAXELMSQ, KPAIRSZ

      parameter (MAXELM=16,NUMTYP=2)                                    LF
      parameter (MAXELMSQ=MAXELM**2, KPAIRSZ=MAXELM**2*NUMTYP**2)       LF0911

      COMMON /BTPAIR/ PRBETA(MAXELM,MAXELM,NUMTYP,NUMTYP),              LF0911
     &                KPAIR (MAXELM,MAXELM,NUMTYP,NUMTYP)               LF0911

      COMMON /CORGEN/ C0AB(MAXELM,MAXELM),                              LF0211
     &                C1AB(MAXELM,MAXELM),                              LF0211
     &                C2AB(MAXELM,MAXELM),                              LF0211
     &                PRALP(MAXELM,MAXELM),                             LF0211
     &                C3AB(MAXELM,MAXELM),                              LF0211
     &                C3RPWR(MAXELM,MAXELM),                            LF0211
     &                PR3ALP(MAXELM,MAXELM)                             LF0211

      DOUBLE PRECISION PRBETA, KPAIR
      DOUBLE PRECISION C0AB, C1AB, C2AB, PRALP
      DOUBLE PRECISION C3AB, C3RPWR, PR3ALP

      
C
C       "MOD7" KEYWORD PARAMETER ARRAYS:
C     PRBETA  = Pairwise B values (replacing means of monoatomic beta parameters)
C     KPAIR   = Pairwise constants inside the new exponential factor
C
C       "MOD5" KEYWORD PARAMETER ARRAYS:
C     C0AB    = Pairwise coefficients for new pairwise alpha parameters exp(-alpha,AB*Rij)
C     C1AB    = Pairwise coefficients for old term exp(-alpha,A*Rij)
C     C2AB    = Pairwise coefficients for old term exp(-alpha,B*Rij) 
C     PRALP   = New pairwise alpha parameters, alpha,AB in C0AB description above
C     C3AB    = Pairwise coefficients for new pairwise alpha parameters in Rij**(C3RPWR,AB)*exp(-PR3ALP,AB*Rij)
C     C3RPWR  = See C3AB description.
C     PR3ALP  = See C3AB description.
C
C
C
C

