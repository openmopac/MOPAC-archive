      SUBROUTINE CONVCK(TOLERG,FFNCT1,FFNCT2,FUNCT,NVAR,ICONFG)         GDH1195
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)                                GDH1195
      INCLUDE 'SIZES.i'                                                 GDH1195
      INCLUDE 'KEYS.i'                                                  GDH0696
C***********************************************************************
C
C    THIS CHECKS TO SEE IF THE GEOMETRY IS CONVERGED.  TO BE CONVERGED
C    THE FOLLOWING CRITERIA MUST BE MET
C
C    1)  THE LARGEST COMPONENT OF THE GRADIENT MUST BE < TOLERG
C    2)  THE LAST STEP MUST HAVE CHANGED LESS THAN 0.1 kcal/mol IN
C        ENERGY
C
C    TOLERG = GRADIENT TOLERENCE LIMIT
C    FFNCT1 = FORMER HEAT OF FORMATION
C    FFNCT2 = DIFFERENCE BETWEEN NEW AND FORMER HEAT OF FORMATION
C    FUNCT = NEW HEAT OF FORMATION
C    NVAR = NUMBER OF VARIABLES (DEGREES OF FREEDOM)
C    ICONFG = CONVERGENCE FLAG
C
C    IF BOTH THESE CRITERIA ARE MET, THEN ICONFG IS SET TO 0 AND
C    JCONV IS SET TO 1.
C
C***********************************************************************
      COMMON /GRADNT/ GRAD(MAXPAR),GNFINA                               GDH1195
      COMMON /ONESCM/ ICONTR(100)                                       GDH1195
      COMMON /CONVCM/ JCONV                                             GDH1195
      IF (ICONTR(62).EQ.0) THEN                                         GDH1195
         JCONV=0                                                        GDH1195
         ICONTR(62)=1                                                   GDH1195
      ENDIF                                                             GDH1195
      JCONFG=0                                                          GDH1195
      ICONFG=1                                                          GDH1195
      DO 57 I=1,NVAR                                                    GDH1195
57      IF (ABS(GRAD(I)).GE.TOLERG) JCONFG=1                            GDH1195
      FFNCT2=FFNCT1-FUNCT                                               GDH1195
      IF(FFNCT1.EQ.0.D0) FFNCT2=1.D0                                    GDH1195
      FFNCT1=FUNCT                                                      GDH1195
      IF (JCONFG.EQ.0.AND.ABS(FFNCT2).LT.0.1D0) THEN                    GDH0496
         ICONFG=0                                                       GDH1195
         JCONV=1                                                        GDH1195
      ENDIF                                                             GDH1195
      RETURN                                                            GDH1195
      END                                                               GDH1195
