      SUBROUTINE PMSIX (NI,NJ,RIJ,GAM,ENUC,GTERM,*)                     CSTP
C***********************************************************************
C
C   PMSIX CALCULATES THE PM6 HAMILTONIAN CORE-CORE INTERACTION.
C   CREATED OCTOBER 2009 BY LUKE FIEDLER.
C
C   ON INPUT  NI     = ATOMIC NUMBER OF FIRST ATOM.
C             NJ     = ATOMIC NUMBER OF SECOND ATOM.
C             RIJ    = DISTANCE BETWEEN NUCLEI. (angstroms)
C             GAM    = VALUE OF TWO-ELECTRON INTEGRAL, (SaSa|SbSb). (eV)
C
C   ON OUTPUT ENUC   = NUCLEAR-NUCLEAR REPULSION TERM. (eV)
C             GTERM  = GAUSSIAN CONTRIBUTION TO NUCLEAR REPULSION. (eV)
C
C
C *** THIS SUBROUTINE CALCULATES THE PM6 CORE-CORE INTERACTION ENERGY
C     AND RETURNS IT AS ENUC TO THE ROTATE SUBROUTINE.
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      COMMON /CORE  / VATNUM(120)
     &       /IDEAS / FN1(120,10),FN2(120,10),FN3(120,10)
     &       /MULTIP/ DD(120),QQ(120),AM(120),AD(120),AQ(120)
      COMMON /PM6BLO/ USSPM6(120), UPPPM6(120), UDDPM6(120),ZSPM6(120),
     &ZPPM6(120), ZDPM6(120), BETASX(120), BETAPX(120), BETADX(120),   
     &GSSPM6(120),GSPPM6(120),GPPPM6(120),GP2PM6(120),HSPPM6(120),     
     &EISOLX(120), DDPM6(120), QQPM6(120), AMPM6(120), ADPM6(120),     
     &AQPM6(120), GAUSX1(120,10), GAUSX2(120,10), GAUSX3(120,10),      
     &ALPPM6(18,18), XPM6(18,18), ZSNPM6(18), ZPNPM6(18), ZDNPM6(18)   
      include 'method.f'
      PARAMETER (THIRD=1.D0/3.D0)

C     First calculate all of the AM1-type gaussian terms for the pair.
      GTERM=0.D0
      DO 10 IG=1,10
         IF(ABS(FN1(NI,IG)).GT.0.D0) THEN
            AX = FN2(NI,IG)*(RIJ-FN3(NI,IG))**2
            IF(AX .LE. 25.D0)
     &       GTERM=GTERM+VATNUM(NI)*VATNUM(NJ)/RIJ*FN1(NI,IG)*EXP(-AX)
         ENDIF
         IF(ABS(FN1(NJ,IG)).GT.0.D0) THEN
            AX = FN2(NJ,IG)*(RIJ-FN3(NJ,IG))**2
            IF(AX .LE. 25.D0) 
     &       GTERM=GTERM+VATNUM(NI)*VATNUM(NJ)/RIJ*FN1(NJ,IG)*EXP(-AX)
         ENDIF
10    CONTINUE

C     Because the arrays of ALPPM6 and XPM6 represent symmetric matrices that
C     only store data as a lower triangular matrix, values for ALPPM6(NI,NJ) should
C     equal ALPPM6(NJ,NI), but are not stored when the first index is less than the
C     second index.  Also, all values of XPM6 given in the original PM6 paper
C     must be doubled in order to agree with the PM6 implementation by the author
C     in his software package.
      IF (NI.GT.NJ) THEN
        ALPHA=ALPPM6(NI,NJ)
        X=XPM6(NI,NJ)*2.d0
      ELSE
        ALPHA=ALPPM6(NJ,NI)
        X=XPM6(NJ,NI)*2.d0
      ENDIF


C     Find the unpolarizable core correction for atoms at very short separation.
C     Note that here the atomic numbers are used in the formula.
      FAB=1.d-8*(((NI)**THIRD+(NJ)**THIRD)/RIJ)**12


C     Get prefactor for core-core energy expressions: Zi.Zj.(si si|sj sj)
C     Note that here the valence atomic numbers are used (# of valence electrons) in the formula.
      PRE=VATNUM(NI)*VATNUM(NJ)*GAM



C     Now use the Voityuk approximation for general and special cases.
      ENUC=0.D0
C     (i) The O-H and N-H cases
      NT=NI+NJ
      IF ((NT.EQ.8.OR.NT.EQ.9).AND.(NI.EQ.1.OR.NJ.EQ.1)) THEN
        ENUC=PRE*(1.D0+X*EXP(-ALPHA*RIJ**2))


C     (ii) The C-C case      
      ELSEIF (NI.EQ.6.AND.NJ.EQ.6) THEN
        ENUC=PRE*(1.D0 + X*EXP(-ALPHA*
     &       (RIJ+0.0003D0*RIJ**6))+9.28D0*EXP(-5.98D0*RIJ))
        IF (LPM6G9.AND.(RIJ.LE.1.28D0)) ENUC=ENUC+2.D0*6.D0/23.061D0
 
C     (iii) The Si-O case
      ELSEIF (NT.EQ.22.AND.(NI.EQ.14.OR.NJ.EQ.14)) THEN
        ENUC=PRE*(1.D0+X*EXP(-ALPHA*
     &       (RIJ+0.0003D0*RIJ**6))-0.0007D0*EXP(-(RIJ-2.9D0)**2))


C     (iv) The C-H case: note this is not in the original PM6 paper but this is
C                        how PM6 is implemented in MOPAC2009 (treat C-H like 
C                        N-H and O-H atom pairs).
      ELSEIF ((NT.EQ.7).AND.((NI.EQ.1).OR.(NJ.EQ.1))) THEN
        ENUC=PRE*(1.D0+X*EXP(-ALPHA*RIJ**2))


C     (v) General case for all other pairs 
      ELSE
        ENUC=PRE*(1.D0+X*EXP(-ALPHA*(RIJ+0.0003D0*RIJ**6)))

      ENDIF



C     Sum all energy contributions to get final ENUC.
      ENUC=ENUC+GTERM+FAB


      RETURN
 9999 RETURN 1                                                          CSTP
      END
