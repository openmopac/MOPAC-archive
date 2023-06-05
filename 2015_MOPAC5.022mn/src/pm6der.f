      SUBROUTINE PM6DER(NI,NJ,RIJ,IX,DEL1,GAM,DGAM,DENUC,*)             CSTP
C***********************************************************************
C
C   PMDER CALCULATES THE FIRST ANALYTICAL DERIVATIVE OF THE PM6 CORE-CORE
C     INTERACTION TERM (EXCEPT FOR THE AM1-TYPE GAUSSIAN TERMS' DERIVATIVES
C     AND THE FIRST DERIVATIVE CONTRIBUTIONS ARISING FROM THE NITROGEN
C     PYRAF2ALIZATION TERMS).
C   CREATED JANUARY 2010 BY LUKE FIEDLER.
C
C   ON INPUT  NI     = ATOMIC NUMBER OF FIRST ATOM.
C             NJ     = ATOMIC NUMBER OF SECOND ATOM.
C             RIJ    = DISTANCE BETWEEN NUCLEI. (angstroms)
C             IX     = COORDINATE DIFFERENTIATING WITH RESPECT TO (1=X,2=Y,3=Z).
C             DEL1   = DIFFERENCE IN NUCLEAR COORDINATES FOR COORDINATE
C                      IX. (angstroms)
C             GAM    = VALUE OF TWO-ELECTRON INTEGRAL,(SaSa|SbSb). (eV)
C             DGAM   = FIRST DERIVATIVE OF TWO-ELECTRON INTEGRAL. (eV)
C
C   ON OUTPUT DENUC  = DERIVATIVE OF THE NUCLEAR-NUCLEAR REPULSION TERM
C                      (OMITTING GAUSSIANS' CONTRIBUTION).  (eV/angstroms)
C
C *** THIS SUBROUTINE CALCULATES THE PM6 CORE-CORE INTERACTION ENERGY
C     FIRST DERIVATIVE (WITHOUT THE AM1-STYLE GAUSSIAN TERMS)
C     AND RETURNS IT AS DENUC TO THE ROTATE SUBROUTINE.
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      COMMON /CORE  / VATNUM(120)
      COMMON /PM6BLO/ USSPM6(120), UPPPM6(120), UDDPM6(120),ZSPM6(120),
     &ZPPM6(120), ZDPM6(120), BETASX(120), BETAPX(120), BETADX(120),   
     &GSSPM6(120),GSPPM6(120),GPPPM6(120),GP2PM6(120),HSPPM6(120),     
     &EISOLX(120), DDPM6(120), QQPM6(120), AMPM6(120), ADPM6(120),     
     &AQPM6(120), GAUSX1(120,10), GAUSX2(120,10), GAUSX3(120,10),      
     &ALPPM6(18,18), XPM6(18,18), ZSNPM6(18), ZPNPM6(18), ZDNPM6(18)   
      include 'method.f'
      PARAMETER (THIRD=1.D0/3.D0)


C     Because the arrays of ALPPM6 and XPM6 represent symmetric matrices that
C     only store data as a lower triangular matrix, values for ALPPM6(NI,NJ) should
C     equal ALPPM6(NJ,NI), but are not stored when the first index is less than the
C     second index.  Also, all values of XPM6 given in the original PM6 paper
C     must be doubled in order to agree with the PM6 implementation by the author
C     in his software package, MOPAC2009.
      IF (NI.GT.NJ) THEN
        ALPHA=ALPPM6(NI,NJ)
        X=XPM6(NI,NJ)*2.d0
      ELSE
        ALPHA=ALPPM6(NJ,NI)
        X=XPM6(NJ,NI)*2.d0
      ENDIF



C     Find the derivative of the unpolarizable core correction for atoms.
C     Note that here the atomic numbers are used in the formula (not the number
C     of valence electrons).
      DFAB= (-1.2d-7) * (((NI)**THIRD+(NJ)**THIRD)**12) *
     &      (DEL1) / (RIJ**14)
    
    
C     Get prefactor for core-core energy expressions: Zi.Zj.(si si|sj sj)
C     Also, get its first energy derivative with respect to the changing nuclear
C     coordinate.
C     Note that here the valence atomic numbers are used (# of valence electrons).
      PRE=VATNUM(NI)*VATNUM(NJ)*GAM
      DPRE=VATNUM(NI)*VATNUM(NJ)*DGAM
    
    
C     Note that because Rij**2=(Rx**2+Ry**2+Rz**2)**(1/2), then 
C                      d(Rij)/d(Ri)=Ri/Rij = DEL1/RIJ.
      F1 = DEL1/RIJ


C     Now use the Voityuk approximation for general and special cases.
      DENUC=0.D0


C     (i) The O-H and N-H cases
      NT=NI+NJ
      IF ((NT.EQ.8.OR.NT.EQ.9).AND.(NI.EQ.1.OR.NJ.EQ.1)) THEN
        POST=1.D0+X*EXP(-ALPHA*RIJ**2)
        DPOST=(POST-1.d0)*((-2.d0)*(ALPHA)*RIJ)*F1
        DENUC=DPRE*POST+PRE*DPOST


C     (ii) The C-C case      
      ELSEIF (NI.EQ.6.AND.NJ.EQ.6) THEN
        F21 = X*EXP(-ALPHA*(RIJ+0.0003D0*RIJ**6))
        DF21 = F21*(-ALPHA)*(1.D0+0.0003D0*6.D0*RIJ**5)*F1
        F22 = 9.28D0*EXP(-5.98D0*RIJ)
        DF22 = F22*(-5.98D0)*F1
        POST = 1.D0 + F21 + F22
        DPOST = DF21 + DF22
        DENUC = DPRE*POST+PRE*DPOST


C     (iii) The Si-O case
      ELSEIF (NT.EQ.22.AND.(NI.EQ.14.OR.NJ.EQ.14)) THEN
        F21 = X*EXP(-ALPHA*(RIJ+0.0003D0*RIJ**6))
        DF21 = F21*(-ALPHA)*(1.D0+0.0003D0*6.D0*RIJ**5)*F1
        F22 = -0.0007D0*EXP(-(RIJ-2.9D0)**2)
        DF22 = F22*(-2.D0)*(RIJ-2.9D0)*F1
        POST = 1.D0 + F21 + F22
        DPOST = DF21 + DF22
        DENUC = DPRE*POST+PRE*DPOST




C     (iv) The C-H case: note this is not in the original PM6 paper but this is
C                        how PM6 is implemented in MOPAC2009 (treat C-H like 
C                        N-H and O-H atom pairs).
      ELSEIF ((NT.EQ.7).AND.((NI.EQ.1).OR.(NJ.EQ.1))) THEN
        POST=1.D0+X*EXP(-ALPHA*RIJ**2)
        DPOST=(POST-1.d0)*((-2.d0)*(ALPHA)*RIJ)*F1
        DENUC=DPRE*POST+PRE*DPOST




C     (v) General case for all other pairs 
      ELSE
        F2 = X*EXP(-ALPHA*(RIJ+0.0003D0*RIJ**6))
        DF2 = F2*(-ALPHA)*(1.D0+0.0003D0*6.D0*RIJ**5)*F1
        POST = 1.D0 + F2
        DPOST = DF2
        DENUC = DPRE*POST+PRE*DPOST
      ENDIF



C     Add together all first energy derivative contributions.
      DENUC=DENUC+DFAB
      
      RETURN
 9999 RETURN 1                                                          CSTP
      END
