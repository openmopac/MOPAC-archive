      SUBROUTINE PM6DE2 (I,IATOM1,IATOM2,IATOM3,DPYRX0,DPYRY0,DPYRZ0,
     &                   DPYRX1, DPYRY1, DPYRZ1,
     &                   DPYRX2, DPYRY2, DPYRZ2,
     &                   DPYRX3, DPYRY3, DPYRZ3,*)                      CSTP
C***********************************************************************
C
C   PM6DE2 CALCULATES THE FIRST DERIVATIVE OF THE PM6 SECONDARY AND 
C     TERTIARY AMINE PYRAMIDALIZATION CORRECTION FOR A GIVEN NITROGEN ATOM
C     AS WELL AS THE FIRST DERIVATIVES ARISING FROM THIS PYRAMIDALIZATION
C     TERM IN PM6 WITH RESPECT TO THE COORDINATES OF THE THREE NEAREST 
C     ATOMS TO THE NITROGEN.
C   WRITTEN BY LUKE FIEDLER, JANUARY 2010.
C
C   ON INPUT  I      = INDEX NUMBER OF NITROGEN ATOM.
C
C   ON OUTPUT IATOM1,
C             IATOM2,
C             IATOM3 = INDICES FOR THE THREE CLOSEST
C                        ATOMS TO THE NITROGEN (CLOSEST IS IATOM1)
C             DPYRX0,
C             DPYRY0,
C             DPYRZ0 = DERIVATIVES OF NITROGEN PYRAMIDALIZATION FOR
C                      THE NITROGEN ATOM WITH RESPECT TO ITS
C                      THREE NUCLEAR COORDINATES (eV/angstrom)
C             DPYRX1,
C             DPYRY1,
C             DPYRZ1,
C             DPYRX2,
C             DPYRY2,
C             DPYRZ2,
C             DPYRX3,
C             DPYRY3,
C             DPYRZ3 = CONTRIBUTIONS TO FIRST DERIVATIVES OF
C                      NUCLEAR ENERGY WITH RESPECT TO NUCLEAR COORDINATES
C                      OF THREE ATOMS NEAREST THE SECONDARY/TERTIARY
C                      NITROGEN (eV/angstrom)
C
C
C *** THIS SUBROUTINE MUST ONLY BE CALLED WHEN I IS THE INDEX OF A 
C     NITROGEN ATOM.  THIS SUBROUTINE WORKS IN CONJUNCTION WITH THE
C     SUBROUTINE PM6DPY FOR CALCULATING FIRST DERIVATIVES OF THE
C     NUCLEAR ENERGY WITH RESPECT TO THE NUCLEAR COORDINATES FOR
C     THE NITROGEN PYRAMIDALIZATION CORRECTION.
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
      IMPLICIT INTEGER (I-N)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /GEOM  / INTGEO(3,NUMATM)
      COMMON /PM6BLO/ USSPM6(120), UPPPM6(120), UDDPM6(120),ZSPM6(120),
     &ZPPM6(120), ZDPM6(120), BETASX(120), BETAPX(120), BETADX(120),   
     &GSSPM6(120),GSPPM6(120),GPPPM6(120),GP2PM6(120),HSPPM6(120),     
     &EISOLX(120), DDPM6(120), QQPM6(120), AMPM6(120), ADPM6(120),     
     &AQPM6(120), GAUSX1(120,10), GAUSX2(120,10), GAUSX3(120,10),      
     &ALPPM6(18,18), XPM6(18,18), ZSNPM6(18), ZPNPM6(18), ZDNPM6(18)   
      DIMENSION XIJ(3),DX(3),DY(3),DZ(3),GEO(3,NUMATM)
      PARAMETER (HALF=0.5D0)
      PARAMETER (THIRD=1.D0/3.D0)

C     Return with no correction if the atom is not nitrogen.
      IF (NAT(I).NE.7) RETURN

C     Get cartesian coordinates from common block's internal coordinates.
      CALL GMETRY(INTGEO,GEO,*9999)                                      CSTP(call)

C     *** START BY THE TYPICAL PM6 N PYRAMIDALIZATION CALCULATION ***

C     Reset energy and initialize variables.
      EPYR=0.D0
      NATOMS=0
      NHEAVY=0
      DPYRX0=0.D0
      DPYRY0=0.D0
      DPYRZ0=0.D0
      DPYRX1=0.D0
      DPYRY1=0.D0
      DPYRZ1=0.D0
      DPYRX2=0.D0
      DPYRY2=0.D0
      DPYRZ2=0.D0
      DPYRX3=0.D0
      DPYRY3=0.D0
      DPYRZ3=0.D0
      IATOM1=0
      IATOM2=0
      IATOM3=0
      DIST1=1.D5
      DIST2=1.D5
      DIST3=1.D5
      DO 10 K=1,3
        DX(K)=0.D0
        DY(K)=0.D0
        DZ(K)=0.D0
10    CONTINUE

C     Identify the three closest atoms to the given nitrogen.  Must be
C     within 2.50 angstroms for a bond to be chosen.
      DO 20 J=1,NUMAT
        IF (I.NE.J) THEN
          XIJ(1)=GEO(1,I)-GEO(1,J)
          XIJ(2)=GEO(2,I)-GEO(2,J)
          XIJ(3)=GEO(3,I)-GEO(3,J)
          RIJ=SQRT(XIJ(1)*XIJ(1)+XIJ(2)*XIJ(2)+XIJ(3)*XIJ(3))

          IF ((RIJ.LT.DIST3).AND.(RIJ.LE.2.5D0)) THEN
            NATOMS=NATOMS+1
            DIST3=RIJ
            IATOM3=J
            IF (RIJ.LT.DIST2) THEN
              DIST3=DIST2
              IATOM3=IATOM2
              DIST2=RIJ
              IATOM2=J
              IF (RIJ.LT.DIST1) THEN
                DIST2=DIST1
                IATOM2=IATOM1
                DIST1=RIJ
                IATOM1=J
              ENDIF
            ENDIF
          ENDIF

        ENDIF
20    CONTINUE

C     Only calculate if a secondary or tertiary amine, otherwise return.  Also
C     must have three atoms within 2.500 angstroms of the nitrogen.
      IF (NATOMS.LT.3) RETURN
      IF (NAT(IATOM1).NE.1) NHEAVY=NHEAVY+1
      IF (NAT(IATOM2).NE.1) NHEAVY=NHEAVY+1
      IF (NAT(IATOM3).NE.1) NHEAVY=NHEAVY+1
      IF (NHEAVY.LT.2) RETURN

C     Calculate the three bond angles to nitrogen.
      DX(1)=GEO(1,IATOM1)-GEO(1,I)
      DY(1)=GEO(2,IATOM1)-GEO(2,I)
      DZ(1)=GEO(3,IATOM1)-GEO(3,I)
      DX(2)=GEO(1,IATOM2)-GEO(1,I)
      DY(2)=GEO(2,IATOM2)-GEO(2,I)
      DZ(2)=GEO(3,IATOM2)-GEO(3,I)
      DX(3)=GEO(1,IATOM3)-GEO(1,I)
      DY(3)=GEO(2,IATOM3)-GEO(2,I)
      DZ(3)=GEO(3,IATOM3)-GEO(3,I)
      DOT12=DX(1)*DX(2)+DY(1)*DY(2)+DZ(1)*DZ(2)  
      DOT13=DX(1)*DX(3)+DY(1)*DY(3)+DZ(1)*DZ(3)  
      DOT23=DX(2)*DX(3)+DY(2)*DY(3)+DZ(2)*DZ(3)
      DIV12=DOT12/(DIST1*DIST2)
      DIV13=DOT13/(DIST1*DIST3)
      DIV23=DOT23/(DIST2*DIST3)
      ANG12=ACOS(DIV12)
      ANG13=ACOS(DIV13)
      ANG23=ACOS(DIV23)

C     Calculate the non-planarity and the PM6 pyramidalization correction.
      DNONPL=(2.D0*3.1415927D0)-ANG12-ANG13-ANG23
      EPYR=(-0.5D0/23.061D0)*EXP((-10.D0)*DNONPL)

C     *** NOW CALCULATE THE FIRST DERIVATIVES FOR THE NITROGEN ATOM AND ***
C     *** CONTRIBUTIONS FOR THE THREE ATOMS PERIPHERAL TO THE NITROGEN. ***

C     First calculate convenient factors that appear in every first derivative expression.
      DEN12=DIST1*DIST2
      DEN13=DIST1*DIST3
      DEN23=DIST2*DIST3
      PREF12= 1.D0/SQRT(1.D0-DIV12**2)/(DEN12*DEN12)
      PREF13= 1.D0/SQRT(1.D0-DIV13**2)/(DEN13*DEN13)
      PREF23= 1.D0/SQRT(1.D0-DIV23**2)/(DEN23*DEN23)

C     Take derivatives of phi1, phi2, and phi3 w.r.t. coordinates x0, y0, and z0.
C     (phi1, phi2, and phi3 correspond to variables ANG12, ANG13, and ANG23 respectively.)
      DA1X0=PREF12*( DEN12*(DX(2)+DX(1)) - 
     &           DOT12*( (DX(1)/DIST1*DIST2) + (DX(2)/DIST2*DIST1) ))
      DA2X0=PREF13*( DEN13*(DX(3)+DX(1)) - 
     &           DOT13*( (DX(1)/DIST1*DIST3) + (DX(3)/DIST3*DIST1) ))
      DA3X0=PREF23*( DEN23*(DX(3)+DX(2)) - 
     &           DOT23*( (DX(2)/DIST2*DIST3) + (DX(3)/DIST3*DIST2) ))

      DA1Y0=PREF12*( DEN12*(DY(2)+DY(1)) - 
     &           DOT12*( (DY(1)/DIST1*DIST2) + (DY(2)/DIST2*DIST1) ))
      DA2Y0=PREF13*( DEN13*(DY(3)+DY(1)) - 
     &           DOT13*( (DY(1)/DIST1*DIST3) + (DY(3)/DIST3*DIST1) ))
      DA3Y0=PREF23*( DEN23*(DY(3)+DY(2)) - 
     &           DOT23*( (DY(2)/DIST2*DIST3) + (DY(3)/DIST3*DIST2) ))

      DA1Z0=PREF12*( DEN12*(DZ(2)+DZ(1)) - 
     &           DOT12*( (DZ(1)/DIST1*DIST2) + (DZ(2)/DIST2*DIST1) ))
      DA2Z0=PREF13*( DEN13*(DZ(3)+DZ(1)) - 
     &           DOT13*( (DZ(1)/DIST1*DIST3) + (DZ(3)/DIST3*DIST1) ))
      DA3Z0=PREF23*( DEN23*(DZ(3)+DZ(2)) - 
     &           DOT23*( (DZ(2)/DIST2*DIST3) + (DZ(3)/DIST3*DIST2) ))

C     Now find first derivative of EPYR w.r.t. coordinates x0, y0, and z0.
C     First derivatives for nitrogen atom can be found by these lines:
C      DPYRX0=EPYR*(-10.D0)*(-DA1X0-DA2X0-DA3X0)
C      DPYRY0=EPYR*(-10.D0)*(-DA1Y0-DA2Y0-DA3Y0)
C      DPYRZ0=EPYR*(-10.D0)*(-DA1Z0-DA2Z0-DA3Z0)
C     ... however it is more efficient to calculate from the negative sum of 
C     the derivatives for the other atoms.

C     Now move on to the three nearest atoms to the nitrogen and find their
C     first derivative contributions from the PM6 nitrogen pyramidalization
C     correction with respect to their coordinates.  (This is done because
C     this subroutine is only executed for the nitrogen atom at the center of
C     these three atoms and so is the only chance to calculate the first
C     derivative contributions for these atoms' derivatives.)

C     First take derivatives of phi1, phi2, and phi3 w.r.t. coordinates of surrounding
C     atoms (which are (x1,y1,z1), (x2,y2,z2), and (x3,y3,z3) for the closest, 
C     second closest, and third closest atoms to the nitrogen respectively).
C     (Here DA2Y3 is d(phi2)/d(y3), for example.)
      DA1X1=PREF12*( DEN12*(-DX(2)) + DOT12*(DX(1)/DIST1*DIST2) )
      DA1Y1=PREF12*( DEN12*(-DY(2)) + DOT12*(DY(1)/DIST1*DIST2) )
      DA1Z1=PREF12*( DEN12*(-DZ(2)) + DOT12*(DZ(1)/DIST1*DIST2) )
      
      DA1X2=PREF12*( DEN12*(-DX(1)) + DOT12*(DX(2)/DIST2*DIST1) )
      DA1Y2=PREF12*( DEN12*(-DY(1)) + DOT12*(DY(2)/DIST2*DIST1) )
      DA1Z2=PREF12*( DEN12*(-DZ(1)) + DOT12*(DZ(2)/DIST2*DIST1) )

C     DA1X3=0.D0
C     DA1Y3=0.D0
C     DA1Z3=0.D0

      DA2X1=PREF13*( DEN13*(-DX(3)) + DOT13*(DX(1)/DIST1*DIST3) )
      DA2Y1=PREF13*( DEN13*(-DY(3)) + DOT13*(DY(1)/DIST1*DIST3) )
      DA2Z1=PREF13*( DEN13*(-DZ(3)) + DOT13*(DZ(1)/DIST1*DIST3) )

C     DA2X2=0.D0
C     DA2Y2=0.D0
C     DA2Z2=0.D0
      
      DA2X3=PREF13*( DEN13*(-DX(1)) + DOT13*(DX(3)/DIST3*DIST1) )
      DA2Y3=PREF13*( DEN13*(-DY(1)) + DOT13*(DY(3)/DIST3*DIST1) )
      DA2Z3=PREF13*( DEN13*(-DZ(1)) + DOT13*(DZ(3)/DIST3*DIST1) )

C     DA3X1=0.D0
C     DA3Y1=0.D0
C     DA3Z1=0.D0
      
      DA3X2=PREF23*( DEN23*(-DX(3)) + DOT23*(DX(2)/DIST2*DIST3) )
      DA3Y2=PREF23*( DEN23*(-DY(3)) + DOT23*(DY(2)/DIST2*DIST3) )
      DA3Z2=PREF23*( DEN23*(-DZ(3)) + DOT23*(DZ(2)/DIST2*DIST3) )
      
      DA3X3=PREF23*( DEN23*(-DX(2)) + DOT23*(DX(3)/DIST3*DIST2) )
      DA3Y3=PREF23*( DEN23*(-DY(2)) + DOT23*(DY(3)/DIST3*DIST2) )
      DA3Z3=PREF23*( DEN23*(-DZ(2)) + DOT23*(DZ(3)/DIST3*DIST2) )

C     Next calculate the overall contribution to the first derivatives for 
C     the three atoms surrounding the nitrogen.  Note that because
C     phi1 does not depend on the coordinates of the third atom (x3,y3,z3)
C     the derivatives w.r.t. those variables have been omitted.  Similarly
C     for phi2 and phi3 which do not depend on the coordinates of the
C     second and first atoms respectively.
      DPYRX1=EPYR*(-10.D0)*(-DA1X1-DA2X1)
      DPYRY1=EPYR*(-10.D0)*(-DA1Y1-DA2Y1)
      DPYRZ1=EPYR*(-10.D0)*(-DA1Z1-DA2Z1)

      DPYRX2=EPYR*(-10.D0)*(-DA1X2-DA3X2)
      DPYRY2=EPYR*(-10.D0)*(-DA1Y2-DA3Y2)
      DPYRZ2=EPYR*(-10.D0)*(-DA1Z2-DA3Z2)
 
      DPYRX3=EPYR*(-10.D0)*(-DA2X3-DA3X3)
      DPYRY3=EPYR*(-10.D0)*(-DA2Y3-DA3Y3)
      DPYRZ3=EPYR*(-10.D0)*(-DA2Z3-DA3Z3)

      DPYRX0=-DPYRX1-DPYRX2-DPYRX3
      DPYRY0=-DPYRY1-DPYRY2-DPYRY3
      DPYRZ0=-DPYRZ1-DPYRZ2-DPYRZ3

      RETURN
 9999 RETURN 1                                                          CSTP
      END
