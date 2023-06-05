      SUBROUTINE PM6PYR (I,EPYR,*)                                      CSTP
C***********************************************************************
C
C   PM6PYR CALCULATES THE PM6 SECONDARY AND TERTIARY AMINE
C     PYRAMIDALIZATION CORRECTION FOR A GIVEN NITROGEN ATOM.
C   WRITTEN BY LUKE FIEDLER, JANUARY 2010.
C
C   ON INPUT  I      = INDEX NUMBER OF NITROGEN ATOM.
C
C   ON OUTPUT EPYR   = PM6 ENERGY OF PYRAMIDALIZATION CORRECTION.
C
C
C *** THIS SUBROUTINE WORKS IN CONJUNCTION WITH SUBROUTINE PMSIX IN
C     ORDER TO CALCULATE THE ENTIRE PM6 CORRECTION.  THIS SUBROUTINE
C     MUST ONLY BE CALLED WHEN I IS THE INDEX OF A NITROGEN ATOM.
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
C
      INCLUDE 'SIZES.i'
C
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

C     Return with no correction if the atom is not nitrogen.
      IF (NAT(I).NE.7) RETURN

C     Get cartesian coordinates from common block's internal coordinates.
      CALL GMETRY(INTGEO,GEO,*9999)                                      CSTP(call)

C     Reset energy and initialize variables.
      EPYR=0.D0
      NATOMS=0
      NHEAVY=0
      IATOM1=0
      IATOM2=0
      IATOM3=0
      DIST1=1.D9
      DIST2=1.D9
      DIST3=1.D9
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
      
      RETURN
 9999 RETURN 1                                                          CSTP
      END
