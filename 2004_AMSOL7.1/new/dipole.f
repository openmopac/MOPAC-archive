      DOUBLE PRECISION FUNCTION DIPOLE (P,Q,COORD,DIPVEC,WAVFUN)        DJG0895
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /MULTIP/ DD(107), QQ(107), AM(107), AD(107), AQ(107) 
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      DIMENSION P(*),Q(*),COORD(3,*),DIPVEC(3) 
C*********************************************************************** 
C     DIPOLE CALCULATES DIPOLE MOMENTS 
C 
C  ON INPUT P     = DENSITY MATRIX 
C           Q     = TOTAL ATOMIC CHARGES, (NUCLEAR + ELECTRONIC) 
C           NUMAT = NUMBER OF ATOMS IN MOLECULE 
C           NAT   = ATOMIC NUMBERS OF ATOMS 
C           NFIRST= START OF ATOM ORBITAL COUNTERS 
C           COORD = COORDINATES OF ATOMS 
C 
C  OUTPUT  DIPOLE = DIPOLE MOMENT 
C*********************************************************************** 
C 
C     IN THE ZDO APPROXIMATION, ONLY TWO TERMS ARE RETAINED IN THE 
C     CALCULATION OF DIPOLE MOMENTS. 
C     1. THE POINT CHARGE TERM (INDEPENDENT OF PARAMETERIZATION). 
C     2. THE ONE-CENTER HYBRIDIZATION TERM, WHICH ARISES FROM MATRIX 
C     ELEMENTS OF THE FORM <NS/R/NP>. THIS TERM IS A FUNCTION OF 
C     THE SLATER EXPONENTS (ZS,ZP) AND IS THUS DEPENDENT ON PARAMETER- 
C     IZATION. THE HYBRIDIZATION FACTORS (HYF(I)) USED IN THIS SUB- 
C     ROUTINE ARE CALCULATED FROM THE FOLLOWING FORMULAE. 
C     FOR SECOND ROW ELEMENTS <2S/R/2P> 
C     HYF(I)= 469.56193322*(SQRT(((ZS(I)**5)*(ZP(I)**5)))/ 
C           ((ZS(I) + ZP(I))**6)) 
C     FOR THIRD ROW ELEMENTS <3S/R/3P> 
C     HYF(I)=2629.107682607*(SQRT(((ZS(I)**7)*(ZP(I)**7)))/ 
C           ((ZS(I) + ZP(I))**8)) 
C     FOR FOURTH ROW ELEMENTS AND UP : 
C     HYF(I)=2*(2.10716)*DD(I) 
C     WHERE DD(I) IS THE CHARGE SEPARATION IN ATOMIC UNITS 
C 
C 
C     REFERENCES: 
C     J.A.POPLE & D.L.BEVERIDGE: APPROXIMATE M.O. THEORY 
C     S.P.MCGLYNN, ET AL: APPLIED QUANTUM CHEMISTRY 
C
C     MODIFIED BY DJG0895 TO HAVE THE OPTION TO PRINT OUT THE HYBRID    DJG0895
C     AND WAVEFUNCTION DIPOLE MOMENTS OR NOT                            DJG0895
C     WAVFUN = .TRUE. -->  PRINT HYBRID AND WAVEFUNCTION DPM            DJG0895
C     WAVFUN = .FALSE. --> DO NOT PRINT HYBRID AND WAVEFUNCTION DPM     DJG0895
C 
      DIMENSION DIP(4,3) 
      DIMENSION HYF(107,2) 
      LOGICAL FORCE,WAVFUN                                              DJG0895
       SAVE
      IF (ICONTR(2).EQ.1) THEN                                          GDH0195
         ICONTR(2)=2                                                    GDH0195
         HYF(1,1)=0.0D0
         HYF(1,2)=0.0D0
         HYF(5,2)=6.520587D0
         HYF(6,2)=4.253676D0
         HYF(7,2)=2.947501D0
         HYF(8,2)=2.139793D0
         HYF(9,2)=2.2210719D0
         HYF(14,2)=6.663059D0
         HYF(15,2)=5.657623D0
         HYF(16,2)=6.345552D0
         HYF(17,2)=2.522964D0
         DO 10 I=4,107
            HYF(I,1)= 5.0832*DD(I)
   10    CONTINUE
         FORCE=(IFORCE.NE.0)                                            DJG0995
         ITYPE=1
         IF(IMINDO.NE.0)ITYPE=2                                         DJG0995
      ENDIF                                                             GDH0195
      DO 20 I=1,4 
         DO 20 J=1,3 
   20 DIP(I,J)=0.0D00 
      IF (WAVFUN.OR.FORCE) THEN                                         DJG0895
         DO 30 I=1,NUMAT
            NI=NAT(I)
            IA=NFIRST(I)
            DO 30 J=1,3
               K=((IA+J)*(IA+J-1))/2+IA
               DIP(J,2)=DIP(J,2)-HYF(NI,ITYPE)*P(K)
   30    CONTINUE                                                       DJG0895
      ENDIF                                                             DJG0895
      DO 35 I=1,NUMAT                                                   DJG0895
         DO 33 J=1,3                                                    DJG0895
            DIP(J,1)=DIP(J,1)+4.803D00*Q(I)*COORD(J,I)                  DJG0895
   33    CONTINUE                                                       DJG0895
   35 CONTINUE                                                          DJG0895
      DO 40 J=1,3 
   40 DIP(J,3)=DIP(J,2)+DIP(J,1) 
      DO 50 J=1,3 
   50 DIP(4,J)=SQRT(DIP(1,J)**2+DIP(2,J)**2+DIP(3,J)**2) 
      IF( FORCE) THEN 
         DIPVEC(1)=DIP(1,3) 
         DIPVEC(2)=DIP(2,3) 
         DIPVEC(3)=DIP(3,3) 
      ELSE 
         IF (WAVFUN) THEN                                               DJG0895
            WRITE (JOUT,60) ((DIP(I,J),I=1,4),J=1,3)
         ELSE                                                           DJG0895
            WRITE (JOUT,70) (DIP(I,1),I=1,4)                            GDH1095
         ENDIF                                                          DJG0895
      ENDIF 
      DIPOLE = DIP(4,3) 
      RETURN 
C 
C  60 FORMAT (' DIPOLE',11X,2HX ,8X,2HY ,8X,2HZ ,6X,'TOTAL',/, 
C    1' POINT-CHG.',4F10.3/,' HYBRID',4X,4F10.3/,' SUM',7X,4F10.3) 
   60 FORMAT (/,' Dipole moment (debyes)',5X,2HX ,8X,2HY ,8X,2HZ ,6X,   BJL1003
     1        'Total',/,' from point charges',1X,4F10.3/,' hybrid ',    PDW1099
     2        'contribution',                                           PDW1099
     3         4F10.3/,' sum',16X,4F10.3)                               GL0492
   70 FORMAT (/,' Dipole moment (debyes)',4X,2HX ,8X,2HY ,8X,2HZ ,6X,   BJL1003
     1        'Total',/,' from point charges',4F10.3)                   PDW1099
C 
      END 
