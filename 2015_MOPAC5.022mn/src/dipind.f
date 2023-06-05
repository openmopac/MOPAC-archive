      SUBROUTINE DIPIND (DIPVEC,*)                                      CSTP
C...............................................................
C  MODIFICATION OF DIPOLE SUBROUTINE FOR USE IN THE CALUCLATION
C  OF THE INDUCED DIPOLES FOR POLARIZABILITIES.
C...............................................................
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      COMMON /CORE  / CORE(120)
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /GEOM  / GEO(3,NUMATM)
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /NUMCAL/ NUMCAL
      COMMON /KEYWRD/ KEYWRD
      COMMON /ISTOPE/ AMS(120)
      COMMON /MULTIP/ DD(120), QQ(120), AM(120), AD(120), AQ(120)
      DIMENSION Q(MAXORB),Q2(MAXORB),DIPVEC(3),CENTER(3),
     1          COORD(3,NUMATM)
      CHARACTER*160 KEYWRD
      COMMON /DOPRNT/ DOPRNT                                            LF1111
      LOGICAL DOPRNT                                                    LF1111
      COMMON /HYFACT/ HFACT, USEHFC                                     LF1111
      LOGICAL USEHFC                                                    LF1111
C
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
      DIMENSION DIP(4,3)
      DIMENSION HYF(120,2)
      LOGICAL FORCE, CHARGD
         SAVE                                                           GL0892
      DATA HYF(1,1)     / 0.0D00           /
      DATA   HYF(1,2) /0.0D0     /
      DATA   HYF(5,2) /6.520587D0/
      DATA   HYF(6,2) /4.253676D0/
      DATA   HYF(7,2) /2.947501D0/
      DATA   HYF(8,2) /2.139793D0/
      DATA   HYF(9,2) /2.2210719D0/
      DATA   HYF(14,2)/6.663059D0/
      DATA   HYF(15,2)/5.657623D0/
      DATA   HYF(16,2)/6.345552D0/
      DATA   HYF(17,2)/2.522964D0/
      DATA ICALCN /0/
C
C  SETUP FOR DIPOLE CALCULATION
C
      CALL CHRGE (P,Q2,*9999)                                            CSTP(call)
      DO 10 I = 1,NUMAT
         L = NAT(I)
         Q(I) = CORE(L) - Q2(I)
   10 CONTINUE
      CALL GMETRY (GEO,COORD,*9999)                                      CSTP(call)
C
      IF (ICALCN.NE.NUMCAL) THEN                                         IR0295
         DO 20 I=2,107
   20    HYF(I,1)= 5.0832*DD(I)
         WTMOL=0.D0
         SUM=0.D0
         DO 30 I=1,NUMAT
            WTMOL=WTMOL+AMS(NAT(I))
   30    SUM=SUM+Q(I)
         CHARGD=(ABS(SUM).GT.0.5D0)
         ICALCN=NUMCAL                                                   IR0295
         FORCE=(INDEX(KEYWRD,'FORCE').NE.0.OR.INDEX(KEYWRD,'IRC').NE.0)
         ITYPE=1
         IF(INDEX(KEYWRD,'MINDO') .NE. 0)ITYPE=2
      ENDIF
      IF(CHARGD)THEN
C
C   NEED TO RESET ION'S POSITION SO THAT THE CENTER OF MASS IS AT THE
C   ORIGIN.
C
         DO 40 I=1,3
   40    CENTER(I)=0.D0
         DO 50 I=1,3
            DO 50 J=1,NUMAT
   50    CENTER(I)=CENTER(I)+AMS(NAT(J))*COORD(I,J)
         DO 60 I=1,3
   60    CENTER(I)=CENTER(I)/WTMOL
         DO 70 I=1,3
            DO 70 J=1,NUMAT
   70    COORD(I,J)=COORD(I,J)-CENTER(I)
      ENDIF
      DO 80 I=1,4
         DO 80 J=1,3
   80 DIP(I,J)=0.0D00
      DO 100 I=1,NUMAT
         NI=NAT(I)
         IA=NFIRST(I)
         L=NLAST(I)-IA
         DO 90 J=1,L
            K=((IA+J)*(IA+J-1))/2+IA
   90    DIP(J,2)=DIP(J,2)-HYF(NI,ITYPE)*P(K)
         DO 100 J=1,3
  100 DIP(J,1)=DIP(J,1)+4.803D00*Q(I)*COORD(J,I)
c      IF (.NOT.USEHFC) HFACT=1.00D0                                     LF1111
c      IF (USEHFC.AND.DOPRNT) WRITE(6,'(A,F6.4)')                        LF1111
c     &                                  "USING HYBRID FACTOR=",HFACT    LF1111
      DO 110 J=1,3
c  110 DIP(J,3)=DIP(J,2)*HFACT+DIP(J,1)                                  LF1111
  110 DIP(J,3)=DIP(J,2)+DIP(J,1)
      DO 120 J=1,3
  120 DIP(4,J)=SQRT(DIP(1,J)**2+DIP(2,J)**2+DIP(3,J)**2)
      DIPVEC(1)= -DIP(1,3)
      DIPVEC(2)= -DIP(2,3)
      DIPVEC(3)= -DIP(3,3)
      DIPCOM = DIP(4,3)
C      WRITE (6,60) ((DIP(I,J),I=1,4),J=1,3)
C   60 FORMAT (3(4F10.3))
      RETURN
C
 9999 RETURN 1                                                          CSTP
      ENTRY DIPIND_INIT                                                 CSAV
            CENTER = 0.0D0                                              CSAV
               DIP = 0.0D0                                              CSAV
             COORD = 0.0D0                                              CSAV
            CHARGD = .FALSE.                                            CSAV
            DIPCOM = 0.0D0                                              CSAV
             FORCE = .FALSE.                                            CSAV
             HYF   =  0.0D0                                             CSAV
             HYF(1,1) = 0.0D00                                          CSAV
             HYF(1,2) = 0.0D0                                           CSAV
             HYF(5,2) = 6.520587D0                                      CSAV
             HYF(6,2) = 4.253676D0                                      CSAV
             HYF(7,2) = 2.947501D0                                      CSAV
             HYF(8,2) = 2.139793D0                                      CSAV
             HYF(9,2) = 2.2210719D0                                     CSAV
             HYF(14,2)= 6.663059D0                                      CSAV
             HYF(15,2)= 5.657623D0                                      CSAV
             HYF(16,2)= 6.345552D0                                      CSAV
             HYF(17,2)= 2.522964D0                                      CSAV
                 I = 0                                                  CSAV
                IA = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
             ITYPE = 0                                                  CSAV
                 J = 0                                                  CSAV
                 K = 0                                                  CSAV
                 L = 0                                                  CSAV
                NI = 0                                                  CSAV
                Q2 = 0.0D0                                              CSAV
               SUM = 0.0D0                                              CSAV
             WTMOL = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
