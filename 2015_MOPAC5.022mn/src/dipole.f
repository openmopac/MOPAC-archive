      FUNCTION DIPOLE (P,Q,COORD,DIPVEC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /KEYWRD/ KEYWRD
      COMMON /ISTOPE/ AMS(120)
      COMMON /MULTIP/ DD(120), QQ(120), AM(120), AD(120), AQ(120)
      COMMON /NUMCAL/ NUMCAL
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION P(*),Q(*),COORD(3,*),DIPVEC(3), CENTER(3)
      DIMENSION CTRNUC(3), DISPLC(3), VCTCOR(3)                         LF0210
      CHARACTER*160 KEYWRD
      include 'result.f'                                                LF1111 - GA change
      COMMON /HYFACT/ HFACT, USEHFC                                     LF1111
      LOGICAL USEHFC                                                    LF1111
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
C**********************************************************************
C
C     ADDED BY LUKE FIEDLER, FEBRUARY 2010.
C     LF0210: In order to match Gaussian 09 program's dipole moment calculations
C             for charged systems it is necessary to adjust the
C             calculated dipole moment by a vector equal to:
C
C             mu(gaussian) = mu(MOPAC-MN) + Q * d
C
C             Here,  mu = dipole moment vector
C                    Q  = overall system charge
C                    d  = displacement vector from MOPAC-MN origin to
C                         Gaussian's origin
C
C             MOPAC-MN chooses the origin as the center of mass using
C             standard isotope values (which may change in the future),
C             however Gaussian chooses the origin to be the center of
C             nuclear charge (which is invariant to isotopic masses and
C             depends only on the types of elements).
C
C**********************************************************************

      DIMENSION DIP(4,3)
      DIMENSION HALFDIP(3), FULLDIP(3)                                  LF0112
      DIMENSION DIPGAU(4,3)                                             LF0210
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
      IF (ICALCN .NE. NUMCAL) THEN
         DO 10 I=2,120
   10    HYF(I,1)= 5.0832*DD(I)
         WTMOL=0.D0
         NUCSUM=0.D0                                                    LF0210
         SUM=0.D0
         DO 20 I=1,NUMAT
            WTMOL=WTMOL+AMS(NAT(I))
            NUCSUM=NUCSUM+NAT(I)                                        LF0210 
   20    SUM=SUM+Q(I)
         CHARGD=(ABS(SUM).GT.0.5D0)
         ICALCN =NUMCAL
         FORCE=(INDEX(KEYWRD,'FORCE').NE.0.OR.INDEX(KEYWRD,'IRC').NE.0)
         ITYPE=1
         IF(INDEX(KEYWRD,'MINDO') .NE. 0)ITYPE=2
      ENDIF
      DO 25 I=1,3                                                       LF0210
   25 DISPLC(I)=0.D0                                                    LF0210
      IF(CHARGD)THEN
C
C   NEED TO RESET ION'S POSITION SO THAT THE CENTER OF MASS IS AT THE
C   ORIGIN.
C
         DO 30 I=1,3
         CENTER(I)=0.D0
   30    CTRNUC(I)=0.D0                                                 LF0210
         DO 40 I=1,3
            DO 40 J=1,NUMAT
               CENTER(I)=CENTER(I)+AMS(NAT(J))*COORD(I,J)
   40    CTRNUC(I)=CTRNUC(I)+NAT(J)*COORD(I,J)                          LF0210
         DO 50 I=1,3
            CENTER(I)=CENTER(I)/WTMOL
   50    CTRNUC(I)=CTRNUC(I)/NUCSUM                                     LF0210
         KHARGE=0                                                       LF0210
         I=INDEX(KEYWRD,'CHARGE')                                       LF0210
         IF(I.NE.0) KHARGE=READA(KEYWRD,I)                              LF0210
         DO 55 I=1,3                                                    LF0210
            DISPLC(I)=CTRNUC(I)-CENTER(I)                               LF0210
   55    VCTCOR(I)=DBLE(KHARGE)*DISPLC(I)*4.803D0                       LF0210
C        The factor of 4.803 is (0.529177 angstrom/a.u.)*(2.542 debye/a.u.) LF0210
C#                                                                        LF0210
C#         WRITE(6,'(A,3F12.8)') 'CENTER(I)=',(CENTER(I),I=1,3)           LF0210
C#         WRITE(6,'(A,3F12.8)') 'CTRNUC(I)=',(CTRNUC(I),I=1,3)           LF0210
C#         WRITE(6,'(A,F12.8)')  'NUCSUM=',NUCSUM                         LF0210
C#         WRITE(6,'(A,F12.8)')  'KHARGE=',DBLE(KHARGE)                   LF0210
C#         WRITE(6,'(A,3F12.8)') 'VCTCOR(I)=',(VCTCOR(I),I=1,3)           LF0210
C#         DO 56 J=1,NUMAT                                                LF0210
C#   56      WRITE(6,'(A,3F12.8)') 'COORD(I,J)=',(COORD(I,J),I=1,3)       LF0210
         DO 60 I=1,3
            DO 60 J=1,NUMAT
   60    COORD(I,J)=COORD(I,J)-CENTER(I)
      ENDIF
      DO 70 I=1,4
         DO 70 J=1,3
   70 DIP(I,J)=0.0D00
      DO 90 I=1,NUMAT
         NI=NAT(I)
         IA=NFIRST(I)
         L=NLAST(I)-IA
         DO 80 J=1,L
            K=((IA+J)*(IA+J-1))/2+IA
   80    DIP(J,2)=DIP(J,2)-HYF(NI,ITYPE)*P(K)
         DO 90 J=1,3
   90 DIP(J,1)=DIP(J,1)+4.803D00*Q(I)*COORD(J,I)
      IF (.NOT.USEHFC) HFACT=1.00D0                                     LF1111
      IF (USEHFC.AND.DOPRNT) WRITE(6,'(A,F6.4)')                        LF1111
     &                                  "USING HYBRID FACTOR=",HFACT    LF1111
      DO 100 J=1,3
      HALFDIP(J)=DIP(J,2)*0.5D0+DIP(J,1)                                LF0112
      FULLDIP(J)=DIP(J,2)+DIP(J,1)                                      LF0112
  100 DIP(J,3)=DIP(J,2)*HFACT+DIP(J,1)                                  LF1111
      FHALFD = SQRT(HALFDIP(1)**2+HALFDIP(2)**2+HALFDIP(3)**2)          LF0112
cdebug      open(47,file='d',access='append')
cdebug      write(47,*) "dipole.f FHALFD=",FHALFD
cdebug      close(47)
      FFULLD = SQRT(FULLDIP(1)**2+FULLDIP(2)**2+FULLDIP(3)**2)          LF0112
      DO 110 J=1,3
  110 DIP(4,J)=SQRT(DIP(1,J)**2+DIP(2,J)**2+DIP(3,J)**2)
      FMDIP  = DIP(4,1)                                                 LF0112
      IF( FORCE) THEN
         DIPVEC(1)=DIP(1,3)
         DIPVEC(2)=DIP(2,3)
         DIPVEC(3)=DIP(3,3)
      ELSE
         IF (INDEX(KEYWRD,'DIPG09').NE.0) THEN                          LF0210
            IF (DOPRNT) WRITE (6,121)                                   CIO
            IF (DOPRNT) WRITE (6,120) ((DIP(I,J),I=1,4),J=1,3)          CIO
            IF (DOPRNT) WRITE (6,122)                                   CIO
            DO 115 J=1,3
               DO 115 I=1,3
               IF (J.NE.2) THEN
                  DIPGAU(I,J)=DIP(I,J)-VCTCOR(I)
               ELSE
                  DIPGAU(I,J)=DIP(I,J)
               ENDIF
  115       DIPGAU(4,J)=SQRT(DIPGAU(1,J)**2+DIPGAU(2,J)**2+
     &                       DIPGAU(3,J)**2)
            IF (DOPRNT) WRITE (6,120) ((DIPGAU(I,J),I=1,4),J=1,3)       CIO
         ELSE
            IF (DOPRNT) WRITE (6,120) ((DIP(I,J),I=1,4),J=1,3)          CIO
         ENDIF
      ENDIF
      FDIPOL = DIP(4,3)                                                 LF1111 - GA change
      DIPOLE = DIP(4,3)
C
  120 FORMAT (/,' DIPOLE',11X,2HX ,8X,2HY ,8X,2HZ ,6X,'TOTAL',/,
     1' POINT-CHG.',4F10.3/,' HYBRID',4X,4F10.3/,' SUM',7X,4F10.3)
C
  121 FORMAT (/,5X,'STANDARD MOPAC DIPOLE USING CENTER OF ISOTOPIC ',   LF0210
     &'MASSES:')                                                        LF0210
  122 FORMAT (/,8X,'DIPOLE MOMENT USING CENTER OF NUCLEAR ',            LF0210
     &'CHARGES:')                                                       LF0210
C
      RETURN
      ENTRY DIPOLE_INIT                                                 CSAV
            CENTER = 0.0D0                                              CSAV
            CTRNUC = 0.0D0                                              CSAV
            DISPLC = 0.0D0                                              CSAV
            VCTCOR = 0.0D0                                              CSAV
               DIP = 0.0D0                                              CSAV
            DIPGAU = 0.0D0                                              CSAV
            CHARGD = .FALSE.                                            CSAV
             FORCE = .FALSE.                                            CSAV
               HYF =0.0D0                                               CSAV
             HYF(1,1) =0.0D0                                            CSAV
             HYF(1,2) =0.0D0                                            CSAV
             HYF(5,2) =6.520587D0                                       CSAV
             HYF(6,2) =4.253676D0                                       CSAV
             HYF(7,2) =2.947501D0                                       CSAV
             HYF(8,2) =2.139793D0                                       CSAV
             HYF(9,2) =2.2210719D0                                      CSAV
             HYF(14,2)=6.663059D0                                       CSAV
             HYF(15,2)=5.657623D0                                       CSAV
             HYF(16,2)=6.345552D0                                       CSAV
             HYF(17,2)=2.522964D0                                       CSAV
                 I = 0                                                  CSAV
                IA = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
             ITYPE = 0                                                  CSAV
                 J = 0                                                  CSAV
                 K = 0                                                  CSAV
            KHARGE = 0                                                  CSAV
                 L = 0                                                  CSAV
                NI = 0                                                  CSAV
            NUCSUM = 0                                                  CSAV
               SUM = 0.0D0                                              CSAV
             WTMOL = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
