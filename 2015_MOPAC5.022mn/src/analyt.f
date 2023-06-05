      SUBROUTINE ANALYT(PSUM,PALPHA,PBETA,COORD,NAT,JJA,JJD,
     1IIA,IID,NORB,ENG,KREP,*)                                          CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION COORD(3,*),ENG(3), PSUM(*), PALPHA(*), PBETA(*),NAT(*)
************************************************************************
*                                                                      *
*         CALCULATION OF ANALYTICAL DERIVATIVES                        *
*                                                                      *
*  INPUT:       PSUM    = WHOLE DENSITY MATRIX (ALPHA+BETA)            * <=== LF1110
*               PALPHA  = ALPHA DENSITY MATRIX                         *
*               PBETA   = BETA DENSITY MATRIX                          *
*               COORD   = 3X2 ARRAY OF ATOM PAIR'S COORDINATES         *
*                             (IN ANGSTROMS)                           *
*               NAT     = TWO VALUES: ATOMIC # OF ATOM #1 AND ATOM #2  *
*               JJA     = INDEX FOR FIRST A.O. ON ATOM #2              *
*               JJD     = INDEX FOR LAST A.O. ON ATOM #2               *
*               IIA     = INDEX FOR FIRST A.O. ON ATOM #1              *
*               IID     = INDEX FOR LAST A.O. ON ATOM #1               *
*               NORB    = TOTAL # ATOMIC ORBITALS                      *
*               KREP    = (NOT USED)                                   *
*                                                                      *
*  OUTPUT:      ENG     = THREE VALUES: ENERGY DERIVATIVE WITH RESPECT *
*                          TO X-, Y-, OR Z-COORDINATE OF ATOM PAIR'S   *
*                          INTERNUCLEAR SEPARATION (KCAL/MOL/ANGSTROM) *
*                                                                      *
************************************************************************
C
C COMMON BLOCKS 'OWNED' BY REST OF PROGRAM.
C
      COMMON /CORE  / CORE(120)
     1       /BETAS / BETAS(120),BETAP(120),BETAD(120)
     2       /EXPONT/ ZS(120),ZP(120),ZD(120)
     3       /ALPHA / ALPA(120)
      COMMON /TWOEL3/ F03(120)
      COMMON /NATORB/ NATORB(120)
      COMMON /ALPHA3/ ALP3(153)
      COMMON /IDEAS / FN1(120,10),FN2(120,10),FN3(120,10)
C     COMMON /WMATRX/ W(N2ELEC), DUMMY(N2ELEC),KDUMMY,NBAND(NUMATM)
      COMMON /WMATRX/ W(N2ELEC), DUMMY(N2ELEC*2),KDUMMY,NBAND(NUMATM)
      COMMON /NATYPE/ NZTYPE(120),MTYPE(30),LTYPE
      COMMON /BETA3 / BETA3(153)
      COMMON /VSIPS / VS(120),VP(120),VD(120)
      COMMON /KEYWRD/ KEYWRD
      COMMON /PM3DSB/ ALPPMD,R0PMD(120),S6PMD,C6PMD(120),USSPMD(120),   LF0809
     &   UPPPMD(120),BETASD(120),BETAPD(120),ALPHAD(120),EISOLD(120)    LF0809
      COMMON /AM1DSB/ ALPAMD,R0AMD(120),S6AMD,C6AMD(120),USSAMD(120),   LF0809
     &   UPPAMD(120),BETASE(120),BETAPE(120),ALPHAE(120),EISOLE(120)    LF0809
      COMMON /MNDODS/ ALPMND,R0MND(120),S6MND,C6MND(120),USSMND(120),
     &   UPPMND(120),BETASF(120),BETAPF(120),ALPHAF(120),EISOLF(120),
     &   GSSMND(120),GSPMND(120),GPPMND(120),GP2MND(120),HSPMND(120),
     &   ZSMND(120),ZPMND(120)
      COMMON /IONPOT/ ATOMIP(120)                                       LF0310
      COMMON /RM1DSB/ ALPRMD,R0RMD(120),S6RMD,C6RMD(120),USSRMD(120),   LF0310
     &   UPPRMD(120),BETASI(120),BETAPI(120),ALPHAI(120),EISOLI(120)    LF0310
      COMMON /PM6DSB/ ALPP6D,R0P6D(120),S6P6D,C6P6D(120),USSP6D(120),   LF0310
     &   UPPP6D(120),BETASJ(120),BETAPJ(120),ALPHAJ(120),EISOLJ(120)    LF0310
      COMMON /SCRTCH/ ANSCR(10000),IANSCP                               LF0610
      COMMON /ORIGS/  UN(100)                                           LF1110

C
C COMMON BLOCKS 'OWNED' BY ANT
C
      COMMON /DERIVS/ DS(16),DG(22),DR(100),TDX(3),TDY(3),TDZ(3)
      COMMON /EXTRA/  G(22), TXYZ(9)
      COMMON /NUMCAL/ NUMCAL
C
C ON RETURN, ENG HOLDS ANALYTICAL DERIVATIVES
C
      COMMON /FORCE3/ IDMY(5),I3N,IX
      COMMON /SRPL/ ISSRP                                               IR0494
C Note that include 'method.f' is not used here because the global variable
C "ANALYT" conflicts with this subroutine name.  Thus the following declaration
C is given for the common block /METHOD/:
      COMMON /METHOD/ LMINDO,LMNDO,LAM1,LPM3,LRM1,LMDG,LPDG,LAM1D,      LF0111
     &                LPM3D,LPM6,AM1PM3,LANLYT,                         LF0111
     &                LPM6G9,                                           LF0111
     &                LMNDOD,LSDAMP,LRM1D,LPM6D,DSPMET,                 LF0111
     &                LPMOV1,LPMOV1A,                                   LF0111
     &                LMNDOD3,LAM1D3,D3METH,                            LF1211
     &                LPM3D3,LRM1D3,LPM6D3,                             LF1211
     &                LHGDAMP,LTDAMP,LHGDISP,LTDISP,LDAMP5              LF0312
      LOGICAL  LMINDO,LMNDO,LAM1,LPM3,LRM1,LMDG,LPDG,LAM1D,LPM3D,LPM6,  LF0111
     &         AM1PM3,LANLYT,LPM6G9,LMNDOD,LSDAMP,LRM1D,LPM6D,DSPMET,   LF0111
     &         LPMOV1,LPMOV1A,                                          LF0111
     &         LMNDOD3,LAM1D3,D3METH,                                   LF1111
     &         LPM3D3,LRM1D3,LPM6D3,                                    LF1211
     &         LHGDAMP,LTDAMP,LHGDISP,LTDISP,LDAMP5                     LF0312
      DIMENSION EAA(3),EAB(3),ENUC(3), BI(4), BJ(4),TRMBET(4,4)         IR0494
      CHARACTER*160 KEYWRD
      LOGICAL ISSRP
      COMMON /HPUSED/ HPUSED                                            LF1110
      LOGICAL HPUSED                                                    LF1110
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1110
      COMMON /TDAMPB/  CPAIR(16,16), DPAIR(16,16),                      LF0312
     &                 RPNUM, RPDEN, DPDEN, SPDEN,                      LF0312
     &                 USEDPR, USECPR                                   LF0312
      LOGICAL          USEDPR, USECPR                                   LF0312
      DIMENSION RV(120),C6(120)                                         LF0312
      DATA ICALCN /0/
      IF (ICALCN .NE. NUMCAL) THEN
C ******************************************************************************************* LF0110
C The logical AM1PM3 is .true. for any of the methods: AM1, PM3, RM1, PDG, AM1-D, PM3-D, and PM6,
C all of which use the AM1-type gaussians for core-core interactions.  For MINDO-3, the
C variable LMINDO is set to .true.
C The dispersion corrective methods, AM1-D and PM3-D, have further nuclear terms to
C differentiate.  For those methods set DSPMET to .true.
C *******************************************************************************************         
         ICALCN = NUMCAL
      ENDIF
      A0=0.529167D0
      JD=JJD-JJA+1
      JA=1
      ID=IID-IIA+1+JD
      IA=JD+1
      DO 10 J=1,3
         EAA(J)=0.0D0
         EAB(J)=0.0D0
         ENUC(J)=0.0D0
         ENG(J)=0.0D0
   10 CONTINUE
      I=2
      NI=NAT(I)
      ISTART=NZTYPE(NI)*4-3
      J=1
      NJ=NAT(J)
      JSTART=NZTYPE(NJ)*4-3
      R2=(COORD(1,I)-COORD(1,J))**2+(COORD(2,I)-COORD(2,J))**2
     1   + (COORD(3,I)-COORD(3,J))**2
      RIJ=SQRT(R2)
      R0=RIJ/A0
      RR=R2/(A0*A0)
      DO 150 IX=1,3
         DEL1=COORD(IX,I)-COORD(IX,J)
         TERMAA=0.0D0
         TERMAB=0.0D0
         ISP=0
         IOL=0
C   THE FIRST DERIVATIVES OF OVERLAP INTEGRALS
         DO 30 K=IA,ID
            KA=K-IA
            KG=ISTART+KA
            DO 30 L=JA,JD
               LA=L-JA
               LG=JSTART+LA
               IOL=IOL+1
               DS(IOL)=0.0D0
               IF(KA.EQ.0.AND.LA.EQ.0) THEN
C   (S/S) TERM
                  IF(ABS(DEL1).LE.1.0D-6) GO TO 30
                  IS=1
               ELSEIF(KA.EQ.0.AND.LA.GT.0) THEN
C   (S/P) TERM
                  IS=3
                  IF(IX.EQ.LA) GO TO 20
                  IF(ABS(DEL1).LE.1.0D-6) GO TO 30
                  IS=2
                  DEL2=COORD(LA,I)-COORD(LA,J)
               ELSEIF(KA.GT.0.AND.LA.EQ.0) THEN
C   (P/S) TERM
                  IS=5
                  IF(IX.EQ.KA) GO TO 20
                  IF(ABS(DEL1).LE.1.0D-6) GO TO 30
                  IS=4
                  DEL2=COORD(KA,I)-COORD(KA,J)
               ELSE
C   (P/P) TERM
                  IF(KA.EQ.LA) THEN
C    P/P
                     IS=9
                     IF(IX.EQ.KA) GO TO 20
                     IF(ABS(DEL1).LE.1.0D-6) GO TO 30
C    P'/P'
                     IS=8
                     DEL2=COORD(KA,I)-COORD(KA,J)
                  ELSEIF(IX.NE.KA.AND.IX.NE.LA) THEN
C    P'/P"
                     IF(ABS(DEL1).LE.1.0D-6) GO TO 30
                     IS=7
                     DEL2=COORD(KA,I)-COORD(KA,J)
                     DEL3=COORD(LA,I)-COORD(LA,J)
                  ELSE
C    P/P' OR P'/P
                     DEL2=COORD(KA+LA-IX,I)-COORD(KA+LA-IX,J)
                     IS=6
                  ENDIF
               ENDIF
C
C        CALCULATE OVERLAP DERIVATIVES, STORE RESULTS IN DS
C
   20          CALL DERS(KG,LG,RR,DEL1,DEL2,DEL3,IS,IOL,NAT(1),
     &                   NAT(2),KA+1,LA+1,*9999)                        CSTP
   30    CONTINUE
CIO         IF((IX.EQ.1).and.(.not.LMINDO)) READ (2) (G(I22),I22=1,22)
         IF((IX.EQ.1).and.(.not.LMINDO)) THEN                           LF0610 / CIO
            G(1:22)=ANSCR(IANSCP:IANSCP+21)                             LF0610 / CIO
            IANSCP=IANSCP+22                                            LF0610 / CIO
         ENDIF                                                          LF0610 / CIO
         IF(.NOT.LMINDO) CALL DELRI(DG,NI,NJ,R0,DEL1,*9999)             CSTP
         IF (PMODS(2).AND.HPUSED.AND.NI.EQ.9.AND.NJ.EQ.9) THEN          LF1110
          CALL DELML2(COORD,I,J,NI,NJ,IA,ID,JA,JD,IX,RIJ,DEL1,ISP,*9999)LF1110
         ELSE                                                           LF1110
          CALL DELMOL(COORD,I,J,NI,NJ,IA,ID,JA,JD,IX,RIJ,DEL1,ISP,*9999)CSTP
         ENDIF                                                          LF1110
C
C ********************************************************************
C
C   CALCULATE THE FIRST DERIVATIVE OF THE NUCLEAR REPULSION TERM
c   FOR VARIOUS METHODS:
C
C ********************************************************************
C **********************     MINDO3 CASE       ***********************
C
C  1) FOR THE MINDO-3 METHOD:
C
C
         IF(LMINDO)THEN
            II=MAX(NI,NJ)
            NBOND=(II*(II-1))/2+NI+NJ-II
            ALPHA=0
            IF(NBOND.LT.154)THEN
               ALPHA=ALP3(NBOND)
            ELSE
               IF(NATORB(NI).EQ.0)ALPHA=ALPA(NI)
               IF(NATORB(NJ).EQ.0)ALPHA=ALPHA+ALPA(NJ)
            ENDIF
            C2=(7.1995D0/F03(NI)+7.1995D0/F03(NJ))**2
            C1=DEL1/RIJ*CORE(NI)*CORE(NJ)*14.399D0
            IF(NBOND.EQ.22.OR.NBOND.EQ.29)THEN
               TERMNC= -C1*ALPHA*(1.D0/RIJ**2 -RIJ*(RIJ**2+C2)**(-1.5D0)
     1                 + 1.D0/RIJ - 1.D0/SQRT(RIJ**2+C2)) * EXP(-RIJ) 
     2                 - C1*RIJ*(RIJ**2+C2)**(-1.5D0)
            ELSEIF(RIJ.LT.1.D0.AND.ALPHA.NE.0.D0)THEN
               TERMNC=0.D0
            ELSE
               TERMNC= -C1*(1.D0/RIJ**2 - RIJ*(RIJ**2+C2)**(-1.5D0) +
     1                 ALPHA/RIJ - ALPHA/SQRT(RIJ**2+C2)) * 
     2                 EXP(-ALPHA*RIJ) - C1*RIJ*(RIJ**2+C2)**(-1.5D0)
            ENDIF
            DR1=DEL1/RIJ*14.399D0*RIJ*(RIJ**2+C2)**(-1.5D0)
         ENDIF
C ********************************************************************
C ********************      NON-MINDO3 CASE     **********************
C
C
C  2) FOR THE METHODS WITH NUCLEAR EXPRESSIONS LIKE:
C
C    Enuc = Za.Zb.(SaSa|SbSb).(1+exp[-ai.rij]+exp[-aj.rij])
C           (for non-OH/NH atom pairs)
C
C    Enuc = Za.Zb.(SaSa|SbSb).(1+tau.exp[-ai.rij]+exp[-aj.rij])
C           (for OH or NH atom pairs, where tau=rij when
C            i is the N or O atom in the atom pair)
C
         IF(RIJ.LT.1.D0.AND.NATORB(NI)*NATORB(NJ).EQ.0)THEN
            TERMNC=0.D0
            GOTO 50
         ENDIF
         IF (LMNDO.OR.LAM1.OR.LPM3.OR.LAM1D.OR.LPM3D.OR.LRM1
     &            .OR.LMNDOD.OR.LRM1D) THEN                             LF0310
           IF(NI.EQ.1.AND.(NJ.EQ.7.OR.NJ.EQ.8)) THEN
              F3=1.0D0+EXP(-ALPA(1)*RIJ)+RIJ*EXP(-ALPA(NJ)*RIJ)
              DD=DG(1)*F3-G(1)*(DEL1/RIJ)*(ALPA(1)*EXP(-ALPA(1)*RIJ)
     &           +(ALPA(NJ)*RIJ-1.0D0)*EXP(-ALPA(NJ)*RIJ))
           ELSEIF((NI.EQ.7.OR.NI.EQ.8).AND.NJ.EQ.1) THEN
              F3=1.0D0+EXP(-ALPA(1)*RIJ)+RIJ*EXP(-ALPA(NI)*RIJ)
              DD=DG(1)*F3-G(1)*(DEL1/RIJ)*(ALPA(1)*EXP(-ALPA(1)*RIJ)
     &           +(ALPA(NI)*RIJ-1.0D0)*EXP(-ALPA(NI)*RIJ))
           ELSE
              IF (PMODS(3).AND.HPUSED.AND.NI.EQ.9.AND.NJ.EQ.9) THEN      LF1110
                 F3=1.0D0+EXP(-PMODVL(1)*RIJ)+EXP(-PMODVL(1)*RIJ)        LF1110
                 DD=DG(1)*F3-G(1)*(DEL1/RIJ)*(PMODVL(1)*EXP(-PMODVL(1)*  LF1110
     &              RIJ)+PMODVL(1)*EXP(-PMODVL(1)*RIJ))                  LF1110
              ELSEIF (PMODS(3).AND.HPUSED.AND.NI.EQ.8.AND.NJ.EQ.8) THEN  LF1110
                 F3=1.0D0+EXP(-PMODVL(2)*RIJ)+EXP(-PMODVL(2)*RIJ)        LF1110
                 DD=DG(1)*F3-G(1)*(DEL1/RIJ)*(PMODVL(2)*EXP(-PMODVL(2)*  LF1110
     &              RIJ)+PMODVL(2)*EXP(-PMODVL(2)*RIJ))                  LF1110
              ELSE                                                       LF1110
                 IF (hpused.and.NI.EQ.9.and.(nj.eq.7.or.nj.eq.8)) THEN   LF0411
                   F3=1.D0+EXP(-ALPA(NI)*RIJ)+RIJ*EXP(-ALPA(NJ)*RIJ)     LF0411
                   DF3=-ALPA(NI)*EXP(-ALPA(NI)*RIJ)+EXP(-ALPA(NJ)*RIJ)*  LF0411
     &                 (1.D0-ALPA(NJ)*RIJ)                               LF0411
                   DD=DG(1)*F3+G(1)*DF3*(DEL1/RIJ)                       LF0411
                 ELSEIF(hpused.and.NJ.EQ.9.and.(ni.eq.7.or.ni.eq.8))THEN LF0411
                   F3=1.D0+RIJ*EXP(-ALPA(NI)*RIJ)+EXP(-ALPA(NJ)*RIJ)     LF0411
                   DF3=-ALPA(NJ)*EXP(-ALPA(NJ)*RIJ)+EXP(-ALPA(NI)*RIJ)*  LF0411
     &                 (1.D0-ALPA(NI)*RIJ)                               LF0411
                   DD=DG(1)*F3+G(1)*DF3*(DEL1/RIJ)                       LF0411
                 ELSE                                                    LF0411
                   F3=1.0D0+EXP(-ALPA(NI)*RIJ)+EXP(-ALPA(NJ)*RIJ)        LF0411
                   DD=DG(1)*F3-G(1)*(DEL1/RIJ)*(ALPA(NI)*EXP(-ALPA(NI)*  LF0411
     &               RIJ)+ALPA(NJ)*EXP(-ALPA(NJ)*RIJ))                   LF0411
                 ENDIF                                                   LF0411
              ENDIF
           ENDIF
           TERMNC=CORE(NI)*CORE(NJ)*DD
         ENDIF
C
C  3) FOR THE PM6 METHOD:                                               LF0110
C
C        Calculate first derivative contributions for the PM6 core-core
C        terms (everything except for the AM1-style gaussians which are
C        calculated separately).
C
         IF (LPM6.OR.LPM6D) THEN 
            CALL PM6DER(NI,NJ,RIJ,IX,DEL1,G(1),DG(1),DENUC,*9999)       CSTP
            TERMNC=DENUC
         ENDIF
C
C ********************************************************************
C
C   ******************************************************************
C   ****   START OF THE AM1-TYPE GAUSSIANS DERIVATIVE CODE         ***
C
C      ANALYT=-A*(1/(R*R)+2.D0*B*(R-C)/R)*EXP(-B*(R-C)**2)
C
         IF( AM1PM3 )THEN
C           Do for all methods containing AM1-type gaussian terms.
            ANAM1=0.D0
            DO 40 IG=1,10
               IF(ABS(FN1(NI,IG)).GT.0.D0)
     1ANAM1=ANAM1+FN1(NI,IG)*
     2(1.D0/(RIJ*RIJ)+2.D0*FN2(NI,IG)*(RIJ-FN3(NI,IG))/RIJ)*
     3EXP(MAX(-30.D0,-FN2(NI,IG)*(RIJ-FN3(NI,IG))**2))
               IF(ABS(FN1(NJ,IG)).GT.0.D0)
     1ANAM1=ANAM1+FN1(NJ,IG)*
     2(1.D0/(RIJ*RIJ)+2.D0*FN2(NJ,IG)*(RIJ-FN3(NJ,IG))/RIJ)*
     3EXP(MAX(-30.D0,-FN2(NJ,IG)*(RIJ-FN3(NJ,IG))**2))
   40       CONTINUE
            ANAM1=ANAM1*CORE(NI)*CORE(NJ)
            TERMNC=TERMNC-ANAM1*DEL1/RIJ
         ENDIF
C
C   ****   END OF THE AM1-TYPE GAUSSIANS DERIVATIVE CODE           ***
C   ******************************************************************
C
C   ******************************************************************
C   ****   START OF DISPERSION DERIVATIVE CODE                     ***
C   ****   This only calculates the derivative of the extra        ***
C   ****   "dispersion" term contributions to the nuclear energy   ***
C   ****   for the xxxx-D methods.                                 ***
C
Cold         IF (DSPMET) THEN
Cold            ANDISP=0.D0
Cold            IF (LPM6D) THEN                                             LF0310
Cold               R0SUM=R0P6D(NI)+R0P6D(NJ)
Cold               IF (.NOT.LSDAMP) THEN
Cold                 EXPTRM=EXP(-ALPP6D*((RIJ/R0SUM)-1))
Cold                 FDAMP=1.D0/(1.D0+EXPTRM)
Cold                 FDDERV= (FDAMP**2)*EXPTRM*ALPP6D/R0SUM
Cold               ELSE
ColdC                LJF0310: Added SDAMP analytical derivative code.       LF0310
Cold                 BETAIJ=SQRT(2.D0*ATOMIP(NI)/27.211D0)+
Cold     &                  SQRT(2.D0*ATOMIP(NJ)/27.211D0)
Cold                 T1=BETAIJ*RIJ/0.529177D0
Cold                 T2=1.D0
Cold                 FDAMP=1.D0
Cold                 DO 44 I=1,6
Cold                   T2 = T2*T1/DBLE(I)
Cold                   FDAMP = FDAMP+T2
Cold   44            CONTINUE
Cold                 FDAMP = 1.D0 - (FDAMP*EXP(-T1))
Cold                 FDDERV=EXP(-T1)*((T1)**6)*BETAIJ/720.D0
ColdC                Require the derivative is with respect to angstroms, not a.u.
Cold                 FDDERV=FDDERV/0.529177D0
Cold               ENDIF
Cold               C6COMB = 2.D0 * C6P6D(NI)*C6P6D(NJ)/
Cold     &                  (C6P6D(NI)+C6P6D(NJ))
Cold               C6COMB = C6COMB * 10.364264D0 
Cold               ANDISP= C6COMB*S6P6D/(RIJ**6)*((6.D0*FDAMP/RIJ)-FDDERV)
Cold               GOTO 200
Cold            ENDIF                                                       LF0310
Cold            TERMNC=TERMNC+ANDISP*DEL1/RIJ
Cold         ENDIF
      IF (DSPMET) THEN                                                  LF0312
C     Be sure to treat "Hp" hydrogens as "H" hydrogens for dispersion.
         na=ni
         nb=nj
         if (ni.eq.9.and.hpused) na=1
         if (nj.eq.9.and.hpused) nb=1
      
C     Use the correct dispersion parameters for the given method.
C     Cannot use variable name r0 here since it is already defined above.
C     Instead use rv in place of r0 (compared to ROTATE subroutine code).
         if (LAM1D) then
            alpd=alpamd
            s6=s6amd
            do ii=1,120
              rv(ii)=r0amd(ii)
              c6(ii)=c6amd(ii)
            enddo
         elseif (LPM3D) then
            alpd=alppmd
            s6=s6pmd
            do ii=1,120
              rv(ii)=r0pmd(ii)
              c6(ii)=c6pmd(ii)
            enddo
         elseif (LRM1D) then
            alpd=alprmd
            s6=s6rmd
            do ii=1,120
              rv(ii)=r0rmd(ii)
              c6(ii)=c6rmd(ii)
            enddo
         elseif (LPM6D) then
            alpd=alpp6d
            s6=s6p6d
            do ii=1,120
              rv(ii)=r0p6d(ii)
              c6(ii)=c6p6d(ii)
            enddo
         else
            alpd=alpmnd
            s6=s6mnd
            do ii=1,120
              rv(ii)=r0mnd(ii)
              c6(ii)=c6mnd(ii)
            enddo
         endif

        DCORR = 0.0D0

        IF (USEDPR) THEN
          dij= DPAIR(NA,NB)
        ELSE
          dij= rv(na) + rv(nb)
        ENDIF

        IF (USECPR) THEN
          c6ij= CPAIR(na,nb)
        ELSE
          c6ij   = 2.0D0 * C6(NA)*C6(NB)/(C6(NA)+C6(NB))
          c6ij   = c6ij   * 10.364264D0
        ENDIF

        IF (LSDAMP) THEN
          betaij=sqrt(2.d0*atomip(na)/27.211d0)+
     &           sqrt(2.D0*atomip(nb)/27.211d0)
          t1=betaij*rij/0.529177d0   ! Need to do Stone's damping with a.u.
          t2=1.d0
          fdamp=1.d0
          do i=1,6
             t2 = t2*t1/DBLE(i)
             fdamp = fdamp+t2
          enddo
          fdamp = 1.d0 - (fdamp*exp(-t1))
          fdderv=exp(-t1)*((t1)**6)*betaij/720.d0
          fdderv=fdderv/0.529177d0   ! Derivative is wrt to angstroms, not a.u.
        ELSEIF (LHGDAMP) THEN
Corig          fdamp=1.d0/(1.d0+(alpd)*(rij/dij)**(-12))
          drij=dij/rij
          drij12=drij**12
          fdamp=1.d0/(1.d0+alpd*drij12)
          fdderv=fdamp**2*(12.d0*alpd/dij)*drij*drij12 
        ELSEIF (LTDAMP) THEN
Corig          fdamp=rij**rpnum/(rij**rpden+dij**dpden)**spden
          top=rij**rpnum
          bsum=rij**rpden+dij**dpden
          bot=dsum**spden
          dtop=top*rpnum/rij
          dbsum=rpden*rij**(rpden-1.d0)+dpden*dij**(dpden-1.d0)
          dbot=bot*spden/bsum*dbsum
          fdderv=(bot*dtop-top*dbot)/bot**2
        ELSEIF (LDAMP5) THEN
Corig          fdamp=(rij**6)*(1.d0-exp(-(rij/dij)**8))
          pre=rij**6
          expo=exp(-(rij/dij)**8)
          post=1.d0-expo
          fdamp=pre*post
          fdderv=6*pre/rij*post+pre*expo*8.d0*(rij/dij)**7/dij
        ELSE
Corig          fdamp = 1.d0/(1.d0+EXP(-alpd*(rij/(dij)-1.d0)))
          exptrm=exp(-alpd*((rij/dij)-1.d0))
          fdamp=1.d0/(1.d0+exptrm)
          fdderv= (fdamp**2)*exptrm*alpd/dij
        ENDIF

        IF (LTDISP) THEN
Corig          dcorr = (-c6ij/(rij**6))*fdamp
          pre= -c6ij/rij**6
          dpre= -6.d0/rij*pre
          post= fdamp
          dpost= fdderv
          andisp= pre*dpost+dpre*post
        ELSEIF (LHGDISP) THEN
Corig          dcorr = (-c6ij/(rij**6))*fdamp
          pre= -c6ij/rij**6
          dpre= -6.d0/rij*pre
          post= fdamp
          dpost= fdderv
          andisp= pre*dpost+dpre*post
        ELSE
Corig          dcorr = (-S6)*fdamp*c6ij/(rij**6)
          pre= -s6*c6ij/rij**6
          dpre= -6.d0/rij*pre
          post= fdamp
          dpost= fdderv
          andisp= pre*dpost+dpre*post
        ENDIF

        TERMNC=termnc+andisp*del1/rij
      ENDIF
 200  CONTINUE
C   ****    END OF DISPERSION DERIVATIVE CODE                      ***
C   ******************************************************************
C
   50    CONTINUE
C
C   COMBINE TOGETHER THE OVERLAP DERIVATIVE PARTS
C
         IF(LMINDO)THEN
            II=MAX(NI,NJ)
            NBOND=(II*(II-1))/2+NI+NJ-II
            IF(NBOND.GT.153)GOTO 60
            BI(1)=BETA3(NBOND)*VS(NI)*2.D0
            BI(2)=BETA3(NBOND)*VP(NI)*2.D0
            BI(3)=BI(2)
            BI(4)=BI(2)
            BJ(1)=BETA3(NBOND)*VS(NJ)*2.D0
            BJ(2)=BETA3(NBOND)*VP(NJ)*2.D0
            BJ(3)=BJ(2)
            BJ(4)=BJ(2)
   60       CONTINUE
         ELSE
            BI(1)=BETAS(NI)
            BI(2)=BETAP(NI)
            BI(3)=BI(2)
            BI(4)=BI(2)
            BJ(1)=BETAS(NJ)
            BJ(2)=BETAP(NJ)
            BJ(3)=BJ(2)
            BJ(4)=BJ(2)
         ENDIF
C
C       CODE COMMON TO MINDO/3, MNDO, AND AM1
C
* 
* ....first generate pairwise BETA                                      IR0494
         IF(ISSRP) THEN                                                 IR0494
            TRMBET(1,1)=2.0d0 *GETBET( NI, NJ, 'BETSS')                 IR0494
            BSP=2.0d0 *GETBET(NJ, NI, 'BETSP')                          IR0494
            DO 62 IBET=2,4                                              IR0494
  62           TRMBET(IBET,1)=BSP                                       IR0494
            BSP=2.0d0 *GETBET(NI, NJ, 'BETSP')                          IR0494
            DO 64 JBET=2,4                                              IR0494
  64           TRMBET(1,JBET)=BSP                                       IR0494
            BPP=2.0d0 *GETBET(NI, NJ, 'BETPP')                          IR0494
            DO 66 JBET=2,4                                              IR0494
               DO 66 IBET=2,4                                           IR0494
  66           TRMBET(IBET,JBET)=BPP                                    IR0494
         ELSE                                                           IR0494
            DO 68 JBET=1,4                                              IR0494
               DO 68 IBET=1,4                                           IR0494
  68              TRMBET(IBET,JBET)=BI(IBET)+BJ(JBET)                   IR0494
         ENDIF                                                          IR0494
         IF (index(keywrd,' DEBUG') .ne. 0) THEN                        IR0494
            Write(6,'("BETA", I4, I4)') NI, NJ                          IR0494
            do 69 ibet=1,4                                              IR0494
  69           Write(6,'(4F10.6)') (TRMBET(IBET,JBET),JBET=1,4)         IR0494
         ENDIF                                                          IR0494
         IOL=0
         DO 70 K=IA,ID
            DO 70 L=JA,JD
               LK=L+K*(K-1)/2
               IOL=IOL+1
               TERMAB=TERMAB+TRMBET((K-IA+1),(L-JA+1))*PSUM(LK)*DS(IOL) IR0494
   70    CONTINUE
         IF(LMINDO)THEN
C
C        FIRST, CORE-ELECTRON ATTRACTION DERIVATIVES (MINDO/3)
C
C          ATOM CORE I AFFECTING A.O.S ON J
            DO 80 M=JA,JD
               MN=(M*(M+1))/2
   80       TERMAB=TERMAB+CORE(NI)*PSUM(MN)*DR1
C          ATOM CORE J AFFECTING A.O.S ON I
            DO 90 M=IA,ID
               MN=(M*(M+1))/2
   90       TERMAB=TERMAB+CORE(NJ)*PSUM(MN)*DR1
C
C   NOW FOR COULOMB AND EXCHANGE TERMS (MINDO/3)
C
            DO 100 I1=IA,ID
               II=(I1*(I1+1))/2
               DO 100 J1=JA,JD
                  JJ=(J1*(J1+1))/2
                  IJ=J1+(I1*(I1-1))/2
C
C           COULOMB TERM
C
                  TERMAA=TERMAA-PSUM(II)*DR1*PSUM(JJ)
C
C           EXCHANGE TERM
C
                  TERMAA=TERMAA+(PALPHA(IJ)*PALPHA(IJ)+PBETA(IJ)*PBETA(I
     1J))*DR1
  100       CONTINUE
         ELSE
C
C        FIRST, CORE-ELECTRON ATTRACTION DERIVATIVES (MNDO AND AM1)
C        (THIS ALSO HANDLES PM3 AS WELL)                                LF0809
C        (FOR AM1-D, PM3-D, PM6, AND MNDO-D THIS IS USED ALSO)          LF0110/LF0310
C
C          ATOM CORE I AFFECTING A.O.S ON J
            ISP=0
            DO 110 M=JA,JD
               BB=1.D0
               DO 110 N=M,JD
                  MN=M+N*(N-1)/2
                  ISP=ISP+1
C Added LF1110:
                  IF (PMODS(2).AND.HPUSED.AND.
     &                                     NI.EQ.9 .AND. NJ.EQ.9 .AND. 
     &                                     M.GT.JA .AND. N.GT.JA) THEN
C                    Core A with two p-orbitals on B case (Hp-Hp atom pair).
C                    Note that M=JA means M is the s orbital on atom NI.
C                    Also then N=JA means N is the s orbital on atom NI.
C                    If M>JA then it is a p-orbital on atom NI.  Same for N>JA.
C                    NI and NJ are the atomic #s of the two atoms.
                     DRNEW=DR(ISP)*(1.D0-0.75D0*EXP(-1.1D0*RIJ**2))+
     &                     UN(ISP)*(1.65D0*RIJ*EXP(-1.1D0*RIJ**2))
                     TERMAB=TERMAB-BB*CORE(NI)*PSUM(MN)*DRNEW
                  ELSE
C End LF1110 additions.
                     TERMAB=TERMAB-BB*CORE(NI)*PSUM(MN)*DR(ISP)
                  ENDIF
  110       BB=2.D0
C          ATOM CORE J AFFECTING A.O.S ON I
            K=MAX(JD-JA+1,1)
            K=(K*(K+1))/2
            ISP=-K+1
            DO 120 M=IA,ID
               BB=1.D0
               DO 120 N=M,ID
                  MN=M+N*(N-1)/2
                  ISP=ISP+K
C Added LF1110:
                  IF (PMODS(2).AND.HPUSED.AND.
     &                                     NI.EQ.9 .AND. NJ.EQ.9 .AND. 
     &                                     M.GT.IA .AND. N.GT.IA) THEN
C                    Core B with two p-orbitals on A case (Hp-Hp atom pair).
C                    Note that M=IA means M is the s orbital on atom NI.
C                    Also then N=IA means N is the s orbital on atom NI.
C                    If M>IA then it is a p-orbital on atom NI.  Same for N>IA.
C                    NI and NJ are the atomic #s of the two atoms.
                     DRNEW=DR(ISP)*(1.D0-0.75D0*EXP(-1.1D0*RIJ**2))+
     &                     UN(ISP)*(1.65D0*RIJ*EXP(-1.1D0*RIJ**2))
                     TERMAB=TERMAB-BB*CORE(NJ)*PSUM(MN)*DRNEW
                  ELSE
C End LF1110 additions.
                     TERMAB=TERMAB-BB*CORE(NJ)*PSUM(MN)*DR(ISP)
                  ENDIF
  120       BB=2.D0
            ISP=0
C
C   NOW FOR COULOMB AND EXCHANGE TERMS (MNDO AND AM1)
C
            DO 140 K=IA,ID
               AA=1.D0
               KK=(K*(K-1))/2
               DO 140 L=K,ID
                  LL=(L*(L-1))/2
                  DO 130 M=JA,JD
                     BB=1.D0
                     DO 130 N=M,JD
                        ISP=ISP+1
                        KL=K+LL
                        MN=M+N*(N-1)/2
C
C    COULOMB TERM
C
                        TERMAA=TERMAA+AA*BB*PSUM(KL)*PSUM(MN)*DR(ISP)
                        MK=M+KK
                        NK=N+KK
                        ML=M+LL
                        NL=N+LL
C
C    EXCHANGE TERM
C
                        TERMAA= TERMAA-0.5D0*AA*BB*(PALPHA(M
     1K)*PALPHA(NL)+PALPHA(NK)*PALPHA(ML)+PBETA(MK)*PBETA(NL)+PBETA(NK)*
     2PBETA(ML))*DR(ISP)
  130             BB=2.D0
  140       AA=2.D0
C           END OF MNDO AND AM1 SPECIFIC CODE
         ENDIF
         EAA(IX)=EAA(IX)+TERMAA
         EAB(IX)=EAB(IX)+TERMAB
         ENUC(IX)=ENUC(IX)+TERMNC
  150 CONTINUE
C#            WRITE(6,*)EAA,EAB,ENUC,NAT(1),NAT(2)
  160 CONTINUE
  170 CONTINUE
  
      DO 180 J=1,3
         ENG(J)=EAA(J)+EAB(J)+ENUC(J)
         ENG(J) = -ENG(J)*2.D0*23.061D0
  180 CONTINUE
      RETURN
 9999 RETURN 1                                                          CSTP
      END
