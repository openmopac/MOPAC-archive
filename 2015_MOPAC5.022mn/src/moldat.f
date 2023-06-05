      SUBROUTINE MOLDAT(IP,*)                                           0304WH93 / CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1010
      include 'method.f'   ! Common block definition for /METHOD/.      LF0111
      include 'corgen.f'   ! Common block for /BTPAIR/ and /CORGEN/.    LF0113
C
************************************************************************
*                                                                      *
*    Sets up all the invariant parameters used during the calculation, *
*    e.g. number of electrons, initial atomic orbital populations,     *
*    number of open shells, etc.  Called once during a run.            *
*                                                                      *
*    ASSIGNS VALUES TO COMMON BLOCK /MOLKSI/ AS FOLLOWS:               *
*                                                                      *
*       (NUMATM)    = MAXIMUM # OF ATOMS ALLOWED (FIXED BY 'SIZES.i')  *
*        NUMAT      = # ATOMS + SPARKLES IN SYSTEM (NOT 'XX' GHOSTS)   *
*        NAT        = ARRAY OF ATOMIC NUMBERS FOR SYSTEM'S ATOMS       *
*        NFIRST     = FIRST A.O. IN RANGE FOR GIVEN ATOM               *
*        NMIDLE     = "MIDDLE" A.O. IN RANGE FOR GIVEN ATOM (FOR       *
*                       D-ORBITAL CONTAINING ATOMS; THEN IS NFIRST+3)  *
*        NLAST      = LAST A.O. IN RANGE FOR GIVEN ATOM                *
*        NORBS      = TOTAL # ATOMIC ORBITALS                          *
*        NELECS     = TOTAL # ELECTRONS IN SYSTEM                      *
*        NALPHA     = # ALPHA ELECTRONS                                *
*        NBETA      = # BETA ELECTRONS                                 *
*        NCLOSE     = # DOUBLY-FILLED MOLECULAR ORBITALS               *
*        NOPEN      = # SINGLY-FILLED MOLECULAR ORBITALS               *
*                                                                      *
*    RELATED COMMON BLOCK VARIABLES IN /GEOKST/ PROVIDED HERE FOR      *
*    COMPARISON: (SET BY READMO; SEE THAT SUBROUTINE ALSO)             *
*                                                                      *
*        NATOMS     = # ATOMS + SPARKLES + DUMMY ATOMS                 *
*                     (BASICALLY THE # LINES IN INPUT FILE FOR STRUCT) *
*        LABELS     = ARRAY OF ATOMIC NUMBERS FOR ALL ATOMS IN SYSTEM  *
*        NA         = ARRAY OF LABELS OF ATOMS, BOND LENGTHS.          *
*        NB         = ARRAY OF LABELS OF ATOMS, BOND ANGLES.           *
*        NC         = ARRAY OF LABELS OF ATOMS, DIHEDRAL ANGLES.       *
*                                                                      *
*    RELATED COMMON BLOCK VARIABLES IN /GEOM/ PROVIDED HERE FOR        *
*    COMPARISON AS WELL:                                               *
*                                                                      *
*        GEO(1,I)   = THE INTERNUCLEAR SEPARATION \IN ANGSTROMS\       *
*                     BETWEEN ATOMS NA(I) AND (I).                     *
*        GEO(2,I)   = THE ANGLE NB(I):NA(I):(I) INPUT IN DEGREES;      *
*                     STORED IN RADIANS.                               *
*        GEO(3,I)   = THE ANGLE BETWEEN THE VECTORS NC(I):NB(I) AND    *
*                     NA(I):(I) INPUT IN DEGREES - STORED IN RADIANS.  *
*                                                                      *
************************************************************************
C
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,20),NNHCO,ITYPE,USEMM
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
     3       /MOLKSR/ FRACT
     4       /KEYWRD/ KEYWRD
     5       /NATORB/ NATORB(120)
      COMMON /CORE  / CORE(120)
     1       /BETAS / BETAS(120),BETAP(120),BETAD(120)
     2       /MOLORB/ USPD(MAXORB),PSPD(MAXORB)
     3       /VSIPS / VS(120),VP(120),VD(120)
     4       /ONELEC/ USS(120),UPP(120),UDD(120)
      COMMON /ATHEAT/ ATHEAT
     1       /POLVOL/ POLVOL(120)
     2       /MULTIP/ DD(120),QQ(120),AM(120),AD(120),AQ(120)
     3       /TWOELE/ GSS(120),GSP(120),GPP(120),GP2(120),HSP(120)
     4                ,GSD(120),GPD(120),GDD(120)
     5       /IDEAS / GAUSS1(120,10),GAUSS2(120,10),GAUSS3(120,10)
     6       /IDEAP / GAUSP1(120,10),GAUSP2(120,10),GAUSP3(120,10)
     7       /IDEAR / GAUSR1(120,10),GAUSR2(120,10),GAUSR3(120,10)
      COMMON /ALPHA / ALP(120)
     1       /REFS/ ALLREF(120,17)                                      LF0509/LF1211/LF1213/LF0614
      COMMON /ATOMIP/ ATOMIP(120)                                       LF0410
      COMMON /MNDO/  USSM(120), UPPM(120), UDDM(120), ZSM(120),
     1ZPM(120), ZDM(120), BETASM(120), BETAPM(120), BETADM(120),
     2ALPM(120), EISOLM(120), DDM(120), QQM(120), AMM(120),
     3ADM(120), AQM(120), GSSM(120), GSPM(120), GPPM(120),
     4GP2M(120), HSPM(120), POLVOM(120)
      COMMON /PM3 /  USSPM3(120), UPPPM3(120), UDDPM3(120), ZSPM3(120),
     1ZPPM3(120), ZDPM3(120), BETASP(120), BETAPP(120), BETADP(120),
     2ALPPM3(120), EISOLP(120), DDPM3(120), QQPM3(120), AMPM3(120),
     3ADPM3(120), AQPM3(120) ,GSSPM3(120), GSPPM3(120), GPPPM3(120),
     4GP2PM3(120), HSPPM3(120),POLVOP(120)
      COMMON /RM1BLO/USSRM1(120), UPPRM1(120), UDDRM1(120), ZSRM1(120),
     1ZPRM1(120), ZDRM1(120), BETASR(120), BETAPR(120), BETADR(120),
     2ALPRM1(120), EISOLR(120), DDRM1(120), QQRM1(120), AMRM1(120),
     3ADRM1(120), AQRM1(120) ,GSSRM1(120), GSPRM1(120), GPPRM1(120),
     4GP2RM1(120), HSPRM1(120),POLVOR(120)
      COMMON /PM3DSB/ ALPPMD,R0PMD(120),S6PMD,C6PMD(120),USSPMD(120),   LF0409
     &   UPPPMD(120),BETASD(120),BETAPD(120),ALPHAD(120),EISOLD(120)    LF0409
      COMMON /AM1DSB/ ALPAMD,R0AMD(120),S6AMD,C6AMD(120),USSAMD(120),   LF0509
     &   UPPAMD(120),BETASE(120),BETAPE(120),ALPHAE(120),EISOLE(120)    LF0509
      COMMON /MNDODS/ ALPMND,R0MND(120),S6MND,C6MND(120),USSMND(120),
     &   UPPMND(120),BETASF(120),BETAPF(120),ALPHAF(120),EISOLF(120),
     &   GSSMND(120),GSPMND(120),GPPMND(120),GP2MND(120),HSPMND(120),
     &   ZSMND(120),ZPMND(120)
      COMMON /RM1DSB/ ALPRMD,R0RMD(120),S6RMD,C6RMD(120),USSRMD(120),   LF0310
     &   UPPRMD(120),BETASI(120),BETAPI(120),ALPHAI(120),EISOLI(120)    LF0310
      COMMON /PM6DSB/ ALPP6D,R0P6D(120),S6P6D,C6P6D(120),USSP6D(120),   LF0310
     &   UPPP6D(120),BETASJ(120),BETAPJ(120),ALPHAJ(120),EISOLJ(120)    LF0310
      COMMON /PM6BLO/ USSPM6(120), UPPPM6(120), UDDPM6(120),ZSPM6(120), LF0709
     &ZPPM6(120), ZDPM6(120), BETASX(120), BETAPX(120), BETADX(120),    LF0709
     &GSSPM6(120),GSPPM6(120),GPPPM6(120),GP2PM6(120),HSPPM6(120),      LF0709
     &EISOLX(120), DDPM6(120), QQPM6(120), AMPM6(120), ADPM6(120),      LF0709
     &AQPM6(120), GAUSX1(120,10), GAUSX2(120,10), GAUSX3(120,10),       LF0709
     &ALPPM6(18,18), XPM6(18,18), ZSNPM6(18), ZPNPM6(18), ZDNPM6(18)    LF1009
      COMMON /PMOV1CB/ ALPPMOV1, R0PMOV1(120), S6PMOV1, C6PMOV1(120),   LF1010
     &                 USSPMOV1(120), UPPPMOV1(120), BETASPMOV1(120),   LF1010
     &                 BETAPPMOV1(120), ALPHAPMOV1(120),                LF1010
     &                 EISOLPMOV1(120), GSSPMOV1(120), GSPPMOV1(120),   LF1010
     &                 GPPPMOV1(120), GP2PMOV1(120), HSPPMOV1(120),     LF1010
     &                 ZSPMOV1(120), ZPPMOV1(120),                      LF1010
     &                 AMPMOV1(120), ADPMOV1(120), AQPMOV1(120),        LF1010
     &                 DDPMOV1(120), QQPMOV1(120)                       LF1010
      COMMON /PMO2CB/  PRBETAPMO2 (120,120,2,2),KPAIRPMO2 (120,120,2,2),LF0113
     &                 C0ABPMO2 (120,120), C1ABPMO2 (120,120),          LF0113
     &                 C2ABPMO2 (120,120), PRALPPMO2 (120,120),         LF0113
     &                 C3ABPMO2 (120,120), C3RPWRPMO2 (120,120),        LF0113
     &                 PR3ALPPMO2 (120,120), CPAIRPMO2 (120,120),       LF0113
     &                 DPAIRPMO2 (120,120),                             LF0113
     &                 USSPMO2(120), UPPPMO2(120), ZSPMO2(120),         LF0113
     &                 ZPPMO2(120), GSSPMO2(120), GSPPMO2(120),         LF0113
     &                 GPPPMO2(120), GP2PMO2(120), HSPPMO2(120),        LF0113
     &                 ALPHAPMO2(120), AMPMO2(120), ADPMO2(120),        LF0113
     &                 AQPMO2(120), DDPMO2(120), QQPMO2(120),           LF0113
     &                 EISOLPMO2(120), C6PMO2(120), RVDWPMO2(120),      LF0113
     &                 RPNUMPMO2, RPDENPMO2, DPDENPMO2, SPDENPMO2,      LF0113
     &                 S6PMO2, ALPDPMO2                                 LF0113
      REAL*8           KPAIRPMO2                                        LF0113
      COMMON /PMO2ACB/ PRBETAPMO2A(120,120,2,2),KPAIRPMO2A(120,120,2,2),LF0614
     &                 C0ABPMO2A (120,120), C1ABPMO2A (120,120),        LF0614
     &                 C2ABPMO2A (120,120), PRALPPMO2A (120,120),       LF0614
     &                 C3ABPMO2A (120,120), C3RPWRPMO2A (120,120),      LF0614
     &                 PR3ALPPMO2A (120,120), CPAIRPMO2A (120,120),     LF0614
     &                 DPAIRPMO2A (120,120),                            LF0614
     &                 USSPMO2A(120), UPPPMO2A(120), ZSPMO2A(120),      LF0614
     &                 ZPPMO2A(120), GSSPMO2A(120), GSPPMO2A(120),      LF0614
     &                 GPPPMO2A(120), GP2PMO2A(120), HSPPMO2A(120),     LF0614
     &                 ALPHAPMO2A(120), AMPMO2A(120), ADPMO2A(120),     LF0614
     &                 AQPMO2A(120), DDPMO2A(120), QQPMO2A(120),        LF0614
     &                 EISOLPMO2A(120), C6PMO2A(120), RVDWPMO2A(120),   LF0614
     &                 RPNUMPMO2A, RPDENPMO2A, DPDENPMO2A, SPDENPMO2A,  LF0614
     &                 S6PMO2A, ALPDPMO2A                               LF0614
      REAL*8           KPAIRPMO2A                                       LF0614
      COMMON /TDAMPB/  CPAIR(16,16), DPAIR(16,16),                      LF0312
     &                 RPNUM, RPDEN, DPDEN, SPDEN,                      LF0312
     &                 USEDPR, USECPR                                   LF0312
      LOGICAL          USEDPR, USECPR                                   LF0312

      COMMON /GEOM  / GEO(3,NUMATM)
      PARAMETER (MDUMY=MAXPAR**2-MPACK)
      COMMON /SCRACH/ RXYZ(MPACK), XDUMY(MDUMY)
*
*  COMMON BLOCKS FOR MINDO/3
*
      COMMON /ONELE3 /  USS3(18),UPP3(18)
     1       /ATOMI3 /  EISOL3(18),EHEAT3(18)
     2       /EXPON3 /  ZS3(18),ZP3(18)
*
*  END OF MINDO/3 COMMON BLOCKS
*
      COMMON /EXPONT/ ZS(120),ZP(120),ZD(120)
      COMMON /ATOMIC/ EISOL(120),EHEAT(120)
C  
      COMMON /WMATRX/ WDUMMY(N2ELEC*3),KDUMMY,NBAND(NUMATM)             JZ0315
C     COMMON /WMATRX/ WDUMMY(N2ELEC*2),KDUMMY,NBAND(NUMATM)             JZ0315
C ---------------------------------------------------------------------
C     COMMON BLOCKS FOR PDDG METHODS
C     (INTRODUCED BY MATT REPASKY APRIL 02)
C ---------------------------------------------------------------------
      COMMON /PDDGP/  USSPDG(120), UPPPDG(120), UDDPDG(120), ZSPDG(120),
     1ZPPDG(120), ZDPDG(120), BETASG(120), BETAPG(120), BETADG(120),
     2ALPPDG(120), EISOLG(120), DDPDG(120), QQPDG(120), AMPDG(120),
     3ADPDG(120), AQPDG(120) ,GSSPDG(120), GSPPDG(120), GPPPDG(120),
     4GP2PDG(120), HSPPDG(120),POLVOG(120),PAPDG(120),PBPDG(120),
     5DAPDG(120),DBPDG(120)
      COMMON /IDEAG / GAUSG1(120,10),GAUSG2(120,10),GAUSG3(120,10)
     1               ,NGAUSG(120)
      COMMON /PDDGM/  USSMDG(120), UPPMDG(120), UDDMDG(120), ZSMDG(120),
     1ZPMDG(120), ZDMDG(120), BETASH(120), BETAPH(120), BETADH(120),
     2ALPMDG(120), EISOLH(120), DDMDG(120), QQMDG(120), AMMDG(120),
     3ADMDG(120), AQMDG(120) ,GSSMDG(120), GSPMDG(120), GPPMDG(120),
     4GP2MDG(120), HSPMDG(120),POLVOH(120),PAMDG(120),PBMDG(120),
     5DAMDG(120),DBMDG(120)
      COMMON /W4G   / PREA(120), PREB(120), DA(120), DB(120)      
      COMMON /HPUSED/ HPUSED                                            LF0210
      COMMON /ELEMTS/ ELEMNT(120)                                       LF0210
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION COORD(3,NUMATM), ISWAP(2,20)
      CHARACTER ALLRF2
      CHARACTER*160 KEYWRD
      CHARACTER*80  OLDE(20)*6, ALLREF
      CHARACTER*160 STR, STR2                                           LF0312
      LOGICAL   DEBUG, UHF,EXCI, TRIP, BIRAD, OPEN, USEMM, SLOW, HALFE  IR0494/GL0892
      LOGICAL   HPUSED                                                  LF0210
      CHARACTER ELEMNT*2                                                LF0210
         SAVE                                                           
C
      IF (IP .EQ. 1) GOTO 35                                            0304WH93
C     
      LMINDO= (INDEX(KEYWRD,'MINDO').NE.0)
      LAM1= ((INDEX(KEYWRD,'AM1').NE.0).AND.
     &      (INDEX(KEYWRD,'AM1-D').EQ.0))                               LF0509
      LPM3  = ((INDEX(KEYWRD,'PM3').NE.0).AND.
     &         (INDEX(KEYWRD,'PM3-D').EQ.0))                            LF0309
      LRM1  = ((INDEX(KEYWRD,'RM1').NE.0).AND.
     &         (INDEX(KEYWRD,'RM1-D').EQ.0))                            LF0310
      LPDG  = (INDEX(KEYWRD,'PDG').NE.0)                                ZJJ0706
      LMDG  = (INDEX(KEYWRD,'MDG').NE.0)                                ZJJ0706 
      LAM1D = (INDEX(KEYWRD,'AM1-D').NE.0.AND.                          LF0509
     &         INDEX(KEYWRD,'AM1-D3').EQ.0)                             LF1111
      LAM1D3= (INDEX(KEYWRD,'AM1-D3').NE.0)                             LF1111
      LPM3D = (INDEX(KEYWRD,'PM3-D').NE.0.AND.                          LF0309/LF1211
     &         INDEX(KEYWRD,'PM3-D3').EQ.0)                             LF0309/LF1211
      LPM3D3= (INDEX(KEYWRD,'PM3-D3').NE.0)                             LF1211
      LPM6  =((INDEX(KEYWRD,'PM6').NE.0).AND.                           LF0709
     &        (INDEX(KEYWRD,'PM6-D').EQ.0).AND.                         LF0310
     &        (INDEX(KEYWRD,'PM6G09-D').EQ.0).AND.                      LF0310
     &        (INDEX(KEYWRD,'PM6-D3').EQ.0))                            LF1211
      LPM6D3= (INDEX(KEYWRD,'PM6-D3').NE.0)                             LF1211
      LPM6G9= (INDEX(KEYWRD,'PM6G09').NE.0)                             LF0110
      LMNDOD= (INDEX(KEYWRD,'MNDO-D').NE.0.AND.                         LF0310
     &         INDEX(KEYWRD,'MNDO-D3').EQ.0)                            LF1111
      LMNDOD3=(INDEX(KEYWRD,'MNDO-D3').NE.0)                            LF1111
      LRM1D = (INDEX(KEYWRD,'RM1-D').NE.0.AND.                          LF0310/LF1211
     &         INDEX(KEYWRD,'RM1-D3').EQ.0)                             LF0310/LF1211
      LRM1D3= (INDEX(KEYWRD,'RM1-D3').NE.0)                             LF1211
      LPM6D =((INDEX(KEYWRD,'PM6-D').NE.0).OR.                          LF0310
     &        (INDEX(KEYWRD,'PM6G09-D').NE.0))                          LF0310
C     Need to be careful because sometimes keywords will fall after EXTERNAL= in the input deck.
      LPMOV1= (KEYWRD(1:5).EQ.'PMOV1'.OR.(INDEX(KEYWRD,' PMOV1').NE.0)) LF0412
      LPMOV1A= (KEYWRD(1:6).EQ.'PMOV1A'.OR.                             LF0412
     &                                 (INDEX(KEYWRD,' PMOV1A').NE.0))  LF0412
      IF (KEYWRD(1:4).EQ.'PMO2'.OR.INDEX(KEYWRD,' PMO2').NE.0) THEN     LF0614
        IF (KEYWRD(1:5).EQ.'PMO2A'.OR.INDEX(KEYWRD,' PMO2A').NE.0) THEN LF0614
          LPMO2A=.TRUE.                                                 LF0614
        ELSE                                                            LF0614
          LPMO2=.TRUE.                                                  LF0614
        ENDIF                                                           LF0614
      ENDIF                                                             LF0614
      IF (LPMOV1) LAM1 =.TRUE.                                          LF1010
      IF (LPMO2)  LAM1 =.TRUE.                                          LF0113
      IF (LPMO2A) LAM1 =.TRUE.                                          LF0614
C     Dispersion for PMOv1 is done in ROTATE the same as for PM3-D.
C     The PM3-D dispersion is done if either the keyword PM3-D or the
C     keyword PMOv1 is specified in the input deck.
      IF (LPMOV1.OR.LPMO2.OR.LPMO2A) HPUSED=.TRUE.                      LF1010 / LF0113 / LF0614
C     LPM6G9 is set to .true. if the Gaussian09 implementation for PM6 is
C     desired (LPM6 is still set to true in this case).                 LF0110
      LMNDO = .NOT. (LMINDO.OR.LAM1.OR.LPM3.OR.LRM1.OR.LPDG.OR.LMDG.OR.
     &               LAM1D.OR.LPM3D.OR.LPM6                             LF0909
     &               .OR.LMNDOD.OR.LRM1D.OR.LPM6D                       LF0310
     &               .OR.LPMOV1.OR.LMNDOD3.OR.LAM1D3.OR.                LF1010/LF1111
     &               LPM3D3.OR.LRM1D3.OR.LPM6D3.OR.LPMO2.OR.LPMO2A)     LF1211/LF0113/LF0614
C     Note that LMNDO is .false. for MNDO-D, but .true. for MNDO.       LF0310
C     Set AM1PM3 to .true. if AM1-style gaussians are used in the nuclear energy.
      AM1PM3 = (LAM1.OR.LPM3.OR.LRM1.OR.LPDG.OR.LAM1D.OR.LPM3D.OR.LPM6  LF0909
     &          .OR.LRM1D.OR.LPM6D.OR.LAM1D3.OR.                        LF0310/LF1111
     &          LPM3D3.OR.LRM1D3.OR.LPM6D3)                             LF1211
C      AM1PM3 INDICATES WHETHER AM1-TYPE GAUSSIANS ARE USED FOR CORE
C      REPULSION FUNCTION.  NOTE "MINDO", "MNDO", "MDG", AND "MNDO-D" DO NOT USE THEM.
      LSDAMP= (INDEX(KEYWRD,'SDAMP').NE.0)                              LF0310
C     LF0412: If PMOv1 or PMO2 or PMO2a method is used turn off Stone's damping
C             regardless of whether SDAMP keyword is present.
      IF (LPMOV1.OR.LPMO2.OR.LPMO2A) LSDAMP=.FALSE.                     LF1010 / LF0113 / LF0614
      LHGDAMP= (INDEX(KEYWRD,'HGDAMP').NE.0)                            LF0312
      LTDAMP= (INDEX(KEYWRD,'TDAMP').NE.0)                              LF0312
      LHGDISP= (INDEX(KEYWRD,'HGDISP').NE.0)                            LF0312
      LTDISP= (INDEX(KEYWRD,'TDISP').NE.0)                              LF0312
      USEDPR= (INDEX(KEYWRD,'DPAIR').NE.0)                              LF0312
      USECPR= (INDEX(KEYWRD,'CPAIR').NE.0)                              LF0312
      LDAMP5= (INDEX(KEYWRD,'DAMP5').NE.0)                              LF0312
      IF (LHGDISP.AND.LTDISP) THEN                                      LF0312
        IF (DOPRNT) WRITE(6,*) "CAN ONLY SPECIFY ONE DISPERSION FORM."  LF0312
        RETURN 1                                                        LF0312
      ENDIF                                                             LF0312 
      IF ((LSDAMP.AND.(LHGDAMP.OR.LTDAMP)).OR.(LHGDAMP.AND.LTDAMP))THEN LF0312
        IF (DOPRNT) WRITE(6,*) "CAN ONLY SPECIFY ONE DAMPING FUNCTIONAL LF0312
     &FORM FOR DISPERSION."                                             LF0312
        RETURN 1                                                        LF0312
      ENDIF                                                             LF0312
      IF (LDAMP5.AND.(LHGDAMP.OR.LTDAMP.OR.LSDAMP)) THEN                LF0312
        IF (DOPRNT) WRITE(6,*) "CAN ONLY SPECIFY ONE DAMPING FUNCTIONAL LF0312
     &FORM FOR DISPERSION."                                             LF0312
        RETURN 1                                                        LF0312
      ENDIF                                                             LF0312
      if (LHGDAMP) then                                                 LF0312
        alpmnd=6.0d0
        alpamd=6.0d0
        alppmd=6.0d0
        alprmd=6.0d0
        alpp6d=6.0d0
      endif                                                             LF0312
      if (LTDAMP) then                                                  LF0312
        if (index(keywrd,'TDAMP=').eq.0) then                           LF0312
          if (doprnt) write(6,'(/A)') "Using default values for TDAMP." LF0312
          rpnum=8.0d0                                                   LF0312
          rpden=2.0d0                                                   LF0312
          dpden=2.0d0                                                   LF0312
          spden=4.0d0                                                   LF0312
        else                                                            LF0312
          i=index(keywrd,'TDAMP=')                                      LF0312
          i=i+6                                                         LF0312
          str=keywrd(i:)                                                LF0312...

          i=index(str,',')
          str2=str(1:i-1)
c#          write(6,*) "Found first value after TDAMP: "//str2//"###"
          rpnum=reada(str2,1)
c#          write(6,*) "Numerically it is ",rpnum
          str=str(i+1:)

          i=index(str,',')
          str2=str(1:i-1)
c#          write(6,*) "Found second value after TDAMP: "//str2//"###"
          rpden=reada(str2,1)
c#          write(6,*) "Numerically it is ",rpden
          str=str(i+1:)

          i=index(str,',')
          str2=str(1:i-1)
c#          write(6,*) "Found third value after TDAMP: "//str2//"###"
          dpden=reada(str2,1)
c#          write(6,*) "Numerically it is ",dpden
          str=str(i+1:)

          i=index(str,' ')
          str2=str(1:i-1)
c#          write(6,*) "Found fourth value after TDAMP: "//str2//"###"
          spden=reada(str2,1)
c#          write(6,*) "Numerically it is ",spden
          str=str(i+1:)
          if (doprnt) then
            write(6,'(/A)') "     For TDAMP option using:"
            write(6,'(A,F12.6)') "  Using RPNUM=",rpnum
            write(6,'(A,F12.6)') "  Using RPDEN=",rpden
            write(6,'(A,F12.6)') "  Using DPDEN=",dpden
            write(6,'(A,F12.6)') "  Using SPDEN=",spden
          endif
        endif                                                           ... LF0312
      endif                                                             LF0312
      
      IF (LPMOV1.OR.LPMO2.OR.LPMO2A) THEN                               LF0312 / LF0113 / LF0614
C       Make sure that PMOv1 uses the PM3-D dispersion.  See also ROTATE subroutine.
        LSDAMP=.FALSE.                                                  LF1010
        LHGDAMP=.FALSE.                                                 LF0312
        LTDAMP=.FALSE.                                                  LF0312
        LDAMP5=.FALSE.                                                  LF0312
        LTDISP=.FALSE.                                                  LF0312
        LHGDISP=.FALSE.                                                 LF0312
      ENDIF                                                             LF0312
      DSPMET= (LAM1D.OR.LPM3D.OR.LMNDOD.OR.LRM1D.OR.LPM6D.OR.LPMOV1     LF0310/LF0412
     &         .OR.LPMO2.OR.LPMO2A)                                     LF0113/LF0614
      D3METH= (LMNDOD3.OR.LAM1D3.OR.LPM3D3.OR.LRM1D3.OR.LPM6D3)         LF1111/LF1211
      ANALYT = (INDEX(KEYWRD,'ANALYT').NE.0)                            LF0909
c#      ANALYT = ( (INDEX(KEYWRD,'ANALYT').NE.0) .OR.                     LF0909
c#     &           (INDEX(KEYWRD,'PMOV1') .NE.0) )                        LF1010

      DEBUG = (INDEX(KEYWRD,'MOLDAT').NE.0)
      UHF=(INDEX(KEYWRD,'UHF') .NE. 0)
      KHARGE=0
      I=INDEX(KEYWRD,'CHARGE')
      IF(I.NE.0) KHARGE=READA(KEYWRD,I)
      ELECS=-KHARGE
      NDORBS=0
      ATHEAT=0.D0
      EAT=0.D0
      NUMAT=0
      IF (LMNDO.OR.LMNDOD.OR.LMNDOD3) THEN                              LF0709/LF0909/LF0310/LF1111
C
C    SWITCH IN MNDO PARAMETERS
C
C
C       ZERO OUT GAUSSIAN 1 FOR CARBON.  THIS WILL BE USED IN
C       ROTATE TO DECIDE WHETHER OR NOT TO USE AM1-TYPE GAUSSIANS
C
         GAUSS1(6,1)=0.D0
         DO 10 I=1,120
            IF(.NOT.LMINDO) POLVOL(I)=POLVOM(I)
            ZS(I)=ZSM(I)
            ZP(I)=ZPM(I)
            ZD(I)=ZDM(I)
            USS(I)=USSM(I)
            UPP(I)=UPPM(I)
            UDD(I)=UDDM(I)
            BETAS(I)=BETASM(I)
            BETAP(I)=BETAPM(I)
            BETAD(I)=BETADM(I)
            ALP(I)=ALPM(I)
            EISOL(I)=EISOLM(I)
            DD(I)=DDM(I)
            QQ(I)=QQM(I)
            AM(I)=AMM(I)
            AD(I)=ADM(I)
            AQ(I)=AQM(I)
            GSS(I)=GSSM(I)
            GPP(I)=GPPM(I)
            GSP(I)=GSPM(I)
            GP2(I)=GP2M(I)
            HSP(I)=HSPM(I)
   10    CONTINUE
      ELSEIF(LPM3.OR.LPM3D.OR.LPM3D3) THEN                              LF0709/LF0909/LF1211
C
C    SWITCH IN PM3 PARAMETERS
C      (FOR PM3-D AND PM6 SOME OF THESE PARAMETERS WILL BE OVERWRITTEN BELOW)    
C
         DO 30 I=1,120
            DO 20 J=1,10
               GAUSS1(I,J)=GAUSP1(I,J)
               GAUSS2(I,J)=GAUSP2(I,J)
   20       GAUSS3(I,J)=GAUSP3(I,J)
            POLVOL(I)=POLVOP(I)
            ZS(I)=ZSPM3(I)
            ZP(I)=ZPPM3(I)
            ZD(I)=ZDPM3(I)
            USS(I)=USSPM3(I)
            UPP(I)=UPPPM3(I)
            UDD(I)=UDDPM3(I)
            BETAS(I)=BETASP(I)
            BETAP(I)=BETAPP(I)
            BETAD(I)=BETADP(I)
            ALP(I)=ALPPM3(I)
            EISOL(I)=EISOLP(I)
            DD(I)=DDPM3(I)
            QQ(I)=QQPM3(I)
            AM(I)=AMPM3(I)
            AD(I)=ADPM3(I)
            AQ(I)=AQPM3(I)
            GSS(I)=GSSPM3(I)
            GPP(I)=GPPPM3(I)
            GSP(I)=GSPPM3(I)
            GP2(I)=GP2PM3(I)
            HSP(I)=HSPPM3(I)
   30    CONTINUE
      ENDIF
C                                                                       LF0310
C    SWITCH IN MNDO-D PARAMETERS                                          .. 
C                                                                         ..
Cobs       THE MNDO-D METHOD SUBSTITUTES SOME PARAMETERS IN OVER TYPICAL     ..
Cobs       MNDO PARAMETERS FOR SOME ELEMENTS.                                ..
C
C   *** MNDO-D IS BEING REDEFINED (FOR VERSION 5.019mn) AS MNDO WITH ONLY
C       PM3-D DISPERSION AND NO OTHER OVERRIDING PARAMETERS (THAT WERE 
C       USED FOR VERSION 5.018mn).
C                                                                         ..
Cobs      IF (LMNDOD) THEN                                                    ..
cobs          DO 95 I=1,120                                                   ..                 
Cobs ONLY H, C, N, O, F, AND S ARE CURRENTLY PARAMETERIZED FOR AM1-D, THE    ..
Cobs PARAMETERS BEING USED CURRENTLY FOR MNDO-D.                             ..
cobs            IF (I.EQ.1.OR.I.EQ.6.OR.I.EQ.7.OR.I.EQ.8.OR.I.EQ.16) THEN     ..
cobs             USS(I)=USSMND(I)                                             .. 
cobs             UPP(I)=UPPMND(I)                                             ..  
cobs             BETAS(I)=BETASF(I)                                           ..
cobs             BETAP(I)=BETAPF(I)                                           ..
cobs             ALP(I)=ALPHAF(I)                                             ..
cobs             EISOL(I)=EISOLF(I)                                           ..
cobs            ENDIF                                                         ..
cobs   95     CONTINUE                                                        ..
Cobs      ENDIF                                                             LF0310
C                                                                       LF0409
C    SWITCH IN PM3-D PARAMETERS                                           .. 
C                                                                         ..
C       THE PM3-D METHOD SUBSTITUTES SOME PARAMETERS IN OVER TYPICAL      ..
C       PM3 PARAMETERS FOR SOME ELEMENTS.                                 ..
C                                                                         ..
      IF (LPM3D) THEN                                                     ..
          DO 96 I=1,120                                                   ..                 
C ONLY H, C, N, O, F, AND S ARE CURRENTLY PARAMETERIZED FOR PM3-D         ..
            IF (I.EQ.1.OR.I.EQ.6.OR.I.EQ.7.OR.I.EQ.8.OR.I.EQ.16) THEN     ..
             USS(I)=USSPMD(I)                                             .. 
             UPP(I)=UPPPMD(I)                                             ..  
             BETAS(I)=BETASD(I)                                           ..
             BETAP(I)=BETAPD(I)                                           ..
             ALP(I)=ALPHAD(I)                                             ..
             EISOL(I)=EISOLD(I)                                           ..
            ENDIF                                                         ..
   96     CONTINUE                                                        ..
      ENDIF                                                             LF0409
*                                                                       LF0709
*    SWITCH IN PM6 PARAMETERS                                             .. 
*                                                                         ..
C       THE PM6 METHOD SUBSTITUTES SOME PARAMETERS IN OVER TYPICAL        ..
C       PM3 PARAMETERS FOR SOME ELEMENTS.                                 ..
C                                                                       LF0709
      IF (LPM6.OR.LPM6D.OR.LPM6D3) THEN                                 LF0310/LF1211
          DO 97 I=1,120                                                 LF0709
C PRESENTLY ONLY THE FIRST 18 ELEMENTS HAVE DATA INCLUDED FOR PM6.      LF1009
            IF (I.LE.18) THEN                                           LF0709
             DO 23 J=1,10                                                 ..
             GAUSS1(I,J)=GAUSX1(I,J)                                    ..
             GAUSS2(I,J)=GAUSX2(I,J)                                    ..
   23        GAUSS3(I,J)=GAUSX3(I,J)                                    ..
             USS(I)=USSPM6(I)                                             .. 
             UPP(I)=UPPPM6(I)                                             ..  
             UDD(I)=UDDPM6(I)                                             ..
             BETAS(I)=BETASX(I)                                           ..
             BETAP(I)=BETAPX(I)                                           ..
             BETAD(I)=BETADX(I)                                           ..
             EISOL(I)=EISOLX(I)                                           ..
             ZS(I)=ZSPM6(I)                                               ..
             ZP(I)=ZPPM6(I)                                               ..
             ZD(I)=ZDPM6(I)                                               ..
             GSS(I)=GSSPM6(I)                                             ..
             GSP(I)=GSPPM6(I)                                             ..
             GPP(I)=GPPPM6(I)                                             ..
             GP2(I)=GP2PM6(I)                                             ..
             HSP(I)=HSPPM6(I)                                             ..
             DD(I)=DDPM6(I)                                               ..
             QQ(I)=QQPM6(I)                                               ..
             AM(I)=AMPM6(I)                                               ..
             AD(I)=ADPM6(I)                                               ..
             AQ(I)=AQPM6(I)                                             LF0709
            ELSE                                                        LF1009
             USS(I)=0.D0                                                  ..
             UPP(I)=0.D0                                                  ..
             UDD(I)=0.D0                                                  ..
             BETAS(I)=0.D0                                                ..
             BETAP(I)=0.D0                                                ..
             BETAD(I)=0.D0                                                ..
             EISOL(I)=0.D0                                                ..
             ZS(I)=0.D0                                                   ..
             ZP(I)=0.D0                                                   ..
             ZD(I)=0.D0                                                   ..
             GSS(I)=0.D0                                                  ..
             GSP(I)=0.D0                                                  ..
             GPP(I)=0.D0                                                  ..
             GP2(I)=0.D0                                                  ..
             HSP(I)=0.D0                                                  ..
             DD(I)=0.D0                                                   ..
             QQ(I)=0.D0                                                   ..
             AM(I)=0.D0                                                   ..
             AD(I)=0.D0                                                   ..
             AQ(I)=0.D0                                                   ..
            ENDIF                                                         ..
   97     CONTINUE                                                      LF1009
          CALL CALPAR(*9999)                                            LF1009 CSTP(call)
      ENDIF                                                             LF0709

C                                                                       LF0310
C    SWITCH IN PM6-D PARAMETERS                                           .. 
C                                                                         ..
Cobs       THE PM6-D METHOD SUBSTITUTES SOME PARAMETERS IN OVER TYPICAL     ..
Cobs       MNDO PARAMETERS FOR SOME ELEMENTS.                                ..
C
C   *** PM6-D IS BEING REDEFINED (FOR VERSION 5.019mn) AS PM6 WITH ONLY
C       PM3-D DISPERSION AND NO OTHER OVERRIDING PARAMETERS (THAT WERE
C       USED FOR VERSION 5.018mn).
Cobs      IF (LPM6D) THEN                                                   LF0310
Cobs          DO 91 I=1,120                                                   ..
Cobs LF0310: NOTE THAT ONLY H, C, N, O, F, AND S ARE CURRENTLY PARAMETERIZED ..
Cobs         FOR THE AM1-D AND PM3-D METHODS.                                ..
Cobs            IF (I.EQ.1.OR.I.EQ.6.OR.I.EQ.7.OR.I.EQ.8.OR.I.EQ.16) THEN     ..
Cobs             USS(I)=USSP6D(I)                                             .. 
Cobs             UPP(I)=UPPP6D(I)                                             ..  
Cobs             BETAS(I)=BETASJ(I)                                           ..
Cobs             BETAP(I)=BETAPJ(I)                                           ..
Cobs             ALP(I)=ALPHAJ(I)                                             ..
Cobs             EISOL(I)=EISOLJ(I)                                           ..
Cobs            ENDIF                                                         ..
Cobs   91     CONTINUE                                                        ..
Cobs      ENDIF                                                             LF0310


*                                                                       LF1010
*    SWITCH IN PMOv1 PARAMETERS                                         LF1010
*                                                                       LF1010

      IF (LPMOV1.AND.ANALYT) THEN                                       LF1110
        IF (DOPRNT) WRITE(6,'(/A//)')                                   LF1110
     &     'CANNOT USE ANALYTICAL DERIVATIVES WITH PMOv1.'              LF1110
        RETURN 1                                                        LF1110
      ENDIF                                                             LF1110
      IF (ANALYT.AND.(PMODS(1).OR.PMODS(2).OR.PMODS(3).OR.PMODS(4)))THEN LF1110
            IF (DOPRNT) WRITE(6,'(/A//)')                               LF1110
     &         'CANNOT USE ANALYTICAL DERIVATIVES WITH PMOv1.'          LF1110
            RETURN 1                                                    LF1110
      ENDIF                                                             LF1110

      IF (LPMOV1) THEN                                                  LF1010
          DO 92 I=1,120
            IF (I.EQ.8.OR.I.EQ.108) THEN
              USS(I)  = USSPMOV1(I)
              UPP(I)  = UPPPMOV1(I)
              ZS(I)   = ZSPMOV1(I)
              ZP(I)   = ZPPMOV1(I)
              BETAS(I)= BETASPMOV1(I)
              BETAP(I)= BETAPPMOV1(I)
              GSS(I)  = GSSPMOV1(I)
              GSP(I)  = GSPPMOV1(I)
              GPP(I)  = GPPPMOV1(I)
              GP2(I)  = GP2PMOV1(I)
              HSP(I)  = HSPPMOV1(I)
              ALP(I)  = ALPHAPMOV1(I)
              AM(I)   = AMPMOV1(I)
              AD(I)   = ADPMOV1(I)
              AQ(I)   = AQPMOV1(I)
              DD(I)   = DDPMOV1(I)
              QQ(I)   = QQPMOV1(I)
              EISOL(I)= EISOLPMOV1(I)
              GAUSS1  = 0.0D0
              HPUSED  = .TRUE.
            ENDIF
          PMODS=.FALSE.                                                  LF1010
          HPUSED=.TRUE.
          PMODS(1)=.TRUE.
          PMODS(2)=.TRUE.
          PMODS(3)=.TRUE.
          PMODS(4)=.TRUE.
C         The PMODVL(x) array values are already set in the BLOCK DATA file and
C         also in the RESETC subroutine.
   92     CONTINUE
      ENDIF                                                             LF1010

*                                                                       LF0113
*    SWITCH IN PMO2 PARAMETERS                                          LF0113
*                                                                       LF0113
      IF (LPMO2.AND.ANALYT) THEN                                        LF0113
        IF (DOPRNT) WRITE(6,'(/A//)')                                   LF0113
     &     'CANNOT USE ANALYTICAL DERIVATIVES WITH PMO2.'               LF0113
        RETURN 1                                                        LF0113
      ENDIF                                                             LF0113
      IF (LPMO2)  THEN                                                  LF0113...
          ! Zero all undefined beta and k parameters (may be unnecessary
          ! as they are never used anyway).
          PRBETA=0.0D0
          KPAIR =0.0D0
          ! Start copying PMO2 parameters into global parameter
          ! variables.
          DO 93 I=1,120
            IF (I.EQ.6.OR.I.EQ.8.OR.I.EQ.108) THEN
              K=I                 ! Done in order to double write element 108 parameters to element 9.
  300         CONTINUE
              ! Loop for pairwise parameters.
              DO 94 J=1,120   
                IF (J.EQ.6.OR.J.EQ.8.OR.J.EQ.108) THEN
                  L=J             ! Done in order to double write element 108 parameters to element 9.
  301             CONTINUE
                  KK=K           ! The following global variables are only indexed up to 16 not 120.
                  LL=L
                  IF (KK.EQ.108) KK=9
                  IF (LL.EQ.108) LL=9
                  DO II=1,2
                   DO JJ=1,2
                    PRBETA (KK,LL,II,JJ) = PRBETAPMO2 (I,J,II,JJ)
                    KPAIR  (KK,LL,II,JJ) = KPAIRPMO2  (I,J,II,JJ)
                   ENDDO
                  ENDDO
                  C0AB   (KK,LL) = C0ABPMO2   (I,J)
                  C1AB   (KK,LL) = C1ABPMO2   (I,J)
                  C2AB   (KK,LL) = C2ABPMO2   (I,J)
                  PRALP  (KK,LL) = PRALPPMO2  (I,J)
                  C3AB   (KK,LL) = C3ABPMO2   (I,J)
                  C3RPWR (KK,LL) = C3RPWRPMO2 (I,J)
                  PR3ALP (KK,LL) = PR3ALPPMO2 (I,J)
                  CPAIR  (KK,LL) = CPAIRPMO2  (I,J)
                  DPAIR  (KK,LL) = DPAIRPMO2  (I,J)
                  IF (J.EQ.108.AND.L.EQ.108) THEN
                    L=9            ! Loop back through and assign parameters to element 9 (already assigned to element 108).
                    GOTO 301
                  ENDIF
                ENDIF
   94         CONTINUE
              ! Now do monoatomic parameters.
              USS(K)  = USSPMO2(I)
              UPP(K)  = UPPPMO2(I)
              ZS(K)   = ZSPMO2(I)
              ZP(K)   = ZPPMO2(I)
              GSS(K)  = GSSPMO2(I)
              GSP(K)  = GSPPMO2(I)
              GPP(K)  = GPPPMO2(I)
              GP2(K)  = GP2PMO2(I)
              HSP(K)  = HSPPMO2(I)
              ALP(K)  = ALPHAPMO2(I)
              AM(K)   = AMPMO2(I)
              AD(K)   = ADPMO2(I)
              AQ(K)   = AQPMO2(I)
              DD(K)   = DDPMO2(I)
              QQ(K)   = QQPMO2(I)
              EISOL(K)= EISOLPMO2(I)
              C6PMD(K)= C6PMO2(I)       ! In subroutine ROTATE PMO2 uses PM6-D dispersion parameters.
              R0PMD(K)= RVDWPMO2(I)     ! In subroutine ROTATE PMO2 uses PM6-D dispersion parameters.
              IF (I.EQ.108.AND.K.EQ.108) THEN
                K=9              ! Loop back through and assign parameters to element 9 (already assigned to element 108).
                GOTO 300
              ENDIF
            ENDIF
          GAUSS1  = 0.0D0        ! No AM1-type gaussians in core-core function.
          HPUSED  = .TRUE.       ! Ensure the usual flags are properly set for the PMO2 method.
          PMODS=.FALSE.          
          PMODS(4)=.TRUE.
          PMODS(5)=.TRUE.
          PMODS(6)=.TRUE.
          PMODS(7)=.TRUE.
          LSDAMP=.FALSE.         ! Make sure that PMO2 uses the proper PM3-D dispersion (same as MNDO-D dispersion).  See also ROTATE subroutine.
          LHGDAMP=.FALSE.
          LTDAMP=.FALSE.
          LDAMP5=.FALSE.
          LTDISP=.FALSE.
          LHGDISP=.FALSE.
          RPNUM  = RPNUMPMO2
          RPDEN  = RPDENPMO2
          DPDEN  = DPDENPMO2
          SPDEN  = SPDENPMO2
          S6PMD  = S6PMO2        ! In subroutine ROTATE PMO2 uses PM6-D dispersion parameters.
          ALPPMD = ALPDPMO2      ! In subroutine ROTATE PMO2 uses PM6-D dispersion parameters.
          C6PMD(1)= C6PMO2(108)       ! In subroutine ROTATE dispersion uses index 1 for ALL hydrogens.
          R0PMD(1)= RVDWPMO2(108)     ! In subroutine ROTATE dispersion uses index 1 for ALL hydrogens.
C         All the PMODVL(x) array values are already set in the BLOCK DATA file and
C         also in the RESETC subroutine, so there is no need to do so here.
   93     CONTINUE
C     For the PMO2 method the only thing left to do is copy all
C     parameters for element 108 to element 9 for the Hp hydrogens.
      ENDIF                                                             ... LF0113

*                                                                       LF0614
*    SWITCH IN PMO2A PARAMETERS                                         LF0614
*                                                                       LF0614
      IF (LPMO2A.AND.ANALYT) THEN                                       LF0614
        IF (DOPRNT) WRITE(6,'(/A//)')                                   LF0614
     &     'CANNOT USE ANALYTICAL DERIVATIVES WITH PMO2a.'              LF0614
        RETURN 1                                                        LF0614
      ENDIF                                                             LF0614
      IF (LPMO2A)  THEN                                                 LF0614...
          ! Zero all undefined beta and k parameters (may be unnecessary
          ! as they are never used anyway).
          PRBETA=0.0D0
          KPAIR =0.0D0
          ! Start copying PMO2a parameters into global parameter
          ! variables.
          DO 403 I=1,120
            IF (I.EQ.6.OR.I.EQ.7.OR.I.EQ.8.OR.I.EQ.16.OR.I.EQ.108) THEN
              K=I                 ! Done in order to double write element 108 parameters to element 9.
  400         CONTINUE
              ! Loop for pairwise parameters.
              DO 404 J=1,120   
                IF (J.EQ.6.OR.J.EQ.7.OR.J.EQ.8.OR.J.EQ.16.OR.
     &                                             J.EQ.108) THEN
                  L=J             ! Done in order to double write element 108 parameters to element 9.
  401             CONTINUE
                  KK=K           ! The following global variables are only indexed up to 16 not 120.
                  LL=L
                  IF (KK.EQ.108) KK=9
                  IF (LL.EQ.108) LL=9
                  DO II=1,2
                   DO JJ=1,2
                    PRBETA (KK,LL,II,JJ) = PRBETAPMO2A (I,J,II,JJ)
                    KPAIR  (KK,LL,II,JJ) = KPAIRPMO2A  (I,J,II,JJ)
                   ENDDO
                  ENDDO
                  C0AB   (KK,LL) = C0ABPMO2A   (I,J)
                  C1AB   (KK,LL) = C1ABPMO2A   (I,J)
                  C2AB   (KK,LL) = C2ABPMO2A   (I,J)
                  PRALP  (KK,LL) = PRALPPMO2A  (I,J)
                  C3AB   (KK,LL) = C3ABPMO2A   (I,J)
                  C3RPWR (KK,LL) = C3RPWRPMO2A (I,J)
                  PR3ALP (KK,LL) = PR3ALPPMO2A (I,J)
                  CPAIR  (KK,LL) = CPAIRPMO2A  (I,J)
                  DPAIR  (KK,LL) = DPAIRPMO2A  (I,J)
                  IF (J.EQ.108.AND.L.EQ.108) THEN
                    L=9            ! Loop back through and assign parameters to element 9 (already assigned to element 108).
                    GOTO 401
                  ENDIF
                ENDIF
  404         CONTINUE
              ! Now do monoatomic parameters.
              USS(K)  = USSPMO2A(I)
              UPP(K)  = UPPPMO2A(I)
              ZS(K)   = ZSPMO2A(I)
              ZP(K)   = ZPPMO2A(I)
              GSS(K)  = GSSPMO2A(I)
              GSP(K)  = GSPPMO2A(I)
              GPP(K)  = GPPPMO2A(I)
              GP2(K)  = GP2PMO2A(I)
              HSP(K)  = HSPPMO2A(I)
              ALP(K)  = ALPHAPMO2A(I)
              AM(K)   = AMPMO2A(I)
              AD(K)   = ADPMO2A(I)
              AQ(K)   = AQPMO2A(I)
              DD(K)   = DDPMO2A(I)
              QQ(K)   = QQPMO2A(I)
              EISOL(K)= EISOLPMO2A(I)
              C6PMD(K)= C6PMO2A(I)       ! In subroutine ROTATE PMO2a uses PM6-D dispersion parameters.
              R0PMD(K)= RVDWPMO2A(I)     ! In subroutine ROTATE PMO2a uses PM6-D dispersion parameters.
              IF (I.EQ.108.AND.K.EQ.108) THEN
                K=9              ! Loop back through and assign parameters to element 9 (already assigned to element 108).
                GOTO 400
              ENDIF
            ENDIF
          GAUSS1  = 0.0D0        ! No AM1-type gaussians in core-core function.
          HPUSED  = .TRUE.       ! Ensure the usual flags are properly set for the PMO2a method.
          PMODS=.FALSE.          
          PMODS(4)=.TRUE.
          PMODS(5)=.TRUE.
          PMODS(6)=.TRUE.
          PMODS(7)=.TRUE.
          LSDAMP=.FALSE.         ! Make sure that PMO2a uses the proper PM3-D dispersion (same as MNDO-D dispersion).  See also ROTATE subroutine.
          LHGDAMP=.FALSE.
          LTDAMP=.FALSE.
          LDAMP5=.FALSE.
          LTDISP=.FALSE.
          LHGDISP=.FALSE.
          RPNUM  = RPNUMPMO2A
          RPDEN  = RPDENPMO2A
          DPDEN  = DPDENPMO2A
          SPDEN  = SPDENPMO2A
          S6PMD  = S6PMO2A        ! In the subroutine ROTATE, PMO2a uses PM6-D dispersion parameters.
          ALPPMD = ALPDPMO2A      ! In the subroutine ROTATE, PMO2a uses PM6-D dispersion parameters.
          C6PMD(1)= C6PMO2A(108)       ! In subroutine ROTATE dispersion uses index 1 for ALL hydrogens.
          R0PMD(1)= RVDWPMO2A(108)     ! In subroutine ROTATE dispersion uses index 1 for ALL hydrogens.
C         All the PMODVL(x) array values are already set in the BLOCK DATA file and
C         also in the RESETC subroutine, so there is no need to do so here.
  403     CONTINUE
C     For the PMO2a method the only thing left to do is copy all
C     parameters for element 108 to element 9 for the Hp hydrogens.
      ENDIF                                                             ... LF0614


*                                                                       LF0509
*    SWITCH IN AM1-D PARAMETERS                                           .. 
*                                                                         ..
C       THE AM1-D METHOD SUBSTITUTES SOME PARAMETERS IN OVER TYPICAL      ..
C       AM1 PARAMETERS FOR SOME ELEMENTS.                                 ..
C                                                                         ..
      IF (LAM1D) THEN                                                     ..
          DO 98 I=1,120                                                   ..
C ONLY H, C, N, O, F, AND S ARE CURRENTLY PARAMETERIZED FOR AM1-D.        ..
            IF (I.EQ.1.OR.I.EQ.6.OR.I.EQ.7.OR.I.EQ.8.OR.I.EQ.16) THEN     ..
             USS(I)=USSAMD(I)                                             .. 
             UPP(I)=UPPAMD(I)                                             ..  
             BETAS(I)=BETASE(I)                                           ..
             BETAP(I)=BETAPE(I)                                           ..
             ALP(I)=ALPHAE(I)                                             ..
             EISOL(I)=EISOLE(I)                                           ..
            ENDIF                                                         ..
   98     CONTINUE                                                        ..
      ENDIF                                                             LF0509
C
      IF (IP .EQ. 0) RETURN                                             0304WH93
C *****************************************************************************
   35 CONTINUE

C     LF0210: If using the atom symbol "Hp" (for hydrogens with p orbitals)
C             then substitute parameters for Hp over fluorine parameters.
      IF (HPUSED) THEN 
            DO 53 J=1,10
               GAUSS1(9,J)=GAUSS1(108,J)
               GAUSS2(9,J)=GAUSS2(108,J)
   53       GAUSS3(9,J)=GAUSS3(108,J)
            ZS(9)=ZS(108)
            ZP(9)=ZP(108)
            ZD(9)=ZD(108)
            USS(9)=USS(108)
            UPP(9)=UPP(108)
            UDD(9)=UDD(108)
            BETAS(9)=BETAS(108)
            BETAP(9)=BETAP(108)
            BETAD(9)=BETAD(108)
            ALP(9)=ALP(108)
            EISOL(9)=EISOL(108)
            DD(9)=DD(108)
            QQ(9)=QQ(108)
            AM(9)=AM(108)
            AD(9)=AD(108)
            AQ(9)=AQ(108)
            GSS(9)=GSS(108)
            GPP(9)=GPP(108)
            GSP(9)=GSP(108)
            GP2(9)=GP2(108)
            HSP(9)=HSP(108)
            ATOMIP(9)=ATOMIP(108)                                       LF0410
C            NATORB(9)=NATORB(108)                                       LF0410

            DO 54 K=1,17
   54       ALLREF(9,K)=ALLREF(108,K)
C            CORE(9)=1
            CORE(9)=CORE(108)
            EHEAT(9)=EHEAT(108)
            ELEMNT(9)=ELEMNT(108)
 
            C6AMD(9)=C6AMD(108)
            C6PMD(9)=C6PMD(108)
            C6MND(9)=C6MND(108)
            C6RMD(9)=C6RMD(108)
            C6P6D(9)=C6P6D(108)
            R0AMD(9)=R0AMD(108)
            R0PMD(9)=R0PMD(108)
            R0MND(9)=R0MND(108)
            R0RMD(9)=R0RMD(108)
            R0P6D(9)=R0P6D(108)
      ENDIF

C     LF0410: Element no. 9 (Hp) needs its dispersion parameters copied in for the
C             dispersion methods, AM1-D, PM3-D, etc. 
      IF (HPUSED.AND.DSPMET) THEN                                       LF0410
            IF (LAM1D) THEN
                R0AMD(9)=R0AMD(108)
                C6AMD(9)=C6AMD(108)
            ENDIF 
            IF (LPM3D) THEN
                R0PMD(9)=R0PMD(108)
                C6PMD(9)=C6PMD(108)
            ENDIF 
            IF (LMNDOD) THEN
                R0MND(9)=R0MND(108)
                C6MND(9)=C6MND(108)
            ENDIF 
            IF (LRM1D) THEN
                R0RMD(9)=R0RMD(108)
                C6RMD(9)=C6RMD(108)
            ENDIF 
            IF (LPM6D) THEN
                R0P6D(9)=R0P6D(108)
                C6P6D(9)=C6P6D(108)
            ENDIF
      ENDIF

C     LF0410: When using the Hp atoms copy the usual hydrogen element
C             PM6 diatomic parameters (not done above).
      IF (HPUSED.AND.(LPM6.OR.LPM6D)) THEN                              LF0410
         DO 71 I=1,18
            IF (I.LT.9) THEN
               ALPPM6(9,I)=ALPPM6(I,1)
               XPM6(9,I)=XPM6(I,1)
            ELSE
               ALPPM6(I,9)=ALPPM6(I,1)
               XPM6(I,9)=XPM6(I,1)
            ENDIF
   71    CONTINUE
      ENDIF

C
C        SWAP IN OLD PARAMETERS FOR ELEMENTS.  OLDE CONTAINS THE
C        CHARACTER NAME OF THE ELEMENT, AND ISWAP(1,1:NEWELE) CONTAINS
C        THE ATOMIC NUMBER OF THE ELEMENT. ISWAP(2,1:NEWELE) CONTAINS
C        THE STORAGE ADDRESS OF THE OLD SET OF PARAMETERS.
C

       IF( LPDG ) THEN                                                  ZJJ0706

*
*    SWITCH IN PDDG/PM3 PARAMETERS
*
         DO 32 I=1,120
            DO 22 J=1,10
               GAUSS1(I,J)=GAUSG1(I,J)
               GAUSS2(I,J)=GAUSG2(I,J)
   22       GAUSS3(I,J)=GAUSG3(I,J)
            POLVOL(I)=POLVOG(I)
            ZS(I)=ZSPDG(I)
            ZP(I)=ZPPDG(I)
            ZD(I)=ZDPDG(I)
            USS(I)=USSPDG(I)
            UPP(I)=UPPPDG(I)
            UDD(I)=UDDPDG(I)
            BETAS(I)=BETASG(I)
            BETAP(I)=BETAPG(I)
            BETAD(I)=BETADG(I)
            ALP(I)=ALPPDG(I)
            EISOL(I)=EISOLG(I)
            DD(I)=DDPDG(I)
            QQ(I)=QQPDG(I)
            AM(I)=AMPDG(I)
            AD(I)=ADPDG(I)
            AQ(I)=AQPDG(I)
            GSS(I)=GSSPDG(I)
            GPP(I)=GPPPDG(I)
            GSP(I)=GSPPDG(I)
            GP2(I)=GP2PDG(I)
            HSP(I)=HSPPDG(I)
            PREA(I)=PAPDG(I)
            PREB(I)=PBPDG(I)
            DA(I)=DAPDG(I)
            DB(I)=DBPDG(I)     
   32    CONTINUE
      ELSEIF(LMDG) THEN
*
*    SWITCH IN PDDG/MNDO PARAMETERS
*
         DO 33 I=1,120
            POLVOL(I)=POLVOH(I)
            ZS(I)=ZSMDG(I)
            ZP(I)=ZPMDG(I)
            ZD(I)=ZDMDG(I)
            USS(I)=USSMDG(I)
            UPP(I)=UPPMDG(I)
            UDD(I)=UDDMDG(I)
            BETAS(I)=BETASH(I)
            BETAP(I)=BETAPH(I)
            BETAD(I)=BETADH(I)
            ALP(I)=ALPMDG(I)
            EISOL(I)=EISOLH(I)
            DD(I)=DDMDG(I)
            QQ(I)=QQMDG(I)
            AM(I)=AMMDG(I)
            AD(I)=ADMDG(I)
            AQ(I)=AQMDG(I)
            GSS(I)=GSSMDG(I)
            GPP(I)=GPPMDG(I)
            GSP(I)=GSPMDG(I)
            GP2(I)=GP2MDG(I)
            HSP(I)=HSPMDG(I)
            PREA(I)=PAMDG(I)
            PREB(I)=PBMDG(I)
            DA(I)=DAMDG(I)
            DB(I)=DBMDG(I)
   33    CONTINUE   
      ELSEIF(LRM1.OR.LRM1D.OR.LRM1D3) THEN                              LF0310/LF1211
*
*    SWITCH IN RM1 PARAMETERS
*
         DO 55 I=1,120                                                  ZJJ0806
            DO 52 J=1,10
               GAUSS1(I,J)=GAUSR1(I,J)
               GAUSS2(I,J)=GAUSR2(I,J)
   52       GAUSS3(I,J)=GAUSR3(I,J)
            POLVOL(I)=POLVOR(I)
            ZS(I)=ZSRM1(I)
            ZP(I)=ZPRM1(I)
            ZD(I)=ZDRM1(I)
            USS(I)=USSRM1(I)
            UPP(I)=UPPRM1(I)
            UDD(I)=UDDRM1(I)
            BETAS(I)=BETASR(I)
            BETAP(I)=BETAPR(I)
            BETAD(I)=BETADR(I)
            ALP(I)=ALPRM1(I)
            EISOL(I)=EISOLR(I)
            DD(I)=DDRM1(I)
            QQ(I)=QQRM1(I)
            AM(I)=AMRM1(I)
            AD(I)=ADRM1(I)
            AQ(I)=AQRM1(I)
            GSS(I)=GSSRM1(I)
            GPP(I)=GPPRM1(I)
            GSP(I)=GSPRM1(I)
            GP2(I)=GP2RM1(I)
            HSP(I)=HSPRM1(I)
   55    CONTINUE

        ENDIF                                                           ZJJ0706

C
C   *** RM1-D IS BEING REDEFINED (FOR VERSION 5.019mn) AS RM1 WITH ONLY
C       RM1-D DISPERSION AND NO OTHER OVERRIDING PARAMETERS (THAT WERE 
C       USED FOR VERSION 5.018mn).
C                                                                         ..
Cobs      IF (LRM1D) THEN                                                   LF0310
Cobs          DO 99 I=1,120                                                   ..
Cobs LF0310: NOTE THAT ONLY H, C, N, O, F, AND S ARE CURRENTLY PARAMETERIZED ..
Cobs         FOR THE AM1-D AND MNDO-D METHODS.                               ..
Cobs            IF (I.EQ.1.OR.I.EQ.6.OR.I.EQ.7.OR.I.EQ.8.OR.I.EQ.16) THEN     ..
Cobs             USS(I)=USSRMD(I)                                             .. 
Cobs             UPP(I)=UPPRMD(I)                                             ..  
Cobs             BETAS(I)=BETASI(I)                                           ..
Cobs             BETAP(I)=BETAPI(I)                                           ..
Cobs             ALP(I)=ALPHAI(I)                                             ..
Cobs             EISOL(I)=EISOLI(I)                                           ..
Cobs            ENDIF                                                         ..
Cobs   99     CONTINUE                                                        ..
Cobs      ENDIF                                                             LF0310

      NEWELE=2                                                          0304WH93
      OLDE(1)=' S1978'
      ISWAP(1,1)=16
      ISWAP(2,1)=91
      OLDE(2)='SI1978'
      ISWAP(1,2)=14
      ISWAP(2,2)=90
      DO 40 K=1,NEWELE
         IF(INDEX(KEYWRD,OLDE(K)).NE.0)THEN
            I=ISWAP(1,K)
            J=ISWAP(2,K)
            ALLRF2 = ALLREF(j,1)
            ALLREF(i,3) = ALLRF2
            ALLREF(i,1) = ALLRF2
            ZS(I)=ZS(J)
            ZP(I)=ZP(J)
            ZD(I)=ZD(J)
            USS(I)=USS(J)
            UPP(I)=UPP(J)
            UDD(I)=UDD(J)
            BETAS(I)=BETAS(J)
            BETAP(I)=BETAP(J)
            BETAD(I)=BETAD(J)
            ALP(I)=ALP(J)
            EISOL(I)=EISOL(J)
            DD(I)=DD(J)
            QQ(I)=QQ(J)
            AM(I)=AM(J)
            AD(I)=AD(J)
            AQ(I)=AQ(J)
            IF(GSS(J).NE.0)GSS(I)=GSS(J)
            IF(GPP(J).NE.0)GPP(I)=GPP(J)
            IF(GSP(J).NE.0)GSP(I)=GSP(J)
            IF(GP2(J).NE.0)GP2(I)=GP2(J)
            IF(HSP(J).NE.0)HSP(I)=HSP(J)
         ENDIF
   40 CONTINUE

      IF( LMINDO ) THEN
         DO 50 I=1,17
            IF(I.EQ.2.OR.I.EQ.10)GOTO 50
            USS(I)=USS3(I)
            UPP(I)=UPP3(I)
            EISOL(I)=EISOL3(I)
            EHEAT(I)=EHEAT3(I)
            ZS(I)=ZS3(I)
            ZP(I)=ZP3(I)
   50    CONTINUE
      ENDIF

      IF(USS(1) .GT. -1.D0) THEN
         IF (DOPRNT) WRITE(6,'(                                         CIO
     &           ''  THE HAMILTONIAN REQUESTED IS NOT AVAILABLE IN'',   CIO
     &           '' THIS PROGRAM'')')                                   CIO
      write(6,*) "USS(1)=",USS(1)                                       DEBUG
      RETURN 1                                                          CSTP (stop)
      ENDIF

      IA=1
      IB=0
      NHEAVY=0
      DO 100 II=1,NATOMS
         IF(LABELS(II).EQ.99.OR.LABELS(II).EQ.107) GOTO 100
         NUMAT=NUMAT+1
         NAT(NUMAT)=LABELS(II)
         NFIRST(NUMAT)=IA
         NI=NAT(NUMAT)
         ATHEAT=ATHEAT+EHEAT(NI)
         EAT   =EAT   +EISOL(NI)
         ELECS=ELECS+CORE(NI)
         IB=IA+NATORB(NI)-1
         NMIDLE(NUMAT)=IB
         IF(NATORB(NI).EQ.9)NDORBS=NDORBS+5
         IF(NATORB(NI).EQ.9)NMIDLE(NUMAT)=IA+3
         NLAST(NUMAT)=IB
         NBAND(NUMAT)=NATORB(NI)*(NATORB(NI)+1)/2                       JZ0315
         IF(IA.GT.MAXORB) GOTO 240
         USPD(IA)=USS(NI)
         IF(IA.EQ.IB) GOTO 90
         K=IA+1
         K1=IA+3
         DO 60 J=K,K1
            IF(J.GT.MAXORB) GOTO 240
            USPD(J)=UPP(NI)
   60    CONTINUE
         NHEAVY=NHEAVY+1
   70    IF(K1.EQ.IB)GOTO 90
         K=K1+1
         DO 80 J=K,IB
   80    USPD(J)=UDD(NI)
   90    CONTINUE
  100 IA=IB+1

      CALL REFER(*9999)                                                 CSTP(call)

      ATHEAT=ATHEAT-EAT*23.061D0
      NORBS=NLAST(NUMAT)

      IF(NORBS.GT.MAXORB)THEN
         IF (DOPRNT) WRITE(6,'(//10X,''**** MAX. NUMBER OF ORBITALS:'', CIO
     &         I4,/10X,''NUMBER OF ORBITALS IN SYSTEM:'',I4)')          CIO
     &         MAXORB,NORBS
      RETURN 1                                                          CSTP (stop)
      ENDIF

      NLIGHT=NUMAT-NHEAVY
      N2EL=50*NHEAVY*(NHEAVY-1)+10*NHEAVY*NLIGHT+(NLIGHT*(NLIGHT-1))/2

      IF(N2EL.GT.N2ELEC)THEN
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     &     ''**** MAX. NUMBER OF TWO-ELECTRON INTEGRALS:'',I8,/10X,     CIO
     &     ''NUMBER OF TWO ELECTRON INTEGRALS IN SYSTEM:'',I8)')
     &     N2ELEC,N2EL
      RETURN 1                                                          CSTP (stop)
      ENDIF

C NOW TO CALCULATE THE NUMBER OF LEVELS OCCUPIED
      TRIP=(INDEX(KEYWRD,'TRIPLET').NE.0)
      EXCI=(INDEX(KEYWRD,'EXCITED').NE.0)
      BIRAD=(EXCI.OR.INDEX(KEYWRD,'BIRAD').NE.0)
      IF(INDEX(KEYWRD,'C.I.') .NE. 0 .AND. UHF ) THEN
         IF (DOPRNT) WRITE(6,'(//10X,''C.I. NOT ALLOWED WITH UHF '')')  CIO
      RETURN 1                                                          CSTP (stop)
      ENDIF

C NOW TO WORK OUT HOW MANY ELECTRONS ARE IN EACH TYPE OF SHELL

      NALPHA=0
      NBETA=0

C PROTECT USERS FROM NONSENSICAL ERRORS!

      NELECS=MAX(ELECS+0.5D0,0.D0)
      NELECS=MIN(2*NORBS,NELECS)
      NCLOSE=0
      NOPEN=0
      IF( UHF ) THEN
         FRACT=1.D0
         NBETA=NELECS/2
         IF( TRIP ) THEN
            IF(NBETA*2 .NE. NELECS) THEN
               IF (DOPRNT) WRITE(6,'(//10X,                             CIO
     &            ''TRIPLET SPECIFIED WITH ODD NUMBER'',                CIO
     &            '' OF ELECTRONS, CORRECT FAULT '')')
               RETURN 1                                                 CSTP (stop)
            ELSE
               IF(DOPRNT)WRITE(6,'(//'' TRIPLET STATE CALCULATION'')')  CIO
               NBETA=NBETA-1
            ENDIF
         ENDIF
         IF(INDEX(KEYWRD,'QUART').NE.0) THEN
            IF(NBETA*2 .EQ. NELECS) THEN
               IF(DOPRNT)WRITE(6,'(//10X,                               CIO
     &            ''QUARTET SPECIFIED WITH EVEN NUMBER'',               CIO
     &            '' OF ELECTRONS, CORRECT FAULT '')')
               RETURN 1                                                 CSTP (stop)
            ELSE
               IF(DOPRNT)WRITE(6,'(//'' QUARTET STATE CALCULATION'')')  CIO
               NBETA=NBETA-1
            ENDIF
         ENDIF
         IF(INDEX(KEYWRD,'QUINT').NE.0) THEN
            IF(NBETA*2 .NE. NELECS) THEN
               IF (DOPRNT) WRITE(6,'(//10X,                             CIO
     &            ''QUINTET SPECIFIED WITH ODD NUMBER'',                CIO
     &            '' OF ELECTRONS, CORRECT FAULT '')')
               RETURN 1                                                 CSTP (stop)
            ELSE
               IF(DOPRNT)WRITE(6,'(//'' QUINTET STATE CALCULATION'')')  CIO
               NBETA=NBETA-2
            ENDIF
         ENDIF
         IF(INDEX(KEYWRD,'SEXT').NE.0) THEN
            IF(NBETA*2 .EQ. NELECS) THEN
               IF (DOPRNT) WRITE(6,'(//10X,                              CIO
     &            ''SEXTET SPECIFIED WITH EVEN NUMBER'',                 CIO
     &            '' OF ELECTRONS, CORRECT FAULT '')')
               RETURN 1                                                  CSTP (stop)
            ELSE
               IF (DOPRNT) WRITE(6,'(//'' SEXTET STATE CALCULATION'')')  CIO
               NBETA=NBETA-1
            ENDIF
         ENDIF
         NALPHA=NELECS-NBETA
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     &                ''UHF CALCULATION, NO. OF ALPHA ELECTRONS ='',I   CIO
     &3,/27X,''NO. OF BETA  ELECTRONS ='',I3)')NALPHA,NBETA             CIO
      ELSE

C   NOW TO DETERMINE OPEN AND CLOSED SHELLS

         OPEN=.FALSE.
         IELEC=0
         ILEVEL=0
         IF( TRIP .OR. EXCI .OR. BIRAD ) THEN
            IF( (NELECS/2)*2 .NE. NELECS) THEN
               IF (DOPRNT) WRITE(6,'(//10X,                             CIO
     &             ''SYSTEM SPECIFIED WITH ODD NUMBER'',                CIO
     &             '' OF ELECTRONS, CORRECT FAULT '')')
               RETURN 1                                                 CSTP (stop)
            ENDIF
            IF (DOPRNT) THEN                                            CIO
              IF(BIRAD)WRITE(6,'(//'' SYSTEM IS A BIRADICAL'')')
              IF(TRIP )WRITE(6,'(//'' TRIPLET STATE CALCULATION'')')
              IF(EXCI )WRITE(6,'(//'' EXCITED STATE CALCULATION'')')
            ENDIF                                                       CIO
            IELEC=2
            ILEVEL=2
         ELSEIF((NELECS/2)*2.NE.NELECS) THEN
            IELEC=1
            ILEVEL=1
         ENDIF
         IF(INDEX(KEYWRD,'QUART').NE.0) THEN
            IF (DOPRNT) WRITE(6,'(//'' QUARTET STATE CALCULATION'')')   CIO
            IELEC=3
            ILEVEL=3
         ENDIF
         IF(INDEX(KEYWRD,'QUINT').NE.0) THEN
            IF (DOPRNT) WRITE(6,'(//'' QUINTET STATE CALCULATION'')')   CIO
            IELEC=4
            ILEVEL=4
         ENDIF
         IF(INDEX(KEYWRD,'SEXT').NE.0) THEN
            IF (DOPRNT) WRITE(6,'(//'' SEXTET STATE CALCULATION'')')    CIO
            IELEC=5
            ILEVEL=5
         ENDIF
         I=INDEX(KEYWRD,'OPEN(')
         IF(I.NE.0)THEN
            IELEC=READA(KEYWRD,I)
            ILEVEL=READA(KEYWRD,I+7)
         ENDIF
         NCLOSE=NELECS/2
         NOPEN = NELECS-NCLOSE*2
         IF( IELEC.NE.0 )THEN
            IF((NELECS/2)*2.EQ.NELECS .NEQV.
     1                  (IELEC/2)*2.EQ.IELEC) THEN
               IF (DOPRNT) WRITE(6,'(                                   CIO
     &                '' IMPOSSIBLE NUMBER OF OPEN SHELL ELECTRONS'')') CIO
               RETURN 1                                                 CSTP (stop)
            ENDIF
            NCLOSE=NCLOSE-IELEC/2
            NOPEN=ILEVEL
            FRACT=IELEC*1.D0/ILEVEL
            IF (DOPRNT) WRITE(6,'('' THERE ARE'',I3,                    CIO
     &                             '' DOUBLY FILLED LEVELS'')')NCLOSE   CIO
         ENDIF
         IF (DOPRNT) WRITE(6,'(//10X,''RHF CALCULATION, NO. OF '',      CIO
     1''DOUBLY OCCUPIED LEVELS ='',I3)')NCLOSE
         IF(DOPRNT.AND.NOPEN.NE.0.AND.ABS(FRACT-1.D0).LT.1.D-4)         CIO
     1WRITE(6,'(/27X,''NO. OF SINGLY OCCUPIED LEVELS ='',I3)')NOPEN
         IF(DOPRNT.AND.NOPEN.NE.0.AND.ABS(FRACT-1.D0).GT.1.D-4)         CIO
     1WRITE(6,'(/27X,''NO. OF LEVELS WITH OCCUPANCY'',F6.3,''  ='',I3)')
     2FRACT,NOPEN
C
C C.I=(n,m) modifications                                               IR0494
C
         IF(INDEX(KEYWRD,'C.I.=(').NE.0) THEN
            I=READA(KEYWRD,INDEX(KEYWRD,'C.I.=(')+5)-
     1      READA(KEYWRD,INDEX(KEYWRD,'C.I.=(')+7)
            IF(NOPEN.GT.I)THEN
               IF (DOPRNT) WRITE(6,'(//,                                CIO
     &             '' NUMBER OF OPEN-SHELLS ALLOWED IN C.I. IS LESS ''  CIO
     &            /''    THAN THAT SPECIFIED BY OTHER KEYWORDS'')')     CIO
      RETURN 1                                                          CSTP (stop)
            ENDIF
         ENDIF
C temporarily commmeted by J. Zheng
C        IF(INDEX(KEYWRD,'C.I.').NE.0.AND.NOPEN.EQ.0)THEN
C           NOPEN=1
C           NCLOSE=NCLOSE-1
C           FRACT=2.D0
C        ENDIF
C
C END of  C.I=(n,m) modifications
C
         NOPEN=NOPEN+NCLOSE
      ENDIF
C other C.I.=(n,m) checks                                               IR0494
C
C  WORK OUT IF DEFINED SPIN-STATE ALLOWED
C
      MSDEL=INDEX(KEYWRD,' MS')
      IF(MSDEL.NE.0)THEN
         MSDEL=1.0001D0*READA(KEYWRD,INDEX(KEYWRD,' MS'))
      ELSE
       IF(INDEX(KEYWRD,'TRIP').GT.0.OR.INDEX(KEYWRD,'QUAR').GT.0)MSDEL=1
       IF(INDEX(KEYWRD,'QUIN').GT.0.OR.INDEX(KEYWRD,'SEXT').GT.0)MSDEL=2
      ENDIF
      IF(MSDEL.NE.0.AND..NOT.UHF)THEN
C
C   MSDEL = NUMBER OF ALPHA ELECTRONS - NUMBER OF BETA ELECTRONS
C
         NDOUBL=99
         IF(INDEX(KEYWRD,'C.I.=(').NE.0)THEN
            NDOUBL=READA(KEYWRD,INDEX(KEYWRD,'C.I.=(')+7)
            NMOS=READA(KEYWRD,INDEX(KEYWRD,'C.I.=(')+5)
         ELSEIF (INDEX(KEYWRD,'C.I.=').NE.0)THEN
            NMOS=READA(KEYWRD,INDEX(KEYWRD,'C.I.=')+5)
         ELSE
            NMOS=NOPEN-NCLOSE
         ENDIF
         IF(NDOUBL.EQ.99)THEN
            J=MAX(MIN((NCLOSE+NOPEN+1)/2-(NMOS-1)/2,NORBS-NMOS+1),1)
         ELSE
            J=NCLOSE-NDOUBL+1
         ENDIF
         NE=MAX(0.D0,(NCLOSE-J+1.D0))*2.D0+
     1     MAX(0.D0,(NOPEN-NCLOSE)*FRACT) + 0.5D0
         NUPP=(NE+1)/2+MSDEL
         NDOWN=NE-NUPP
C
C  NUPP  = NUMBER OF ALPHA ELECTRONS IN ACTIVE SPACE
C  NDOWN = NUMBER OF BETA  ELECTRONS IN ACTIVE SPACE
C
         IF(NUPP*NDOWN.LT.0.OR.NUPP.GT.NMOS.OR.NDOWN.GT.NMOS)THEN
            IF (DOPRNT) WRITE(6,'(A)')                                  CIO
     1' SPECIFIED SPIN COMPONENT NOT SPANNED BY ACTIVE SPACE'
      RETURN 1                                                          CSTP (stop)
         ENDIF
      ENDIF
C#      WRITE(6,'(''  NOPEN,NCLOSE,NALPHA,NBETA,FRACT'',4I4,F12.5)')
C#     1 NOPEN, NCLOSE, NLAPHA, NBETA, FRACT
C
C   MAKE SURE ANALYT IS NOT USED WITH ANALYTICAL C.I. DERIVATIVES
C
      HALFE = (NOPEN.GT.NCLOSE.AND.FRACT.NE.2.D0.AND.FRACT.NE.0.D0
     1         .OR. (INDEX(KEYWRD,'C.I.').NE.0))                        IR0494
      SLOW=(INDEX(KEYWRD,'EXCI').NE.0.OR.
     1INDEX(KEYWRD,'ROOT').NE.0.AND.INDEX(KEYWRD,'ROOT=1').EQ.0)
      IF(HALFE)HALFE=(.NOT.SLOW)
      IF(INDEX(KEYWRD,'ANALYT').NE.0.AND.HALFE)THEN
         IF (DOPRNT) WRITE(6,*)
         IF (DOPRNT) WRITE(6,'(A)')                                     CIO
     &      ' KEYWORD ''ANALYT'' CANNOT BE USED HERE: ',                CIO
     &      ' ANALYTICAL C.I. DERIVATIVES MUST USE FINITE DIFFERENCES', CIO
     &      ' TO CORRECT, REMOVE KEYWORD ''ANALYT'''                    CIO
      RETURN 1                                                          CSTP (stop)
      ENDIF
C#      WRITE(6,'(''  NOPEN,NCLOSE,NALPHA,NBETA,FRACT'',4I4,F12.5)')
C#     1 NOPEN, NCLOSE, NLAPHA, NBETA, FRACT
C  END of other C.I.=(n,m) checks
      YY=FLOAT(KHARGE)/(NORBS+1.D-10)
      L=0
      DO 130 I=1,NUMAT
         NI=NAT(I)
         XX=1.D0/(NLAST(I)-NFIRST(I)+1+1.D-10)
         W=CORE(NI)*XX-YY
         IA=NFIRST(I)
         IC=NMIDLE(I)
         IB=NLAST(I)
         DO 110 J=IA,IC
            L=L+1
  110    PSPD(L)=W
         DO 120 J=IC+1,IB
            L=L+1
  120    PSPD(L)=0.D0
  130 CONTINUE
C
C   WRITE OUT THE INTERATOMIC DISTANCES
C
      CALL GMETRY(GEO,COORD,*9999)                                       CSTP(call)
      RMIN=100.D0
      L=0
      DO 140 I=1,NUMAT
         DO 140 J=1,I
            L=L+1
            RXYZ(L)=SQRT((COORD(1,I)-COORD(1,J))**2+
     1                     (COORD(2,I)-COORD(2,J))**2+
     2                     (COORD(3,I)-COORD(3,J))**2)
            IF(RMIN.GT.RXYZ(L) .AND. I .NE. J .AND.
     1 (NAT(I).LT.103 .OR. NAT(J).LT.103)) THEN
               IMINR=I
               JMINR=J
               RMIN=RXYZ(L)
            ENDIF
  140 CONTINUE
      NNHCO=0
C
C   SET UP MOLECULAR-MECHANICS CORRECTION TO -(C=O)-(NH)- LINKAGE
C   THIS WILL BE USED IF MMOK HAS BEEN SPECIFIED.
C
         ITYPE=1
         IF (LAM1   .OR. LAM1D .OR. LAM1D3)   ITYPE=2                   LF0309/LF0909/LF0310/LF1211
         IF (LPM3   .OR. LPM3D .OR. LPM3D3)   ITYPE=3                   LF0309/LF0909/LF0310/LF1211
         IF (LMINDO)              ITYPE=4                               LF0909
         IF (LPDG)                ITYPE=5                               ZJJ0706/LF0909
         IF (LMDG)                ITYPE=6                               ZJJ0706/LF0909
         IF (LPM6   .OR. LPM6D .OR. LPM6D3)   ITYPE=7                   LF0809/LF0909/LF0310/LF1211
C
C   IDENTIFY O=C-N-H SYSTEMS VIA THE INTERATOMIC DISTANCES MATRIX
         DO 190 I=1,NUMAT
            IF(NAT(I).NE.8) GOTO 190
            DO 180 J=1,NUMAT
               IF(NAT(J).NE.6) GOTO 180
               IJ=MAX(I,J)
               JI=I+J-IJ
               IF(RXYZ((IJ*(IJ-1))/2+JI).GT.1.3)GOTO 180
               DO 170 K=1,NUMAT
                  IF(NAT(K).NE.7) GOTO 170
                  JK=MAX(J,K)
                  KJ=J+K-JK
                  IF(RXYZ((JK*(JK-1))/2+KJ).GT.1.6)GOTO 180
                  DO 160 L=1,NUMAT
                     IF(NAT(L).NE.1) GOTO 160
                     KL=MAX(K,L)
                     LK=K+L-KL
                     IF(RXYZ((KL*(KL-1))/2+LK).GT.1.3)GOTO 180
C
C   WE HAVE A H-N-C=O SYSTEM.  THE ATOM NUMBERS ARE L-K-J-I
C   NOW SEARCH OUT ATOM ATTACHED TO NITROGEN, THIS SPECIFIES
C   THE SYSTEM X-N-C=O
C
                     DO 150 M=1,NUMAT
                        IF(M.EQ.K.OR.M.EQ.L.OR.M.EQ.J) GOTO 150
                        MK=MAX(M,K)
                        KM=M+K-MK
                        IF(RXYZ((MK*(MK-1))/2+KM).GT.1.7)GOTO 150
                        NNHCO=NNHCO+1
                        NHCO(1,NNHCO)=I
                        NHCO(2,NNHCO)=J
                        NHCO(3,NNHCO)=K
                        NHCO(4,NNHCO)=M
                        NNHCO=NNHCO+1
                        NHCO(1,NNHCO)=I
                        NHCO(2,NNHCO)=J
                        NHCO(3,NNHCO)=K
                        NHCO(4,NNHCO)=L
                        GOTO 160
  150                CONTINUE
  160             CONTINUE
  170          CONTINUE
  180       CONTINUE
  190    CONTINUE
      IF(NNHCO.NE.0)THEN
      IF(INDEX(KEYWRD,'MMOK').NE.0) THEN
      IF (DOPRNT) WRITE(6,'(A)')                                        CIO
     &    ' MOLECULAR MECHANICS CORRECTION APPLIED TO PEPTIDE LINKAGE'  CIO
      ELSEIF(INDEX(KEYWRD,'NOMM').NE.0)THEN
      IF (DOPRNT) WRITE(6,'(A,I2,A)')' THERE ARE ',NNHCO/2,             CIO
     &                 ' PEPTIDE LINKAGES IDENTIFIED IN THIS SYSTEM'    CIO
      IF (DOPRNT) WRITE(6,'(A)')' IF YOU WANT MM CORRECTION TO THE '//  CIO
     &                     'CONH BARRIER, ADD THE KEY-WORD "MMOK"'      CIO
      ELSE
      IF (DOPRNT) WRITE(6,'(A)')' THIS SYSTEM CONTAINS -HNCO- GROUPS.'  CIO
      IF (DOPRNT) WRITE(6,'(A)')' YOU MUST SPECIFY "NOMM" OR '          CIO
     &           //'"MMOK" REGARDING MOLECULAR MECHANICS CORRECTION'    CIO
      RETURN 1                                                          CSTP (stop)
      ENDIF
      ENDIF
      IF (NOPEN.NE.NCLOSE.AND.DOPRNT)                                    LF0112
     &   WRITE(6,'(/,4X,A/)') " WARNING: THIS IS AN OPEN-SHELL SYSTEM.  LF0112
     &  THE DEFAULT CALCULATION USES THE RESTRICTED WAVEFUNCTION (RHF), LF0112
     & SO USE THE 'UHF' KEYWORD IF YOU WANT AN UNRESTRICTED CALCULATION.LF0112
     &"                                                                 LF0112
      IF (INDEX(KEYWRD,'NOINTER') .EQ. 0) THEN
         IF (DOPRNT) WRITE(6,'(//10X,''  INTERATOMIC DISTANCES'')')     CIO
         CALL VECPRT(RXYZ,NUMAT,*9999)                                  CSTP(call)
      ENDIF
      IF(RMIN.LT.0.8D0.AND.INDEX(KEYWRD,'GEO-OK') .EQ.0) THEN
         IF (DOPRNT) WRITE(6,200)IMINR,JMINR,RMIN                       CIO
  200    FORMAT(//,'   ATOMS',I3,' AND',I3,' ARE SEPARATED BY',F8.4,
     1' ANGSTROMS.',/'   TO CONTINUE CALCULATION SPECIFY "GEO-OK"')
      RETURN 1                                                          CSTP (stop)
      ENDIF
c      write (6,*) "NOPEN=",NOPEN,"   NCLOSE=",NCLOSE                    debug
c      write (6,*) "NALPHA=",nalpha,"   NBETA=",nbeta                    debug
      IF(.NOT. DEBUG) RETURN
      IF (DOPRNT) WRITE(6,210)NUMAT,NORBS,NDORBS,NATOMS                 CIO
  210 FORMAT('   NUMBER OF REAL ATOMS:',I4,/
     1      ,'   NUMBER OF ORBITALS:  ',I4,/
     2      ,'   NUMBER OF D ORBITALS:',I4,/
     3      ,'   TOTAL NO. OF ATOMS:  ',I4)
      IF (DOPRNT) WRITE(6,220)(USPD(I),I=1,NORBS)                       CIO
  220 FORMAT('   ONE-ELECTRON DIAGONAL TERMS',/,10(/,10F8.3))
      IF (DOPRNT) WRITE(6,230)(PSPD(I),I=1,NORBS)                       CIO
  230 FORMAT('   INITIAL P FOR ALL ATOMIC ORBITALS',/,10(/,10F8.3))
      RETURN
  240 IF (DOPRNT) WRITE(6,'(//10X,'' MAXIMUM NUMBER OF ATOMIC ''/       CIO
     &                              ''ORBITALS EXCEEDED'')')            CIO
      IF (DOPRNT) WRITE(6,'(  10X,'' MAXIMUM ALLOWED ='',I4)')MAXORB    CIO
      RETURN 1                                                          CSTP (stop)
 9999 RETURN 1                                                          CSTP
      ENTRY MOLDAT_INIT                                                 CSAV
            ALLRF2 = ''                                                 CSAV
             BIRAD = .FALSE.                                            CSAV
             COORD = 0.0D0                                              CSAV
             DEBUG = .FALSE.                                            CSAV
               EAT = 0.0D0                                              CSAV
             ELECS = 0.0D0                                              CSAV
              EXCI = .FALSE.                                            CSAV
             HALFE = .FALSE.                                            CSAV
                 I = 0                                                  CSAV
                IA = 0                                                  CSAV
                IB = 0                                                  CSAV
                IC = 0                                                  CSAV
             IELEC = 0                                                  CSAV
                II = 0                                                  CSAV
                IJ = 0                                                  CSAV
            ILEVEL = 0                                                  CSAV
             IMINR = 0                                                  CSAV
             ISWAP = 0                                                  CSAV
             ITYPE = 0                                                  CSAV
                 J = 0                                                  CSAV
                JI = 0                                                  CSAV
                JK = 0                                                  CSAV
             JMINR = 0                                                  CSAV
                 K = 0                                                  CSAV
                K1 = 0                                                  CSAV
            KHARGE = 0                                                  CSAV
                KJ = 0                                                  CSAV
                KL = 0                                                  CSAV
                KM = 0                                                  CSAV
                 L = 0                                                  CSAV
                LK = 0                                                  CSAV
                 M = 0                                                  CSAV
                MK = 0                                                  CSAV
             MSDEL = 0                                                  CSAV
              N2EL = 0                                                  CSAV
            NDORBS = 0                                                  CSAV
            NDOUBL = 0                                                  CSAV
             NDOWN = 0                                                  CSAV
              NEQV = 0                                                  CSAV
            NEWELE = 0                                                  CSAV
              NHCO = 0                                                  CSAV
            NHEAVY = 0                                                  CSAV
                NI = 0                                                  CSAV
            NLIGHT = 0                                                  CSAV
              NMOS = 0                                                  CSAV
             NNHCO = 0                                                  CSAV
              OLDE = ""                                                 CSAV
              RMIN = 0.0D0                                              CSAV
              SLOW = .FALSE.                                            CSAV
              TRIP = .FALSE.                                            CSAV
               UHF = .FALSE.                                            CSAV
             USEMM = .FALSE.                                            CSAV
                 W = 0.0D0                                              CSAV
                XX = 0.0D0                                              CSAV
                YY = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
