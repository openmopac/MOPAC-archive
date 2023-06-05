      SUBROUTINE DEBUGS(MSG,*)                                          CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1110
      include 'corgen.f'   ! Common block definition for /CORGEN/.      LF0211
      CHARACTER  MSG*(*)
C
C     CREATED BY LUKE FIEDLER, FEB 2010.
C     MODIFY THIS FILE FOR OUTPUT OF VARIOUS COMMON BLOCK VARIABLES.
C     INPUT VARIABLE, MSG, IS OUTPUT AS HEADER.
C

      COMMON /ALPARM/ ALPARM(3,MAXPAR),X0, X1, X2, JLOOP
      COMMON /ALPHA / ALP(120)
      COMMON /ALPHA3/ ALP3(153)
      COMMON /ALPTM / ALPTM(30), EMUDTM(30)
      COMMON /AM1DSB/ ALPAMD,R0AMD(120),S6AMD,C6AMD(120),USSAMD(120), 
     &                UPPAMD(120),BETASE(120),BETAPE(120),ALPHAE(120),
     &                EISOLE(120)
      COMMON /ATHEAT/ ATHEAT
      COMMON /ATMASS/ ATMASS(NUMATM)
      COMMON /ATOMI3 / EISOL3(18),EHEAT3(18)
      COMMON /ATOMIC/ EISOL(120),EHEAT(120)
      COMMON /ATOMIP/ ATMIP(120)
      COMMON /BASEOC/ OCCA(NMECI),KDUM(NMECI+2)                         JZ0315
      COMMON /BETA3 / BETA3(153)
      COMMON /BETAS / BETAS(120),BETAP(120),BETAD(120)
      PARAMETER (NATMS2=MAXPAR*MAXPAR-MAXORB*MAXORB)
      LOGICAL PRINT
      COMMON /BONDCM/ BONDAB(NATMS2),PRINT
      COMMON /CIMATS/ ENGYCI(3),VECTCI(9),ECI(6)
      COMMON /CITERM/ XI,XJ,XK
      COMMON /CKDD/CK(105,105),DDD(105,105)
      COMMON /CM1SUM/ BB(NUMATM,NUMATM),QM(NUMATM),DELTA2(NUMATM),
     &                QC0(NUMATM),QC(NUMATM) ,QD0(NUMATM),
     &                FD(NUMATM,NUMATM),QD(NUMATM)
      COMMON /COORD / COORD(3,NUMATM)
      COMMON /CORE / CORE(120)
      COMMON /DCARTC/ K1L,K2L,K3L,K1U,K2U,K3U
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /DERIVS/ DS(16),DG(22),DR(100),TDX(3),TDY(3),TDZ(3)
      COMMON /DRCCOM/ MCOPRT(2,MAXPAR), NCOPRT, PARMAX
      LOGICAL PARMAX
      COMMON /ELECT / ELECT
      COMMON /ELEMTS/ ELEMNT(120)
      CHARACTER ELEMNT*2
      COMMON /ENUCLR/ ENUCLR
      COMMON /ERRFN / ERRFN(MAXPAR)
      COMMON /ETAEPS/ ETA, EPS
      COMMON /EULER / TVEC(3,3), ID
      COMMON /EXPON3 / ZS3(18),ZP3(18)
      COMMON /EXPONT/ ZS(120),ZP(120),ZD(120)
      COMMON /EXTRA/ G(22),TX(3),TY(3),TZ(3)
      COMMON /FIELD/ EFIELD(3)
      COMMON /FITDAT/ XLOW,XHIGH,XMIN,FMIN,DFMIN,XX(12),FF(12),DF(12)
      COMMON /FITPTS/ N
      COMMON /FMATRX/ ALLXYZ(3,MAXPAR),ALLVEL(3,MAXPAR),PARREF(MAXPAR), 
     &                XYZ3(3,MAXPAR),VEL3(3,MAXPAR), ALLGEO(3,MAXPAR), 
     &                GEO3(3,MAXPAR), DUMMY(MAXHES-19*MAXPAR)
      COMMON /FOKMAT/ F(MPACK), FB(MPACK)
      COMMON /FORCE3/ IDMY(5),I3N,IX
      COMMON /GAUSSB/ FN1(120),FN2(120)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM), NA(NUMATM),NB(NUMATM),
     &                NC(NUMATM)
      COMMON /GEOM / GEO(3,NUMATM)
      LOGICAL  IGEOOK
      COMMON /GEOOK/ IGEOOK
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /GEOVAR/ XPARAM(MAXPAR), NVAR, LOC(2,MAXPAR)
      COMMON /GRADNT/ GRAD(MAXPAR), GNORM
      COMMON /GRAVEC/ COSINE
      COMMON /HHGAU/ HHA0(3),HHR0(3),HHD0(3)
      COMMON /HMATRX/ H(MPACK)
      LOGICAL  HPUSED
      COMMON /HPUSED/ HPUSED
      COMMON /IDEAG / GAUSG1(120,10),GAUSG2(120,10),GAUSG3(120,10),
     &                NGAUSG(120)
      COMMON /IDEAP / GAUSP1(120,10),GAUSP2(120,10),GAUSP3(120,10)
      COMMON /IDEAR / GAUSR1(120,10),GAUSR2(120,10),GAUSR3(120,10)
      COMMON /IDEAS / GAUSS1(120,10), GAUSS2(120,10), GAUSS3(120,10)
      COMMON /IOCM/ IREAD
      COMMON /IONPOT/ ATOMIP(120)
      COMMON /ISTOPE/ AMS(120)
      COMMON /KEYWRD/ KEYWRD
      CHARACTER KEYWRD*160
      COMMON /LAST / LAST
      COMMON /LOCVAR/ LOCVAR(2,MAXPAR)
      COMMON /MESAGE/ IFLEPO,ISCF
      COMMON /MESH / LATOM1, LPARA1, LATOM2, LPARA2
      include 'method.f'
      COMMON /MNDO/ USSM(120), UPPM(120), UDDM(120), ZSM(120),ZPM(120),
     &              ZDM(120), BETASM(120), BETAPM(120), BETADM(120),
     &              ALPM(120), EISOLM(120), DDM(120), QQM(120), 
     &              AMM(120), ADM(120), AQM(120) ,GSSM(120),GSPM(120),
     &              GPPM(120),GP2M(120),HSPM(120), POLVOM(120)
      COMMON /MNDODS/ ALPMND,R0MND(120),S6MND,C6MND(120),USSMND(120),
     &   UPPMND(120),BETASF(120),BETAPF(120),ALPHAF(120),EISOLF(120),
     &   GSSMND(120),GSPMND(120),GPPMND(120),GP2MND(120),HSPMND(120),
     &   ZSMND(120),ZPMND(120)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM), NMIDLE(NUMATM),
     &                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,NCLOSE,
     &                NOPEN
      COMMON /MOLKSR/ FRACT
      LOGICAL  USEMM
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,20),NNHCO,ITYPE,USEMM
      COMMON /MOLORB/ USPD(MAXORB),PSPD(MAXORB)
      COMMON /MULTIP/ DD(120),QQ(120),AM(120),AD(120),AQ(120)
      COMMON /NATORB/ NATORB(120)
      COMMON /NATYPE/ NZTYPE(120),MTYPE(30),LTYPE
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),
     &                PMAT(MAXPAR,MAXPAR)
      COMMON /NLLSQI/ NCOUNT
      COMMON /NUMCAL/ NUMCAL
      COMMON /NUMSCF/ NSCF
      COMMON /ONELE3 / USS3(18),UPP3(18)
      COMMON /ONELEC/ USS(120),UPP(120),UDD(120)
      COMMON/OPTEF/ OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     &              U(MAXPAR,MAXPAR),DDDD,RMIN,RMAX,OMIN,XLAMD,XLAMD0,
     &              SKAL, MODE,NSTEP,NEGREQ,IPRNT
      COMMON /PATHI / LATOM,LPARAM
      COMMON /PATHR / REACT(200)
      COMMON /PDDGM/ USSMDG(120), UPPMDG(120), UDDMDG(120), ZSMDG(120),
     &               ZPMDG(120), ZDMDG(120), BETASH(120), BETAPH(120), 
     &               BETADH(120), ALPMDG(120), EISOLH(120), DDMDG(120), 
     &               QQMDG(120), AMMDG(120), ADMDG(120), AQMDG(120),
     &               GSSMDG(120), GSPMDG(120), GPPMDG(120), GP2MDG(120),
     &               HSPMDG(120),POLVOH(120),PAMDG(120),PBMDG(120),
     &               DAMDG(120),DBMDG(120)
      COMMON /PDDGP/ USSPDG(120), UPPPDG(120), UDDPDG(120), ZSPDG(120),
     &               ZPPDG(120), ZDPDG(120), BETASG(120), BETAPG(120),
     &               BETADG(120), ALPPDG(120), EISOLG(120), DDPDG(120),
     &               QQPDG(120), AMPDG(120), ADPDG(120), AQPDG(120),
     &               GSSPDG(120), GSPPDG(120), GPPPDG(120), GP2PDG(120),
     &               HSPPDG(120),POLVOG(120),PAPDG(120),PBPDG(120),
     &               DAPDG(120),DBPDG(120)
      COMMON /PM3 / USSPM3(120), UPPPM3(120), UDDPM3(120), ZSPM3(120),
     &              ZPPM3(120), ZDPM3(120), BETASP(120), BETAPP(120),
     &              BETADP(120), ALPPM3(120), EISOLP(120), DDPM3(120),
     &              QQPM3(120), AMPM3(120), ADPM3(120), AQPM3(120),
     &              GSSPM3(120), GSPPM3(120), GPPPM3(120), GP2PM3(120),
     &              HSPPM3(120),POLVOP(120)
      COMMON /PM3DSB/ ALPPMD,R0PMD(120),S6PMD,C6PMD(120),USSPMD(120),
     &                UPPPMD(120),BETASD(120),BETAPD(120),ALPHAD(120),
     &                EISOLD(120)
      COMMON /PM6BLO/ USSPM6(120), UPPPM6(120), UDDPM6(120),ZSPM6(120),
     &                ZPPM6(120), ZDPM6(120), BETASX(120), BETAPX(120),
     &                BETADX(120), GSSPM6(120),GSPPM6(120),GPPPM6(120),
     &                GP2PM6(120),HSPPM6(120), EISOLX(120), DDPM6(120),
     &                QQPM6(120), AMPM6(120), ADPM6(120), AQPM6(120),
     &                GAUSX1(120,10), GAUSX2(120,10), GAUSX3(120,10),
     &                ALPPM6(18,18), XPM6(18,18), ZSNPM6(18),ZPNPM6(18),
     &                ZDNPM6(18)
      COMMON /PM6DSB/ ALPP6D,R0P6D(120),S6P6D,C6P6D(120),USSP6D(120),
     &                UPPP6D(120),BETASJ(120),BETAPJ(120),ALPHAJ(120),
     &                EISOLJ(120)
      COMMON /POLVOL/ POLVOL(120)
      COMMON /REACTN/ STEP, GEOA, GEOVEC,CALCST
      COMMON /REFS/ REFMN(120), REFM3(120), REFAM(120), REFPM3(120),
     &              REFPDG(120), REFMDG(120), REFRM(120), REFPMD(120),
     &              REFAMD(120), REFPM6(120), REFMND(120),REFRMD(120),
     &              REFP6D(120),
     &              REFPMOV1(120),                                      LF1010
     &              REFD3(120),REFPMO2(120),REFPMO2A                    LF1211/LF0614
      COMMON /RM1BLO/ USSRM1(120), UPPRM1(120), UDDRM1(120), ZSRM1(120),
     &                ZPRM1(120), ZDRM1(120), BETASR(120), BETAPR(120),
     &                BETADR(120), ALPRM1(120), EISOLR(120), DDRM1(120),
     &                QQRM1(120), AMRM1(120), ADRM1(120), AQRM1(120),
     &                GSSRM1(120), GSPRM1(120), GPPRM1(120),GP2RM1(120),
     &                HSPRM1(120),POLVOR(120)
      COMMON /RM1DSB/ ALPRMD,R0RMD(120),S6RMD,C6RMD(120),USSRMD(120), 
     &                UPPRMD(120),BETASI(120),BETAPI(120),ALPHAI(120),
     &                EISOLI(120)
      COMMON /ROTDU2/ X(3),Y(3),Z(3)
      COMMON /ROTDUM/ CSS1,CSP1,CPPS1,CPPP1,CSS2,CSP2,CPPS2,CPPP2
      COMMON /SCRACH/ RXYZ(MPACK), XDUMY(MAXPAR**2-MPACK)
      COMMON /SETC/ A(7),B(7),SA,SB,FACTOR,ISP,IPS
      COMMON /SIGMA1/ GNEXT, AMIN, ANEXT
      COMMON /SIGMA2/ GNEXT1(MAXPAR), GMIN1(MAXPAR)
      COMMON /SPQR/ ISPQR(NMECI**2,NMECI),IS,I,K
      COMMON /SRPI/ IBTPTR(120), NATPTR(MXATSP), NATSP
      LOGICAL  ISSRP
      COMMON /SRPL/ ISSRP
      COMMON /SRPR/ BETSS(MAXBET), BETSP(MXATSP,MXATSP), BETPP(MAXBET)
      COMMON /SWAP0/ PSI(MAXORB), STDPSI(MAXORB)
      COMMON /TEMP/ CC(60,6),ZZ(60,6)
      COMMON /TIMDMP/ TLEFT, TDUMP
      COMMON /TIMER / TIME0
      COMMON /TITLES/ KOMENT,TITLE
      CHARACTER KOMENT*80, TITLE*80
      COMMON /TWOEL3/ F03(120)
      COMMON /TWOELE/ GSS(120),GSP(120),GPP(120),GP2(120),HSP(120),
     &                GSD(120),GPD(120),GDD(120)
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U
      COMMON /VALVAR/ VALVAR(MAXPAR),NUMVAR
      COMMON /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON /VSIPS / VS(120),VP(120),VD(120)
      COMMON /W4G / PREA(120), PREB(120), DA(120), DB(120)
C     COMMON /WMATRX/ WJ(N2ELEC), WK(N2ELEC),KDUMMY,NBAND(NUMATM)
      COMMON /WMATRX/ WJ(N2ELEC), WK(N2ELEC*2),KDUMMY,NBAND(NUMATM)
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      COMMON /XYZGRA/ DXYZ(3,NUMATM*27)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510


C =====================================================================================
C     START OF OUTPUT STATEMENTS FOR DEBUGGING:
C =====================================================================================

      write (6,*) "     ========= DEBUG OUTPUT ========="
      write (6,*) MSG

      do 100 i=1,natoms
  100    write(6,'(''ATOM #'',I3,'' LABEL IS '',I7,''.'')') I,LABELS(I)

  911 DO 60 I=1,120
      IF(I.EQ.1.OR.I.EQ.6.OR.I.EQ.7.OR.I.EQ.8.OR.I.EQ.9.OR.I.EQ.108)THEN
C      IF (I.EQ.1.OR.I.EQ.6.OR.I.EQ.7.OR.I.EQ.8.OR.I.EQ.9.OR.I.EQ.16)THEN
        IF (I.EQ.1) WRITE(6,*) '   For hydrogen .....'
        IF (I.EQ.6) WRITE(6,*) '   For carbon   .....'
        IF (I.EQ.7) WRITE(6,*) '   For nitrogen .....'
        IF (I.EQ.8) WRITE(6,*) '   For oxygen   .....'
        IF (I.EQ.9) WRITE(6,*) '   For fluorine .....'
        IF (I.EQ.16) WRITE(6,*) '   For sulfur   .....'
        IF (I.EQ.108) WRITE(6,*) '   For elem 108 .....'
        WRITE(6,'(A,I3,A,F14.7)')  'USS    (',I,')  ', USS (I)
        WRITE(6,'(A,I3,A,F14.7)')  'UPP    (',I,')  ', UPP (I)
        WRITE(6,'(A,I3,A,F14.7)')  'BETAS  (',I,')  ', BETAS (I)
        WRITE(6,'(A,I3,A,F14.7)')  'BETAP  (',I,')  ', BETAP (I)
        WRITE(6,'(A,I3,A,F14.7)')  'ZS     (',I,')  ', ZS (I)
        WRITE(6,'(A,I3,A,F14.7)')  'ZP     (',I,')  ', ZP (I)
        WRITE(6,'(A,I3,A,F14.7)')  'ALP    (',I,')  ', ALP (I)
        WRITE(6,'(A,I3,A,F14.7)')  'EISOL  (',I,')  ', EISOL (I)
        WRITE(6,'(A,I3,A,F14.7)')  'EHEAT  (',I,')  ', EHEAT (I)
        WRITE(6,'(A,I3,A,F14.7)')  'GSS    (',I,')  ', GSS (I)
        WRITE(6,'(A,I3,A,F14.7)')  'GSP    (',I,')  ', GSP (I)
        WRITE(6,'(A,I3,A,F14.7)')  'GPP    (',I,')  ', GPP (I)
        WRITE(6,'(A,I3,A,F14.7)')  'GP2    (',I,')  ', GP2 (I)
        WRITE(6,'(A,I3,A,F14.7)')  'HSP    (',I,')  ', HSP (I)
        WRITE(6,'(A,I3,A,F14.7)')  'DD     (',I,')  ', DD (I)
        WRITE(6,'(A,I3,A,F14.7)')  'QQ     (',I,')  ', QQ (I)
        WRITE(6,'(A,I3,A,F14.7)')  'AM     (',I,')  ', AM (I)
        WRITE(6,'(A,I3,A,F14.7)')  'AD     (',I,')  ', AD (I)
        WRITE(6,'(A,I3,A,F14.7)')  'AQ     (',I,')  ', AQ (I)
        WRITE(6,'(A,I3,A,F14.7)')  'GAUSS1 (',I,',1) ', GAUSS1 (I,1)
        WRITE(6,'(A,I3,A,F14.7)')  'GAUSS2 (',I,',1) ', GAUSS2 (I,1)
        WRITE(6,'(A,I3,A,F14.7)')  'GAUSS3 (',I,',1) ', GAUSS3 (I,1)
        WRITE(6,'(A,I3,A,F14.7)')  'GAUSS1 (',I,',2) ', GAUSS1 (I,2)
        WRITE(6,'(A,I3,A,F14.7)')  'GAUSS2 (',I,',2) ', GAUSS2 (I,2)
        WRITE(6,'(A,I3,A,F14.7)')  'GAUSS3 (',I,',2) ', GAUSS3 (I,2)
        WRITE(6,'(A,I3,A,F14.7)')  'GAUSS1 (',I,',3) ', GAUSS1 (I,3)
        WRITE(6,'(A,I3,A,F14.7)')  'GAUSS2 (',I,',3) ', GAUSS2 (I,3)
        WRITE(6,'(A,I3,A,F14.7)')  'GAUSS3 (',I,',3) ', GAUSS3 (I,3)
        WRITE(6,'(A,I3,A,F14.7)')  'GAUSS1 (',I,',4) ', GAUSS1 (I,4)
        WRITE(6,'(A,I3,A,F14.7)')  'GAUSS2 (',I,',4) ', GAUSS2 (I,4)
        WRITE(6,'(A,I3,A,F14.7)')  'GAUSS3 (',I,',4) ', GAUSS3 (I,4)
        WRITE(6,'(A,I3,A,I7)')     'NATORB (',I,')  ', NATORB(I)
c#        write(6,*) "r0pmd(8)=",r0pmd(8)
c#        write(6,*) "lam1d, lpm3d, lpmov1, lmndod=",lam1d,lpm3d,
c#     &     lpmov1,lmndod
        IF (LAM1D) THEN
          WRITE(6,'(A,I3,A,F14.7)')  'RVDW   (',I,')   ', R0AMD(I)
          WRITE(6,'(A,I3,A,F14.7)')  'C6     (',I,')   ', C6AMD(I)
          WRITE(6,'(A,F14.7)')  'ALPD          ',    ALPAMD
          WRITE(6,'(A,F14.7)')  'S6            ',    S6AMD 
        ENDIF
        IF (LPM3D.or.LPMOV1.OR.LPMO2.OR.LPMO2A) THEN
          WRITE(6,'(A,I3,A,F14.7)')  'RVDW   (',I,')   ', R0PMD(I)
          WRITE(6,'(A,I3,A,F14.7)')  'C6     (',I,')   ', C6PMD(I)
          WRITE(6,'(A,F14.7)')  'ALPD          ',    ALPPMD
          WRITE(6,'(A,F14.7)')  'S6            ',    S6PMD 
        ENDIF
        IF (LMNDOD) THEN
          WRITE(6,'(A,I3,A,F14.7)')  'RVDW   (',I,')   ', R0MND(I)
          WRITE(6,'(A,I3,A,F14.7)')  'C6     (',I,')   ', C6MND(I)
          WRITE(6,'(A,F14.7)')  'ALPD          ',    ALPMND
          WRITE(6,'(A,F14.7)')  'S6            ',    S6MND 
        ENDIF
        IF (LSDAMP) THEN
          WRITE(6,'(A,F14.7)')  'ATOMIP       ',    ATOMIP(I)
        ENDIF
        IF (LPM6) THEN
          DO 55 J=1,120
          IF (J.NE.1.AND.J.NE.8) GOTO 55
          IF (I.LT.J) GOTO 55
          WRITE(6,'(A,I3,",",I3,A,F14.7)')'ALPPM6(',I,J,') ',ALPPM6(I,J)
          WRITE(6,'(A,I3,",",I3,A,F14.7)')'XPM6  (',I,J,') ',XPM6(I,J)
  55      CONTINUE
        ENDIF
        WRITE(6,*) ' '
      ENDIF
  60  CONTINUE
      DO 70 I=1,8
  70  WRITE(6,'(A,I3,A,F14.7)') 'PMODVL (',I,') ',PMODVL(I)

      write(6,*) "MOD5 values:"
  80  format("MOD5  C0AB(",I2,",",I2,"): ",F10.6)
  81  format("MOD5  C1AB(",I2,",",I2,"): ",F10.6)
  82  format("MOD5  C2AB(",I2,",",I2,"): ",F10.6)
  83  format("MOD5 ALPPR(",I2,",",I2,"): ",F10.6)
  84  format("MOD5  C3AB(",I2,",",I2,"): ",F10.6)
  85  format("MOD5 C3RPWR(",I2,",",I2,"): ",F10.6)
  86  format("MOD5 PR3ALP(",I2,",",I2,"): ",F10.6)
      do i=1,9
        do j=1,9
          write(6,80) i,j,c0ab(i,j)
        enddo
      enddo
      do i=1,9
        do j=1,9
          write(6,81) i,j,c1ab(i,j)
        enddo
      enddo
      do i=1,9
        do j=1,9
          write(6,82) i,j,c2ab(i,j)
        enddo
      enddo
      do i=1,9
        do j=1,9
          write(6,83) i,j,pralp(i,j)
        enddo
      enddo
      do i=1,9
        do j=1,9
          write(6,84) i,j,c3ab(i,j)
        enddo
      enddo
      do i=1,9
        do j=1,9
          write(6,85) i,j,c3rpwr(i,j)
        enddo
      enddo
      do i=1,9
        do j=1,9
          write(6,86) i,j,pr3alp(i,j)
        enddo
      enddo
      write(6,*) "MOD7 values:"
      write(6,'(/A)') "Pairwise beta parameters (PRBETA):"              LF0811
      write(6,'(6x,(16(2x,i2,12x)))') (k,k=1,16)                        LF0811
      write(6,'(6x,(16(3x,"s",4x,3x,"p",4x)))')                         LF0811
      do i=1,16                                                         LF0811
        write(6,'(x,i2," s ",32(f7.3,x))')                              LF0811
     &          i,((prbeta(i,k,1,l),l=1,2),k=1,16)                      LF0811
        write(6,'(x,i2," p ",32(f7.3,x))')                              LF0811
     &          i,((prbeta(i,k,2,l),l=1,2),k=1,16)                      LF0811
      enddo                                                             LF0811
      write(6,'(/A)') "Pairwise k parameters (KPAIR):"                  LF0811
      write(6,'(6x,(16(2x,i2,12x)))') (k,k=1,16)                        LF0811
      write(6,'(6x,(16(3x,"s",4x,3x,"p",4x)))')                         LF0811
      do i=1,16                                                         LF0811
        write(6,'(x,i2," s ",32(f7.3,x))')                              LF0811
     &          i,((kpair (i,k,1,l),l=1,2),k=1,16)                      LF0811
        write(6,'(x,i2," p ",32(f7.3,x))')                              LF0811
     &          i,((kpair (i,k,2,l),l=1,2),k=1,16)                      LF0811
      enddo                                                             LF0811
      write(6,*)                                                        LF0811
cobs  90  format("MOD7  PRBETA(",I4,"): ",F10.6)
cobs  91  format("MOD7  KPAIR (",I4,"): ",F10.6)
cobs      do i=1,544
cobs        write(6,90) i,prbeta(i)
cobs      enddo
cobs      do i=1,544
cobs        write(6,91) i,kpair(i)
cobs      enddo
      write (6,*) "     ================================"
      RETURN
 9999 RETURN 1                                                          CSTP
      END
