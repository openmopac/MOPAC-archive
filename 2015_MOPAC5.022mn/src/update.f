      SUBROUTINE UPDATE(IPARAM, IELMNT, PARAM, MODE,KFN,*)              CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*
*  UPDATE UPDATES THE COMMON BLOCKS WHICH HOLD ALL THE PARAMETERS FOR
*         RUNNING MNDO.
*         IPARAM REFERS TO THE TYPE OF PARAMETER,
*         IELMNT REFERS TO THE ELEMENT,
*         PARAM IS THE VALUE OF THE PARAMETER, AND
*         IF MODE = 1 THEN A COMMON BLOCK IS UPDATED,
*         IF MODE = 2 THEN A DATUM IS EXTRACTED FROM THE COMMON BLOCK.
*
************************************************************************
      COMMON /MNDO/  USSM(120), UPPM(120), UDDM(120), ZSM(120),ZPM(120),
     1ZDM(120), BETASM(120), BETAPM(120), BETADM(120), ALPM(120),
     2EISOLM(120), DDM(120), QQM(120), AMM(120), ADM(120), AQM(120)
     3,GSSM(120),GSPM(120),GPPM(120),GP2M(120),HSPM(120), POLVOM(120)
      COMMON /EXPONT/ ZS(120),ZP(120),ZD(120)
     1       /NATORB/ NATORB(120)
     2       /BETAS / BETAS(120),BETAP(120),BETAD(120)
     3       /VSIPS / VS(120),VP(120),VD(120)
     4       /ONELEC/ USS(120),UPP(120),UDD(120)
     5       /MULTIP/ DD(120),QQ(120),AM(120),AD(120),AQ(120)
     6       /TWOELE/ GSS(120),GSP(120),GPP(120),GP2(120),HSP(120)
     7                ,GSD(120),GPD(120),GDD(120)
     8       /ALPHA / ALP(120)
     9       /IDEAS / GAUSS1(120,10), GAUSS2(120,10), GAUSS3(120,10)
      COMMON /GAUSSB/ FN1(120),FN2(120)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
         SAVE                                                           GL0892
      GOTO
     1(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,
     2190,200,210,220,230,240,250),IPARAM
   10 USS (IELMNT)=PARAM
      USSM(IELMNT)=PARAM
      RETURN
   20 UPP (IELMNT)=PARAM
      UPPM(IELMNT)=PARAM
      RETURN
   30 UDD (IELMNT)=PARAM
      UDDM(IELMNT)=PARAM
      RETURN
   40 ZS (IELMNT)=PARAM
      ZSM(IELMNT)=PARAM
      RETURN
   50 ZP (IELMNT)=PARAM
      ZPM(IELMNT)=PARAM
      RETURN
   60 ZD (IELMNT)=PARAM
      ZDM(IELMNT)=PARAM
      RETURN
   70 BETAS (IELMNT)=PARAM
      BETASM(IELMNT)=PARAM
      RETURN
   80 BETAP (IELMNT)=PARAM
      BETAPM(IELMNT)=PARAM
      RETURN
   90 BETAD (IELMNT)=PARAM
      BETADM(IELMNT)=PARAM
      RETURN
  100 GSS (IELMNT)=PARAM
      GSSM(IELMNT)=PARAM
      RETURN
  110 GSP (IELMNT)=PARAM
      GSPM(IELMNT)=PARAM
      RETURN
  120 GPP (IELMNT)=PARAM
      GPPM(IELMNT)=PARAM
      RETURN
  130 GP2 (IELMNT)=PARAM
      GP2M(IELMNT)=PARAM
      RETURN
  140 HSP (IELMNT)=PARAM
      HSPM(IELMNT)=PARAM
      RETURN
  150 RETURN
  160 RETURN
  170 RETURN
  180 ALP (IELMNT)=PARAM
      ALPM(IELMNT)=PARAM
      RETURN
  190 RETURN
  200 RETURN
  210 RETURN
  220 GAUSS1(IELMNT,KFN)=PARAM
      RETURN
  230 GAUSS2(IELMNT,KFN)=PARAM
      RETURN
  240 GAUSS3(IELMNT,KFN)=PARAM
      RETURN
  250 NATORB(IELMNT)=PARAM
      I=INT(PARAM+0.5)
      IF(I.NE.9.AND.I.NE.4.AND.I.NE.1)THEN
         IF (DOPRNT) WRITE(6,'(///10X,                                  CIO
     &'' UNACCEPTABLE VALUE FOR NO. OF ORBITALS'',                      CIO
     &'' ON ATOM'')')
      RETURN 1                                                          CSTP (stop)
      ENDIF
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY UPDATE_INIT                                                 CSAV
CSAV  Appears that no local variables need to be initialized.  Leave ENTRY point
CSAV  in subprogram for time being.
      RETURN                                                            CSAV
      END
