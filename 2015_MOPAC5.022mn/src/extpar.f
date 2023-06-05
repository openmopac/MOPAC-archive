      SUBROUTINE EXTPAR(*)                                              CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
************************************************************************
      PARAMETER (MXNPAR=54)                                             LF0312
      CHARACTER NUMBRS(0:9)*1, PARTYP(MXNPAR)*5, FILES*64, DUMMY(2)*70, IR0494 / LF0211
     1          KEYWRD*160, TEXT*70, TXTNEW*70, ELEMNT(120)*2,
     2          NTEXT*70, TXTFRG*70,                                    LF0710              
     3          ATSYM1*2, ATSYM2*2                                      LF1212
      COMMON /ATHEAT/ ATHEAT
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /ATOMIC/ EISOL(120),EHEAT(120)
      COMMON /KEYWRD/ KEYWRD

*     **********************************************************************
*     COMMON BLOCKS FOR SRP (Ivan Rossi - April 94)                    
*     **********************************************************************
      COMMON /BETAS / BETAS(120),BETAP(120),BETAD(120)
      COMMON /SRPI/ IBTPTR(120), NATPTR(MXATSP), NATSP
     &       /SRPL/ ISSRP
     &       /SRPR/ BETSS(MAXBET), BETSP(MXATSP,MXATSP), BETPP(MAXBET)

*     **********************************************************************
*     COMMON BLOCKS FOR MNDO-D, AM1-D, AND PM3-D (D=DISPERSION) METHODS
*     **********************************************************************
      COMMON /PM3DSB/ ALPPMD,R0PMD(120),S6PMD,C6PMD(120),USSPMD(120),   LF0310
     &   UPPPMD(120),BETASD(120),BETAPD(120),ALPHAD(120),EISOLD(120)    LF0310
      COMMON /AM1DSB/ ALPAMD,R0AMD(120),S6AMD,C6AMD(120),USSAMD(120),   LF0310
     &   UPPAMD(120),BETASE(120),BETAPE(120),ALPHAE(120),EISOLE(120)    LF0310
      COMMON /MNDODS/ ALPMND,R0MND(120),S6MND,C6MND(120),USSMND(120),
     &   UPPMND(120),BETASF(120),BETAPF(120),ALPHAF(120),EISOLF(120),
     &   GSSMND(120),GSPMND(120),GPPMND(120),GP2MND(120),HSPMND(120),
     &   ZSMND(120),ZPMND(120)
      COMMON /RM1DSB/ ALPRMD,R0RMD(120),S6RMD,C6RMD(120),USSRMD(120),   LF0310
     &   UPPRMD(120),BETASI(120),BETAPI(120),ALPHAI(120),EISOLI(120)    LF0310
      COMMON /PM6DSB/ ALPP6D,R0P6D(120),S6P6D,C6P6D(120),USSP6D(120),   LF0310
     &   UPPP6D(120),BETASJ(120),BETAPJ(120),ALPHAJ(120),EISOLJ(120)    LF0310

*     **********************************************************************
*     COMMON BLOCK FOR PM6 METHOD
*     **********************************************************************
      COMMON /PM6BLO/ USSPM6(120), UPPPM6(120), UDDPM6(120),ZSPM6(120), LF0310
     &  ZPPM6(120), ZDPM6(120), BETASX(120), BETAPX(120), BETADX(120),  LF0310
     &  GSSPM6(120),GSPPM6(120),GPPPM6(120),GP2PM6(120),HSPPM6(120),    LF0310
     &  EISOLX(120), DDPM6(120), QQPM6(120), AMPM6(120), ADPM6(120),    LF0310
     &  AQPM6(120), GAUSX1(120,10), GAUSX2(120,10), GAUSX3(120,10),     LF0310
     &  ALPPM6(18,18), XPM6(18,18), ZSNPM6(18), ZPNPM6(18), ZDNPM6(18)  LF0310

*     **********************************************************************
*     COMMON BLOCK FOR STONE'S ALTERNATIVE DAMPING FUNCTION
*     **********************************************************************
      COMMON /IONPOT/ ATOMIP(120)                                       LF0310

*     **********************************************************************
*     COMMON BLOCK FOR WHETHER HP ATOMS ARE USED IN MOLECULAR SPECIFICATION
*     **********************************************************************
      COMMON /HPUSED/ HPUSED                                            LF0410
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
*     **********************************************************************
*     COMMON BLOCK FOR GRIMME'S -D3 DISPERSION PARAMETERS
*     **********************************************************************
      COMMON /D3PARAM/ C6AB(17,17,5,5,3),R0AB(17,17),D3SR6,D3S8,LSETD3,
     &                 LD3SR6,LD3S8
      LOGICAL          LSETD3, LD3SR6, LD3S8
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1010
      include 'corgen.f'   ! Common block declaration for /CORGEN/.     LF1010
      COMMON /TDAMPB/  CPAIR(16,16), DPAIR(16,16),                      LF0312
     &                 RPNUM, RPDEN, DPDEN, SPDEN,                      LF0312
     &                 USEDPR, USECPR                                   LF0312
      LOGICAL          USEDPR, USECPR                                   LF0312

      DIMENSION  IJPARS(5,500), PARSIJ(500), IBLNK(2)
CSAV         SAVE                                                           GL0892
      LOGICAL ISSRP                                                     IR0494
      LOGICAL PM6P                                                      LF0310/LF0312
      LOGICAL LPMOD5, LPMOD7                                            LF0211
      INTEGER IELMNT, IELMN2                                            LF0211
      INTEGER IEL1, IEL2                                                LF0211
      LOGICAL HPUSED                                                    LF0410

      DATA NUMBRS/' ','1','2','3','4','5','6','7','8','9'/


      DATA PARTYP/'USS  ','UPP  ','UDD  ','ZS   ','ZP   ',  ! index 01-05
     &            'ZD   ','BETAS','BETAP','BETAD','GSS  ',  ! index 06-10
     1            'GSP  ','GPP  ','GP2  ','HSP  ','AM1  ',  ! index 11-15
     &            'EXPC ','GAUSS','ALP  ','GSD  ','GPD  ',  ! index 16-20
     2            'GDD  ','FN1  ','FN2  ','FN3  ','ORB  ',  ! index 21-25
     &            'BETPP','BETSP','BETSS','ALPM6','XPM6 ',  ! index 26-30
     3            'RVDW ','C6   ','S6   ','ALPD ','ATMIP',  ! index 31-35
     &            'PMODS','C0AB ','C1AB ','C2AB ','ALPPR',  ! index 36-40
     4            'KPRSS','KPRSP','KPRPP','C3AB ','ALPP3',  ! index 41-45
     &            'C3PWR','SR6  ','S8   ','CPAIR','DPAIR',  ! index 46-50
     5            'RPNUM','RPDEN','DPDEN','SPDEN'        /  ! index 51-


C     LF0210: Expanded the number of atom-type symbols in ELEMNT 
C             to 120 from 107.  Added element 108, 'Hp'.
      DATA (ELEMNT(I),I=1,120)/'H','HE',
     1 'LI','BE','B','C','N','O','F','NE',
     2 'NA','MG','AL','SI','P','S','CL','AR',
     3 'K','CA','SC','TI','V','CR','MN','FE','CO','NI','CU',
     4 'ZN','GA','GE','AS','SE','BR','KR',
     5 'RB','SR','Y','ZR','NB','MO','TC','RU','RH','PD','AG',
     6 'CD','IN','SN','SB','TE','I','XE',
     7 'CS','BA','LA','CE','PR','ND','PM','SM','EU','GD','TB','DY',
     8 'HO','ER','TM','YB','LU','HF','TA','W','RE','OS','IR','PT',
     9 'AU','HG','TL','PB','BI','PO','AT','RN',
     1 'FR','RA','AC','TH','PA','U','NP','PU','AM','CM','BK','CF','XX',
     2 'FM','MD','CB','++','+','--','-','TV',
     3 'HP','','','','','','','',                                       LF0310
     4 '','','','',''/                                                  LF0310
      DATA (IBLNK(i),i=1,2) / 32, 160/                                  IR0494 
      PM6P=.FALSE.                                                      LF0610
      LPMOD5=.FALSE.                                                    LF0211
      LPMOD7=.FALSE.                                                    LF0211
      D3SR6=-1.0D0                                                      LF1211
      D3S8 =-1.0D0                                                      LF1211
      LD3SR6=.FALSE.                                                    LF1211
      LD3S8 =.FALSE.                                                    LF1211
      LPARS=0                                                           LF0610
      DATA ITAB /9/                                                     LF0710     
      DO 5 I=1,160                                                      LF0710
    5 IF (ICHAR(KEYWRD(I:I)).EQ.ITAB) KEYWRD(I:I)=' '                   LF0710
      ISSRP=.FALSE.                                                     IR0494
      I=INDEX(KEYWRD,'EXTERNAL=')+9
      J=INDEX(KEYWRD(I:),' ')+I-1
      FILES=KEYWRD(I:J)
      IF (DOPRNT) THEN                                                  CIO
      WRITE(6,'(//5X,'' EXTERNAL PARAMETERS LOADED:'')')                LF0310
      WRITE(6,'(//5X,'' PARAMETER TYPE      ELEMENT    PARAMETER'')')  
      ENDIF                                                             CIO
      OPEN(14,STATUS='OLD',FILE=FILES)
      I=0
   10 CONTINUE
      READ(14,'(A69)',ERR=90,IOSTAT=IOS)TEXT
      DO 15 II=1,LEN(TEXT)                                              LF1211
        IF (ICHAR(TEXT(II:II)).EQ.ITAB) TEXT(II:II)=' '                 LF1211
   15 CONTINUE
      IF (TEXT(1:1).NE.' ') GOTO 17                                     LF1211
      NF=0                                                              LF1211
      DO 16 II=1,LEN_TRIM(TEXT)                                         LF1211
        IF (NF.EQ.0.AND.TEXT(II:II).NE.' ') NF=II                       LF1211
   16 CONTINUE                                                          LF1211
      IF (NF.EQ.0) GOTO 90                                              LF1211
      NTEXT=TEXT(NF:)                                                   LF1211
      TEXT=NTEXT                                                        LF1211
   17 CONTINUE                                                          LF1211
      IF(INDEX(TEXT,'END').NE.0)GOTO 90
      ILOWA = ICHAR('a')
      ILOWZ = ICHAR('z')
      ICAPA = ICHAR('A')
************************************************************************
      DO 20 I=1,70
         ILINE=ICHAR(TEXT(I:I))
         IF(ILINE.GE.ILOWA.AND.ILINE.LE.ILOWZ) THEN
            TEXT(I:I)=CHAR(ILINE+ICAPA-ILOWA)
         ENDIF
   20 CONTINUE
************************************************************************
      IF(INDEX(TEXT,'END') .NE. 0) GOTO 90
C     See if the PMODS parameter type is specified.                     LF9010
      IF(INDEX(TEXT,'PMODS') .NE. 0) THEN                               LF0910
         it=index(text,"PMODS")
         txtnew=text(it+5:)
            NUMPAR=INT(READA(TXTNEW,1))
            IF (NUMPAR.LT.1.OR.NUMPAR.GT.7) THEN
              IF (DOPRNT) WRITE(6,*) "ILLEGAL PMODS VALUE NUMBER."
              RETURN 1
            ENDIF
         ll=len(txtnew)
         nn=0
  21     nn=nn+1
         if (nn.eq.ll) then
           IF (DOPRNT) write(6,*) "No index found for PMODS."
           return 1
         endif
         if (txtnew(nn:nn).eq." ") goto 21
  22     nn=nn+1
         if (nn.eq.ll) then
           IF (DOPRNT) write(6,*) "No value found for PMODS."
           return 1
         endif
         if (txtnew(nn:nn).ne." ") goto 22
         txtnew=txtnew(nn+1:)
         PMODVL(NUMPAR)=READA(TXTNEW,1)
c#         write(6,'(a,f30.25)') "X:  pmodvl(2)=",pmodvl(2)
         if (doprnt) write(6,'(10X,A,I2,5X,F12.6)')
     &          "PMODS parameter #",NUMPAR,PMODVL(NUMPAR)
         goto 10
      ENDIF
      DO 30 J=1,25
C        Search for a match against only the first 25 paramater types.  LF0310
C        Searches for the rest of the parameter types occurs below.     LF0310
C        Caution: 'ALP  ' can be confused with 'ALPM6' or 'PRALP'.      LF0211
         IF(J.GE.22.AND.J.LE.24) THEN
            IT=INDEX(TEXT,'FN')
            TXTNEW = TEXT(1:IT+2)
            IF(INDEX(TXTNEW,PARTYP(J)) .NE. 0) GOTO 40
         ENDIF
         IF(INDEX(TEXT,PARTYP(J)) .NE. 0) GOTO 40
   30 CONTINUE
C
C SPECIAL SRP BETA initialization (Ivan Rossi - April '94)
C
      DO 31 J=26,28
         IF(INDEX(TEXT,PARTYP(J)) .NE. 0) THEN 
            IF(.NOT. ISSRP) THEN 
               ISSRP=.TRUE.                          
C Check if one of the parameters specified before is a BETA and update
               DO 32 IPAR=1,LPARS
                 IF (IJPARS(3,IPAR).eq. 7 .or.IJPARS(3,IPAR).eq.8) THEN 
                    CALL UPDATE( IJPARS(3,IPAR), IJPARS(2,IPAR),
     1                           PARSIJ(IPAR), 1, IJPARS(1,IPAR) ,*9999) CSTP(call)
                 ENDIF
  32           CONTINUE
c#               write(6,*) "INIBET being called."
c#               do jj=1,9
c#               write(6,*)"labels(",jj,")= ",labels(jj)
c#               enddo
               CALL INIBET(*9999)                                        CSTP(call)
            ENDIF
            GOTO 40
         ENDIF                                  
  31  CONTINUE
C
C End SRP BETA init
C


C Special PM6 method parameter input (LF0310)
      PM6P=.FALSE.
      DO 33 J=29,30
        IF (INDEX(TEXT,PARTYP(J)) .NE. 0) THEN
           PM6P=.TRUE.
           GOTO 40
        ENDIF
   33 CONTINUE


C Special Dispersion method (e.g. AM1-D) parameter input (LF0310)
      DO 34 J=31,34
        IF (INDEX(TEXT,PARTYP(J)) .NE. 0) THEN
           IF (J.EQ.31.OR.J.EQ.32) THEN
              GOTO 40
           ELSEIF (J.EQ.33.OR.J.EQ.34) THEN
              GOTO 40
           ENDIF
        ENDIF
   34 CONTINUE
      
C Special MOD5 parameters (C0AB, C1AB, C2AB, PRALP) (LF0211)
      LPMOD5=.FALSE.
      DO 35 J=37,40
        IF (INDEX(TEXT,PARTYP(J)) .NE. 0) THEN
           LPMOD5=.TRUE.
           GOTO 40
        ENDIF
   35 CONTINUE
      DO 37 J=44,46   ! Check for C3AB, ALPP3, C3PWR (LF0211)
        IF (INDEX(TEXT,PARTYP(J)) .NE. 0) THEN
           LPMOD5=.TRUE.
           GOTO 40
        ENDIF
   37 CONTINUE

C Special -D3 dispersion parameters (D3SR6, D3S8) (LF1211)
      DO J=47,48
        IF (INDEX(TEXT,PARTYP(J)) .NE. 0) THEN
           GOTO 40
        ENDIF
      ENDDO

C Special pairwise dispersion C and D parameters (CPAIR, DPAIR) (LF0312)
      DO J=49,50
        IF (INDEX(TEXT,PARTYP(J)) .NE. 0) THEN
           GOTO 40
        ENDIF
      ENDDO

C Special MOD7 parameters (KPRSS,KPRSP,KPRPP) (LF0211)
      LPMOD7=.FALSE.
      DO 36 J=41,43
        IF (INDEX(TEXT,PARTYP(J)) .NE. 0) THEN
           LPMOD7=.TRUE.
           GOTO 40
        ENDIF
   36 CONTINUE
      
C Special atomic ionization potential for Stone's alt. damping function (LF0310)
      IF (INDEX(TEXT,PARTYP(35)).NE.0) THEN
        J=35
        GOTO 40
      ENDIF

C Special constants used in Truhlar group dispersion function.  If
C present any parameters in the external parameter file will override
C those on the command line if the TDAMP=_,_,_,_ option is used to
C specify the parameters instead of the bare TDAMP keyword. (LF1112)
      DO 38 J=51,54
        IF (INDEX(TEXT,PARTYP(J)) .NE. 0) THEN
           GOTO 40
        ENDIF
   38 CONTINUE

C Terminate program with error message if keyword was not identified.
      IF (DOPRNT) THEN                                                  CIO
      WRITE(6,'(''  FAULTY LINE:'',A)')TXTNEW
      WRITE(6,'(''  FAULTY LINE:'',A)')TEXT
      WRITE(6,'(''   NAME NOT FOUND'')')
      ENDIF                                                             CIO
      RETURN 1                                                           CSTP (stop)
C
C Starts looking for element name(s) and/or parameter values here.
C
   40 IPARAM=J
      IELMNT=0                                                          LF1211
      IF(IPARAM.GE.22 .AND. IPARAM.LE.25) THEN                          IR0494
         I=INDEX(TEXT,'FN')
         KFN=READA(TEXT,I+3)
      ELSE
         KFN=0
         I=INDEX(TEXT,PARTYP(IPARAM))
      ENDIF
      K=INDEX(TEXT(I:),' ')+1
      DUMMY(1)=TEXT(K:)
      TEXT=DUMMY(1)
      I=1
C Quick and dirty parsing for the first element in the string           IR0494
C      i=i+3
   42 NASC=ICHAR(text(i:i))
      IF(NASC .eq. IBLNK(1) .or. NASC .eq. IBLNK(2)) goto 43
      i=i+1
      goto 42
C
C LF0310: Skip element name search if parameter does not depend
C         on element/atom type.  Start search for first element
C         name here otherwise.
C
   43 CONTINUE                                                          LF0310
      IF ((IPARAM.EQ.33.OR.IPARAM.EQ.34).OR.                            LF0310
     &    (IPARAM.EQ.47.OR.IPARAM.EQ.48).OR.                            LF1211/LF0312
     &    (IPARAM.GE.51.AND.IPARAM.LE.54)) THEN                         LF1112/LF1212
C        No element name specified as S6 and ALPD apply to all elements.
         PARAM=READA(TEXT,I)                                            LF0310
c#         write(6,*) "checkpoint alpha: iparam,param,ielmnt=",iparam,
c#     &               param,ielmnt
         GOTO 65                                                        LF0310
      ENDIF                                                             LF0310
C
   45 CONTINUE                                                          LF0312
c#      write(6,*) "checkpoint echo: iparam=",iparam
      NASC=ICHAR(TEXT(I:I))
      IF(NASC .ne. IBLNK(1) .and. NASC .ne. IBLNK(2)) goto 44
      i=i+1
      goto 45                                                           LF0312
   44 DUMMY(1)=TEXT(i-1 : i+1)
      DUMMY(2)=TEXT(i:)
      TEXT=DUMMY(2)
      DO 50 J=1,120
   50 IF(INDEX(DUMMY(1), ' '//ELEMNT(J)) .NE. 0) GOTO 60
   51 IF (DOPRNT) WRITE(6,'('' ELEMENT NOT FOUND '')')                  CIO
      IF (DOPRNT) WRITE(6,*)' FAULTY LINE: "'//TEXT//'"'                CIO
      RETURN 1                                                          CSTP (stop)
   60 IELMNT=J
c#      write(6,*) "checkpoint foxtrot: iparam,ielmnt=",iparam,ielmnt
C
C READ second element for SRP BETA and CHECK (Ivan Rossi - April '94)
C Also read second element for PM6 pairwise parameters and any
C other diatomic parameters here.
C
      ielmn2=-1
      IF ((IPARAM.GE.26.AND.IPARAM.LE.30).OR.
     &    (IPARAM.GE.37.AND.IPARAM.LE.46).OR.                  
     &    (IPARAM.GE.49.AND.IPARAM.LE.50)) THEN                         LF0211
         DUMMY(2)=TEXT(3:) 
         TEXT=DUMMY(2)
         DO 61 J=1,120
  61        IF(INDEX(TEXT,' '//ELEMNT(J)) .NE. 0) goto 62
C ...if second element is wrong then error
         GOTO 51                                                        LF0310
  62     IF(J .gt. 120 ) GOTO 51
         IELMN2=j
      ENDIF
C
C END READ second element
C
  63  CONTINUE                                                          LF0310
      PARAM=READA(TEXT,INDEX(TEXT,ELEMNT(J)))
c#      write(6,*) "ielmnt,ielmn2,param,iparam=",ielmnt,
c#     &          ielmn2,param,iparam
      if (ielmnt.eq.108.and.hpused) iela  =9                            LF0211
      if (ielmn2.eq.108.and.hpused) ielb  =9                            LF0211
C
C Clarification note by Luke Fiedler, March 2012:
C At this point IELMNT should be element #1 and IELMN2 should be element #2 (if the
C parameter is a diatomic parameter requiring two element names).  Also the variable
C PARAM should have the actual value of the parameter being assigned and the variable
C IPARAM is the index telling which parameter it is (e.g. USS, BETSS, etc.; see the
C array "PARTYP" for keywords associated with IPARAM values).
C 
C The immediately following IF...ENDIF sections are for different diatomic parameter types.
C Otherwise monoatomic parameters and universal parameters are handled after label "65".
C
C SET special SRP BETA (Ivan Rossi - April '94)
C
      IF(ISSRP.AND.IPARAM.GE.26.AND.IPARAM.LE.28) THEN
         iela=ielmnt
         ielb=ielmn2
C ******
C Allow Hp atoms to use atomic # 9 in look-ups to SRP beta parameter arrays.
C Do not assign Hp atoms to atomic # 108 for the SRP beta arrays.
C ******
         if (ielmnt.eq.108.and.hpused) iela  =9 
         if (ielmn2.eq.108.and.hpused) ielb  =9 
c#         write(6,*) "Doing SETBET for elements# ",iela,ielb
         CALL SETBET( IELA  , IELB  , PARTYP(IPARAM), PARAM,*9999)      CSTP(call)
         IF (DOPRNT) WRITE(6,'(10x,"SRP",A6,8X,A2,1X,A2,F14.6)')        CIO
     &            PARTYP(IPARAM),ELEMNT(IELMNT),ELEMNT(IELMN2),PARAM    CIO
      ENDIF
C
C end SET 
C

C
C Set DPAIR parameters (LF0312)
C
      IF (IPARAM.EQ.50) THEN                                            LF0312
         iel1=IELMNT                                                    LF0312
         iel2=IELMN2                                                    LF0312
         if (iel1.eq.108.and.hpused) iel1=9                             LF0312
         if (iel2.eq.108.and.hpused) iel2=9                             LF0312
         IF (iel1.GT.16.OR.iel2.GT.16) THEN                             LF0312
            IF (DOPRNT) WRITE(6,*) " ** CANNOT SPECIFY DPAIR PARAMETERS"LF0312
     &                        //" FOR ELEMENTS BEYOND ATOMIC NO. 16."   LF0312
            RETURN 1                                                    LF0312
         ENDIF                                                          LF0312
         DPAIR(iel1,  iel2  )=PARAM                                     LF0312
         DPAIR(iel2,  iel1  )=PARAM                                     LF0312
         IEL1=MIN(IELMNT,IELMN2)                                        LF0312
         IEL2=MAX(IELMNT,IELMN2)                                        LF0312
         IF (DOPRNT) WRITE(6,'(9x,A6,12X,A2,1X,A2,F14.6)')              LF0312
     &            PARTYP(IPARAM),ELEMNT(IEL1  ),ELEMNT(IEL2  ),PARAM    LF0312
      ENDIF                                                             LF0312

C
C Set CPAIR parameters (LF0312)
C
      IF (IPARAM.EQ.49) THEN                                            LF0312
         iel1=IELMNT                                                    LF0312
         iel2=IELMN2                                                    LF0312
         if (iel1.eq.108.and.hpused) iel1=9                             LF0312
         if (iel2.eq.108.and.hpused) iel2=9                             LF0312
         IF (iel1.GT.16.OR.iel2.GT.16) THEN                             LF0312
            IF (DOPRNT) WRITE(6,*) " ** CANNOT SPECIFY CPAIR PARAMETERS"LF0312
     &                        //" FOR ELEMENTS BEYOND ATOMIC NO. 16."   LF0312
            RETURN 1                                                    LF0312
         ENDIF                                                          LF0312
         CPAIR(iel1,  iel2  )=PARAM                                     LF0312
         CPAIR(iel2,  iel1  )=PARAM                                     LF0312
         IEL1=MIN(IELMNT,IELMN2)                                        LF0312
         IEL2=MAX(IELMNT,IELMN2)                                        LF0312
         IF (DOPRNT) WRITE(6,'(9x,A6,12X,A2,1X,A2,F14.6)')              LF0312
     &            PARTYP(IPARAM),ELEMNT(IEL1  ),ELEMNT(IEL2  ),PARAM    LF0312
      ENDIF                                                             LF0312

C
C Set PM6 parameters (LF0310)
C
      IF (PM6P.AND.IPARAM.GE.29.AND.IPARAM.LE.30) THEN                  LF0310
         IEL1=MIN(IELMNT,IELMN2)                                        LF0310
         IEL2=MAX(IELMNT,IELMN2)                                        LF0310
         IF (IPARAM.EQ.29) THEN                                         LF0310
            ALPPM6(IEL2,IEL1)=PARAM                                     LF0310
         ELSEIF (IPARAM.EQ.30) THEN                                     LF0310
            XPM6(IEL2,IEL1)=PARAM                                       LF0310
         ENDIF                                                          LF0310
         IF (DOPRNT) WRITE (6,'(10X,"PM6",A6,8X,A2,X,A2,F14.6)')        LF0310 / CIO
     &          PARTYP(IPARAM),ELEMNT(IELMNT),ELEMNT(IELMN2),PARAM      LF0310 / CIO
      ENDIF                                                             LF0310
C
C End set PM6 parameters
C Start set MOD5 parameters (LF0211)
C
      IF (LPMOD5.AND.((IPARAM.GE.37.AND.IPARAM.LE.40).OR.               LF0211
     &                (IPARAM.GE.44.AND.IPARAM.LE.46))) THEN            LF0211
         IEL1=IELMNT
         IEL2=IELMN2
         if (iel1.eq.108) iel1=9
         if (iel2.eq.108) iel2=9
c#         write(6,*) ""
c#         write(6,*) "text  =",text
c#         write(6,*) "iparam=",iparam
c#         write(6,*) "iel1  =",iel1
c#         write(6,*) "iel2  =",iel2
         IF (IPARAM.EQ.37) THEN
            C0AB(IEL1,IEL2)=PARAM
            C0AB(IEL2,IEL1)=PARAM
         ELSEIF (IPARAM.EQ.38) THEN
            C1AB(IEL1,IEL2)=PARAM                                       LF0911
            C2AB(IEL2,IEL1)=PARAM                                       LF0911
         ELSEIF (IPARAM.EQ.39) THEN
            C2AB(IEL1,IEL2)=PARAM                                       LF0911
            C1AB(IEL2,IEL1)=PARAM                                       LF0911
         ELSEIF (IPARAM.EQ.40) THEN
            PRALP(IEL1,IEL2)=PARAM
            PRALP(IEL2,IEL1)=PARAM
         ELSEIF (IPARAM.EQ.44) THEN
            C3AB (IEL1,IEL2)=PARAM
            C3AB (IEL2,IEL1)=PARAM
         ELSEIF (IPARAM.EQ.44) THEN
            C3AB (IEL1,IEL2)=PARAM
            C3AB (IEL2,IEL1)=PARAM
         ELSEIF (IPARAM.EQ.45) THEN
            PR3ALP(IEL1,IEL2)=PARAM
            PR3ALP(IEL2,IEL1)=PARAM
         ELSEIF (IPARAM.EQ.46) THEN
            C3RPWR(IEL1,IEL2)=PARAM
            C3RPWR(IEL2,IEL1)=PARAM
         ELSE
            WRITE(6,*) "Program should not be here in EXTPAR!"
            RETURN 1
         ENDIF
         ATSYM1=ELEMNT(IEL1)                                            LF1212
         ATSYM2=ELEMNT(IEL2)                                            LF1212
         IF (HPUSED.AND.IEL1.EQ.9) ATSYM1="HP"                          LF1212
         IF (HPUSED.AND.IEL2.EQ.9) ATSYM2="HP"                          LF1212
         IF (DOPRNT) WRITE(6,'(10X,"MOD5",A6,7X,A2,X,A2,F14.6)')
     &      PARTYP(IPARAM),ATSYM1,ATSYM2,PARAM                          LF1212
      ENDIF
C
C End set MOD5 parameters
C Start set MOD7 parameters (LF0211)
C
      IF (LPMOD7.AND.IPARAM.GE.41.AND.IPARAM.LE.43) THEN
         if (ielmnt.eq.108) ielmnt=9
         if (ielmn2.eq.108) ielmn2=9
         IEL1=MAX(IELMNT,IELMN2)
         IEL2=MIN(IELMNT,IELMN2)
         baseindex=(IEL1*(IEL1-1)/2+IEL2)*4-3
c#         write(6,*) ""
c#         write(6,*) "text  =",text
c#         write(6,*) "ielmnt=",ielmnt
c#         write(6,*) "ielmn2=",ielmn2
c#         write(6,*) "iparam=",iparam
c#         write(6,*) "iel1  =",iel1
c#         write(6,*) "iel2  =",iel2
c#         write(6,*) "baseindex=",baseindex
         ATSYM1=ELEMNT(IELMNT)                                          LF1212
         ATSYM2=ELEMNT(IELMN2)                                          LF1212
         IF (HPUSED.AND.IELMNT.EQ.9) ATSYM1="HP"                        LF1212
         IF (HPUSED.AND.IELMN2.EQ.9) ATSYM2="HP"                        LF1212
         IF (IPARAM.EQ.41) THEN   ! k value for s on A, s on B.
            kpair(ielmnt,ielmn2,1,1)=param                              LF0811
            kpair(ielmn2,ielmnt,1,1)=param                              LF0811
            if (DOPRNT) write(6,'(10X,"MOD7",A6,7X,A2,X,A2,F14.6)')
     &         PARTYP(IPARAM),ATSYM1,ATSYM2,PARAM                       LF1212
         ELSEIF (IPARAM.EQ.42) THEN
               kpair(ielmnt,ielmn2,1,2)=param                           LF0811
               kpair(ielmn2,ielmnt,2,1)=param                           LF0811
               if (DOPRNT) write(6,'(10X,"MOD7",A6,7X,A2,X,A2,F14.6)')
     &            PARTYP(IPARAM),ATSYM1,ATSYM2,PARAM                    LF1212
         ELSEIF (IPARAM.EQ.43) THEN
            kpair(ielmnt,ielmn2,2,2)=param                              LF0811
            kpair(ielmn2,ielmnt,2,2)=param                              LF0811
            if (DOPRNT) write(6,'(10X,"MOD7",A6,7X,A2,X,A2,F14.6)')
     &         PARTYP(IPARAM),ATSYM1,ATSYM2,PARAM                       LF1212
         ELSE
            WRITE(6,*) "Program should not be here in EXTPAR!"
            STOP
         ENDIF
      ENDIF
C
C End set MOD7 parameters
C
C
C
C
   65 CONTINUE                                                          LF0310
c#      write(6,*) "checkpoint delta: lpars,ielmnt,iparam=",lpars,ielmnt,
c#     &                iparam
      DO 70 I=1,LPARS
         IF(IJPARS(1,I).EQ.KFN.AND.IJPARS(2,I).EQ.IELMNT.AND.
     1IJPARS(3,I).EQ.IPARAM) GOTO 80
   70 CONTINUE
      LPARS=LPARS+1
      I=LPARS
   80 IJPARS(1,I)=KFN        ! KFN is the second digit in "FN1_", "FN2_", and "FN3_" (as underscore here).
      IJPARS(2,I)=IELMNT
      IJPARS(3,I)=IPARAM
      PARSIJ(I)=PARAM
      GOTO 10
   90 CONTINUE
      CLOSE(14)
      DO 120 J=1,120
         DO 110 K=1,MXNPAR                                              LF0312
C           Skip for element pair parameters.  They must be done above not here.
C           This section is only for monoatomic and universal parameter types.
            IF (K.GE.26.AND.K.LE.30) GOTO 110                           LF0310
            IF (K.GE.49.AND.K.LE.50) GOTO 110                           LF0312
            IF (K.GE.37.AND.K.LE.46) GOTO 110                           LF0312
            DO 100 I=1,LPARS
               IPARAM=IJPARS(3,I)
               KFN=IJPARS(1,I)
               IELMNT=IJPARS(2,I)
               IF(IPARAM.NE.K) GOTO 100
               IF(IELMNT.NE.J) GOTO 100
               PARAM=PARSIJ(I)
               IF(KFN.NE.0)THEN
                  IF (DOPRNT) WRITE(6,'(10X,A6,11X,A2,F17.6)')          CIO
     1PARTYP(IPARAM)(:3)//NUMBRS(KFN)//'  ',
     2ELEMNT(IELMNT),PARAM
               ELSE
c#                  if (iparam.eq.32) write(6,*) "At checkpoint bravo."
                  IF (DOPRNT) WRITE(6,'(10X,A6,11X,A2,F17.6)')          CIO
     1PARTYP(IPARAM)//NUMBRS(KFN),
     2ELEMNT(IELMNT),PARAM
               ENDIF
               IF (K.LE.25) THEN                                        LF0310
C                Note that UPDATE subroutine is only for first 25 parameter types.
                 CALL UPDATE(IPARAM,IELMNT,PARAM,1,KFN,*9999)           LF0310 CSTP(call)
               ELSEIF (K.EQ.35) THEN                                    LF0310
                 ATOMIP(IELMNT)=PARAM                                   LF0310
               ENDIF                                                    LF0310
  100       CONTINUE
  110    CONTINUE
  120 CONTINUE
      DO 140 I=1,LPARS                                                  LF1211
         IPARAM=IJPARS(3,I)                                             LF1211
         KFN=IJPARS(1,I)                                                LF1211
         IELMNT=IJPARS(2,I)                                             LF1211
         PARAM=PARSIJ(I)                                                LF1211
         IF (IPARAM.EQ.31) THEN                                         LF0312
            IF (ielmnt.eq.108.and.hpused) ielmnt=1                      LF0512
            R0AMD(IELMNT)=PARAM                                         LF0312
            R0PMD(IELMNT)=PARAM                                         LF0312
            R0MND(IELMNT)=PARAM                                         LF0312
            R0RMD(IELMNT)=PARAM                                         LF0312
            R0P6D(IELMNT)=PARAM                                         LF0312
         ELSEIF (IPARAM.EQ.32) THEN                                     LF0312
c#            write(6,*) "At checkpoint charlie."
            IF (ielmnt.eq.108.and.hpused) ielmnt=1                      LF0512
            C6AMD(IELMNT)=PARAM                                         LF0312
            C6PMD(IELMNT)=PARAM                                         LF0312
            C6MND(IELMNT)=PARAM                                         LF0312
            C6RMD(IELMNT)=PARAM                                         LF0312
            C6P6D(IELMNT)=PARAM                                         LF0312
            IF (DOPRNT) WRITE(6,'(10X,A6,11X,A2,F17.6)')                LF0312
     &                  PARTYP(IPARAM)//NUMBRS(KFN),                    LF0312
     &                  ELEMNT(IELMNT),PARAM                            LF0312
         ELSEIF (IPARAM.EQ.33) THEN                                     LF1211
            S6AMD=PARAM                                                 LF1211
            S6PMD=PARAM                                                 LF1211
            S6MND=PARAM                                                 LF1211
            S6RMD=PARAM                                                 LF1211
            S6P6D=PARAM                                                 LF1211
            IF (DOPRNT) WRITE(6,'(10X,A6,11X,''ALL'',F16.6)')           LF1211
     &                  PARTYP(IPARAM)//NUMBRS(KFN),PARAM               LF1211
         ELSEIF (IPARAM.EQ.34) THEN                                     LF1211
            ALPAMD=PARAM                                                LF1211
            ALPPMD=PARAM                                                LF1211
            ALPMND=PARAM                                                LF1211
            ALPRMD=PARAM                                                LF1211
            ALPP6D=PARAM                                                LF1211
            IF (DOPRNT) WRITE(6,'(10X,A6,11X,''ALL'',F16.6)')           LF1211
     &                  PARTYP(IPARAM)//NUMBRS(KFN),PARAM               LF1211
         ELSEIF (IPARAM.EQ.47) THEN                                     LF1211
            D3SR6=PARAM                                                 LF1211
            LD3SR6=.TRUE.                                               LF1211
            IF (DOPRNT) WRITE(6,'(10X,A6,11X,''ALL'',F16.6)')           LF1211
     &                  PARTYP(IPARAM)//NUMBRS(KFN),PARAM               LF1211
         ELSEIF (IPARAM.EQ.48) THEN                                     LF1211
            D3S8=PARAM                                                  LF1211
            LD3S8=.TRUE.                                                LF1211
            IF (DOPRNT) WRITE(6,'(10X,A6,11X,''ALL'',F16.6)')           LF1211
     &                  PARTYP(IPARAM)//NUMBRS(KFN),PARAM               LF1211
         ELSEIF (IPARAM.EQ.51) THEN                                     LF1112
            RPNUM=PARAM                                                 LF1112
            IF (DOPRNT) WRITE(6,'(10X,A6,11X,''ALL'',F16.6)')           LF1112
     &                  PARTYP(IPARAM)//NUMBRS(KFN),PARAM               LF1112
         ELSEIF (IPARAM.EQ.52) THEN                                     LF1112
            RPDEN=PARAM                                                 LF1112
            IF (DOPRNT) WRITE(6,'(10X,A6,11X,''ALL'',F16.6)')           LF1112
     &                  PARTYP(IPARAM)//NUMBRS(KFN),PARAM               LF1112
         ELSEIF (IPARAM.EQ.53) THEN                                     LF1112
            DPDEN=PARAM                                                 LF1112
            IF (DOPRNT) WRITE(6,'(10X,A6,11X,''ALL'',F16.6)')           LF1112
     &                  PARTYP(IPARAM)//NUMBRS(KFN),PARAM               LF1112
         ELSEIF (IPARAM.EQ.54) THEN                                     LF1112
            SPDEN=PARAM                                                 LF1112
            IF (DOPRNT) WRITE(6,'(10X,A6,11X,''ALL'',F16.6)')           LF1112
     &                  PARTYP(IPARAM)//NUMBRS(KFN),PARAM               LF1112
         ENDIF                                                          LF1211
  140 CONTINUE                                                          LF1211
      CALL MOLDAT(1,*9999)                                              0304WH93 CSTP(call)
      CALL CALPAR(*9999)                                                 CSTP(call)
      ATHEAT=0.D0
      ETH=0.D0
      DO 130 I=1,NUMAT
         NI=NAT(I)
         ATHEAT=ATHEAT+EHEAT(NI)
  130 ETH=ETH+EISOL(NI)
      ATHEAT=ATHEAT-ETH*23.061D0
      RETURN
 9999 RETURN 1                                                          CSTP
      END
