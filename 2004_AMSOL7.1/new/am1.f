      SUBROUTINE AM1 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
      CHARACTER NUMBRS(0:9)*1, PARTYP(25)*5, FILES*64, 
     1          TEXT*50, TXTNEW*50, ELEMNT(107)*2                       DJG0995
      COMMON /ATHEAT/ ATHEAT 
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /ATOMIC/ EISOL(107),EHEAT(107) 
      DIMENSION  IJPARS(5,99), PARSIJ(99) 
      SAVE
      DATA NUMBRS/' ','1','2','3','4','5','6','7','8','9'/ 
      DATA PARTYP/'USS  ','UPP  ','UDD  ','ZS   ','ZP   ','ZD   ', 
     1    'BETAS','BETAP','BETAD','GSS  ','GSP  ','GPP  ','GP2  ', 
     2    'HSP  ','AM1  ','EXPC ','GAUSS','ALP  ','GSD  ','GPD  ', 
     3    'GDD  ','FN1  ','FN2  ','FN3  ','ORB  '/ 
      DATA (ELEMNT(I),I=1,107)/'H ','HE', 
     1 'LI','BE','B ','C ','N ','O ','F ','NE', 
     2 'NA','MG','AL','SI','P ','S ','CL','AR', 
     3 'K ','CA','SC','TI','V ','CR','MN','FE','CO','NI','CU', 
     4 'ZN','GA','GE','AS','SE','BR','KR', 
     5 'RB','SR','Y ','ZR','NB','MO','TC','RU','RH','PD','AG', 
     6 'CD','IN','SN','SB','TE','I ','XE', 
     7 'CS','BA','LA','CE','PR','ND','PM','SM','EU','GD','TB','DY', 
     8 'HO','ER','TM','YB','LU','HF','TA','W ','RE','OS','IR','PT', 
     9 'AU','HG','TL','PB','BI','PO','AT','RN', 
     1 'FR','RA','AC','TH','PA','U ','NP','PU','AM','CM','BK','CF','XX', 
     2 'FM','MD','NO','++','+','--','-','TV'/ 
C      I=INDEX(KEYWRD,'EXTERNAL=')+9 
C      J=INDEX(KEYWRD(I:),' ')+I-1 
C      FILES=KEYWRD(I:J) 
      FILES=CIEXTE                                                      DJG0995
      WRITE(JOUT,                                                       GDH1095
     .    '(//5X,'' PARAMETER TYPE      ELEMENT    PARAMETER'')') 
      OPEN(JDVP,STATUS='OLD',FILE=FILES) 
      I=0 
   10 READ(JDVP,'(A40)',ERR=100,IOSTAT=IOS)TEXT 
      IF(TEXT.EQ.' ')GOTO 100 
      IF(INDEX(TEXT,'END').NE.0)GOTO 100 
      CALL CAPCNV(TEXT,NUMUD,50)                                        GDH1195
      IF(INDEX(TEXT,'END') .NE. 0) GOTO 100 
      DO 30 J=1,25 
      IF(J.GT.21) THEN 
         IT=INDEX(TEXT,'FN') 
         TXTNEW = TEXT(1:IT+2) 
         IF(INDEX(TXTNEW,PARTYP(J)) .NE. 0) GOTO 50 
      ENDIF 
      IF(INDEX(TEXT,PARTYP(J)) .NE. 0) GOTO 50 
  30  CONTINUE 
      WRITE(JOUT,'(''   NAME NOT FOUND'')') 
      ISTOP=1                                                           GDH1095
      IWHERE=1                                                          GDH1095
      RETURN                                                            GDH1095
   50 IPARAM=J 
      IF(IPARAM.GT.21) THEN 
         I=INDEX(TEXT,'FN') 
         KFN=READIF(TEXT,I+3) 
      ELSE 
         KFN=0 
      ENDIF 
      I=INDEX(TEXT,PARTYP(J)) 
      K=INDEX(TEXT(I:),' ')+1 
      TXTNEW=TEXT(K:) 
      TEXT=TXTNEW 
      DO 60 J=1,107 
   60 IF(INDEX(TEXT,' '//ELEMNT(J)) .NE. 0) GOTO 70 
      WRITE(JOUT,'('' ELEMENT NOT FOUND '')') 
      WRITE(JOUT,*)' FAULTY LINE: "'//TEXT//'"' 
      ISTOP=1                                                           GDH1095
      IWHERE=2                                                          GDH1095
      RETURN                                                            GDH1095
   70 IELMNT=J 
      PARAM=READIF(TEXT,INDEX(TEXT,ELEMNT(J))) 
      DO 80 I=1,LPARS 
         IF(IJPARS(1,I).EQ.KFN.AND.IJPARS(2,I).EQ.IELMNT.AND. 
     1IJPARS(3,I).EQ.IPARAM) GOTO 90 
   80 CONTINUE 
      LPARS=LPARS+1 
      I=LPARS 
   90 IJPARS(1,I)=KFN 
      IJPARS(2,I)=IELMNT 
      IJPARS(3,I)=IPARAM 
      PARSIJ(I)=PARAM 
      GOTO 10 
  100 CONTINUE 
      CLOSE(JDVP) 
      DO 130 J=1,107 
         DO 120 K=1,25 
            DO 110 I=1,LPARS 
               IPARAM=IJPARS(3,I) 
               KFN=IJPARS(1,I) 
               IELMNT=IJPARS(2,I) 
               IF(IPARAM.NE.K) GOTO 110 
               IF(IELMNT.NE.J) GOTO 110 
               PARAM=PARSIJ(I) 
               WRITE(JOUT,'(10X,A5,A1,11X,A2,F17.6)') 
     1PARTYP(IPARAM),NUMBRS(KFN), 
     2ELEMNT(IELMNT),PARAM 
               CALL UPDATE(IPARAM,IELMNT,PARAM,1,KFN) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
  110       CONTINUE 
  120    CONTINUE 
  130 CONTINUE 
      CALL MOLDAT 
      IF (ISTOP.EQ.1) RETURN                                            GDH1095
      CALL CALPAR 
      ATHEAT=0.D0 
      ETH=0.D0 
      DO 140 I=1,NUMAT 
         NI=NAT(I) 
         ATHEAT=ATHEAT+EHEAT(NI) 
  140 ETH=ETH+EISOL(NI) 
      ATHEAT=ATHEAT-ETH*23.061D0 
      RETURN 
      END 
