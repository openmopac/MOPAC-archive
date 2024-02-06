      SUBROUTINE EXTSMX
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
       INCLUDE 'FFILES.i'                                               GDH1095
C******************************************************************************
C 
C   THIS SUBROUTINE READS IN THE EXTSM DATA.
C
C   SUBROUTINE EXTSMY RETURNS ALL THE NUMERICAL VALUES FROM THE LINE (ANUM)
C   AND ALSO THE NUMBER OF VALUES (ICNT)
C
C       SIGMA0 = SURFACE TENSIONS (SIGMA ZERO)
C       RKCDS = SURFACE TENSION RADII (Rk)
C       RHONOT = COULOMB RADII PART I "rho zero"
C       RHOONE = COULOMB RADII PART II "rho one"
C       QKNOT = PART OF COULOMB RADII CALC "q not"
C       QKONE = PART OF COULOMB RADII CALC "q one"
C       HBCORC = SURFACE TENSION CORRECTION FOR H-BONDS (SM2(.X) AND SM3(.X))
C
C   CREATED BY DJG 0995 FROM EXISTING LINES IN SMX1
C
C******************************************************************************
      COMMON /OPTIMI/ IMP,IMP0           
      COMMON /HEXSTM/ HRFN4(100),CRFNH4,CNRFN4,CORFN5,                  GDH0797
     X                CRFNH5,OORFN5,ONRFN4,SSRFN5,OPRFN5,               GDH0797
     1                FCRFN,CLCRFN,BRCRFN,CNSTC2,TNCSC3,TNCSC2,         GDH0797
     2                HOHST1,HNNST1, SPRFN5,OSIBD5                      PDW0901
      COMMON /SOLVCM/ CELEID, SLVRAD, SLVRD2, CSSIGM
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),
     1                HBCORC(100),QKNOT(100),QKONE(100)
      DIMENSION ANUM(7)
      CHARACTER ELEMNT(107)*2, LINE*80, SPACE*1, NINE*1, ZERO*1, ELE*2  DJG0995
      DATA (ELEMNT(I),I=1,107)/'H','HE',                                GDH1293
     1 'LI','BE','B','C','N','O','F','NE',                              GDH1293
     2 'NA','MG','AL','SI','P','S','CL','AR',                           GDH1293
     3 'K','CA','SC','TI','V','CR','MN','FE','CO','NI','CU',            GDH1293
     4 'ZN','GA','GE','AS','SE','BR','KR',                              GDH1293
     5 'RB','SR','Y','ZR','NB','MO','TC','RU','RH','PD','AG',           GDH1293
     6 'CD','IN','SN','SB','TE','I','XE',                               GDH1293
     7 'CS','BA','LA','CE','PR','ND','PM','SM','EU','GD','TB','DY',     GDH1293
     8 'HO','ER','TM','YB','LU','HF','TA','W','RE','OS','IR','PT',      GDH1293
     9 'AU','HG','TL','PB','BI','PO','AT','RN',                         GDH1293
     1 'FR','RA','AC','TH','PA','U','NP','PU','AM','CM','BK','CF','XX', GDH1293
     2 'FM','MD','NO','++','+','--','-','TV'/                           GDH1293
      DATA SPACE,NINE,ZERO/' ','9','0'/                                 DJG0995
      REWIND (JXSM)                                                     GDH1293
   33 READ(JXSM,'(A)',END=34) LINE                                      GDH1293
         CALL CAPCNV(LINE,NUMUD,80)                                     GDH1195
      ELE=LINE(1:2)                                                     GDH1293
      ICNT=1
      IF (ELE.EQ.'# ') THEN                                             DJG0495
C                                                                       DJG0495
C     COMMENT LINE DO NOTHING                                           DJG0495
C                                                                       DJG0495
         CONTINUE                                                       DJG0495
      ELSE IF (ELE.EQ.'CS') THEN                                        DJG1094
         CALL EXTSMY(ANUM,LINE,ICNT)                                    DJG0995
         IF (ICNT.NE.1) THEN                                            DJG0995
            WRITE(JOUT,1000) 'CS'                                       DJG0995
            ISTOP=1                                                     GDH1095
            IWHERE=40                                                   GDH1095
            RETURN                                                      GDH1095
         ENDIF                                                          DJG0995
         CSSIGM=ANUM(1)                                                 DJG1094
C  See if there are O-C bond surface tensions... read them in
      ELSE IF (ELE.EQ.'OC') THEN                                        CCC0395
         CALL EXTSMY(ANUM,LINE,ICNT)                                    DJG0995
         IF (ICNT.NE.1) THEN                                            DJG0995
            WRITE(JOUT,1000) 'OC'                                       DJG0995
            ISTOP=1                                                     GDH1095
            IWHERE=41                                                   GDH1095
            RETURN                                                      GDH1095
         ENDIF                                                          CCC0495
         CORFN5=ANUM(1)                                                 CCC0495
         WRITE(JOUT,*)'OC ',CORFN5                                      CCCDEVL
C  See if there are C-C bond surface tensions... read them in           CCC0495
      ELSE IF (ELE.EQ.'CC') THEN                                        DJG1094
         CALL EXTSMY(ANUM,LINE,ICNT)                                    DJG0995
         IF(IRAD.EQ.6)THEN                                              CCC0495
            IF (ICNT.NE.1) THEN                                         DJG0995
               WRITE(JOUT,1000) 'CC'                                    DJG0995
            ISTOP=1                                                     GDH1095
            IWHERE=42                                                   GDH1095
            RETURN                                                      GDH1095
            ENDIF                                                       DJG0995
            CRFNH4=ANUM(1)                                              DJG1094
         ELSEIF(IRAD.EQ.4.OR.IRAD.EQ.5.OR.IRAD.EQ.7.OR.IRAD.EQ.8.       GDH0997
     .          OR.IRAD.EQ.9.OR.IRAD.EQ.10)THEN                         GDH0997
            IF (ICNT.NE.2) THEN                                         CCC0495
               WRITE(JOUT,1000) 'CC'                                    DJG0995
            ISTOP=1                                                     GDH1095
            IWHERE=43                                                   GDH1095
            RETURN                                                      GDH1095
            ENDIF                                                       CCC0495
            CRFNH4=ANUM(1)                                              CCC0495
            CRFNH5=ANUM(2)                                              CCC0495
         ENDIF                                                          CCC0495
         WRITE(JOUT,*)'CC ',CRFNH4,CRFNH5                               CCCDEVL
C  See there are N-C bond surface tensions... read them in              CCC0495
      ELSE IF (ELE.EQ.'NC') THEN                                        CCC0495
         CALL EXTSMY(ANUM,LINE,ICNT)                                    DJG0995
         IF (ICNT.NE.1) THEN                                            CCC0695
            WRITE(JOUT,1000) 'NC'                                       DJG0995
            ISTOP=1                                                     GDH1095
            IWHERE=47                                                   GDH1095
            RETURN                                                      GDH1095
         ENDIF                                                          CCC0695
         CNRFN4=ANUM(1)                                                 CCC0495
         WRITE(JOUT,*)'NC ',CNRFN4                                      CCCDEVL
C  See if there are O-N bond surface tensions... read them in           CCC0495
      ELSE IF (ELE.EQ.'ON') THEN                                        CCC0495
         CALL EXTSMY(ANUM,LINE,ICNT)                                    DJG0995
         IF (ICNT.NE.1) THEN                                            DJG0995
            WRITE(JOUT,1000) 'ON'                                       DJG0995
            ISTOP=1                                                     GDH1095
            IWHERE=44                                                   GDH1095
            RETURN                                                      GDH1095
         ENDIF                                                          DJG0995
         ONRFN4=ANUM(1)                                                 CCC0495
         WRITE(JOUT,*)'ON ',ONRFN4                                      CCCDEVL
C  See if there are O-O bond surface tensions... read them in           CCC0495
      ELSE IF (ELE.EQ.'OO') THEN                                        CCC0495
         CALL EXTSMY(ANUM,LINE,ICNT)                                    DJG0995
         IF (ICNT.NE.1) THEN                                            DJG0995
            WRITE(JOUT,1000) 'OO'                                       DJG0995
            ISTOP=1                                                     GDH1095
            IWHERE=45                                                   GDH1095
            RETURN                                                      GDH1095
         ENDIF                                                          DJG0995
         OORFN5=ANUM(1)                                                 CCC0495
         WRITE(JOUT,*)'OO ',OORFN5                                      CCCDEVL
C  See if there are S-S bond surface tensions... read them in           CCC0695
      ELSE IF (ELE.EQ.'SS') THEN                                        CCC0695
         CALL EXTSMY(ANUM,LINE,ICNT)                                    DJG0995
         IF (ICNT.NE.1) THEN                                            DJG0995
            WRITE(JOUT,1000) 'SS'                                       DJG0995
            ISTOP=1                                                     GDH1095
            IWHERE=46                                                   GDH1095
            RETURN                                                      GDH1095
         ENDIF                                                          DJG0995
         SSRFN5=ANUM(1)                                                 CCC0695
         WRITE(JOUT,*)'SS ',SSRFN5                                      CCCDEVL
      ELSE                                                              DJG1094
         IF (ELE(1:1).EQ.SPACE) THEN                                    GDH1293
            WRITE(JOUT,387)                                             DJG0295
387         FORMAT('THE FIRST CHARACTER ON A LINE IN THE EXTSM',        DJG0295
     1             ' FILE MUST NOT BE A SPACE')                         DJG0295
            ISTOP=1                                                     GDH1095
            IWHERE=49                                                   GDH1095
            RETURN                                                      GDH1095
         ENDIF                                                          GDH1293
         CALL EXTSMY(ANUM,LINE,ICNT)                                    DJG0995
         IF (ICNT.NE.7) THEN                                            DJG0995
            WRITE(JOUT,1000) LINE(1:2)                                  DJG0995
            ISTOP=1                                                     GDH1095
            IWHERE=49                                                   GDH1095
            RETURN                                                      GDH1095
         ENDIF                                                          DJG0995
         ILAB=-1                                                        DJG1094
         DO 32 I=1,107                                                  GDH1293
   32    IF(ELE.EQ.ELEMNT(I)) ILAB=I                                    GDH1293
         IF (ILAB.GT.0) THEN                                            DJG1094
            RKCDS(ILAB)=ANUM(1)                                         DJG0995
            SIGMA0(ILAB)=ANUM(2)                                        DJG0995
            IF (IRAD.EQ.4.OR.IRAD.EQ.5.OR.IRAD.EQ.6.OR.IRAD.EQ.7.OR.    DJG0896
     1          IRAD.EQ.8.OR.IRAD.EQ.9.OR.IRAD.EQ.10) THEN              GDH0897
               HRFN4(ILAB)=ANUM(3)                                      DJG1094
            ELSE IF (IRAD.EQ.2.OR.IRAD.EQ.3.OR.IRAD.EQ.12.OR.IRAD.EQ.13 DJG0995
     1               .OR.IRAD.EQ.22.OR.IRAD.EQ.23) THEN                 DJG0995
               HBCORC(ILAB)=ANUM(3)                                     DJG0995
            ENDIF                                                       DJG1094
            HBCORC(ILAB)=ANUM(3)                                        DJG0995
            RHONOT(ILAB)=ANUM(4)                                        DJG0995
            RHOONE(ILAB)=ANUM(5)                                        DJG0995
            QKNOT(ILAB)=ANUM(6)                                         DJG0995
            QKONE(ILAB)=ANUM(7)                                         DJG0995
         ELSE                                                           DJG1094
            PRINT *, 'UNRECOGNIZED ELEMENT', ELE                        DJG1094
            ISTOP=1                                                     GDH1095
            IWHERE=50                                                   GDH1095
            RETURN                                                      GDH1095
         ENDIF                                                          DJG1094
      ENDIF                                                             DJG1094
      ICNT=1                                                            GDH1293
      GOTO 33                                                           GDH1293
   34 CONTINUE                                                          GDH1293
1000  FORMAT(/,'THERE IS A PROBLEM WITH THE ',A2,' LINE IN THE',        DJG0995
     1       ' EXTSM FILE')                                             DJG0995
      RETURN
      END
