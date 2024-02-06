      SUBROUTINE GETGEO(IREAD,LABELS,GEO,LOPT,NA,NB,NC,AMS,NATOMS,INT) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
************************************************************************ 
* 
*   GETGEO READS IN THE GEOMETRY. THE ELEMENT IS SPECIFIED BY IT'S 
*          CHEMICAL SYMBOL, OR, OPTIONALLY, BY IT'S ATOMIC NUMBER. 
* 
*  ON INPUT   IREAD  = CHANNEL NUMBER FOR READ 
*             AMS    = DEFAULT ATOMIC MASSES. 
* 
* ON OUTPUT LABELS = ATOMIC NUMBERS OF ALL ATOMS, INCLUDING DUMMIES. 
*           GEO    = INTERNAL COORDINATES, IN ANGSTROMS, AND DEGREES. 
*           LOPT   = INTEGER ARRAY, A '1' MEANS OPTIMISE THIS PARAMETER, 
*                    '0' MEANS DO NOT OPTIMISE, AND A '-1' LABELS THE 
*                    REACTION COORDINATE. 
*           NA     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT) 
*           NB     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT) 
*           NC     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT) 
*           ATMASS = ATOMIC MASSES OF ATOMS. 
************************************************************************ 
C     COMMON /OPTIM/ IMP,IMP0           
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /ATMASS/ ATMASS(NUMATM) 
      COMMON /CMCOM/ ECMCG(NUMATM)                                      GDH1293
      COMMON /IVSZCM/ IVSZ(NUMATM), IVSZC                               GDH0494
      DIMENSION ISTART(40), XYZ(3,NUMATM) 
      DIMENSION GEO(3,*),NA(*),NB(*),NC(*),AMS(*), LOPT(3,*) 
     .         ,LABELS(*) 
      LOGICAL INT 
      LOGICAL LEADSP 
      CHARACTER ELEMNT(107)*2, LINE*80, SPACE*1, NINE*1, ZERO*1 
     1         ,COMMA*1, STRING*80, ELE*2 
       SAVE
      DATA (ELEMNT(I),I=1,107)/'H','HE', 
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
     2 'FM','MD','NO','++','+','--','-','TV'/ 
      DATA COMMA,SPACE,NINE,ZERO/',',' ','9','0'/ 
      NATOMS=0 
      NUMAT=0 
      IHEV=0
      ILIT=0
   10 READ(IREAD,'(A)',END=90,ERR=110)LINE 
      IF(LINE.EQ.' ') GO TO 90 
      IF(IHEV.GE.MAXHEV) THEN
        WRITE(JOUT,'(//9x,''***WARNING***'',/,''THE MAXIMUM NUMBER '',  GDH0594
     .       ''OF NON-HYDROGEN ATOMS HAS BEEN EXCEEDED.'',/,''TO '',    DJG0896
     .       ''INCREASE THE NUMBER OF ALLOWED HYDROGEN ATOMS,'',/,      DJG0896
     .       ''THE VALUE OF MAXHEV IN THE SIZES.i INCLUDE FILE '',/,    DJG0896
     .       ''MUST BE INCREASED AND THE CODE RECOMPILED.'',/)')        DJG0896
         ISTOP=1                                                        GDH1095
         IWHERE=74                                                      GDH1095
         RETURN                                                         GDH1095
      ELSE IF (ILIT.GT.MAXLIT) THEN                                     GDH0594
        WRITE(JOUT,'(//9x,''***WARNING***'',/,''THE MAXIMUM NUMBER '',  GDH0594
     .       ''OF HYDROGEN ATOMS HAS BEEN EXCEEDED.'',/,''TO '',        DJG0896
     .       ''INCREASE THE NUMBER OF ALLOWED HYDROGEN ATOMS,'',/,      DJG0896
     .       ''THE VALUE OF MAXLIT IN THE SIZES.i INCLUDE FILE '',/,    DJG0896
     .       ''MUST BE INCREASED AND THE CODE RECOMPILED.'',/)')        DJG0896
         ISTOP=1                                                        GDH1095
         IWHERE=75                                                      GDH1095
         RETURN                                                         GDH1095
      ENDIF                                                              GDH0594
      NATOMS=NATOMS+1 
*   CLEAN THE INPUT DATA 
      CALL CAPCNV(LINE,NUMUD,80)                                         GDH1195
      DO 30 I=1,80 
   30 IF(LINE(I:I).LT.SPACE .OR. 
     1       LINE(I:I).EQ.COMMA) LINE(I:I)=SPACE 
* 
*   INITIALIZE ISTART TO INTERPRET BLANKS AS ZERO'S 
      DO 40 I=1,10 
   40 ISTART(I)=80 
* FIND INITIAL DIGIT OF ALL NUMBERS, CHECK FOR LEADING SPACES FOLLOWED 
*     BY A CHARACTER AND STORE IN ISTART 
      LEADSP=.TRUE. 
      NVALUE=0 
      DO 50 I=1,80 
         IF (LEADSP.AND.LINE(I:I).NE.SPACE) THEN 
            NVALUE=NVALUE+1 
            ISTART(NVALUE)=I 
         END IF 
         LEADSP=(LINE(I:I).EQ.SPACE) 
   50 CONTINUE 
* 
* ESTABLISH THE ELEMENT'S NAME AND ISOTOPE, CHECK FOR ERRORS OR E.O.DATA 
* 
      WEIGHT=0.D0 
      STRING=LINE(ISTART(1):ISTART(2)-1) 
      IF( STRING(1:1) .GE. ZERO .AND. STRING(1:1) .LE. NINE) THEN 
*  ATOMIC NUMBER USED: NO ISOTOPE ALLOWED 
         LABEL=READIF(STRING,1) 
         IF (LABEL.EQ.0) GO TO 80 
         IF (LABEL.LT.0.OR.LABEL.GT.107) THEN 
            WRITE(JOUT,'(''  ILLEGAL ATOMIC NUMBER'')') 
            GO TO 120 
         END IF 
         IF (LABEL.EQ.1) THEN                                            GDH0594
             ILIT=ILIT+1                                                 GDH0594
         ELSE                                                            GDH0594
             IF (LABEL.NE.99) IHEV=IHEV+1                                GDH0594
         ENDIF                                                           GDH0594
         GO TO 70 
      END IF 
*  ATOMIC SYMBOL USED 
      REAL=READIF(STRING,1) 
      IF (REAL.LT..005) THEN 
*   NO ISOTOPE 
         ELE=STRING(1:2) 
      ELSE 
         WEIGHT=REAL 
         IF( STRING(2:2) .GE. ZERO .AND. STRING(2:2) .LE. NINE) THEN 
            ELE=STRING(1:1) 
         ELSE 
            ELE=STRING(1:2) 
         END IF 
      END IF 
*   CHECK FOR ERROR IN ATOMIC SYMBOL 
      DO 60 I=1,107 
         IF(ELE.EQ.ELEMNT(I)) THEN 
            LABEL=I 
            IF (LABEL.EQ.1) THEN                                         GDH0594
                ILIT=ILIT+1                                              GDH0594
            ELSE                                                         GDH0594
                IF (LABEL.NE.99) IHEV=IHEV+1                             GDH0594
            ENDIF                                                        GDH0594
            GO TO 70 
         END IF 
   60 CONTINUE 
      WRITE(JOUT,'(''  UNRECOGNIZED ELEMENT NAME: ('',A,'')'')')ELE 
      GOTO 120 
* 
* ALL O.K. 
* 
   70 IF (LABEL.NE.99) NUMAT=NUMAT+1 
      IF(WEIGHT.NE.0.D0)THEN 
         WRITE(JOUT,'('' FOR ATOM'',I4,''  ISOTOPIC MASS:'' 
     1    ,F12.5)')NATOMS, WEIGHT 
         ATMASS(NUMAT)=WEIGHT 
      ELSE 
         IF(LABEL .NE. 99)  ATMASS(NUMAT)=AMS(LABEL) 
      ENDIF 
      LABELS(NATOMS)   =LABEL 
      GEO(1,NATOMS)    =READIF(LINE,ISTART(2)) 
      LOPT(1,NATOMS)   =READIF(LINE,ISTART(3)) 
      GEO(2,NATOMS)    =READIF(LINE,ISTART(4)) 
      LOPT(2,NATOMS)   =READIF(LINE,ISTART(5)) 
      GEO(3,NATOMS)    =READIF(LINE,ISTART(6)) 
      LOPT(3,NATOMS)   =READIF(LINE,ISTART(7)) 
      INXT=8                                                            GDH0496
      IF (ICART.EQ.0) THEN                                              GDH0496
         NA(NATOMS)       =READIF(LINE,ISTART(8)) 
         NB(NATOMS)       =READIF(LINE,ISTART(9)) 
         NC(NATOMS)       =READIF(LINE,ISTART(10)) 
         INXT=11
      ENDIF                                                             GDH0496
C                                                                       DJG0996
C     IVSZC=1 IF USING EXTERNAL VALUES FOR M SHELLS TO EXPAND FROM ATOM DJG0996
C                                                                       DJG0996
      IVSZC=0                                                           GDH0494
      IF (IEXTM.NE.0) THEN                                              DJG0995
         IF (LABEL.NE.99) THEN                                          GDH0494
            IF (NVALUE.LT.INXT) THEN                                    GDH0496
               WRITE(JOUT,150)                                          GDH1095
150            FORMAT('THERE IS A PROBLEM READING M VALUES FROM THE ',  DJG0995
     1                'DATA SET')                                       DJG0995
               ISTOP=1                                                  GDH1095
               IWHERE=76                                                GDH1095
               RETURN                                                   GDH1095
            ELSE                                                        GDH0494
               IVSZC=1                                                  GDH0494
               IVSZ(NUMAT)    =READIF(LINE,ISTART(INXT))                GDH0496
               INXT=INXT+1                                              GDH0496
            ENDIF                                                       GDH0494
         ENDIF                                                          GDH0494
      ELSE IF (ISTDM.NE.0) THEN                                         DJG0496
         INXT=INXT+1                                                    DJG0496
      ENDIF                                                             GDH0494
      IF (IEXTCM.NE.0) THEN                                             GDH0496
         IF (LABEL.NE.99) THEN                                          GDH0496
            IF (NVALUE.LT.INXT) THEN                                    GDH0496
               WRITE(JOUT,160)                                          GDH0496
160            FORMAT('THERE IS A PROBLEM READING CHARGES ',            DJG0496
     1                'IN THE DATA SET')                                DJG0496
               ISTOP=1                                                  GDH0496
               IWHERE=78                                                GDH0496
               RETURN                                                   GDH0496
            ELSE                                                        GDH0496
               ECMCG(NUMAT)    =READIF(LINE,ISTART(INXT))               GDH0496
               INXT=INXT+1                                              GDH0496
            ENDIF                                                       GDH0494
         ENDIF                                                          GDH0494
      ENDIF                                                             GDH1293
      GOTO 10 
* 
* ALL DATA READ IN, CLEAN UP AND RETURN 
* 
   80 NATOMS=NATOMS-1 
   90 NA(2)=1 
      INT=(ICART.EQ.0) 
      IF(  .NOT. INT ) THEN 
         DO 100 I=1,NATOMS 
            DO 100 J=1,3 
  100    XYZ(J,I)=GEO(J,I) 
         DEGREE=180.D0/3.141592652589D0 
         CALL XYZINT(XYZ,NATOMS,NA,NB,NC,DEGREE,GEO) 
      ELSE 
         IF(NATOMS.GT.3.AND.INT) THEN                                   DJG0295
            DO 95 KL=4,NATOMS                                           DJG0295
               IF (NA(KL).EQ.0.OR.NB(KL).EQ.0.OR.NC(KL).EQ.0) THEN      DJG0295
                  WRITE(JOUT,'(//10X,''WHEN USING INTERNAL '',          DJG0295
     2                  ''COORDINATES, ALL ATOM CONNECTIVITIES MUST'',  DJG0295
     3                  /,10X,''SPECIFIED EXPLICITLY.'',/,10X,''ATOM #''DJG0295
     4                  ,I3,'' HAS AN ILLEGAL CONNECTIVITY OF ZERO.'')')DJG0295
     5                  KL                                              DJG0295
                  WRITE(JOUT,'(//,''NOTE: The problem may be caused'',  DJG0496
     2                 '' because a heat of formation was placed'',     GDH1195
     3                 /,''on a line after the comments as was '',      GDH1195
     4                 ''supported in prior versions.  Use'',/,         GDH1195
     5                 ''the HFOPT=x keyword instead.  See the '',      GDH0196
     6                 ''manual for details.'')')                       GDH1195
      ISTOP=1                                                           GDH1095
      IWHERE=80                                                         GDH1095
      RETURN                                                            GDH1095
               ENDIF                                                    DJG0295
 95         CONTINUE                                                    DJG0295
         ENDIF                                                          DJG0295
         IF(LOPT(1,1)+LOPT(2,1)+LOPT(3,1)+LOPT(2,2)+LOPT(3,2)+ 
     1        LOPT(3,3) .GT. 0)THEN 
            LOPT(1,1)=0 
            LOPT(2,1)=0 
            LOPT(3,1)=0 
            LOPT(2,2)=0 
            LOPT(3,2)=0 
            LOPT(3,3)=0 
            WRITE(JOUT,'(//10X,'' AN UNOPTIMIZABLE GEOMETRIC PARAMETER H 
     .AS'',/10X,'' BEEN MARKED FOR OPTIMIZATION. THIS IS A NON-FATAL '' 
     .,''ERROR'')') 
         ENDIF 
      ENDIF 
      IF(NA(3).EQ.0) THEN 
         NB(3)=1 
         NA(3)=2 
      ENDIF 
      RETURN 
* ERROR CONDITIONS 
  110 IF(IREAD.EQ.JDAT) THEN 
         WRITE(JOUT,'( '' ERROR DURING READ AT ATOM NUMBER '', I3 )') 
     .              NATOMS 
      ELSE 
         NATOMS=0 
         RETURN 
      ENDIF 
  120 J=NATOMS-1 
      WRITE(JOUT,'('' DATA CURRENTLY READ IN ARE '')') 
      DO 130 K=1,J 
  130 WRITE(JOUT,140) LABELS(K),(GEO(J,K),LOPT(J,K),J=1,3) 
     .               ,NA(K),NB(K),NC(K) 
  140 FORMAT(I4,2X,3(F10.5,2X,I2,2X),3(I2,1X)) 
      ISTOP=1                                                           GDH1095
      IWHERE=81                                                         GDH1095
      RETURN                                                            GDH1095
      END 
