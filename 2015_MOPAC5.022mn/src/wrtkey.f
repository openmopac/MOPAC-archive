      SUBROUTINE WRTKEY(KEYWRD,*)                                       CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
      COMMON /TIMDMP/ TLEFT, TDUMP
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      COMMON /HYFACT/ HFACT, USEHFC                                     LF1111
      LOGICAL USEHFC                                                    LF1111
C
C
      CHARACTER*160 KEYWRD, ALLKEY
***********************************************************************
*
*  WRTKEY CHECKS ALL KEY-WORDS AND PRINTS THOSE IT RECOGNIZES.  IF IT
*  FINDS A WORD IT DOES NOT RECOGNIZE THE PROGRAM WILL BE STOPPED.
*
***********************************************************************
      LOGICAL UHF, TRIP, BIRAD, EXCI, CI, WORD
      LOGICAL AM1, MNDO, MINDO3, PM3, PDG, MDG, RM1, 
     &        PM3D, AM1D, PM6, MNDOD, RM1D, PM6D,                       LF0310
     &        MNDOD3, AM1D3, PM3D3, RM1D3, PM6D3                        LF1111/LF1211
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1010
      CHARACTER SPACE*1, DOT*1, ZERO*1, NINE*1, CH*1, CHRONO*7
         SAVE                                                           GL0892
      DATA SPACE,DOT,ZERO,NINE /' ','.','0','9'/
      DATA AM1, MNDO, MINDO3, PM3, PDG, MDG, RM1 /7*.FALSE./
      DATA AM1D, PM3D, PM6, MNDOD, RM1D, PM6D /6*.FALSE./               LF0709/LF0310
      DATA MNDOD3, AM1D3, PM3D3, RM1D3, PM6D3 / 5* .FALSE. /            LF1111/LF1211
C                                                                       
C PARAMETER FILE FOR HH GAUSSIAN REPLUSION AND COMMON BLOCK TO HOLD     JP1203
C THE GAUSSIAN PARAMETERS                                                 ..
C                                                                         ..
      CHARACTER FHHGAU*64                                                 ..
      COMMON /HHGAU/ HHA0(3),HHR0(3),HHD0(3)                            JP1203
C
      PMODS=.FALSE.                                                     LF0910
      ALLKEY=KEYWRD
      IF(DOPRNT.AND.WORD(ALLKEY,'AUTHOR')) THEN                         CIO
         WRITE(6,'(10X,'' MOPAC - A GENERAL MOLECULAR ORBITAL PACKAGE'',
     1/         ,10X,''   ORIGINAL VERSION WRITTEN IN 1983'')')
         WRITE(6,'(10X,''     BY JAMES J. P. STEWART AT THE'',/
     1         ,10X,''     UNIVERSITY OF TEXAS AT AUSTIN'',/
     2         ,10X,''          AUSTIN, TEXAS, 78712'')')
      ENDIF
      IF (WORD(ALLKEY,'VECT').AND.DOPRNT )                              CIO 
     &    WRITE(6,70)
      IF (WORD(ALLKEY,' EXTE') ) THEN
         I=INDEX(KEYWRD,' EXTE')
         J=INDEX(KEYWRD(I:),'=')+I
         I=INDEX(KEYWRD(J:),' ')+J-1
         IF (DOPRNT) WRITE(6,80)KEYWRD(J:I)                             CIO
      ENDIF
      MAXGEO=0
      IF (DOPRNT.AND.WORD(ALLKEY,' DENS') )                             CIO
     & WRITE(6,90)
      IF (DOPRNT.AND.WORD(ALLKEY,'SPIN') )                              CIO
     & WRITE(6,100)
      IF (DOPRNT.AND.WORD(ALLKEY,' DEPVAR') )                           CIO
     & WRITE(6,110)READA(KEYWRD,INDEX(KEYWRD,'DEPVAR'))
      IF (DOPRNT.AND.WORD(ALLKEY,' DEP ') )                             CIO
     & WRITE(6,120)
      IF (DOPRNT.AND.WORD(ALLKEY,'VELO') )                              CIO
     & WRITE(6,130)
      IF (DOPRNT.AND.WORD(ALLKEY,'TIMES') )                             CIO
     & WRITE(6,140)
      IF (DOPRNT.AND.WORD(ALLKEY,'PARASOK') )                           CIO
     & WRITE(6,150)
      IF (DOPRNT.AND.WORD(ALLKEY,'FLEPO') )                             CIO
     & WRITE(6,160)
      IF (DOPRNT.AND.WORD(ALLKEY,'BONDS') )                             CIO
     & WRITE(6,170)
      IF (DOPRNT.AND.WORD(ALLKEY,'GEO-OK') )                            CIO
     & WRITE(6,180)
      IF (DOPRNT.AND.WORD(ALLKEY,'FOCK') )                              CIO
     & WRITE(6,190)
      IF (DOPRNT.AND.WORD(ALLKEY,'LARGE') )                             CIO
     & WRITE(6,200)
      IF (DOPRNT.AND.WORD(ALLKEY,' S1978') )                            CIO
     & WRITE(6,210)
      IF (DOPRNT.AND.WORD(ALLKEY,' SI1978') )                           CIO
     & WRITE(6,220)
      IF (DOPRNT.AND.WORD(ALLKEY,' GRAP') )                             CIO
     & WRITE(6,230)
      IF (DOPRNT.AND.WORD(ALLKEY,'1ELEC') )                             CIO
     & WRITE(6,240)
      IF (DOPRNT.AND.WORD(ALLKEY,' NOMM') )                             CIO
     & WRITE(6,260)
      IF (DOPRNT.AND.WORD(ALLKEY,' MMOK') )                             CIO
     & WRITE(6,265)
      IF (DOPRNT.AND.WORD(ALLKEY,'INTERP') )                            CIO
     & WRITE(6,270)
      IF (DOPRNT.AND.WORD(ALLKEY,' ESR') )                              CIO
     & WRITE(6,250)
      IF (DOPRNT.AND.WORD(ALLKEY,'DFP') )                               CIO
     & WRITE(6,280)
      IF (DOPRNT.AND.WORD(ALLKEY,'ANALYT') )                            CIO
     & WRITE(6,290)
      IF (DOPRNT.AND.WORD(ALLKEY,' MECI') )                             CIO
     & WRITE(6,300)
      IF (DOPRNT.AND.WORD(ALLKEY,'LOCAL') )                             CIO
     & WRITE(6,340)
      IF (DOPRNT.AND.WORD(ALLKEY,'MULLIK') )                            CIO
     & WRITE(6,350)
      IF (DOPRNT.AND.WORD(ALLKEY,' XYZ') )                              CIO
     & WRITE(6,360)
      IF (DOPRNT.AND.WORD(ALLKEY,' PI') )                               CIO
     & WRITE(6,370)
      IF (DOPRNT.AND.WORD(ALLKEY,'ECHO') )                              CIO
     & WRITE(6,380)
      IF (DOPRNT.AND.WORD(ALLKEY, 'SING') )                             CIO
     & WRITE(6,540)
      IF (DOPRNT.AND.WORD(ALLKEY, 'DOUB') )                             CIO
     & WRITE(6,550)
      IF (DOPRNT.AND.WORD(ALLKEY, 'QUAR') )                             CIO
     & WRITE(6,570)
      IF (DOPRNT.AND.WORD(ALLKEY, 'QUIN') )                             CIO
     & WRITE(6,580)
      IF (DOPRNT.AND.WORD(ALLKEY, 'SEXT') )                             CIO
     & WRITE(6,590)
      IF (DOPRNT.AND.WORD(ALLKEY,'H-PRIO') )                            CIO
     & WRITE(6,390)
      IF (DOPRNT.AND.WORD(ALLKEY,'X-PRIO') )                            CIO
     & WRITE(6,400)
      IF (DOPRNT.AND.WORD(ALLKEY,'T-PRIO') )                            CIO
     & WRITE(6,410)
      IF (DOPRNT.AND.WORD(ALLKEY,'POWSQ') )                             CIO
     & WRITE(6,420)
      IF (DOPRNT.AND.WORD(ALLKEY,'POLAR') )                             CIO
     & WRITE(6,430)
      IF (DOPRNT.AND.WORD(ALLKEY,'DEBUG ') )                            CIO
     & WRITE(6,440)
      IF (DOPRNT.AND.WORD(ALLKEY,'RESTART') )                           CIO
     & WRITE(6,450)
      IF (WORD(ALLKEY,'IRC=') ) THEN
         MAXGEO=1
         IF(DOPRNT) WRITE(6,460)NINT(READA(KEYWRD,INDEX(KEYWRD,'IRC='))) CIO
      ELSEIF (WORD(ALLKEY,'IRC') ) THEN
         MAXGEO=1
         IF (DOPRNT) WRITE(6,470)                                       CIO
      ENDIF
      IF (DOPRNT.AND.WORD(ALLKEY,'CHARGE') )                            CIO
     1 WRITE(6,480)NINT(READA(KEYWRD,INDEX(KEYWRD,'CHARGE')))

      IF (WORD(ALLKEY,'HYBRID=').AND.DOPRNT ) THEN                      LF1111
         USEHFC=.TRUE.                                                  LF1111
         HFACT=READA(KEYWRD,INDEX(KEYWRD,'HYBRID'))                     LF1111
         WRITE(6,1510) HFACT                                            LF1111
      ENDIF                                                             LF1111


      IF (DOPRNT.AND.WORD(ALLKEY,'GRAD') )                              CIO
     & WRITE(6,490)
      UHF=(WORD(ALLKEY,'UHF') )
      IF(DOPRNT.AND.UHF)WRITE(6,500)                                    CIO
      BIRAD=(WORD(ALLKEY,'BIRAD') )
      IF(DOPRNT.AND.BIRAD)WRITE(6,520)                                  CIO
      EXCI=(WORD(ALLKEY,'EXCITED') )
      IF(DOPRNT.AND.EXCI) WRITE(6,530)                                  CIO
      TRIP=(WORD(ALLKEY,'TRIPLET') )
      IF(DOPRNT.AND.TRIP)WRITE(6,560)                                   CIO
      IF (DOPRNT.AND.WORD(ALLKEY,'SYM') )                               CIO
     & WRITE(6,600)
      IF(WORD(ALLKEY,'OPEN('))THEN
         I=INDEX(KEYWRD,'OPEN(')
         IELEC=READA(KEYWRD,I)
         ILEVEL=READA(KEYWRD,I+7)
         IF (DOPRNT) WRITE(6,620)IELEC,ILEVEL                           CIO
      ENDIF
      IF(DOPRNT.AND.WORD(ALLKEY,'MICROS'))                              CIO
     1WRITE(6,610)INT(READA(KEYWRD,INDEX(KEYWRD,'MICROS')))
      IF(WORD(ALLKEY,'DRC='))THEN
         MAXGEO=1
         IF (DOPRNT) WRITE(6,320)READA(KEYWRD,INDEX(KEYWRD,'DRC='))     CIO
      ELSEIF (WORD(ALLKEY,' DRC') ) THEN
         MAXGEO=1
         IF (DOPRNT) WRITE(6,310)                                       CIO
      ENDIF
      IF(DOPRNT.AND.WORD(ALLKEY,'KINE'))                                CIO
     1WRITE(6,330)READA(KEYWRD,INDEX(KEYWRD,'KINE'))
      CHRONO='SECONDS'
      IF(WORD(ALLKEY,' T=')) THEN
         I=INDEX(KEYWRD,' T=')
         TIME=READA(KEYWRD,I)
         DO 10 J=I+3,160
            IF( KEYWRD(J+1:J+1).EQ.' ') THEN
               CH=KEYWRD(J:J)
               IF( CH .EQ. 'M') CHRONO='MINUTES'
               IF( CH .EQ. 'H') CHRONO='HOURS'
               IF( CH .EQ. 'D') CHRONO='DAYS'
               GOTO 20
            ENDIF
   10    CONTINUE
   20    CONTINUE
         IF(TIME.LT.99999.9D0)THEN
            IF (DOPRNT) WRITE(6,630)TIME,CHRONO                         CIO
         ELSE
            IF (DOPRNT) WRITE(6,640)TIME,CHRONO                         CIO
         ENDIF
      ELSE
         IF (DOPRNT) WRITE(6,630)MAXTIM*1.D0,CHRONO                     CIO
      ENDIF
               CHRONO='SECONDS'
      IF(WORD(ALLKEY,' DUMP')) THEN
         TDUMP=READA(KEYWRD,INDEX(KEYWRD,' DUMP'))
         DO 30 J=I+6,160
            IF( KEYWRD(J+1:J+1).EQ.' ') THEN
               CH=KEYWRD(J:J)
               IF( CH .EQ. 'M') CHRONO='MINUTES'
               IF( CH .EQ. 'H') CHRONO='HOURS'
               IF( CH .EQ. 'D') CHRONO='DAYS'
               GOTO 40
            ENDIF
   30    CONTINUE
   40    CONTINUE
         IF(TDUMP.LT.99999.9D0)THEN
            IF (DOPRNT) WRITE(6,650)TDUMP,CHRONO                        CIO
         ELSE
            IF (DOPRNT) WRITE(6,660)TDUMP,CHRONO                        CIO
         ENDIF
      ELSE
         TDUMP=MAXDMP                                                   PF1099
C        WRITE(6,650)MAXDMP*1.D0,CHRONO                                 PF1099
         IF (DOPRNT) WRITE(6,650)TDUMP*1.0D0,CHRONO                     PF1099
      ENDIF
      IF (WORD(ALLKEY,'1SCF') ) THEN
         IF (DOPRNT) WRITE(6,670)                                       CIO
         IF(INDEX(KEYWRD,'RESTART').EQ.0)MAXGEO=MAXGEO+1
      ENDIF
      CI=WORD(ALLKEY,'C.I.')
C C.I.=(n,m) keyword                                                    IR0494
      IF (CI) THEN
         J=INDEX(KEYWRD,' C.I.=(')
         IF(J.NE.0)THEN
            IF (DOPRNT) WRITE(6,681)INT(READA(KEYWRD,INDEX(KEYWRD,      CIO
     &         'C.I.=(')+7)),                                           CIO
     &         INT(READA(KEYWRD,INDEX(KEYWRD,'C.I.=(')+5))              CIO
         ELSE
            IF (DOPRNT) WRITE(6,680)INT(READA(KEYWRD,                   CIO
     &                                 INDEX(KEYWRD,'C.I.')+5))         CIO
         ENDIF
      ENDIF
C End of C.I.=(n,m)
      IF (WORD(ALLKEY,'DFORCE') ) THEN
         IF (DOPRNT) WRITE(6,690)                                       CIO
c        MAXGEO=MAXGEO+1
      ELSEIF (WORD(ALLKEY,'FORCE') ) THEN
         IF (DOPRNT) WRITE(6,700)                                       CIO
c        MAXGEO=MAXGEO+1
      ENDIF
      METHOD=0
      IF (WORD(ALLKEY,'MINDO') ) THEN
         IF (DOPRNT) WRITE(6,710)                                       CIO
         MINDO3=.TRUE.
         METHOD=1
      ENDIF
      IF (WORD(ALLKEY,'AM1-D3') ) THEN                                  LF1111
         IF (DOPRNT) WRITE(6,726)                                       LF1111
         AM1D3=.TRUE.                                                   LF1111
         METHOD=METHOD+1                                                LF1111
      ELSEIF (WORD(ALLKEY,'AM1-D') ) THEN                               LF0509 / LF1111
         IF (DOPRNT) WRITE(6,721)                                       LF0509 / CIO
         AM1D=.TRUE.                                                    LF0509
         METHOD=METHOD+1                                                LF0509
      ELSEIF (WORD(ALLKEY,'AM1') ) THEN                                 LF0509
         IF (DOPRNT) WRITE(6,720)                                       CIO
         AM1=.TRUE.
         METHOD=METHOD+1
      ENDIF
      IF (WORD(ALLKEY,'RM1-D3') ) THEN                                  LF1211
         IF (DOPRNT) WRITE(6,728)                                       LF1211
         RM1D3=.TRUE.                                                   LF1211
         METHOD=METHOD+1                                                LF1211
      ELSEIF (WORD(ALLKEY,'RM1-D') ) THEN                               LF0310
         IF (DOPRNT) WRITE(6,723)                                       LF0310 / CIO
         RM1D=.TRUE.                                                    LF0310
         METHOD=METHOD+1                                                LF0310
      ELSEIF (WORD(ALLKEY,'RM1') ) THEN
         IF (DOPRNT) WRITE(6,736)                                       CIO
         RM1=.TRUE.
         METHOD=METHOD+1
      ENDIF
      IF (WORD(ALLKEY,'PM3-D3') ) THEN                                  LF1211
         IF (DOPRNT) WRITE(6,727)                                       LF1211
         PM3D3=.TRUE.                                                   LF1211
         METHOD=METHOD+1                                                LF1211
      ELSEIF (WORD(ALLKEY,'PM3-D') ) THEN                               LF0309
         IF (DOPRNT) WRITE(6,731)                                       LF0309 / CIO
         PM3D=.TRUE.                                                    LF0309
         METHOD=METHOD+1                                                LF0309
      ELSEIF (WORD(ALLKEY,'PM3') ) THEN                                 LF0309
         IF (DOPRNT) WRITE(6,730)                                       CIO
         PM3=.TRUE.
         METHOD=METHOD+1
      ENDIF
      IF (WORD(ALLKEY,'PM6-D3') ) THEN                                  LF1211
         IF (DOPRNT) WRITE(6,729)                                       LF1211
         PM6D3=.TRUE.                                                   LF1211
         METHOD=METHOD+1                                                LF1211
      ELSEIF (WORD(ALLKEY,'PM6G09-D') ) THEN                            LF0310
         IF (DOPRNT) WRITE(6,724)                                       LF0310 / CIO
         IF (DOPRNT) WRITE(6,1410)                                      LF0310 / CIO
         PM6D=.TRUE.                                                    LF0310
         METHOD=METHOD+1                                                LF0310
      ELSEIF (WORD(ALLKEY,'PM6-D') ) THEN                               LF0210
         IF (DOPRNT) WRITE(6,724)                                       LF0310 / CIO
         PM6D=.TRUE.                                                    LF0310
         METHOD=METHOD+1                                                LF0310
      ELSEIF (WORD(ALLKEY,'PM6G09') ) THEN                              LF0709
         IF (DOPRNT) WRITE(6,733)                                       LF0310 / CIO
         IF (DOPRNT) WRITE(6,1410)                                      LF0709 / CIO
         PM6=.TRUE.                                                     LF0709
         METHOD=METHOD+1                                                LF0709
      ELSEIF (WORD(ALLKEY,'PM6') ) THEN                                 LF0709
         IF (DOPRNT) WRITE(6,733)                                       LF0709 / CIO
         PM6=.TRUE.                                                     LF0709
         METHOD=METHOD+1                                                LF0709
      ENDIF                                                             LF0210
      IF (WORD(ALLKEY,'PMOV1')) THEN                                    LF1010
         IF (DOPRNT) WRITE(6,1506)                                      LF1010
         PMODS=.FALSE.                                                  LF1010
         PMODS(1)=.TRUE.                                                LF1010
         PMODS(2)=.TRUE.                                                LF1010
         PMODS(3)=.TRUE.                                                LF1010
         PMODS(4)=.TRUE.                                                LF1010
         METHOD=METHOD+1                                                LF1010
      ENDIF                                                             LF1010
      IF (WORD(ALLKEY,'PMO2A')) THEN                                    LF0614
         IF (DOPRNT) WRITE(6,1512)                                      LF0614
         PMODS=.FALSE.                                                  LF0614
         PMODS(4)=.TRUE.                                                LF0614
         PMODS(5)=.TRUE.                                                LF0614
         PMODS(6)=.TRUE.                                                LF0614
         PMODS(7)=.TRUE.                                                LF0614
         METHOD=METHOD+1                                                LF0614
      ENDIF                                                             LF0614
      IF (WORD(ALLKEY,'PMO2')) THEN                                     LF0113
         IF (DOPRNT) WRITE(6,1511)                                      LF0113
         PMODS=.FALSE.                                                  LF0113
         PMODS(4)=.TRUE.                                                LF0113
         PMODS(5)=.TRUE.                                                LF0113
         PMODS(6)=.TRUE.                                                LF0113
         PMODS(7)=.TRUE.                                                LF0113
         METHOD=METHOD+1                                                LF0113
      ENDIF                                                             LF0113
      IF (WORD(ALLKEY,'PDG') ) THEN
         IF (DOPRNT) WRITE(6,732)                                       CIO
         PDG=.TRUE.
         METHOD=METHOD+1
      ENDIF
      IF (WORD(ALLKEY,'MDG') ) THEN
         IF (DOPRNT) WRITE(6,734)                                       CIO
         MDG=.TRUE.
         METHOD=METHOD+1
      ENDIF
      IF (WORD(ALLKEY,'MNDO-D3') ) THEN                                 LF1111
         IF (DOPRNT) WRITE(6,725)                                       LF1111
         MNDOD3=.TRUE.                                                  LF1111
         METHOD=METHOD+1                                                LF1111
      ELSEIF (WORD(ALLKEY,'MNDO-D') ) THEN                              LF0310/LF1111
         IF (DOPRNT) WRITE(6,722)                                       LF0310 / CIO
         MNDOD=.TRUE.                                                   LF0310
         METHOD=METHOD+1                                                LF0310
      ELSEIF (WORD(ALLKEY,'MNDO')) THEN
         MNDO=.TRUE.
         METHOD=METHOD+1
      ENDIF
      IF (WORD(ALLKEY,'DIPG09') ) THEN                                  LF0210
         IF (DOPRNT) WRITE(6,1420)                                      LF0210 / CIO
      ENDIF                                                             LF0210
      IF (WORD(ALLKEY,'SDAMP') ) THEN
         IF (AM1D.OR.PM3D.OR.MNDOD.OR.PM6D.OR.RM1D) THEN
            IF (DOPRNT) WRITE(6,1430)                                   CIO
         ELSE
            IF (DOPRNT) WRITE(6,1431)                                   CIO
            RETURN 1                                                    CSTP (stop)
         ENDIF
      ENDIF
      IF (WORD(ALLKEY,'HGDAMP') ) THEN                                  LF0312
         IF (AM1D.OR.PM3D.OR.MNDOD.OR.PM6D.OR.RM1D) THEN                LF0312
            IF (DOPRNT) WRITE(6,1432)                                   LF0312
         ELSE                                                           LF0312
            IF (DOPRNT) WRITE(6,1431)                                   LF0312
            RETURN 1                                                    LF0312
         ENDIF                                                          LF0312
      ENDIF                                                             LF0312
      IF (WORD(ALLKEY,'TDAMP') ) THEN                                   LF0312
         IF (AM1D.OR.PM3D.OR.MNDOD.OR.PM6D.OR.RM1D) THEN                LF0312
            IF (DOPRNT) WRITE(6,1433)                                   LF0312
         ELSE                                                           LF0312
            IF (DOPRNT) WRITE(6,1431)                                   LF0312
            RETURN 1                                                    LF0312
         ENDIF                                                          LF0312
      ENDIF                                                             LF0312
      IF (WORD(ALLKEY,'HGDISP') ) THEN                                  LF0312
         IF (AM1D.OR.PM3D.OR.MNDOD.OR.PM6D.OR.RM1D) THEN                LF0312
            IF (DOPRNT) WRITE(6,1434)                                   LF0312
         ELSE                                                           LF0312
            IF (DOPRNT) WRITE(6,1431)                                   LF0312
            RETURN 1                                                    LF0312
         ENDIF                                                          LF0312
      ENDIF                                                             LF0312
      IF (WORD(ALLKEY,'TDISP') ) THEN                                   LF0312
         IF (AM1D.OR.PM3D.OR.MNDOD.OR.PM6D.OR.RM1D) THEN                LF0312
            IF (DOPRNT) WRITE(6,1435)                                   LF0312
         ELSE                                                           LF0312
            IF (DOPRNT) WRITE(6,1431)                                   LF0312
            RETURN 1                                                    LF0312
         ENDIF                                                          LF0312
      ENDIF                                                             LF0312
      IF (WORD(ALLKEY,'DPAIR') ) THEN                                   LF0312
         IF (AM1D.OR.PM3D.OR.MNDOD.OR.PM6D.OR.RM1D) THEN                LF0312
            IF (DOPRNT) WRITE(6,1436)                                   LF0312
         ELSE                                                           LF0312
            IF (DOPRNT) WRITE(6,1431)                                   LF0312
            RETURN 1                                                    LF0312
         ENDIF                                                          LF0312
      ENDIF                                                             LF0312
      IF (WORD(ALLKEY,'CPAIR') ) THEN                                   LF0312
         IF (AM1D.OR.PM3D.OR.MNDOD.OR.PM6D.OR.RM1D) THEN                LF0312
            IF (DOPRNT) WRITE(6,1437)                                   LF0312
         ELSE                                                           LF0312
            IF (DOPRNT) WRITE(6,1431)                                   LF0312
            RETURN 1                                                    LF0312
         ENDIF                                                          LF0312
      ENDIF                                                             LF0312
      IF (WORD(ALLKEY,'DAMP5') ) THEN                                   LF0312
         IF (AM1D.OR.PM3D.OR.MNDOD.OR.PM6D.OR.RM1D) THEN                LF0312
            IF (DOPRNT) WRITE(6,1438)                                   LF0312
         ELSE                                                           LF0312
            IF (DOPRNT) WRITE(6,1431)                                   LF0312
            RETURN 1                                                    LF0312
         ENDIF                                                          LF0312
      ENDIF                                                             LF0312
      IF (WORD(ALLKEY,'MOD1') ) THEN                                    LF1010
         IF (DOPRNT) WRITE(6,1501)                                      LF1010
         PMODS(1)=.TRUE.                                                LF1010
      ENDIF                                                             LF1010
      IF (WORD(ALLKEY,'MOD2') ) THEN                                    LF1010
         IF (DOPRNT) WRITE(6,1502)                                      LF1010
         PMODS(2)=.TRUE.                                                LF1010
      ENDIF                                                             LF1010
      IF (WORD(ALLKEY,'MOD3') ) THEN                                    LF1010
         IF (DOPRNT) WRITE(6,1503)                                      LF1010
         PMODS(3)=.TRUE.                                                LF1010
      ENDIF                                                             LF1010
      IF (WORD(ALLKEY,'MOD4') ) THEN                                    LF1010
         IF (DOPRNT) WRITE(6,1504)                                      LF1010
         PMODS(4)=.TRUE.                                                LF1010
      ENDIF                                                             LF1010
      IF (WORD(ALLKEY,'MOD5') ) THEN                                    LF1010
         IF (DOPRNT) WRITE(6,1505)                                      LF1010
         IF (PMODS(3)) THEN
            WRITE(6,*) "Cannot use both MOD5 and MOD3 keywords."
            STOP
         ENDIF
         PMODS(5)=.TRUE.                                                LF1010
      ENDIF                                                             LF1010
      IF (WORD(ALLKEY,'MOD6') ) THEN                                    LF0111
         IF (DOPRNT) WRITE(6,1507)                                      LF0111
         PMODS(6)=.TRUE.                                                LF0111
      ENDIF                                                             LF0111
      IF (WORD(ALLKEY,'MOD7') ) THEN                                    LF0211
         IF (DOPRNT) WRITE(6,1508)                                      LF0211
         PMODS(7)=.TRUE.                                                LF0211
      ENDIF                                                             LF0211
      IF (WORD(ALLKEY,'CM2') ) THEN                                     0909YC99
        IF (AM1) THEN                                                   0909YC99
          IF (DOPRNT) WRITE(6,738)                                      0909YC99 / CIO
        ELSEIF (PM3) THEN                                               0909YC99
          IF (DOPRNT) WRITE(6,739)                                      0909YC99 / CIO
        ELSE                                                            0909YC99
          IF (DOPRNT) WRITE (6,*) 'CM2 ONLY WORKS WITH AM1 and PM3'     0909YC99 / CIO
      RETURN 1                                                          CSTP (stop)

        ENDIF                                                           0909YC99
      ENDIF                                                             0909YC99
      IF (WORD(ALLKEY,'PREC').AND.DOPRNT ) WRITE(6,740)                 CIO
      IF (WORD(ALLKEY,'NOINTER').AND.DOPRNT ) WRITE(6,750)              CIO
      IF (WORD(ALLKEY,'ISOTOPE').AND.DOPRNT ) WRITE(6,760)              CIO
      IF (WORD(ALLKEY,'DENOUT').AND.DOPRNT ) WRITE(6,770)               CIO
      IF (WORD(ALLKEY,'SHIFT')) WRITE(6,780)
     & READA(KEYWRD,INDEX(KEYWRD,'SHIFT'))
      IF (WORD(ALLKEY,'OLDENS').AND.DOPRNT ) WRITE(6,790)               CIO
      IF (WORD(ALLKEY,'SCFCRT').AND.DOPRNT ) WRITE(6,800)               CIO
     & READA(KEYWRD,INDEX(KEYWRD,'SCFCRT'))
      IF (WORD(ALLKEY,'ENPART').AND.DOPRNT ) WRITE(6,810)               CIO
      IF (WORD(ALLKEY,'NOXYZ').AND.DOPRNT ) WRITE(6,820)                CIO
      IF (WORD(ALLKEY,'SIGMA')) THEN
         IF (DOPRNT) WRITE(6,830)                                       CIO
         MAXGEO=MAXGEO+1
      ENDIF
      IF (WORD(ALLKEY,'NLLSQ') ) THEN
         IF (DOPRNT) WRITE(6,840)                                       CIO
         MAXGEO=MAXGEO+1
      ENDIF
      IF (WORD(ALLKEY,'ROOT').AND.DOPRNT ) WRITE(6,850)                 CIO
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'ROOT')))
      IF (WORD(ALLKEY,'TRANS=') ) THEN
         IF (DOPRNT) WRITE(6,870)NINT(READA(KEYWRD,                     CIO
     &                                  INDEX(KEYWRD,'TRANS=')))        CIO
      ELSEIF (WORD(ALLKEY,'TRANS') ) THEN
         IF (DOPRNT) WRITE(6,860)                                       CIO
      ENDIF
      IF (WORD(ALLKEY,'SADDLE') ) THEN
         IF (DOPRNT) WRITE(6,880)                                       CIO
         MAXGEO=MAXGEO+1
      ENDIF
C
C	Start of EF section						IR0494
C
      IF (WORD(ALLKEY,'EF') ) THEN
         IF (DOPRNT) WRITE(6,1190)                                      CIO
         MAXGEO=MAXGEO+1
      ENDIF
      IF (WORD(ALLKEY,'TS') ) THEN
         IF (DOPRNT) WRITE(6,1200)                                      CIO
         MAXGEO=MAXGEO+1
      ENDIF
      IF (WORD(ALLKEY,' IUPD')) THEN
         I=NINT(READA(KEYWRD,INDEX(KEYWRD,' IUPD=')))
         IF (I.EQ.0.AND.DOPRNT) WRITE(6,1210)                           CIO
         IF (I.EQ.1.AND.DOPRNT) WRITE(6,1220)                           CIO
         IF (I.EQ.2.AND.DOPRNT) WRITE(6,1230)                           CIO
      ENDIF
      IF (WORD(ALLKEY,' NONR').AND.DOPRNT ) WRITE(6,1240)               CIO
      IF (WORD(ALLKEY,' HESS')) THEN
         I=NINT(READA(KEYWRD,INDEX(KEYWRD,' HESS=')))
         IF (I.EQ.0.AND.DOPRNT) WRITE(6,1250)                           CIO
         IF (I.EQ.1.AND.DOPRNT) WRITE(6,1260)                           CIO
         IF (I.EQ.2.AND.DOPRNT) WRITE(6,1270)                           CIO
         IF (I.EQ.3.AND.DOPRNT) WRITE(6,1280)                           CIO
      ENDIF
      IF (WORD(ALLKEY,' MODE').AND.DOPRNT) WRITE(6,1290)                CIO
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'MODE=')))
      IF (WORD(ALLKEY,' RSCAL').AND.DOPRNT ) WRITE(6,1300)              CIO
      IF (WORD(ALLKEY,' RECALC').AND.DOPRNT) WRITE(6,1310)              CIO
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'RECALC')))
      IF (WORD(ALLKEY,' DMAX').AND.DOPRNT) WRITE(6,1320)                CIO
     1 READA(KEYWRD,INDEX(KEYWRD,'DMAX='))
      IF (WORD(ALLKEY,' OMIN').AND.DOPRNT) WRITE(6,1330)                CIO
     1 READA(KEYWRD,INDEX(KEYWRD,'OMIN='))
      IF (WORD(ALLKEY,' RMIN').AND.DOPRNT) WRITE(6,1340)                CIO
     1 READA(KEYWRD,INDEX(KEYWRD,' RMIN='))
      IF (WORD(ALLKEY,' RMAX').AND.DOPRNT) WRITE(6,1350)                CIO
     1 READA(KEYWRD,INDEX(KEYWRD,' RMAX='))
      IF (WORD(ALLKEY,' DDMIN').AND.DOPRNT)  WRITE(6,1360)              CIO
     1 READA(KEYWRD,INDEX(KEYWRD,' DDMIN='))
      IF (WORD(ALLKEY,' DDMAX').AND.DOPRNT)  WRITE(6,1370)              CIO
     1 READA(KEYWRD,INDEX(KEYWRD,' DDMAX='))
      IF (WORD(ALLKEY,' PRNT').AND.DOPRNT)  WRITE(6,1380)               CIO
C
C	END of EF section						IR0494
C
      IF (WORD(ALLKEY,' LET').AND.DOPRNT )  WRITE(6,890)                CIO
      IF (WORD(ALLKEY,'COMPFG').AND.DOPRNT )  WRITE(6,900)              CIO
      IF (WORD(ALLKEY,'DERIV').AND.DOPRNT )  WRITE(6,910)               CIO
      IF (WORD(ALLKEY,'FULSCF').AND.DOPRNT )  WRITE(6,920)              CIO
      IF (WORD(ALLKEY,'DCART').AND.DOPRNT )  WRITE(6,930)               CIO
      IF (WORD(ALLKEY,'GNORM').AND.DOPRNT )  WRITE(6,940)               CIO
     1 READA(KEYWRD,INDEX(KEYWRD,'GNORM'))
      IF (WORD(ALLKEY,'FMAT').AND.DOPRNT )  WRITE(6,950)                CIO
      IF (WORD(ALLKEY,'HCORE').AND.DOPRNT )  WRITE(6,960)               CIO
      IF (WORD(ALLKEY,'ITER').AND.DOPRNT )  WRITE(6,970)                CIO
      IF (WORD(ALLKEY,'PULAY').AND.DOPRNT )  WRITE(6,980)               CIO
      IF (WORD(ALLKEY,'LINMIN').AND.DOPRNT )  WRITE(6,990)              CIO
      IF (WORD(ALLKEY,'LOCMIN') ) THEN
         IF (DOPRNT) WRITE(6,1000)                                      CIO
         I=INDEX(KEYWRD,'LOCMIN')
         KEYWRD(I:I+5)='LINMIN'
      ENDIF
      IF (WORD(ALLKEY,' STEP1').AND.DOPRNT  )  WRITE(6,1010)            CIO
     1 READA(KEYWRD,INDEX(KEYWRD,'STEP1')+6)
      IF (WORD(ALLKEY,' STEP2').AND.DOPRNT  )  WRITE(6,1020)            CIO
     1 READA(KEYWRD,INDEX(KEYWRD,'STEP2')+6)
      IF (WORD(ALLKEY,'BAR').AND.DOPRNT )  WRITE(6,1030)                CIO
     1 READA(KEYWRD,INDEX(KEYWRD,'BAR'))
      IF (WORD(ALLKEY,'DEBUGPULAY').AND.DOPRNT )  WRITE(6,1040)         CIO
      IF (WORD(ALLKEY,'CAMP').AND.DOPRNT )  WRITE(6,1050)               CIO
      IF (WORD(ALLKEY,'KING').AND.DOPRNT )  WRITE(6,1050)               CIO
      IF (WORD(ALLKEY,'EIGS').AND.DOPRNT )  WRITE(6,1060)               CIO
      IF (WORD(ALLKEY,'MOLDAT').AND.DOPRNT )  WRITE(6,1070)             CIO
      IF (WORD(ALLKEY,'HYPERF').AND.DOPRNT )  WRITE(6,1080)             CIO
      IF (WORD(ALLKEY,'OPCI').AND.DOPRNT )  WRITE(6,1090)               CIO
      IF (WORD(ALLKEY,' PL').AND.DOPRNT )  WRITE(6,1100)                CIO
      IF (WORD(ALLKEY,'SEARCH') ) THEN
         IF (DOPRNT) WRITE(6,1110)                                      CIO
         I=INDEX(KEYWRD,'SEARCH')
         KEYWRD(I:I+5)='LINMIN'
      ENDIF
      IF (WORD(ALLKEY,'CYCLES').AND.DOPRNT ) WRITE(6,1130)              CIO
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'CYCLES')))
      IF (WORD(ALLKEY,'FILL').AND.DOPRNT ) WRITE(6,1120)                CIO
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'FILL')))
      IF (WORD(ALLKEY,'ITRY').AND.DOPRNT ) WRITE(6,1160)                CIO
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'ITRY')))
      IF (WORD(ALLKEY,'0SCF').AND.DOPRNT ) WRITE(6,1180)                CIO
      IF(UHF)THEN
         IF(BIRAD.OR.EXCI.OR.CI)THEN
            IF (DOPRNT) WRITE(6,'(//10X,                                CIO
     1'' UHF USED WITH EITHER BIRAD, EXCITED OR C.I. '')')
            IF (DOPRNT) WRITE(6,1170)                                   CIO
            GOTO 60
         ENDIF
      ELSE
         IF(EXCI.AND. TRIP) THEN
            IF(DOPRNT)WRITE(6,'(//10X,'' EXCITED USED WITH TRIPLET'')') CIO
            IF (DOPRNT) WRITE(6,1170)                                   CIO
            GOTO 60
         ENDIF
      ENDIF
      IF (INDEX(KEYWRD,'T-PRIO').NE.0.AND.
     1INDEX(KEYWRD,'DRC').EQ.0) THEN
         IF (DOPRNT) WRITE (6,'(//10X,''T-PRIO AND NO DRC'')')          CIO
         IF (DOPRNT) WRITE (6,1170)                                     CIO
         GOTO 60
      ENDIF
      IF ( METHOD .GT. 1) THEN
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     1'' ONLY ONE OF MINDO, MNDO, AM1, PM3, PDG, MDG, '',
     2'' AM1-D, PM3-D, PM6, ETC. ALLOWED'')')                           LF0310
         IF (DOPRNT) WRITE (6,1170)                                     CIO
         GOTO 60
      ENDIF
      IF (WORD(ALLKEY,'THERMO') )THEN
         IF (DOPRNT) WRITE(6,1140)                                      CIO
         IF(WORD(ALLKEY,' ROT')) THEN                                   CIO
            IF (DOPRNT) WRITE(6,1150)                                   CIO
     &               NINT(READA(KEYWRD,INDEX(KEYWRD,' ROT')))           CIO
         ELSE
            IF (DOPRNT) WRITE(6,'                                       CIO
     1    (//10X,'' YOU MUST SUPPLY THE SYMMETRY NUMBER "ROT"'')')
      RETURN 1                                                           CSTP (stop)
         ENDIF
      ENDIF
      IF(MAXGEO.GT.1)THEN
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     &   ''MORE THAN ONE GEOMETRY OPTION HAS BEEN '',                   CIO
     1''SPECIFIED'',/10X,
     2''CONFLICT MUST BE RESOLVED BEFORE JOB WILL RUN'')')
      RETURN 1                                                           CSTP (stop)
      ENDIF
C
C ADD H...H REPULSIVE COORECTION                                        JP1203
C                                                                         .. 
      IF ( WORD(ALLKEY,'HHON') ) THEN                                   JP1203
         IF (AM1D.OR.PM3D.OR.MNDOD.OR.                                  LF0310
     &       MNDOD3.OR.AM1D3)   THEN                                    LF1111
          IF(DOPRNT)WRITE(6,'(//10X,'' YOU CANNOT USE HHON REPULSIVE '',LF0310 / CIO
     &                ''CORRECTION WITH DISPERSION METHODS.'')')        LF0310
      RETURN 1                                                           CSTP (stop)

         ENDIF                                                          LF0310
C                                                                       JP1203
C READ THE HH GAUSSIAN FROM AN EXTERNAL FILE                              ..
C                                                                         ..
         IF ( INDEX(KEYWRD,'HHON=').NE.0 ) THEN                           ..
           I=INDEX(KEYWRD,'HHON=')+5                                      ..
           J=INDEX(KEYWRD(I:),' ')+I-1                                    ..
           FHHGAU=KEYWRD(I:J)                                             ..
                                                                          ..
           OPEN (UNIT=98,STATUS='UNKNOWN',FILE=FHHGAU)                    .. / No CIO
           READ(98,*) J                                                   .. / No CIO 
           IF (J .LT. 0 .OR. J .GT. 3) THEN                               ..
               IF (DOPRNT) WRITE(6, *)                                    .. / CIO
     &               'ONLY UP TO THREE H...H GAUSSIAN ALLOWED'            .. / CIO
               RETURN 1                                                   .. / CSTP (stop)
                                                                          ..
           ENDIF                                                          .. 
           DO I = 1, J                                                    ..
             READ(98, *) HHA0(I), HHR0(I), HHD0(I)                        .. / No CIO
           ENDDO                                                          ..
           CLOSE(98)                                                      .. / No CIO
C                                                                         ..
C SET SOME DEFAULT VALUES WITH ZERO AMPLITUDE GAUSSIAN                    ..  
C                                                                         ..
           DO I = J+1, 3                                                  ..
               HHA0(I)=0.0D0                                              ..
               HHR0(I)=1.0D0                                              ..
               HHD0(I)=0.3D0                                              ..
           ENDDO                                                          ..
C                                                                         ..
C PRINT OUT HH GAUSSIAN INFORMATION                                       ..
C                                                                         ..
           IF (DOPRNT) WRITE(6,1390)                                      .. / CIO
           IF (DOPRNT) WRITE(6,1395) FHHGAU                               .. / CIO
           DO I = 1, J                                                    ..
              IF (DOPRNT) WRITE(6,1400) HHA0(I),HHR0(I),HHD0(I)           .. / CIO
           ENDDO                                                          ..
C                                                                         ..
C OTHER WISE, PM3-CHC-SRP AND AM1-CHC-SRP GAUSSIAN AS DEFAULT             .. 
C                                                                         ..
         ELSE                                                             ..
           DO I = 1, 3                                                    ..
               HHA0(I)=0.0D0                                              ..
               HHR0(I)=1.0D0                                              ..
               HHD0(I)=0.3D0                                              ..
           ENDDO                                                          ..
           IF ( ( INDEX(KEYWRD,'PM3').NE.0 ) .AND.                      JP1203
     1          (INDEX(KEYWRD,'PM3-D').EQ.0) ) THEN                     LF0309
              HHA0(1) = 3.198D0                                         JP1203
              HHR0(1) = 1.700D0                                           ..
              HHD0(1) = 0.522D0                                           ..
           ELSE IF ( ( INDEX(KEYWRD,'AM1').NE.0 ) .AND.                 JP1203
     1          (INDEX(KEYWRD,'AM1-D').EQ.0) ) THEN                     LF0509 
              HHA0(1) = 1.064D0                                         JP1203
              HHR0(1) = 1.603D0                                           ..
              HHD0(1) = 0.808D0                                           ..
           ENDIF                                                          ..
C                                                                         ..
C PRINT OUT DEFAULT HH GAUSSIAN INFORMATION                               ..
C                                                                         ..
           IF (DOPRNT) WRITE(6,1390)                                      .. / CIO  
           IF ( (INDEX(KEYWRD,'PM3').NE.0).AND.                         JP1203
     1          (INDEX(KEYWRD,'PM3-D').EQ.0) ) THEN                     LF0309
               IF (DOPRNT) WRITE(6,1396)                                JP1203 / CIO
           ELSE IF ( (INDEX(KEYWRD,'AM1').NE.0 ) .AND.                  JP1203
     1          (INDEX(KEYWRD,'AM1-D').EQ.0) ) THEN                     LF0509
               IF (DOPRNT) WRITE(6,1397)                                JP1203 / CIO
           ENDIF                                                          ..
           IF (DOPRNT) WRITE(6,1400) HHA0(1),HHR0(1),HHD0(1)              .. / CIO
         ENDIF                                                            ..
      ENDIF                                                             JP1203
C
      IF(ALLKEY.NE.' ')THEN
C
C  AN UNRECOGNIZED KEY-WORD HAS BEEN USED. FIND IT AND THEN ABANDON THE
C  RUN
         J=0
         DO 50 I=1,79
            IF(ALLKEY(I:I).NE.' '.OR.ALLKEY(I:I+1).NE.'  ')THEN
               J=J+1
               KEYWRD(J:J)=ALLKEY(I:I)
            ENDIF
   50    CONTINUE
         IF(ALLKEY(160:160).NE.' ')THEN
            J=J+1
            KEYWRD(J:J)=ALLKEY(160:160)
         ENDIF
         J=MAX(1,J)
         IF (DOPRNT) THEN                                               CIO
           WRITE(6,'(///10X,''UNRECOGNIZED KEY-WORDS: ('',A,'')'')')    CIO
     &       KEYWRD(:J)                                                 CIO
           WRITE(6,'(///10X,                                            CIO
     &       ''CALCULATION STOPPED TO AVOID WASTING TIME.'')')          CIO
         ENDIF                                                          CIO
      RETURN 1                                                          CSTP (stop)
      ENDIF
      RETURN
   60 IF (DOPRNT) WRITE(6,'(//10X,'' CALCULATION ABANDONED, SORRY!'')') CIO
      RETURN 1                                                           CSTP (stop)
   70 FORMAT(' *  VECTORS  - FINAL EIGENVECTORS TO BE PRINTED')
   80 FORMAT(' *  EXTERNAL - USE ATOMIC PARAMETERS FROM THE FOLLOWING '
     1,'FILE',/15X,A)
   90 FORMAT(' *  DENSITY  - FINAL DENSITY MATRIX TO BE PRINTED')
  100 FORMAT(' *  SPIN     - FINAL UHF SPIN MATRIX TO BE PRINTED')
  110 FORMAT(' *  DEPVAR=N - SPECIFIED DISTANCE IS',F7.4,
     1' TIMES BOND LENGTH')
  120 FORMAT(' *  DEP      - OUTPUT FORTRAN CODE FOR BLOCK-DATA')
  130 FORMAT(' *  VELOCITY - INPUT STARTING VELOCITIES FOR DRC')
  140 FORMAT(' *  TIMES    - TIMES OF VARIOUS STAGES TO BE PRINTED')
  150 FORMAT(' *  PARASOK  - USE SOME MNDO PARAMETERS IN AN AM1 CALCULA'
     1,'TION')
  160 FORMAT(' *  FLEPO    - PRINT DETAILS OF GEOMETRY OPTIMIZATION')
  170 FORMAT(' *  BONDS    - FINAL BOND-ORDER MATRIX TO BE PRINTED')
  180 FORMAT(' *  GEO-OK   - OVERRIDE INTERATOMIC DISTANCE CHECK')
  190 FORMAT(' *  FOCK     - LAST FOCK MATRIX TO BE PRINTED')
  200 FORMAT(' *  LARGE    - EXPANDED OUTPUT TO BE PRINTED')
  210 FORMAT(' *  S1978    - 1978 SULFUR PARAMETERS TO BE USED')
  220 FORMAT(' *  SI1978   - 1978 SILICON PARAMETERS TO BE USED')
  230 FORMAT(' *  GRAPH    - GENERATE FILE FOR GRAPHICS')
  240 FORMAT(' *  1ELECTRON- FINAL ONE-ELECTRON MATRIX TO BE PRINTED')
  250 FORMAT(' *  ESR      - RHF SPIN DENSITY CALCULATION REQUESTED')
  260 FORMAT(' *  NOMM     - DO NOT MAKE MM CORRECTION TO CONH BARRIER')
  265 FORMAT(' *  MMOK     - APPLY MM CORRECTION TO CONH BARRIER')
  270 FORMAT(' *  INTERP   - PRINT DETAILS OF CAMP-KING CONVERGER')
  280 FORMAT(' *  DFP      - USE DAVIDON FLETCHER POWELL OPTIMIZER')
  290 FORMAT(' *  ANALYT   - USE ANALYTIC DERIVATIVES ')
  300 FORMAT(' *  MECI     - M.E.C.I. WORKING TO BE PRINTED')
  310 FORMAT(' *  DRC      - DYNAMIC REACTION COORDINATE CALCULATION')
  320 FORMAT(' *  DRC=     - HALF-LIFE FOR KINETIC ENERGY LOSS =',F9.2,
     1' * 10**(-14) SECONDS')
  330 FORMAT(' *  KINETIC= - ',F7.3,' KCAL KINETIC ENERGY ADDED TO DRC')
  340 FORMAT(' *  LOCALIZE - LOCALIZED ORBITALS TO BE PRINTED')
  350 FORMAT(' *  MULLIK   - THE MULLIKEN ANALYSIS TO BE PERFORMED')
  360 FORMAT(' *   XYZ     - CARTESIAN COORDINATE SYSTEM TO BE USED')
  370 FORMAT(' *   PI      - BONDS MATRIX, SPLIT INTO SIGMA-PI-DELL',
     1' COMPONENTS, TO BE PRINTED')
  380 FORMAT(' *  ECHO     - ALL INPUT DATA TO BE ECHOED BEFORE RUN')
  390 FORMAT(' *  H-PRIOR  - HEAT OF FORMATION TAKES PRIORITY IN DRC')
  400 FORMAT(' *  X-PRIOR  - GEOMETRY CHANGES TAKE PRIORITY IN DRC')
  410 FORMAT(' *  T-PRIOR  - TIME TAKES PRIORITY IN DRC')
  420 FORMAT(' *  POWSQ    - PRINT DETAILS OF WORKING IN POWSQ')
  430 FORMAT(' *  POLAR    - CALCULATE FIRST, SECOND AND THIRD-ORDER'
     1,' POLARIZABILITIES')
  440 FORMAT(' *  DEBUG    - DEBUG OPTION TURNED ON')
  450 FORMAT(' *  RESTART  - CALCULATION RESTARTED')
  460 FORMAT(' *  IRC=N    - INTRINSIC REACTION COORDINATE',I3,
     1' DEFINED')
  470 FORMAT(' *  IRC      - INTRINSIC REACTION COORDINATE CALCULATION')
  480 FORMAT(3(' *',/),' *',15X,'  CHARGE ON SYSTEM =',I3,3(/,' *'))
  490 FORMAT(' *  GRADIENTS- ALL GRADIENTS TO BE PRINTED')
  500 FORMAT(' *  UHF      - UNRESTRICTED HARTREE-FOCK CALCULATION')
  510 FORMAT(' *  SINGLET  - STATE REQUIRED MUST BE A SINGLET')
  520 FORMAT(' *  BIRADICAL- SYSTEM HAS TWO UNPAIRED ELECTRONS')
  530 FORMAT(' *  EXCITED  - FIRST EXCITED STATE IS TO BE OPTIMIZED')
  540 FORMAT(' *  SINGLET  - SPIN STATE DEFINED AS A SINGLET')
  550 FORMAT(' *  DOUBLET  - SPIN STATE DEFINED AS A DOUBLET')
  560 FORMAT(' *  TRIPLET  - SPIN STATE DEFINED AS A TRIPLET')
  570 FORMAT(' *  QUARTET  - SPIN STATE DEFINED AS A QUARTET')
  580 FORMAT(' *  QUINTET  - SPIN STATE DEFINED AS A QUINTET')
  590 FORMAT(' *  SEXTET   - SPIN STATE DEFINED AS A SEXTET')
  600 FORMAT(' *  SYMMETRY - SYMMETRY CONDITIONS TO BE IMPOSED')
  610 FORMAT(' *  MICROS=N -',I4,' MICROSTATES TO BE SUPPLIED FOR C.I.')
  620 FORMAT(' *  OPEN(N,N)- THERE ARE',I2,' ELECTRONS IN',I2,' LEVELS')
  630 FORMAT(' *   T=      - A TIME OF',F8.1,' ',A7,' REQUESTED')
  640 FORMAT(' *   T=      - A TIME OF',G11.3,' ',A7,' REQUESTED')
  650 FORMAT(' *  DUMP=N   - RESTART FILE WRITTEN EVERY',F8.1,
     1' ',A7)
  660 FORMAT(' *  DUMP=N   - RESTART FILE WRITTEN EVERY',G11.3,
     1' ',A7)
  670 FORMAT(' *  1SCF     - DO 1 SCF AND THEN STOP ')
  680 FORMAT(' *  C.I.=N   -',I2,' M.O.S TO BE USED IN C.I.')
  681 FORMAT(' * C.I.=(N,M)-',I2,' DOUBLY FILLED LEVELS USED IN A ',/
     1,      ' *             C.I. INVOLVING ',I2,' M.O.''S')
  690 FORMAT(' *  DFORCE   - PRINT HESSIAN MATRIX IN FORCE')
  700 FORMAT(' *  FORCE    - FORCE CALCULATION SPECIFIED')
  710 FORMAT(' *  MINDO/3  - THE MINDO/3 HAMILTONIAN TO BE USED')
  720 FORMAT(' *  AM1      - THE AM1 HAMILTONIAN TO BE USED')
  721 FORMAT(' *  AM1-D    - THE AM1-D HAMILTONIAN TO BE USED')         LF0509
  722 FORMAT(' *  MNDO-D   - THE MNDO-D HAMILTONIAN TO BE USED')        LF0310
  723 FORMAT(' *  RM1-D    - THE RM1-D HAMILTONIAN TO BE USED')         LF0310
  724 FORMAT(' *  PM6-D    - THE PM6-D HAMILTONIAN TO BE USED')         LF0310
  725 FORMAT(' *  MNDO-D3  - THE MNDO-D3 HAMILTONIAN TO BE USED')       LF1111
  726 FORMAT(' *  AM1-D3   - THE AM1-D3 HAMILTONIAN TO BE USED')        LF1111
  727 FORMAT(' *  PM3-D3   - THE PM3-D3 HAMILTONIAN TO BE USED')        LF1211
  728 FORMAT(' *  RM1-D3   - THE RM1-D3 HAMILTONIAN TO BE USED')        LF1211
  729 FORMAT(' *  PM6-D3   - THE PM6-D3 HAMILTONIAN TO BE USED')        LF1211
  730 FORMAT(' *  PM3      - THE PM3 HAMILTONIAN TO BE USED')
  731 FORMAT(' *  PM3-D    - THE PM3-D HAMILTONIAN TO BE USED')         LF0309
  732 FORMAT(' *  PDG      - THE PDDG/PM3 HAMILTONIAN TO BE USED')      0706ZJJ
  733 FORMAT(' *  PM6      - THE PM6 HAMILTONIAN TO BE USED')           LF0709
  734 FORMAT(' *  MDG      - THE PDDG/MNDO HAMILTONIAN TO BE USED')     0706ZJJ 
  736 FORMAT(' *  RM1      - THE RM1 HAMILTONIAN TO BE USED')           0806ZJJ
  738 FORMAT(' *  CM2      - THE CHARGE MODEL 2 TO BE USED (CM2A)')     0909YC99
  739 FORMAT(' *  CM2      - THE CHARGE MODEL 2 TO BE USED (CM2P)')     0909YC99
  740 FORMAT(' *  PRECISE  - CRITERIA TO BE INCREASED BY 100 TIMES')
  750 FORMAT(' *  NOINTER  - INTERATOMIC DISTANCES NOT TO BE PRINTED')
  760 FORMAT(' *  ISOTOPE  - FORCE MATRIX WRITTEN TO DISK (CHAN. 9 )')
  770 FORMAT(' *  DENOUT   - DENSITY MATRIX OUTPUT ON CHANNEL 10')
  780 FORMAT(' *  SHIFT    - A DAMPING FACTOR OF',F8.2,' DEFINED')
  790 FORMAT(' *  OLDENS   - INITIAL DENSITY MATRIX READ OF DISK')
  800 FORMAT(' *  SCFCRT   - DEFAULT SCF CRITERION REPLACED BY',G12.3)
  810 FORMAT(' *  ENPART   - ENERGY TO BE PARTITIONED INTO COMPONENTS')
  820 FORMAT(' *  NOXYZ    - CARTESIAN COORDINATES NOT TO BE PRINTED')
  830 FORMAT(' *  SIGMA    - GEOMETRY TO BE OPTIMIZED USING SIGMA.')
  840 FORMAT(' *  NLLSQ    - GRADIENTS TO BE MINIMIZED USING NLLSQ.')
  850 FORMAT(' *  ROOT     - IN A C.I. CALCULATION, ROOT',I2,
     1                       ' TO BE OPTIMIZED.')
  860 FORMAT(' *  TRANS    - THE REACTION VIBRATION TO BE DELETED FROM',
     1' THE THERMO CALCULATION')
  870 FORMAT(' *  TRANS=   - ',I4,' VIBRATIONS ARE TO BE DELETED FROM',
     1' THE THERMO CALCULATION')
  880 FORMAT(' *  SADDLE   - TRANSITION STATE TO BE OPTIMIZED')
  890 FORMAT(' *   LET     - OVERRIDE SOME SAFETY CHECKS')
  900 FORMAT(' *  COMPFG   - PRINT HEAT OF FORMATION CALC''D IN COMPFG')
  910 FORMAT(' *  DERIV    - PRINT PART OF WORKING IN SUB. DERIV')
  920 FORMAT(' *  FULSCF   - IN SEARCHES, FULL SCF CALCN''S TO BE DONE')
  930 FORMAT(' *  DCART    - PRINT DETAILS OF WORKING IN DCART')
  940 FORMAT(' *  GNORM=   - EXIT WHEN GRADIENT NORM DROPS BELOW ',G8.3)
  950 FORMAT(' *  FMAT     - PRINT DETAILS OF WORKING IN FMAT')
  960 FORMAT(' *  HCORE    - PRINT DETAILS OF WORKING IN HCORE')
  970 FORMAT(' *  ITER     - PRINT DETAILS OF WORKING IN ITER')
  980 FORMAT(' *  PULAY    - PULAY''S METHOD TO BE USED IN SCF')
  990 FORMAT(' *  LINMIN   - PRINT DETAILS OF LINE MINIMIZATION')
 1000 FORMAT(' *  LOCMIN   - USE LINMIN INSTEAD OF THIS KEYWORD')
 1010 FORMAT(' *  STEP1    - FIRST  STEP-SIZE IN GRID =',F7.2)
 1020 FORMAT(' *  STEP2    - SECOND STEP-SIZE IN GRID =',F7.2)
 1030 FORMAT(' *  BAR=     - REDUCE BAR LENGTH BY A MAX. OF',F7.2)
 1040 FORMAT(' *  DEBUGPULAY-PRINT DETAILS OF WORKING IN PULAY')
 1050 FORMAT(' *  CAMP,KING- THE CAMP-KING CONVERGER TO BE USED')
 1060 FORMAT(' *  EIGS     - PRINT ALL EIGENVALUES IN ITER')
 1070 FORMAT(' *  MOLDAT   - PRINT DETAILS OF WORKING IN MOLDAT')
 1080 FORMAT(' *  HYPERFINE- HYPERFINE COUPLING CONSTANTS TO BE'
     1,' PRINTED')
 1090 FORMAT(' *  OPCI     - PRINT DETAILS OF WORKING IN OPCI')
 1100 FORMAT(' *   PL      - MONITOR CONVERGANCE IN DENSITY MATRIX')
 1110 FORMAT(' *  SEARCH   - USE LINMIN INSTEAD OF THIS KEYWORD')
 1120 FORMAT(' *  FILL=    - IN RHF CLOSED SHELL, FORCE M.O.',I3,' TO BE
     1 FILLED')
 1130 FORMAT(' *  CYCLES=  - DO A MAXIMUM OF ',I4,' STEPS')
 1140 FORMAT(' *  THERMO   - THERMODYNAMIC QUANTITIES TO BE CALCULATED')
 1150 FORMAT(' *  ROT      - SYMMETRY NUMBER OF',I3,' SPECIFIED')
 1160 FORMAT(' *  ITRY=    - DO A MAXIMUM OF',I6,' ITERATIONS FOR SCF')
 1170 FORMAT( //10X,' IMPOSSIBLE OPTION REQUESTED,')
 1180 FORMAT(' *  0SCF     - AFTER READING AND PRINTING DATA, STOP')
C
C	EF-specific FORMATs                                             IR0494
C
 1190 FORMAT(' *  EF       - USE EF ROUTINE FOR MINIMUM SEARCH')
 1200 FORMAT(' *  TS       - USE EF ROUTINE FOR TS SEARCH')
 1210 FORMAT(' *  IUPD=0   - EF: HESSIAN WILL NOT BE UPDATED')
 1220 FORMAT(' *  IUPD=1   - EF: HESSIAN UPDATED USING POWELL')
 1230 FORMAT(' *  IUPD=2   - EF: HESSIAN UPDATED USING BFGS')
 1240 FORMAT(' *  NONR     - EF: DO NOT USE NEWTON-RAPHSON STEP')
 1250 FORMAT(' *  HESS=0   - EF: DIAGONAL HESSIAN AS INITIAL GAUSS')
 1260 FORMAT(' *  HESS=1   - EF: HESSIAN CALCULATED NUMERICALLY')
 1270 FORMAT(' *  HESS=2   - EF: INITIAL HESSIAN READ FROM DISK')
 1280 FORMAT(' *  HESS=3   - EF: HESSIAN CALCULATED DOUBLE NUMERICALLY')    
 1290 FORMAT(' *  MODE=    - EF: FOLLOW HESSIAN MODE',I3,' TOWARD TS')
 1300 FORMAT(' *  RSCAL    - EF: SCALE P-RFO STEP TO TRUST RADIUS')
 1310 FORMAT(' *  RECALC=  - EF: HESSIAN RECALC EVERY',I4,' CYCLES ')
 1320 FORMAT(' *  DMAX=    - EF: STARTING TRUST RADIUS',F7.3,' ANG/RAD')   
 1330 FORMAT(' *  OMIN=    - EF: MINIMUM EIGENVECTOR OVERLAP ',F7.3)
 1340 FORMAT(' *  RMIN=    - EF: MIN. CALC./PRED. ENERGY STEP ',F7.3)
 1350 FORMAT(' *  RMAX=    - EF: MAX. CALC./PRED. ENERGY STEP ',F7.3)
 1360 FORMAT(' *  DDMIN=   - EF: MINIMUM TRUST RADIUS',F7.3,' ANG/RAD')
 1370 FORMAT(' *  DDMAX=   - EF: MAXIMUM TRUST RADIUS',F7.3,' ANG/RAD')
 1380 FORMAT(' *  PRNT     - EF: EXTRA PRINTING')
 1390 FORMAT(' *  HHON     - ADD H...H REPULSIVE GAUSSIAN CORRECTION')  JP1203
 1395 FORMAT(' *             READ FROM FOLLOWING FILE:',1X,A)             ..
 1396 FORMAT(' *             USE PM3-CHC-SRP GAUSSIAN AS THE DEFUALT')    ..
 1397 FORMAT(' *             USE AM1-CHC-SRP GAUSSIAN AS THE DEFUALT')    .. 
 1400 FORMAT(' *             E(HH)=',                                     ..
     &                       F5.3,'exp{-[(rHH-',F5.3,')/',F5.3,']**2}') JP1203 
C
C	End of  EF-specific FORMATs
C       Start of more recent keyword FORMATs
C
 1410 FORMAT(' *  PM6G09   - THE PM6 HAMILTONIAN WITH GAUSSIAN C-C',    LF0210
     &       ' TRIPLE BONDS TO BE USED')                                LF0210
 1420 FORMAT(' *  DIPG09   - OUTPUT BOTH MOPAC AND GAUSSIAN DIPOLE',    LF0210
     &       ' MOMENTS')                                                LF0210
 1430 FORMAT(' *  SDAMP    - TANG/TOENNIES DAMPING FUNCTION USED IN',   LF0310
     &       ' DISPERSION TERMS')                                       LF0310
 1431 FORMAT(///10X,'CANNOT USE DAMPING WITHOUT ONE OF THE METHODS:',   LF0312
     &       //10X,'AM1-D, PM3-D, MNDO-D, PM6-D, OR RM1-D.',//8X,       LF0312
     &       'CALCULATION STOPPED.')                                    LF0310
 1432 FORMAT(' *  HGDAMP   - HEAD-GORDON DAMPING FUNCTION USED IN',     LF0312
     &       ' DISPERSION TERMS')                                       LF0312
 1433 FORMAT(' *  TDAMP    - TRUHLAR DAMPING FUNCTION USED IN',         LF0312
     &       ' DISPERSION TERMS')                                       LF0312
 1434 FORMAT(' *  HGDISP   - HEAD-GORDON DISPERSION FUNCTION USED')     LF0312
 1435 FORMAT(' *  TDISP    - TRUHLAR DISPERSION FUNCTION USED')         LF0312
 1436 FORMAT(' *  DPAIR    - USE PAIRWISE D VALUES FOR DISPERSION')     LF0312
 1437 FORMAT(' *  CPAIR    - USE PAIRWISE C6 VALUES FOR DISPERSION')    LF0312
 1438 FORMAT(' *  DAMP5    - USING ALTERNATE DAMPING FUNCTION:',/,      LF0312
     &       '                   R**6*(1-EXP(-(R/d)**8))')              LF0312
 1501 FORMAT(' *  MOD1     - USE RESONANCE INTEGRAL MODIFICATION')      LF1010
 1502 FORMAT(' *  MOD2     - USE MODIFIED 2-ELECTRON INTEGRAL')         LF1010
 1503 FORMAT(' *  MOD3     - USE PAIRWISE CORE-CORE INTERACTION')       LF1010
 1504 FORMAT(' *  MOD4     - ZERO QUADRUPOLE TERMS IN HP 2-ELEC INTEGRAL LF1010
     &')                                                                 LF1010
 1505 FORMAT(' *  MOD5     - USE GENERALIZED PAIRWISE CORE-CORE INTERACT LF1010
     &ION')                                                              LF1010
 1506 FORMAT(' *  PMOv1    - THE PMO VERSION 1 HAMILTONIAN TO BE USED') LF1010
 1507 FORMAT(' *  MOD6     - USING PMOV1A MODIFICATIONS TO PMOV1.')     LF0111
 1508 FORMAT(' *  MOD7     - USING NEW PAIRWISE RESONANCE INTEGRAL.')   LF0211
 1510 FORMAT(' *  HYBRID=x - TOTAL DIPOLE USES HYBRID DIPOLE CONTRIBUTIOLF1111
     &N WITH FACTOR=',F6.4)                                             LF1111
 1511 FORMAT(' *  PMO2     - THE PMO2 HAMILTONIAN IS TO BE USED')       LF0113
 1512 FORMAT(' *  PMO2a    - THE PMO2a HAMILTONIAN IS TO BE USED')      LF0614


      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY WRTKEY_INIT                                                 CSAV
            ALLKEY = ""                                                 CSAV
               AM1 = .FALSE.                                            CSAV
              AM1D = .FALSE.                                            CSAV
             AM1D3 = .FALSE.                                            CSAV / LF1111
             BIRAD = .FALSE.                                            CSAV
                CH = ''                                                 CSAV
            CHRONO = ""                                                 CSAV
                CI = .FALSE.                                            CSAV
              EXCI = .FALSE.                                            CSAV
            FHHGAU = ""                                                 CSAV
              HHD0 = 0.0D0                                              CSAV
              HHR0 = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
             IELEC = 0                                                  CSAV
            ILEVEL = 0                                                  CSAV
                 J = 0                                                  CSAV
            MAXGEO = 0                                                  CSAV
               MDG = .FALSE.                                            CSAV
            METHOD = 0                                                  CSAV
            MINDO3 = .FALSE.                                            CSAV
              MNDO = .FALSE.                                            CSAV
             MNDOD = .FALSE.                                            CSAV
            MNDOD3 = .FALSE.                                            CSAV / LF1111
               PDG = .FALSE.                                            CSAV
               PM3 = .FALSE.                                            CSAV
              PM3D = .FALSE.                                            CSAV
             PM3D3 = .FALSE.                                            CSAV / LF1211
               PM6 = .FALSE.                                            CSAV
              PM6D = .FALSE.                                            CSAV
             PM6D3 = .FALSE.                                            CSAV / LF1211
               RM1 = .FALSE.                                            CSAV
              RM1D = .FALSE.                                            CSAV
             RM1D3 = .FALSE.                                            CSAV / LF1211
              TIME = 0.0D0                                              CSAV
              TRIP = .FALSE.                                            CSAV
               UHF = .FALSE.                                            CSAV
      RETURN                                                            CSAV
      END
