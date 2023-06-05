      SUBROUTINE REFER(*)                                               CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      COMMON /REFS/ ALLREF(120,17)                                      LF1010/LF1211/LF0113
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /KEYWRD/ KEYWRD
      COMMON /HPUSED/ HPUSED                                            LF0210
      include 'method.f'
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      LOGICAL HPUSED                                                    LF0210
      LOGICAL ALLOK, ELEMNS(120), MIXOK, MIX
      LOGICAL USED3                                                     LF1211
      CHARACTER*160 KEYWRD
      CHARACTER*80 ALLREF
CSAV         SAVE                                                           GL0892
      MIX = .FALSE.                                                     CSAV
      MIXOK=(INDEX(KEYWRD,'PARASOK').NE.0)
      DO 10 I=1,102
   10 ELEMNS(I)=.FALSE.
      IF((INDEX(KEYWRD,'PM3').NE.0).AND.
     1  (INDEX(KEYWRD,'PM3-D').EQ.0)) THEN                              LF0309
         MODE=4
      ELSEIF((INDEX(KEYWRD,'AM1').NE.0).AND.
     1  (INDEX(KEYWRD,'AM1-D').EQ.0)) THEN                              LF0509
         MODE=3
      ELSEIF(INDEX(KEYWRD,'MINDO').NE.0)THEN
         MODE=2
      ELSEIF(INDEX(KEYWRD,'PDG').NE.0)THEN                              ZJJ0706
         MODE=5
      ELSEIF(INDEX(KEYWRD,'MDG').NE.0)THEN                              ZJJ0706
         MODE=6 
      ELSEIF(INDEX(KEYWRD,'RM1-D').NE.0)THEN                            LF0310
         MODE=12
      ELSEIF(INDEX(KEYWRD,'RM1').NE.0)THEN                              ZJJ0806
         MODE=7
      ELSEIF(INDEX(KEYWRD,'PM3-D').NE.0)THEN                            LF0309
         MODE=8
      ELSEIF(INDEX(KEYWRD,'AM1-D').NE.0)THEN                            LF0509
         MODE=9
      ELSEIF((INDEX(KEYWRD,'PM6-D').NE.0).OR.                           LF0310
     &       (INDEX(KEYWRD,'PM6G09-D').NE.0))THEN                       LF0310
         MODE=13
      ELSEIF(INDEX(KEYWRD,'PM6').NE.0)THEN                              LF0709
         MODE=10
      ELSEIF(INDEX(KEYWRD,'MNDO-D').NE.0)THEN                           LF0310
         MODE=11
      ELSEIF(INDEX(KEYWRD,'PMOV1').NE.0)THEN
         MODE=14
      ELSEIF(INDEX(KEYWRD,'PMO2A').NE.0)THEN                            LF0614
         MODE=17
      ELSEIF(INDEX(KEYWRD,'PMO2').NE.0)THEN                             LF0113
         MODE=16
      ELSE
         MODE=1
      ENDIF
      USED3=.FALSE.                                                     LF1212
      IF(INDEX(KEYWRD,'MNDO-D3').NE.0)THEN                              LF1211
         MODE=1                                                         LF1211
         USED3=.TRUE.                                                   LF1211
      ELSEIF(INDEX(KEYWRD,'AM1-D3').NE.0)THEN                           LF1211
         MODE=3                                                         LF1211
         USED3=.TRUE.                                                   LF1211
      ELSEIF(INDEX(KEYWRD,'PM3-D3').NE.0)THEN                           LF1211
         MODE=4                                                         LF1211
         USED3=.TRUE.                                                   LF1211
      ELSEIF(INDEX(KEYWRD,'RM1-D3').NE.0)THEN                           LF1211
         MODE=7                                                         LF1211
         USED3=.TRUE.                                                   LF1211
      ELSEIF(INDEX(KEYWRD,'PM6-D3').NE.0)THEN                           LF1211
         MODE=10                                                        LF1211
         USED3=.TRUE.                                                   LF1211
      ENDIF                                                             LF1211
      ALLREF(99,MODE)=' DUMMY ATOMS ARE USED; THESE DO NOT AFFECT '
     1//'THE CALCULATION'
      ALLREF(100,MODE)=' '
      DO 20 I=1,NUMAT
         J=NAT(I)
   20 ELEMNS(J)=.TRUE.
      ALLOK=.TRUE.
      DO 30 I=1,102
         IF(ELEMNS(I))THEN
            IF (USED3.AND.ALLREF(I,15)(1:1).NE.' ') THEN                LF1211
              IF (.NOT.(HPUSED.AND.I.EQ.9)) THEN                        LF1211
                IF (DOPRNT)                                             LF1211
     &            WRITE(6,'(/A,I3/)')                                   LF1211
     &               "***** DISPERSION (-D3) DATA IS NOT AVAILABLE "//  LF1211
     &               "FOR ELEMENT NO. ",I                               LF1211
                ALLOK=.FALSE.                                           LF1211
              ENDIF                                                     LF1211
            ENDIF                                                       LF1211
            IF(I.LT.99.AND..NOT.MIX.AND.MODE.EQ.3)
     1         MIX=(INDEX(ALLREF(I,3),'MNDO').NE.0)
            IF(ALLREF(I,MODE)(1:1).NE.' ')THEN
               IF (DOPRNT) WRITE(6,'(/A,I3/)')
     &               '***** DATA ARE NOT AVAILABLE FOR ELEMENT NO.',I   LF1211
               ALLOK=.FALSE.
            ELSE
              IF (DOPRNT) THEN                                          CIO
               IF (INDEX(KEYWRD,'MNDO-D3').NE.0) THEN                   LF1211
                  WRITE(6,'(A)')ALLREF(I,1)                             LF1211
                  WRITE(6,'(A)')ALLREF(I,15)                            LF1211
               ELSEIF (INDEX(KEYWRD,'AM1-D3').NE.0) THEN                LF1211
                  WRITE(6,'(A)')ALLREF(I,3)                             LF1211
                  WRITE(6,'(A)')ALLREF(I,15)                            LF1211
               ELSEIF (INDEX(KEYWRD,'PM3-D3').NE.0) THEN                LF1211
                  WRITE(6,'(A)')ALLREF(I,4)                             LF1211
                  WRITE(6,'(A)')ALLREF(I,15)                            LF1211
               ELSEIF (INDEX(KEYWRD,'RM1-D3').NE.0) THEN                LF1211
                  WRITE(6,'(A)')ALLREF(I,7)                             LF1211
                  WRITE(6,'(A)')ALLREF(I,15)                            LF1211
               ELSEIF (INDEX(KEYWRD,'PM6-D3').NE.0) THEN                LF1211
                  WRITE(6,'(A)')ALLREF(I,4)                             LF1211
                  WRITE(6,'(A)')ALLREF(I,15)                            LF1211
               ELSEIF (INDEX(KEYWRD,'PM3-D').NE.0) THEN                 LF0309/LF1211
                  WRITE(6,'(A)')ALLREF(I,4)                             LF0309
                  WRITE(6,'(A)')ALLREF(I,8)                             LF0309
               ELSE IF (INDEX(KEYWRD,'AM1-D').NE.0) THEN                LF0509
                  WRITE(6,'(A)')ALLREF(I,3)                             LF0509
                  WRITE(6,'(A)')ALLREF(I,9)                             LF0509
               ELSEIF ((INDEX(KEYWRD,'PM6-D').NE.0).OR.                 LF0310
     &                 (INDEX(KEYWRD,'PM6G09-D').NE.0)) THEN            LF0310
                  WRITE(6,'(A)')ALLREF(I,10)                            LF0310
                  WRITE(6,'(A)')ALLREF(I,13)                            LF0310
                  WRITE(6,'(A)')ALLREF(I,8)                             LF0310
               ELSE IF (INDEX(KEYWRD,'PM6').NE.0) THEN                  LF0709
                  WRITE(6,'(A)')ALLREF(I,4)                             LF0709
                  WRITE(6,'(A)')ALLREF(I,10)                            LF0709
               ELSE IF (INDEX(KEYWRD,'MNDO-D').NE.0) THEN               LF0310
                  WRITE(6,'(A)')ALLREF(I,1)                             LF0310
                  WRITE(6,'(A)')ALLREF(I,11)                            LF0310
                  WRITE(6,'(A)')ALLREF(I,8)                             LF0310
               ELSEIF (INDEX(KEYWRD,'RM1-D').NE.0) THEN                 LF0310
                  WRITE(6,'(A)')ALLREF(I,7)                             LF0310
                  WRITE(6,'(A)')ALLREF(I,12)                            LF0310
                  WRITE(6,'(A)')ALLREF(I,9)                             LF0310
               ELSE                                                     LF0309
                  WRITE(6,'(A)')ALLREF(I,MODE)
               ENDIF                                                    LF0309
              ENDIF                                                     CIO
            ENDIF
         ENDIF
   30 CONTINUE
      IF(MIX.AND..NOT.MIXOK)THEN
         IF (DOPRNT) WRITE(6,40)                                        CIO
     1  'SOME ELEMENTS HAVE BEEN SPECIFIED FOR WHICH ONLY MNDO',
     2  'PARAMETERS ARE AVAILABLE.  SUCH MIXTURES OF METHODS ARE',
     3  'VERY RISKY AND HAVE NOT BEEN FULLY TESTED.  IF YOU FEEL',
     4  'THE RISK IS WORTHWHILE - CHECK THE MANUAL FIRST - THEN',
     5  'SPECIFY "PARASOK" IN THE KEYWORDS'
      RETURN 1                                                           CSTP (stop)
      ENDIF
      IF (LSDAMP.AND.DOPRNT) THEN                                       LF0310 / CIO
        WRITE(6,'(2X,A)') 'Atomic ionization energies reference:'       LF0310
        WRITE(6,'(14X,A)') 'Lide, D. "CRC Handbook of '//               LF0310   
     &                    'Chemistry and Physics",'                     LF0310
        WRITE(6,'(14X,A)') '90th ed.; CRC Press: Boca Raton, FL. 2009.' LF0310
      ENDIF                                                             LF0310
      IF(ALLOK)RETURN
      IF (DOPRNT) WRITE(6,40)                                           CIO
     1 'SOME ELEMENTS HAVE BEEN SPECIFIED FOR WHICH',
     2 'NO PARAMETERS ARE AVAILABLE.  CALCULATION STOPPED.'
   40 FORMAT(/////10X,A,4(/10X,A))
      RETURN 1                                                          CSTP (stop)
 9999 RETURN 1                                                          CSTP
      END
