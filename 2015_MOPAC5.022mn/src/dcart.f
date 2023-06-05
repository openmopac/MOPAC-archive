      SUBROUTINE DCART (COORD,DXYZ,*)                                   CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION COORD(3,*), DXYZ(3,*)
      DIMENSION D3DERV(3,NUMATM)                                        LF1211
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
C Note that include 'method.f' is not used here because the global variable
C "ANALYT" conflicts with a call to the subroutine of the same name.  Thus 
C the following declaration is given for the common block /METHOD/:
      COMMON /METHOD/ LMINDO,LMNDO,LAM1,LPM3,LRM1,LMDG,LPDG,LAM1D,      LF0111
     &                LPM3D,LPM6,AM1PM3,LANLYT,                         LF0111
     &                LPM6G9,                                           LF0111
     &                LMNDOD,LSDAMP,LRM1D,LPM6D,DSPMET,                 LF0111
     &                LPMOV1,LPMOV1A,                                   LF0111
     &                LMNDOD3,LAM1D3,D3METH,                            LF1111
     &                LPM3D3,LRM1D3,LPM6D3,                             LF1211
     &                LHGDAMP,LTDAMP,LHGDISP,LTDISP,LDAMP5              LF0312
      LOGICAL  LMINDO,LMNDO,LAM1,LPM3,LRM1,LMDG,LPDG,LAM1D,LPM3D,LPM6,  LF0111
     &         AM1PM3,LANLYT,LPM6G9,LMNDOD,LSDAMP,LRM1D,LPM6D,DSPMET,   LF0111
     &         LPMOV1,LPMOV1A,                                          LF0111
     &         LMNDOD3,LAM1D3,D3METH,                                   LF1211
     &         LPM3D3,LRM1D3,LPM6D3,                                    LF1211
     &         LHGDAMP,LTDAMP,LHGDISP,LTDISP,LDAMP5                     LF0312
C
C PARAMETER FILE FOR HH GAUSSIAN REPULSION AND COMMON BLOCK TO HOLD     JP1203
C THE GAUSSIAN PARAMETERS                                                 ..
C                                                                         ..
      CHARACTER FHHGAU*64                                                 ..
      COMMON /HHGAU/ HHA0(3),HHR0(3),HHD0(3)                            JP1203
C
C***********************************************************************
C
C    DCART CALCULATES THE DERIVATIVES OF THE ENERGY WITH RESPECT TO THE
C          CARTESIAN COORDINATES. THIS IS DONE BY FINITE DIFFERENCES.
C
C    THE MAIN ARRAYS IN DCART ARE:
C        DXYZ   ON EXIT CONTAINS THE CARTESIAN DERIVATIVES.
C               (KCAL/MOL/ANGSTROM OR KCAL/MOL/RADIAN DEPENDING ON TYPE
C                OF INTERNAL COORDINATE TAKING THE DERIVATIVE WITH
C                RESPECT TO)
C
C***********************************************************************
      COMMON /KEYWRD/ KEYWRD
      COMMON /EULER / TVEC(3,3), ID
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,20),NNHCO,ITYPE,USEMM
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U
      COMMON /DCARTC/ K1L,K2L,K3L,K1U,K2U,K3U
      COMMON /NUMCAL/ NUMCAL
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      COMMON /SCRTCH/ ANSCR(10000),IANSCP                               LF0610
      CHARACTER*160 KEYWRD
      DIMENSION PDI(171),PADI(171),PBDI(171),
     1CDI(3,2),NDI(2),LSTOR1(6), LSTOR2(6), ENG(3)
      LOGICAL DEBUG, FORCE, MAKEP, ANADER, USEMM
      EQUIVALENCE (LSTOR1(1),L1L), (LSTOR2(1), K1L)
         SAVE                                                           GL0892
      DATA CHNGE,CHNGE2 /1.D-4,5.D-5/
*
* CHNGE IS A MACHINE-PRECISION DEPENDENT CONSTANT
* CHNGE2=CHNGE/2
*
      DATA ICALCN /0/
      IF (ICALCN .NE. NUMCAL) THEN
         ANADER= (INDEX(KEYWRD,'ANALYT') .NE. 0)
c#         ANADER= ((INDEX(KEYWRD,'ANALYT') .NE. 0).OR.
c#     &            (INDEX(KEYWRD,'PMOV1') .NE. 0))                       LF1010
         DEBUG = (INDEX(KEYWRD,'DCART') .NE. 0)
         FORCE = (INDEX(KEYWRD,'PRECISE').NE.0.OR.INDEX(KEYWRD,'FORCE')
     1    .NE. 0)
         ICALCN = NUMCAL
      ENDIF
      NCELLS=(L1U-L1L+1)*(L2U-L2L+1)*(L3U-L3L+1)
      DO 10 I=1,6
         LSTOR2(I)=LSTOR1(I)
   10 LSTOR1(I)=0
      IOFSET=(NCELLS+1)/2
      NUMTOT=NUMAT*NCELLS
      DO 20 I=1,NUMTOT
         DO 20 J=1,3
   20 DXYZ(J,I)=0.D0
CIO      IF(ANADER) REWIND 2
      IF(ANADER) IANSCP=1                                               LF0610 / CIO
      KREP=0
      DO 130 II=1,NUMAT
         III=NCELLS*(II-1)+IOFSET
         IM1=II
         IF=NFIRST(II)
         IM=NMIDLE(II)
         IL=NLAST(II)
         NDI(2)=NAT(II)
         DO 30 I=1,3
   30    CDI(I,2)=COORD(I,II)
         DO 130 JJ=1,IM1
            JJJ=NCELLS*(JJ-1)
C  FORM DIATOMIC MATRICES
            JF=NFIRST(JJ)
            JM=NMIDLE(JJ)
            JL=NLAST(JJ)
C   GET FIRST ATOM
            NDI(1)=NAT(JJ)
            MAKEP=.TRUE.
            DO 120 IK=K1L,K1U
               DO 120 JK=K2L,K2U
                  DO 120 KL=K3L,K3U
                     JJJ=JJJ+1
                     DO 40 L=1,3
   40                CDI(L,1)=COORD(L,JJ)+TVEC(L,1)*IK+TVEC(L,2)*JK+TVEC
     1(L,3)*KL
                     IF(.NOT. MAKEP) GOTO 90
                     MAKEP=.FALSE.
                     IJ=0
                     DO 50 I=JF,JL
                        K=I*(I-1)/2+JF-1
                        DO 50 J=JF,I
                           IJ=IJ+1
                           K=K+1
                           PADI(IJ)=PA(K)
                           PBDI(IJ)=PB(K)
   50                PDI(IJ)=P(K)
C GET SECOND ATOM FIRST ATOM INTERSECTION
                     DO 80 I=IF,IL
                        L=I*(I-1)/2
                        K=L+JF-1
                        DO 60 J=JF,JL
                           IJ=IJ+1
                           K=K+1
                           PADI(IJ)=PA(K)
                           PBDI(IJ)=PB(K)
   60                   PDI(IJ)=P(K)
                        K=L+IF-1
                        DO 70 L=IF,I
                           K=K+1
                           IJ=IJ+1
                           PADI(IJ)=PA(K)
                           PBDI(IJ)=PB(K)
   70                   PDI(IJ)=P(K)
   80                CONTINUE
   90                CONTINUE
                     IF(II.EQ.JJ) GOTO  120
                     IF(ANADER)THEN
                        CALL ANALYT(PDI,PADI,PBDI,CDI,NDI,JF,JL,IF,IL
     1,                 NORBS,ENG,KREP,*9999)                            CSTP(call)
                        DO 100 K=1,3
                           DXYZ(K,III)=DXYZ(K,III)+ENG(K)
  100                   DXYZ(K,JJJ)=DXYZ(K,JJJ)-ENG(K)
                     ELSE
                        IF( .NOT. FORCE) THEN
                           CDI(1,1)=CDI(1,1)+CHNGE2
                           CDI(2,1)=CDI(2,1)+CHNGE2
                           CDI(3,1)=CDI(3,1)+CHNGE2
                           CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,IM
     1,IL,                 NORBS,AA,*9999)                               CSTP(call)
                        ENDIF
                        DO 110 K=1,3
                           IF( FORCE )THEN
                              CDI(K,2)=CDI(K,2)-CHNGE2
                              CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF
     1,IM,IL,                 NORBS,AA,*9999)                            CSTP(call)
                           ENDIF
                           CDI(K,2)=CDI(K,2)+CHNGE
                           CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,IM
     1,IL,                 NORBS,EE,*9999)                               CSTP(call)
                           CDI(K,2)=CDI(K,2)-CHNGE2
                           IF( .NOT. FORCE) CDI(K,2)=CDI(K,2)-CHNGE2
                           DERIV=(AA-EE)*46.122D0/CHNGE
                           DXYZ(K,III)=DXYZ(K,III)+DERIV
                           DXYZ(K,JJJ)=DXYZ(K,JJJ)-DERIV
  110                   CONTINUE
                     ENDIF
  120       CONTINUE
  130 CONTINUE
      IF(USEMM)THEN
C
C   NOW ADD IN MOLECULAR-MECHANICS CORRECTION TO THE H-N-C=O TORSION
C
         DEL=1.D-5
         DO 160 I=1,NNHCO
            DO 150 J=1,4
               DO 140 K=1,3
                  COORD(K,NHCO(J,I))=COORD(K,NHCO(J,I))-DEL
                  CALL DIHED(COORD,NHCO(1,I),NHCO(2,I),NHCO(3,I),NHCO(4,
     1I),ANGLE,*9999)                                                    CSTP(call)
                  REFH=HTYPE(ITYPE)*SIN(ANGLE)**2
                  COORD(K,NHCO(J,I))=COORD(K,NHCO(J,I))+DEL*2.D0
                  CALL DIHED(COORD,NHCO(1,I),NHCO(2,I),NHCO(3,I),NHCO(4,
     1I),ANGLE,*9999)                                                    CSTP(call)
                  COORD(K,NHCO(J,I))=COORD(K,NHCO(J,I))-DEL
                  HEAT=HTYPE(ITYPE)*SIN(ANGLE)**2
                  SUM=(REFH-HEAT)/DEL
                  DXYZ(K,NHCO(J,I))=DXYZ(K,NHCO(J,I))+SUM
  140          CONTINUE
  150       CONTINUE
  160    CONTINUE
      ENDIF
C
C IF ANALYTICAL DERIVATIVE CALCULATION IS REQUESTED, NOW DO THE
C ANALYTICAL PM6 NITROGEN PYRAMIDALIZATION DERIVATIVE CALCS.
C
      IF (ANADER.AND.(LPM6.OR.LPM6D)) CALL PM6DPY(DXYZ,NCELLS,IOFSET,   LF0110/LF0310 CSTP(call)
     & *9999)                                                           CSTP(call)
C DO ANALYTICAL DERIVATIVE CALCULATION FOR "-D3" DISPERSION.  BECAUSE
C OF THE NECESSITY OF CALCULATING COORDINATE NUMBERS FOR ALL ATOM PAIRS
C REPEATED CALLS FOR THE "-D3" ENERGIES FOR NUMERICAL DERIVATIVES IS 
C VERY SLOW.  THEREFORE IT IS ADVISEABLE TO ONLY DO ANALYTICAL 
C DERIVATIVES FOR "-D3" DISPERSION.
c      IF (ANADER.AND.D3METH) THEN                                       LF1211
      IF (D3METH) THEN                                                  LF1211
         CALL ANAD3(COORD,D3ENER,D3DERV,0,*9999)                        LF1211
         DO II=1,NUMAT                                                  LF1211
           DO JJ=1,3                                                    LF1211
             DXYZ(JJ,II)=DXYZ(JJ,II)+D3DERV(JJ,II)                      LF1211
           ENDDO                                                        LF1211
         ENDDO                                                          LF1211
      ENDIF                                                             LF1211
C
C ADD H...H REPULSIVE GAUSSIAN DERIVATIVE FOR PM3 AND AM1               JP1203
C                                                                         ..
      IF ( INDEX(KEYWRD,'HHON') .NE. 0 .AND.                            JP1203
     &     ((INDEX(KEYWRD,'PM3').NE.0 .AND. INDEX(KEYWRD,'PM3-D').EQ.0) LF0309
     &   .OR.(INDEX(KEYWRD,'AM1').NE.0.AND.INDEX(KEYWRD,'AM1-D').EQ.0)))LF0509
     &   THEN                                                           JP1203
C                                                                         ..
C ALL HH REPULSION                                                        ..
C                                                                         ..
        DO I = 1, NUMAT                                                   ..
           DO J = 1, I-1                                                  ..
             IF (NAT(I).EQ.1 .AND. NAT(J).EQ.1) THEN                      ..
               RHH=0.0D0                                                  .. 
               DO K=1,3                                                   ..
                 RHH = RHH + (COORD(K,I)-COORD(K,J))**2.0                 ..
               ENDDO                                                      ..
               RHH = SQRT(RHH)                                            ..
               DO L = 1, 3                                                ..
                  PFAC = 4.0*(RHH-HHR0(L))/(HHD0(L)*HHD0(L))/RHH*         .. 
     &                   HHA0(L)*EXP(-((RHH-HHR0(L))/HHD0(L))**2.0)       ..
                  DO K=1, 3                                               ..
                    DXYZ(K,I)=DXYZ(K,I)+PFAC*(COORD(K,I)-COORD(K,J))      ..
                    DXYZ(K,J)=DXYZ(K,J)-PFAC*(COORD(K,I)-COORD(K,J))      ..
                  ENDDO                                                   ..
               ENDDO                                                      ..
             ENDIF                                                        ..
          ENDDO                                                           .. 
         ENDDO                                                            ..
C                                                                         .. 
      ENDIF                                                             JP1203
C
      DO 170 I=1,6
  170 LSTOR1(I)=LSTOR2(I)
      IF (  .NOT. DEBUG) RETURN
      IF (DOPRNT) WRITE(6,'(//10X,''CARTESIAN COORDINATE DERIVATIVES'', CIO
     &  //3X,''ATOM  AT. NO.'',5X,''X'',12X,''Y'',12X,''Z'',/)')        CIO
      IF (DOPRNT) WRITE(6,'(2I6,F13.6,2F13.6)')                         CIO
     1 (I,NAT((I-1)/NCELLS+1),(DXYZ(J,I),J=1,3),I=1,NUMTOT)             CIO
CIO      IF (ANADER) REWIND 2
      IF (ANADER) IANSCP=1                                              LF0610 / CIO
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY DCART_INIT                                                  CSAV
                AA = 0.0D0                                              CSAV
            ANADER = .FALSE.                                            CSAV
             ANGLE = 0.0D0                                              CSAV
             CHNGE = 1.0D-4                                             CSAV
            CHNGE2 = 5.0D-5                                             CSAV
             DEBUG = .FALSE.                                            CSAV
               DEL = 0.0D0                                              CSAV
             DERIV = 0.0D0                                              CSAV
                EE = 0.0D0                                              CSAV
            FHHGAU = ""                                                 CSAV
             FORCE = .FALSE.                                            CSAV
              HEAT = 0.0D0                                              CSAV
              HHD0 = 0.0D0                                              CSAV
              HHR0 = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
                II = 0                                                  CSAV
               III = 0                                                  CSAV
                IJ = 0                                                  CSAV
                IL = 0                                                  CSAV
                IM = 0                                                  CSAV
               IM1 = 0                                                  CSAV
            IOFSET = 0                                                  CSAV
             ITYPE = 0                                                  CSAV
                 J = 0                                                  CSAV
                JF = 0                                                  CSAV
                JJ = 0                                                  CSAV
               JJJ = 0                                                  CSAV
                JL = 0                                                  CSAV
                JM = 0                                                  CSAV
                 K = 0                                                  CSAV
              KREP = 0                                                  CSAV
                 L = 0                                                  CSAV
             MAKEP = .FALSE.                                            CSAV
            NALPHA = 0                                                  CSAV
             NBETA = 0                                                  CSAV
            NCELLS = 0                                                  CSAV
            NCLOSE = 0                                                  CSAV
            NELECS = 0                                                  CSAV
              NHCO = 0                                                  CSAV
             NLAST = 0                                                  CSAV
            NMIDLE = 0                                                  CSAV
             NOPEN = 0                                                  CSAV
             NORBS = 0                                                  CSAV
            NUMTOT = 0                                                  CSAV
           DO 8890 I=1,171                                              CSAV
           PDI(I)=0.0D0                                                 CSAV
           PADI(I)=0.0D0                                                CSAV
           PBDI(I)=0.0D0                                                CSAV
 8890      CONTINUE                                                     CSAV
           DO 8891 I=1,3                                                CSAV
           DO 8891 J=1,2                                                CSAV
           CDI(I,J)=0.0D0                                               CSAV
           ENG(I)=0.0D0                                                 CSAV
           NDI(J)=0                                                     CSAV
 8891      CONTINUE                                                     CSAV
              PFAC = 0.0D0                                              CSAV
              REFH = 0.0D0                                              CSAV
               RHH = 0.0D0                                              CSAV
               SUM = 0.0D0                                              CSAV
             USEMM = .FALSE.                                            CSAV
      RETURN                                                            CSAV
      END
