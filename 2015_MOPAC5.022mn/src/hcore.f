      SUBROUTINE HCORE (COORD,H,W, WJ,WK,ENUCLR,*)                      CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      LOGICAL FLDON
      DIMENSION COORD(3,*),H(*), WJ(*), WK(*), W(*)
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /MOLORB/ USPD(MAXORB),DUMY(MAXORB)
      COMMON /KEYWRD/ KEYWRD
      COMMON /EULER / TVEC(3,3), ID
      COMMON /MULTIP/ DD(120),QQ(120),AM(120),AD(120),AQ(120)
      COMMON /CORE  / CORE(120)
      COMMON /FIELD / EFIELD(3)
      COMMON /NUMCAL/ NUMCAL
      include 'method.f'
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      COMMON /SUMDISP/ DISPTOTAL, LDISPON                               LF0312
      LOGICAL LDISPON                                                   LF0412
      COMMON /DWMAT/  DWJ(N2ELEC),DWK(N2ELEC*2)                         JZ0315
     1       /WMATRX/ WDUMMY(N2ELEC*3),KR,NBAND(NUMATM)                 JZ0315
************************************************************************
C
C   HCORE GENERATES THE ONE-ELECTRON MATRIX AND TWO ELECTRON INTEGRALS
C         FOR A GIVEN MOLECULE WHOSE GEOMETRY IS DEFINED IN CARTESIAN
C         COORDINATES.
C
C  ON INPUT  COORD   = COORDINATES OF THE MOLECULE (ANGSTROMS).
C
C  ON OUTPUT  H      = ONE-ELECTRON MATRIX.
C             W      = TWO-ELECTRON INTEGRALS.
C             ENUCLR = NUCLEAR ENERGY (EV).
************************************************************************
      CHARACTER*160 KEYWRD
      LOGICAL DEBUG
      INTEGER DCODE                                                     LF1111
      DIMENSION E1B(10),E2A(10),DI(9,9), WJD(100), WKD(100),WT(100)
      DIMENSION D3DERV(3,NUMATM)                                        LF1211
      DIMENSION FCT(100)                                                JZ0315
         SAVE                                                           GL0892
      DATA ICALCN /0/
      DATA FCT /.25D0, .50D0, .25D0, .50D0, .50D0, .25D0, .50D0, .50D0, JZ0315
     1          .50D0, .25D0, .50D0, 1.0D0, .50D0, 1.0D0, 1.0D0, .50D0, JZ0315
     2          1.0D0, 1.0D0, 1.0D0, .50D0, .25D0, 0.5D0, .25D0, .50D0, JZ0315
     3          .50D0, .25D0, .50D0, .50D0, .50D0, .25D0, .50D0, 1.0D0, JZ0315
     4          .50D0, 1.0D0, 1.0D0, .50D0, 1.0D0, 1.0D0, 1.0D0, .50D0, JZ0315
     5          .50D0, 1.0D0, .50D0, 1.0D0, 1.0D0, .50D0, 1.0D0, 1.0D0, JZ0315
     6          1.0D0, .50D0, .25D0, .50D0, .25D0, .50D0, .50D0, .25D0, JZ0315
     7          .50D0, .50D0, .50D0, .25D0, .50D0, 1.0D0, .50D0, 1.0D0, JZ0315
     8          1.0D0, .50D0, 1.0D0, 1.0D0, 1.0D0, 1.0D0, .50D0, 1.0D0, JZ0315
     9          .50D0, 1.0D0, 1.0D0, .50D0, 1.0D0, 1.0D0, 1.0D0, 1.0D0, JZ0315
     9          .50D0, 1.0D0, .50D0, 1.0D0, 1.0D0, .50D0, 1.0D0, 1.0D0, JZ0315
     9          1.0D0, 1.0D0, .25D0, .50D0, .25D0, .50D0, .50D0, .25D0, JZ0315
     9          1.0D0, 1.0D0, 1.0D0, .25D0/                             JZ0315
      
      IF (ICALCN .NE. NUMCAL) THEN
         IONE=1
         CUTOFF=1.D10
         IF(ID.NE.0)CUTOFF=60.D0
         IF(ID.NE.0)IONE=0
         ICALCN = NUMCAL
         DEBUG=(INDEX(KEYWRD,'HCORE') .NE. 0)
      ENDIF
      FLDON = .FALSE.
      IF ((EFIELD(1).NE.0.0D00).OR.(EFIELD(2).NE.0.0D00).OR.
     1    (EFIELD(3).NE.0.0D00)) THEN
         FLDCON = 51.4257D00
         FLDON = .TRUE.
      ENDIF
      DO 10 I=1,(NORBS*(NORBS+1))/2
   10 H(I)=0
      ENUCLR=0.D0
      EPYR=0.D0                                                         LF0110
      KR=1
      GTERM=0.D0                                                        LF0110
      DISPTOTAL=0.0D0                                                   LF0312
      NROW = 0                                                          JZ0315
      IPQRS = 1
c#      write(6,*) "Reset DISPTOTAL back to zero."
      DO 110 I=1,NUMAT
         IA=NFIRST(I)
         IB=NLAST(I)
         IC=NMIDLE(I)
         NI=NAT(I)
         IF ((LPM6.OR.LPM6D).AND.(NI.EQ.7)) THEN                        LF0110/LF0310
            CALL PM6PYR(I,EPM6PR,*9999)                                 LF0110 CSTP(call)
         ELSE                                                           LF0110
            EPM6PR=0.D0                                                 LF0110
         ENDIF                                                          LF0110
         EPYR=EPM6PR+EPYR                                               LF0110
C
C FIRST WE FILL THE DIAGONALS, AND OFF-DIAGONALS ON THE SAME ATOM
C
         DO 30 I1=IA,IB
            I2=I1*(I1-1)/2+IA-1
            DO 20 J1=IA,I1
               I2=I2+1
               H(I2)=0.D0
               IF (FLDON) THEN
                  IO1 = I1 - IA
                  JO1 = J1 - IA
                  IF ((JO1.EQ.0).AND.(IO1.EQ.1)) THEN
                     HTERME = -0.529177D00*DD(NI)*EFIELD(1)*FLDCON
                     H(I2) = HTERME
                  ENDIF
                  IF ((JO1.EQ.0).AND.(IO1.EQ.2)) THEN
                     HTERME = -0.529177D00*DD(NI)*EFIELD(2)*FLDCON
                     H(I2) = HTERME
                  ENDIF
                  IF ((JO1.EQ.0).AND.(IO1.EQ.3)) THEN
                     HTERME = -0.529177D00*DD(NI)*EFIELD(3)*FLDCON
                     H(I2) = HTERME
                  ENDIF
               ENDIF
   20       CONTINUE
            H(I2) = USPD(I1)
            IF (FLDON) THEN
               FNUC = -(EFIELD(1)*COORD(1,I) + EFIELD(2)*COORD(2,I) +
     1              EFIELD(3)*COORD(3,I))*FLDCON
               H(I2) = H(I2) + FNUC
            ENDIF
   30    CONTINUE
C
C   FILL THE ATOM-OTHER ATOM ONE-ELECTRON MATRIX<PSI(LAMBDA)|PSI(SIGMA)>
C
         IM1=I-IONE
         IF(IM1.GT.0) THEN                                              JZ0315
            NROW=NROW+NBAND(IM1)
            NCOL=NBAND(I)
            NBAND2=0
         ENDIF                                                          JZ0315
         DO 100 J=1,IM1
            HALF=1.D0
            IF(I.EQ.J)HALF=0.5D0
            JA=NFIRST(J)
            JB=NLAST(J)
            JC=NMIDLE(J)
            NJ=NAT(J)
            CALL H1ELEC(NI,NJ,COORD(1,I),COORD(1,J),DI,*9999)            CSTP(call)
            I2=0
            DO 40 I1=IA,IB
               II=I1*(I1-1)/2+JA-1
               I2=I2+1
               J2=0
               JJ=MIN(I1,JB)
               DO 40 J1=JA,JJ
                  II=II+1
                  J2=J2+1
   40       H(II)=H(II)+DI(I2,J2)
C
C   CALCULATE THE TWO-ELECTRON INTEGRALS, W; THE ELECTRON NUCLEAR TERMS
C   E1B AND E2A; AND THE NUCLEAR-NUCLEAR TERM ENUC.
C
            KRO = KR                                                    JZ0315
            NBAND1=NBAND2+1                                             JZ0315
            NBAND2=NBAND2+NBAND(J)                                      JZ0315
            IF(ID.EQ.0) THEN
               LDISPON=.TRUE.                                           LF0412
               CALL ROTATE(NI,NJ,COORD(1,I),COORD(1,J),
     1                     W(KR), KR,E1B,E2A,ENUC,CUTOFF,GTNEW,*9999)    CSTP(call)
               IF(KR.LE.KRO) GO TO 55
               LDISPON=.FALSE.                                          LF0412
               GTERM=GTERM+GTNEW
               JJ = 0
               DO K = KRO,KR-1
               JJ = JJ + 1
                 WT(JJ) = W(K)*FCT(JJ)
               ENDDO
               CALL WCANON (WT,DWJ(IPQRS),NROW,NCOL,NBAND1,NBAND2)
C              DWJ(1:N2ELEC)=W(1:N2ELEC)
            ELSE
               KRO=KR
               CALL SOLROT(NI,NJ,COORD(1,I),COORD(1,J),
     1                WJD, WKD,KR,E1B,E2A,ENUC,CUTOFF,*9999)             CSTP(call)
               JJ=0
               DO 50 II=KRO,KR-1
                  JJ=JJ+1
                  WJ(II)=WJD(JJ)
   50          WK(II)=WKD(JJ)
            ENDIF
   55       ENUCLR = ENUCLR + ENUC + EPM6PR                             LF0110
C
C   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM I.
C
            I2=0
            DO 60 I1=IA,IC
               II=I1*(I1-1)/2+IA-1
               DO 60 J1=IA,I1
                  II=II+1
                  I2=I2+1
   60       H(II)=H(II)+E1B(I2)*HALF
            DO  70 I1=IC+1,IB
               II=(I1*(I1+1))/2
   70       H(II)=H(II)+E1B(1)*HALF
C
C   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM J.
C
            I2=0
            DO 80 I1=JA,JC
               II=I1*(I1-1)/2+JA-1
               DO 80 J1=JA,I1
                  II=II+1
                  I2=I2+1
   80       H(II)=H(II)+E2A(I2)*HALF
            DO 90 I1=JC+1,JB
               II=(I1*(I1+1))/2
   90       H(II)=H(II)+E2A(1)*HALF
  100    CONTINUE
         IPQRS=IPQRS+NROW*NCOL                                          JZ0315
  110 CONTINUE
      
C     LF0312: The next output is for debugging by getting the total dispersion
C     energy to be displayed after going over all atom pairs with the 
C     "xxxx-D" type dispersion methods.  Comment out if you do not care to
C     know the contribution to the core-core energy from the dispersion energy.
c#      IF (DSPMET) WRITE(6,'(A,F17.12)')                                 LF0312
c#     &  " '-D' dispersion energy total (kcal/mol): ",DISPTOTAL          LF0312

C LF1111: IF USING GRIMME'S "-D3" DISPERSION FOR SEMIEMPIRICAL METHODS 
C   (e.g. MNDO-D3, AM1-D3) THEN DO CALCULATION OVER ALL ATOM PAIRS IN 
C   SEPARATE SUBROUTINE INSTEAD OF WITHIN THIS SUBROUTINE. 
C   NOTE THAT ENUCLR HERE IS ENERGY IN eV, WHEREAS D3DISP RETURNS THE
C   DISPERSION ENERGY IN kcal/mol.
      IF (D3METH) THEN                                                  LF1111
         D3ENERGY=0.0D0                                                 LF1111
         DCODE=0                                                        LF1211
         IF (INDEX(KEYWRD,'DEBUG').NE.0) DCODE=10                       LF1111
         CALL ANAD3(COORD, D3ENERGY, D3DERV, DCODE, *9999)              LF1211
         ENUCLR=ENUCLR+(D3ENERGY/23.061D0)                              LF1111
C#         write(6,*) "Adding D3 energy: ",d3energy/23.061d0," kcal/mol"  LF1211
      ENDIF                                                             LF1111
      IF (ID.EQ.0) THEN
C 
C        UNPACK (-0.5)*K  FOR FURTHER USE IN FOCK2, FOCK ETC 
C        --------------------------------------------------- 
         IPQRS=1
         KPQRS=0
         CONST=-0.5D0
         DO 150 II=2,NUMAT
         IA=NFIRST(II)
         IC=NLAST(II)
         DO 150 I=IA,IC
         DO 150 J=IA,I
         DO 150 JJ=1,II-1
         JJLEN=MAX(0,NLAST(JJ)-NFIRST(JJ)+1)
         DO 140 K=1,JJLEN
         LK=(K-1)*JJLEN+1
         KL=K
         DO 130 L=1,K-1
         DWK(KPQRS+KL)=DWJ(IPQRS)*CONST
         DWK(KPQRS+LK)=DWJ(IPQRS)*CONST
         IPQRS=IPQRS+1
         LK=LK+1
  130    KL=KL+JJLEN
         DWK(KPQRS+LK)=-DWJ(IPQRS)
  140    IPQRS=IPQRS+1
  150    KPQRS=KPQRS+JJLEN*JJLEN
      ENDIF

      IF( .NOT. DEBUG) RETURN
      IF (DOPRNT) WRITE(6,'(//10X,''ONE-ELECTRON MATRIX FROM HCORE'')') CIO
      CALL VECPRT(H,NORBS,*9999)                                        CSTP(call)
      J=MIN(400,KR)
c      J=KR                                                                     debug LF
      IF (DOPRNT) THEN                                                  CIO
        IF(ID.EQ.0) THEN
          WRITE(6,'(//10X,''TWO-ELECTRON MATRIX IN HCORE''/)')
          WRITE(6,120)(W(I),I=1,J)
        ELSE
          WRITE(6,'(//10X,''TWO-ELECTRON J MATRIX IN HCORE''/)')
          WRITE(6,120)(WJ(I),I=1,J)
          WRITE(6,'(//10X,''TWO-ELECTRON K MATRIX IN HCORE''/)')
          WRITE(6,120)(WK(I),I=1,J)
        ENDIF
      ENDIF                                                             CIO
  120 FORMAT(10F8.4)
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY HCORE_INIT                                                  CSAV
            CUTOFF = 0.0D0                                              CSAV
             DEBUG = .FALSE.                                            CSAV
                DI = 0.0D0                                              CSAV
               E1B = 0.0D0                                              CSAV
               E2A = 0.0D0                                              CSAV
              ENUC = 0.0D0                                              CSAV
            EPM6PR = 0.0D0                                              CSAV
              EPYR = 0.0D0                                              CSAV
            FLDCON = 0.0D0                                              CSAV
             FLDON = .FALSE.                                            CSAV
              FNUC = 0.0D0                                              CSAV
             GTERM = 0.0D0                                              CSAV
             GTNEW = 0.0D0                                              CSAV
              HALF = 0.0D0                                              CSAV
            HTERME = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
                I1 = 0                                                  CSAV
                I2 = 0                                                  CSAV
                IA = 0                                                  CSAV
                IB = 0                                                  CSAV
                IC = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
                ID = 0                                                  CSAV
                II = 0                                                  CSAV
               IM1 = 0                                                  CSAV
               IO1 = 0                                                  CSAV
              IONE = 0                                                  CSAV
                 J = 0                                                  CSAV
                J1 = 0                                                  CSAV
                J2 = 0                                                  CSAV
                JA = 0                                                  CSAV
                JB = 0                                                  CSAV
                JC = 0                                                  CSAV
                JJ = 0                                                  CSAV
               JO1 = 0                                                  CSAV
                KR = 0                                                  CSAV
               KRO = 0                                                  CSAV
                NI = 0                                                  CSAV
                NJ = 0                                                  CSAV
               WJD = 0.0D0                                              CSAV
               WKD = 0.0D0                                              CSAV
               DWJ = 0.0D0
      RETURN                                                            CSAV
      END
