      SUBROUTINE MOLDAT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
     2       /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     3                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     4                NCLOSE,NOPEN,NDUMY,FRACT
     5       /KEYWRD/ KEYWRD
     6       /NATORB/ NATORB(107)
     7       /CORE  / CORE(107)
     8       /BETAS / BETAS(107),BETAP(107),BETAD(107)
     9       /MOLORB/ USPD(MAXORB),PSPD(MAXORB)
     1       /VSIPS / VS(107),VP(107),VD(107)
     2       /ONELEC/ USS(107),UPP(107),UDD(107)
     3       /ATHEAT/ ATHEAT
     4       /POLVOL/ POLVOL(107)
     5       /MULTIP/ DD(107),QQ(107),AM(107),AD(107),AQ(107)
     6       /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107)
     7                ,GSD(107),GPD(107),GDD(107)
     8       /ALPHA / ALP(107)
      COMMON /GAUSS / FN1(107),FN2(107)
      COMMON /MNDO/  USSM(107), UPPM(107), UDDM(107), ZSM(107),
     1ZPM(107), ZDM(107), BETASM(107), BETAPM(107), BETADM(107),
     2ALPM(107), EISOLM(107), DDM(107), QQM(107), AMM(107),
     3ADM(107), AQM(107), GSSM(107), GSPM(107), GPPM(107),
     4GP2M(107), HSPM(107), POLVOM(107)
      COMMON /GEOM  / GEO(3,NUMATM)
      PARAMETER (MDUMY=MAXPAR**2-MPACK)
      COMMON /SCRACH/ RXYZ(MPACK), XDUMY(MDUMY)
*
*  COMMON BLOCKS FOR MINDO/3
*
      COMMON /ONELE3 /  USS3(18),UPP3(18)
     1       /ATOMI3 /  EISOL3(18),EHEAT3(18)
     2       /EXPON3 /  ZS3(18),ZP3(18)
*
*  END OF MINDO/3 COMMON BLOCKS
*
      COMMON /EXPONT/ ZS(107),ZP(107),ZD(107)
      COMMON /ATOMIC/ EISOL(107),EHEAT(107)
      DIMENSION COORD(3,NUMATM)
      CHARACTER*80 KEYWRD
      LOGICAL DEBUG, UHF,EXCI, TRIP, MINDO3, BIRAD,PARAM,AM1, OPEN
      DEBUG = (INDEX(KEYWRD,'MOLDAT').NE.0)
      MINDO3= (INDEX(KEYWRD,'MINDO').NE.0)
      UHF=(INDEX(KEYWRD,'UHF') .NE. 0)
      AM1= (INDEX(KEYWRD,'AM1')+INDEX(KEYWRD,'PARAM').NE.0)
      KHARGE=0
      I=INDEX(KEYWRD,'CHARGE')
      IF(I.NE.0) KHARGE=READA(KEYWRD,I)
      NELECS=-KHARGE
      NDORBS=0
      ATHEAT=0.D0
      EAT=0.D0
      NUMAT=0
      IF (   .NOT.   AM1   ) THEN
*
*    SWITCH IN MNDO PARAMETERS
*
         DO 10 I=1,107
            IF(.NOT.MINDO3) POLVOL(I)=POLVOM(I)
            FN1(I)=0.D0
            ZS(I)=ZSM(I)
            ZP(I)=ZPM(I)
            ZD(I)=ZDM(I)
            USS(I)=USSM(I)
            UPP(I)=UPPM(I)
            UDD(I)=UDDM(I)
            BETAS(I)=BETASM(I)
            BETAP(I)=BETAPM(I)
            BETAD(I)=BETADM(I)
            ALP(I)=ALPM(I)
            EISOL(I)=EISOLM(I)
            DD(I)=DDM(I)
            QQ(I)=QQM(I)
            AM(I)=AMM(I)
            AD(I)=ADM(I)
            AQ(I)=AQM(I)
            GSS(I)=GSSM(I)
            GPP(I)=GPPM(I)
            GSP(I)=GSPM(I)
            GP2(I)=GP2M(I)
            HSP(I)=HSPM(I)
   10    CONTINUE
      ENDIF
      IF( MINDO3 ) THEN
         DO 20 I=1,17
            IF(I.EQ.2.OR.I.EQ.10)GOTO 20
            USS(I)=USS3(I)
            UPP(I)=UPP3(I)
            EISOL(I)=EISOL3(I)
            EHEAT(I)=EHEAT3(I)
            ZS(I)=ZS3(I)
            ZP(I)=ZP3(I)
   20    CONTINUE
      ENDIF
      IF(USS(1) .GT. -1.D0) THEN
         WRITE(6,'(''  THE HAMILTONIAN REQUESTED IS NOT AVAILABLE IN''
     1,'' THIS PROGRAM'')')
         STOP
      ENDIF
      IA=1
      IB=0
      NHEAVY=0
      DO 70 II=1,NATOMS
         IF(LABELS(II).EQ.99.OR.LABELS(II).EQ.107) GOTO 70
         NUMAT=NUMAT+1
         NAT(NUMAT)=LABELS(II)
         NFIRST(NUMAT)=IA
         NI=NAT(NUMAT)
         ATHEAT=ATHEAT+EHEAT(NI)
         EAT   =EAT   +EISOL(NI)
         NELECS=NELECS+NINT(CORE(NI))
         IB=IA+NATORB(NI)-1
         NMIDLE(NUMAT)=IB
         IF(NATORB(NI).EQ.9)NDORBS=NDORBS+5
         IF(NATORB(NI).EQ.9)NMIDLE(NUMAT)=IA+3
         NLAST(NUMAT)=IB
         USPD(IA)=USS(NI)
         IF(IA.EQ.IB) GOTO 60
         K=IA+1
         K1=IA+3
         DO 30 J=K,K1
            USPD(J)=UPP(NI)
   30    CONTINUE
         NHEAVY=NHEAVY+1
   40    IF(K1.EQ.IB)GOTO 60
         K=K1+1
         DO 50 J=K,IB
   50    USPD(J)=UDD(NI)
   60    CONTINUE
   70 IA=IB+1
      ATHEAT=ATHEAT-EAT*23.061D0
      NORBS=NLAST(NUMAT)
      IF(NORBS.GT.MAXORB)THEN
         WRITE(6,'(//10X,''**** MAX. NUMBER OF ORBITALS:'',I4,/
     1            10X,''NUMBER OF ORBITALS IN SYSTEM:'',I4)')
     2MAXORB,NORBS
         STOP
      ENDIF
      NLIGHT=NUMAT-NHEAVY
      N2EL=50*NHEAVY*(NHEAVY-1)+10*NHEAVY*NLIGHT+(NLIGHT*(NLIGHT-1))/2
      IF(N2EL.GT.N2ELEC)THEN
         WRITE(6,'(//10X,''**** MAX. NUMBER OF TWO-ELECTRON INTEGRALS:''
     1,I8,/
     2            10X,''NUMBER OF TWO ELECTRON INTEGRALS IN SYSTEM:'',
     3I8)')
     4N2ELEC,N2EL
         STOP
      ENDIF
C
C   NOW TO CALCULATE THE NUMBER OF LEVELS OCCUPIED
      TRIP=(INDEX(KEYWRD,'TRIPLET').NE.0)
      EXCI=(INDEX(KEYWRD,'EXCITED').NE.0)
      BIRAD=(EXCI.OR.INDEX(KEYWRD,'BIRAD').NE.0)
      IF(INDEX(KEYWRD,'C.I.') .NE. 0 .AND. UHF ) THEN
         WRITE(6,'(//10X,''C.I. NOT ALLOWED WITH UHF '')')
         STOP
      ENDIF
C
C NOW TO WORK OUT HOW MANY ELECTRONS ARE IN EACH TYPE OF SHELL
C
      NALPHA=0
      NBETA=0
      NCLOSE=0
      NOPEN=0
      IF( UHF ) THEN
         FRACT=1.D0
         NBETA=NELECS/2
         IF( TRIP ) THEN
            IF(NBETA*2 .NE. NELECS) THEN
               WRITE(6,'(//10X,''TRIPLET SPECIFIED WITH ODD NUMBER'',
     1            '' OF ELECTRONS, CORRECT FAULT '')')
               STOP
            ELSE
               WRITE(6,'(//'' TRIPLET STATE CALCULATION'')')
               NBETA=NBETA-1
            ENDIF
         ENDIF
         NALPHA=NELECS-NBETA
         WRITE(6,'(//10X,''UHF CALCULATION, NO. OF ALPHA ELECTRONS ='',I
     13,/27X,''NO. OF BETA  ELECTRONS ='',I3)')NALPHA,NBETA
      ELSE
C
C   NOW TO DETERMINE OPEN AND CLOSED SHELLS
C
         OPEN=.FALSE.
         IELEC=0
         ILEVEL=0
         IF( TRIP .OR. EXCI .OR. BIRAD ) THEN
            IF( (NELECS/2)*2 .NE. NELECS) THEN
               WRITE(6,'(//10X,''SYSTEM SPECIFIED WITH ODD NUMBER'',
     1            '' OF ELECTRONS, CORRECT FAULT '')')
               STOP
            ENDIF
            IF(BIRAD)WRITE(6,'(//'' SYSTEM IS A BIRADICAL'')')
            IF(TRIP )WRITE(6,'(//'' TRIPLET STATE CALCULATION'')')
            IF(EXCI )WRITE(6,'(//'' EXCITED STATE CALCULATION'')')
            IELEC=2
            ILEVEL=2
         ELSEIF((NELECS/2)*2.NE.NELECS) THEN
            IELEC=1
            ILEVEL=1
         ENDIF
         IF(INDEX(KEYWRD,'QUART').NE.0) THEN
            WRITE(6,'(//'' QUARTET STATE CALCULATION'')')
            IELEC=3
            ILEVEL=3
         ENDIF
         IF(INDEX(KEYWRD,'QUINT').NE.0) THEN
            WRITE(6,'(//'' QUINTET STATE CALCULATION'')')
            IELEC=4
            ILEVEL=4
         ENDIF
         IF(INDEX(KEYWRD,'SEXT').NE.0) THEN
            WRITE(6,'(//'' SEXTET STATE CALCULATION'')')
            IELEC=5
            ILEVEL=5
         ENDIF
         I=INDEX(KEYWRD,'OPEN(')
         IF(I.NE.0)THEN
            IELEC=READA(KEYWRD,I)
            ILEVEL=READA(KEYWRD,I+7)
         ENDIF
         NCLOSE=NELECS/2
         NOPEN = NELECS-NCLOSE*2
         IF( IELEC.NE.0 )THEN
            IF((NELECS/2)*2.EQ.NELECS .NEQV.
     1                  (IELEC/2)*2.EQ.IELEC) THEN
               WRITE(6,'('' IMPOSSIBLE NUMBER OF OPEN SHELL ELECTR
     1ONS'')')
               STOP
            ENDIF
            NCLOSE=NCLOSE-IELEC/2
            NOPEN=ILEVEL
            FRACT=IELEC*1.D0/ILEVEL
            WRITE(6,'('' THERE ARE'',I3,'' DOUBLY FILLED LEVELS'')
     1')NCLOSE
         ENDIF
         IF( .NOT. PARAM)WRITE(6,'(//10X,''RHF CALCULATION, NO. OF '',
     1''DOUBLY OCCUPIED LEVELS ='',I3)')NCLOSE
         IF(NOPEN.NE.0.AND.ABS(FRACT-1.D0).LT.1.D-4)
     1WRITE(6,'(/27X,''NO. OF SINGLY OCCUPIED LEVELS ='',I3)')NOPEN
         IF(NOPEN.NE.0.AND.ABS(FRACT-1.D0).GT.1.D-4)
     1WRITE(6,'(/27X,''NO. OF LEVELS WITH OCCUPANCY'',F6.3''  ='',I3)')
     2FRACT,NOPEN
         NOPEN=NOPEN+NCLOSE
      ENDIF
C#      WRITE(6,'(''  NOPEN,NCLOSE,NALPHA,NBETA,FRACT'',4I4,F12.5)')
C#     1 NOPEN, NCLOSE, NLAPHA, NBETA, FRACT
      YY=FLOAT(KHARGE)/(NORBS+1.D-10)
      L=0
      DO 100 I=1,NUMAT
         NI=NAT(I)
         XX=1.D0
         IF(NI.GT.2) XX=0.25D0
         W=CORE(NI)*XX-YY
         IA=NFIRST(I)
         IC=NMIDLE(I)
         IB=NLAST(I)
         DO 80 J=IA,IC
            L=L+1
   80    PSPD(L)=W
         DO 90 J=IC+1,IB
            L=L+1
   90    PSPD(L)=0.D0
  100 CONTINUE
C
C   WRITE OUT THE INTERATOMIC DISTANCES
C
      CALL GMETRY(GEO,COORD)
      RMIN=100.D0
      L=0
      DO 110 I=1,NUMAT
         DO 110 J=1,I
            L=L+1
            RXYZ(L)=SQRT((COORD(1,I)-COORD(1,J))**2+
     1                     (COORD(2,I)-COORD(2,J))**2+
     2                     (COORD(3,I)-COORD(3,J))**2)
            IF(RMIN.GT.RXYZ(L) .AND. I .NE. J .AND.
     1 (NAT(I).LT.103 .OR. NAT(J).LT.103)) THEN
               IMINR=I
               JMINR=J
               RMIN=RXYZ(L)
            ENDIF
  110 CONTINUE
      IF (INDEX(KEYWRD,'PARAM')+INDEX(KEYWRD,'NOINTER') .EQ. 0) THEN
         WRITE(6,'(//10X,''  INTERATOMIC DISTANCES'')')
         CALL VECPRT(RXYZ,NUMAT)
      ENDIF
      IF(RMIN.LT.0.8D0.AND.INDEX(KEYWRD,'GEO-OK') .EQ.0) THEN
         WRITE(6,120)IMINR,JMINR,RMIN
  120    FORMAT(//,'   ATOMS',I3,' AND',I3,' ARE SEPARATED BY',F8.4,
     1' ANGSTROMS.',/'   TO CONTINUE CALCULATION SPECIFY "GEO-OK"')
         STOP
      ENDIF
      IF(.NOT. DEBUG) RETURN
      WRITE(6,130)NUMAT,NORBS,NDORBS,NATOMS
  130 FORMAT('   NUMBER OF REAL ATOMS:',I4,/
     1      ,'   NUMBER OF ORBITALS:  ',I4,/
     2      ,'   NUMBER OF D ORBITALS:',I4,/
     3      ,'   TOTAL NO. OF ATOMS:  ',I4)
      WRITE(6,140)(USPD(I),I=1,NORBS)
  140 FORMAT('   ONE-ELECTRON DIAGONAL TERMS',/,10(/,10F8.3))
      WRITE(6,150)(PSPD(I),I=1,NORBS)
  150 FORMAT('   INITIAL P FOR ALL ATOMIC ORBITALS',/,10(/,10F8.3))
      RETURN
      END
