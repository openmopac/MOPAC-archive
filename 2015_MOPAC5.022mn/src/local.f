      SUBROUTINE LOCAL(C,MDIM,NOCC,EIG,*)                               CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION C(MDIM,MDIM), EIG(MAXORB)
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
C**********************************************************************
C
C   LOCALISATION SUBROUTINE
C ON INPUT
C        C = EIGENVECTORS IN AN MDIM*MDIM MATRIX
C        NOCC = NUMBER OF FILLED LEVELS
C        NORBS = NUMBER OF ORBITALS
C        NUMAT = NUMBER OF ATOMS
C        NLAST   = INTEGER ARRAY OF ATOM ORBITAL COUNTERS
C        NFIRST   = INTEGER ARRAY OF ATOM ORBITAL COUNTERS
C
C       SUBROUTINE MAXIMIZES (PSI)**4
C       REFERENCE_
C       A NEW RAPID METHOD FOR ORBITAL LOCALISATION, P.G. PERKINS AND
C       J.J.P. STEWART, J.C.S. FARADAY (II) 77, 000, (1981).
C
C       MODIFIED AND CORRECTED TO AVOID SIGMA-PI ORBITAL MIXING BY
C       JUAN CARLOS PANIAGUA, UNIVERSITY OF BARCELONA, MAY 1983.
C
C**********************************************************************
      COMMON /SCRACH/ COLD(MAXORB,MAXORB),XDUMY(MAXPAR**2-MAXORB*MAXORB)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION EIG1(MAXORB),PSI1(MAXORB),PSI2(MAXORB),
     1          CII(MAXORB), REFEIG(MAXORB),IEL(20)
      CHARACTER*2 ELEMNT(99)
         SAVE                                                           GL0892
      DATA ELEMNT/'H','HE',
     1 'LI','BE','B','C','N','O','F','NE',
     2 'NA','MG','AL','SI','P','S','CL','AR',
     3 'K','CA','SC','TI','V','CR','MN','FE','CO','NI','CU',
     4 'ZN','GA','GE','AS','SE','BR','KR',
     5 'RB','SR','Y','ZR','NB','MO','TC','RU','RH','PD','AG',
     6 'CD','IN','SN','SB','TE','I','XE',
     7 'CS','BA','LA','CE','PR','ND','PM','SM','EU','GD','TB','DY',
     8 'HO','ER','TM','YB','LU','HF','TA','W','RE','OS','IR','PT',
     9 'AU','HG','TL','PB','BI','PO','AT','RN',
     1 'FR','RA','AC','TH','PA','U','NP','PU','AM','CM','BK','CF','XX'/
      NITER=100
      EPS=1.0D-7
      DO 10 I=1,NORBS
         REFEIG(I)=EIG(I)
         DO 10 J=1,NORBS
   10 COLD(I,J)=C(I,J)
      ITER=0
   20 CONTINUE
      SUM=0.D0
      ITER=ITER+1
      DO 80 I=1,NOCC
         DO 70 J=1,NOCC
            IF(J.EQ.I) GOTO 70
            XIJJJ=0.0D0
            XJIII=0.0D0
            XIIII=0.0D0
            XJJJJ=0.0D0
            XIJIJ=0.0D0
            XIIJJ=0.0D0
            DO 30 K=1,NORBS
               PSI1(K)=C(K,I)
   30       PSI2(K)=C(K,J)
C NOW FOLLOWS THE RATE-DETERMINING STEP FOR THE CALCULATION
            DO 50 K1=1,NUMAT
               KL=NFIRST(K1)
               KU=NLAST(K1)
               DIJ=0.D0
               DII=0.D0
               DJJ=0.D0
               DO 40 K=KL,KU
                  DIJ=DIJ+PSI1(K)*PSI2(K)
                  DII=DII+PSI1(K)*PSI1(K)
                  DJJ=DJJ+PSI2(K)*PSI2(K)
   40          CONTINUE
               XIJJJ=XIJJJ+DIJ*DJJ
               XJIII=XJIII+DIJ*DII
               XIIII=XIIII+DII*DII
               XJJJJ=XJJJJ+DJJ*DJJ
               XIJIJ=XIJIJ+DIJ*DIJ
               XIIJJ=XIIJJ+DII*DJJ
   50       CONTINUE
            AIJ=XIJIJ-(XIIII+XJJJJ-2.0D0*XIIJJ)/4.0D0
            BIJ=XJIII-XIJJJ
            CA=SQRT(AIJ*AIJ+BIJ*BIJ)
            SA=AIJ+CA
            IF(SA.LT.1.0D-14) GO TO 70
            SUM=SUM+SA
            CA=-AIJ/CA
            CA=(1.0D0+SQRT((1.0D0+CA)/2.0D0))/2.0D0
            IF((2.0D0*CA-1.0D0)*BIJ.LT.0.0D0)CA=1.0D0-CA
            SA=SQRT(1.0D0-CA)
            CA=SQRT(CA)
            DO 60 K=1,NORBS
               C(K,I)=CA*PSI1(K)+SA*PSI2(K)
   60       C(K,J)=-SA*PSI1(K)+CA*PSI2(K)
   70    CONTINUE
   80 CONTINUE
      SUM1=0.D0
      DO 100 I=1,NOCC
         DO 100 J=1,NUMAT
            IL=NFIRST(J)
            IU=NLAST(J)
            X=0.0
            DO 90 K=IL,IU
   90       X=X+C(K,I)**2
  100 SUM1=SUM1+X*X
      IF(SUM.GT.EPS.AND.ITER.LT.NITER) GO TO 20
      IF (DOPRNT) WRITE(6,110)ITER,SUM1                                 CIO
  110 FORMAT(/10X,'NUMBER OF ITERATIONS =',I4/
     110X,'LOCALISATION VALUE =',F14.9,/)
      IF (DOPRNT) WRITE(6,120)                                          CIO
  120 FORMAT(3X,'NUMBER OF CENTERS',14X,'(COMPOSITION OF ORBITALS)'//)
      DO 150 I=1,NOCC
         SUM=0.D0
         DO 140 J=1,NOCC
            CO=0.D0
            DO 130 K=1,NORBS
  130       CO=CO+COLD(K,J)*C(K,I)
  140    SUM=SUM+CO*CO*EIG(J)
  150 EIG1(I)=SUM
      DO 180 I=1,NOCC
         X=100.D0
         DO 160 J=I,NOCC
            IF (X.LT.EIG1(J))  GOTO  160
            X=EIG1(J)
            I1=J
  160    CONTINUE
         EIG(I)=EIG1(I1)
         X=EIG1(I1)
         EIG1(I1)=EIG1(I)
         EIG1(I)=X
         DO 170 J=1,NORBS
            X=C(J,I1)
            C(J,I1)=C(J,I)
  170    C(J,I)=X
  180 CONTINUE
      DO 250 I=1,NOCC
         X=0.D0
         DO 200 K1=1,NUMAT
            KL=NFIRST(K1)
            KU=NLAST(K1)
            DII=0.D0
            DO 190 K=KL,KU
  190       DII=DII+C(K,I)**2
            X=X+DII*DII
  200    PSI1(K1)=DII*100.D0
         X=1/X
         DO 220 II=1,NUMAT
            SUM=0.D0
            DO 210 J=1,NUMAT
               IF(PSI1(J).LT.SUM) GOTO 210
               SUM=PSI1(J)
               K=J
  210       CONTINUE
            PSI1(K)=0.D0
            CII(II)=SUM
            IEL(II)=K
            IF(SUM.LT.1.D0) GOTO 230
  220    CONTINUE
  230    CONTINUE
         II=II-1
         IF (DOPRNT)                                                    CIO
     &     WRITE(6,240)X,(ELEMNT(NAT(IEL(K))),IEL(K),CII(K),K=1,II)     CIO
  240    FORMAT(F10.4,4(5(3X,A2,I3,F6.2),/10X))
  250 CONTINUE
  260 FORMAT(//20X,20H LOCALIZED ORBITALS   ,//)
      IF (DOPRNT) WRITE(6,260)                                          CIO
      CALL MATOUT(C,EIG,NOCC,NORBS,MDIM,*9999)                          CSTP(call)
  270 FORMAT(10F12.6)
      DO 280 I=1,NOCC
         EIG(I)=REFEIG(I)
         DO 280 J=1,NORBS
  280 C(J,I)=COLD(J,I)
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY LOCAL_INIT                                                  CSAV
               AIJ = 0.0D0                                              CSAV
               BIJ = 0.0D0                                              CSAV
                CA = 0.0D0                                              CSAV
               CII = 0.0D0                                              CSAV
                CO = 0.0D0                                              CSAV
                 D = 0.0D0                                              CSAV
                D0 = 0.0D0                                              CSAV
               DII = 0.0D0                                              CSAV
               DIJ = 0.0D0                                              CSAV
               DJJ = 0.0D0                                              CSAV
              EIG1 = 0.0D0                                              CSAV
               EPS = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
                I1 = 0                                                  CSAV
               IEL = 0                                                  CSAV
                II = 0                                                  CSAV
                IL = 0                                                  CSAV
              ITER = 0                                                  CSAV
                IU = 0                                                  CSAV
                 J = 0                                                  CSAV
                 K = 0                                                  CSAV
                K1 = 0                                                  CSAV
                KL = 0                                                  CSAV
                KU = 0                                                  CSAV
             NITER = 0                                                  CSAV
              PSI1 = 0.0D0                                              CSAV
              PSI2 = 0.0D0                                              CSAV
            REFEIG = 0.0D0                                              CSAV
                SA = 0.0D0                                              CSAV
               SUM = 0.0D0                                              CSAV
              SUM1 = 0.0D0                                              CSAV
                 X = 0.0D0                                              CSAV
             XIIII = 0.0D0                                              CSAV
             XIIJJ = 0.0D0                                              CSAV
             XIJIJ = 0.0D0                                              CSAV
             XIJJJ = 0.0D0                                              CSAV
             XJIII = 0.0D0                                              CSAV
             XJJJJ = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
