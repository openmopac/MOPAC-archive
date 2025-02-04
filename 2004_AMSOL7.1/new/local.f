      SUBROUTINE LOCAL(C,MDIM,NOCC,EIG) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
      DIMENSION C(MDIM,MDIM), EIG(MAXORB) 
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
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
C       SUBROUTINE MAXIMISES )PSI#**4 
C       REFERENCE_ 
C       A NEW RAPID METHOD FOR ORBITAL LOCALISATION, P.G. PERKINS AND 
C       J.J.P. STEWART, J.C.S. FARADAY (II) 77, 000, (1981). 
C 
C       MODIFIED AND CORRECTED TO AVOID SIGMA-PI ORBITAL MIXING BY 
C       JUAN CARLOS PANIAGUA, UNIVERSITY OF BARCELONA, MAY 1983. 
C 
C********************************************************************** 
C     COMMON /SCRACH/ COLD(MAXORB,MAXORB),XDUMY(MAXPAR**2-MAXORB*MAXORB) 
C
C     /SCRACH/ HAS BEEN CONVERTED TO /SCRCHR/ AND /SCRCHI/ FOR AMSOL    
      COMMON /SCRCHR/ COLD(MAXORB,MAXORB),                              3GL3092
     1                XDUMY(6*MAXPAR**2+1-MAXORB*MAXORB)                GCL0393
C
      DIMENSION EIG1(MAXORB),PSI1(MAXORB),PSI2(MAXORB), 
     1          CII(MAXORB), REFEIG(MAXORB),IEL(20) 
      CHARACTER*2 ELEMNT(99) 
       SAVE
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
      DO 90 I=1,NOCC 
         DO 80 J=1,NOCC 
            IF(J.EQ.I) GOTO 80 
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
            IF(SA.LT.1.0D-14) GO TO 80 
            SUM=SUM+SA 
            CA=-AIJ/CA 
            CA=(1.0D0+SQRT((1.0D0+CA)/2.0D0))/2.0D0 
            IF((2.0D0*CA-1.0D0)*BIJ.LT.0.0D0)CA=1.0D0-CA 
            SA=SQRT(1.0D0-CA) 
            CA=SQRT(CA) 
            DO 60 K=1,NORBS 
               C(K,I)=CA*PSI1(K)+SA*PSI2(K) 
   60       C(K,J)=-SA*PSI1(K)+CA*PSI2(K) 
   70       FORMAT(2I4,2F10.5) 
   80    CONTINUE 
   90 CONTINUE 
      DO 110 I=1,NOCC 
         DO 110 J=1,NUMAT 
            IL=NFIRST(J) 
            IU=NLAST(J) 
            X=0.0 
            DO 100 K=IL,IU 
  100       X=X+C(K,I)**2 
  110 SUM1=SUM1+X*X 
      IF(SUM.GT.EPS.AND.ITER.LT.NITER) GO TO 20 
      WRITE(JOUT,120)ITER,SUM1 
  120 FORMAT(/10X,'NUMBER OF ITERATIONS =',I4/ 
     110X,'LOCALISATION VALUE =',F14.9,/) 
      WRITE(JOUT,130) 
  130 FORMAT(3X,'NUMBER OF CENTERS',20X,'( COMPOSITION OF ORBITALS)'//) 
      DO 160 I=1,NOCC 
         SUM=0.D0 
         DO 150 J=1,NOCC 
            CO=0.D0 
            DO 140 K=1,NORBS 
  140       CO=CO+COLD(K,J)*C(K,I) 
  150    SUM=SUM+CO*CO*EIG(J) 
  160 EIG1(I)=SUM 
      DO 190 I=1,NOCC 
         X=100.D0 
         DO 170 J=I,NOCC 
            IF (X.LT.EIG1(J))  GOTO  170 
            X=EIG1(J) 
            I1=J 
  170    CONTINUE 
         EIG(I)=EIG1(I1) 
         X=EIG1(I1) 
         EIG1(I1)=EIG1(I) 
         EIG1(I)=X 
         DO 180 J=1,NORBS 
            X=C(J,I1) 
            C(J,I1)=C(J,I) 
  180    C(J,I)=X 
  190 CONTINUE 
      DO 260 I=1,NOCC 
         X=0.D0 
         DO 210 K1=1,NUMAT 
            KL=NFIRST(K1) 
            KU=NLAST(K1) 
            DII=0.D0 
            DO 200 K=KL,KU 
  200       DII=DII+C(K,I)**2 
            X=X+DII*DII 
  210    PSI1(K1)=DII*100.D0 
         X=1/X 
         DO 230 II=1,NUMAT 
            SUM=0.D0 
            DO 220 J=1,NUMAT 
               IF(PSI1(J).LT.SUM) GOTO 220 
               SUM=PSI1(J) 
               K=J 
  220       CONTINUE 
            PSI1(K)=0.D0 
            CII(II)=SUM 
            IEL(II)=K 
            IF(SUM.LT.1.D0) GOTO 240 
  230    CONTINUE 
  240    CONTINUE 
         II=II-1 
         WRITE(JOUT,250)X,(ELEMNT(NAT(IEL(K))),IEL(K),CII(K),K=1,II) 
  250    FORMAT(F10.4,4(5(3X,A2,I3,F6.2),/10X)) 
  260 CONTINUE 
  270 FORMAT(//20X,20H LOCALISED ORBITALS   ,//) 
      WRITE(JOUT,270) 
      CALL MATOUT(C,EIG,NOCC,NORBS,MDIM) 
  280 FORMAT(10F12.6) 
      DO 290 I=1,NOCC 
         EIG(I)=REFEIG(I) 
         DO 290 J=1,NORBS 
  290 C(J,I)=COLD(J,I) 
      RETURN 
      END 
