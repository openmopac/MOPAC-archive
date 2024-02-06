      SUBROUTINE DCART (COORD,DXYZ)                                     DL0496
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH0196
       PARAMETER (MSIZE=NUMATM*(NUMATM-1)/2)                            DJG0496
C*********************************************************************** 
C 
C    DCART CALCULATES THE DERIVATIVES OF THE ENERGY WITH RESPECT TO THE 
C          CARTESIAN COORDINATES FOR RHF CLOSED SHELL OR UHF FUNCTION. 
C    THE INTEGRAL DERIVATIVES ARE COMPUTED BY 1 OR 2-POINTS FINITE 
C          DIFFERENCE WITH STEP SIZE CHNGE2 . 
C 
C    INPUT 
C        COORD(3,*) : CARTESIAN COORDINATES (ANGSTROMS). 
C        DXYZ       : NOT DEFINED. 
C    OUTPUT 
C        DXYZ(3,*)  : CARTESIAN DERIVATIVES. 
C        RG(3,*)    : 2-BODY CARTESIAN DERIVATIVES.
C    REVISED BY DL, MAY 96  (STEPSIZE AND /HBODY2/).
C 
C*********************************************************************** 
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK) 
      COMMON /EULER / TVEC(3,3), ID 
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U, K1L,K2L,K3L,K1U,K2U,K3U 
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      COMMON /HBODY2/ RG(3,MSIZE),BJACOB(MAXPAR**2),CXYZ(3,NUMATM)      DJG0496
      DIMENSION COORD(3,*), DXYZ(3,*) 
      DIMENSION PDI(171),PADI(171),PBDI(171), 
     1CDI(3,2),NDI(2),LSTOR1(6), LSTOR2(6) 
      LOGICAL DEBUG, MAKEP, CUC, FORWRD                                 DJG0495
      EQUIVALENCE (LSTOR1(1),L1L), (LSTOR2(1), K1L) 
       SAVE
C 
      IF (ICONTR(15).EQ.1) THEN                                         GDH0195
         ICONTR(15)=2                                                   GDH0195
         DEBUG = IDCART.NE.0                                            DJG0995
         FORWRD = IFORWR.NE.0                                           DJG0995
C        STEPSIZE FOR FINITE DIFFERENCE                                 DL0496
         CHNGE=1.0D-4                                                   DL0496
         CHNGE2=CHNGE*0.5D0                                             DL0496
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
      IIIJJJ=0                                                          DJG0496
      DO 120 II=1,NUMAT 
         III=NCELLS*(II-1)+IOFSET 
         IM1=II 
         IF=NFIRST(II) 
         IM=NMIDLE(II) 
         IL=NLAST(II) 
         NDI(2)=NAT(II) 
         DO 30 I=1,3 
   30    CDI(I,2)=COORD(I,II) 
         DO 120 JJ=1,IM1 
            JJJ=NCELLS*(JJ-1) 
C  FORM DIATOMIC MATRICES 
            JF=NFIRST(JJ) 
            JM=NMIDLE(JJ) 
            JL=NLAST(JJ) 
C   GET FIRST ATOM 
            NDI(1)=NAT(JJ) 
            MAKEP=.TRUE. 
            DO 110 IK=K1L,K1U 
               DO 110 JK=K2L,K2U 
                  DO 110 KL=K3L,K3U 
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
                     IF(II.EQ.JJ) GOTO  110 
                     IF(FORWRD) THEN                                    DJG0495
                        CDI(1,1)=CDI(1,1)+CHNGE2 
                        CDI(2,1)=CDI(2,1)+CHNGE2 
                        CDI(3,1)=CDI(3,1)+CHNGE2 
                        CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,IM,IL 
     1                          ,NORBS,AA,CUC) 
                     ENDIF 
                     IIIJJJ=IIIJJJ+1                                    DJG0496
                     DO 100 K=1,3 
                     IF(.NOT.FORWRD) THEN                               DJG0495
                        CDI(K,2)=CDI(K,2)-CHNGE2 
                        CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,IM,IL 
     1                          ,NORBS,AA,CUC) 
                        ENDIF 
                        CDI(K,2)=CDI(K,2)+CHNGE 
                        CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,IM,IL 
     1                          ,NORBS,EE,CUC) 
                        CDI(K,2)=CDI(K,2)-CHNGE2 
                        IF(FORWRD) CDI(K,2)=CDI(K,2)-CHNGE2             DJG0495
                        DERIV=(AA-EE)*23.061D0/CHNGE 
                        DXYZ(K,III)=DXYZ(K,III)-DERIV 
                        DXYZ(K,JJJ)=DXYZ(K,JJJ)+DERIV 
                        RG(K,IIIJJJ)=-DERIV                             DJG0496
  100                CONTINUE 
  110       CONTINUE 
  120 CONTINUE 
      DO 130 I=1,6 
  130 LSTOR1(I)=LSTOR2(I) 
      IF (  .NOT. DEBUG) RETURN 
      WRITE(JOUT,'(//10X,''CARTESIAN COORDINATE DERIVATIVES'',//3X, 
     1''ATOM  AT. NO.'',5X,''X'',12X,''Y'',12X,''Z'',/)') 
      WRITE(JOUT,'(2I6,F13.6,2F13.6)') 
     1 (I,NAT((I-1)/NCELLS+1),(DXYZ(J,I),J=1,3),I=1,NUMTOT) 
      RETURN 
      END 
