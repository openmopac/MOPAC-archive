      SUBROUTINE COULRD
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
         INCLUDE 'SIZES2.i'
         INCLUDE 'SIZES.i'
         INCLUDE 'KEYS.i'
C******************************************************************************
C
C   THIS SUBROUTINE CALCULATES THE COULOMB RADII FOR THE SOLVATION MODELS
C
C   CREATED FROM EXISTING LINES IN SMX1 BY DJG 0995
C
C QKONE is the array used to alter the QK(1) parameter (Set to .1)      DJG0995
C RAC = THE COULOMB RADII
C RAC2 = THE SQUARES OF THE COULOMB RADII
C
CCC If using SM5U, SM5A, or SM5P, define the radii based on             CCC0496
CCC    the geometry. All SM5-like models use constant radii for         CCC0496
CCC    all atom types EXCEPT hydrogen. The H radii are dependent upon   CCC0395
CCC    distances (RLIO) to oxygen and nitrogen atoms.                   GDH0597
CCC    RLIO(I,J) contains the distance between any two atoms in the     CCC0395
CCC    molecule, I and J.                                               CCC0495
CCC    Same for GSM5                                                    DJG0196
C
C      ADDED ANALYTICAL DERIVATIVES DRAC(i,j) = dRAC(j)/dR(i,j)         DL0397
C******************************************************************************
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /CMCOM/  ECMCG(NUMATM)
      COMMON /BORN  / BP(NUMATM),FGB(NPACK),CCT1,ZEFF(NUMATM),
     1                QEFF2(NUMATM),DRVPOL(MPACK)
      COMMON /ONESCM/ ICONTR(100)
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM), RMAX(NUMATM)
     1               ,URLIO(3,NUMATM,NUMATM)                            DL0397
      COMMON /ARACOM/ R(NPACK),R2(NPACK),ISAVE,DONE
     1               ,RAC(NUMATM),RAC2(NUMATM),DRAC(NUMATM,NUMATM)      DL0397
     2               ,RAS(NUMATM),RALG(NUMATM)
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),
     1                HBCORC(100),QKNOT(100),QKONE(100)
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),               GDH0297
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)           GDH0297
      COMMON /QESTR/  SCOORD(3,NUMATM), ISPEC                           GDH0197
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL0397
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL0397
     2               ,DATLGR(3,NUMATM)                                  DL0397
     3               ,LGR,FLAGRD                                        DL0397
                      LOGICAL LGR,FLAGRD                                DL0397
      LOGICAL SM5PK,DONE, SM5RPK                                        GDH0597
      INCLUDE 'PARAMS.i'                                                GDH0597
      SAVE 
      DATA ZERO /0D0/
      IF (ICONTR(51).EQ.1) THEN
         ICONTR(51)=2
         PIHALF=PD2
         PIINV=1.D0/PI
         SM5PK=IRAD.EQ.4.OR.IRAD.EQ.7.OR.IRAD.EQ.8                      GDH0896
         SM5RPK=IRAD.EQ.20.OR.IRAD.EQ.21.OR.IRAD.EQ.9.OR.IRAD.EQ.10     GDH0997
      ENDIF
      DO 100 I=1,NUMAT                                                  GDH1093
         RAC(I)=RHONOT(NAT(I))                                          DL0397
         IF (LGR) CALL SCOPY (NUMAT,ZERO,0,DRAC(1,I),1)                 DL0397
         IF (SM5PK) THEN                                                DJG0995
            RTKK=0.0D0                                                  CCC0495
            IF (NAT(I).EQ.1) THEN                                       GDH0596
               DO 10 JC=1,NUMAT                                         CCC0495
                  IF (NAT(JC).EQ.7.OR.NAT(JC).EQ.8) THEN                CCC0695
                     RECCC=1.7D0                                        CCC0495
                     DELTAR=0.3D0                                       CCC0495
                     BCCC=1.0D0                                         CCC0495
                     CUTOF1=RECCC+DELTAR                                CCC0495
                     IF (RLIO(I,JC).LT.CUTOF1) THEN                     DL0397
                        EXPXX=BCCC*DELTAR/(RLIO(I,JC)-CUTOF1)           DL0397
                        EXPIJ=EXP(EXPXX)                                DL0397
                        RTKK=RTKK+EXPIJ                                 DL0397
                        IF (LGR)                                        DL0397
     .                  DRAC(JC,I)=-EXPXX*EXPIJ/(RLIO(I,JC)-CUTOF1)     DL0397
                     ENDIF                                              DL0397
                  ENDIF                                                 CCC0495
   10          CONTINUE                                                 CCC0495
            ENDIF                                                       CCC0495
            XRTKK=0.0D0                                                 CCC0695
C  note:    BYYY=1.0D0, SRTN=-RTKK, REYYY=-DELTAY ==> CUTOF1=0          DL0397
            IF (RTKK.GT.0.00000001) THEN                                CCC0795
               DELTAY=0.4D0                                             CCC0695
               XRTKK=EXP(-DELTAY/RTKK)                                  DL0397
               RAC(I)=RAC(I)+RHOONE(NAT(I))*XRTKK                       DL0397
               IF (LGR) THEN                                            DL0397
                  DXRTKK=RHOONE(NAT(I))*DELTAY*XRTKK/RTKK**2            DL0397
                  CALL DSCAL (NUMAT,DXRTKK,DRAC(1,I),1)                 DL0397
               ENDIF                                                    DL0397
            ENDIF                                                       CCC0795
         ELSE IF (SM5RPK) THEN                                          GDH0197
            IF (ITYPE(I).GT.0) THEN                                     GDH0197
               RAC(I)=RHON50(NAT(I))                                    GDH0197
            ELSE                                                        GDH0197
               RAC(I)=RHONOT(NAT(I))                                    DJG0995
            ENDIF                                                       GDH0197
         ELSE IF (ISM23.EQ.0.AND.IRAD.NE.5) THEN                        GDH0596
C           NO DERIVATIVES AVAILABLE HERE: RAC(I) DEPEND ON QEFF        DL0397
            LGR=.FALSE.                                                 DL0397
            FLAGRD=.FALSE.                                              DL0397
            IF (IEXTCM.NE.0) THEN                                       DL0397
               QEFF=ECMCG(I)                                            GDH1293
            ELSE                                                        DL0397
               QEFF=QEFF2(I)                                            GDH1293
            ENDIF                                                       DL0397
            SSX=(PIHALF-ATAN((QEFF+QKNOT(NAT(I)))/                      DJG0995
     X          QKONE(NAT(I))))*PIINV                                   DJG0995
            RAC(I)=RAC(I)+RHOONE(NAT(I))*SSX                            DL0397
         ENDIF                                                          CCC0495
         RAC2(I)=RAC(I)*RAC(I)                                          DJG0995
100   CONTINUE
      END
