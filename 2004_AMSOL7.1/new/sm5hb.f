      SUBROUTINE SM5HB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'FFILES.i'                                                GDH1095
C******************************************************************************
C
C   THIS SUBROUTINE CALCULATES SURFACE TENSIONS WHICH ARE LATER
C   MULTIPLIED TIMES SASA TO GET THE CDS TERM.
C
C   CREATED BY DJG 0995 FROM EXISTING LINES IN BORNPL AND BRNPL2
C   REVISITED, ADDED CARTESIAN DERIVATIVES FOR THE SUM OF HBs.         DL 0397
C******************************************************************************
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS, NALPHA, NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /HEXSTM/ HRFN4(100),CRFNH4,CNRFN4,CORFN5,                  GDH0797
     X                CRFNH5,OORFN5,ONRFN4,SSRFN5,OPRFN5,               GDH0797
     1                FCRFN,CLCRFN,BRCRFN,CNSTC2,TNCSC3,TNCSC2,         GDH0797
     2                HOHST1,HNNST1, SPRFN5,OSIBD5                      PDW0901
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /SPARCM/ SIGMA0(100),RKCDS(100),RHONOT(100),RHOONE(100),
     1                HBCORC(100),QKNOT(100),QKONE(100)
      COMMON /HBORDS/ HBORD(NUMATM), HB(NUMATM)
      COMMON /HBONDA/ HBONDO(100),CBONDO,COBND5,CNBNDO,CBOND5,          GDH1196
     .                OOBNDO,ONBNDO,SSBNDO,OPBND5,CLCBND,BRCBND,        GDH1196
     .                FCBNDO,CNBOC2,TNCBC3,TNCBC2,HOHBND,HNNBND,        GDH1296
     .                SPBNDO,OSIBND                                     PDW0601
      COMMON /ONESCM/ ICONTR(100)
      COMMON /SURF  / SURFCT,SRFACT(NUMATM),ATAR(NUMATM),
     1                HEXLGS,ATLGAR,CSAREA(100),ITYPE(NUMATM)
      COMMON /SCRCHR/ BUF(NUMATM),BUF2(NUMATM,NUMATM)                   DL0397
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM),RMAX(NUMATM)
     1               ,URLIO(3,NUMATM,NUMATM)                            DL0397
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL0397
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL0397
     2               ,DATLGR(3,NUMATM)                                  DL0397
     3               ,LGR,FLAGRD                                        DL0397
                      LOGICAL LGR,FLAGRD                                DL0397
      DATA ZERO /0D0/                                                   DL0397
      IF (ICONTR(56).EQ.1) THEN
         ICONTR(56)=2
      ENDIF
      CALL SCOPY (100,ZERO,0,HBONDO,1)
      CBONDO=0.0D0                                                      CCC0695
      CBOND5=0.0D0                                                      CCC0495
      COBND5=0.0D0                                                      CCC0495
      CNBNDO=0.0D0                                                      CCC0495
      OOBNDO=0.0D0                                                      CCC0495
      ONBNDO=0.0D0                                                      CCC0595
      SSBNDO=0.0D0                                                      CCC0695
      OPBNDO=0.0D0                                                      GDH1096
      IF (LGR) CALL SCOPY (3*NUMAT,ZERO,0,DCDS,1)
      DO 950 IAT=1,NUMAT
         HB(IAT)=0D0                                                    CCC0495
         IF (NAT(IAT).EQ.1) THEN                                        CCC0495
CCC These are the NEW H-X  CDS term, based on a Tkk                     CCC0495
CCC NOTE: Hydrogen only has this extra surface tension when bonded to   CCC0495
CCC   Carbon, Oxygen, and Nitrogen.                                     CCC0495
            DELTAR=0.3D0                                                CCC0495
            DO 10 J=1,NUMAT                                             CCC0495
               IF (NAT(J).EQ.6) THEN                                    CCC0495
                  RECCC=1.85D0                                          CCC0495
                  CUTOF1=RECCC+DELTAR                                   CCC0495
                  IF (RLIO(IAT,J).LT.CUTOF1) CALL HBADD1 (IAT,J,1,      DL0397
     .            CUTOF1,DELTAR,HRFN4(NAT(J)),ATAR(IAT),HB(IAT),
     .            HBONDO(NAT(J)))
               ELSE IF (NAT(J).EQ.7) THEN                               CCC0495
                  RECCC=1.7D0                                           CCC0495
                  CUTOF1=RECCC+DELTAR                                   CCC0495
                  IF (RLIO(IAT,J).LT.CUTOF1) CALL HBADD1 (IAT,J,1,      DL0397
     .            CUTOF1,DELTAR,HRFN4(NAT(J)),ATAR(IAT),HB(IAT),
     .            HBONDO(NAT(J)))
               ELSE IF (NAT(J).EQ.8) THEN                               CCC0495
                  RECCC=1.7D0                                           CCC0495
                  CUTOF1=RECCC+DELTAR                                   CCC0495
                  IF (RLIO(IAT,J).LT.CUTOF1) CALL HBADD1 (IAT,J,1,      DL0397
     .            CUTOF1,DELTAR,HRFN4(NAT(J)),ATAR(IAT),HB(IAT),
     .            HBONDO(NAT(J)))
               ELSE IF (NAT(J).EQ.16) THEN                              CCC0695
                  RECCC=2.14D0                                          CCC0695
                  CUTOF1=RECCC+DELTAR                                   CCC0695
                  IF (RLIO(IAT,J).LT.CUTOF1) CALL HBADD1 (IAT,J,1,      DL0397
     .            CUTOF1,DELTAR,HRFN4(NAT(J)),ATAR(IAT),HB(IAT),
     .            HBONDO(NAT(J)))
               ENDIF                                                    CCC0495
   10       CONTINUE                                                    CCC0495
         ELSE IF (NAT(IAT).EQ.8) THEN                                   CCC0495
            RTKK=0.0D0                                                  CCC0495
            KCCAN=0                                                     CCC0795
            IF (LGR) CALL SCOPY (NUMAT,ZERO,0,BUF,1)                    DL0397
            DO 20 J=1,NUMAT                                             CCC0395
              IF (NAT(J).EQ.6) THEN                                     CCC0495
CCC These are the NEW O-C CDS terms. They are of the Tkk form.          CCC0495
                  RECCC=1.33D0                                          CCC0495
                  DELTAR=0.1D0                                          CCC0495
                  CUTOF1=RECCC+DELTAR                                   CCC0495
                  IF (RLIO(IAT,J).LT.CUTOF1) CALL HBADD1 (IAT,J,1,      DL0397
     .            CUTOF1,DELTAR,CORFN5,ATAR(IAT),HB(IAT),COBND5)
              ELSE IF (NAT(J).EQ.7) THEN                                CCC0595
CCC These are the NEW O-N CDS terms. They are of the Tkk form.          CCC0595
                 RECCC=1.50D0                                           CCC0595
                 DELTAR=0.3D0                                           CCC0595
                 CUTOF1=RECCC+DELTAR                                    CCC0595
                 IF (RLIO(IAT,J).LT.CUTOF1) CALL HBADD1 (IAT,J,1,       DL0397
     .           CUTOF1,DELTAR,ONRFN4,ATAR(IAT),HB(IAT),ONBNDO)
              ELSE IF (NAT(J).EQ.15) THEN                               GDH0596
CCC These are the NEW O-P CDS terms. They are of the Tkk form.          GSH0596
                 RECCC=2.00D0                                           GDH0596
                 DELTAR=0.3D0                                           GDH0596
                 CUTOF1=RECCC+DELTAR                                    GDH0596
                 IF (RLIO(IAT,J).LT.CUTOF1) CALL HBADD1 (IAT,J,1,       DL0397
     .           CUTOF1,DELTAR,OPRFN5,ATAR(IAT),HB(IAT),OPBNDO)
              ELSE IF (NAT(J).EQ.8.AND.J.NE.IAT) THEN                   CCC0495
CC Add the Oxygen - Oxygen Surface Tension term                         CCC0595
CCC   sigma(OO) surface tension.
                 RECCC=2.75D0                                           CCC0495
                 DELTAR=0.3D0                                           CCC0495
                 CUTOF1=RECCC+DELTAR                                    CCC0495
                 IF (RLIO(IAT,J).LT.CUTOF1) THEN                        CCC0495
                    KCCAN=KCCAN+1                                       CCC0795
                    CALL HBADD1 (IAT,J,2,                               DL0397
     .              CUTOF1,DELTAR,DUMMY,DUMMY,RTKK,DUMMY)
                 ENDIF                                                  CCC0495
              ENDIF                                                     GDH0596
   20       CONTINUE                                                    CCC0495
            IF (KCCAN.GE.1.AND.RTKK.GT.0.00000001) THEN                 CCC0795
C note, DL 0397: BYYO=1.0D0, REYYO=-0.4D0  DELTA0=0.4D0 ==> CUTOF1=0D0  DL0397
               DELTAO=0.4D0                                             CCC0795
               XRTKO=EXP(-DELTAO/RTKK)                                  DL0397
               OOBNDO=OOBNDO+XRTKO*ATAR(IAT)                            DL0397
               FACTR1=XRTKO*OORFN5*1D-3                                 DL0397
               HB(IAT)=HB(IAT)+FACTR1*ATAR(IAT)                         DL0397
               IF (LGR) THEN                                            DL0397
                  FACTR2=OORFN5*ATAR(IAT)*1D-3*XRTKO*DELTAO/RTKK**2     DL0397
                  DO 30 K=1,3                                           DL0397
                  DO 30 L=1,NUMAT                                       DL0397
                  DCDS(K,L)=DCDS(K,L)+FACTR1*DATAR(K,L,IAT)             DL0397
     .                     +FACTR2*BUF(L)*URLIO(K,IAT,L)                DL0397
                  DCDS(K,IAT)=DCDS(K,IAT)-FACTR2*BUF(L)*URLIO(K,IAT,L)  DL0397
   30             CONTINUE                                              DL0397
               ENDIF                                                    CCC0795
            ENDIF                                                       CCC0795
         ELSE IF (NAT(IAT).EQ.7) THEN                                   CCC0695
CCC Add the Nitrogen bonded to Carbon Surface Tension terms             CCC0695
CCC The terms are a sum over all N bonded to C of a COT times the       CCC0695
C   sum of all other atoms attached to the C attached to the N raised   GDH0796
C   to a power.  See the paper for more explicit details.               GDH0796
            RECCC=1.84D0                                                CCC0695
            RECCH=1.55D0                                                GDH0796
            DELTAR=0.3D0                                                CCC0695
            POWER=1.3D0                                                 DL0397
            CUTOF1=RECCC+DELTAR                                         CCC0695
            RTKKS=0.0D0                                                 GDH0796
            IF (LGR) THEN                                               DL0397
               DO 40 J=1,NUMAT                                          DL0397
   40          CALL SCOPY (NUMAT,ZERO,0,BUF2(1,J),1)                    DL0397
            ENDIF                                                       DL0397
            DO 60 J=1,NUMAT                                             CCC0695
              IF (NAT(J).EQ.6) THEN                                     CCC0695
                 IF (RLIO(IAT,J).LT.CUTOF1) THEN                        CCC0695
                    RTKK3=0.0D0                                         GDH0796
                    IF (LGR) CALL SCOPY (NUMAT,ZERO,0,BUF,1)            DL0397
                    DO 50 K=1,NUMAT                                     GDH0796
                      IF (K.NE.IAT.AND.K.NE.J) THEN                     GDH0796
                         IF (NAT(K).EQ.1) THEN                          GDH0796
                            CUTOF2=RECCH+DELTAR                         GDH0796
                         ELSE                                           GDH0796
                            CUTOF2=RECCC+DELTAR                         GDH0796
                         ENDIF                                          GDH0796
                         IF (RLIO(J,K).LT.CUTOF2) CALL HBADD1 (J,K,2,   DL0397
     .                   CUTOF2,DELTAR,DUMMY,DUMMY,RTKK3,DUMMY)
                      ENDIF                                             GDH0796
   50               CONTINUE                                            GDH0796
                    IF (RTKK3.GT.0D0) THEN                              DL0397
                       EXPON=DELTAR/(RLIO(IAT,J)-CUTOF1)                DL0397
                       RTKK=EXP(EXPON)                                  DL0397
                       RTKK32=RTKK3**2                                  DL0397
                       RTKKS=RTKKS+RTKK*RTKK32                          DL0397
                       IF (LGR) THEN                                    DL0397
                          FCTR1=2D0*RTKK*RTKK3                          DL0397
                          CALL SAXPY (NUMAT,FCTR1,BUF,1,BUF2(1,J),1)    DL0397
                          BUF2(J,IAT)=BUF2(J,IAT)                       DL0397
     .                    -RTKK*EXPON/(RLIO(IAT,J)-CUTOF1)*RTKK32       DL0397
                       ENDIF                                            DL0397
                    ENDIF                                               DL0397
                 ENDIF                                                  GDH0796
              ENDIF                                                     GDH0796
   60       CONTINUE                                                    GDH0796
            IF (RTKKS.GT.0D0) THEN                                      DL0397
               RTKKSP=RTKKS**(POWER-1D0)                                DL0397
               CNBNDO=CNBNDO+RTKKS*RTKKSP*ATAR(IAT)                     DL0397
               FCTR1=RTKKSP*ATAR(IAT)*1D-3*CNRFN4                       DL0397
               HB(IAT)=HB(IAT)+RTKKS*FCTR1                              DL0397
               IF (LGR) THEN                                            DL0397
                  FCTR2=POWER*FCTR1                                     DL0397
                  FCTR1=RTKKS*RTKKSP*1D-3*CNRFN4                        DL0397
                  DO 71 J=1,NUMAT                                       DL0397
                  DO 70 L=1,J-1                                         DL0397
                  TERM=FCTR2*(BUF2(J,L)+BUF2(L,J))                      DL0397
                  DCDS(1,J)=DCDS(1,J)+TERM*URLIO(1,L,J)                 DL0397
                  DCDS(2,J)=DCDS(2,J)+TERM*URLIO(2,L,J)                 DL0397
                  DCDS(3,J)=DCDS(3,J)+TERM*URLIO(3,L,J)                 DL0397
                  DCDS(1,L)=DCDS(1,L)-TERM*URLIO(1,L,J)                 DL0397
                  DCDS(2,L)=DCDS(2,L)-TERM*URLIO(2,L,J)                 DL0397
                  DCDS(3,L)=DCDS(3,L)-TERM*URLIO(3,L,J)                 DL0397
   70             CONTINUE                                              DL0397
                  DCDS(1,J)=DCDS(1,J)+FCTR1*DATAR(1,J,IAT)              DL0397
                  DCDS(2,J)=DCDS(2,J)+FCTR1*DATAR(2,J,IAT)              DL0397
                  DCDS(3,J)=DCDS(3,J)+FCTR1*DATAR(3,J,IAT)              DL0397
   71             CONTINUE                                              DL0397
               ENDIF                                                    DL0397
            ENDIF                                                       DL0397
         ELSE IF (NAT(IAT).EQ.6) THEN                                   CCC0495
CCC  These are the NEW C-C CDS terms. They are of the Tkk form.         CCC0495
            DO 85 KCCC=1,2                                              CCC0495
               IF (KCCC.EQ.1) THEN                                      CCC0495
                  RECCC=1.84D0                                          CCC0495
                  DELTAR=0.3D0                                          CCC0495
               ELSE IF (KCCC.EQ.2) THEN                                 CCC0495
                  RECCC=1.27D0                                          CCC0495
                  DELTAR=0.07D0                                         CCC0495
               ENDIF                                                    CCC0495
               CUTOF1=RECCC+DELTAR                                      CCC0495
               DO 80 J=1,NUMAT                                          CCC0495
                 IF (J.EQ.IAT) GOTO 80                                  CCC0495
                 IF (NAT(J).EQ.6) THEN                                  CCC0495
                    IF (RLIO(IAT,J).LT.CUTOF1) THEN
                       IF (KCCC.EQ.1) THEN
                          CALL HBADD1 (IAT,J,1,                         DL0397
     .                    CUTOF1,DELTAR,CRFNH4,ATAR(IAT),HB(IAT),CBONDO)
                       ELSE IF (KCCC.EQ.2) THEN
                          CALL HBADD1 (IAT,J,1,                         DL0397
     .                    CUTOF1,DELTAR,CRFNH5,ATAR(IAT),HB(IAT),CBOND5)
                       ENDIF
                    ENDIF
                 ENDIF                                                  CCC0495
   80          CONTINUE                                                 CCC0495
   85       CONTINUE                                                    CCC0495
         ELSE IF (NAT(IAT).EQ.16) THEN                                  CCC0695
CCC These are the NEW S-S CDS terms. They are of the Tkk form.          GSH0596
            RECCC=2.75D0                                                CCC0695
            DELTAR=0.3D0                                                CCC0695
            DO 90 J=1,NUMAT                                             CCC0695
               IF (J.EQ.IAT) GOTO 90                                    CCC0695
               IF (NAT(J).EQ.16) THEN                                   CCC0695
                  CUTOF1=RECCC+DELTAR                                   CCC0695
                  IF (RLIO(IAT,J).LT.CUTOF1) CALL HBADD1 (IAT,J,1,      DL0397
     .            CUTOF1,DELTAR,SSRFN5,ATAR(IAT),HB(IAT),SSBNDO)
               ENDIF                                                    CCC0495
   90       CONTINUE                                                    CCC0495
         ENDIF                                                          CCC0495
950   CONTINUE
      END
C ======================================================================
      SUBROUTINE HBADD1 (IAT,J,IOPT,CUTOF,DELTAR,HRF,ATAR,SUM,BOND)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES.i'
*******************************************************************************
*     BASIC Tkk TERM + DERIVATIVES. CREATED BY DL 0397.
*     DEDICATED TO CALLING ROUTINE: SM5HB.
*******************************************************************************
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM),RMAX(NUMATM)
     1               ,URLIO(3,NUMATM,NUMATM)                            DL0397
      COMMON /SCRCHR/ BUF(NUMATM),BUF2(NUMATM,NUMATM)                   DL0397
      COMMON /DSOLVA/ DATAR(3,NUMATM,NUMATM),DCDS(3,NUMATM)             DL0397
     1               ,DBORN(3,NUMATM,NUMATM),DFGB(NPACK,3,NUMATM)       DL0397
     2               ,DATLGR(3,NUMATM)                                  DL0397
     3               ,LGR,FLAGRD                                        DL0397
                      LOGICAL LGR,FLAGRD                                DL0397
C     FUNCTION OF THE Tkk FORM, TAKING BCCC = 1.0D0                     DL0397
      EXPONT=DELTAR/(RLIO(IAT,J)-CUTOF)                                 DL0397
      RTKK=EXP(EXPONT)                                                  DL0397
      IF (LGR) DRTKK=-RTKK*EXPONT/(RLIO(IAT,J)-CUTOF)                   DL0397
      IF (IOPT.EQ.1) THEN                                               DL0397
         BOND=BOND+RTKK*ATAR                                            DL0397
         FACTR1=RTKK*HRF*1D-3                                           DL0397
         SUM=SUM+ATAR*FACTR1                                            DL0397
         IF (.NOT.LGR) RETURN                                           DL0397
C        ADD TO DCDS THE DERIVATIVE OF THE CURRENT CONTRIBUTION TO SUM. DL0397
         FACTR2=ATAR*HRF*1D-3*DRTKK                                     DL0397
         DO 11 K=1,3                                                    DL0397
         DCDS(K,IAT)=DCDS(K,IAT)+FACTR2*URLIO(K,J,IAT)                  DL0397
         DCDS(K,J  )=DCDS(K,J  )-FACTR2*URLIO(K,J,IAT)                  DL0397
         DO 10 L=1,NUMAT                                                DL0397
   10    DCDS(K,L)=DCDS(K,L)+FACTR1*DATAR(K,L,IAT)                      DL0397
   11    CONTINUE                                                       DL0397
      ELSE IF (IOPT.EQ.2) THEN                                          DL0397
C        ADD TO SUM THE CURRENT RTKK  AND                               DL0397
C        ADD TO BUF(J) THE DERIVATIVE OF RTKK VS RLIO(IAT,J).           DL0397
         SUM=SUM+RTKK                                                   DL0397
         IF (LGR) BUF(J)=BUF(J)+DRTKK                                   DL0397
      ENDIF                                                             DL0397
      END
